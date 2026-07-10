;;; esh-tldr-test.el --- Tests for esh-tldr.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'esh-tldr)

(defmacro esh-tldr-test-with-env (bindings &rest body)
  (declare (indent 1))
  (let ((old (make-symbol "old")))
    `(let ((,old (mapcar (lambda (var) (cons var (getenv var)))
                         '("LC_ALL" "LC_MESSAGES" "LANG"))))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (binding)
                         `(setenv ,(car binding) ,(cadr binding)))
                       bindings)
             ,@body)
         (dolist (cell ,old)
           (setenv (car cell) (cdr cell)))))))

(defun esh-tldr-test-page (root language platform command content)
  (let ((dir (expand-file-name (format "%s/%s" language platform) root)))
    (make-directory dir t)
    (let ((file (expand-file-name (concat command ".md") dir)))
      (with-temp-file file
        (insert content))
      file)))

(defconst esh-tldr-test-tar-page
  "# tar

> Archive utility.
> More information: <https://example.com>.

- Extract an archive:

`tar xf {{source.tar}} -C {{directory}}`

- List contents:

`tar tvf {{source.tar}}`
")

(defun esh-tldr-test-target (buffer beg end)
  (with-current-buffer buffer
    (esh-tldr--target beg end)))

(defmacro esh-tldr-test-with-pop-to-buffer (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'pop-to-buffer)
              (lambda (buffer &rest _)
                (set-buffer buffer))))
     ,@body))

(ert-deftest esh-tldr-locale-parsing ()
  (esh-tldr-test-with-env (("LC_ALL" nil) ("LC_MESSAGES" nil) ("LANG" "zh_CN.UTF-8"))
    (let ((esh-tldr-language 'auto))
      (should (equal (esh-tldr--language-directories)
                     '("pages.zh_CN" "pages.zh" "pages")))))
  (dolist (locale '("C" "POSIX" "" "C.UTF-8"))
    (should (equal (esh-tldr--language-directories locale) '("pages"))))
  (esh-tldr-test-with-env (("LC_ALL" "  ") ("LC_MESSAGES" "")
                           ("LANG" "zh_CN.UTF-8"))
    (let ((esh-tldr-language 'auto))
      (should (equal (esh-tldr--language-directories)
                     '("pages.zh_CN" "pages.zh" "pages")))))
  (should (equal (esh-tldr--language-directories "C.utf8") '("pages")))
  (should (equal (esh-tldr--language-directories "pt_BR.UTF-8")
                 '("pages.pt_BR" "pages.pt" "pages"))))

(ert-deftest esh-tldr-platform-parsing ()
  (should (equal (esh-tldr--platform-directories 'darwin) '("osx" "common")))
  (should (equal (esh-tldr--platform-directories 'gnu/linux) '("linux" "common")))
  (should (equal (esh-tldr--platform-directories 'windows-nt) '("windows" "common")))
  (should (equal (esh-tldr--platform-directories 'berkeley-unix) '("common")))
  (let ((esh-tldr-platform "freebsd"))
    (should (equal (esh-tldr--platform-directories) '("freebsd" "common")))))

(ert-deftest esh-tldr-candidate-paths-are-finite-and-ordered ()
  (let ((esh-tldr-pages-directory "/tmp/esh-tldr")
        (esh-tldr-language "zh_CN.UTF-8")
        (esh-tldr-platform "osx"))
    (should (equal (mapcar (lambda (file)
                             (file-relative-name file esh-tldr-pages-directory))
                           (esh-tldr--candidate-files "tar"))
                   '("pages.zh_CN/osx/tar.md"
                     "pages.zh_CN/common/tar.md"
                     "pages.zh/osx/tar.md"
                     "pages.zh/common/tar.md"
                     "pages/osx/tar.md"
                     "pages/common/tar.md")))))

(ert-deftest esh-tldr-command-candidates-are-active-dirs-only-and-deduped ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "osx"))
    (esh-tldr-test-page root "pages" "osx" "tar" esh-tldr-test-tar-page)
    (esh-tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (esh-tldr-test-page root "pages" "common" "tar" "# tar\n\n> Tar.\n")
    (esh-tldr-test-page root "pages" "linux" "apt" "# apt\n\n> Apt.\n")
    (should (equal (esh-tldr--command-candidate-entries)
                   '(("tar" . "pages/osx") ("git" . "pages/common"))))))

(ert-deftest esh-tldr-command-candidates-are-cached ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common")
         (esh-tldr--command-entry-cache nil)
         (calls 0)
         (original-directory-files (symbol-function 'directory-files)))
    (esh-tldr-test-page root "pages" "common" "git" "# git\n")
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply original-directory-files args))))
      (should (equal (esh-tldr--command-candidate-entries)
                     '(("git" . "pages/common"))))
      (esh-tldr--command-candidate-entries)
      (should (= calls 1))
      (esh-tldr--clear-command-cache)
      (esh-tldr--command-candidate-entries)
      (should (= calls 2)))))

(ert-deftest esh-tldr-completion-table-exposes-emacs-31-metadata ()
  (let* ((table (esh-tldr--completion-table
                 '(("tar" . "pages/osx") ("git" . "pages/common"))))
         (metadata (completion-metadata "" table nil))
         (annotation (completion-metadata-get metadata 'annotation-function)))
    (should (eq (completion-metadata-get metadata 'category) 'esh-tldr-command))
    (should (eq (completion-metadata-get metadata 'eager-display) t))
    (should (eq (completion-metadata-get metadata 'eager-update) t))
    (should (equal (all-completions "t" table) '("tar")))
    (should (equal (substring-no-properties (funcall annotation "tar"))
                   "  pages/osx"))))

(ert-deftest esh-tldr-read-command-uses-standard-completing-read ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common")
         called)
    (esh-tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt table &rest options)
                 (setq called (list prompt table options))
                 "git status")))
      (should (equal (esh-tldr--read-command "git") "git"))
      (should (string-match-p "default git" (car called)))
      (should (member "git" (all-completions "" (cadr called))))
      (should (eq (nth 3 (nth 2 called)) 'esh-tldr-command-history)))))

(ert-deftest esh-tldr-removed-consult-and-capf-interfaces ()
  (should-not (fboundp 'consult-esh-tldr))
  (should-not (fboundp 'capf-esh-tldr))
  (should-not (fboundp 'esh-tldr-capf-setup)))

(ert-deftest esh-tldr-markdown-page-parsing ()
  (let* ((file (make-temp-file "esh-tldr" nil ".md" esh-tldr-test-tar-page))
         (page (esh-tldr--parse-page file)))
    (should (equal (esh-tldr--page-title page) "tar"))
    (should (string-match-p "Archive utility" (esh-tldr--page-description page)))
    (should (equal (length (esh-tldr--page-examples page)) 2))
    (should (equal (esh-tldr--example-description
                    (car (esh-tldr--page-examples page)))
                   "Extract an archive"))
    (should (equal (esh-tldr--example-command
                    (car (esh-tldr--page-examples page)))
                   "tar xf {{source.tar}} -C {{directory}}"))))

(ert-deftest esh-tldr-missing-page-returns-nil ()
  (let ((esh-tldr-pages-directory (make-temp-file "esh-tldr" t))
        (esh-tldr-language "C")
        (esh-tldr-platform "common"))
    (should-not (esh-tldr--read-page "missing"))))

(ert-deftest esh-tldr-placeholder-template-generation ()
  (pcase (esh-tldr--template-elements
          "tar xf {{source.tar}} -C {{directory}}")
    (`("tar xf " (p "source.tar" ,source)
       " -C " (p "directory" ,directory))
     (should (string= (symbol-name source) "esh-tldr-1"))
     (should (string= (symbol-name directory) "esh-tldr-2")))
    (template (ert-fail (format "Unexpected template: %S" template)))))

(ert-deftest esh-tldr-repeated-placeholder-reuse ()
  (pcase (esh-tldr--template-elements "cp {{source}} {{source}}.bak")
    (`("cp " (p "source" ,first) " " (s ,second) ".bak")
     (should (eq first second)))
    (template (ert-fail (format "Unexpected template: %S" template)))))

(ert-deftest esh-tldr-shell-command-token-parsing ()
  (should (equal (esh-tldr--command-token-in-string "ls -l" 5)
                 '("ls" 0 2)))
  (should (equal (esh-tldr--command-token-in-string "echo ok | ls -l" 15)
                 '("ls" 10 12)))
  (should (equal (esh-tldr--command-token-in-string "FOO=1 ls -l" 11)
                 '("ls" 6 8)))
  (should (equal (esh-tldr--command-token-in-string
                  "echo \"a|b\" | grep x" 19)
                 '("grep" 13 17)))
  (should (equal (esh-tldr--command-token-in-string "ls -l # keep" 12)
                 '("ls" 0 2)))
  (should (equal (esh-tldr--command-token-in-string "ls -l > out" 11)
                 '("ls" 0 2)))
  (dolist (input '("echo ok | "
                   "echo $(foo | bar)"
                   "echo `foo | bar`"
                   "echo \"unterminated"))
    (should-not (esh-tldr--command-token-in-string input (length input)))))

(ert-deftest esh-tldr-region-context-targets-command-token ()
  (with-temp-buffer
    (insert "before ls -l after")
    (let ((transient-mark-mode t))
      (goto-char 8)
      (set-mark 13)
      (setq mark-active t)
      (let* ((context (esh-tldr--region-context))
             (target (esh-tldr--context-target context)))
        (should (equal (esh-tldr--context-command context) "ls"))
        (should (equal (esh-tldr--target-original target) "ls"))))))

(ert-deftest esh-tldr-point-context-replaces-command-word ()
  (with-temp-buffer
    (insert "Run ls here")
    (goto-char 6)
    (let* ((context (esh-tldr--point-context))
           (target (esh-tldr--context-target context)))
      (should (equal (esh-tldr--context-command context) "ls"))
      (should (equal (esh-tldr--target-original target) "ls")))))

(ert-deftest esh-tldr-comint-context-targets-current-command-token ()
  (with-temp-buffer
    (comint-mode)
    (insert "$ echo ok | ls -l")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'comint-line-beginning-position)
               (lambda () 3)))
      (let* ((context (esh-tldr--terminal-context))
             (target (esh-tldr--context-target context)))
        (should (equal (esh-tldr--context-command context) "ls"))
        (should (equal (esh-tldr--target-original target) "ls"))))))

(ert-deftest esh-tldr-safe-replacement-only-replaces-command-token ()
  (dolist (case '(("ls" "ls -a")
                  ("ls -l" "ls -a -l")
                  ("FOO=1 ls -l" "FOO=1 ls -a -l")
                  ("echo ok | ls -l" "echo ok | ls -a -l")
                  ("ls -l # keep" "ls -a -l # keep")
                  ("ls -l > out" "ls -a -l > out")))
    (let ((buffer (generate-new-buffer "esh-tldr-source")))
      (unwind-protect
          (with-current-buffer buffer
            (insert (car case))
            (let* ((context (esh-tldr--context-in-bounds
                             (point-min) (point-max) (point-max)))
                   (target (esh-tldr--context-target context)))
              (esh-tldr-test-with-pop-to-buffer
                (should (esh-tldr--replace-buffer-target "ls -a" target)))
              (should (equal (buffer-string) (cadr case)))))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-zero-width-target-inserts-at-point ()
  (let ((buffer (generate-new-buffer "esh-tldr-source")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "run ")
          (let ((target (esh-tldr--target (point) (point))))
            (esh-tldr-test-with-pop-to-buffer
              (should (esh-tldr--replace-buffer-target "ls -a" target)))
            (should (equal (buffer-string) "run ls -a"))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-stale-target-copies-without-overwriting ()
  (let ((buffer (generate-new-buffer "esh-tldr-source"))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (with-current-buffer buffer
          (insert "ls")
          (let ((target (esh-tldr--target (point-min) (point-max))))
            (erase-buffer)
            (insert "pwd")
            (should-not (esh-tldr--use-command "ls -a" target))
            (should (equal (buffer-string) "pwd"))
            (should (equal (current-kill 0) "ls -a"))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-read-only-target-falls-back-to-copy ()
  (let ((buffer (generate-new-buffer "esh-tldr-source"))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (with-current-buffer buffer
          (insert "ls")
          (let ((target (esh-tldr--target (point-min) (point-max))))
            (setq buffer-read-only t)
            (should-not (esh-tldr--use-command "ls -a" target))
            (should (equal (buffer-string) "ls"))
            (should (equal (current-kill 0) "ls -a"))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-unexpected-template-failure-rolls-back-and-surfaces ()
  (let ((buffer (generate-new-buffer "esh-tldr-source"))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (with-current-buffer buffer
          (insert "ls")
          (let ((target (esh-tldr--target (point-min) (point-max))))
            (cl-letf (((symbol-function 'esh-tldr--insert-template)
                       (lambda (_command)
                         (insert "partial")
                         (error "template failed")))
                      ((symbol-function 'pop-to-buffer)
                       (lambda (target-buffer &rest _)
                         (set-buffer target-buffer))))
              (should-error (esh-tldr--use-command "ls -a" target)
                            :type 'error))
            (should (equal (buffer-string) "ls"))
            (should-not kill-ring)))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-rejects-directory-separators-in-command-names ()
  (should-not (esh-tldr--valid-command-p "../ls"))
  (should-not (esh-tldr--valid-command-p "..\\ls"))
  (should (esh-tldr--valid-command-p "[")))

(ert-deftest esh-tldr-help-page-renders-actions-and-placeholders ()
  (let* ((source (generate-new-buffer "esh-tldr-source"))
         (target (esh-tldr-test-target source 1 1))
         (page (esh-tldr--make-page
                :title "tar"
                :description "Archive utility."
                :file "/tmp/tar.md"
                :examples (list (esh-tldr--make-example
                                 :description "Extract archive"
                                 :command "tar xf {{archive}}")))))
    (unwind-protect
        (with-temp-buffer
          (esh-tldr--show-page page "tar" target)
          (should (derived-mode-p 'help-mode))
          (should (equal (esh-tldr--current-example-index) 0))
          (should (string-match-p "Insert/replace" (buffer-string)))
          (goto-char (point-min))
          (search-forward "{{archive}}")
          (should (memq 'esh-tldr-placeholder-face
                        (ensure-list
                         (get-text-property (1- (point)) 'face)))))
      (kill-buffer source))))

(ert-deftest esh-tldr-empty-page-shows-actionable-state ()
  (let ((source (generate-new-buffer "esh-tldr-source")))
    (unwind-protect
        (with-temp-buffer
          (let ((esh-tldr-pages-directory "/tmp/no-pages")
                (esh-tldr-executable nil))
            (esh-tldr--show-page nil "missing"
                                 (esh-tldr-test-target source 1 1))
            (should (derived-mode-p 'help-mode))
            (should (string-match-p "No local TL;DR page" (buffer-string)))
            (should (string-match-p "Search another command" (buffer-string)))))
      (kill-buffer source))))

(ert-deftest esh-tldr-page-navigation-wraps ()
  (let ((page (esh-tldr--make-page
               :title "x" :description "" :file "/tmp/x.md"
               :examples (list (esh-tldr--make-example
                                :description "One" :command "x one")
                               (esh-tldr--make-example
                                :description "Two" :command "x two")))))
    (with-temp-buffer
      (esh-tldr--show-page page "x" (esh-tldr--target (point) (point)))
      (should (= (esh-tldr--current-example-index) 0))
      (esh-tldr-previous-example)
      (should (= (esh-tldr--current-example-index) 1))
      (esh-tldr-next-example)
      (should (= (esh-tldr--current-example-index) 0)))))

(ert-deftest esh-tldr-open-reuses-single-buffer-and-supports-empty-pages ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common"))
    (esh-tldr-test-page root "pages" "common" "tar" esh-tldr-test-tar-page)
    (unwind-protect
        (esh-tldr-test-with-pop-to-buffer
          (with-temp-buffer
            (esh-tldr--open "tar")
            (let ((buffer (get-buffer esh-tldr--buffer-name)))
              (should (buffer-live-p buffer))
              (esh-tldr--open "missing")
              (should (eq buffer (get-buffer esh-tldr--buffer-name)))
              (with-current-buffer buffer
                (should (equal esh-tldr--command "missing"))
                (should-not esh-tldr--page)
                (should (string-match-p "No local TL;DR page" (buffer-string)))))))
      (when-let* ((buffer (get-buffer esh-tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-page-ret-replaces-source-and-closes-page ()
  (let ((source (generate-new-buffer "esh-tldr-source"))
        (page-buffer (get-buffer-create esh-tldr--buffer-name)))
    (unwind-protect
        (let (target)
          (with-current-buffer source
            (insert "ls")
            (setq target (esh-tldr--target (point-min) (point-max))))
          (with-current-buffer page-buffer
            (esh-tldr--show-page
             (esh-tldr--make-page
              :title "ls" :description "List files." :file "/tmp/ls.md"
              :examples (list (esh-tldr--make-example
                               :description "List all" :command "ls -a")))
             "ls" target)
            (esh-tldr-test-with-pop-to-buffer
              (esh-tldr-insert-command)))
          (should-not (buffer-live-p page-buffer))
          (with-current-buffer source
            (should (equal (buffer-string) "ls -a"))))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p page-buffer) (kill-buffer page-buffer)))))

(ert-deftest esh-tldr-successful-update-invalidates-command-cache ()
  (let ((esh-tldr-executable "tldr")
        (esh-tldr--command-entry-cache '(cached . entries))
        sentinel refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find) (lambda (_) "/tmp/tldr"))
                  ((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq sentinel (plist-get args :sentinel))
                     'fake-process))
                  ((symbol-function 'process-status) (lambda (_) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_) 0))
                  ((symbol-function 'process-buffer)
                   (lambda (_) (get-buffer-create "*esh-tldr update*")))
                  ((symbol-function 'esh-tldr--refresh-open-buffers)
                   (lambda () (setq refreshed t))))
          (esh-tldr-update)
          (funcall sentinel 'fake-process "finished")
          (should-not esh-tldr--command-entry-cache)
          (should refreshed))
      (when-let* ((buffer (get-buffer "*esh-tldr update*")))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-page-commands-are-mode-specific ()
  (dolist (command '(esh-tldr-activate-or-insert
                     esh-tldr-copy-command
                     esh-tldr-insert-command
                     esh-tldr-next-example
                     esh-tldr-previous-example
                     esh-tldr-reload
                     esh-tldr-search-command))
    (should (equal (get command 'command-modes) '(esh-tldr-mode))))
  (dolist (command '(esh-tldr esh-tldr-at-point esh-tldr-dwim esh-tldr-update))
    (should-not (get command 'command-modes))))

(provide 'esh-tldr-test)

;;; esh-tldr-test.el ends here
