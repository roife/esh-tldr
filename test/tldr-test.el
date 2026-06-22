;;; tldr-test.el --- Tests for tldr.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tldr)

(defmacro tldr-test-with-env (bindings &rest body)
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

(defun tldr-test-page (root language platform command content)
  (let ((dir (expand-file-name (format "%s/%s" language platform) root)))
    (make-directory dir t)
    (let ((file (expand-file-name (concat command ".md") dir)))
      (with-temp-file file
        (insert content))
      file)))

(defconst tldr-test-tar-page
  "# tar

> Archive utility.
> More information: <https://example.com>.

- Extract an archive:

`tar xf {{source.tar}} -C {{directory}}`

- List contents:

`tar tvf {{source.tar}}`
")

(ert-deftest tldr-locale-parsing ()
  (tldr-test-with-env (("LC_ALL" nil) ("LC_MESSAGES" nil) ("LANG" "zh_CN.UTF-8"))
    (let ((tldr-language 'auto))
      (should (equal (tldr--language-directories)
                     '("pages.zh_CN" "pages.zh" "pages")))))
  (dolist (locale '("C" "POSIX" "" "C.UTF-8"))
    (should (equal (tldr--language-directories locale) '("pages"))))
  (should (equal (tldr--language-directories "pt_BR.UTF-8")
                 '("pages.pt_BR" "pages.pt" "pages"))))

(ert-deftest tldr-platform-parsing ()
  (should (equal (tldr--platform-directories 'darwin) '("osx" "common")))
  (should (equal (tldr--platform-directories 'gnu/linux) '("linux" "common")))
  (should (equal (tldr--platform-directories 'windows-nt) '("windows" "common")))
  (should (equal (tldr--platform-directories 'ms-dos) '("windows" "common")))
  (should (equal (tldr--platform-directories 'cygwin) '("windows" "common")))
  (should (equal (tldr--platform-directories 'berkeley-unix) '("common")))
  (let ((tldr-platform "freebsd"))
    (should (equal (tldr--platform-directories) '("freebsd" "common")))))

(ert-deftest tldr-candidate-paths-are-finite-and-ordered ()
  (let ((tldr-pages-directory "/tmp/tldr")
        (tldr-language "zh_CN.UTF-8")
        (tldr-platform "osx"))
    (should (equal (mapcar (lambda (file)
                             (file-relative-name file tldr-pages-directory))
                           (tldr--candidate-files "tar"))
                   '("pages.zh_CN/osx/tar.md"
                     "pages.zh_CN/common/tar.md"
                     "pages.zh/osx/tar.md"
                     "pages.zh/common/tar.md"
                     "pages/osx/tar.md"
                     "pages/common/tar.md")))))

(ert-deftest tldr-does-not-scan-for-candidates ()
  (let ((tldr-pages-directory "/tmp/tldr")
        (tldr-language "zh_CN.UTF-8")
        (tldr-platform "osx"))
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _)
                 (error "directory scan"))))
      (should (= (length (tldr--candidate-files "tar")) 6)))))

(ert-deftest tldr-command-candidates-are-active-dirs-only-and-deduped ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "osx"))
    (tldr-test-page root "pages" "osx" "tar" tldr-test-tar-page)
    (tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (tldr-test-page root "pages" "common" "tar" "# tar\n\n> Tar.\n")
    (tldr-test-page root "pages" "linux" "apt" "# apt\n\n> Apt.\n")
    (should (equal (tldr--command-candidates) '("tar" "git")))))

(ert-deftest tldr-read-command-uses-consult-when-available ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common")
         (orig-require (symbol-function 'require))
         called)
    (tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'consult)
                     t
                   (funcall orig-require feature filename noerror))))
              ((symbol-function 'consult--read)
               (lambda (candidates &rest options)
                 (setq called (list candidates options))
                 "git")))
      (should (equal (tldr--read-command) "git"))
      (should (equal (car called) '("git")))
      (should (eq (plist-get (cadr called) :category) 'tldr-command)))))

(ert-deftest tldr-markdown-page-parsing ()
  (let* ((file (make-temp-file "tldr" nil ".md" tldr-test-tar-page))
         (page (tldr--parse-page file)))
    (should (equal (tldr--page-title page) "tar"))
    (should (string-match-p "Archive utility" (tldr--page-description page)))
    (should (equal (length (tldr--page-examples page)) 2))
    (should (equal (tldr--example-description (car (tldr--page-examples page)))
                   "Extract an archive"))
    (should (equal (tldr--example-command (car (tldr--page-examples page)))
                   "tar xf {{source.tar}} -C {{directory}}"))))

(ert-deftest tldr-placeholder-template-generation ()
  (should (equal (tldr--tempo-template "tar xf {{source.tar}} -C {{directory}}")
                 '("tar xf " (p "source.tar" tldr-1) " -C " (p "directory" tldr-2))))
  (should (equal (tldr--tempel-template "tar xf {{source.tar}} -C {{directory}}")
                 '("tar xf " (p "source.tar" tldr-1) " -C " (p "directory" tldr-2)))))

(ert-deftest tldr-repeated-placeholder-reuse ()
  (should (equal (tldr--tempo-template "cp {{source}} {{source}}.bak")
                 '("cp " (p "source" tldr-1) " " (s tldr-1) ".bak"))))

(ert-deftest tldr-template-title-slug ()
  (should (equal (tldr--template-name
                  "ls"
                  "List files in [l]ong format, sorted by [S]ize (descending) recursively")
                 "ls/list-files-in-long-format_sorted-by-size-descending-recursively")))

(ert-deftest tldr-capf-candidate-generation ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common"))
    (tldr-test-page root "pages" "common" "tar" tldr-test-tar-page)
    (with-temp-buffer
      (insert "tar/")
      (let* ((capf (capf-tldr))
             (candidates (nth 2 capf)))
        (should (member "tar/extract-an-archive" candidates))
        (should (member "tar/list-contents" candidates))))))

(ert-deftest tldr-capf-expansion-replaces-title-with-template ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common")
         (tldr-use-tempel nil))
    (tldr-test-page
     root "pages" "common" "tar"
     "# tar

> Archive utility.

- List contents:

`tar tvf archive.tar`
")
    (with-temp-buffer
      (insert "tar/list")
      (let* ((capf (capf-tldr))
             (candidate "tar/list-contents")
             (exit (plist-get (nthcdr 3 capf) :exit-function)))
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert candidate)
        (funcall exit candidate 'finished)
        (should (equal (buffer-string) "tar tvf archive.tar"))))))

(ert-deftest tldr-open-reuses-single-buffer ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common"))
    (tldr-test-page root "pages" "common" "tar" tldr-test-tar-page)
    (tldr-test-page
     root "pages" "common" "git"
     "# git

> Version control.

- Show status:

`git status`
")
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     buffer)))
          (with-temp-buffer
            (tldr--open "tar")
            (let ((buffer (get-buffer tldr--buffer-name)))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (equal tldr--command "tar")))
              (tldr--open "git")
              (should (eq buffer (get-buffer tldr--buffer-name)))
              (with-current-buffer buffer
                (should (equal tldr--command "git"))
                (should (string-match-p "git status" (buffer-string)))))))
      (when-let* ((buffer (get-buffer tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest tldr-consult-candidate-display ()
  (let* ((source-buffer (generate-new-buffer "tldr-source"))
         (marker (with-current-buffer source-buffer (point-marker)))
         (example (tldr--make-example
                   :description "[c]reate files in [l]ong format, sorted by [S]ize (descending) recursively"
                   :command "ls -lSR"))
         (page (tldr--make-page :title "ls"
                                :description "List directory contents."
                                :file "/tmp/ls.md"
                                :examples (list example))))
    (unwind-protect
        (let ((candidates (tldr--consult-candidates "ls" page marker)))
          (should (equal (substring-no-properties (car candidates))
                         "Create files in long format, sorted by Size (descending) recursively"))
          (should (equal (plist-get (tldr--consult-data (car candidates)) :example)
                         example)))
      (kill-buffer source-buffer))))

(ert-deftest tldr-open-locates-consult-entry ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common"))
    (tldr-test-page root "pages" "common" "tar" tldr-test-tar-page)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     buffer)))
          (with-temp-buffer
            (tldr--open "tar" 1)
            (with-current-buffer (get-buffer tldr--buffer-name)
              (should (equal (get-text-property (point) 'tldr-example) 1))
              (should (looking-at-p "- List contents")))))
      (when-let* ((buffer (get-buffer tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest tldr-consult-embark-action-inserts-template ()
  (let ((source-buffer (generate-new-buffer "tldr-source"))
        (tldr-use-tempel nil))
    (unwind-protect
        (let* ((marker (with-current-buffer source-buffer
                         (insert "run ")
                         (point-marker)))
               (example (tldr--make-example
                         :description "List contents"
                         :command "tar tvf archive.tar"))
               (page (tldr--make-page :title "tar"
                                      :description "Archive utility."
                                      :file "/tmp/tar.md"
                                      :examples (list example)))
               (candidate (car (tldr--consult-candidates "tar" page marker))))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (set-buffer buffer))))
            (tldr-consult-insert-template candidate))
          (with-current-buffer source-buffer
            (should (equal (buffer-string) "run tar tvf archive.tar"))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest tldr-consult-empty-selection-opens-command-page ()
  (let* ((root (make-temp-file "tldr" t))
         (tldr-pages-directory root)
         (tldr-language "C")
         (tldr-platform "common")
         (orig-require (symbol-function 'require)))
    (tldr-test-page root "pages" "common" "tar" tldr-test-tar-page)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'consult)
                         t
                       (funcall orig-require feature filename noerror))))
                  ((symbol-function 'consult--read)
                   (lambda (candidates &rest _options)
                     (should (equal (car candidates) ""))
                     (should (null (tldr--consult-lookup "" candidates)))
                     nil))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     buffer)))
          (with-temp-buffer
            (consult-tldr "tar")
            (with-current-buffer (get-buffer tldr--buffer-name)
              (should (equal tldr--command "tar"))
              (should (= (point) (point-min)))
              (should-not (get-text-property (point) 'tldr-example)))))
      (when-let* ((buffer (get-buffer tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest tldr-insert-closes-page-buffer ()
  (let ((source-buffer (generate-new-buffer "tldr-source"))
        (page-buffer (get-buffer-create tldr--buffer-name))
        (tldr-use-tempel nil))
    (unwind-protect
        (let (source-marker)
          (with-current-buffer source-buffer
            (insert "run ")
            (setq source-marker (point-marker)))
          (with-current-buffer page-buffer
            (tldr--show-page
             (tldr--make-page
              :title "tar"
              :description "Archive utility."
              :file "/tmp/tar.md"
              :examples (list (tldr--make-example
                               :description "List contents"
                               :command "tar tvf archive.tar")))
             "tar"
             source-marker)
            (goto-char (text-property-any (point-min) (point-max) 'tldr-example 0))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (set-buffer buffer))))
              (tldr-insert-command)))
          (should-not (buffer-live-p page-buffer))
          (with-current-buffer source-buffer
            (should (equal (buffer-string) "run tar tvf archive.tar"))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (when (buffer-live-p page-buffer)
        (kill-buffer page-buffer)))))

(ert-deftest tldr-mode-has-no-i-or-w-binding ()
  (should-not (lookup-key tldr-mode-map (kbd "i")))
  (should-not (lookup-key tldr-mode-map (kbd "w"))))

(ert-deftest tldr-page-commands-are-mode-specific ()
  (dolist (command '(tldr-copy-command
                     tldr-insert-command
                     tldr-next-example
                     tldr-previous-example
                     tldr-reload))
    (should (equal (get command 'command-modes) '(tldr-mode))))
  (dolist (command '(tldr tldr-at-point tldr-dwim tldr-update consult-tldr))
    (should-not (get command 'command-modes))))

(provide 'tldr-test)

;;; tldr-test.el ends here
