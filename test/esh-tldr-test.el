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

(ert-deftest esh-tldr-locale-parsing ()
  (esh-tldr-test-with-env (("LC_ALL" nil) ("LC_MESSAGES" nil) ("LANG" "zh_CN.UTF-8"))
    (let ((esh-tldr-language 'auto))
      (should (equal (esh-tldr--language-directories)
                     '("pages.zh_CN" "pages.zh" "pages")))))
  (dolist (locale '("C" "POSIX" "" "C.UTF-8"))
    (should (equal (esh-tldr--language-directories locale) '("pages"))))
  (should (equal (esh-tldr--language-directories "pt_BR.UTF-8")
                 '("pages.pt_BR" "pages.pt" "pages"))))

(ert-deftest esh-tldr-platform-parsing ()
  (should (equal (esh-tldr--platform-directories 'darwin) '("osx" "common")))
  (should (equal (esh-tldr--platform-directories 'gnu/linux) '("linux" "common")))
  (should (equal (esh-tldr--platform-directories 'windows-nt) '("windows" "common")))
  (should (equal (esh-tldr--platform-directories 'ms-dos) '("windows" "common")))
  (should (equal (esh-tldr--platform-directories 'cygwin) '("windows" "common")))
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

(ert-deftest esh-tldr-does-not-scan-for-candidates ()
  (let ((esh-tldr-pages-directory "/tmp/esh-tldr")
        (esh-tldr-language "zh_CN.UTF-8")
        (esh-tldr-platform "osx"))
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _)
                 (error "directory scan"))))
      (should (= (length (esh-tldr--candidate-files "tar")) 6)))))

(ert-deftest esh-tldr-command-candidates-are-active-dirs-only-and-deduped ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "osx"))
    (esh-tldr-test-page root "pages" "osx" "tar" esh-tldr-test-tar-page)
    (esh-tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (esh-tldr-test-page root "pages" "common" "tar" "# tar\n\n> Tar.\n")
    (esh-tldr-test-page root "pages" "linux" "apt" "# apt\n\n> Apt.\n")
    (should (equal (esh-tldr--command-candidates) '("tar" "git")))))

(ert-deftest esh-tldr-read-command-uses-consult-when-available ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common")
         (orig-require (symbol-function 'require))
         called)
    (esh-tldr-test-page root "pages" "common" "git" "# git\n\n> Git.\n")
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'consult)
                     t
                   (funcall orig-require feature filename noerror))))
              ((symbol-function 'consult--read)
               (lambda (candidates &rest options)
                 (setq called (list candidates options))
                 "git")))
      (should (equal (esh-tldr--read-command) "git"))
      (should (equal (car called) '("git")))
      (should (eq (plist-get (cadr called) :category) 'esh-tldr-command)))))

(ert-deftest esh-tldr-markdown-page-parsing ()
  (let* ((file (make-temp-file "esh-tldr" nil ".md" esh-tldr-test-tar-page))
         (page (esh-tldr--parse-page file)))
    (should (equal (esh-tldr--page-title page) "tar"))
    (should (string-match-p "Archive utility" (esh-tldr--page-description page)))
    (should (equal (length (esh-tldr--page-examples page)) 2))
    (should (equal (esh-tldr--example-description (car (esh-tldr--page-examples page)))
                   "Extract an archive"))
    (should (equal (esh-tldr--example-command (car (esh-tldr--page-examples page)))
                   "tar xf {{source.tar}} -C {{directory}}"))))

(ert-deftest esh-tldr-placeholder-template-generation ()
  (should (equal (esh-tldr--template-elements "tar xf {{source.tar}} -C {{directory}}")
                 '("tar xf " (p "source.tar" esh-tldr-1) " -C " (p "directory" esh-tldr-2)))))

(ert-deftest esh-tldr-repeated-placeholder-reuse ()
  (should (equal (esh-tldr--template-elements "cp {{source}} {{source}}.bak")
                 '("cp " (p "source" esh-tldr-1) " " (s esh-tldr-1) ".bak"))))

(ert-deftest esh-tldr-template-title-slug ()
  (should (equal (esh-tldr--template-name
                  "ls"
                  "List files in [l]ong format, sorted by [S]ize (descending) recursively")
                 "ls/list-files-in-long-format_sorted-by-size-descending-recursively")))

(ert-deftest esh-tldr-capf-candidate-generation ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common"))
    (esh-tldr-test-page root "pages" "common" "tar" esh-tldr-test-tar-page)
    (with-temp-buffer
      (insert "tar/")
      (let* ((capf (capf-esh-tldr))
             (candidates (nth 2 capf)))
        (should (member "tar/extract-an-archive" candidates))
        (should (member "tar/list-contents" candidates))))))

(ert-deftest esh-tldr-capf-expansion-replaces-title-with-template ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common")
         (esh-tldr-use-tempel nil))
    (esh-tldr-test-page
     root "pages" "common" "tar"
     "# tar

> Archive utility.

- List contents:

`tar tvf archive.tar`
")
    (with-temp-buffer
      (insert "tar/list")
      (let* ((capf (capf-esh-tldr))
             (candidate "tar/list-contents")
             (exit (plist-get (nthcdr 3 capf) :exit-function)))
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert candidate)
        (funcall exit candidate 'finished)
        (should (equal (buffer-string) "tar tvf archive.tar"))))))

(ert-deftest esh-tldr-open-reuses-single-buffer ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common"))
    (esh-tldr-test-page root "pages" "common" "tar" esh-tldr-test-tar-page)
    (esh-tldr-test-page
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
            (esh-tldr--open "tar")
            (let ((buffer (get-buffer esh-tldr--buffer-name)))
              (should (buffer-live-p buffer))
              (with-current-buffer buffer
                (should (equal esh-tldr--command "tar")))
              (esh-tldr--open "git")
              (should (eq buffer (get-buffer esh-tldr--buffer-name)))
              (with-current-buffer buffer
                (should (equal esh-tldr--command "git"))
                (should (string-match-p "git status" (buffer-string)))))))
      (when-let* ((buffer (get-buffer esh-tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-consult-candidate-display ()
  (let* ((source-buffer (generate-new-buffer "esh-tldr-source"))
         (marker (with-current-buffer source-buffer (point-marker)))
         (example (esh-tldr--make-example
                   :description "[c]reate files in [l]ong format, sorted by [S]ize (descending) recursively"
                   :command "ls -lSR"))
         (page (esh-tldr--make-page :title "ls"
                                :description "List directory contents."
                                :file "/tmp/ls.md"
                                :examples (list example))))
    (unwind-protect
        (let ((candidates (esh-tldr--consult-candidates "ls" page marker)))
          (should (equal (substring-no-properties (car candidates))
                         "Create files in long format, sorted by Size (descending) recursively"))
          (should (equal (plist-get (esh-tldr--consult-data (car candidates)) :example)
                         example)))
      (kill-buffer source-buffer))))

(ert-deftest esh-tldr-open-locates-consult-entry ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common"))
    (esh-tldr-test-page root "pages" "common" "tar" esh-tldr-test-tar-page)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     buffer)))
          (with-temp-buffer
            (esh-tldr--open "tar" 1)
            (with-current-buffer (get-buffer esh-tldr--buffer-name)
              (should (equal (get-text-property (point) 'esh-tldr-example) 1))
              (should (looking-at-p "- List contents")))))
      (when-let* ((buffer (get-buffer esh-tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-consult-embark-action-inserts-template ()
  (let ((source-buffer (generate-new-buffer "esh-tldr-source"))
        (esh-tldr-use-tempel nil))
    (unwind-protect
        (let* ((marker (with-current-buffer source-buffer
                         (insert "run ")
                         (point-marker)))
               (example (esh-tldr--make-example
                         :description "List contents"
                         :command "tar tvf archive.tar"))
               (page (esh-tldr--make-page :title "tar"
                                      :description "Archive utility."
                                      :file "/tmp/tar.md"
                                      :examples (list example)))
               (candidate (car (esh-tldr--consult-candidates "tar" page marker))))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (set-buffer buffer))))
            (esh-tldr-consult-insert-template candidate))
          (with-current-buffer source-buffer
            (should (equal (buffer-string) "run tar tvf archive.tar"))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer)))))

(ert-deftest esh-tldr-consult-empty-selection-opens-command-page ()
  (let* ((root (make-temp-file "esh-tldr" t))
         (esh-tldr-pages-directory root)
         (esh-tldr-language "C")
         (esh-tldr-platform "common")
         (orig-require (symbol-function 'require)))
    (esh-tldr-test-page root "pages" "common" "tar" esh-tldr-test-tar-page)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'consult)
                         t
                       (funcall orig-require feature filename noerror))))
                  ((symbol-function 'consult--read)
                   (lambda (candidates &rest _options)
                     (should (equal (car candidates) ""))
                     (should (null (esh-tldr--consult-lookup "" candidates)))
                     nil))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _)
                     buffer)))
          (with-temp-buffer
            (consult-esh-tldr "tar")
            (with-current-buffer (get-buffer esh-tldr--buffer-name)
              (should (equal esh-tldr--command "tar"))
              (should (= (point) (point-min)))
              (should-not (get-text-property (point) 'esh-tldr-example)))))
      (when-let* ((buffer (get-buffer esh-tldr--buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest esh-tldr-insert-closes-page-buffer ()
  (let ((source-buffer (generate-new-buffer "esh-tldr-source"))
        (page-buffer (get-buffer-create esh-tldr--buffer-name))
        (esh-tldr-use-tempel nil))
    (unwind-protect
        (let (source-marker)
          (with-current-buffer source-buffer
            (insert "run ")
            (setq source-marker (point-marker)))
          (with-current-buffer page-buffer
            (esh-tldr--show-page
             (esh-tldr--make-page
              :title "tar"
              :description "Archive utility."
              :file "/tmp/tar.md"
              :examples (list (esh-tldr--make-example
                               :description "List contents"
                               :command "tar tvf archive.tar")))
             "tar"
             source-marker)
            (goto-char (text-property-any (point-min) (point-max) 'esh-tldr-example 0))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (set-buffer buffer))))
              (esh-tldr-insert-command)))
          (should-not (buffer-live-p page-buffer))
          (with-current-buffer source-buffer
            (should (equal (buffer-string) "run tar tvf archive.tar"))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (when (buffer-live-p page-buffer)
        (kill-buffer page-buffer)))))

(ert-deftest esh-tldr-page-commands-are-mode-specific ()
  (dolist (command '(esh-tldr-copy-command
                     esh-tldr-insert-command
                     esh-tldr-next-example
                     esh-tldr-previous-example
                     esh-tldr-reload))
    (should (equal (get command 'command-modes) '(esh-tldr-mode))))
  (dolist (command '(esh-tldr esh-tldr-at-point esh-tldr-dwim esh-tldr-update consult-esh-tldr))
    (should-not (get command 'command-modes))))

(provide 'esh-tldr-test)

;;; esh-tldr-test.el ends here
