;;; esh-tldr-ghostty-test.el --- Tests for Ghostty integration -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'esh-tldr)

;; A minimal Ghostel test double avoids loading its native module.
(unless (featurep 'ghostel)
  (define-derived-mode ghostel-mode fundamental-mode "Ghostel-Test")
  (defvar-local ghostel--input-mode nil)
  (defvar-local ghostel--line-input-start nil)
  (defvar-local ghostel--line-input-end nil)
  (defun ghostel-input-start-point () nil)
  (defun ghostel-cursor-point () nil)
  (defun ghostel-line-mode (&optional _force) nil)
  (provide 'ghostel))

(require 'esh-tldr-ghostty)

(defun esh-tldr-ghostty-test-buffer (input mode)
  (let ((buffer (generate-new-buffer "esh-tldr-ghostty")))
    (with-current-buffer buffer
      (ghostel-mode)
      (let ((inhibit-read-only t))
        (insert "$ ")
        (let ((start (point)))
          (insert input)
          (put-text-property start (point) 'ghostel-input t)))
      (setq-local ghostel--input-mode mode)
      (setq buffer-read-only (not (eq mode 'line)))
      (when (eq mode 'line)
        (setq-local ghostel--line-input-start (copy-marker 3 nil))
        (setq-local ghostel--line-input-end (copy-marker (point-max) t))))
    buffer))

(defmacro esh-tldr-ghostty-test-with-api (buffer &rest body)
  (declare (indent 1))
  `(let ((ghostel-test-buffer ,buffer))
     (cl-letf (((symbol-function 'ghostel-input-start-point)
                (lambda () 3))
               ((symbol-function 'ghostel-cursor-point)
                (lambda ()
                  (with-current-buffer ghostel-test-buffer (point-max))))
               ((symbol-function 'pop-to-buffer)
                (lambda (target-buffer &rest _)
                  (set-buffer target-buffer))))
       ,@body)))

(ert-deftest esh-tldr-ghostty-registers-separate-adapter-hooks ()
  (should (memq #'esh-tldr-ghostty--context esh-tldr-context-functions))
  (should (memq #'esh-tldr-ghostty--fallback-target
                esh-tldr-fallback-target-functions))
  (esh-tldr-ghostty-teardown)
  (should-not (memq #'esh-tldr-ghostty--context esh-tldr-context-functions))
  (esh-tldr-ghostty-setup)
  (should-not (esh-tldr-ghostty-unload-function))
  (should-not (memq #'esh-tldr-ghostty--context esh-tldr-context-functions))
  (esh-tldr-ghostty-setup))

(ert-deftest esh-tldr-ghostty-semi-char-enters-line-and-replaces ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls -l" 'semi-char)))
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context))
                   switched)
              (should (equal (esh-tldr--context-command context) "ls"))
              (cl-letf (((symbol-function 'ghostel-line-mode)
                         (lambda (&optional _force)
                           (setq switched t
                                 buffer-read-only nil
                                 ghostel--input-mode 'line
                                 ghostel--line-input-start (copy-marker 3 nil)
                                 ghostel--line-input-end
                                 (copy-marker (point-max) t)))))
                (should (esh-tldr--use-command "ls -a" target)))
              (should switched)
              (should (eq ghostel--input-mode 'line))
              (should (equal (buffer-string) "$ ls -a -l")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-line-mode-replaces-without-switching ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "echo ok | ls -l" 'line)))
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (cl-letf (((symbol-function 'ghostel-line-mode)
                         (lambda (&optional _force)
                           (ert-fail "line mode should not be entered twice"))))
                (should (esh-tldr--use-command "ls -a" target)))
              (should (equal (buffer-string) "$ echo ok | ls -a -l")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-line-mode-uses-emacs-point-for-pipelines ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls -l | grep x" 'line)))
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (goto-char 4)
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (should (equal (esh-tldr--context-command context) "ls"))
              (should (equal (esh-tldr--target-original target) "ls"))
              (should (esh-tldr--use-command "ls -a" target))
              (should (equal (buffer-string) "$ ls -a -l | grep x")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-same-input-at-new-tick-falls-back-to-copy ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls" 'semi-char))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (let ((inhibit-read-only t))
                (delete-region 3 (point-max))
                (insert "ls")
                (put-text-property 3 (point-max) 'ghostel-input t))
              (should-not (esh-tldr--use-command "ls -a" target))
              (should (equal (buffer-string) "$ ls"))
              (should (equal (current-kill 0) "ls -a")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-line-mode-entry-failure-falls-back-to-copy ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls" 'semi-char))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (cl-letf (((symbol-function 'ghostel-line-mode)
                         (lambda (&optional _force)
                           (user-error "No prompt"))))
                (should-not (esh-tldr--use-command "ls -a" target)))
              (should (equal (buffer-string) "$ ls"))
              (should (equal (current-kill 0) "ls -a")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-missing-input-region-falls-back-to-copy ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls" 'semi-char))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (cl-letf (((symbol-function 'esh-tldr-ghostty--input-bounds)
                         (lambda () nil)))
                (should-not (esh-tldr--use-command "ls -a" target)))
              (should (equal (buffer-string) "$ ls"))
              (should (equal (current-kill 0) "ls -a")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-changed-input-falls-back-to-copy ()
  (let ((buffer (esh-tldr-ghostty-test-buffer "ls" 'semi-char))
        kill-ring kill-ring-yank-pointer)
    (unwind-protect
        (esh-tldr-ghostty-test-with-api buffer
          (with-current-buffer buffer
            (let* ((context (esh-tldr-ghostty--context))
                   (target (esh-tldr--context-target context)))
              (let ((inhibit-read-only t))
                (delete-region 3 (point-max))
                (goto-char (point-max))
                (insert "pwd")
                (put-text-property 3 (point-max) 'ghostel-input t))
              (should-not (esh-tldr--use-command "ls -a" target))
              (should (equal (buffer-string) "$ pwd"))
              (should (equal (current-kill 0) "ls -a")))))
      (kill-buffer buffer))))

(ert-deftest esh-tldr-ghostty-readonly-modes-fall-back-to-copy ()
  (dolist (mode '(char copy emacs))
    (let ((buffer (esh-tldr-ghostty-test-buffer "ls" 'semi-char))
          kill-ring kill-ring-yank-pointer)
      (unwind-protect
          (esh-tldr-ghostty-test-with-api buffer
            (with-current-buffer buffer
              (let* ((context (esh-tldr-ghostty--context))
                     (target (esh-tldr--context-target context)))
                (setq ghostel--input-mode mode)
                (should-not (esh-tldr--use-command "ls -a" target))
                (should (equal (buffer-string) "$ ls"))
                (should (equal (current-kill 0) "ls -a")))))
        (kill-buffer buffer)))))

(provide 'esh-tldr-ghostty-test)

;;; esh-tldr-ghostty-test.el ends here
