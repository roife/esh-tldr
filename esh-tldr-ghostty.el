;;; esh-tldr-ghostty.el --- Ghostty support for esh-tldr -*- lexical-binding: t; -*-

;; Author: roifewu
;; Package-Requires: ((emacs "31.0"))
;; Keywords: help, tools, terminals

;;; Commentary:

;; Optional Ghostty terminal integration for `esh-tldr', implemented through
;; Ghostel.  Requiring this file registers a context provider and a safe target
;; handler.  Loading this adapter requires Ghostel.

;;; Code:

(require 'esh-tldr)
(require 'ghostel)

(defun esh-tldr-ghostty--buffer-p ()
  (derived-mode-p 'ghostel-mode))

(defun esh-tldr-ghostty--line-input-bounds ()
  "Return the editable Ghostel line-mode input bounds, or nil."
  (when (eq ghostel--input-mode 'line)
    (when-let* ((start (and (markerp ghostel--line-input-start)
                            (marker-position ghostel--line-input-start)))
                (end (and (markerp ghostel--line-input-end)
                          (marker-position ghostel--line-input-end))))
      (cons start end))))

(defun esh-tldr-ghostty--input-bounds ()
  "Return the current Ghostel input bounds, or nil."
  (or (esh-tldr-ghostty--line-input-bounds)
      (when-let* ((start (ghostel-input-start-point))
                  (cursor (ghostel-cursor-point)))
        (let* ((limit (save-excursion
                        (goto-char cursor)
                        (line-end-position)))
               (input-pos (text-property-any start limit 'ghostel-input t))
               (property-end
                (and input-pos
                     (or (next-single-property-change
                          input-pos 'ghostel-input nil limit)
                         limit))))
          (cons start (max start cursor (or property-end start)))))))

(defun esh-tldr-ghostty--cursor (bounds)
  "Return the active editing position within Ghostel input BOUNDS."
  (max (car bounds)
       (min (or (and (eq ghostel--input-mode 'line)
                     (point))
                (ghostel-cursor-point)
                (cdr bounds))
            (cdr bounds))))

(defun esh-tldr-ghostty--target (beg end input-beg input-end)
  "Create a Ghostty target from BEG and END within the complete input."
  (esh-tldr--make-target
   :buffer (current-buffer)
   :start (copy-marker beg nil)
   :end (copy-marker end t)
   :original (buffer-substring-no-properties beg end)
   :handler #'esh-tldr-ghostty--replace-target
   :data (list :input-original
               (buffer-substring-no-properties input-beg input-end)
               :start-offset (- beg input-beg)
               :end-offset (- end input-beg)
               :buffer-tick (buffer-chars-modified-tick))))

(defun esh-tldr-ghostty--context ()
  "Return the current Ghostty command context, or nil."
  (when-let* (((esh-tldr-ghostty--buffer-p))
              (bounds (esh-tldr-ghostty--input-bounds))
              (token (esh-tldr--command-token-in-buffer
                      (car bounds) (cdr bounds)
                      (esh-tldr-ghostty--cursor bounds))))
    (esh-tldr--make-context
     :command (car token)
     :target (esh-tldr-ghostty--target
              (nth 1 token) (nth 2 token) (car bounds) (cdr bounds)))))

(defun esh-tldr-ghostty--fallback-target ()
  "Return a zero-width Ghostty target when in a Ghostel buffer."
  (when-let* (((esh-tldr-ghostty--buffer-p))
              (bounds (esh-tldr-ghostty--input-bounds)))
    (let ((position (esh-tldr-ghostty--cursor bounds)))
      (esh-tldr-ghostty--target
       position position (car bounds) (cdr bounds)))))

(defun esh-tldr-ghostty--replace-target (command target)
  "Replace Ghostty TARGET with COMMAND, entering line mode if necessary."
  (unless (buffer-live-p (esh-tldr--target-buffer target))
    (esh-tldr--unsafe "Ghostel source buffer no longer exists"))
  (with-current-buffer (esh-tldr--target-buffer target)
    (unless (esh-tldr-ghostty--buffer-p)
      (esh-tldr--unsafe "Source is no longer a Ghostel buffer"))
    (unless (memq ghostel--input-mode '(semi-char line))
      (esh-tldr--unsafe "Ghostel is not in semi-char or line mode"))
    (let* ((data (esh-tldr--target-data target))
           (input-original (plist-get data :input-original))
           (start-offset (plist-get data :start-offset))
           (end-offset (plist-get data :end-offset))
           (buffer-tick (plist-get data :buffer-tick))
           (bounds (esh-tldr-ghostty--input-bounds)))
      (unless bounds
        (esh-tldr--unsafe "Ghostel has no current input region"))
      (unless (= buffer-tick (buffer-chars-modified-tick))
        (esh-tldr--unsafe "Ghostel changed while the page was open"))
      (unless (equal (buffer-substring-no-properties
                      (car bounds) (cdr bounds))
                     input-original)
        (esh-tldr--unsafe "Ghostel input changed while the page was open"))
      (when (eq ghostel--input-mode 'semi-char)
        (condition-case err
            (ghostel-line-mode)
          (user-error
           (esh-tldr--unsafe "%s" (error-message-string err))))
        (unless (eq ghostel--input-mode 'line)
          (esh-tldr--unsafe "Ghostel could not enter line mode at this prompt")))
      (let ((bounds (esh-tldr-ghostty--line-input-bounds)))
        (unless bounds
          (esh-tldr--unsafe "Ghostel has no editable input region"))
        (pcase-let* ((`(,input-beg . ,input-end) bounds)
                     (input (buffer-substring-no-properties
                             input-beg input-end)))
          (unless (equal input input-original)
            (esh-tldr--unsafe "Ghostel input changed while entering line mode"))
          (let ((beg (+ input-beg start-offset))
                (end (+ input-beg end-offset)))
            (unless (and (<= input-beg beg)
                         (<= beg end)
                         (<= end input-end))
              (esh-tldr--unsafe "Saved Ghostel input range is no longer valid"))
            (esh-tldr--replace-buffer-target
             command
             (esh-tldr--make-target
              :buffer (current-buffer)
              :start (copy-marker beg nil)
              :end (copy-marker end t)
              :original (esh-tldr--target-original target)))))))))

;;;###autoload
(defun esh-tldr-ghostty-setup ()
  "Enable Ghostty support for `esh-tldr'."
  (interactive)
  (add-hook 'esh-tldr-context-functions #'esh-tldr-ghostty--context)
  (add-hook 'esh-tldr-fallback-target-functions
            #'esh-tldr-ghostty--fallback-target))

;;;###autoload
(defun esh-tldr-ghostty-teardown ()
  "Disable Ghostty support for `esh-tldr'."
  (interactive)
  (remove-hook 'esh-tldr-context-functions #'esh-tldr-ghostty--context)
  (remove-hook 'esh-tldr-fallback-target-functions
               #'esh-tldr-ghostty--fallback-target))

(esh-tldr-ghostty-setup)

(defun esh-tldr-ghostty-unload-function ()
  "Remove Ghostty integration hooks before unloading this feature."
  (esh-tldr-ghostty-teardown)
  nil)

(provide 'esh-tldr-ghostty)

;;; esh-tldr-ghostty.el ends here
