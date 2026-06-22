;;; esh-tldr.el --- Browse local tldr pages -*- lexical-binding: t; -*-

;; Author: roifewu
;; Package-Requires: ((emacs "27.1"))
;; Keywords: help, tools

;;; Commentary:

;; Browse local tldr pages, copy examples, and insert examples as templates.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'subr-x)
(require 'tempo)
(require 'thingatpt)

(declare-function consult--read "consult" (table &rest options))
(declare-function tempel-insert "ext:tempel" (template))
(defvar embark-default-action-overrides)
(defvar embark-keymap-alist)

(defgroup esh-tldr nil
  "Browse local tldr pages."
  :group 'help)

(defcustom esh-tldr-pages-directory (expand-file-name "~/.tldrc/tldr")
  "Root directory of local tldr pages."
  :type 'directory)

(defcustom esh-tldr-language 'auto
  "Language used to read tldr pages.
When set to `auto', use LC_ALL, LC_MESSAGES, then LANG."
  :type '(choice (const auto) string))

(defcustom esh-tldr-platform 'auto
  "Platform used to read tldr pages.
When set to `auto', derive the platform from `system-type'."
  :type '(choice (const auto) string))

(defcustom esh-tldr-executable (executable-find "tldr")
  "External tldr executable used by `esh-tldr-update'."
  :type '(choice (const nil) file))

(defcustom esh-tldr-use-tempel nil
  "When non-nil, use Tempel for example template insertion."
  :type 'boolean)

(defface esh-tldr-title-face
  '((t :inherit font-lock-function-name-face :height 1.4 :weight bold))
  "Face for tldr page titles.")

(defface esh-tldr-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for esh-tldr headings.")

(defface esh-tldr-command-face
  '((t :inherit fixed-pitch :weight bold))
  "Face for esh-tldr example commands.")

(cl-defstruct (esh-tldr--example (:constructor esh-tldr--make-example))
  description command)

(cl-defstruct (esh-tldr--page (:constructor esh-tldr--make-page))
  title description examples file)

(defvar-local esh-tldr--command nil)
(defvar-local esh-tldr--page nil)
(defvar-local esh-tldr--source-marker nil)

(defvar esh-tldr--buffer-name "*esh-tldr*")

(defun esh-tldr--locale-base (locale)
  (let ((locale (string-trim (or locale ""))))
    (unless (member locale '("" "C" "POSIX" "C.UTF-8"))
      (setq locale (replace-regexp-in-string "[.@].*\\'" "" locale))
      (unless (string-empty-p locale)
        locale))))

(defun esh-tldr--language-value ()
  (if (memq esh-tldr-language '(auto nil))
      (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG"))
    (if (symbolp esh-tldr-language)
        (symbol-name esh-tldr-language)
      esh-tldr-language)))

(defun esh-tldr--language-directories (&optional locale)
  "Return ordered esh-tldr language directories for LOCALE."
  (let ((base (esh-tldr--locale-base (or locale (esh-tldr--language-value)))))
    (if (not base)
        '("pages")
      (append (list (concat "pages." base))
              (when (string-match "\\`\\([^_]+\\)_" base)
                (list (concat "pages." (match-string 1 base))))
              '("pages")))))

(defun esh-tldr--platform-name (&optional platform)
  (let ((platform (or platform
                      (if (eq esh-tldr-platform 'auto)
                          system-type
                        esh-tldr-platform))))
    (cond
     ((eq platform 'darwin) "osx")
     ((eq platform 'gnu/linux) "linux")
     ((memq platform '(windows-nt ms-dos cygwin)) "windows")
     ((stringp platform) platform)
     (t "common"))))

(defun esh-tldr--platform-directories (&optional platform)
  "Return ordered esh-tldr platform directories for PLATFORM."
  (let ((name (esh-tldr--platform-name platform)))
    (if (string= name "common")
        '("common")
      (list name "common"))))

(defun esh-tldr--command-from-string (string)
  (let* ((parts (split-string (string-trim (or string ""))
                              "\\(?:&&\\|||\\|[;|]\\)" t "[ \t\n]+"))
         (command (car (last parts)))
         (tokens (and command (split-string command "[ \t\n]+" t))))
    (while (and tokens (string-match-p "\\`[[:alnum:]_]+=" (car tokens)))
      (setq tokens (cdr tokens)))
    (when-let* ((token (car tokens))
                (name (file-name-nondirectory token)))
      (unless (string-empty-p name)
        name))))

(defun esh-tldr--command-at-point ()
  (when-let* ((bounds (or (bounds-of-thing-at-point 'filename)
                          (bounds-of-thing-at-point 'symbol)))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (esh-tldr--command-from-string text)))

(defun esh-tldr--region-command ()
  (when (use-region-p)
    (esh-tldr--command-from-string
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun esh-tldr--shell-input ()
  (when (derived-mode-p 'shell-mode 'comint-mode 'eshell-mode)
    (let ((beg (cond
                ((derived-mode-p 'comint-mode) (comint-line-beginning-position))
                ((and (boundp 'eshell-last-output-end)
                      (markerp eshell-last-output-end))
                 (max (point-min) (marker-position eshell-last-output-end)))
                (t (line-beginning-position)))))
      (buffer-substring-no-properties beg (point)))))

(defun esh-tldr--shell-command ()
  (when-let* ((input (esh-tldr--shell-input)))
    (esh-tldr--command-from-string input)))

(defun esh-tldr--default-command ()
  (or (esh-tldr--region-command)
      (esh-tldr--shell-command)
      (esh-tldr--command-at-point)))

(defun esh-tldr--valid-command-p (command)
  (and (stringp command)
       (not (string-empty-p command))
       (not (string-match-p "/" command))))

(defun esh-tldr--check-command (command)
  (let ((command (esh-tldr--command-from-string command)))
    (unless command
      (user-error "Command name is empty"))
    (unless (esh-tldr--valid-command-p command)
      (user-error "Illegal command name: %s" command))
    command))

(defun esh-tldr--candidate-files (command)
  "Return finite ordered candidate page files for COMMAND."
  (cl-loop for language in (esh-tldr--language-directories)
           append (cl-loop for platform in (esh-tldr--platform-directories)
                           collect (expand-file-name
                                    (format "%s/%s/%s.md" language platform command)
                                    esh-tldr-pages-directory))))

(defun esh-tldr--command-directories ()
  (cl-loop for language in (esh-tldr--language-directories)
           append (cl-loop for platform in (esh-tldr--platform-directories)
                           collect (expand-file-name
                                    (format "%s/%s" language platform)
                                    esh-tldr-pages-directory))))

(defun esh-tldr--command-candidates ()
  "Return tldr command candidates from the active language/platform directories."
  (let (seen commands)
    (dolist (directory (esh-tldr--command-directories))
      (when (file-directory-p directory)
        (dolist (file (directory-files directory nil "\\.md\\'"))
          (let ((command (file-name-sans-extension file)))
            (unless (member command seen)
              (push command seen)
              (push command commands))))))
    (nreverse commands)))

(defun esh-tldr--find-page (command)
  (let ((file (cl-find-if #'file-readable-p (esh-tldr--candidate-files command))))
    (unless file
      (user-error "No tldr page found for %s" command))
    file))

(defun esh-tldr--parse-page (file)
  "Parse a tldr markdown FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (title descriptions pending examples)
      (dolist (line (split-string (buffer-string) "\n"))
        (cond
         ((string-match "\\`# +\\(.+\\)\\'" line)
          (setq title (match-string 1 line)))
         ((string-match "\\`> ?\\(.*\\)\\'" line)
          (push (match-string 1 line) descriptions))
         ((string-match "\\`- +\\(.+\\):[ \t]*\\'" line)
          (setq pending (match-string 1 line)))
         ((and pending (string-match "\\`[ \t]*`\\(.+\\)`[ \t]*\\'" line))
          (push (esh-tldr--make-example :description pending
                                    :command (match-string 1 line))
                examples)
          (setq pending nil))))
      (esh-tldr--make-page :title title
                       :description (string-join (nreverse descriptions) "\n")
                       :examples (nreverse examples)
                       :file file))))

(defun esh-tldr--read-page (command)
  (esh-tldr--parse-page (esh-tldr--find-page command)))

(defun esh-tldr--slug (description)
  (let ((slug (downcase description)))
    (setq slug (replace-regexp-in-string "[][()]" "" slug))
    (setq slug (replace-regexp-in-string "[,.][ \t\n]*" "_" slug))
    (setq slug (replace-regexp-in-string "[^[:alnum:]_ -]" "" slug))
    (setq slug (replace-regexp-in-string "[ \t\n]+" "-" slug))
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    (setq slug (replace-regexp-in-string "_+" "_" slug))
    (string-trim slug "[-_]+" "[-_]+")))

(defun esh-tldr--template-name (command description)
  (concat command "/" (esh-tldr--slug description)))

(defun esh-tldr--plain-description (description)
  (let ((description (replace-regexp-in-string "[][]" "" description)))
    (if (string-match "[[:alpha:]]" description)
        (replace-match (upcase (match-string 0 description)) nil nil description)
      description)))

(defun esh-tldr--template-elements (command)
  (let ((start 0)
        (index 0)
        names
        elements)
    (while (string-match "{{\\([^{}]+\\)}}" command start)
      (let ((literal (substring command start (match-beginning 0)))
            (name (string-trim (match-string 1 command))))
        (unless (string-empty-p literal)
          (push literal elements))
        (if-let* ((cell (assoc name names)))
            (push `(s ,(cdr cell)) elements)
          (setq index (1+ index))
          (let ((symbol (intern (format "esh-tldr-%d" index))))
            (push (cons name symbol) names)
            (push `(p ,name ,symbol) elements))))
      (setq start (match-end 0)))
    (let ((tail (substring command start)))
      (unless (string-empty-p tail)
        (push tail elements)))
    (nreverse elements)))

(defun esh-tldr--insert-template (command)
  (let ((template (esh-tldr--template-elements command)))
    (if esh-tldr-use-tempel
        (progn
          (unless (require 'tempel nil t)
            (user-error "Tempel is not available"))
          (tempel-insert template))
      (let ((tempo-interactive t)
            (template-symbol (make-symbol "esh-tldr-tempo-template")))
        (set template-symbol template)
        (tempo-insert-template template-symbol nil)))))

(defvar esh-tldr-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "y") #'esh-tldr-copy-command)
    (define-key map (kbd "RET") #'esh-tldr-insert-command)
    (define-key map (kbd "e") #'esh-tldr-insert-command)
    (define-key map (kbd "g") #'esh-tldr-reload)
    (define-key map (kbd "n") #'esh-tldr-next-example)
    (define-key map (kbd "p") #'esh-tldr-previous-example)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    map)
  "Keymap for `esh-tldr-mode'.")

;;;###autoload
(define-derived-mode esh-tldr-mode special-mode "esh-tldr"
  "Major mode for tldr pages.")

(defun esh-tldr--render-page (page)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (esh-tldr--page-title page) 'face 'esh-tldr-title-face)
            "\n\n")
    (let ((description (esh-tldr--page-description page)))
      (unless (string-empty-p description)
        (insert description "\n\n")))
    (insert (propertize "Source: " 'face 'esh-tldr-heading-face)
            (abbreviate-file-name (esh-tldr--page-file page))
            "\n\n")
    (cl-loop for example in (esh-tldr--page-examples page)
             for index from 0
             do (let ((start (point)))
                  (insert (propertize (concat "- " (esh-tldr--example-description example) "\n")
                                      'face 'esh-tldr-heading-face))
                  (insert (propertize (esh-tldr--example-command example)
                                      'face 'esh-tldr-command-face))
                  (insert "\n\n")
                  (add-text-properties start (point)
                                       `(esh-tldr-example ,index))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun esh-tldr--show-page (page command source-marker)
  (esh-tldr-mode)
  (setq esh-tldr--command command
        esh-tldr--page page
        esh-tldr--source-marker source-marker)
  (esh-tldr--render-page page))

(defun esh-tldr--open (command &optional example-index source-marker)
  (let* ((page (esh-tldr--read-page command))
         (buffer (get-buffer-create esh-tldr--buffer-name))
         (source-marker (or source-marker (point-marker))))
    (with-current-buffer buffer
      (esh-tldr--show-page page command source-marker)
      (when example-index
        (esh-tldr--goto-example example-index)))
    (pop-to-buffer buffer)))

(defun esh-tldr--read-command ()
  (let* ((default (esh-tldr--default-command))
         (prompt (if default
                     (format "esh-tldr command (default %s): " default)
                   "esh-tldr command: "))
         (candidates (esh-tldr--command-candidates)))
    (esh-tldr--check-command
     (if (require 'consult nil t)
         (consult--read candidates
                        :prompt prompt
                        :category 'esh-tldr-command
                        :default default
                        :require-match nil
                        :sort nil)
       (completing-read prompt candidates nil nil nil nil default)))))

;;;###autoload
(defun esh-tldr (command)
  "Open the local tldr page for COMMAND."
  (interactive (list (esh-tldr--read-command)))
  (esh-tldr--open command))

;;;###autoload
(defun esh-tldr-at-point ()
  "Open the local tldr page for the command at point."
  (interactive)
  (let ((command (esh-tldr--command-at-point)))
    (unless command
      (user-error "No command at point"))
    (esh-tldr--open command)))

;;;###autoload
(defun esh-tldr-dwim ()
  "Open a tldr page using region, shell input, point, or minibuffer input."
  (interactive)
  (if-let* ((command (esh-tldr--default-command)))
      (esh-tldr--open command)
    (call-interactively #'esh-tldr)))

(defun esh-tldr--current-example-index ()
  (or (get-text-property (point) 'esh-tldr-example)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'esh-tldr-example))))

(defun esh-tldr--current-example ()
  (let ((index (esh-tldr--current-example-index)))
    (unless index
      (user-error "No esh-tldr example at point"))
    (nth index (esh-tldr--page-examples esh-tldr--page))))

(defun esh-tldr-copy-command ()
  "Copy the current esh-tldr example command."
  (interactive)
  (let ((command (esh-tldr--example-command (esh-tldr--current-example))))
    (kill-new command)
    (message "Copied: %s" command)))

(defun esh-tldr--ensure-marker-buffer (marker)
  (unless (and marker
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
    (user-error "Source buffer no longer exists"))
  (let ((buffer (marker-buffer marker)))
    (when (buffer-local-value 'buffer-read-only buffer)
      (user-error "Source buffer is read-only"))
    buffer))

(defun esh-tldr--insert-template-at-marker (command marker)
  (let ((buffer (esh-tldr--ensure-marker-buffer marker)))
    (pop-to-buffer buffer)
    (goto-char marker)
    (esh-tldr--insert-template command)))

(defun esh-tldr--close-page-buffer (buffer window)
  (when (and (window-live-p window)
             (eq (window-buffer window) buffer))
    (quit-window 'kill window))
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun esh-tldr-insert-command ()
  "Insert the current esh-tldr example command as a template."
  (interactive)
  (let ((command (esh-tldr--example-command (esh-tldr--current-example)))
        (page-buffer (current-buffer))
        (page-window (get-buffer-window (current-buffer) t))
        (source-marker esh-tldr--source-marker))
    (esh-tldr--insert-template-at-marker command source-marker)
    (esh-tldr--close-page-buffer page-buffer page-window)))

(defun esh-tldr--consult-display (command example)
  (ignore command)
  (esh-tldr--plain-description (esh-tldr--example-description example)))

(defun esh-tldr--consult-candidates (command page source-marker)
  (cl-loop for example in (esh-tldr--page-examples page)
           for index from 0
           for display = (esh-tldr--consult-display command example)
           for data = (list :command command
                            :index index
                            :example example
                            :source-marker source-marker)
           collect (propertize display 'esh-tldr--consult-data data)))

(defun esh-tldr--consult-data (candidate)
  (get-text-property 0 'esh-tldr--consult-data candidate))

(defun esh-tldr--consult-lookup (selected candidates &rest _)
  (unless (string-empty-p selected)
    (if-let* ((candidate (car (member selected candidates))))
        (esh-tldr--consult-data candidate)
      (esh-tldr--consult-data selected))))

;;;###autoload
(defun consult-esh-tldr (command)
  "Select an esh-tldr example for COMMAND with Consult.
An empty selection opens the command page without jumping to an example."
  (interactive
   (list (if current-prefix-arg
             (esh-tldr--read-command)
           (or (esh-tldr--default-command)
               (esh-tldr--read-command)))))
  (unless (require 'consult nil t)
    (user-error "Consult is not available"))
  (let* ((command (esh-tldr--check-command command))
         (page (esh-tldr--read-page command))
         (source-marker (point-marker))
         (candidates (cons "" (esh-tldr--consult-candidates command page source-marker))))
    (let ((data (consult--read candidates
                               :prompt (format "esh-tldr %s: " command)
                               :category 'esh-tldr-example
                               :require-match t
                               :sort nil
                               :lookup #'esh-tldr--consult-lookup)))
      (if data
          (esh-tldr--open (plist-get data :command)
                      (plist-get data :index)
                      (plist-get data :source-marker))
        (esh-tldr--open command nil source-marker)))))

(defun esh-tldr-consult-insert-template (candidate)
  "Insert the template for a `consult-esh-tldr' CANDIDATE.
CANDIDATE must be the propertized string returned by
`esh-tldr--consult-candidates'; intended to be invoked via Embark."
  (let* ((data (esh-tldr--consult-data candidate))
         (example (plist-get data :example)))
    (esh-tldr--insert-template-at-marker
     (esh-tldr--example-command example)
     (plist-get data :source-marker))))

(defvar esh-tldr-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'esh-tldr-consult-insert-template)
    map)
  "Embark actions for `consult-esh-tldr' candidates.")

(with-eval-after-load 'embark
  (setf (alist-get 'esh-tldr-example embark-keymap-alist) 'esh-tldr-embark-map)
  (setf (alist-get 'esh-tldr-example embark-default-action-overrides)
        #'esh-tldr-consult-insert-template))

(defun esh-tldr--goto-example (index)
  (let ((pos (text-property-any (point-min) (point-max) 'esh-tldr-example index)))
    (unless pos
      (user-error "No esh-tldr example"))
    (goto-char pos)
    (beginning-of-line)))

(defun esh-tldr-next-example ()
  "Move to the next esh-tldr example."
  (interactive)
  (let* ((examples (esh-tldr--page-examples esh-tldr--page))
         (index (esh-tldr--current-example-index))
         (next (if index (1+ index) 0)))
    (when (>= next (length examples))
      (user-error "No next esh-tldr example"))
    (esh-tldr--goto-example next)))

(defun esh-tldr-previous-example ()
  "Move to the previous esh-tldr example."
  (interactive)
  (let* ((index (esh-tldr--current-example-index))
         (previous (if index (1- index) -1)))
    (when (< previous 0)
      (user-error "No previous esh-tldr example"))
    (esh-tldr--goto-example previous)))

(defun esh-tldr-reload ()
  "Reload the current tldr page."
  (interactive)
  (unless esh-tldr--command
    (user-error "No tldr page in this buffer"))
  (let ((page (esh-tldr--read-page esh-tldr--command)))
    (setq esh-tldr--page page)
    (esh-tldr--render-page page)
    (message "Reloaded %s" esh-tldr--command)))

(defun esh-tldr--refresh-open-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'esh-tldr-mode)
        (esh-tldr-reload)))))

;;;###autoload
(defun esh-tldr-update ()
  "Run tldr --update asynchronously."
  (interactive)
  (unless (and esh-tldr-executable (executable-find esh-tldr-executable))
    (user-error "Could not find tldr executable"))
  (let ((buffer (get-buffer-create "*esh-tldr update*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (make-process
     :name "esh-tldr-update"
     :buffer buffer
     :command (list esh-tldr-executable "--update")
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((status (process-exit-status process))
               (buffer (process-buffer process)))
           (if (zerop status)
               (progn
                 (esh-tldr--refresh-open-buffers)
                 (message "tldr pages updated"))
             (message "tldr update failed with status %s; see %s"
                      status (buffer-name buffer)))))))))

(defun esh-tldr--capf-bounds ()
  (let ((beg (save-excursion
               (skip-chars-backward "[:alnum:]_+@:%.,=/-")
               (point)))
        (end (save-excursion
               (skip-chars-forward "[:alnum:]_+@:%.,=/-")
               (point))))
    (cons beg end)))

(defun esh-tldr--capf-context ()
  (let* ((bounds (esh-tldr--capf-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (token (buffer-substring-no-properties beg end))
         shell)
    (cond
     ((string-match "\\`\\([^/]+\\)/" token)
      (list (match-string 1 token) beg end))
     ((setq shell (esh-tldr--shell-command))
      (list shell beg end))
     ((not (string-empty-p token))
      (list (esh-tldr--command-from-string token) beg end)))))

;;;###autoload
(defun capf-esh-tldr ()
  "Complete esh-tldr examples at point."
  (when-let* ((context (esh-tldr--capf-context))
              (command (nth 0 context))
              (beg (nth 1 context))
              (end (nth 2 context))
              (examples (ignore-errors
                          (esh-tldr--page-examples (esh-tldr--read-page command)))))
    (let* ((table (mapcar (lambda (example)
                            (cons (esh-tldr--template-name command
                                                       (esh-tldr--example-description example))
                                  (esh-tldr--example-command example)))
                          examples))
           (candidates (mapcar #'car table)))
      (when candidates
        (list beg end candidates
              :annotation-function
              (lambda (candidate)
                (concat "  " (cdr (assoc candidate table))))
              :exit-function
              (lambda (candidate status)
                (when (eq status 'finished)
                  (let ((start (- (point) (length candidate)))
                        (command (cdr (assoc candidate table))))
                    (delete-region start (point))
                    (esh-tldr--insert-template command)))))))))

;;;###autoload
(defun esh-tldr-capf-setup ()
  "Add `capf-esh-tldr' to `completion-at-point-functions' in the current buffer."
  (interactive)
  (add-hook 'completion-at-point-functions #'capf-esh-tldr nil t))

(dolist (command '(esh-tldr-copy-command
                   esh-tldr-insert-command
                   esh-tldr-next-example
                   esh-tldr-previous-example
                   esh-tldr-reload))
  (function-put command 'command-modes '(esh-tldr-mode)))

(provide 'esh-tldr)

;;; esh-tldr.el ends here
