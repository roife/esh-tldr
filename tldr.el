;;; tldr.el --- Browse local tldr pages -*- lexical-binding: t; -*-

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

(defgroup tldr nil
  "Browse local tldr pages."
  :group 'help)

(defcustom tldr-pages-directory (expand-file-name "~/.tldrc/tldr")
  "Root directory of local tldr pages."
  :type 'directory)

(defcustom tldr-language 'auto
  "Language used to read tldr pages.
When set to `auto', use LC_ALL, LC_MESSAGES, then LANG."
  :type '(choice (const auto) string))

(defcustom tldr-platform 'auto
  "Platform used to read tldr pages.
When set to `auto', derive the platform from `system-type'."
  :type '(choice (const auto) string))

(defcustom tldr-executable (executable-find "tldr")
  "External tldr executable used by `tldr-update'."
  :type '(choice (const nil) file))

(defcustom tldr-use-tempel nil
  "When non-nil, use Tempel for example template insertion."
  :type 'boolean)

(defface tldr-title-face
  '((t :inherit font-lock-function-name-face :height 1.4 :weight bold))
  "Face for tldr page titles.")

(defface tldr-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for tldr headings.")

(defface tldr-command-face
  '((t :inherit fixed-pitch :weight bold))
  "Face for tldr example commands.")

(cl-defstruct (tldr--example (:constructor tldr--make-example))
  description command)

(cl-defstruct (tldr--page (:constructor tldr--make-page))
  title description examples file)

(defvar-local tldr--command nil)
(defvar-local tldr--page nil)
(defvar-local tldr--source-marker nil)

(defvar tldr--buffer-name "*tldr*")

(defun tldr--locale-base (locale)
  (let ((locale (string-trim (or locale ""))))
    (unless (member locale '("" "C" "POSIX" "C.UTF-8"))
      (setq locale (replace-regexp-in-string "[.@].*\\'" "" locale))
      (unless (string-empty-p locale)
        locale))))

(defun tldr--language-value ()
  (if (memq tldr-language '(auto nil))
      (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG"))
    (if (symbolp tldr-language)
        (symbol-name tldr-language)
      tldr-language)))

(defun tldr--language-directories (&optional locale)
  "Return ordered tldr language directories for LOCALE."
  (let ((base (tldr--locale-base (or locale (tldr--language-value)))))
    (if (not base)
        '("pages")
      (append (list (concat "pages." base))
              (when (string-match "\\`\\([^_]+\\)_" base)
                (list (concat "pages." (match-string 1 base))))
              '("pages")))))

(defun tldr--platform-name (&optional platform)
  (let ((platform (or platform
                      (if (eq tldr-platform 'auto)
                          system-type
                        tldr-platform))))
    (cond
     ((eq platform 'darwin) "osx")
     ((eq platform 'gnu/linux) "linux")
     ((memq platform '(windows-nt ms-dos cygwin)) "windows")
     ((stringp platform) platform)
     (t "common"))))

(defun tldr--platform-directories (&optional platform)
  "Return ordered tldr platform directories for PLATFORM."
  (let ((name (tldr--platform-name platform)))
    (if (string= name "common")
        '("common")
      (list name "common"))))

(defun tldr--command-from-string (string)
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

(defun tldr--command-at-point ()
  (when-let* ((bounds (or (bounds-of-thing-at-point 'filename)
                          (bounds-of-thing-at-point 'symbol)))
              (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (tldr--command-from-string text)))

(defun tldr--region-command ()
  (when (use-region-p)
    (tldr--command-from-string
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun tldr--shell-input ()
  (when (derived-mode-p 'shell-mode 'comint-mode 'eshell-mode)
    (let ((beg (cond
                ((derived-mode-p 'comint-mode) (comint-line-beginning-position))
                ((and (boundp 'eshell-last-output-end)
                      (markerp eshell-last-output-end))
                 (max (point-min) (marker-position eshell-last-output-end)))
                (t (line-beginning-position)))))
      (buffer-substring-no-properties beg (point)))))

(defun tldr--shell-command ()
  (when-let* ((input (tldr--shell-input)))
    (tldr--command-from-string input)))

(defun tldr--default-command ()
  (or (tldr--region-command)
      (tldr--shell-command)
      (tldr--command-at-point)))

(defun tldr--valid-command-p (command)
  (and (stringp command)
       (not (string-empty-p command))
       (not (string-match-p "/" command))))

(defun tldr--check-command (command)
  (let ((command (tldr--command-from-string command)))
    (unless command
      (user-error "Command name is empty"))
    (unless (tldr--valid-command-p command)
      (user-error "Illegal command name: %s" command))
    command))

(defun tldr--candidate-files (command)
  "Return finite ordered candidate page files for COMMAND."
  (cl-loop for language in (tldr--language-directories)
           append (cl-loop for platform in (tldr--platform-directories)
                           collect (expand-file-name
                                    (format "%s/%s/%s.md" language platform command)
                                    tldr-pages-directory))))

(defun tldr--command-directories ()
  (cl-loop for language in (tldr--language-directories)
           append (cl-loop for platform in (tldr--platform-directories)
                           collect (expand-file-name
                                    (format "%s/%s" language platform)
                                    tldr-pages-directory))))

(defun tldr--command-candidates ()
  "Return tldr command candidates from the active language/platform directories."
  (let (seen commands)
    (dolist (directory (tldr--command-directories))
      (when (file-directory-p directory)
        (dolist (file (directory-files directory nil "\\.md\\'"))
          (let ((command (file-name-sans-extension file)))
            (unless (member command seen)
              (push command seen)
              (push command commands))))))
    (nreverse commands)))

(defun tldr--find-page (command)
  (let ((file (cl-find-if #'file-readable-p (tldr--candidate-files command))))
    (unless file
      (user-error "No tldr page found for %s" command))
    file))

(defun tldr--parse-page (file)
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
          (push (tldr--make-example :description pending
                                    :command (match-string 1 line))
                examples)
          (setq pending nil))))
      (tldr--make-page :title title
                       :description (string-join (nreverse descriptions) "\n")
                       :examples (nreverse examples)
                       :file file))))

(defun tldr--read-page (command)
  (tldr--parse-page (tldr--find-page command)))

(defun tldr--slug (description)
  (let ((slug (downcase description)))
    (setq slug (replace-regexp-in-string "[][()]" "" slug))
    (setq slug (replace-regexp-in-string "[,.][ \t\n]*" "_" slug))
    (setq slug (replace-regexp-in-string "[^[:alnum:]_ -]" "" slug))
    (setq slug (replace-regexp-in-string "[ \t\n]+" "-" slug))
    (setq slug (replace-regexp-in-string "-+" "-" slug))
    (setq slug (replace-regexp-in-string "_+" "_" slug))
    (string-trim slug "[-_]+" "[-_]+")))

(defun tldr--template-name (command description)
  (concat command "/" (tldr--slug description)))

(defun tldr--plain-description (description)
  (let ((description (replace-regexp-in-string "[][]" "" description)))
    (if (string-match "[[:alpha:]]" description)
        (replace-match (upcase (match-string 0 description)) nil nil description)
      description)))

(defun tldr--template-elements (command)
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
          (let ((symbol (intern (format "tldr-%d" index))))
            (push (cons name symbol) names)
            (push `(p ,name ,symbol) elements))))
      (setq start (match-end 0)))
    (let ((tail (substring command start)))
      (unless (string-empty-p tail)
        (push tail elements)))
    (nreverse elements)))

(defun tldr--insert-template (command)
  (let ((template (tldr--template-elements command)))
    (if tldr-use-tempel
        (progn
          (unless (require 'tempel nil t)
            (user-error "Tempel is not available"))
          (tempel-insert template))
      (let ((tempo-interactive t)
            (template-symbol (make-symbol "tldr-tempo-template")))
        (set template-symbol template)
        (tempo-insert-template template-symbol nil)))))

(defvar tldr-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "y") #'tldr-copy-command)
    (define-key map (kbd "RET") #'tldr-insert-command)
    (define-key map (kbd "e") #'tldr-insert-command)
    (define-key map (kbd "g") #'tldr-reload)
    (define-key map (kbd "n") #'tldr-next-example)
    (define-key map (kbd "p") #'tldr-previous-example)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    map)
  "Keymap for `tldr-mode'.")

;;;###autoload
(define-derived-mode tldr-mode special-mode "tldr"
  "Major mode for tldr pages.")

(defun tldr--render-page (page)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (tldr--page-title page) 'face 'tldr-title-face)
            "\n\n")
    (let ((description (tldr--page-description page)))
      (unless (string-empty-p description)
        (insert description "\n\n")))
    (insert (propertize "Source: " 'face 'tldr-heading-face)
            (abbreviate-file-name (tldr--page-file page))
            "\n\n")
    (cl-loop for example in (tldr--page-examples page)
             for index from 0
             do (let ((start (point)))
                  (insert (propertize (concat "- " (tldr--example-description example) "\n")
                                      'face 'tldr-heading-face))
                  (insert (propertize (tldr--example-command example)
                                      'face 'tldr-command-face))
                  (insert "\n\n")
                  (add-text-properties start (point)
                                       `(tldr-example ,index))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun tldr--show-page (page command source-marker)
  (tldr-mode)
  (setq tldr--command command
        tldr--page page
        tldr--source-marker source-marker)
  (tldr--render-page page))

(defun tldr--open (command &optional example-index source-marker)
  (let* ((page (tldr--read-page command))
         (buffer (get-buffer-create tldr--buffer-name))
         (source-marker (or source-marker (point-marker))))
    (with-current-buffer buffer
      (tldr--show-page page command source-marker)
      (when example-index
        (tldr--goto-example example-index)))
    (pop-to-buffer buffer)))

(defun tldr--read-command ()
  (let* ((default (tldr--default-command))
         (prompt (if default
                     (format "tldr command (default %s): " default)
                   "tldr command: "))
         (candidates (tldr--command-candidates)))
    (tldr--check-command
     (if (require 'consult nil t)
         (consult--read candidates
                        :prompt prompt
                        :category 'tldr-command
                        :default default
                        :require-match nil
                        :sort nil)
       (completing-read prompt candidates nil nil nil nil default)))))

;;;###autoload
(defun tldr (command)
  "Open the local tldr page for COMMAND."
  (interactive (list (tldr--read-command)))
  (tldr--open command))

;;;###autoload
(defun tldr-at-point ()
  "Open the local tldr page for the command at point."
  (interactive)
  (let ((command (tldr--command-at-point)))
    (unless command
      (user-error "No command at point"))
    (tldr--open command)))

;;;###autoload
(defun tldr-dwim ()
  "Open a tldr page using region, shell input, point, or minibuffer input."
  (interactive)
  (if-let* ((command (tldr--default-command)))
      (tldr--open command)
    (call-interactively #'tldr)))

(defun tldr--current-example-index ()
  (or (get-text-property (point) 'tldr-example)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'tldr-example))))

(defun tldr--current-example ()
  (let ((index (tldr--current-example-index)))
    (unless index
      (user-error "No tldr example at point"))
    (nth index (tldr--page-examples tldr--page))))

(defun tldr-copy-command ()
  "Copy the current tldr example command."
  (interactive)
  (let ((command (tldr--example-command (tldr--current-example))))
    (kill-new command)
    (message "Copied: %s" command)))

(defun tldr--ensure-marker-buffer (marker)
  (unless (and marker
               (marker-buffer marker)
               (buffer-live-p (marker-buffer marker)))
    (user-error "Source buffer no longer exists"))
  (let ((buffer (marker-buffer marker)))
    (when (buffer-local-value 'buffer-read-only buffer)
      (user-error "Source buffer is read-only"))
    buffer))

(defun tldr--insert-template-at-marker (command marker)
  (let ((buffer (tldr--ensure-marker-buffer marker)))
    (pop-to-buffer buffer)
    (goto-char marker)
    (tldr--insert-template command)))

(defun tldr--close-page-buffer (buffer window)
  (when (and (window-live-p window)
             (eq (window-buffer window) buffer))
    (quit-window 'kill window))
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(defun tldr-insert-command ()
  "Insert the current tldr example command as a template."
  (interactive)
  (let ((command (tldr--example-command (tldr--current-example)))
        (page-buffer (current-buffer))
        (page-window (get-buffer-window (current-buffer) t))
        (source-marker tldr--source-marker))
    (tldr--insert-template-at-marker command source-marker)
    (tldr--close-page-buffer page-buffer page-window)))

(defun tldr--consult-display (command example)
  (ignore command)
  (tldr--plain-description (tldr--example-description example)))

(defun tldr--consult-candidates (command page source-marker)
  (cl-loop for example in (tldr--page-examples page)
           for index from 0
           for display = (tldr--consult-display command example)
           for data = (list :command command
                            :index index
                            :example example
                            :source-marker source-marker)
           collect (propertize display 'tldr--consult-data data)))

(defun tldr--consult-data (candidate)
  (get-text-property 0 'tldr--consult-data candidate))

(defun tldr--consult-lookup (selected candidates &rest _)
  (unless (string-empty-p selected)
    (if-let* ((candidate (car (member selected candidates))))
        (tldr--consult-data candidate)
      (tldr--consult-data selected))))

;;;###autoload
(defun consult-tldr (command)
  "Select a tldr example for COMMAND with Consult.
An empty selection opens the command page without jumping to an example."
  (interactive
   (list (if current-prefix-arg
             (tldr--read-command)
           (or (tldr--default-command)
               (tldr--read-command)))))
  (unless (require 'consult nil t)
    (user-error "Consult is not available"))
  (let* ((command (tldr--check-command command))
         (page (tldr--read-page command))
         (source-marker (point-marker))
         (candidates (cons "" (tldr--consult-candidates command page source-marker))))
    (let ((data (consult--read candidates
                               :prompt (format "tldr %s: " command)
                               :category 'tldr-example
                               :require-match t
                               :sort nil
                               :lookup #'tldr--consult-lookup)))
      (if data
          (tldr--open (plist-get data :command)
                      (plist-get data :index)
                      (plist-get data :source-marker))
        (tldr--open command nil source-marker)))))

(defun tldr-consult-insert-template (candidate)
  "Insert the template for a `consult-tldr' CANDIDATE.
CANDIDATE must be the propertized string returned by
`tldr--consult-candidates'; intended to be invoked via Embark."
  (let* ((data (tldr--consult-data candidate))
         (example (plist-get data :example)))
    (tldr--insert-template-at-marker
     (tldr--example-command example)
     (plist-get data :source-marker))))

(defvar tldr-embark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'tldr-consult-insert-template)
    map)
  "Embark actions for `consult-tldr' candidates.")

(with-eval-after-load 'embark
  (setf (alist-get 'tldr-example embark-keymap-alist) 'tldr-embark-map)
  (setf (alist-get 'tldr-example embark-default-action-overrides)
        #'tldr-consult-insert-template))

(defun tldr--goto-example (index)
  (let ((pos (text-property-any (point-min) (point-max) 'tldr-example index)))
    (unless pos
      (user-error "No tldr example"))
    (goto-char pos)
    (beginning-of-line)))

(defun tldr-next-example ()
  "Move to the next tldr example."
  (interactive)
  (let* ((examples (tldr--page-examples tldr--page))
         (index (tldr--current-example-index))
         (next (if index (1+ index) 0)))
    (when (>= next (length examples))
      (user-error "No next tldr example"))
    (tldr--goto-example next)))

(defun tldr-previous-example ()
  "Move to the previous tldr example."
  (interactive)
  (let* ((index (tldr--current-example-index))
         (previous (if index (1- index) -1)))
    (when (< previous 0)
      (user-error "No previous tldr example"))
    (tldr--goto-example previous)))

(defun tldr-reload ()
  "Reload the current tldr page."
  (interactive)
  (unless tldr--command
    (user-error "No tldr page in this buffer"))
  (let ((page (tldr--read-page tldr--command)))
    (setq tldr--page page)
    (tldr--render-page page)
    (message "Reloaded %s" tldr--command)))

(defun tldr--refresh-open-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'tldr-mode)
        (tldr-reload)))))

;;;###autoload
(defun tldr-update ()
  "Run tldr --update asynchronously."
  (interactive)
  (unless (and tldr-executable (executable-find tldr-executable))
    (user-error "Could not find tldr executable"))
  (let ((buffer (get-buffer-create "*tldr update*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (make-process
     :name "tldr-update"
     :buffer buffer
     :command (list tldr-executable "--update")
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((status (process-exit-status process))
               (buffer (process-buffer process)))
           (if (zerop status)
               (progn
                 (tldr--refresh-open-buffers)
                 (message "tldr pages updated"))
             (message "tldr update failed with status %s; see %s"
                      status (buffer-name buffer)))))))))

(defun tldr--capf-bounds ()
  (let ((beg (save-excursion
               (skip-chars-backward "[:alnum:]_+@:%.,=/-")
               (point)))
        (end (save-excursion
               (skip-chars-forward "[:alnum:]_+@:%.,=/-")
               (point))))
    (cons beg end)))

(defun tldr--capf-context ()
  (let* ((bounds (tldr--capf-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (token (buffer-substring-no-properties beg end))
         shell)
    (cond
     ((string-match "\\`\\([^/]+\\)/" token)
      (list (match-string 1 token) beg end))
     ((setq shell (tldr--shell-command))
      (list shell beg end))
     ((not (string-empty-p token))
      (list (tldr--command-from-string token) beg end)))))

;;;###autoload
(defun capf-tldr ()
  "Complete tldr examples at point."
  (when-let* ((context (tldr--capf-context))
              (command (nth 0 context))
              (beg (nth 1 context))
              (end (nth 2 context))
              (examples (ignore-errors
                          (tldr--page-examples (tldr--read-page command)))))
    (let* ((table (mapcar (lambda (example)
                            (cons (tldr--template-name command
                                                       (tldr--example-description example))
                                  (tldr--example-command example)))
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
                    (tldr--insert-template command)))))))))

;;;###autoload
(defun tldr-capf-setup ()
  "Add `capf-tldr' to `completion-at-point-functions' in the current buffer."
  (interactive)
  (add-hook 'completion-at-point-functions #'capf-tldr nil t))

(dolist (command '(tldr-copy-command
                   tldr-insert-command
                   tldr-next-example
                   tldr-previous-example
                   tldr-reload))
  (function-put command 'command-modes '(tldr-mode)))

(provide 'tldr)

;;; tldr.el ends here
