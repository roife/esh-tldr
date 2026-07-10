;;; esh-tldr.el --- Browse local tldr pages -*- lexical-binding: t; -*-

;; Author: roifewu
;; Package-Requires: ((emacs "31.0"))
;; Keywords: help, tools

;;; Commentary:

;; Browse local tldr pages and insert examples as editable templates.
;; `esh-tldr' uses Emacs' standard completion protocol.  `esh-tldr-dwim'
;; understands regular buffers, Eshell, Comint, and Shell.  Additional
;; terminal integrations can register context and target handlers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'comint)
(require 'help-mode)
(require 'subr-x)
(require 'tempo)
(require 'thingatpt)

(declare-function tempel-insert "ext:tempel" (template))
(defvar eshell-last-output-end)

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
  '((t :inherit fixed-pitch :weight semi-bold))
  "Face for esh-tldr example commands.")

(defface esh-tldr-placeholder-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for placeholders in esh-tldr example commands.")

(cl-defstruct (esh-tldr--example (:constructor esh-tldr--make-example))
  description command)

(cl-defstruct (esh-tldr--page (:constructor esh-tldr--make-page))
  title description examples file)

(cl-defstruct (esh-tldr--target (:constructor esh-tldr--make-target))
  "A source range which an example may replace.
HANDLER, when non-nil, performs integration-specific replacement.  DATA
holds opaque state owned by that handler."
  buffer start end original handler data)

(cl-defstruct (esh-tldr--context (:constructor esh-tldr--make-context))
  command target)

(defvar-local esh-tldr--command nil)
(defvar-local esh-tldr--page nil)
(defvar-local esh-tldr--source-target nil)

(defvar esh-tldr--buffer-name "*esh-tldr*"
  "Name of the reusable TL;DR help buffer.")
(defvar esh-tldr-command-history nil
  "Minibuffer history for TL;DR command selection.")
(defvar esh-tldr--command-entry-cache nil
  "Cached command entries as (KEY . ENTRIES).")

(define-error 'esh-tldr-unsafe-replacement
  "The saved source can no longer be replaced safely"
  'user-error)

(defvar esh-tldr-context-functions nil
  "Abnormal hook for optional context providers.
Each function is called with no arguments in the source buffer and should
return an `esh-tldr--context' or nil.")

(defvar esh-tldr-fallback-target-functions nil
  "Abnormal hook for optional zero-width target providers.
Each function is called with no arguments in the source buffer and should
return an `esh-tldr--target' or nil.")

;;; Page discovery and parsing

(defun esh-tldr--locale-base (locale)
  (let ((locale (string-trim (or locale ""))))
    (setq locale (replace-regexp-in-string "[.@].*\\'" "" locale))
    (unless (member locale '("" "C" "POSIX"))
      locale)))

(defun esh-tldr--language-value ()
  (if (memq esh-tldr-language '(auto nil))
      (cl-loop for variable in '("LC_ALL" "LC_MESSAGES" "LANG")
               for value = (getenv variable)
               when (and value (not (string-empty-p (string-trim value))))
               return value)
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

(defun esh-tldr--platform-directories (&optional platform)
  "Return ordered esh-tldr platform directories for PLATFORM."
  (let* ((platform (or platform
                       (if (eq esh-tldr-platform 'auto)
                           system-type
                         esh-tldr-platform)))
         (name (cond
                ((eq platform 'darwin) "osx")
                ((eq platform 'gnu/linux) "linux")
                ((memq platform '(windows-nt ms-dos cygwin)) "windows")
                ((stringp platform) platform)
                (t "common"))))
    (if (string= name "common")
        '("common")
      (list name "common"))))

(defun esh-tldr--candidate-files (command)
  "Return finite ordered candidate page files for COMMAND."
  (mapcar (lambda (directory)
            (expand-file-name (concat command ".md") directory))
          (esh-tldr--command-directories)))

(defun esh-tldr--command-directories ()
  (cl-loop for language in (esh-tldr--language-directories)
           append (cl-loop for platform in (esh-tldr--platform-directories)
                           collect (expand-file-name
                                    (format "%s/%s" language platform)
                                    esh-tldr-pages-directory))))

(defun esh-tldr--command-cache-key (directories)
  "Return a cache key for DIRECTORIES."
  (list directories
        (mapcar
         (lambda (directory)
           (file-attribute-modification-time
            (file-attributes directory 'string)))
         directories)))

(defun esh-tldr--clear-command-cache ()
  "Discard cached command completion entries."
  (setq esh-tldr--command-entry-cache nil))

(defun esh-tldr--command-candidate-entries ()
  "Return ordered (COMMAND . SOURCE) entries for standard completion."
  (let* ((directories (esh-tldr--command-directories))
         (key (esh-tldr--command-cache-key directories)))
    (if (equal key (car esh-tldr--command-entry-cache))
        (cdr esh-tldr--command-entry-cache)
      (let ((seen (make-hash-table :test #'equal))
            entries)
        (dolist (directory directories)
          (when (file-directory-p directory)
            (dolist (file (directory-files directory nil "\\.md\\'"))
              (let ((command (file-name-sans-extension file)))
                (unless (gethash command seen)
                  (puthash command t seen)
                  (push (cons command
                              (file-relative-name
                               directory esh-tldr-pages-directory))
                        entries))))))
        (setq entries (nreverse entries)
              esh-tldr--command-entry-cache (cons key entries))
        entries))))

(defun esh-tldr--completion-table (entries)
  "Build an Emacs 31 completion table from command ENTRIES."
  (let ((annotations (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (car entry) (cdr entry) annotations))
    (completion-table-with-metadata
     (mapcar #'car entries)
     `((category . esh-tldr-command)
       (annotation-function
        . ,(lambda (candidate)
             (when-let* ((source (gethash candidate annotations)))
               (propertize (concat "  " source)
                           'face 'completions-annotations))))
       (eager-display . t)
       (eager-update . t)))))

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
          (push (esh-tldr--make-example
                 :description pending
                 :command (match-string 1 line))
                examples)
          (setq pending nil))))
      (esh-tldr--make-page
       :title title
       :description (string-join (nreverse descriptions) "\n")
       :examples (nreverse examples)
       :file file))))

(defun esh-tldr--read-page (command)
  "Return the parsed page for COMMAND, or nil when it is unavailable."
  (when-let* ((file (cl-find-if #'file-readable-p
                                (esh-tldr--candidate-files command))))
    (esh-tldr--parse-page file)))

;;; Shell parsing and source targets

(defun esh-tldr--shell-segment-bounds (string offset)
  "Return the simple-command bounds in STRING containing OFFSET.
Unquoted shell control operators delimit segments.  Return nil for nested
or incomplete shell syntax rather than risk replacing the wrong token."
  (catch 'unsafe
    (let ((length (length string))
          (offset (max 0 (min offset (length string))))
          (start 0)
          quote escaped result
          (index 0))
      (while (and (< index length) (not result))
        (let ((char (aref string index))
              (next (and (< (1+ index) length)
                         (aref string (1+ index)))))
          (cond
           (escaped (setq escaped nil))
           ((and quote (= char quote)) (setq quote nil))
           ((and (not (eq quote ?\')) (= char ?\\)) (setq escaped t))
           ((and (not (eq quote ?\'))
                 (or (= char ?`)
                     (and (= char ?$) (eq next 40))))
            (throw 'unsafe nil))
           ((and (not quote) (memq char '(40 41)))
            (throw 'unsafe nil))
           ((and (not quote) (memq char '(?\' ?\"))) (setq quote char))
           ((and (not quote)
                 (= char ?#)
                 (or (zerop index)
                     (memq (aref string (1- index))
                           '(?\s ?\t ?\n ?\; ?\| ?\&))))
            (setq result (cons start index)))
           ((and (not quote) (memq char '(?\; ?\| ?\& ?\n)))
            (let ((operator-end
                   (if (and next (memq char '(?\| ?\&)) (= char next))
                       (+ index 2)
                     (1+ index))))
              (if (<= offset index)
                  (setq result (cons start index))
                (setq start operator-end))
              (setq index (1- operator-end))))))
        (setq index (1+ index)))
      (when (or quote escaped)
        (throw 'unsafe nil))
      (or result (cons start length)))))

(defun esh-tldr--command-token-in-range (string start end)
  "Return the first non-assignment token in STRING between START and END."
  (catch 'command
    (let ((index start))
      (while (< index end)
        (while (and (< index end)
                    (memq (aref string index) '(?\s ?\t ?\n)))
          (setq index (1+ index)))
        (when (< index end)
          (let ((token-start index)
                quote escaped)
            (while (and (< index end)
                        (or quote escaped
                            (not (memq (aref string index) '(?\s ?\t ?\n)))))
              (let ((char (aref string index)))
                (cond
                 (escaped (setq escaped nil))
                 ((and quote (= char quote)) (setq quote nil))
                 ((and (not (eq quote ?\')) (= char ?\\)) (setq escaped t))
                 ((and (not quote) (memq char '(?\' ?\"))) (setq quote char))))
              (setq index (1+ index)))
            (let* ((raw (substring string token-start index))
                   (value (condition-case nil
                              (or (car (split-string-shell-command raw)) raw)
                            (error raw))))
              (unless (string-match-p
                       "\\`[[:alpha:]_][[:alnum:]_]*=" value)
                (when (string-match-p "\\`[<>]" value)
                  (throw 'command nil))
                (throw 'command (list value token-start index))))))))))

(defun esh-tldr--command-token-in-string (string offset)
  "Return (COMMAND START END) for the command token at OFFSET in STRING."
  (when-let* ((bounds (esh-tldr--shell-segment-bounds string offset)))
    (pcase-let* ((`(,segment-start . ,segment-end) bounds)
                 (token (esh-tldr--command-token-in-range
                         string segment-start segment-end)))
      (when-let* ((token token)
                  (value (car token))
                  (command (file-name-nondirectory value))
                  ((not (string-empty-p command))))
        (list command (nth 1 token) (nth 2 token))))))

(defun esh-tldr--command-from-string (string)
  (let ((string (string-trim-right (or string ""))))
    (when-let* ((token (esh-tldr--command-token-in-string
                        string (length string))))
      (car token))))

(defun esh-tldr--valid-command-p (command)
  (and (stringp command)
       (not (string-empty-p command))
       (not (string-match-p "[/\\\\]" command))))

(defun esh-tldr--check-command (command)
  (let ((command (esh-tldr--command-from-string command)))
    (unless command
      (user-error "Command name is empty"))
    (unless (esh-tldr--valid-command-p command)
      (user-error "Illegal command name: %s" command))
    command))

(defun esh-tldr--target (beg end)
  "Create a normal buffer target from BEG to END."
  (esh-tldr--make-target
   :buffer (current-buffer)
   :start (copy-marker beg nil)
   :end (copy-marker end t)
   :original (buffer-substring-no-properties beg end)))

(defun esh-tldr--command-token-in-buffer (beg end cursor)
  "Return an absolute command token between BEG and END near CURSOR."
  (let* ((input (buffer-substring-no-properties beg end))
         (offset (- (max beg (min cursor end)) beg)))
    (when-let* ((token (esh-tldr--command-token-in-string input offset)))
      (list (car token)
            (+ beg (nth 1 token))
            (+ beg (nth 2 token))))))

(defun esh-tldr--context-in-bounds (beg end cursor)
  "Return a context parsed from BEG, END, and CURSOR."
  (when-let* ((token (esh-tldr--command-token-in-buffer beg end cursor)))
    (esh-tldr--make-context
     :command (car token)
     :target (esh-tldr--target (nth 1 token) (nth 2 token)))))

(defun esh-tldr--region-context ()
  (when (use-region-p)
    (esh-tldr--context-in-bounds
     (region-beginning) (region-end) (region-end))))

(defun esh-tldr--comint-input-bounds ()
  (when (derived-mode-p 'comint-mode)
    (cons (comint-line-beginning-position) (line-end-position))))

(defun esh-tldr--eshell-input-bounds ()
  (when (and (derived-mode-p 'eshell-mode)
             (boundp 'eshell-last-output-end)
             (markerp eshell-last-output-end)
             (marker-position eshell-last-output-end))
    (cons (max (point-min) (marker-position eshell-last-output-end))
          (point-max))))

(defun esh-tldr--terminal-context ()
  (when-let* ((bounds (or (esh-tldr--eshell-input-bounds)
                          (esh-tldr--comint-input-bounds))))
    (esh-tldr--context-in-bounds (car bounds) (cdr bounds) (point))))

(defun esh-tldr--point-context ()
  (when-let* ((bounds (or (bounds-of-thing-at-point 'filename)
                          (bounds-of-thing-at-point 'symbol)))
              (text (buffer-substring-no-properties
                     (car bounds) (cdr bounds)))
              (command (esh-tldr--command-from-string text)))
    (esh-tldr--make-context
     :command command
     :target (esh-tldr--target (car bounds) (cdr bounds)))))

(defun esh-tldr--context ()
  (or (esh-tldr--region-context)
      (run-hook-with-args-until-success 'esh-tldr-context-functions)
      (esh-tldr--terminal-context)
      (esh-tldr--point-context)))

(defun esh-tldr--fallback-target ()
  "Return a zero-width target suitable for the current buffer."
  (or (run-hook-with-args-until-success 'esh-tldr-fallback-target-functions)
      (esh-tldr--target (point) (point))))

;;; Templates and safe replacement

(defun esh-tldr--unsafe (format-string &rest args)
  "Signal an expected unsafe-replacement error."
  (signal 'esh-tldr-unsafe-replacement
          (list (apply #'format format-string args))))

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
          (let ((symbol (make-symbol (format "esh-tldr-%d" index))))
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
            (esh-tldr--unsafe "Tempel is not available"))
          (tempel-insert template))
      (let ((tempo-interactive t)
            (template-symbol (make-symbol "esh-tldr-tempo-template")))
        (set template-symbol template)
        (tempo-insert-template template-symbol nil)))))

(defun esh-tldr--target-bounds (target)
  "Return live buffer positions for TARGET, or nil."
  (let ((buffer (esh-tldr--target-buffer target))
        (start (esh-tldr--target-start target))
        (end (esh-tldr--target-end target)))
    (when (and (buffer-live-p buffer)
               (markerp start) (markerp end)
               (eq (marker-buffer start) buffer)
               (eq (marker-buffer end) buffer))
      (when-let* ((beg (marker-position start))
                  (end-pos (marker-position end))
                  ((<= beg end-pos)))
        (cons beg end-pos)))))

(defun esh-tldr--replace-buffer-target (command target)
  "Replace TARGET with COMMAND as a template.
Return non-nil on success and signal an error when replacement is unsafe."
  (let ((bounds (esh-tldr--target-bounds target)))
    (unless bounds
      (esh-tldr--unsafe "Source buffer no longer exists"))
    (with-current-buffer (esh-tldr--target-buffer target)
      (when buffer-read-only
        (esh-tldr--unsafe "Source buffer is read-only"))
      (pcase-let ((`(,beg . ,end) bounds))
        (unless (equal (buffer-substring-no-properties beg end)
                       (esh-tldr--target-original target))
          (esh-tldr--unsafe "Source command changed while the page was open"))
        (pop-to-buffer (current-buffer))
        (condition-case err
            (atomic-change-group
              (delete-region beg end)
              (goto-char beg)
              (esh-tldr--insert-template command))
          (text-read-only
           (esh-tldr--unsafe "%s" (error-message-string err))))
        t))))

(defun esh-tldr--copy (command &optional reason)
  (kill-new command)
  (if reason
      (message "Could not replace input (%s); copied: %s" reason command)
    (message "Copied: %s" command)))

(defun esh-tldr--use-command (command target)
  "Use COMMAND at TARGET, falling back to copying on unsafe replacement."
  (condition-case err
      (if-let* ((handler (esh-tldr--target-handler target)))
          (funcall handler command target)
        (esh-tldr--replace-buffer-target command target))
    (esh-tldr-unsafe-replacement
     (esh-tldr--copy command (or (cadr err) (error-message-string err)))
     nil)))

;;; Help-style page

(defvar-keymap esh-tldr-mode-map
  :doc "Keymap for `esh-tldr-mode'."
  :parent help-mode-map
  "RET" #'esh-tldr-activate-or-insert
  "w" #'esh-tldr-copy-command
  "y" #'esh-tldr-copy-command
  "n" #'esh-tldr-next-example
  "p" #'esh-tldr-previous-example
  "s" #'esh-tldr-search-command
  "g" #'esh-tldr-reload)

;;;###autoload
(define-derived-mode esh-tldr-mode help-mode "esh-tldr"
  "Major mode for interactive tldr help pages."
  (setq-local truncate-lines nil)
  (setq-local header-line-format
              " RET use   w copy   TAB next action   n/p example   s search   g reload   q quit "))

(defun esh-tldr--formatted-command (command)
  (let ((text (copy-sequence command))
        (start 0))
    (add-face-text-property 0 (length text) 'esh-tldr-command-face t text)
    (while (string-match "{{[^{}]+}}" text start)
      (add-face-text-property (match-beginning 0) (match-end 0)
                              'esh-tldr-placeholder-face t text)
      (setq start (match-end 0)))
    text))

(defun esh-tldr--target-summary (target)
  (when target
    (let ((original (esh-tldr--target-original target)))
      (if (string-empty-p original)
          "RET will insert at the original point."
        (format "RET will replace: %s"
                (truncate-string-to-width
                 (replace-regexp-in-string "[\n\t ]+" " " original)
                 100 nil nil "…"))))))

(defun esh-tldr--insert-action-button (label index action)
  (insert-text-button
   label
   'follow-link t
   'help-echo (if (eq action 'copy) "Copy this command" "Use this command")
   'esh-tldr-example index
   'esh-tldr-action action
   'action #'esh-tldr--button-action))

(defun esh-tldr--button-action (button)
  (let ((index (button-get button 'esh-tldr-example))
        (action (button-get button 'esh-tldr-action)))
    (esh-tldr--goto-example index)
    (if (eq action 'copy)
        (esh-tldr-copy-command)
      (esh-tldr-insert-command))))

(defun esh-tldr--render-empty-state (command)
  (insert (propertize "No local TL;DR page is available.\n\n"
                      'face 'warning))
  (insert (format "Command: %s\nPages:   %s\n\n"
                  command (abbreviate-file-name esh-tldr-pages-directory)))
  (insert-text-button "[Search another command]"
                      'follow-link t
                      'action (lambda (_button) (esh-tldr-search-command)))
  (when esh-tldr-executable
    (insert "  ")
    (insert-text-button "[Update pages]"
                        'follow-link t
                        'action (lambda (_button) (call-interactively #'esh-tldr-update))))
  (insert "\n"))

(defun esh-tldr--render-page (page command)
  (let ((inhibit-read-only t)
        first-example-position)
    (erase-buffer)
    (insert (propertize (or (and page (esh-tldr--page-title page)) command)
                        'face 'esh-tldr-title-face)
            "\n\n")
    (when-let* ((description (and page (esh-tldr--page-description page)))
                ((not (string-empty-p description))))
      (insert description "\n\n"))
    (when-let* ((summary (esh-tldr--target-summary esh-tldr--source-target)))
      (insert (propertize summary 'face 'shadow) "\n\n"))
    (cond
     ((not page)
      (esh-tldr--render-empty-state command))
     ((null (esh-tldr--page-examples page))
      (insert (propertize "This page contains no examples.\n" 'face 'warning)))
     (t
      (insert (propertize "Examples\n" 'face 'esh-tldr-heading-face))
      (cl-loop for example in (esh-tldr--page-examples page)
               for index from 0
               do (let ((start (point)))
                    (setq first-example-position
                          (or first-example-position start))
                    (insert "\n" (propertize
                                    (esh-tldr--plain-description
                                     (esh-tldr--example-description example))
                                    'face 'esh-tldr-heading-face)
                            "\n  ")
                    (esh-tldr--insert-action-button
                     (esh-tldr--formatted-command
                      (esh-tldr--example-command example))
                     index 'use)
                    (insert "\n  ")
                    (esh-tldr--insert-action-button "[Insert/replace]" index 'use)
                    (insert "  ")
                    (esh-tldr--insert-action-button "[Copy]" index 'copy)
                    (insert "\n")
                    (add-text-properties start (point)
                                         `(esh-tldr-example ,index))))))
    (when page
      (insert "\n" (propertize "Source: " 'face 'esh-tldr-heading-face)
              (abbreviate-file-name (esh-tldr--page-file page)) "\n"))
    (goto-char (or first-example-position (point-min)))
    (set-buffer-modified-p nil)))

(defun esh-tldr--show-page (page command target)
  (esh-tldr-mode)
  (setq esh-tldr--command command
        esh-tldr--page page
        esh-tldr--source-target target)
  (esh-tldr--render-page page command))

(defun esh-tldr--open (command &optional target)
  (let* ((command (esh-tldr--check-command command))
         (target (or target (esh-tldr--fallback-target)))
         (page (esh-tldr--read-page command))
         (buffer (get-buffer-create esh-tldr--buffer-name)))
    (with-current-buffer buffer
      (esh-tldr--show-page page command target))
    (pop-to-buffer buffer)))

(defun esh-tldr--read-command (&optional default)
  (let* ((entries (esh-tldr--command-candidate-entries))
         (table (esh-tldr--completion-table entries))
         (prompt (if default
                     (format "TL;DR command (default %s): " default)
                   "TL;DR command: ")))
    (esh-tldr--check-command
     (completing-read prompt table nil nil nil
                      'esh-tldr-command-history default))))

;;;###autoload
(defun esh-tldr (command &optional target)
  "Search for COMMAND and open its local TL;DR page."
  (interactive
   (let* ((context (esh-tldr--context))
          (target (or (and context (esh-tldr--context-target context))
                      (esh-tldr--fallback-target)))
          (default (and context (esh-tldr--context-command context))))
     (list (esh-tldr--read-command default) target)))
  (esh-tldr--open command target))

;;;###autoload
(defun esh-tldr-at-point ()
  "Open the local TL;DR page for the command at point."
  (interactive)
  (let ((context (esh-tldr--point-context)))
    (unless context
      (user-error "No command at point"))
    (esh-tldr--open (esh-tldr--context-command context)
                    (esh-tldr--context-target context))))

;;;###autoload
(defun esh-tldr-dwim ()
  "Open a TL;DR page using region, terminal input, point, or completion."
  (interactive)
  (if-let* ((context (esh-tldr--context)))
      (esh-tldr--open (esh-tldr--context-command context)
                      (esh-tldr--context-target context))
    (esh-tldr (esh-tldr--read-command) (esh-tldr--fallback-target))))

(defun esh-tldr-search-command ()
  "Search for another command while preserving the original source target."
  (interactive)
  (unless (derived-mode-p 'esh-tldr-mode)
    (user-error "This command is only available in an esh-tldr page"))
  (esh-tldr--open (esh-tldr--read-command esh-tldr--command)
                  esh-tldr--source-target))

(defun esh-tldr--current-example-index ()
  (or (get-text-property (point) 'esh-tldr-example)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'esh-tldr-example))))

(defun esh-tldr--current-example ()
  (let ((index (esh-tldr--current-example-index)))
    (unless (and index esh-tldr--page)
      (user-error "No TL;DR example at point"))
    (nth index (esh-tldr--page-examples esh-tldr--page))))

(defun esh-tldr-copy-command ()
  "Copy the current TL;DR example command."
  (interactive)
  (esh-tldr--copy
   (esh-tldr--example-command (esh-tldr--current-example))))

(defun esh-tldr--close-page-buffer (buffer window)
  (if (and (window-live-p window) (eq (window-buffer window) buffer))
      (quit-window 'kill window)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun esh-tldr-insert-command ()
  "Use the current TL;DR example at its saved source target."
  (interactive)
  (let ((command (esh-tldr--example-command (esh-tldr--current-example)))
        (page-buffer (current-buffer))
        (page-window (get-buffer-window (current-buffer) t))
        (target esh-tldr--source-target))
    (unless target
      (user-error "No source target for this page"))
    (when (esh-tldr--use-command command target)
      (esh-tldr--close-page-buffer page-buffer page-window))))

(defun esh-tldr-activate-or-insert ()
  "Activate the button at point, or use the current TL;DR example."
  (interactive)
  (if-let* ((button (button-at (point))))
      (button-activate button)
    (esh-tldr-insert-command)))

(defun esh-tldr--goto-example (index)
  (let ((pos (text-property-any (point-min) (point-max)
                                'esh-tldr-example index)))
    (unless pos
      (user-error "No TL;DR example"))
    (goto-char pos)
    (beginning-of-line)))

(defun esh-tldr--move-example (delta)
  (let ((count (length (and esh-tldr--page
                            (esh-tldr--page-examples esh-tldr--page)))))
    (when (zerop count)
      (user-error "No TL;DR examples"))
    (esh-tldr--goto-example
     (mod (+ (or (esh-tldr--current-example-index)
                 (if (> delta 0) -1 0))
             delta)
          count))))

(defun esh-tldr-next-example ()
  "Move to the next TL;DR example, wrapping at the end."
  (interactive)
  (esh-tldr--move-example 1))

(defun esh-tldr-previous-example ()
  "Move to the previous TL;DR example, wrapping at the beginning."
  (interactive)
  (esh-tldr--move-example -1))

(defun esh-tldr-reload ()
  "Reload the current TL;DR page, including an empty page."
  (interactive)
  (unless esh-tldr--command
    (user-error "No TL;DR page in this buffer"))
  (let ((index (esh-tldr--current-example-index)))
    (setq esh-tldr--page (esh-tldr--read-page esh-tldr--command))
    (esh-tldr--render-page esh-tldr--page esh-tldr--command)
    (when (and index esh-tldr--page
               (< index (length (esh-tldr--page-examples esh-tldr--page))))
      (esh-tldr--goto-example index))
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
  (unless (and esh-tldr-executable
               (or (and (file-name-absolute-p esh-tldr-executable)
                        (file-executable-p esh-tldr-executable))
                   (executable-find esh-tldr-executable)))
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
                 (esh-tldr--clear-command-cache)
                 (esh-tldr--refresh-open-buffers)
                 (message "TL;DR pages updated"))
             (message "TL;DR update failed with status %s; see %s"
                      status (buffer-name buffer)))))))))

(dolist (command '(esh-tldr-activate-or-insert
                   esh-tldr-copy-command
                   esh-tldr-insert-command
                   esh-tldr-next-example
                   esh-tldr-previous-example
                   esh-tldr-reload
                   esh-tldr-search-command))
  (function-put command 'command-modes '(esh-tldr-mode)))

(provide 'esh-tldr)

;;; esh-tldr.el ends here
