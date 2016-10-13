(defun *-super-completing-find-file (force-normal)
  (interactive "P")
  (call-interactively
   (if (and (not force-normal) (projectile-project-p))
       #'projectile-find-file
     #'counsel-find-file)))

(defun *-insert-hfill (char)
  (interactive
   (list (if current-prefix-arg
             (let ((input (read-char "Fill char: ")))
               (cond
                ((eq input ?r) nil)
                (t input))))))
  (let* ((eol (save-excursion
                (move-end-of-line nil)
                (current-column))))
    (insert-char (or char ? ) (- (current-fill-column) eol))))

(defun find-obsolete (list-of-functions)
  (let (f obsolete ret)
    (while list-of-functions
      (setq f (first list-of-functions)
            list-of-functions (rest list-of-functions)
            obsolete (plist-get (symbol-plist f) 'byte-obsolete-info))
      (when obsolete
        (add-to-list 'ret (cons f (car obsolete)))))
    ret))

;; (find-obsolete (delq nil (mapcar (lambda (s) (when (functionp s) s)) obarray)))

(defconst *-windows-p
  (eq system-type 'windows-nt)
  "Predicate indicating if this is a Windows environment.")
(defconst *-osx-p
  (eq system-type 'darwin)
  "Predicate indicating if this is a OS X environment.")
(defconst *-redhat-p
  (eq system-type 'gnu/linux)
  "Predicate indicating if this is a Redhat environment.")

(defconst *-dropbox-directory
  (expand-file-name
   "Dropbox"
   (cond
    (*-windows-p "T:")
    (*-osx-p "~")))
  "Dropbox directory")

(defun *-get-custom.el (&optional prefix)
  (locate-user-emacs-file
   (concat (or prefix
               ;; these files are not tracked by git!
               ;; such settings can be considered private
               (cond (*-windows-p "windows")
                     (*-osx-p "osx")))
           ".custom.el")))

(defun *-try-load (file)
  (if (file-readable-p file)
      (load-file file)
    (warn "Unable to read %S" file)
    nil))

(defun *-load-customizations ()
  (interactive)
  (*-try-load (*-get-custom.el))
  (*-try-load (setq custom-file (*-get-custom.el ""))))


(defun *-copy-buffer-file-name-as-kill (&optional scope pos-style)
  "Copy the buffer-file-name to the kill-ring.

SCOPE must be one of

  nil
     Copy the full name (/path/to/file)

  `directory'
     Copy the directory part (/path/to)

  `basename'
     Copy the basename (file)


POS-STYLE must be one of

  nil
     No styling (file)

  `line'
     Get the line number (file:505)

  `line-column'
     Get both the line and column number (file:505c30)

  `point'
     Get the value of point (file:40053)

POS-STYLE has no effect when SCOPE is `directory'."
  (interactive
   (let ((scope
          (cdr
           (assoc
            (read-char-choice
             "Copy (f)ull name, (d)irectory, or just the (b)asename? "
             '(?f ?d ?b))
            '((?f . full)
              (?d . directory)
              (?b . basename))))))
     (list scope
           (and (not (equal scope 'directory)) current-prefix-arg
                (cdr (assoc (read-char-choice
                             "Style: (n)one, (l)ine, (c)olumn, or (p)oint? "
                             '(?n ?l ?c ?p))
                            '((?n . nil)
                              (?l . line)
                              (?c . line-column)
                              (?p . point))))))))
  ;; @todo error up here

  (let* ((name (if (eq major-mode 'dired-mode)
                   (dired-get-filename)
                 (or (buffer-file-name)
                     (user-error "Invalid context"))))
         (file-part
          (cond ((equal scope 'full)
                 name)
                ((equal scope 'directory)
                 (file-name-directory name))
                ((equal scope 'basename)
                 (file-name-nondirectory name))
                ((null scope) nil)
                (t (error "Invalid scope %S" scope))))
         ;; @todo can make this whole part a lot easier with a clever use of
         ;; assoc.  (when pos-style (cdr (assoc pos-style ...)))
         (pos-part
          (when pos-style
            (concat ":"
                    (cond ((equal pos-style 'line)
                           ;; @todo return absolute line number
                           (format "%d" (line-number-at-pos)))
                          ((equal pos-style 'line-column)
                           (format "%dc%d"
                                   (line-number-at-pos)
                                   (current-column)))
                          ((equal pos-style 'point)
                           (format "p%d" (point)))
                          (t (error "Invalid style %S" pos-style))))))
         (new-kill-string (concat file-part pos-part)))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defcustom *-delete-trailing-whitespace-on-save
  nil
  "If `t', files will be stripped of trailing whitespace before saving."
  :group '*-files)

(defun *-maybe-delete-trailing-whitespace ()
  (when *-delete-trailing-whitespace-on-save
    (delete-trailing-whitespace)))

(defun *-delete-empty-directories (root-directory)
  "Recursively delete empty directories in ROOT-DIRECTORY.

When called from dired, `dired-current-directory' is used for
ROOT-DIRECTORY."

  ;; Interface
  (interactive (list (if (eq major-mode 'dired-mode)
                         (expand-file-name (dired-current-directory))
                       (read-from-minibuffer "Root directory: "))))
  (when (or (null root-directory) (string= "" root-directory))
    (user-error "No root directory provided"))
  (when (called-interactively-p 'interactive)
    (unless (yes-or-no-p (format "Delete all non-empty directories in `%s'? "
                                 root-directory))
      (user-error "Directory `%s' has been left untouched" root-directory)))

  ;; Implementation
  (require 'f)
  (let ((entries (f-directories root-directory)))
    (while entries
      (let ((curdir (car entries)))
        (when (f-directories curdir)
          (*-delete-empty-directories curdir))
        (unless (f-entries curdir)
          (delete-directory curdir)
          (message "Directory deleted: `%s'" curdir))
        (setq entries (cdr entries))))))

(defun unfill-region (beg end)
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun *-isearch-yank-thing-at-point ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))

(defun *-read-from-minibuffer (prompt &optional default)
  (let ((response
         (read-from-minibuffer
          (concat prompt (if default (format " (default `%s')" default)) ": "))))
    (if (string= response "") default response)))

(defun *-TeX-find-kpathsea (string)
  (interactive
   (list
    (let ((default (concat (thing-at-point 'symbol t) ".sty")))
      (setq string
            (*-read-from-minibuffer
             "Find file in TeX distribution"
             default)))))
  (find-file (substring (shell-command-to-string
                         (format "kpsewhich %s" string))
                        0 -1)))

(defun *-TeX-find-texdoc (texdoc-query)
  (interactive "sPackage: ")
  (if (string-equal texdoc-query "")
      (error "Cannot query texdoc against an empty string")
    (let ((texdoc-output (shell-command-to-string
                          (format "texdoc -l -M %s"
                                  texdoc-query))))
      (if (string-match texdoc-output "")
          (error "Sorry, no documentation found for %s" texdoc-query)
        (let ((texdoc-file (nth 2 (split-string texdoc-output))))
          (if (file-readable-p texdoc-file)
              (find-file-other-window texdoc-file)
            (error "Sorry, the file returned by texdoc for %s isn't readable"
                   texdoc-query)))))))

(defun *-org-agenda-next-items ()
  (interactive)
  (org-agenda nil "d"))

(defun *-org-sync-attachments ()
  "Update the current entry's attachment metadata."
  (interactive)
  (require 'org-attach)
  (let ((attachments
         (when (org-attach-dir)
           (org-attach-file-list (org-attach-dir)))))
    (if attachments
        (org-set-property org-attach-file-list-property
                          (mapconcat #'url-encode-url attachments " "))
      (org-delete-property org-attach-file-list-property))
    (org-toggle-tag org-attach-auto-tag (if attachments 'on 'off))))

(defun *-org-sync-attachments-all ()
  "Update all attachment properties in the buffer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward-regexp "^\\* " nil t)
      (*-org-sync-attachments))))

(defcustom *-god-mode-update-cursor-affected-forms
  '(god-local-mode buffer-read-only)
  "If any of these forms evaluate to non-nil, the cursor will change."
  :group '*-god)

(defcustom *-god-mode-cursor
  'hbar
  "The cursor to use"
  :group '*-god)

(defun *-god-mode-update-cursor ()
  (setq cursor-type
        (if (member t (mapcar #'eval *-god-mode-update-cursor-affected-forms))
            *-god-mode-cursor
          t)))

(defun *-cider-connect ()
  (interactive)
  (if (cider-connected-p)
      (cider-switch-to-repl-buffer)
    (cider-connect "localhost" 31337)))

(defun *-twittering-update-status-from-minibuffer ()
  (interactive)
  (let ((twittering-update-status-function
         #'twittering-update-status-from-minibuffer))
    (twittering-update-status)))

(defun *-do-replacements ()
  (save-excursion
    (goto-char 0)
    (while (search-forward " -- " nil t)
      (backward-char 4)
      (delete-char 1)
      (insert " "))))
(defun *-set-replacements ()
  (add-hook (make-local-variable 'before-save-hook)
            #'*-do-replacements))
(defun *-insert-post-url (bare)
  (interactive "P")
  (require 'f)
  (require 'cl-lib)
  (let* ((basedir (expand-file-name "~/blog/_posts/"))
         (files (f-files basedir
                         (lambda (f)
                           (string= (file-name-extension f)
                                    "markdown"))
                         t))
         (candidates (cl-remove-duplicates
                      (mapcar #'file-name-base files)
                      :test #'string=)))
    (insert
     (format
      (if bare "%s" "{%% post_url %s %%}")
      (file-name-base (completing-read "Post: " candidates))))))

(defun *-dired-for-each-marked-file (function)
  "Do stuff for each marked file, only works in dired window"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (mapcar function (dired-get-marked-files))
    (error "Not a Dired buffer `%s'" major-mode)))

(defun *-dired-zip-files (zip-file)
  "Create an archive containing the marked files."
  (interactive "sEnter name of zip file: ")
  (let ((zip-file
         (if (string-match ".zip$" zip-file)
             zip-file
           (concat zip-file ".zip"))))
    (shell-command
     (concat "zip "
             zip-file
             " "
             (mapconcat #'file-name-nondirectory
                        (dired-get-marked-files) " "))))

  ;; remove the mark on all the files  "*" to " "
  (revert-buffer)
  ;; mark zip file
  (dired-change-marks 42 ?\040)
  ;;(filename-to-regexp zip-file))
  (dired-mark-files-regexp zip-file))

(defun *-delete-empty-directories (root-directory)
  "Recursively delete empty directories in ROOT-DIRECTORY.

When called from dired, `dired-current-directory' is used for
ROOT-DIRECTORY."

  ;; Interface
  (interactive (list (if (eq major-mode 'dired-mode)
                         (expand-file-name (dired-current-directory))
                       (read-from-minibuffer "Root directory: "))))
  (when (or (null root-directory) (string= "" root-directory))
    (user-error "No root directory provided"))
  (when (called-interactively-p 'interactive)
    (unless (yes-or-no-p (format "Delete all non-empty directories in `%s'? "
                                 root-directory))
      (user-error "Directory `%s' has been left untouched" root-directory)))

  ;; Implementation
  (require 'f)
  (let ((entries (f-directories root-directory)))
    (while entries
      (let ((curdir (car entries)))
        (when (f-directories curdir)
          (*-delete-empty-directories curdir))
        (unless (f-entries curdir)
          (delete-directory curdir)
          (message "Empty directory deleted: %s" curdir))
        (setq entries (cdr entries))))))

(defun *-show-duplicate-lines ()
  (interactive)
  (font-lock-refresh-defaults)
  (let ((hi-lock-mode -1))
    (highlight-lines-matching-regexp
     (concat "^"
             (regexp-quote
              (substring-no-properties
               (thing-at-point 'line) 0 -1))
             "$")
     font-lock-warning-face)))

(defun *-goto-8-2-qa ()
  (interactive)
  (cd "c:/EpicSource/8.2/2015Q3 QA/Obstetrics/Delivery/")
  (find-name-dired))

(defun *-tsv-paste ()
  (interactive)
  (switch-to-buffer "*tsv*")
  (erase-buffer)
  (yank)
  (goto-char 0)
  (while (search-forward "\t" nil t)
    (replace-match " | " nil t))
  (string-rectangle 0 (point-max) "| ")
  (org-table-align)
  (goto-char 3)
  (org-table-hline-and-move)
  (goto-char 0)
  (orgtbl-mode))

(defun *-neo-down-and-next ()
  (interactive)
  (neotree-enter)
  (neotree-next-node))

(defun *-erc-bitlbee ()
  (interactive)
  (erc :server "localhost"
       :port 6667
       :nick "sean"))
(defun *-erc-send-ident ()
  (interactive)
  (erc-send-line "identify ecce-gratum"))
(defun *-swiper-thing-at-point (thing)
  (interactive (list 'symbol))
  (require 'thingatpt)
  (swiper (thing-at-point thing)))
(defun *-eval-and-replace ()
  (interactive)
  (save-excursion
    (eval-last-sexp t))
  (backward-kill-sexp)
  (forward-sexp))
(defun *-devlog-new-entry ()
  (interactive)
  (require 'magit)
  (require 'f)
  (let* ((default-directory (projectile-project-root))
         (rev (magit-get-current-branch))
         (dir "devlog")
         (devlog (f-expand (concat rev *-devlog-ext) dir)))
    (unless (f-exists? dir)
      (make-directory dir))
    (with-current-buffer (find-file devlog)
      (funcall (or *-devlog-major-mode #'text-mode))
      (run-hooks *-devlog-hooks))))
(defun *-make-scope ()
  (interactive)
  (let (needed-exchange)
    (if (region-active-p)
        (progn
          (when (< (mark) (point))
            (setq needed-exchange t)
            (exchange-point-and-mark))
          (insert "{\n")
          (exchange-point-and-mark)
          (insert "\n}")
          (indent-region
           (save-excursion
             (goto-char (mark))
             (forward-line -1)
             (point))
           (save-excursion
             (forward-line 1)
             (point)))
          (unless needed-exchange
            (exchange-point-and-mark))
          (deactivate-mark))
      (insert "\n{")
      (indent-for-tab-command)
      (newline-and-indent)
      (save-excursion
        (insert "\n}")
        (indent-for-tab-command)))))
(defun *-kill-all-buffers (buffers)
  (interactive (list (buffer-list)))
  (if (not buffers) (delete-other-windows)
    (kill-buffer (first buffers))
    (*-kill-all-buffers (rest buffers))))
(defmacro *-and-replace (function-name inner-function)
  `(defun ,function-name (begin end)
     (interactive "r")
     (let* ((input (buffer-substring-no-properties begin end))
            (output (funcall ,inner-function input)))
       (delete-region begin end)
       (insert (if (stringp output) output
                 (format "%S" output))))))

(defvar *-yas-vb-hungtypes-alist
  '(("Double" . "d")
    ("Long" . "l")
    ("Currency" . "cur")
    ("String" . "s")
    ("Date" . "dt")
    ("Boolean" . "Is")))

(defvar *-devlog-hooks nil)
(defvar *-devlog-major-mode nil)
(defvar *-devlog-ext nil)

(defun *-upgrade-all-packages ()
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(*-and-replace eval-and-replace (lambda (s) (eval (read s))))
(*-and-replace calc-eval-region #'calc-eval)

(defun *-epic-files ()
  (interactive)
  (let ((default-directory "~/epic/"))
    (call-interactively #'*-super-completing-find-file)))

(bind-key "C-c e" #'*-epic-files)

(message "Loaded personal functions")

(provide 'my-functions)
