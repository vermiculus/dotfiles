
(require 'package)
(defun *-require-package (pkg)
  (let ((pkg (if (consp pkg) (car pkg) pkg))
        (ftr (if (consp pkg) (cdr pkg) pkg)))
    (when (not (package-installed-p pkg))
      (package-install pkg))
    (require ftr)))

(package-initialize)      
(mapc #'*-require-package
      '(
        (auctex . latex)
        bbdb
        bf-mode
        coffee-mode
        color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow
        csv-mode
        evil
        fish-mode
        flx-ido
        github-clone
        god-mode
        helm
        helm-ag
        htmlize
        magit
        monokai-theme
        multiple-cursors
        nose
        org
        slime
        smex
        twittering-mode
        yaml-mode
        yasnippet
        ))

(defun *-with-map-bind-keys-to-functions (map ft-k-f)
  (when ft-k-f
    (let ((feature (caar ft-k-f))
          (keys   (cadar ft-k-f))
          (func  (caddar ft-k-f)))
      (eval-after-load (if (not (booleanp feature)) feature 'emacs)
        `(define-key map ,(kbd keys) (function ,func)))
      (*-with-map-bind-keys-to-functions map (rest ft-k-f)))))

(defun *-after-feature-set-keys-to-functions (feature k-f)
  (when k-f
    (eval-after-load (if (not (booleanp feature)) feature 'emacs)
      (prog1 t
        (global-set-key (kbd (caar k-f)) (eval (cadar k-f)))))
    (*-after-feature-set-keys-to-functions feature (rest k-f))))

(*-with-map-bind-keys-to-functions
 TeX-mode-map
 '((latex "C-c t" *-TeX-find-texdoc)))

(defvar c-mode-base-map)
(eval-after-load 'cc-mode
  '(*-with-map-bind-keys-to-functions
    c-mode-base-map
    '((find-file "C-c RET" ff-find-related-file)
      (cc-mode "C-c C-'" compile))))

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defcustom *-god-mode-update-cursor-affected-forms
  '(god-local-mode buffer-read-only)
  "If any of these forms evaluate to non-nil, the cursor will change."
  :group '*-god)

(defcustom *-god-mode-update-cursor-cursor
  'hbar
  "The cursor to use"
  :group '*-god)

(defun *--god-mode-update-cursor ()
  (setq cursor-type
        (if (member t (mapcar #'eval *-god-mode-update-cursor-affected-forms))
            *-god-mode-update-cursor-cursor t)))

(mapc
 (lambda (hook)
   (add-hook hook #'*--god-mode-update-cursor))
 '(god-mode-enabled-hook god-mode-disabled-hook))

(define-key god-local-mode-map (kbd ".") 'repeat)

(defcustom *-TeX-find-texdoc-temp-file-format
  "TeX-find-texdoc--%s--"
  "The prefix for temporary files created with `*-TeX-find-texdoc'"
  :group '*-tex)

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
              (let ((new-file (*-create-temporary-file
                               "pdf"
                               (format *-TeX-find-texdoc-temp-file-format
                                       texdoc-query
                                       texdoc-file))))
                (copy-file texdoc-file new-file t)
                (find-file-other-window new-file))
            (error "Sorry, the file returned by texdoc for %s isn't readable"
                   texdoc-query)))))))

(defcustom *-text-sans-type
  "Arial"
  "The type to use for sans-serif body text."
  :group '*-fonts)

(defcustom *-text-serif-type
  "Georgia"
  "The type to use for sans-serif body text."
  :group '*-fonts)

(defcustom *-text-mono-type
  "Courier"
  "The type to use for sans-serif body text."
  :group '*-fonts)

(set-frame-font *-text-mono-type)

(defconst *--windows-p
  (equal system-type 'windows-nt)
  "Predicate indicating if this is a Windows environment.")
(defconst *--osx-p
  (equal system-type 'darwin)
  "Predicate indicating if this is a OS X environment.")
(defconst *--redhat-p
  (equal system-type 'gnu/linux)
  "Predicate indicating if this is a Redhat environment.")

;; Prepare a list of conses - see docstring
;; http://stackoverflow.com/a/13946304/1443496
(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
      see `auto-mode-alist'. All elements of this alist are checked,
      meaning you can enable multiple minor modes for the same regexp.")

;; Create a hook
(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
       the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(defun *-create-temporary-file (ext &optional prefix)
  "Creates a temporary file with EXT as the extension."
  (interactive "sExtension: ")
   (make-temp-file
    (concat "temp-file--" prefix) nil
    (concat "." ext)))

(defun *-find-temporary-file (ext &optional prefix)
  (interactive "sExtension: ")
  (find-file (*-create-temporary-file ext prefix)))

(defun *-smex-smart-smex ()
  (interactive)
  (or (boundp 'smex-cache)
      (smex-initialize))
  (global-set-key (kbd "M-x") 'smex)
  (smex))

(defun *-smex-smart-smex-major-mode-commands ()
  (interactive)
  (or (boundp 'smex-cache)
      (smex-initialize))
  (global-set-key (kbd "M-S-x") 'smex-major-mode-commands)
  (smex-major-mode-commands))

(*-with-map-bind-keys-to-functions
 global-map
 '((magit "M-?" magit-status)
   (multiple-cursors "C-M->" mc/mark-next-like-this)
   (multiple-cursors "C-M-S-r" mc/mark-all-like-this-dwim)
   (t "C-x t" *-find-temporary-file)
   (t "C-c C-SPC" speedbar-get-focus)
   (t "<escape>" god-local-mode)
   (t "C-x C-1" delete-other-windows)
   (t "C-x C-2" split-window-below)
   (t "C-x C-3" split-window-right)
   (t "M-x" *-smex-smart-smex)
   (t "M-S-x" *-smex-smart-smex-major-mode-commands)
   (t "C-x C-0" delete-window)))

(*-with-map-bind-keys-to-functions
 isearch-mode-map
 '((t "C-SPC" *-isearch-yank-thing-at-point)))

(*-with-map-bind-keys-to-functions
 god-local-mode-map
 '((god-mode "." repeat)))

(defun *-isearch-yank-thing-at-point ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))

(defvar m4-mode-syntax-table)
(eval-after-load 'm4-mode
 '(modify-syntax-entry ?# "@" m4-mode-syntax-table))

(load
 (setq custom-file
       (concat user-emacs-directory
               ".custom.el")))
