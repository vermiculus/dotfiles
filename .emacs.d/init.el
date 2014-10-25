
(require 'package)
(defun *-require-package (pkg)
  (let ((pkg (if (consp pkg) (car pkg) pkg))
        (ftr (if (consp pkg) (cdr pkg) pkg)))
    (when (not (package-installed-p pkg))
      (package-install pkg))
    (require ftr)))
      
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
        twittering-mode
        yaml-mode
        yasnippet
        ))

(defun *-with-map-bind-keys-to-functions (map ft-k-f)
  (when ft-k-f
    (let ((feature (caar ft-k-f))
          (keys (cadar ft-k-f))
          (func (caddar ft-k-f)))
      (eval-after-load feature
        '(define-key map (kbd keys) (eval func)))
      (*-with-map-bind-keys-to-functions map (rest ft-k-f)))))

(defun *-after-feature-set-keys-to-functions (feature k-f)
  (when k-f
    (eval-after-load (if (not (booleanp feature))
                         feature 'emacs)
      (prog1 t
        (global-set-key (kbd (caar k-f)) (eval (cadar k-f)))))
    (*-after-feature-set-keys-to-functions feature (rest k-f))))

(*-with-map-bind-keys-to-functions
 TeX-mode-map
 '((latex "C-c t" #'*-TeX-find-texdoc)))

(*-with-map-bind-keys-to-functions
 c-mode-base-map
 '((find-file "C-c RET" #'ff-find-related-file)
   (cc-mode "C-c C-'" #'compile)))

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

(setq
 *--windows-p (equal system-type 'windows-nt)
 *--osx-p     (equal system-type 'darwin)
 *--redhat-p  (equal system-type 'gnu/linux))

(defun *-create-temporary-file (ext &optional prefix)
  "Creates a temporary file with EXT as the extension."
  (interactive "sExtension: ")
   (make-temp-file
    (concat "temp-file--" prefix) nil
    (concat "." ext)))

(defun *-find-temporary-file (ext &optional prefix)
  (interactive "sExtension: ")
  (find-file (*-create-temporary-file ext prefix)))

(*-with-map-bind-keys-to-functions
 global-map
 '((magit "M-?" #'magit-status)
   (multiple-cursors "C-M->" #'mc/mark-next-like-this)
   (multiple-cursors "C-M-S-r" #'mc/mark-all-like-this-dwim)
   (t "C-x t" #'*-find-temporary-file)
   (t "C-c C-SPC" #'speedbar-get-focus)
   (t "<escape>" #'god-local-mode)
   (t "C-x C-1" #'delete-other-windows)
   (t "C-x C-2" #'split-window-below)
   (t "C-x C-3" #'split-window-right)
   (t "C-x C-0" #'delete-window)))

(*-with-map-bind-keys-to-functions
 isearch-mode-map
 '((t "C-SPC" #'*-isearch-yank-thing-at-point)))

(*-with-map-bind-keys-to-functions
 god-local-mode-map
 '((god-mode "." #'repeat)))

(defun *-isearch-yank-thing-at-point ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))

(eval-after-load 'm4-mode
 (modify-syntax-entry ?# "@" m4-mode-syntax-table))

(load
 (setq custom-file
       (concat user-emacs-directory
               ".custom.el")))
