
(setq
 *--windows-p (equal system-type 'windows-nt)
 *--osx-p (equal system-type 'darwin)
 *--redhat-p (equal system-type 'gnu/linux))

(defun *-create-temporary-file (ext &optional prefix)
  "Creates a temporary file with EXT as the extension."
  (interactive "sExtension: ")
   (make-temp-file
    (concat "temp-file--" prefix) nil
    (concat "." ext)))

(defun *-find-temporary-file (ext &optional prefix)
  (interactive "sExtension: ")
  (find-file (*-create-temporary-file ext prefix)))

(global-set-key (kbd "C-x t") '*find--temporary-file)

(require 'm4-mode)
(setq m4-font-lock-keywords
      '(("\\(\\b\\(m4_\\)?dnl\\b\\).*$" . font-lock-comment-face)
        ("\\$#" . font-lock-variable-name-face)
        ("\\$\\@" . font-lock-variable-name-face)
        ("\\$\\*" . font-lock-variable-name-face)
        ("\\b\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|gnu\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|un\\(d\\(efine\\|ivert\\)\\|ix\\)\\)\\b" . font-lock-keyword-face)
        ("\\b\\(m4_\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(_undefine\\|exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|undivert\\)\\)\\b" . font-lock-keyword-face)))
(modify-syntax-entry ?# "@" m4-mode-syntax-table)

;(load
 (setq custom-file ".emacs-custom.el");)

(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.melpa.org/packages/") t)

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

(setq custom-file (concat user-emacs-directory ".custom.el"))

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

(defun *--with-map-bind-keys-to-functions (map ft-k-f)
  (when ft-k-f
    (let ((feature (caar ft-k-f))
          (keys (cadar ft-k-f))
          (func (caddar ft-k-f)))
 (mapc #'print (list feature keys func))
      (eval-after-load feature
        '(define-key map (kbd keys) (eval func)))
      (*--with-map-bind-keys-to-functions map (rest ft-k-f)))))

(defun *--after-feature-set-keys-to-functions (feature k-f)
  (when k-f
    (eval-after-load feature
      (prog1 t
        (global-set-key (kbd (caar k-f)) (eval (cadar k-f)))))
    (*--after-feature-set-keys-to-functions feature (rest k-f))))

(global-set-key (kbd "M-?") #'magit-status)

(*--with-map-bind-keys-to-functions
 TeX-mode-map
 '((latex "C-c t" #'*-TeX-find-texdoc)))

(*--with-map-bind-keys-to-functions
 c-mode-base-map
 '((find-file "C-c RET" #'ff-find-related-file)
   (cc-mode "C-c C-'" #'compile)))

(*--after-feature-set-keys-to-functions
 'multiple-cursors
 '(("C-M->" #'mc/mark-next-like-this)
   ("M->" #'mc/mark-next-lines)))

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defcustom *-god-mode-update-cursor-affected-forms
  '(god-local-mode buffer-read-only)
  "If any of these forms evaluate to non-nil, the cursor will change."
  :group 'vermiculus-god)

(defcustom *-god-mode-update-cursor-cursor
  'hbar
  "The cursor to use"
  :group 'vermiculus-god)

(defun *--god-mode-update-cursor ()
  (setq cursor-type
        (if (member t (mapcar #'eval *-god-mode-update-cursor-affected-forms))
            *-god-mode-update-cursor-cursor t)))

(mapc
 (lambda (hook)
   (add-hook hook #'*--god-mode-update-cursor))
 '(god-mode-enabled-hook god-mode-disabled-hook))

(define-key god-local-mode-map (kbd ".") 'repeat)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(defcustom *-TeX-find-texdoc-temp-file-format
  "TeX-find-texdoc--%s--"
  "The prefix for temporary files created with `*-TeX-find-texdoc'"
  :group 'vermiculus-tex)

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

(require 'find-file)
(require 'cc-mode)
