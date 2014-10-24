
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

(setq custom-file (concat user-emacs-directory ".custom.el"))

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)

(defcustom *-god-mode-update-cursor-affected-forms
  '(god-local-mode buffer-read-only)
  "If any of these forms evaluate to non-nil, the cursor will change.")

(defcustom *-god-mode-update-cursor-cursor
  'hbar
  "The cursor to use")

(eval-when-compile (require 'cl))
(defun *--god-mode-update-cursor ()
  (setq cursor-type
        (if (some #'eval *-god-mode-update-cursor-affected-forms)
            *-god-mode-update-cursor-cursor t)))

(mapcar
 (lambda (hook)
   (add-hook hook #'*--god-mode-update-cursor))
 '(god-mode-enabled-hook god-mode-disabled-hook))

(define-key god-local-mode-map (kbd ".") 'repeat)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(defvar *-TeX-find-texdoc-temp-file-format
  "TeX-find-texdoc--%s--")

(defun *-TeX-find-texdoc (texdoc-query)
  (interactive "sPackage: ")
  (if (string-equal texdoc-query "")
      (error "Cannot query texdoc against an empty string")
    (let ((texdoc-output (shell-command-to-string
                          (format "texdoc -l -M %s"
                                  texdoc-query))))
      (if (string-match "no documentation found" texdoc-output)
          (error "Sorry, no documentation found for %s" texdoc-query)
        (let ((texdoc-file (nth 2 (split-string texdoc-output))))
          (if (file-readable-p texdoc-file)
              (let ((new-file (*-create-temporary-file
                               ".pdf"
                               (format *-TeX-find-texdoc-temp-file-format
                                       texdoc-query
                                       texdoc-file))))
                (copy-file texdoc-file new-file t)
                (find-file-other-window new-file))
            (error "Sorry, the file returned by texdoc for %s isn't readable"
                   texdoc-query)))))))

(require 'find-file)
(require 'cc-mode)
(define-key c-mode-base-map (kbd "C-c RET") #'ff-find-related-file)
(define-key c-mode-base-map (kbd "C-c C-'") #'compile)
