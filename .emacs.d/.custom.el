(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(*-text-mono-type "Source Code Pro")
 '(*-text-sans-type "Avenir Book")
 '(*-text-serif-type "Baskerville Old")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-file (concat user-emacs-directory ".custom.el"))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" default)))
 '(debug-on-error t)
 '(desktop-save-mode t)
 '(display-time-mode t)
 '(doc-view-mode-hook (quote (auto-revert-mode)) t)
 '(find-file-hook
   (quote
    (auto-insert enable-minor-mode-based-on-extension yas-global-mode-check-buffers global-undo-tree-mode-check-buffers undo-tree-load-history-hook mode-local-post-major-mode-change url-handlers-set-buffer-mode global-font-lock-mode-check-buffers epa-file-find-file-hook vc-find-file-hook)))
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(inferior-lisp-program "clisp" t)
 '(inhibit-startup-screen t)
 '(m4-font-lock-keywords
   (quote
    (("\\(\\b\\(m4_\\)?dnl\\b\\).*$" . font-lock-comment-face)
     ("\\$#" . font-lock-variable-name-face)
     ("\\$\\@" . font-lock-variable-name-face)
     ("\\$\\*" . font-lock-variable-name-face)
     ("\\b\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|gnu\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|un\\(d\\(efine\\|ivert\\)\\|ix\\)\\)\\b" . font-lock-keyword-face)
     ("\\b\\(m4_\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(_undefine\\|exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|undivert\\)\\)\\b" . font-lock-keyword-face))) t)
 '(menu-bar-mode (not *--windows-p))
 '(org-edit-src-content-indentation 0)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.melpa.org/packages/"))))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(user-emacs-directory (concat (if *--windows-p "T:" "~") "/.emacs.d/") t)
 '(visible-bell t)
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
