(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-goto-char-style (quote at))
 '(avy-style (quote at))
 '(cider-repl-use-pretty-printing t)
 '(confirm-kill-emacs
   (lambda
     (prompt)
     (or
      (not window-system)
      (yes-or-no-p prompt))))
 '(erc-minibuffer-notice t)
 '(erc-notifications-mode t)
 '(erc-track-enable-keybindings t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-showcount nil)
 '(find-file-hook
   (quote
    (auto-insert
     #[nil "\302\301!\210\303\304!8\211\207"
           [buffer-file-name auto-revert-tail-pos make-local-variable 7 file-attributes]
           3]
     global-font-lock-mode-check-buffers epa-file-find-file-hook vc-find-file-hook)))
 '(indent-tabs-mode nil)
 '(magit-use-overlays nil)
 '(neo-dont-be-alone t)
 '(neo-vc-integration (quote (face char)))
 '(org-agenda-files (quote ("~/epic.org")))
 '(org-capture-templates
   (quote
    (("T" "Stack ToDo" entry
      (file "~/github/vermiculus/sx.el/todo.org")
      "")
     ("w" "work" entry
      (file "~/work.org")
      ""))))
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex man md odt org texinfo)))
 '(org-src-fontify-natively t)
 '(projectile-globally-ignored-file-suffixes (quote ("~")))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   (quote
    ((org-export-date-timestamp-format . "$B %e %Y")
     (user-mail-address . "code@seanallred.com")
     (column-number-mode . t))))
 '(save-interprogram-paste-before-kill t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sunshine-location "Madison, WI")
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erefactor-highlight-face ((t (:inherit match :foreground "yellow1"))))
 '(org-verbatim ((t (:inherit shadow))))
 '(sx-question-mode-tags ((t (:inherit font-lock-function-name-face :underline nil :slant normal)))))
