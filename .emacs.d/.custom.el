(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-latitude 43.008447)
 '(calendar-longitude -89.508943)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(display-time-mode t)
 '(find-file-hook
   (quote
    (auto-insert
     #[nil "\302\301!\210\303\304!8\211\207"
           [buffer-file-name auto-revert-tail-pos make-local-variable 7 file-attributes]
           3]
     global-font-lock-mode-check-buffers epa-file-find-file-hook vc-find-file-hook)))
 '(indent-tabs-mode nil)
 '(ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
 '(magit-use-overlays nil)
 '(org-agenda-files (quote ("c:/Users/sallred/tasks")))
 '(org-capture-templates
   (quote
    (("T" "Stack ToDo" entry
      (file "~/github/vermiculus/stack-mode/todo.org")
      ""))))
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex man md odt org texinfo)))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   (quote
    ((visual-basic-mode-indent . 2)
     (org-export-date-timestamp-format . "$B %e %Y")
     (user-mail-address . "code@seanallred.com")
     (column-number-mode . t))))
 '(sunshine-location "Madison, Wisconsin"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-mode-line-face ((t (:inherit ace-jump-face-foreground))))
 '(erefactor-highlight-face ((t (:inherit match :foreground "yellow1"))))
 '(sx-question-mode-tags ((t (:inherit font-lock-function-name-face :underline nil :slant normal)))))
