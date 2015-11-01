(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(avy-word-punc-regexp "[!-/:-@[-`{-~]")
 '(blink-matching-delay 0.25)
 '(calendar-latitude 43.008447)
 '(calendar-longitude -89.508943)
 '(caps-lock-mode nil)
 '(compilation-message-face (quote default))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "e2ad5dd396cf0bb3250ece88f82389303c96db87bdf507deab63b0b53891f911" default)))
 '(default-input-method "TeX")
 '(display-time-mode t)
 '(emc2:assume-training-is-ok t)
 '(fci-rule-color "#49483E")
 '(find-file-hook
   (quote
    (auto-insert
     #[nil "\302\301!\210\303\304!8\211\207"
           [buffer-file-name auto-revert-tail-pos make-local-variable 7 file-attributes]
           3]
     global-font-lock-mode-check-buffers epa-file-find-file-hook vc-find-file-hook)))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(indent-tabs-mode nil)
 '(ispell-program-name "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(neo-theme (quote ascii))
 '(org-agenda-files
   (quote
    ("~/tasks" "c:/Users/sallred/admin" "c:/Users/sallred/tasks")))
 '(org-capture-templates
   (quote
    (("a" "Epic QA Note" entry
      (file "~/../../captured-qans")
      (file "~/.emacs.d/org/capture-templates/qa-note.org"))
     ("d" "Idea" entry
      (file "~/../../ideas")
      (file "~/.emacs.d/org/capture-templates/idea.org"))
     ("m" "Question for Mehmet" entry
      (file+headline "~/../../admin" "Weekly Check-In with Mehmet")
      (file "~/.emacs.d/org/capture-templates/question.org")))))
 '(org-epic-unique-properties "")
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex man md odt org texinfo)))
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")
     ("n" "#+NAME: ?" ""))))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   (quote
    ((org-id-link-to-org-use-id . t)
     (org-log-into-drawer . t)
     (org-attach-directory . "c:/users/sallred/data/")
     (visual-basic-mode-indent . 2)
     (org-export-date-timestamp-format . "$B %e %Y")
     (user-mail-address . "code@seanallred.com")
     (column-number-mode . t))))
 '(sunshine-location "Madison, Wisconsin")
 '(truncate-lines t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:foreground "gold2"))))
 '(org-verbatim ((t (:inherit shadow :foreground "gold1"))))
 '(variable-pitch ((t (:weight bold)))))
