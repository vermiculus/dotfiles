(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#757575" "#CD5542" "#4A8F30" "#7D7C21" "#4170B3" "#9B55C3" "#68A5E9" "gray43"])
 '(avy-goto-char-style (quote at))
 '(avy-style (quote at-full))
 '(avy-styles-alist (quote ((ivy-avy . at-full))))
 '(avy-word-punc-regexp "[!-/:-@[-`{-~]")
 '(blink-cursor-mode nil)
 '(blink-matching-delay 0.25)
 '(c-mode-common-hook nil)
 '(cider-repl-use-pretty-printing t)
 '(compilation-message-face (quote default))
 '(confirm-kill-emacs
   (lambda
     (prompt)
     (or
      (not window-system)
      (yes-or-no-p prompt))))
 '(counsel-find-file-ignore-regexp nil)
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "ee89863f86247942d9fc404d47b2704475b146c079c1dcd2390d697ddfa9bdf4" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "33969ab092ac7ab559e44bfbc1c4ec95f73c8230914ea18bb70e0044047967aa" "5436e5df71047d1fdd1079afa8341a442b1e26dd68b35b7d3c5ef8bd222057d1" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "e2ad5dd396cf0bb3250ece88f82389303c96db87bdf507deab63b0b53891f911" default)))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(display-time-mode t)
 '(doc-view-resolution 100)
 '(erc-minibuffer-notice t)
 '(erc-notifications-mode t)
 '(erc-track-enable-keybindings t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-showcount nil)
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(indent-tabs-mode nil)
 '(ivy-fixed-height-minibuffer nil)
 '(ivy-format-function (quote ivy-format-function-arrow))
 '(magit-commit-arguments (quote ("--gpg-sign=1184707BCA088EBD")))
 '(magit-diff-section-arguments (quote ("--no-ext-diff")))
 '(magit-diff-use-overlays nil)
 '(magit-popup-show-common-commands nil)
 '(magit-popup-show-help-echo nil)
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-use-overlays nil)
 '(magithub-clone-default-directory "~/github/")
 '(magithub-label-color-replacement-alist
   (quote
    (("#0052cc" . "DeepSkyBlue1")
     ("0052cc" . "LightGreen")
     ("#5319e7" . "magenta")
     ("#128A0C" . "green"))) nil nil "Auto-saved by `magithub-label-color-replace'")
 '(magithub-preferred-remote-method (quote ssh_url))
 '(mouse-scroll-delay 1)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(neo-dont-be-alone t)
 '(neo-theme (quote ascii))
 '(neo-vc-integration (quote (face char)))
 '(org-agenda-files
   (quote
    ("~/epic/on-site.org" "~/epic/automation.org" "~/everything.org")))
 '(org-capture-templates
   (quote
    (("li" "Lumps Idea" entry
      (file "~/lumps/idea-bin.org")
      "")
     ("ls" "Lumps Syntax" entry
      (file "~/lumps/unfiled-syntax.org")
      ""))))
 '(org-export-backends
   (quote
    (ascii beamer html icalendar latex man md odt org texinfo)))
 '(org-src-fontify-natively t)
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
 '(package-selected-packages
   (quote
    (lsp-python package-build buttercup clojure-mode multiple-cursors helpful vlf lispy avy company counsel hydra magit org paredit projectile typescript-mode yasnippet easy-hugo hierarchy emojify sudoku company-lsp lsp-rust diminish auto-compile visual-fill-column yarn-mode edit-indirect pcache nameless lively graphql-mode suggest dumb-jump dante haskell-mode ert-runner flycheck-package vagrant vagrant-tramp xmlgen sly sly-company sly-quicklisp sly-repl-ansi-color package-lint help-fns+ feature-mode ediprolog sicp sml-mode edit-server kaesar-file omnisharp benchmark-init markdownfmt markdown-mode markdown-mode+ markdown-preview-mode company-racer racer toml-mode shut-up epl git commander f dash cargo cask cask-package-toolset caskxy rust-mode rustfmt fm cmake-mode company-irony company-irony-c-headers flycheck-irony irony minimap ample-theme exec-path-from-shell company-go guide-key free-keys cask-mode gh marshal s cl-lib git-commit unidecode wc-goal-mode go-mode counsel-osx-app sql-indent org-projectile counsel-projectile emmet-mode beacon org-bullets kpm-list vline bitlbee zone-nyan yaml-mode xbm-life xahk-mode wgrep-ag vimish-fold use-package twittering-mode sunshine stash smex slime scss-mode sass-mode rvm robe rainbow-mode rainbow-blocks psvn paredit-menu paredit-everywhere pacmacs ox-twbs osx-lib org-jekyll oauth2 nyan-prompt nyan-mode nose neotree names monokai-theme magit-svn lua-mode lorem-ipsum key-chord json-rpc jekyll-modes indent-guide impatient-mode hyde helm-swoop helm-spotify helm-rails helm-package helm-gtags helm-commandlinefu helm-bibtex helm-ag gscholar-bibtex goto-last-change golden-ratio god-mode gitignore-mode github-clone github-browse-file gitconfig-mode git-link ggtags font-lock-studio flx-ido fish-mode expand-region evil erefactor erc-terminal-notifier emacsshot edbi-sqlite dirtree demo-it define-word debbugs darkroom csv-mode csharp-mode company-restclient company-c-headers company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode chicken-scheme caps-lock btc-ticker bf-mode bbdb auctex aggressive-indent ag ace-window ace-jump-mode @ 4clojure 2048-game)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-globally-ignored-file-suffixes (quote ("~")))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   (quote
    ((python-shell-interpreter . "python3")
     (column-number-mode . t)
     (user-mail-address . "code@seanallred.com"))))
 '(save-interprogram-paste-before-kill t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sunshine-location "Madison, WI")
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
 '(visible-bell t)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erefactor-highlight-face ((t (:inherit match :foreground "yellow1"))))
 '(org-code ((t (:foreground "gold2" :underline t))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#FD971F" :height 1.3))))
 '(org-verbatim ((t (:inherit shadow :foreground "gold1"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "dark green"))))
 '(sx-question-mode-content-face ((t (:background "#090909"))))
 '(sx-question-mode-tags ((t (:inherit font-lock-function-name-face :underline nil :slant normal))))
 '(variable-pitch ((t (:weight bold)))))
