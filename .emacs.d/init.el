(setq gc-cons-threshold 100000000
      debug-on-error t)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000
         debug-on-error nil)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))


;; Package Management

;; use order C-M-S-s-c
(require 'package)

;; Add MELPA before initialization
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package package
  :config
  (setq package-archive-priorities
        '(("gnu" . 0)
          ("melpa" . 10))))


;;; General Setup
(setq user-mail-address "code@seanallred.com"
      *-devlog-major-mode #'markdown-mode
      *-devlog-ext ".md")

(add-to-list 'load-path ".")
(prefer-coding-system 'utf-8)

(require 'my-functions)
(require 'bind-key)

(when *-windows-p
  (setq-default default-directory (expand-file-name "../.." (getenv "HOME")))
  (setq find-program "cygwin-find"))

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (*-load-customizations))

(when (or *-windows-p
          (not window-system))
  (menu-bar-mode -1))

(bind-keys ("C-S-j" . join-line)
           ("C-x C-f" . *-super-completing-find-file)
           ("C-c g" . helm-google-suggest)
           ("C-M-=" . *-upgrade-all-packages)
           ("C-x C-M-r" . *-eval-and-replace)
           ("C-c x" . *-copy-buffer-file-name-as-kill)
           ("C-x M-u" . capitalize-region)
           ("C-c L" . *-load-customizations))
(bind-keys :map isearch-mode-map
           ("C-SPC" . *-isearch-yank-thing-at-point))

(dolist (c (list upcase-region downcase-region
                 narrow-to-region narrow-to-page
                 scroll-left))
  (put c 'disabled nil))


(add-hook 'before-save-hook #'delete-trailing-whitespace)
(auto-fill-mode)
(column-number-mode 1)


;;; Packages

(use-package auto-minor-mode
  :disabled t
  :load-path "my-packages/")

(use-package tempfile
  ;; @TODO: to be released on GitHub and published on MELPA
  :load-path "~/github/vermiculus/tempfile.el"
  :bind (("C-c t" . tempfile-find-temporary-file)
         ("C-c k" . tempfile-delete-this-buffer-and-file)))

(use-package m4-mode
  :defer t
  :config
  (modify-syntax-entry ?# "@" m4-mode-syntax-table))

(use-package tex
  :ensure auctex
  :config
  (add-to-list
   'TeX-command-list
   '("Arara"
     "arara %s"
     TeX-run-command
     nil                              ; ask for confirmation
     t                                ; active in all modes
     :help "Run Arara"))
  :bind (("C-c ?" . *-TeX-find-texdoc)
         ("C-c M-?" . *-TeX-find-kpathsea)))

(use-package expl3
  :after tex
  :if *-osx-p
  :load-path "~/github/auctex/")

(use-package reftex
  :after tex
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package latex
  :after tex
  :config
  (add-to-list 'LaTeX-font-list
               '(?\C-a "\\Algorithm{" "}")))

(use-package god-mode
  :bind (("<escape>" . god-local-mode)
         :map god-local-mode-map
         ("[" . backward-page)
         ("]" . forward-page))
  :config
  (add-hook 'god-local-mode-hook
            #'*-god-mode-update-cursor))

(use-package multiple-cursors
  :defines (mc/keymap)
  :bind (("C-<"   . mc/mark-previous-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("C-M->" . mc/mark-all-like-this-dwim)
         :map mc/keymap
         ("M-i" . mc/insert-numbers)
         ("C-v" . mc/cycle-forward))
  :config
  (require 'mc-cycle-cursors))

(use-package smex
  :disabled t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands)))

(use-package yasnippet
  :if window-system
  :diminish yas-minor-mode
  :commands yas-global-mode
  :config
  (add-hook 'typescript-mode-hook #'yas-minor-mode-on))

(use-package cc-mode
  :bind (("C-c RET" . ff-find-related-file)
         ("C-c C-'" . compile)
         :map c-mode-map
         ("C-{" . #'*-make-scope)))

(use-package ggtags
  :disabled t
  :after cc-mode
  :if *-osx-p
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))

(use-package company
  :if window-system
  :commands (company-mode company-mode-on)
  :diminish company-mode
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort)
              ("C-M-SPC" . company-complete))
  :init
  (add-hook 'prog-mode-hook #'company-mode-on)
  (mapc (lambda (s) (add-hook s #'company-mode-on))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook
          ruby-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook)))

(use-package bbdb
  :if window-system)

(use-package github-clone
  :if window-system)

(use-package helm
  :init (require 'helm-config)
  :disabled t
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t)
  :bind
  (("C-x C-a" . helm-command-prefix)
   ("C-x C-a C-o" . helm-org-in-buffer-headings)
   ("C-x C-a C-s" . helm-swoop)
   ("C-c g" . helm-google-suggest)
   ("C-x b" . helm-mini)
   ("M-y"   . helm-show-kill-ring)
   ("C-x c C-o" . helm-org-in-buffer-headings)))

(use-package helm-ag
  :bind (("<f12>" . helm-do-ag)
         ("s-f" . helm-do-ag))
  :config
  (when *-windows-p
    (custom-set-variables
     '(helm-ag-base-command "pt --nocolor --nogroup"))))

(use-package nose
  :if window-system
  :commands nose-mode)

(use-package org
  :if window-system
  :config
  (require 'org-id)
  (add-hook 'org-src-mode-hook #'hack-local-variables)
  (require 'org-agenda)
  (org-add-agenda-custom-command
   '("d" "Agenda + Next Actions"
     ((agenda) (todo "NEXT"))))
  (add-to-list 'org-file-apps
               (cons (rx "." (or "doc" "xls" "ppt") (opt "x") string-end)
                     'default))
  (setq org-src-fontify-natively t
        org-id-link-to-org-use-id t
        org-publish-project-alist '(("blog"
                                     :base-directory "~/blog-twbs/org/"
                                     :publishing-directory "~/blog-twbs/public-html/"
                                     :publishing-function org-twbs-export-to-html
                                     :with-sub-superscript nil))
        org-hide-emphasis-markers t
        org-export-async-debug nil)
  (require 'ox-twbs)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("M-<apps>" . *-org-agenda-next-items)
         ("M-n" . org-metadown)
         ("M-p" . org-metaup)))

(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (mapc (lambda (todo) (add-to-list 'org-agenda-files todo))
        (org-projectile:todo-files))
  (add-to-list 'org-capture-templates
               (org-projectile:project-todo-entry "p"))
  :bind (("C-c p n" . org-projectile:capture-for-current-project)))

(use-package outorg
  :if window-system
  :config
  :bind ("M-#" . outorg-edit-as-org))

(use-package outshine
  :after outorg)

(use-package slime
  :if window-system
  :commands slime
  :config
  (setq inferior-lisp-program "clisp"))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package erefactor
  :if window-system
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . erefactor-map))
  :config
  (add-hook 'emacs-lisp-mode-hook #'erefactor-lazy-highlight-turn-on))

(use-package eldoc
  :if window-system
  :diminish eldoc-mode
  :config
  (mapc (lambda (s) (add-hook s #'eldoc-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook)))

(use-package paredit
  :diminish paredit-mode
  :config
  (mapc (lambda (s) (add-hook s #'paredit-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          ielm-mode-hook
          cask-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook)))

(use-package cider
  :config
  :bind (:map cider-mode-map
              ("C-c C-c" . *-cider-connect)
              :map cider-inspector-mode-map
              ("i" . cider-inspector-operate-on-point)
              ("m" . cider-inspector-operate-on-point)
              ("/" . cider-inspector-pop)
              ("'" . cider-inspector-pop)
              ("n" . cider-inspector-next-inspectable-object)
              ("p" . cider-inspector-previous-inspectable-object)))

(use-package aggressive-indent
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'sh-mode)
  (aggressive-indent-global-mode))

(use-package clj-refactor
  :config
  (setq cljr-suppress-middleware-warnings t)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package lisp-mode
  :config
  (mapc (lambda (s) (add-hook s #'show-paren-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook))
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
      font-lock-builtin-face)))
  :bind (("C-x C-e" . pp-eval-last-sexp)
         ("C-x M-e" . pp-macroexpand-last-sexp)))

(use-package ielm
  :if window-system
  :config
  (add-hook 'ielm-mode-hook #'show-paren-mode))

(use-package twittering-mode
  :if window-system
  :commands (twit twittering-mode twittering-update-status)
  :config
  (setq twittering-use-master-password t)
  :bind (("C-c m" . *-twittering-update-status-from-minibuffer)
         ("C-c n" . twittering-update-status-interactive)))

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook #'*-set-replacements)
  (bind-key "M-M" #'*-insert-post-url markdown-mode-map))

(use-package evil
  :bind ("C-M-`" . evil-mode))

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package ido
  :disabled t
  :config
  (ido-mode t)
  (setq ido-everywhere t))

(use-package flx-ido
  :disabled t
  :after ido
  :config
  (flx-ido-mode t))

(use-package coffee-mode
  :disabled t
  :mode "\\.coffee\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package dired
  :config
  (bind-keys :map dired-mode-map
             ("/" . dired-up-directory)
             ("z" . *-dired-zip-files))
  (when *-osx-p
    (customize-set-value
     'insert-directory-program "gls"
     "Use ls from core-utils")))

(use-package dired-aux
  :after dired
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package bf-mode
  :after dired)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package projectile
  :if window-system
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (when *-osx-p
    (setq projectile-generic-command
          (concat "g" projectile-generic-command)))
  (projectile-global-mode))

(use-package helm-projectile
  :disabled t
  :config
  (setq projectile-completion-system 'helm)
  (bind-key "s-f" #'helm-projectile-ag projectile-command-map))

(use-package ag
  :commands (ag-regexp)
  :config
  (bind-key "s-f" #'helm-projectile-ag projectile-command-map))

(use-package hyde)

(use-package impatient-mode
  :if window-system)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package speedbar
  :if window-system
  :bind ("C-c C-SPC" . speedbar-get-focus))

(use-package ace-jump-mode
  :disabled t
  :config
  (bind-keys :prefix-map my:ace-jump-map
             :prefix "C-c j"
             ("j" . ace-jump-mode)
             ("k" . ace-jump-char-mode)
             ("l" . ace-jump-line-mode))
  (setq ace-jump-mode-move-keys
        (loop for i from ?a to ?z collect i)))

(use-package key-chord
  :after avy
  :config
  (key-chord-define-global ";o" #'avy-goto-word-1)
  (key-chord-define-global ";'" #'avy-goto-word-0)
  (key-chord-define-global ";k" #'avy-goto-char)
  (key-chord-define-global ";l" #'avy-goto-line)
  (key-chord-mode 1))

(use-package avy
  :config
  (setq avy-style 'at-full)
  (bind-keys :prefix-map my:avy-jump-map
             :prefix "C-c C-'"
             ("C-'" . avy-goto-word-0)
             ("C-;" . avy-goto-char)
             ("C-k" . avy-goto-char-2)
             ("C-l" . avy-goto-line))
  (bind-key "C-'" #'avy-isearch isearch-mode-map))

(use-package golden-ratio
  :disabled t
  :config (add-to-list 'golden-ratio-extra-commands 'aw--callback))

(use-package ruby-mode
  ;; Use M-x racr to use `rvm-activate-corresponding-ruby'
  :disabled t
  :if window-system
  :defer t)

(use-package rvm
  :after ruby-mode)
(use-package robe
  :after ruby-mode)
(use-package projectile-rails
  :after ruby-mode
  :disabled t
  :config
  (add-hook 'projectile-mode-hook #'projectile-rails-on))

(use-package vb6-mode
  :load-path "~/dotfiles/.emacs.d/my-packages/"
  :config
  (add-to-list 'auto-mode-alist `(,(rx "EpicSource/" (* not-newline) (or "frm" "ctl" "bas" "cls")) 'visual-basic-mode))
  (setq visual-basic-mode-indent 2
        visual-basic-ide-pathname "C:/Program Files (x86)/Microsoft Visual Studio/VB98/VB6.EXE"))

(use-package mumps-mode
  :load-path "~/dotfiles/.emacs.d/my-packages/")

(use-package org-epic
  :if *-windows-p
  :load-path "c:/users/sallred/git/org-epic/"
  :bind (("C-c e" . org-epic:emc2:edit-from-org-task)
         ("C-c E" . org-epic:emc2:edit)
         ("C-c b" . org-epic:brainbow:open-course)
         ("C-c B" . org-epic:brainbow:open-course-by-id)))

(use-package epic-vb6
  :if *-windows-p
  :load-path "c:/Users/sallred/git/epic-vb6/")

(use-package caps-lock
  :disabled t
  :bind ("C-]" . caps-lock-mode))

(use-package xahk-mode
  :if *-windows-p)

(use-package define-word
  :bind ("M-'" . define-word-at-point))

(use-package evil
  :config
  (setq evil-symbol-word-search t))

(use-package sunshine
  :disabled t
  :config
  (setq sunshine-location "Madison, WI"))

(use-package neotree
  :bind ("s-d" . neotree)
  :config
  (setq neo-dont-be-alone t
        neo-theme 'nerd)
  (bind-keys :map neotree-mode-map
             ("u" . neotree-select-up-node)
             ;;("d" . *-neo-down-and-next)
             ("i" . neotree-enter)
             ("K" . neotree-delete-node)))

(use-package eshell
  :bind ("s-s" . eshell))

(use-package erc
  :config
  (setq erc-autojoin-channels-alist
        '(("localhost" "kat")))
  :bind (("s-e" . *-erc-bitlbee)
         :map erc-mode-map
         ("s-." . *-erc-send-ident)))

(use-package sx
  :if *-osx-p
  :config
  (bind-keys :prefix-map *-sx-map
             :prefix "s-x"
             ("i" . sx-inbox)
             ("s" . sx-tab-all-questions)))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex)
          (t . ivy--regex-fuzzy))))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (setq counsel-find-file-ignore-regexp
        (rx (or (: bos (* any) "~" eos)
                (: bos (* any) "." (or "tiff" "png") eos))))
  :bind (("C-x C-r" . counsel-recentf)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (when *-windows-p
    (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
    (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH"))))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package counsel-projectile
  :after counsel
  :bind ("C-x C-p" . counsel-projectile-switch-project))

(use-package counsel-osx-app
  :after counsel
  :bind ("s-o" . counsel-osx-app))

(use-package swiper
  :ensure t
  :init
  (require 'ivy)
  :bind (("C-s" . swiper)
         ("C-#" . *-swiper-thing-at-point)))

(use-package help
  :config
  (bind-keys :map help-mode-map
             ("[" . help-go-back)
             ("]" . help-go-forward)))

(use-package go-mode)
(use-package company-go
  :after go-mode company
  :commands company-go
  :config
  (add-to-list 'company-backends #'company-go))

(use-package projectile
  :config
  (bind-key "[" #'*-devlog-new-entry projectile-command-map))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :if window-system
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (when *-windows-p
    (setq magit-git-executable "git.exe")
    (add-to-list 'exec-path "C:/cygwin64/bin"))
  :bind ("M-m" . magit-status))

(use-package magithub
  :after magit
  :init (shell-command "cd ~/github/magithub; make clean-elc")
  :load-path "~/github/magithub"
  :config
  (bind-key
   "s-."
   (lambda () (interactive)
     (find-file "~/github/magithub/magithub.el")
     (eval-buffer))))

(use-package imenu
  :bind (("C-c C-," . imenu)))

(use-package hi-lock
  :bind (("C-x w ." . highlight-symbol-at-point)
         ("C-x w u" . unhighlight-regexp)))

(use-package css-mode
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package windmove
  :bind (("S-<right>" . windmove-right)
         ("S-<left>" . windmove-left)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))


;;; Themes

(use-package monokai-theme
  :if window-system
  :disabled t
  :config
  (enable-theme 'monokai))

(use-package ample-theme
  :init
  (load-theme 'ample t t)
  (load-theme 'ample-flat t t)
  (load-theme 'ample-light t t)
  (enable-theme 'ample)
  :defer t
  :ensure t)

;; Local Variables:
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
