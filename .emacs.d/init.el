(add-to-list 'load-path ".")


;; Windows

(when window-system
 (tool-bar-mode -1)
 (scroll-bar-mode -1))
(when (not window-system)
  (menu-bar-mode -1))


;; Multi-Platform Support

(defconst *-windows-p
  (equal system-type 'windows-nt)
  "Predicate indicating if this is a Windows environment.")
(defconst *-osx-p
  (equal system-type 'darwin)
  "Predicate indicating if this is a OS X environment.")
(defconst *-redhat-p
  (equal system-type 'gnu/linux)
  "Predicate indicating if this is a Redhat environment.")

;; Dropbox
(defconst *-dropbox-directory
  (expand-file-name
   "Dropbox folder"
   (cond
    (*-windows-p "T:")
    (*-osx-p "~")))
  "Dropbox directory")


;; Custom
(load-file
 (setq custom-file
       (expand-file-name ".custom.el"
                         user-emacs-directory)))


;; Package Management

;; use order C-M-S-s-c
(require 'package)

;; Add MELPA before initialization
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(require 'use-package)


;; Auto-Minor-Mode

;; http://stackoverflow.com/a/13946304/1443496
(defvar *-auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions,
see `auto-mode-alist'. All elements of this alist are checked,
meaning you can enable multiple minor modes for the same
regexp.")

(defun *-enable-minor-mode-based-on-extension ()
  "check file name against `*-auto-minor-mode-alist' to enable
minor modes the checking happens for all pairs in
`*-auto-minor-mode-alist'"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist *-auto-minor-mode-alist))
      (setq name (file-name-sans-versions name))
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook #'*-enable-minor-mode-based-on-extension)


;; Creating Temporary Files

;; TODO: to be released on GitHub and published on MELPA
(use-package tempfile
  :load-path "~/github/vermiculus/tempfile.el"
  :bind (("C-x t" . tempfile-find-temporary-file)
         ("C-c k" . tempfile-delete-this-buffer-and-file)))


;; Files and Buffers

(defun *-copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (f)ull, (d)irectory, (b)asename:")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (setq new-kill-string
          (cond ((eq choice ?f) name)
                ((eq choice ?d) (file-name-directory name))
                ((eq choice ?b) (file-name-nondirectory name))
                (t (message "Quit") nil)))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(global-set-key (kbd "C-c x") #'*-copy-buffer-file-name-as-kill)


(defcustom *-delete-trailing-whitespace-on-save
  nil
  "If `t', files will be stripped of trailing whitespace before saving."
  :group '*-files)

(defun *-maybe-delete-trailing-whitespace ()
  (when *-delete-trailing-whitespace-on-save
    (delete-trailing-whitespace)))


(defun unfill-region (beg end)
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;; TODO: (if (buffer-is-visiting file) don't close
(defun file-string (file)
  "Read the contents of a file and return as a string,
closing the file if it was not already open."
  (with-temp-buffer (find-file-noselect file)
    (buffer-string)))

(defun file-lines (file)
  (split-string (file-string file) "\n"))


(use-package m4-mode
  :defer t
  :config
  (progn
    (modify-syntax-entry ?# "@" m4-mode-syntax-table)))


;; Interactive Search

(defun *-isearch-yank-thing-at-point ()
  (interactive)
  (isearch-yank-string (thing-at-point 'symbol)))

(define-key isearch-mode-map (kbd "C-SPC") #'*-isearch-yank-thing-at-point)


;; TeX initialization

(defun *-read-from-minibuffer (prompt &optional default)
  (let ((response
         (read-from-minibuffer
          (concat prompt (if default (format " (default `%s')" default)) ": "))))
    (if response response default)))



(use-package tex
  :ensure auctex
  :config
  (progn
    (defun *-TeX-find-kpathsea (string)
      (interactive)
      (unless string
        (let ((default (thing-at-point 'symbol t)))
          (setq string
                (*-read-from-minibuffer
                 "Find file in TeX distribution"
                 (thing-at-point 'symbol)))))

      (find-file (substring (shell-command-to-string
                             (format "kpsewhich %s" string))
                            0 -1)))

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
                  (find-file-other-window new-file)
                (error "Sorry, the file returned by texdoc for %s isn't readable"
                       texdoc-query)))))))

    (add-to-list
     'TeX-command-list
     '("Arara"
       "arara %(verbose)%s"
       TeX-run-command
       nil                              ; ask for confirmation
       t                                ; active in all modes
       :help "Run Arara")))
  :bind (("C-c ?" . *-TeX-find-texdoc)
         ("C-c M-?" . *-TeX-find-kpathsea)))


;; God-Mode

(use-package god-mode
  :bind ("<escape>" . god-local-mode)
  :config
  (progn
    (add-hook 'god-local-mode-hook
              #'*-god-mode-update-cursor)

    (defcustom *-god-mode-update-cursor-affected-forms
      '(god-local-mode buffer-read-only)
      "If any of these forms evaluate to non-nil, the cursor will change."
      :group '*-god)

    (defcustom *-god-mode-cursor
      'hbar
      "The cursor to use"
      :group '*-god)

    (defun *-god-mode-update-cursor ()
      (setq cursor-type
            (if (member t (mapcar #'eval *-god-mode-update-cursor-affected-forms))
                *-god-mode-cursor
              t)))))



;; Multiple Cursors

(use-package multiple-cursors
  :bind (("C-<"   . mc/mark-previous-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("C-M->" . mc/mark-all-like-this-dwim)))


;; Smex

(use-package smex
  :config
  (progn
    (smex-initialize))
  :bind (("M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands)))


;; Yasnippet

(use-package yasnippet
  :ensure t
  :idle (yas-global-mode)
  :idle-priority 99)


;; C Modes

(use-package cc-mode
  :bind (("C-c RET" . ff-find-related-file)
         ("C-c C-'" . compile)))


;; Company

(use-package company
  :ensure t
  :commands company-mode)


;; Theming

(use-package monokai-theme
  :if window-system
  :config
  (progn
    (enable-theme 'monokai)))


;; Big Brother Insidious Database

(use-package bbdb
  :if window-system)


;; GitHub

(use-package github-clone
  :if window-system)


;; Helm

(use-package helm
  :ensure t
  :if window-system
  :config
  (progn
    (use-package helm-command))
  :bind ("s-x" . helm-M-x))

(use-package helm-ag
  :if window-system
  :bind ("s-f" . helm-do-ag))


;; HTMLize

(use-package htmlize
  :commands (htmlize-buffer htmlize-file))


;; Magit

(use-package magit
  :if window-system
  :bind ("M-m" . magit-status))


;; Python
(use-package nose
  :if window-system
  :commands nose-mode)


;; Org

(use-package org
  :if window-system
  :bind ("C-c c" . org-capture))

(use-package outorg
  :ensure t
  :config
  (progn
    (use-package outshine :ensure t))
  :bind ("M-#" . outorg-edit-as-org))

;; 
;; Lisp

(use-package slime
  :if window-system
  :commands slime
  :config
  (progn
    (setq inferior-lisp-program "clisp")))

(use-package erefactor
  :ensure t)

(use-package lisp-mode
  :config
  (progn
    (add-hook 'lisp-mode-hook
              #'erefactor-highlight-mode)
    (mapc (lambda (f)
            (add-hook 'emacs-lisp-mode-hook f))
          '(paredit-mode
            eldoc-mode
            company-mode
            show-paren-mode))
    (font-lock-add-keywords
     'emacs-lisp-mode
     '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
        font-lock-builtin-face))))
  :bind (("C-x C-e" . pp-eval-last-sexp)
         ("C-x M-e" . pp-macroexpand-last-sexp)))


;; Twitter

(use-package twittering-mode
  :if window-system
  :commands (twit twittering-mode twittering-update-status)
  :config
  (progn
    (defun *-twittering-update-status-from-minibuffer ()
      (interactive)
      (let ((twittering-update-status-function
             #'twittering-update-status-from-minibuffer))
        (twittering-update-status))))
  :bind (("C-c m" . *-twittering-update-status-from-minibuffer)
         ("C-c n" . twittering-update-status-interactive)))


;; YAML

(use-package yaml-mode
  :defer t)


;; Markdown

(use-package markdown-mode
  :defer t)


;; Evil

(use-package evil
  :bind ("C-M-`" . evil-mode))


;; Fish

(use-package fish-mode
  :mode "\\.fish\\'")


;; Ido
(use-package ido
  :config
  (progn
    (use-package flx-ido
      :config
      (progn
        (flx-ido-mode t)))
    (ido-mode t)
    (setq ido-everywhere t)))


;; Browse Files
(use-package bf-mode)


;; CoffeeScript

(use-package coffee-mode
  :mode "\\.coffee\\'")


;; CSV

(use-package csv-mode
  :mode "\\.csv\\'")


;; PATH Setup


(let ((more-paths '("/usr/texbin" "/usr/local/bin")))
  (setenv "PATH"
          (mapconcat #'identity
                     `(,@more-paths
                       ,(getenv "PATH"))
                     path-separator))
  (mapc (lambda (p) (add-to-list 'exec-path p))
        more-paths))


;; Dired

(use-package dired-aux
  :config
  (progn
    (add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip"))))

(use-package dired
  :init (bind-key "z" #'*-dired-zip-files dired-mode-map)
  :config
  (progn
    (defun *-dired-for-each-marked-file (function)
      "Do stuff for each marked file, only works in dired window"
      (interactive)
      (if (eq major-mode 'dired-mode)
          (mapcar function (dired-get-marked-files))
        (error "Not a Dired buffer `%s'" major-mode)))

    (defun *-dired-zip-files (zip-file)
      "Create an archive containing the marked files."
      (interactive "sEnter name of zip file: ")
      (let ((zip-file
             (if (string-match ".zip$" zip-file)
                 zip-file
               (concat zip-file ".zip"))))
        (shell-command
         (concat "zip "
                 zip-file
                 " "
                 (mapconcat (lambda (filename)
                              (file-name-nondirectory filename))
                            (dired-get-marked-files) " "))))

      ;; remove the mark on all the files  "*" to " "
      (revert-buffer)
      ;; mark zip file
      (dired-change-marks 42 ?\040)
      ;;(filename-to-regexp zip-file))
      (dired-mark-files-regexp zip-file))))


;; Ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


;; Projectile

(use-package projectile
  :ensure t
  :defer t
  :config
  (progn
    (projectile-global-mode)
    (add-hook 'projectile-mode-hook #'projectile-rails-on)))
(use-package ag
  :ensure t
  :commands (ag-regexp))
(use-package helm-projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (bind-key "s-f" #'helm-projectile-ag projectile-command-map)))


;;; Impatient Mode

(use-package impatient-mode
  :ensure t)


;;; Expand Region by Semantic Units
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


;;; Speedbar
(use-package speedbar
  :bind ("C-c C-SPC" . speedbar-get-focus))

(use-package ace-jump-mode
  :ensure t
  :config
  (progn
    (define-prefix-command 'my:ace-jump-map)
    (mapc (lambda (x) (define-key my:ace-jump-map
                        (car x) (cadr x)))
          '(("j" ace-jump-mode)
            ("k" ace-jump-char-mode)
            ("l" ace-jump-line-mode)))
    (setq ace-jump-mode-move-keys
          (loop for i from ?a to ?z collect i)))
  :bind (("C-c j" . my:ace-jump-map)))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))


;;; Ruby
(use-package ruby-mode
  ;; Use M-x racr to use `rvm-activate-corresponding-ruby'
  :init (use-package rvm :ensure t)
  :ensure t)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
