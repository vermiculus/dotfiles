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

(add-hook 'find-file-hook
          #'*-enable-minor-mode-based-on-extension)


;; Creating Temporary Files

;; @TODO: to be released on GitHub and published on MELPA
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

(ignore
 (quote
  (defun *-copy-buffer-file-name-as-kill (&optional scope pos-style)
    "Copy the buffer-file-name to the kill-ring.

SCOPE must be one of

  nil
     Copy the full name (/path/to/file)

  `directory'
     Copy the directory part (/path/to)

  `basename'
     Copy the basename (file)


POS-STYLE must be one of

  nil
     No styling (file)

  `line'
     Get the line number (file:505)

  `line-column'
     Get both the line and column number (file:505c30)

  `point'
     Get the value of point (file:40053)"
    (interactive
     (cdr (assoc (prompt user for char)
                 '((?f . full)
                   (?d . directory)
                   (?b . basename))))
     (when prefix-arg
       (cdr (assoc (prompt user for char)
                   '((?n . nil)
                     (?l . line)
                     (?c . line-column)
                     (?p . point))))))
    ;; @todo error up here

    (let* ((name (if (eq major-mode 'dired-mode)
                     (dired-get-filename)
                   (or (buffer-file-name)
                       (user-error "Invalid context"))))
           (file-part
            (cond ((equal scope 'full)
                   name)
                  ((equal scope 'directory)
                   (file-name-directory name))
                  ((equal scope 'basename)
                   (file-name-nondirectory name))
                  ((null scope) nil)
                  (t (error "Invalid scope %S" scope))))
           ;; @todo can make this whole part a lot easier with a clever use of
           ;; assoc.  (when pos-style (cdr (assoc pos-style ...)))
           (pos-part
            (when pos-style
              (concat
               ":"
               (cond ((equal pos-style 'line)
                   ;; @todo return absolute line number
                      (line-number-at-pos))
                     ((equal pos-style 'line-column)
                      (concat (line-number-at-pos)
                              "c"
                              (current-column)))
                     ((equal pos-style 'point)
                      (point))
                     (t (error "Invalid style %S" pos-style))))))
           (new-kill-string (concat file-part pos-part)))
      (when new-kill-string
        (message "%s copied" new-kill-string)
        (kill-new new-kill-string))))))

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
  :ensure t
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
  :ensure t
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
  :ensure t
  :bind (("C-<"   . mc/mark-previous-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("C-M->" . mc/mark-all-like-this-dwim)))


;; Smex

(use-package smex
  :ensure t
  :config
  (progn
    (smex-initialize))
  :bind (("M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands)))


;; Yasnippet

(use-package yasnippet
  :ensure t
  :if window-system
  :diminish yas-minor-mode
  :commands yas-global-mode)


;; C Modes

(use-package cc-mode
  :ensure t
  :bind (("C-c RET" . ff-find-related-file)
         ("C-c C-'" . compile)))


;; Company

(use-package company
  :ensure t
  :if window-system
  :commands company-mode
  :diminish company-mode
  :config
  (progn
    (add-hook 'prog-mode-hook #'company-mode-on)
    (mapc (lambda (s) (add-hook s #'company-mode-on))
          '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook
            ielm-mode-hook
            ruby-mode-hook))))


;; Theming

(use-package monokai-theme
  :ensure t
  :if window-system
  :config
  (progn
    (enable-theme 'monokai)))


;; Big Brother Insidious Database

(use-package bbdb
  :ensure t
  :if window-system)


;; GitHub

(use-package github-clone
  :ensure t
  :if window-system)


;; Helm

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (helm-mode))
  :config
  (progn
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (use-package helm-swoop
      :ensure t
      :bind ("C-S-s" . helm-swoop)
      :commands (helm-swoop helm-swoop-from-isearch)
      :init
      (progn
        (bind-key "C-s" #'helm-swoop helm-command-prefix)
        (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)))
    (use-package helm-ag
      :ensure t
      :bind ("s-f" . helm-do-ag)))
  :bind (("s-x"   . helm-M-x)
         ("C-c g" . helm-google-suggest)
         ("C-x b" . helm-mini)
         ;; use `ag' for recursive grep?  is it possible?
         ("C-x C-f" . helm-find-files)
         ("M-y"   . helm-show-kill-ring)))


;; HTMLize

(use-package htmlize
  :ensure t
  :commands (htmlize-buffer htmlize-file))


;; Magit

(use-package magit
  :ensure t
  :if window-system
  :diminish magit-auto-revert-mode
  :bind ("M-m" . magit-status))


;; Python
(use-package nose
  :ensure t
  :if window-system
  :commands nose-mode)


;; Org

(use-package org
  :ensure t
  :if window-system
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)))

(use-package outorg
  :ensure t
  :if window-system
  :config
  (progn
    (use-package outshine :ensure t))
  :bind ("M-#" . outorg-edit-as-org))

;; 
;; Lisp

(use-package slime
  :ensure t
  :if window-system
  :commands slime
  :config
  (progn
    (setq inferior-lisp-program "clisp")))

(use-package erefactor
  :ensure t
  :if window-system
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'erefactor-lazy-highlight-turn-on)
    (bind-key "C-c C-d" erefactor-map emacs-lisp-mode-map)))

(use-package eldoc
  :ensure t
  :if window-system
  :diminish eldoc-mode
  :config
  (progn
    (mapc (lambda (s) (add-hook s #'turn-on-eldoc-mode))
          '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook
            ielm-mode-hook))))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (progn
    (mapc (lambda (s) (add-hook s #'paredit-mode))
          '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook
            ielm-mode-hook))))

(use-package lisp-mode
  :config
  (progn
    (mapc (lambda (s) (add-hook s #'show-paren-mode))
          '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook))
    (font-lock-add-keywords
     'emacs-lisp-mode
     '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
        font-lock-builtin-face))))
  :bind (("C-x C-e" . pp-eval-last-sexp)
         ("C-x M-e" . pp-macroexpand-last-sexp)))

(use-package ielm
  ;; :ensure fails
  :if window-system
  :config
  (progn
    (add-hook 'ielm-mode-hook show-paren-mode)))


;; Twitter

(use-package twittering-mode
  :ensure t
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
  :ensure t
  :defer t)


;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t)


;; Evil

(use-package evil
  :ensure t
  :bind ("C-M-`" . evil-mode))


;; Fish

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")


;; Ido
(use-package ido
  :config
  (progn
    (use-package flx-ido
      :ensure t
      :config
      (progn
        (flx-ido-mode t)))
    (ido-mode t)
    (setq ido-everywhere t)))


;; CoffeeScript

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")


;; CSV

(use-package csv-mode
  :ensure t
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
    (use-package bf-mode)
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
  :if window-system
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (projectile-global-mode)))

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
  :ensure t
  :if window-system)


;;; Expand Region by Semantic Units

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


;;; Speedbar
(use-package speedbar
  :ensure t
  :if window-system
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
  :if window-system
  :bind ("C-x o" . ace-window))


;;; Ruby
(use-package ruby-mode
  :ensure t
  ;; Use M-x racr to use `rvm-activate-corresponding-ruby'
  :if window-system
  :defer t
  :init
  (progn
    (use-package rvm  :ensure t)
    (use-package robe :ensure t)
    (use-package projectile-rails
      :ensure t
      :config
      (progn
        (add-hook 'projectile-mode-hook
                  #'projectile-rails-on)))))

(use-package twittering-mode
  :ensure t
  :if window-system
  :config
  (progn
    (setq twittering-use-master-password t)))

;; Local Variables:
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
