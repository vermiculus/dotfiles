(setq gc-cons-threshold 100000000)
(add-to-list 'load-path ".")

(defun *-insert-hfill (char)
  (interactive
   (list (if current-prefix-arg
             (let ((input (read-char "Fill char: ")))
               (cond
                ((eq input ?r) nil)
                (t input))))))
  (let* ((eol (save-excursion
                (move-end-of-line nil)
                (current-column))))
    (insert-char (or char ? ) (- (current-fill-column) eol))))


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


;; Windows

(when window-system
 (tool-bar-mode -1)
 (scroll-bar-mode -1))
(when (or *-windows-p
          (not window-system))
  (menu-bar-mode -1))


;; Custom
(defun *-load-customizations ()
  (interactive)
  (let ((f (locate-user-emacs-file ".custom.el")))
    (if (file-readable-p f)
        (load-file (setq custom-file f))
      (warn "Unable to read .custom.el"))))
(if window-system
    (*-load-customizations)
  (package-initialize)
  (require 'bind-key)
  (bind-key "C-c L" #'*-load-customizations))


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


;; Auto-Minor-Mode

(use-package auto-minor-mode
  :disabled t
  :load-path "my-packages/")


;; Creating Temporary Files

;; @TODO: to be released on GitHub and published on MELPA
(use-package tempfile
  :load-path "~/github/vermiculus/tempfile.el"
  :bind (("C-c t" . tempfile-find-temporary-file)
         ("C-c k" . tempfile-delete-this-buffer-and-file)))


;; Files and Buffers

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
     Get the value of point (file:40053)

POS-STYLE has no effect when SCOPE is `directory'."
  (interactive
   (let ((scope
          (cdr
           (assoc
            (read-char-choice
             "Copy (f)ull name, (d)irectory, or just the (b)asename? "
             '(?f ?d ?b))
            '((?f . full)
              (?d . directory)
              (?b . basename))))))
     (list scope
           (and (not (equal scope 'directory)) current-prefix-arg
                (cdr (assoc (read-char-choice
                             "Style: (n)one, (l)ine, (c)olumn, or (p)oint? "
                             '(?n ?l ?c ?p))
                            '((?n . nil)
                              (?l . line)
                              (?c . line-column)
                              (?p . point))))))))
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
            (concat ":"
                    (cond ((equal pos-style 'line)
                           ;; @todo return absolute line number
                           (format "%d" (line-number-at-pos)))
                          ((equal pos-style 'line-column)
                           (format "%dc%d"
                                   (line-number-at-pos)
                                   (current-column)))
                          ((equal pos-style 'point)
                           (format "p%d" (point)))
                          (t (error "Invalid style %S" pos-style))))))
         (new-kill-string (concat file-part pos-part)))
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

(defun *-delete-empty-directories (root-directory)
  "Recursively delete empty directories in ROOT-DIRECTORY.

When called from dired, `dired-current-directory' is used for
ROOT-DIRECTORY."

  ;; Interface
  (interactive (list (if (eq major-mode 'dired-mode)
                         (expand-file-name (dired-current-directory))
                       (read-from-minibuffer "Root directory: "))))
  (when (or (null root-directory) (string= "" root-directory))
    (user-error "No root directory provided"))
  (when (called-interactively-p 'interactive)
    (unless (yes-or-no-p (format "Delete all non-empty directories in `%s'? "
                                 root-directory))
      (user-error "Directory `%s' has been left untouched" root-directory)))

  ;; Implementation
  (require 'f)
  (let ((entries (f-directories root-directory)))
    (while entries
      (let ((curdir (car entries)))
        (when (f-directories curdir)
          (*-delete-empty-directories curdir))
        (unless (f-entries curdir)
          (delete-directory curdir)
          (message "Directory deleted: `%s'" curdir))
        (setq entries (cdr entries))))))


(defun unfill-region (beg end)
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


(use-package m4-mode
  :ensure t
  :defer t
  :config
  (modify-syntax-entry ?# "@" m4-mode-syntax-table))


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
    (if (string= response "") default response)))

(defun *-TeX-find-kpathsea (string)
  (interactive
   (list
    (let ((default (concat (thing-at-point 'symbol t) ".sty")))
      (setq string
            (*-read-from-minibuffer
             "Find file in TeX distribution"
             default)))))
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
              (find-file-other-window texdoc-file)
            (error "Sorry, the file returned by texdoc for %s isn't readable"
                   texdoc-query)))))))

(use-package tex
  :ensure auctex
  :config
  (use-package reftex
    :ensure t
    :config
    (setq reftex-plug-into-AUCTeX t))
  :config
  (add-to-list
   'TeX-command-list
   '("Arara"
     "arara %t"
     TeX-run-command
     nil                              ; ask for confirmation
     t                                ; active in all modes
     :help "Run Arara"))
  :config
  (load "/Users/sean/github/auctex/expl3.el")
  :bind (("C-c ?" . *-TeX-find-texdoc)
         ("C-c M-?" . *-TeX-find-kpathsea)))

(use-package latex
  :config
  (add-to-list 'LaTeX-font-list
               '(?\C-a "\\Algorithm{" "}")))


;; God-Mode

(use-package god-mode
  :ensure t
  :bind ("<escape>" . god-local-mode)
  :config
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
            t)))

  (bind-key "[" #'backward-page god-local-mode-map)
  (bind-key "]" #'forward-page god-local-mode-map))



;; Multiple Cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C-<"   . mc/mark-previous-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("C-M->" . mc/mark-all-like-this-dwim)
         ("M-i"   . mc/insert-numbers)))


;; Smex

(use-package smex
  :ensure t
  :config
  (smex-initialize)
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
         ("C-c C-'" . compile))
  :config
  (require 'ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1)))))


;; Company

(use-package company
  :ensure t
  :if window-system
  :commands (company-mode company-mode-on)
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode-on)
  (mapc (lambda (s) (add-hook s #'company-mode-on))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook
          ruby-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook))
  :config
  (bind-keys :map company-active-map
             ("C-n" . company-select-next-or-abort)
             ("C-p" . company-select-previous-or-abort)))


;; Theming

(use-package monokai-theme
  :ensure t
  :if window-system
  :config
  (enable-theme 'monokai))


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

  :init (require 'helm-config)
  :config (helm-mode 1)

  :config (when (executable-find "curl")
            (setq helm-google-suggest-use-curl-p t))

  :config (use-package helm-swoop
            :ensure t
            :bind ("C-S-s" . helm-swoop)
            :commands (helm-swoop helm-swoop-from-isearch)
            :config (bind-key "C-s" #'helm-swoop
                              helm-command-prefix)
            :config (bind-key "M-i" #'helm-swoop-from-isearch
                              isearch-mode-map))

  :config (use-package helm-ag
            :ensure t
            :bind ("s-f" . helm-do-ag))

  :config (setq helm-M-x-fuzzy-match t)

  :bind (("s-x"   . helm-M-x)
         ("C-x C-a" . helm-command-prefix)
         ("C-x C-a C-o" . helm-org-in-buffer-headings)
         ("C-x C-a C-s" . helm-swoop)
         ("C-c g" . helm-google-suggest)
         ("C-x b" . helm-mini)
         ;; use `ag' for recursive grep?  is it possible?
         ;; ("C-x C-f" . helm-find-files)
         ;; ^^ I don't seem to like it as much as ido-find-file with flx-ido
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
  :bind ("M-m" . magit-status)
  :config
  (setq magit-diff-refine-hunk nil
        magit-use-overlays nil))


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
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

(use-package outorg
  :ensure t
  :if window-system
  :config
  (use-package outshine :ensure t)
  :bind ("M-#" . outorg-edit-as-org))

;; 
;; Lisp

(use-package slime
  :ensure t
  :if window-system
  :commands slime
  :config
  (setq inferior-lisp-program "clisp"))

(use-package erefactor
  :ensure t
  :if window-system
  :config
  (add-hook 'emacs-lisp-mode-hook #'erefactor-lazy-highlight-turn-on)
  (bind-key "C-c C-d" erefactor-map emacs-lisp-mode-map))

(use-package eldoc
  :ensure t
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
  :ensure t
  :diminish paredit-mode
  :config
  (mapc (lambda (s) (add-hook s #'paredit-mode))
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          lisp-mode-hook
          ielm-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook)))

(use-package clj-refactor
  :ensure t
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
  ;; :ensure fails
  :if window-system
  :config
  (add-hook 'ielm-mode-hook show-paren-mode))


;; Twitter

(use-package twittering-mode
  :ensure t
  :if window-system
  :commands (twit twittering-mode twittering-update-status)
  :config
  (defun *-twittering-update-status-from-minibuffer ()
    (interactive)
    (let ((twittering-update-status-function
           #'twittering-update-status-from-minibuffer))
      (twittering-update-status)))
  :bind (("C-c m" . *-twittering-update-status-from-minibuffer)
         ("C-c n" . twittering-update-status-interactive)))


;; YAML

(use-package yaml-mode
  :ensure t
  :defer t)


;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (defun *-insert-post-url (bare)
  (interactive "P")
  (require 'f)
  (require 'cl-lib)
  (let* ((basedir (expand-file-name "~/blog/_posts/"))
         (files (f-files basedir
                         (lambda (f)
                           (string= (file-name-extension f)
                                    "markdown"))
                         t))
         (candidates (cl-remove-duplicates
                      (mapcar
                       (lambda (f) (s-chop-prefix
                                    basedir
                                    (s-chop-suffix
                                     ".markdown"
                                     (s-chop-suffix "~" f))))
                       files)
        :test #'string=)))
    (insert
     (format
      (if bare "%s" "{%% post_url %s %%}")
      (file-name-base (completing-read "Post: " candidates))))))
  (bind-key "M-M" #'*-insert-post-url markdown-mode-map))


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
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode t))
  (ido-mode t)
  (setq ido-everywhere t))


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
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(use-package dired
  :init (bind-key "z" #'*-dired-zip-files dired-mode-map)
  :config
  (use-package bf-mode)
  (when *-osx-p
    (customize-set-value
     'insert-directory-program "gls"
     "Use ls from core-utils"))
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
    (dired-mark-files-regexp zip-file)))


;; Ibuffer

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


;; Projectile

(use-package projectile
  :ensure t
  :if window-system
  :config
  (setq projectile-completion-system 'helm
        projectile-generic-command
        (concat "g" projectile-generic-command))
  (projectile-global-mode))

(use-package ag
  :ensure t
  :commands (ag-regexp))

(use-package helm-projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm)
  (bind-key "s-f" #'helm-projectile-ag projectile-command-map))


;;; Hyde and Jekyll
(use-package hyde)


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


;;; Ace-Jump
(use-package ace-jump-mode
  :ensure t
  :disabled t
  :config
  (define-prefix-command 'my:ace-jump-map)
  (mapc (lambda (x) (define-key my:ace-jump-map
                      (car x) (cadr x)))
        '(("j" ace-jump-mode)
          ("k" ace-jump-char-mode)
          ("l" ace-jump-line-mode)))
  (setq ace-jump-mode-move-keys
        (loop for i from ?a to ?z collect i))
  :bind (("C-c j" . my:ace-jump-map)))

(use-package avy
  :ensure t
  :config
  (setq avy-style 'at-full)
  (bind-keys :prefix-map my:avy-jump-map
             :prefix "C-c C-'"
             ("C-'" . avy-goto-word-0)
             ("C-;" . avy-goto-char)
             ("C-k" . avy-goto-char-2)
             ("C-l" . avy-goto-line))
  :config
  (bind-key "C-'" #'avy-isearch isearch-mode-map)
  :config
  (setq avy-style 'at-full))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global ";l" #'avy-goto-line)
  (key-chord-define-global ";o" #'avy-goto-word-1)
  (key-chord-define-global ";m" #'avy-goto-char))

(use-package ace-window
  :ensure t
  :disabled
  :if window-system
  :bind ("C-x o" . ace-window)
  :config (ace-window-display-mode))


;;; Golden Ratio
(use-package golden-ratio
  :ensure t
  :config (add-to-list 'golden-ratio-extra-commands 'aw--callback))


;;; Ruby
(use-package ruby-mode
  :ensure t
  ;; Use M-x racr to use `rvm-activate-corresponding-ruby'
  :if window-system
  :defer t
  :init
  (use-package rvm  :ensure t)
  (use-package robe :ensure t)
  (use-package projectile-rails
    :ensure t
    :config
    (add-hook 'projectile-mode-hook #'projectile-rails-on)))


;;; Twitter
(use-package twittering-mode
  :ensure t
  :if window-system
  :config
  (setq twittering-use-master-password t))


;;; Sunshine
(use-package sunshine
  :config
  (setq sunshine-location "Madison, WI"))


;;; Basic Config and Keybindings
(bind-keys ("C-S-j" . join-line))


;;; Neotree
(use-package neotree
  :bind ("s-d" . neotree)
  :config
  (setq neo-dont-be-alone t
        neo-theme 'ascii)
  (bind-keys :map neotree-mode-map
             ("u" . neotree-select-up-node)
             ;;("d" . *-neo-down-and-next)
             ("i" . neotree-enter)
             ("K" . neotree-delete-node))
  (defun *-neo-down-and-next ()
    (interactive)
    (neotree-enter)
    (neotree-next-node)))


(use-package eshell
  :bind ("s-s" . eshell))

(defun *-erc-bitlbee ()
  (interactive)
  (erc :server "localhost"
       :port 6667
       :nick "sean"))

(use-package erc
  :bind ("s-e" . *-erc-bitlbee)
  :config
  (bind-key "s-." (lambda () (interactive)
                    (insert "identify ecce-gratum")
                    (erc-send-current-line))
            erc-mode-map))

(use-package sx
  :config
  (bind-keys :prefix-map *-sx-map
             :prefix "s-x"
             ("i" . sx-inbox)
             ("s" . sx-tab-frontpage)))

(use-package swiper
  :init (require 'ivy)
  :bind ("C-s" . swiper))

(use-package help
  :config
  (bind-keys :map help-mode-map
             ("[" . help-go-back)
             ("]" . help-go-forward)))

(ignore
 '(defhydra windows (global-map "C-x o"
                               :after-exit flash-active-buffer)
   "window moving"
   ("o" other-window "other")))

(make-face 'flash-active-buffer-face)
(set-face-attribute 'flash-active-buffer-face nil
                    :background "red"
                    :foreground "black")
(defun flash-active-buffer ()
  (interactive)
  (run-at-time "50 millisec" nil
               (lambda (remap-cookie)
                 (face-remap-remove-relative remap-cookie))
               (face-remap-add-relative 'default 'flash-active-buffer-face)))

(defun *-eval-and-replace ()
  (interactive)
  (save-excursion
    (eval-last-sexp t))
  (backward-kill-sexp)
  (forward-sexp))
(bind-key "C-x C-M-r" #'*-eval-and-replace)

(column-number-mode 1)

(bind-key "C-M-="
          (lambda ()
            (interactive)
            (save-window-excursion
              (list-packages)
              (package-menu-mark-upgrades)
              (package-menu-execute t))))


(defvar *-devlog-hooks nil)
(defvar *-devlog-major-mode nil)
(defvar *-devlog-ext nil)
(defun *-devlog-new-entry ()
  (interactive)
  (require 'magit)
  (require 'f)
  (let* ((default-directory (projectile-project-root))
         (rev (magit-rev-parse "HEAD"))
         (dir "devlog")
         (devlog (f-expand (concat rev *-devlog-ext) dir)))
    (unless (f-exists? dir)
      (make-directory dir))
    (with-current-buffer (find-file devlog)
      (funcall (or *-devlog-major-mode #'text-mode))
      (run-hooks *-devlog-hooks))))

(use-package projectile
  :config
  (bind-key "[" #'*-devlog-new-entry projectile-command-map))

(setq *-devlog-major-mode #'markdown-mode
      *-devlog-ext ".md")

(defun *-make-scope ()
  (interactive)
  (let (needed-exchange)
    (if (region-active-p)
        (progn
          (when (< (mark) (point))
            (setq needed-exchange t)
            (exchange-point-and-mark))
          (insert "{\n")
          (exchange-point-and-mark)
          (insert "\n}")
          (indent-region
           (save-excursion
             (goto-char (mark))
             (forward-line -1)
             (point))
           (save-excursion
             (forward-line 1)
             (point)))
          (unless needed-exchange
            (exchange-point-and-mark))
          (deactivate-mark))
      (insert "\n{")
      (indent-for-tab-command)
      (newline-and-indent)
      (save-excursion
        (insert "\n}")
        (indent-for-tab-command)))))

(require 'cc-mode)
(define-key c-mode-map (kbd "C-{") #'*-make-scope)

;; Local Variables:
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;(setq gc-cons-threshold 800000)


