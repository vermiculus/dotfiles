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

;(add-hook 'find-file-hook #'*-enable-minor-mode-based-on-extension)
