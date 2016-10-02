                                        ;(require 'tint-instances nil t)  ;optional ;TODO-replace with hilight-regexp
(require 'font-lock)

                                        ;some customizable variables for mumps-mode

                                        ;*CW+3 6/17/08 adding support to automatically use windows-1252 for *.mumps*, *.ROU* (as unix) and ARD-*.txt (as dos) 
                                        ; may need make the regular expression more precise, is "\\'" means end of string like "$"?
                                        ; Now moved to .emacs-init.el: it seems too late to do this when first time mumps-mode is loading for opening a mumps file.  Put it here it won'r work for the first opened file.
                                        ;(add-to-list 'file-coding-system-alist '("\\.\\(mumps\\|ROU\\)\\(\\.\\sw*\\)?\\'" . windows-1252-unix) )
                                        ;(add-to-list 'file-coding-system-alist '("ARD-.*\\.txt\\'" . windows-1252-dos) )


                                        ;*CW 3/10/2011
(defvar mumps-mode-env nil   
  "the environment this mode is associated with.")

                                        ;keymap for mumps-shell mode
(defvar mumps-mode-map nil
  "Keymap for mumps major mode.")

(defvar mumps-mode-use-mumps-shell nil
  "t if it uses mumps-shell")

(defvar mumps-mode-use-checksum
  "t if it has checksum.el")

(defvar mumps-mode-line-format nil
  "Mumps mode line format")

                                        ;buffer local variabls
(defvar mumps-mode-file-name nil
  "File name in mumps mode")

(defvar mumps-mode-DLG nil
  "DLG number in the file name, like 'ROU.mumps.12345'")

(defvar mumps-mode-rev nil
  "Revision number when in revision control")

(make-variable-buffer-local 'mumps-mode-file-name)
(make-variable-buffer-local 'mumps-mode-DLG)
(make-variable-buffer-local 'mumps-mode-rev)

(setq mumps-mode-use-mumps-shell
      (require 'mumps-shell nil t))

(setq mumps-mode-use-checksum
      (require 'checksum nil t))

(if mumps-mode-map
    nil
  (let ((map (make-sparse-keymap "Mumps-Mode"))
        (map2 (make-sparse-keymap)))
                                        ;*CW+2 future change?
                                        ;(setq mumps-mode-map
                                        ;  (nconc (make-sparse-keymap) mumps-shell-map))
                                        ;(setq mumps-mode-map (make-keymap))
    (when mumps-mode-use-mumps-shell
      (define-key map2 "\C-cf" 'mumps-checkin-routine-with-version-lock)
      (define-key map2 "\C-c\C-f" 'mumps-find-routine)
      (define-key map2 "\C-c\C-r" 'mumps-find-routine-read-only)
      (define-key map2 "\C-cm" 'mumps-shell-lookitt-macro)
      (define-key map2 "\C-c\C-l" 'mumps-display-mumps-shell)
      (define-key map2 "\C-c\C-m"
        '(lambda () (interactive) (mumps-display-mumps-shell t)))
                                        ;
      (define-key map2 "\C-c\C-s" 'mumps-save-buffer)
      (define-key map2 "\C-cl" 'mumps-lock-routine)
      (define-key map2 "\C-cr" 'mumps-release-lock-routine)
      (define-key map2 "\C-ck" 'mumps-show-routine-lock-count))
    (when mumps-mode-use-checksum
      (define-key map2 "\C-cc" 'mumps-checksum)
      (define-key map2 "\C-c\C-c" 'checksum-region-noeol-show))
    (when mumps-mode-use-mumps-shell
      (define-key map2 "\C-c\C-w" 'mumps-save-log)
      (define-key map2 "\C-ci" 'mumps-check-in-buffer)
      (define-key map2 "\C-co" 'mumps-check-out-buffer)
      (define-key map2 "\C-c~" 'mumps-mode-diff-with-previous-version))
                                        ;
    (define-key map2 "\C-ct" 'mumps-find-tag)
    (define-key map2 "\C-c\C-t" 'mumps-find-tag-line)
    (define-key map2 "\C-c=" 'mumps-count-tag-lines)
    (define-key map2 "\C-c-" 'mumps-move-line)
    (define-key map2 "\C-c\C-n" 'mumps-next-tag)
    (define-key map2 "\C-c\C-p" '(lambda () (interactive) (mumps-next-tag t)))
                                        ;
                                        ;the menu bar keymap
    (define-key map2 [menu-bar] (make-sparse-keymap))
    (define-key map2 [menu-bar mumps-mode]
      (cons "Mumps-Mode" map))
                                        ;    (define-key map [prev-tag]
                                        ;      '("Prev tag" . '(lambda () (interactive) (mumps-next-tag t)))
    (define-key map [next-tag] '("Next tag" . mumps-next-tag))
    (define-key map [move-line] '("Move line forward " . mumps-move-line))
    (define-key map [count-tag-lines] '("Count tag line" . mumps-count-tag-lines))
    (define-key map [find-tag-line] '("Find tag line". mumps-find-tag-line))
    (define-key map [find-tag] '("Find tag" . mumps-find-tag))
                                        ;    (define-key map [switch-to-mumps-shell] 
                                        ;      '("Switch to cache in other window" . lambda () (interactive) (mumps-display-mumps-shell t))))
    (when mumps-mode-use-mumps-shell
      (define-key map [separator-2] '("--"))
      (define-key map [diff-prev] '("Diff with version" . mumps-mode-diff-with-previous-version))
      (define-key map [checkout] '("Check-out" . mumps-check-out-buffer))
      (define-key map [checkin] '("Check-in" . mumps-check-in-buffer))
      (define-key map [save-routines-in-DLG]
        '("Save routines in DLG" . mumps-save-log)))
    (when mumps-mode-use-checksum
      (define-key map [region-checksum]
        '("Region checksum" . checksum-region-noeol-show))
      (define-key map [checksum] '("Checksum" . mumps-checksum)))
    (when mumps-mode-use-mumps-shell
      (define-key map [routine-show-lock-count] '("Show lock count" . mumps-show-routine-lock-count))
      (define-key map [routine-release-lock]
        '("Release lock" . mumps-release-lock-routine))
      (define-key map [routine-lock] '("Lock routine" . mumps-lock-routine))
      (define-key map [save-buffer] '("Save to Cache" . mumps-save-buffer))
      (define-key map [separator-1] '("--")))
    
    (when mumps-mode-use-mumps-shell
      (define-key map [display-mumps-shell]
        '("Display cache in other window" . mumps-display-mumps-shell))
      (define-key map [lookitt-macro]
        '("Lookitt macro" . mumps-shell-lookitt-macro))
      (define-key map [find-read-only] 
        '("Find read-only" . mumps-find-routine-read-only))
      (define-key map [find-routine] 
        '("Find routine". mumps-find-routine))
      (define-key map [checkin-routine-version-lock]
        '("Checkin routine with version lock" . mumps-checkin-routine-with-version-lock)))
    (if mumps-mode-use-checksum
        (put 'region-checksum 'menu-enable 'mark-active)) ;modify some property of region-checksum
    (setq mumps-mode-map map2)))

                                        ;(defvar mumps-mode-hook nil  ;*CW+6 6/08 use defcustom
                                        ;  "*List of functions to call when entering Mumps mode.")
(defcustom mumps-mode-hook nil
  "*List of hooks run when entering Mumps mode."
  :type 'hook
  :options '(turn-on-flyspell turn-on-font-lock)
  :group 'data)

(add-hook 'mumps-mode-hook 'turn-on-flyspell)

                                        ;variables for mumps-mode

(defvar mumps-mode-syntax-table nil
  "Sntax table used in mumps-mode buffers.")

(if mumps-mode-syntax-table ;when done replace nil with mumps-mode-syntax-table
    ()
  (setq mumps-mode-syntax-table (make-syntax-table))
  ;; add comment syntax
  (modify-syntax-entry ?\; "<    " mumps-mode-syntax-table)  ;comment start
  (modify-syntax-entry ?\n ">    " mumps-mode-syntax-table)  ;comment end
  (modify-syntax-entry ?\^m ">    " mumps-mode-syntax-table) ;comment end
  ;; map some M operators as symbol
  (modify-syntax-entry ?\\ "_"   mumps-mode-syntax-table)    ;like +,-,*
  (modify-syntax-entry ?$  "_"   mumps-mode-syntax-table)    ;function/intrinsic function
  (modify-syntax-entry ?!  "_"   mumps-mode-syntax-table)    ;logical or
  (modify-syntax-entry ?#  "_"   mumps-mode-syntax-table)    ;modulo
  (modify-syntax-entry ?'  "_"   mumps-mode-syntax-table)    ;logical not
  (modify-syntax-entry ??  "_"   mumps-mode-syntax-table)    ;patten matching
  (modify-syntax-entry ?@  "_"   mumps-mode-syntax-table)    ;indirection
  (modify-syntax-entry ?[  "_"   mumps-mode-syntax-table)    ;contains
                       (modify-syntax-entry ?]  "_"   mumps-mode-syntax-table)    ;follows (]) and sort after(]])
  )


(defvar mumps-mode-abbrev-table nil "")

(if (< emacs-major-version 22)  ;before version 22 vc-mode is in minor-mode-alist, but in default mode-line-format after 22
    (setq mumps-mode-line-format ; for emacs version before 22
          '("-"
            mode-line-mule-info
            mode-line-modified
            mode-line-frame-identification
            mode-line-buffer-identification "   "
            "<" mumps-shell-working-env ":" mumps-shell-working-dir ">  "
            global-mode-string
            "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
            (which-func-mode
             ("" which-func-format "--"))
            (line-number-mode "L%l--")
            (column-number-mode "C%c--")
            (-3 . "%p") "-%-"))
  (setq mumps-mode-line-format  ;for emacs version after 22
        '("%e" ;error message about memory full
          #("-" 0 1
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          mode-line-mule-info
          mode-line-modified
          mode-line-frame-identification
          mode-line-buffer-identification 
          #("   " 0 3
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          "<" mumps-shell-working-env ":" mumps-shell-working-dir ">"
          #("  " 0 2
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          (vc-mode vc-mode)
          "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]"
          #("--" 0 2
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          (which-func-mode
           ("" which-func-format 
            #("--" 0 2 
              (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))))
          (line-number-mode "L%l")
          #("-" 0 1
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          (column-number-mode "C%c")
          #("--" 0 2
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          (-3 . "%p")
          #("--" 0 2
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          (global-mode-string
           (#("--" 0 2
              (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
            global-mode-string))
          #("-%-" 0 3
            (help-echo "mouse-1: select (drag to resize), mosue-2 = C-x 1, mouse-3 = C-x 0"))
          ))
  )


(defun mumps-mode-local-variables (lisp-syntax)
  (cond (lisp-syntax
	 (set-syntax-table mumps-mode-syntax-table)))
                                        ;(setq local-abbrev-table mumps-mode-abbrev-table)
  (make-local-variable 'mumps-mode-env)   ;*CW 3/10/11
                                        ;*CW+7 defined in newcomment.el
  (set (make-local-variable 'comment-start) ";")    ; ";" indicating start a new comment
  (set (make-local-variable 'comment-end) "")       ; empty string indicating comments are terminated by end-of-line
  (set (make-local-variable 'comment-column) 1)     ; column to indent right-margin comments to
  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")    ; regexp to match the start of a comment plus everyting up to its body
  (setq indent-tabs-mode nil) ;makes TAB insert spaces
  (setq tab-width 1) ;make TAB shows as one space - we don't want to have TAB in the routine anyway - indent-tabs-mode nil above
  "Make and define some buffer local-variables for mumps-mode"
  )

                                        ;I don't know how to use facemenu yet.
                                        ;(if (fboundp 'facemenu-unlisted-faces)
                                        ;    (add-to-list 'facemenu-unlisted-faces 'mumps-space-face)

                                        ;setup mumps-space-face
(if (fboundp 'defface) ;defface is a macro in emacs 20.2
    (defface mumps-space-face
      '((((class color)) (:background  "hotpink"))
	(t (:reverse-video t)))
      "Face to use for highlighting leading spaces in Font-Lock mode."
      :group 'faces
      :group 'mumpsmode)
  ;; for emacs 19.34.1, use following (from emacs 19 make-mode.el)
  ;; make-file-define-space-face
  (if (fboundp 'make-face)
      (progn
	(make-face 'mumps-space-face)
	(or (not (or (eq window-system 'x) (eq window-system 'win32)))
	    (face-differs-from-default-p 'mumps-space-face)
	    (let* ((params (frame-parameters))
		   (light-bg (cdr (assq 'background-mode params)))
		   (bg-color (cond ((eq (cdr (assq 'display-type params)) 'mono)
				    (if light-bg "black" "white"))
				   ((eq (cdr (assq 'display-type params)) 'grayscale)
				    (if light-bg "black" "white"))
				   (light-bg	; Light color background.
				    "hotpink")
				   (t		; Dark color background.
				    "hotpink"))))
	      (set-face-background 'mumps-space-face bg-color))))))

                                        ;setup mumps-DLG-face
(if (fboundp 'defface) ;defface is a macro in emacs 20.2
    (defface mumps-DLG-face
      '((((class color)) (:background  "Firebrick" :foreground "Yellow"))
	(t (:reverse-video t)))
      "Face to use for highlighting leading spaces in Font-Lock mode."
      :group 'faces
      :group 'mumpsmode)
  ;; for emacs 19.34.1, use following (from emacs 19 make-mode.el)
  ;; make-file-define-space-face
  (if (fboundp 'make-face)
      (progn
	(make-face 'mumps-DLG-face)
	(or (not (or (eq window-system 'x) (eq window-system 'win32)))
	    (face-differs-from-default-p 'mumps-DLG-face)
	    (let* ((params (frame-parameters))
		   (light-bg (cdr (assq 'background-mode params)))
		   (bg-color (cond ((eq (cdr (assq 'display-type params)) 'mono)
				    (if light-bg "black" "white"))
				   ((eq (cdr (assq 'display-type params)) 'grayscale)
				    (if light-bg "black" "white"))
				   (light-bg	; Light color background.
				    "Firebrick")
				   (t		; Dark color background.
				    "Firebrick"))))
	      (set-face-background 'mumps-DLG-face bg-color))))))



(defvar mumps-space-face 'mumps-space-face
  "Face to use for highlighting leading spaces in Font-Lock mode.")

(defvar mumps-DLG-face 'mumps-DLG-face
  "Face to make DLG number")

                                        ;followed is referenced from pascal.el
(defconst mumps-font-lock-keywords
  (purecopy
   (list
    ;; preprocessor tags and comments
    '("[[:space:]]\\(;;#[[:alnum:]]+#\\)" 1 font-lock-preprocessor-face t)
    ;; 
    ;; #STR# comments
                                        ;'("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#\\)" 1 font-lock-type-face t)
    '("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#[[:alnum:]]+#\\)" 1 font-lock-type-face t)   ;002, 002L2
                                        ;'("[[:space:]];[[:space:]]*\\(#STR#[[:alnum:]]+#\\)" 1 font-lock-type-face t) 
                                        ;'("[[:space:]];[[:space:]]*\\(#CMT#[[:digit:]]+#\\)" 1 font-lock-type-face t)
    '("[[:space:]];[[:space:]]*\\(#\\(STR\\|CMT\\)#[[:alnum:]]+#\\)\\(.*\\)" 3 font-lock-doc-face t)
    ;; 
    ;; tags/labels
    '("^%?[[:digit:]]+" . font-lock-type-face) ; tag that start with digit may not contain letters
    '("^%?[a-z][[:alnum:]]*" . font-lock-type-face)
    ;; 
    ;; function calls, match $$^ROU by not forcing a tag name
    '("\\$\\$%?[[:digit:]]+" . font-lock-type-face) ; tag that start with digit may not contain letters
    '("\\$\\$%?[a-z][[:alnum:]]*" . font-lock-type-face)
    '("\\<do?\\(:.+?\\)? \\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?" 2 font-lock-type-face)
                                        ;'("\\<do?\\(:.+?\\)? \\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?,\\)\\{1\\}\\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\(.*\\)?\\)" 7 font-lock-type-face)  ;TODO - how to handle comma case?
                                        ;'("do?\\(:.+?\\)? \\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?,\\)*\\(\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?\\)\\(,\\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?\\((.*)\\)?\\)*" 7 font-lock-warning-face)

    '("\\<g\\(oto\\)?\\(:.+?\\)? \\(%?[[:alnum:]]+\\)\\(\\^%?[[:alnum:]]+\\)?" 3 font-lock-type-face)
    ;; 
    ;; built-in functions
    '("\\$[A-Za-z]+" . font-lock-builtin-face)
    ;; 
    ;; variables & arguments
    '("[[:space:]]n\\(ew\\)? \\(.+\\)" 2 font-lock-variable-name-face)
    '("^%?[[:digit:]]+(\\(.+\\)[ \n]" 1 font-lock-variable-name-face)
    '("^%?[a-z][[:alnum:]]*(\\(.+\\))[ \n]" 1 font-lock-variable-name-face)
    ;; 
    ;; routines or globals
    '("\\^%?[[:alnum:]]+" . font-lock-constant-face)
    ;; 
    ;; highlight keywords
    (cons (concat "\\<\\(c\\(lose\\)?\\|g\\(oto\\)?\\|h\\(ang\\)?\\|j\\(ob\\)?\\|m\\(erge\\)?\\|n\\(ew\\)?\\|o\\(pen\\)?\\|r\\(ead\\)?\\|s\\(et\\)?\\|u\\(se\\)?\\|v\\(iew\\)?\\|w\\(rite\\)?\\|x\\(ecute\\)?\\)\\>[ :]")
	  'font-lock-keyword-face) ; type 1 keywords
    (cons (concat "\\<\\(b\\(reak\\)?\\|h\\(alt\\)?\\|tc\\(ommit\\)?\\|tre\\(start\\)?\\|tro\\(llback\\)?\\|ts\\(tart\\)?\\)\\>[:\n ]")
	  'font-lock-keyword-face) ; type 2 keywords
    (cons (concat "\\<\\(d\\(o\\)?\\|k\\(ill\\)?\\|l\\(ock\\)?\\|q\\(uit\\)?\\)\\>[:\n ]")
	  'font-lock-keyword-face) ; type 3 keywords
    (cons (concat "\\<\\(f\\(or\\)?\\|i\\(f\\)?\\)\\>[ ]")
	  'font-lock-keyword-face) ; type 4 keywords-need more work
    (cons (concat "\\<\\(e\\(lse\\)?\\)\\>  ")
	  'font-lock-keyword-face) ; type 5 keywords
    '("\t+" . mumps-space-face) ;highlight TABs since MUMPS does not like TABS
    ))

  "Additional expressions to highlight in Mumps mode.")

                                        ;(put 'mumps-mode 'font-lock-defaults '(mumps-font-lock-keywords nil t)) ;cwwu 10/8/03: looks like redundant to add/create a font-lock-defaults property to mumps-mode

(defvar mumps-font-lock-keywords-local nil
  "The real expressions to highlight in Mumps mode.")

                                        ;(defvar mumps-time-string-format "%b %d, %Y %l:%M %p"  ; refert to documentation of format-time-string
(defvar mumps-time-string-format "%Y-%m-%d %H:%M:%S"
  "Time string format for mumps-mode.")

                                        ;functions for mumps-mode

(defun mumps-locate-timestamp (&optional noerr)
  "Locate the timestamp position in mumps-code, and move the cursor there.
Return a pair of points to the beginning and end of timestamp in the buffer."
  (interactive "P")
  (let* ((min-point (point-min)) endline spoint epoint)
    (goto-char min-point)
    (forward-line 4)
    (end-of-line)
    (setq endline (point))
    (goto-char min-point)
    (setq spoint (re-search-forward "^\\sw+\\((.*)\\)? *;[^;\n]*;[^;\n]*\\(;\\).*$" endline noerr 1))
    (setq spoint (match-beginning 2))
    (if (not (null spoint)) 
	(progn
	  (setq spoint (+ spoint 1))
	  (end-of-line)
	  (setq endline (point))
	  (goto-char spoint)
	  (setq epoint (re-search-forward ";" endline 0 1)) ;if failed just move the the limit search
	  (if epoint (setq epoint (- epoint 1))
	    (setq epoint (point)))
	  (list spoint epoint)))
    )
  )

(defun mumps-insert-timestamp ()
  "Insert a timestamp in mumps-mode, using format in mumps-time-string-format."
  (interactive "*") ;abort if buffer is readonly
  (insert (format-time-string mumps-time-string-format (current-time)))
  )

(defun mumps-update-timestamp ()
  "Find timestamp in mumps code and replace it with the current time."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(let* ((pairs (mumps-locate-timestamp t))
	       start end)
	  (if pairs
	      (progn
		(setq start (car pairs))
		(setq end (car (cdr pairs)))
		(goto-char start)
		(delete-region start end)
                                        ;(insert " ") ;*CW 8/27/07 don't add space
		(mumps-insert-timestamp))
	    (message "Timestamp not updated.")
	    (ding nil)
	    (sleep-for 1)
                                        ;(run-at-time 1 nil 'message "Timestamp not updated.")
	    ))
	)
      )
    )
  nil)

(defun mumps-find-tag (tag)
  "Find the tag in the Mumps code."
  (interactive "sTags: ")
  (let ((match (concat "^" tag))
                                        ;(spoint (push-mark (point) t)))
	(spoint (point)))
    (goto-char (point-min))
    (if (not (search-forward-regexp match nil t))
	(progn (message "Tag \"%s\" not found." tag)
	       (goto-char spoint))))
  )

(defun mumps-find-tag-line (tag ln)
  "Move the cursor to the lines of tag in the Mumps code."
  (interactive "sTags: \nNnumber of lines: ")
  (mumps-find-tag tag)
  (if ln (mumps-move-line ln)))

(defun mumps-mode ()
  "Major mode for editing Mumps/M/Cache code.
While saving the file, it will update the time stamp and
remove all tailing spaces at the end of each line.
\\{mumps-mode-map}
Entry to this mode calls the value of `mumps-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mumps-mode-map)
  (setq major-mode 'mumps-mode)
  (setq mode-name "Mumps")
  ;; Font lock support
                                        ;(make-local-variable 'font-lock-defaults)
                                        ;(setq font-lock-defaults '(mumps-font-lock-keywords nil t))
  (mumps-mode-local-variables t)
  ;; for parsing file name, is there a difference than using make-local-variable?
                                        ;(make-variable-buffer-local 'mumps-mode-file-name)
                                        ;(make-variable-buffer-local 'mumps-mode-DLG)
                                        ;(make-variable-buffer-local 'mumps-mode-rev)
  (mumps-mode-parse-file-name)
  ;; Font lock support and use mumps-mode-DLG to highlight DLG number
                                        ;(if (require 'tint-instances nil t)
  (if (not (null mumps-mode-DLG)) ;highlight existing DLG number
      (if (stringp mumps-mode-DLG)
          (if (not (string= "" mumps-mode-DLG))
                                        ;(tint_all_instances_of_string mumps-mode-DLG))
              (highlight-phrase mumps-mode-DLG 'hi-yellow))
        ))
                                        ;  )
                                        ;this is not really working yet, because DLG is bared inside comments, how to override that just for the DLG number?
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'mumps-font-lock-keywords-local)
  (if (stringp mumps-mode-DLG)
      (if (string= "" mumps-mode-DLG)
          (setq mumps-font-lock-keywords-local mumps-font-lock-keywords) ;if an empty string
        (setq mumps-font-lock-keywords-local 
              (append
               mumps-font-lock-keywords
               (list
                                        ;(cons (concat "\\<" mumps-mode-DLG "\\>") 'mumps-DLG-face)
                                        ;(cons (concat "\\b" mumps-mode-DLG "\\b") 'mumps-DLG-face)
                (list (concat "\\b\\(" mumps-mode-DLG "\\)\\b") 1 'mumps-DLG-face))
               ))
        )
                                        ;if mumps-mode-DLG is not a string
    (setq mumps-font-lock-keywords-local mumps-font-lock-keywords)
    )
  (setq font-lock-defaults '(mumps-font-lock-keywords-local nil t))
                                        ;  
  (add-hook 'local-write-file-hooks 'mumps-update-timestamp)
  (add-hook 'local-write-file-hooks 'mumps-remove-trailing-space t) ;should be the last one performed
  (setq mode-line-format mumps-mode-line-format)
  (run-hooks 'mumps-mode-hook)
  )

(if mumps-mode-use-checksum
    (defun mumps-checksum ()
      "Display checksum for the current mumps code in the current buffer. Actually 
what it does is calculate checksum starting from line 2. There are two 
checksum numbers reported. The one in the parenthesis is the checksum 
excluding end-of-line's (the one used by our release team ^%ZaHSUM2."
      (interactive)
      (save-excursion
        (save-restriction
          (save-match-data
            (let (start lastline
                        (sum1 0.0)
                        (sum2 0.0))
              (mumps-locate-timestamp)
              (forward-line 1)
              (setq start (checksum-line-number (point)))
              (setq lastline (checksum-last-line-number))
              (setq sum1 (checksum-lines start lastline sum1))
              (setq sum2 (checksum-lines start lastline sum2 t))
                                        ;(message "Mumps checksum for %s = %10.0f (neol %10.0f)" buffer-file-truename sum1 sum2)))))
              (message "Mumps checksum for %s = %10.0f (neol %10.0f)" (buffer-name) sum1 sum2)))))
      nil)
  )

(when mumps-mode-use-mumps-shell
  (defun mumps-display-mumps-shell (&optional select)
    "Make the Mumps shell buffer visible in a window from a mumps mode buffer.
If the optional argument select is non-nil, it will select the buffer."
    (interactive "P")
    (mumps-shell-display-shell select))

  (defun mumps-save-log (lognum)
    "Save the current file to file with the lognum without changing the visit 
buffer as `FILE.mumps.12345'. Use write-file to do similar and also switch 
the buffer to that new file."
    (interactive "Nlog numer: ")
    (let ((name (buffer-name)))
      (setq name (concat name "." lognum))
      (write-region (point-min) (point-max) name))))


(defun mumps-move-line (linenum)
  "Move the cursor forward/backward linenum lines."
  (interactive "Nnumber of lines: ")
  (let* ((tmp (forward-line linenum)))
    (if (not (= tmp 0)) (message "Out of rang (%d)" tmp))))

(defun mumps-next-tag (&optional reverse)
  "Find the next tag. If reverse is non-nil, find the previous tag."
  (interactive "P")
  (let ((match "^\\S-")
                                        ;(spoint (push-mark (point) t)))
	(spoint (point))
	tpoint
	ok)
    (if reverse
	(setq ok (search-backward-regexp match nil t))
      (end-of-line)
      (setq ok (search-forward-regexp match nil t)))
    (if (not ok)
	(progn (message "No tag found")
	       (goto-char spoint))
      (beginning-of-line)
      (setq tpoint (point))
      tpoint)))

(defun mumps-count-tag-lines ()
  "Return the line number of the current position to the previous tag."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(let (curpos tagpos lines epos taglabel)
	  (beginning-of-line)
	  (setq curpos (point))
	  (setq tagpos (mumps-next-tag t))
	  (if (not tagpos)
              (message "No tag found")
	    (setq epos (search-forward-regexp " " nil t))
            (setq taglabel (buffer-substring tagpos epos))
            (setq lines (count-lines tagpos curpos))
            (message "%d lines to previous tag %s" lines taglabel)))))))

(defun mumps-find-trailing-space (&optional cp)
  "Find a line with a trailing spaces and move to the first space of that 
   trailing spaces. If cp is not nil, it will start search from point cp"
  (interactive)
  (let (pt)
    (if (numberp cp) (goto-char cp))
    (setq pt (re-search-forward "^.*\\s +$" (point-max) t))
    (if pt
	(progn
	  (setq pt (search-backward-regexp "\\S " (point-min) t))
	  (forward-char 1)))
    pt))

(defun mumps-remove-trailing-space (&optional cp)
  "Remove all trailing spaces for each line of text. "
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
        (if (numberp cp) (goto-char cp) (goto-char (point-min)))
	(while (mumps-find-trailing-space)
	  (delete-horizontal-space))
	)))
  nil)


(defun mumps-locate-routine-header (&optional noerr)
  "Locate the routine header"
  (interactive "P")
  (let* ((spoint (point-min)) endline epoint)
    (goto-char spoint)
    (end-of-line)
    (setq endline (point))
    (goto-char spoint)
    (setq epoint (re-search-forward "^Epic Systems Corporation\\^*" endline noerr))
    (if epoint
	(progn
	  (end-of-line 4)
	  (setq endline (point))
	  (beginning-of-line)
	  (setq epoint (re-search-forward "^\\^\\sw+$" endline noerr))
          ))
    ))

(defun mumps-locate-routine-tail (&optional noerr)
  "Locate the routine header"
  (interactive "P")
  (let* ((epoint (point-max)) endline spoint)
    (goto-char epoint)
    (forward-line -1)
    (setq spoint (point))
    (setq epoint (re-search-forward "^\\*\\*\\*\\*\\*\\*\\*$" endline noerr))
    ))

(if mumps-mode-use-mumps-shell
    (progn
      (defun mumps-checkin-routine-with-version-lock (routine &optional ext)
        (interactive "sRoutine: \nP")
        (mumps-shell-checkin-routine-with-version-lock routine ext))

      (defun mumps-find-routine (routine &optional ext)
        (interactive "sRoutine: \nP")
        (save-excursion
          (mumps-shell-find-routine routine ext)))

      (defun mumps-find-routine-read-only (routine &optional ext)
        (interactive "sRoutine: \nP")
        (save-excursion
          (mumps-shell-find-routine-read-only routine)))

      ;;To-do: prompt for save buffer first if there is unsaved chnage in the buffer
      (defun mumps-save-buffer (&optional rellock)
        "Load the current routine back to M environment.  If rellock is 1 or there is
a prefix argument, the M session lock will be released (decrement by 1).
Note, it will load the *saved* plain file back to M.  So unsaved changes in 
the buffer will not be saved into M environment."
        (interactive)
        (save-excursion
          (let (filename)
            (if current-prefix-arg
                (setq rellock "1") (setq rellock "0"))
            (setq filename (buffer-file-name))
            (mumps-shell-load-file filename rellock))))

      ;;This is working but I am not satisified with it yet.
      (defun mumps-check-in-buffer (&optional DLG)
        "RCS check in the file in the current buffer.  DLG is a DLG number used 
to use with ci option -t and -n to stamp the DLG number."
        (interactive "*")
        (save-excursion
          (let (filename cioptions oscmd)
            (if (and (null DLG) current-prefix-arg)
                (setq DLG (read-string "DLG: ")))
            (if (not (null DLG))
                (progn
                  (if (numberp DLG) (setq DLG (number-to-string DLG)))
                  (setq cioptions (concat " -t-DLG" DLG " -nDLG" DLG))))
            (setq cioptions (concat "-l" cioptions))
            (setq filename (mumps-shell-get-server-file-fullname (buffer-file-name)))
            (setq oscmd (mumps-shell-form-string-args (list "ci" cioptions filename)))
            (mumps-shell-OS-cmd oscmd)
            (if (null DLG)
                (message "File %s checked in" filename)
              (message "File %s checked in for %s" filename DLG)))))
      
      (defun mumps-check-out-buffer ()
        "RCS check in the file in the current buffer"
        (interactive)
        (save-excursion
          (let (filename oscmd)
            (setq filename (mumps-shell-get-server-file-fullname (buffer-file-name)))
            (setq oscmd (mumps-shell-form-string-args (list "co" filename)))
            (mumps-shell-OS-cmd oscmd)
            (message "File %s checked out" filename))))


      (defun mumps-lock-routine ()
        (interactive)
        (save-excursion
          (let (filename)
            (setq filename (buffer-file-name))
            (mumps-shell-lock-routine filename))))

      (defun mumps-release-lock-routine ()
        (interactive)
        (save-excursion
          (let (filename)
            (setq filename (buffer-file-name))
            (mumps-shell-release-lock-routine filename)
            )))

      (defun mumps-show-routine-lock-count ()
        "Display the current lock count for the routine in the current buffer.
This is using lookit macro and displayed the lock table in the mumps-shell buffer"
        (interactive)
        (let (filename node)
          (setq filename (file-relative-name (buffer-file-name) (mumps-shell-get-client-file-directory)))
          (setq node (substring filename 0 (string-match "\\." filename)))
                                        ;not finished yet.  Need to pice out and get just the routine name
                                        ;(message "%S" node)
          (mumps-shell-show-lock-count node)
          ))
      ))


(defun mumps-narrow ()
  "Narrow the buffer to show only the real routine body.  For file saved by
^%ZeRSAVE that will include some extra header and trailer."
  (interactive)
  (let (spoint epoint)
    (setq spoint (mumps-locate-routine-header t))
    (if spoint
	(progn
	  (forward-line 1)
	  (setq spoint (point))
	  (setq epoint (mumps-locate-routine-tail t))
	  (if epoint
	      (progn
		(beginning-of-line)
		(setq epoint (point))
		(narrow-to-region spoint epoint)))))))

(defun mumps-widen ()
  "Widen the buffer to show the whole file.  For file saved by ^%ZeRSAVE 
that will include some extra header and trailer."
  (interactive)
  (widen))

(defun mumps-mode-get-previous-version (&optional REV)
  "Visit the original version that before modification if available.  
This is using vc to get the last version"
  (interactive)
  (if (null REV)
      (setq REV (read-string "REV: [1.1]")))
  (if (not (stringp REV))
      (message "REV must be a string")
    (progn
      (if (string= REV "")
          (setq REV "1.1"))
      (vc-revision-other-window REV) ;always assume 1.1 is the original version
      )
    )
  )

(defun mumps-mode-diff-with-previous-version (&optional REV)
  "Do a diff with a previous version in source conrol if available."
  (interactive)
  (let (buf1 buf2)
    (if (null REV)
        (setq REV (read-string "REV: [1.1]")))
    (if (not (stringp REV))
        (message "REV must be a string")
      (progn
        (if (string= REV "")
            (setq REV "1.1"))
        (vc-revision-other-window REV)
        (setq buf1 (concat mumps-mode-file-name ".mumps." mumps-mode-DLG))
        (setq buf2 (concat buf1 "." mumps-mode-rev))
        (ediff-buffers buf1 buf2)
        )
      )
    )
  )

(defun mumps-mode-parse-file-name ()
  "Parse the full file name into three pieces: `mumps-mode-file-name', 
   `mumps-mode-DLG', and `mumps-mode-rev'.  For example, `KNSUBAT1.mumps.12345.~1.1~',
    mumps-mode-file-name : KNSUBAT1
    mumps-mode-DLG:        12345
    mumps-mode-rev:        1.1    <-- when in source control"
  (interactive)
  (let (str p1 p2 p3 p4 s1 s2 s3 s4 e1 e2 e3 e4)
    (setq str buffer-file-name)
                                        ;(if (not (null (string-match "\\(\\w+\\)\.mumps\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str)))
                                        ;(if (not (null (string-match "^.*/\\(\\w+\\)\.mumps\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str))) ;*CW+1 1/04/12 handle buffer without file name
    (if (not (null str))
        (if (not (null (string-match "^.*/\\(\\w+\\)\.mumps?\.\\(\\w+\\)\\($\\|.\\.*\\)\\(.*\\)" str)))
            (progn
              (setq s1 (match-beginning 1))
              (setq s2 (match-beginning 2))
              (setq s3 (match-beginning 3))
              (setq s4 (match-beginning 4))

              (setq e1 (match-end 1))
              (setq e2 (match-end 2))
              (setq e3 (match-end 3))
              (setq e4 (match-end 4))

              (if (not (null s1)) (setq p1 (substring str s1 e1)))
              (if (not (null s2)) (setq p2 (substring str s2 e2)))
              (if (not (null s3)) (setq p3 (substring str s3 e3)))
              (if (not (null s4)) (setq p4 (substring str s4 e4)))
                                        ;piece 3 now is just to match the additional `.' if in source control: `ABC.mumps.1234.~1.1~'
                                        ;(message "P1=%s, P2=%s, P3=%s, P4=%s, P5=%s" p1 p2 p3 p4 p5)
              (setq mumps-mode-file-name p1)
              (setq mumps-mode-DLG p2)
              (setq mumps-mode-rev p4)
              )))))

(provide 'mumps-mode)

;;
;;EOF mumps-mode.el




