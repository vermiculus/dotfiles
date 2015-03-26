;;; mumps-mode.el --- mumps editing support

;; Copyright (C) 2015  

;; Author: sallred
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on code provided in the wiki, but heavily extended.

;;; Code:

(defvar mumps-mode-syntax-table
  (let ((table (make-syntax-table
                (standard-syntax-table))))
    ;; change the standard string escape to be treated as a punctuation mark
    (modify-syntax-entry ?\\ "." table)
    ;; start comments with a semicolon
    (modify-syntax-entry ?\; "<" table)
    ;; end comments with a newline
    (modify-syntax-entry ?\n ">" table)
    table))

(defgroup mumps nil
  "MUMPS editing"
  :group 'languages)

(defcustom mumps-globals-alist
  nil
  "Globals to highlight and provide for completion.

Each entry in the alist is its name and a description to
display."
  :group 'mumps)

;;;###autoload
(defun mumps-mode ()
  "Mode for highlighting Mumps program files"
  (interactive)
  (setq mode-name "mumps-mode"
        major-mode 'mumps-mode)
  (make-local-variable 'font-lock-defaults)
  ;; warning: syntactic analysis is needed for strings with semicolons to render properly
  ;; uncomment the following line to disable syntactic analysis of strings and comments.
  ;(setq font-lock-keywords-only t)
  (set-syntax-table mumps-mode-syntax-table)
  ;; uncomment the following line to use an alternate comment-marking strategy
  ;(setq comment-start-skip ";")
  (setq font-lock-defaults '(mumps-font-lock-keywords nil t))
  (font-lock-mode t))

(defun mumps--command-variants (command &optional stack)
  "Returns the possible abbreviations of COMMAND.
    For example, input of \"set\" would yield the list
      \(\"s\" \"se\" \"set\")"
  (let ((case-insensitive-command-list
         (mumps--case-insensitize command))))
  (if (= 1 (length command))
      (cons command stack)
    (mumps--command-variants
     (substring command 0 (1- (length command)))
     (cons command stack))))

(defun mumps--case-insensitize (string)
  (unless (string-equal "" string)
    ))

(defvar mumps-font-lock-keywords
  `(
   ;; strings - if not done with syntactic analysis
   ;'("\"[^\"]*\"" . font-lock-string-face)
   ;; comments - if not done with syntactic analysis
   ;'(";.*$" 0 font-lock-comment-face t)
   ;; preprocessor tags and comments
    (,(rx ";"
          (* whitespace)
          (group "#"
                 (* (not (any "#")))
                 "#"))
     1 font-lock-preprocessor-face t)
    ;; tags
    (,(rx (+ (not (any "%" alphanumeric))))
     . font-lock-function-name-face)
    ;; function calls, match $$^ROU by not forcing a tag name
    (,(rx "$$" (* (any "%" alphanumeric)))
     . font-lock-function-name-face)
    ;; built-in functions
    (,(rx "$" (+ (any "%" alphanumeric)))
     . font-lock-builtin-face)
    ;; Globals, accounting for brackets (sloppy)
    ( ;(rx "^" (+ (any "[" "]" "%" alphanumeric)))
     ; I think the above is correct, but I'm not sure.
     "\\^[]%[:alnum:][]+" . font-lock-constant-face)
    ;; single-character commands
    (,(rx (+ (any " " "."))
          (group letter)
          (group (| " " ":" "$")))
     1 font-lock-keyword-face)
    "Additional expressions to highlight in mumps mode."))

(add-to-list 'auto-mode-alist
             (cons "\\.ROU$" 'mumps-mode))

(provide 'mumps-mode)
;;; mumps-mode.el ends here
