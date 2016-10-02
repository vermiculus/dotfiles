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

(defgroup mumps nil
  "MUMPS editing"
  :group 'languages)

(defvar mumps-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

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
  (set-syntax-table mumps-mode-syntax-table)
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
  `((,(rx line-start (group (+ (any "%" alphanumeric))))    1 font-lock-function-name-face t)
    (,(rx ";" (* whitespace) (group "#" (*? anything) "#")) 1 font-lock-preprocessor-face t)
    (,(rx (group "$$")
          (group (+ (any "%" alphanumeric))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(rx "$" (+ alpha))                                    . font-lock-builtin-face)
    (,(rx "^" (+ (any "[" "]" "%" alphanumeric)))           . font-lock-constant-face)
    (,(rx letter (* letter))                                . font-lock-keyword-face)
    "Additional expressions to highlight in mumps mode."))

(add-to-list 'auto-mode-alist
             (cons "\\.ROU$" 'mumps-mode))

(provide 'mumps-mode)
;;; mumps-mode.el ends here
