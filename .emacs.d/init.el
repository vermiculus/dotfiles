
(defun create-temporary-file (ext)
(interactive "sExtension: ")
(find-file (make-temp-file "" nil (concat "." ext))))

(load
 (setq custom-file ".emacs-custom.el"))
