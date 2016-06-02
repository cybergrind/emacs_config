;;; hook-defuns.el --- hooks
;;; Commentary:
;;; Code:
(require 'whitespace)

(defun clean-hook ()
  "Cleanup whitespaces before save."
  (add-hook 'before-save-hook
            (lambda ()
              (interactive)
              (whitespace-cleanup))))
;;; hook-defuns.el ends here
