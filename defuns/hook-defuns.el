;;; hook-defuns.el --- hooks
;;; Commentary:
;;; Code:
(require 'whitespace)

(defun clean-hook ()
  "Cleanup whitespaces before save."
  (interactive)
  (add-hook 'before-save-hook
            (lambda ()
              (whitespace-cleanup))))
;;; hook-defuns.el ends here
