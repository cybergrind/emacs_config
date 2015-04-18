(defgroup yapf nil
  "Emacs integration with yapf"
  :prefix "yapf-"
  :group 'applications)

(defcustom yapf-args '(" ")
  "Arguments for yapf script."
  :type '(repeat string)
  :group 'yapf)

(defcustom yapf-error-buffer-name "*yapf-error*"
  "Buffer name of yapf error."
  :type 'string
  :group 'yapf)

(defcustom yapf-display-error-buffer nil
  "Display error buffer on error."
  :type 'boolean
  :group 'yapf)

;;;###autoload
(defun yapf-region (begin end)
  "Apply yapf over to region BEGIN to END points."
  (interactive "r")
  (let* ((command (mapconcat 'identity (cons "yapf2" yapf-args) " ")))
    (atomic-change-group
      (or (zerop (shell-command-on-region begin end command nil 'replace yapf-error-buffer-name yapf-display-error-buffer))
          (error "Command exited abnormally.  See %s for details" yapf-error-buffer-name)))))

;;;###autoload
(defun yapf-buffer ()
  "Apply yapf to current buffer."
  (interactive)
  (yapf-region (point-min) (point-max)))

(provide 'yapf)
