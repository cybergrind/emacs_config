(defun x-paste ()
  "insert text on X11's clipboard to current buffer."
  (interactive)
  (insert (shell-command-to-string "xsel -b")))

(defun x-copy ()
  "copy text on local kill-ring to X11's clipboard."
  (interactive)
  (copy-region-as-kill (point) (mark t))
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
      (process-send-string proc (car kill-ring))
              (process-send-eof proc))))

(provide 'x-clipboard)
