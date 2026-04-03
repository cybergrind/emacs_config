(defun x-clipboard--ensure-wayland-display ()
  "Ensure WAYLAND_DISPLAY is set, detecting the socket if needed."
  (unless (getenv "WAYLAND_DISPLAY")
    (let ((socket (car (directory-files "/run/user/1000/" nil "^wayland-[0-9]+$"))))
      (when socket
        (setenv "WAYLAND_DISPLAY" socket)))))

(defun x-paste ()
  "insert text on X11's clipboard to current buffer."
  (interactive)
  (x-clipboard--ensure-wayland-display)
  (let ((input-method-function nil)
        ;; xsel -o
        (str (shell-command-to-string "wl-paste")))
    (insert str)))

(defun x-copy ()
  "copy text on local kill-ring to X11's clipboard."
  (interactive)
  (x-clipboard--ensure-wayland-display)
  (copy-region-as-kill (point) (mark t))
  (let ((process-connection-type nil))
    ;; "xsel" "-i" "-b"
    (let ((proc (start-process "xsel" "*Messages*" "wl-copy")))
      (process-send-string proc (car kill-ring))
              (process-send-eof proc))))

(provide 'x-clipboard)
