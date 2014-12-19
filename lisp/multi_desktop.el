
(require 'cl)

(setq desktop-path (list default-directory))
(setq desktop-dirname default-directory)
(setq desktop-base-file-name ".emacs.desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
            '(lambda ()
               (windows-restore)
               (setq inhibit-start-screen 1)
               (setq inhibit-splash-screen 1)
               (desktop-save-in-desktop-dir)
               ;; windows-restore-save <= runs after next string
               (desktop-save-mode 1)
               (add-hook 'kill-emacs-hook 'force-save-desktop)))
               
(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found."))
  
  )

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save-in-desktop-dir)
        (message "Session not saved."))
  ))

(defun force-save-desktop ()
  (interactive)
  (desktop-save-in-desktop-dir))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
          '(lambda ()
             (if (and (saved-session)
                      (y-or-n-p "Restore desktop"))
                 (session-restore))))

;; frame save/restore support
;; originally http://www.emacswiki.org/emacs/frame-restore.el

(defun open-buffer (name)
  (let ((len (length (window-list))))
    (cond
     ((not (get-buffer name)) ())
     ((equal len 1) (progn
                      (split-window-horizontally)
                      (set-window-buffer (first (window-list)) name)))
     ((equal len 2) (set-window-buffer (second (window-list)) name))
     (t ()))))


(defun windows-restore ()
  "Restore frame from `frame-restore-params'."
  (if (boundp 'w-restore-params)
      (mapcar 'open-buffer w-restore-params)))

;(add-hook 'desktop-after-read-hook 'windows-restore)

;; Add our vars to the save list so `desktop.el' will save them out to disk
(defun windows-restore-save ()
  "Save the frame parameters in `frame-restore-params'."
  (add-to-list 'desktop-globals-to-save 'w-restore-params)
  (setq w-restore-params (buffer-names)))

(defun window-to-buffer (win)
  (buffer-name (window-buffer win)))

(defun buffer-names ()
  (mapcar 'window-to-buffer (window-list)))

(add-hook 'desktop-save-hook 'windows-restore-save)



(provide 'multi_desktop)
