
(setq desktop-path (list default-directory))
(setq desktop-dirname default-directory)
(setq desktop-base-file-name ".emacs.desktop")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
            '(lambda ()
               (desktop-save-in-desktop-dir)
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
                    (if (saved-session)
                         (if (y-or-n-p "Restore desktop? ")
                                  (session-restore)))))


(provide 'multi_desktop)
