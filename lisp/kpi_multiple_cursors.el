(require 'multiple-cursors)

(defun mci/message ()
  (if (eq mc/mark-more-like-this-extended-direction 'up)
      (message "p to mark previous, P to skip, N to remove, n to mark next ./, cycle cursors")
    (message "n to mark next, N to skip, P to remove, p to mark previous ./, cycle cursors")))

(defun mci/up ()
  (interactive)
  (mc/mark-previous-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'up)
  (mci/message))

(defun mci/down ()
  (interactive)
  (mc/mark-next-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'down)
  (mci/message))

(defun mci/left ()
  (interactive)
  (if (eq mc/mark-more-like-this-extended-direction 'down)
      (mc/unmark-next-like-this)
    (mc/skip-to-previous-like-this))
  (mci/message))

(defun mci/right ()
  (interactive)
  (if (eq mc/mark-more-like-this-extended-direction 'up)
      (mc/unmark-previous-like-this)
    (mc/skip-to-next-like-this))
  (mci/message))


(defvar mc/interactive-keymap (make-sparse-keymap))
(define-key mc/interactive-keymap (kbd "p") 'mci/up)
(define-key mc/interactive-keymap (kbd "n") 'mci/down)
(define-key mc/interactive-keymap (kbd "P") 'mci/left)
(define-key mc/interactive-keymap (kbd "N") 'mci/right)
(define-key mc/interactive-keymap (kbd ".") 'mc/cycle-forward)
(define-key mc/interactive-keymap (kbd ",") 'mc/cycle-backward)


(defun mci/mark ()
  (interactive)
  ;; if no selection - select current word with M-@
  (if (not (use-region-p))
      (progn
        (mark-word)
        ))
  (mci/down)
  (set-transient-map mc/interactive-keymap t))

(use-package multiple-cursors
  :bind
  (("M-N" . mci/mark)
   ("M-P" . mc/mark-previous-like-this)
   ("M-A" . mc/mark-all-like-this)
   ("M-E" . mc/mark-more-like-this-extended)
   :map
   mc/keymap
   ("M-'" . mc-hide-unmatched-lines-mode))

  :config
  (add-to-list 'mc--default-cmds-to-run-once 'mci/mark)
  (add-to-list 'mc--default-cmds-to-run-once 'mci/up)
  (add-to-list 'mc--default-cmds-to-run-once 'mci/down)
  (add-to-list 'mc--default-cmds-to-run-once 'mci/left)
  (add-to-list 'mc--default-cmds-to-run-once 'mci/right)

  (define-key mc/mark-more-like-this-extended-keymap (kbd ".") 'mc/cycle-forward)
  (define-key mc/mark-more-like-this-extended-keymap (kbd ",") 'mc/cycle-backward)
  )


(provide 'kpi_multiple_cursors)
