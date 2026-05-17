;;; vc_annotate_setup.el --- vc-annotate tweaks  -*- lexical-binding: t; -*-
;;; Commentary:
;; Better blame flags, line-aware p/n navigation, full commit on `d`.
;;; Code:

(require 'vc-annotate)

(setq vc-git-annotate-switches '("-w" "-M" "-C" "-C" "-C"))

;; Colour the annotation column by age instead of painting full-line
;; backgrounds, which makes the source itself unreadable on dark themes.
(setq vc-annotate-background-mode nil)

(defun kpi/vc-annotate-show-commit-at-line ()
  "Show the full commit (message + diff) for the revision at point via magit."
  (interactive)
  (require 'magit)
  (let ((rev (car (vc-annotate-extract-revision-at-line))))
    (magit-show-commit rev)))

(defvar kpi/vc-annotate-line-history nil
  "Alist of (file . stack-of-revisions) tracking `p` -> `n` navigation.")

(defun kpi/vc-annotate-line-prev ()
  "Re-annotate at the parent of this line's commit; remember where we came from."
  (interactive)
  (let ((file vc-annotate-parent-file)
        (rev vc-annotate-parent-rev))
    (when (and file rev)
      (push rev (alist-get file kpi/vc-annotate-line-history nil nil #'equal)))
    (vc-annotate-revision-previous-to-line)))

(defun kpi/vc-annotate-line-next ()
  "Step forward through the revisions pushed by `kpi/vc-annotate-line-prev`."
  (interactive)
  (let* ((file vc-annotate-parent-file)
         (stack (alist-get file kpi/vc-annotate-line-history nil nil #'equal)))
    (if (null stack)
        (message "No forward history for %s" (file-name-nondirectory (or file "")))
      (let ((rev (pop stack)))
        (setf (alist-get file kpi/vc-annotate-line-history nil nil #'equal) stack)
        (vc-annotate file rev vc-annotate-parent-display-mode
                     (current-buffer))))))

(defun kpi/vc-annotate-apply-bindings ()
  "Install vc-annotate key overrides.
Runs from the mode hook so it wins over evil-collection's setup,
which registers via `eval-after-load 'vc-annotate` and clobbers
normal-state bindings applied at load time."
  (let ((pairs `(("d" . ,#'kpi/vc-annotate-show-commit-at-line)
                 ("p" . ,#'kpi/vc-annotate-line-prev)
                 ("n" . ,#'kpi/vc-annotate-line-next)
                 ("P" . ,#'vc-annotate-prev-revision)
                 ("N" . ,#'vc-annotate-next-revision))))
    (dolist (kv pairs)
      (define-key vc-annotate-mode-map (kbd (car kv)) (cdr kv)))
    (when (fboundp 'evil-define-key*)
      (dolist (state '(normal motion emacs))
        (dolist (kv pairs)
          (evil-define-key* state vc-annotate-mode-map
            (kbd (car kv)) (cdr kv)))))))

(add-hook 'vc-annotate-mode-hook #'kpi/vc-annotate-apply-bindings)

(provide 'vc_annotate_setup)
;;; vc_annotate_setup.el ends here
