;;; lisp-defuns.el --- lisp defuns
;;; Commentary:
;; https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
;;; Code:
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defmacro basis/define-keys (keymap &rest keydefs)
  "Define multiple key bindings for KEYMAP."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (let* ((key (car keydef))
                        (def (cadr keydef))
                        (kbd (if (vectorp key) key `(kbd ,key))))
                   `(define-key ,keymap ,kbd ,def)))
               keydefs)))

(defun charged-kill (arg)
  (interactive "P")
  (message "Arg: %s" arg)
  (pcase arg
    ('(4) (kill-whole-line))
    ('(16) (sp-kill-sexp))
    (arg (sp-kill-hybrid-sexp))))


;;; lisp-defuns.el ends here
