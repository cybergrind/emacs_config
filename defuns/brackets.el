;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Brackets

(defun basis/paredit-doublequote-space-p (endp delimiter)
  ;; For use as a member of `paredit-space-for-delimiter-predicates'
  (not (and (not endp)
            (eq delimiter ?\")
            (derived-mode-p 'lisp-mode 'common-lisp-mode 'inferior-lisp-mode
                            'slime-repl-mode)
            (let ((point (point)))
              (and (eq (char-before point) ?p)
                   (eq (char-before (1- point)) ?#))))))

(defun basis/paredit-splicing-unquote-p (endp delimiter)
  ;; For use as a member of `paredit-space-for-delimiter-predicates'
  (not (and (not endp)
            (eq delimiter ?\()
            (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode
                            'lisp-mode 'common-lisp-mode 'inferior-lisp-mode
                            'slime-repl-mode)
            (let ((point (point)))
              (and (eq (char-before point) ?@)
                   (eq (char-before (1- point)) ?,))))))

(defun basis/paredit-open-something ()
  (interactive)
  (call-interactively
   (if (memq major-mode '(clojure-mode cider-repl-mode))
       #'paredit-open-square
     #'paredit-open-round)))

(defun basis/paredit-kill-something ()
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'paredit-backward-kill-word)))

(defun basis/paredit-wrap-from-behind (wrapper &optional spacep)
  (paredit-backward)
  (funcall wrapper)
  (when spacep
    (insert " ")
    (forward-char -1)))

(defun basis/paredit-wrap-round-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-round t))

(defun basis/paredit-wrap-square-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-square nil))

(defun basis/paredit-wrap-curly-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-curly nil))

(defun basis/sp-backward-delete-no-prefix (args)
  "Advice for `sp-backward-delete-char'.
Do not treat raw universal arguments specially (treat it as a
numeric argument)."
  (cons (prefix-numeric-value (car args))
        (cdr args)))

(defun basis/sp-kill-something ()
  "Call `sp-backward-kill-word' or `kill-region'. "
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'sp-backward-kill-word)))

(defun basis/sp-kill-sexp (&optional arg)
  "Variant of `sp-kill-sexp'.
If called inside a symbol, only kill to the end of the
symbol (like `kill-sexp')."
  (interactive "P")
  (if (and (not arg)
           (looking-at-p "\\sw\\|\\s_")
           (not (looking-at-p "\\_<")))
      (kill-sexp 1)
    (sp-kill-sexp arg)))

(defmacro basis/def-sp-backspace-command (name command)
  (declare (indent defun))
  (let ((doc (ignore-errors (documentation (eval command) t))))
    `(progn
       (defun ,name ()
         ,@(and doc (list doc))
         (interactive)
         (cl-letf (((symbol-function 'backward-delete-char)
                    #'sp-backward-delete-char)
                   ((symbol-function 'backward-delete-char-untabify)
                    #'sp-backward-delete-char))
           (call-interactively ,command)))
       (put ',name 'delete-selection 'supersede)
       ',name)))

(basis/def-sp-backspace-command basis/sp-python-backspace
  #'python-indent-dedent-line-backspace)

(basis/def-sp-backspace-command basis/sp-yaml-backspace
  #'yaml-electric-backspace)

(basis/def-sp-backspace-command basis/sp-markdown-backspace
  #'markdown-exdent-or-delete)

(defun basis/sp-comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters or send an EOF to subprocess."
  ;; Copy the code of `comint-delchar-or-maybe-eof' rather than using advice
  ;; and/or `cl-letf' to avoid recursion errors.
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
	(comint-send-eof)
      (sp-delete-char arg))))

(put 'basis/sp-comint-delchar-or-maybe-eof 'delete-selection 'supersede)

(defun basis/sp-backward-up (&optional arg _interactive)
  "Like `sp-backward-up-sexp' but augmented in `python-mode'.
In `python-mode', fall back to `python-nav-backward-up-list' if
`sp-backward-up-sexp' doesn't move point."
  (interactive "^p\np")
  (if (eq major-mode 'python-mode)
      (catch 'done
        (let ((arg (or arg 1)))
          (while (> arg 0)
            (let ((point (point)))
              (sp-backward-up-sexp 1 1)
              (when (= (point) point)
                (python-nav-backward-up-list 1))
              (when (= (point) point)
                (throw 'done t))
              (setq arg (1- arg))))))
    (call-interactively #'sp-backward-up-sexp)))

(defun basis/sp-point-after-word-p (id action context)
  "Augmented version of `sp-point-after-word-p'.
Handle the special string literals in Python and SQL."
  (and (sp-point-after-word-p id action context)
       (let ((regexp (pcase major-mode
                       ((or `python-mode `inferior-python-mode)
                        "\\_<[BbRrUu]")
                       ((or `sql-mode `sql-interactive-mode)
                        "\\_<[BbNnXx]"))))
         (or (null regexp) ; No change for this mode
             (not (and (member id '("'" "\""))
                       (save-excursion (forward-char -2)
                                       (looking-at-p regexp))))))))

(defvar basis/sp-inhibit-cleanup-list
  '(indent-relative
    indent-relative-maybe
    python-indent-line-function
    haskell-indentation-indent-line)
  "Indentation functions for which to inhibit smartparens's cleanup.")

(defun basis/sp-unwrap-no-cleanup (args)
  "Advice for `sp--unwrap-sexp' to inhibit problematic cleanup."
  (if (memq indent-line-function basis/sp-inhibit-cleanup-list)
      (list (car args) t)
    args))

(defun basis/sp-cleanup-maybe-not (&rest _)
  "Advice for `sp--cleanup-after-kill' to inhibit problematic cleanup."
  ;; Used as `:before-until' advice, so returning non-nil prevents cleanup
  (memq indent-line-function basis/sp-inhibit-cleanup-list))
