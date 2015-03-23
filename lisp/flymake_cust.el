
(require 'flymake-cursor)

(defun get_linter ()
  (if (file-exists-p "/usr/bin/flake8-python2")
      "/usr/bin/flake8-python2"
    "flake8-python2"
    "pyflakes-python2"
  ))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) ()))
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
    (list (get_linter) (list local-file)))))


  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

(when (load "flymake" t)
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jslint" (list "--sloppy" local-file))))

  (setq flymake-err-line-patterns
        (cons '("Error:\\([[:digit:]]+\\):\\([[:digit:]]+\\):\\(.*\\)$"
                nil 1 2 3)
                    flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init))

  (require 'flymake-cursor)
)

(add-hook 'espresso-mode-hook
            (lambda ()
      (flymake-mode 1)
      (define-key espresso-mode-map "\C-c\C-n" 'flymake-goto-next-error)))

(add-hook 'python-mode-hook
            (lambda ()
      (flymake-mode 1)
      (define-key python-mode-map "\C-c\C-n" 'flymake-goto-next-error)))

;(defun flymake-erlang-init ()
;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                     'flymake-create-temp-inplace))
;         (local-file (file-relative-name temp-file
;                                         (file-name-directory buffer-file-name))))
;    (list "/home/kpi/.emacs.d/check_erlang.erl" (list local-file))))
(defun flymake-erlang-init ()
     ; Make sure it's not a remote buffer or flymake would not work
  (when 't
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/kpi/.emacs.d/check_erlang.erl" (list local-file))))
)
(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
(setq flymake-log-level 3)
(add-hook 'erlang-mode-hook '(lambda () (flymake-mode t)))

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

(provide 'flymake_cust)
