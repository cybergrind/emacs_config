
(require 'flymake-cursor)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

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

(provide 'flymake_cust)
