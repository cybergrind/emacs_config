
(require 'flycheck) ; noqa
;;; Code:

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-eslintrc nil)


(flycheck-add-mode 'javascript-eslint 'web-mode)

(add-hook 'web-mode-hook (lambda ()
                           (flycheck-select-checker 'javascript-eslint)
                           (flycheck-mode)))

; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (flycheck-mode)))

(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'flycheck_setup)
;;; flycheck_setup.el ends here
