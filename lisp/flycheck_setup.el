
(require 'flycheck) ; noqa
;;; Code:

; JSX checker
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

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
