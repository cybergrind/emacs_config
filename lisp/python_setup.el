(require 'flycheck) ; noqa

; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (flycheck-mode)))

(provide 'python_setup)
