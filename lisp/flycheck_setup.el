(require 'flycheck) ; noqa

(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(add-hook 'after-init-hook #'global-flycheck-mode)
(provide 'flycheck_setup)
;;; flycheck_setup.el ends here
