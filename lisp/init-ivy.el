;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package ivy
  :after (flx)
  :diminish
  :config
  (ivy-mode)
  (setq-default ivy-use-virtual-buffers t
                  ivy-virtual-abbreviate 'fullpath
                  ivy-count-format ""
                  projectile-completion-system 'ivy
                  ivy-magic-tilde nil
                  ivy-dynamic-exhibit-delay-ms 150
                  ivy-use-selectable-prompt t
                  ivy-initial-inputs-alist
                  '((Man-completion-table . "^")
                    (woman . "^")))
  (setq-default ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy)))
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done)
        ("C-j" . ivy-immeidate-done)
        ("C-RET" . ivy-immeidate-done)
        ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode)
        )
  )

(use-package smex)

(use-package counsel
  :after (ivy smex)
  :diminish
  :bind
  (("M-x" . counsel-M-x))
  :config
  (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))

(use-package swiper
  :after (ivy)
  :config
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point))

(provide 'init-ivy)
;;; init-ivy.el ends here
