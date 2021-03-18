;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package wgrep)

(use-package ivy
  :after (flx)
  :diminish
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'fullpath)
  (ivy-count-format "")
  (ivy-use-selectable-prompt t)
  (projectile-completion-system 'ivy)
  (ivy-magic-tilde nil)
  (ivy-dynamic-exhibit-delay-ms 150)
  (ivy-display-style 'fancy)
  (ivy-initial-inputs-alist
   '((Man-completion-table . "^")
     (woman . "^")))
  :config
  (ivy-mode)
  (add-to-list 'ivy-height-alist (cons 'counsel-ag 20))
  (add-to-list 'ivy-re-builders-alist
               '(t . ivy--regex-ignore-order))
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done)
        ("M-d" . backward-kill-word)
        ("C-j" . ivy-immeidate-done)
        ("C-RET" . ivy-immeidate-done)
        ("C-c C-q" . ivy-wgrep-change-to-wgrep-mode)))

(use-package smex)

(use-package counsel
  :after (ivy smex)
  :diminish
  :bind
  (
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . counsel-switch-buffer)
   ("C-x C-r" . counsel-buffer-or-recentf)
   ;; cousel-buffer-or-recentf
   ;; ("C-r" . counsel-grep-or-swiper)
   ;; ("C-s" . swiper)
   ))

(use-package swiper
  :after (ivy)
  :config
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point))

(provide 'init-ivy)
;;; init-ivy.el ends here
