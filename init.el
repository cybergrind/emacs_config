
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/auto-complete"))

(require 'flymake_cust)
(require 'auto-complete)
(require 'vis_cust)
(require 'autopair)
(require 'paredit)
(require 'espresso)


(autopair-global-mode t)
(show-paren-mode 1)


;;(autoload 'espresso-mode "espresso" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))

;;(autoload 'puppet-mode "puppet" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (setq-default indent-tabs-mode nil)
 (ido-mode t)
 (menu-bar-mode -1) 
 
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "lightyellow" :foreground "black"))))
 '(whitespace-empty ((t nil)))
 '(whitespace-space ((t nil)))
 '(whitespace-tab ((t nil)))
 )
