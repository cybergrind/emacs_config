
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
(require 'smex)
;; smex delayed initialization
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(setq erlang-indent-level 2)
(require 'erlang-start)


(autopair-global-mode t)
(show-paren-mode 1)
(ido-mode t)


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
 '(nil nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "lightyellow" :foreground "black"))))
 '(flymake-warnline ((t (:background "#999" :foreground "black"))))
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t nil)))
 '(whitespace-space ((t nil)))
 '(whitespace-tab ((t nil))))
