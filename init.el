(require 'cl)
(defvar *emacs-load-start* (current-time))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/auto-complete"))
(add-to-list 'load-path (concat dotfiles-dir "/icicles"))
(add-to-list 'load-path (concat dotfiles-dir "/bookmark-plus"))
(add-to-list 'load-path (concat dotfiles-dir "/anything-config"))
(add-to-list 'load-path (concat dotfiles-dir "/magit"))
(add-to-list 'load-path (concat dotfiles-dir "/tramp"))


(require 'anything-config)
(require 'flymake_cust)
(require 'auto-complete)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)
;;(require 'tramp)
;(autoload 'auto-complete "auto-complete" nil t)
(require 'vis_cust)
(autoload 'autopair-global-mode "autopair" nil t)
(autoload 'paredit "paredit" nil t)
(autoload 'espresso-mode "espresso" nil t)
(autoload 'smex-initialize "smex" "smex-initialize" t)
(require 'magit)


;; tramp configs
(tramp-set-completion-function "ssh"
'((tramp-parse-sconfig "/etc/ssh_config")
(tramp-parse-sconfig "~/.ssh/config")))
(setenv "SHELL" "/bin/sh") ;; workaround for zsh
(setq tramp-default-method "scp")
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer t)
(setq tramp-verbose 10)


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

(autoload 'icicle-mode "icicles" nil t)
(global-set-key [(meta s) (i)] (lambda ()
				 (interactive)
				 (icicle-mode)))
;;(run-at-time "0.8 sec" nil 'icicles)

(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(setq erlang-indent-level 2)
(require 'erlang-start)


;(autopair-global-mode t)
;;(auto-complete-mode 1)
(add-to-list 'ac-modes 'python)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(show-paren-mode 1)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq confirm-kill-emacs nil)
;; remove <2> on doubled buffers
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'forward
  uniquify-separator ":")
;; no tabs
(setq-default indent-tabs-mode nil)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


;;(autoload 'espresso-mode "espresso" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))

(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
;;(add-to-list 'auto-mode-alist '("" . auto-complete-mode))

;;additional repos
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(icicle-buffers-ido-like-flag t)
 '(icicle-files-ido-like-flag t)
 '(nil nil t)
 '(safe-local-variable-values (quote ((test-case-name . twotp\.test) (test-case-name . twotp\.test\.test_term) (test-case-name . twotp\.test\.test_server) (test-case-name . twotp\.test\.test_client) (test-case-name . twotp\.test\.test_node) (test-case-name . twotp\.test\.test_epmd))))
 '(org-directory "~/Dropbox/gtd")
 '(org-mobile-directory "~/Dropbox/gtd")
 '(org-mobile-inbox-for-pull "~/Dropbox/gtd/mobileorg.org")
 '(safe-local-variable-values (quote ((test-case-name . twisted\.test\.test_logfile) (test-case-name . twisted\.test\.test_log) (test-case-name . twisted\.test\.test_internet) (test-case-name . twisted\.test\.test_reflect) (codiing . utf-8))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "lightyellow" :foreground "black"))))
 '(flymake-warnline ((t (:background "#999" :foreground "black"))))
 '(rst-level-1-face ((t (:background "grey10"))) t)
 '(rst-level-2-face ((t nil)) t)
 '(rst-level-3-face ((t nil)) t)
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t nil)))
 '(whitespace-space ((t nil)))
 '(whitespace-tab ((t nil))))


;(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;                           (- (+ lo ms) (+ (second *emacs-load-start*) (third *emacs-load-start*)))))

(message "%s" (current-time))
(put 'set-goal-column 'disabled nil)

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/lib/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-repl))

(add-hook 'slime-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'not-working-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
