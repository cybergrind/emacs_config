(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")
(setq packages-list '(clojure-mode magit paredit anything smex
                                   python-mode slim-mode slime ace-jump-mode
                                   tramp auto-complete scala-mode2
                                   puppet-mode hexrgb fuzzy-match prolog-el
                                   js2-mode yaml-mode bookmark+
                                   groovy-mode rust-mode nimrod-mode))
(el-get 'sync packages-list)

(require 'cl)
(defvar *emacs-load-start* (current-time))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
;(add-to-list 'load-path (concat dotfiles-dir "/tramp/lisp"))


(require 'anything-config)
(require 'flymake_cust)
(require 'auto-complete)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

;(require 'tramp)
;(autoload 'auto-complete "auto-complete" nil t)
(require 'vis_cust)
(autoload 'autopair-global-mode "autopair" nil t)
(autoload 'paredit "paredit" nil t)
;(autoload 'espresso-mode "espresso" nil t)
(autoload 'smex-initialize "smex" "smex-initialize" t)
;(require 'magit)


;; tramp configs
;(tramp-set-completion-function "ssh"
;'((tramp-parse-sconfig "/etc/ssh_config")
;(tramp-parse-sconfig "~/.ssh/config")))
;(setenv "SHELL" "/bin/sh") ;; workaround for zsh
;(setq tramp-default-method "scp")
;(setq tramp-default-method "ssh")
;(setq tramp-debug-buffer t)
;(setq tramp-verbose 10)
;(require 'tramp-cmds)


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
(setq erlang-indent-level 4)
(require 'erlang-start)


;(autopair-global-mode t)
;;(auto-complete-mode 1)
(add-to-list 'ac-modes 'python)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(show-paren-mode 1)

;(ido-mode t)
(ido-mode 'both)
(setq ido-save-directory-list-file "~/.emacs.d/var/ido")
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


(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nim$" . nimrod-mode))
;;(add-to-list 'auto-mode-alist '("" . auto-complete-mode))

(require 'multi_desktop)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(edts-man-root "~/.emacs.d/edts/doc/R16B02")
 '(icicle-buffers-ido-like-flag t)
 '(icicle-files-ido-like-flag t)
 '(nil nil t)
 '(nrepl-host "192.168.42.129")
 '(nrepl-port 9999)
 '(org-agenda-files (quote ("~/Dropbox/gtd/calendar.org")))
 '(org-directory "~/Dropbox/gtd")
 '(org-mobile-directory "~/Dropbox/gtd")
 '(org-mobile-inbox-for-pull "~/Dropbox/gtd/mobileorg.org")
 '(safe-local-variable-values (quote ((python-indent-offset . 4) (erlang-mode . 1) (erlang-mode\;erlang-indent-level . 4) (erlang\;erlang-indent-level . 4) (encoding . utf-8) (test-case-name . twisted\.web\.test\.test_xmlrpc) (test-case-name . test\.test_txpostgres) (test-case-name . twisted\.test\.test_logfile) (test-case-name . twisted\.test\.test_log) (test-case-name . twisted\.test\.test_internet) (test-case-name . twisted\.test\.test_reflect) (codiing . utf-8))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-completion-face ((t (:underline t))))
 '(custom-state ((t (:foreground "color-43"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-changed ((t (:underline t))))
 '(diff-context ((t nil)))
 '(diff-file-header ((t (:background "grey10" :weight bold))))
 '(diff-header ((t (:background "grey10"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(flymake-errline ((t (:background "color-83" :foreground "black"))))
 '(flymake-warnline ((t (:foreground "#6c6c6c" :underline (:color "#ff0087" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "color-208"))))
 '(font-lock-function-name-face ((t (:foreground "brightblue"))))
 '(font-lock-keyword-face ((t (:foreground "DarkOliveGreen1"))))
 '(font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
 '(font-lock-string-face ((t (:foreground "color-34"))))
 '(font-lock-type-face ((t (:foreground "color-47"))))
 '(font-lock-variable-name-face ((t (:foreground "color-51"))))
 '(highlight ((t (:background "darkseagreen4"))))
 '(isearch-fail ((t (:background "Grey10"))))
 '(lazy-highlight ((t (:background "grey20"))))
 '(match ((t (:background "pink" :foreground "darkblue"))))
 '(minibuffer-prompt ((t (:foreground "Yellow1"))))
 '(mode-line ((t (:background "grey10" :foreground "pink" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey2" :foreground "pink3" :box (:line-width -1 :color "grey10") :weight light))))
 '(region ((t (:background "color-235"))))
 '(rst-level-1 ((t (:background "grey8"))))
 '(rst-level-1-face ((t (:background "grey10"))) t)
 '(rst-level-2 ((t (:background "grey1"))))
 '(rst-level-2-face ((t nil)) t)
 '(rst-level-3 ((t (:background "grey10"))))
 '(rst-level-3-face ((t nil)) t)
 '(secondary-selection ((t (:background "color-101" :foreground "black"))))
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

;; python mode
(add-hook 'python-mode-hook
          #'(lambda ()
              (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
          ))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clj-mode))


(package-initialize)
(require 'key_chord_setup)

;; open archives
(auto-compression-mode t)
(set-terminal-coding-system 'utf-8-unix)
