;;; Code:
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/lisp")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")
(setq packages-list '(dockercontrol-mode))
(el-get 'sync packages-list)

(setq package-selected-packages
      '(clojure-mode
        erlang
        flycheck
        magit paredit anything smex cider
        python slim-mode slime
        tramp
        auto-complete
        scala-mode2
        puppet-mode fuzzy prolog-el
        js2-mode yaml-mode
        bookmark+ bookmark+-lit bookmark+-1 bookmark+-mac
        rust-mode nim-mode
        dockerfile-mode
        go-mode
        flx flx-ido
        ensime
        coffee-mode
        web-mode
        multiple-cursors
        ido-ubiquitous
        hydra
        avy
        vimish-fold
        helm
        helm-ag
        goto-chg
        zenburn-theme))
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
        (package-install package)))


(require 'cl)
(defvar *emacs-load-start* (current-time))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'flycheck_setup)

;(require 'tramp)
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

(global-set-key (kbd "C-x ;") 'eval-expression)

(autoload 'icicle-mode "icicles" nil t)
(global-set-key [(meta s) (i)] (lambda ()
                 (interactive)
                 (icicle-mode)))
;;(run-at-time "0.8 sec" nil 'icicles)

(cond ((file-exists-p "/usr/lib/erlang")
       (setq erlang-root-dir "/usr/lib/erlang")
       (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
       (setq erlang-indent-level 4)
       (require 'erlang-start)))


;(autopair-global-mode t)
;;(auto-complete-mode 1)
(require 'auto-complete)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'web-mode)
(setq ac-fuzzy-enable t)
(show-paren-mode 1)

;(ido-mode t)
(ido-mode 'both)
(setq ido-save-directory-list-file "~/.emacs.d/var/ido")
(setq ido-enable-flex-matching t)

; flx customization
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

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


;; (autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(autoload 'puppet-mode "puppet-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nim$" . nimrod-mode))
;;(add-to-list 'auto-mode-alist '("" . auto-complete-mode))

(require 'multi_desktop)

;(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;                           (- (+ lo ms) (+ (second *emacs-load-start*) (third *emacs-load-start*)))))

(message "%s" (current-time))
(put 'set-goal-column 'disabled nil)

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/lib/sbcl")
;(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
;(require 'slime)
;(slime-setup '(slime-repl))

(add-hook 'slime-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'not-working-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))

;; python mode
(add-hook 'python-mode-hook
          #'(lambda ()
              ;(define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
              ;(define-key python-mode-map (kbd "TAB") 'py-indent-line)
          ))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clj-mode))


(package-initialize)

;; open archives
(auto-compression-mode t)
(set-terminal-coding-system 'utf-8-unix)

(require 'yapf)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq ensime-sem-high-faces
      ;; NOTE: Inconsolata doesn't have italics
      ;; FURTHER NOTE: these are overlays, not faces
      '((var . (:foreground "color-208"))
        (val . (:foreground "grey"))
        (varField . (:foreground "color-208"))
        (valField . (:foreground "grey"))
        (functionCall . (:foreground "grey"))
        (operator . (:foreground "grey"))
        (param . (:foreground "grey"))
        (class . (:foreground "color-47"))
        (trait . (:foreground "#4e807d"))
        (object . (:foreground "#6897bb"))
        (package . (:foreground "#cc7832"))))


(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(require 'web_snippets)


(require 'avy)
(define-key global-map (kbd "M-SPC") 'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-c c") 'avy-goto-char)
(define-key global-map (kbd "C-c l") 'avy-goto-line)

(require 'goto-chg)
(define-key global-map (kbd "C-c .") 'goto-last-change)
(define-key global-map (kbd "C-c ,") 'goto-last-change-reverse)

(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c C-f") 'vimish-fold-avy)
(global-set-key (kbd "C-c C-u") 'vimish-fold-toggle)

;; defuns
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-regular-p file)
             (not (string-suffix-p "~" file)))
    (load file)))

; macro
(fset 'fix-indent
      "\C-i\C-n\-m\C-x(")

; hydra

(require 'hydra)

(defhydra cqql-multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))


(require 'yasnippet)
(yas-global-mode 1)


(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; bookmark jump
(defun ido-bookmark-jump (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump bname))

(defun ido-bookmark-jump-other-windows (bname)
  "*Switch to bookmark interactively using `ido'."
  (interactive (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
  (bookmark-jump-other-window bname))

;;; keyboard shortcuts

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(require 'multiple-cursors)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-A") 'mc/mark-all-like-this)
(global-set-key (kbd "M-E") 'mc/mark-more-like-this-extended)
(define-key mc/keymap (kbd "M-'") 'mc-hide-unmatched-lines-mode)


(require 'bookmark+)
(global-set-key (kbd "M-p") 'bmkp-previous-bookmark-this-file/buffer)
(global-set-key (kbd "M-n") 'bmkp-next-bookmark-this-file/buffer)
(global-set-key (kbd "M-t") 'bmkp-toggle-autonamed-bookmark-set/delete)
(global-set-key (kbd "C-t") 'bookmark-set)
(global-set-key (kbd "M-j") 'ido-bookmark-jump)
(global-set-key (kbd "M-J") 'ido-bookmark-jump-other-windows)

;;; init.el ends here

;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))

(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" default)))
 '(flycheck-eslintrc "~/.eslintrc")
 '(flymake-log-level -1)
 '(flymake-no-changes-timeout 5)
 '(helm-ag-use-agignore t)
 '(helm-mode-fuzzy-match t)
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(magit-pull-arguments nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/gtd/tipsi.org" "~/Dropbox/gtd/gtd.org" "~/Dropbox/gtd/logbook/14_09.org" "~/Dropbox/gtd/calendar.org_archive" "~/Dropbox/gtd/calendar.org")))
 '(org-directory "~/Dropbox/gtd")
 '(org-mobile-directory "~/Dropbox/gtd")
 '(org-mobile-inbox-for-pull "~/Dropbox/gtd/mobileorg.org")
 '(safe-local-variable-values
   (quote
    ((content-type . "jsx")
     (web-mode-content-type . "jsx")
     (web-mode-content-type . jsx)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)
     (python-indent-offset . 4)
     (erlang-mode . 1)
     (erlang-mode\;erlang-indent-level . 4)
     (erlang\;erlang-indent-level . 4)
     (encoding . utf-8))))
 '(tab-width 4))
