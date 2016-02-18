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
        hydra))
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

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)


;(require 'anything-config)
; (require 'flymake_cust)
(require 'flycheck_setup)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/ssd/tipsi/tipsi_web/bookmarks")
 '(coffee-tab-width 4)
 '(edts-man-root "~/.emacs.d/edts/doc/R16B02")
 '(flycheck-eslintrc "~/.eslintrc")
 '(flymake-log-level -1)
 '(flymake-no-changes-timeout 5)
 '(icicle-buffers-ido-like-flag t)
 '(icicle-files-ido-like-flag t)
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(nil nil t)
 '(nrepl-host "192.168.42.129")
 '(nrepl-port 9999)
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
     (encoding . utf-8)
     (test-case-name . twisted\.web\.test\.test_xmlrpc)
     (test-case-name . test\.test_txpostgres)
     (test-case-name . twisted\.test\.test_logfile)
     (test-case-name . twisted\.test\.test_log)
     (test-case-name . twisted\.test\.test_internet)
     (test-case-name . twisted\.test\.test_reflect)
     (codiing . utf-8))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-completion-face ((t (:underline t))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "white" :weight bold :height 1.2))))
 '(custom-state ((t (:foreground "color-43"))))
 '(custom-variable-tag ((t (:foreground "white" :weight bold))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-changed ((t (:underline t))))
 '(diff-context ((t nil)))
 '(diff-file-header ((t (:background "grey10" :weight bold))))
 '(diff-header ((t (:background "grey10" :foreground "white"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(ensime-implicit-highlight ((t (:inherit nil :background "grey15"))))
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
 '(magit-branch-local ((t (:foreground "#41A3C4"))))
 '(magit-diff-add ((t (:inherit diff-added :foreground "darkgreen"))))
 '(magit-diff-added ((t (:background "grey15" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "grey10" :foreground "#22aa22"))))
 '(magit-diff-context ((t (:foreground "grey80"))))
 '(magit-diff-context-highlight ((t (:background "grey10" :foreground "grey85"))))
 '(magit-diff-hunk-heading ((t (:background "grey10" :foreground "grey80"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "grey30" :foreground "grey75"))))
 '(magit-diff-removed ((t (:background "grey10" :foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:background "grey15" :foreground "#aa2222"))))
 '(magit-section-highlight ((t (:background "grey15"))))
 '(match ((t (:background "pink" :foreground "darkblue"))))
 '(minibuffer-prompt ((t (:foreground "Yellow1"))))
 '(moccur-face ((t (:background "grey15" :weight bold))))
 '(mode-line ((t (:background "grey10" :foreground "pink" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey2" :foreground "pink3" :box (:line-width -1 :color "grey10") :weight light))))
 '(org-agenda-done ((t (:foreground "color-34"))))
 '(org-agenda-structure ((t (:foreground "Blue"))))
 '(org-scheduled ((t (:foreground "color-67"))))
 '(org-scheduled-today ((t (:foreground "color-51"))))
 '(region ((t (:background "color-235"))))
 '(rst-level-1 ((t (:background "grey8"))))
 '(rst-level-1-face ((t (:background "grey10"))) t)
 '(rst-level-2 ((t (:background "grey1"))))
 '(rst-level-2-face ((t nil)) t)
 '(rst-level-3 ((t (:background "grey10"))))
 '(rst-level-3-face ((t nil)) t)
 '(secondary-selection ((t (:background "color-101" :foreground "black"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightmagenta"))))
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


(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c C-f") 'vimish-fold-avy)
(global-set-key (kbd "C-c C-u") 'vimish-fold-toggle)

;; defuns
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
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
