;;; Code:

;; defuns
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-regular-p file)
             (not (string-suffix-p "~" file)))
    (load file)))


(autoload 'smex-initialize "smex" "smex-initialize" t)

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
(global-set-key (kbd "C-x g") 'magit-status)

;; C-t to save position. Go anywhere and then push M-, and return to last marked point
(global-set-key (kbd "C-t") (lambda ()
                              (interactive)
                              (xref-push-marker-stack)))


(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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
  (cl-flet ((process-list ())) ad-do-it))

(require 'multi_desktop)

(message "%s" (current-time))
(put 'set-goal-column 'disabled nil)


;; open archives
(auto-compression-mode t)
(set-terminal-coding-system 'utf-8-unix)

(require 'avy)
(define-key global-map (kbd "M-SPC") 'avy-goto-word-or-subword-1)
(define-key global-map (kbd "C-c c") 'avy-goto-char)
(define-key global-map (kbd "C-c l") 'avy-goto-line)

(require 'goto-chg)
(define-key global-map (kbd "C-c .") 'goto-last-change)
(define-key global-map (kbd "C-c ,") 'goto-last-change-reverse)

(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c f") 'vimish-fold-avy)
(global-set-key (kbd "C-c u") 'vimish-fold-toggle)

; macro
(fset 'fix-indent
      "\C-i\C-n\-m\C-x(")

(fset 'mirror-buffer
      "\C-xo\C-xb\C-r\C-r\C-m")

(fset 'magit/remove-branch
      "bk\C-M\C-p")

(global-set-key (kbd "M-i") 'mirror-buffer)

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


(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


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

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "M-N") 'mc/mark-next-like-this)
    (global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
    (global-set-key (kbd "M-A") 'mc/mark-all-like-this)
    (global-set-key (kbd "M-E") 'mc/mark-more-like-this-extended)
    (define-key mc/keymap (kbd "M-'") 'mc-hide-unmatched-lines-mode)))


(use-package diminish)

(use-package color-identifiers-mode
  :diminish
  :config
  (global-color-identifiers-mode 1)
  :custom
  (color-identifiers-coloring-method 'hash))


;; outdated
;; (use-package bookmark+
;;   ; :ensure bookmark+-lit :ensure bookmark+-1 :ensure bookmark+-mac
;;   :config
;;   (progn
;;     (global-set-key (kbd "M-p") 'bmkp-previous-bookmark-this-file/buffer)
;;     (global-set-key (kbd "M-n") 'bmkp-next-bookmark-this-file/buffer)
;;     (global-set-key (kbd "M-t") 'bmkp-toggle-autonamed-bookmark-set/delete)
;;     (global-set-key (kbd "C-t") 'bookmark-set)
;;     (global-set-key (kbd "M-j") 'ido-bookmark-jump)
;;     (global-set-key (kbd "M-J") 'ido-bookmark-jump-other-windows)))



(require 'x-clipboard)

(setq org-log-done 'time)
(setq org-clock-idle-time 10)
(setq org-agenda-include-diary t)
(setq org-log-into-drawer t)

(projectile-global-mode)

(defmacro rel-path (path)
  `(file-truename (concat (file-name-directory (or load-file-name buffer-file-name)) ,path)))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(provide 'emacs_setup)
