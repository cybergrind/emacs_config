;;; Code:

;; defuns
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-regular-p file)
             (not (string-suffix-p "~" file)))
    (load file)))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(global-set-key (kbd "C-x ;") 'eval-expression)

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :custom
  (magit-pull-arguments nil))

(setq enable-remote-dir-locals t)

;; C-t to save position. Go anywhere and then push M-, and return to last marked point
(global-set-key (kbd "C-t") (lambda ()
                              (interactive)
                              (xref-push-marker-stack)))

(use-package ag)

(use-package helm
  :custom
  (helm-adaptive-mode t nil (helm-adaptive))
  (helm-mode-fuzzy-match t))

(use-package helm-ag :after (helm ag)
  :custom
  (helm-ag-insert-at-point (quote word))
  (helm-ag-use-agignore t))

(use-package helm-projectile :after (helm projectile))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))

(use-package ido-completing-read+ :after (ido-mode))

(ido-mode 'both)
(setq ido-save-directory-list-file "~/.emacs.d/var/ido")
(setq ido-enable-flex-matching t)

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))


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

(use-package avy
  :bind
  (("M-SPC" . avy-goto-word-or-subword-1)
   ("C-c c" . avy-goto-char)
   ("C-c l" . avy-goto-line))
  )

(use-package goto-chg
  :bind
  (("C-c ." . goto-last-change)
   ("C-c ," . goto-last-change-reverse)))

(use-package vimish-fold
  :bind
  (("C-c f" . vimish-fold-avy)
   ("C-c u" . vimish-fold-toggle))
  :config
  (vimish-fold-global-mode 1))

; macro
(fset 'fix-indent
      "\C-i\C-n\-m\C-x(")

(fset 'mirror-buffer
      "\C-xo\C-xb\C-r\C-r\C-m")

(fset 'magit/remove-branch
      "bk\C-M\C-p")

(global-set-key (kbd "M-i") 'mirror-buffer)

; hydra

(use-package hydra
  :config
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))
  )

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

(require 'x-clipboard)

(setq org-log-done 'time)
(setq org-clock-idle-time 10)
(setq org-agenda-include-diary t)
(setq org-log-into-drawer t)

(use-package projectile
  :bind (("C-c p p" . projectile-switch-project))
  :custom
  (projectile-completion-system (quote ido))
  (projectile-enable-caching t)
  (projectile-generic-command "ag -g \"\" -0")
  (projectile-mode t nil (projectile))
  (projectile-tags-exclude-supports-globs t)
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (defun projectile-tags-exclude-patterns ()
    "Return a string with exclude patterns for ctags. Add support for wildcards passthrough"
    (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                    (directory-file-name pattern)))
               (append (projectile-paths-to-ignore) projectile-globally-ignored-directories) " ")
  )
)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(provide 'emacs_setup)
