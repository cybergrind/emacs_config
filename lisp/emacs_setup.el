;;; Code:

;; defuns
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))

(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (and (file-regular-p file)
             (not (string-suffix-p "~" file)))
    (load file)))

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

(use-package ag
  :custom
  (ag-arguments (list "--smart-case" "--stats" "--hidden")))

(use-package helm
  :custom
  (helm-adaptive-mode t nil (helm-adaptive))
  (helm-mode-fuzzy-match t))

(use-package helm-ag :after (helm ag)
  :custom
  (helm-ag-insert-at-point (quote word))
  (helm-ag-use-agignore t))

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop)
   :map helm-swoop-map
   ("M-k" . backward-kill-sentence)
   ("C-s" . helm-swoop-next-line)
   ("C-r" . helm-swoop-previous-line)))

(use-package helm-projectile :after (helm projectile))

(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :custom
  (recentf-max-menu-items 60)
  (recentf-max-saved-items 60)
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package ido-completing-read+ :after (ido-mode))

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

;; remove me
(fset 'mirror-buffer-old
      "\C-xo\C-xb\C-r\C-r\C-m")

(fset 'mirror-buffer
      "\C-xo\C-xb\C-p\C-m")

(fset 'magit/remove-branch
      "bk\C-M\C-p")

(global-set-key (kbd "M-i") 'mirror-buffer)

(defun switch_to_results ()
  (interactive)
  (let*
      ((prefix (if (not (string= (buffer-name) "*Messages*"))
                   "\C-xo\C-xbmessag\C-m"
                 ""))
       (full-cmd (s-concat prefix "\C-x]\C-rtest session starts\C-m")))
    ;; (message "Full-CMD: %s" full-cmd)
    (setq unread-command-events
          (nconc
           (listify-key-sequence full-cmd)
           unread-command-events))))

(global-set-key (kbd "M-c") 'switch_to_results)


;; hydra
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

(use-package auto-yasnippet
  :after hydra
  :config
  (defhydra actions (global-map "C-c a")
    "aya"
    ("c" aya-create)
    ("e" aya-expand)
    ("a" lsp-execute-code-action)
    ("p" py/codestyle))
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

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "M-N") 'mc/mark-next-like-this)
    (global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
    (global-set-key (kbd "M-A") 'mc/mark-all-like-this)
    (global-set-key (kbd "M-E") 'mc/mark-more-like-this-extended)
    (define-key mc/keymap (kbd "M-'") 'mc-hide-unmatched-lines-mode)))

(use-package expand-region
  :bind ("C-\\" . er/expand-region))


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


;; (add-to-list 'load-path "/home/kpi/devel/github/projectile")
(use-package projectile
  ;; :ensure nil
  ;; :pin manual
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
                                    (directory-file-name (f-join (projectile-project-root) pattern))))
               (append (projectile-paths-to-ignore) projectile-globally-ignored-directories) " ")
  )
)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package editorconfig
  :diminish
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package undo-tree
  :config
  (global-undo-tree-mode))


(require 'init-ivy)
(provide 'emacs_setup)
