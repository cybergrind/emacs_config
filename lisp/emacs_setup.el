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
  (magit-pull-arguments nil)
  (transient-append-suffix 'magit-push "-u"
    '(1 "-s" "Set CI variable" "--push-option=ci.variable=SKIP_DB_RESET=1")))

(setq enable-remote-dir-locals t)

;; C-t to save position. Go anywhere and then push M-, and return to last marked point
(global-set-key (kbd "C-t") (lambda ()
                              (interactive)
                              (xref-push-marker-stack)))

(use-package ag
  :custom
  (ag-arguments (list "--smart-case" "--stats" "--hidden")))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-x C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package which-key
  :config
  (which-key-mode))

(use-package helm
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini))
  :custom
  (helm-adaptive-mode t nil (helm-adaptive))
  (helm-mode-fuzzy-match t))

(use-package helm-ag :after (helm ag)
  :custom
  (helm-ag-insert-at-point (quote word))
  (helm-ag-use-agignore t))

(defun swoop-query ()
  (interactive)
  (print current-prefix-arg)
  (pcase
      current-prefix-arg
    ('(4)
     (let ((current-prefix-arg 1))
       (helm-swoop)))
    (t (helm-swoop :query ""))))

(use-package helm-swoop
  :bind
  (("C-s" . swoop-query)
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

(fset 'magit/remove-branch
      "bk\C-M\C-p")

(defun mirror-buffer-function ()
  (interactive)
  (let ((bname (buffer-name)))
    (open-buffer bname))
  (other-window 1 nil t))

(global-set-key (kbd "M-i") 'mirror-buffer-function)

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
    ("p" py/codestyle)
    ("l" lsp-avy-lens))
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
  :custom
  (key-chord-two-keys delay 0.02)
  (key-chord-in-macros nil)
  :config (key-chord-mode 1)
  (key-chord-define-global " v" 'x-paste)
  (key-chord-define-global " c" 'x-copy))


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

  (defun projectile-regenerate-tags ()
    (interactive)
    (let* ((project-root (projectile-acquire-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format "ctags -f \"%s\" -e $(echo $(ag -l -0)) %s"
                            (or (file-remote-p tags-file 'localname) tags-file)
                            tags-exclude))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (process-file-shell-command command nil (current-buffer))
              shell-output (string-trim
                            (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file)
      (message "Regenerated %s" tags-file)))

  (defun projectile-tags-exclude-patterns ()
    "Return a string with exclude patterns for ctags. Add support for wildcards passthrough"
    (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                    (directory-file-name (f-join (projectile-project-root) pattern))))
               (append (projectile-paths-to-ignore) projectile-globally-ignored-directories) " ")))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package editorconfig
  :diminish
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package company
  :diminish)


(use-package undo-tree
  :diminish
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))


(use-package yaml)

(use-package yaml-mode
  :bind
  (:map yaml-mode-map
        ("C-M-i" . copilot-complete)))


(setq emacs-backup-dir "~/.cache/emacs")
(if (not (file-directory-p emacs-backup-dir))
    (make-directory emacs-backup-dir))


(setq backup-directory-alist
      `((".*" . ,emacs-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-backup-dir t)))


(require 'init-ivy)
(provide 'emacs_setup)
(require 'helm_helm)

;; (require 'init-evil)
