(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/lib/sbcl")

;(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
;(require 'slime)
;(slime-setup '(slime-repl))

(add-hook 'slime-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'not-working-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))


(use-package puppet-mode :mode "\\.pp")
(use-package prolog :mode ("\\.pl" . prolog-mode))


(use-package clojure-mode :mode ("\\.clj" . clj-mode)) ;; maybe add cider here later

(use-package flycheck-nim :after (nim-mode flycheck))
(use-package nim-mode
  :hook (nim-mode-hook . nimsuggest-mode)
  :mode "\\.nim")

(use-package go-mode
  :mode "\\.go"
  :bind
  (:map go-mode-map
        ("M-p" . gofmt)))


(use-package lua-mode)

(defun terraform-fmt ()
  (interactive)
  (call-process "terraform" nil "*terraform*" "terraform" "fmt" buffer-file-name)
  (revert-buffer :ignore-auto :noconfirm))

(use-package hcl-mode
  :mode ("\\.tf" . hcl-mode)
  :bind
  (:map hcl-mode-map
        ("M-p" . terraform-fmt)))

(use-package geiser
  :config
  (setq geiser-active-implementations '(chicken)))


(use-package idris-mode
  :hook (idris-mode-hook . (lambda () (setq charged-kill-nil 'kill-line)))
  :config
  (message "Idris init"))

;; c++
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode))

(use-package company-irony
  :requires irony)

(use-package flycheck-irony
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package julia-mode
  :mode ("\\.jl" . julia-mode))

(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))

(use-package vterm
  :ensure t
  :bind
  (:map vterm-mode-map
        ("C-v" . scroll-up-command)
        ("M-v" . scroll-down-command)))

(defun my/julia-repl-send-cell()
  ;; "Send the current julia cell (delimited by ###) to the julia shell"
  (interactive)
  (save-excursion (setq cell-begin (if (re-search-backward "^###" nil t) (point) (point-min))))
  (save-excursion (setq cell-end (if (re-search-forward "^###" nil t) (point) (point-max))))
  (set-mark cell-begin)
  (goto-char cell-end)
  (julia-repl-send-region-or-line)
  (next-line))

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'vterm)

  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))


;; (cond ((file-exists-p "/usr/lib/erlang")
;;        (setq erlang-root-dir "/usr/lib/erlang")
;;        (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;        (setq erlang-indent-level 4)
;;        (require 'erlang-start)))

(provide 'extra_langs)
