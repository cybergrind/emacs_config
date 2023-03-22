;;; Code:

(use-package yaml-mode :mode "\\.yaml")

(use-package flycheck
  :custom
  (flycheck-flake8rc ".flake8")
  (flycheck-disabled-checkers
   (quote
    (javascript-jshint json-python-json javascript-jshint javascript-gjslint javascript-jscs emacs-lisp-checkdoc)))
  (flycheck-eslintrc nil t)
  :config
  (global-flycheck-mode)
  (global-set-key (kbd "C-c C-n") 'flycheck-next-error))

(require 'js_setup)
(require 'python_setup)

(use-package auto-complete
  :disabled
  :diminish
  :config
  ;; (add-to-list 'ac-modes 'python-mode)
  (add-to-list 'ac-modes 'emacs-lisp-mode)
  (add-to-list 'ac-modes 'web-mode)
  (setq ac-fuzzy-enable t)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))


(defvar basis/sp-ignore-modes
  '(lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode
    cider-repl-mode
    clojure-mode
    clojure-mode-mode
    inferior-scheme-mode
    geiser-repl-mode
    inferior-lisp-mode
    scheme-mode
    slime-repl-mode)
  "List of modes in which not to active `smartparens'.")

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :bind
  (:map lispy-mode-map
        ("M-m" . back-to-indentation)
        ("M-i" . mirror-buffer-function)
        ("C-y" . yank)
        ("M-d" . sp-kill-sexp)))

(use-package smartparens
  :ensure t
  :custom
  (sp-python-insert-colon-in-function-definitions nil)
  :config
  (turn-off-smartparens-strict-mode)
  (smartparens-global-mode)
  (global-set-key (kbd "M-d") 'sp-kill-sexp)
  (require 'smartparens-config)
  (dolist (mode basis/sp-ignore-modes)(add-to-list 'sp-ignore-modes-list mode))
  )


(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'company-backends 'company-yasnippet t))


(use-package yasnippet-snippets)

(if (getenv "EMACS_EXTRA_LANGS")
    (require 'extra_langs))

(defun run-lsp ()
  (lsp))

(defun dart-test-interactive (arg)
  (interactive "P")
  (pcase (car arg)
    (4 (lsp-dart-run-test-at-point))
    (0 (lsp-dart-run-last-test))))

(use-package dart-mode
  :mode "\\.dart"
  :custom
  (lsp-dart-line-length 100)
  (lsp-dart-main-code-lens nil)
  (lsp-dart-test-code-lens nil)
  :chords ((" p" . lsp-format-buffer))
  :bind
  (:map dart-mode-map
        ("C-o" . dart-test-interactive)
        ("M-o" . lsp-dart-dap-flutter-hot-reload)
        ("M-p" . lsp-format-buffer)
        ("M-l a" . lsp-execute-code-action))
  :hook (dart-mode . run-lsp))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :diminish
  :ensure t
  :custom
  (copilot-idle-delay 2.0)
  (global-copilot-mode t)
  :bind
  ("C-M-i" . 'copilot-complete)
  (:map copilot-mode-map
        ("M-TAB" . 'copilot-complete)
        :map copilot-completion-map
        ("C-M-]" . 'copilot-previous-completion)
        ("M-]" . 'copilot-next-completion)
        ("M-/" . 'copilot-accept-completion-by-word)
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        :map lsp-mode-map
        ("M-TAB" . 'copilot-complete)
        :map copilot-completion-map
        ("C-M-]" . 'copilot-previous-completion)
        ("M-]" . 'copilot-next-completion)
        ("M-/" . 'copilot-accept-completion-by-word)
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode))

(use-package rustic
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (lsp-disalbed-clients '(rls))
  :bind
  (:map rust-mode-map
        ("M-TAB" . 'copilot-complete)
        ("M-p" . 'rustic-cargo-fmt)))

(provide 'lang_setup)
