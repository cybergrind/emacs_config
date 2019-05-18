;;; Code:

(use-package puppet-mode :mode "\\.pp")
(use-package prolog :mode ("\\.pl" . prolog-mode))
(use-package yaml-mode :mode "\\.yaml")
(use-package clojure-mode :mode ("\\.clj" . clj-mode)) ;; maybe add cider here later

(use-package flycheck-nim :after (nim-mode flycheck))
(use-package nim-mode
  :hook (nim-mode-hook . nimsuggest-mode)
  :mode "\\.nim")
(use-package go-mode :mode "\\.go")

(use-package flycheck
  :custom
  (flycheck-flake8rc "~/.config/flake8")
  (flycheck-disabled-checkers
   (quote
    (javascript-jshint json-python-json javascript-jshint javascript-gjslint javascript-jscs emacs-lisp-checkdoc)))
  (flycheck-eslintrc nil t)
  :config
  (global-flycheck-mode)
  (global-set-key (kbd "C-c C-n") 'flycheck-next-error))

(require 'js_setup)
(require 'python_setup)

(cond ((file-exists-p "/usr/lib/erlang")
       (setq erlang-root-dir "/usr/lib/erlang")
       (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
       (setq erlang-indent-level 4)
       (require 'erlang-start)))


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

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/lib/sbcl")


(defvar basis/sp-ignore-modes
  '(magit-key-mode
    lisp-mode
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
    slime-repl-mode
    org-mode)
  "List of modes in which not to active `smartparens'.")

(use-package smartparens
  :ensure t
  :config
  (turn-off-smartparens-strict-mode)
  (smartparens-global-mode)
  (global-set-key (kbd "M-d") 'sp-kill-sexp)
  (require 'smartparens-config)
  ; (dolist (mode basis/sp-ignore-modes)(add-to-list 'sp-ignore-modes-list mode))
  )




;(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
;(require 'slime)
;(slime-setup '(slime-repl))

(add-hook 'slime-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'not-working-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))

(use-package ensime
  :hook (scala-mode-hook . ensime-scala-mode-hook)
  :config
  (progn
    (add-hook 'ensime-mode-hook
              #'(lambda ()
                  (interactive)
                  (auto-complete-mode -1)))
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
        (package . (:foreground "#cc7832"))))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'company-backends 'company-yasnippet t))


(use-package yasnippet-snippets)

;; (defun add-yasnippet-ac-sources ()
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))
;; (add-hook 'python-mode-hook 'add-yasnippet-ac-sources)


(use-package geiser
  :config
  (setq geiser-active-implementations '(chicken)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(use-package idris-mode
  :hook (idris-mode-hook . (lambda () (setq charged-kill-nil 'kill-line)))
  :config
  (message "Idris init"))

(use-package lua-mode)

(use-package hcl-mode  :mode ("\\.tf" . hcl-mode))

(provide 'lang_setup)
