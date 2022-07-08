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


;; (cond ((file-exists-p "/usr/lib/erlang")
;;        (setq erlang-root-dir "/usr/lib/erlang")
;;        (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
;;        (setq erlang-indent-level 4)
;;        (require 'erlang-start)))

(provide 'extra_langs)
