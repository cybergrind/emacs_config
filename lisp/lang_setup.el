;;; Code:

(autoload 'puppet-mode "puppet-mode" nil t)
(autoload 'yaml-mode "yaml-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.nim$" . nim-mode))

(require 'js_setup)
(require 'python_setup)
(require 'flycheck_setup)


(cond ((file-exists-p "/usr/lib/erlang")
       (setq erlang-root-dir "/usr/lib/erlang")
       (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
       (setq erlang-indent-level 4)
       (require 'erlang-start)))


(require 'auto-complete)
(add-to-list 'ac-modes 'python-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'web-mode)
(setq ac-fuzzy-enable t)

; (seq nim-nimsuggest-path "/usr/bin/nimsuggest")
(add-hook 'nim-mode-hook 'nimsuggest-mode)

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "/usr/lib/sbcl")
;(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
;(require 'slime)
                                        ;(slime-setup '(slime-repl))

(add-hook 'slime-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'not-working-mode-hook
           #'(lambda () (setq autopair-dont-activate t)))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clj-mode))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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
        (package . (:foreground "#cc7832"))))


(require 'yasnippet)
(yas-global-mode 1)

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-hook 'python-mode-hook 'add-yasnippet-ac-sources)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(require 'geiser)
(setq geiser-active-implementations '(chicken))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(provide 'lang_setup)
