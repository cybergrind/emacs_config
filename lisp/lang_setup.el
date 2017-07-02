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

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (paredit-mode 1)))

(defun basis/init-paredit-mode ()
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode
                                               geiser-repl-mode
                                               cider-repl-mode)))
    (local-set-key (kbd "RET") #'paredit-newline)))

(use-package paredit
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :config
  (progn
    (basis/define-keys paredit-mode-map
      ("M-?"             nil) ; Make room for `xref-find-references'
      ("M-)"             #'basis/paredit-wrap-round-from-behind)
      ("M-e"             #'paredit-forward)
      ("M-a"             #'paredit-backward)
      ("M-k"             #'kill-sexp)
      ("C-w"             #'basis/paredit-kill-something)
      ("M-DEL"           #'basis/paredit-kill-something)
      ("C-k"             #'charged-kill))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'basis/paredit-doublequote-space-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'basis/paredit-splicing-unquote-p)
    ;; Show `eldoc' messages after Paredit motion commands
    (with-eval-after-load 'eldoc
      (eldoc-add-command 'paredit-forward
                         'paredit-forward-up
                         'paredit-forward-down
                         'paredit-backward
                         'paredit-backward-up
                         'paredit-backward-down
                         'paredit-newline))
    (add-hook 'paredit-mode-hook #'basis/init-paredit-mode)
    (pcase-dolist (`(,sym . ,act) '((paredit-kill            . supersede)
                                    (paredit-forward-delete  . supersede)
                                    (paredit-backward-delete . supersede)
                                    (paredit-newline         . t)))
      (put sym 'delete-selection act))))


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
  :config
  (progn
    (smartparens-global-strict-mode)
    (dolist (mode basis/sp-ignore-modes)
      (add-to-list 'sp-ignore-modes-list mode))
    (sp-use-paredit-bindings)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-autoescape-string-quote nil)
    (setq sp-use-subword t)
    (setq-default sp-autoskip-closing-pair 'always)
    (sp-pair "'" nil
             :unless '(basis/sp-point-after-word-p)
             :actions '(insert wrap autoskip))
    (pcase-dolist (`(,mode ,open ,close ,actions)
                   '((org-mode  "=" "=" (wrap))
                     (rust-mode "'" nil (:rem insert autoskip))
                     (c-mode    "{" "}" (:rem insert autoskip))
                     (c++-mode  "{" "}" (:rem insert autoskip))
                     (java-mode "{" "}" (:rem insert autoskip))))
      (sp-local-pair mode open close :actions actions))
    (message "init smartparens")
    (basis/define-keys sp-keymap
      ("M-DEL"           #'basis/sp-kill-something)
      ("C-DEL"           #'basis/sp-kill-something)
      ("<C-backspace>"   #'basis/sp-kill-something)
      ("C-w"             #'basis/sp-kill-something)
      ("M-k"             #'basis/sp-kill-sexp)
      ("M-e"             #'sp-forward-sexp)
      ("M-a"             #'sp-backward-sexp)
      ("C-M-u"           #'basis/sp-backward-up)
      ("C-k"             #'charged-kill))
    (advice-add 'sp--cleanup-after-kill :before-until
                #'basis/sp-cleanup-maybe-not)
    (advice-add 'sp--unwrap-sexp :filter-args #'basis/sp-unwrap-no-cleanup)
    (advice-add 'sp-backward-delete-char :filter-args
                #'basis/sp-backward-delete-no-prefix)))



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

(use-package idris-mode

  :init
  (add-hook 'idris-mode-hook
            #'(lambda () (setq charged-kill-nil 'kill-line)))
  :config
  (progn
    (smartparens-global-strict-mode nil)
    (message "Idris init")))

(provide 'lang_setup)
