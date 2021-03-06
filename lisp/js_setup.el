;; javascript / html
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc.*$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint json-python-json javascript-jshint
      javascript-gjslint javascript-jscs)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; adjust indents for web-mode to 2 spaces
(defun custom-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq-default
   standard-indent 2
   tab-width 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-indent-style 2
   setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'custom-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/vendor/flow"
                                      root))))
    (when (and flow (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

;; Flycheck + Flowtype
(use-package flycheck-flow
  :after (flow)
  :custom
  (flycheck-javascript-flow-args nil)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))


(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'smartparens-mode)
(add-hook 'js2-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'smartparens-mode)
(add-hook 'web-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'smartparens-mode)
(add-hook 'json-mode-hook 'smartparens-mode)
(add-hook 'html-mode-hook 'subword-mode)

(setq js-indent-level 2)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))


(require 'prettier-js)

(setq prettier-width-mode 'fill)
(setq prettier-args '("--single-quote" "--trailing-comma=all" "--parser=flow"))

(defun my/use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-command prettier))))

(defun my/prettier-before-save ()
  (add-hook 'before-save-hook 'prettier-before-save))

;; (add-hook 'js-mode-hook (lambda ()
;;                           (my/use-prettier-from-node-modules)
;;                           (my/prettier-before-save)))
;; (add-hook 'js2-mode-hook 'my/prettier-before-save)
;; (add-hook 'web-mode-hook 'my/prettier-before-save)


(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

(provide 'js_setup)
