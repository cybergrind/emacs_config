(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook nil)
 '(bmkp-last-as-first-bookmark-file "~/devel/tipsi/tipsi_web/bookmarks")
 '(coffee-tab-width 4)
 '(color-identifiers-coloring-method 'hash)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   '("5a68850c66edcf32eb859345c331c9b20a50ddfcb577b70e0a69ad5d939d10d9" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" default))
 '(dired-dwim-target t)
 '(flycheck-disabled-checkers
   '(javascript-jshint json-python-json javascript-jshint javascript-gjslint javascript-jscs emacs-lisp-checkdoc))
 '(flycheck-eslintrc nil t)
 '(flycheck-flake8rc "~/.config/flake8")
 '(flymake-log-level -1)
 '(flymake-no-changes-timeout 5)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-ag-insert-at-point 'word)
 '(helm-ag-use-agignore t)
 '(helm-mode-fuzzy-match t t)
 '(js-indent-level 2 t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-sideline-delay 0.2)
 '(lsp-ui-sideline-show-hover nil t)
 '(magit-pull-arguments nil t)
 '(org-agenda-files
   '("~/Dropbox/gtd/tipsi.org" "~/Dropbox/gtd/gtd.org" "~/Dropbox/gtd/logbook/14_09.org" "~/Dropbox/gtd/calendar.org_archive" "~/Dropbox/gtd/calendar.org"))
 '(org-directory "~/Dropbox/gtd")
 '(org-mobile-directory "~/Dropbox/gtd")
 '(org-mobile-inbox-for-pull "~/Dropbox/gtd/mobileorg.org")
 '(package-selected-packages
   '(jedi-core lsp-ui lsp-python-ms elm-mode rvm robe smartparens-config ag anything auto-complete avy cider clojure-mode coffee-mode docker docker-tramp dockerfile-mode erlang flx flx-ido flycheck flycheck-nim flycheck-flow fuzzy go-mode goto-chg geiser helm helm-ag helm-projectile hydra ido-completing-read+ js2-mode json-mode lua-mode magit markdown-mode prettier-js projectile rainbow-delimiters restclient rust-mode scala-mode2 slim-mode slime tagedit tramp vimish-fold web-mode use-package zenburn-theme))
 '(projectile-completion-system 'ido)
 '(projectile-enable-caching t)
 '(projectile-generic-command "ag -g \"\" -0")
 '(projectile-mode t nil (projectile))
 '(projectile-tags-exclude-supports-globs t t)
 '(ps-bottom-margin 5)
 '(ps-footer-offset 5)
 '(ps-header-font-size '(8 . 8))
 '(ps-header-lines 1)
 '(ps-header-offset 0)
 '(ps-header-title-font-size '(8 . 8))
 '(ps-inter-column 25)
 '(ps-left-margin 25)
 '(ps-paper-type 'a4)
 '(ps-print-header-frame nil)
 '(ps-print-only-one-header t)
 '(ps-right-margin 25)
 '(ps-spool-duplex nil)
 '(ps-top-margin 5)
 '(safe-local-variable-values
   '((logstash-indent . 2)
     (python-shell-extra-pythonpaths ".")
     (python-shell-interpreter . "~/extra/devel/tipsi/osx_ci/venv/bin/python")
     (python-shell-extra-pythonpaths "/home/kpi/devel/tipsi/tipsi_web/integration/venv/bin/python")
     (python-shell-interpreter . "/home/kpi/devel/tipsi/tipsi_web/integration/venv/bin/python")
     (python-shell-extra-pythonpaths "/home/kpi/devel/tipsi/tipsi_web/tipsi_web/")
     (python-shell-interpreter . "/home/kpi/devel/tipsi/tipsi_web/tipsi_web/venv/bin/ipython")
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (lexical-bindings . true)
     (vimish-fold-dir . "/ssd/kpi/tipsi/tipsi_web/.emacs.d/vimish-fold")
     (bookmark-default-file . "/ssd/kpi/tipsi/tipsi_web/bookmarks")
     (some-variable . "test")
     (content-type . "jsx")
     (web-mode-content-type . "jsx")
     (web-mode-content-type . jsx)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)
     (python-indent-offset . 4)
     (erlang-mode . 1)
     (erlang-mode\;erlang-indent-level . 4)
     (erlang\;erlang-indent-level . 4)
     (encoding . utf-8)))
 '(tab-width 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-global ((t (:background "color-17"))))
 '(lsp-ui-sideline-symbol-info ((t (:background "color-16" :height 0.99)))))
