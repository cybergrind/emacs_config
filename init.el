;;; init.el --- emacs config
;;; Commentary:
;;; make elisp linter happy
;;; Code:
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor")

(defconst basis/emacs-dir
  (file-name-directory (file-chase-links (or load-file-name buffer-file-name)))
  "This Emacs's configuration directory.")

(defun basis/emacs-dir (name)
  "Return directory NAME expanded in `basis/emacs-dir'.
Create the directory if it does not exist and CREATE is non-nil."
  (if (string-suffix-p "/" name)
      (expand-file-name name basis/emacs-dir)
    ;; This isn't actually necessary
    (error "Directory name should end with a slash")))

(defun basis/emacs-file (name)
  "Return file NAME expanded in `basis/emacs-dir'."
  (if (not (string-suffix-p "/" name))
      (expand-file-name name basis/emacs-dir)
    (error "File name should not end with a slash")))


(setq package-selected-packages
      '(
        docker
        docker-tramp
        dockerfile-mode
        erlang
        fuzzy
        js2-mode
        json-mode
        lua-mode
        markdown-mode
        prettier-js
        rainbow-delimiters
        restclient
        rust-mode
        scala-mode2
        slim-mode
        slime
        tagedit
        tramp
        web-mode
        use-package
        zenburn-theme
        ))

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; workaround TLS but with melpa in some emacs versions
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package)))


(require 'use-package)
(setq use-package-always-ensure t)


(require 'cl)

(require 'kpi_utils)
(require 'emacs_setup)
(require 'lang_setup)
(require 'vis_cust)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load-theme 'cybergrind)
