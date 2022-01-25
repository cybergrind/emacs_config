;;; python_setup.py --- python specific code -*- lexical-bindings: true -*-
;;; Commentary:
;;; Code:

(require 'flycheck)

(require 'python)

(require 'async)
(require 'kpi_utils)
(require 'emacs_setup)
(require 'f)

(use-package company-jedi)
(use-package virtualenvwrapper
  :ensure t)

(require 'virtualenvwrapper)

(defun sjoin (strings)
  "join strings with whitespace, skip nil"
  (string-join (cl-remove-if 'null strings) " "))


; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (if (not py/lsp)
                (progn (flycheck-select-checker 'python-flake8)
                       (flycheck-mode)))))

(defvar py/lsp t)
(defvar py-test-name "")
(defvar py-test-full-path nil)
(defvar py-chdir nil)
(defvar py-is-running-test nil)
(defvar py-disable-codestyle nil)

(defvar py-current-test nil)


(defun py-build-test-command ()
  (let* ((chdir (cond (py-chdir (sjoin (list "cd" py-chdir "&&")))))
         (cmd (sjoin (list chdir py-test-command py-test-params py-test-name))))
    cmd))

(defun run-py-test ()
  "Execute test and print result."
  (cond
   ((equal py-is-running-test t) (message "there is test in progress"))
   ((not py-current-test) (message "please select test first"))
   (t (let ((cmd py-current-test))
        (message "command: %s\n" cmd)
        (setq py-is-running-test t)
        (async-start
         `(lambda ()
            ,(async-inject-variables "cmd")
            (shell-command-to-string cmd))
         (lambda (result)
           (setq py-is-running-test nil)
           (message "ret is: %s" result)))))))


(defun get-path-pytest ()
  "In pytest format: path/to/file.py::function_name ."
  (let* ((curr_defun (python-info-current-defun))
         (curr_test (cond (curr_defun (replace-regexp-in-string "\\." "::" curr_defun))))
         ;; need to split when run in docker because full path will be different
         (curr_base (cond
                     (py-test-full-path (file-truename (buffer-file-name)))
                     (t (cadr (split-string (file-truename (buffer-file-name)) py-project-root)))))
         (test_path (cond
                     (curr_test
                      (concat curr_base "::" curr_test))
                     (t
                      curr_base))))
    (message "Test path: %s Base: %s TF: %s Defun: %s" test_path curr_base curr_test curr_defun)
    test_path))

(defun get-path-django-runner ()
  "In django format: path_to_module.with_dots.ClassName.test_name ."
  (let* ((curr_test (python-info-current-defun))
         (curr_file (replace-regexp-in-string ".py$" "" (buffer-file-name)))
         (curr_base (cadr (split-string curr_file py-project-root)))
         (with_dots (replace-regexp-in-string "/" "." curr_base))
         (test_path (concat with_dots "." curr_test)))
    test_path))

(defun get-py-test-path ()
  "Get test path, respect test runner syntax."
  (cond ((and (boundp 'py-test-runner)
              (equal py-test-runner 'pytest))
         (get-path-pytest))
        (t
         (get-path-django-runner))))

(defun assign-py-test ()
  "Assign test."
  (cond ((boundp 'py-project-root)
         (cond
          ((equal py-is-running-test t) (message "there is test in progress"))
          ((not (boundp 'py-project-root)) (message "there is no py-project-root"))
          ((not (boundp 'py-test-command)) (message "there is no py-test-command"))
          ((not (boundp 'py-test-params)) (message "there is no py-test-params"))
          (t (let ((test-path (get-py-test-path)))
               (setq py-test-name test-path)
               (setq py-current-test (py-build-test-command))))))
        (t (message "Please set py-project-root"))))


(defun py-test-interactive (arg)
  "Set and run python test.  ARG - if need to set test."
  (interactive "P")
  (let ((run_test t))
    (pcase (car arg)
      (4 (assign-py-test))
      (16 (x-copy-string (py-build-test-command))
          (setq run_test nil)))
    (if run_test (run-py-test))))


(defun ensure-trailing-slash (dir)
  (cond
   ((not dir) dir)
   ((s-suffix? "/" dir) dir)
   (t (concat dir "/"))))


(cl-defun py-test-setup-default (dir &key chdir (py-test-params ""))
  "Setup default values. Use it (py-test-setup-default (file-truename \".\")) ."
  (let* ((dirpath (ensure-trailing-slash dir))
         (chdirpath (cond
                     ((not chdir) dirpath)
                     (t (ensure-trailing-slash chdir)))))
    (defvar py-test-runner 'pytest)
    (defvar py-test-command (concat dirpath "venv/bin/py.test"))
    (defvar py-test-params py-test-params)
    (defvar py-project-root dirpath)
    (setq py-chdir chdirpath)))


(defun py/pprint-region (start end)
  (interactive "r")
  (shell-command-on-region start end
                           "python -c 'import sys; from pprint import pprint as pp; pp(eval(sys.stdin.read()))'"
                           '(4) '(4))
  (indent-for-tab-command))

(defun py/pprint ()
  (interactive)
  (set-mark-command nil)
  (forward-list)
  (py/pprint-region (region-beginning) (region-end)))


(defun py/verbose-toogle ()
  (interactive)
  (cond
   ((boundp 'py-test-params)
    (pcase py-test-params
      ("-vv" (setq py-test-params ""))
      ("" "-vv" (setq py-test-params "-vv"))))))


(defun py/get-ipython ()
  (file-truename (concat (projectile-project-root) "venv/bin/ipython")))

(cl-defun py/set-ipython (&optional (ipython-path "ipython"))
  (setq python-shell-interpreter ipython-path)
  (setq python-shell-interpreter-args "--simple-prompt -i"))

(defun py/setup-interpreter ()
  (cond
   ((and (projectile-project-p)
         (file-exists-p (py/get-ipython)))
    (py/set-ipython (py/get-ipython)))))

(defun py/send-defun (&optional arg msg)
  "Send the current defun to inferior Python process.
When argument ARG is non-nil do not include decorators.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list current-prefix-arg t))
  (cond
   ((region-active-p) (python-shell-send-region))
   (t (save-excursion
        (progn
          (python-mark-defun)
          (python-shell-send-region (region-beginning) (region-end))
          (deactivate-mark)
          )
        )))
  )

(defun py/run-python ()
  (interactive)
  (if (boundp 'py-project-root)
      (cd py-project-root))
  (run-python))

(add-hook 'python-mode-hook
          #'(lambda ()
              ;; (setq python-shell-interpreter "ipython")
              (global-set-key (kbd "C-o") 'py-test-interactive)
              (define-key python-mode-map (kbd "C-c .") 'goto-last-change)
              (define-key python-mode-map (kbd "C-c r") 'py/send-defun)
              (define-key python-mode-map (kbd "C-c C-p") 'py/run-python)
              (py/setup-interpreter)
              (company-mode-on)
              ;; (define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
              ;; (define-key python-mode-map (kbd "TAB") 'py-indent-line)
              ))


;;; this is pretty common case, should already be in some library



(defun not_pythonpath (s)
  (not (string-prefix-p "PYTHONPATH" s)))

(cl-defun pop-pythonpath (cmd &key (drop-pypath t))
  (print (format "Drop pypath: %s" drop-pypath))
  (cond
   (drop-pypath
    (let ((process-environment (seq-filter 'not_pythonpath process-environment)))
      (eval cmd)))
   (t (eval cmd))))


(cl-defun py/call-bin (command input-buffer output-buffer error-buffer &key (call-args '()) (drop-pypath t))
  "Call command on input-buffer

Send INPUT-BUFFER content to the process stdin.  Saving the
output to OUTPUT-BUFFER.  Saving process stderr to ERROR-BUFFER.
Return command process the exit code."
  (with-current-buffer input-buffer
    (let ((process (pop-pythonpath '(make-process :name "py/call-bin"
                                                  :command `(,command ,@call-args)
                                                  :buffer output-buffer
                                                  :stderr error-buffer
                                                  :noquery t
                                                  :sentinel (lambda (process event)))
                                   :drop-pypath drop-pypath)))

      (set-process-query-on-exit-flag (get-buffer-process error-buffer) nil)
      (set-process-sentinel (get-buffer-process error-buffer) (lambda (process event)))
      (save-restriction
        (widen)
        (process-send-region process (point-min) (point-max)))
      (process-send-eof process)
      (accept-process-output process nil nil t)
      (while (process-live-p process)
        (accept-process-output process nil nil t))
      (process-exit-status process))))

(cl-defun py/process-buffer (command &key (display 't) (call-args '("-")))
  "Show output, if COMMAND exit abnormally and DISPLAY is t."
  (interactive (list t))
  (let* ((original-buffer (current-buffer))
         (original-window-pos (window-start))
         (tmpbuf (get-buffer-create (format "*py/process/%s*" command)))
         (errbuf (get-buffer-create (format "*py/process-error/%s*" command))))
    ;; This buffer can be left after previous black invocation.  It
    ;; can contain error message of the previous run.
    (dolist (buf (list tmpbuf errbuf))
      (with-current-buffer buf
        (erase-buffer)))
    (condition-case err
        (if (not (zerop (py/call-bin command original-buffer tmpbuf errbuf :call-args call-args)))
            (error "Black failed, see %s buffer for details" (buffer-name errbuf))
          (unless (eq (compare-buffer-substrings tmpbuf nil nil original-buffer nil nil) 0)
            (with-current-buffer original-buffer
              (replace-buffer-contents tmpbuf)))
          (mapc 'kill-buffer (list tmpbuf errbuf)))
      (error (message "%s" (error-message-string err))
             (when display
               (pop-to-buffer errbuf))))))


(defun py/codestyle ()
  (interactive)
  (when (and (string-equal "python-mode" major-mode) (not py-disable-codestyle))
    (py/process-buffer "isort")
    (py/process-buffer "black")))


(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3)
  ;; aligns annotation to the right hand side
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  :init
  (global-company-mode 1))

(use-package python-environment
  :ensure t
  :chords ((" p" . py/codestyle)))


(defvar anaconda-ready-p nil)

(defun init-all-anaconda ()
  (use-package jedi-core
    :ensure t)
  (use-package pythonic
    :ensure t)
  (use-package
    anaconda-mode
    :ensure t)
  (use-package
    company-anaconda
    :after (anaconda-mode company)
    :commands company-anaconda
    :ensure t
    :init
    (add-to-list 'company-backends '(company-anaconda :with company-capf :with company-yasnippet))
    :bind
    (:map anaconda-mode-map
          ("M-TAB" . company-complete)
          ("M-/" . company-complete)))
  (setq anaconda-ready-p t))


;; :init (add-to-list 'company-backend '(company-caps :with company-yasnippet)))

(if py/lsp
    (progn
      (use-package lsp-pyright
        :ensure t
        :custom
        (lsp-enable-file-watchers nil)
        ;; :config
        ;; (lsp-dependency 'pyright
        ;;                 '(:system "pylance-language-server"))
        )
      (use-package lsp-ui
        :ensure t
        :bind
        (:map python-mode-map
              ("M-?" . lsp-ui-peek-find-references)
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-/" . company-complete)
              ("M-TAB" . company-complete)
              )
        :custom
        (lsp-ui-sideline-show-hover nil))))

(defun py/eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))


(defvar py-env-dir-name "venv")

(defun py/get_py_env (props py_project)
  (cond
   ((not py_project) nil)
   ((gethash 'emacs_py_env props) (f-join (projectile-project-root) (gethash 'emacs_py_env props "")))
   ((f-exists? (f-join py_project py-env-dir-name))(f-join py_project py-env-dir-name))
   (t (print (format "no matches %s" (f-join py_project py-env-dir-name))) nil)))


;; for very basic setup just create .editorconfig in root with content:
; [*.py]
; emacs_py_project = (projectile-project-root)


(defun ensure-lsp-checker ()
  (let*
      ((all-checkers (cons flycheck-checker (flycheck-get-next-checkers flycheck-checker)))
       (has-lsp (member 'lsp all-checkers))
       (has-flake8 (member 'python-flake8 all-checkers)))
    (cond
     ((and (not has-lsp) has-flake8) (flycheck-add-next-checker 'lsp 'python-flake8))
     ((and has-lsp (not has-flake8)) (flycheck-add-next-checker 'lsp 'python-flake8)))))

(add-hook 'flycheck-mode-hook
          (lambda ()
            (if py/lsp
                (ensure-lsp-checker))))

(defun py/runlsp (root venv)
  ;; (require 'lsp-pyright)
  (lsp--suggest-project-root)
  (lsp-workspace-root root)
  (lsp-ui-mode)
  (lsp))


;; [abv_api/**.py]
;; emacs_py_project = (f-join (projectile-project-root) "abv_api")
;; emacs_py_test_command = docker exec -i abvtest_web_1 pytest -n0
;; emacs_py_project_root = tipsi_web/abv_api/


;; [tipsi_web/**.py]
;; emacs_py_project = (f-join (projectile-project-root) "tipsi_web")
;; emacs_py_test_command = docker exec -i cluster_web_1 pytest -n0
;; emacs_py_project_root = tipsi_web/tipsi_web/
;; emacs_py_interactive = (python-shell-send-string (format "%%run %s/shutil_dev/emacs_interactive.py" (f-join (projectile-project-root) "tipsi_web")))
;; emacs_py_save_touch = (f-join (projectile-project-root) "tipsi_web" "uwsgi.ini")

;; [order_management/**.py]
;; emacs_py_project = (f-join (projectile-project-root) "order_management")
;; emacs_py_test_command = docker exec -i cluster_ordermanagement_1 pytest -n0
;; emacs_py_project_root = tipsi_web/order_management/
;; emacs_py_save_touch = (f-join (projectile-project-root) "order_management" "uwsgi.ini")

;; [integration/**.py]
;; emacs_py_project = (f-join (projectile-project-root) "integration")
;; emacs_py_test_command = docker exec -i cluster_integration_1 pytest -n0
;; emacs_py_project_root = integration/



(defun py/editorhook-wrapped (props)
  (let* ((emacs_py_project (f-join (projectile-project-root) (gethash 'emacs_py_project props "")))
         (emacs_py_env (py/get_py_env props emacs_py_project))
         (emacs_py_test_command (gethash 'emacs_py_test_command props))
         (emacs_py_test_full_path (gethash 'emacs_py_test_full_path props))
         (emacs_py_project_root (gethash 'emacs_py_project_root props))
         (emacs_py_extra_path (gethash 'emacs_py_extra_path props))
         (emacs_py_interactive (gethash 'emacs_py_interactive props))
         (emacs_py_save_touch (gethash 'emacs_py_save_touch props))
         (emacs_disable_lsp (gethash 'emacs_disable_lsp props)))
    (print (format "py_project before check: %s" emacs_py_project))
    (when emacs_py_project
      (if emacs_py_env
          (progn
            (print (format "setup in environment!!: %s %s" emacs_py_env (f-join emacs_py_env "./bin/ipython")))
            (setq-local python-shell-interpreter
                        (let ((ipython (f-join emacs_py_env "./bin/ipython"))
                              (regular-python (f-join emacs_py_env "./bin/python")))
                          (print (format "IPY: %s PY: %s" ipython regular-python))
                          (cond
                           ((f-exists? ipython) ipython)
                           (t regular-python))))
            (print (format "Shell interpr: %s" python-shell-interpreter))
            (if (and py/lsp (not emacs_disable_lsp))
                (py/runlsp emacs_py_project emacs_py_env)
              (progn
                (if (not anaconda-ready-p) (init-all-anaconda))
                (anaconda-mode)
                (anaconda-eldoc-mode)
                ))

            ))
      (cond
       ((and (not emacs_py_test_command) emacs_py_env)
        (print (format "setup regular => %s" emacs_py_project))
        (py-test-setup-default emacs_py_project))
       (emacs_py_test_command
        (setq-local py-test-params nil)
        (setq-local py-test-runner 'pytest)))

      (if emacs_py_test_command
          (setq-local py-test-command emacs_py_test_command)
        (if emacs_py_env
            (setq-local py-test-command (f-join emacs_py_env "./bin/py.test"))))

      (if emacs_py_test_full_path
          (setq-local py-test-full-path t))
      (if emacs_py_project_root
          (let ((abs_root (f-join (projectile-project-root) emacs_py_project_root "./")))
            (print (format "py-project-root => %s" abs_root))
            (setq-local py-project-root abs_root))
        (setq-local py-project-root (f-join emacs_py_project "./")))
      (if emacs_py_interactive
          (add-hook 'inferior-python-mode-hook
                    `(lambda ()
                       (interactive)
                       (py/eval-string ,emacs_py_interactive)))))
    (if emacs_py_save_touch
        (setq-local after_save_touch (f-join (projectile-project-root) emacs_py_save_touch)))))


(defun touch-on-save ()
  (interactive)
  (if (boundp 'after_save_touch)
      (progn (print (format "touch %s" after_save_touch))
             (start-process "touch" nil "touch" after_save_touch))))

(add-hook 'after-init-hook
          '(lambda () (add-hook 'after-save-hook 'touch-on-save)))


(defun py/editorhook (props)
  (when (and (eq major-mode 'python-mode)
             (projectile-project-root)
             ;; (getash 'emacs_py_project props)
             )
    (py/editorhook-wrapped props)))

;; (debug-on-entry 'py/editorhook-wrapped)
(add-hook 'editorconfig-after-apply-functions 'py/editorhook)
; (remove-hook 'editorconfig-after-apply-functions 'py/editorhook)


(provide 'python_setup)

;;; python_setup.el ends here
