;;; python_setup.py --- python specific code -*- lexical-bindings: true -*-
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'python)
(require 'async)

; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (flycheck-mode)))

(defvar py-test-name "")
(defvar py-chdir nil)
(defvar py-is-running-test nil)


(defun py-build-test-command ()
  (let* ((chdir (cond (py-chdir (concat "cd " py-chdir " && "))))
         (cmd (concat chdir py-test-command " " py-test-params " " py-test-name)))
    cmd))

(defun run-py-test ()
  "Execute test and print result."
  (cond
   ((equal py-is-running-test t) (message "there is test in progress"))
   ((not (boundp 'py-project-root)) (message "there is no py-project-root"))
   ((not (boundp 'py-test-command)) (message "there is no py-test-command"))
   ((not (boundp 'py-test-params)) (message "there is no py-test-params"))
   ((not (string> py-test-name "")) (message "please select test first"))
   (t (let ((cmd (py-build-test-command)))
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
         (curr_base (cadr (split-string (buffer-file-name) py-project-root)))
         (test_path (cond
                     (curr_test
                      (concat curr_base "::" curr_test))
                     (t
                      (buffer-file-name)))))
    (message "Test path: %s" test_path)
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
         (let ((test-path (get-py-test-path)))
           ; (message "!!TEST PATH: %s" test-path)
           (setq py-test-name test-path)
           (run-py-test)))
        (t (message "Please set py-project-root"))))


(defun py-test-interactive (arg)
  "Set and run python test.  ARG - if need to set test."
  (interactive "P")
  (pcase arg
    ('(4) (assign-py-test)))
  (run-py-test))

(cl-defun py-test-setup-default (dir &key chdir (py-test-params ""))
  "Setup default values. Use it (py-test-setup-default (file-truename \".\")) ."
  (defvar py-test-runner 'pytest)
  (defvar py-test-command (concat dir "/venv/bin/py.test -n0"))
  (defvar py-test-params py-test-params)
  (defvar py-project-root (concat dir "/"))
  (setq py-chdir chdir))


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


(add-hook 'python-mode-hook
          #'(lambda ()
              ; (setq python-shell-interpreter "ipython")
              (define-key python-mode-map (kbd "C-o") 'py-test-interactive)
              (define-key python-mode-map (kbd "C-c .") 'goto-last-change)
              (cond ((string= python-shell-interpreter "python")
                      (setq python-shell-interpreter "ipython")))

              (setq python-shell-interpreter-args "--simple-prompt -i")
              ;(define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
              ;(define-key python-mode-map (kbd "TAB") 'py-indent-line)
              ))

(provide 'python_setup)

;;; python_setup.el ends here
