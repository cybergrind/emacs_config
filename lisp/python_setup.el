;;; python_setup.py --- python specific code
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'python)

; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (flycheck-mode)))

(defvar py-test-name "")

(defun run-py-test ()
  "Execute test and print result."
  (cond
   ((and (boundp 'py-project-root)
         (boundp 'py-test-command)
         (string> py-test-name ""))
    (message "ret is: %s"
             (shell-command-to-string
              (concat
               py-test-command
               py-test-name))))
        (t (message "Please set py-project-root or chose test"))))


(defun get-path-pytest ()
  "In pytest format: path/to/file.py::function_name ."
  (let* ((curr_test (python-info-current-defun))
         (test_path (concat (buffer-file-name) "::" curr_test)))
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
  (if arg (assign-py-test))
  (run-py-test))


(add-hook 'python-mode-hook
          #'(lambda ()
              (setq python-shell-interpreter "ipython")
              (define-key python-mode-map (kbd "C-o") 'py-test-interactive)
              (define-key python-mode-map (kbd "C-c .") 'goto-last-change)
              ;(define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
              ;(define-key python-mode-map (kbd "TAB") 'py-indent-line)
          ))

(provide 'python_setup)

;;; python_setup.el ends here
