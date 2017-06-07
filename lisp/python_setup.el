(require 'flycheck) ; noqa

; Python checker
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-select-checker 'python-flake8)
            (flycheck-mode)))



(defvar py-test-name "")

(defun run-py-test ()
  (cond ((and (boundp 'py-project-root)
              (boundp 'py-test-command)
              (string> py-test-name ""))
         (message "ret is: %s"
                  (shell-command-to-string
                   (concat
                    py-test-command
                    py-test-name))))
        (t (message "Please set py-project-root or chose test"))))

(defun assign-py-test ()
  (cond ((boundp 'py-project-root)
         (let* ((curr_test (python-info-current-defun))
                (curr_file (replace-regexp-in-string ".py$" "" (buffer-file-name)))
                (curr_base (cadr (split-string curr_file py-project-root)))
                (with_dots (replace-regexp-in-string "/" "." curr_base))
                (test_path (concat with_dots "." curr_test)))
           (message "Test path %s" test_path)
           (setq py-test-name test_path)
           (run-py-test)))
        (t (message "Please set py-project-root"))))


(defun py-test-interactive (arg)
  (interactive "P")
  (if arg (assign-py-test))
  (run-py-test))


(add-hook 'python-mode-hook
          #'(lambda ()
              (setq python-shell-interpreter "ipython")
              ;; (setq python-shell-interpreter-args "--pylab")
              (define-key python-mode-map (kbd "C-o") 'py-test-interactive)
              (define-key python-mode-map (kbd "C-c .") 'goto-last-change)
              ;(define-key python-mode-map (kbd "DEL") 'py-electric-backspace)
              ;(define-key python-mode-map (kbd "TAB") 'py-indent-line)
          ))


(provide 'python_setup)
