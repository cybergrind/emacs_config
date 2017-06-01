;;; vis_cust.el -- customization
;;; Commentary:
;;; Code:

(require 'cl)
(defvar *emacs-load-start* (current-time))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(eval-after-load 'python-mode

  (font-lock-add-keywords
   'python-mode
   '(("\\(lambda\\)" (0 (progn ()
                          (compose-region (match-beginning 1)
                                          (match-end 1)
                                          ?λ)))))))

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)

(defun pretty-lambdas ()
  "Draw pretty lambdas."
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))



(autoload 'autopair-global-mode "autopair" nil t)
(autoload 'paredit "paredit" nil t)


(provide 'vis_cust)
;;; vis_cust.el ends here
