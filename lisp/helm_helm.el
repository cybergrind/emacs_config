;;;
;; https://github.com/abo-abo/helm-make/blob/master/helm-make.el

(require 'helm)

(defgroup helm-helm nil
  "kubernetes helm"
  :group 'helm-external)
(defvar helm-source-helm-repos nil)

(defcustom helm-source-helm-repos-fuzzy-match nil
  "fff"
  :group 'helm-helm
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (let ((helm-fuzzy-sort-fn 'helm-fuzzy-matching-sort-fn-preserve-ties-order))
           (setq helm-source-helm-repos
                 (helm-make-source "Helm Repos" 'helm-helm-repos-yaml-source
                   :fuzzy-match helm-source-helm-repos-fuzzy-match)))))

(defvar helm-versions-bname "*helm-proc*")

(defun build-helm-proc-buffer (outbuff repo)
  (with-current-buffer outbuff
    (erase-buffer))
  (call-process "helm" nil  outbuff nil "search" "repo" repo "--versions"))

(defun helm-source-helm-versions (outbuff repo)
  (message "start process %s" outbuff)
  (build-helm-proc-buffer outbuff repo)
  (helm-build-in-buffer-source "helm-versions"
    :data outbuff))

(defun helm-helm-versions (repo)
  (interactive)
  (let ((outbuff (get-buffer-create helm-versions-bname)))
    (helm :sources (helm-source-helm-versions outbuff repo)
          :buffer "*kube helm repos*")
    ))

(provide 'helm_helm)
