;;;
;; https://github.com/abo-abo/helm-make/blob/master/helm-make.el

(require 'helm)
(require 'yaml)

(defgroup helm-helm nil
  "kubernetes helm"
  :group 'helm-external)


(defvar helm-versions-bname "*helm-proc*")

(defun build-helm-proc-buffer (outbuff repo)
  (message "Find versions in repo: %s" repo)
  (call-process "helm" nil  outbuff nil "search" "repo" repo "--versions"))

(defun helm-source-helm-versions (outbuff repo)
  (message "start process %s" outbuff)
  (build-helm-proc-buffer outbuff repo)
  (helm-build-in-buffer-source "helm-versions"
    :data outbuff))

(defun helm-helm-versions (repo)
  (interactive)
  (let ((outbuff (generate-new-buffer helm-versions-bname)))
    (helm :sources (helm-source-helm-versions outbuff repo)
          :buffer "*kube helm repo versions*")))


(defun get-helm-repos ()
  (let*
      ((data (yaml-parse-string (f-read-text "~/.config/helm/repositories.yaml")))
       (repos (gethash 'repositories data))
       (items (seq-map
               '(lambda (item)
                  (let* ((url (gethash 'url item))
                         (name (gethash 'name item))
                         (fmt (format "%s => %s" name url)))
                    `(,fmt . ,name)))
               repos)))
    items))


(defun helm-source-helm-repos ()
  (let ((items (get-helm-repos)))
    (helm-build-sync-source "helm-repos-source"
      :action (lambda (item) (helm-helm-versions item))
      :candidates items)))

(defun helm-helm-repos ()
  "Show versions from helm repositories"
  (interactive)
  (helm :sources (helm-source-helm-repos)
        :buffer "*kube helm repos*"))


(provide 'helm_helm)
