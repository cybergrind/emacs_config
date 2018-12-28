(defmacro rel-path (path)
  `(file-truename (concat (file-name-directory (or load-file-name buffer-file-name)) ,path)))


(defun x-copy-string (str)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
      (process-send-string proc str)
      (process-send-eof proc))))

(provide 'kpi_utils)
