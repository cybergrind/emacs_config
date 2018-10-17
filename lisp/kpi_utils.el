(defmacro rel-path (path)
  `(file-truename (concat (file-name-directory (or load-file-name buffer-file-name)) ,path)))

(provide 'kpi_utils)
