(let* ((current-file-path (file-truename (or load-file-name buffer-file-name)))
       (current-dir (file-name-directory current-file-path))
       (theme-customization  "doom-tomorrow-night")) ;; change this to theme customization corresponding to elisp file in this directory
  (load-file (concat current-dir theme-customization ".el"))
  (message (format "Loaded personal theme customization for '%s'" theme-customization)))

(provide 'personal-theme) ;;FIXME: this doesn't work. The package isn't picked up by doom or loaded
