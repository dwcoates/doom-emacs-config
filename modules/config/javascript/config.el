;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(after! typescript-mode
  (let ((eslint-executable (or (executable-find "eslint_d")
                               (executable-find "eslint"))))
    ;;25b2065a1024876
    (if eslint-executable
        (progn
          (set-formatter! 'typescript
            `(,eslint-executable "--fix" "--format=json" "--stdin" "--stdin-filename")
            :modes '(typescript-mode typescript-ts-mode typescript-tsx-mode))
          (message "Using typescript formatter exectuable %s" eslint-executable))
      (warn! "No eslint executable detected. Formatting wont work as expected.")
      )))
