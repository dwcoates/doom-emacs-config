;;; app/chess/package.el -*- lexical-binding: t; -*-

(package! tree-sitter)
(package! tree-sitter-langs)

(defmacro register-dwc-package (package-name package-local-env-name &optional only-local)
  "Define with `package!' a dwcoates repository.

Prefer a local repository version denoted with system environment
variable `PACKAGE-LOCAL-ENV-NAME' if available.

If `ONLY-LOCAL' is non-nil, do not look for package on github."
  (let ((package-local-path (getenv package-local-env-name)))
    (if package-local-path
        (progn
          (unless (file-directory-p package-local-path)
            (error "PYGNPATH environment variable points to a non-existent directory: '%s'"
                   package-local-path))
          `(package! ,package-name :recipe (:local-repo ,package-local-path :build (:not compile))))
      (unless ,only-local
        `(package! ,package-name
           :recipe (:type git :host github :repo ,(concat "dwcoates/" (symbol-name package-name))))))))

(register-dwc-package pygn-mode "PYGNPATH")
(register-dwc-package uci-mode "UCIMODEPATH")
;; (register-dwc-package cee-pygn-mode "CEEPYGNPATH" t) ;; private code
