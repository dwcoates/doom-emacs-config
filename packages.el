;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;(package! doom-themes)
;
(package! fixmee)
(package! chess :recipe (:host github :repo "dwcoates/emacs-chess" :upgrade t))
(package! highlight-indentation)
