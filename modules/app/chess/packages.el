;;; app/chess/package.el -*- lexical-binding: t; -*-

(package! uci-mode :recipe (:type git :host github :repo "dwcoates/uci-mode"))

;; If system defines a PYGNPATH, then we use that code for pygn, forgoing bytecompiling for
;; development (no need to run `doom sync'). Otherwise, we install from official source.
(if (and (getenv "PYGNPATH") (file-directory-p (getenv "PYGNPATH")))
    (package! pygn-mode :recipe (:local-repo "~/workspace/pygn-mode" :build (:not compile)))
 (package! pygn-mode :recipe (:type git :host github :repo "dwcoates/pygn-mode")))
