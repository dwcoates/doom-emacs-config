;;; lang/personal-clojure/config.el -*- lexical-binding: t; -*-


(after! cider
  (set-popup-rule! "\\*cider-error\\*" :side 'right :width 300)
  (defun +personal-cider-repl-hook ()
    (lispy-mode)
    (lispyville-mode))
  (add-hook! :local cider-repl-mode-hook +personal-cider-repl-hook))
