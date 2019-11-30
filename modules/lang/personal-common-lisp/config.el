;;; lang/personal-common-lisp/config.el -*- lexical-binding: t; -*-

(add-hook 'common-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'sly-mrepl-mode-hook (lambda () (lispy-mode 1)))
