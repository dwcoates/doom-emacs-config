;;; editor/lispy/config.el -*- lexical-binding: t; -*-

(after! lispy
  (add-hook 'evil-escape-inhibit-functions (lambda () lispy-mode)))
