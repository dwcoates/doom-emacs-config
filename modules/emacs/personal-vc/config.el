;;; emacs/vc/config.el -*- lexical-binding: t; -*-

(after! log-view
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'magit-log-mode 'normal)
  (evil-set-initial-state 'magit-revision-mode 'normal)
  (evil-set-initial-state 'magit-diff-mode 'normal))
