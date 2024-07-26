;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(after! treemacs
  (setq treemacs-width 60  ; Increase this value to make it wider
        treemacs-position 'left
        treemacs-collapse-dirs 3
        treemacs-deferred-git-apply-delay 0.5
        treemacs-display-in-side-window t)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (map! :leader
        (:prefix ("t" . "treemacs")
         :desc "Toggle treemacs" "t" #'treemacs
         :desc "Select treemacs window" "1" #'treemacs-select-window)))
