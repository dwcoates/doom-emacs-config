;;; config/vc/config.el -*- lexical-binding: t; -*-

(use-package! git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

(use-package! blamer
  :bind ("s-i" . blamer-show-commit-info)
  :defer 20
  :custom
  (blamer-idle-time 1)
  (blamer-min-offset 20)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode 1))
