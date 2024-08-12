;;; config/vc/config.el -*- lexical-binding: t; -*-

(use-package! git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; (after! personal-theme ;;FIXME: this doesn't work, because personal-theme is not being properly provided as a package
;;   (use-package! blamer
;;     :bind ("s-i" . blamer-show-commit-info)
;;     :defer 5
;;     :custom
;;     (blamer-idle-time 0.3)
;;     (blamer-min-offset 20)
;;     :custom-face
;;     (blamer-face ((t :foreground ,(face-attribute 'font-lock-comment-face :foreground)
;;                      :background nil
;;                      :italic t)))
;;     :config
;;     (global-blamer-mode 1))
;;   )
