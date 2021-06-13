;;; app/chess/config.el -*- lexical-binding: t; -*-

(use-package! uci-mode
  :config
  (setq uci-mode-engine-command '("explanation-engine"))
  (set-popup-rule! "*UCI*" :height 0.4 :quit nil))

(use-package! pygn-mode
  :config
  (setq pygn-mode-script-directory (expand-file-name "~/workspace/pygn-mode"))
  (map! :map pygn-mode-map
        :nig "h" #'pygn-mode-previous-move
        :nig "H" #'pygn-mode-previous-move-follow-board
        :nig "l" #'pygn-mode-next-move
        :nig "L" #'pygn-mode-next-move-follow-board
        :nig "j" #'pygn-mode-next-game
        :nig "k" #'pygn-mode-previous-game
        :nig "RET" #'pygn-mode-echo-fen-at-point
        :nig "<mouse-8>" #'pygn-mode-mouse-display-gui-board)
  (set-popup-rule! "*pygn-mode-board*" :ignore nil :height 50))
