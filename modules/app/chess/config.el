;;; app/chess/config.el -*- lexical-binding: t; -*-

(use-package! pygn-mode
  :config
  (map! :map pygn-mode-map
        :g "h" #'pygn-mode-previous-move
        :g "H" #'pygn-mode-previous-move-follow-gui-board
        :g "l" #'pygn-mode-next-move
        :g "L" #'pygn-mode-next-move-follow-gui-board
        :g "j" #'pygn-mode-next-game
        :g "k" #'pygn-mode-previous-game
        :g "RET" #'pygn-mode-echo-fen-at-point
        :g "<mouse-8>" #'pygn-mode-mouse-display-gui-board)
  (set-popup-rule! "*pygn-mode-board*" :ignore t))

