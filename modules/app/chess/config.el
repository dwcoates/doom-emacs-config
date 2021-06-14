;;; app/chess/config.el -*- lexical-binding: t; -*-

(use-package! uci-mode
  :config
  (setq uci-mode-engine-command '("explanation-engine"))
  (set-popup-rule! "*UCI*" :side 'right :width 0.4 :quit nil :select t :ttl nil)
  (map! (:map global-map
         :prefix "C-c 8"
         :ivomrg "e" #'uci-mode-run-engine
         :ivomrg "r" #'uci-mode-restart-engine
         :ivomrg "q" #'uci-mode-quit)
        (:leader
         :n "8e" #'uci-mode-run-engine
         :n "8r" #'uci-mode-restart-engine
         :n "8q" #'uci-mode-quit)))

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
  (set-popup-rule! "*pygn-mode-board*" :ignore nil :height 50)
  (unless (pygn-mode-do-diagnostic)
    (warn "pygn-mode diagnostic failed.")))

;; (use-package! cee-pygn-mode
;;   :if (getenv "CEEPYGNPATH")
;;   :load-path (lambda () (getenv "CEEPYGNPATH"))
;;   :config
;;   (map! (:map global
;;          :prefix "C-c 8"
;;          :ivomrg "?" #'cee-pygn-mode-open-integration-test-pgn)
;;         (:leader
;;          :n "8?" #'cee-pygn-mode-open-integration-test-pgn)))
