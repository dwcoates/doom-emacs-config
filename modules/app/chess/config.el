;;; app/chess/config.el -*- lexical-binding: t; -*-

(use-package! uci-mode
  :config
  (let ((exe (getenv "EMACS_CHESS_ENGINE_BIN")))
    (if (executable-find exe)
        (setq uci-mode-engine-command `(,exe))
      (warn "explanation-engine executable not found. Not available as a `uci-mode-engine-command' executable")))
  (set-popup-rule! "*UCI*" :side 'right :width 0.4 :quit nil :select t :ttl nil)
  (map! (:map global-map
         :prefix "C-c 8"
         :ivomrg "e" #'uci-mode-run-engine
         :ivomrg "r" #'uci-mode-restart-engine
         :ivomrg "q" #'uci-mode-quit
         )
        (:map uci-mode-map
         :n "q" #'uci-mode-quit
         )
        (:leader
         :n "8e" #'uci-mode-run-engine
         :n "8r" #'uci-mode-restart-engine
         :n "8q" #'uci-mode-quit)))

(use-package! pygn-mode
  :init
  (setq pygn-mode-python-executable "python3")
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
  (setq pygn-mode-board-size 400)
  (set-popup-rule! "\\*pygn-mode-board\\*" :height 29 :side 'bottom)
  (unless (pygn-mode-run-diagnostic)
    (warn "pygn-mode diagnostic failed.")))

(use-package! cee-pygn-mode
  :if (and (getenv "CEEPYGNPATH") (file-directory-p (getenv "CEEPYGNPATH")))
  :load-path (lambda () (getenv "CEEPYGNPATH"))
  :config
  (map! (:map global
         :prefix "C-c 8"
         :ivomrg "?" #'cee-pygn-mode-open-integration-test-pgn
         :ivomrg "o" #'cee-pygn-mode-open-integration-test-engine-output
         :ivomrg "O" #'cee-pygn-mode-fetch-integration-test-engine-output
         :ivomrg "i" #'cee-pygn-mode-fetch-integration-test-engine-input
         :ivomrg "I" #'cee-pygn-mode-open-integration-test-engine-input)
        (:leader
         :n "8?" #'cee-pygn-mode-open-integration-test-pgn
         :n "8o" #'cee-pygn-mode-open-integration-test-engine-output
         :n "8O" #'cee-pygn-mode-fetch-integration-test-engine-output
         :n "8i" #'cee-pygn-mode-fetch-integration-test-engine-input
         :n "8I" #'cee-pygn-mode-open-integration-test-engine-input)))
