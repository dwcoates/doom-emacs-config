;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(map! :nvg
      "C-k" 'kill-line
      :nvig
      "C-e" 'end-of-line)

;; (after! avy)
(setq display-line-numbers-type 'relative)
