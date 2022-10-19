;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "") term-prompt-regexp "^.*@.*:.*\$")

(set-popup-rule! "*doom:vterm.**" :quit nil :select t :width 0.42 :side 'right :ttl nil)

(after! vterm
  :config
  (setq vterm-max-scrollback 100000))
