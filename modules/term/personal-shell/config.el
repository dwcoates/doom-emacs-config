;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "") term-prompt-regexp "^.*@.*:.*\$")

(set-popup-rule! "*doom:vterm.**" :quit nil :select t :height 0.5 :side 'bottom :ttl nil)

(after! vterm
  :config
  (setq vterm-max-scrollback 100000))
