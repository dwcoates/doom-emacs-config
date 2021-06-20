;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "") term-prompt-regexp "^.*@.*:.*\$")

(set-popup-rule! "*doom:vterm.**" :quit nil :select t :height 0.50 :side 'bottom :ttl nil)
