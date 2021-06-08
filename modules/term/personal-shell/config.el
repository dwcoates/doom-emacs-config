;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "") term-prompt-regexp "^.*@.*:.*\$")

(set-popup-rule! "*doom:vterm.**" :quit nil :select t :width 0.38 :side 'right :ttl nil)
