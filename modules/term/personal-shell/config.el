;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "")
      term-prompt-regexp "^.*@.*:.*\$")

(set-popup-rule! "*doom:vterm.**" :quit nil :select t :width 120 :side 'right)
