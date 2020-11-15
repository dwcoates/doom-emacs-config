;;; term/shell/config.el -*- lexical-binding: t; -*-

(setq comint-get-old-input (lambda () "")
      term-prompt-regexp "^.*@.*:.*\$")
