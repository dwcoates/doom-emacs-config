;;; lang/javascript/config.el -*- lexical-binding: t; -*-

;;; Javascript
(after! js2-mode
  (set-pretty-symbols! 'js2-mode
    :lambda "function"))
