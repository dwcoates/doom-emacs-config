;;; lang/cc/config.el -*- lexical-binding: t; -*-

(add-to-list 'exec-path "$HOME/usr/lib/llvm-6.0/lib/clang/6.0.0/include")

(after! rtags
  (setq rtags-path "/home/dodge/src/rtags/bin")

  (rtags-enable-standard-keybindings)

  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running))

;;; C/C++
(after! cc-mode
  (set-pretty-symbols! 'c++-mode nil)
  (set-popup-rule! "*compilation*" :side 'right :width 80 :quit #'doom/popup-ctrl-g-close :select t))
