;;; lang/cc/config.el -*- lexical-binding: t; -*-

(add-to-list 'exec-path "$HOME/usr/lib/llvm-6.0/lib/clang/6.0.0/include")

;; (after! rtags
;;   (setq rtags-path "/home/dodge/bin")
;;   (rtags-enable-standard-keybindings)
;;   (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;   (add-hook 'c++-mode-hook 'rtags-start-process-unless-running))

;;; C/C++
(after! cc-mode
  (set-pretty-symbols! '(c-mode c++-mode) nil)
  (set-popup-rule! "*compilation*" :side 'bottom :height 20 :quit #'doom/popup-ctrl-g-close :select t)
  (set-popup-rule! "*input/output of .*" :quit :quit #'doom/popup-ctrl-g-close)
  (setq realgud-safe-mode nil))



(set-popup-rule! "\\*gdb.*shell\\*" :side 'right :width 100 :quit nil)

(set-popup-rule! "\\*compilation.*\\*" :side 'right :width 100 :quit nil)

(after! projectile
  ;; Yikes
  (put 'projectile-project-configure-cmd 'safe-local-variable (lambda (cmd) t))
  (put 'projectile-project-compilation-cmd 'safe-local-variable (lambda (cmd) t))
  (put 'projectile-project-test-cmd 'safe-local-variable (lambda (cmd) t)))

(when (featurep! +lsp)
  (add-hook! '(c-mode-local-vars-hook
               c++-mode-local-vars-hook
               objc-mode-local-vars-hook)
             #'lsp-ui-peek-mode #'lsp-ui-doc-mode)
  (setq lsp-clients-clangd-args '("-j=5"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--pch-storage=memory"))
  (after! lsp-clangd (set-lsp-priority! 'clangd 2))
  (setq company-idle-delay 0.0))

(map!
 :map cpp-mode-map
 :nvigr
 "C-c ?" 'realgud:gdb)
