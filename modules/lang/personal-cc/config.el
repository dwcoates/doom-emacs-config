;;; lang/cc/config.el -*- lexical-binding: t; -*-

;;; C/C++
(after! cc-mode
  (set-pretty-symbols! '(c-mode c++-mode) nil)
  (set-popup-rule! "*input/output of .*" :quit :quit #'doom/popup-ctrl-g-close)
  (setq realgud-safe-mode nil))

(set-popup-rule! "\\*gdb.*shell\\*" :side 'right :width 0.4 :quit nil)
(set-popup-rule! "\\*compilation.*\\*" :side 'bottom :height 0.45 :width 0.45 :quit nil)
(set-popup-rule! "\\*assembly*\\*" :side 'right :width 0.35 0.45 :quit nil)

(after! projectile
  ;; Yikes
  (put 'projectile-project-configure-cmd 'safe-local-variable (lambda (cmd) t))
  (put 'projectile-project-compilation-cmd 'safe-local-variable (lambda (cmd) t))
  (put 'projectile-project-test-cmd 'safe-local-variable (lambda (cmd) t)))

(when (featurep! +lsp)
  (defun dwc-c-modes-hook ()
    (electric-indent-mode -1)
    (electric-indent-local-mode -1)
    (setq counsel-grep-swiper-limit 2200))
  (add-hook! '(c-mode-local-vars-hook
               c++-mode-local-vars-hook
               objc-mode-local-vars-hook)
             '(dwc-c-modes-hook))
  (setq lsp-clients-clangd-args '("-j=5"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--pch-storage=memory")
        lsp-ui-doc-show-with-cursor nil)
  (after! lsp-clangd (set-lsp-priority! 'clangd 2))
  (after! dap-mode
    (require 'dap-cpptools)
    (setq dap-auto-configure-features '(locals breakpoints sessions)))
  (setq company-idle-delay 0.0))

(map!
 (:map projectile-mode-map
  :leader
  :nvr
  "pT" #'+projectile-test-project
  "p<" #'+switch-to-compilation-buffer)
 (:map cpp-mode-map
  :nvigr
  "C-c ?" 'realgud:gdb)
 (:map cpp-mode-map
  :leader
  :prefix "c"
  :n
  "??" #'dap-debug-last
  "?/" #'dap-debug
  "?b" #'dap-breakpoint-add
  "?c" #'dap-breakpoint-condition
  "?l" #'dap-breakpoint-log-message
  "?t" #'dap-breakpoint-toggle
  "?d" #'dap-breakpoint-delete
  "?1" #'dap-breakpoint-delete-all)
(:after lsp-mode
 :map lsp-mode-map
 :leader
 :nvr
 "cf" #'+lookup-current-c-function-references) )
