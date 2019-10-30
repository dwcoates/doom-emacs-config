;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'exec-path "$HOME/bin")
(add-to-list 'exec-path "$HOME/usr/lib/llvm-6.0/lib/clang/6.0.0/include")



(after! rtags
  (setq rtags-path "/home/dodge/src/rtags/bin"))

;; Relative line numbers are pretty cool. Makes a lot of VIM commands easier to use.
(setq! display-line-numbers-type 'relative)

;; When on the laptop, use some special settings.
(when (string= system-name "blackbox")
  (display-battery-mode t)
  (set-face-attribute 'default nil :height 100))

;;; Load theme
(load-theme 'doom-molokai t)

;;; Roland's package.
(use-package! fixmee)

;;; Chess
(use-package! chess
  :commands (+chess-ivy-show-fens +chess-ivy-show-fens-new-frame)
  :init
  (map! :leader
        (:prefix-map ("k" . "chess")
          :desc "Display position for FEN found on current line." "f" '+chess-ivy-show-fens
          :desc "Display position for FEN found on current line in new frame." "F" '+chess-ivy-show-fens-new-frame))
  ;; Main chess configuration file.
  (load! "+chess")
  :config
  ;; TODO: this decision should really be made on a screen-size basis.
  (set-popup-rule! "^\\*Chessboard.*" :side (if (string= system-name "blackbox") 'top 'left)))


;;; Org-mode
(after! org
  (+org-pretty-mode))

;;; Python
(after! python
  (set-pretty-symbols! 'python-mode
    :lambda "lambda")

  (defun dwc-python-mode-hook ()
    (highlight-indentation-mode))

  (add-hook 'python-mode-hook 'dwc-python-mode-hook))

;;; Javascript
(after! js2-mode
  (set-pretty-symbols! 'js2-mode
    :lambda "function"))

;;; C/C++
(after! cc-mode
  (set-pretty-symbols! 'c++-mode nil)

  ;; Rtags
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running))
