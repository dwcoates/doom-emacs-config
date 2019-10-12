;;; .doom.d/config.el -*- lexical-binding: t; -*-

 ;; Use normal previous/next keys in insert-mode
(unmap! evil-insert-state-map "C-p" "C-n")

;; Place your private configuration here
(map!
 ;; Use some basic emacs navigation bindings in insert-mode
 :nvig
 "C-k" 'kill-line
 "C-e" 'end-of-line
 :map global-map :i
 "C-d" 'delete-char
 "C-p" 'previous-line
 "C-n" 'next-line


;; (after! avy)
(setq display-line-numbers-type 'relative)

(load-theme 'doom-gruvbox t)
