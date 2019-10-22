;;; .doom.d/config.el -*- lexical-binding: t; -*-

 ;; Use normal previous/next keys in insert-mode
(unmap! evil-insert-state-map "C-p" "C-n") ;; No keys stealing C-p and C-n, for now.
(unmap! evil-visual-state-map "s") ;; No snipe in visual mode
(unmap! evil-insert-state-map "C-f" "C-b") ;; Use emacs keys, for now
(unmap! evil-normal-state-map "M-y")

(unbind-key "C-x C-p")

;; Place your private configuration here
(map!
 ;; Use some basic emacs navigation bindings in insert-mode
 :nvigr
 "C-k" 'kill-line
 "C-e" 'end-of-line
 :map global-map
 :i
 "C-d" 'delete-char
 "C-p" 'previous-line
 "C-n" 'next-line
 "C-f" 'forward-char
 "C-b" 'backward-char
 ;; Surround (paren manipulation)
 :n
 "M-]" 'evil-surround-delete
 "M-[" 'evil-surround-change
 "M-y" 'counsel-yank-pop
 :v
 "s"   'evil-surround-edit
 ;; File finding. FIXME: This seems like a shitty way to do this.
 :prefix "C-x M-f"
 ;; work
 "c" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac"))
 "s" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac/chess_engines/stockfish-tep/src/"))
 "t" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac/TEP/libs/"))
 )

;; Relative line numbers are pretty cool. Makes a lot of VIM commands easier to use.
(setq! display-line-numbers-type 'relative)

;; When on the laptop, use some special settings.
(when (string= system-name "blackbox")
  (display-battery-mode t)
  (set-face-attribute 'default nil :height 100))

;;; Load theme
(load-theme 'doom-molokai t)
