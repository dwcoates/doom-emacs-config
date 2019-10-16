;;; .doom.d/config.el -*- lexical-binding: t; -*-

 ;; Use normal previous/next keys in insert-mode
(unmap! evil-insert-state-map "C-p" "C-n") ;; No keys stealing C-p and C-n, for now.
(unmap! evil-visual-state-map "s") ;; No snipe in visual mode

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
 ;; Surround (paren manipulation)
 :n
 "M-]" 'evil-surround-delete
 "M-[" 'evil-surround-change
 :v
 "s"   'evil-surround-edit

 ;; File finding. FIXME: This seems like a shitty way to do this.
 :prefix "C-x M-f"
 ;; work
 "c" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac"))
 "s" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac/chess_engines/stockfish-tep/src/"))
 "t" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac/TEP/libs/"))
 )

(setq display-line-numbers-type 'relative)

;;; Load theme
(load-theme 'doom-molokai t)
