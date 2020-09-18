;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; Use normal previous/next keys in insert-mode
;(unmap! evil-insert-state-map "C-p" "C-n") ;; No keys stealing C-p and C-n, for now.
;(unmap! evil-visual-state-map "s") ;; No snipe in visual mode
;(unmap! evil-insert-state-map "C-f" "C-b") ;; Use emacs keys, for now
;(unmap! evil-normal-state-map "M-y")
;(unmap! evil-insert-state-map "C-w") ;; No more deleting work accidentally in insert mode.

(unbind-key "C-x C-p")

;;; Paren match should pop a bit more. No more squinting.
(set-face-attribute 'show-paren-match nil :background "grey25" :foreground "red3" :weight 'ultra-bold)

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
 "M-p" 'evil-paste-before
 "M-P" 'evil-paste-after
 ;;
 ;; Surround (paren manipulation)
 :n
 "M-]" 'evil-surround-delete
 "M-[" 'evil-surround-change
 "M-y" 'counsel-yank-pop
 :v
 "s"   'evil-surround-edit
 :n
 "SPC h M-m" 'man
 :i
 "C-h M-m" 'man
 ;; File finding. FIXME: This seems like a shitty way to do this.}}
 :leader
 "L" #'global-hide-mode-line-mode
 (:after avy
    :m "y" 'evil-avy-goto-char-in-line)
 (:after smartparens
   :leader
   :map smartparens-mode-map
   :prefix "r"
   :n "a"     #'sp-beginning-of-sexp
   :n "e"     #'sp-end-of-sexp

   :n "d"     #'sp-down-sexp
   :n "bd"    #'sp-backward-down-sexp
   :n "]"     #'sp-up-sexp
   :n "["     #'sp-backward-up-sexp

   :n "p"     #'sp-backward-sexp

   :n "n"     #'sp-next-sexp

   :n "m"     #'sp-forward-symbol
   :n "bm"    #'sp-backward-symbol

   :n ">"     #'sp-forward-slurp-sexp
   :n "f"     #'sp-forward-barf-sexp
   :n "<"    #'sp-backward-slurp-sexp
   :n "bf"    #'sp-backward-barf-sexp

   :n "t"     #'sp-transpose-sexp
   :n "k"     #'sp-kill-sexp
   :n "hk"    #'sp-kill-hybrid-sexp
   :n "bk"    #'sp-backward-kill-sexp
   :n "c"     #'sp-copy-sexp

   :n "u"     #'sp-unwrap-sexp
   :n "bu"    #'sp-backward-unwrap-sexp

   :n "w"     #'sp-wrap-round
   :n "y"     #'sp-wrap-curly
   :n "r"     #'sp-wrap-square))
