;;; .doom.d/config.el -*- lexical-binding: t; -*-

 ;; Use normal previous/next keys in insert-mode
(unmap! evil-insert-state-map "C-p" "C-n") ;; No keys stealing C-p and C-n, for now.
(unmap! evil-visual-state-map "s") ;; No snipe in visual mode
(unmap! evil-insert-state-map "C-f" "C-b") ;; Use emacs keys, for now
(unmap! evil-normal-state-map "M-y")
(unmap! evil-insert-state-map "C-w") ;; No more deleting work accidentally in insert mode.

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
 "t" '(lambda () (interactive) (find-file "~/workspace/ChessCom/ceac/TEP/libs/")))

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

;;; Chess stuff.
(use-package! chess
  :commands (+chess-show-positions-new-frame +chess-show-fen-at-point +chess-show-positions-new-frame
                                             chess-pgn-show-position +chess-pgn-show-position)
  :init
  (map! :leader
        (:prefix-map ("k" . "chess")
          :desc "Display position for FEN found on current line." "f" '+chess-ivy-find-fens
          :desc "Display position for FEN found on current line in new frame." "F" '+chess-show-positions-new-frame))
  :config
  (set-popup-rule! "^\\*Chessboard.*" :side (if (string= system-name "blackbox") 'top 'left))

  (defun +chess-make-pos-from-fen (fen)
    "Display the position resulting from FEN."
    (let* ((game (chess-game-create))
           (new-display (chess-display-create game 'chess-images nil)))
      (chess-game-set-start-position game (chess-fen-to-pos fen))
      (chess-display-set-game new-display game 1)
      (chess-display-popup new-display)))

;;;###autoload
  (defun +chess-show-fen-at-point ()
    "Find the first fen on the line and display it."
    (interactive)
    (+chess-make-pos-from-fen (call-interactively '+chess-ivy-find-fens)))

  (defun +chess-show-positions-new-frame ()
    "Show the first fen on the line and display it in a second frame."
    (interactive)
    (let ((chess-images-separate-frame t))
      (call-interactively '+chess-ivy-find-fens)))

  (setq! chess-images-directory "/home/dodge/.emacs.d/.local/straight/repos/emacs-chess/pieces/xboard"
         chess-images-default-size 40
         chess-images-separate-frame nil)
  
  (defun +chess-images-popup ()
    "A special chess images popup function to work with doom buffers."
    (unless chess-images-size
      (chess-error 'no-images))

    (let* ((size (float (+ (* (or chess-images-border-width 0) 8)
                           (* chess-images-size 8))))
           (max-char-height (ceiling (/ size (frame-char-height))))
           (max-char-width  (ceiling (/ size (frame-char-width)))))
      ;; create the frame whenever necessary
      (if chess-images-separate-frame
          (chess-display-popup-in-frame 
           (+ max-char-height 2)
           max-char-width
           (cdr (assq 'font (frame-parameters))))
        (+chess--display-popup-in-window nil (+ max-char-height 1)))))

  (setq chess-images-popup-function ' +chess-images-popup)

  (defun +chess--display-popup-in-window (&optional max-h min-h max-w min-w)
    "Popup the given DISPLAY, so that it's visible to the user."
    (unless (get-buffer-window (current-buffer))
      (if (> (length (window-list)) 1)
          (fit-window-to-buffer (display-buffer (current-buffer)) 
                                max-h min-h max-w min-w)
        (display-buffer (current-buffer)))))

;;; If we're using Ivy, we can use a special function for getting the chess fens.
  (after! ivy
    (defun +chess-ivy-find-fens (beginning end)
      "Use ivy to select all chess fens between BEGINNING and END.

if beginning and end are not supplied (or the region is not active) confine to current line."
      (interactive "r")
      (let ((chess-fen-regex "\\([bnrqkpBNRQKP1-8]*/?\\)+ [bw] \\(-\\|[KQkq]+\\) \\(-\\|[1-8]\\)")
            (fen-region (buffer-substring-no-properties
                         (if (use-region-p) beginning (line-beginning-position))
                         (if (use-region-p) end (line-end-position))))
            (matches '())
            (pos 0))
        (save-excursion
          (while (string-match chess-fen-regex fen-region pos)
            (push (match-string 0 fen-region) matches)
            (setq pos (match-end 0))))
        (if matches
            (if (> (length matches) 1)
                (ivy-read
                 "Found FENs: " matches
                 :action '+chess-make-pos-from-fen
                 :unwind
                 (lambda ()
                   (mapc #'kill-buffer counsel--switch-buffer-temporary-buffers)
                   (mapc #'bury-buffer (cl-remove-if-not
                                        #'buffer-live-p
                                        counsel--switch-buffer-previous-buffers))
                   (setq counsel--switch-buffer-temporary-buffers nil
                         counsel--switch-buffer-previous-buffers nil))
                 :update-fn
                 (lambda ()
                   (unless counsel--switch-buffer-previous-buffers
                     (setq counsel--switch-buffer-previous-buffers (buffer-list)))
                   (let* ((current (ivy-state-current ivy-last))
                          (virtual (assoc current ivy--virtual-buffers)))
                     (mapc 'kill-buffer
                           (seq-filter (apply-partially 'string-match-p "\\*Chessboard\\*\\(<[0-9]+>\\)?")
                                       (mapcar (function buffer-name) (buffer-list))))
                     (let ((chess-images-separate-frame nil))
                      ;;;FIXME: this is horrible. Use let or something.
                       (chess-message-catalog 'english
                         '((piece-images-loading . "")
                           (piece-images-loaded  . "")))

                       (+chess-make-pos-from-fen current)
                       (chess-message-catalog 'english
                         '((piece-images-loading . "Loading chess piece images...")
                           (piece-images-loaded  . "Loading chess piece images...done")))
                       )
                     ;;; Clean up the minibuffer so we can display the candidates again.
                     (message ""))))
              (+chess-make-pos-from-fen (car matches)))
          (message "%s" "Could not find FENs."))))))

;; Add smartparens mappings
(map!
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

   :n "s"     #'sp-forward-slurp-sexp
   :n "f"     #'sp-forward-barf-sexp
   :n "bs"    #'sp-backward-slurp-sexp
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


;;; Org-mode
(after! org
  (+org-pretty-mode))

;;; Python
(after! python-mode
  (set-pretty-symbols! 'python-mode
    :lambda "lambda"))

;;; Javascript
(after! js2-mode
  (set-pretty-symbols! 'js2-mode
    :lambda "function"))

;;; C/C++
(after! c++-mode
  (set-pretty-symbols! 'c++-mode nil))
