;;; ~/.doom.d/chess.el -*- lexical-binding: t; -*-
;;;
;;;
;;; Configuration and modification file for emacs-chess.


;;; Chess
(use-package! chess
  :commands (+chess-ivy-show-fens +chess-ivy-show-fens-new-frame +chess-make-pos-from-fen)
  :init
  (map! :leader
        (:prefix-map ("k" . "chess")
          :desc "Display position for FEN found on current line." "f" '+chess-ivy-show-fens
          :desc "Display position for FEN found on current line in new frame." "F" '+chess-ivy-show-fens-new-frame))
  :config
  ;; TODO: this decision should really be made on a screen-size basis.
  (set-popup-rule! "^\\*Chessboard.*" :side (if (string= system-name "blackbox") 'top 'left))

  (add-to-list 'evil-emacs-state-modes 'chess-display-mode)

  (map! :map chess-display-mode
        :g
        "h" 'chess-display-move-forward)

  (defun +chess-make-pos-from-fen (fen)
    "Display the position resulting from FEN."
    (let* ((game (chess-game-create))
           (new-display (chess-display-create game 'chess-images nil)))
      (chess-game-set-start-position game (chess-fen-to-pos fen))
      (chess-display-set-game new-display game 1)
      (chess-display-popup new-display)))

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

  ;; We set the chess-images popup to be doom-compatible.
  (setq chess-images-popup-function ' +chess-images-popup)

  (defun +chess--display-popup-in-window (&optional max-h min-h max-w min-w)
    "Popup the given DISPLAY, so that it's visible to the user."
    (unless (get-buffer-window (current-buffer))
      (if (> (length (window-list)) 1)
          (fit-window-to-buffer (display-buffer (current-buffer))
                                max-h min-h max-w min-w)
        (display-buffer (current-buffer))))))
