;;; app/chess/autoload.el -*- lexical-binding: t; -*-

;;; If we're using Ivy, we can use a special function for getting the chess fens.
;;;###autoload
(defun +chess-ivy-show-fens (beginning end)
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
      (message "%s" "Could not find FENs."))))

;;; TODO: move this inside of chess-ivy-show-frame. Use univeral arg or something.
;;;###autoload
(defun +chess-ivy-show-fens-new-frame ()
  "Show the first fen on the line and display it in a second frame."
  (interactive)
  (let ((chess-images-separate-frame t))
    (call-interactively '+chess-ivy-show-fens)))
