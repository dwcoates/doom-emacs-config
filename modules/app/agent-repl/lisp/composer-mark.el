;;; composer-mark.el --- the composer's purple marks on known words -*- lexical-binding: t; -*-

;;; Commentary:

;; A WORD THE COMPOSER KNOWS IS PURPLE.  A slash word naming a known command
;; (command-completion.el) and a word naming an open workspace
;; (workspace-completion.el) are both drawn in ONE face,
;; `agent-repl-composer-known', so the two kinds can never drift apart in
;; color.
;;
;; THE MARKING IS ONE SHAPE, shared by every kind: an edit re-marks the whole
;; lines it touched (`agent-repl-composer-mark-lines').  Each kind tags its
;; overlays with its own property, so re-marking one kind never removes
;; another kind's marks, and FINDS its own words through a function; this
;; file owns the rest -- the line widening, the unmarking, and the overlay
;; with the face.

;;; Code:

(defface agent-repl-composer-known
  '((t :foreground "#7d4bcf"))
  "Face of a word in the composer that names something known.
A slash word naming a known command, or a word naming an open workspace."
  :group 'agent-repl)

(define-obsolete-face-alias 'agent-repl-command-completion-known
  'agent-repl-composer-known "2026-11-08")

(defun agent-repl-composer-mark-lines (beg end tag find)
  "Mark the words FIND finds on the lines BEG to END with TAG, unmark the rest.
The region is widened to whole lines.  Every overlay carrying the
property TAG in it is removed first; then FIND is called with point at
the region's start and the region's end as its one argument, again and
again, each call answering the next word to mark as (START . END), with
point left at or past END, or nil when no word is left.  Each word
gets an overlay with TAG set and the face `agent-repl-composer-known'."
  (save-excursion
    (let ((beg (progn (goto-char beg) (line-beginning-position)))
          (end (progn (goto-char end) (line-end-position))))
      (remove-overlays beg end tag t)
      (goto-char beg)
      (let (bounds)
        (while (setq bounds (funcall find end))
          (let ((ov (make-overlay (car bounds) (cdr bounds))))
            (overlay-put ov tag t)
            (overlay-put ov 'face 'agent-repl-composer-known)))))))

(provide 'composer-mark)
;;; composer-mark.el ends here
