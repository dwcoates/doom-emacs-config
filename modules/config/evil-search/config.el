;;; config/evil-search/config.el -*- lexical-binding: t; -*-
;;
;; Evil's `/' and `?' search highlighting.
;;
;; The match point is ON wears the `evil-ex-search' face, and every other
;; match wears `evil-ex-lazy-highlight'.  Evil itself draws the current
;; match only while the `/' prompt is open: `RET' drops it (with
;; `evil-ex-search-persistent-highlight' nil), and `n' / `N' re-highlight
;; every match but never the one they land on.  The current-match overlay
;; below follows point instead: `RET', `n', `N', `*' and `#' all move it to
;; the match they land on, and it goes when the search highlight does
;; (`evil-ex-nohighlight', which Doom's ESC runs).

(defvar-local +dwc/evil-search-current-overlay nil
  "The overlay marking the search match point last landed on, or nil.")

(defun +dwc/evil-search-mark-current (&rest _)
  "Mark the match the last Evil search landed on with `evil-ex-search'.
Evil records that match's bounds in `evil-ex-search-match-beg' and
`evil-ex-search-match-end'; a search that lands records both."
  (unless (and (integer-or-marker-p evil-ex-search-match-beg)
               (integer-or-marker-p evil-ex-search-match-end))
    (error "evil-search: a landed search left no match bounds (beg=%S end=%S)"
           evil-ex-search-match-beg evil-ex-search-match-end))
  (if (overlayp +dwc/evil-search-current-overlay)
      (move-overlay +dwc/evil-search-current-overlay
                    evil-ex-search-match-beg evil-ex-search-match-end
                    (current-buffer))
    (let ((overlay (make-overlay evil-ex-search-match-beg evil-ex-search-match-end)))
      ;; Above Evil's own highlight overlays (priority 1000), as Evil's
      ;; in-prompt current-match overlay is.
      (overlay-put overlay 'priority 1001)
      (overlay-put overlay 'face 'evil-ex-search)
      (setq +dwc/evil-search-current-overlay overlay))))

(defun +dwc/evil-search-unmark-current (name)
  "Drop the current-match overlay when Evil deletes highlight NAME.
Only the `evil-ex-search' highlight owns the overlay."
  (when (and (eq name 'evil-ex-search)
             (overlayp +dwc/evil-search-current-overlay))
    (delete-overlay +dwc/evil-search-current-overlay)
    (setq +dwc/evil-search-current-overlay nil)))

(defun +dwc/evil-search-current-face-background ()
  "Return the current match's background: agent-repl's one purple.
The purple status color the webapp paints its purple in, read from
render-colors.json by `agent-repl-status--color-value'."
  (agent-repl-status--color-value "purple"))

(with-eval-after-load 'evil
  (setq evil-ex-search-persistent-highlight nil)
  (set-face-attribute 'evil-ex-lazy-highlight nil
                      :foreground "gray" :background "#a9a9a9")
  (advice-add 'evil-ex-start-search :after #'+dwc/evil-search-mark-current)
  (advice-add 'evil-ex-search :after #'+dwc/evil-search-mark-current)
  (advice-add 'evil-ex-delete-hl :after #'+dwc/evil-search-unmark-current)
  (with-eval-after-load 'agent-repl-status
    (set-face-attribute 'evil-ex-search nil
                        :foreground "black"
                        :background (+dwc/evil-search-current-face-background)
                        :weight 'bold)))
