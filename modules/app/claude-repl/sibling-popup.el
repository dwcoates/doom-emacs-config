;;; sibling-popup.el --- bottom popup that respects claude panels -*- lexical-binding: t; -*-

;;; Commentary:

;; Display-buffer helpers for "sibling" popups that should NOT span
;; under the claude panels (vterm + input column on the right) when
;; those panels are open.
;;
;; The Doom popup default for `:side 'bottom' uses a frame-wide side
;; window, so a bottom popup (e.g. the normal `*doom:vterm*' opened by
;; `SPC o t') spans the entire frame width — overlapping the claude
;; vterm / input column on the right.
;;
;; `claude-repl-sibling-popup-display-fn' is a `display-buffer' action
;; that, when claude panels are visible, instead splits BELOW the
;; leftmost non-side, non-claude-panel "work" window.  The new popup
;; window inherits the work window's column width, so it stops on the
;; right edge of the work area rather than spanning under claude.  When
;; claude panels are NOT visible, it falls back to the standard Doom
;; bottom side-window behavior so callers can install it as a
;; drop-in replacement for `:side 'bottom'.

;;; Code:

(require 'cl-lib)

(declare-function claude-repl-window--panel-buffer "window")
(declare-function claude-repl-window--side-window-p "window")
(declare-function +popup-display-buffer-stacked-side-window-fn "ext:popup")

(defun claude-repl-sibling-popup--target-window (&optional frame)
  "Return the work window to split below for a sibling bottom popup.

Returns the leftmost live, non-side, non-claude-panel window on FRAME
\(defaults to the selected frame\) when the claude vterm panel is
visible — that is the work column whose width the popup should match.

Returns nil when claude panels are not visible, signalling to callers
that the default frame-wide bottom popup behavior should apply."
  (let* ((vterm-buf (claude-repl-window--panel-buffer :vterm))
         (vterm-win (and vterm-buf
                         (buffer-live-p vterm-buf)
                         (get-buffer-window vterm-buf frame))))
    (when (window-live-p vterm-win)
      (let* ((input-buf (claude-repl-window--panel-buffer :input))
             (best nil)
             (best-x most-positive-fixnum))
        (dolist (win (window-list frame 'no-mini))
          (let* ((buf (window-buffer win))
                 (x (car (window-edges win nil nil t)))
                 (is-side (claude-repl-window--side-window-p win))
                 (is-claude (or (eq buf vterm-buf)
                                (and input-buf (eq buf input-buf)))))
            (when (and (not is-side) (not is-claude) (< x best-x))
              (setq best win
                    best-x x))))
        best))))

(defun claude-repl-sibling-popup--popup-height (target alist)
  "Compute the popup height in lines for TARGET window given ALIST.
Reads `window-height' (preferred) or `size' from ALIST; accepts an
integer (lines) or a float fraction of TARGET's total height.  Floors
at `window-min-height' so the split is always valid."
  (let ((size (or (cdr (assq 'window-height alist))
                  (cdr (assq 'size alist))
                  0.3)))
    (max window-min-height
         (cond
          ((integerp size) size)
          ((floatp size) (round (* size (window-total-height target))))
          (t (round (* 0.3 (window-total-height target))))))))

(defun claude-repl-sibling-popup--fallback (buffer alist)
  "Fallback display when no claude work window is found.
Prefers `+popup-display-buffer-stacked-side-window-fn' (Doom's
stacked-side-window helper) so vslot/slot semantics are preserved
when the popup module is loaded; otherwise uses the plain
`display-buffer-in-side-window' with the requested or default side."
  (cond
   ((fboundp '+popup-display-buffer-stacked-side-window-fn)
    (+popup-display-buffer-stacked-side-window-fn buffer alist))
   (t (display-buffer-in-side-window
       buffer
       (cons (cons 'side (or (cdr (assq 'side alist)) 'bottom))
             alist)))))

;;;###autoload
(defun claude-repl-sibling-popup-display-fn (buffer alist)
  "`display-buffer' action that respects claude panels for bottom popups.

When the claude vterm panel is visible, splits BELOW the leftmost
work (non-side, non-claude-panel) window and displays BUFFER there;
the popup width matches the work column, leaving claude panels
untouched.  When claude panels are absent, defers to the Doom
stacked-side-window action so the default frame-wide bottom-popup
behavior is preserved.

Intended for installation as the `:actions' of a `set-popup-rule!'
entry that would otherwise use `:side 'bottom' — see config.el's
vterm popup rule for the canonical caller."
  (let ((target (claude-repl-sibling-popup--target-window)))
    (if (window-live-p target)
        (let* ((height-lines (claude-repl-sibling-popup--popup-height target alist))
               (new-win (split-window target (- height-lines) 'below)))
          (set-window-buffer new-win buffer)
          new-win)
      (claude-repl-sibling-popup--fallback buffer alist))))

(provide 'claude-repl-sibling-popup)
;;; sibling-popup.el ends here
