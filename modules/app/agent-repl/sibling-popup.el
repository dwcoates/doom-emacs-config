;;; sibling-popup.el --- bottom popup that respects agent panels -*- lexical-binding: t; -*-

;;; Commentary:

;; Display-buffer helpers for "sibling" popups that should NOT span
;; under the agent panels (the webview + input column on the right)
;; when those panels are open.
;;
;; The Doom popup default for `:side 'bottom' uses a frame-wide side
;; window, so a bottom popup (e.g. the normal `*doom:vterm*' opened by
;; `SPC o t') spans the entire frame width — overlapping the agent
;; view / input column on the right.
;;
;; `agent-repl-sibling-popup-display-fn' is a `display-buffer' action
;; that, when agent panels are visible, instead splits BELOW the
;; leftmost non-side, non-agent-panel "work" window.  The new popup
;; window inherits the work window's column width, so it stops on the
;; right edge of the work area rather than spanning under the agent.  When
;; agent panels are NOT visible, it falls back to the standard Doom
;; bottom side-window behavior so callers can install it as a
;; drop-in replacement for `:side 'bottom'.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl-window--panel-buffer "window")
(declare-function agent-repl-window--side-window-p "window")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function +popup-display-buffer-stacked-side-window-fn "ext:popup")

(defun agent-repl-sibling-popup--target-window (&optional frame ws)
  "Return the work window to split below for a sibling bottom popup.

Returns the leftmost live, non-side, non-agent-panel window on FRAME
\(defaults to the selected frame\) when the agent view panel is
visible — that is the work column whose width the popup should match.

Returns nil when agent panels are not visible, signalling to callers
that the default frame-wide bottom popup behavior should apply."
  (let* ((view-buf (agent-repl-window--panel-buffer :view ws))
         (view-win (and view-buf
                        (buffer-live-p view-buf)
                        (get-buffer-window view-buf frame))))
    (agent-repl--log ws
                     "sibling-popup target: frame=%S view-buffer=%S view-buffer-live=%s view-window=%S view-window-live=%s"
                     frame view-buf (buffer-live-p view-buf) view-win
                     (window-live-p view-win))
    (when (window-live-p view-win)
      (let* ((input-buf (agent-repl-window--panel-buffer :input ws))
             (best nil)
             (best-x most-positive-fixnum))
        (dolist (win (window-list frame 'no-mini))
          (let* ((buf (window-buffer win))
                 (x (car (window-edges win nil nil t)))
                 (is-side (agent-repl-window--side-window-p win))
                 (is-agent (or (eq buf view-buf)
                                (and input-buf (eq buf input-buf)))))
            (agent-repl--log-verbose
             ws
             "sibling-popup target: candidate=%S buffer=%S x=%s side=%s agent=%s current-best=%S current-best-x=%s"
             win buf x is-side is-agent best best-x)
            (when (and (not is-side) (not is-agent) (< x best-x))
              (setq best win
                    best-x x)
              (agent-repl--log ws
                               "sibling-popup target: selected candidate=%S buffer=%S x=%s"
                               best buf best-x))))
        (agent-repl--log ws
                         "sibling-popup target: result=%S result-buffer=%S result-x=%s input-buffer=%S"
                         best (and (window-live-p best) (window-buffer best))
                         best-x input-buf)
        best))))

(defun agent-repl-sibling-popup--popup-height (target alist &optional ws)
  "Compute the popup height in lines for TARGET window given ALIST.
Reads `window-height' (preferred) or `size' from ALIST; accepts an
integer (lines) or a float fraction of TARGET's total height.  Floors
at `window-min-height' so the split is always valid."
  (let ((size (or (cdr (assq 'window-height alist))
                  (cdr (assq 'size alist))
                  0.3)))
    (let* ((height-entry (assq 'window-height alist))
           (size-entry (assq 'size alist))
           (target-height (window-total-height target))
           (kind (cond
                  ((integerp size) 'integer)
                  ((floatp size) 'float)
                  (t 'unsupported)))
           (requested-height (cond
                              ((integerp size) size)
                              ((floatp size) (round (* size target-height)))
                              (t (round (* 0.3 target-height)))))
           (height-lines (max window-min-height requested-height)))
      (agent-repl--log
       ws
       "sibling-popup height: target=%S target-height=%s window-height=%S size=%S selected-size=%S kind=%s requested=%s minimum=%s result=%s"
       target target-height height-entry size-entry size kind requested-height
       window-min-height height-lines)
      height-lines)))

(defun agent-repl-sibling-popup--fallback (buffer alist &optional ws)
  "Fallback display when no claude work window is found.
Prefers `+popup-display-buffer-stacked-side-window-fn' (Doom's
stacked-side-window helper) so vslot/slot semantics are preserved
when the popup module is loaded; otherwise uses the plain
`display-buffer-in-side-window' with the requested or default side."
  (cond
   ((fboundp '+popup-display-buffer-stacked-side-window-fn)
    (agent-repl--log ws
                     "sibling-popup fallback: buffer=%S alist=%S handler=stacked-side-window"
                     buffer alist)
    (+popup-display-buffer-stacked-side-window-fn buffer alist))
   (t (agent-repl--log ws
                        "sibling-popup fallback: buffer=%S alist=%S handler=side-window side=%S"
                        buffer alist (cdr (assq 'side alist)))
      (display-buffer-in-side-window
       buffer
       (cons (cons 'side (or (cdr (assq 'side alist)) 'bottom))
             alist)))))

;;;###autoload
(defun agent-repl-sibling-popup-display-fn (buffer alist)
  "`display-buffer' action that respects claude panels for bottom popups.

When the claude agent view panel is visible, splits BELOW the leftmost
work (non-side, non-agent-panel) window and displays BUFFER there;
the popup width matches the work column, leaving claude panels
untouched.  When claude panels are absent, defers to the Doom
stacked-side-window action so the default frame-wide bottom-popup
behavior is preserved.

Intended for installation as the `:actions' of a `set-popup-rule!'
entry that would otherwise use `:side \\='bottom' — see config.el's
vterm popup rule for the canonical caller."
  (let* ((ws (agent-repl--ws-current-name))
         (target (agent-repl-sibling-popup--target-window nil ws)))
    (agent-repl--log ws
                     "sibling-popup display: buffer=%S alist=%S target=%S target-live=%s"
                     buffer alist target (window-live-p target))
    (if (window-live-p target)
        (let* ((height-lines (agent-repl-sibling-popup--popup-height target alist ws))
               (new-win (split-window target (- height-lines) 'below)))
          (set-window-buffer new-win buffer)
          (agent-repl--log ws
                           "sibling-popup display: action=split target=%S height=%s new-window=%S buffer=%S"
                           target height-lines new-win buffer)
          new-win)
      (agent-repl--log ws
                       "sibling-popup display: action=fallback buffer=%S alist=%S"
                       buffer alist)
      (agent-repl-sibling-popup--fallback buffer alist ws))))

(provide 'agent-repl-sibling-popup)
;;; sibling-popup.el ends here
