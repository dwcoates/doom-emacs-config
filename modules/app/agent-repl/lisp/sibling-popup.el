;;; sibling-popup.el --- bottom popup that respects agent panels -*- lexical-binding: t; -*-

;;; Commentary:

;; Display-buffer helpers for "sibling" popups that should NOT span
;; under the sidebar when the agent panels are open.
;;
;; With the panels open, the frame is the sidebar (a left side window,
;; sidebar-view.el) beside the agent column: the feed's window over the
;; composer's.  The Doom popup default for `:side 'bottom' is a
;; frame-wide side window, and with `window-sides-vertical' nil a bottom
;; side window runs under the left one, so a bottom popup (e.g. the
;; `*doom:vterm*' opened by `SPC o t') would span under the sidebar too.
;;
;; `agent-repl-sibling-popup-display-fn' is a `display-buffer' action
;; that, when the agent view is visible, instead splits BELOW the agent
;; column: the window holding the feed over the composer.  The new popup
;; window inherits the column's width, so it sits under the composer
;; alone and the sidebar keeps the frame's whole height.  When the agent
;; panels are NOT visible, it falls back to the standard Doom bottom
;; side-window behavior so callers can install it as a drop-in
;; replacement for `:side 'bottom'.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)

(declare-function agent-repl-window--panel-window "window")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function +popup-display-buffer-stacked-side-window-fn "ext:popup")

(defun agent-repl-sibling-popup--target-window (&optional frame ws)
  "Return the window to split below for a sibling bottom popup.

When WS's agent view is visible on FRAME \(defaults to the selected
frame\), returns the agent column: the composer's window when it
stands, else the view's (a standing gate hides the composer), widened
to its parent when that parent stacks it vertically \(the feed over the
composer\).  Splitting that window below puts the popup under the
composer, as wide as the column, and never under the sidebar.

Returns nil when the agent view is not visible, signalling to callers
that the default frame-wide bottom popup behavior should apply."
  (let ((view-win (agent-repl-window--panel-window :view ws frame)))
    (agent-repl--log ws "sibling-popup target: frame={frame} view-window={view_window}" :frame (prin1-to-string frame) :view_window (prin1-to-string view-win))
    (when view-win
      (let* ((input-win (agent-repl-window--panel-window :input ws frame))
             (anchor (or input-win view-win))
             (stacked (window-combined-p anchor))
             (target (if stacked (window-parent anchor) anchor)))
        (agent-repl--log ws
                         "sibling-popup target: result={result} input-window={input_window} anchor={anchor} stacked={stacked}"
                         :result (prin1-to-string target) :input_window (prin1-to-string input-win) :anchor (prin1-to-string anchor) :stacked stacked)
        target))))

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
       "sibling-popup height: target={target} target-height={target_height} window-height={window_height} size={size} selected-size={selected_size} kind={kind} requested={requested} minimum={minimum} result={result}"
       :target (prin1-to-string target) :target_height target-height :window_height (prin1-to-string height-entry) :size (prin1-to-string size-entry) :selected_size (prin1-to-string size) :kind kind :requested requested-height
       :minimum window-min-height :result height-lines)
      height-lines)))

(defun agent-repl-sibling-popup--fallback (buffer alist &optional ws)
  "Fallback display when the agent view is not visible.
Prefers `+popup-display-buffer-stacked-side-window-fn' (Doom's
stacked-side-window helper) so vslot/slot semantics are preserved
when the popup module is loaded; otherwise uses the plain
`display-buffer-in-side-window' with the requested or default side."
  (cond
   ((fboundp '+popup-display-buffer-stacked-side-window-fn)
    (agent-repl--log ws
                     "sibling-popup fallback: buffer={buffer} alist={alist} handler=stacked-side-window"
                     :buffer (prin1-to-string buffer) :alist (prin1-to-string alist))
    (+popup-display-buffer-stacked-side-window-fn buffer alist))
   (t (agent-repl--log ws
                        "sibling-popup fallback: buffer={buffer} alist={alist} handler=side-window side={side}"
                        :buffer (prin1-to-string buffer) :alist (prin1-to-string alist) :side (prin1-to-string (cdr (assq 'side alist))))
      (display-buffer-in-side-window
       buffer
       (cons (cons 'side (or (cdr (assq 'side alist)) 'bottom))
             alist)))))

;;;###autoload
(defun agent-repl-sibling-popup-display-fn (buffer alist)
  "`display-buffer' action that keeps bottom popups out from under the sidebar.

When the agent view panel is visible, splits BELOW the agent column
\(`agent-repl-sibling-popup--target-window') and displays BUFFER there;
the popup sits under the composer, as wide as the column, leaving the
sidebar its whole height.  When the agent panels are absent, defers to the Doom
stacked-side-window action so the default frame-wide bottom-popup
behavior is preserved.

Intended for installation as the `:actions' of a `set-popup-rule!'
entry that would otherwise use `:side \\='bottom' — see config.el's
vterm popup rule for the canonical caller."
  (let* ((ws (agent-repl--ws-current-name))
         (target (agent-repl-sibling-popup--target-window nil ws)))
    (agent-repl--log ws
                     "sibling-popup display: buffer={buffer} alist={alist} target={target} target-valid={target_valid}"
                     :buffer (prin1-to-string buffer) :alist (prin1-to-string alist) :target (prin1-to-string target) :target_valid (window-valid-p target))
    (if (window-valid-p target)
        (let* ((height-lines (agent-repl-sibling-popup--popup-height target alist ws))
               (new-win (split-window target (- height-lines) 'below)))
          (set-window-buffer new-win buffer)
          (agent-repl--log ws
                           "sibling-popup display: action=split target={target} height={height} new-window={new_window} buffer={buffer}"
                           :target (prin1-to-string target) :height height-lines :new_window (prin1-to-string new-win) :buffer (prin1-to-string buffer))
          new-win)
      (agent-repl--log ws
                       "sibling-popup display: action=fallback buffer={buffer} alist={alist}"
                       :buffer (prin1-to-string buffer) :alist (prin1-to-string alist))
      (agent-repl-sibling-popup--fallback buffer alist ws))))

(provide 'agent-repl-sibling-popup)
;;; sibling-popup.el ends here
