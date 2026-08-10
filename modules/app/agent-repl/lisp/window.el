;;; window.el --- Centralized window-management helpers for agent-repl -*- lexical-binding: t; -*-

;;; Commentary:

;; Single home for window-management primitives shared across the
;; agent-repl package.  Before this module existed, identical
;; recipes for window hardening, subset-deletion, and side-window
;; awareness were inlined into panels.el, magit.el, session.el, and
;; keybindings.el — drift across the copies turned simple operations
;; (e.g. `SPC g g' opening magit) into surprising side-effects on
;; sibling windows.
;;
;; Conceptual scope:
;;
;;   • Panel registry (data describing each known panel kind).
;;   • Window finders (resolve a panel kind to a live window).
;;   • Hardening (dedicate, size-fix, delete-protect, preserve-size).
;;   • Subset deletion (`--delete-where' with side-window skipping by
;;     default — the SPC g g class of bug is fixed at this layer).
;;   • Focus + buffer-current dispatch.
;;   • Layout transactions (`--ensure-layout' for declarative target
;;     layouts that diff against current and apply minimum changes).
;;   • Per-workspace window-configuration save/restore.
;;   • Centralized hook install/uninstall.
;;
;; Out of scope (each lives in its own module and CALLS helpers here):
;;
;;   • Persp save/restore policy (panels.el / persp integration).
;;   • Hide-overlay (a buffer overlay, not a window).
;;   • Tab-bar shuffle.
;;   • Magit advice glue.
;;
;; Today the module ships the panel finders, per-window hardening, the
;; side-window-aware subset-deletion primitive (the helper that retired
;; the dedicated SPC-g-g bug), and the `--ensure-layout' reconciler;
;; the remaining bullets stay reserved for follow-up migrations.

;;; Code:

(require 'cl-lib)

;;;; --- Panel registry -----------------------------------------------------
;;
;; Reserved: populated in a follow-up step when callers are migrated.
;; The intended shape is:
;;
;;   '((:view   :fill main :buffer-key :frontend-buffer
;;              :harden (dedicate size-fix-width  delete-protect))
;;     (:input  :side below :buffer-key :input-buffer
;;              :height-frac agent-repl-input-height-fraction
;;              :harden (dedicate size-fix-height delete-protect preserve-size)))

;;;; --- Panel finders -----------------------------------------------------

(defun agent-repl-window--panel-buffer (kind &optional ws)
  "Return the buffer for panel KIND in workspace WS.

KIND ∈ (:view :input).  WS defaults to the current workspace.

Returns the buffer object regardless of liveness — callers needing
liveness must check `buffer-live-p' (matches the historical lookup
pattern this helper replaces).  Returns nil when the panel has not
been initialized.  Signals an error for an unknown KIND so typos
surface at call time.

WS is caller-supplied and may name any perspective — the persp activation
path asks this helper about persp-mode's own \"main\" and \"none\", which own
no durable log sink.  Lookups use the resolved name; records use
`agent-repl--ws-log-name' and carry the name in their text."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (log-ws (agent-repl--ws-log-name resolved-ws)))
    (pcase kind
      (:view
       (let ((buf (agent-repl--ws-get resolved-ws :frontend-buffer)))
         (agent-repl--log-verbose
          log-ws
          "window--panel-buffer: ws=%s kind=%S key=:frontend-buffer buffer=%S live=%s"
          resolved-ws kind buf (and buf (buffer-live-p buf)))
         buf))
      (:input
       (let ((buf (agent-repl--ws-get resolved-ws :input-buffer)))
         (agent-repl--log-verbose
          log-ws
          "window--panel-buffer: ws=%s kind=%S key=:input-buffer buffer=%S live=%s"
          resolved-ws kind buf (and buf (buffer-live-p buf)))
         buf))
      (_
       (agent-repl--log log-ws
                         "window--panel-buffer: ws=%s unknown kind=%S" resolved-ws kind)
       (error "agent-repl-window--panel-buffer: unknown KIND %S" kind)))))

(defun agent-repl-window--panel-window (kind &optional ws frame)
  "Return the live window displaying panel KIND, or nil.

KIND ∈ (:view :input).  WS defaults to the current workspace.
FRAME is passed through to `get-buffer-window' (nil = selected
frame, t = all frames, a frame value = that frame).

Guards on `buffer-live-p' so a stale buffer reference returns nil
rather than tripping `get-buffer-window' with a dead buffer."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (buf (agent-repl-window--panel-buffer kind resolved-ws))
         (live (and buf (buffer-live-p buf)))
         (win (and live (get-buffer-window buf frame))))
    (agent-repl--log-verbose
     (agent-repl--ws-log-name resolved-ws)
     "window--panel-window: ws=%s kind=%S buffer=%S live=%s frame=%S found=%S"
     resolved-ws kind buf live frame win)
    win))

;;;; --- Side-window awareness ---------------------------------------------

(defun agent-repl-window--side-window-p (win &optional ws)
  "Return non-nil if WIN is a side window.
A side window is one created via `display-buffer-in-side-window' (or
equivalent), distinguished by a non-nil `window-side' window
parameter.  Treat these as exempt from generic layout-clearing
operations so commands that reset the main window tree (e.g.
`delete-other-windows', `+dwc/magit-status-workspace') don't trample
frame-level side-window UI elements."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (live (window-live-p win))
         (side (and live (window-parameter win 'window-side))))
    (agent-repl--log-verbose resolved-ws
                             "window--side-window-p: window=%S live=%s side=%S"
                             win live side)
    side))

;;;; --- Per-window hardening ----------------------------------------------

(cl-defun agent-repl-window--harden
    (win &key
         dedicate
         size-fix
         delete-protect
         preserve-size
         no-other-window
         fringes
         ws)
  "Apply a hardening recipe to WIN.

Centralizes the dedicate/size-fix/delete-protect/preserve-size
combinations that panel-style windows (view, input) all need.
Before this helper existed, the same four-line recipe was inlined
into 3+ call sites with subtle drift.

Each keyword is independent and may be omitted:

  :DEDICATE        non-nil → `set-window-dedicated-p WIN t'.

  :SIZE-FIX        Symbol passed to `set-window-parameter WIN
                   \\='window-size-fixed'.  Accepted values match
                   Emacs's `window-size-fixed' contract: `width',
                   `height', or `t' (both).  Window parameter (not
                   buffer-local) so the lock is per-window even
                   when the same buffer appears elsewhere.

  :DELETE-PROTECT  non-nil → sets `no-delete-other-windows' window
                   parameter so `delete-other-windows' refuses to
                   remove WIN.  Note: this is NOT enough on its
                   own — direct `delete-window' calls bypass this
                   parameter; the side-window-aware `--delete-where'
                   covers the rest.

  :PRESERVE-SIZE   Symbol controlling `window-preserve-size'.
                   Accepted values: `width', `height', `t' (both).
                   This is the only way to defend a window's size
                   against `window--resize-mini-window' (which
                   ignores `window-size-fixed' alone — see
                   `panels.el' input-panel comment for the gory
                   detail).

  :NO-OTHER-WINDOW non-nil → sets the `no-other-window' parameter
                   so `other-window' (keyboard nav) skips WIN.

  :FRINGES         nil (leave alone) | integer N (both fringes to
                   N px) | cons cell (LEFT . RIGHT).  Common
                   shorthand: pass 0 to hide both fringes (and so
                   the wrap-continuation glyph).

  :WS              Workspace used only for diagnostics.  When omitted,
                   diagnostics use the current workspace."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (live (window-live-p win)))
    (agent-repl--log resolved-ws
                     "window--harden: window=%S live=%s dedicate=%S size-fix=%S delete-protect=%S preserve-size=%S no-other-window=%S fringes=%S"
                     win live dedicate size-fix delete-protect preserve-size
                     no-other-window fringes)
    (when (and live dedicate)
      (set-window-dedicated-p win t))
    (when (and live size-fix)
      (set-window-parameter win 'window-size-fixed size-fix))
    (when (and live delete-protect)
      (set-window-parameter win 'no-delete-other-windows t))
    (when (and live no-other-window)
      (set-window-parameter win 'no-other-window t))
    (when (and live preserve-size)
      (cond
       ((eq preserve-size 'width)
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: preserve-size=width window=%S" win)
        (window-preserve-size win t t))
       ((eq preserve-size 'height)
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: preserve-size=height window=%S" win)
        (window-preserve-size win nil t))
       ((eq preserve-size t)
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: preserve-size=both window=%S" win)
        (window-preserve-size win t t)
        (window-preserve-size win nil t))
       (t
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: preserve-size=unrecognized value=%S window=%S"
                                 preserve-size win))))
    (when (and live fringes)
      (cond
       ((integerp fringes)
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: fringes=uniform value=%S window=%S"
                                 fringes win)
        (set-window-fringes win fringes fringes nil))
       ((and (consp fringes)
             (integerp (car fringes))
             (integerp (cdr fringes)))
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: fringes=asymmetric left=%S right=%S window=%S"
                                 (car fringes) (cdr fringes) win)
        (set-window-fringes win (car fringes) (cdr fringes) nil))
       (t
        (agent-repl--log-verbose resolved-ws
                                 "window--harden: fringes=unrecognized value=%S window=%S"
                                 fringes win))))))

;;;; --- Deletion, or its structural substitute -----------------------------
;;
;; A frame's SOLE ordinary window and the minibuffer window cannot be
;; deleted; `delete-window' signals "Attempt to delete minibuffer or sole
;; ordinary window" and its siblings.  Every agent-repl teardown path that
;; targets a window has to answer the same question — what happens when the
;; window is the last one — and answering it with a `condition-case' answers
;; it WRONGLY: the error is caught, the window survives, and the buffer the
;; teardown was retiring stays on screen in a workspace that no longer owns
;; it.  That is the observed close-path failure (the
;; `window--delete-buffer-windows' error logged closing
;; slack-core-chess-ai-cxo) and the observed post-merge one (a dead
;; workspace's panel left displayed).
;;
;; `agent-repl-window--delete-or-neutralize' answers it once, for every
;; caller: delete when the window is deletable, and otherwise SWITCH it to a
;; fallback buffer.  The undeletable window is never asked to be deleted, so
;; the error is not caught — it is not raised.  Un-dedicating first is part
;; of the same guarantee: `set-window-buffer' signals on a STRONGLY dedicated
;; window, which is exactly what agent-repl's own hardened panels are, and
;; that signal is the `strongly-dedicated-window' error seen right after a
;; merge.

(defun agent-repl-window--delete-or-neutralize (win &optional fallback ws)
  "Delete WIN, or switch it to FALLBACK when WIN cannot be deleted.
Returns `deleted' when WIN was deleted, `neutralized' when its buffer was
switched instead, and nil when WIN was not live to begin with.

FALLBACK defaults to `doom-fallback-buffer'.  WS is used only for
workspace-scoped diagnostics.

Un-dedicates WIN and strips its `no-delete-other-windows' parameter
first: a hardened agent-repl panel is STRONGLY dedicated, which blocks
both the deletion and the buffer switch that stands in for it.

The minibuffer window is never a valid target and is neutralized-by-doing
nothing rather than touched — it holds no buffer a teardown owns.

Signals when WIN is undeletable and no live fallback buffer exists,
rather than silently leaving a retired buffer on screen."
  (cond
   ((not (window-live-p win)) nil)
   ((window-minibuffer-p win)
    (agent-repl--log-verbose ws "window--delete-or-neutralize: skip-minibuffer window=%S" win)
    nil)
   (t
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-dedicated-p win nil)
    (if (eq (window-deletable-p win) t)
        (progn
          (agent-repl--log ws "window--delete-or-neutralize: deleting window=%S buffer=%s"
                           win (agent-repl--safe-buffer-name (window-buffer win)))
          (delete-window win)
          'deleted)
      (let ((fb (or fallback
                    (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))))
        (agent-repl--log ws
                         "window--delete-or-neutralize: window=%S undeletable (deletable-p=%S) — switching buffer=%s to fallback=%s"
                         win (window-deletable-p win)
                         (agent-repl--safe-buffer-name (window-buffer win))
                         (agent-repl--safe-buffer-name fb))
        (unless (and fb (buffer-live-p fb))
          ;; No buffer to neutralize the window with: fail loudly rather than
          ;; leave a retired workspace's panel displayed.
          (error "agent-repl-window--delete-or-neutralize: no fallback buffer for undeletable window %s"
                 win))
        (set-window-buffer win fb)
        'neutralized)))))

(defun agent-repl--safe-buffer-name (b)
  "Return the name of buffer B if non-nil, otherwise nil.
Lives here rather than in panels.el because window.el is the lower of the
two layers that label buffers in their records, and both call this."
  (and b (buffer-name b)))

;;;; --- Subset deletion ---------------------------------------------------

(cl-defun agent-repl-window--delete-where
    (predicate &key (skip-side-windows t) frame ws)
  "Delete each live window for which PREDICATE returns non-nil.

PREDICATE is called with the window as its single argument.  Returns
the list of windows that were deleted (useful for callers wanting to
verify the sweep took effect).

The minibuffer window is ALWAYS preserved and never passed to
PREDICATE: it is undeletable and only appears in `window-list'
transiently while a minibuffer is active.

When SKIP-SIDE-WINDOWS is non-nil (the default), windows with a
non-nil `window-side' parameter are unconditionally preserved
regardless of PREDICATE.  This is the side-window-aware default that
prevents layout-clearing commands from destroying frame-level
side-window UI elements.  Pass nil ONLY when the caller genuinely
wants to clear side windows too (e.g. a full-frame fullscreen toggle
that should reduce to a single panel).

FRAME limits the scan; defaults to the selected frame.  WS is used only for
workspace-scoped diagnostics; when omitted the current workspace is used.
Deletion is
wrapped in `condition-case' so one undeletable window doesn't abort
the sweep — the error is logged via `agent-repl--warn' and iteration
continues.

When the caller knows the target buffer ahead of time, prefer
`agent-repl-window--delete-buffer-windows' — it's both simpler at
the call site and intentionally bypasses the side-window skip
(because targeting a specific buffer means the caller has already
opted into specificity)."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (windows (window-list frame))
         (deleted '()))
    (agent-repl--log resolved-ws
                     "window--delete-where: begin frame=%S skip-side-windows=%S windows=%d"
                     frame skip-side-windows (length windows))
    (dolist (win windows)
      (cond
       ((not (window-live-p win))
        (agent-repl--log-verbose resolved-ws
                                 "window--delete-where: skip-dead window=%S" win))
       ;; Never sweep the minibuffer window.  `window-list' includes it
       ;; whenever a minibuffer is active, and this sweep can run
       ;; mid-minibuffer — the debounced `on-window-change' idle timer fires
       ;; while the `SPC p p' picker is open.  `delete-window' always refuses
       ;; the minibuffer ("Attempt to delete minibuffer or sole ordinary
       ;; window"), so it is never a valid layout-sweep target; excluding it
       ;; here protects every caller and predicate rather than relying on
       ;; each one to filter it.
       ((window-minibuffer-p win)
        (agent-repl--log-verbose resolved-ws
                                 "window--delete-where: skip-minibuffer window=%S" win))
       ((and skip-side-windows
             (agent-repl-window--side-window-p win resolved-ws))
        (agent-repl--log-verbose resolved-ws
                                 "window--delete-where: skip-side window=%S" win))
       ((not (funcall predicate win))
        (agent-repl--log-verbose resolved-ws
                                 "window--delete-where: predicate=false window=%S" win))
       (t
        (agent-repl--log-verbose resolved-ws
                                 "window--delete-where: predicate=true deleting window=%S" win)
        (condition-case err
            (progn
              (delete-window win)
              (push win deleted)
              (agent-repl--log resolved-ws
                               "window--delete-where: deleted window=%S" win))
          (error
           ;; Three structural delete-window errors are benign here —
           ;; they fire when prior iterations have already collapsed the
           ;; tree such that this window is the sole remaining
           ;; main/side/frame window.  The sweep's job is best-effort
           ;; trimming, not strict deletion, so swallow these quietly
           ;; instead of dumping a scary [agent-repl] error into
           ;; *Messages* on every `SPC w f' that lands in this shape
           ;; (the original `SPC w f' regression).  Any other failure
           ;; is still surfaced.
           (if (agent-repl-window--benign-undeletable-error-p err resolved-ws)
               (agent-repl--log resolved-ws
                                "window--delete-where: skip-undeletable %s: %S" win err)
             (agent-repl--warn resolved-ws
                               "window--delete-where: could not delete %s: %S"
                               win err)))))))
    (setq deleted (nreverse deleted))
    (agent-repl--log resolved-ws
                     "window--delete-where: complete deleted-count=%d deleted=%S"
                     (length deleted) deleted)
    deleted))

(defun agent-repl-window--benign-undeletable-error-p (err &optional ws)
  "Return non-nil when ERR is one of `delete-window's structural refusals.
Matches the three error strings Emacs raises when a window is the sole
remaining main/side/ordinary window of its frame — these are expected
mid-sweep outcomes and should not be reported as failures.  WS is used only
for diagnostics."
  (let ((benign
         (and (consp err)
              (eq (car err) 'error)
              (stringp (cadr err))
              (or (string-prefix-p "Attempt to delete main window of frame" (cadr err))
                  (string-prefix-p "Attempt to delete sole side window of frame" (cadr err))
                  (string-prefix-p "Attempt to delete sole ordinary window of frame" (cadr err))))))
    (agent-repl--log-verbose ws
                             "window--benign-undeletable-error-p: err=%S benign=%s"
                             err benign)
    benign))

(cl-defun agent-repl-window--delete-buffer-windows
    (buf &key (all-frames t) ws)
  "Delete every live window currently displaying BUF.

ALL-FRAMES has the same semantics as `get-buffer-window-list's
ALL-FRAMES argument — default `t' scans every frame; `nil' scans
only the selected frame; a frame value scans that frame's windows.
WS is used only for workspace-scoped diagnostics; when omitted the current
workspace is used.

This helper deliberately bypasses the side-window skip that
`--delete-where' applies, because the caller is targeting a specific
buffer — if BUF lives in a side window, that side window is the
precise target.

A nil BUF or a killed BUF is a no-op (returns nil).

A window that CANNOT be deleted — the frame's sole ordinary window, or
the minibuffer — is never asked to be.  Each target goes through
`agent-repl-window--delete-or-neutralize', which switches such a window
to the fallback buffer instead, so BUF stops being displayed either way.
That is the whole difference from the old shape, which called
`delete-window' unconditionally and logged the resulting \"Attempt to
delete minibuffer or sole ordinary window\" as a failure while leaving
the retired buffer on screen.

Remaining errors are still caught per window and logged via
`agent-repl--warn', so one problem window does not abort the sweep.
Returns the list of windows that were actually deleted; a neutralized
window is not in that list, because it is still a live window."
  (let* ((resolved-ws (or ws (agent-repl--ws-current-name)))
         (live (and buf (buffer-live-p buf)))
         (windows (and live (get-buffer-window-list buf nil all-frames)))
         (deleted '()))
    (agent-repl--log resolved-ws
                     "window--delete-buffer-windows: begin buffer=%S live=%s all-frames=%S candidates=%d"
                     buf live all-frames (length windows))
    (when live
      (dolist (win windows)
        (if (not (window-live-p win))
            (agent-repl--log-verbose resolved-ws
                                     "window--delete-buffer-windows: skip-dead window=%S" win)
          (agent-repl--log-verbose resolved-ws
                                   "window--delete-buffer-windows: retiring window=%S" win)
          (condition-case err
              (when (eq (agent-repl-window--delete-or-neutralize win nil resolved-ws)
                        'deleted)
                (push win deleted)
                (agent-repl--log resolved-ws
                                 "window--delete-buffer-windows: deleted window=%S" win))
            (error
             (agent-repl--warn resolved-ws
                               "window--delete-buffer-windows: could not retire %s: %S"
                               win err))))))
    (setq deleted (nreverse deleted))
    (agent-repl--log resolved-ws
                     "window--delete-buffer-windows: complete deleted-count=%d deleted=%S"
                     (length deleted) deleted)
    deleted))

;;;; --- Layout reconciliation -----------------------------------------------

(defun agent-repl-window--panels-restorable-p (ws)
  "Return non-nil when WS's panel pair can be remounted through its frontend.
The one hard requirement is a live VIEW buffer: the mount recipe
\(`agent-repl--frontend-display-webview') recreates a dead or nil input
buffer on the way in (`agent-repl--ensure-input-buffer'), but a dead
view buffer would send the show dispatch down the open path — booting a
webview (and possibly a daemon session), which no implicit restore path
may do.  Shared by every implicit restore path — the window-change
reconciler (`agent-repl-window--ensure-layout'), the persp-switch
restore (`agent-repl--ensure-own-panels-on-persp-switch'), and the
frame reclaim (`agent-repl--reclaim-frame-fullscreen') — so their
eligibility checks cannot drift."
  (let* ((view-buf (agent-repl-window--panel-buffer :view ws))
         (restorable (buffer-live-p view-buf)))
    (agent-repl--log-verbose ws
                             "window--panels-restorable-p: view-buffer=%S live=%s"
                             view-buf restorable)
    restorable))

(defvar agent-repl-window--ensure-layout-in-progress nil
  "Non-nil while `agent-repl-window--ensure-layout' dispatches a repair.
The repair remounts the two-panel layout, which re-fires
`window-configuration-change-hook'.  Pure window work cannot re-enter
the debounced handler mid-repair (timers never preempt running lisp),
but a frontend show path that yields to the timer queue (`sleep-for' /
`accept-process-output' while waiting on a daemon) would let a pending
window-change timer start a second repair under the first — this flag
makes that nested pass a no-op.")

(defun agent-repl-window--ensure-layout ()
  "Restore the current workspace's two-panel layout when half of it is missing.

The declarative target: a workspace presenting its agent panels shows
BOTH of them — the view filling the frame's main area with the input
panel split below it, the canonical recipe built by
`agent-repl--frontend-display-webview'.  Any window-configuration
change can knock one window of the pair off the frame (e.g. quitting a
fullscreen magit-status restores a saved configuration captured without
the input window).  Runs from the debounced
`window-configuration-change-hook' handler
\(`agent-repl--on-window-change'), diffs the frame against the target,
and repairs by re-dispatching the workspace's frontend show — the same
recipe every explicit panel-show uses, so the repaired layout cannot
drift from the canonical one.

The trigger is strictly \"exactly one of the two panel windows is
present\".  Everything else is an expected state and a no-op:

- BOTH present: the layout already conforms.
- NEITHER present: the workspace's panels are hidden or closed
  \(`agent-repl--hide-panels', the `SPC o c' / `SPC o C' paths); a
  repair here would resurrect a deliberately hidden layout.
- No current workspace: nothing to reconcile against.
- `agent-repl--eager-open-in-progress' set: a background workspace
  build is laying panels out itself; a concurrent repair would fight
  that mount.
- `agent-repl-window--ensure-layout-in-progress' set: a repair is
  already dispatching (see that variable).
- Input window present but the VIEW buffer dead: the surviving input
  window of a died/rebound webview.  Remounting the view would mean
  creating a webview (and possibly a daemon session) from a
  window-change hook; that state is instead healed by the next explicit
  show, whose mount deletes or reclaims the stale input window.

Only the CURRENT workspace is reconciled: other workspaces' drifted
panels are `agent-repl--sync-panels''s territory, and side windows
are neither counted as panels nor touched.

Returns non-nil when a repair was dispatched, nil on every no-op."
  (let ((ws (agent-repl--ws-current-name)))
    (cond
     ((not ws)
      (agent-repl--log-verbose nil
                               "window--ensure-layout: noop reason=no-current-workspace")
      nil)
     (agent-repl--eager-open-in-progress
      (agent-repl--log-verbose ws
                               "window--ensure-layout: noop reason=eager-open-in-progress")
      nil)
     (agent-repl-window--ensure-layout-in-progress
      (agent-repl--log-verbose ws
                               "window--ensure-layout: noop reason=repair-in-progress")
      nil)
     (t
      (let ((view-win (agent-repl-window--panel-window :view ws))
            (input-win (agent-repl-window--panel-window :input ws)))
        (cond
         ((not (xor view-win input-win))
          (agent-repl--log-verbose
           ws
           "window--ensure-layout: noop reason=panel-pair-complete view-window=%S input-window=%S"
           view-win input-win)
          nil)
         ((not (agent-repl-window--panels-restorable-p ws))
          (agent-repl--log ws
                           "window--ensure-layout: ws=%s input window present but view buffer dead — leaving for the next explicit show"
                           ws)
          nil)
         (t
          (agent-repl--log ws
                           "window--ensure-layout: ws=%s missing=%s — remounting panels through the frontend"
                           ws (if view-win "input" "view"))
          (let ((agent-repl-window--ensure-layout-in-progress t))
            (agent-repl--frontend-dispatch-show ws))
          (agent-repl--log ws
                           "window--ensure-layout: repair-dispatched view-window=%S input-window=%S"
                           view-win input-win)
          t)))))))

(provide 'agent-repl-window)
;;; window.el ends here
