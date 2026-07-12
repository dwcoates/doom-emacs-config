;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

(defcustom agent-repl-input-height-fraction 0.23
  "Fraction of the vterm window's height allocated to the input panel."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-loading-placeholder-name " *agent-loading*"
  "Buffer name for the loading placeholder shown while the agent starts."
  :type 'string
  :group 'agent-repl)

(defcustom agent-repl-sigkill-delay 0.5
  "Seconds to wait before sending SIGKILL to a lingering agent process."
  :type 'number
  :group 'agent-repl)

(defcustom agent-repl-autoselect-input-on-workspace-switch t
  "When non-nil, auto-select the agent input window on workspace switch.
If the input panel is visible after switching to a workspace, the input
window is selected so the user can start typing immediately."
  :type 'boolean
  :group 'agent-repl)

(defcustom agent-repl-session-id-display-length 8
  "Number of characters of session ID to display in messages."
  :type 'integer
  :group 'agent-repl)

;;;; Panel visibility predicates

(defun agent-repl--ws-buffer-visible-p (key)
  "Return non-nil if the buffer stored at KEY in current workspace is visible."
  (let* ((buf (agent-repl--ws-get (agent-repl--ws-current-name) key))
         (result (and buf (buffer-live-p buf) (get-buffer-window buf))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
    result))

(defun agent-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (agent-repl--ws-buffer-visible-p :input-buffer))

(defun agent-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (agent-repl--ws-buffer-visible-p :vterm-buffer))

(defun agent-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (let ((result (and (agent-repl--input-visible-p)
                     (agent-repl--vterm-visible-p))))
    (agent-repl--log-verbose (agent-repl--ws-current-name) "panels-visible-p: result=%s" (if result "visible" "hidden"))
    result))

(defun agent-repl--output-visible-input-hidden-p ()
  "Return t when the output (vterm) panel is visible but the input is not.
This is the inconsistent half-shown state — e.g. a fullscreen frame
showing only the agent output window — that
`agent-repl--show-input-beside-output' repairs by adding the input
window alongside the existing output window."
  (let ((result (and (agent-repl--vterm-visible-p)
                     (not (agent-repl--input-visible-p)))))
    (agent-repl--log-verbose (agent-repl--ws-current-name)
                              "output-visible-input-hidden-p: result=%s"
                              (if result "yes" "no"))
    result))

;;;; Panel display and hide

(defun agent-repl--safe-buffer-name (b)
  "Return the name of buffer B if non-nil, otherwise nil."
  (and b (buffer-name b)))

(defun agent-repl--close-buffer-window (buf)
  "Close windows displaying BUF in the selected frame.
Delegates to `agent-repl-window--delete-buffer-windows' with
`:all-frames nil' to preserve the historical selected-frame-only
scope.  If a panel buffer is torn out to another frame, this
function leaves that frame's window alone — by design, since the
caller is doing a per-frame teardown."
  (agent-repl-window--delete-buffer-windows buf :all-frames nil))

(defun agent-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (agent-repl--log (agent-repl--ws-current-name) "close-buffer-windows %s" (mapcar #'agent-repl--safe-buffer-name bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (agent-repl--close-buffer-window buf))))

(defun agent-repl--configure-vterm-window (win)
  "Configure WIN as a dedicated, width-locked, protected vterm window.
Recipe (delegated to `agent-repl-window--harden'):

  • Dedicated — `display-buffer' can't repurpose the window.
  • `window-size-fixed: width' — prevents resize-triggered vterm reflow.
  • `no-delete-other-windows' — `delete-other-windows' can't kill it.

Keyboard-navigation isolation is handled dynamically by
`agent-repl--bounce-from-vterm' rather than by a static
`no-other-window' parameter — that way windmove/`other-window' can see
vterm, but any non-mouse selection gets auto-corrected back to the
input panel (or a warning if the input isn't displayed)."
  (agent-repl--log (agent-repl--ws-current-name) "configure-vterm-window: win=%s" win)
  (agent-repl-window--harden win
                              :dedicate       t
                              :size-fix       'width
                              :delete-protect t))

(defun agent-repl--ensure-input-buffer (ws)
  "Return a live input buffer for WS, adopting or (re)creating one if needed.

The panel-show path (`agent-repl--show-panels') builds the input
window by `split-window'-ing the output (vterm) window, so the new
window transiently inherits the vterm buffer until the input buffer is
set into it.  If WS's `:input-buffer' is dead or nil at that moment,
`set-window-buffer' errors and leaves the vterm buffer stranded in the
adjacent window — the duplicated-output corruption seen when switching
to a freshly generated workspace with the drawer open.  Guaranteeing a
live input buffer here keeps that reassignment from ever failing.

Resolution order, loud rather than silent about a missing buffer:
- the recorded `:input-buffer' when it is still live;
- else the canonically-named `*agent-panel-input-WS*' buffer when a
  live one already exists (re-adopting it without re-running
  `agent-repl-input-mode', which would trip its already-initialized guard);
- else a fresh buffer via `agent-repl--initialize-input-buffer'."
  (or (let ((buf (agent-repl--ws-get ws :input-buffer)))
        (and (buffer-live-p buf) buf))
      (let ((named (get-buffer (agent-repl--buffer-name "-input" ws))))
        (when (buffer-live-p named)
          (agent-repl--log ws "ensure-input-buffer: ws=%s adopting live named buffer %s"
                            ws (buffer-name named))
          (agent-repl--ws-put ws :input-buffer named)
          named))
      (progn
        (agent-repl--log ws "ensure-input-buffer: ws=%s input buffer dead/nil — recreating" ws)
        (agent-repl--initialize-input-buffer ws)
        (agent-repl--ws-get ws :input-buffer))))

;; Fullscreen window layout: vterm fills the frame's main area, input below it.
(defun agent-repl--show-panels ()
  "Display vterm and input panels filling the frame (fullscreen).
Saves the pre-panel window layout as WS's `:fullscreen-config' (so the
close path can restore the work windows the panels are about to cover),
then clears the frame's main area (deleting every non-side window via
`agent-repl--clear-main-area-for-panels') so the panels fill the
frame, then puts vterm in the surviving window and splits it bottom for
input (`agent-repl-input-height-fraction').  Fullscreen is the sole
display format — there is no splitscreen layout alongside a work window.
If the selected window is a side window (e.g. the drawer), redirects
to the frame's main window before clearing — side windows can't be
split, and we must never touch the drawer.

The `:fullscreen-config' save is guarded so it captures the layout only
on a genuine open (no panels visible and no config already saved): the
many re-show paths (workspace-switch reclaim, half-shown repair) call
through here too and must not clobber the saved work layout with a
panels-already-up one.

The input buffer is resolved through `agent-repl--ensure-input-buffer'
so it is always live before the split: the new input window inherits
the vterm buffer until it is reassigned, so a dead/nil input buffer
would otherwise leave the vterm duplicated in the adjacent window."
  (when (agent-repl-window--side-window-p (selected-window))
    (when-let ((main (and (fboundp 'window-main-window) (window-main-window))))
      (select-window main)))
  (let ((ws (agent-repl--ws-current-name)))
    (unless (or (agent-repl--ws-get ws :fullscreen-config)
                (agent-repl--panels-visible-p))
      (agent-repl--ws-put ws :fullscreen-config (current-window-configuration))))
  (agent-repl--clear-main-area-for-panels)
  (let* ((ws (agent-repl--ws-current-name))
         (vterm-buf (agent-repl--ws-get ws :vterm-buffer))
         ;; Guarantee a live input buffer BEFORE the split below: the new
         ;; input window inherits the vterm buffer until `set-window-buffer'
         ;; reassigns it, so a dead/nil input buffer would strand the vterm
         ;; duplicated in the adjacent window (see `--ensure-input-buffer').
         (input-buf (agent-repl--ensure-input-buffer ws)))
    (agent-repl--log ws "show-panels vterm=%s input=%s"
                      (agent-repl--safe-buffer-name vterm-buf)
                      (agent-repl--safe-buffer-name input-buf))
    (let* ((vterm-win (selected-window))
           (input-win (split-window vterm-win (round (* (- agent-repl-input-height-fraction) (window-total-height vterm-win))) 'below)))
      (agent-repl--log ws "show-panels: vterm-win=%s input-win=%s" vterm-win input-win)
      (agent-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (agent-repl--configure-vterm-window vterm-win)
      ;; Input window recipe: dedicated, height-locked, delete-protected,
      ;; AND height-preserved.  `window-size-fixed' alone is bypassed by
      ;; `window--resize-mini-window' (ignore=t), so a multi-line echo
      ;; area shrinks the input.  The preserved-size parameter is only
      ;; bypassed by ignore='preserved', so preserving here steers
      ;; mini-window shrink onto vterm/work-win instead.
      (agent-repl-window--harden input-win
                                  :dedicate       t
                                  :size-fix       'height
                                  :delete-protect t
                                  :preserve-size  'height)))
  ;; Event-driven (user just opened panels) → kick a fresh update pass
  ;; via the unguarded entrypoint, bypassing the 1Hz timer's in-flight
  ;; reentry guard.  See `--update-all-workspace-states-now' docstring.
  (agent-repl--update-all-workspace-states-now))

(defun agent-repl--focus-input-panel ()
  "Focus the input panel window.
Signals an error if the input buffer or its window cannot be found —
callers should ensure panels are displayed before calling this.
Buffer/window resolution delegates to
`agent-repl-window--panel-buffer' and `--panel-window'."
  (agent-repl--log (agent-repl--ws-current-name) "focus-input-panel")
  (let* ((ws (agent-repl--ws-current-name))
         (buf (agent-repl-window--panel-buffer :input ws)))
    (unless buf
      (error "agent-repl--focus-input-panel: no :input-buffer for workspace %s" ws))
    (let ((win (agent-repl-window--panel-window :input ws)))
      (unless win
        (error "agent-repl--focus-input-panel: input buffer %s is not displayed in any window"
               (buffer-name buf)))
      (select-window win))))

(defun agent-repl--show-panels-and-focus ()
  "Display both agent panels and focus the input panel.
Convenience wrapper combining `agent-repl--show-panels' and
`agent-repl--focus-input-panel'."
  (agent-repl--show-panels)
  (agent-repl--focus-input-panel))

(defun agent-repl--show-input-beside-output ()
  "Add the input panel below the already-visible output (vterm) window.

For the half-shown state where the output window is visible but the
input window is not (e.g. a fullscreen frame with only the output
window): splits the existing output window and displays the input
buffer beneath it, using the same normal below-split layout
`agent-repl--show-panels' applies.  Deliberately does NOT recreate
the output window or clear any other windows — only the missing input
window is added, alongside the output window already on screen.

Returns the new input window, or nil when the output window is not
visible or the input buffer is unavailable.  Hardens the input window
with the same recipe as `agent-repl--show-panels' (dedicated,
height-locked, delete-protected, height-preserved)."
  (let* ((ws (agent-repl--ws-current-name))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (vterm-win (agent-repl-window--panel-window :vterm ws)))
    (agent-repl--log ws "show-input-beside-output vterm-win=%s input-buf=%s"
                      vterm-win (agent-repl--safe-buffer-name input-buf))
    (if (not (and vterm-win input-buf (buffer-live-p input-buf)))
        (agent-repl--log ws "show-input-beside-output: no-op (vterm-win=%s input-live=%s)"
                          vterm-win (and input-buf (buffer-live-p input-buf)))
      (let ((input-win (split-window
                        vterm-win
                        (round (* (- agent-repl-input-height-fraction)
                                  (window-total-height vterm-win)))
                        'below)))
        (agent-repl--log ws "show-input-beside-output: input-win=%s" input-win)
        (set-window-buffer input-win input-buf)
        (agent-repl-window--harden input-win
                                    :dedicate       t
                                    :size-fix       'height
                                    :delete-protect t
                                    :preserve-size  'height)
        input-win))))

(defun agent-repl--ensure-input-beside-output ()
  "Repair a half-shown layout by adding the input window beside the output.
No-op unless the output (vterm) panel is visible while the input panel
is not (see `agent-repl--output-visible-input-hidden-p').  When that
state holds — including a fullscreen frame showing only the output
window after a workspace switch — `agent-repl--show-input-beside-output'
adds the input window alongside the existing output window."
  (when (agent-repl--output-visible-input-hidden-p)
    (agent-repl--log (agent-repl--ws-current-name)
                      "ensure-input-beside-output: repairing half-shown layout")
    (agent-repl--show-input-beside-output)))

;;;; Vterm refresh

(defun agent-repl--snap-vterm-window-to-cursor (win)
  "Set WIN's `window-start' so the buffer cursor lands on the last visible line.

Avoids the visible scroll-down animation that redisplay would otherwise
produce when point and the saved `window-start' are far apart — instead
of letting Emacs scroll line-by-line until the cursor is on screen, this
jumps `window-start' directly to a position that places the cursor at
the bottom of WIN's body, so the new view appears in a single redisplay
without intermediate scroll frames.

Uses the calling buffer's current `point' as the cursor — production
callers run this after `vterm-reset-cursor-point' so point already
matches vterm's prompt cursor.  Passes NOFORCE=t to `set-window-start'
so the chosen start sticks across the next redisplay cycle.  When the
buffer is shorter than WIN's body height, the backward-line walk caps
naturally at `point-min' (the `line-beginning-position' fallback), so
the entire buffer remains visible without forcing a scroll.

Selecting WIN to drive this via `recenter -1' would re-trigger
`window-selection-change-functions' (and the `bounce-from-vterm'
redirect), so the implementation deliberately works through
`set-window-start' + `set-window-point' alone."
  (let* ((cursor (point))
         (body-height (window-body-height win))
         (new-start (save-excursion
                      (goto-char cursor)
                      (forward-line (- 1 body-height))
                      (line-beginning-position))))
    (set-window-start win new-start t)
    (set-window-point win cursor)))

(defun agent-repl--vterm-redraw ()
  "Redraw the current vterm buffer with read-only suppressed.
Assumes the current buffer is in vterm-mode."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "vterm-redraw: buf=%s" (buffer-name))
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term))))

(defun agent-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "do-refresh: buf=%s" (buffer-name))
  (agent-repl--vterm-redraw)
  (redisplay t))

(defun agent-repl--fix-vterm-scroll (buf)
  "Snap the vterm window for BUF to its cursor without a visible scroll.

Replaces the previous brief-select hack: instead of momentarily
selecting BUF's window so vterm's selection-change side effect scrolls
the display to the cursor, this resets the buffer cursor explicitly
and jumps `window-start' so the cursor lands on the last visible line
in a single redisplay step (see `agent-repl--snap-vterm-window-to-cursor').

No-op when BUF is dead, has no displayed window, or its window is the
currently selected one — reading/copying flows preserve the user's
manual scroll position in the selected window."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (if (and vterm-win (not (eq vterm-win orig-win)))
        (progn
          (agent-repl--log-verbose (agent-repl--ws-current-name) "fix-vterm-scroll: snapping buf=%s" (buffer-name buf))
          (with-current-buffer buf
            (when (and (eq major-mode 'vterm-mode)
                       (fboundp 'vterm-reset-cursor-point))
              (condition-case nil (vterm-reset-cursor-point) (end-of-buffer nil)))
            (agent-repl--snap-vterm-window-to-cursor vterm-win)))
      (agent-repl--log-verbose (agent-repl--ws-current-name) "fix-vterm-scroll: skipped buf=%s vterm-win=%s same-win=%s"
                                (buffer-name buf) (if vterm-win "yes" "no")
                                (if (eq vterm-win orig-win) "yes" "no")))))

(defun agent-repl--resolve-vterm-buffer ()
  "Return the vterm buffer to refresh.
Uses the current buffer if it is in vterm-mode, otherwise looks up the
workspace's vterm buffer."
  (if (eq major-mode 'vterm-mode)
      (progn
        (agent-repl--log-verbose (agent-repl--ws-current-name) "resolve-vterm-buffer: path=vterm-mode buf=%s" (buffer-name))
        (current-buffer))
    (when-let ((ws (agent-repl--ws-current-name)))
      (let ((buf (agent-repl--ws-get ws :vterm-buffer)))
        (agent-repl--log-verbose (agent-repl--ws-current-name) "resolve-vterm-buffer: path=workspace-lookup ws=%s buf=%s"
                                  ws (agent-repl--safe-buffer-name buf))
        buf))))

(defun agent-repl--refresh-vterm ()
  "Refresh the agent vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (agent-repl--resolve-vterm-buffer)))
    (cond
     ((not buf)
      (agent-repl--log-verbose (agent-repl--ws-current-name) "refresh-vterm: no buffer found"))
     ((not (buffer-live-p buf))
      (agent-repl--log-verbose (agent-repl--ws-current-name) "refresh-vterm: buffer not live buf=%s" (buffer-name buf)))
     (t
      (with-current-buffer buf
        (if (eq major-mode 'vterm-mode)
            (agent-repl--do-refresh)
          (agent-repl--log-verbose (agent-repl--ws-current-name) "refresh-vterm: buf=%s not vterm-mode (mode=%s)"
                                    (buffer-name buf) major-mode)))
      (agent-repl--fix-vterm-scroll buf)))))

(defun agent-repl--drain-pending-show-panels (ws)
  "Show WS's session if a preemptive prompt queued a :pending-show-panels flag.
When the agent is ready, clears the flag and shows the session THROUGH
WS'S FRONTEND — the vterm panels for a vterm workspace, the webview for
a gui one.  A generated workspace is born with this flag set and its
session booted headlessly (`agent-repl--frontend-boot-session'), so this
drain is where a gui workspace first becomes visible; routing it
straight to `--show-hidden-panels' would look for vterm panels that a
gui workspace does not have.

When the agent is still starting, leaves the flag set so
`on-session-start-event' can re-drain via `open-panels-after-ready' once
ready — avoids displaying an unloaded vterm window.  The starting check
is vterm-shaped (a launched process that has not signalled ready); a gui
workspace has no such window to display prematurely, since its webview
attaches to the daemon session and streams whenever the agent gets
there."
  (cond
   ((not (agent-repl--ws-get ws :pending-show-panels))
    (agent-repl--log-verbose ws "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws))
   ((agent-repl--session-starting-p ws)
    (agent-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending session-starting — deferring" ws))
   (t
    (agent-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending draining frontend=%s"
                      ws (agent-repl--ws-frontend-name ws))
    (agent-repl--ws-put ws :pending-show-panels nil)
    (agent-repl--frontend-dispatch-show ws))))

(defun agent-repl--drain-pending-magit (ws)
  "Open `magit-status' for WS if it was created with `:pending-magit' set.
Reads the worktree path from `:project-dir', clears the flag, and removes
the Doom dashboard so magit is the sole main buffer in the new workspace.

When WS is also about to show its agent panels (`:pending-show-panels'
still set — this drain runs before that one in
`agent-repl--on-workspace-switch'), the magit buffer is created WITHOUT
a window (`save-window-excursion'): the panels open filling the frame
as the sole main-area display, so a magit window would only flash and
be wiped (vterm) or linger beside the panels (gui) — the
extra-windows-on-first-switch bug.  A workspace with no pending panel
show (the no-agent `SPC TAB n' path) still displays magit as before."
  (if (agent-repl--ws-get ws :pending-magit)
      (let ((path (agent-repl--ws-get ws :project-dir))
            (windowless (and (agent-repl--ws-get ws :pending-show-panels) t)))
        (agent-repl--log ws "drain-pending-magit: ws=%s branch=had-pending path=%s windowless=%s draining"
                          ws path windowless)
        (agent-repl--ws-put ws :pending-magit nil)
        (when path
          (if windowless
              (save-window-excursion (magit-status path))
            (magit-status path))
          (agent-repl--remove-doom-dashboard)))
    (agent-repl--log-verbose ws "drain-pending-magit: ws=%s branch=no-pending no-op" ws)))

(defun agent-repl--drain-pending-initial-buffers (ws)
  "Open configured initial buffers for WS if `:pending-initial-buffers' is set.
Reads the worktree path from `:project-dir' and clears the flag.  Deferred
from `finalize-worktree-workspace' so `find-file-noselect' runs while WS is
the current perspective, preventing the opened buffers from leaking into
the caller's workspace."
  (if (agent-repl--ws-get ws :pending-initial-buffers)
      (let ((path (agent-repl--ws-get ws :project-dir)))
        (agent-repl--log ws "drain-pending-initial-buffers: ws=%s branch=had-pending path=%s draining" ws path)
        (agent-repl--ws-put ws :pending-initial-buffers nil)
        (when path
          (agent-repl--open-initial-buffers ws path)))
    (agent-repl--log-verbose ws "drain-pending-initial-buffers: ws=%s branch=no-pending no-op" ws)))

;; Refresh vterm on workspace switch
(defun agent-repl--maybe-autoselect-input (ws)
  "Select the agent input window for WS if visible and autoselect is enabled.
Respects `agent-repl-autoselect-input-on-workspace-switch'.
Window lookup delegates to `agent-repl-window--panel-window'.

When the vterm output window is also visible, its display is snapped to
the cursor before the input window is selected, via
`agent-repl--snap-vterm-window-to-cursor'.  This replaces the old
brief-select hack (transiently selecting the vterm window so vterm's
selection-change side effect would recenter on the cursor), which
produced a visible scroll-down animation on workspace switch.  Jumping
`window-start' directly puts the cursor on the last visible line in a
single redisplay step — a snap, not a scroll."
  (when agent-repl-autoselect-input-on-workspace-switch
    (when-let ((win (agent-repl-window--panel-window :input ws)))
      (when-let ((vterm-win (agent-repl-window--panel-window :vterm ws)))
        (agent-repl--log ws "maybe-autoselect-input: snap-vterm via vterm-win=%s" vterm-win)
        (when-let ((vterm-buf (window-buffer vterm-win)))
          (when (buffer-live-p vterm-buf)
            (with-current-buffer vterm-buf
              (when (and (eq major-mode 'vterm-mode)
                         (fboundp 'vterm-reset-cursor-point))
                (condition-case nil (vterm-reset-cursor-point) (end-of-buffer nil)))
              (agent-repl--snap-vterm-window-to-cursor vterm-win)))))
      (agent-repl--log ws "maybe-autoselect-input: selecting input-win=%s" win)
      (select-window win))))

(defun agent-repl--stale-panel-windows ()
  "Return a list of windows showing agent panel buffers from a different workspace.
Each element is a window whose buffer is a agent panel (vterm or input) whose
workspace identifier (extracted from the buffer name) does not match the
currently active workspace.  Returns nil when all visible panels belong to the
current workspace or no panels are visible."
  (let* ((ws (agent-repl--ws-current-name))
         (sanitized (and ws (agent-repl--sanitize-ws-name ws))))
    (when sanitized
      (cl-loop for win in (window-list)
               for buf = (window-buffer win)
               for name = (buffer-name buf)
               for id = (agent-repl--extract-panel-id name)
               when (and id (not (string= id sanitized)))
               collect win))))

(defun agent-repl--stale-window-buffers (windows)
  "Return the unique live buffers displayed in WINDOWS.
Used to capture the foreign agent panel buffers occupying the stale
windows returned by `agent-repl--stale-panel-windows' BEFORE those
windows are deleted, so the buffers can be detached from the current
workspace's persp buffer list afterward.  Dead windows and nil buffers
are dropped."
  (delete-dups
   (delq nil
         (mapcar (lambda (w) (and (window-live-p w) (window-buffer w)))
                 windows))))

(defun agent-repl--lone-output-window ()
  "Return the sole non-side window when it shows a agent output buffer.

Returns the window displaying an agent vterm (output) buffer — of ANY
workspace, whether or not it belongs to the current one — when that
window is the only non-side window in the frame.  Returns nil otherwise.

This is the \"lone output\" state: the frame shows just an agent output
window (e.g. a fullscreen Agent REPL whose saved layout restored only
its output window, or another workspace's leftover output window) with
no input panel beside it.  Because the output window is the only
non-side window, the absence of a visible input panel is implied.

`agent-repl--ensure-own-panels-on-persp-switch' uses this to replace
such a lone output window with the switched-to workspace's own
output+input panels in fullscreen (via
`agent-repl--reclaim-frame-fullscreen')."
  (let ((non-side (cl-remove-if #'agent-repl-window--side-window-p (window-list))))
    (when (and (= (length non-side) 1)
               (agent-repl--agent-buffer-p (window-buffer (car non-side))))
      (car non-side))))

(defun agent-repl--detach-foreign-panel-buffers (ws buffers)
  "Detach foreign agent panel BUFFERS from WS's persp buffer list.
Each live buffer in BUFFERS is removed from the current workspace's
perspective via `agent-repl--ws-remove-buffer', so listing WS's buffers
no longer surfaces another workspace's agent panel.  The buffers are
NOT killed and remain attached to their home workspace.  No-op for nil
or dead buffers."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (agent-repl--log ws "detach-foreign-panel-buffers: removing %s from ws=%s buffer list"
                        (buffer-name buf) ws)
      (agent-repl--ws-remove-buffer buf))))

(defun agent-repl--safe-delete-window (win &optional fallback)
  "Delete WIN, or swap its buffer to FALLBACK when WIN cannot be deleted.
`delete-window' signals \"Attempt to delete sole window\" on a frame's
only deletable ordinary window (and would close the frame itself when
`window-deletable-p' reports `frame').  Unhandled inside the
`--on-workspace-switch' timer, that error aborts the rest of the switch
handler before `--reclaim-frame-fullscreen' runs, stranding the
previously selected workspace's lone agent output window on screen with
no input panel — the exact new-workspace bug this guards.  When
`window-deletable-p' does not report WIN as a deletable ordinary window
\(value `t'), this swaps WIN's buffer to FALLBACK (default
`doom-fallback-buffer') instead, so a stale foreign buffer is never left
displayed and the caller proceeds to reclaim the frame.  Un-dedicates
WIN and strips `no-delete-other-windows' first so a dedicated panel
window can be torn down.  No-op on a dead WIN.  Signals when WIN is
undeletable and no live fallback buffer exists, rather than silently
leaving the stale window in place."
  (when (window-live-p win)
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-dedicated-p win nil)
    (if (eq (window-deletable-p win) t)
        (progn
          (agent-repl--log (agent-repl--ws-current-name)
                            "safe-delete-window: deleting win=%s buf=%s"
                            win (agent-repl--safe-buffer-name (window-buffer win)))
          (delete-window win))
      (let ((fb (or fallback
                    (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))))
        (agent-repl--log (agent-repl--ws-current-name)
                          "safe-delete-window: win=%s undeletable (deletable-p=%S) — swapping buf=%s to fallback=%s"
                          win (window-deletable-p win)
                          (agent-repl--safe-buffer-name (window-buffer win))
                          (agent-repl--safe-buffer-name fb))
        (if (and fb (buffer-live-p fb))
            (set-window-buffer win fb)
          ;; No fallback buffer to neutralize the stale window with: fail
          ;; loudly rather than silently leave a foreign agent buffer
          ;; displayed in the new workspace's frame.
          (error "agent-repl--safe-delete-window: no fallback buffer to neutralize undeletable window %s" win))))))

(defun agent-repl--reclaim-frame-fullscreen (ws)
  "Take over the frame with WS's own agent panels (fullscreen).

Called after a workspace switch found the frame in a state that should be
replaced by WS's own panels, namely either:
- a *different* workspace's agent panel windows were purged, or
- a lone agent output window (see `agent-repl--lone-output-window')
  remained, whether or not it belonged to WS.

Reclaims through WS's own frontend: a gui workspace with a live
webview re-displays it via `agent-repl--frontend-dispatch-show' (the
webview + input layout, which clears the main area itself), a vterm
workspace shows its input+output panels via `agent-repl--show-panels'
\(which clears the frame's main area — deleting every non-panel
window, including any foreign output window left over from another
workspace — and lays out the panels filling the frame).

No-op when WS has no live view to reclaim the frame with (no live
webview for a gui workspace, no live panel buffers for a vterm one),
so the existing layout is left as-is."
  (cond
   ((and (agent-repl--ws-gui-frontend-p ws)
         (buffer-live-p (agent-repl--ws-get ws :frontend-buffer)))
    (agent-repl--log ws "reclaim-frame-fullscreen: showing gui view for ws=%s" ws)
    (agent-repl--frontend-dispatch-show ws))
   ((let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer))
          (input-buf (agent-repl--ws-get ws :input-buffer)))
      (and vterm-buf (buffer-live-p vterm-buf)
           input-buf (buffer-live-p input-buf)))
    (agent-repl--log ws "reclaim-frame-fullscreen: showing own panels for ws=%s" ws)
    (agent-repl--show-panels))
   (t
    (agent-repl--log ws "reclaim-frame-fullscreen: no live view for ws=%s, skipping" ws))))

(defun agent-repl--ensure-own-panels-on-persp-switch (ws)
  "Reconcile panel visibility with workspace ownership after a persp switch.

Closes any panel windows that belong to a *different* workspace —
persp-mode's `window-state-put' can leave stale panel windows when
the target workspace has no saved window config (first visit) or
when the saved config itself carried drifted panels from a prior
save.

When such foreign panels are found, also detaches their buffers from
THIS workspace's persp buffer list (via
`agent-repl--detach-foreign-panel-buffers') so listing this
workspace's buffers no longer surfaces another workspace's Claude
panel, and then takes over the frame with this workspace's own panels
in fullscreen (via `agent-repl--reclaim-frame-fullscreen').  The
foreign buffers are NOT killed and stay attached to their home
workspace.

Also handles a lone agent output window — the frame showing just a
single agent output (vterm) window with no input beside it, whether or
not that output belongs to the switched-to workspace (see
`agent-repl--lone-output-window').  Such a lone output is replaced by
THIS workspace's own output+input panels in fullscreen, again via
`agent-repl--reclaim-frame-fullscreen'.

After purging stale panels, restores this workspace's own panels if
they were visible when this workspace was last deactivated
\(`:panels-were-visible' flag set by `--before-persp-deactivate').

Mirrors the drawer's `ensure-visible-on-persp-switch' approach:
the drawer uses a global visibility flag; panels use a per-workspace
flag because each workspace has its own panel buffers."
  (let* ((stale (agent-repl--stale-panel-windows))
         (foreign-bufs (agent-repl--stale-window-buffers stale))
         ;; Captured BEFORE any repair below adds an input window beside a
         ;; lone output (which would make it no longer "lone").
         (lone-output (agent-repl--lone-output-window)))
    (agent-repl--log ws "ensure-own-panels: ws=%s stale=%d lone-output=%s panels-visible=%s windows=%d"
                      ws (length stale) (and lone-output t)
                      (agent-repl--panels-visible-p) (length (window-list)))
    (when stale
      (agent-repl--log ws "ensure-own-panels: closing %d stale panel windows: %S"
                        (length stale)
                        (mapcar (lambda (w) (buffer-name (window-buffer w))) stale))
      (dolist (win stale)
        ;; Sole-window-safe deletion: a stale window that is the frame's
        ;; only window cannot be `delete-window'-ed (it signals "Attempt to
        ;; delete sole window").  Left unguarded that error aborts this
        ;; timer-driven handler before the `--reclaim-frame-fullscreen' below
        ;; runs, leaving the previously selected workspace's lone output
        ;; window stranded with no input panel — the new-workspace bug.
        (agent-repl--safe-delete-window win))
      ;; Detach the foreign panel buffers from THIS workspace's persp
      ;; buffer list AFTER their windows are gone, so listing this
      ;; workspace's buffers no longer surfaces another workspace's
      ;; agent panel.  The buffers stay alive in their home workspace.
      (agent-repl--detach-foreign-panel-buffers ws foreign-bufs))
    ;; If this workspace's panels were visible before its last deactivation
    ;; but are not visible now (persp dropped them or we just purged stale
    ;; ones), re-show them.
    (when (and (agent-repl--ws-get ws :panels-were-visible)
               (not (agent-repl--panels-visible-p))
               ;; Only restore if this ws actually has live panel buffers.
               (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer))
                     (input-buf (agent-repl--ws-get ws :input-buffer)))
                 (and vterm-buf (buffer-live-p vterm-buf)
                      input-buf (buffer-live-p input-buf))))
      (if (agent-repl--vterm-visible-p)
          ;; Output window survived the switch but the input window was
          ;; dropped — add only the input window so the output window is
          ;; not duplicated.
          (progn
            (agent-repl--log ws "ensure-own-panels: output up, input missing — adding input only")
            (agent-repl--show-input-beside-output))
        (agent-repl--log ws "ensure-own-panels: re-showing panels (were-visible but now missing)")
        (agent-repl--show-panels)))
    ;; Independently of the were-visible flag, repair a frame that shows
    ;; only the output window (e.g. a fullscreen Agent REPL restored with
    ;; just its output window) by adding the input window beside it.
    (agent-repl--ensure-input-beside-output)
    ;; Take over the frame with THIS workspace's own panels in fullscreen —
    ;; replacing every visible window with the input+output panels — when
    ;; either a foreign workspace's panels were just purged, or the frame
    ;; showed a lone agent output window (own or foreign).  A single
    ;; reclaim call covers both, so a foreign lone output (which is BOTH
    ;; stale and lone) is reclaimed exactly once.
    (when (or stale lone-output)
      (agent-repl--log ws "ensure-own-panels: reclaiming frame stale=%s lone-output=%s"
                        (and stale t) (and lone-output t))
      (agent-repl--reclaim-frame-fullscreen ws))))

(defun agent-repl--on-workspace-switch (&optional ws)
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors.
WS is the workspace name to operate on; when nil, falls back to
`(agent-repl--ws-current-name)' at call time.  Callers from
`--after-persp-activated' pass the ws captured at hook-fire time so
the deferred call operates on the workspace that was just switched
to, even if another switch raced ahead before the timer fired.

Also opens panels for workspaces that were created with a preemptive
prompt, and auto-selects the input window if visible.

If the newly-active workspace has `:agent-state :done', stamps
`:done-acked' to t and `:done-acked-at' to the current time so the
decay timer can clear :done → :idle once
`agent-repl-done-idle-delay' seconds of continuous focus have
elapsed.  The companion clear in `--before-persp-deactivate' resets
the timestamp on switch-away, so a quick transit through the tab
never causes a silent decay.

Also runs `agent-repl--maybe-sweep-hidden-on-switch' so workspaces
marked `:hidden' (via `SPC o C') are persp-killed when hide-mode is on
— the persp-level enforcement of hide-mode.

Runs `agent-repl--dequeue-merge' so a workspace parked in the merge
queue is pulled from it on switch — activating a queued workspace is
read as the user wanting to work on it directly rather than have its
pending merge auto-fire."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log-verbose ws "workspace-switch ws=%s" ws)
    ;; Purge stale panel windows from other workspaces and restore own
    ;; panels if they were visible before this workspace was deactivated.
    ;; Must run BEFORE refresh-vterm / autoselect so they see the correct
    ;; panel windows.
    (agent-repl--ensure-own-panels-on-persp-switch ws)
    (when (eq (agent-repl--ws-agent-state ws) :done)
      (agent-repl--ws-put ws :done-acked t)
      (agent-repl--ws-put ws :done-acked-at (float-time)))
    (agent-repl--maybe-sweep-hidden-on-switch ws)
    (agent-repl--dequeue-merge ws)
    ;; Event-driven (workspace just activated) → kick a fresh pass via
    ;; the unguarded entrypoint so the in-flight reentry guard from the
    ;; 1Hz timer doesn't swallow the switch's refresh.
    (agent-repl--update-all-workspace-states-now)
    (agent-repl--refresh-vterm)
    (agent-repl--reset-vterm-cursors)
    (agent-repl--drain-pending-magit ws)
    (agent-repl--drain-pending-initial-buffers ws)
    (agent-repl--drain-pending-show-panels ws)
    (agent-repl--maybe-autoselect-input ws)
    ;; Flip the emacs-side bit on the fully-loaded latch.  If
    ;; --on-session-start-event has also fired, this fires the
    ;; ws-fully-loaded hook; otherwise we just record the bit and wait
    ;; for agent-ready.  Guarded on ws non-nil so the nil-ws fallback
    ;; (test envs, persp init) doesn't poison the hash table.
    (when ws
      (agent-repl--latch-and-maybe-fire-loaded ws :ws-loaded))))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.

(defun agent-repl--non-agent-panel-window-p (w)
  "Return non-nil if window W does not display a agent panel buffer."
  (not (agent-repl--agent-panel-buffer-p (window-buffer w))))

(defun agent-repl--save-target-window-p (w)
  "Return non-nil when W is a safe selected-window for persp save.

\"Safe\" means a later `switch-to-buffer' (e.g. Doom's `+workspace/kill'
fallback path) can repurpose the window in-place rather than splitting
a new one.  Excludes agent panel buffers, side windows, dedicated
windows, and the minibuffer."
  (and (window-live-p w)
       (not (window-minibuffer-p w))
       (not (window-parameter w 'window-side))
       (not (window-dedicated-p w))
       (not (agent-repl--agent-panel-buffer-p (window-buffer w)))))

(defun agent-repl--redirect-from-agent-before-save ()
  "Select a redirect-safe window before persp saves window state.

Redirects when the selected window is unsuitable as a future
`switch-to-buffer' target: a agent panel buffer, a side window
(e.g. the drawer), or a dedicated window.  Persp saves the selected
window into the workspace's restored layout; if that window is a
side/dedicated/panel window, Doom's `+workspace/kill' fallback
`(switch-to-buffer (doom-fallback-buffer))' cannot repurpose it and
instead splits a new window showing the doom splash buffer.

Picks the first window that satisfies `agent-repl--save-target-window-p'.
No-op when no safe target exists (fullscreen-agent or drawer-only)."
  (let ((sel (selected-window)))
    (when (or (agent-repl--agent-panel-buffer-p (window-buffer sel))
              (window-parameter sel 'window-side)
              (window-dedicated-p sel))
      (when-let ((target (cl-find-if
                          #'agent-repl--save-target-window-p
                          (window-list))))
        (select-window target)))))

(defun agent-repl--clear-done-ack-on-switch-away (ws)
  "Reset WS's :done focus-dwell tracking when switching away.
If WS is in `:agent-state :done' and the decay has not yet fired,
clear `:done-acked' and `:done-acked-at' so the dwell countdown
restarts on the next return.  This makes the
`agent-repl-done-idle-delay' check count only continuous focus
periods — a quick transit (< delay) leaves the workspace green."
  (when (and ws (eq (agent-repl--ws-agent-state ws) :done))
    (agent-repl--log ws "clear-done-ack-on-switch-away ws=%s" ws)
    (agent-repl--ws-put ws :done-acked nil)
    (agent-repl--ws-put ws :done-acked-at nil)))

(defun agent-repl--before-persp-deactivate (&rest _)
  "Save window state before perspective deactivation.
Redirects away from agent buffers and saves frame state.  Also
records `:panels-were-visible' so `--ensure-own-panels-on-persp-switch'
can restore the correct workspace's panels after activation.
Clears the `:done' focus-dwell tracking on the outgoing workspace so
a sub-`agent-repl-done-idle-delay' transit never decays it.
Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "before-persp-deactivate: entry cache=%S"
                      (or (agent-repl--ws-names-cache) "(unbound)"))
    (agent-repl--clear-done-ack-on-switch-away ws)
    ;; Record whether panels are visible BEFORE redirecting/saving so
    ;; the activated hook can restore them if persp-mode drops them.
    (agent-repl--ws-put ws :panels-were-visible (agent-repl--panels-visible-p))
    (agent-repl--redirect-from-agent-before-save)
    (condition-case err
        (agent-repl--ws-frame-save-state)
      (error (agent-repl--warn ws "persp-frame-save-state failed: %S" err)
             (agent-repl--log ws "before-persp-deactivate: persp-frame-save-state error: %S" err)))))

(defun agent-repl--after-persp-activated (&rest _)
  "Handle perspective activation by scheduling a workspace switch.
Captures `(agent-repl--ws-current-name)' at hook-fire time and passes it
to the deferred `--on-workspace-switch' so the call operates on the
workspace that just activated, not whatever happens to be current
when the run-at-time-0 timer eventually fires (rapid back-to-back
switches would otherwise have every deferred call resolve to the
latest ws, dropping bookkeeping on the intermediate ones).

Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (agent-repl--log (agent-repl--ws-current-name) "after-persp-activated: entry cache=%S"
                    (or (agent-repl--ws-names-cache) "(unbound)"))
  (let ((ws (agent-repl--ws-current-name)))
    (run-at-time 0 nil #'agent-repl--on-workspace-switch ws)))

(when (modulep! :ui workspaces)
  (agent-repl--ws-add-before-deactivate-hook #'agent-repl--before-persp-deactivate)
  (agent-repl--ws-add-activated-hook #'agent-repl--after-persp-activated))

(defun agent-repl--hide-panels ()
  "Hide both agent panels without killing buffers."
  (let* ((ws (agent-repl--ws-current-name))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
    (agent-repl--log ws "hide-panels")
    (agent-repl--close-buffer-windows input-buf vterm-buf)))

(defun agent-repl--save-tab-index (ws)
  "Persist WS's current tab-bar index to its plist as `:saved-tab-index'.
Read on reopen by `agent-repl--restore-tab-index' so the workspace
returns to its prior slot in the tab-bar after a close-deprio cycle.
Reads positions from `persp-names-current-frame-fast-ordered'; no-op
when that helper is unavailable (e.g. test envs without persp-mode).

Also writes the index to disk via `--state-save' so a deprioritized ws
that the user closes Emacs on still returns to its saved slot on
restart (without this, `:saved-tab-index' is in-memory only and the ws
loses its prior position across an Emacs lifecycle)."
  (when-let ((idx (cl-position ws (agent-repl--ws-frame-ordered-names)
                               :test #'string=)))
    (agent-repl--log ws "save-tab-index ws=%s index=%d" ws idx)
    (agent-repl--ws-put ws :saved-tab-index idx)
    (agent-repl--state-save ws)))

(defun agent-repl--restore-tab-index (ws)
  "Move WS back to its persisted `:saved-tab-index' slot, if any.
Clears `:saved-tab-index' after restoring so each close-deprio cycle
captures a fresh baseline.  No-op when no index is saved or when persp
helpers are unavailable.  Index is clamped to the current names list
length so a saved index past the new tail is handled gracefully.

Drives `agent-repl--force-tab-bar-redraw' so the trailing-space
toggle is flipped — the tab-bar's string-equality cache otherwise
risks holding the pre-restore order if the new tabline string happens
to compare equal under propertized-string semantics."
  (when-let ((idx (agent-repl--ws-get ws :saved-tab-index)))
    (when-let ((names (agent-repl--ws-frame-ordered-names)))
      (let* ((without-ws (remove ws names))
             (clamped (min idx (length without-ws)))
             (head (cl-subseq without-ws 0 clamped))
             (tail (cl-subseq without-ws clamped))
             (reordered (append head (list ws) tail)))
        (agent-repl--log ws "restore-tab-index ws=%s saved-idx=%d clamped=%d"
                          ws idx clamped)
        (agent-repl--ws-update-names-cache reordered)
        (agent-repl--ws-put ws :saved-tab-index nil)
        ;; Persist the cleared index so a future restart doesn't see a
        ;; stale value and re-restore (the ws is no longer deprioritized
        ;; once we've reseated it).
        (agent-repl--state-save ws)
        (when (fboundp 'agent-repl--force-tab-bar-redraw)
          (agent-repl--force-tab-bar-redraw))))))

(defun agent-repl--workspace-magit-status-buffer (ws)
  "Return an existing `magit-status-mode' buffer for workspace WS, or nil.
Matches a live buffer whose `major-mode' derives from `magit-status-mode'
and whose `default-directory' canonicalizes to the same git root as WS's
project directory.  Never creates a buffer — it only locates one already
available for the workspace, so the `SPC o c' fallback
\(`agent-repl--panel-fallback-buffer') can prefer an existing
magit-status buffer over the Doom splash."
  (when-let* ((dir (ignore-errors (agent-repl--ws-dir ws)))
              (root (agent-repl--git-root dir)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'magit-status-mode)
              (equal (agent-repl--path-canonical default-directory) root))))
     (buffer-list))))

(defun agent-repl--panel-fallback-buffer (ws)
  "Return the buffer to display when `SPC o c' clears WS's sole panels.
Prefers an existing workspace magit-status buffer
\(`agent-repl--workspace-magit-status-buffer'); when none exists, falls
back to the Doom splash (`doom-fallback-buffer').  Signals when neither
is available rather than returning nil, so the caller never swaps a
panel window to a dead buffer (see the fail-hard invariant)."
  (or (agent-repl--workspace-magit-status-buffer ws)
      (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer))
      (error "agent-repl--panel-fallback-buffer: no magit-status or doom-fallback buffer for ws=%s" ws)))

(defun agent-repl--replace-panels-with-fallback (ws)
  "Replace WS's frame-filling panels with a single fallback buffer.
The `SPC o c' path calls this when the panels are the only windows on
screen (no saved `:fullscreen-config' to restore to): rather than
stranding the lone output window, closes the input window and swaps the
output window's buffer to WS's magit-status buffer when one already
exists, else the Doom splash (see `agent-repl--panel-fallback-buffer').
Routes the output-window swap through `agent-repl--safe-delete-window'
so the dedicated vterm window is un-dedicated before the buffer swap."
  (let ((input-buf (agent-repl--ws-get ws :input-buffer))
        (vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
    (agent-repl--log ws "replace-panels-with-fallback ws=%s" ws)
    (when input-buf
      (agent-repl--close-buffer-window input-buf))
    (when-let ((vterm-win (and vterm-buf (get-buffer-window vterm-buf))))
      (agent-repl--safe-delete-window
       vterm-win (agent-repl--panel-fallback-buffer ws)))))

(defun agent-repl--on-simple-close (&optional ws)
  "Bookkeep + hide panels; do NOT touch tab-bar order.
Sets `:repl-state :inactive' on WS (`:agent-state' untouched so an
in-flight :thinking / :permission survives the close), then hides
the panel windows.  No save-tab-index, no push-to-back, no flash —
this is the simple-close audit point that `SPC o c' is bound to.

The panels fill the frame (fullscreen is the only display format).
When a pre-panel layout was captured at open time, restores it via
`agent-repl--restore-fullscreen-config' — re-establishing the work
windows that were on screen before the panels took over — then
`agent-repl--hide-panels' leaves those work windows behind rather than
hitting `delete-window's sole-ordinary-window refusal and stranding a
panel onscreen.

When there is NO saved layout to restore (the panels were the only
windows on screen), replaces both panels with a single fallback buffer
via `agent-repl--replace-panels-with-fallback' — the workspace's
magit-status buffer when one already exists, else the Doom splash —
rather than stranding the output window."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log ws "on-simple-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (agent-repl--log ws "on-simple-close ws=%s agent-state=%s -> repl-state=:inactive"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-set-repl-state ws :inactive))
    (if (agent-repl--restore-fullscreen-config ws)
        (agent-repl--hide-panels)
      (agent-repl--replace-panels-with-fallback ws))))

(defun agent-repl--on-close (&optional ws)
  "Full close: bookkeep + restore pre-panel layout + hide + deprio + save tab index.
Sets WS's `:repl-state' to `:hidden' (NOT `:inactive' like the
simple-close path) so the workspace becomes a kill candidate for the
next sweep when `agent-repl-hide-mode-enabled' is on.  Restores the
pre-panel layout via `agent-repl--restore-fullscreen-config' before
hiding so the frame-filling panels go away cleanly (same contract as
`agent-repl--on-simple-close').  See
`agent-repl--ws-set-repl-state' for the `:hidden' contract.  Then
hides panels and pushes WS to the second-to-last tab position via
`agent-repl-workspace-push-to-back', snapshotting the tab index
first via `agent-repl--save-tab-index' so a future reopen can
restore the position.

Bound to `SPC o C' (the deprio toggle); also fires from
`agent-repl-send-and-hide' since send-and-hide is semantically
\"I'm done with this prompt, move on\".

WS defaults to the current workspace; when WS is nil the function still
hides panels but skips the bookkeeping write and the tab shuffle."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log ws "on-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (agent-repl--log ws "on-close ws=%s agent-state=%s -> repl-state=:hidden"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-set-repl-state ws :hidden))
    (agent-repl--restore-fullscreen-config ws)
    (agent-repl--hide-panels)
    (when (and ws (equal ws (agent-repl--ws-current-name)))
      (agent-repl--save-tab-index ws)
      (agent-repl--log ws "on-close: pushing ws=%s to second-to-last" ws)
      (agent-repl-workspace-push-to-back))))

(defun agent-repl--unhide-workspace (ws)
  "Reverse `agent-repl--on-close' for WS by setting `:repl-state' to `:active'.
A no-op when WS is nil or has a `:repl-state' other than `:hidden' —
non-hidden workspaces don't need unhiding, and overwriting
`:inactive' / `:dead' / nil with `:active' would lie about lifecycle
state.  Does NOT re-show panels or re-shuffle tab order; that side of
the close is reversible only by an explicit panel-show."
  (when ws
    (let ((rstate (agent-repl--ws-get ws :repl-state)))
      (agent-repl--log ws "unhide-workspace: ws=%s repl-state=%s" ws rstate)
      (when (eq rstate :hidden)
        (agent-repl--ws-set-repl-state ws :active)))))

;;;; Window synchronization

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun agent-repl--extract-panel-id (name)
  "Extract the workspace identifier from a agent panel buffer NAME.
Returns the identifier string, or nil if NAME is not a agent panel buffer.
Input-buffer check comes first since `agent-repl--vterm-buffer-re' is a
superset that also matches input-buffer names."
  (cond
   ((string-match-p agent-repl--input-buffer-re name)
    (substring name (length "*agent-panel-input-") (- (length name) (length "*"))))
   ((string-match-p agent-repl--vterm-buffer-re name)
    (substring name (length "*agent-panel-") (- (length name) (length "*"))))))

(defun agent-repl--partner-buffer-name (name id)
  "Return the partner buffer name for agent panel NAME with identifier ID.
For a vterm buffer, the partner is the input buffer, and vice versa.
Checks input-re first since vterm-re is a superset that also matches inputs."
  (if (string-match-p agent-repl--input-buffer-re name)
      (format "*agent-panel-%s*" id)
    (format "*agent-panel-input-%s*" id)))

(defun agent-repl--orphaned-panel-p (name)
  "Return non-nil if NAME is a agent panel buffer whose partner is not visible.
Ignores single-window frames.  Input buffers are not orphaned while the
loading placeholder exists (the vterm has not been swapped in yet), nor
while the workspace's frontend WEBVIEW is visible — in the hybrid UI
the input panel's live partner is the webview, not a vterm."
  (when-let ((id (agent-repl--extract-panel-id name)))
    (let* ((is-input (string-match-p agent-repl--input-buffer-re name))
           (partner (agent-repl--partner-buffer-name name id))
           (result (and (not (one-window-p))
                        (not (get-buffer-window partner))
                        ;; Input panels are not orphaned while loading placeholder is live
                        (or (not is-input)
                            (and (not (get-buffer agent-repl-loading-placeholder-name))
                                 (not (get-buffer-window
                                       (agent-repl--frontend-webview-buffer-name id))))))))
      (when result
        (agent-repl--log-verbose (agent-repl--ws-current-name) "orphaned-panel-p: name=%s partner=%s is-orphaned" name partner))
      result)))

(defun agent-repl--sync-panels ()
  "Close any agent panel whose partner is no longer visible.
Side windows (e.g. the drawer) can never be agent panels by
predicate construction, so the default `--delete-where' side-skip
costs nothing and remains defense-in-depth.

Logs each orphan's buffer name BEFORE the sweep (capturing names
while windows are still live) so the per-orphan log survives the
deletion that follows."
  (let* ((ws (agent-repl--ws-current-name))
         (orphan-names
          (cl-loop for win in (window-list)
                   for name = (buffer-name (window-buffer win))
                   when (agent-repl--orphaned-panel-p name)
                   collect name)))
    (agent-repl--log-verbose ws "sync-panels: entry windows=%d"
                              (length (window-list)))
    (dolist (name orphan-names)
      (agent-repl--log ws "sync-panels closing orphaned %s" name))
    (let ((deleted
           (agent-repl-window--delete-where
            (lambda (win)
              (agent-repl--orphaned-panel-p
               (buffer-name (window-buffer win)))))))
      (agent-repl--log-verbose ws "sync-panels: closed %d orphans"
                                (length deleted)))))

;; Keep visible agent vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun agent-repl--refresh-vterm-window (win)
  "Refresh the agent vterm buffer shown in WIN.
Resets cursor, redraws, and snaps `window-start' so the cursor lands on
the last visible line — replaces the bare `set-window-point' tail with
`agent-repl--snap-vterm-window-to-cursor' so the new view appears in a
single redisplay rather than animating a scroll from the saved
`window-start' down to the cursor."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (agent-repl--agent-buffer-p buf))
      (agent-repl--log-verbose (agent-repl--ws-current-name) "refresh-vterm-window: win=%s buf=%s" win (buffer-name buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (condition-case nil
              (progn
                (vterm-reset-cursor-point)
                (agent-repl--vterm-redraw)
                (vterm-reset-cursor-point)
                (agent-repl--snap-vterm-window-to-cursor win))
            (end-of-buffer nil)))))))

(defun agent-repl--reset-vterm-cursors ()
  "Refresh every visible agent vterm window except the selected one."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "reset-vterm-cursors: entry")
  (let ((sel (selected-window)))
    (dolist (win (window-list))
      (unless (eq win sel)
        (agent-repl--refresh-vterm-window win)))))

(defvar agent-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defun agent-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels and refreshes overlay.

Does NOT reset vterm cursors.  `agent-repl--reset-vterm-cursors'
calls `vterm-reset-cursor-point' + `set-window-point' on every visible
non-selected agent vterm window; that pulls window-start back to the
bottom of the buffer, undoing any user scroll-up (e.g. via `C-S-k').
The reset is only useful right after a workspace switch (to recenter
the new vterm on its prompt), so it lives in
`agent-repl--on-workspace-switch' alone — not on every window-config
change, selection change, or buffer-list update."
  (agent-repl--log-verbose (agent-repl--ws-current-name) "on-window-change")
  (agent-repl--sync-panels)
  (agent-repl--update-hide-overlay))

(defmacro agent-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR.
Cancels any pending timer and schedules FN to run at next idle."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(defalias 'agent-repl--debounced-on-window-change
  (agent-repl--deferred agent-repl--sync-timer #'agent-repl--on-window-change)
  "Debounced handler for `window-configuration-change-hook'.
Cancels any pending timer and schedules `agent-repl--on-window-change'.")

(add-hook 'window-configuration-change-hook
          #'agent-repl--debounced-on-window-change)

;; Redirect keyboard navigation away from the vterm output window.
;; Mouse clicks (checked via last-input-event) are allowed through so the
;; user can still click into the output when needed.
(defun agent-repl--bounce-from-vterm (_frame)
  "If the selected window shows an agent vterm buffer, redirect to the input window.
Allows mouse-initiated selection through so clicking into the output to
scroll or copy works.  When no input window is currently displayed
\(e.g. panels are hidden), emits a warning via `message' rather than
leaving point stranded silently.

Predicate is buffer-identity (`agent-repl--agent-buffer-p' — vterm-only,
excludes input buffers) rather than the `no-other-window' parameter, so
this bounce alone is sufficient to keep keyboard nav out of vterm."
  (let ((win (selected-window)))
    (if (and (agent-repl--agent-buffer-p (window-buffer win))
             (not (mouse-event-p last-input-event)))
        (let* ((ws (agent-repl--ws-current-name))
               (input-buf (and ws (agent-repl--ws-get ws :input-buffer)))
               (input-win (and input-buf (get-buffer-window input-buf))))
          (if input-win
              (progn
                (agent-repl--log-verbose (agent-repl--ws-current-name) "bounce-from-vterm: bouncing to input-win=%s" input-win)
                (select-window input-win))
            (message "[agent-repl] keyboard navigation landed in agent vterm but input panel isn't visible — stuck here until you click out or reopen panels")
            (agent-repl--log (agent-repl--ws-current-name) "bounce-from-vterm: no input-win to bounce to (warned)")))
      (agent-repl--log-verbose (agent-repl--ws-current-name) "bounce-from-vterm: skipped vterm-buffer=%s mouse=%s"
                                (if (agent-repl--agent-buffer-p (window-buffer win)) "yes" "no")
                                (if (mouse-event-p last-input-event) "yes" "no")))))

(add-hook 'window-selection-change-functions #'agent-repl--bounce-from-vterm)

;;;; Buffer creation

(defun agent-repl--initialize-input-buffer (ws)
  "Create the agent input buffer for workspace WS and enable agent-repl-input-mode.
Errors if the buffer is already initialized (already in `agent-repl-input-mode')."
  (agent-repl--log ws "initialize-input-buffer")
  (let ((input-buf (agent-repl--create-buffer ws "-input")))
    (agent-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (when (eq major-mode 'agent-repl-input-mode)
        (error "agent-repl--initialize-input-buffer: already initialized ws=%s" ws))
      (agent-repl-input-mode)
      (agent-repl--history-restore ws))))

(defun agent-repl--kill-stale-vterm (&optional ws)
  "Kill any leftover agent vterm buffer for WS before a fresh launch.
Callers reach here only after the already-running guard passed, so an
existing buffer is a leftover by definition — dead-process buffers are
plain stale; a LIVE process means a zombie from a failed teardown
(e.g. an aborted frontend switch), which previously no-op'd here and
made the subsequent launch die on \"already initialized\" after an
interactive kill-buffer prompt.  Both cases now go through the
queryless `agent-repl--kill-vterm-process'.
WS defaults to the current workspace."
  (let ((existing (get-buffer (agent-repl--buffer-name nil ws))))
    (cond
     ((not existing)
      (agent-repl--log (agent-repl--ws-current-name) "kill-stale-vterm: no existing buffer"))
     ((get-buffer-process existing)
      (agent-repl--log (agent-repl--ws-current-name)
                       "kill-stale-vterm: buf=%s has LIVE process — zombie from failed teardown, killing querylessly"
                       (buffer-name existing))
      (agent-repl--kill-vterm-process existing))
     (t
      (agent-repl--log (agent-repl--ws-current-name) "kill-stale-vterm: killing stale buf=%s" (buffer-name existing))
      (kill-buffer existing)))))

;;;; Panel show/hide strategies

(defun agent-repl--show-loading-panels ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once the agent is ready."
  (let* ((ws (agent-repl--ws-current-name))
         (real-vterm (agent-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create agent-repl-loading-placeholder-name)))
    (agent-repl--log ws "show-loading-panels")
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (agent-repl--set-buffer-background agent-repl--vterm-background-grey))
    (agent-repl--ws-put ws :vterm-buffer placeholder)
    (agent-repl--show-panels-and-focus)
    (agent-repl--ws-put ws :vterm-buffer real-vterm)))

(defun agent-repl--initialize-agent (&optional ws project-dir-hint active-env-hint)
  "Initialize an agent session for WS.
Calls `initialize-ws-env' with PROJECT-DIR-HINT and ACTIVE-ENV-HINT
(creation paths — worktree setup or new-workspace — pass known values
here; regular `SPC o c' passes nil and lets the helper derive from
the state file or the current buffer's git-root).  Then creates the
output vterm buffer, launches the agent CLI inside it, creates the
input buffer, enables the hide-overlay, marks `:agent-state' as
`:init', and announces the startup.  Errors if the agent is already
running for WS.

Writes `:agent-state :init' immediately after launching the vterm
process (documented lifecycle exception to the sentinel-only-writes
rule — no hook fires between process launch and session-start, so
Emacs is the only observer of \"agent process exists, not ready yet\").

Panels open IMMEDIATELY after a successful launch (when WS is the
current workspace and no persisted hidden/inactive preference says
otherwise) so the agent TUI is visible from process start — blocking
first-run screens (claude's folder-trust dialog, codex onboarding)
fire no readiness hook, and the historical wait-for-ready gate left
them invisible.  `on-session-start-event' still runs the after-ready
path later for pending-prompt draining; its panel-show is idempotent."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (unless ws (error "agent-repl--initialize-agent: no active workspace"))
    (when (agent-repl--agent-running-p ws)
      (error "agent-repl--initialize-agent: already running ws=%s" ws))
    (agent-repl--log ws "initialize-agent: starting new session for ws=%s" ws)
    ;; This function IS the vterm boot, so the workspace's presentation is
    ;; vterm from here on — stamp it before the session_start sentinel can
    ;; race the lazy `:frontend' resolution.  Without the stamp, a workspace
    ;; whose frontend still resolves lazily could take the gui branch of
    ;; `agent-repl--on-session-start-event', which never marks the vterm
    ;; ready nor opens its panels — stranding the session it just launched.
    ;;
    ;; INCIDENTAL, not deliberate: a plain `--ws-put', NOT
    ;; `agent-repl--ws-choose-frontend'.  The distinction is what the
    ;; restore path keys on — a workspace that merely happened to boot a
    ;; vterm here is NOT thereby a vterm workspace forever, and comes back
    ;; under whatever `agent-repl-default-frontend' says after a restart.
    ;; Only `SPC o F' (and friends) make the choice stick.
    (agent-repl--ws-put ws :frontend 'vterm)
    (agent-repl--initialize-ws-env ws project-dir-hint active-env-hint)
    (let* ((root (agent-repl--ws-dir ws))
           (default-directory root))
      (agent-repl--kill-stale-vterm ws)
      ;; Build the start command BEFORE creating the vterm buffer.
      ;; `build-start-cmd' can make a non-local exit — most notably a
      ;; `user-error' from `get-sandbox-image' when the sandbox image still
      ;; needs building — so computing it first means such an abort happens
      ;; before any buffer exists and leaves no orphan panel buffer behind.
      (let* ((start-info (agent-repl--build-start-cmd ws))
             (cmd         (plist-get start-info :cmd))
             (inst        (plist-get start-info :inst))
             (vterm-buf   (agent-repl--create-buffer ws))
             (launched    nil))
        ;; If anything between buffer creation and a successful launch makes a
        ;; non-local exit, kill the orphan buffer so a failed start cannot
        ;; leave a half-initialized zombie workspace behind.
        (unwind-protect
            (progn
              (agent-repl--ws-put ws :vterm-buffer vterm-buf)
              (setf (agent-repl-instantiation-start-cmd inst) cmd)
              (when (plist-get start-info :fork-session-id)
                (agent-repl--log ws "initialize-agent: clearing fork-session-id for ws=%s" ws)
                (agent-repl--ws-put ws :fork-session-id nil))
              (agent-repl--log-session-start ws start-info)
              (with-current-buffer vterm-buf
                (when (eq major-mode 'vterm-mode)
                  (error "agent-repl--initialize-agent: vterm buffer already initialized ws=%s" ws))
                (vterm-mode)
                (setq-local truncate-lines nil)
                (setq-local word-wrap t)
                (agent-repl--set-buffer-background agent-repl--vterm-background-grey)
                (agent-repl--apply-vterm-font-scale)
                (setq-local mode-line-format
                            (agent-repl--workspace-mode-line ws))
                (setq-local agent-repl--ready nil)
                (agent-repl--log ws "initialize-agent: vterm=%s sending cmd len=%d"
                                  (buffer-name) (length cmd))
                (vterm-send-string (concat agent-repl-startup-prefix cmd))
                (vterm-send-return))
              (agent-repl--schedule-ready-timer ws)
              (agent-repl--initialize-input-buffer ws)
              (agent-repl--ws-put ws :prefix-counter 0)
              (agent-repl--enable-hide-overlay)
              (agent-repl--ws-set-agent-state ws :init)
              (agent-repl--info ws "Starting agent... ws=%s ws-id=%s dir=%s cmd=%s"
                                ws (agent-repl--workspace-id) root (or cmd "?"))
              (agent-repl--state-save ws)
              ;; Open panels NOW rather than waiting for readiness (see
              ;; docstring): blocking first-run screens fire no hook, so
              ;; the wait-for-ready gate left them invisible.  Guards
              ;; mirror `agent-repl--open-panels-after-ready': honor a
              ;; persisted panels-closed preference and never steal the
              ;; frame from a different workspace's background boot.
              (cond
               ((memq (agent-repl--ws-repl-state ws) '(:inactive :hidden))
                (agent-repl--log ws "initialize-agent: persisted %s ws=%s — not opening panels"
                                  (agent-repl--ws-repl-state ws) ws))
               ((agent-repl--current-ws-p ws)
                (agent-repl--log ws "initialize-agent: opening panels at launch ws=%s" ws)
                (agent-repl--show-hidden-panels))
               (t
                (agent-repl--log ws "initialize-agent: background boot ws=%s — not opening panels" ws)))
              (setq launched t))
          (unless launched
            (agent-repl--log ws "initialize-agent: launch aborted, killing orphan buffer ws=%s" ws)
            (when (buffer-live-p vterm-buf)
              (agent-repl--ws-put ws :vterm-buffer nil)
              ;; Queryless: the CLI may already be running inside the
              ;; orphan (the launch aborted AFTER vterm-send-string), and
              ;; a raw kill-buffer would block on the process-exit prompt.
              (agent-repl--kill-vterm-process vterm-buf))))))))

(defun agent-repl--clear-main-area-for-panels ()
  "Delete every non-side window other than the selected one.
Side-window-aware replacement for `delete-other-windows' on the
panel-show path: the workspace drawer (a left-side window) must
survive panel reopen.  `delete-other-windows' relies on each side
window carrying `no-delete-other-windows', which is fragile —
window-parameter loss anywhere upstream (e.g. a buffer redisplayed
without the original action alist) leaves the drawer vulnerable.
Routing through `agent-repl-window--delete-where' makes the
side-window skip explicit and parameter-independent."
  (agent-repl-window--delete-where
   (lambda (win) (not (eq win (selected-window))))))

(defun agent-repl--show-existing-panels ()
  "Show panels for an already-running agent session.
Demotes indicators, refreshes display, and restores panel layout.
Sets `:repl-state :active' now that panels are visible and the
session is in use.

Tab-bar bookkeeping happens FIRST (before any window manipulation) so
the persp-names reorder is in place before `show-panels-and-focus'
triggers redisplay — otherwise the intermediate paint can lock the
pre-restore order into the tab-bar's cache.  After panels are up,
pulses the tab via `agent-repl-flash-tab' so the user can track its
return to the prior slot — symmetric with the deprio-on-close flash.

The frame's main area is cleared by `agent-repl--show-panels' itself
(via `--clear-main-area-for-panels', which preserves the drawer side
window) AFTER it has captured the pre-panel layout as
`:fullscreen-config', so this function must NOT clear the main area
first — doing so would destroy the work layout before it is saved."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "show-existing-panels")
    (unless ws (error "agent-repl--show-existing-panels: no active workspace"))
    (agent-repl--ws-set-repl-state ws :active)
    (agent-repl--restore-tab-index ws)
    (agent-repl--refresh-vterm)
    (agent-repl--show-panels-and-focus)
    (agent-repl--update-hide-overlay)
    (agent-repl--flash-current-tab)))

(defun agent-repl--show-hidden-panels ()
  "Restore hidden panels.  `show-existing-panels' writes :repl-state :active.
`:agent-state' is untouched; rendering follows the same rule whether
panels are visible or hidden.

Panels always open filling the frame (fullscreen is the sole display
format), so there is no separate maximize step — `show-existing-panels'
lays them out full-frame via `agent-repl--show-panels'."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "showing panels ws=%s agent-state=%s"
                      ws (agent-repl--ws-agent-state ws))
    (agent-repl--show-existing-panels)))

(defun agent-repl--hide-and-preserve-status ()
  "Close-and-KILL with full deprio + tab-bar shuffle (the `SPC o C' path).
Runs `agent-repl--on-close' (restore layout, hide, deprio bookkeeping)
and then KILLS the session through the workspace's frontend registry —
`SPC o C' means \"done with this session\", unlike the plain-close
`SPC o c' which only puts the view away.  The `:repl-state :hidden'
marker is re-asserted after the kill (the vterm kill capability resets
the state axes) so hide-mode's sweep semantics survive.

Deliberately NOT folded into `agent-repl--on-close': its other callers
(`agent-repl-send-and-hide', the drawer close) hide a session that
must keep running."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws (error "agent-repl--hide-and-preserve-status: no active workspace"))
    (agent-repl--on-close ws)
    (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)
    (agent-repl--ws-put ws :repl-state :hidden)))

(defun agent-repl--simple-hide-and-preserve-status ()
  "Hide agent panels with NO tab-bar update (the `SPC o c' path).
Thin wrapper around `agent-repl--on-simple-close' that enforces the
invariant that a workspace is active.  See
`agent-repl--hide-and-preserve-status' for the deprio + flash variant
bound to `SPC o C'."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws (error "agent-repl--simple-hide-and-preserve-status: no active workspace"))
    (agent-repl--on-simple-close ws)))

;;;; Entry point

(cl-defun agent-repl--toggle (close-fn &key always-close)
  "Generic toggle.  CLOSE-FN handles the visible-panels case.
Open / start / send-selection paths are shared.  Used by both
`agent-repl' (deprio close) and `agent-repl-simple' (plain close).

When ALWAYS-CLOSE is non-nil, every non-selection branch routes to
CLOSE-FN regardless of running / starting / panel-visibility state —
the workspace is hidden even if the agent isn't visible (or isn't running
at all).  This is the `SPC o C' contract: pressing it again on a
workspace that is already hidden / never-started should still mark it
`:hidden' and push it to the back, not re-show or launch the agent."
  (let* ((ws (agent-repl--ws-current-name))
         (vterm-running (agent-repl--agent-running-p))
         (session-starting (agent-repl--session-starting-p))
         (panels-visible (agent-repl--panels-visible-p))
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (agent-repl--log ws "agent-repl running=%s starting=%s visible=%s selection=%s always-close=%s"
                      vterm-running session-starting panels-visible
                      (if selection "yes" "no") (if always-close "yes" "no"))
    (cond
     ;; GUI frontend: its own three-way toggle (send-selection / hide /
     ;; open-or-show) — the vterm branches below reason about vterm
     ;; process state and panel pairs that do not exist for the gui.
     ((agent-repl--ws-gui-frontend-p ws)
      (let ((fe (agent-repl--ws-frontend ws))
            (webview (agent-repl--ws-get ws :frontend-buffer)))
        (cond
         (selection
          (agent-repl--log ws "toggle[gui]: branch=send-selection")
          (deactivate-mark)
          (agent-repl--send-to-agent selection))
         ((and (buffer-live-p webview) (get-buffer-window webview))
          (agent-repl--log ws "toggle[gui]: branch=hide")
          (funcall (agent-repl-frontend-hide-fn fe) ws))
         ((funcall (agent-repl-frontend-running-p-fn fe) ws)
          (agent-repl--log ws "toggle[gui]: branch=show")
          (funcall (agent-repl-frontend-show-fn fe) ws))
         (t
          (agent-repl--log ws "toggle[gui]: branch=open")
          (funcall (agent-repl-frontend-open-fn fe) ws)))))
     (selection
      (deactivate-mark)
      (agent-repl--send-to-agent selection))
     (always-close
      (funcall close-fn))
     ((not vterm-running)
      (agent-repl--initialize-agent))
     (session-starting
      (message "Agent is loading…"))
     (panels-visible
      (funcall close-fn))
     ;; Output window is up but the input window was dropped (e.g. a
     ;; fullscreen frame with only the output window).  Add the input
     ;; window alongside the existing output window and focus it —
     ;; don't rebuild the whole layout (which would duplicate the
     ;; already-visible output window).
     ((agent-repl--output-visible-input-hidden-p)
      (agent-repl--show-input-beside-output)
      (agent-repl--focus-input-panel))
     (t
      (agent-repl--show-hidden-panels)))))

(defun agent-repl ()
  "Hide Agent REPL panels and deprio the workspace.
If text is selected: send it directly to the agent (orthogonal to hide).
Otherwise: mark the workspace `:repl-state :hidden', hide both panels
\(no-op if already hidden), and push the workspace tab to the back.
Always hides, regardless of whether the agent is running or panels are
currently visible — if hide-mode is on, the next workspace switch will
persp-kill the workspace via `agent-repl--sweep-hidden-workspaces'.
Bound to `SPC o C'.  See `agent-repl-simple' for the no-tab-bar variant."
  (interactive)
  (agent-repl--toggle #'agent-repl--hide-and-preserve-status :always-close t))

(defun agent-repl-simple ()
  "Toggle Agent REPL panels with a plain close (no tab-bar update).
Same dispatch as `agent-repl' except the close branch only hides the
panels and sets `:repl-state :inactive' — no save-tab-index, no
push-to-back, no flash.  Bound to `SPC o c'."
  (interactive)
  (agent-repl--toggle #'agent-repl--simple-hide-and-preserve-status))

;;;; Session cleanup

(defun agent-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (agent-repl--log (agent-repl--ws-current-name) "kill-placeholder exists=%s" (if (get-buffer agent-repl-loading-placeholder-name) "yes" "no"))
  (when-let ((placeholder (get-buffer agent-repl-loading-placeholder-name)))
    (agent-repl--close-buffer-window placeholder)
    (kill-buffer placeholder)))

(defun agent-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (when (process-live-p proc)
    (agent-repl--log (agent-repl--ws-current-name) "sigkill fallback for lingering process")
    (signal-process proc 'SIGKILL)))

(defun agent-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (agent-repl--log (agent-repl--ws-current-name) "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time agent-repl-sigkill-delay nil #'agent-repl--sigkill-if-alive proc))

(defun agent-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process.
Suppresses both the standard process-exit query (via
`set-process-query-on-exit-flag') and any other
`kill-buffer-query-functions' (e.g., vterm's own kill query) so the
nuke path never prompts about the agent process."
  (agent-repl--log (agent-repl--ws-current-name) "kill-vterm-process buf=%s" (agent-repl--safe-buffer-name buf))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf))
          (kill-buffer-query-functions nil))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (agent-repl--schedule-sigkill proc)))))

(defun agent-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (agent-repl--log ws "teardown-session-state ws=%s env=%s"
                    ws (agent-repl--ws-get ws :active-env))
  (condition-case err
      (agent-repl--disable-hide-overlay)
    (error (agent-repl--warn ws "disable-hide-overlay failed during teardown: %S" err)))
  (when agent-repl--sync-timer
    (cancel-timer agent-repl--sync-timer)
    (setq agent-repl--sync-timer nil))
  ;; Update instantiation and persist state BEFORE clearing buffer refs,
  ;; since state-save needs the vterm buffer to resolve the project root.
  (let ((inst (agent-repl--active-inst ws)))
    (setf (agent-repl-instantiation-start-cmd inst) nil))
  (agent-repl--state-save ws)
  (agent-repl--ws-put ws :vterm-buffer nil)
  (agent-repl--ws-put ws :input-buffer nil))

(defun agent-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (agent-repl--log (agent-repl--ws-current-name) "destroy-session-buffers")
  (agent-repl--close-buffer-windows vterm-buf input-buf)
  (agent-repl--kill-placeholder)
  (agent-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun agent-repl--kill-session (ws)
  "Cancel timers, tear down state, and destroy buffers for workspace WS.
Captures the current buffer references before teardown clears them."
  (agent-repl--log ws "kill-session: ws=%s" ws)
  (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer))
        (input-buf (agent-repl--ws-get ws :input-buffer)))
    (agent-repl--cancel-ready-timer ws)
    (agent-repl--teardown-session-state ws)
    (agent-repl--destroy-session-buffers vterm-buf input-buf)))

(defun agent-repl--kill-workspace-buffers (ws)
  "Kill every buffer (and attached process) belonging to persp WS.
Idempotent: no-op when persp-mode is inactive, the persp does not
exist, or the persp slot holds a symbol sentinel rather than a real
perspective.  Each buffer is killed inside its own `condition-case' so
one bad buffer cannot block the rest.  File-visiting buffers are
marked unmodified before killing so `kill-buffer' does not prompt —
the user has already confirmed the destructive nuke.

Agent buffers owned by a different workspace (see
`agent-repl--foreign-owned-buffer-p') are skipped, not killed: persp-mode
can drift another workspace's live panel into this persp, and nuking it
would wipe that workspace's running session."
  (when (agent-repl--ws-system-available-p)
    (when-let ((persp (agent-repl--ws-resolve-persp ws)))
      (let ((bufs (agent-repl--ws-buffers persp))
            (kill-buffer-query-functions nil))
        (agent-repl--log ws "kill-workspace-buffers: count=%d" (length bufs))
        (dolist (buf bufs)
          (condition-case err
              (if (agent-repl--foreign-owned-buffer-p buf ws)
                  (agent-repl--log ws "kill-workspace-buffers: SKIP foreign buf=%s owner=%s"
                                    (agent-repl--safe-buffer-name buf)
                                    (agent-repl--buffer-owner buf))
                (let* ((buf-name (agent-repl--safe-buffer-name buf))
                       (live (buffer-live-p buf))
                       (proc (and live (get-buffer-process buf)))
                       (t-buf (float-time)))
                  (agent-repl--log ws "kill-workspace-buffers: buf=%s live=%s proc=%s"
                                    buf-name (if live "t" "nil")
                                    (if proc (process-name proc) "nil"))
                  (when live
                    (when proc
                      (set-process-query-on-exit-flag proc nil)
                      (ignore-errors (delete-process proc))
                      (agent-repl--schedule-sigkill proc))
                    (with-current-buffer buf
                      (set-buffer-modified-p nil))
                    (kill-buffer buf))
                  (agent-repl--log ws "kill-workspace-buffers: buf=%s done elapsed=%.3fs"
                                    buf-name (- (float-time) t-buf))))
            (error
             (agent-repl--log ws "kill-workspace-buffers: error on %s: %S"
                               (agent-repl--safe-buffer-name buf) err))))
        (agent-repl--log ws "kill-workspace-buffers: dolist done count=%d" (length bufs))))))

;;;; User commands

(defun agent-repl-kill ()
  "Kill the agent session and its view for the current workspace.
Frontend-blind: dispatches the workspace's registered frontend's
`:kill-fn' (vterm process + panels, or daemon session + webview).
The vterm capability carries the historical lifecycle-reset semantics
\(see `agent-repl--vterm-kill')."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "kill")
    (unless ws (error "agent-repl-kill: no active workspace"))
    (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)))

(defun agent-repl-restart ()
  "Hard restart the agent for the current workspace.
Frontend-blind: dispatches the workspace's registered frontend's
`:restart-fn'.  For vterm the agent state file on disk is preserved so
the new process resumes via `--continue'; for the gui a fresh daemon
session is created."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "restart")
    (funcall (agent-repl-frontend-restart-fn (agent-repl--ws-frontend ws)) ws)))

(defun agent-repl-focus-input ()
  "Focus the agent input buffer, or return to previous window if already there.
If the agent isn't running, start it (same as `agent-repl')."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (agent-repl--ws-get ws :input-buffer))
      (agent-repl--log ws "focus-input branch=jump-back")
      (evil-window-left 1))
     ;; Not running — start fresh
     ((not (agent-repl--agent-running-p))
      (agent-repl--log ws "focus-input branch=initialize-agent")
      (agent-repl))
     ;; Running but panels hidden — show them
     (t
      (agent-repl--log ws "focus-input branch=show-or-focus")
      (unless (agent-repl--panels-visible-p)
        (agent-repl--show-panels))
      (when-let ((win (get-buffer-window (agent-repl--ws-get ws :input-buffer))))
        (select-window win))))))

(defun agent-repl--restore-fullscreen-config (ws)
  "Restore WS's saved pre-panel layout, clearing `:fullscreen-config'.
Returns non-nil when a restore happened, nil when WS had no saved config.

`:fullscreen-config' is the window layout captured by
`agent-repl--show-panels' the moment the frame-filling panels were
opened (fullscreen is the sole display format).  The close paths
\(`agent-repl--on-simple-close' for `SPC o c' and
`agent-repl--on-close' for `SPC o C') restore it before hiding so the
work windows the panels covered come back rather than the close
stranding a panel onscreen.  Only the saved-config case is handled: a
frame with no `:fullscreen-config' has no layout to restore to."
  (when-let ((saved (and ws (agent-repl--ws-get ws :fullscreen-config))))
    (set-window-configuration saved)
    (agent-repl--ws-put ws :fullscreen-config nil)
    t))

(defvar agent-repl--window-fullscreen-config nil
  "Saved window configuration for non-agent fullscreen toggle.
Set when `agent-repl-fullscreen-and-focus' maximizes a non-agent window,
cleared on restore.")

(defun agent-repl--fullscreen-leave-side-window ()
  "Move out of a side window before fullscreening.

When `agent-repl-fullscreen-and-focus' is invoked from inside a side
window (e.g. the workspace drawer), `selected-window' is the side
window itself.  The non-agent branch would then treat the drawer as
the window to KEEP and sweep every main-area window — leaving the
user's actual work window (or agent panels) destroyed and only the
drawer alongside an arbitrary survivor from `delete-window's benign
sole-main-window error.

Pre-selecting a real main-area leaf window sidesteps the path: the
subsequent branch dispatch reads the buffer of a real main-area
window and the delete sweep keeps that window instead of the
drawer.

`window-main-window' returns an internal container window when the
main area has been split, so we descend the tree to a live leaf
before `select-window'.

No-op when `selected-window' is not a side window."
  (when (agent-repl-window--side-window-p (selected-window))
    (when-let* ((main (and (fboundp 'window-main-window) (window-main-window)))
                (leaf (agent-repl--first-live-leaf main)))
      (select-window leaf))))

(defun agent-repl--first-live-leaf (win)
  "Return the first live leaf window beneath WIN.
A live leaf is one that displays a buffer (`window-live-p').  If WIN
is itself live, returns WIN.  Otherwise descends `window-child' until
a leaf is reached.  Returns nil if no leaf is found."
  (cond
   ((null win) nil)
   ((window-live-p win) win)
   (t (agent-repl--first-live-leaf (window-child win)))))

(defun agent-repl-fullscreen-and-focus ()
  "Focus the agent input window, or maximize a non-agent work window.
When in a agent panel buffer, moves point to the input buffer — the
agent panels already fill the frame (fullscreen is the sole display
format), so there is nothing to maximize.
When not in a agent panel buffer, maximizes the current window within
the non-side area (preserving the workspace drawer) and saves the
layout; calling again restores it.
When invoked from a side window (e.g. the workspace drawer), first
moves point to the frame's main window so the maximize target is a
real main-area window — see
`agent-repl--fullscreen-leave-side-window'."
  (interactive)
  (agent-repl--fullscreen-leave-side-window)
  (if (agent-repl--agent-panel-buffer-p)
      (let* ((ws (agent-repl--ws-current-name))
             (input-buf (agent-repl--ws-get ws :input-buffer))
             (input-win (and input-buf (get-buffer-window input-buf))))
        (when input-win
          (select-window input-win)))
    (if agent-repl--window-fullscreen-config
        (progn
          (set-window-configuration agent-repl--window-fullscreen-config)
          (setq agent-repl--window-fullscreen-config nil))
      (setq agent-repl--window-fullscreen-config (current-window-configuration))
      (let ((keep (selected-window)))
        (agent-repl-window--delete-where
         (lambda (win) (not (eq win keep))))))))

(defun agent-repl-cycle ()
  "Send backtab to the agent vterm to cycle through options."
  (interactive)
  (agent-repl--log (agent-repl--ws-current-name) "cycle")
  (when (agent-repl--vterm-live-p)
    (with-current-buffer (agent-repl--ws-get (agent-repl--ws-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

(defun agent-repl--validate-env-switch (ws new-env worktree-p session-id)
  "Validate that workspace WS can switch to NEW-ENV.
WORKTREE-P and SESSION-ID describe the current workspace state.
Signals `user-error' if any precondition is not met.

The frontend check is the first one: `agent-repl-switch-environment' is
vterm machinery end to end (`--kill-session', then `--initialize-agent'
under the other environment), and the gui — which cannot present a
`:sandbox' session at all (`agent-repl-frontend' SUPPORTED-ENVS) — has
no counterpart.  Running it on a gui workspace would leave the daemon
session orphaned and boot a second agent on the same directory, so it
refuses out loud instead.  Switch the workspace to vterm first
\(`SPC o F')."
  (agent-repl--log ws "validate-env-switch: ws=%s new-env=%s" ws new-env)
  (when (agent-repl--ws-gui-frontend-p ws)
    (user-error "Environment switching is a vterm capability — switch %s to the vterm frontend first (SPC o F)" ws))
  (unless worktree-p
    (user-error "Sandbox switching requires a worktree workspace"))
  (unless session-id
    (user-error "No session ID captured yet — session may still be starting"))
  (when (agent-repl--ws-get ws :thinking)
    (user-error "Cannot switch environment while Claude is thinking"))
  (when (and (eq new-env :sandbox)
             (not (agent-repl--resolve-sandbox-config
                   (agent-repl--ws-get ws :project-dir))))
    (user-error "No sandbox configuration found for this workspace")))

(defun agent-repl--seed-new-env-session (ws new-env session-id)
  "Ensure the NEW-ENV instantiation for WS has a session-id.
If this is the first switch, seeds the new environment's session-id
from SESSION-ID.  The value signals to `compute-claude-flags' that
the env should emit `--continue' on start — which picks up the most
recent session in the worktree's cwd (i.e. the one we just left)."
  (let ((new-inst (or (agent-repl--ws-get ws new-env)
                      (make-agent-repl-instantiation))))
    (if (agent-repl-instantiation-session-id new-inst)
        (agent-repl--log ws "seed-new-env-session: ws=%s new-env=%s reusing existing session-id" ws new-env)
      (agent-repl--log ws "seed-new-env-session: ws=%s new-env=%s seeding new session-id=%s"
                        ws new-env session-id)
      (setf (agent-repl-instantiation-session-id new-inst) session-id))
    (agent-repl--ws-put ws new-env new-inst)))

(defun agent-repl-switch-environment ()
  "Switch the current workspace between Docker sandbox and bare-metal.
Kills the current agent process and resumes it in the other environment.
On the first switch, the new environment seeds its session-id from the
current one so `--continue' in the other env picks up the conversation.
On subsequent switches each environment resumes its own prior session
independently.  Requires a worktree workspace with a captured session ID."
  (interactive)
  (let* ((ws (agent-repl--ws-current-name))
         (active-env (agent-repl--ws-get ws :active-env))
         (_ (agent-repl--log ws "switch-environment: ws=%s active-env=%s" ws active-env))
         (worktree-p (agent-repl--ws-get ws :worktree-p))
         (inst (agent-repl--active-inst ws))
         (session-id (agent-repl-instantiation-session-id inst))
         (new-env (if (eq active-env :sandbox) :bare-metal :sandbox)))
    (agent-repl--validate-env-switch ws new-env worktree-p session-id)
    (agent-repl--seed-new-env-session ws new-env session-id)
    (agent-repl--kill-session ws)
    (agent-repl--ws-put ws :active-env new-env)
    (agent-repl--info ws "Switching to %s (resuming session %s...)"
                      (if (eq new-env :sandbox) "Docker sandbox" "bare-metal")
                      (substring session-id 0 agent-repl-session-id-display-length))
    (agent-repl--initialize-agent ws)
    (agent-repl--show-panels-and-focus)))

;;;; ---- Frontend registration (vterm) -----------------------------------------
;;
;; The vterm frontend wraps the classic machinery this file and
;; session.el own: interactive TUI in a vterm process, panels shown via
;; the fullscreen layout. Registered here (rather than frontends.el) so
;; the registry file stays mechanism-only, mirroring how backend.el
;; hosts the registry while claude/codex register from their homes.

(defun agent-repl--vterm-kill (ws)
  "The vterm frontend's kill capability (registry `:kill-fn').
Lifecycle-reset: kill destroys the session, so both state axes reset to
nil (documented exception to \"sentinel-only writes agent-state\").
The workspace returns to a pristine no-agent state awaiting the next
initialize."
  (agent-repl--ws-put ws :agent-state nil)
  (agent-repl--ws-put ws :repl-state nil)
  (force-mode-line-update t)
  (agent-repl--kill-session ws))

(defun agent-repl--vterm-interrupt (ws kind)
  "The vterm frontend's interrupt capability (registry `:interrupt-fn').
KIND `ctrl-c' clears the TUI prompt line (raw ETX byte); `escape' stops
the in-flight generation.  Returns non-nil only when the gesture was
actually delivered to a live vterm."
  (pcase kind
    ('ctrl-c (agent-repl--vterm-send-raw-ctrl-c))
    ('escape
     (let ((vterm-buf (agent-repl--ws-get ws :vterm-buffer)))
       (when (and vterm-buf (buffer-live-p vterm-buf))
         (agent-repl--send-interrupt-escape ws vterm-buf)
         t)))))

(agent-repl-register-frontend
 (agent-repl-frontend-create
  :name 'vterm
  :open-fn (lambda (_ws) (agent-repl--initialize-agent))
  ;; The boot capability IS the open capability for vterm — the TUI has
  ;; no headless mode, so `--initialize-agent' both launches the process
  ;; and (when WS is current) shows its panels.  It takes the creation
  ;; hints directly, which is why `boot-fn' carries them at all.
  :boot-fn (lambda (ws project-dir-hint active-env-hint)
             (agent-repl--initialize-agent ws project-dir-hint active-env-hint))
  :kill-fn #'agent-repl--vterm-kill
  :send-fn #'agent-repl--vterm-send-turn
  :interrupt-fn #'agent-repl--vterm-interrupt
  :running-p-fn (lambda (ws) (agent-repl--agent-running-p ws))
  :show-fn (lambda (_ws) (agent-repl--show-hidden-panels))
  ;; The hide capability is the SPC-o-c CLOSE semantic (restore the
  ;; pre-panel layout, mark :inactive), not the bare window hide —
  ;; the gui frontend's hide is its analog, and the frontend switch
  ;; composes hide+open, so both must leave a clean frame behind.
  :hide-fn (lambda (ws) (agent-repl--on-simple-close ws))
  :restart-fn (lambda (ws)
                (agent-repl--vterm-kill ws)
                (agent-repl--initialize-agent ws))
  :supported-backends '(claude codex)
  ;; The vterm launches the agent through `--build-start-cmd', which
  ;; wraps it in `docker run' for `:sandbox' — so the TUI is the only
  ;; presentation a sandboxed workspace can have.
  :supported-envs '(:bare-metal :sandbox)
  ;; Durable-resume currency: the hook-captured instantiation session
  ;; id IS the CLI session uuid; adopting one persists it so the next
  ;; initialize resumes through the state file.
  :durable-session-id-fn #'agent-repl--ws-durable-claude-session-id
  :adopt-session-fn (lambda (ws id) (agent-repl--set-session-id ws id))))
