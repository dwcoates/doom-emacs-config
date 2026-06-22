;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

(defcustom claude-repl-input-height-fraction 0.23
  "Fraction of the vterm window's height allocated to the input panel."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-loading-placeholder-name " *claude-loading*"
  "Buffer name for the loading placeholder shown while Claude starts."
  :type 'string
  :group 'claude-repl)

(defcustom claude-repl-sigkill-delay 0.5
  "Seconds to wait before sending SIGKILL to a lingering Claude process."
  :type 'number
  :group 'claude-repl)

(defcustom claude-repl-autoselect-input-on-workspace-switch t
  "When non-nil, auto-select the Claude input window on workspace switch.
If the input panel is visible after switching to a workspace, the input
window is selected so the user can start typing immediately."
  :type 'boolean
  :group 'claude-repl)

(defcustom claude-repl-session-id-display-length 8
  "Number of characters of session ID to display in messages."
  :type 'integer
  :group 'claude-repl)

;;;; Panel visibility predicates

(defun claude-repl--ws-buffer-visible-p (key)
  "Return non-nil if the buffer stored at KEY in current workspace is visible."
  (let* ((buf (claude-repl--ws-get (claude-repl--ws-current-name) key))
         (result (and buf (buffer-live-p buf) (get-buffer-window buf))))
    (claude-repl--log-verbose (claude-repl--ws-current-name) "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
    result))

(defun claude-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (claude-repl--ws-buffer-visible-p :input-buffer))

(defun claude-repl--vterm-visible-p ()
  "Return t if vterm buffer for the current workspace is visible in a window."
  (claude-repl--ws-buffer-visible-p :vterm-buffer))

(defun claude-repl--panels-visible-p ()
  "Return t if both panels are visible."
  (let ((result (and (claude-repl--input-visible-p)
                     (claude-repl--vterm-visible-p))))
    (claude-repl--log-verbose (claude-repl--ws-current-name) "panels-visible-p: result=%s" (if result "visible" "hidden"))
    result))

(defun claude-repl--output-visible-input-hidden-p ()
  "Return t when the output (vterm) panel is visible but the input is not.
This is the inconsistent half-shown state — e.g. a fullscreen frame
showing only the Claude output window — that
`claude-repl--show-input-beside-output' repairs by adding the input
window alongside the existing output window."
  (let ((result (and (claude-repl--vterm-visible-p)
                     (not (claude-repl--input-visible-p)))))
    (claude-repl--log-verbose (claude-repl--ws-current-name)
                              "output-visible-input-hidden-p: result=%s"
                              (if result "yes" "no"))
    result))

;;;; Panel display and hide

(defun claude-repl--safe-buffer-name (b)
  "Return the name of buffer B if non-nil, otherwise nil."
  (and b (buffer-name b)))

(defun claude-repl--close-buffer-window (buf)
  "Close windows displaying BUF in the selected frame.
Delegates to `claude-repl-window--delete-buffer-windows' with
`:all-frames nil' to preserve the historical selected-frame-only
scope.  If a panel buffer is torn out to another frame, this
function leaves that frame's window alone — by design, since the
caller is doing a per-frame teardown."
  (claude-repl-window--delete-buffer-windows buf :all-frames nil))

(defun claude-repl--close-buffer-windows (&rest bufs)
  "Close windows displaying any of BUFS."
  (claude-repl--log (claude-repl--ws-current-name) "close-buffer-windows %s" (mapcar #'claude-repl--safe-buffer-name bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (claude-repl--close-buffer-window buf))))

(defun claude-repl--configure-vterm-window (win)
  "Configure WIN as a dedicated, width-locked, protected vterm window.
Recipe (delegated to `claude-repl-window--harden'):

  • Dedicated — `display-buffer' can't repurpose the window.
  • `window-size-fixed: width' — prevents resize-triggered vterm reflow.
  • `no-delete-other-windows' — `delete-other-windows' can't kill it.

Keyboard-navigation isolation is handled dynamically by
`claude-repl--bounce-from-vterm' rather than by a static
`no-other-window' parameter — that way windmove/`other-window' can see
vterm, but any non-mouse selection gets auto-corrected back to the
input panel (or a warning if the input isn't displayed)."
  (claude-repl--log (claude-repl--ws-current-name) "configure-vterm-window: win=%s" win)
  (claude-repl-window--harden win
                              :dedicate       t
                              :size-fix       'width
                              :delete-protect t))

;; Fullscreen window layout: vterm fills the frame's main area, input below it.
(defun claude-repl--show-panels ()
  "Display vterm and input panels filling the frame (fullscreen).
Saves the pre-panel window layout as WS's `:fullscreen-config' (so the
close path can restore the work windows the panels are about to cover),
then clears the frame's main area (deleting every non-side window via
`claude-repl--clear-main-area-for-panels') so the panels fill the
frame, then puts vterm in the surviving window and splits it bottom for
input (`claude-repl-input-height-fraction').  Fullscreen is the sole
display format — there is no splitscreen layout alongside a work window.
If the selected window is a side window (e.g. the drawer), redirects
to the frame's main window before clearing — side windows can't be
split, and we must never touch the drawer.

The `:fullscreen-config' save is guarded so it captures the layout only
on a genuine open (no panels visible and no config already saved): the
many re-show paths (workspace-switch reclaim, half-shown repair) call
through here too and must not clobber the saved work layout with a
panels-already-up one."
  (when (claude-repl-window--side-window-p (selected-window))
    (when-let ((main (and (fboundp 'window-main-window) (window-main-window))))
      (select-window main)))
  (let ((ws (claude-repl--ws-current-name)))
    (unless (or (claude-repl--ws-get ws :fullscreen-config)
                (claude-repl--panels-visible-p))
      (claude-repl--ws-put ws :fullscreen-config (current-window-configuration))))
  (claude-repl--clear-main-area-for-panels)
  (let* ((ws (claude-repl--ws-current-name))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer))
         (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--log ws "show-panels vterm=%s input=%s"
                      (claude-repl--safe-buffer-name vterm-buf)
                      (claude-repl--safe-buffer-name input-buf))
    (let* ((vterm-win (selected-window))
           (input-win (split-window vterm-win (round (* (- claude-repl-input-height-fraction) (window-total-height vterm-win))) 'below)))
      (claude-repl--log ws "show-panels: vterm-win=%s input-win=%s" vterm-win input-win)
      (claude-repl--refresh-vterm)
      (set-window-buffer vterm-win vterm-buf)
      (set-window-buffer input-win input-buf)
      (claude-repl--configure-vterm-window vterm-win)
      ;; Input window recipe: dedicated, height-locked, delete-protected,
      ;; AND height-preserved.  `window-size-fixed' alone is bypassed by
      ;; `window--resize-mini-window' (ignore=t), so a multi-line echo
      ;; area shrinks the input.  The preserved-size parameter is only
      ;; bypassed by ignore='preserved', so preserving here steers
      ;; mini-window shrink onto vterm/work-win instead.
      (claude-repl-window--harden input-win
                                  :dedicate       t
                                  :size-fix       'height
                                  :delete-protect t
                                  :preserve-size  'height)))
  ;; Event-driven (user just opened panels) → kick a fresh update pass
  ;; via the unguarded entrypoint, bypassing the 1Hz timer's in-flight
  ;; reentry guard.  See `--update-all-workspace-states-now' docstring.
  (claude-repl--update-all-workspace-states-now))

(defun claude-repl--focus-input-panel ()
  "Focus the input panel window.
Signals an error if the input buffer or its window cannot be found —
callers should ensure panels are displayed before calling this.
Buffer/window resolution delegates to
`claude-repl-window--panel-buffer' and `--panel-window'."
  (claude-repl--log (claude-repl--ws-current-name) "focus-input-panel")
  (let* ((ws (claude-repl--ws-current-name))
         (buf (claude-repl-window--panel-buffer :input ws)))
    (unless buf
      (error "claude-repl--focus-input-panel: no :input-buffer for workspace %s" ws))
    (let ((win (claude-repl-window--panel-window :input ws)))
      (unless win
        (error "claude-repl--focus-input-panel: input buffer %s is not displayed in any window"
               (buffer-name buf)))
      (select-window win))))

(defun claude-repl--show-panels-and-focus ()
  "Display both Claude panels and focus the input panel.
Convenience wrapper combining `claude-repl--show-panels' and
`claude-repl--focus-input-panel'."
  (claude-repl--show-panels)
  (claude-repl--focus-input-panel))

(defun claude-repl--show-input-beside-output ()
  "Add the input panel below the already-visible output (vterm) window.

For the half-shown state where the output window is visible but the
input window is not (e.g. a fullscreen frame with only the output
window): splits the existing output window and displays the input
buffer beneath it, using the same normal below-split layout
`claude-repl--show-panels' applies.  Deliberately does NOT recreate
the output window or clear any other windows — only the missing input
window is added, alongside the output window already on screen.

Returns the new input window, or nil when the output window is not
visible or the input buffer is unavailable.  Hardens the input window
with the same recipe as `claude-repl--show-panels' (dedicated,
height-locked, delete-protected, height-preserved)."
  (let* ((ws (claude-repl--ws-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (vterm-win (claude-repl-window--panel-window :vterm ws)))
    (claude-repl--log ws "show-input-beside-output vterm-win=%s input-buf=%s"
                      vterm-win (claude-repl--safe-buffer-name input-buf))
    (if (not (and vterm-win input-buf (buffer-live-p input-buf)))
        (claude-repl--log ws "show-input-beside-output: no-op (vterm-win=%s input-live=%s)"
                          vterm-win (and input-buf (buffer-live-p input-buf)))
      (let ((input-win (split-window
                        vterm-win
                        (round (* (- claude-repl-input-height-fraction)
                                  (window-total-height vterm-win)))
                        'below)))
        (claude-repl--log ws "show-input-beside-output: input-win=%s" input-win)
        (set-window-buffer input-win input-buf)
        (claude-repl-window--harden input-win
                                    :dedicate       t
                                    :size-fix       'height
                                    :delete-protect t
                                    :preserve-size  'height)
        input-win))))

(defun claude-repl--ensure-input-beside-output ()
  "Repair a half-shown layout by adding the input window beside the output.
No-op unless the output (vterm) panel is visible while the input panel
is not (see `claude-repl--output-visible-input-hidden-p').  When that
state holds — including a fullscreen frame showing only the output
window after a workspace switch — `claude-repl--show-input-beside-output'
adds the input window alongside the existing output window."
  (when (claude-repl--output-visible-input-hidden-p)
    (claude-repl--log (claude-repl--ws-current-name)
                      "ensure-input-beside-output: repairing half-shown layout")
    (claude-repl--show-input-beside-output)))

;;;; Vterm refresh

(defun claude-repl--snap-vterm-window-to-cursor (win)
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

(defun claude-repl--vterm-redraw ()
  "Redraw the current vterm buffer with read-only suppressed.
Assumes the current buffer is in vterm-mode."
  (claude-repl--log-verbose (claude-repl--ws-current-name) "vterm-redraw: buf=%s" (buffer-name))
  (let ((inhibit-read-only t))
    (when vterm--term
      (vterm--redraw vterm--term))))

(defun claude-repl--do-refresh ()
  "Low-level refresh of the current vterm buffer.
Must be called with a vterm-mode buffer current."
  (claude-repl--log-verbose (claude-repl--ws-current-name) "do-refresh: buf=%s" (buffer-name))
  (claude-repl--vterm-redraw)
  (redisplay t))

(defun claude-repl--fix-vterm-scroll (buf)
  "Snap the vterm window for BUF to its cursor without a visible scroll.

Replaces the previous brief-select hack: instead of momentarily
selecting BUF's window so vterm's selection-change side effect scrolls
the display to the cursor, this resets the buffer cursor explicitly
and jumps `window-start' so the cursor lands on the last visible line
in a single redisplay step (see `claude-repl--snap-vterm-window-to-cursor').

No-op when BUF is dead, has no displayed window, or its window is the
currently selected one — reading/copying flows preserve the user's
manual scroll position in the selected window."
  (let ((vterm-win (get-buffer-window buf))
        (orig-win (selected-window)))
    (if (and vterm-win (not (eq vterm-win orig-win)))
        (progn
          (claude-repl--log-verbose (claude-repl--ws-current-name) "fix-vterm-scroll: snapping buf=%s" (buffer-name buf))
          (with-current-buffer buf
            (when (and (eq major-mode 'vterm-mode)
                       (fboundp 'vterm-reset-cursor-point))
              (condition-case nil (vterm-reset-cursor-point) (end-of-buffer nil)))
            (claude-repl--snap-vterm-window-to-cursor vterm-win)))
      (claude-repl--log-verbose (claude-repl--ws-current-name) "fix-vterm-scroll: skipped buf=%s vterm-win=%s same-win=%s"
                                (buffer-name buf) (if vterm-win "yes" "no")
                                (if (eq vterm-win orig-win) "yes" "no")))))

(defun claude-repl--resolve-vterm-buffer ()
  "Return the vterm buffer to refresh.
Uses the current buffer if it is in vterm-mode, otherwise looks up the
workspace's vterm buffer."
  (if (eq major-mode 'vterm-mode)
      (progn
        (claude-repl--log-verbose (claude-repl--ws-current-name) "resolve-vterm-buffer: path=vterm-mode buf=%s" (buffer-name))
        (current-buffer))
    (when-let ((ws (claude-repl--ws-current-name)))
      (let ((buf (claude-repl--ws-get ws :vterm-buffer)))
        (claude-repl--log-verbose (claude-repl--ws-current-name) "resolve-vterm-buffer: path=workspace-lookup ws=%s buf=%s"
                                  ws (claude-repl--safe-buffer-name buf))
        buf))))

(defun claude-repl--refresh-vterm ()
  "Refresh the claude vterm display.
Works from any buffer or from within the vterm buffer itself."
  (let ((buf (claude-repl--resolve-vterm-buffer)))
    (cond
     ((not buf)
      (claude-repl--log-verbose (claude-repl--ws-current-name) "refresh-vterm: no buffer found"))
     ((not (buffer-live-p buf))
      (claude-repl--log-verbose (claude-repl--ws-current-name) "refresh-vterm: buffer not live buf=%s" (buffer-name buf)))
     (t
      (with-current-buffer buf
        (if (eq major-mode 'vterm-mode)
            (claude-repl--do-refresh)
          (claude-repl--log-verbose (claude-repl--ws-current-name) "refresh-vterm: buf=%s not vterm-mode (mode=%s)"
                                    (buffer-name buf) major-mode)))
      (claude-repl--fix-vterm-scroll buf)))))

(defun claude-repl--drain-pending-show-panels (ws)
  "Open panels for WS if a preemptive prompt queued a :pending-show-panels flag.
When Claude is ready, clears the flag and shows panels.  When Claude
is still starting, leaves the flag set so `on-session-start-event' can
re-drain via `open-panels-after-ready' once ready — avoids displaying
an unloaded vterm window."
  (cond
   ((not (claude-repl--ws-get ws :pending-show-panels))
    (claude-repl--log-verbose ws "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws))
   ((claude-repl--session-starting-p ws)
    (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending session-starting — deferring" ws))
   (t
    (claude-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending draining" ws)
    (claude-repl--ws-put ws :pending-show-panels nil)
    (claude-repl--show-hidden-panels))))

(defun claude-repl--drain-pending-magit (ws)
  "Open `magit-status' for WS if it was created with `:pending-magit' set.
Reads the worktree path from `:project-dir', clears the flag, and removes
the Doom dashboard so magit is the sole main buffer in the new workspace."
  (if (claude-repl--ws-get ws :pending-magit)
      (let ((path (claude-repl--ws-get ws :project-dir)))
        (claude-repl--log ws "drain-pending-magit: ws=%s branch=had-pending path=%s draining" ws path)
        (claude-repl--ws-put ws :pending-magit nil)
        (when path
          (magit-status path)
          (claude-repl--remove-doom-dashboard)))
    (claude-repl--log-verbose ws "drain-pending-magit: ws=%s branch=no-pending no-op" ws)))

(defun claude-repl--drain-pending-initial-buffers (ws)
  "Open configured initial buffers for WS if `:pending-initial-buffers' is set.
Reads the worktree path from `:project-dir' and clears the flag.  Deferred
from `finalize-worktree-workspace' so `find-file-noselect' runs while WS is
the current perspective, preventing the opened buffers from leaking into
the caller's workspace."
  (if (claude-repl--ws-get ws :pending-initial-buffers)
      (let ((path (claude-repl--ws-get ws :project-dir)))
        (claude-repl--log ws "drain-pending-initial-buffers: ws=%s branch=had-pending path=%s draining" ws path)
        (claude-repl--ws-put ws :pending-initial-buffers nil)
        (when path
          (claude-repl--open-initial-buffers ws path)))
    (claude-repl--log-verbose ws "drain-pending-initial-buffers: ws=%s branch=no-pending no-op" ws)))

;; Refresh vterm on workspace switch
(defun claude-repl--maybe-autoselect-input (ws)
  "Select the Claude input window for WS if visible and autoselect is enabled.
Respects `claude-repl-autoselect-input-on-workspace-switch'.
Window lookup delegates to `claude-repl-window--panel-window'.

When the vterm output window is also visible, its display is snapped to
the cursor before the input window is selected, via
`claude-repl--snap-vterm-window-to-cursor'.  This replaces the old
brief-select hack (transiently selecting the vterm window so vterm's
selection-change side effect would recenter on the cursor), which
produced a visible scroll-down animation on workspace switch.  Jumping
`window-start' directly puts the cursor on the last visible line in a
single redisplay step — a snap, not a scroll."
  (when claude-repl-autoselect-input-on-workspace-switch
    (when-let ((win (claude-repl-window--panel-window :input ws)))
      (when-let ((vterm-win (claude-repl-window--panel-window :vterm ws)))
        (claude-repl--log ws "maybe-autoselect-input: snap-vterm via vterm-win=%s" vterm-win)
        (when-let ((vterm-buf (window-buffer vterm-win)))
          (when (buffer-live-p vterm-buf)
            (with-current-buffer vterm-buf
              (when (and (eq major-mode 'vterm-mode)
                         (fboundp 'vterm-reset-cursor-point))
                (condition-case nil (vterm-reset-cursor-point) (end-of-buffer nil)))
              (claude-repl--snap-vterm-window-to-cursor vterm-win)))))
      (claude-repl--log ws "maybe-autoselect-input: selecting input-win=%s" win)
      (select-window win))))

(defun claude-repl--stale-panel-windows ()
  "Return a list of windows showing Claude panel buffers from a different workspace.
Each element is a window whose buffer is a Claude panel (vterm or input) whose
workspace identifier (extracted from the buffer name) does not match the
currently active workspace.  Returns nil when all visible panels belong to the
current workspace or no panels are visible."
  (let* ((ws (claude-repl--ws-current-name))
         (sanitized (and ws (claude-repl--sanitize-ws-name ws))))
    (when sanitized
      (cl-loop for win in (window-list)
               for buf = (window-buffer win)
               for name = (buffer-name buf)
               for id = (claude-repl--extract-panel-id name)
               when (and id (not (string= id sanitized)))
               collect win))))

(defun claude-repl--stale-window-buffers (windows)
  "Return the unique live buffers displayed in WINDOWS.
Used to capture the foreign Claude panel buffers occupying the stale
windows returned by `claude-repl--stale-panel-windows' BEFORE those
windows are deleted, so the buffers can be detached from the current
workspace's persp buffer list afterward.  Dead windows and nil buffers
are dropped."
  (delete-dups
   (delq nil
         (mapcar (lambda (w) (and (window-live-p w) (window-buffer w)))
                 windows))))

(defun claude-repl--lone-output-window ()
  "Return the sole non-side window when it shows a Claude output buffer.

Returns the window displaying a Claude vterm (output) buffer — of ANY
workspace, whether or not it belongs to the current one — when that
window is the only non-side window in the frame.  Returns nil otherwise.

This is the \"lone output\" state: the frame shows just a Claude output
window (e.g. a fullscreen Claude REPL whose saved layout restored only
its output window, or another workspace's leftover output window) with
no input panel beside it.  Because the output window is the only
non-side window, the absence of a visible input panel is implied.

`claude-repl--ensure-own-panels-on-persp-switch' uses this to replace
such a lone output window with the switched-to workspace's own
output+input panels in fullscreen (via
`claude-repl--reclaim-frame-fullscreen')."
  (let ((non-side (cl-remove-if #'claude-repl-window--side-window-p (window-list))))
    (when (and (= (length non-side) 1)
               (claude-repl--claude-buffer-p (window-buffer (car non-side))))
      (car non-side))))

(defun claude-repl--detach-foreign-panel-buffers (ws buffers)
  "Detach foreign Claude panel BUFFERS from WS's persp buffer list.
Each live buffer in BUFFERS is removed from the current workspace's
perspective via `claude-repl--ws-remove-buffer', so listing WS's buffers
no longer surfaces another workspace's Claude panel.  The buffers are
NOT killed and remain attached to their home workspace.  No-op for nil
or dead buffers."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (claude-repl--log ws "detach-foreign-panel-buffers: removing %s from ws=%s buffer list"
                        (buffer-name buf) ws)
      (claude-repl--ws-remove-buffer buf))))

(defun claude-repl--reclaim-frame-fullscreen (ws)
  "Take over the frame with WS's own Claude panels (fullscreen).

Called after a workspace switch found the frame in a state that should be
replaced by WS's own panels, namely either:
- a *different* workspace's Claude panel windows were purged, or
- a lone Claude output window (see `claude-repl--lone-output-window')
  remained, whether or not it belonged to WS.

Shows WS's own input+output panels via `claude-repl--show-panels',
which clears the frame's main area (deleting every non-panel window,
including any foreign output window left over from another workspace)
and lays out the panels filling the frame.

No-op when WS has no live panel buffers to show — there is nothing to
reclaim the frame with, so the existing layout is left as-is."
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
        (input-buf (claude-repl--ws-get ws :input-buffer)))
    (if (not (and vterm-buf (buffer-live-p vterm-buf)
                  input-buf (buffer-live-p input-buf)))
        (claude-repl--log ws "reclaim-frame-fullscreen: no live panel buffers for ws=%s, skipping" ws)
      (claude-repl--log ws "reclaim-frame-fullscreen: showing own panels for ws=%s" ws)
      (claude-repl--show-panels))))

(defun claude-repl--ensure-own-panels-on-persp-switch (ws)
  "Reconcile panel visibility with workspace ownership after a persp switch.

Closes any panel windows that belong to a *different* workspace —
persp-mode's `window-state-put' can leave stale panel windows when
the target workspace has no saved window config (first visit) or
when the saved config itself carried drifted panels from a prior
save.

When such foreign panels are found, also detaches their buffers from
THIS workspace's persp buffer list (via
`claude-repl--detach-foreign-panel-buffers') so listing this
workspace's buffers no longer surfaces another workspace's Claude
panel, and then takes over the frame with this workspace's own panels
in fullscreen (via `claude-repl--reclaim-frame-fullscreen').  The
foreign buffers are NOT killed and stay attached to their home
workspace.

Also handles a lone Claude output window — the frame showing just a
single Claude output (vterm) window with no input beside it, whether or
not that output belongs to the switched-to workspace (see
`claude-repl--lone-output-window').  Such a lone output is replaced by
THIS workspace's own output+input panels in fullscreen, again via
`claude-repl--reclaim-frame-fullscreen'.

After purging stale panels, restores this workspace's own panels if
they were visible when this workspace was last deactivated
\(`:panels-were-visible' flag set by `--before-persp-deactivate').

Mirrors the drawer's `ensure-visible-on-persp-switch' approach:
the drawer uses a global visibility flag; panels use a per-workspace
flag because each workspace has its own panel buffers."
  (let* ((stale (claude-repl--stale-panel-windows))
         (foreign-bufs (claude-repl--stale-window-buffers stale))
         ;; Captured BEFORE any repair below adds an input window beside a
         ;; lone output (which would make it no longer "lone").
         (lone-output (claude-repl--lone-output-window)))
    (when stale
      (claude-repl--log ws "ensure-own-panels: closing %d stale panel windows: %S"
                        (length stale)
                        (mapcar (lambda (w) (buffer-name (window-buffer w))) stale))
      (dolist (win stale)
        (when (window-live-p win)
          ;; Un-dedicate before deleting so `delete-window' doesn't error.
          (set-window-dedicated-p win nil)
          (delete-window win)))
      ;; Detach the foreign panel buffers from THIS workspace's persp
      ;; buffer list AFTER their windows are gone, so listing this
      ;; workspace's buffers no longer surfaces another workspace's
      ;; Claude panel.  The buffers stay alive in their home workspace.
      (claude-repl--detach-foreign-panel-buffers ws foreign-bufs))
    ;; If this workspace's panels were visible before its last deactivation
    ;; but are not visible now (persp dropped them or we just purged stale
    ;; ones), re-show them.
    (when (and (claude-repl--ws-get ws :panels-were-visible)
               (not (claude-repl--panels-visible-p))
               ;; Only restore if this ws actually has live panel buffers.
               (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
                     (input-buf (claude-repl--ws-get ws :input-buffer)))
                 (and vterm-buf (buffer-live-p vterm-buf)
                      input-buf (buffer-live-p input-buf))))
      (if (claude-repl--vterm-visible-p)
          ;; Output window survived the switch but the input window was
          ;; dropped — add only the input window so the output window is
          ;; not duplicated.
          (progn
            (claude-repl--log ws "ensure-own-panels: output up, input missing — adding input only")
            (claude-repl--show-input-beside-output))
        (claude-repl--log ws "ensure-own-panels: re-showing panels (were-visible but now missing)")
        (claude-repl--show-panels)))
    ;; Independently of the were-visible flag, repair a frame that shows
    ;; only the output window (e.g. a fullscreen Claude REPL restored with
    ;; just its output window) by adding the input window beside it.
    (claude-repl--ensure-input-beside-output)
    ;; Take over the frame with THIS workspace's own panels in fullscreen —
    ;; replacing every visible window with the input+output panels — when
    ;; either a foreign workspace's panels were just purged, or the frame
    ;; showed a lone Claude output window (own or foreign).  A single
    ;; reclaim call covers both, so a foreign lone output (which is BOTH
    ;; stale and lone) is reclaimed exactly once.
    (when (or stale lone-output)
      (claude-repl--reclaim-frame-fullscreen ws))))

(defun claude-repl--on-workspace-switch (&optional ws)
  "Handle workspace switch: update all workspace states, refresh vterm, reset cursors.
WS is the workspace name to operate on; when nil, falls back to
`(claude-repl--ws-current-name)' at call time.  Callers from
`--after-persp-activated' pass the ws captured at hook-fire time so
the deferred call operates on the workspace that was just switched
to, even if another switch raced ahead before the timer fired.

Also opens panels for workspaces that were created with a preemptive
prompt, and auto-selects the input window if visible.

If the newly-active workspace has `:claude-state :done', stamps
`:done-acked' to t and `:done-acked-at' to the current time so the
decay timer can clear :done → :idle once
`claude-repl-done-idle-delay' seconds of continuous focus have
elapsed.  The companion clear in `--before-persp-deactivate' resets
the timestamp on switch-away, so a quick transit through the tab
never causes a silent decay.

Also runs `claude-repl--maybe-sweep-hidden-on-switch' so workspaces
marked `:hidden' (via `SPC o C') are persp-killed when hide-mode is on
— the persp-level enforcement of hide-mode.

Runs `claude-repl--dequeue-merge' so a workspace parked in the merge
queue is pulled from it on switch — activating a queued workspace is
read as the user wanting to work on it directly rather than have its
pending merge auto-fire."
  (let ((ws (or ws (claude-repl--ws-current-name))))
    (claude-repl--log-verbose ws "workspace-switch ws=%s" ws)
    ;; Purge stale panel windows from other workspaces and restore own
    ;; panels if they were visible before this workspace was deactivated.
    ;; Must run BEFORE refresh-vterm / autoselect so they see the correct
    ;; panel windows.
    (claude-repl--ensure-own-panels-on-persp-switch ws)
    (when (eq (claude-repl--ws-claude-state ws) :done)
      (claude-repl--ws-put ws :done-acked t)
      (claude-repl--ws-put ws :done-acked-at (float-time)))
    (claude-repl--maybe-sweep-hidden-on-switch ws)
    (claude-repl--dequeue-merge ws)
    ;; Event-driven (workspace just activated) → kick a fresh pass via
    ;; the unguarded entrypoint so the in-flight reentry guard from the
    ;; 1Hz timer doesn't swallow the switch's refresh.
    (claude-repl--update-all-workspace-states-now)
    (claude-repl--refresh-vterm)
    (claude-repl--reset-vterm-cursors)
    (claude-repl--drain-pending-magit ws)
    (claude-repl--drain-pending-initial-buffers ws)
    (claude-repl--drain-pending-show-panels ws)
    (claude-repl--maybe-autoselect-input ws)
    ;; Flip the emacs-side bit on the fully-loaded latch.  If
    ;; --on-session-start-event has also fired, this fires the
    ;; ws-fully-loaded hook; otherwise we just record the bit and wait
    ;; for claude-ready.  Guarded on ws non-nil so the nil-ws fallback
    ;; (test envs, persp init) doesn't poison the hash table.
    (when ws
      (claude-repl--latch-and-maybe-fire-loaded ws :ws-loaded))))

;; Save window state for current workspace before switching away,
;; so update-all-workspace-states can inspect the saved config.

(defun claude-repl--non-claude-panel-window-p (w)
  "Return non-nil if window W does not display a Claude panel buffer."
  (not (claude-repl--claude-panel-buffer-p (window-buffer w))))

(defun claude-repl--save-target-window-p (w)
  "Return non-nil when W is a safe selected-window for persp save.

\"Safe\" means a later `switch-to-buffer' (e.g. Doom's `+workspace/kill'
fallback path) can repurpose the window in-place rather than splitting
a new one.  Excludes Claude panel buffers, side windows, dedicated
windows, and the minibuffer."
  (and (window-live-p w)
       (not (window-minibuffer-p w))
       (not (window-parameter w 'window-side))
       (not (window-dedicated-p w))
       (not (claude-repl--claude-panel-buffer-p (window-buffer w)))))

(defun claude-repl--redirect-from-claude-before-save ()
  "Select a redirect-safe window before persp saves window state.

Redirects when the selected window is unsuitable as a future
`switch-to-buffer' target: a Claude panel buffer, a side window
(e.g. the drawer), or a dedicated window.  Persp saves the selected
window into the workspace's restored layout; if that window is a
side/dedicated/panel window, Doom's `+workspace/kill' fallback
`(switch-to-buffer (doom-fallback-buffer))' cannot repurpose it and
instead splits a new window showing the doom splash buffer.

Picks the first window that satisfies `claude-repl--save-target-window-p'.
No-op when no safe target exists (fullscreen-claude or drawer-only)."
  (let ((sel (selected-window)))
    (when (or (claude-repl--claude-panel-buffer-p (window-buffer sel))
              (window-parameter sel 'window-side)
              (window-dedicated-p sel))
      (when-let ((target (cl-find-if
                          #'claude-repl--save-target-window-p
                          (window-list))))
        (select-window target)))))

(defun claude-repl--clear-done-ack-on-switch-away (ws)
  "Reset WS's :done focus-dwell tracking when switching away.
If WS is in `:claude-state :done' and the decay has not yet fired,
clear `:done-acked' and `:done-acked-at' so the dwell countdown
restarts on the next return.  This makes the
`claude-repl-done-idle-delay' check count only continuous focus
periods — a quick transit (< delay) leaves the workspace green."
  (when (and ws (eq (claude-repl--ws-claude-state ws) :done))
    (claude-repl--log ws "clear-done-ack-on-switch-away ws=%s" ws)
    (claude-repl--ws-put ws :done-acked nil)
    (claude-repl--ws-put ws :done-acked-at nil)))

(defun claude-repl--before-persp-deactivate (&rest _)
  "Save window state before perspective deactivation.
Redirects away from Claude buffers and saves frame state.  Also
records `:panels-were-visible' so `--ensure-own-panels-on-persp-switch'
can restore the correct workspace's panels after activation.
Clears the `:done' focus-dwell tracking on the outgoing workspace so
a sub-`claude-repl-done-idle-delay' transit never decays it.
Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (let ((ws (claude-repl--ws-current-name)))
    (claude-repl--log ws "before-persp-deactivate: entry cache=%S"
                      (or (claude-repl--ws-names-cache) "(unbound)"))
    (claude-repl--clear-done-ack-on-switch-away ws)
    ;; Record whether panels are visible BEFORE redirecting/saving so
    ;; the activated hook can restore them if persp-mode drops them.
    (claude-repl--ws-put ws :panels-were-visible (claude-repl--panels-visible-p))
    (claude-repl--redirect-from-claude-before-save)
    (condition-case err
        (claude-repl--ws-frame-save-state)
      (error (message "[claude-repl] WARNING: persp-frame-save-state failed: %S" err)
             (claude-repl--log ws "before-persp-deactivate: persp-frame-save-state error: %S" err)))))

(defun claude-repl--after-persp-activated (&rest _)
  "Handle perspective activation by scheduling a workspace switch.
Captures `(claude-repl--ws-current-name)' at hook-fire time and passes it
to the deferred `--on-workspace-switch' so the call operates on the
workspace that just activated, not whatever happens to be current
when the run-at-time-0 timer eventually fires (rapid back-to-back
switches would otherwise have every deferred call resolve to the
latest ws, dropping bookkeeping on the intermediate ones).

Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (claude-repl--log (claude-repl--ws-current-name) "after-persp-activated: entry cache=%S"
                    (or (claude-repl--ws-names-cache) "(unbound)"))
  (let ((ws (claude-repl--ws-current-name)))
    (run-at-time 0 nil #'claude-repl--on-workspace-switch ws)))

(when (modulep! :ui workspaces)
  (claude-repl--ws-add-before-deactivate-hook #'claude-repl--before-persp-deactivate)
  (claude-repl--ws-add-activated-hook #'claude-repl--after-persp-activated))

(defun claude-repl--hide-panels ()
  "Hide both Claude panels without killing buffers."
  (let* ((ws (claude-repl--ws-current-name))
         (input-buf (claude-repl--ws-get ws :input-buffer))
         (vterm-buf (claude-repl--ws-get ws :vterm-buffer)))
    (claude-repl--log ws "hide-panels")
    (claude-repl--close-buffer-windows input-buf vterm-buf)))

(defun claude-repl--save-tab-index (ws)
  "Persist WS's current tab-bar index to its plist as `:saved-tab-index'.
Read on reopen by `claude-repl--restore-tab-index' so the workspace
returns to its prior slot in the tab-bar after a close-deprio cycle.
Reads positions from `persp-names-current-frame-fast-ordered'; no-op
when that helper is unavailable (e.g. test envs without persp-mode).

Also writes the index to disk via `--state-save' so a deprioritized ws
that the user closes Emacs on still returns to its saved slot on
restart (without this, `:saved-tab-index' is in-memory only and the ws
loses its prior position across an Emacs lifecycle)."
  (when-let ((idx (cl-position ws (claude-repl--ws-frame-ordered-names)
                               :test #'string=)))
    (claude-repl--log ws "save-tab-index ws=%s index=%d" ws idx)
    (claude-repl--ws-put ws :saved-tab-index idx)
    (claude-repl--state-save ws)))

(defun claude-repl--restore-tab-index (ws)
  "Move WS back to its persisted `:saved-tab-index' slot, if any.
Clears `:saved-tab-index' after restoring so each close-deprio cycle
captures a fresh baseline.  No-op when no index is saved or when persp
helpers are unavailable.  Index is clamped to the current names list
length so a saved index past the new tail is handled gracefully.

Drives `claude-repl--force-tab-bar-redraw' so the trailing-space
toggle is flipped — the tab-bar's string-equality cache otherwise
risks holding the pre-restore order if the new tabline string happens
to compare equal under propertized-string semantics."
  (when-let ((idx (claude-repl--ws-get ws :saved-tab-index)))
    (when-let ((names (claude-repl--ws-frame-ordered-names)))
      (let* ((without-ws (remove ws names))
             (clamped (min idx (length without-ws)))
             (head (cl-subseq without-ws 0 clamped))
             (tail (cl-subseq without-ws clamped))
             (reordered (append head (list ws) tail)))
        (claude-repl--log ws "restore-tab-index ws=%s saved-idx=%d clamped=%d"
                          ws idx clamped)
        (claude-repl--ws-update-names-cache reordered)
        (claude-repl--ws-put ws :saved-tab-index nil)
        ;; Persist the cleared index so a future restart doesn't see a
        ;; stale value and re-restore (the ws is no longer deprioritized
        ;; once we've reseated it).
        (claude-repl--state-save ws)
        (when (fboundp 'claude-repl--force-tab-bar-redraw)
          (claude-repl--force-tab-bar-redraw))))))

(defun claude-repl--on-simple-close (&optional ws)
  "Bookkeep + hide panels; do NOT touch tab-bar order.
Sets `:repl-state :inactive' on WS (`:claude-state' untouched so an
in-flight :thinking / :permission survives the close), then hides
the panel windows.  No save-tab-index, no push-to-back, no flash —
this is the simple-close audit point that `SPC o c' is bound to.

The panels fill the frame (fullscreen is the only display format), so
first restores the pre-panel layout saved at open time via
`claude-repl--restore-fullscreen-config' — re-establishing the work
windows that were on screen before the panels took over — so the
subsequent `claude-repl--hide-panels' leaves those work windows behind
rather than hitting `delete-window's sole-ordinary-window refusal and
stranding a panel onscreen."
  (let ((ws (or ws (claude-repl--ws-current-name))))
    (claude-repl--log ws "on-simple-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (claude-repl--log ws "on-simple-close ws=%s claude-state=%s -> repl-state=:inactive"
                        ws (claude-repl--ws-claude-state ws))
      (claude-repl--ws-set-repl-state ws :inactive))
    (claude-repl--restore-fullscreen-config ws)
    (claude-repl--hide-panels)))

(defun claude-repl--on-close (&optional ws)
  "Full close: bookkeep + restore pre-panel layout + hide + deprio + save tab index.
Sets WS's `:repl-state' to `:hidden' (NOT `:inactive' like the
simple-close path) so the workspace becomes a kill candidate for the
next sweep when `claude-repl-hide-mode-enabled' is on.  Restores the
pre-panel layout via `claude-repl--restore-fullscreen-config' before
hiding so the frame-filling panels go away cleanly (same contract as
`claude-repl--on-simple-close').  See
`claude-repl--ws-set-repl-state' for the `:hidden' contract.  Then
hides panels and pushes WS to the second-to-last tab position via
`claude-repl-workspace-push-to-back', snapshotting the tab index
first via `claude-repl--save-tab-index' so a future reopen can
restore the position.

Bound to `SPC o C' (the deprio toggle); also fires from
`claude-repl-send-and-hide' since send-and-hide is semantically
\"I'm done with this prompt, move on\".

WS defaults to the current workspace; when WS is nil the function still
hides panels but skips the bookkeeping write and the tab shuffle."
  (let ((ws (or ws (claude-repl--ws-current-name))))
    (claude-repl--log ws "on-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (claude-repl--log ws "on-close ws=%s claude-state=%s -> repl-state=:hidden"
                        ws (claude-repl--ws-claude-state ws))
      (claude-repl--ws-set-repl-state ws :hidden))
    (claude-repl--restore-fullscreen-config ws)
    (claude-repl--hide-panels)
    (when (and ws (equal ws (claude-repl--ws-current-name)))
      (claude-repl--save-tab-index ws)
      (claude-repl--log ws "on-close: pushing ws=%s to second-to-last" ws)
      (claude-repl-workspace-push-to-back))))

(defun claude-repl--unhide-workspace (ws)
  "Reverse `claude-repl--on-close' for WS by setting `:repl-state' to `:active'.
A no-op when WS is nil or has a `:repl-state' other than `:hidden' —
non-hidden workspaces don't need unhiding, and overwriting
`:inactive' / `:dead' / nil with `:active' would lie about lifecycle
state.  Does NOT re-show panels or re-shuffle tab order; that side of
the close is reversible only by an explicit panel-show."
  (when ws
    (let ((rstate (claude-repl--ws-get ws :repl-state)))
      (claude-repl--log ws "unhide-workspace: ws=%s repl-state=%s" ws rstate)
      (when (eq rstate :hidden)
        (claude-repl--ws-set-repl-state ws :active)))))

;;;; Window synchronization

;; Auto-close orphaned panels: if one is closed, close the other.
;; Also refresh the hide overlay in case a window change invalidated it.
(defun claude-repl--extract-panel-id (name)
  "Extract the workspace identifier from a Claude panel buffer NAME.
Returns the identifier string, or nil if NAME is not a Claude panel buffer.
Input-buffer check comes first since `claude-repl--vterm-buffer-re' is a
superset that also matches input-buffer names."
  (cond
   ((string-match-p claude-repl--input-buffer-re name)
    (substring name (length "*claude-panel-input-") (- (length name) (length "*"))))
   ((string-match-p claude-repl--vterm-buffer-re name)
    (substring name (length "*claude-panel-") (- (length name) (length "*"))))))

(defun claude-repl--partner-buffer-name (name id)
  "Return the partner buffer name for Claude panel NAME with identifier ID.
For a vterm buffer, the partner is the input buffer, and vice versa.
Checks input-re first since vterm-re is a superset that also matches inputs."
  (if (string-match-p claude-repl--input-buffer-re name)
      (format "*claude-panel-%s*" id)
    (format "*claude-panel-input-%s*" id)))

(defun claude-repl--orphaned-panel-p (name)
  "Return non-nil if NAME is a Claude panel buffer whose partner is not visible.
Ignores single-window frames.  Input buffers are not orphaned while the
loading placeholder exists (the vterm has not been swapped in yet)."
  (when-let ((id (claude-repl--extract-panel-id name)))
    (let* ((is-input (string-match-p claude-repl--input-buffer-re name))
           (partner (claude-repl--partner-buffer-name name id))
           (result (and (not (one-window-p))
                        (not (get-buffer-window partner))
                        ;; Input panels are not orphaned while loading placeholder is live
                        (or (not is-input)
                            (not (get-buffer claude-repl-loading-placeholder-name))))))
      (when result
        (claude-repl--log-verbose (claude-repl--ws-current-name) "orphaned-panel-p: name=%s partner=%s is-orphaned" name partner))
      result)))

(defun claude-repl--sync-panels ()
  "Close any Claude panel whose partner is no longer visible.
Side windows (e.g. the drawer) can never be claude panels by
predicate construction, so the default `--delete-where' side-skip
costs nothing and remains defense-in-depth.

Logs each orphan's buffer name BEFORE the sweep (capturing names
while windows are still live) so the per-orphan log survives the
deletion that follows."
  (let* ((ws (claude-repl--ws-current-name))
         (orphan-names
          (cl-loop for win in (window-list)
                   for name = (buffer-name (window-buffer win))
                   when (claude-repl--orphaned-panel-p name)
                   collect name)))
    (claude-repl--log-verbose ws "sync-panels: entry windows=%d"
                              (length (window-list)))
    (dolist (name orphan-names)
      (claude-repl--log ws "sync-panels closing orphaned %s" name))
    (let ((deleted
           (claude-repl-window--delete-where
            (lambda (win)
              (claude-repl--orphaned-panel-p
               (buffer-name (window-buffer win)))))))
      (claude-repl--log-verbose ws "sync-panels: closed %d orphans"
                                (length deleted)))))

;; Keep visible Claude vterm buffers scrolled to the cursor.
;; Skips the selected window so clicking into vterm to read/copy isn't disrupted.
(defun claude-repl--refresh-vterm-window (win)
  "Refresh the Claude vterm buffer shown in WIN.
Resets cursor, redraws, and snaps `window-start' so the cursor lands on
the last visible line — replaces the bare `set-window-point' tail with
`claude-repl--snap-vterm-window-to-cursor' so the new view appears in a
single redisplay rather than animating a scroll from the saved
`window-start' down to the cursor."
  (let ((buf (window-buffer win)))
    (when (and buf (buffer-live-p buf) (claude-repl--claude-buffer-p buf))
      (claude-repl--log-verbose (claude-repl--ws-current-name) "refresh-vterm-window: win=%s buf=%s" win (buffer-name buf))
      (with-current-buffer buf
        (when (and (eq major-mode 'vterm-mode)
                   (fboundp 'vterm-reset-cursor-point))
          (condition-case nil
              (progn
                (vterm-reset-cursor-point)
                (claude-repl--vterm-redraw)
                (vterm-reset-cursor-point)
                (claude-repl--snap-vterm-window-to-cursor win))
            (end-of-buffer nil)))))))

(defun claude-repl--reset-vterm-cursors ()
  "Refresh every visible Claude vterm window except the selected one."
  (claude-repl--log-verbose (claude-repl--ws-current-name) "reset-vterm-cursors: entry")
  (let ((sel (selected-window)))
    (dolist (win (window-list))
      (unless (eq win sel)
        (claude-repl--refresh-vterm-window win)))))

(defvar claude-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defun claude-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Syncs orphaned panels and refreshes overlay.

Does NOT reset vterm cursors.  `claude-repl--reset-vterm-cursors'
calls `vterm-reset-cursor-point' + `set-window-point' on every visible
non-selected Claude vterm window; that pulls window-start back to the
bottom of the buffer, undoing any user scroll-up (e.g. via `C-S-k').
The reset is only useful right after a workspace switch (to recenter
the new vterm on its prompt), so it lives in
`claude-repl--on-workspace-switch' alone — not on every window-config
change, selection change, or buffer-list update."
  (claude-repl--log-verbose (claude-repl--ws-current-name) "on-window-change")
  (claude-repl--sync-panels)
  (claude-repl--update-hide-overlay))

(defmacro claude-repl--deferred (timer-var fn)
  "Return a lambda that debounces calls to FN via TIMER-VAR.
Cancels any pending timer and schedules FN to run at next idle."
  `(lambda (&rest _)
     (when ,timer-var
       (cancel-timer ,timer-var))
     (setq ,timer-var (run-at-time 0 nil ,fn))))

(defalias 'claude-repl--debounced-on-window-change
  (claude-repl--deferred claude-repl--sync-timer #'claude-repl--on-window-change)
  "Debounced handler for `window-configuration-change-hook'.
Cancels any pending timer and schedules `claude-repl--on-window-change'.")

(add-hook 'window-configuration-change-hook
          #'claude-repl--debounced-on-window-change)

;; Redirect keyboard navigation away from the vterm output window.
;; Mouse clicks (checked via last-input-event) are allowed through so the
;; user can still click into the output when needed.
(defun claude-repl--bounce-from-vterm (_frame)
  "If the selected window shows a Claude vterm buffer, redirect to the input window.
Allows mouse-initiated selection through so clicking into the output to
scroll or copy works.  When no input window is currently displayed
\(e.g. panels are hidden), emits a warning via `message' rather than
leaving point stranded silently.

Predicate is buffer-identity (`claude-repl--claude-buffer-p' — vterm-only,
excludes input buffers) rather than the `no-other-window' parameter, so
this bounce alone is sufficient to keep keyboard nav out of vterm."
  (let ((win (selected-window)))
    (if (and (claude-repl--claude-buffer-p (window-buffer win))
             (not (mouse-event-p last-input-event)))
        (let* ((ws (claude-repl--ws-current-name))
               (input-buf (and ws (claude-repl--ws-get ws :input-buffer)))
               (input-win (and input-buf (get-buffer-window input-buf))))
          (if input-win
              (progn
                (claude-repl--log-verbose (claude-repl--ws-current-name) "bounce-from-vterm: bouncing to input-win=%s" input-win)
                (select-window input-win))
            (message "[claude-repl] keyboard navigation landed in Claude vterm but input panel isn't visible — stuck here until you click out or reopen panels")
            (claude-repl--log (claude-repl--ws-current-name) "bounce-from-vterm: no input-win to bounce to (warned)")))
      (claude-repl--log-verbose (claude-repl--ws-current-name) "bounce-from-vterm: skipped vterm-buffer=%s mouse=%s"
                                (if (claude-repl--claude-buffer-p (window-buffer win)) "yes" "no")
                                (if (mouse-event-p last-input-event) "yes" "no")))))

(add-hook 'window-selection-change-functions #'claude-repl--bounce-from-vterm)

;;;; Buffer creation

(defun claude-repl--initialize-input-buffer (ws)
  "Create the Claude input buffer for workspace WS and enable claude-input-mode.
Errors if the buffer is already initialized (already in `claude-input-mode')."
  (claude-repl--log ws "initialize-input-buffer")
  (let ((input-buf (claude-repl--create-buffer ws "-input")))
    (claude-repl--ws-put ws :input-buffer input-buf)
    (with-current-buffer input-buf
      (when (eq major-mode 'claude-input-mode)
        (error "claude-repl--initialize-input-buffer: already initialized ws=%s" ws))
      (claude-input-mode)
      (claude-repl--history-restore ws))))

(defun claude-repl--kill-stale-vterm (&optional ws)
  "Kill the Claude vterm buffer for WS if it exists but has no live process.
WS defaults to the current workspace."
  (let ((existing (get-buffer (claude-repl--buffer-name nil ws))))
    (if (not existing)
        (claude-repl--log (claude-repl--ws-current-name) "kill-stale-vterm: no existing buffer")
      (if (get-buffer-process existing)
          (claude-repl--log (claude-repl--ws-current-name) "kill-stale-vterm: buf=%s has live process no-op" (buffer-name existing))
        (claude-repl--log (claude-repl--ws-current-name) "kill-stale-vterm: killing stale buf=%s" (buffer-name existing))
        (kill-buffer existing)))))

;;;; Panel show/hide strategies

(defun claude-repl--show-loading-panels ()
  "Show panels using a loading placeholder in the vterm slot.
The placeholder is swapped for the real vterm buffer once Claude is ready."
  (let* ((ws (claude-repl--ws-current-name))
         (real-vterm (claude-repl--ws-get ws :vterm-buffer))
         (placeholder (get-buffer-create claude-repl-loading-placeholder-name)))
    (claude-repl--log ws "show-loading-panels")
    (with-current-buffer placeholder
      (setq-local mode-line-format nil)
      (claude-repl--set-buffer-background claude-repl--vterm-background-grey))
    (claude-repl--ws-put ws :vterm-buffer placeholder)
    (claude-repl--show-panels-and-focus)
    (claude-repl--ws-put ws :vterm-buffer real-vterm)))

(defun claude-repl--initialize-claude (&optional ws project-dir-hint active-env-hint)
  "Initialize a Claude session for WS.
Calls `initialize-ws-env' with PROJECT-DIR-HINT and ACTIVE-ENV-HINT
(creation paths — worktree setup or new-workspace — pass known values
here; regular `SPC o c' passes nil and lets the helper derive from
the state file or the current buffer's git-root).  Then creates the
output vterm buffer, launches the Claude CLI inside it, creates the
input buffer, enables the hide-overlay, marks `:claude-state' as
`:init', and announces the startup.  Errors if Claude is already
running for WS.

Writes `:claude-state :init' immediately after launching the vterm
process (documented lifecycle exception to the sentinel-only-writes
rule — no hook fires between process launch and session-start, so
Emacs is the only observer of \"Claude process exists, not ready yet\").
Panels are deliberately NOT opened here — `on-session-start-event'
opens them once `:claude-state' transitions from `:init' to `:idle'.
During that window the user sees the blue `:init' tab and the
echo-area message below."
  (let ((ws (or ws (claude-repl--ws-current-name))))
    (unless ws (error "claude-repl--initialize-claude: no active workspace"))
    (when (claude-repl--claude-running-p ws)
      (error "claude-repl--initialize-claude: already running ws=%s" ws))
    (claude-repl--log ws "initialize-claude: starting new session for ws=%s" ws)
    (claude-repl--initialize-ws-env ws project-dir-hint active-env-hint)
    (let* ((root (claude-repl--ws-dir ws))
           (default-directory root))
      (claude-repl--kill-stale-vterm ws)
      (let* ((vterm-buf (claude-repl--create-buffer ws))
             (start-info (claude-repl--build-start-cmd ws))
             (cmd         (plist-get start-info :cmd))
             (inst        (plist-get start-info :inst)))
        (claude-repl--ws-put ws :vterm-buffer vterm-buf)
        (setf (claude-repl-instantiation-start-cmd inst) cmd)
        (when (plist-get start-info :fork-session-id)
          (claude-repl--log ws "initialize-claude: clearing fork-session-id for ws=%s" ws)
          (claude-repl--ws-put ws :fork-session-id nil))
        (claude-repl--log-session-start ws start-info)
        (with-current-buffer vterm-buf
          (when (eq major-mode 'vterm-mode)
            (error "claude-repl--initialize-claude: vterm buffer already initialized ws=%s" ws))
          (vterm-mode)
          (setq-local truncate-lines nil)
          (setq-local word-wrap t)
          (claude-repl--set-buffer-background claude-repl--vterm-background-grey)
          (setq-local mode-line-format
                      (claude-repl--workspace-mode-line ws))
          (setq-local claude-repl--ready nil)
          (claude-repl--log ws "initialize-claude: vterm=%s sending cmd len=%d"
                            (buffer-name) (length cmd))
          (vterm-send-string (concat claude-repl-startup-prefix cmd))
          (vterm-send-return))
        (claude-repl--schedule-ready-timer ws)
        (claude-repl--initialize-input-buffer ws)
        (claude-repl--ws-put ws :prefix-counter 0)
        (claude-repl--enable-hide-overlay)
        (claude-repl--ws-set-claude-state ws :init)
        (message "Starting Claude... ws=%s ws-id=%s dir=%s cmd=%s"
                 ws (claude-repl--workspace-id) root (or cmd "?"))
        (claude-repl--state-save ws)))))

(defun claude-repl--clear-main-area-for-panels ()
  "Delete every non-side window other than the selected one.
Side-window-aware replacement for `delete-other-windows' on the
panel-show path: the workspace drawer (a left-side window) must
survive panel reopen.  `delete-other-windows' relies on each side
window carrying `no-delete-other-windows', which is fragile —
window-parameter loss anywhere upstream (e.g. a buffer redisplayed
without the original action alist) leaves the drawer vulnerable.
Routing through `claude-repl-window--delete-where' makes the
side-window skip explicit and parameter-independent."
  (claude-repl-window--delete-where
   (lambda (win) (not (eq win (selected-window))))))

(defun claude-repl--show-existing-panels ()
  "Show panels for an already-running Claude session.
Demotes indicators, refreshes display, and restores panel layout.
Sets `:repl-state :active' now that panels are visible and the
session is in use.

Tab-bar bookkeeping happens FIRST (before any window manipulation) so
the persp-names reorder is in place before `show-panels-and-focus'
triggers redisplay — otherwise the intermediate paint can lock the
pre-restore order into the tab-bar's cache.  After panels are up,
pulses the tab via `claude-repl-flash-tab' so the user can track its
return to the prior slot — symmetric with the deprio-on-close flash.

The frame's main area is cleared by `claude-repl--show-panels' itself
(via `--clear-main-area-for-panels', which preserves the drawer side
window) AFTER it has captured the pre-panel layout as
`:fullscreen-config', so this function must NOT clear the main area
first — doing so would destroy the work layout before it is saved."
  (let ((ws (claude-repl--ws-current-name)))
    (claude-repl--log ws "show-existing-panels")
    (unless ws (error "claude-repl--show-existing-panels: no active workspace"))
    (claude-repl--ws-set-repl-state ws :active)
    (claude-repl--restore-tab-index ws)
    (claude-repl--refresh-vterm)
    (claude-repl--show-panels-and-focus)
    (claude-repl--update-hide-overlay)
    (claude-repl--flash-current-tab)))

(defun claude-repl--show-hidden-panels ()
  "Restore hidden panels.  `show-existing-panels' writes :repl-state :active.
`:claude-state' is untouched; rendering follows the same rule whether
panels are visible or hidden.

Panels always open filling the frame (fullscreen is the sole display
format), so there is no separate maximize step — `show-existing-panels'
lays them out full-frame via `claude-repl--show-panels'."
  (let ((ws (claude-repl--ws-current-name)))
    (claude-repl--log ws "showing panels ws=%s claude-state=%s"
                      ws (claude-repl--ws-claude-state ws))
    (claude-repl--show-existing-panels)))

(defun claude-repl--hide-and-preserve-status ()
  "Hide Claude panels with full deprio + tab-bar shuffle (the `SPC o C' path).
Thin wrapper around `claude-repl--on-close' that enforces the invariant
that a workspace is active.  See `claude-repl--simple-hide-and-preserve-status'
for the no-tab-bar-update variant bound to `SPC o c'."
  (let ((ws (claude-repl--ws-current-name)))
    (unless ws (error "claude-repl--hide-and-preserve-status: no active workspace"))
    (claude-repl--on-close ws)))

(defun claude-repl--simple-hide-and-preserve-status ()
  "Hide Claude panels with NO tab-bar update (the `SPC o c' path).
Thin wrapper around `claude-repl--on-simple-close' that enforces the
invariant that a workspace is active.  See
`claude-repl--hide-and-preserve-status' for the deprio + flash variant
bound to `SPC o C'."
  (let ((ws (claude-repl--ws-current-name)))
    (unless ws (error "claude-repl--simple-hide-and-preserve-status: no active workspace"))
    (claude-repl--on-simple-close ws)))

;;;; Entry point

(cl-defun claude-repl--toggle (close-fn &key always-close)
  "Generic toggle.  CLOSE-FN handles the visible-panels case.
Open / start / send-selection paths are shared.  Used by both
`claude-repl' (deprio close) and `claude-repl-simple' (plain close).

When ALWAYS-CLOSE is non-nil, every non-selection branch routes to
CLOSE-FN regardless of running / starting / panel-visibility state —
the workspace is hidden even if Claude isn't visible (or isn't running
at all).  This is the `SPC o C' contract: pressing it again on a
workspace that is already hidden / never-started should still mark it
`:hidden' and push it to the back, not re-show or launch Claude."
  (let* ((ws (claude-repl--ws-current-name))
         (vterm-running (claude-repl--claude-running-p))
         (session-starting (claude-repl--session-starting-p))
         (panels-visible (claude-repl--panels-visible-p))
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (claude-repl--log ws "claude-repl running=%s starting=%s visible=%s selection=%s always-close=%s"
                      vterm-running session-starting panels-visible
                      (if selection "yes" "no") (if always-close "yes" "no"))
    (cond
     (selection
      (deactivate-mark)
      (claude-repl--send-to-claude selection))
     (always-close
      (funcall close-fn))
     ((not vterm-running)
      (claude-repl--initialize-claude))
     (session-starting
      (message "Claude is loading…"))
     (panels-visible
      (funcall close-fn))
     ;; Output window is up but the input window was dropped (e.g. a
     ;; fullscreen frame with only the output window).  Add the input
     ;; window alongside the existing output window and focus it —
     ;; don't rebuild the whole layout (which would duplicate the
     ;; already-visible output window).
     ((claude-repl--output-visible-input-hidden-p)
      (claude-repl--show-input-beside-output)
      (claude-repl--focus-input-panel))
     (t
      (claude-repl--show-hidden-panels)))))

(defun claude-repl ()
  "Hide Claude REPL panels and deprio the workspace.
If text is selected: send it directly to Claude (orthogonal to hide).
Otherwise: mark the workspace `:repl-state :hidden', hide both panels
\(no-op if already hidden), and push the workspace tab to the back.
Always hides, regardless of whether Claude is running or panels are
currently visible — if hide-mode is on, the next workspace switch will
persp-kill the workspace via `claude-repl--sweep-hidden-workspaces'.
Bound to `SPC o C'.  See `claude-repl-simple' for the no-tab-bar variant."
  (interactive)
  (claude-repl--toggle #'claude-repl--hide-and-preserve-status :always-close t))

(defun claude-repl-simple ()
  "Toggle Claude REPL panels with a plain close (no tab-bar update).
Same dispatch as `claude-repl' except the close branch only hides the
panels and sets `:repl-state :inactive' — no save-tab-index, no
push-to-back, no flash.  Bound to `SPC o c'."
  (interactive)
  (claude-repl--toggle #'claude-repl--simple-hide-and-preserve-status))

;;;; Session cleanup

(defun claude-repl--kill-placeholder ()
  "Close and kill the loading placeholder buffer if it exists."
  (claude-repl--log (claude-repl--ws-current-name) "kill-placeholder exists=%s" (if (get-buffer claude-repl-loading-placeholder-name) "yes" "no"))
  (when-let ((placeholder (get-buffer claude-repl-loading-placeholder-name)))
    (claude-repl--close-buffer-window placeholder)
    (kill-buffer placeholder)))

(defun claude-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (when (process-live-p proc)
    (claude-repl--log (claude-repl--ws-current-name) "sigkill fallback for lingering process")
    (signal-process proc 'SIGKILL)))

(defun claude-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (claude-repl--log (claude-repl--ws-current-name) "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time claude-repl-sigkill-delay nil #'claude-repl--sigkill-if-alive proc))

(defun claude-repl--kill-vterm-process (buf)
  "Kill the vterm buffer BUF and its process.
Suppresses both the standard process-exit query (via
`set-process-query-on-exit-flag') and any other
`kill-buffer-query-functions' (e.g., vterm's own kill query) so the
nuke path never prompts about the claude process."
  (claude-repl--log (claude-repl--ws-current-name) "kill-vterm-process buf=%s" (claude-repl--safe-buffer-name buf))
  (when (and buf (buffer-live-p buf))
    (let ((proc (get-buffer-process buf))
          (kill-buffer-query-functions nil))
      (when proc
        (set-process-query-on-exit-flag proc nil))
      (kill-buffer buf)
      (when proc
        (claude-repl--schedule-sigkill proc)))))

(defun claude-repl--teardown-session-state (ws)
  "Save history, disable overlay, cancel timers, and clear session state for workspace WS."
  (claude-repl--log ws "teardown-session-state ws=%s env=%s"
                    ws (claude-repl--ws-get ws :active-env))
  (condition-case err
      (claude-repl--disable-hide-overlay)
    (error (message "[claude-repl] WARNING: disable-hide-overlay failed during teardown: %S" err)))
  (when claude-repl--sync-timer
    (cancel-timer claude-repl--sync-timer)
    (setq claude-repl--sync-timer nil))
  ;; Update instantiation and persist state BEFORE clearing buffer refs,
  ;; since state-save needs the vterm buffer to resolve the project root.
  (let ((inst (claude-repl--active-inst ws)))
    (setf (claude-repl-instantiation-start-cmd inst) nil))
  (claude-repl--state-save ws)
  (claude-repl--ws-put ws :vterm-buffer nil)
  (claude-repl--ws-put ws :input-buffer nil))

(defun claude-repl--destroy-session-buffers (vterm-buf input-buf)
  "Close windows and kill VTERM-BUF, INPUT-BUF, and any placeholder."
  (claude-repl--log (claude-repl--ws-current-name) "destroy-session-buffers")
  (claude-repl--close-buffer-windows vterm-buf input-buf)
  (claude-repl--kill-placeholder)
  (claude-repl--kill-vterm-process vterm-buf)
  (when (and input-buf (buffer-live-p input-buf))
    (kill-buffer input-buf)))

(defun claude-repl--kill-session (ws)
  "Cancel timers, tear down state, and destroy buffers for workspace WS.
Captures the current buffer references before teardown clears them."
  (claude-repl--log ws "kill-session: ws=%s" ws)
  (let ((vterm-buf (claude-repl--ws-get ws :vterm-buffer))
        (input-buf (claude-repl--ws-get ws :input-buffer)))
    (claude-repl--cancel-ready-timer ws)
    (claude-repl--teardown-session-state ws)
    (claude-repl--destroy-session-buffers vterm-buf input-buf)))

(defun claude-repl--kill-workspace-buffers (ws)
  "Kill every buffer (and attached process) belonging to persp WS.
Idempotent: no-op when persp-mode is inactive, the persp does not
exist, or the persp slot holds a symbol sentinel rather than a real
perspective.  Each buffer is killed inside its own `condition-case' so
one bad buffer cannot block the rest.  File-visiting buffers are
marked unmodified before killing so `kill-buffer' does not prompt —
the user has already confirmed the destructive nuke.

Claude buffers owned by a different workspace (see
`claude-repl--foreign-owned-buffer-p') are skipped, not killed: persp-mode
can drift another workspace's live panel into this persp, and nuking it
would wipe that workspace's running session."
  (when (claude-repl--ws-system-available-p)
    (when-let ((persp (claude-repl--ws-resolve-persp ws)))
      (let ((bufs (claude-repl--ws-buffers persp))
            (kill-buffer-query-functions nil))
        (claude-repl--log ws "kill-workspace-buffers: count=%d" (length bufs))
        (dolist (buf bufs)
          (condition-case err
              (if (claude-repl--foreign-owned-buffer-p buf ws)
                  (claude-repl--log ws "kill-workspace-buffers: SKIP foreign buf=%s owner=%s"
                                    (claude-repl--safe-buffer-name buf)
                                    (claude-repl--buffer-owner buf))
                (let* ((buf-name (claude-repl--safe-buffer-name buf))
                       (live (buffer-live-p buf))
                       (proc (and live (get-buffer-process buf)))
                       (t-buf (float-time)))
                  (claude-repl--log ws "kill-workspace-buffers: buf=%s live=%s proc=%s"
                                    buf-name (if live "t" "nil")
                                    (if proc (process-name proc) "nil"))
                  (when live
                    (when proc
                      (set-process-query-on-exit-flag proc nil)
                      (ignore-errors (delete-process proc))
                      (claude-repl--schedule-sigkill proc))
                    (with-current-buffer buf
                      (set-buffer-modified-p nil))
                    (kill-buffer buf))
                  (claude-repl--log ws "kill-workspace-buffers: buf=%s done elapsed=%.3fs"
                                    buf-name (- (float-time) t-buf))))
            (error
             (claude-repl--log ws "kill-workspace-buffers: error on %s: %S"
                               (claude-repl--safe-buffer-name buf) err))))
        (claude-repl--log ws "kill-workspace-buffers: dolist done count=%d" (length bufs))))))

;;;; User commands

(defun claude-repl-kill ()
  "Kill Claude REPL buffers and windows for the current workspace."
  (interactive)
  (let ((ws (claude-repl--ws-current-name)))
    (claude-repl--log ws "kill")
    (unless ws (error "claude-repl-kill: no active workspace"))
    ;; Lifecycle-reset: kill destroys the session, so both state axes are
    ;; reset to nil.  (Documented exception to "sentinel-only writes
    ;; claude-state" — see analysis/12.)  :repl-state nil means "no panels
    ;; and no particular inactive/dead designation"; the workspace returns
    ;; to a pristine no-Claude state awaiting the next initialize-claude.
    (claude-repl--ws-put ws :claude-state nil)
    (claude-repl--ws-put ws :repl-state nil)
    (force-mode-line-update t)
    (claude-repl--kill-session ws)))

(defun claude-repl-restart ()
  "Hard restart Claude for the current workspace.
Kills the process, windows, and buffers for the current session and
re-initializes. The Claude state file on disk is preserved so the new
process resumes via `--continue'. Panels reopen once the new session
signals ready."
  (interactive)
  (let ((ws (claude-repl--ws-current-name)))
    (claude-repl--log ws "restart")
    (claude-repl-kill)
    (claude-repl--initialize-claude ws)))

(defun claude-repl-focus-input ()
  "Focus the Claude input buffer, or return to previous window if already there.
If Claude isn't running, start it (same as `claude-repl')."
  (interactive)
  (let ((ws (claude-repl--ws-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (claude-repl--ws-get ws :input-buffer))
      (claude-repl--log ws "focus-input branch=jump-back")
      (evil-window-left 1))
     ;; Not running — start fresh
     ((not (claude-repl--claude-running-p))
      (claude-repl--log ws "focus-input branch=initialize-claude")
      (claude-repl))
     ;; Running but panels hidden — show them
     (t
      (claude-repl--log ws "focus-input branch=show-or-focus")
      (unless (claude-repl--panels-visible-p)
        (claude-repl--show-panels))
      (when-let ((win (get-buffer-window (claude-repl--ws-get ws :input-buffer))))
        (select-window win))))))

(defun claude-repl--restore-fullscreen-config (ws)
  "Restore WS's saved pre-panel layout, clearing `:fullscreen-config'.
Returns non-nil when a restore happened, nil when WS had no saved config.

`:fullscreen-config' is the window layout captured by
`claude-repl--show-panels' the moment the frame-filling panels were
opened (fullscreen is the sole display format).  The close paths
\(`claude-repl--on-simple-close' for `SPC o c' and
`claude-repl--on-close' for `SPC o C') restore it before hiding so the
work windows the panels covered come back rather than the close
stranding a panel onscreen.  Only the saved-config case is handled: a
frame with no `:fullscreen-config' has no layout to restore to."
  (when-let ((saved (and ws (claude-repl--ws-get ws :fullscreen-config))))
    (set-window-configuration saved)
    (claude-repl--ws-put ws :fullscreen-config nil)
    t))

(defvar claude-repl--window-fullscreen-config nil
  "Saved window configuration for non-Claude fullscreen toggle.
Set when `claude-repl-fullscreen-and-focus' maximizes a non-Claude window,
cleared on restore.")

(defun claude-repl--fullscreen-leave-side-window ()
  "Move out of a side window before fullscreening.

When `claude-repl-fullscreen-and-focus' is invoked from inside a side
window (e.g. the workspace drawer), `selected-window' is the side
window itself.  The non-Claude branch would then treat the drawer as
the window to KEEP and sweep every main-area window — leaving the
user's actual work window (or Claude panels) destroyed and only the
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
  (when (claude-repl-window--side-window-p (selected-window))
    (when-let* ((main (and (fboundp 'window-main-window) (window-main-window)))
                (leaf (claude-repl--first-live-leaf main)))
      (select-window leaf))))

(defun claude-repl--first-live-leaf (win)
  "Return the first live leaf window beneath WIN.
A live leaf is one that displays a buffer (`window-live-p').  If WIN
is itself live, returns WIN.  Otherwise descends `window-child' until
a leaf is reached.  Returns nil if no leaf is found."
  (cond
   ((null win) nil)
   ((window-live-p win) win)
   (t (claude-repl--first-live-leaf (window-child win)))))

(defun claude-repl-fullscreen-and-focus ()
  "Focus the Claude input window, or maximize a non-Claude work window.
When in a Claude panel buffer, moves point to the input buffer — the
Claude panels already fill the frame (fullscreen is the sole display
format), so there is nothing to maximize.
When not in a Claude panel buffer, maximizes the current window within
the non-side area (preserving the workspace drawer) and saves the
layout; calling again restores it.
When invoked from a side window (e.g. the workspace drawer), first
moves point to the frame's main window so the maximize target is a
real main-area window — see
`claude-repl--fullscreen-leave-side-window'."
  (interactive)
  (claude-repl--fullscreen-leave-side-window)
  (if (claude-repl--claude-panel-buffer-p)
      (let* ((ws (claude-repl--ws-current-name))
             (input-buf (claude-repl--ws-get ws :input-buffer))
             (input-win (and input-buf (get-buffer-window input-buf))))
        (when input-win
          (select-window input-win)))
    (if claude-repl--window-fullscreen-config
        (progn
          (set-window-configuration claude-repl--window-fullscreen-config)
          (setq claude-repl--window-fullscreen-config nil))
      (setq claude-repl--window-fullscreen-config (current-window-configuration))
      (let ((keep (selected-window)))
        (claude-repl-window--delete-where
         (lambda (win) (not (eq win keep))))))))

(defun claude-repl-cycle ()
  "Send backtab to Claude vterm to cycle through options."
  (interactive)
  (claude-repl--log (claude-repl--ws-current-name) "cycle")
  (when (claude-repl--vterm-live-p)
    (with-current-buffer (claude-repl--ws-get (claude-repl--ws-current-name) :vterm-buffer)
      (vterm-send-key "<backtab>"))))

(defun claude-repl--validate-env-switch (ws new-env worktree-p session-id)
  "Validate that workspace WS can switch to NEW-ENV.
WORKTREE-P and SESSION-ID describe the current workspace state.
Signals `user-error' if any precondition is not met."
  (claude-repl--log ws "validate-env-switch: ws=%s new-env=%s" ws new-env)
  (unless worktree-p
    (user-error "Sandbox switching requires a worktree workspace"))
  (unless session-id
    (user-error "No session ID captured yet — session may still be starting"))
  (when (claude-repl--ws-get ws :thinking)
    (user-error "Cannot switch environment while Claude is thinking"))
  (when (and (eq new-env :sandbox)
             (not (claude-repl--resolve-sandbox-config
                   (claude-repl--ws-get ws :project-dir))))
    (user-error "No sandbox configuration found for this workspace")))

(defun claude-repl--seed-new-env-session (ws new-env session-id)
  "Ensure the NEW-ENV instantiation for WS has a session-id.
If this is the first switch, seeds the new environment's session-id
from SESSION-ID.  The value signals to `compute-claude-flags' that
the env should emit `--continue' on start — which picks up the most
recent session in the worktree's cwd (i.e. the one we just left)."
  (let ((new-inst (or (claude-repl--ws-get ws new-env)
                      (make-claude-repl-instantiation))))
    (if (claude-repl-instantiation-session-id new-inst)
        (claude-repl--log ws "seed-new-env-session: ws=%s new-env=%s reusing existing session-id" ws new-env)
      (claude-repl--log ws "seed-new-env-session: ws=%s new-env=%s seeding new session-id=%s"
                        ws new-env session-id)
      (setf (claude-repl-instantiation-session-id new-inst) session-id))
    (claude-repl--ws-put ws new-env new-inst)))

(defun claude-repl-switch-environment ()
  "Switch the current workspace between Docker sandbox and bare-metal.
Kills the current Claude process and resumes it in the other environment.
On the first switch, the new environment seeds its session-id from the
current one so `--continue' in the other env picks up the conversation.
On subsequent switches each environment resumes its own prior session
independently.  Requires a worktree workspace with a captured session ID."
  (interactive)
  (let* ((ws (claude-repl--ws-current-name))
         (active-env (claude-repl--ws-get ws :active-env))
         (_ (claude-repl--log ws "switch-environment: ws=%s active-env=%s" ws active-env))
         (worktree-p (claude-repl--ws-get ws :worktree-p))
         (inst (claude-repl--active-inst ws))
         (session-id (claude-repl-instantiation-session-id inst))
         (new-env (if (eq active-env :sandbox) :bare-metal :sandbox)))
    (claude-repl--validate-env-switch ws new-env worktree-p session-id)
    (claude-repl--seed-new-env-session ws new-env session-id)
    (claude-repl--kill-session ws)
    (claude-repl--ws-put ws :active-env new-env)
    (message "Switching to %s (resuming session %s...)"
             (if (eq new-env :sandbox) "Docker sandbox" "bare-metal")
             (substring session-id 0 claude-repl-session-id-display-length))
    (claude-repl--initialize-claude ws)
    (claude-repl--show-panels-and-focus)))
