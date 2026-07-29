;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

(defcustom agent-repl-input-height-fraction 0.23
  "Fraction of the agent view window's height allocated to the input panel."
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
    (agent-repl--log-verbose (agent-repl--ws-current-log-name) "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
    result))

(defun agent-repl--input-visible-p ()
  "Return t if input buffer for the current workspace is visible in a window."
  (agent-repl--ws-buffer-visible-p :input-buffer))

(defun agent-repl--view-visible-p ()
  "Return t when the current workspace's agent VIEW (the webview) is visible."
  (agent-repl--ws-buffer-visible-p :frontend-buffer))

(defun agent-repl--panels-visible-p ()
  "Return t if both the input panel and the agent view are visible."
  (let ((result (and (agent-repl--input-visible-p)
                     (agent-repl--view-visible-p))))
    (agent-repl--log-verbose (agent-repl--ws-current-log-name) "panels-visible-p: result=%s" (if result "visible" "hidden"))
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
  (agent-repl--log (agent-repl--ws-current-log-name) "close-buffer-windows %s" (mapcar #'agent-repl--safe-buffer-name bufs))
  (dolist (buf bufs)
    (when (and buf (buffer-live-p buf))
      (agent-repl--close-buffer-window buf))))

(defun agent-repl--ensure-input-buffer (ws)
  "Return a live input buffer for WS, adopting or (re)creating one if needed.

The panel-show path builds the input window by `split-window'-ing the
window showing the agent view, so the new window transiently inherits
that view buffer until the input buffer is set into it.  If WS's
`:input-buffer' is dead or nil at that moment, `set-window-buffer'
errors and leaves the view buffer stranded in the adjacent window —
the duplicated-output corruption seen when switching to a freshly
generated workspace with a side window open.  Guaranteeing a live input
buffer here keeps that reassignment from ever failing.

Resolution order, loud rather than silent about a missing buffer:
- the recorded `:input-buffer' when it is still live;
- else the canonically-named `*agent-panel-input-WS*' buffer when a
  live one already exists (re-adopting it without re-running
  `agent-repl-input-mode', which would trip its already-initialized guard);
- else a fresh buffer via `agent-repl--initialize-input-buffer'.

Whichever buffer is resolved, its `default-directory' is realigned to
WS's project root via `agent-repl--align-buffer-to-ws-dir' before it is
returned.  The input buffer inherits `default-directory' from whatever
buffer was current when it was created, so without this realignment
`SPC .' from the input window can open in a foreign repository.  Running
the alignment on every resolve — not only at creation — self-heals a
buffer that was created (or session-restored) against the wrong dir."
  (let ((buf (or (let ((buf (agent-repl--ws-get ws :input-buffer)))
                   (when (buffer-live-p buf)
                     (agent-repl--log ws "ensure-input-buffer: ws=%s branch=recorded-live buffer=%s"
                                       ws (buffer-name buf))
                     buf))
                 (let ((named (get-buffer (agent-repl--buffer-name "-input" ws))))
                   (when (buffer-live-p named)
                     (agent-repl--log ws "ensure-input-buffer: ws=%s adopting live named buffer %s"
                                       ws (buffer-name named))
                     (agent-repl--ws-put ws :input-buffer named)
                     named))
                 (progn
                   (agent-repl--log ws "ensure-input-buffer: ws=%s branch=recreate recorded=%s named=%s"
                                     ws
                                     (agent-repl--safe-buffer-name (agent-repl--ws-get ws :input-buffer))
                                     (agent-repl--buffer-name "-input" ws))
                   (agent-repl--initialize-input-buffer ws)
                   (agent-repl--ws-get ws :input-buffer)))))
    (agent-repl--align-buffer-to-ws-dir buf ws)
    (agent-repl--log ws "ensure-input-buffer: ws=%s complete buffer=%s project-dir=%s"
                      ws (agent-repl--safe-buffer-name buf)
                      (agent-repl--ws-get ws :project-dir))
    buf))

(defun agent-repl--drain-pending-show-panels (ws)
  "Show WS's session if a preemptive prompt queued a :pending-show-panels flag.
Clears the flag and shows the session through WS's frontend (the
webview).  A generated workspace is born with this flag set and its
session booted headlessly (`agent-repl--frontend-boot-session'), so
this drain is where a gui workspace first becomes visible."
  (if (not (agent-repl--ws-get ws :pending-show-panels))
      (agent-repl--log-verbose ws "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws)
    (agent-repl--log ws "drain-pending-show-panels: ws=%s branch=had-pending draining frontend=%s"
                      ws (agent-repl--ws-frontend-name ws))
    (agent-repl--ws-put ws :pending-show-panels nil)
    (agent-repl--frontend-dispatch-show ws)))

(defun agent-repl--drain-pending-magit (ws)
  "Open `magit-status' for WS if it was created with `:pending-magit' set.
Reads the worktree path from `:project-dir', clears the flag, and removes
the Doom dashboard so magit is the sole main buffer in the new workspace.

When WS is also about to show its agent panels (`:pending-show-panels'
still set — this drain runs before that one in
`agent-repl--on-workspace-switch'), the magit buffer is created WITHOUT
a window (`save-window-excursion'): the panels open filling the frame
as the sole main-area display, so a magit window would only linger
beside the panels — the extra-windows-on-first-switch bug.  A
workspace with no pending panel show (the no-agent `SPC TAB n' path)
still displays magit as before."
  (if (agent-repl--ws-get ws :pending-magit)
      (let ((path (agent-repl--ws-get ws :project-dir))
            (windowless (and (agent-repl--ws-get ws :pending-show-panels) t)))
        (agent-repl--log ws "drain-pending-magit: ws=%s branch=had-pending path=%s windowless=%s draining"
                          ws path windowless)
        (agent-repl--ws-put ws :pending-magit nil)
        (if path
            (progn
              (if windowless
                  (save-window-excursion (agent-repl--magit-status-same-window path))
                (agent-repl--magit-status-same-window path))
              (agent-repl--remove-doom-dashboard))
          (agent-repl--log ws "drain-pending-magit: ws=%s branch=missing-project-dir request-cleared=t"
                            ws)))
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
        (if path
            (agent-repl--open-initial-buffers ws path)
          (agent-repl--log ws "drain-pending-initial-buffers: ws=%s branch=missing-project-dir request-cleared=t"
                            ws)))
    (agent-repl--log-verbose ws "drain-pending-initial-buffers: ws=%s branch=no-pending no-op" ws)))

(defun agent-repl--maybe-autoselect-input (ws)
  "Select the agent input window for WS if visible and autoselect is enabled.
Respects `agent-repl-autoselect-input-on-workspace-switch'.
Window lookup delegates to `agent-repl-window--panel-window'."
  (if agent-repl-autoselect-input-on-workspace-switch
      (if-let ((win (agent-repl-window--panel-window :input ws)))
          (progn
            (agent-repl--log ws "maybe-autoselect-input: ws=%s branch=select input-win=%s" ws win)
            (select-window win))
        (agent-repl--log ws "maybe-autoselect-input: ws=%s branch=no-input-window" ws))
    (agent-repl--log ws "maybe-autoselect-input: ws=%s branch=disabled" ws)))

(defun agent-repl--stale-panel-windows ()
  "Return a list of windows showing agent panel buffers from a different workspace.
Each element is a window whose buffer is a agent panel (webview or input) whose
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
          (agent-repl--log (agent-repl--ws-current-log-name)
                            "safe-delete-window: deleting win=%s buf=%s"
                            win (agent-repl--safe-buffer-name (window-buffer win)))
          (delete-window win))
      (let ((fb (or fallback
                    (and (fboundp 'doom-fallback-buffer) (doom-fallback-buffer)))))
        (agent-repl--log (agent-repl--ws-current-log-name)
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
replaced by WS's own panels, namely a *different* workspace's agent panel
windows were purged.

Reclaims through WS's own frontend: a live webview is re-displayed via
`agent-repl--frontend-dispatch-show' (the webview + input layout,
which clears the main area itself).

No-op when WS has no live webview to reclaim the frame with
\(`agent-repl-window--panels-restorable-p'), so the existing layout is
left as-is."
  (if (agent-repl-window--panels-restorable-p ws)
      (progn
        (agent-repl--log ws "reclaim-frame-fullscreen: showing gui view for ws=%s" ws)
        (agent-repl--frontend-dispatch-show ws))
    (agent-repl--log ws "reclaim-frame-fullscreen: no live view for ws=%s, skipping" ws)))

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

After purging stale panels, restores this workspace's own panels if
they were visible when this workspace was last deactivated
\(`:panels-were-visible' flag set by `--before-persp-deactivate').

The visibility flag is per-workspace rather than global because each
workspace has its own panel buffers."
  (let* ((stale (agent-repl--stale-panel-windows))
         (foreign-bufs (agent-repl--stale-window-buffers stale)))
    (agent-repl--log ws "ensure-own-panels: ws=%s stale=%d panels-visible=%s windows=%d"
                      ws (length stale)
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
    ;; ones), re-show them.  The re-show dispatches through WS's own
    ;; frontend, which lays out the webview and input panel together from
    ;; scratch, so there is no separate half-shown repair to make.
    (when (and (agent-repl--ws-get ws :panels-were-visible)
               (not (agent-repl--panels-visible-p))
               ;; Eligibility is a live VIEW buffer only: the mount
               ;; recreates a dead/nil input buffer itself
               ;; (`agent-repl--ensure-input-buffer').
               (agent-repl-window--panels-restorable-p ws))
      (agent-repl--log ws "ensure-own-panels: re-showing panels (were-visible but now missing)")
      (agent-repl--frontend-dispatch-show ws))
    ;; Take over the frame with THIS workspace's own panels in fullscreen —
    ;; replacing every visible window with the input+view panels — when a
    ;; foreign workspace's panels were just purged.
    (when stale
      (agent-repl--log ws "ensure-own-panels: reclaiming frame stale=%s" (and stale t))
      (agent-repl--reclaim-frame-fullscreen ws))))

(defun agent-repl--on-workspace-switch (&optional ws)
  "Handle workspace switch: update all workspace states and reconcile panels.
WS is the workspace name to operate on; when nil, falls back to
`(agent-repl--ws-current-name)' at call time.  Callers from
`--after-persp-activated' pass the ws captured at hook-fire time so
the deferred call operates on the workspace that was just switched
to, even if another switch raced ahead before the timer fired.

Also opens panels for workspaces that were created with a preemptive
prompt, and auto-selects the input window if visible.

Snaps the agent's webview feed to its last message
\(`agent-repl--frontend-snap-webview-to-tail'), so a switched-to
workspace never shows stale middle-of-history output.

never causes a silent decay.

Runs `agent-repl--dequeue-merge' so a workspace parked in the merge
queue is pulled from it on switch — activating a queued workspace is
read as the user wanting to work on it directly rather than have its
pending merge auto-fire."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log-verbose ws "workspace-switch ws=%s" ws)
    ;; Purge stale panel windows from other workspaces and restore own
    ;; panels if they were visible before this workspace was deactivated.
    ;; Must run BEFORE autoselect so it sees the correct panel windows.
    (agent-repl--ensure-own-panels-on-persp-switch ws)
    (agent-repl--dequeue-merge ws)
    ;; Event-driven (workspace just activated) → kick a fresh pass via
    ;; the unguarded entrypoint so the in-flight reentry guard from the
    ;; 1Hz timer doesn't swallow the switch's refresh.
    (agent-repl--update-all-workspace-states-now)
    (agent-repl--drain-pending-magit ws)
    (agent-repl--drain-pending-initial-buffers ws)
    (agent-repl--drain-pending-show-panels ws)
    (agent-repl--maybe-autoselect-input ws)
    ;; The workspace comes back showing its agent's newest output, never
    ;; the middle of the history the feed was left scrolled up to.  Runs
    ;; after the show drain, so a webview that just became visible is
    ;; snapped too.
    (agent-repl--frontend-snap-webview-to-tail ws)
    ;; NEVER-BLUE, switch half: tell the daemon this workspace was switched
    ;; to, so it discovers + binds any on-disk transcript and brings the shim
    ;; up.  A workspace the user merely LOOKS at must render its history
    ;; rather than sitting blue until they type.  Heavily skipped (live
    ;; session, cooldown, give-up) inside the notifier, and fire-and-forget
    ;; here — a daemon that cannot open the workspace must never stall the
    ;; switch, which is why the failure path is an ack callback and not a
    ;; signal.
    (agent-repl--frontend-notify-workspace-switch ws)
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
`switch-to-buffer' target: a agent panel buffer, a side window, or a
dedicated window.  Persp saves the selected
window into the workspace's restored layout; if that window is a
side/dedicated/panel window, Doom's `+workspace/kill' fallback
`(switch-to-buffer (doom-fallback-buffer))' cannot repurpose it and
instead splits a new window showing the doom splash buffer.

Picks the first window that satisfies `agent-repl--save-target-window-p'.
No-op when no safe target exists (fullscreen-agent or a
side-window-only frame)."
  (let ((sel (selected-window)))
    (when (or (agent-repl--agent-panel-buffer-p (window-buffer sel))
              (window-parameter sel 'window-side)
              (window-dedicated-p sel))
      (when-let ((target (cl-find-if
                          #'agent-repl--save-target-window-p
                          (window-list))))
        (select-window target)))))

;; `agent-repl--clear-done-ack-on-switch-away' is gone: there is no dwell
;; countdown left to restart, because there is no decay left to pace.

(defun agent-repl--before-persp-deactivate (&rest _)
  "Save window state before perspective deactivation.
Redirects away from agent buffers and saves frame state.  Also
records `:panels-were-visible' so `--ensure-own-panels-on-persp-switch'
can restore the correct workspace's panels after activation.
Logs `persp-names-cache' so cache mutations across persp lifecycle
events (kill, switch, add) are traceable."
  (let ((ws (agent-repl--ws-current-name)))
    (agent-repl--log ws "before-persp-deactivate: entry cache=%S"
                      (or (agent-repl--ws-names-cache) "(unbound)"))
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
  (agent-repl--log (agent-repl--ws-current-log-name) "after-persp-activated: entry cache=%S"
                    (or (agent-repl--ws-names-cache) "(unbound)"))
  ;; Suppressed during `agent-repl--eager-open-panels': its transient
  ;; switch-in/build/switch-back would otherwise schedule a deferred
  ;; `--on-workspace-switch' for the background workspace that fires after
  ;; focus has returned to the caller and reclaims the caller's frame with
  ;; the background workspace's panels (the eviction bug `--gui-boot' documents).
  (if agent-repl--eager-open-in-progress
      (agent-repl--log (agent-repl--ws-current-log-name)
                        "after-persp-activated: suppressed (eager-open in progress)")
    (let ((ws (agent-repl--ws-current-name)))
      (run-at-time 0 nil #'agent-repl--on-workspace-switch ws))))

(when (modulep! :ui workspaces)
  (agent-repl--ws-add-before-deactivate-hook #'agent-repl--before-persp-deactivate)
  (agent-repl--ws-add-activated-hook #'agent-repl--after-persp-activated))

(defun agent-repl--hide-panels ()
  "Hide both agent panels without killing buffers."
  (let* ((ws (agent-repl--ws-current-name))
         (input-buf (agent-repl--ws-get ws :input-buffer))
         (frontend-buf (agent-repl--ws-get ws :frontend-buffer)))
    (agent-repl--log ws "hide-panels: ws=%s input=%s frontend=%s"
                      ws (agent-repl--safe-buffer-name input-buf)
                      (agent-repl--safe-buffer-name frontend-buf))
    (agent-repl--close-buffer-windows input-buf frontend-buf)))

(defun agent-repl--save-tab-index (ws)
  "Persist WS's current tab-bar index to its plist as `:saved-tab-index'.
Reads positions from `persp-names-current-frame-fast-ordered'; no-op
when that helper is unavailable (e.g. test envs without persp-mode).

Also writes the index to disk via `--state-save'.

NOTE: this was historically read back on reopen by
`agent-repl--restore-tab-index', which restored the workspace to its
prior tab-bar slot after a close-deprio cycle.  That reader was reached
only through the vterm panel-show path and was deleted as dead code
along with it, so `:saved-tab-index' is currently write-only — nothing
restores the position it records.  Left in place (rather than deleted
too) because a future frontend-agnostic re-show path may want to
consume it again; flagging here so the asymmetry isn't mistaken for an
oversight."
  (when-let ((idx (cl-position ws (agent-repl--ws-frame-ordered-names)
                               :test #'string=)))
    (agent-repl--log ws "save-tab-index ws=%s index=%d" ws idx)
    (agent-repl--ws-put ws :saved-tab-index idx)
    (agent-repl--state-save ws)))

(defun agent-repl--close-view (ws direct-teardown)
  "Put WS's view away as part of a close, dispatching through its frontend.

A workspace with a resolvable gui frontend closes its WEBVIEW through the
registry's hide capability.  A nil WS — which has no frontend to resolve
at all — runs DIRECT-TEARDOWN, a thunk, instead.

The thunk parameter is what gives a resolvable-frontend workspace the
close BOOKKEEPING every frontend shares (see the callers,
`agent-repl--on-simple-close' and `agent-repl--on-close'), while still
leaving a ws-less close somewhere safe to land rather than erroring on
a frontend that cannot be resolved."
  (if (and ws (agent-repl--ws-gui-frontend-p ws))
      (progn
        (agent-repl--log ws "close-view: ws=%s branch=frontend-dispatch" ws)
        (agent-repl--frontend-dispatch-hide ws))
    (agent-repl--log ws "close-view: ws=%s branch=direct-teardown" ws)
    (funcall direct-teardown)))

(defun agent-repl--on-simple-close (&optional ws)
  "Bookkeep + hide the view; do NOT touch tab-bar order.
Sets `:repl-state :inactive' on WS (`:agent-state' untouched so an
in-flight :thinking / :permission survives the close), then puts the view
away through WS's own frontend.  No save-tab-index, no push-to-back, no
this is the simple-close audit point that `SPC o c' is bound to.

The teardown is frontend-dispatched rather than hard-wired to a single
mechanism, so a gui workspace closes its actual view AND records that it
did."
  (let ((ws (or ws (agent-repl--ws-current-name))))
    (agent-repl--log ws "on-simple-close: CALLED this-command=%s last-command=%s"
                      this-command last-command)
    (when ws
      (agent-repl--log ws "on-simple-close ws=%s agent-state=%s -> repl-state=:inactive"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-set-repl-state ws :inactive))
    (agent-repl--close-view ws (lambda ()
                                  (agent-repl--restore-fullscreen-config ws)
                                  (agent-repl--hide-panels)))))

(defun agent-repl--on-close (&optional ws)
  "Full close: bookkeep, restore pre-panel layout, hide, deprio, save tab index.
Sets WS's `:repl-state' to `:inactive', exactly like the simple-close
path — a closed workspace stays listed, and only repo folding takes a
workspace off the tab-bar.  Restores the pre-panel layout via
`agent-repl--restore-fullscreen-config' before hiding so the
frame-filling panels go away cleanly (same contract as
`agent-repl--on-simple-close').  Then hides panels and pushes WS to the second-to-last tab position via
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
      (agent-repl--log ws "on-close ws=%s agent-state=%s -> repl-state=:inactive"
                        ws (agent-repl--ws-agent-state ws))
      (agent-repl--ws-set-repl-state ws :inactive))
    (agent-repl--close-view
     ws
     (lambda ()
       (agent-repl--restore-fullscreen-config ws)
       (agent-repl--hide-panels)))
    (when (and ws (equal ws (agent-repl--ws-current-name)))
      (agent-repl--save-tab-index ws)
      (agent-repl--log ws "on-close: pushing ws=%s to second-to-last" ws)
      (agent-repl-workspace-push-to-back))))

;;;; Window synchronization

;; Reap orphaned panels drifted in from OTHER workspaces; the current
;; workspace's own half-missing pair is instead healed by the
;; window-change reconciler (`agent-repl-window--ensure-layout').
(defun agent-repl--extract-panel-id (name)
  "Extract the workspace identifier from a agent panel buffer NAME.
Returns the identifier string, or nil if NAME is not a agent panel buffer.
Matches either the input buffer (*agent-panel-input-WS*) or the
frontend webview buffer (*agent-frontend-WS*) — the two buffers a
workspace has."
  (cond
   ((string-match-p agent-repl--input-buffer-re name)
    (substring name (length "*agent-panel-input-") (- (length name) (length "*"))))
   ((string-match-p agent-repl--frontend-buffer-re name)
    (substring name (length "*agent-frontend-") (- (length name) (length "*"))))))

(defun agent-repl--partner-buffer-name (name id)
  "Return the partner buffer name for agent panel NAME with identifier ID.
For the input buffer, the partner is the frontend webview buffer, and
vice versa."
  (if (string-match-p agent-repl--input-buffer-re name)
      (format "*agent-frontend-%s*" id)
    (format "*agent-panel-input-%s*" id)))

(defun agent-repl--orphaned-panel-p (name)
  "Return non-nil if NAME is a agent panel buffer whose partner is not visible.
Ignores single-window frames.  Input buffers are not orphaned while the
loading placeholder exists, nor while the workspace's frontend WEBVIEW is
visible — the input panel's live partner is the webview."
  (when-let ((id (agent-repl--extract-panel-id name)))
    (let* ((is-input (string-match-p agent-repl--input-buffer-re name))
           (partner (agent-repl--partner-buffer-name name id))
           (one-window (one-window-p))
           (partner-window (get-buffer-window partner))
           (loading (and is-input (get-buffer agent-repl-loading-placeholder-name)))
           (webview-window (and is-input
                                (get-buffer-window
                                 (agent-repl--frontend-webview-buffer-name id))))
           (result (and (not one-window)
                        (not partner-window)
                        ;; Input panels are not orphaned while loading placeholder is live
                        (or (not is-input)
                            (and (not loading)
                                 (not webview-window))))))
      (agent-repl--log-verbose (agent-repl--ws-current-log-name)
                                "orphaned-panel-p: name=%s id=%s input=%s partner=%s one-window=%s partner-visible=%s loading=%s webview-visible=%s result=%s"
                                name id is-input partner one-window
                                (and partner-window t) (and loading t)
                                (and webview-window t)
                                (and result t))
      result)))

(defun agent-repl--own-panel-p (name)
  "Return non-nil when panel NAME belongs to the CURRENT workspace.
Compares the workspace id extracted from NAME (see
`agent-repl--extract-panel-id') against the sanitized current
workspace name.  Panel buffer names embed the SANITIZED workspace
name (see `agent-repl--buffer-name'), so the current name is
sanitized before the comparison — mirroring how
`agent-repl--stale-panel-windows' sanitizes before deciding
foreign-ness.  Returns nil when NAME is not a panel buffer, or when
there is no current workspace to compare against."
  (when-let ((id (agent-repl--extract-panel-id name))
             (current (agent-repl--sanitize-ws-name (agent-repl--ws-current-name))))
    (string= id current)))

(defun agent-repl--sweepable-panel-p (name)
  "Return non-nil when panel NAME may be reaped by `agent-repl--sync-panels'.
A panel is sweepable only when it is orphaned
\(`agent-repl--orphaned-panel-p') AND does NOT belong to the current
workspace (`agent-repl--own-panel-p').

The current workspace's own panels are laid out and torn down by the
explicit show/hide paths, never by the window-change sweep: during a
webview mount the input window is split below the webview and the
webview window is briefly not observable via `get-buffer-window', a
transient mid-split state in which `agent-repl--orphaned-panel-p'
would (correctly, in isolation) report the current workspace's input
panel as orphaned and the sweep would delete it out from under the
mount.  Skipping any panel whose id matches the current workspace
closes that race, leaving the sweep to reap only panels drifted in
from OTHER, switched-away workspaces.

The guard lives here (and via `agent-repl--own-panel-p'), rather than
inside `agent-repl--orphaned-panel-p', so the orphan predicate stays a
pure partner-visibility test and the sweep policy is localized with
the sweeper."
  (let* ((orphaned (agent-repl--orphaned-panel-p name))
         (own (and orphaned (agent-repl--own-panel-p name)))
         (result (and orphaned (not own))))
    (agent-repl--log-verbose (agent-repl--ws-current-log-name)
                              "sweepable-panel-p: name=%s orphaned=%s own=%s result=%s"
                              name (and orphaned t) (and own t) (and result t))
    result))

(defun agent-repl--sync-panels ()
  "Close any OTHER workspace's agent panel whose partner is no longer visible.
The current workspace's own panels are never swept (see
`agent-repl--sweepable-panel-p') — only panels belonging to other,
switched-away workspaces are reaped.

Side windows can never be agent panels by
predicate construction, so the default `--delete-where' side-skip
costs nothing and remains defense-in-depth.

Logs each orphan's buffer name BEFORE the sweep (capturing names
while windows are still live) so the per-orphan log survives the
deletion that follows."
  (let* ((ws (agent-repl--ws-current-name))
         (orphan-names
          (cl-loop for win in (window-list)
                   for name = (buffer-name (window-buffer win))
                   when (agent-repl--sweepable-panel-p name)
                   collect name)))
    (agent-repl--log-verbose ws "sync-panels: entry windows=%d"
                              (length (window-list)))
    (dolist (name orphan-names)
      (agent-repl--log ws "sync-panels closing orphaned %s" name))
    (let ((deleted
           (agent-repl-window--delete-where
            (lambda (win)
              (agent-repl--sweepable-panel-p
               (buffer-name (window-buffer win)))))))
      (agent-repl--log-verbose ws "sync-panels: closed %d orphans"
                                (length deleted)))))

(defvar agent-repl--sync-timer nil
  "Timer for debounced window-change handler.")

(defun agent-repl--on-window-change ()
  "Deferred handler for window configuration changes.
Sweeps orphaned panels drifted in from other workspaces
\(`agent-repl--sync-panels'), then reconciles the current workspace's
own two-panel layout (`agent-repl-window--ensure-layout') so a window
change that knocked exactly one of the view/input pair off the frame
heals back to the canonical shape.  It also used to refresh the
hide-overlay that blanked the vterm's bottom rows (the TUI drew its
own input box there, which Emacs's input panel replaced); the webview
hides its composer declaratively instead, so there is nothing left to
refresh."
  (if (active-minibuffer-window)
      ;; Skip reconciliation while a minibuffer is active (e.g. the
      ;; `SPC p p' picker).  The window configuration is transient — the
      ;; picker's own window churn is what fired this debounced idle
      ;; timer — so reconciling now would rearrange windows under the open
      ;; picker (and sweep the undeletable minibuffer window).  Closing the
      ;; minibuffer changes the window configuration again, re-firing this
      ;; hook to reconcile the settled layout.
      (agent-repl--log-verbose (agent-repl--ws-current-log-name)
                                "on-window-change: minibuffer active — deferring reconcile")
    (agent-repl--log-verbose (agent-repl--ws-current-log-name) "on-window-change")
    (agent-repl--sync-panels)
    (agent-repl-window--ensure-layout)))

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

;;;; Buffer creation

(defun agent-repl--initialize-input-buffer (ws)
  "Create the agent input buffer for workspace WS and enable agent-repl-input-mode.
Errors if the buffer is already initialized (already in
`agent-repl-input-mode')."
  ;; Resolve the history root before creating or recording the buffer.  Input
  ;; initialization is atomic: a workspace without its mandatory project
  ;; directory must not retain a half-initialized composer after history
  ;; hydration correctly rejects that workspace.
  (let ((project-dir (agent-repl--ws-dir ws)))
    (agent-repl--log ws "initialize-input-buffer: ws=%s project-dir=%s precondition=validated"
                      ws project-dir)
    (let ((input-buf (agent-repl--create-buffer ws "-input")))
      (agent-repl--ws-put ws :input-buffer input-buf)
      (with-current-buffer input-buf
        (when (eq major-mode 'agent-repl-input-mode)
          (agent-repl--log ws "initialize-input-buffer: ws=%s buffer=%s branch=already-initialized"
                            ws (buffer-name input-buf))
          (error "agent-repl--initialize-input-buffer: already initialized ws=%s" ws))
        (agent-repl-input-mode)
        (agent-repl--log ws "initialize-input-buffer: ws=%s buffer=%s mode=enabled history=restore"
                          ws (buffer-name input-buf))
        (agent-repl--history-restore ws)))))

;;;; Panel show/hide strategies

(defun agent-repl--clear-main-area-for-panels ()
  "Delete every non-side window other than the selected one.
Side-window-aware replacement for `delete-other-windows' on the
panel-show path: side windows must survive panel reopen.
`delete-other-windows' relies on each side window carrying
`no-delete-other-windows', which is fragile — window-parameter loss
anywhere upstream (e.g. a buffer redisplayed without the original
action alist) leaves a side window vulnerable.
Routing through `agent-repl-window--delete-where' makes the
side-window skip explicit and parameter-independent."
  (let* ((ws (agent-repl--ws-current-name))
         (selected (selected-window))
         (deleted (agent-repl-window--delete-where
                   (lambda (win) (not (eq win selected))))))
    (agent-repl--log ws "clear-main-area-for-panels: ws=%s selected=%s deleted-count=%d"
                      ws selected (length deleted))
    deleted))

(defun agent-repl--hide-and-preserve-status ()
  "Close-and-KILL with full deprio + tab-bar shuffle (the `SPC o C' path).
Runs `agent-repl--on-close' (restore layout, hide, deprio bookkeeping)
and then KILLS the session through the workspace's frontend registry —
`SPC o C' means \"done with this session\", unlike the plain-close
`SPC o c' which only puts the view away.  The `:repl-state :inactive'
marker is re-asserted after the kill so a frontend's kill capability
resetting the state axes cannot leave the workspace claiming an open
REPL.

Deliberately NOT folded into `agent-repl--on-close': its other callers
(e.g. `agent-repl-send-and-hide') hide a session that must keep
running."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws (error "agent-repl--hide-and-preserve-status: no active workspace"))
    (agent-repl--on-close ws)
    (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)
    (agent-repl--ws-put ws :repl-state :inactive)))

(defun agent-repl--simple-hide-and-preserve-status ()
  "Hide agent panels with NO tab-bar update (the `SPC o c' path).
Thin wrapper around `agent-repl--on-simple-close' that enforces the
invariant that a workspace is active.  See
`agent-repl--hide-and-preserve-status' for the deprio variant
bound to `SPC o C'."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws (error "agent-repl--simple-hide-and-preserve-status: no active workspace"))
    (agent-repl--on-simple-close ws)))

;;;; Entry point

(cl-defun agent-repl--toggle (close-fn &key always-close)
  "Generic toggle for a workspace's gui view.  CLOSE-FN handles the
visible-view case.  Used by both `agent-repl' (deprio close) and
`agent-repl-simple' (plain close).

When ALWAYS-CLOSE is non-nil, every non-selection branch routes to
CLOSE-FN regardless of running / visibility state — the workspace is
hidden even if its view isn't visible (or isn't running at all).  This
is the `SPC o C' contract: pressing it again on a workspace that is
already closed / never-started should still mark it `:inactive' and
push it to the back, not re-show or launch the agent."
  (let* ((ws (agent-repl--ws-current-name))
         (fe (agent-repl--ws-frontend ws))
         (webview (agent-repl--ws-get ws :frontend-buffer))
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (agent-repl--log ws "agent-repl selection=%s always-close=%s"
                      (if selection "yes" "no") (if always-close "yes" "no"))
    (cond
     (selection
      (agent-repl--log ws "toggle: branch=send-selection")
      (deactivate-mark)
      (agent-repl--send-to-agent selection))
     ;; `SPC o C' means "done with this workspace" whether or not its
     ;; view happens to be on screen.
     (always-close
      (agent-repl--log ws "toggle: branch=always-close")
      (funcall close-fn))
     ((and (buffer-live-p webview) (get-buffer-window webview))
      (agent-repl--log ws "toggle: branch=close")
      (funcall close-fn))
     ((funcall (agent-repl-frontend-running-p-fn fe) ws)
      (agent-repl--log ws "toggle: branch=show")
      (funcall (agent-repl-frontend-show-fn fe) ws))
     (t
      (agent-repl--log ws "toggle: branch=open")
      (funcall (agent-repl-frontend-open-fn fe) ws)))))

(defun agent-repl ()
  "Hide Agent REPL panels and deprio the workspace.
If text is selected: send it directly to the agent (orthogonal to hide).
Otherwise: mark the workspace `:repl-state :inactive', hide both panels
\(no-op if already hidden), and push the workspace tab to the back.
Always hides, regardless of whether the agent is running or panels are
currently visible.  The workspace stays listed on the tab-bar.
Bound to `SPC o C'.  See `agent-repl-simple' for the no-tab-bar variant."
  (interactive)
  (agent-repl--toggle #'agent-repl--hide-and-preserve-status :always-close t))

(defun agent-repl-simple ()
  "Toggle Agent REPL panels with a plain close (no tab-bar update).
Same dispatch as `agent-repl' except the close branch only hides the
panels and sets `:repl-state :inactive' — no save-tab-index, no
push-to-back.  Bound to `SPC o c'."
  (interactive)
  (agent-repl--toggle #'agent-repl--simple-hide-and-preserve-status))

;;;; Session cleanup

(defun agent-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (if (process-live-p proc)
      (progn
        (agent-repl--log (agent-repl--ws-current-log-name) "sigkill-if-alive: branch=signal proc=%s" proc)
        (signal-process proc 'SIGKILL))
    (agent-repl--log-verbose (agent-repl--ws-current-log-name)
                              "sigkill-if-alive: branch=already-dead proc=%s" proc)))

(defun agent-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (agent-repl--log (agent-repl--ws-current-log-name) "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time agent-repl-sigkill-delay nil #'agent-repl--sigkill-if-alive proc))

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
`:kill-fn' (daemon session + webview)."
  (interactive)
  (let ((ws (agent-repl--ws-current-name))
        (agent-repl--kill-cause (or agent-repl--kill-cause
                                    "interactive agent-repl-kill command")))
    (agent-repl--log ws "kill: ws=%s kill-cause=%s" ws (agent-repl--kill-cause-str))
    (unless ws (error "agent-repl-kill: no active workspace"))
    (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)))

(defun agent-repl-restart ()
  "Hard restart the agent for the current workspace.
Frontend-blind: dispatches the workspace's registered frontend's
`:restart-fn'.  For the gui a fresh daemon session is created."
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
        (agent-repl--frontend-dispatch-show ws))
      (when-let ((win (get-buffer-window (agent-repl--ws-get ws :input-buffer))))
        (select-window win))))))

(defun agent-repl--restore-fullscreen-config (ws)
  "Restore WS's saved pre-panel layout, clearing `:fullscreen-config'.
Returns non-nil when a restore happened, nil when WS had no saved config.

`:fullscreen-config' is the window layout captured the moment the
frame-filling panels were opened (fullscreen is the sole display
format).  The close paths (`agent-repl--on-simple-close' for `SPC o c' and
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
window, `selected-window' is the side
window itself.  The non-agent branch would then treat the side window
as the window to KEEP and sweep every main-area window — leaving the
user's actual work window (or agent panels) destroyed and only the
side window alongside an arbitrary survivor from `delete-window's
benign sole-main-window error.

Pre-selecting a real main-area leaf window sidesteps the path: the
subsequent branch dispatch reads the buffer of a real main-area
window and the delete sweep keeps that window instead of the side
window.

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
the non-side area (preserving side windows) and saves the
layout; calling again restores it.
When invoked from a side window, first
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
