;;; panels.el --- panel/window management and entry point -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl--open-progress-placeholder "open-progress" (ws))
(declare-function agent-repl-held-ingress-refresh "held-ingress" (ws))
(declare-function agent-repl--agent-panel-buffer-p "core")
(declare-function agent-repl--align-buffer-to-ws-dir "status")
(declare-function agent-repl--buffer-name "core")
(declare-function agent-repl--call-in-background-workspace "workspace")
(declare-function agent-repl--buffer-owner "core")
(declare-function agent-repl--create-buffer "core")
(declare-function agent-repl--foreign-owned-buffer-p "core")
(declare-function agent-repl--frontend-dispatch-hide "frontends")
(declare-function agent-repl--frontend-dispatch-show "frontends")
(declare-function agent-repl--frontend-webview-buffer-name "frontend")
(declare-function agent-repl--history-restore "history")
(declare-function agent-repl--info "core")
(declare-function agent-repl--kill-cause-str "core")
(declare-function agent-repl--log "core")
(declare-function agent-repl--log-verbose "core")
(declare-function agent-repl--magit-status-same-window "magit")
(declare-function agent-repl--open-initial-buffers "worktree")
(declare-function agent-repl--pseudo-workspace-name-p "core")
(declare-function agent-repl--remove-doom-dashboard "worktree")
(declare-function agent-repl--safe-buffer-name "window")
(declare-function agent-repl--sanitize-ws-name "core")
(declare-function agent-repl--send-to-agent "commands")
(declare-function agent-repl--state-save "history")
(declare-function agent-repl--warn "core")
(declare-function agent-repl--with-log-context "core" (workspace request-id function))
(declare-function agent-repl--ws-buffers "workspace")
(declare-function agent-repl--ws-current-log-name "workspace")
(declare-function agent-repl--ws-current-name "workspace")
(declare-function agent-repl--ws-dir "status")
(declare-function agent-repl--ws-frame-ordered-names "workspace")
(declare-function agent-repl--ws-frame-save-state "workspace")
(declare-function agent-repl--ws-frontend "frontends")
(declare-function agent-repl--ws-frontend-name "frontends")
(declare-function agent-repl--ws-get "workspace")
(declare-function agent-repl--ws-gui-frontend-p "frontends")
(declare-function agent-repl--ws-known-p "workspace")
(declare-function agent-repl--ws-live-p "workspace")
(declare-function agent-repl--ws-log-name "workspace")
(declare-function agent-repl--ws-names-cache "workspace")
(declare-function agent-repl--ws-put "workspace")
(declare-function agent-repl--ws-remove-buffer "workspace")
(declare-function agent-repl--ws-resolve-persp "workspace")
(declare-function agent-repl--ws-shared-unowned-buffer-p "workspace" (buf ws))
(declare-function agent-repl--ws-switch "workspace")
(declare-function agent-repl--ws-system-available-p "workspace")
(declare-function agent-repl--ws-update-names-cache "workspace")
(declare-function agent-repl-frontend-kill-fn "frontends")
(declare-function agent-repl-frontend-open-fn "frontends")
(declare-function agent-repl-frontend-restart-fn "frontends")
(declare-function agent-repl-frontend-running-p-fn "frontends")
(declare-function agent-repl-frontend-show-fn "frontends")
(declare-function agent-repl-input-mode "input")
(declare-function agent-repl-window--delete-buffer-windows "window")
(declare-function agent-repl-window--delete-or-neutralize "window")
(declare-function agent-repl-window--delete-where "window")
(declare-function agent-repl-window--ensure-layout "window")
(declare-function agent-repl-window--panel-buffer "window")
(defvar agent-repl--global-log-scope)
(declare-function agent-repl-window--panel-window "window")
(declare-function agent-repl-window--panels-restorable-p "window")
(declare-function agent-repl-window--side-window-p "window")

;; Special variables owned by other sources in this module, declared here
;; so the byte-compiler binds and reads them dynamically rather than
;; lexically.
(defvar agent-repl--eager-open-in-progress)
(defvar agent-repl--frontend-buffer-re)
(defvar agent-repl--input-buffer-re)
(defvar agent-repl--kill-cause)

;; open-progress.el loads AFTER this file (it needs frontend.el's main-area
;; host resolution, which this file's layout helpers sit beside), so the
;; entry point's placeholder calls are declared rather than resolved at
;; compile time.
(declare-function agent-repl--open-progress-active-p "agent-repl-open-progress" (ws))
(declare-function agent-repl--open-progress-start "agent-repl-open-progress" (ws))
(declare-function agent-repl--open-progress-finish "agent-repl-open-progress" (ws))
(declare-function agent-repl--preregistration-log-workspace-p "core" (ws))

(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl-host-conn "host" (ws))
(declare-function agent-repl-host-state "host" (ws))
(declare-function agent-repl-host-subscribe "host" (conn ws ref))
(declare-function agent-repl-host--apply-naming "host" (ws))
(declare-function agent-repl-link-primary "daemon-link" ())
(declare-function agent-repl--force-tab-bar-redraw "status" ())
(declare-function agent-repl-roster-tab-order "roster" ())
(declare-function agent-repl-roster-move-tab-to-back "roster" (ws))

(defun agent-repl--foreign-perspective-p (ws)
  "Return non-nil when WS is a persp-mode perspective agent-repl never touched.

persp-mode activates every perspective it restores at startup — including
ones left over from a prior session that agent-repl already tore down (a
kill/merge whose bookkeeping never reached persp-mode's own saved
perspective list) — and fires `persp-activated-functions' for each one
exactly as it would for a live switch.

Such a WS is neither a pseudo perspective
(`agent-repl--pseudo-workspace-name-p',
persp-mode's own \"none\"/\"main\") nor a workspace agent-repl has ever
registered (`agent-repl--ws-known-p' — live, tombstoned, or mid-creation
via `agent-repl--preregistration-log-workspace-p'): it is simply not ours.
Scheduling the switch machinery for it would feed its name into the
logging ladder with no sink to route to, ever — not a transient gap that
registration closes, but a name that will never own one — which is what
made `agent-repl--note-unroutable-log-workspace' warn once per stale
restored perspective at every boot.

A workspace genuinely missing its registration (the anomaly that warning
exists to shout about) is unaffected: this predicate is FALSE for it
whenever it is known or mid-creation, so its records still reach the
ladder unscreened from their own call sites."
  (and (stringp ws)
       (not (agent-repl--pseudo-workspace-name-p ws))
       (not (agent-repl--ws-known-p ws))
       (not (agent-repl--preregistration-log-workspace-p ws))))

(defcustom agent-repl-input-height-fraction 0.23
  "Fraction of the frame's main area allocated to the input panel.

Read ONCE PER FRAME GEOMETRY by `agent-repl-window--input-height', which
turns it into a fixed line count every mount of every workspace on that
frame then reuses.  It is not re-applied to whatever window the mount
happens to split, which is what used to make one workspace's composer
taller than another's and one workspace's composer change height across
a remount."
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

;;;; Panel visibility predicates

(defun agent-repl--ws-buffer-visible-p (key)
  "Return non-nil if the buffer stored at KEY in current workspace is visible."
  (let* ((buf (agent-repl--ws-get (agent-repl--ws-current-name) key))
         (result (and buf (buffer-live-p buf) (get-buffer-window buf))))
    (agent-repl--log-verbose
     '(:agent-repl-context "panel visibility can be checked outside a workspace")
     "ws-buffer-visible-p: key=%s result=%s" key (if result "visible" "hidden"))
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
    (agent-repl--log-verbose
     '(:agent-repl-context "panel visibility can be checked outside a workspace")
     "panels-visible-p: result=%s" (if result "visible" "hidden"))
    result))

(defun agent-repl--panels-any-visible-p ()
  "Return non-nil when EITHER the input panel or the agent view is visible.
`agent-repl--panels-visible-p' requires BOTH windows; the toggle's close
branch uses THIS so a single `SPC o c' closes whenever any panel is on
screen.  A both-visible close check let the input-only case (webview
window gone, or a no-ref open that showed only the composer) fall
through to a re-show branch, so the first press re-opened and only the
second closed — the two-press-to-close bug."
  (let ((result (or (agent-repl--input-visible-p)
                    (agent-repl--view-visible-p))))
    (agent-repl--log-verbose
     '(:agent-repl-context "panel visibility can be checked outside a workspace")
     "panels-any-visible-p: result=%s" (if result "visible" "hidden"))
    result))

;;;; Panel-open default and the explicit-close preference

(defun agent-repl--ws-panels-open-preferred-p (ws)
  "Return non-nil unless the user has EXPLICITLY closed WS's panels.
Panels default to OPEN: a workspace with no recorded panel-visibility
preference shows its panels when it becomes current / is switched to, so
the user never has to open them.  An explicit `SPC o c'/`SPC o C' close
records `:panels-closed-by-user'
\(`agent-repl--note-panels-closed-by-user'), and that preference is
honored until the panels are shown again
\(`agent-repl--note-panels-shown' clears it) so an explicit close is not
fought on the next switch.

This is the switch-restore gate in
`agent-repl--ensure-own-panels-on-persp-switch'.  It replaced the old
`:panels-were-visible' gate, whose nil value could not tell a
never-recorded workspace (which should default open) from one recorded
hidden."
  (not (agent-repl--ws-get ws :panels-closed-by-user)))

(defun agent-repl--note-panels-closed-by-user (ws)
  "Record that the user EXPLICITLY closed WS's panels.
Sets `:panels-closed-by-user' so `agent-repl--ws-panels-open-preferred-p'
stops treating WS as panels-open until the panels are shown again.  No-op
for a nil WS."
  (when ws
    (agent-repl--ws-put ws :panels-closed-by-user t)
    (agent-repl--log ws "note-panels-closed-by-user: ws=%s" ws)))

(defun agent-repl--note-panels-shown (ws)
  "Clear WS's explicit-close preference because its panels are being shown.
Restores the panels-open default so a later switch re-shows them
\(`agent-repl--ws-panels-open-preferred-p').  No-op when WS is nil or was
not marked closed, so it is cheap to call from every show path."
  (when (and ws (agent-repl--ws-get ws :panels-closed-by-user))
    (agent-repl--ws-put ws :panels-closed-by-user nil)
    (agent-repl--log ws "note-panels-shown: ws=%s cleared explicit-close" ws)))

;;;; Panel display and hide

;; `agent-repl--safe-buffer-name' now lives in window.el, the layer below
;; this one: the window helpers label buffers in their own records and
;; loading order makes window.el the only home both layers can share.

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
  (agent-repl--log
   '(:agent-repl-context "panel cleanup can run outside a workspace")
   "close-buffer-windows %s" (mapcar #'agent-repl--safe-buffer-name bufs))
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
  "Show WS's session if something armed the :pending-show-panels flag.
Clears the flag and shows the session through WS's frontend (the
webview).  The gui pre-creates a workspace's page WITHOUT displaying it
\(`agent-repl--gui-boot'), so this drain is where a gui workspace first
becomes visible.

WHO ARMS IT: `agent-repl-switch-to-project' for the workspace a
projectile switch is about to stand on -- which is how a workspace you
just created comes up showing itself -- through
`agent-repl--arm-landing-panels', which arms BEFORE the switch, so the
flag is set by the time the persp activation hook drains it.  A teardown
landing arms nothing (`agent-repl--land-before-teardown'): arrival
re-shows panels by default, and an explicit close in the landing
workspace is not overridden by some other workspace's teardown.  (The
old headless birth path armed it too; it went with
`agent-repl--frontend-boot-session', which no longer exists.)"
  (if (not (agent-repl--ws-get ws :pending-show-panels))
      ;; The no-op branch is the one a persp placeholder reaches (it owns no
      ;; pending flags), so its record is screened for routability.
      (agent-repl--log-verbose (agent-repl--ws-log-name ws)
                                "drain-pending-show-panels: ws=%s branch=no-pending no-op" ws)
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
    ;; See `--drain-pending-show-panels': the no-pending branch is the persp
    ;; placeholder's branch, so it is screened for routability.
    (agent-repl--log-verbose (agent-repl--ws-log-name ws)
                              "drain-pending-magit: ws=%s branch=no-pending no-op" ws)))

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
    ;; See `--drain-pending-show-panels': the no-pending branch is the persp
    ;; placeholder's branch, so it is screened for routability.
    (agent-repl--log-verbose (agent-repl--ws-log-name ws)
                              "drain-pending-initial-buffers: ws=%s branch=no-pending no-op" ws)))

(defun agent-repl--input-enter-command-state ()
  "Put evil into normal (command) state in the just-selected input window.
Landing on a switch leaves the cursor in the composer ready for command
keys, not mid-insert: a switch is navigation, so the user arrives in
command state rather than typing.  No-op when evil is absent (batch
tests, or a non-evil session).  Only ever called after the input window
has been selected on a switch (`agent-repl--maybe-autoselect-input'), so
it never forces normal state in an unrelated buffer."
  (when (fboundp 'evil-normal-state)
    (evil-normal-state)))

(defun agent-repl--maybe-autoselect-input (ws)
  "Select the agent input window for WS if visible and autoselect is enabled.
Respects `agent-repl-autoselect-input-on-workspace-switch'.
Window lookup delegates to `agent-repl-window--panel-window'.

After selecting the input window, puts evil in NORMAL (command) state
via `agent-repl--input-enter-command-state' so a switch lands the cursor
in the composer ready for command keys rather than mid-insert.

WS reaches here straight off the persp activation path, so it may be a
persp-mode placeholder that owns no log sink; the records go through
`agent-repl--ws-log-name' and carry the name in their text instead.

EVERY BRANCH RECORDS AT `info'.  This record is the only thing that says the
SWITCH is what moved the cursor into the composer -- the cursor's position
alone cannot, because a composer that was already selected looks identical.
On the debug rung it fell below the default durable threshold and never
reached a sink, so the landing was invisible afterwards."
  (let ((log-ws (agent-repl--ws-log-name ws)))
    (if agent-repl-autoselect-input-on-workspace-switch
        (if-let ((win (agent-repl-window--panel-window :input ws)))
            (progn
              (agent-repl--info log-ws "maybe-autoselect-input: ws=%s branch=select input-win=%s" ws win)
              (select-window win)
              (agent-repl--input-enter-command-state))
          (agent-repl--info log-ws "maybe-autoselect-input: ws=%s branch=no-input-window" ws))
      (agent-repl--info log-ws "maybe-autoselect-input: ws=%s branch=disabled" ws))))

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
leaving the stale window in place.

The recipe itself lives in `agent-repl-window--delete-or-neutralize',
which is the one place agent-repl answers \"what if this is the last
window\" — the close path's buffer-window sweep answers it identically
through the same helper.  This wrapper survives as the persp-switch
caller's name for it."
  (agent-repl-window--delete-or-neutralize
   win fallback (agent-repl--ws-current-log-name)))

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

After purging stale panels, shows this workspace's own panels unless the
user EXPLICITLY closed them: panels default to open, so a workspace with
no recorded panel-visibility preference shows itself on arrival
\(`agent-repl--ws-panels-open-preferred-p'), and only an explicit
`SPC o c'/`SPC o C' close keeps them hidden.

The preference is per-workspace rather than global because each
workspace has its own panel buffers.

WS arrives from the persp activation path, so it can be persp-mode's own
\"main\" or \"none\" — a perspective whose stale panels must still be purged
but which owns no log sink.  The reconciliation therefore uses the
unscreened WS while every record uses `agent-repl--ws-log-name'."
  (let* ((stale (agent-repl--stale-panel-windows))
         (foreign-bufs (agent-repl--stale-window-buffers stale))
         (log-ws (agent-repl--ws-log-name ws)))
    (agent-repl--log log-ws "ensure-own-panels: ws=%s stale=%d panels-visible=%s windows=%d"
                      ws (length stale)
                      (agent-repl--panels-visible-p) (length (window-list)))
    (when stale
      (agent-repl--log log-ws "ensure-own-panels: ws=%s closing %d stale panel windows: %S"
                        ws (length stale)
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
    ;; Panels default to OPEN: unless the user EXPLICITLY closed this
    ;; workspace's panels, show them when they are not visible now (persp
    ;; dropped them, we just purged stale ones, or the workspace has never
    ;; been stood in and has no saved configuration).  A workspace with no
    ;; recorded preference is treated as panels-open, so it shows itself on
    ;; switch/startup without the user having to open it
    ;; (`agent-repl--ws-panels-open-preferred-p'); an explicit close is
    ;; honored and not fought.  The re-show dispatches through WS's own
    ;; frontend, which lays out the webview and input panel together from
    ;; scratch, so there is no separate half-shown repair to make.
    ;;
    ;; The decision is recorded at INFO, whichever way it goes: which
    ;; panels an arrival puts on the frame is the user-visible outcome of
    ;; every switch and every teardown landing, and an invisible decision
    ;; is a logging defect.
    (let ((reason
           (cond ((not (agent-repl--ws-panels-open-preferred-p ws)) 'closed-by-user)
                 ((agent-repl--panels-visible-p) 'already-visible)
                 ;; Eligibility is a live VIEW buffer only: the mount
                 ;; recreates a dead/nil input buffer itself
                 ;; (`agent-repl--ensure-input-buffer').
                 ((not (agent-repl-window--panels-restorable-p ws)) 'no-live-view)
                 (t 'default-open-now-missing))))
      (agent-repl--info log-ws "elisp.panels.restore-decision: ws=%s decision=%s reason=%s"
                        ws (if (eq reason 'default-open-now-missing) "re-show" "no-show")
                        reason)
      (when (eq reason 'default-open-now-missing)
        (agent-repl--frontend-dispatch-show ws)))
    ;; Take over the frame with THIS workspace's own panels in fullscreen —
    ;; replacing every visible window with the input+view panels — when a
    ;; foreign workspace's panels were just purged.
    (when stale
      (agent-repl--log log-ws "ensure-own-panels: ws=%s reclaiming frame stale=%s" ws (and stale t))
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

The webview is left ALONE: it is bound to its buffer for life and draws
whatever the daemon's own pushes say, so there is nothing for the switch
to tell it.

never causes a silent decay.

WS is whatever perspective persp-mode activated, which includes its own
\"none\" and Doom's initial \"main\".  Those are not agent-repl workspaces and
own no durable log sink, so every record this path emits is screened through
`agent-repl--ws-log-name' and carries the name in its message text.  A real
workspace keeps its attribution — the screen only demotes names that could
not be routed at all.
"
  (let* ((ws (or ws (agent-repl--ws-current-name)))
         (log-ws (agent-repl--ws-log-name ws))
         (scope (cond
                 (log-ws log-ws)
                 ((or (null ws) (agent-repl--pseudo-workspace-name-p ws))
                  agent-repl--global-log-scope)
                 (t ws))))
    (agent-repl--with-log-context
     scope nil
     (lambda ()
       (agent-repl--log-verbose log-ws "workspace-switch ws=%s" ws)
       ;; Purge stale panel windows from other workspaces and restore own
       ;; panels if they were visible before this workspace was deactivated.
       ;; Must run BEFORE autoselect so it sees the correct panel windows.
       (agent-repl--ensure-own-panels-on-persp-switch ws)
       ;; Repaint: the switch changed which tab is SELECTED, and selection is
       ;; the one part of a tab's appearance the roster does not carry.  There
       ;; is nothing to refresh beyond that — every workspace's state arrived on
       ;; the roster stream.
       (agent-repl--force-tab-bar-redraw)
       (agent-repl--drain-pending-magit ws)
       (agent-repl--drain-pending-initial-buffers ws)
       (agent-repl--drain-pending-show-panels ws)
       (agent-repl--maybe-autoselect-input ws)
       ;; The workspace is SELECTED: SelectWorkspace is Emacs's own second
       ;; and last contribution to the roster, and host.el sends it from the
       ;; perspective-activated hook.  Nothing else about the switch reaches
       ;; the daemon, and nothing about it is asked of the page.
       ;; THE FULLY-LOADED LATCH IS GONE with the session events that armed
       ;; its other half: readiness is the daemon's, and a workspace's state
       ;; arrives on the roster rather than being latched together here out of
       ;; two local observations.
       (when ws
         ;; Screened through `--ws-log-name' like every other record on this
         ;; path: persp-mode hands it its own placeholders too, and those own
         ;; no durable sink to route a workspace-attributed record to.
         (agent-repl--info log-ws "elisp.panels.switch: complete ws=%s" ws))))))

;; Save window state for current workspace before switching away,
;; so the panel-visibility paint can inspect the saved config.

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
events (kill, switch, add) are traceable.

This hook fires for EVERY perspective, including persp-mode's own
placeholders (`persp-nil-name' \"none\", Doom's initial \"main\"), so the
bookkeeping uses the unscreened name while the log lines use
`agent-repl--ws-current-log-name': a placeholder owns no durable sink, and
its deactivation is a genuinely global-scope event."
  (let* ((ws (agent-repl--ws-current-name))
         (log-ws (agent-repl--ws-current-log-name))
         (scope (if log-ws log-ws agent-repl--global-log-scope)))
    (agent-repl--with-log-context
     scope nil
     (lambda ()
       (agent-repl--log log-ws "before-persp-deactivate: entry ws=%s cache=%S"
                         ws (or (agent-repl--ws-names-cache) "(unbound)"))
       ;; Record whether panels are visible BEFORE redirecting/saving so
       ;; the activated hook can restore them if persp-mode drops them.
       (agent-repl--ws-put ws :panels-were-visible (agent-repl--panels-visible-p))
       (agent-repl--redirect-from-agent-before-save)
       (condition-case err
           (agent-repl--ws-frame-save-state)
         (error (agent-repl--warn log-ws "persp-frame-save-state failed for ws=%s: %S" ws err)
                (agent-repl--log log-ws "before-persp-deactivate: persp-frame-save-state error ws=%s: %S" ws err)))))))

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
  (agent-repl--log '(:agent-repl-context "perspective activation can name no agent workspace")
                   "after-persp-activated: entry cache=%S"
                    (or (agent-repl--ws-names-cache) "(unbound)"))
  ;; Suppressed during `agent-repl--eager-open-panels': its transient
  ;; switch-in/build/switch-back would otherwise schedule a deferred
  ;; `--on-workspace-switch' for the background workspace that fires after
  ;; focus has returned to the caller and reclaims the caller's frame with
  ;; the background workspace's panels (the eviction bug `--gui-boot' documents).
  (cond
   (agent-repl--eager-open-in-progress
    (agent-repl--log '(:agent-repl-context "perspective activation can name no agent workspace")
                      "after-persp-activated: suppressed (eager-open in progress)"))
   ((agent-repl--foreign-perspective-p (agent-repl--ws-current-name))
    ;; A perspective persp-mode activated that agent-repl never registered —
    ;; typically a stale leftover from a prior session's saved perspective
    ;; list.  Scheduling `--on-workspace-switch' for it would feed its name
    ;; into the logging ladder with no sink it will ever own; see
    ;; `agent-repl--foreign-perspective-p'.
    (agent-repl--log-verbose '(:agent-repl-central "a foreign perspective has no agent workspace") "after-persp-activated: skipped foreign perspective ws=%s"
                              (agent-repl--ws-current-name)))
   (t
    (let ((ws (agent-repl--ws-current-name)))
      (run-at-time 0 nil #'agent-repl--on-workspace-switch ws)))))

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
Writes no state on WS (its lifecycle is the roster's, and an
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
      ;; NO LIFECYCLE STATE IS WRITTEN.  Panel visibility is a LOCAL
      ;; presentation fact, and it reaches the tab through the bracket-only
      ;; paint that reads the live window layout; the workspace's lifecycle
      ;; is the roster's and closing a panel says nothing about it.
      ;;
      ;; The ONE local preference this records: an EXPLICIT close, so the
      ;; panels-open default (`agent-repl--ws-panels-open-preferred-p') does
      ;; not fight the user by re-showing panels they just dismissed on the
      ;; next switch.
      (agent-repl--note-panels-closed-by-user ws)
      (agent-repl--log ws "elisp.panels.simple-close: ws=%s panels=hidden" ws))
    (agent-repl--close-view ws (lambda ()
                                  (agent-repl--restore-fullscreen-config ws)
                                  (agent-repl--hide-panels)))))

(defun agent-repl-workspace-push-to-back (&optional keep-focus)
  "Push the current workspace's tab to the BACK of the tab order.
The back is the LAST slot: a deprioritized workspace must stop being the
next tab the user lands on, and any slot short of last would leave it
ahead of some other workspace it was just ranked below.

The roster owns the order, so the shuffle goes through
`agent-repl-roster-move-tab-to-back' and the persp-mode names cache is
then set to the SAME list — the two must agree, or the tab bar renders
one order while every roster-driven lookup uses another.

By default focus moves to the workspace that now occupies the vacated
slot, which is the point of the deprio gesture: the user said they are done
here and want to move on.  With KEEP-FOCUS non-nil focus stays on the moved
workspace.

No-op when there is no current workspace, or when it holds no tab."
  (interactive)
  (let* ((current (agent-repl--ws-current-name))
         (order (agent-repl-roster-tab-order))
         (old-index (and current (cl-position current order :test #'string=))))
    (if (null old-index)
        (agent-repl--log current
                         "workspace-push-to-back: ws=%s branch=no-tab" current)
      (let* ((without (remove current order))
             (reordered (agent-repl-roster-move-tab-to-back current))
             (next (and without
                        (nth (min old-index (1- (length without))) without))))
        (agent-repl--log current
                         "workspace-push-to-back: ws=%s old-index=%s next=%s keep-focus=%s"
                         current old-index next keep-focus)
        (agent-repl--ws-update-names-cache reordered)
        (agent-repl--force-tab-bar-redraw)
        (if (and next (not keep-focus))
            (progn
              (agent-repl--ws-switch next)
              (message "Pushed '%s' to the back; switched to '%s'." current next))
          (message "Pushed '%s' to the back." current))))))

(defun agent-repl--on-close (&optional ws)
  "Full close: restore the pre-panel layout, hide panels, save the tab index.
Writes no state, exactly like the simple-close path — a closed workspace
stays listed, and only the roster takes a workspace off the tab bar.
Restores the pre-panel layout via
`agent-repl--restore-fullscreen-config' before hiding so the
frame-filling panels go away cleanly (same contract as
`agent-repl--on-simple-close').  Then hides panels and pushes WS to the
LAST tab position via
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
      ;; NO LIFECYCLE STATE IS WRITTEN — see `agent-repl--on-simple-close'.
      ;; Records the one local preference an explicit close carries so the
      ;; panels-open default does not re-show on the next switch.
      (agent-repl--note-panels-closed-by-user ws)
      (agent-repl--log ws "elisp.panels.close: ws=%s panels=hidden" ws))
    (agent-repl--close-view
     ws
     (lambda ()
       (agent-repl--restore-fullscreen-config ws)
       (agent-repl--hide-panels)))
    (when (and ws (equal ws (agent-repl--ws-current-name)))
      (agent-repl--save-tab-index ws)
      (agent-repl--log ws "on-close: pushing ws=%s to the back" ws)
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
    ;; The identity segment ends at the space that introduces the optional
    ;; display title, or at the closing star when there is no title.
    (let* ((body (substring name (length "*agent-panel-input-")
                            (- (length name) (length "*"))))
           (space (string-search " " body)))
      (if space (substring body 0 space) body)))
   ((string-match-p agent-repl--frontend-buffer-re name)
    (substring name (length "*agent-frontend-") (- (length name) (length "*"))))))

(defun agent-repl--input-buffer-name-for-id (id)
  "Return the name of the live input buffer whose identity segment is ID.
The input buffer's name carries an optional display title, so it cannot
be reconstructed from ID alone; the live buffers are the only place the
current title is written down.  Answers nil when no such buffer lives."
  (seq-some (lambda (buf)
              (let ((name (buffer-name buf)))
                (and name
                     (string-match-p agent-repl--input-buffer-re name)
                     (equal (agent-repl--extract-panel-id name) id)
                     name)))
            (buffer-list)))

(defun agent-repl--partner-buffer-name (name id)
  "Return the partner buffer name for agent panel NAME with identifier ID.
For the input buffer, the partner is the frontend webview buffer, and
vice versa."
  (if (string-match-p agent-repl--input-buffer-re name)
      (format "*agent-frontend-%s*" id)
    ;; The partner input buffer may carry a display title, so its name is
    ;; READ off the live buffer rather than rebuilt; the canonical name is
    ;; the answer only while no such buffer lives, which is exactly the
    ;; case the orphan check is about.
    (or (agent-repl--input-buffer-name-for-id id)
        (format "*agent-panel-input-%s*" id))))

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
      (agent-repl--log-verbose '(:agent-repl-context "panel restoration can run outside a workspace")
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
    (agent-repl--log-verbose '(:agent-repl-context "panel restoration can run outside a workspace")
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
  (let* ((log-ws (agent-repl--ws-current-log-name))
         (scope (if log-ws log-ws agent-repl--global-log-scope)))
    (agent-repl--with-log-context
     scope nil
     (lambda ()
       (if (active-minibuffer-window)
           ;; Skip reconciliation while a minibuffer is active (e.g. the
           ;; `SPC p p' picker).  The window configuration is transient — the
           ;; picker's own window churn is what fired this debounced idle
           ;; timer — so reconciling now would rearrange windows under the open
           ;; picker (and sweep the undeletable minibuffer window).  Closing the
           ;; minibuffer changes the window configuration again, re-firing this
           ;; hook to reconcile the settled layout.
           (agent-repl--log-verbose log-ws
                                    "on-window-change: minibuffer active — deferring reconcile")
         (agent-repl--log-verbose log-ws "on-window-change")
         (agent-repl--sync-panels)
         (agent-repl-window--ensure-layout))))))

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
      ;; TITLES NAME THE BUFFERS (fanout §7), and the daemon may already
      ;; have pushed `naming' before the composer existed -- host.el's
      ;; rename runs on a PUSH, so a buffer born after the last one would
      ;; otherwise wear the bare canonical name forever.  Creation goes
      ;; through the very function the push uses, so the two cannot
      ;; disagree about what this buffer is called.  Inert when no title
      ;; has arrived.
      (agent-repl-host--apply-naming ws)
      (with-current-buffer input-buf
        (when (eq major-mode 'agent-repl-input-mode)
          (agent-repl--log ws "initialize-input-buffer: ws=%s buffer=%s branch=already-initialized"
                            ws (buffer-name input-buf))
          (error "agent-repl--initialize-input-buffer: already initialized ws=%s" ws))
        (agent-repl-input-mode)
        (agent-repl--log ws "initialize-input-buffer: ws=%s buffer=%s mode=enabled history=restore"
                          ws (buffer-name input-buf))
        (agent-repl--history-restore ws))
      ;; THE WAITING LINE IS READ FROM DISK when the composer is born, so a
      ;; prompt held durably before an Emacs restart is shown again at once.
      (agent-repl-held-ingress-refresh ws))))

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
`SPC o c' which only puts the view away.  No state is written: the
workspace's lifecycle is the roster's.

Deliberately NOT folded into `agent-repl--on-close': its other callers
(e.g. `agent-repl-send-and-hide') hide a session that must keep
running."
  (let ((ws (agent-repl--ws-current-name)))
    (unless ws (error "agent-repl--hide-and-preserve-status: no active workspace"))
    (agent-repl--on-close ws)
    (funcall (agent-repl-frontend-kill-fn (agent-repl--ws-frontend ws)) ws)))

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

(defun agent-repl--panels-ensure-host-subscription (ws)
  "Ensure WS's `WatchHostWorkspace' subscription exists, returning non-nil.

THE PANELS ENTRY IS WHERE A WORKSPACE BECOMES OBSERVED: opening the
panels is Emacs saying it is looking at this workspace, and the host
stream is what it looks at it through.  Idempotent — host.el keeps one
subscription per open workspace, so re-entering the panels re-uses it.

Answers nil when WS has no `WorkspaceRef' yet.  That is not a failure:
the roster is what brings a ref, and until it does there is no identity
to subscribe with and no URL to mount a webview at.  The caller shows
the panels anyway, without a webview, rather than refusing the whole
gesture over a fact that is seconds away."
  (let ((ref (and (fboundp 'agent-repl-host-ref) (agent-repl-host-ref ws))))
    (cond
     ((null ref)
      (agent-repl--info ws "elisp.panels.host-subscribe: skipped ws=%s reason=no-ref-yet" ws)
      nil)
     ((and (fboundp 'agent-repl-host-state) (agent-repl-host-state ws))
      (agent-repl--log ws "elisp.panels.host-subscribe: ws=%s already-subscribed" ws)
      t)
     (t
      (let ((conn (or (and (fboundp 'agent-repl-host-conn) (agent-repl-host-conn ws))
                      (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary)))))
        (if (null conn)
            (progn
              (agent-repl--info ws "elisp.panels.host-subscribe: skipped ws=%s reason=no-link" ws)
              nil)
          (agent-repl-host-subscribe conn ws ref)
          (agent-repl--info ws "elisp.panels.host-subscribe: subscribed ws=%s" ws)
          t))))))

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
         (selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
    (agent-repl--log ws "agent-repl selection=%s always-close=%s"
                      (if selection "yes" "no") (if always-close "yes" "no"))
    (cond
     (selection
      (agent-repl--log ws "toggle: branch=send-selection")
      (deactivate-mark)
      ;; The region IS the user's own words, sent by the user, so it
      ;; travels under the origin every other user send uses.  There is no
      ;; `PromptOrigin' for a panel selection, and naming one Emacs never
      ;; spells is refused before a request is built
      ;; (`agent-repl--input-origins').  Being explicit TEXT, the send
      ;; leaves any unrelated composer draft alone (audit-3 #51).
      (agent-repl--send-to-agent selection :user-sent))
     ;; `SPC o C' means "done with this workspace" whether or not its
     ;; view happens to be on screen.
     (always-close
      (agent-repl--log ws "toggle: branch=always-close")
      (funcall close-fn))
     ;; A single press closes whenever EITHER panel is on screen.  The
     ;; old check keyed on the WEBVIEW window alone, so an input-only
     ;; layout (composer up, webview window gone, or a no-ref open that
     ;; showed only the composer) skipped this branch and fell through to
     ;; the show branch below — re-opening on the first press and closing
     ;; only on the second.
     ((agent-repl--panels-any-visible-p)
      (agent-repl--log ws "toggle: branch=close")
      (funcall close-fn))
     (t
      (agent-repl--panels-show-or-open ws)))))

(defun agent-repl--panels-show-or-open (ws)
  "Bring WS's panels up: the toggle's OPEN direction, and the only one.
Split out of `agent-repl--toggle' so the arrival path
\(`agent-repl--panels-open-on-arrival') opens a workspace exactly as
`SPC o c' does rather than growing a second opener beside it.  WS must
be the current workspace — every branch here reasons about the frame the
panels land on.

The three branches, in order:
- an open ALREADY in flight re-shows its placeholder and dispatches
  nothing;
- a frontend that is RUNNING is shown;
- anything else is OPENED, falling back to the composer alone when no
  `WorkspaceRef' has arrived to mount a webview with."
  (let ((fe (agent-repl--ws-frontend ws)))
    (cond
     ;; An open is ALREADY in flight for this workspace.  Re-show the
     ;; placeholder it owns and dispatch nothing: establishment takes
     ;; seconds to tens of seconds, so a second keypress in that window is
     ;; the NORMAL impatient reaction, and answering it with a second
     ;; establishment stacks a duplicate `openWorkspace' behind a
     ;; placeholder the user is already looking at.
     ((agent-repl--open-progress-active-p ws)
      (agent-repl--log ws "toggle: branch=already-opening")
      (agent-repl--note-panels-shown ws)
      (agent-repl--open-progress-start ws))
     ((funcall (agent-repl-frontend-running-p-fn fe) ws)
      (agent-repl--log ws "toggle: branch=show")
      (agent-repl--note-panels-shown ws)
      (agent-repl--open-progress-start ws)
      (agent-repl--settle-placeholder
       ws (funcall (agent-repl-frontend-show-fn fe) ws)))
     (t
      (agent-repl--log ws "toggle: branch=open")
      ;; The user is opening the panels: clear any explicit-close
      ;; preference so the panels-open default applies again on later
      ;; switches (`agent-repl--ws-panels-open-preferred-p').
      (agent-repl--note-panels-shown ws)
      ;; Raised BEFORE the dispatch, inside the command that read the key:
      ;; the first redisplay after `SPC o c' must already carry the
      ;; workspace's name, not the frame the user pressed it on.
      (agent-repl--open-progress-start ws)
      (if (agent-repl--panels-ensure-host-subscription ws)
          (agent-repl--settle-placeholder
           ws (funcall (agent-repl-frontend-open-fn fe) ws))
        ;; NO REF YET, so no webview: show the input buffer and let the
        ;; roster bring the identity the mount needs.  Refusing the whole
        ;; gesture would be worse — the user asked for the workspace's
        ;; panels, and the composer half of them is available now.
        (agent-repl--ensure-input-buffer ws)
        (agent-repl--settle-placeholder ws :no-webview))))))


;;;; Opening a workspace opens its panels

(defvar agent-repl--panels-arrivals-armed nil
  "Non-nil once the FIRST roster reconcile of this Emacs session has run.

The workspaces that were already open when Emacs started arrive on that
first push, and their panels are the STARTUP path\='s business, not this
one\='s (owner ruling, 2026-09-13, item 6).  Every arrival after it is a
workspace that BECAME open with this editor watching, so it opens its
panels immediately.  Armed by `agent-repl--panels-arm-arrivals\='.")

(defvar agent-repl--panels-arrival-reasons (make-hash-table :test 'equal)
  "Ref id -> the reason string the verb that asked for it recorded.

A verb knows WHY a workspace is about to arrive; the roster, which is
where it actually lands, does not.  The verb writes the reason here
against the minted ref id and the arrival CONSUMES it, so a second
arrival for the same id cannot re-use a stale one.  An arrival with no
entry is a daemon-side one (`arrived\=').")

(defun agent-repl--panels-arm-arrivals ()
  "Mark the startup roster as delivered, so later arrivals open their panels.
Called at the end of every reconcile; the first call is the one that
matters and the rest are inert."
  (unless agent-repl--panels-arrivals-armed
    (setq agent-repl--panels-arrivals-armed t)
    (agent-repl--log '(:agent-repl-central
                      "the startup roster spans every workspace")
                     "elisp.panels.arrivals-armed")))

(defun agent-repl--panels-note-arrival-reason (id reason)
  "Record REASON as why the workspace with ref ID is about to arrive open.
REASON is one of `created\=', `reopened\=', `registered\=' or `forked\=';
an arrival nobody claimed reads `arrived\='.  No-op for a nil ID."
  (when id
    (puthash id reason agent-repl--panels-arrival-reasons)
    (agent-repl--log '(:agent-repl-central
                      "a workspace that has not arrived owns no sink")
                     "elisp.panels.arrival-reason-noted id=%s reason=%s" id reason)))

(defun agent-repl--panels-take-arrival-reason (id)
  "Return and forget the reason recorded for ref ID, or \"arrived\"."
  (let ((reason (and id (gethash id agent-repl--panels-arrival-reasons))))
    (when reason (remhash id agent-repl--panels-arrival-reasons))
    (or reason "arrived")))

(defun agent-repl--panels-open-on-arrival (ws id)
  "Open WS\='s panels because WS just became open in this editor session.

OPENING A WORKSPACE OPENS ITS PANELS, BEFORE IT IS SWITCHED TO (owner
ruling, 2026-09-13, item 6).  Creation, fork, one-shot, re-open,
register and every daemon-side arrival land in exactly one place --
`agent-repl-roster--open-tab\=' -- and this is what that landing does about
them.  A plain SWITCH to a workspace that is already open never reaches
here, so switching still changes nothing about panel state.

ID is the arriving row\='s ref id, which is how the reason the verb
recorded is found (`agent-repl--panels-take-arrival-reason\=').

Two refusals, both silent about the panels:
- before the startup roster has been delivered
  \(`agent-repl--panels-arrivals-armed\'), unless a verb claimed this
  arrival by name -- a workspace that was already open when Emacs
  started is the startup path\='s to restore;
- a workspace that is not live.

The open itself runs through `agent-repl--panels-show-or-open\=', the
toggle\='s own open direction, so it is idempotent and the webview
pre-creation park behaves exactly as it does for `SPC o c\='.  It is
anchored in WS through `agent-repl--call-in-background-workspace\='
because WS is NOT current yet: the panels must be built into WS\='s own
perspective, and the caller\='s focus restored afterwards."
  (let ((reason (agent-repl--panels-take-arrival-reason id)))
    (cond
     ((and (not agent-repl--panels-arrivals-armed)
           (equal reason "arrived"))
      (agent-repl--log ws "elisp.panels.open-on-arrival: skipped ws=%s reason=startup-restore" ws))
     ((not (agent-repl--ws-live-p ws))
      (agent-repl--log ws "elisp.panels.open-on-arrival: skipped ws=%s reason=not-live" ws))
     (t
      (agent-repl--call-in-background-workspace
       ws
       (lambda ()
         (if (agent-repl--panels-any-visible-p)
             (agent-repl--log ws "elisp.panels.open-on-arrival: ws=%s branch=already-open" ws)
           (agent-repl--panels-show-or-open ws))
         (agent-repl--info ws "elisp.panels.opened-on-arrival ws=%s reason=%s" ws reason)))))))

(defun agent-repl--settle-placeholder (ws outcome)
  "Tear WS's placeholder down unless OUTCOME says the open is still in flight.

A frontend's open/show capability returns `:pending' precisely when it
has taken responsibility for reporting its own outcome later — which is
also when it owns the placeholder's resolution.  ANY OTHER return value
means the capability already finished inside this call, so the
placeholder has nothing left to describe and is removed here.

Without this, a frontend that opens SYNCHRONOUSLY would leave a
placeholder nobody resolves, and the escalation timer would eventually
paint a warning over a workspace that opened perfectly."
  (if (eq outcome :pending)
      outcome
    (agent-repl--log ws "toggle: settling placeholder outcome=%S" outcome)
    (agent-repl--open-progress-finish ws)
    outcome))

(defun agent-repl ()
  "Hide Agent REPL panels and deprio the workspace.
If text is selected: send it directly to the agent (orthogonal to hide).
Otherwise: hide both panels
\(no-op if already hidden), and push the workspace tab to the back.
Always hides, regardless of whether the agent is running or panels are
currently visible.  The workspace stays listed on the tab-bar.
Bound to `SPC o C'.  See `agent-repl-simple' for the no-tab-bar variant."
  (interactive)
  (agent-repl--toggle #'agent-repl--hide-and-preserve-status :always-close t))

(defun agent-repl-simple ()
  "Toggle Agent REPL panels with a plain close (no tab-bar update).
Same dispatch as `agent-repl' except the close branch only hides the
panels — no save-tab-index, no
push-to-back.  Bound to `SPC o c'."
  (interactive)
  (agent-repl--toggle #'agent-repl--simple-hide-and-preserve-status))

;;;; Session cleanup

(defun agent-repl--sigkill-if-alive (proc)
  "Send SIGKILL to PROC if it is still alive."
  (if (process-live-p proc)
      (progn
        (agent-repl--log '(:agent-repl-context "process cleanup can run after workspace teardown")
                         "sigkill-if-alive: branch=signal proc=%s" proc)
        (signal-process proc 'SIGKILL))
    (agent-repl--log-verbose '(:agent-repl-context "process cleanup can run after workspace teardown")
                              "sigkill-if-alive: branch=already-dead proc=%s" proc)))

(defun agent-repl--schedule-sigkill (proc)
  "Schedule a SIGKILL for PROC after 0.5s if it's still alive."
  (agent-repl--log '(:agent-repl-context "process cleanup can run after workspace teardown")
                   "schedule-sigkill: scheduling for proc=%s" proc)
  (run-at-time agent-repl-sigkill-delay nil #'agent-repl--sigkill-if-alive proc))

(defun agent-repl--kill-workspace-buffers (ws)
  "Kill every buffer (and attached process) belonging to persp WS.
Idempotent: no-op when persp-mode is inactive, the persp does not
exist, or the persp slot holds a symbol sentinel rather than a real
perspective.  Each buffer is killed inside its own `condition-case' so
one bad buffer cannot block the rest.  File-visiting buffers are
marked unmodified before killing so `kill-buffer' does not prompt —
the user has already confirmed the destructive kill.

Agent buffers owned by a different workspace (see
`agent-repl--foreign-owned-buffer-p') are skipped, not killed: persp-mode
can drift another workspace's live panel into this persp, and nuking it
would wipe that workspace's running session.  A buffer owned by no
workspace that another live perspective also holds -- the workspace a
teardown lands on among them -- is skipped too
\(`agent-repl--ws-shared-unowned-buffer-p'): it is that workspace's
buffer as well, and a teardown must change nothing about another
workspace."
  (when (agent-repl--ws-system-available-p)
    (when-let ((persp (agent-repl--ws-resolve-persp ws)))
      (let ((bufs (agent-repl--ws-buffers persp))
            (kill-buffer-query-functions nil))
        (agent-repl--log ws "kill-workspace-buffers: count=%d" (length bufs))
        (dolist (buf bufs)
          (condition-case err
              (cond
               ((agent-repl--foreign-owned-buffer-p buf ws)
                (agent-repl--log ws "kill-workspace-buffers: SKIP foreign buf=%s owner=%s"
                                  (agent-repl--safe-buffer-name buf)
                                  (agent-repl--buffer-owner buf)))
               ((agent-repl--ws-shared-unowned-buffer-p buf ws)
                (agent-repl--log ws "kill-workspace-buffers: SKIP shared buf=%s"
                                  (agent-repl--safe-buffer-name buf)))
               (t
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
                                    buf-name (- (float-time) t-buf)))))
            (error
             (agent-repl--warn ws "kill-workspace-buffers: error on %s: %S"
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
  "Focus the agent input buffer, or return to the webview if already there.
If the agent isn't running, start it (same as `agent-repl')."
  (interactive)
  (let ((ws (agent-repl--ws-current-name)))
    (cond
     ;; Already in the input buffer — jump back
     ((eq (current-buffer) (agent-repl--ws-get ws :input-buffer))
      ;; JUMP BACK IS TO THE WEBVIEW, NOT LEFTWARD.  The panels are
      ;; STACKED — the composer is split BELOW the webview
      ;; (`agent-repl--frontend-display-webview') — so there is no window
      ;; to the left of the composer and `evil-window-left' SIGNALLED
      ;; ("No window left from selected window"): pressing `SPC o v' a
      ;; second time raised an error instead of leaving the composer.
      ;; The window to come back to is the workspace's own webview.
      ;;
      ;; With no webview window the press is a recorded no-op: there is
      ;; nowhere to jump back to, and an error is not an answer to that.
      (let* ((webview (agent-repl--ws-get ws :frontend-buffer))
             (win (and (buffer-live-p webview) (get-buffer-window webview))))
        (if win
            (progn
              (agent-repl--log ws "focus-input branch=jump-back win=%s" win)
              (select-window win))
          (agent-repl--log ws "focus-input branch=jump-back-nowhere webview=%s"
                           (agent-repl--safe-buffer-name webview)))))
     ;; No composer buffer yet — bring the workspace's panels up.  The
     ;; question here is editor-local ("are this workspace's panels
     ;; mounted?"), not "is a session running": session liveness is the
     ;; daemon's and reaches Emacs as host state, never as a local probe.
     ((not (agent-repl--ws-get ws :input-buffer))
      (agent-repl--log ws "focus-input branch=mount-panels")
      (agent-repl))
     ;; Running but panels hidden — show them
     (t
      (agent-repl--log ws "focus-input branch=show-or-focus")
      (unless (agent-repl--panels-visible-p)
        (agent-repl--frontend-dispatch-show ws))
      (when-let ((win (get-buffer-window (agent-repl--ws-get ws :input-buffer))))
        (select-window win))))))

(defun agent-repl--panels-own-buffers (ws)
  "Return the buffers WS's panel mount puts on the frame.
Its agent view, its input panel, and -- while an open is in flight --
its open placeholder (`agent-repl--open-progress-placeholder'), which
takes the main-area window the view is about to mount into.  The
placeholder is part of the mount: the frame under it is the frame the
panels are covering."
  (delq nil (list (agent-repl-window--panel-buffer :view ws)
                  (agent-repl-window--panel-buffer :input ws)
                  (and (fboundp 'agent-repl--open-progress-placeholder)
                       (agent-repl--open-progress-placeholder ws)))))

(defun agent-repl--save-pre-panel-layout (ws site)
  "Record the layout WS's panels are about to cover as its `:fullscreen-config'.
THE ONE PLACE the pre-panel layout is saved.  SITE names the caller in
the record.

It is saved the moment ANY part of the mount first takes the frame --
the open placeholder when an open shows one, the view otherwise -- and
kept, not re-saved, while the frame holds nothing but WS's own panels
\(`agent-repl--panels-cover-frame-p'): the layout underneath is then
the one already saved, and saving again would record the mount itself
as the frame to restore.  That is what used to happen: the placeholder
took the main area before the view saved, so the saved layout WAS the
placeholder, the close restored it after the placeholder was killed,
and Emacs filled the window with the webview -- which the layout
reconciler then remounted the composer beside, so a close undid itself."
  (if (agent-repl--panels-cover-frame-p ws)
      (agent-repl--log ws "%s: kept-fullscreen-layout reason=panels-cover-frame" site)
    (agent-repl--ws-put ws :fullscreen-config (current-window-configuration))
    (agent-repl--log ws "%s: saved-fullscreen-layout" site)))

(defun agent-repl--panels-cover-frame-p (ws)
  "Return non-nil when WS's own panels are all this frame's main area holds.

That is the state a `:fullscreen-config' is recorded FOR: the mount
covers the frame whole (fullscreen is the sole display format), so the
saved layout is the one underneath it and restoring the layout is how
the panels come down.  Side windows are exempt because the mount
preserves them and the saved layout carries them too
\(`agent-repl--clear-main-area-for-panels').

Any OTHER window on the frame says the frame has moved on from what the
configuration describes."
  (let ((own (agent-repl--panels-own-buffers ws)))
    (cl-every (lambda (win)
                (or (agent-repl-window--side-window-p win ws)
                    (memq (window-buffer win) own)))
              (window-list))))

(defun agent-repl--fullscreen-config-stale-p (ws)
  "Return non-nil when WS's `:fullscreen-config' no longer describes this frame.

`:fullscreen-config' is the frame WITHOUT WS's panels, recorded the
moment those panels take it whole.  It stays true for exactly as long
as they still hold it (`agent-repl--panels-cover-frame-p'), because
restoring it is HOW they come down.

A bare `delete-other-windows' from the composer breaks that: it takes
the webview window while the composer, being delete-protected
\(`no-delete-other-windows'), survives alone.  `agent-repl--panels-visible-p'
then answers nil, so no close path runs and the landing-era
configuration stands while the user builds a frame of their own on top
of it.  Restoring it later would put the LANDING's windows back over
that work — the windows the user had would simply have none.  So a
configuration whose frame carries windows that are not this
workspace's panels is stale: it is DROPPED rather than restored here,
and REPLACED rather than kept by the next mount
\(`agent-repl--frontend-display-webview').

A frame holding only the remains of the mount is NOT stale — the
window-change reconciler repairs exactly that state
\(`agent-repl-window--ensure-layout'), and the layout underneath is
still the one the configuration was recorded from.

The judgement needs a webview to judge: a workspace whose view buffer
was never created or has been KILLED (a vterm workspace, a webview lost
to a crash) is left alone here, since its configuration is the only
teardown its close has."
  (let ((webview (and ws (agent-repl--ws-get ws :frontend-buffer))))
    (and (buffer-live-p webview)
         (not (agent-repl--panels-cover-frame-p ws))
         t)))

(defun agent-repl--restore-fullscreen-config (ws)
  "Restore WS's saved pre-panel layout, clearing `:fullscreen-config'.
Returns non-nil when a restore happened, nil when WS had no saved
config, and nil when the saved config is stale
\(`agent-repl--fullscreen-config-stale-p') — which DROPS it, so the
caller falls back to closing the panel windows one by one instead of
restoring the landing over the user's own work.

`:fullscreen-config' is the window layout captured the moment the
frame-filling panels were opened (fullscreen is the sole display
format).  The close paths (`agent-repl--on-simple-close' for `SPC o c' and
`agent-repl--on-close' for `SPC o C') restore it before hiding so the
work windows the panels covered come back rather than the close
stranding a panel onscreen.  Only the saved-config case is handled: a
frame with no `:fullscreen-config' has no layout to restore to."
  (when-let ((saved (and ws (agent-repl--ws-get ws :fullscreen-config))))
    (if (agent-repl--fullscreen-config-stale-p ws)
        (progn
          (agent-repl--ws-put ws :fullscreen-config nil)
          (agent-repl--log ws "restore-fullscreen-config: dropped stale config ws=%s" ws)
          nil)
      (set-window-configuration saved)
      (agent-repl--ws-put ws :fullscreen-config nil)
      t)))

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
