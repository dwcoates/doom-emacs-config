;;; sidebar.el --- Workspaces sidebar: roster feed + actions -*- lexical-binding: t; -*-

;;; Commentary:

;; Feeds the webapp's workspaces sidebar and services its actions.
;;
;; The roster universe is the LIVE workspaces that still hold a
;; perspective: registered, non-tombstoned (`agent-repl--live-ws-names')
;; AND present in the tab bar (`agent-repl--sidebar-rostered-p').
;; Tombstoned registrations, snapshot-only history, and workspaces whose
;; perspective was killed do not render — a workspace that is GONE loses
;; its row outright.  Greying (`agent-repl--sidebar-closed-p') is
;; reserved for the opposite case: a workspace that still exists, tab and
;; all, whose agent-repl panes the user dismissed.  Merged workspaces are
;; the one perspective-less exception; they outlive their tab inside the
;; Recently Merged section and keep receding there.
;;
;; Data flows one way, Emacs -> daemon -> every client: Emacs owns the
;; workspace model (workspace.el), so it builds the roster — repos,
;; family-nested rows, per-row lifecycle status — shapes it as the frozen
;; `WorkspaceRoster' contract and PUBLISHES it to the daemon as a
;; `publishWorkspaceRoster' frontend command.  The daemon retains it and
;; rebroadcasts it; the roster is ordinary daemon surface, snapshotted on
;; connect like every other frame.
;;
;; It used to be injected into each live webview by execute-script, which
;; made it per-view state a second view could not share and a reconnect
;; could not recover.  Two consequences of the move are worth stating:
;;
;;   * The 1Hz signature (status.el's state tick) is now ONLY a change
;;     detector.  A skipped tick cannot lose delivery, because the daemon
;;     still holds the last roster published and hands it to whoever
;;     connects — where a missed injection used to leave a webview blank.
;;   * A (re)connect publishes unconditionally, because the one thing the
;;     daemon cannot survive is its own restart: it comes back retaining
;;     no roster, and Emacs is the only author of one.
;;
;; Actions flow the long way around, webview -> daemon -> Emacs: the
;; webapp cannot call into Emacs, so the daemon persists each UI-only effect
;; as a HostAction and pushes it over the frontend UDS connection.  The
;; workspace-create client dispatches those durable actions into the
;; "switch" / "fold" handlers here and acknowledges completion.
;;
;; Keyboard navigation never leaves Emacs: `C-S-n' / `C-S-p' (input.el,
;; from the input window — the xwidget swallows keystrokes, so the page
;; itself never sees keys) move an Emacs-owned cursor that rides the
;; roster push as `navDir' AND open the workspace it lands on, so the
;; highlight always doubles as the active selection.  `C-S-RET' unfolds
;; the cursor row's detail panel via the expand hook — the one gesture
;; that touches webapp-owned panel state instead of the roster.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function agent-repl--frontend-webview-execute-script "frontend" (buf script))
(declare-function agent-repl--picker-open-selection "commands" (payload))
(declare-function agent-repl--force-tab-bar-redraw "status" ())
(declare-function agent-repl--frontend-boot-session "frontends"
                  (ws &optional project-dir-hint active-env-hint))
(declare-function agent-repl--task-get "tasks" (id))
(declare-function agent-repl--task-create "tasks" (title))
(declare-function agent-repl--task-toggle-done "tasks" (id))
(declare-function agent-repl--task-open "tasks" (id))
(declare-function agent-repl--task-assign-workspace "tasks" (name id))
(declare-function agent-repl--ws-effective-task-id "tasks" (name))
(declare-function agent-repl--tasks-sorted "tasks" ())
(declare-function agent-repl--tasks-signature "tasks" ())
(declare-function agent-repl--uds-connected-p "frontend-uds" ())
(declare-function agent-repl--uds-send-command "frontend-uds"
                  (field payload &optional workspace process))
(declare-function agent-repl--uds-track-command "frontend-uds"
                  (request-id field workspace &optional on-failure on-success on-challenge))

;;;; ---- The view selector -----------------------------------------------

(defvar agent-repl--sidebar-view :repository
  "Which sidebar view is active: `:repository' or `:task'.
`:repository' (the default) groups workspaces by their git repo; `:task'
groups them under the user-defined tasks (tasks.el) they belong to.
Session-scoped like the repo-fold set — a view preference the user
re-picks per session rather than persisted state.")

(defconst agent-repl--sidebar-view-wire
  '((:repository . "repository")
    (:task       . "task"))
  "Maps `agent-repl--sidebar-view' keywords onto the webapp's wire strings.
The value set is the webapp's WorkspaceRoster.view union
\(webapp/src/sidebar.ts) — the two sides are one contract and MUST match.")

(defconst agent-repl--sidebar-no-task-key "__no_task__"
  "Section key and fold key of the catch-all \"No task\" section.
Shaped like a task id without being one so the section renders through
the same group path; workspaces belonging to no task collect here.")

(defun agent-repl--sidebar-view-wire ()
  "Return the wire string for the active view, signalling on an unknown one."
  (or (alist-get agent-repl--sidebar-view agent-repl--sidebar-view-wire)
      (progn
        (agent-repl--log nil "sidebar-view-wire: unknown view=%S" agent-repl--sidebar-view)
        (error "agent-repl--sidebar-view-wire: unknown view %S"
               agent-repl--sidebar-view))))

;;;; ---- The host-hook contract ------------------------------------------

(defconst agent-repl--sidebar-expand-hook "agentReplWorkspaceExpand"
  "Name of the webapp global that toggles a row's detail panel.
Planted on `window' at boot (`EXPAND_HOOK' in webapp/src/sidebar.ts) —
the two names are one contract and MUST match.  Receives one argument,
the row's canonical project dir, as a JSON string.  Backs `C-S-RET',
whose target panel is webapp-owned client state (`openDirs') the
roster never carries, so the keyboard reaches it through this hook
rather than a roster rebuild.")

(defconst agent-repl--sidebar-status-wire
  '((:submitting      . ("submitting"      . nil))
    (:thinking        . ("thinking"        . nil))
    (:clearing        . ("clearing"        . nil))
    (:compacting      . ("compacting"      . nil))
    (:permission      . ("permission"      . nil))
    (:init            . ("init"            . nil))
    (:severed         . ("severed"         . nil))
    (:hibernated      . ("hibernated"      . nil))
    (:done            . ("done"            . nil))
    (:interrupted     . ("interrupted"     . nil))
    (:ready           . ("ready"           . nil))
    (:idle            . ("ready"           . nil))
    (:idle-async      . ("idle-async"      . nil))
    (:vendor-blocked  . ("vendor-blocked"  . nil))
    (:start-failed    . ("start-failed"    . nil))
    (:degraded        . ("degraded"        . nil))
    (:dead            . ("dead"            . nil))
    (:merge-enqueuing . ("merge-enqueuing" . nil))
    (:merging         . ("merging"         . nil))
    (:merge-queued    . ("merge-queued"    . nil))
    (:merge-conflict  . ("merge-conflict"  . nil))
    (:merge-failed    . ("merge-failed"    . nil))
    (:merged          . ("merged"          . t)))
  "Maps `agent-repl--ws-render-status' keywords onto sidebar wire rows.
Each entry's value is (WIRE-STRING . PERSPECTIVE-INDEPENDENT-P).

WIRE-STRING is the dot the row draws.  The dots speak the SAME
six-color vocabulary the tab-bar does, so a workspace can never read
one way in the rail and another in the tabs.  Nothing coarsens any
more: `:idle-async' is its own yellow dot rather than borrowing idle's
ring, and `:vendor-blocked' is its own purple dot rather than sharing
the grey one with `:dead'.  A workspace that read differently in the
two places was exactly what the old coarsening produced.

`:start-failed' and `:degraded' are mapped rather than absent: both are
real render states the daemon can push, and an unmapped one signals an
error instead of drawing a dot.

PERSPECTIVE-INDEPENDENT-P marks a status that describes CONCLUDED work:
its dot is meaningful even when the workspace has no open perspective,
because the status itself — not the tab-bar — is what is being
reported.  `:merged' is the only such status: `preserve-entry' keeps a
landed merge's roster row alive specifically so it can outlive its tab
inside Recently Merged, so its dot must survive perspective loss.  Every
in-flight merge status (`:merge-enqueuing', `:merging', `:merge-queued',
`:merge-conflict', `:merge-failed') is still live work riding a real
tab, exactly like `:ready' or `:thinking', so it stays perspective-bound:
losing the tab mid-merge is itself the noteworthy fact, and that fact is
`\"inactive\"'.

The wire-string set is the webapp's closed WorkspaceRow.status union
\(webapp/src/sidebar.ts) — the two sides are one contract and MUST
stay in sync.  \"done-viewed\" and \"inactive\" are absent
here because neither is a render-status keyword —
`agent-repl--sidebar-wire-status' derives \"done-viewed\" from `:done' +
`agent-repl--ws-open-p'.")

(defun agent-repl--sidebar-wire-status (name)
  "Return the wire status string for known workspace NAME.
\"inactive\" when NAME has no open perspective (`agent-repl--ws-open-p'
nil) AND its render status is perspective-bound: a workspace registered
in the roster but absent from the tab-bar has no live session whose
lifecycle a status dot could report, so the webapp draws a question
mark in place of the dot.  A status marked perspective-independent in
`agent-repl--sidebar-status-wire' is the one exception — it describes
concluded work the row is reporting on its own account, not the
workspace's tab-bar membership, so it serializes regardless of
`agent-repl--ws-open-p'.

\"none\" when `agent-repl--ws-render-status' reports nil (tombstoned /
unborn) and NAME has an open perspective; a perspective-less workspace
with no render status still reads \"inactive\", since there is no
concluded-work status to report on its own account.

Signals on an unmapped keyword: a render state missing from
`agent-repl--sidebar-status-wire' means a new state was added without
extending the sidebar contract — a violated invariant, never a silent
default dot."
  (let ((kw (agent-repl--ws-render-status name)))
    (cond
     ((null kw)
      (if (agent-repl--ws-open-p name)
          (progn
            (agent-repl--log-verbose name "sidebar-wire-status: ws=%s open=t status=nil -> none" name)
            "none")
        (agent-repl--log-verbose name "sidebar-wire-status: ws=%s open=nil status=nil -> inactive" name)
        "inactive"))
     (t (if-let ((entry (alist-get kw agent-repl--sidebar-status-wire)))
            (let ((wire (car entry))
                  (independent (cdr entry)))
              (if (or independent (agent-repl--ws-open-p name))
                  (progn
                    (agent-repl--log-verbose name "sidebar-wire-status: ws=%s open=%s status=%s -> %s"
                                              name (agent-repl--ws-open-p name) kw wire)
                    wire)
                (agent-repl--log-verbose name "sidebar-wire-status: ws=%s open=nil status=%s -> inactive" name kw)
                "inactive"))
          (agent-repl--log name "sidebar-wire-status: ws=%s unmapped render-status=%S" name kw)
          (error "agent-repl--sidebar-wire-status: unmapped render state %S for ws=%s"
                 kw name))))))

(defconst agent-repl--sidebar-status-arm
  '(("submitting"      . :submitting)
    ("thinking"        . :thinking)
    ("clearing"        . :clearing)
    ("compacting"      . :compacting)
    ("permission"      . :permission)
    ("done"            . :done)
    ("interrupted"     . :interrupted)
    ("ready"           . :ready)
    ("idle-async"      . :idleAsync)
    ("vendor-blocked"  . :vendorBlocked)
    ("init"            . :init)
    ("severed"         . :severed)
    ("hibernated"      . :hibernated)
    ("start-failed"    . :startFailed)
    ("degraded"        . :degraded)
    ("dead"            . :dead)
    ("merge-enqueuing" . :mergeEnqueuing)
    ("merging"         . :merging)
    ("merge-queued"    . :mergeQueued)
    ("merge-conflict"  . :mergeConflict)
    ("merge-failed"    . :mergeFailed)
    ("merged"          . :merged)
    ("none"            . :none)
    ("inactive"        . :inactive))
  "Maps a sidebar wire status string onto its protojson `status' ARM KEY.

THE SET ARM IS THE STATUS.  `RosterRow.status' is a oneof of 24 empty
messages, so the row carries no `status' field at all: it carries ONE of
these keys, whose value is the empty object `{}'.  Which key is present
is the entire lifecycle assertion — there is no value to disagree with
it, and an unset oneof is a contract breach the consumers reject rather
than draw as a default dot.

Keys are protojson field names, hence lowerCamel (`idleAsync',
`mergeConflict', `startFailed'), while the wire strings this maps FROM
stay kebab-case.  The pairing is the exact inverse of the webapp's
`ROSTER_ROW_STATUS_KEYWORD' table (webapp/src/frontend-proto.ts): that
table names each arm's keyword, this one recovers each keyword's arm.
The two sides and the proto's oneof are one contract and MUST match.

TOTAL over `agent-repl--sidebar-wire-status's output, deliberately: a
wire status with no arm here signals rather than publishing a row with
no lifecycle.")

(defun agent-repl--sidebar-status-arm (wire)
  "Return the protojson `RosterRow.status' arm keyword for WIRE.
Signals on an unmapped status: a row published with no arm set is
rejected by every consumer as a contract breach, so failing here — where
the offending status is still named — beats emitting one."
  (or (cdr (assoc wire agent-repl--sidebar-status-arm))
      (progn
        (agent-repl--log nil "sidebar-status-arm: unmapped wire status=%S" wire)
        (error "agent-repl--sidebar-status-arm: unmapped wire status %S" wire))))

(defun agent-repl--sidebar-closed-p (name)
  "Return non-nil when roster workspace NAME renders greyed (closed).
Greyed is RESERVED for the panels-dismissed condition: the workspace
still exists — it has its tab — but its agent-repl panes are closed, so
`:repl-state' reads `:inactive' (close-panels-on-open.el is what puts
it there).  A nil `:repl-state' is NOT closed: that is a session that
never started, which the \"none\" status dot already conveys.

Explicitly NOT keyed on perspective-lessness any more.  A workspace
whose perspective is gone is gone, and `agent-repl--sidebar-entries'
drops its row outright rather than greying it — greying a row for a
workspace the user can no longer switch to was the confusion this
predicate used to produce.

A merged workspace is the one perspective-less case that still greys,
because merged rows deliberately outlive their tab in the Recently Merged
section and must keep receding there exactly as before.  Merged-ness is
`agent-repl--sidebar-merged-p' — the durable merge instant, not the
transient `:merged' render state."
  (let ((repl-state (agent-repl--ws-get name :repl-state))
        (merged (agent-repl--sidebar-merged-p name)))
    (let ((closed (or merged (eq repl-state :inactive))))
      (agent-repl--log-verbose name "sidebar-closed-p: ws=%s merged=%s repl-state=%s -> closed=%s"
                                name merged repl-state closed)
      closed)))

;;;; ---- The recently-merged window ---------------------------------------

(defconst agent-repl--sidebar-merged-key "__recently_merged__"
  "Fold key and roster key of the Recently Merged section.
Deliberately shaped like a repo key without being one: reusing the repo
fold machinery (`agent-repl--repo-folded-p') lets the section collapse
through the same click path and the same fold set the repo sections use,
so the webapp needs no second fold contract.")

(defcustom agent-repl-sidebar-merged-window-seconds (* 6 60 60)
  "Inactivity gap, in seconds, that wipes the Recently Merged section.
Once agent-repl observes no user activity for this long, every
already-merged workspace ages out of the section and then renders
nowhere in the sidebar.  Both flavors of inactivity count: Emacs sitting
idle, and Emacs not running at all."
  :type 'integer
  :group 'agent-repl)

(defvar agent-repl--sidebar-merged-epoch nil
  "Epoch seconds after which a completed merge still counts as recent.
`agent-repl--sidebar-refresh-merged-window' bumps it to now on every
observed inactivity gap, and that bump is what wipes the section.")

(defvar agent-repl--sidebar-last-activity nil
  "Epoch seconds of the last observed USER activity.
Persisted so the gap check spans restarts: a shutdown longer than
`agent-repl-sidebar-merged-window-seconds' must read as inactivity
exactly like an equally long idle stretch does.")

(defvar agent-repl--sidebar-merged-persisted-at 0
  "Epoch seconds of the last merged-window write, throttling disk churn.")

(defun agent-repl--sidebar-merged-state-file ()
  "Absolute path of the file persisting the recently-merged window."
  (agent-repl--global-state-file "sidebar-merged-window.el"))

(defun agent-repl--sidebar-load-merged-window ()
  "Hydrate the merged-window state from disk into memory."
  (let ((file (agent-repl--sidebar-merged-state-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (read (current-buffer))))
          (setq agent-repl--sidebar-last-activity (plist-get data :last-activity)
                agent-repl--sidebar-merged-epoch (plist-get data :epoch))
          (agent-repl--log-verbose nil "sidebar-merged-window: loaded state last-activity=%s epoch=%s"
                                    agent-repl--sidebar-last-activity
                                    agent-repl--sidebar-merged-epoch))))))

(defun agent-repl--sidebar-save-merged-window ()
  "Write the merged-window state to disk."
  (let ((file (agent-repl--sidebar-merged-state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (prin1 (list :last-activity agent-repl--sidebar-last-activity
                   :epoch agent-repl--sidebar-merged-epoch)
             (current-buffer)))))

(defun agent-repl--sidebar-refresh-merged-window ()
  "Advance the activity stamp, wiping the merged window across a gap.
Returns the current epoch.

The stamp is the moment of the last real user input, `now' minus
`current-idle-time', which is what makes ONE comparison cover both
flavors of inactivity.  While Emacs sits idle the stamp freezes at the
last keystroke, so the gap grows with the idle time.  Across a restart
the persisted stamp is still the pre-shutdown keystroke, so the gap is
caught on the first tick back.  A tick during ordinary use keeps the
stamp near `now', so the gap stays at zero.

The write is throttled: this runs at 1Hz and the state changes at human
timescales, so it hits disk only when the epoch actually moved or a
minute has passed."
  (unless agent-repl--sidebar-last-activity
    (agent-repl--log-verbose nil "sidebar-merged-window: no in-memory activity stamp; loading persisted state")
    (agent-repl--sidebar-load-merged-window))
  (let* ((now (float-time))
         (idle (if (current-idle-time) (float-time (current-idle-time)) 0))
         (activity (- now idle))
         (last (or agent-repl--sidebar-last-activity activity))
         (wiped nil))
    (when (>= (- now last) agent-repl-sidebar-merged-window-seconds)
      (agent-repl--log nil "sidebar-merged-window: %.0fs gap >= %ds — wiping recently-merged"
                        (- now last) agent-repl-sidebar-merged-window-seconds)
      (setq agent-repl--sidebar-merged-epoch now
            wiped t))
    (setq agent-repl--sidebar-last-activity (max last activity))
    (when (or wiped (> (- now agent-repl--sidebar-merged-persisted-at) 60))
      (setq agent-repl--sidebar-merged-persisted-at now)
      (agent-repl--sidebar-save-merged-window)
      (agent-repl--log-verbose nil "sidebar-merged-window: persisted wiped=%s last-activity=%.0f epoch=%s"
                                wiped agent-repl--sidebar-last-activity
                                agent-repl--sidebar-merged-epoch))
    agent-repl--sidebar-merged-epoch))

(defun agent-repl--sidebar-merged-at (name)
  "Return NAME's merge-completion time as epoch seconds, or 0 when absent.
Sourced from `:merge-completed-at', which `frontend-state.el' retains off
every pushed `WorkspaceState' (`agent-repl--frontend-retain-merged-at')
and `session.el' restores from saved state."
  (let ((at (agent-repl--ws-get name :merge-completed-at)))
    (cond ((null at) 0)
          ((numberp at) at)
          (t (float-time at)))))

(defun agent-repl--sidebar-merged-p (name)
  "Return non-nil when workspace NAME belongs to the Recently Merged section.

Keyed on the DURABLE merge instant plus the absence of a perspective,
NOT on the render state being `:merged'.  The render state is transient:
after a merge lands the daemon hibernates the session, so within seconds
the pushed state reads `:hibernated' — and a section keyed on `:merged'
dropped the row exactly then, while `agent-repl--sidebar-rostered-p' (which
exempts merged rows from the perspective requirement) dropped it from the
roster entirely.  The workspace vanished from the sidebar with no trace.
A merge instant never un-happens, so keying on it makes that disappearance
unrepresentable rather than merely unlikely.

The perspective clause is what keeps the section honest in the other
direction.  A merged row that the user REVIVES out of the section
\(`agent-repl--sidebar-open-dir') gets its perspective and its session
back, and a live workspace does not belong in a list of work that has
receded — it rejoins its repo or task group and reports its live status
there.  Merging kills the perspective, so a settled merge satisfies this
clause for as long as it stays settled."
  (and (> (agent-repl--sidebar-merged-at name) 0)
       (not (agent-repl--ws-open-p name))))

(defun agent-repl--sidebar-recently-merged-p (name)
  "Return non-nil when merged NAME merged at or after the current epoch.
A nil epoch means no inactivity gap has been observed yet, so every
completed merge still counts as recent."
  (let ((at (agent-repl--sidebar-merged-at name)))
    (and (> at 0)
         (or (null agent-repl--sidebar-merged-epoch)
             (>= at agent-repl--sidebar-merged-epoch)))))

(defcustom agent-repl-sidebar-merged-max-rows 10
  "Most recently-merged rows the Recently Merged section will render.
Merges accumulate faster than the activity window
\(`agent-repl-sidebar-merged-window-seconds') retires them, and an
unbounded section pushes the live workspaces — the rows the user acts on
— off the bottom of the rail.  The newest merges survive the cap, since
recency is the only thing the section claims to order on.  Rows dropped
by the cap are logged, never silently truncated."
  :type 'integer
  :group 'agent-repl)

(defun agent-repl--sidebar-merged-sorted (entries)
  "Return the recently-merged ENTRIES, newest merge first, capped.
The cap is `agent-repl-sidebar-merged-max-rows'; whatever it drops is
named in the log, so a merge missing from the rail is explainable from
the log alone rather than looking like the retention bug this section
exists to fix."
  (let* ((recent (sort (cl-remove-if-not
                        (lambda (e) (agent-repl--sidebar-recently-merged-p (car e)))
                        (copy-sequence entries))
                       (lambda (a b)
                         (> (agent-repl--sidebar-merged-at (car a))
                            (agent-repl--sidebar-merged-at (car b))))))
         (total (length recent)))
    (if (<= total agent-repl-sidebar-merged-max-rows)
        recent
      (let ((kept (cl-subseq recent 0 agent-repl-sidebar-merged-max-rows)))
        (agent-repl--log nil
                         "sidebar-merged-sorted: cap=%d recent=%d — dropping %d older merge(s) from Recently Merged: %s"
                         agent-repl-sidebar-merged-max-rows total
                         (- total agent-repl-sidebar-merged-max-rows)
                         (mapcar #'car (cl-subseq recent agent-repl-sidebar-merged-max-rows)))
        kept))))

(defun agent-repl--sidebar-merged-group (recent current-name)
  "Return the Recently Merged group plist for RECENT, or nil when empty.
RECENT is the already-sorted, already-capped entry list
`agent-repl--sidebar-merged-sorted' returns, passed in rather than
recomputed so the cap is decided — and logged — exactly once per build.

Shaped exactly like a repo group so the webapp validates and folds it
through the same path.  Merged rows carry no children: the family tree
is an organizing claim about live work, and a merged workspace has left
that tree."
  (when recent
    (agent-repl--log-verbose nil "sidebar-merged-group: recent=%d current=%s"
                             (length recent) current-name)
    (list :key agent-repl--sidebar-merged-key
          :label "Recently Merged"
          :folded (if (agent-repl--repo-folded-p agent-repl--sidebar-merged-key)
                      t :false)
          ;; Uniform group shape: `:done' is meaningful only for task
          ;; sections, but every group carries it so the webapp's
          ;; validateGroup stays one code path.
          :done :false
          :rows (vconcat
                 (mapcar (lambda (e)
                           (agent-repl--sidebar-row-plist
                            (car e) (agent-repl--path-canonical (cdr e))
                            current-name []))
                         recent)))))

;;;; ---- Roster building --------------------------------------------------

(defvar agent-repl--sidebar-nav-dir nil
  "Canonical project dir of the keyboard cursor's row, or nil.
Serialized into the roster as `navDir'; the webapp draws the dashed
cursor ring on the matching row.  Emacs owns this state entirely —
see the Commentary for why keyboard selection needs no webapp
round-trip.")

(defvar agent-repl--sidebar-flat-dirs nil
  "Visible row dirs in sidebar render order, cached by the last build.
Depth-first over unfolded repos — exactly the order the webapp renders
rows in, so `C-S-n' / `C-S-p' walk what the user actually sees.  Rows
of folded repos are excluded (they are hidden); greyed closed-REPL
rows are included (they are visible and selectable).")

(defun agent-repl--sidebar-rostered-p (name)
  "Return non-nil when live workspace NAME still belongs in the roster.
A workspace that has left the tab bar is GONE, and a gone workspace
gets no row at all: its perspective was killed, its entry nuked, or
both, and the sidebar must not keep offering a row the user cannot
switch to.  Membership is therefore `agent-repl--ws-open-p' —
perspective presence — not mere registration.

Two deliberate exemptions:

  - `:merged' workspaces.  A completed merge kills the perspective but
    keeps the registration (the merge path's `preserve-entry'), because
    the row is supposed to survive its tab inside the Recently Merged
    section.  Dropping it here would empty that section.

  - Every row, when the persp system has nothing to say.  With
    `agent-repl--ws-names-cache-usable-p' nil (persp-mode not loaded,
    or the cache not yet populated during startup) `--ws-open-p'
    answers nil for EVERYTHING, which is an absence of evidence rather
    than evidence of absence.  The roster falls back to plain live
    registration there, exactly as `--collect-snapshot-entries' does."
  (or (not (agent-repl--ws-names-cache-usable-p))
      (agent-repl--ws-open-p name)
      (agent-repl--sidebar-merged-p name)))

(defun agent-repl--sidebar-entries ()
  "Return the roster entries (NAME . DIR): rostered live workspaces only.
The universe is `agent-repl--live-ws-names' narrowed by
`agent-repl--sidebar-rostered-p': a registered, non-tombstoned
workspace that still holds its perspective (plus merged rows, which
outlive theirs on purpose).  Tombstoned registrations, snapshot-only
history, and workspaces whose perspective has been killed do not
render at all — greying is reserved for a workspace that still exists
with its panes dismissed (`agent-repl--sidebar-closed-p'), and folds
render as collapsed sections rather than drops.  A live workspace
without a `:project-dir' cannot be keyed by dir; it is skipped with a
log entry rather than silently, since every normally-created workspace
records one."
  (let (entries)
    (dolist (name (agent-repl--live-ws-names) (nreverse entries))
      (cond
       ((not (agent-repl--sidebar-rostered-p name))
        (agent-repl--log name "sidebar-entries: dropping ws=%s — no perspective (gone)" name))
       ((agent-repl--ws-get name :project-dir)
        (push (cons name (agent-repl--ws-get name :project-dir)) entries))
       (t
        (agent-repl--log name "sidebar-entries: skipping live ws=%s with no :project-dir" name))))))

(defun agent-repl--sidebar-entry-created-at (name)
  "Return NAME's `:created-at' as a float for sibling ordering.
`most-positive-fixnum' when unrecorded, so undated rows sort after
dated ones rather than interleaving unpredictably."
  (let ((v (agent-repl--ws-get name :created-at)))
    (if v (float-time v) most-positive-fixnum)))

(defun agent-repl--sidebar-sibling-sort (nodes)
  "Sort sibling NODES (plists with :name) by creation time, then name.
Creation order IS the family chronology — a workspace cut earlier
lists earlier — and the name tiebreak keeps the order deterministic
for undated rows."
  (sort nodes
        (lambda (a b)
          (let ((ta (agent-repl--sidebar-entry-created-at (plist-get a :name)))
                (tb (agent-repl--sidebar-entry-created-at (plist-get b :name))))
            (if (= ta tb)
                (string< (plist-get a :name) (plist-get b :name))
              (< ta tb))))))

(defun agent-repl--sidebar-row-plist (name dir current-name children)
  "Serialize one roster row for open workspace NAME at canonical DIR.
CURRENT-NAME is the active workspace's name (computed once per build).
CHILDREN is the already-serialized vector of child rows.  Keys and
value shapes are the webapp's WorkspaceRow contract
\(webapp/src/sidebar.ts): JSON booleans (`t' / `:false'), `:null' for
absent optionals, epoch-seconds float for lastViewedAt."
  (let ((viewed (agent-repl--ws-get name :last-viewed-at)))
    (list :name name
          :dir dir
          :status (agent-repl--sidebar-wire-status name)
          :closed (if (agent-repl--sidebar-closed-p name) t :false)
          :current (if (equal name current-name) t :false)
          :lastViewedAt (if viewed (float-time viewed) :null)
          :mergedAt (let ((at (agent-repl--sidebar-merged-at name)))
                      (if (> at 0) at :null))
          :branch (or (agent-repl--ws-get name :branch-name) :null)
          :parentBranch (or (agent-repl--ws-get name :parent-branch-name) :null)
          :summary (or (agent-repl--ws-get name :last-prompt-summary) :null)
          :children children)))

(defun agent-repl--sidebar-build-sections (entries current key-fn label-fn done-fn key-sort extra-keys)
  "Group ENTRIES into folded sections; return (SECTIONS-VECTOR . FLAT-DIRS).
SECTIONS-VECTOR is a vector of `{:key :label :folded :done :rows}' group
plists; FLAT-DIRS the visible row dirs in render order.

KEY-FN maps a workspace NAME to its ROOT section key — a TOTAL function,
so every root lands in exactly one section.  LABEL-FN maps a key to its
display label, DONE-FN maps a key to a JSON boolean (`t' / `:false') for
the section's done state, and KEY-SORT orders the resulting key list.
EXTRA-KEYS lists keys that must render even with zero rows (an empty
task section for a brand-new task).

Family shape is view-independent and lives here: an entry whose
`:source-ws-dir' canonically matches another entry's project dir nests
under it; everything else roots in its own section via KEY-FN.  A child
renders under its parent regardless of how KEY-FN would classify the
child alone — the family line is the sidebar's organizing claim — so
the section a family renders in is its ROOT's key.  Siblings sort by
creation time (`agent-repl--sidebar-sibling-sort').

EMITTED counts every serialized row; a count short of the entry total
means a `:source-ws-dir' cycle orphaned a family from every root — rows
silently missing from the sidebar would be a debugging tarpit, so fail
loudly instead."
  (let* ((nodes (mapcar (lambda (e)
                          (list :name (car e)
                                :dir (agent-repl--path-canonical (cdr e))))
                        entries))
         (by-dir (make-hash-table :test 'equal))
         (children (make-hash-table :test 'equal))
         (roots-by-key (make-hash-table :test 'equal))
         (emitted 0)
         keys)
    (dolist (n nodes) (puthash (plist-get n :dir) n by-dir))
    ;; Attach each node under its parent when the parent is itself an
    ;; entry; self-parenting is a corrupted plist, not a family.
    (dolist (n nodes)
      (let* ((name (plist-get n :name))
             (dir (plist-get n :dir))
             (src (agent-repl--ws-get name :source-ws-dir))
             (parent-dir (and src (agent-repl--path-canonical src))))
        (when (equal parent-dir dir)
          (agent-repl--log name "sidebar-build-sections: ws=%s self-parent dir=%s" name dir)
          (error "agent-repl--sidebar-build-sections: ws=%s is its own parent (dir=%s)"
                 name dir))
        (if (and parent-dir (gethash parent-dir by-dir))
            (progn
              (agent-repl--log-verbose name "sidebar-build-sections: ws=%s parent-dir=%s -> child"
                                        name parent-dir)
              (push n (gethash parent-dir children)))
          (let ((key (funcall key-fn name)))
            (agent-repl--log-verbose name "sidebar-build-sections: ws=%s parent-dir=%s key=%s -> root"
                                      name parent-dir key)
            (unless (gethash key roots-by-key)
              (push key keys))
            (push n (gethash key roots-by-key))))))
    ;; A key that must render even empty (e.g. a fresh task) joins the
    ;; key set with a nil root list, so it becomes a zero-row section
    ;; without disturbing the emitted-vs-node-count invariant.
    (dolist (key extra-keys)
      (unless (gethash key roots-by-key)
        (push key keys)
        (puthash key nil roots-by-key)))
    (let* ((flat nil)
           (emit-node nil)
           (sections
            (vconcat
             (mapcar
              (lambda (key)
                (let ((folded (and (agent-repl--repo-folded-p key) t)))
                  (setq emit-node
                        (lambda (node)
                          (setq emitted (1+ emitted))
                          (let* ((dir (plist-get node :dir))
                                 (kids (agent-repl--sidebar-sibling-sort
                                        (gethash dir children))))
                            (unless folded (push dir flat))
                            (agent-repl--sidebar-row-plist
                             (plist-get node :name) dir current
                             (vconcat (mapcar (lambda (k) (funcall emit-node k))
                                              kids))))))
                  (list :key key
                        :label (funcall label-fn key)
                        :folded (if folded t :false)
                        :done (funcall done-fn key)
                        :rows (vconcat
                               (mapcar (lambda (n) (funcall emit-node n))
                                       (agent-repl--sidebar-sibling-sort
                                        (gethash key roots-by-key)))))))
              (funcall key-sort keys)))))
      (unless (= emitted (length nodes))
        (agent-repl--log nil "sidebar-build-sections: emitted=%d nodes=%d; family cycle detected"
                          emitted (length nodes))
        (error "agent-repl--sidebar-build-sections: emitted %d of %d rows — :source-ws-dir cycle orphaned a family"
               emitted (length nodes)))
      (agent-repl--log-verbose nil "sidebar-build-sections: nodes=%d sections=%d emitted=%d flat=%d"
                                (length nodes) (length sections) emitted (length flat))
      (cons sections (nreverse flat)))))

(defun agent-repl--sidebar-repo-key-sort (keys)
  "Sort repo KEYS by label with the `(no repo)' sentinel forced last."
  (sort (copy-sequence keys)
        (lambda (a b)
          (cond
           ((equal a agent-repl--repo-key-unknown) nil)
           ((equal b agent-repl--repo-key-unknown) t)
           (t (string< (agent-repl--repo-label a)
                       (agent-repl--repo-label b)))))))

(defun agent-repl--sidebar-task-key (name)
  "Return the section key for workspace NAME under the task view.
Its effective task id (direct or inherited), or the no-task catch-all."
  (or (agent-repl--ws-effective-task-id name)
      agent-repl--sidebar-no-task-key))

(defun agent-repl--sidebar-task-label (key)
  "Return the task-section label for KEY.
\"No task\" for the catch-all; otherwise the task's title, falling back
to the bare id only when the task somehow no longer exists."
  (if (equal key agent-repl--sidebar-no-task-key)
      "No task"
    (let ((task (agent-repl--task-get key)))
      (if task
          (plist-get task :title)
        (agent-repl--log nil "sidebar-task-label: missing task id=%s" key)
        key))))

(defun agent-repl--sidebar-task-done (key)
  "Return the JSON boolean done state for task-section KEY.
`:false' for the catch-all and for any task not marked done."
  (if (equal key agent-repl--sidebar-no-task-key)
      :false
    (let ((task (agent-repl--task-get key)))
      (if (and task (plist-get task :done))
          t
        (when (null task)
          (agent-repl--log nil "sidebar-task-done: missing task id=%s" key))
        :false))))

(defun agent-repl--sidebar-task-key-sort (keys)
  "Sort task-section KEYS by task creation order, the catch-all last."
  (let ((order (make-hash-table :test 'equal))
        (i 0))
    (dolist (task (agent-repl--tasks-sorted))
      (puthash (plist-get task :id) i order)
      (setq i (1+ i)))
    (sort (copy-sequence keys)
          (lambda (a b)
            (cond
             ((equal a agent-repl--sidebar-no-task-key) nil)
             ((equal b agent-repl--sidebar-no-task-key) t)
             (t (< (gethash a order most-positive-fixnum)
                   (gethash b order most-positive-fixnum))))))))

(defun agent-repl--sidebar-build ()
  "Build the roster from every live workspace, per the active view.
Returns (ROSTER . FLAT-DIRS): ROSTER is the `json-serialize'-ready
plist, FLAT-DIRS the visible row dirs in render order (see
`agent-repl--sidebar-flat-dirs').

The roster always carries both `:repos' and `:tasks'; the inactive
view's array is empty, so the webapp switches on `:view' and renders
exactly one grouping.  Repository view groups by git repo (repos sort by
label, `(no repo)' last); task view groups by the task each family's
ROOT belongs to (tasks in creation order, \"No task\" last) and always
renders every task, empty ones included.

Merged workspaces are partitioned out BEFORE the sections are built, so
they never enter the active grouping and the emitted-vs-node-count
invariant compares two exact counts.  They render instead in the
`:recentlyMerged' group while they sit inside the window
`agent-repl--sidebar-refresh-merged-window' maintains."
  (let* ((all-entries (agent-repl--sidebar-entries))
         (merged-entries (cl-remove-if-not
                          (lambda (e) (agent-repl--sidebar-merged-p (car e)))
                          all-entries))
         (entries (cl-remove-if
                   (lambda (e) (agent-repl--sidebar-merged-p (car e)))
                   all-entries))
         (current (ignore-errors (agent-repl--ws-current-name)))
         (task-view (eq agent-repl--sidebar-view :task))
         (built (if task-view
                    (agent-repl--sidebar-build-sections
                     entries current
                     #'agent-repl--sidebar-task-key
                     #'agent-repl--sidebar-task-label
                     #'agent-repl--sidebar-task-done
                     #'agent-repl--sidebar-task-key-sort
                     (mapcar (lambda (task) (plist-get task :id))
                             (agent-repl--tasks-sorted)))
                  (agent-repl--sidebar-build-sections
                   entries current
                   #'agent-repl--ws-repo-group
                   #'agent-repl--repo-label
                   (lambda (_key) :false)
                   #'agent-repl--sidebar-repo-key-sort
                   nil)))
         (sections (car built))
         (flat (cdr built))
         ;; Sorted and capped ONCE: the group and the keyboard-navigable dir
         ;; list must walk the identical row set, and the cap's drop log must
         ;; not double per build.
         (recent-entries (agent-repl--sidebar-merged-sorted merged-entries))
         (recent (agent-repl--sidebar-merged-group recent-entries current))
         (recent-folded (agent-repl--repo-folded-p agent-repl--sidebar-merged-key))
         (recent-dirs (and recent (not recent-folded)
                           (mapcar (lambda (e)
                                     (agent-repl--path-canonical (cdr e)))
                                   recent-entries))))
    (agent-repl--log-verbose nil "sidebar-build: view=%s entries=%d grouped=%d merged=%d sections=%d recent=%s flat=%d"
                              agent-repl--sidebar-view (length all-entries) (length entries)
                              (length merged-entries) (length sections) (and recent t)
                              (length flat))
    (cons (list :view (agent-repl--sidebar-view-wire)
                :repos (if task-view [] sections)
                :tasks (if task-view sections [])
                :recentlyMerged (or recent :null)
                :navDir (or agent-repl--sidebar-nav-dir :null))
          (append flat recent-dirs))))

;;;; ---- Shaping the roster as a protojson WorkspaceRoster ----------------
;;
;; `agent-repl--sidebar-build' keeps authoring the roster in Emacs's own
;; shape; these functions translate that one value into the frozen
;; `WorkspaceRoster' contract.  Translating rather than re-authoring keeps
;; the grouping, family-nesting and ordering rules in exactly one place.
;;
;; Two proto3 encoding rules govern what comes out:
;;
;;   * A oneof is FLATTENED into its parent, so a row carries one status
;;     arm key directly (`"mergeConflict": {}'), never a `status' field.
;;     Same for the view: `WorkspaceRoster' carries `repository' or `task',
;;     never a mode string beside them.
;;   * Zero values are OMITTED.  A false bool and an empty string are
;;     absent rather than present-and-falsy, which is what protojson emits
;;     and what both consumers read back as false/"".  This also keeps
;;     `json-encode' away from the `:false' sentinel, which is the JSON
;;     library's spelling, not this wire's.

(defun agent-repl--sidebar-epoch-ms (seconds)
  "Return epoch SECONDS as integer epoch MILLISECONDS.

The contract's stamps are MILLISECONDS while every Emacs-side stamp is
`float-time' SECONDS, so this is the single conversion site — one place
to be wrong rather than one per field.

A non-numeric value (`:null' for a stamp the build had none for) and a
non-positive one both answer 0, which is not a missing value on this
wire: `last_viewed_at_ms' reads 0 as NEVER VIEWED and `merged_at_ms'
reads it as NOT MERGED, so the zero IS the assertion the row means.
Rounded rather than truncated so a stamp round-trips to the millisecond
it names instead of to the one below it."
  (if (and (numberp seconds) (> seconds 0))
      (round (* 1000 seconds))
    0))

(defun agent-repl--sidebar-proto-string (value)
  "Return VALUE when it is a non-empty string, else nil.
The built roster spells an absent optional `:null'; this wire spells it
by OMITTING the field, which both consumers read back as the proto3
empty string — the contract's \"unknown / none\".  Returning nil lets
each caller drop the field with the same `when' every other zero-valued
field here uses."
  (and (stringp value) (not (string-empty-p value)) value))

(defun agent-repl--sidebar-proto-row (row)
  "Translate one built ROW plist into a protojson `RosterRow' plist.
Recurses through `:children', which is the family nesting the build
already resolved.  ROW's wire status selects the oneof ARM; the arm's
value is the empty object every status message is by contract.

The display fields (the when-column stamps, the detail panel's branch
lines and summary, and `closed') ride alongside the status arm, each
carrying a MEANINGFUL proto3 zero, so each is omitted at its zero rather
than sent as an explicit null."
  (let ((viewed-ms (agent-repl--sidebar-epoch-ms (plist-get row :lastViewedAt)))
        (merged-ms (agent-repl--sidebar-epoch-ms (plist-get row :mergedAt)))
        (branch (agent-repl--sidebar-proto-string (plist-get row :branch)))
        (parent-branch (agent-repl--sidebar-proto-string
                        (plist-get row :parentBranch)))
        (summary (agent-repl--sidebar-proto-string (plist-get row :summary))))
    (append
     (list :dir (plist-get row :dir)
           :name (plist-get row :name))
     (list (agent-repl--sidebar-status-arm (plist-get row :status))
           ;; `{}', not `null': the arm must be a present empty MESSAGE, and
           ;; only a hash table encodes as an empty JSON object.
           (make-hash-table :test 'equal))
     (when (eq (plist-get row :current) t) (list :current t))
     (when (> viewed-ms 0) (list :lastViewedAtMs viewed-ms))
     (when (> merged-ms 0) (list :mergedAtMs merged-ms))
     (when branch (list :branch branch))
     (when parent-branch (list :parentBranch parent-branch))
     (when summary (list :summary summary))
     (when (eq (plist-get row :closed) t) (list :closed t))
     (list :children (agent-repl--sidebar-proto-rows (plist-get row :children))))))

(defun agent-repl--sidebar-proto-rows (rows)
  "Translate the vector ROWS into a vector of protojson `RosterRow's."
  (vconcat (mapcar #'agent-repl--sidebar-proto-row (append rows nil))))

(defun agent-repl--sidebar-proto-repo-section (group)
  "Translate a built GROUP plist into a protojson `RosterRepoSection'.
The group's key IS the `repoKey' — identity, and the fold state's key.
The label rides beside it as DISPLAY ONLY (`agent-repl--repo-label',
already resolved by the build), deliberately not as the identity: the
fold a user set is keyed by `repoKey', so renaming what a section is
called can never orphan it."
  (append
   (list :repoKey (plist-get group :key))
   (when (eq (plist-get group :folded) t) (list :folded t))
   (let ((label (agent-repl--sidebar-proto-string (plist-get group :label))))
     (when label (list :label label)))
   ;; A FOLDED SECTION STILL CARRIES ITS ROWS: folding is display state,
   ;; so unfolding must need no republish.
   (list :rows (agent-repl--sidebar-proto-rows (plist-get group :rows)))))

(defun agent-repl--sidebar-proto-task-section (group)
  "Translate a built GROUP plist into a protojson `RosterTaskSection'.
The group's key is the `taskId' (identity) and its label the `title'
\(display only) — a task can be renamed without becoming another task.
Task sections carry `done'; repo sections have no such axis."
  (append
   (list :taskId (plist-get group :key)
         :title (plist-get group :label))
   (when (eq (plist-get group :done) t) (list :done t))
   (list :rows (agent-repl--sidebar-proto-rows (plist-get group :rows)))))

(defun agent-repl--sidebar-proto-current-dir (rows)
  "Return the dir of the row marked current within ROWS, or nil.
Searches children too, since the current workspace can be a nested one.

Resolved FROM the rows rather than recomputed beside them: the contract
carries `current_dir' and a per-row `current' flag deliberately, so the
author settles the question once and no consumer can answer it two ways."
  (cl-loop for row across (or rows [])
           thereis (if (eq (plist-get row :current) t)
                       (plist-get row :dir)
                     (agent-repl--sidebar-proto-current-dir
                      (plist-get row :children)))))

(defun agent-repl--sidebar-proto-roster (roster revision)
  "Translate built ROSTER into a protojson `WorkspaceRoster' at REVISION.

REVISION is scoped by `bootId', this boot's publisher epoch, which every
publication carries: the daemon nacks a roster that names no epoch, so
it is emitted unconditionally rather than at the author's discretion.

The built roster always carries both groupings with the inactive one
empty; the contract carries only the ACTIVE one, because the set arm is
the grouping.  An unrecognized view signals rather than defaulting to
repository — silently publishing the wrong grouping would misdraw every
row instead of failing where the bad value is still named."
  (let* ((view (plist-get roster :view))
         (task-view (cond ((equal view "task") t)
                          ((equal view "repository") nil)
                          (t (agent-repl--log nil "sidebar-proto-roster: unknown view=%S" view)
                             (error "agent-repl--sidebar-proto-roster: unknown view %S" view))))
         (sections (vconcat
                    (mapcar (if task-view
                                #'agent-repl--sidebar-proto-task-section
                              #'agent-repl--sidebar-proto-repo-section)
                            (append (plist-get roster (if task-view :tasks :repos))
                                    nil))))
         (merged (plist-get roster :recentlyMerged))
         (merged-rows (if (eq merged :null)
                          []
                        (agent-repl--sidebar-proto-rows (plist-get merged :rows))))
         (nav (plist-get roster :navDir))
         ;; Every row the roster carries is a candidate, recently-merged
         ;; included: a merged workspace can still be the current one.
         (current-dir (or (cl-loop for section across sections
                                   thereis (agent-repl--sidebar-proto-current-dir
                                            (plist-get section :rows)))
                          (agent-repl--sidebar-proto-current-dir merged-rows))))
    (append
     (list :revision revision
           :bootId (agent-repl--sidebar-boot-id))
     (list (if task-view :task :repository) (list :sections sections))
     (when (> (length merged-rows) 0)
       (list :recentlyMerged
             (append
              (list :rows merged-rows)
              ;; The section folds through the same fold set the repo
              ;; sections use, and the author reports the resolved state
              ;; rather than every client re-deriving the same key.
              (when (eq (plist-get merged :folded) t) (list :folded t))
              (let ((label (agent-repl--sidebar-proto-string
                            (plist-get merged :label))))
                (when label (list :label label))))))
     (when (and (stringp current-dir) (not (string-empty-p current-dir)))
       (list :currentDir current-dir))
     (when (and (stringp nav) (not (string-empty-p nav)))
       (list :navDir nav)))))

(defun agent-repl--sidebar-expand-script (dir)
  "Return the JavaScript that toggles DIR's detail panel in the webapp.
Calls the hook only when the webapp has already planted it: a webview
mid-navigation has no hook yet, and that is an expected state rather
than a violated invariant.  DIR rides in as a JSON string literal
\(`json-serialize'), so any character in a path survives interpolation
intact."
  (format "window.%s && window.%s(%s);"
          agent-repl--sidebar-expand-hook
          agent-repl--sidebar-expand-hook
          (json-serialize dir)))

(defun agent-repl--sidebar-live-webview-buffers ()
  "Return every live frontend webview buffer across live workspaces."
  (let (bufs)
    (dolist (name (agent-repl--live-ws-names) (nreverse bufs))
      (let ((buf (agent-repl--ws-get name :frontend-buffer)))
        (when (buffer-live-p buf)
          (agent-repl--log-verbose name "sidebar-live-webview-buffers: ws=%s buffer=%s live=t"
                                    name (buffer-name buf))
          (push buf bufs))))))

;;;; ---- Publishing the roster to the daemon ------------------------------
;;
;; The roster used to be injected into each live webview by execute-script.
;; That made it PER-VIEW state: a second webview could not share it, and a
;; webview that remounted had nothing until the next push.  It is now a
;; frontend command, so the daemon retains one roster and rebroadcasts it —
;; ordinary daemon surface, snapshotted on connect like everything else.
;;
;; The delivery guarantee moved with it.  The 1Hz signature compare is now
;; ONLY a change detector: a tick that skips cannot lose a roster, because
;; the daemon still holds the last one published and hands it to every
;; client that connects.  What the daemon cannot survive is its own
;; restart, which is what `agent-repl--sidebar-publish-on-connect' covers.

(defconst agent-repl--sidebar-publish-field "publishWorkspaceRoster"
  "protojson name of the `FrontendCommand' arm carrying the roster.
Mirrors `publish_workspace_roster' in frontend.proto — one contract.")

(defvar agent-repl--sidebar-roster-revision 0
  "Revision of the last roster published; 0 before the first publish.

Monotonic WITHIN ONE PUBLISHER EPOCH (`agent-repl--sidebar-boot-id'), as
the contract specifies: the daemon refuses a same-epoch publish whose
revision is not newer than the one it holds, so an out-of-order delivery
cannot resurrect a superseded roster.  A restarted Emacs restarts this
counter AND opens a new epoch, so its revision 1 is accepted against a
retained revision 500 rather than refused until the counter caught up.")

(defvar agent-repl--sidebar-boot-id nil
  "This Emacs boot's opaque roster-publisher identity, or nil before minting.

The EPOCH KEY the revision counter is scoped by: the daemon compares it
for equality and nothing else — never for order, never for a time — and
a publish carrying a boot id different from the retained roster's opens
a fresh epoch that resets the monotonicity floor.  An EMPTY one is
nacked, so this is a hard prerequisite of every publish rather than an
optional annotation.

Minted lazily and exactly once per boot (`agent-repl--sidebar-boot-id'),
and `defvar' so a module hot reload keeps the epoch it already published
under — re-minting mid-boot would silently reset the daemon's floor and
make a genuinely stale roster acceptable.")

(defun agent-repl--sidebar-boot-id ()
  "Return this Emacs boot's roster-publisher identity, minting it once.

Opaque by construction: an md5 over the pid, the wall clock and a random
draw, which is the module's established shape for a locally-minted
identity (`agent-repl--task-new-id',
`agent-repl--workspace-create-command-id').  Nothing downstream parses
it, so its only job is to differ across boots and never within one."
  (or agent-repl--sidebar-boot-id
      (let ((id (substring (md5 (format "%s-%s-%s"
                                        (emacs-pid) (float-time)
                                        (random most-positive-fixnum)))
                           0 16)))
        (agent-repl--log nil "sidebar-boot-id: minted publisher epoch id=%s" id)
        (setq agent-repl--sidebar-boot-id id))))

(defun agent-repl--sidebar-publish-nacked (revision err)
  "Surface a REVISION publish rejected by the daemon, with reason ERR.

Loud by construction and never absorbed: the ack path already logs and
echoes the rejection, and this adds the roster-specific fact — WHICH
revision the daemon refused — that the generic surfacing cannot know.

The revision counter is deliberately NOT rewound here.  Rewinding would
need the revision the daemon actually retains, and the frozen `CommandAck'
carries no such field, so any figure recovered from the error text would
be a guess dressed as a recovery.  The counter therefore keeps climbing:
a monotonic counter that outruns the daemon is harmless, where one that
silently reverses could publish a stale roster over a newer one."
  (agent-repl--log nil "sidebar-publish: NACKED revision=%d reason=%s"
                   revision (or err "unspecified")))

(defun agent-repl--sidebar-publish (roster)
  "Publish built ROSTER to the daemon as the next revision.

Returns the command's `request_id', or nil when the link is down.

A DOWN LINK IS NOT A LOST ROSTER, and is logged rather than raised: the
daemon that would receive it does not exist to receive anything, and the
reconnect publish delivers the state of the world as of reconnection —
which is fresher than the frame that could not be sent.  Raising here
would instead fire on every 1Hz tick for the whole of an outage."
  (if (not (agent-repl--uds-connected-p))
      (progn
        (agent-repl--log nil "sidebar-publish: link down — deferring to the reconnect publish")
        nil)
    (let* ((revision (cl-incf agent-repl--sidebar-roster-revision))
           (proto (agent-repl--sidebar-proto-roster roster revision))
           (request-id (agent-repl--uds-send-command
                        agent-repl--sidebar-publish-field
                        (list :roster proto))))
      ;; TRACKED, not fire-and-forget: a rejected roster means the sidebar
      ;; every client draws is not the one Emacs authored, which is exactly
      ;; the kind of divergence that must never pass silently.
      (agent-repl--uds-track-command
       request-id agent-repl--sidebar-publish-field nil
       (lambda (err) (agent-repl--sidebar-publish-nacked revision err)))
      (agent-repl--log nil "sidebar-publish: boot-id=%s revision=%d request-id=%s view=%s"
                       (agent-repl--sidebar-boot-id) revision request-id
                       (plist-get roster :view))
      request-id)))

(defun agent-repl--sidebar-push ()
  "Rebuild the roster and publish it to the daemon.
Unconditional: change-gating lives in `agent-repl--sidebar-tick's
signature compare, and the event-driven callers (fold / switch / nav)
push precisely because they just changed what the sidebar shows.
Also refreshes `agent-repl--sidebar-flat-dirs' as the build's side
product, so navigation always walks the order last shown."
  (let ((built (agent-repl--sidebar-build)))
    (setq agent-repl--sidebar-flat-dirs (cdr built))
    (agent-repl--sidebar-publish (car built))))

(defun agent-repl--sidebar-publish-on-connect ()
  "Republish the roster onto a freshly opened frontend UDS connection.

Subscriber for `agent-repl-uds-connected-functions'.  A daemon that just
restarted retains no roster, and Emacs is the only author of one, so
without this the sidebar would stay empty until something happened to
move the signature.  Rebuilding rather than resending the last frame is
the point: the roster published is the state of the world NOW."
  (agent-repl--log nil "sidebar-publish-on-connect: republishing the roster")
  (agent-repl--sidebar-push))

(add-hook 'agent-repl-uds-connected-functions
          #'agent-repl--sidebar-publish-on-connect)

(defun agent-repl--sidebar-expand-push (dir)
  "Toggle DIR's detail panel across every live webview.
The panel is webapp-owned client state (`openDirs'), so unlike a fold
this fires the expand hook WITHOUT rebuilding the roster — the page
flips the panel and re-renders on its own.  No roster round-trip means
`agent-repl--sidebar-flat-dirs' is left untouched here."
  (let ((script (agent-repl--sidebar-expand-script dir))
        (bufs (agent-repl--sidebar-live-webview-buffers)))
    (dolist (buf bufs)
      (agent-repl--frontend-webview-execute-script buf script))
    (agent-repl--log nil "sidebar-expand-push: dir=%s webviews=%d" dir (length bufs))))

;;;; ---- The 1Hz change gate ----------------------------------------------

(defvar agent-repl--sidebar-last-signature nil
  "Signature of the roster state as of the last pushed build.")

(defun agent-repl--sidebar-signature ()
  "Return a cheap value that changes whenever the roster would.
Pure in-memory reads plus the persp membership probe:
per-live-workspace render state (status keyword, repl-state, open-p —
the greyed flag's other input, last-viewed), the current workspace, the
fold set, and the nav cursor.

The set of live webview buffers is deliberately NOT an input any more.
It was one while the roster was injected per webview, because a freshly
mounted view held nothing until a push reached it.  The roster now lives
in the daemon, which hands its retained copy to every client that
connects, so a new view is served without Emacs noticing it exists —
and this signature is purely a change detector over roster CONTENT."
  (list (mapcar (lambda (name)
                  (list name
                        (agent-repl--ws-render-status name)
                        (agent-repl--ws-get name :repl-state)
                        (and (agent-repl--ws-open-p name) t)
                        (agent-repl--ws-get name :last-viewed-at)
                        ;; Task membership: assigning a workspace to a
                        ;; task changes which section it renders under
                        ;; without touching any lifecycle field above.
                        (agent-repl--ws-get name :task-id)))
                (agent-repl--live-ws-names))
        (ignore-errors (agent-repl--ws-current-name))
        (agent-repl--folded-repo-keys)
        agent-repl--sidebar-nav-dir
        ;; The active view and the task list are their own axes: a view
        ;; switch, task create, rename, or done-toggle changes the
        ;; roster without moving any per-workspace lifecycle field.
        agent-repl--sidebar-view
        (agent-repl--tasks-signature)
        ;; The epoch is its own axis: wiping the Recently Merged section
        ;; drops rows without changing any per-workspace render state, so
        ;; without this the gate would skip the push that clears them.
        agent-repl--sidebar-merged-epoch))

(defun agent-repl--sidebar-tick ()
  "1Hz entry point (status.el's state tick): push when the signature moved.
The signature compare is the hot-path gate — the rebuild and push
behind it run only on actual change, so per-tick logging stays on the
verbose ladder.

The merged-window refresh runs BEFORE the signature is taken, since the
epoch it maintains is one of the signature's inputs and a wipe must be
visible to this very tick's compare rather than the next one."
  (agent-repl--sidebar-refresh-merged-window)
  (let ((sig (agent-repl--sidebar-signature)))
    (if (equal sig agent-repl--sidebar-last-signature)
        (agent-repl--log-verbose nil "sidebar-tick: signature unchanged, skip")
      (setq agent-repl--sidebar-last-signature sig)
      (agent-repl--log-verbose nil "sidebar-tick: signature changed, pushing roster")
      (agent-repl--sidebar-push))))

;;;; ---- The dot and the tab take the same value at the same moment --------
;;
;; The rail's dots and the tab-bar's colors are two renderings of ONE value:
;; `agent-repl--ws-render-status', which is the render keyword the daemon
;; pushed.  What used to differ was WHEN each read it.  The tab bar recolors
;; the instant `frontend-state.el' stores the pushed state, while the rail
;; waited for the 1Hz tick's signature compare to notice — so for up to a
;; second the two surfaces showed different colors for the same workspace.
;; That is the green-tab-beside-a-yellow-dot divergence, on the Emacs side of
;; it.
;;
;; Pushing the roster FROM the state-transition hook removes the window rather
;; than shrinking it: the rail is rebuilt as part of applying the state, from
;; the same value the tab bar is about to read, so there is no interval in
;; which one surface has the new state and the other has the old one.
;;
;; The 1Hz tick stays for the axes no pushed state moves — folds, task
;; membership, the merged-window epoch, a freshly mounted webview — and its
;; signature compare makes the overlap a no-op rather than a double push.

(defun agent-repl--sidebar-react-to-pushed-state (ws new previous)
  "Repaint the rail as part of applying WS's pushed render state.
Subscriber for `agent-repl-ws-state-transition-functions'
\(frontend-state.el).  NEW and PREVIOUS are the render keywords; a push
that did not move the keyword changes no dot, so it is skipped.

The push also refreshes `agent-repl--sidebar-last-signature', so the
next 1Hz tick sees the state it already delivered and stays quiet."
  (unless (eq new previous)
    (agent-repl--log ws "sidebar-react-to-pushed-state: ws=%s %s -> %s — repainting the rail with the tab bar"
                     ws previous new)
    (setq agent-repl--sidebar-last-signature (agent-repl--sidebar-signature))
    (agent-repl--sidebar-push))
  (when (eq new previous)
    (agent-repl--log-verbose ws "sidebar-react-to-pushed-state: ws=%s state=%s unchanged, skip"
                              ws new)))

;; Registered here though the hook variable is defined in frontend-state.el:
;; `add-hook' auto-vivifies the unbound variable and that file's `defvar ...
;; nil' does not reset an already-bound one, so this survives either load
;; order (the same arrangement status.el's death reactor uses).
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--sidebar-react-to-pushed-state)

;;;; ---- Keyboard navigation ----------------------------------------------

(defun agent-repl--sidebar-visible-dirs ()
  "Return the visible row dirs, building the roster when never built.
The build's flat list is the navigation order; an empty cache before
any push simply means no tick has run yet, so build once rather than
navigate a stale nothing."
  (unless agent-repl--sidebar-flat-dirs
    (agent-repl--log-verbose nil "sidebar-visible-dirs: cache empty; rebuilding roster")
    (setq agent-repl--sidebar-flat-dirs (cdr (agent-repl--sidebar-build))))
  agent-repl--sidebar-flat-dirs)

(defun agent-repl--sidebar-nav-move (delta)
  "Move the keyboard cursor DELTA rows (wrapping) and open where it lands.
With no cursor yet, DELTA > 0 starts at the first visible row and
DELTA < 0 at the last — the two ends a user reaching for next/prev
expects to land on.  Opening the landed workspace IS the auto-select:
`C-S-n' / `C-S-p' both move the highlight and switch to it, so the
cursor is never a mere preview.  `agent-repl--sidebar-open-dir' pushes
the fresh roster, so the moved highlight rides that same push and no
separate highlight push is needed."
  (let ((dirs (agent-repl--sidebar-visible-dirs)))
    (unless dirs
      (agent-repl--log nil "sidebar-nav-move: delta=%d no visible rows" delta)
      (user-error "agent-repl sidebar: no workspaces to navigate"))
    (let* ((cur (and agent-repl--sidebar-nav-dir
                     (cl-position agent-repl--sidebar-nav-dir dirs :test #'equal)))
           (next (if (null cur)
                     (if (> delta 0) 0 (1- (length dirs)))
                   (mod (+ cur delta) (length dirs)))))
      (setq agent-repl--sidebar-nav-dir (nth next dirs))
      (agent-repl--log nil "sidebar-nav-move: delta=%d idx=%s -> dir=%s of %d"
                        delta cur agent-repl--sidebar-nav-dir (length dirs))
      (agent-repl--sidebar-open-dir agent-repl--sidebar-nav-dir))))

(defun agent-repl-sidebar-nav-next ()
  "Move the sidebar's keyboard cursor to the next row and open it."
  (interactive)
  (agent-repl--sidebar-nav-move 1))

(defun agent-repl-sidebar-nav-prev ()
  "Move the sidebar's keyboard cursor to the previous row and open it."
  (interactive)
  (agent-repl--sidebar-nav-move -1))

(defun agent-repl-sidebar-nav-show-info ()
  "Unfold the detail panel of the workspace under the sidebar's cursor.
Toggles the same per-row panel the chevron click drives, so a second
press folds it back.  Emacs owns the cursor but the panel is
webapp-owned client state, so this reaches the page through
`agent-repl--sidebar-expand-push' rather than a roster rebuild."
  (interactive)
  (let ((dir agent-repl--sidebar-nav-dir))
    (unless dir
      (agent-repl--log nil "sidebar-nav-show-info: no cursor row selected")
      (user-error "agent-repl sidebar: no row selected — C-S-n / C-S-p first"))
    (agent-repl--sidebar-expand-push dir)))

;;;; ---- Opening a workspace (shared by keyboard + click) ------------------

(defun agent-repl--sidebar-entry-for-dir (dir)
  "Return the live entry (NAME . DIR) canonically matching DIR, or nil."
  (let ((canon (agent-repl--path-canonical dir)))
    (cl-find-if (lambda (e)
                  (equal (agent-repl--path-canonical (cdr e)) canon))
                (agent-repl--sidebar-entries))))

(defun agent-repl--sidebar-open-dir (dir)
  "Switch Emacs to the live workspace whose project dir is DIR, and open it.
Routes through `agent-repl--picker-open-selection', the canonical
switch-or-revive: an open perspective switches in place, a
perspective-less or closed-REPL one is re-established.  Signals when
DIR matches no live entry — an unknown dir means the click/cursor and
the roster disagree, which is a contract violation to surface, not a
row to silently ignore.

Then boots the agent via `agent-repl--frontend-boot-session'.  Opening
from the sidebar means \"open the workspace AND start agent-repl in
it\", but `--picker-open-selection' boots a session only on its revive
branch: a workspace whose perspective is still open while its REPL was
torn down (`:repl-state' `:inactive', the greyed rows
`agent-repl--sidebar-closed-p' marks) takes the switch branch and would
otherwise land the user in a dead REPL.  The boot door no-ops when a
live session already exists, so the revive branch's own boot is never
doubled.

Booting alone leaves the session running but HEADLESS — a target that
is not currently hosting panels would land the user in bare windows
with the agent panels hidden.  That is the greyed rows
`agent-repl--sidebar-closed-p' marks, PLUS any perspective-less target
that reaches here (a merged row being revived out of Recently Merged).
The perspective-less arm is named explicitly rather than inherited from
`--sidebar-closed-p', which is now keyed on the panes-dismissed
condition alone and no longer speaks for perspective membership.  The
`:pending-show-panels' flag is armed BEFORE the switch, and the switch's own
`agent-repl--on-workspace-switch' drain then shows the panels the
instant the workspace activates — the same door workspace generation
comes up visible through.  This is the keyboard / click equivalent of
pressing `SPC o c' in the target while it is closed.  An already-open
target needs no arming: `agent-repl--ensure-own-panels-on-persp-switch'
restores its panels on switch."
  (let ((entry (agent-repl--sidebar-entry-for-dir dir)))
    (unless entry
      (agent-repl--log nil "sidebar-open: no live workspace for dir=%s" dir)
      (error "agent-repl sidebar: no live workspace for dir %s" dir))
    (let* ((name (car entry))
           (closed (agent-repl--sidebar-closed-p name))
           (perspless (not (agent-repl--ws-open-p name)))
           (headless (or closed perspless)))
      (agent-repl--log name "sidebar-open: name=%s dir=%s live=%s closed=%s perspless=%s merged=%s"
                        name (cdr entry)
                        (and (agent-repl--ws-live-p name) t)
                        closed perspless
                        (or (eq (agent-repl--ws-get name :repl-state) :merged)
                            (eq (agent-repl--ws-get name :merge-completed) t)))
      ;; Arm the panel show BEFORE switching so a headless target comes up
      ;; running AND open rather than running-but-headless.  Drained by
      ;; `agent-repl--on-workspace-switch' once the workspace activates.
      (when headless
        (agent-repl--ws-put name :pending-show-panels t))
      (agent-repl--picker-open-selection
       (list :name name
             :project-dir (cdr entry)
             :live-p (agent-repl--ws-live-p name)))
      (agent-repl--frontend-boot-session name (cdr entry))
      (agent-repl--sidebar-push))))

;;;; ---- Workspace-command handlers (webview clicks, via the daemon) -------

(defun agent-repl--handle-switch-command (cmd)
  "Handle a \"switch\" workspace command CMD (a sidebar row click).
CMD carries `dir', the clicked row's canonical project dir (the
daemon's POST /workspace-command validated presence, but this handler
re-validates — the file channel is also writable by other emitters)."
  (let ((dir (alist-get 'dir cmd)))
    (unless (and (stringp dir) (not (string-empty-p dir)))
      (agent-repl--log nil "host-action switch-workspace: rejected missing-or-empty dir")
      (error "agent-repl switch command: missing dir in %S" cmd))
    (agent-repl--log nil "host-action switch-workspace: dir=%s" dir)
    (agent-repl--sidebar-open-dir dir)))

(defun agent-repl--sidebar-repo-key-known-p (key)
  "Return non-nil when KEY names a repo some live workspace belongs to.
The `(no repo)' sentinel counts — its section folds like any other."
  (or (equal key agent-repl--repo-key-unknown)
      (cl-some (lambda (e) (equal key (agent-repl--ws-repo-group (car e))))
               (agent-repl--sidebar-entries))))

(defun agent-repl--handle-fold-command (cmd)
  "Handle a \"fold\" workspace command CMD (a sidebar repo-header click).
CMD carries `repo_key' and the DESIRED `folded' state (explicit target
rather than a toggle, so a duplicate or delayed file cannot double-
toggle past the user's intent).  Folding a repo hides its tab-bar
entries too — `agent-repl--ws-tabline-names' already filters on the
same fold set, so forcing the tab-bar redraw is the whole sync."
  (let ((key (alist-get 'repo_key cmd))
        (folded-cell (assq 'folded cmd)))
    (unless (and (stringp key) (not (string-empty-p key)))
      (agent-repl--log nil "host-action set-repository-fold: rejected missing-or-empty key")
      (error "agent-repl fold command: missing repo_key in %S" cmd))
    (unless folded-cell
      (agent-repl--log nil "host-action set-repository-fold: rejected missing folded key=%s" key)
      (error "agent-repl fold command: missing folded in %S" cmd))
    (unless (agent-repl--sidebar-repo-key-known-p key)
      (agent-repl--log nil "host-action set-repository-fold: rejected unknown key=%s" key)
      (error "agent-repl fold command: unknown repo key %s" key))
    (let ((desired (eq (cdr folded-cell) t)))
      (if (eq desired (agent-repl--repo-folded-p key))
          (agent-repl--log-verbose nil "host-action set-repository-fold: key=%s already folded=%s"
                                    key desired)
        (agent-repl--toggle-repo-fold key))
      (agent-repl--log nil "host-action set-repository-fold: key=%s folded=%s" key desired)
      (agent-repl--force-tab-bar-redraw)
      (agent-repl--sidebar-push))))

;;;; ---- View + task command handlers (webview clicks, via the daemon) -----

(defun agent-repl--sidebar-set-view (view)
  "Set the active sidebar VIEW (`:repository' / `:task') and push the roster.
Signals on an unknown VIEW rather than defaulting — the caller is the
webapp's view selector, whose value set is the same contract."
  (unless (memq view '(:repository :task))
    (agent-repl--log nil "sidebar-set-view: rejected unknown view=%S" view)
    (error "agent-repl--sidebar-set-view: unknown view %S" view))
  (setq agent-repl--sidebar-view view)
  (agent-repl--log nil "sidebar-set-view: view=%s" view)
  (agent-repl--sidebar-push))

(defun agent-repl--handle-set-view-command (cmd)
  "Handle a \"set-view\" command CMD (the sidebar's view selector).
CMD carries `view', one of the wire strings in
`agent-repl--sidebar-view-wire'."
  (let ((view (alist-get 'view cmd)))
    (unless (member view '("repository" "task"))
      (agent-repl--log nil "host-action set-sidebar-view: rejected view=%S" view)
      (error "agent-repl set-view command: bad view in %S" cmd))
    (agent-repl--log nil "host-action set-sidebar-view: view=%s" view)
    (agent-repl--sidebar-set-view (if (equal view "task") :task :repository))))

(defvar agent-repl--sidebar-prompt-pending nil
  "Label of the deferred sidebar minibuffer prompt currently in flight.
Nil when none is scheduled or running.  Only ONE sidebar prompt may be
outstanding at a time: a second `read-string' entered from a timer while
another minibuffer is already active either errors or stacks a recursive
read the user cannot see the top of, so a second click is dropped with a
log line rather than queued.  `defvar' keeps the flag across module hot
reload, which is what stops a reload mid-prompt from re-enabling a second
concurrent read.")

(defun agent-repl--sidebar-defer-prompt (label thunk)
  "Schedule THUNK, a zero-arg minibuffer prompt, off the transport context.
LABEL names the host action for the log lines and for
`agent-repl--sidebar-prompt-pending'.

WHY IT EXISTS.  Host-action handlers run inside the unix-socket
transport's process filter.  A synchronous `read-string' /
`completing-read' there opens a recursive minibuffer with no normal
command loop behind it: input events route by the pre-prompt selected
window — the xwidget webview buffer, whose keymap binds almost nothing —
so every key the user types reports as unbound.  Handing the read to a
zero-delay timer runs it from the ordinary event loop instead, where the
minibuffer gets input.

ACK SEMANTICS.  The handler returns as soon as the prompt is SCHEDULED
and does NOT call `agent-repl--host-action-defer', so the executor
completes the action `ok=true' immediately.  That is deliberate on both
sides: holding an action open across a human's typing would pin the
daemon's action indefinitely, and a cancelled prompt is a normal outcome
that must never be reported as a failed action.  The ack means the user
was asked, which is the whole of what the daemon asked Emacs to do.

Returns non-nil when scheduled, nil when dropped as a duplicate."
  (if agent-repl--sidebar-prompt-pending
      (progn
        (agent-repl--log
         nil
         "host-action %s: sidebar prompt already pending=%s, dropping this request"
         label agent-repl--sidebar-prompt-pending)
        nil)
    (setq agent-repl--sidebar-prompt-pending label)
    (agent-repl--log nil "host-action %s: deferring the minibuffer prompt" label)
    (run-at-time
     0 nil
     (lambda ()
       ;; `unwind-protect' clears the flag on a `keyboard-quit' out of the
       ;; minibuffer and on a genuine error alike, so one aborted prompt
       ;; cannot wedge the button.  The error itself is NOT caught — it
       ;; still propagates to the timer machinery and is reported.
       (unwind-protect
           (funcall thunk)
         (setq agent-repl--sidebar-prompt-pending nil))))
    t))

(defun agent-repl--task-create-interactive ()
  "Prompt for a task title in the minibuffer and create the task.
Emacs owns the task model, so the webapp asks Emacs to create rather than
carrying a title itself.  An empty title is a cancelled prompt and creates
nothing.  MUST run from the ordinary command loop, never from a process
filter — see `agent-repl--sidebar-defer-prompt'."
  (let ((title (read-string "Task title: ")))
    (if (string-empty-p (string-trim title))
        (agent-repl--log nil "host-action task-create: empty title, skipping")
      (agent-repl--task-create title)
      (agent-repl--log nil "host-action task-create: created task from non-empty title")
      (agent-repl--sidebar-push))))

(defun agent-repl--handle-task-create-command (_cmd)
  "Handle a \"task-create\" command (the Task view's + button).
Defers the title prompt out of the transport's process-filter context;
see `agent-repl--sidebar-defer-prompt' for why, and for what this
handler's ack does and does not promise."
  (agent-repl--sidebar-defer-prompt
   "task-create" #'agent-repl--task-create-interactive))

(defun agent-repl--handle-task-toggle-done-command (cmd)
  "Handle a \"task-toggle-done\" command CMD (a task checkbox click).
CMD carries the task `id'."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (agent-repl--log nil "host-action task-toggle-done: rejected missing-or-empty id")
      (error "agent-repl task-toggle-done command: missing id in %S" cmd))
    (agent-repl--log nil "host-action task-toggle-done: id=%s" id)
    (agent-repl--task-toggle-done id)
    (agent-repl--sidebar-push)))

(defun agent-repl--handle-task-open-command (cmd)
  "Handle a \"task-open\" command CMD (a task label click).
Opens the task's org notes file in a popup (`agent-repl--task-open')."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (agent-repl--log nil "host-action task-open: rejected missing-or-empty id")
      (error "agent-repl task-open command: missing id in %S" cmd))
    (agent-repl--log nil "host-action task-open: id=%s" id)
    (agent-repl--task-open id)))

(defun agent-repl--task-add-workspace-interactive (id)
  "Prompt for a live workspace and assign it to task ID.
Offers every live workspace not already effectively in this task; the
chosen workspace's `:task-id' is set so it and its future children
render under the task.  A workspace already inheriting the task through
its parent is omitted — it needs no assignment.  A brand-new workspace
for a task is made the ordinary way (or cut as a child of one already in
the task, which inherits it), then added here.

MUST run from the ordinary command loop, never from a process filter —
see `agent-repl--sidebar-defer-prompt'."
  (unless (agent-repl--task-get id)
    (agent-repl--log nil "task-add-workspace: rejected unknown task id=%s" id)
    (error "agent-repl--task-add-workspace-interactive: unknown task %s" id))
  (let* ((candidates
          (cl-remove-if
           (lambda (e) (equal (agent-repl--ws-effective-task-id (car e)) id))
           (agent-repl--sidebar-entries)))
         (names (mapcar #'car candidates)))
    (if (null names)
        (progn
          (agent-repl--log nil "task-add-workspace: task=%s no eligible workspaces" id)
          (message "agent-repl: no workspaces available to add to this task"))
      (let ((name (completing-read "Add workspace to task: " names nil t)))
        (when (and name (not (string-empty-p name)))
          (agent-repl--log name "task-add-workspace: task=%s assigning ws=%s" id name)
          (agent-repl--task-assign-workspace name id)
          (agent-repl--sidebar-push))
        (when (or (null name) (string-empty-p name))
          (agent-repl--log nil "task-add-workspace: task=%s chooser cancelled" id))))))

(defun agent-repl--handle-task-add-workspace-command (cmd)
  "Handle a \"task-add-workspace\" command CMD (a task section + button).
CMD carries the task `id'; the workspace is chosen in the minibuffer.
The chooser is deferred out of the transport's process-filter context
\(see `agent-repl--sidebar-defer-prompt'), so every check that must be
able to NACK the action runs HERE, synchronously — an error signalled
from the deferred timer would be reported to the user but could no
longer fail the host action.  `agent-repl--task-add-workspace-interactive'
re-validates the id anyway; the duplication is deliberate, since that
function is also reachable outside this handler."
  (let ((id (alist-get 'id cmd)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (agent-repl--log nil "host-action task-add-workspace: rejected missing-or-empty id")
      (error "agent-repl task-add-workspace command: missing id in %S" cmd))
    (unless (agent-repl--task-get id)
      (agent-repl--log nil "host-action task-add-workspace: rejected unknown task id=%s" id)
      (error "agent-repl task-add-workspace command: unknown task %s" id))
    (agent-repl--log nil "host-action task-add-workspace: id=%s" id)
    (agent-repl--sidebar-defer-prompt
     "task-add-workspace"
     (lambda () (agent-repl--task-add-workspace-interactive id)))))

(provide 'agent-repl-sidebar)
;;; sidebar.el ends here
