;;; roster.el --- the WatchWorkspaceRoster consumer -*- lexical-binding: t; -*-

;;; Commentary:

;; THE ROSTER IS THE ONE SOURCE OF EMACS'S TABS: which workspaces exist,
;; what order they are in, what each one is called, and what colour it
;; paints.  `agentrepl.v1' WatchWorkspaceRoster is the one GLOBAL stream —
;; it carries no workspace, every client watches the same view, and the
;; daemon's stream order is the only order.  Emacs never builds a roster: its
;; whole contribution to it is two verbs elsewhere (RegisterWorkspace and
;; SelectWorkspace), and everything drawn here is read-only.
;;
;; WHAT THIS FILE OWNS:
;;
;; - the subscription (started on link-up, restarted after a link bounce);
;; - `agent-repl-roster-view', the last decoded roster, and
;;   `agent-repl-roster-update-functions', run after every accepted push;
;; - TAB RECONCILIATION from the rows' `closed' flag: a row with
;;   `closed = false' has a tab, one with `closed = true' has none, and the
;;   tab order is the roster's walk order strictly (repository sections in
;;   order, rows depth-first, then the recently-merged rows).  The task view
;;   is IGNORED: it is the same rows regrouped, and drawing both would
;;   double every tab;
;; - TAB IDENTITY IS THE REF ID.  A row whose id matches an existing tab is
;;   the same workspace whatever its name, so a renamed row RENAMES its tab
;;   rather than opening a second one.  Names collide across repos; ids do
;;   not, so a duplicated display name is disambiguated with "·<repo label>";
;; - the R8 CURRENT-CHANGE reaction: a `current' Emacs did not originate is a
;;   tab-switch request (re-selection is idempotent, so no loop forms);
;; - THE FINISH EDGE — a row moving from a RUNNING status to a SETTLED one —
;;   and the three Emacs-local reactions that ride it.  The fourth (the
;;   deferred-prompt drain) is registered by prompt-queue.el on the same
;;   hook.
;;
;; WHAT THIS FILE DOES NOT OWN: registration (the roster's rows are already
;; registered daemon-side; Register is host.el's verb on Emacs's own
;; worktrees), the paint itself (status.el composes it from the status arm),
;; and the per-workspace host stream (host.el), which this file only asks to
;; start and stop as tabs come and go.

;;; Code:

(require 'cl-lib)

(declare-function agent-repl--log "core" (ws format-string &rest args))
(declare-function agent-repl--info "core" (ws format-string &rest args))
(declare-function agent-repl--warn "core" (ws format-string &rest args))
(declare-function agent-repl--error "core" (ws format-string &rest args))
(declare-function agent-repl--next-log-request-id "core" ())
(declare-function agent-repl--with-log-context "core"
                  (workspace request-id function))
(defvar agent-repl--global-log-scope)
(defvar agent-repl--log-context-request-id)
(defvar agent-repl--log-context-workspace)
(declare-function agent-repl--current-ws-p "core" (ws))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-rpc-watch-workspace-roster "rpc"
                  (conn on-push on-close &optional on-open))
(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl--ws-live-p "workspace" (ws))
(declare-function agent-repl--live-ws-names "workspace" ())
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--ws-create "workspace" (ws &optional project-dir))
(declare-function agent-repl--ws-del "workspace" (ws))
(declare-function agent-repl--ws-revive "workspace" (ws))
(declare-function agent-repl--ws-land-then-kill "workspace" (ws))
(declare-function agent-repl--ws-rename-state "workspace" (old new dir))
(declare-function agent-repl--ws-rename-persp "workspace" (old new))
(declare-function agent-repl--ws-switch "workspace" (ws &rest args))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--ws-log-name "workspace" (ws))
(declare-function agent-repl--refresh-magit-status-for-dir "session" (dir &optional ws))
(declare-function agent-repl--maybe-notify-finished "session" (ws))
;; W2-A's names (host.el, daemon-link.el).  Declared, never defined here.
(declare-function agent-repl--panels-open-on-arrival "panels" (ws id))
(declare-function agent-repl--panels-arm-arrivals "panels" ())
(declare-function agent-repl-host-subscribe "host" (conn ws ref))
(declare-function agent-repl-host-unsubscribe "host" (ws))
(declare-function agent-repl-host-rename "host" (old new))
(declare-function agent-repl-link-primary "daemon-link" ())
(defvar agent-repl-host-last-selected-id)
(defvar agent-repl-host-reselect-pending)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-link-down-functions)

;;;; ---- The view ---------------------------------------------------------

(defvar agent-repl-roster-view nil
  "The last roster the daemon pushed, decoded, or nil before the first push.
Whole, never a delta — a partially-applied roster is unrepresentable by
construction, so a new push REPLACES this value outright.")

(defvar agent-repl-roster-update-functions nil
  "Abnormal hook run with the decoded ROSTER after every accepted push.
Runs AFTER reconciliation, so a handler observing the tabs sees the ones
this push produced.")

(defvar agent-repl-roster-status-change-functions nil
  "Abnormal hook run with (WS PREVIOUS CURRENT) per row whose ARM CHANGED.

THE ONE ANNOUNCEMENT THAT A WORKSPACE\='S STATUS CHANGED, whatever caused
it: a prompt the user sent, a frame the shim pushed, a merge, a session
that died.  Emacs learns every status from this one stream, so a reaction
registered here sees every origin without enumerating any of them.

Runs before `agent-repl-roster-update-functions\=' and only for rows that
have a tab.  A first sighting is NOT a change (PREVIOUS is never nil).")

(defvar agent-repl-roster-viewed-cleared-functions nil
  "Abnormal hook run with WS per row whose VIEWED MARKER WAS CLEARED.

The daemon is the single source of the viewed (partial) mode: it raises
`RosterRowViewed' on a row when Emacs reports a dwell and clears it on any
status change.  This hook fires on the present->absent edge of that marker,
computed against the previous accepted push.  A restated marker is not a
clear, and a first sighting without the marker is not a clear.

Runs before `agent-repl-roster-update-functions\=' and only for rows that
have a tab.")

(defvar agent-repl-roster-bringup-functions nil
  "Abnormal hook run with (OPENED TOTAL FINISHED) as a reconcile opens tabs.

THE ONLY REPORT OF A WORKSPACE BRING-UP THAT DOES NOT DEPEND ON PAINT.
The pending-open registry (`agent-repl-open-progress-opening-workspaces\=')
answers \"which workspace is not PAINTED yet\", and that is the right
answer for an open the user asked for by hand.  It is the wrong one for a
STARTUP: panels park until a workspace is focused, so a cold start that
brings up ten workspaces paints none of them and the painted count never
moves.  The startup phase the user was promised — `loading workspaces
\(n/m\)\' between \"linking\" and \"ready\" — then never appeared at all.

This hook reports the other thing, the one that IS happening: the roster
opening a tab per row it has and this Emacs does not.  OPENED counts the
tabs this reconcile has opened so far, TOTAL the tabs it set out to open
\(rows with no tab yet, counted before the walk\), and FINISHED is nil on
each step and non-nil on the one call that closes the pass.

It fires ONLY for tabs that are actually born, so a steady-state push —
every row already tabbed — publishes nothing and no handler has to filter
one out.")

(defvar agent-repl-roster-finish-functions nil
  "Abnormal hook run with WS on the FINISH EDGE — RUNNING to SETTLED.
Fires exactly ONCE per edge: a row already settled that is pushed again
is not an edge, and a permission ask returning to thinking is a move
WITHIN the running set, not out of it.")

(defvar agent-repl-roster--stream nil
  "The standing WatchWorkspaceRoster stream, or nil when unsubscribed.")

(defvar agent-repl-roster--rows-by-id (make-hash-table :test 'equal)
  "Ref id -> the decoded `RosterRow' the last accepted push carried.
status.el reads a workspace's paint out of this table, so it is indexed
from EVERY row (closed ones included): a closed row has no tab, but a
caller that names it still deserves the daemon's answer rather than a
guess.")

(defvar agent-repl-roster--status-by-id (make-hash-table :test 'equal)
  "Ref id -> the status arm keyword of the PREVIOUS accepted push.
The finish edge is a comparison against this table, which is why it is
updated only after the edges of a push have been computed.")

(defvar agent-repl-roster--viewed-by-id (make-hash-table :test 'equal)
  "Ref id -> non-nil when the PREVIOUS accepted push carried the viewed marker.
The viewed-cleared edge is a comparison against this table, so it is
replaced only after that edge has been computed.")

(defvar agent-repl-roster--tab-order nil
  "Workspace names in roster walk order — the tab bar's order, strictly.
No local ordering exists: the resolver orders the roster (priority
included) and the tab bar follows it.")

;;;; ---- The status vocabulary --------------------------------------------

(defconst agent-repl-roster-running-statuses
  '(:submitting :thinking :clearing :compacting :permission)
  "The RUNNING half of the finish edge — the agent holds the turn.
A permission ask is RUNNING: the turn has not ended, it is waiting on
the user, so permission -> thinking is a move within this set and no
finish edge at all.")

(defconst agent-repl-roster-settled-statuses
  '(:ready :done :interrupted :idle-async)
  "The SETTLED half of the finish edge — the foreground turn has ended.
`idle-async' is settled deliberately: no FOREGROUND turn is running, and
detached work is work the user may talk over.")

;;;; ---- Reading a row ----------------------------------------------------

(defun agent-repl-roster-row-ref (row)
  "Return ROW's `WorkspaceRef' plist.
The contract nests it (`RosterRow.workspace' is a `RosterRowWorkspace',
which carries the ref), and the decode preserves that nesting, so the
reach-through lives here once instead of at every reader."
  (plist-get (plist-get row :workspace) :workspace))

(defun agent-repl-roster-row-id (row)
  "Return ROW's workspace id — the opaque, byte-wise-compared identity."
  (plist-get (agent-repl-roster-row-ref row) :id))

(defun agent-repl-roster-row-name (row)
  "Return ROW's display name text.  Never an identity."
  (plist-get (plist-get row :name) :text))

(defun agent-repl-roster-row-status (row)
  "Return ROW's status arm keyword — the set arm IS the status."
  (plist-get (plist-get row :status) :arm))

(defun agent-repl-roster-row-closed-p (row)
  "Return non-nil when ROW is CLOSED: no tab, whatever its lifecycle."
  (eq (plist-get (plist-get row :closed) :closed) t))

(defun agent-repl-roster-row-attention-p (row)
  "Return non-nil when ROW carries the attention marker."
  (and (plist-get row :attention) t))

(defun agent-repl-roster-row-priority-label (row)
  "Return ROW's priority badge label, or nil when it is unprioritized."
  (plist-get (plist-get row :priority) :label))

(defun agent-repl-roster-row-viewed-p (row)
  "Return non-nil when ROW carries the viewed (partial) marker."
  (and (plist-get row :viewed) t))

(defun agent-repl-roster-viewed-for-ws (ws)
  "Return the viewed marker of WS's current roster row, or nil.
Nil before any push has carried a row for WS.  This is the ONLY input to
the tab-bar's partial/full decision: the daemon owns the mode."
  (let ((row (agent-repl-roster-row-for-ws ws)))
    (and row (plist-get row :viewed))))

;;;; ---- The walk ---------------------------------------------------------

(defun agent-repl-roster--walk-rows (rows label acc)
  "Walk ROWS depth-first under section LABEL, pushing entries onto ACC.
Each entry is `(:row ROW :label LABEL)'.  A row precedes its children,
which is the render order the contract's nesting states."
  (dolist (row rows)
    (push (list :row row :label label) (car acc))
    (agent-repl-roster--walk-rows (plist-get row :children) label acc))
  acc)

(defun agent-repl-roster-walk (roster)
  "Return ROSTER's rows in tab order: `(:row ROW :label REPO-LABEL)' entries.
Repository sections in the resolver's order, each section's rows
depth-first, then the recently-merged section's rows.  THE TASK VIEW IS
IGNORED — it is the same workspaces regrouped, and walking it too would
give every workspace a second tab."
  (let ((acc (list nil)))
    (dolist (section (plist-get (plist-get roster :repository) :sections))
      (agent-repl-roster--walk-rows
       (plist-get (plist-get section :rows) :rows)
       (plist-get (plist-get (plist-get section :header) :label) :text)
       acc))
    (let ((merged (plist-get roster :recently-merged)))
      (agent-repl-roster--walk-rows
       (plist-get (plist-get merged :rows) :rows)
       (plist-get (plist-get (plist-get merged :header) :label) :text)
       acc))
    (nreverse (car acc))))

(defun agent-repl-roster--section-holds-id-p (section id)
  "Return non-nil when SECTION's rows include the workspace ID."
  (let ((acc (list nil)))
    (agent-repl-roster--walk-rows
     (plist-get (plist-get section :rows) :rows) nil acc)
    (seq-some (lambda (entry)
                (equal (agent-repl-roster-row-id (plist-get entry :row)) id))
              (car acc))))

(defun agent-repl-roster-repository-of (id &optional roster)
  "Return the `RepositoryRef' of the section holding workspace ID, or nil.
ROSTER defaults to `agent-repl-roster-view'.  The repository is the
SECTION'S OWN key, never anything derived from a row: sections are the
repository grouping, and the key is the imported join token a verb echoes
back.  Nil when no section holds ID — including before the first push,
when Emacs holds no roster at all."
  (let ((sections (plist-get (plist-get (or roster agent-repl-roster-view)
                                        :repository)
                             :sections))
        (found nil))
    (dolist (section sections)
      (when (and (null found) (agent-repl-roster--section-holds-id-p section id))
        (setq found (plist-get (plist-get section :key) :repository))))
    found))

;;;; ---- Invariants -------------------------------------------------------

(defun agent-repl-roster--duplicate-id (entries)
  "Return the first ref id appearing twice in ENTRIES, or nil when unique.
Ids are the tab identity, so a duplicate would make two rows the same
tab: the push is unusable and is dropped rather than half-applied."
  (let ((seen (make-hash-table :test 'equal))
        (dup nil))
    (dolist (entry entries)
      (let ((id (agent-repl-roster-row-id (plist-get entry :row))))
        (if (gethash id seen)
            (unless dup (setq dup id))
          (puthash id t seen))))
    dup))

;;;; ---- Names ------------------------------------------------------------

(defun agent-repl-roster--tab-name (entry collisions)
  "Return ENTRY's tab name, suffixed when its display name COLLIDES.
COLLISIONS is the set of names carried by more than one open row.  Names
collide across repos and ids do not, so a collision is disambiguated
with the repo label rather than resolved by dropping a tab."
  (let ((name (agent-repl-roster-row-name (plist-get entry :row)))
        (label (plist-get entry :label)))
    (if (and (gethash name collisions) (stringp label) (not (string-empty-p label)))
        (concat name "·" label)
      name)))

(defun agent-repl-roster--colliding-names (entries)
  "Return a hash of the display names more than one of ENTRIES carries."
  (let ((counts (make-hash-table :test 'equal))
        (collisions (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((name (agent-repl-roster-row-name (plist-get entry :row))))
        (puthash name (1+ (gethash name counts 0)) counts)))
    (maphash (lambda (name count) (when (> count 1) (puthash name t collisions)))
             counts)
    collisions))

(defun agent-repl-roster-desired-tabs (roster)
  "Return the tabs ROSTER asks for, in order: plists `(:id :name :ref)'.
Only rows with `closed = false' get a tab; the daemon sets `closed' on
merged, closed and killed rows, so this is the whole membership rule.

THE ROWS IT REFUSES A TAB ARE RECORDED.  This predicate is the whole
answer to \"why is there no tab for that workspace\", and it used to give
it silently: a register whose row came back CLOSED produced no tab, and
nothing anywhere said the roster had been asked for one and declined.
Once per push over every closed row, which is why it is DEBUG."
  (let* ((all (agent-repl-roster-walk roster))
         (entries (cl-remove-if (lambda (entry)
                                  (agent-repl-roster-row-closed-p (plist-get entry :row)))
                                all))
         (collisions (agent-repl-roster--colliding-names entries)))
    (dolist (entry all)
      (let ((row (plist-get entry :row)))
        (when (agent-repl-roster-row-closed-p row)
          (agent-repl--log '(:agent-repl-central
                             "a closed row the roster gives no tab owns no workspace sink")
                           "elisp.roster.no-tab: id=%s name=%s reason=closed"
                           (agent-repl-roster-row-id row)
                           (agent-repl-roster-row-name row)))))
    (mapcar (lambda (entry)
              (let ((row (plist-get entry :row)))
                (list :id (agent-repl-roster-row-id row)
                      :name (agent-repl-roster--tab-name entry collisions)
                      :ref (agent-repl-roster-row-ref row))))
            entries)))

;;;; ---- Reconciliation ---------------------------------------------------

(defun agent-repl-roster--open-tab (desired)
  "Open the tab DESIRED asks for and start its host stream.
The roster's rows are ALREADY REGISTERED daemon-side — Register is not
this file's verb — so the tab's whole birth is a perspective plus the
`WatchHostWorkspace' subscription."
  (let* ((name (plist-get desired :name))
         (ref (plist-get desired :ref))
         (dir (plist-get ref :dir)))
    ;; A row that went `closed = true' and came back tore its tab down, and
    ;; `--ws-del' TOMBSTONES rather than removes: the name still carries
    ;; `:killed-at', so `--ws-by-ref-id', `--live-ws-names' and every
    ;; filtered iterator skip it.  Reopening it without clearing that stamp
    ;; would write the ref and the dir onto a name that stays dead --
    ;; `closed = false' would never actually "ensure a tab exists" (fanout
    ;; §8).  The tombstone is cleared through workspace.el's own revive
    ;; boundary, which keeps `:last-killed-at' so the previous close stays
    ;; on the record.
    (agent-repl--ws-revive name)
    (agent-repl--ws-create name dir)
    (agent-repl--ws-put name :ref ref)
    (agent-repl--ws-put name :dir dir)
    ;; `:project-dir' is the identity key the REST of the system reads -- the
    ;; durable log sink, history, the composer's attachment root, panels,
    ;; magit.  `agent-repl--ws-create' seeds it only when persp-mode hands
    ;; back a real perspective object, so a tab born without one (persp-mode
    ;; absent, or the `persp-not-persp' sentinel) would be a project-dir-less
    ;; `(no repo)' stub for the rest of its life.  The row's ref is the only
    ;; authority on the directory, so the write happens here, unconditionally,
    ;; from the same `dir' `:dir' gets.
    (agent-repl--ws-put name :project-dir dir)
    (agent-repl--info name "elisp.roster.tab-open: ws=%s id=%s dir=%s"
                      name (plist-get desired :id) dir)
    (agent-repl-roster--subscribe-host name ref)
    ;; A TAB BORN HERE IS A WORKSPACE THAT JUST BECAME OPEN, and opening a
    ;; workspace opens its panels before it is switched to (owner ruling,
    ;; 2026-09-13, item 6).  This is the one landing every open takes --
    ;; create, fork, one-shot, re-open, register, and any daemon-side
    ;; arrival -- so it is the one place the panels are asked for.  The
    ;; startup roster is exempt and panels.el is what knows it
    ;; (`agent-repl--panels-arrivals-armed').  A plain switch never reaches
    ;; here: its tab already exists.
    (agent-repl--panels-open-on-arrival name (plist-get desired :id))
    name))

(defun agent-repl-roster--subscribe-host (name ref)
  "Start NAME's host stream for REF on the primary link, when there is one."
  (let ((conn (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary))))
    (if conn
        (progn
          (agent-repl-host-subscribe conn name ref)
          (agent-repl--log name "elisp.roster.host-subscribe: ws=%s id=%s"
                           name (plist-get ref :id)))
      (agent-repl--info name
                        "elisp.roster.host-subscribe: skipped ws=%s reason=no-primary-link"
                        name))))

(defun agent-repl-roster--rename-tab (old new ref)
  "Rename the tab OLD to NEW, keeping every fact keyed on the ref id.
A row's id is its identity, so a changed name is a RENAME and never a
second tab."
  (if (not (agent-repl-roster--rename-state old new (plist-get ref :dir)))
      ;; Refused WHOLE.  `agent-repl--ws-rename-state' validates before it
      ;; mutates anything, so a refusal leaves every keyed fact under OLD
      ;; and the tab keeps its old name rather than half-moving.  The ref
      ;; is still written through, because the row's ref is the authority
      ;; on the identity whatever the name ended up being.
      (progn
        (agent-repl--ws-put old :ref ref)
        old)
    ;; Host state is keyed on the workspace name too (fanout §7), so it is
    ;; re-keyed in the SAME breath: a rename that moved only the workspace
    ;; table leaves `agent-repl-host-ref' answering nil for the new name.
    (agent-repl-host-rename old new)
    (agent-repl--ws-rename-persp old new)
    (agent-repl--ws-put new :ref ref)
    (agent-repl--ws-put new :dir (plist-get ref :dir))
    (agent-repl--info new "elisp.roster.tab-rename: old=%s new=%s id=%s"
                      old new (plist-get ref :id))
    new))

(defun agent-repl-roster--rename-state (old new dir)
  "Rename OLD to NEW with DIR, returning nil when the rename is REFUSED.
`agent-repl--ws-rename-state' signals `user-error' on every refusal --
a target colliding with a live OR TOMBSTONED name among them.  That
signal must not escape the push handler: it would abort the reconcile
walk mid-list, leaving the tab bar describing a roster nobody finished
reading.  The refusal is recorded at ERROR with the reason and answered
as nil, so the caller applies the rename or leaves it alone entire."
  (condition-case err
      (progn (agent-repl--ws-rename-state old new dir) t)
    (user-error
     (agent-repl--error old
                        "elisp.roster.tab-rename-refused: old=%s new=%s dir=%S error=%s"
                        old new dir (error-message-string err))
     nil)))

(defun agent-repl-roster--tear-down-tab (name)
  "Tear NAME's tab down: cancel its host stream, kill the persp, tombstone it.
IDEMPOTENT, because CloseWorkspace's own success tears the tab down too
and the roster push that follows must not care which of the two got
there first.

The persp kill runs against a LIVE FRAME -- windows, dedications, buffers
with processes -- so it is the one step here that can signal on something
the roster knows nothing about.  Such a signal must not escape the push
handler, for the same reason a refused rename must not
(`agent-repl-roster--rename-state'): it would abort the reconcile walk
mid-list, leaving every tab after this one describing a roster nobody
finished reading.  It is recorded at ERROR with the workspace and the
error, and the tombstone is still written -- the row is gone from the
daemon either way, and a tab whose persp outlived its kill must not also
keep its registry entry.

A TEARDOWN THAT TAKES THE WORKSPACE THE USER IS STANDING ON MUST NAME
WHERE THEY END UP, and it names it BEFORE the kill, through the one
teardown order every teardown uses (`agent-repl--ws-land-then-kill'), the
same one `agent-repl--kill-one-workspace' calls -- never a second order of
its own.  Killing first dropped the frame wherever persp-mode left it and
landed afterwards; landing first means the persp killed is no longer
current, so the kill cannot reach the landing workspace's windows.  The
landing is a no-op when the user stands on some other live workspace, so
a teardown of some OTHER tab does not move the user.

A refused kill or a failed landing signals out of that function and is
contained here for the same reason a failing kill is: an escape would
abort the reconcile walk mid-list."
  (when (fboundp 'agent-repl-host-unsubscribe)
    (agent-repl-host-unsubscribe name))
  (condition-case err
      (agent-repl--ws-land-then-kill name)
    (error
     (agent-repl--error name "elisp.roster.tab-teardown-persp-kill-failed: ws=%s error=%s"
                        name (error-message-string err))))
  (agent-repl--ws-del name)
  (agent-repl--info name "elisp.roster.tab-teardown: ws=%s" name))

(defun agent-repl-roster--roster-owned-names ()
  "Return every live workspace this file opened — those carrying a `:ref'.
A workspace with no ref never came from the roster, so reconciliation
leaves it alone rather than tearing down something it does not own."
  (cl-remove-if-not (lambda (name) (agent-repl--ws-get name :ref))
                    (agent-repl--live-ws-names)))

(defun agent-repl-roster--log-row-failure (ws fmt &rest args)
  "Record a per-row reconcile failure for WS at ERROR.
A row whose reconcile failed is exactly the row whose workspace may own no
durable sink — a worktree deleted underneath us is the commonest way to get
here — so the scope is screened through `agent-repl--ws-log-name' and falls
back to the central sink, with the name kept in the message text."
  (apply #'agent-repl--error
         (or (and ws (agent-repl--ws-log-name ws))
             '(:agent-repl-central
               "a roster row whose reconcile failed may own no workspace sink"))
         fmt args))

(defun agent-repl-roster--reconcile-row (want wanted-ids)
  "Reconcile one roster row WANT, recording its id in WANTED-IDS.
Returns the tab name the row settled on, or nil when the row FAILED.

EACH ROW IS ISOLATED.  A tab's birth touches persp-mode, the workspace
registry and the host subscription, any of which can signal on something
the roster knows nothing about — a worktree deleted under a `:project-dir',
a persp kill that hit a live process buffer.  An escape aborts the walk
mid-list and every remaining row goes untabbed: that is exactly how one
workspace with a vanished directory left a whole cold start with no tabs
at all.  So a failed row is recorded at ERROR and DROPPED from the order,
and every other row is still opened and ordered."
  (puthash (plist-get want :id) t wanted-ids)
  (let* ((id (plist-get want :id))
         (name (plist-get want :name))
         (existing (agent-repl--ws-by-ref-id id)))
    (condition-case err
        (cond
         ((null existing)
          (agent-repl-roster--open-tab want))
         ((equal existing name)
          (agent-repl--ws-put name :ref (plist-get want :ref))
          (agent-repl--log name "elisp.roster.tab-kept: ws=%s id=%s" name id)
          name)
         (t
          (agent-repl-roster--rename-tab existing name (plist-get want :ref))))
      (error
       (agent-repl-roster--log-row-failure
        (or existing name)
        "elisp.roster.row-reconcile-failed: ws=%s id=%s error=%s"
        name id (error-message-string err))
       nil))))

(defun agent-repl-roster--note-bringup (opened total finished)
  "Publish OPENED of TOTAL tabs opened to `agent-repl-roster-bringup-functions\='.
FINISHED marks the call that closes the reconcile pass.  A handler that
signals is CONTAINED, exactly as a change handler is: a progress display
must never take down the reconciliation it is reporting on."
  (dolist (fn agent-repl-roster-bringup-functions)
    (condition-case err
        (funcall fn opened total finished)
      (error
       (agent-repl--warn
        '(:agent-repl-central "roster reconciliation spans every workspace")
        "elisp.roster.bringup-handler-failed fn=%s error=%s"
        fn (error-message-string err))))))

(defun agent-repl-roster-reconcile (roster)
  "Bring the tab bar in line with ROSTER and return the tab names in order.
Opens a tab for each `closed = false' row that has none, renames the tab
whose row's name changed, tears down every roster-owned tab whose row is
gone or closed, and sets the tab ORDER to the walk order strictly.

A row that fails to reconcile is contained rather than fatal; see
`agent-repl-roster--reconcile-row'."
  (let* ((desired (agent-repl-roster-desired-tabs roster))
         (wanted-ids (make-hash-table :test 'equal))
         ;; Counted BEFORE the walk, because the walk is what makes rows
         ;; stop being tabless: asking the same question afterwards would
         ;; always answer zero.
         (untabbed (cl-count-if
                    (lambda (want)
                      (null (agent-repl--ws-by-ref-id (plist-get want :id))))
                    desired))
         (opened 0)
         (names nil))
    (dolist (want desired)
      (let* ((fresh (null (agent-repl--ws-by-ref-id (plist-get want :id))))
             (name (agent-repl-roster--reconcile-row want wanted-ids)))
        (when name (push name names))
        ;; A row that FAILED opened no tab, so it does not count towards
        ;; the bring-up and the pass ends below `untabbed'.
        (when (and fresh name)
          (setq opened (1+ opened))
          (agent-repl-roster--note-bringup opened untabbed nil))))
    (dolist (name (agent-repl-roster--roster-owned-names))
      (let ((id (plist-get (agent-repl--ws-get name :ref) :id)))
        (unless (gethash id wanted-ids)
          ;; The teardown contains its own signals step by step, but the
          ;; registry writes BETWEEN those steps are covered by none of them,
          ;; and a signal here would abort the walk over the remaining torn
          ;; down rows for the same reason a failed open must not.
          (condition-case err
              (agent-repl-roster--tear-down-tab name)
            (error
             (agent-repl-roster--log-row-failure
              name "elisp.roster.row-teardown-failed: ws=%s error=%s"
              name (error-message-string err)))))))
    (setq agent-repl-roster--tab-order (nreverse names))
    ;; INFO, not DEBUG.  This is the record that says the roster push became
    ;; a tab bar, and it is the end of the startup's first-roster phase --
    ;; but the DEBUG rung does not clear the durable sink's default `info'
    ;; level, so for the whole life of this line it reached no file at all:
    ;; 23 accumulated realtest runs hold 108 `elisp.roster.tab-open' records
    ;; written by this very walk and not one `elisp.roster.reconcile'.  An
    ;; invisible action is a logging defect (AGENTS.md), and this is
    ;; lifecycle a person asks about, at the level its siblings in this file
    ;; already use.
    (agent-repl--info '(:agent-repl-central "roster reconciliation spans every workspace")
                      "elisp.roster.reconcile: tabs=%d order=%S"
                      (length agent-repl-roster--tab-order)
                      agent-repl-roster--tab-order)
    ;; LAST, after the order is set, so a handler that reads the tab bar
    ;; sees the one this pass produced rather than the previous pass's.
    (when (> opened 0)
      (agent-repl-roster--note-bringup opened untabbed t))
    ;; The startup roster has now been delivered, so every LATER arrival is a
    ;; workspace that became open with this editor watching and opens its own
    ;; panels (`agent-repl--panels-open-on-arrival').
    (agent-repl--panels-arm-arrivals)
    agent-repl-roster--tab-order))

;;;; ---- Lookups the renderers use ----------------------------------------

(defun agent-repl-roster-row-for-ws (ws)
  "Return the roster row for workspace WS, or nil when the roster has none."
  (let ((id (plist-get (agent-repl--ws-get ws :ref) :id)))
    (and id (gethash id agent-repl-roster--rows-by-id))))

(defun agent-repl-roster-status-for-ws (ws)
  "Return WS's roster status arm keyword, or nil before its first push."
  (let ((row (agent-repl-roster-row-for-ws ws)))
    (and row (agent-repl-roster-row-status row))))

(defun agent-repl-roster-tab-order ()
  "Return the tab names in roster walk order — the tab bar's only order."
  agent-repl-roster--tab-order)

(defun agent-repl-roster-move-tab-to-back (ws)
  "Move WS to the LAST slot of the tab order and return the new order.
Returns nil, leaving the order untouched, when WS is not on the tab bar
at all — a workspace with no tab has no slot to vacate.

This is the roster-side half of the deprio close (`SPC o C'): the roster
owns the tab order, so the shuffle is applied HERE and the persp-mode
names cache is brought in line by the caller
\=`agent-repl-workspace-push-to-back\=' (panels.el).  The next accepted
roster push re-derives the order from the daemon's walk, exactly as it
does for every other local paint."
  (when (and ws (member ws agent-repl-roster--tab-order))
    (let ((reordered (append (remove ws agent-repl-roster--tab-order)
                             (list ws))))
      (setq agent-repl-roster--tab-order reordered)
      (agent-repl--log ws "elisp.roster.tab-to-back: ws=%s order=%S" ws reordered)
      reordered)))

;;;; ---- The current workspace (R8) ---------------------------------------

(defun agent-repl-roster--current-id (roster)
  "Return ROSTER's selected workspace id, or nil when there is none."
  (plist-get (plist-get (plist-get roster :current) :workspace) :id))

(defun agent-repl-roster-react-to-current (roster)
  "Switch tabs when ROSTER names a `current' Emacs did not originate (R8).
A sidebar row click or a merge-queue entry click in the webapp calls
SelectWorkspace, and the roster's new `current' is how that reaches
Emacs — there is no daemon-to-host command loop.  The resulting
SelectWorkspace from Emacs's own switch is idempotent, so no loop
forms.  Returns the workspace switched to, or nil.

A RE-REGISTRATION IS THE ONE TIME `current' IS NOT A REQUEST.  A daemon
that just relaunched stamps `current' on whichever workspace Emacs
re-registered first — a walk order, not a click — and Emacs is at that
moment re-asserting the selection the user actually made.  Following the
stamp there took the frame to the other workspace's magit buffer.  So
while `agent-repl-host-reselect-pending' stands, nothing here moves the
frame; host.el clears it when its re-select is acknowledged."
  (let* ((id (agent-repl-roster--current-id roster))
         (row-ws (and id (agent-repl--ws-by-ref-id id)))
         (row-scope (if row-ws
                        row-ws
                      '(:agent-repl-central
                        "a selected roster row without a tab has no workspace sink"))))
    (cond
     ((null id)
      (agent-repl--log '(:agent-repl-central "the roster names no selected workspace")
                       "elisp.roster.current: none")
      nil)
     ((bound-and-true-p agent-repl-host-reselect-pending)
      (agent-repl--log row-scope "elisp.roster.current: relink-pending id=%s dir=%s"
                       id agent-repl-host-reselect-pending)
      nil)
     ((equal id (and (boundp 'agent-repl-host-last-selected-id)
                     agent-repl-host-last-selected-id))
      (agent-repl--log row-scope "elisp.roster.current: ours id=%s" id)
      nil)
     (t
      (let* ((name (agent-repl--ws-by-ref-id id))
             (selected (agent-repl--ws-current-name)))
        (cond
         ((null name)
          (agent-repl--log '(:agent-repl-central
                             "a selected roster row without a tab has no workspace sink")
                           "elisp.roster.current: no tab id=%s" id)
          nil)
         ((equal name selected)
          (agent-repl--log name "elisp.roster.current: already selected ws=%s" name)
          nil)
         (t
          (agent-repl--info name "elisp.roster.current: switching ws=%s id=%s" name id)
          (agent-repl--ws-switch name)
          name)))))))

;;;; ---- The finish edge --------------------------------------------------

(defun agent-repl-roster--finish-edge-p (previous current)
  "Return non-nil when PREVIOUS to CURRENT is the finish edge.
Both halves are required: a first sighting (PREVIOUS nil) is not an
edge, and neither is a move within the running set."
  (and (memq previous agent-repl-roster-running-statuses)
       (memq current agent-repl-roster-settled-statuses)
       t))

(defun agent-repl-roster--walk-status-transitions (roster fn)
  "Call FN once per row of ROSTER with that row\='s status transition.

FN receives a plist `(:row ROW :id ID :ws WS :scope SCOPE :previous
PREVIOUS :current CURRENT)\=', where PREVIOUS is the arm the last accepted
push carried for the row (nil on a first sighting), WS is the tab\='s
workspace or nil when no tab exists for the id yet, and SCOPE is the log
sink to attribute a record about the row to.

THE ONE PLACE A PUSH IS COMPARED AGAINST THE ONE BEFORE IT.  Both
readers of that comparison — the FINISH EDGE and the STATUS CHANGE —
walk through here, so there is one answer to \"what changed in this
push\" rather than two tables drifting apart.  It must run BEFORE
`agent-repl-roster--record-statuses\=', which is what makes this push\='s
arms the next push\='s PREVIOUS."
  (dolist (entry (agent-repl-roster-walk roster))
    (let* ((row (plist-get entry :row))
           (id (agent-repl-roster-row-id row))
           (current (agent-repl-roster-row-status row))
           (previous (gethash id agent-repl-roster--status-by-id))
           (ws (agent-repl--ws-by-ref-id id))
           (scope (if ws
                      ws
                    '(:agent-repl-central
                      "a roster row without a tab has no workspace sink"))))
      (funcall fn (list :row row :id id :ws ws :scope scope
                        :previous previous :current current)))))

(defun agent-repl-roster--run-finish-edges (roster)
  "Run `agent-repl-roster-finish-functions' for every finish edge in ROSTER.
Returns the workspaces whose rows crossed the edge."
  (let ((fired nil))
    (agent-repl-roster--walk-status-transitions
     roster
     (lambda (transition)
       (let ((id (plist-get transition :id))
             (ws (plist-get transition :ws))
             (scope (plist-get transition :scope))
             (previous (plist-get transition :previous))
             (current (plist-get transition :current)))
         (if (agent-repl-roster--finish-edge-p previous current)
             (if ws
                 (progn
                   (agent-repl--info ws "elisp.roster.finish-edge: ws=%s from=%s to=%s"
                                     ws previous current)
                   (push ws fired)
                   (run-hook-with-args 'agent-repl-roster-finish-functions ws))
               (agent-repl--log scope "elisp.roster.finish-edge: no tab id=%s from=%s to=%s"
                                id previous current))
           (agent-repl--log scope "elisp.roster.status: id=%s from=%s to=%s edge=nil"
                            id previous current)))))
    (nreverse fired)))

(defun agent-repl-roster--run-status-changes (roster)
  "Run `agent-repl-roster-status-change-functions' for every changed arm.

A CHANGE is a row whose arm differs from the arm the last accepted push
carried for it.  A FIRST SIGHTING is not a change — there is no previous
status for the row to have moved away from — which is exactly the rule
the daemon applies to the same edge, so the two cannot disagree about
what counts as new activity.

Returns the workspaces whose status changed."
  (let ((changed nil))
    (agent-repl-roster--walk-status-transitions
     roster
     (lambda (transition)
       (let ((ws (plist-get transition :ws))
             (previous (plist-get transition :previous))
             (current (plist-get transition :current)))
         (when (and ws previous (not (eq previous current)))
           (push ws changed)
           (run-hook-with-args 'agent-repl-roster-status-change-functions
                               ws previous current)))))
    (nreverse changed)))

(defun agent-repl-roster--run-viewed-clears (entries)
  "Run `agent-repl-roster-viewed-cleared-functions' per cleared viewed marker.
A CLEAR is a row of ENTRIES that carried the marker in the previous
accepted push and does not carry it now.  Only rows with a tab fire.
Returns the workspaces whose marker cleared."
  (let ((cleared nil))
    (dolist (entry entries)
      (let* ((row (plist-get entry :row))
             (id (agent-repl-roster-row-id row)))
        (when (and (gethash id agent-repl-roster--viewed-by-id)
                   (not (agent-repl-roster-row-viewed-p row)))
          (let ((ws (agent-repl--ws-by-ref-id id)))
            (if (not ws)
                (agent-repl--log '(:agent-repl-central "a roster push spans every workspace")
                                 "elisp.roster.viewed-cleared: no tab id=%s" id)
              (agent-repl--log ws "elisp.roster.viewed-cleared: ws=%s id=%s" ws id)
              (push ws cleared)
              (run-hook-with-args 'agent-repl-roster-viewed-cleared-functions ws))))))
    (nreverse cleared)))

(defun agent-repl-roster--record-viewed (entries)
  "Replace the previous-viewed table from ENTRIES, keyed by ref id."
  (clrhash agent-repl-roster--viewed-by-id)
  (dolist (entry entries)
    (let ((row (plist-get entry :row)))
      (puthash (agent-repl-roster-row-id row)
               (agent-repl-roster-row-viewed-p row)
               agent-repl-roster--viewed-by-id))))

;;;; ---- The reactions ----------------------------------------------------

(defun agent-repl-roster-notify-finished (ws)
  "Reaction (1): the unfocused desktop banner for WS.
The focus test and the per-workspace debounce both live in
`agent-repl--maybe-notify-finished' — Emacs owns this presentation
policy because Emacs is the only process that knows whether it is
focused."
  (agent-repl--maybe-notify-finished ws))

(defun agent-repl-roster-echo-finished (ws)
  "Reaction (2): the cross-workspace echo for WS.
Only when WS is NOT the selected tab: a user standing in the workspace
already has the footer's activity line telling them."
  (if (agent-repl--current-ws-p ws)
      (agent-repl--log ws "elisp.roster.echo: skipped ws=%s reason=selected" ws)
    (agent-repl--log ws "elisp.roster.echo: ws=%s" ws)
    (message "Agent finished in workspace: %s" ws)))

(defun agent-repl-roster-refresh-magit (ws)
  "Reaction (3): refresh any magit-status buffer on WS's directory.
The turn ended, so the worktree the user is looking at has probably
changed underneath their status buffer."
  (agent-repl--refresh-magit-status-for-dir (agent-repl--ws-get ws :dir) ws))

(add-hook 'agent-repl-roster-finish-functions #'agent-repl-roster-notify-finished)
(add-hook 'agent-repl-roster-finish-functions #'agent-repl-roster-echo-finished)
(add-hook 'agent-repl-roster-finish-functions #'agent-repl-roster-refresh-magit)

;;;; ---- The push ---------------------------------------------------------

(defun agent-repl-roster--index (entries)
  "Replace the row index from ENTRIES, keyed by ref id."
  (clrhash agent-repl-roster--rows-by-id)
  (dolist (entry entries)
    (let ((row (plist-get entry :row)))
      (puthash (agent-repl-roster-row-id row) row agent-repl-roster--rows-by-id))))

(defun agent-repl-roster--record-statuses (entries)
  "Replace the previous-status table from ENTRIES, keyed by ref id."
  (clrhash agent-repl-roster--status-by-id)
  (dolist (entry entries)
    (let ((row (plist-get entry :row)))
      (puthash (agent-repl-roster-row-id row)
               (agent-repl-roster-row-status row)
               agent-repl-roster--status-by-id))))

(defun agent-repl-roster-apply (roster)
  "Apply ROSTER: index it, reconcile the tabs, fire the edges, run the hooks.
A push that DECODES but violates an invariant this file relies on — two
rows sharing one ref id — is logged at ERROR and DROPPED whole: the tab
identity would be ambiguous, and half-applying it would be worse than
ignoring it.  Malformed pushes never reach here at all; rpc.el drops
those.  Returns the tab names in order, or nil when the push was
dropped."
  (let* ((entries (agent-repl-roster-walk roster))
         (duplicate (agent-repl-roster--duplicate-id entries)))
    (if duplicate
        (progn
          (agent-repl--error '(:agent-repl-central
                               "duplicate roster identity is ambiguous across workspaces")
                             "elisp.roster.push: dropped reason=duplicate-ref-id id=%s rows=%d"
                             duplicate (length entries))
          nil)
      (agent-repl-roster--index entries)
      (setq agent-repl-roster-view roster)
      (let ((order (agent-repl-roster-reconcile roster)))
        (agent-repl-roster--run-finish-edges roster)
        ;; BEFORE the statuses are recorded: this compares against the arms
        ;; the LAST push left behind, which the record below replaces.
        (agent-repl-roster--run-status-changes roster)
        (agent-repl-roster--record-statuses entries)
        ;; Same rule for the viewed marker: edge first, then record.
        (agent-repl-roster--run-viewed-clears entries)
        (agent-repl-roster--record-viewed entries)
        (agent-repl-roster-react-to-current roster)
        (run-hook-with-args 'agent-repl-roster-update-functions roster)
        (agent-repl--log '(:agent-repl-central "a roster push spans every workspace")
                         "elisp.roster.push: applied rows=%d tabs=%d"
                         (length entries) (length order))
        order))))

(defun agent-repl-roster-on-push (push)
  "Handle one decoded WatchWorkspaceRoster PUSH."
  (let ((request-id (agent-repl--next-log-request-id)))
    (agent-repl--with-log-context
     agent-repl--global-log-scope request-id
     (lambda () (agent-repl-roster-apply (plist-get push :roster))))))

(defvar agent-repl-roster--accepted nil
  "The roster stream whose subscription the daemon ACCEPTED, or nil.
Only a stream that was accepted and then lost is re-subscribed at once:
one that died before its acceptance met a daemon that is not answering,
and re-dialing it from its own close would loop until the link\='s own
edge said so.")

(defun agent-repl-roster-on-close (reason &optional stream)
  "Handle the roster STREAM closing for REASON.
A cancel is Emacs's own graceful close.  Anything else is the producer
dropping a STANDING stream: it is recorded at ERROR, and the roster
FOLLOWS THE LIVE DAEMON (`agent-repl-roster--follow-live-daemon').

A close of a stream that is no longer the standing one is STALE: a
re-subscription already replaced it, and forgetting the stream that
stands now would leave the tabs with no roster at all.  STREAM nil means
the caller did not name the stream."
  (cond
   ((and stream (not (eq stream agent-repl-roster--stream)))
    (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                     "elisp.roster.stale-stream-close: reason=%S" reason))
   (t
    (setq agent-repl-roster--stream nil)
    (pcase (car-safe reason)
      (:cancelled (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                                    "elisp.roster.stream-close: reason=cancelled"))
      (:ended (agent-repl--error '(:agent-repl-central "the roster stream spans workspaces")
                                  "elisp.roster.stream-close: reason=ended-without-cancel — a standing stream the producer ended")
              (agent-repl-roster--follow-live-daemon stream))
      (_ (agent-repl--error '(:agent-repl-central "the roster stream spans workspaces")
                             "elisp.roster.stream-close: reason=%S" reason)
         (agent-repl-roster--follow-live-daemon stream))))))

(defun agent-repl-roster--follow-live-daemon (lost)
  "Re-subscribe the roster on the LIVE daemon after the stream LOST died.
Regression, 2026-09-27: a daemon exited under a standing roster stream
and nothing re-subscribed it unless the link itself went down or was
promoted -- the roster was left to those edges alone.  The live daemon is
the link\='s (`agent-repl-link-live'), never an address this file keeps:

  - no link stands: the link-up edge subscribes the roster
    (`agent-repl-roster-on-link-up'), and nothing polls meanwhile;
  - LOST was never accepted: the daemon it dialed is not answering, and
    the link\='s own edge (down then up, or a promotion) re-subscribes;
  - otherwise the roster is re-subscribed on the live daemon now."
  (let ((live (and (fboundp 'agent-repl-link-live) (agent-repl-link-live))))
    (cond
     ((null live)
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribe-awaiting-link"))
     ((not (and lost (eq lost agent-repl-roster--accepted)))
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribe-awaiting-link-edge reason=never-accepted address=%S"
                        (agent-repl-connect-connection-address live)))
     (t
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribed-on-loss address=%S"
                        (agent-repl-connect-connection-address live))
      (agent-repl-roster-subscribe live)))))

;;;; ---- The subscription -------------------------------------------------

(defun agent-repl-roster-subscribe (conn)
  "Subscribe to the roster on CONN, replacing any stream already standing.
SUBSCRIBED IS AN ACCEPTANCE: `elisp.roster.subscribed' is written from the
transport's ON-OPEN — the daemon's HTTP 200 header block — and not from
the spawn, because a roster stream that was never accepted will never
deliver the tabs."
  (when agent-repl-roster--stream
    (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                     "elisp.roster.subscribe: cancelling prior stream")
    (agent-repl-connect-stream-cancel agent-repl-roster--stream)
    (setq agent-repl-roster--stream nil))
  ;; THE CLOSE AND THE ACCEPTANCE NAME THEIR OWN STREAM, so a close that
  ;; lands after a re-subscription is recognized as stale.
  (let (stream)
    (setq stream
          (agent-repl-rpc-watch-workspace-roster
           conn #'agent-repl-roster-on-push
           (lambda (reason) (agent-repl-roster-on-close reason stream))
           (lambda ()
             (setq agent-repl-roster--accepted stream)
             (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                               "elisp.roster.subscribed method=%S address=%S"
                               "WatchWorkspaceRoster"
                               (agent-repl-connect-connection-address conn)))))
    (setq agent-repl-roster--stream stream))
  (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                    "elisp.roster.subscribe: opened")
  agent-repl-roster--stream)

(defun agent-repl-roster-on-link-up (conn)
  "Subscribe on CONN when the daemon link comes up.
Registered on `agent-repl-link-up-functions', which runs again after a
link-down/up, so the re-subscription is the same act as the first."
  (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                   "elisp.roster.link-up: subscribing")
  (agent-repl-roster-subscribe conn))

(defun agent-repl-roster-on-link-down (_conn)
  "Forget the roster stream when the link goes down.
The last view is KEPT: it is the newest thing anyone knows, and blanking
the tab bar on a reconnect would be a worse lie than a stale paint."
  (setq agent-repl-roster--stream nil)
  (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                    "elisp.roster.link-down: stream forgotten view-kept=%s"
                    (if agent-repl-roster-view "t" "nil")))

(defun agent-repl-roster-on-link-promote (_old new)
  "Re-subscribe the roster on NEW when the successor is PROMOTED to primary.
A promotion fires no up hooks -- by design, because every workspace was
already adopted onto the successor and re-registering the fleet would be
a lie.  The ROSTER is not adopted, though: its stream rode the OLD
connection and dies with it, so without this re-subscription Emacs comes
out of every blue-green rollout with no roster stream at all and the tabs
stop reconciling."
  (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                    "elisp.roster.resubscribed-on-promotion address=%S"
                    (agent-repl-connect-connection-address new))
  (agent-repl-roster-subscribe new))

(add-hook 'agent-repl-link-up-functions #'agent-repl-roster-on-link-up)
(add-hook 'agent-repl-link-promote-functions #'agent-repl-roster-on-link-promote)
(add-hook 'agent-repl-link-down-functions #'agent-repl-roster-on-link-down)

(provide 'agent-repl-roster)
;;; roster.el ends here
