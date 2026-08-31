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
(declare-function agent-repl--current-ws-p "core" (ws))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-rpc-watch-workspace-roster "rpc"
                  (conn on-push on-close &optional on-open))
(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl--ws-live-p "workspace" (ws))
(declare-function agent-repl--live-ws-names "workspace" ())
(declare-function agent-repl--ws-get "workspace" (ws key))
(declare-function agent-repl--ws-put "workspace" (ws key val))
(declare-function agent-repl--ws-create "workspace" (ws &optional project-dir))
(declare-function agent-repl--ws-del "workspace" (ws))
(declare-function agent-repl--ws-persp-kill "workspace" (ws))
(declare-function agent-repl--ws-rename-state "workspace" (old new dir))
(declare-function agent-repl--ws-rename-persp "workspace" (old new))
(declare-function agent-repl--ws-switch "workspace" (ws &rest args))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--refresh-magit-status-for-dir "session" (dir &optional ws))
(declare-function agent-repl--maybe-notify-finished "session" (ws))
;; W2-A's names (host.el, daemon-link.el).  Declared, never defined here.
(declare-function agent-repl-host-subscribe "host" (conn ws ref))
(declare-function agent-repl-host-unsubscribe "host" (ws))
(declare-function agent-repl-link-primary "daemon-link" ())
(defvar agent-repl-host-last-selected-id)
(defvar agent-repl-link-up-functions)
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

(defun agent-repl-roster-row-dir (row)
  "Return ROW's workspace directory — display and file-opening only."
  (plist-get (agent-repl-roster-row-ref row) :dir))

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
merged, closed and killed rows, so this is the whole membership rule."
  (let* ((entries (cl-remove-if (lambda (entry)
                                  (agent-repl-roster-row-closed-p (plist-get entry :row)))
                                (agent-repl-roster-walk roster)))
         (collisions (agent-repl-roster--colliding-names entries)))
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
    (agent-repl--ws-create name dir)
    (agent-repl--ws-put name :ref ref)
    (agent-repl--ws-put name :dir dir)
    (agent-repl--info name "elisp.roster.tab-open: ws=%s id=%s dir=%s"
                      name (plist-get desired :id) dir)
    (agent-repl-roster--subscribe-host name ref)
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
  (agent-repl--ws-rename-state old new (plist-get ref :dir))
  (agent-repl--ws-rename-persp old new)
  (agent-repl--ws-put new :ref ref)
  (agent-repl--ws-put new :dir (plist-get ref :dir))
  (agent-repl--info new "elisp.roster.tab-rename: old=%s new=%s id=%s"
                    old new (plist-get ref :id))
  new)

(defun agent-repl-roster--tear-down-tab (name)
  "Tear NAME's tab down: cancel its host stream, kill the persp, tombstone it.
IDEMPOTENT, because CloseWorkspace's own success tears the tab down too
and the roster push that follows must not care which of the two got
there first."
  (when (fboundp 'agent-repl-host-unsubscribe)
    (agent-repl-host-unsubscribe name))
  (agent-repl--ws-persp-kill name)
  (agent-repl--ws-del name)
  (agent-repl--info name "elisp.roster.tab-teardown: ws=%s" name))

(defun agent-repl-roster--roster-owned-names ()
  "Return every live workspace this file opened — those carrying a `:ref'.
A workspace with no ref never came from the roster, so reconciliation
leaves it alone rather than tearing down something it does not own."
  (cl-remove-if-not (lambda (name) (agent-repl--ws-get name :ref))
                    (agent-repl--live-ws-names)))

(defun agent-repl-roster-reconcile (roster)
  "Bring the tab bar in line with ROSTER and return the tab names in order.
Opens a tab for each `closed = false' row that has none, renames the tab
whose row's name changed, tears down every roster-owned tab whose row is
gone or closed, and sets the tab ORDER to the walk order strictly."
  (let* ((desired (agent-repl-roster-desired-tabs roster))
         (wanted-ids (make-hash-table :test 'equal))
         (names nil))
    (dolist (want desired)
      (puthash (plist-get want :id) t wanted-ids)
      (let* ((id (plist-get want :id))
             (name (plist-get want :name))
             (existing (agent-repl--ws-by-ref-id id)))
        (cond
         ((null existing)
          (push (agent-repl-roster--open-tab want) names))
         ((equal existing name)
          (agent-repl--ws-put name :ref (plist-get want :ref))
          (agent-repl--log name "elisp.roster.tab-kept: ws=%s id=%s" name id)
          (push name names))
         (t
          (push (agent-repl-roster--rename-tab existing name (plist-get want :ref))
                names)))))
    (dolist (name (agent-repl-roster--roster-owned-names))
      (let ((id (plist-get (agent-repl--ws-get name :ref) :id)))
        (unless (gethash id wanted-ids)
          (agent-repl-roster--tear-down-tab name))))
    (setq agent-repl-roster--tab-order (nreverse names))
    (agent-repl--log nil "elisp.roster.reconcile: tabs=%d order=%S"
                     (length agent-repl-roster--tab-order)
                     agent-repl-roster--tab-order)
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
forms.  Returns the workspace switched to, or nil."
  (let ((id (agent-repl-roster--current-id roster)))
    (cond
     ((null id)
      (agent-repl--log nil "elisp.roster.current: none")
      nil)
     ((equal id (and (boundp 'agent-repl-host-last-selected-id)
                     agent-repl-host-last-selected-id))
      (agent-repl--log nil "elisp.roster.current: ours id=%s" id)
      nil)
     (t
      (let* ((name (agent-repl--ws-by-ref-id id))
             (selected (agent-repl--ws-current-name)))
        (cond
         ((null name)
          (agent-repl--log nil "elisp.roster.current: no tab id=%s" id)
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

(defun agent-repl-roster--run-finish-edges (roster)
  "Run `agent-repl-roster-finish-functions' for every finish edge in ROSTER.
Returns the workspaces whose rows crossed the edge."
  (let ((fired nil))
    (dolist (entry (agent-repl-roster-walk roster))
      (let* ((row (plist-get entry :row))
             (id (agent-repl-roster-row-id row))
             (current (agent-repl-roster-row-status row))
             (previous (gethash id agent-repl-roster--status-by-id)))
        (if (agent-repl-roster--finish-edge-p previous current)
            (let ((ws (agent-repl--ws-by-ref-id id)))
              (if ws
                  (progn
                    (agent-repl--info ws "elisp.roster.finish-edge: ws=%s from=%s to=%s"
                                      ws previous current)
                    (push ws fired)
                    (run-hook-with-args 'agent-repl-roster-finish-functions ws))
                (agent-repl--log nil "elisp.roster.finish-edge: no tab id=%s from=%s to=%s"
                                 id previous current)))
          (agent-repl--log nil "elisp.roster.status: id=%s from=%s to=%s edge=nil"
                           id previous current))))
    (nreverse fired)))

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
          (agent-repl--error nil "elisp.roster.push: dropped reason=duplicate-ref-id id=%s rows=%d"
                             duplicate (length entries))
          nil)
      (agent-repl-roster--index entries)
      (setq agent-repl-roster-view roster)
      (let ((order (agent-repl-roster-reconcile roster)))
        (agent-repl-roster--run-finish-edges roster)
        (agent-repl-roster--record-statuses entries)
        (agent-repl-roster-react-to-current roster)
        (run-hook-with-args 'agent-repl-roster-update-functions roster)
        (agent-repl--log nil "elisp.roster.push: applied rows=%d tabs=%d"
                         (length entries) (length order))
        order))))

(defun agent-repl-roster-on-push (push)
  "Handle one decoded WatchWorkspaceRoster PUSH."
  (agent-repl-roster-apply (plist-get push :roster)))

(defun agent-repl-roster-on-close (reason)
  "Handle the roster stream closing for REASON.
A cancel is Emacs's own graceful close; anything else is the link going
down, and the reconnect is daemon-link.el's — this file only forgets the
stream so the next link-up subscribes a fresh one."
  (setq agent-repl-roster--stream nil)
  (pcase (car-safe reason)
    (:cancelled (agent-repl--log nil "elisp.roster.stream-close: reason=cancelled"))
    (:ended (agent-repl--error nil "elisp.roster.stream-close: reason=ended-without-cancel — a standing stream the producer ended"))
    (_ (agent-repl--error nil "elisp.roster.stream-close: reason=%S" reason))))

;;;; ---- The subscription -------------------------------------------------

(defun agent-repl-roster-subscribe (conn)
  "Subscribe to the roster on CONN, replacing any stream already standing.
SUBSCRIBED IS AN ACCEPTANCE: `elisp.roster.subscribed' is written from the
transport's ON-OPEN — the daemon's HTTP 200 header block — and not from
the spawn, because a roster stream that was never accepted will never
deliver the tabs."
  (when agent-repl-roster--stream
    (agent-repl--log nil "elisp.roster.subscribe: cancelling prior stream")
    (agent-repl-connect-stream-cancel agent-repl-roster--stream)
    (setq agent-repl-roster--stream nil))
  (setq agent-repl-roster--stream
        (agent-repl-rpc-watch-workspace-roster
         conn #'agent-repl-roster-on-push #'agent-repl-roster-on-close
         (lambda ()
           (agent-repl--info nil "elisp.roster.subscribed method=%S address=%S"
                             "WatchWorkspaceRoster"
                             (agent-repl-connect-connection-address conn)))))
  (agent-repl--info nil "elisp.roster.subscribe: opened")
  agent-repl-roster--stream)

(defun agent-repl-roster-unsubscribe ()
  "Cancel the roster stream.  Cancelling IS the graceful close."
  (if agent-repl-roster--stream
      (progn
        (agent-repl-connect-stream-cancel agent-repl-roster--stream)
        (setq agent-repl-roster--stream nil)
        (agent-repl--info nil "elisp.roster.unsubscribe: cancelled"))
    (agent-repl--log nil "elisp.roster.unsubscribe: no stream")))

(defun agent-repl-roster-on-link-up (conn)
  "Subscribe on CONN when the daemon link comes up.
Registered on `agent-repl-link-up-functions', which runs again after a
link-down/up, so the re-subscription is the same act as the first."
  (agent-repl--log nil "elisp.roster.link-up: subscribing")
  (agent-repl-roster-subscribe conn))

(defun agent-repl-roster-on-link-down (_conn)
  "Forget the roster stream when the link goes down.
The last view is KEPT: it is the newest thing anyone knows, and blanking
the tab bar on a reconnect would be a worse lie than a stale paint."
  (setq agent-repl-roster--stream nil)
  (agent-repl--info nil "elisp.roster.link-down: stream forgotten view-kept=%s"
                    (if agent-repl-roster-view "t" "nil")))

(add-hook 'agent-repl-link-up-functions #'agent-repl-roster-on-link-up)
(add-hook 'agent-repl-link-down-functions #'agent-repl-roster-on-link-down)

(provide 'agent-repl-roster)
;;; roster.el ends here
