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
;; - THE STREAMS: each push (`frontend.v1.RosterPush') carries the roster's
;;   four snapshot streams -- the workspaces, the merge queue, the Recently
;;   Merged summary and the closed rows -- each WHOLE when it changed and
;;   absent when it did not.  The latest snapshot of each is held
;;   (`agent-repl-roster--streams'); a subscription's first push carries
;;   every one, and a first push missing one is refused;
;; - `agent-repl-roster-view', the streams ASSEMBLED into one plist, and
;;   `agent-repl-roster-update-functions', run after every accepted push;
;; - THE MERGED PAGES: a merged workspace is in no stream, so a reader that
;;   needs one pages `ListMergedWorkspaces' (`agent-repl-roster-merged-entries-sync',
;;   `agent-repl-roster-merged-entries');
;; - TAB RECONCILIATION from the rows' `closed' flag: a row with
;;   `closed = false' has a tab, one with `closed = true' has none, and the
;;   tab order is the roster's walk order strictly (repository sections in
;;   order, rows depth-first, then the hidden rows, then the closed rows).
;;   The task view is IGNORED: it is the same rows regrouped, and drawing
;;   both would double every tab;
;; - TAB IDENTITY IS THE REF ID.  A row whose id matches an existing tab is
;;   the same workspace whatever its name, so a renamed row RENAMES its tab
;;   rather than opening a second one.  Names collide across repos; ids do
;;   not, so a duplicated display name is disambiguated with "·<repo label>";
;; - the R8 CURRENT-CHANGE reaction: a `current' Emacs did not originate is a
;;   tab-switch request (re-selection is idempotent, so no loop forms);
;; - THE FINISH EDGE — a row moving from a RUNNING status to a SETTLED one —
;;   and the two Emacs-local reactions that ride it.  A deferred prompt
;;   (`SPC j RET') is NOT one of them: the daemon holds it and runs it as
;;   its own turn when the running one ends (prompt-queue.el).
;;
;; WHAT THIS FILE DOES NOT OWN: registration (the roster's rows are already
;; registered daemon-side; Register is host.el's verb on Emacs's own
;; worktrees), the paint itself (status.el composes it from the status arm),
;; and the per-workspace host stream (host.el), which this file only asks to
;; start and stop as tabs come and go.

;;; Code:

;; Every log call is a log-site macro (log-site.el), needed where this
;; file is expanded; config.el loads it first.
(require 'log-site)

(require 'cl-lib)

(declare-function agent-repl--next-log-request-id "core" ())
(declare-function agent-repl--with-log-context "core"
                  (workspace request-id function))
(defvar agent-repl--global-log-scope)
(defvar agent-repl--recency-walk-origin)
(defvar agent-repl--panels-arrivals-armed)
(defvar agent-repl--log-context-request-id)
(defvar agent-repl--log-context-workspace)
(declare-function agent-repl--current-ws-p "core" (ws))
(declare-function agent-repl-connect-connection-address "connect" (conn))
(declare-function agent-repl-rpc-watch-workspace-roster "rpc"
                  (conn on-push on-close &optional on-open))
(declare-function agent-repl-connect-stream-cancel "connect" (stream))
(declare-function agent-repl-link-live "daemon-link" ())
(declare-function agent-repl-link-departing-p "daemon-link" (live &optional ended-planned-on))
(declare-function agent-repl--ws-current-name "workspace" ())
(declare-function agent-repl--ws-list-names "workspace" ())
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl--ws-tombstoned-p "workspace" (ws))
(declare-function agent-repl--ws-forget "workspace" (ws))
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
(declare-function agent-repl--when-minibuffer-free "workspace" (scope label key fn))
(declare-function agent-repl--ws-by-ref-id "workspace" (id))
(declare-function agent-repl--ws-ref-id-table "workspace" ())
(declare-function agent-repl--ws-log-name "workspace" (ws))
(declare-function agent-repl--refresh-magit-status-for-dir "session" (dir &optional ws))
;; W2-A's names (host.el, daemon-link.el).  Declared, never defined here.
(declare-function agent-repl--panels-open-on-arrival "panels" (ws id))
(declare-function agent-repl--panels-arm-arrivals "panels" ())
(declare-function agent-repl-host-subscribe "host" (conn ws ref))
(declare-function agent-repl-host-rename "host" (old new))
(declare-function agent-repl--frontend-rename-webview "frontend" (ws))
(declare-function agent-repl-host--apply-naming "host" (ws))
(declare-function agent-repl-link-primary "daemon-link" ())
(defvar agent-repl-host-reselect-pending)
(defvar agent-repl-link-up-functions)
(defvar agent-repl-link-promote-functions)
(defvar agent-repl-link-down-functions)

;;;; ---- The view ---------------------------------------------------------

(declare-function agent-repl-open-tabs-holds-p "open-tabs" (id))
(declare-function agent-repl-rpc-list-merged-workspaces "rpc"
                  (conn request &key on-response on-failure timeout))
(declare-function agent-repl-rpc-list-merged-workspaces-sync "rpc"
                  (conn request &optional timeout))
(declare-function agent-repl-open-tabs-reconcile "open-tabs" (rows))
(declare-function agent-repl--force-tab-bar-redraw "status" ())

(defvar agent-repl-roster-view nil
  "The last accepted roster, assembled from its streams, or nil before one.
ONE PLIST, so its readers read one roster: the WORKSPACES stream\='s keys
\(`agent-repl-wire-decode-workspace-roster'), plus `:closed' (the closed
stream), `:merge-queue' (the merge queue stream), `:recently-merged' (the
Recently Merged summary, never a row) and `:revision', the PUSH\='s
revision.  Every stream is a whole snapshot, never a delta, and a push
REPLACES this value outright with the streams it carries and the ones the
last push left unchanged (`agent-repl-roster-apply-push').")

(defconst agent-repl-roster-stream-keys
  '(:workspaces :merge-queue :recently-merged :closed)
  "The roster\='s four snapshot streams, as `RosterPush' decodes their keys.")

(defvar agent-repl-roster--streams nil
  "The latest snapshot of each roster stream, or nil before any push.
A plist `(:stream S :revision R :workspaces W :merge-queue Q
:recently-merged M :closed C)': S is the subscription the snapshots came
on, and a push on any OTHER subscription is that subscription\='s first,
which must carry every stream (`agent-repl-roster-apply-push').  It
mirrors what the daemon holds this Emacs was sent, so it is replaced on
every complete push, also one whose assembled view an invariant then
drops: the next push leaves out exactly the streams it already sent.")

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

The daemon is the single source of the viewed (partial) mode: it derives
`RosterRowViewed' from the workspace's read-result fact, which a reported
dwell on a turn-end row (done, interrupted or turn-failed) sets and the next
turn resets, and it carries the marker only while the row stands on a
turn-end arm.  This hook fires on the present->absent edge of that marker,
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

(defvar agent-repl-roster--stream-conn nil
  "The connection the standing roster stream rides, or nil when unsubscribed.
Read when that stream ends PLANNED: a planned end on the connection the
link still names is its daemon leaving, and the roster waits for the
link\='s own edge rather than re-subscribing on it.")

(defvar agent-repl-roster--view-stream nil
  "The roster stream that served `agent-repl-roster-view', or nil.
A view is the daemon's word only while the stream that served it stands
\(`agent-repl-roster--held-revision').  A re-subscription keeps the view
\(blanking the tabs would be worse than a stale paint) but NOT its
authority: its revision and its `current' are the last daemon's, and the
new stream's revisions start afresh.  On 2026-10-20 a rollout's new daemon
pushed a field this Emacs refused, every one of its pushes was dropped,
and the old daemon's view, at a revision past the new daemon's, released
each answered selection at once and moved the frame back to the
workspace the user had just left.")

(defvar agent-repl-roster--stream-addresses (make-hash-table :test 'eq :weakness 'key)
  "Roster stream -> the address of the daemon it rides.
Written when the daemon accepts the subscription, and read to say
which daemon\='s roster a tab was opened or torn down by
\(`agent-repl-roster--view-address').")

(defvar agent-repl-roster--view-address nil
  "The address of the daemon that served `agent-repl-roster-view', or nil.
Nil when the stream was never subscribed here, as a batch test\='s is.")

(defvar agent-repl-roster--placed nil
  "Where the last reconciled roster placed every row, or nil before the first.
A plist `(:rows ROWS :address A :revision R)': ROWS the roster\='s
`agent-repl-roster--placements', A and R the daemon that served it and its
revision.  The next reconcile reads it as the state BEFORE its roster, so a
tab it opens or tears down names the row\='s section and visibility on both
sides and the roster on each side (`agent-repl-roster--decision').")

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

(defvar agent-repl-roster--walked nil
  "The last reconcile's tabs in walk order, as `(NAME . REF-ID)' pairs.
The tab order is these less the tabs still held for their open (open-tabs.el).")

(defvar agent-repl-roster--walked-hidden nil
  "The last reconcile's collapsed-repository tab names.")

(defvar agent-repl-roster--reconciled nil
  "The inputs of the last full reconcile walk, as `(DESIRED . LISTED-OPEN)'.
DESIRED is the walk's desired rows, LISTED-OPEN the sorted ids the roster
listed open.  A push whose inputs are these, while every tab they want
stands under its name and no roster-owned tab is owed a teardown, changes
nothing the walk would do, so the walk is skipped
(`agent-repl-roster--walk-unchanged-p').")

(defvar agent-repl-roster--tab-order nil
  "Workspace names in roster walk order — every workspace with a tab.
No local ordering exists: the resolver orders the roster (priority
included) and the tab bar follows it.  The bar DRAWS the subset
`agent-repl-roster-drawn-tab-order' answers.")

(defvar agent-repl-roster--tab-numbers nil
  "The last reconcile's numbered tabs, as `(NAME . NUMBER)' pairs in walk order.
THE DAEMON NUMBERS EVERY TAB (`frontend.v1.RosterOpenRowTab.numbered'), and
the sidebar paints the same number beside the row, so the bar's numbers and
its slot gestures read this rather than counting the tabs Emacs draws: a
tab held for its open (open-tabs.el) leaves a gap instead of shifting every
tab after it onto a number the sidebar gives another workspace.  A folded,
hidden or thread tab has no number and no pair.")

(defvar agent-repl-roster--hidden-tabs nil
  "Tab names whose repository the daemon holds COLLAPSED, or that are HIDDEN.
They keep their tabs (their sessions are untouched) and are off the
bar's drawing, numbering and navigation.  Set by the reconcile pass from
the same push as `agent-repl-roster--tab-order'.")

;;;; ---- The status vocabulary --------------------------------------------

(defconst agent-repl-roster-running-statuses
  '(:submitting :thinking :vendor-silent :clearing :compacting :permission
    :question :waiting)
  "The RUNNING half of the finish edge — the agent holds the turn.
A permission or question gate is RUNNING: the turn has not ended, it is
waiting on the user, so permission -> thinking is a move within this set
and no finish edge at all.  So is any other wait on the user (`waiting').")

(defconst agent-repl-roster-settled-statuses
  '(:ready :done :interrupted :turn-failed :vendor-blocked :turn-died :idle-async)
  "The SETTLED half of the finish edge — the foreground turn has ended.
`turn-failed' is settled: a turn that FAILED has ended as surely as one
that finished or was stopped.  So are `vendor-blocked' and `turn-died',
the faults a failed turn now stands as until the next one (owner ruling,
2026-10-06): the turn they report has ended.
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

(defun agent-repl-roster-row-availability (row)
  "Return ROW's availability arm keyword.
One of `:pending', `:available' or `:unavailable'.  The daemon resolves
it from the workspace's shim link; a tab is opened for the row only once
it leaves `:pending'."
  (plist-get (plist-get row :availability) :arm))

(defun agent-repl-roster-row-visible-p (row)
  "Return non-nil when this editor holds ROW\='s workspace open.
THE DAEMON DECIDES IT (frontend.v1.RosterRowVisibility,
`wsm.Workspace.Visibility'): ROW is visible or the selection.  Emacs holds a
perspective, a composer, a host stream and a page for exactly these rows,
and works out nothing of its own (owner ruling, 2026-11-21).  ROW is a
workspace row or a thread row: both carry the one visibility."
  (not (eq (plist-get row :visibility) :not-visible)))

(defun agent-repl-roster-row-closed-p (row)
  "Return non-nil when ROW is CLOSED: no tab, whatever its lifecycle."
  (eq (plist-get (plist-get row :closed) :closed) t))

(defun agent-repl-roster-row-closed-by-user-at (row)
  "Return the epoch-ms instant the USER closed ROW, or nil.
Nil for an open row, for one the daemon, an agent or a merge closed, and
for a close that predates the daemon's record (`RosterRowClosure')."
  (and (eq (agent-repl-roster-row-closed-by row) :user)
       (agent-repl-roster-row-closed-at row)))

(defun agent-repl-roster-row-closed-at (row)
  "Return the epoch-ms instant ROW was closed, by any cause, or nil.
Nil for an open row and for a close that predates the daemon's record
\(`RosterRowClosure')."
  (plist-get (plist-get row :closure) :at-ms))

(defun agent-repl-roster-row-closed-by (row)
  "Return who closed ROW: `:user', `:agent', `:merge' or `:daemon', or nil.
Nil for an open row and for a close that predates the daemon's record."
  (plist-get (plist-get (plist-get row :closure) :by) :arm))

(defun agent-repl-roster-row-merged-at (row)
  "Return the epoch-ms instant ROW's merge landed, or nil.
The when-column's `merged' arm carries it, and MERGED WINS that column's
precedence, so every merged row states it."
  (let ((when-column (plist-get row :when)))
    (and (eq (plist-get when-column :arm) :merged)
         (plist-get (plist-get when-column :value) :at-ms))))

(defun agent-repl-roster-row-instant-ms (row field)
  "Return the epoch ms of ROW's instant FIELD, or nil.
FIELD is the row key of one of the row's instants (`:last-selected',
`:last-turn-ended', `:last-turn-boundary', `:last-updated'), each
decoded to `(:at-ms MS)' by the one instant decoder.  nil when ROW is
nil or does not carry FIELD.  The one reader every instant accessor
goes through."
  (plist-get (plist-get row field) :at-ms))

(defun agent-repl-roster-row-commit-count (row)
  "Return how many commits ROW's branch holds past its merge-base, or nil.
Nil when the daemon has no count to state; zero is a real count."
  (plist-get (plist-get (plist-get row :detail) :commits) :count))

(defun agent-repl-roster-row-summary (row)
  "Return ROW's summary text, or nil when the daemon states none.
The workspace's generated summary (`RosterRowDetailSummary'), the same
text the topbar draws over the workspace's name."
  (plist-get (plist-get (plist-get row :detail) :summary) :text))

(defun agent-repl-roster-row-attention-p (row)
  "Return non-nil when ROW carries the attention marker."
  (and (plist-get row :attention) t))

(defun agent-repl-roster-row-name-prefix (row)
  "Return the marks ROW's name wears before it, or nil when it wears none.
A decoded `WorkspaceNamePrefix' (`:priority LEVEL'), drawn by
`agent-repl-name-prefix-string'."
  (plist-get (plist-get row :name) :prefix))

(defun agent-repl-roster-row-viewed-p (row)
  "Return non-nil when ROW carries the viewed (partial) marker."
  (and (plist-get row :viewed) t))

(defun agent-repl-roster-viewed-for-ws (ws)
  "Return the viewed marker of WS's current roster row, or nil.
Nil before any push has carried a row for WS.  This is the ONLY input to
the tab-bar's partial/full decision: the daemon owns the mode."
  (agent-repl-roster--for-ws ws (lambda (row) (plist-get row :viewed))))

(defun agent-repl-roster-row-idle-async-unread (row)
  "Return the turn end ROW\='s unread-turn-end marker names, or nil.
One of `:done', `:interrupted' or `:turn-failed' while ROW is
`idle_async' over an unread result (`RosterRowIdleAsyncUnread'), nil
when it carries no marker."
  (plist-get row :idle-async-unread))

(defun agent-repl-roster-idle-async-unread-for-ws (ws)
  "Return the unread turn end under WS\='s `idle_async' roster row, or nil.
Nil before any push has carried a row for WS, and while the row carries
no unread-turn-end marker.  The tab bar draws WS\='s tab name in that
turn end\='s color from it
\(`agent-repl--tab-name-state')."
  (agent-repl-roster--for-ws ws #'agent-repl-roster-row-idle-async-unread))

(defun agent-repl-roster-row-one-shot (row)
  "Return non-nil when ROW carries the one-shot mark (`RosterRowOneShot').
The mark is presence alone: the workspace was created as a one-shot."
  (plist-get row :one-shot))

(defun agent-repl-roster-one-shot-for-ws (ws)
  "Return non-nil when WS's current roster row is a one-shot's.
Nil before any push has carried a row for WS, and for a workspace that is
not a one-shot.  The tab bar draws its glyph from it while the one-shot
works \(`agent-repl-status-tab-glyph')."
  (agent-repl-roster--for-ws ws #'agent-repl-roster-row-one-shot))

(defun agent-repl-roster-row-async-merging (row)
  "Return non-nil when ROW carries the async-merging marker.
The marker (`RosterRowAsyncMerging\=') is presence alone: the workspace\='s
own branch merges async in a hidden merge workspace."
  (plist-get row :async-merging))

(defun agent-repl-roster-async-merging-for-ws (ws)
  "Return non-nil when WS\='s current roster row carries the async-merging marker.
Nil before any push has carried a row for WS, and while no async merge of
its branch is in flight.  The tab bar paints its [N] run purple from it
\(`agent-repl--tab-spec-async-merge\=')."
  (agent-repl-roster--for-ws ws #'agent-repl-roster-row-async-merging))

;;;; ---- The walk ---------------------------------------------------------

(defun agent-repl-roster--walk-rows (rows label section acc collapsed)
  "Walk the list rows ROWS depth-first under LABEL, pushing entries onto ACC.
ROWS are one list's own row type (`RosterOpenRow', `RosterHiddenRow',
`RosterMergedRow', `RosterClosedRow'), each a plist carrying the shared
`:row' and, for the lists that nest, its family (`agent-repl-roster-row-kin').
Each entry
is `(:row ROW :label LABEL :section SECTION :collapsed C :tab TAB)'.
SECTION names which list the row sits in -- `:repository' (the Open
section), `:hidden', `:merged' or `:closed' -- so a reader that treats them
differently asks the walk rather than re-deriving membership from a row's
own fields.  COLLAPSED is called with each list row and answers whether its
workspace is off the bar's drawing, numbering and navigation.  TAB is an
open row's own tab, nil in the other lists.  A row precedes its children,
which is the render order the contract's nesting states."
  (dolist (wrapped rows)
    (push (list :row (plist-get wrapped :row) :label label :section section
                :collapsed (funcall collapsed wrapped) :tab (plist-get wrapped :tab))
          (car acc))
    (agent-repl-roster--walk-rows (agent-repl-roster-row-kin wrapped) label section acc collapsed))
  acc)

(defun agent-repl-roster-row-kin (wrapped)
  "Return the list row WRAPPED's nested rows, in drawn order.
Its `:children' first, then, for an open row, every repository block's
`:rows', block by block (`frontend.v1.RosterOpenRow.repository_blocks'):
the order the sidebar draws them and the tab bar walks them.  A reader that
took only `:children' would miss every child of another repository."
  (append (plist-get wrapped :children)
          (mapcan (lambda (block) (copy-sequence (plist-get block :rows)))
                  (plist-get wrapped :repository-blocks))))

(defun agent-repl-roster-flatten (rows)
  "Return the shared rows of the list rows ROWS and their families, depth-first.
ROWS are one list's own row type (`agent-repl-roster--walk-rows'); the
answer is the `RosterRow's they carry, each before its family."
  (let ((acc (list nil)))
    (agent-repl-roster--walk-rows rows nil nil acc #'ignore)
    (mapcar (lambda (entry) (plist-get entry :row)) (nreverse (car acc)))))

(defun agent-repl-roster-section-rows (section)
  "Return repository SECTION's top-level open rows in drawn order.
Entry by entry (`frontend.v1.RosterRepoSection.entries'): a row in no
subsection where it stands, a subsection's rows where the subsection
stands.  A reader that took only the rows in no subsection would miss
every workspace in a subsection."
  (mapcan (lambda (entry)
            (let ((arm (plist-get entry :entry)))
              (pcase (plist-get arm :arm)
                (:row (list (plist-get arm :value)))
                (:subsection (copy-sequence
                              (plist-get (plist-get (plist-get arm :value) :open-rows) :rows)))
                (other (error "agent-repl: a repository section entry has no known arm: %S" other)))))
          (plist-get section :entries)))

(defun agent-repl-roster-tab-number (tab)
  "Return the number open row TAB is drawn at, or nil for a folded tab."
  (let ((arm (plist-get tab :tab)))
    (when (eq (plist-get arm :arm) :numbered)
      (plist-get (plist-get arm :value) :number))))

(defun agent-repl-roster--open-row-folded-p (open)
  "Return non-nil when open row OPEN's tab is folded off the bar.
THE DAEMON DECIDES IT (`frontend.v1.RosterOpenRow.tab'): a collapsed
repository's or subsection's rows carry a folded tab."
  (null (agent-repl-roster-tab-number (plist-get open :tab))))

(defun agent-repl-roster-walk (roster)
  "Return ROSTER's rows in tab order, as `agent-repl-roster--walk-rows' entries.
THE ROSTER IS ONE LISTING (`frontend.v1.WorkspaceRoster'): every workspace
sits in exactly one list.  The Open section first -- repository sections in
the resolver's order, each section's rows (its subsections' included,
`agent-repl-roster-section-rows') depth-first, each open row's `:collapsed'
read off its own folded tab -- then the hidden section's rows, then the
closed stream's rows.

THE OPEN SECTION IS THE TAB BAR, so its walk is the bar's order and its
numbers are the bar's (`agent-repl-roster--misnumbered').  A HIDDEN
WORKSPACE IS WALKED AS A FOLDED ONE IS: it keeps an undrawn tab (its session
is untouched, so its perspective stays), `:collapsed' taking it off the
bar's drawing, numbering and navigation by the one hiding machinery there
is.  Closed rows have no tab; they are walked for the readers that reach
every workspace (the switcher, the log retention).

A MERGED WORKSPACE IS IN NO STREAM, so the walk holds none: a reader that
needs merged rows pages them (`agent-repl-roster-merged-entries-sync')."
  (let ((acc (list nil)))
    (dolist (section (plist-get (plist-get roster :repository) :sections))
      (agent-repl-roster--walk-rows
       (agent-repl-roster-section-rows section)
       (plist-get (plist-get (plist-get section :header) :label) :text)
       :repository
       acc
       #'agent-repl-roster--open-row-folded-p))
    (let ((hidden (plist-get roster :hidden)))
      (agent-repl-roster--walk-rows
       (plist-get (plist-get hidden :hidden-rows) :rows)
       (plist-get (plist-get (plist-get hidden :header) :label) :text)
       :hidden
       acc
       (lambda (_) t)))
    (agent-repl-roster--walk-rows
     (plist-get (plist-get roster :closed) :rows) nil :closed acc (lambda (_) t))
    (nreverse (car acc))))

(defun agent-repl-roster--section-holds-id-p (section id)
  "Return non-nil when SECTION's rows include the workspace ID."
  (seq-some (lambda (row) (equal (agent-repl-roster-row-id row) id))
            (agent-repl-roster-flatten (agent-repl-roster-section-rows section))))

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

(defun agent-repl-roster-ws-repository (ws)
  "Return the `RepositoryRef' of the section holding workspace WS, or nil.
WS's daemon ref names the row, and `agent-repl-roster-repository-of'
the section holding it: the ONE lookup every \"this workspace's
repository\" reader in Emacs shares.  Nil when WS is nil or no workspace
\(a persp-mode placeholder), or when no section holds it yet."
  (let ((id (and ws (plist-get (agent-repl--ws-get ws :ref) :id))))
    (and id (agent-repl-roster-repository-of id))))

;;;; ---- The merged pages ------------------------------------------------
;;
;; A MERGED WORKSPACE IS IN NO STREAM (`frontend.v1.RosterPush'): it is
;; finished, and the roster carries only the Recently Merged summary.  A
;; reader that needs merged workspaces -- the switcher, a name lookup, the
;; log retention -- pages `ListMergedWorkspaces' across every repository,
;; most recently merged first, ten to a page.  Every page states the merged
;; set it was resolved from; a walk whose pages disagree was read across a
;; change of the set, and is read again from its first page.

(define-error 'agent-repl-roster-merged-error
  "agent-repl: the merged workspaces could not be listed")

(defconst agent-repl-roster-merged-page-size 10
  "How many merged workspaces one `ListMergedWorkspaces' page holds.
Fixed by the daemon (endpoint_list_merged_workspaces.proto).")

(defconst agent-repl-roster-merged-walk-attempts 2
  "How many times a merged walk is read from its first page.
A second read is for a set that changed between two pages of the first;
a set that changes under both is a failure, surfaced, never a mixed list.")

(defconst agent-repl-roster--merged-scope
  '(:agent-repl-central "the merged workspaces span every repository")
  "The log scope of the merged pages\=' records.")

(defun agent-repl-roster--merged-request (page)
  "Return the `ListMergedWorkspacesRequest' for PAGE of every repository."
  (list :repository nil :order '(:arm :most-recently-merged :value nil) :page page))

(defun agent-repl-roster--merged-fail (page fmt &rest args)
  "Record the failure of merged PAGE at ERROR and signal it.
FMT and ARGS say why.  The record names the page; the signal carries the
same sentence, so a caller surfaces the very cause the log holds."
  (let ((cause (apply #'format fmt args)))
    (agent-repl--error agent-repl-roster--merged-scope
                       "elisp.roster.merged-page-failed: page={page} cause={cause}" :page page :cause cause)
    (signal 'agent-repl-roster-merged-error (list page cause))))

(defun agent-repl-roster--merged-page (response page)
  "Read RESPONSE, a decoded `ListMergedWorkspacesResponse' for PAGE.
Answers `(TOKEN ROWS MORE)': TOKEN the merged set\='s revision, ROWS the
page\='s `RosterRow's in order, MORE non-nil when pages follow.  The
daemon\='s error arm is a failure (`agent-repl-roster--merged-fail'):
`not_ready' is no empty list, and an empty list would claim no merges."
  (pcase response
    (`(:arm :success :value ,success)
     (let* ((extent (plist-get success :extent))
            (rows (mapcar (lambda (merged) (plist-get merged :row))
                          (plist-get (plist-get (plist-get extent :value) :rows) :rows))))
       (list (plist-get (plist-get success :revision) :revision)
             rows
             (eq (plist-get extent :arm) :more))))
    (`(:arm :error :value ,error)
     (agent-repl-roster--merged-fail
      page "error-arm %S" (plist-get (plist-get error :cause) :arm)))
    (_ (agent-repl-roster--merged-fail page "unknown answer %S" response))))

(defun agent-repl-roster--merged-ids-in-view ()
  "Return a hash of the ref ids the streams\=' walk holds, for the dedupe.
A page is resolved from the daemon\='s set as of its answer and the view
as of its last push, so a workspace may stand in both while a push is in
flight; the stream\='s row wins, as open beats merged."
  (let ((ids (make-hash-table :test 'equal)))
    (when agent-repl-roster-view
      (dolist (entry (agent-repl-roster-walk agent-repl-roster-view))
        (puthash (agent-repl-roster-row-id (plist-get entry :row)) t ids)))
    ids))

(defun agent-repl-roster--merged-label ()
  "Return the Recently Merged band\='s drawn label, or nil before a push."
  (plist-get (plist-get (plist-get (plist-get agent-repl-roster-view :recently-merged)
                                   :header)
                        :label)
             :text))

(defun agent-repl-roster--merged-entries-of (rows)
  "Return merged ROWS as walk entries, less those the streams hold.
Each is `(:row ROW :label L :section :merged :collapsed nil :tab nil)',
the shape `agent-repl-roster-walk' answers, so a reader treats a paged
merged row as it treated a walked one."
  (let ((ids (agent-repl-roster--merged-ids-in-view))
        (label (agent-repl-roster--merged-label)))
    (delq nil
          (mapcar (lambda (row)
                    (unless (gethash (agent-repl-roster-row-id row) ids)
                      (list :row row :label label :section :merged
                            :collapsed nil :tab nil)))
                  rows))))

(defun agent-repl-roster--merged-walk-sync (conn max-pages)
  "Read merged pages 0.. on CONN, at most MAX-PAGES (nil: until the last).
Answers `(TOKEN ROWS AGREED)', AGREED nil when a page named another set
than page 0\='s.  A transport failure or an undecodable answer is a
failure (`agent-repl-roster--merged-fail')."
  (let ((page 0) (token nil) (rows nil) (agreed t) (more t))
    (while (and more agreed (or (null max-pages) (< page max-pages)))
      (let* ((response
              (condition-case err
                  (agent-repl-rpc-list-merged-workspaces-sync
                   conn (agent-repl-roster--merged-request page))
                ((agent-repl-connect-error agent-repl-wire-error)
                 (agent-repl-roster--merged-fail page "%s %S" (car err) (cdr err)))))
             (read (agent-repl-roster--merged-page response page)))
        (if (and token (not (equal token (nth 0 read))))
            (setq agreed nil)
          (setq token (nth 0 read)
                rows (append rows (nth 1 read))
                more (nth 2 read)
                page (1+ page)))))
    (list token rows agreed)))

(defun agent-repl-roster-merged-entries-sync (conn &optional max-pages)
  "Return the merged workspaces as walk entries, most recently merged first.
Pages `ListMergedWorkspaces' on CONN across every repository, at most
MAX-PAGES pages (nil: every page until the last), BLOCKING the caller:
for a command the user is waiting on.  A workspace the streams hold is
left out (`agent-repl-roster--merged-entries-of').

Every failure -- the transport, an undecodable answer, the daemon\='s
error arm, or a set that changed under every read
\(`agent-repl-roster-merged-walk-attempts') -- is recorded at ERROR and
SIGNALLED as `agent-repl-roster-merged-error': a list nobody could read
is never offered as the list."
  (let ((attempt 0) (walked nil))
    (while (and (< attempt agent-repl-roster-merged-walk-attempts)
                (not (nth 2 walked)))
      (setq attempt (1+ attempt)
            walked (agent-repl-roster--merged-walk-sync conn max-pages))
      (unless (nth 2 walked)
        (agent-repl--info agent-repl-roster--merged-scope
                          "elisp.roster.merged-set-moved: attempt={attempt}" :attempt attempt)))
    (unless (nth 2 walked)
      (agent-repl-roster--merged-fail
       0 "the merged set changed under %d reads" attempt))
    (let ((entries (agent-repl-roster--merged-entries-of (nth 1 walked))))
      (agent-repl--log agent-repl-roster--merged-scope
                       "elisp.roster.merged-listed: rows={rows} entries={entries} max-pages={max_pages} revision={revision}"
                       :rows (length (nth 1 walked)) :entries (length entries)
                       :max_pages (prin1-to-string max-pages) :revision (nth 0 walked))
      entries)))

(defun agent-repl-roster-merged-entries (conn on-entries on-failure)
  "Page every merged workspace on CONN asynchronously, as walk entries.
ON-ENTRIES receives the entries `agent-repl-roster-merged-entries-sync'
would answer; ON-FAILURE receives the failure\='s signal data
\(`agent-repl-roster-merged-error''s `(PAGE CAUSE)'), which was recorded
at ERROR first.  Exactly one of them is called.  Nothing blocks: for a
reader running off a push, where a blocking walk would stall the editor."
  (let ((attempt 1))
    (cl-labels
        ((fail (page fmt &rest args)
           (condition-case err
               (apply #'agent-repl-roster--merged-fail page fmt args)
             (agent-repl-roster-merged-error (funcall on-failure (cdr err)))))
         (step (page token rows)
           (agent-repl-rpc-list-merged-workspaces
            conn (agent-repl-roster--merged-request page)
            :on-response
            (lambda (response)
              (let ((read (condition-case err
                              (agent-repl-roster--merged-page response page)
                            (agent-repl-roster-merged-error
                             (funcall on-failure (cdr err))
                             nil))))
                (when read
                  (cond
                   ((and token (not (equal token (nth 0 read))))
                    (if (< attempt agent-repl-roster-merged-walk-attempts)
                        (progn
                          (setq attempt (1+ attempt))
                          (agent-repl--info agent-repl-roster--merged-scope
                                            "elisp.roster.merged-set-moved: attempt={attempt}" :attempt (1- attempt))
                          (step 0 nil nil))
                      (fail 0 "the merged set changed under %d reads" attempt)))
                   ((nth 2 read)
                    (step (1+ page) (nth 0 read) (append rows (nth 1 read))))
                   (t
                    (let ((entries (agent-repl-roster--merged-entries-of
                                    (append rows (nth 1 read)))))
                      (agent-repl--log agent-repl-roster--merged-scope
                                       "elisp.roster.merged-listed: rows={rows} entries={entries} max-pages=nil revision={revision}"
                                       :rows (+ (length rows) (length (nth 1 read)))
                                       :entries (length entries) :revision (nth 0 read))
                      (funcall on-entries entries)))))))
            :on-failure
            (lambda (detail) (fail page "transport %S" detail)))))
      (step 0 nil nil))))

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

(defun agent-repl-roster--tab-bar-entries (walked)
  "Return the WALKED entries that have a tab, in the tab bar's order.
THE OPEN SECTION IS THE TAB BAR (`agent-repl-roster-walk'): every open row
is a tab, drawn at its number or folded, and every hidden workspace keeps
an undrawn one.  Merged and closed rows have none."
  (seq-filter (lambda (entry) (memq (plist-get entry :section) '(:repository :hidden)))
              walked))

(defun agent-repl-roster--misnumbered (entries)
  "Return the first of ENTRIES whose tab number is not its place, or nil.
The daemon numbers the Open section's tabs 1..N in the very walk Emacs
draws them in, so the Nth drawn tab carries N.  A tab that disagrees means
the bar and the sidebar would number one workspace two ways: the push is
unusable and is dropped rather than drawn."
  (let ((next 0))
    (seq-find (lambda (entry)
                (let ((number (agent-repl-roster-tab-number (plist-get entry :tab))))
                  (when number
                    (setq next (1+ next))
                    (/= number next))))
              (seq-filter (lambda (entry) (eq (plist-get entry :section) :repository)) entries))))

(defun agent-repl-roster-desired-tabs (roster)
  "Return the tabs ROSTER asks for, in order: plists `(:id :name :ref)'.
The Open section's walk is the order, and THE VISIBILITY IS THE
MEMBERSHIP (`agent-repl-roster-row-visible-p'): Emacs opens a workspace only
while the daemon says it is visible or the selection, so a folded or hidden
workspace the user cannot see, and every merged or closed row, gets none,
and costs this editor nothing (owner ruling, 2026-11-21).

`:collapsed' is a tab off the bar, and `:hidden' the user\='s hide.  A
visible row is collapsed only when it is hidden and the selection: a
folded one is visible only while the daemon draws it inside its fold,
numbered.

THE ROWS IT REFUSES A TAB ARE RECORDED.  This predicate is the whole
answer to \"why is there no tab for that workspace\", and it used to give
it silently: a register whose row came back CLOSED produced no tab, and
nothing anywhere said the roster had been asked for one and declined.
Once per push over every closed row, which is why it is DEBUG."
  (let* ((all (agent-repl-roster-walk roster))
         (entries (agent-repl-roster--tab-bar-entries all))
         (collisions (agent-repl-roster--colliding-names entries)))
    (dolist (entry all)
      (let ((row (plist-get entry :row)))
        (when (agent-repl-roster-row-closed-p row)
          (agent-repl--log-decision (list 'roster-no-tab (agent-repl-roster-row-id row))
                                    (agent-repl-roster-row-name row)
                                    '(:agent-repl-central
                                      "a closed row the roster gives no tab owns no workspace sink")
                                    "elisp.roster.no-tab: id={id} name={name} reason=closed"
                                    :id (agent-repl-roster-row-id row)
                                    :name (agent-repl-roster-row-name row)))))
    (mapcar (lambda (entry)
              (let ((row (plist-get entry :row)))
                (list :id (agent-repl-roster-row-id row)
                      :name (agent-repl-roster--tab-name entry collisions)
                      :number (agent-repl-roster-tab-number (plist-get entry :tab))
                      :ref (agent-repl-roster-row-ref row)
                      :availability (agent-repl-roster-row-availability row)
                      :collapsed (plist-get entry :collapsed)
                      :hidden (eq (plist-get entry :section) :hidden))))
            (seq-filter (lambda (entry) (agent-repl-roster-row-visible-p (plist-get entry :row)))
                        entries))))

;;;; ---- Threads -----------------------------------------------------------
;;
;; A ZETTEL CHAT THREAD IS A WORKSPACE (the daemon's `thread' kind) with a
;; page and a composer of its own, so Emacs opens one exactly as it opens a
;; tab -- a perspective, its host stream, its panels -- from the roster's
;; thread section.  It is never DRAWN: threads are not on the tab bar and are
;; not in workspace cycling (owner ruling, 2026-10-24), so every thread's
;; workspace is one of the walk's hidden ones, off the bar's drawing,
;; numbering and navigation.  It is reached by the thread commands
;; (threads.el) and by the roster's `current', like any other workspace.

(defconst agent-repl-roster-thread-prefix "chat/"
  "The prefix of a thread's workspace name, so no thread's name can take a tab's.")

(defun agent-repl-roster-threads (&optional roster)
  "Return ROSTER's OPEN thread rows, the one last asked a question first.
ROSTER defaults to the last accepted roster (`agent-repl-roster-view').
An archived thread's workspace is closed, so Emacs opens no workspace for
it and offers it to no thread command: the sidebar restores it."
  (cl-remove-if-not (lambda (row)
                      (eq (plist-get (plist-get row :standing) :arm) :open))
              (plist-get (plist-get (or roster agent-repl-roster-view) :threads) :rows)))

(defun agent-repl-roster-thread-p (ws)
  "Return non-nil when the workspace named WS is a zettel chat thread.
The roster states it on every reconcile (`agent-repl-roster--reconcile-row');
a name the roster opened no workspace under, nil included, is no thread."
  (and ws (agent-repl--ws-get ws :thread)))

(defun agent-repl-roster-thread-row-ws (row)
  "Return the name of the workspace Emacs opened for the thread ROW, or nil.
Nil while the thread's workspace is not open yet."
  (agent-repl--ws-by-ref-id (plist-get (plist-get row :thread) :id)))

(defun agent-repl-roster-shown-app (&optional roster)
  "Return the sidebar app ROSTER says the rail shows: `:workspaces' or `:chat'.
ROSTER defaults to the last accepted roster (`agent-repl-roster-view').
The daemon resolves it from the selection's kind (a thread on screen shows
Chat), and every roster carries it, so nil means no roster has arrived."
  (let ((roster (or roster agent-repl-roster-view)))
    (and roster (plist-get (plist-get roster :app-switcher) :shown))))

(defun agent-repl-roster-thread-workspace-name (row)
  "Return the workspace name a thread ROW is opened under: `chat/' and its ID.
NEVER ITS DISPLAY NAME.  That is words with spaces (\"Why fusion stops at iron\"),
and an input panel's name ends its workspace at the first space
\(`agent-repl--extract-panel-id'), so a named thread's panels read as
another workspace's orphans: the window sweep deleted them and the layout
reconciler showed them again, forever.  The id has no space, is unique,
and never changes, so the name arriving after the thread opened renames
nothing in Emacs; the name is the roster's `:name', drawn wherever the
user picks a thread (`agent-repl-thread-pick')."
  (concat agent-repl-roster-thread-prefix (plist-get (plist-get row :thread) :id)))

(defun agent-repl-roster-desired-threads (roster)
  "Return the workspaces ROSTER's threads ask for: the visible ones.
Shaped as `agent-repl-roster-desired-tabs' returns them.  A thread is
visible only while it is the selection (frontend.v1.RosterRowVisibility),
so at most one is open here.  Each carries `:thread' t and is never `:collapsed'
nor `:hidden': its page loads, it is only never drawn."
  (mapcar (lambda (row)
            (let ((ref (plist-get row :thread)))
              (list :id (plist-get ref :id)
                    :name (agent-repl-roster-thread-workspace-name row)
                    :ref ref
                    :availability nil
                    :collapsed nil
                    :hidden nil
                    :thread t)))
          (seq-filter #'agent-repl-roster-row-visible-p
                      (agent-repl-roster-threads roster))))

;;;; ---- Reconciliation ---------------------------------------------------

(defun agent-repl-roster--placements (roster)
  "Return where ROSTER places every row it lists, keyed by ref id.
Each value is `(:section S :visibility V)': S the walk\='s section
\(`:repository', `:hidden', `:closed'), or `:thread' for an open thread;
V the row\='s visibility as the daemon stamped it."
  (let ((rows (make-hash-table :test 'equal)))
    (dolist (entry (agent-repl-roster-walk roster))
      (let ((row (plist-get entry :row)))
        (puthash (agent-repl-roster-row-id row)
                 (list :section (plist-get entry :section)
                       :visibility (plist-get row :visibility))
                 rows)))
    (dolist (thread (agent-repl-roster-threads roster))
      (puthash (plist-get (plist-get thread :thread) :id)
               (list :section :thread :visibility (plist-get thread :visibility))
               rows))
    rows))

(defun agent-repl-roster--decision (id before after)
  "Return the roster facts that decided the tab of ID, from BEFORE to AFTER.
BEFORE and AFTER are `agent-repl-roster--placed' plists, BEFORE nil for
the first roster.  The answer is a plist of the row\='s section and
visibility in each, and the daemon and revision of each roster: what a tab
open or teardown records (`elisp.roster.tab-open', `elisp.roster.tab-teardown')."
  (let ((was (and before (gethash id (plist-get before :rows))))
        (now (gethash id (plist-get after :rows))))
    (list :section (plist-get now :section)
          :visibility (plist-get now :visibility)
          :was-section (plist-get was :section)
          :was-visibility (plist-get was :visibility)
          :daemon (plist-get after :address)
          :revision (plist-get after :revision)
          :was-daemon (plist-get before :address)
          :was-revision (plist-get before :revision))))

(defun agent-repl-roster--open-tab (desired &optional decided)
  "Open the tab DESIRED asks for and start its host stream.
DECIDED is the roster facts that asked for it (`agent-repl-roster--decision'),
recorded with the open.
The roster's rows are ALREADY REGISTERED daemon-side — Register is not
this file's verb — so the tab's whole birth is a perspective plus the
`WatchHostWorkspace' subscription."
  (let* ((name (plist-get desired :name))
         (ref (plist-get desired :ref))
         (dir (plist-get ref :dir))
         ;; A RE-HOLD IS NO ARRIVAL: a workspace this editor closed only
         ;; because it stopped being held (`agent-repl-roster--tear-down-tab')
         ;; was open all along, so its return neither enters the selection
         ;; history nor opens its panels; a switch to it shows them.
         (rehold (and (agent-repl--ws-tombstoned-p name)
                      (equal (plist-get (agent-repl--ws-get name :ref) :id) (plist-get desired :id))
                      (agent-repl--ws-get name :released))))
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
    (agent-repl--ws-put name :released nil)
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
    ;; A THREAD IS MARKED AT ITS BIRTH, before its panels open below: the
    ;; tab bar's rule reads the mark the moment the thread's webview comes
    ;; on screen (`agent-repl--tabbar-target-rows'), and no later write
    ;; repins the frame.
    (agent-repl--ws-put name :thread (and (plist-get desired :thread) t))
    (agent-repl--info name "elisp.roster.tab-open: ws={ws} id={id} dir={dir} rehold={rehold} section={section} visibility={visibility} was-section={was_section} was-visibility={was_visibility} daemon={daemon} revision={revision} was-daemon={was_daemon} was-revision={was_revision}"
                      :ws name :id (plist-get desired :id) :dir dir :rehold (and rehold t)
                      :section (plist-get decided :section)
                      :visibility (plist-get decided :visibility)
                      :was_section (plist-get decided :was-section)
                      :was_visibility (plist-get decided :was-visibility)
                      :daemon (plist-get decided :daemon)
                      :revision (plist-get decided :revision)
                      :was_daemon (plist-get decided :was-daemon)
                      :was_revision (plist-get decided :was-revision))
    (agent-repl-roster--subscribe-host name ref)
    ;; A TAB BORN HERE IS A WORKSPACE THAT JUST BECAME OPEN, and opening a
    ;; workspace opens its panels before it is switched to (owner ruling,
    ;; 2026-09-13, item 6).  This is the one landing every open takes --
    ;; create, fork, one-shot, re-open, register, and any daemon-side
    ;; arrival -- so it is the one place the panels are asked for.  The
    ;; startup roster is exempt and panels.el is what knows it
    ;; (`agent-repl--panels-arrivals-armed').  A plain switch never reaches
    ;; here: its tab already exists.
    (unless rehold
      (agent-repl--panels-open-on-arrival name (plist-get desired :id)))
    name))

(defun agent-repl-roster--subscribe-host (name ref)
  "Start NAME's host stream for REF on the primary link, when there is one."
  (let ((conn (and (fboundp 'agent-repl-link-primary) (agent-repl-link-primary))))
    (if conn
        (progn
          (agent-repl-host-subscribe conn name ref)
          (agent-repl--log name "elisp.roster.host-subscribe: ws={ws} id={id}"
                           :ws name :id (plist-get ref :id)))
      (agent-repl--info name
                        "elisp.roster.host-subscribe: skipped ws={ws} reason=no-primary-link"
                        :ws name))))

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
    ;; The webview's NAME is a panel key too: left on OLD, the window sweep
    ;; deletes it as another workspace's orphan and the layout shows it
    ;; again, a loop that pins Emacs.
    (agent-repl--frontend-rename-webview new)
    ;; So is the composer's: its identity segment is the workspace name.
    (agent-repl-host--apply-naming new)
    (agent-repl--info new "elisp.roster.tab-rename: old={old} new={new} id={id}"
                      :old old :new new :id (plist-get ref :id))
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
                        "elisp.roster.tab-rename-refused: old={old} new={new} dir={dir} error={error}"
                        :old old :new new :dir (prin1-to-string dir) :error (error-message-string err))
     nil)))

(defun agent-repl-roster--tear-down-tab (name &optional released decided)
  "Tear NAME's tab down: kill the persp, tombstone it.
RELEASED non-nil marks a workspace the roster still lists open but no
longer marks visible (frontend.v1.RosterRowVisibility): it is only closed
in this editor, and its tombstone says so (`:released'), so its next open is a
re-hold rather than a workspace becoming open (`agent-repl-roster--open-tab').
IDEMPOTENT, because CloseWorkspace's own success tears the tab down too
and the roster push that follows must not care which of the two got
there first.

THE HOST STREAM IS NOT CANCELLED HERE.  The tombstone
\(`agent-repl--ws-del') runs `agent-repl-ws-del-hook', and host.el forgets
the workspace there, stream and all, whichever teardown tombstoned it.

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
abort the reconcile walk mid-list.

THE COMPOSER'S TEXT OUTLIVES THE TAB.  A workspace the roster stops
marking visible (folded, hidden, or left) is torn down here while the daemon
keeps it, so its composer is saved first as the history's unsent head
\(`agent-repl--input-save-unsent'), as Emacs's own shutdown saves it, and
the composer born on its next open puts it back.  A failed save is
recorded at ERROR and the teardown goes on: the roster has already said
the tab is gone."
  (condition-case err
      (agent-repl--input-save-unsent name)
    (error
     (agent-repl--error name "elisp.roster.tab-teardown-save-failed: ws={ws} error={error}"
                        :ws name :error (error-message-string err))))
  (condition-case err
      (agent-repl--ws-land-then-kill name)
    (error
     (agent-repl--error name "elisp.roster.tab-teardown-persp-kill-failed: ws={ws} error={error}"
                        :ws name :error (error-message-string err))))
  (agent-repl--ws-del name)
  (agent-repl--ws-put name :released (and released t))
  (agent-repl--info name "elisp.roster.tab-teardown: ws={ws} released={released} section={section} visibility={visibility} was-section={was_section} was-visibility={was_visibility} daemon={daemon} revision={revision} was-daemon={was_daemon} was-revision={was_revision}"
                    :ws name :released (and released t)
                    :section (plist-get decided :section)
                    :visibility (plist-get decided :visibility)
                    :was_section (plist-get decided :was-section)
                    :was_visibility (plist-get decided :was-visibility)
                    :daemon (plist-get decided :daemon)
                    :revision (plist-get decided :revision)
                    :was_daemon (plist-get decided :was-daemon)
                    :was_revision (plist-get decided :was-revision)))

(defun agent-repl-roster--listed-open-ids (roster)
  "Return the ids ROSTER lists open, held here or not, as a hash set.
Its Open and Hidden sections' rows and its open threads: the workspaces this
editor closes only because they stopped being held, never because they
closed."
  (let ((ids (make-hash-table :test 'equal)))
    (dolist (entry (agent-repl-roster-walk roster))
      (when (memq (plist-get entry :section) '(:repository :hidden))
        (puthash (agent-repl-roster-row-id (plist-get entry :row)) t ids)))
    (dolist (thread (agent-repl-roster-threads roster))
      (puthash (plist-get (plist-get thread :thread) :id) t ids))
    ids))

(defun agent-repl-roster--roster-owned-names ()
  "Return every live workspace this file opened — those carrying a `:ref'.
A workspace with no ref never came from the roster, so reconciliation
leaves it alone rather than tearing down something it does not own."
  (cl-remove-if-not (lambda (name) (agent-repl--ws-get name :ref))
                    (agent-repl--live-ws-names)))

(defun agent-repl-roster--log-row-failure (ws message)
  "Record a per-row reconcile failure for WS at ERROR, as log MESSAGE.
MESSAGE is the caller's (`agent-repl--log-msg'), so the site is its own.
A row whose reconcile failed is exactly the row whose workspace may own no
durable sink — a worktree deleted underneath us is the commonest way to get
here — so the scope is screened through `agent-repl--ws-log-name' and falls
back to the central sink, with the name kept in the message text."
  (apply #'agent-repl--emit-error
         (or (and ws (agent-repl--ws-log-name ws))
             '(:agent-repl-central
               "a roster row whose reconcile failed may own no workspace sink"))
         message))

(defun agent-repl-roster--purge-foreign-tombstone (name id)
  "Purge the tombstone under NAME when it is not the workspace ID's.
THE DAEMON NAMES WORKSPACES, and it frees a name once its workspace is
forgotten or nuked, so a row the roster names NAME is the one workspace
holding it.  A tombstone Emacs keeps under NAME for another workspace --
its ref names another id, or none -- is one the daemon no longer knows by
that name, and kept it refused the rename onto NAME
\(`agent-repl--ws-rename-state') and revived the wrong identity on an open
\(`agent-repl-roster--open-tab'), leaving the tab on its old name while the
sidebar and the CLI used the new one.  A tombstone of ID itself is the row
coming back, which the open revives, and is left alone."
  (when (agent-repl--ws-tombstoned-p name)
    (let ((tombstone-id (plist-get (agent-repl--ws-get name :ref) :id)))
      (unless (equal tombstone-id id)
        (agent-repl--info name
                          "elisp.roster.tombstone-purged: ws={ws} tombstone-id={tombstone_id} row-id={row_id}"
                          :ws name :tombstone_id tombstone-id :row_id id)
        (agent-repl--ws-forget name)))))

(defun agent-repl-roster--reconcile-row (want wanted-ids &optional decided)
  "Reconcile one roster row WANT, recording its id in WANTED-IDS.
DECIDED is the roster facts a tab it opens records
\(`agent-repl-roster--decision').
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
        (let ((settled
               (progn
                 (agent-repl-roster--purge-foreign-tombstone name id)
               (cond
                ((null existing)
                 (agent-repl-roster--open-tab want decided))
                ((equal existing name)
                 (agent-repl--ws-put name :ref (plist-get want :ref))
                 (agent-repl--log-decision (list 'roster-tab-kept id) name
                                           name "elisp.roster.tab-kept: ws={ws} id={id}" :ws name :id id)
                 name)
                (t
                 (agent-repl-roster--rename-tab existing name (plist-get want :ref)))))))
          ;; WHETHER THE WORKSPACE IS A CHAT THREAD is the roster's to say,
          ;; and is stated on every reconcile (`agent-repl-roster-thread-p').
          (when settled
            (agent-repl--ws-put settled :thread (and (plist-get want :thread) t)))
          settled)
      (error
       (agent-repl-roster--log-row-failure
        (or existing name)
        (agent-repl--log-msg "elisp.roster.row-reconcile-failed: ws={ws} id={id} error={error}"
        :ws name :id id :error (error-message-string err)))
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
        "elisp.roster.bringup-handler-failed fn={fn} error={error}"
        :fn fn :error (error-message-string err))))))

(defvar agent-repl-roster--walking nil
  "Non-nil while a roster walk asks for its opens.
A tab answered open meanwhile is ordered and selected by the walk itself,
which sets the order and applies the selection once it is done.")

(defun agent-repl-roster--walk-unchanged-p (inputs desired)
  "Non-nil when a walk over INPUTS, whose desired rows are DESIRED, changes nothing.
INPUTS are the last walk's own (`agent-repl-roster--reconciled'), every
wanted tab stands under the name it wants, and every roster-owned tab is
wanted, so the walk would open, rename and tear down nothing."
  (and (equal inputs agent-repl-roster--reconciled)
       (cl-every (lambda (want)
                   (equal (agent-repl--ws-by-ref-id (plist-get want :id)) (plist-get want :name)))
                 desired)
       (let ((ids (make-hash-table :test 'equal)))
         (dolist (want desired) (puthash (plist-get want :id) t ids))
         (cl-every (lambda (name) (gethash (plist-get (agent-repl--ws-get name :ref) :id) ids))
                   (agent-repl-roster--roster-owned-names)))))

(defun agent-repl-roster-reconcile (roster)
  "Bring the tab bar in line with ROSTER and return the tab names in order.
Opens a tab for each `closed = false' row that has none, renames the tab
whose row's name changed, tears down every roster-owned tab whose row is
gone or closed, and sets the tab ORDER to the walk order strictly.

A PUSH THAT CHANGES NOTHING THE WALK READS WALKS NOTHING (2026-11-23).  A
push that moved only statuses hands the walk the same desired rows, and the
walk used to run whole for each: 92 passes in three minutes of a handover.
Such a push is recorded at DEBUG and answers the order as it stands
(`agent-repl-roster--walk-unchanged-p').

A row that fails to reconcile is contained rather than fatal; see
`agent-repl-roster--reconcile-row'."
  (let* ((desired (append (agent-repl-roster-desired-tabs roster)
                          (agent-repl-roster-desired-threads roster)))
         (listed-open (agent-repl-roster--listed-open-ids roster))
         (inputs (cons desired (sort (hash-table-keys listed-open) #'string<))))
    (if (agent-repl-roster--walk-unchanged-p inputs desired)
        (progn
          ;; WHERE THIS ROSTER PLACES THE ROWS is still the state the next
          ;; walk reads as BEFORE, though this one opened and tore down nothing.
          (setq agent-repl-roster--placed (agent-repl-roster--placed-of roster))
          (agent-repl--log '(:agent-repl-central "roster reconciliation spans every workspace")
                           "elisp.roster.reconcile-unchanged: tabs={tabs}"
                           :tabs (length agent-repl-roster--tab-order))
          agent-repl-roster--tab-order)
      (prog1 (agent-repl-roster--walk roster desired listed-open)
        (setq agent-repl-roster--reconciled inputs)))))

(defun agent-repl-roster--placed-of (roster)
  "Where ROSTER places every row, as `agent-repl-roster--placed' records it."
  (list :rows (agent-repl-roster--placements roster)
        :address agent-repl-roster--view-address
        :revision (plist-get roster :revision)))

(defun agent-repl-roster--walk (roster desired listed-open)
  "Walk ROSTER's DESIRED rows into the tab bar; LISTED-OPEN are the ids it lists open.
The whole of `agent-repl-roster-reconcile' for a push that changed what it reads."
  (let* ((wanted-ids (make-hash-table :test 'equal))
         ;; Counted BEFORE the walk, because the walk is what makes rows
         ;; stop being tabless: asking the same question afterwards would
         ;; always answer zero.
         (total (cl-count-if
                 (lambda (want)
                   (null (agent-repl--ws-by-ref-id (plist-get want :id))))
                 desired))
         (opened 0)
         (names nil)
         ;; The daemon's number of every numbered tab, by its settled name.
         (numbers nil)
         ;; The tabs of rows whose repository the daemon holds collapsed.
         (hidden nil)
         ;; Every reconciled row, for the opens (open-tabs.el).
         (rows nil)
         ;; WHERE THE ROWS STOOD BEFORE THIS ROSTER AND WHERE IT PUTS THEM,
         ;; so each tab opened or torn down records what decided it.
         (before agent-repl-roster--placed)
         (after (agent-repl-roster--placed-of roster)))
    (dolist (want desired)
      (let ((fresh (null (agent-repl--ws-by-ref-id (plist-get want :id)))))
        ;; EVERY ROW IS RECONCILED AT ONCE, and its tab stays out of the bar
        ;; until the daemon answers it open (open-tabs.el): no row waits on
        ;; another, and its page loads the moment it is asked for.
        (let ((name (agent-repl-roster--reconcile-row
                     want wanted-ids
                     (agent-repl-roster--decision (plist-get want :id) before after))))
          (when name
            (push (cons name (plist-get want :id)) names)
            (when (plist-get want :number)
              (push (cons name (plist-get want :number)) numbers))
            (push (list :id (plist-get want :id) :name name :ref (plist-get want :ref)
                        :collapsed (plist-get want :collapsed)
                        :hidden (plist-get want :hidden))
                  rows)
            ;; A THREAD IS NEVER DRAWN, but its page loads: it is hidden
            ;; from the bar while open-tabs asks for it like any shown tab.
            (when (or (plist-get want :collapsed) (plist-get want :thread))
              (push name hidden)))
          ;; A row that FAILED opened no tab, so it does not count towards
          ;; the bring-up.
          (when (and fresh name)
            (setq opened (1+ opened))
            (agent-repl-roster--note-bringup opened total nil)))))
    (dolist (name (agent-repl-roster--roster-owned-names))
      (let ((id (plist-get (agent-repl--ws-get name :ref) :id)))
        (unless (gethash id wanted-ids)
          ;; The teardown contains its own signals step by step, but the
          ;; registry writes BETWEEN those steps are covered by none of them,
          ;; and a signal here would abort the walk over the remaining torn
          ;; down rows for the same reason a failed open must not.
          (condition-case err
              (agent-repl-roster--tear-down-tab name (gethash id listed-open)
                                                (agent-repl-roster--decision id before after))
            (error
             (agent-repl-roster--log-row-failure
              name (agent-repl--log-msg "elisp.roster.row-teardown-failed: ws={ws} error={error}"
              :ws name :error (error-message-string err))))))))
    (setq agent-repl-roster--placed after
          agent-repl-roster--walked (nreverse names)
          agent-repl-roster--walked-hidden hidden
          agent-repl-roster--tab-numbers (nreverse numbers))
    ;; THE OPENS, once the walk has made every row's workspace and BEFORE
    ;; the order is set: each one owed an open is asked for, its page loaded
    ;; now, and the order below leaves out every tab still held.  An answer
    ;; that arrives within the walk leaves the order and the selection to it
    ;; (`agent-repl-roster--walking').
    (let ((agent-repl-roster--walking t))
      (agent-repl-open-tabs-reconcile (nreverse rows)))
    (agent-repl-roster--filter-order)
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
                      "elisp.roster.reconcile: tabs={tabs} order={order} hidden={hidden}"
                      :tabs (length agent-repl-roster--tab-order)
                      :order (prin1-to-string agent-repl-roster--tab-order)
                      :hidden (prin1-to-string agent-repl-roster--hidden-tabs))
    ;; LAST, after the order is set, so a handler that reads the tab bar
    ;; sees the one this pass produced rather than the previous pass's.
    (when (> opened 0)
      (agent-repl-roster--note-bringup opened total t))
    ;; The startup roster has now been delivered, so every LATER arrival is a
    ;; workspace that became open with this editor watching and opens its own
    ;; panels (`agent-repl--panels-open-on-arrival').
    (agent-repl--panels-arm-arrivals)
    agent-repl-roster--tab-order))

;;;; ---- Lookups the renderers use ----------------------------------------

(defun agent-repl-roster-row-of-id (id)
  "Return the roster row of the workspace whose ref id is ID, or nil.
Every row the roster lists, held here or not."
  (gethash id agent-repl-roster--rows-by-id))

(defun agent-repl-roster-row-for-ws (ws)
  "Return the roster row for workspace WS, or nil when the roster has none."
  (let ((id (plist-get (agent-repl--ws-get ws :ref) :id)))
    (and id (agent-repl-roster-row-of-id id))))

(defun agent-repl-roster-row-dormant-p (row)
  "Return non-nil when ROW\='s session runs no work (frontend.v1.RosterRowDormant)."
  (plist-get row :dormant))

(defun agent-repl-roster-dormant-p (ws)
  "Return non-nil when WS\='s session runs no work, nil before its first push."
  (agent-repl-roster--for-ws ws #'agent-repl-roster-row-dormant-p))

(defun agent-repl-roster-status-for-ws (ws)
  "Return WS's roster status arm keyword, or nil before its first push."
  (agent-repl-roster--for-ws ws #'agent-repl-roster-row-status))

(defun agent-repl-roster--for-ws (ws reader)
  "Return READER applied to WS's current roster row, or nil.
Nil, with READER never called, before any push has carried a row for WS.
The one shape every `agent-repl-roster-*-for-ws' reader shares."
  (let ((row (agent-repl-roster-row-for-ws ws)))
    (and row (funcall reader row))))

(defun agent-repl-roster-last-selected-ms (ws)
  "Return WS's durable last-selected instant in epoch ms, or nil.
Read from WS's roster row (`RosterRowLastSelected'), which the daemon
fills from its own record, so it survives an Emacs restart.  nil when the
roster has no row for WS or the user never selected it."
  (agent-repl-roster--instant-ms ws :last-selected))

(defun agent-repl-roster-last-interacted-ms (ws)
  "Return WS\='s durable interaction instant in epoch ms, or nil.
Read from WS\='s roster row (`RosterRowLastInteracted'), or from its thread
row (`RosterThreadLastInteracted') for a chat thread, which the daemon
fills from its own record of the user\='s interactions, so it survives an
Emacs restart.  nil when the roster has neither row for WS or the user
never interacted with it."
  (let ((row (agent-repl-roster-row-for-ws ws)))
    (if row
        (agent-repl-roster-row-instant-ms row :last-interacted)
      (let ((id (plist-get (agent-repl--ws-get ws :ref) :id)))
        (and id
             (agent-repl-roster-row-instant-ms
              (cl-find-if (lambda (thread) (equal (plist-get (plist-get thread :thread) :id) id))
                          (agent-repl-roster-threads))
              :last-interacted))))))

(defun agent-repl-roster-interaction-standing-p (ws)
  "Return non-nil when an interaction with WS would tell the daemon nothing.
That is: WS\='s row is already viewed (`agent-repl-roster-viewed-for-ws')
AND no other row\='s interaction instant is newer than WS\='s, so WS is
already the workspace interacted with most recently.  The one gate
`agent-repl-host-report-interaction' reads, so a keystroke reports only
what the roster does not already say."
  (let ((mine (agent-repl-roster-last-interacted-ms ws)))
    (and mine
         (agent-repl-roster-viewed-for-ws ws)
         (not (cl-some (lambda (name)
                         (let ((theirs (and (not (equal name ws))
                                            (agent-repl-roster-last-interacted-ms name))))
                           (and theirs (> theirs mine))))
                       (agent-repl--ws-list-names))))))

(defun agent-repl-roster-last-turn-ended-ms (ws)
  "Return WS's durable last-turn-ended instant in epoch ms, or nil.
Read from WS's roster row (`RosterRowLastTurnEnded'), which the daemon
fills from its turn record, so it survives an Emacs restart.  nil when
the roster has no row for WS or no turn of WS has closed."
  (agent-repl-roster--instant-ms ws :last-turn-ended))

(defun agent-repl-roster-last-turn-boundary-ms (ws)
  "Return the instant one of WS's turns most recently started or ended, or nil.
Epoch ms, read from WS's roster row (`RosterRowLastTurnBoundary'): the
running turn's start, else the last end, and under a sync merge the start
of the turn that finished into it, all resolved by the daemon.  nil when
the roster has no row for WS or no turn of WS has started.  The one-line
tab bar keeps the tabs newest by it (`agent-repl--tabline-recency-keep')."
  (agent-repl-roster--instant-ms ws :last-turn-boundary))

(defun agent-repl-roster-last-turn-started-ms (ws)
  "Return WS's most recent turn-start instant in epoch ms, or nil.
Read from WS's roster row (`RosterRowLastTurnStarted'): the running turn's
start, else the newest durable start, resolved by the daemon so it survives
an Emacs restart.  nil when the roster has no row for WS or no turn of WS
has started."
  (agent-repl-roster--instant-ms ws :last-turn-started))

(defun agent-repl-roster-last-started-name (names)
  "Return the one of NAMES whose turn most recently STARTED, or nil.
Ranks by `agent-repl-roster-last-turn-started-ms', newest first, whatever
the workspace's status now: a turn still running and a turn long closed
both count by their start.  A name with no recorded start is not a
candidate.  Ties keep the order NAMES gives them."
  (let (best best-ms)
    (dolist (name names)
      (let ((ms (agent-repl-roster-last-turn-started-ms name)))
        (when (and ms (or (null best-ms) (> ms best-ms)))
          (setq best name best-ms ms))))
    best))

(defun agent-repl-roster-last-finished-name (names)
  "Return the one of NAMES that most recently FINISHED, or nil.
FINISHED is the move from working (red) to done (green): the row's arm
is `:done' NOW, and its durable last-turn-ended instant
\(`agent-repl-roster-last-turn-ended-ms') says when.  The arm excludes a
workspace whose next turn already runs, an interrupted or failed turn,
and every other state; the instant ranks the rest, newest first.  A
`:done' row without an instant has no recorded close to rank by and is
not a candidate (`RosterRowLastTurnEnded' allows its absence).  Ties keep
the order NAMES gives them."
  (let (best best-ms)
    (dolist (name names)
      (let ((ms (and (eq (agent-repl-roster-status-for-ws name) :done)
                     (agent-repl-roster-last-turn-ended-ms name))))
        (when (and ms (or (null best-ms) (> ms best-ms)))
          (setq best name best-ms ms))))
    best))

(defun agent-repl-roster--instant-ms (ws field)
  "Return the epoch ms of WS's roster-row instant FIELD, or nil.
nil when the roster has no row for WS; otherwise
`agent-repl-roster-row-instant-ms' reads it."
  (agent-repl-roster-row-instant-ms (agent-repl-roster-row-for-ws ws) field))

(defun agent-repl-roster-selection-recency-order (names)
  "Return NAMES ordered by the user\='s interaction recency, newest first.
THE ONE RECENCY ORDER: the recency walk (`s-[', `s-]'), the jump to the
previous workspace, the landing after a close
\(`agent-repl--teardown-landing-target') and the jump to the most recent
workspace (`agent-repl--roster-recent-names') all order by it, so none
can disagree about which workspace came before.  The daemon is its
source: Emacs keeps no history of its own.

  1. The walk\='s origin while a recency walk runs
     (`agent-repl--recency-walk-origin'), and otherwise the workspace the
     frame stands in: the place a step starts from.
  2. Then the names the user interacted with, by the daemon\='s durable
     interaction instant (`agent-repl-roster-last-interacted-ms'), newest
     first.  A step of the walk stamps none, so the order holds still
     under it.
  3. Then the names never interacted with, in the order NAMES gives them.

NAMES is never filtered, only reordered."
  (let* ((lead (or agent-repl--recency-walk-origin (agent-repl--ws-current-name)))
         (head (and lead (member lead names) (list lead)))
         (rest (remove lead names))
         (stamped (cl-remove-if-not #'agent-repl-roster-last-interacted-ms rest))
         (never (cl-remove-if #'agent-repl-roster-last-interacted-ms rest)))
    (append head
            (cl-stable-sort (copy-sequence stamped) #'>
                            :key #'agent-repl-roster-last-interacted-ms)
            never)))

(defun agent-repl-roster--filter-order ()
  "Set the tab order from the last walk, less the tabs held for their open.
A held tab is PRE-CREATED -- its workspace exists -- but the bar draws it
only once the daemon answers it open (open-tabs.el)."
  (let ((open (cl-remove-if (lambda (pair) (agent-repl-open-tabs-holds-p (cdr pair)))
                            agent-repl-roster--walked)))
    (setq agent-repl-roster--tab-order (mapcar #'car open)
          agent-repl-roster--hidden-tabs
          (cl-remove-if-not (lambda (name) (member name agent-repl-roster--tab-order))
                            agent-repl-roster--walked-hidden))))

(defun agent-repl-roster-refresh-order ()
  "Re-derive the tab order after a tab was answered open, and redraw the bar."
  (agent-repl-roster--filter-order)
  (agent-repl--info '(:agent-repl-central "roster reconciliation spans every workspace")
                    "elisp.roster.order-refreshed: order={order}" :order (prin1-to-string agent-repl-roster--tab-order))
  (force-mode-line-update t)
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'agent-repl--force-tab-bar-redraw))
    (agent-repl--force-tab-bar-redraw))
  agent-repl-roster--tab-order)

(defun agent-repl-roster-tab-order ()
  "Return every tab name in roster walk order, the hidden ones included."
  agent-repl-roster--tab-order)

(defun agent-repl-roster-drawn-tab-order ()
  "Return the tab names the bar DRAWS, in roster walk order.
Every tab but those of a repository the daemon holds collapsed
\(`agent-repl-roster--hidden-tabs').  The bar's numbering and its
navigation index THIS list, so a hidden tab takes no number and no stop."
  (cl-remove-if (lambda (name) (member name agent-repl-roster--hidden-tabs))
                agent-repl-roster--tab-order))

(defun agent-repl-roster-tab-number-of (name)
  "Return the number the daemon gives tab NAME, or nil when it has none.
The sidebar paints the same number beside NAME's row
\(`agent-repl-roster--tab-numbers')."
  (cdr (assoc name agent-repl-roster--tab-numbers)))

;;;; ---- Families ---------------------------------------------------------

(defun agent-repl-roster--sibling-groups (roster)
  "Return ROSTER's Open section as sibling groups, `(PARENT-ID . IDS)' pairs.
Each repository section's top-level rows are one group whose PARENT-ID is
nil (`agent-repl-roster-section-rows', its subsections' rows included), and
every open row's family (`agent-repl-roster-row-kin') is a group under that
row's id.  IDS are in drawn order.  The hidden section is no family: a
hidden workspace is no stop of any walk."
  (let ((groups nil))
    (cl-labels ((walk (rows parent)
                  (when rows
                    (push (cons parent (mapcar (lambda (wrapped)
                                                 (agent-repl-roster-row-id (plist-get wrapped :row)))
                                               rows))
                          groups)
                    (dolist (wrapped rows)
                      (walk (agent-repl-roster-row-kin wrapped)
                            (agent-repl-roster-row-id (plist-get wrapped :row)))))))
      (dolist (section (plist-get (plist-get roster :repository) :sections))
        (walk (agent-repl-roster-section-rows section) nil)))
    (nreverse groups)))

(defun agent-repl-roster-family-of (name &optional roster)
  "Return tab NAME's family in ROSTER as tab names, or nil when it has none.
The answer is `(:parent PARENT :siblings SIBLINGS :aunts AUNTS)': PARENT is
the row NAME is nested under, nil for a top-level row; SIBLINGS is NAME's
sibling group, NAME included, and AUNTS is PARENT's, PARENT included (nil
when PARENT is nil), each in drawn order (`agent-repl-roster--sibling-groups').
A top-level row's siblings are the other top-level rows of its repository,
the same family `agent-repl-workspace-move' moves within.  Ids are named by
the last reconcile's tabs (`agent-repl-roster--walked'), so a row with no
tab is left out.  ROSTER defaults to `agent-repl-roster-view'."
  (let* ((id (cdr (assoc name agent-repl-roster--walked)))
         (groups (agent-repl-roster--sibling-groups (or roster agent-repl-roster-view)))
         (named (lambda (ids)
                  (delq nil (mapcar (lambda (each) (car (rassoc each agent-repl-roster--walked)))
                                    ids))))
         (own (and id (seq-find (lambda (group) (member id (cdr group))) groups))))
    (when own
      (let* ((parent (car own))
             (above (and parent (seq-find (lambda (group) (member parent (cdr group))) groups))))
        (list :parent (and parent (car (rassoc parent agent-repl-roster--walked)))
              :siblings (funcall named (cdr own))
              :aunts (and above (funcall named (cdr above))))))))

(defun agent-repl-roster-drawn-tab-numbers (names)
  "Return the number the bar paints on each of the drawn tabs NAMES, in order.
The daemon's number (`agent-repl-roster-tab-number-of') once a roster has
numbered any tab, so a tab held for its open leaves a gap rather than
renumbering the tabs after it.  Before that -- a pre-roster boot draws the
workspaces in their registration order -- each tab's 1-based place.

A DRAWN TAB WITH NO NUMBER IS AN ERROR.  The drawn order is the Open
section less its folded and hidden tabs, and the daemon numbers every
other one (`agent-repl-roster--misnumbered' drops a push that does not)."
  (if (null agent-repl-roster--tab-numbers)
      (number-sequence 1 (length names))
    (mapcar (lambda (name)
              (or (agent-repl-roster-tab-number-of name)
                  (error "agent-repl: drawn tab %S has no number from the daemon" name)))
            names)))

(defun agent-repl-roster-numbered-tab (number)
  "Return the name of the tab the daemon numbers NUMBER, or nil.
The tab may be one Emacs does not draw yet, held for its open; the
caller asks `agent-repl-roster-drawn-tab-order' whether it is drawn."
  (car (rassq number agent-repl-roster--tab-numbers)))

;;;; ---- The current workspace (R8) ---------------------------------------

(defun agent-repl-roster--current-id (roster)
  "Return ROSTER's selected workspace id, or nil when there is none."
  (plist-get (plist-get (plist-get roster :current) :workspace) :id))

(defvar agent-repl-roster-following nil
  "Non-nil while the frame is being moved to follow the roster's `current'.
Bound around the switch `agent-repl-roster-apply-current' makes, so the
perspective-activation hook (`agent-repl-host--on-workspace-activated')
knows the daemon already holds this selection and sends nothing back.")

(defvar agent-repl-roster--pushed-at nil
  "When the roster push being applied reached Emacs, as `float-time'.
The follow record carries it, so a switch read back from the log shows
both edges of its latency: the daemon's stamp to this arrival, and this
arrival to the switch.")

(defvar agent-repl-roster--judged-current nil
  "The `current' id whose \"already shown\" judgment was last recorded at INFO.
Every roster push carries `current', and nearly every push names the
workspace already shown; recording that at INFO on each push would bury
the decisions.  The FIRST judgment of an id is INFO, its repeats DEBUG.")

(declare-function agent-repl-host-pending-selection-ref "host" ())
(declare-function agent-repl--input-save-unsent "input" (ws))
(declare-function agent-repl-host-release-confirmed "host" (revision))
(declare-function agent-repl-host-forget-confirming "host" (reason))
(declare-function agent-repl-host-ref "host" (ws))
(declare-function agent-repl--maybe-autoselect-input "panels" (ws))

(defvar agent-repl-host-last-request)

(defun agent-repl-roster--ms-since (time)
  "Return the whole milliseconds since TIME (a `float-time'), or \"none\"."
  (if time (round (* 1000 (- (float-time) time))) "none"))

(defun agent-repl-roster--current-scope (ws)
  "Return the log scope for a `current' decision about WS (nil: no tab)."
  (or ws '(:agent-repl-central
           "a selected roster row without a tab has no workspace sink")))

(defun agent-repl-roster-apply-current ()
  "Move the frame to the workspace the roster's `current' names, if needed.
THE ROSTER IS THE ONE SOURCE OF THE SELECTION.  Every switch -- a key, a
picker, a notification, a sidebar row, a merge-queue entry -- ends in a
SelectWorkspace, and the frame follows here when the last accepted roster
\(`agent-repl-roster-view') names a workspace other than the one shown.  A
switch Emacs requested, which every page's click is (SelectInEditor),
moves the frame at once as well
\(`agent-repl-host-request-switch'); the roster stays the authority, since
every push while that request is pending is deferred, and the answer
re-applies the roster, which moves the frame back if the daemon refused.
Returns the workspace switched to, or nil.

Takes no argument and reads the roster NOW, so any caller that has just
made a switch possible may call it: the roster push itself, a selection
being answered (host.el), a tab opening at startup.

Every decision is recorded at INFO -- an ignored user action must never
be invisible:

  - relink-pending: a relaunched daemon's arbitrary stamp is not obeyed
    while Emacs re-asserts the user's selection (host.el clears it);
  - stream-ended, unserved: the view did not come from the stream that
    stands now (`agent-repl-roster--held-revision'), so its `current' is
    another daemon's;
  - deferred: Emacs has REQUESTED another workspace that the daemon has
    not answered yet (`agent-repl-host-pending-selection-ref'); that answer
    re-applies the roster, so the last push wins once nothing is pending,
    and rapid presses do not walk the frame through every stop;
  - no tab yet: the workspace has no tab to land on; the tab's opener
    calls this again;
  - already shown;
  - held: a minibuffer is active, so the follow waits for its exit
    (`agent-repl--when-minibuffer-free');
  - followed: the switch, landing in the destination's input window; the
    record carries the push's arrival, the milliseconds since it, and the
    milliseconds since Emacs's own request for this workspace, if any.

A roster with no `current' decides nothing and says so at DEBUG."
  ;; An answered selection is released once the roster held carries it,
  ;; before the selection pending is read below.  Only a view the standing
  ;; stream served can carry it (`agent-repl-roster--held-revision').
  (let ((held (agent-repl-roster--held-revision)))
    (when held
      (agent-repl-host-release-confirmed held)))
  (let* ((roster agent-repl-roster-view)
         (id (and roster (agent-repl-roster--current-id roster)))
         ;; A workspace still held for its open has no tab to land on yet;
         ;; open-tabs.el calls this again once it is answered open.
         (name (and id (not (agent-repl-open-tabs-holds-p id))
                    (agent-repl--ws-by-ref-id id)))
         (scope (agent-repl-roster--current-scope name))
         (pending (agent-repl-host-pending-selection-ref))
         (pending-id (plist-get pending :id))
         (shown (agent-repl--ws-current-name)))
    (cond
     ((null id)
      (agent-repl--log '(:agent-repl-central "the roster names no selected workspace")
                       "elisp.roster.current: none")
      nil)
     ((bound-and-true-p agent-repl-host-reselect-pending)
      (agent-repl--info scope "elisp.roster.current: relink-pending id={id} dir={dir}"
                        :id id :dir agent-repl-host-reselect-pending)
      nil)
     ;; A VIEW WHOSE STREAM HAS ENDED DECIDES NOTHING.  The roster is the
     ;; selection's one source only while the stream that served the view
     ;; stands; once it has closed (`agent-repl-roster-on-close'), the view's
     ;; `current' is a daemon's that is gone, and following it moved the frame
     ;; off the user's own choice (2026-10-07).  The next daemon's roster
     ;; states the selection afresh after the link-up re-asserts it.
     ((null agent-repl-roster--stream)
      (agent-repl--info scope "elisp.roster.current: stream-ended id={id}; a view from a daemon that is gone is not followed"
                        :id id)
      nil)
     ;; NOR DOES ONE THE STANDING STREAM HAS NOT SERVED: a re-subscription
     ;; keeps the last daemon's view until the new stream's first accepted
     ;; push replaces it, and that push follows the selection itself.
     ((not (agent-repl-roster--held-revision))
      (agent-repl--info scope "elisp.roster.current: unserved id={id}; a view the standing stream has not served is not followed"
                        :id id)
      nil)
     ((and pending (not (equal id pending-id)))
      (agent-repl--info scope "elisp.roster.current: deferred id={id} ws={ws} requested={requested}"
                        :id id :ws (or name "none") :requested pending-id)
      nil)
     ((null name)
      (agent-repl--info scope "elisp.roster.current: no tab yet id={id}" :id id)
      nil)
     ((equal name shown)
      ;; Judged once at INFO, then at DEBUG: one site, its level the news.
      (let ((judged (equal id agent-repl-roster--judged-current)))
        (setq agent-repl-roster--judged-current id)
        (agent-repl--log-at (if judged 'debug 'info)
                            name "elisp.roster.current: already shown ws={ws}" :ws name))
      nil)
     (t
      ;; A MINIBUFFER HOLDS THE FOLLOW: moving the frame selects the
      ;; destination's input window, which would take the user out of the
      ;; prompt they are typing in.  The release re-applies the roster as it
      ;; stands THEN, so only the newest selection is followed.
      (let ((outcome (agent-repl--when-minibuffer-free
                      scope "roster-follow" 'roster-follow
                      (lambda (released)
                        (if released
                            (agent-repl-roster-apply-current)
                          (agent-repl-roster--follow name id shown))))))
        (unless (eq outcome :held) outcome))))))

(defun agent-repl-roster--follow (name id shown)
  "Move the frame from SHOWN to NAME, the roster\='s `current\' row ID.
The `followed\' decision of `agent-repl-roster-apply-current\', recorded at
INFO with the push\='s timing.  Returns NAME."
  (setq agent-repl-roster--judged-current id)
  (agent-repl--info name "elisp.roster.current: followed ws={ws} id={id} from={from} pushed-at={pushed_at} since-push-ms={since_push_ms} since-request-ms={since_request_ms}"
                    :ws name :id id :from shown
                    :pushed_at (if agent-repl-roster--pushed-at
                        (format-time-string "%T.%3N" agent-repl-roster--pushed-at)
                      "none")
                    :since_push_ms (agent-repl-roster--ms-since agent-repl-roster--pushed-at)
                    ;; Only a request for THIS workspace is the switch's origin;
                    ;; a sidebar click has no Emacs request behind it.
                    :since_request_ms (agent-repl-roster--ms-since
                     (and (equal (car agent-repl-host-last-request) name)
                          (cdr agent-repl-host-last-request))))
  (agent-repl-roster-move-frame name)
  name)

(defun agent-repl-roster-move-frame (name)
  "Move the frame to workspace NAME, landing in its input window.
The one way the frame moves to a selected workspace: the roster's follow
\(`agent-repl-roster-apply-current') and Emacs's own switch request, which
moves the frame at once rather than after the daemon's answer and push
\(`agent-repl-host-request-switch').  Bound as a follow, so the activation
is never counted as a second switch (`agent-repl-host--editor-switches')."
  (let ((agent-repl-roster-following t))
    (agent-repl--ws-switch name))
  ;; LAND IN THE INPUT WINDOW AT ONCE, before the next redisplay, rather
  ;; than only on the deferred switch pass: a frame redisplayed with the
  ;; webview selected hands the page a focus event and a repaint from
  ;; blank -- the flash keyboard switches showed and sidebar ones did not.
  (agent-repl--maybe-autoselect-input name))

;;;; ---- The finish edge --------------------------------------------------

(defun agent-repl-roster--finish-edge-p (previous current)
  "Return non-nil when PREVIOUS to CURRENT is the finish edge.
Both halves are required: a first sighting (PREVIOUS nil) is not an
edge, and neither is a move within the running set."
  (and (memq previous agent-repl-roster-running-statuses)
       (memq current agent-repl-roster-settled-statuses)
       t))

(defun agent-repl-roster--walk-status-transitions (entries fn)
  "Call FN once per row of ENTRIES with that row\='s status transition.
ENTRIES are the push\='s walk (`agent-repl-roster-walk'), taken once by
`agent-repl-roster-apply' for every reader of the push.

FN receives a plist `(:row ROW :id ID :ws WS :scope SCOPE :previous
PREVIOUS :current CURRENT)\=', where PREVIOUS is the arm the last accepted
push carried for the row (nil on a first sighting), WS is the tab\='s
workspace or nil when no tab exists for the id yet, and SCOPE is the log
sink to attribute a record about the row to.

EVERY ROW'S WS IS READ FROM THE REGISTRY AS IT STOOD WHEN THE WALK BEGAN
\(`agent-repl--ws-ref-id-table'), one scan for the whole push rather than
one per row: the push is applied against one registry, and no FN opens or
closes a tab.

THE ONE PLACE A PUSH IS COMPARED AGAINST THE ONE BEFORE IT.  Both
readers of that comparison — the FINISH EDGE and the STATUS CHANGE —
walk through here, so there is one answer to \"what changed in this
push\" rather than two tables drifting apart.  It must run BEFORE
`agent-repl-roster--record-statuses\=', which is what makes this push\='s
arms the next push\='s PREVIOUS."
  (let ((names (agent-repl--ws-ref-id-table)))
    (dolist (entry entries)
      (let* ((row (plist-get entry :row))
             (id (agent-repl-roster-row-id row))
             (current (agent-repl-roster-row-status row))
             (previous (gethash id agent-repl-roster--status-by-id))
             (ws (gethash id names))
             (scope (if ws
                        ws
                      '(:agent-repl-central
                        "a roster row without a tab has no workspace sink"))))
        (funcall fn (list :row row :id id :ws ws :scope scope
                          :previous previous :current current))))))

(defun agent-repl-roster--run-finish-edges (entries)
  "Run `agent-repl-roster-finish-functions' for every finish edge in ENTRIES.
Returns the workspaces whose rows crossed the edge."
  (let ((fired nil))
    (agent-repl-roster--walk-status-transitions
     entries
     (lambda (transition)
       (let ((id (plist-get transition :id))
             (ws (plist-get transition :ws))
             (scope (plist-get transition :scope))
             (previous (plist-get transition :previous))
             (current (plist-get transition :current)))
         (if (agent-repl-roster--finish-edge-p previous current)
             (if ws
                 (progn
                   (agent-repl--info ws "elisp.roster.finish-edge: ws={ws} from={from} to={to}"
                                     :ws ws :from previous :to current)
                   (push ws fired)
                   (run-hook-with-args 'agent-repl-roster-finish-functions ws))
               (agent-repl--log scope "elisp.roster.finish-edge: no tab id={id} from={from} to={to}"
                                :id id :from previous :to current))
           ;; A row whose arm did not move is no change, and every push walks
           ;; every row: only a row that moved is recorded.
           (unless (equal previous current)
             (agent-repl--log scope "elisp.roster.status: id={id} from={from} to={to} edge=nil"
                              :id id :from previous :to current))))))
    (nreverse fired)))

(defun agent-repl-roster--record-drawn-statuses (entries)
  "Record, at INFO, every row of ENTRIES whose drawn status arm changed.

The tab paints a workspace from the arm this push carries, so a row whose
arm differs from the one the last accepted push left — a first sighting
included — is a tab now drawing something else.  That change is stated on
the record, with the arm it replaced, so the tab\='s drawn status can be
read back beside the footer\='s and the rail\='s (owner\='s report,
2026-10-08: the footer and the tab disagreed, and only the daemon\='s side
of it was on the record).  A row whose arm did not move says nothing.

Must run BEFORE `agent-repl-roster--record-statuses\=', like every reader
of the comparison."
  (agent-repl-roster--walk-status-transitions
   entries
   (lambda (transition)
     (let ((previous (plist-get transition :previous))
           (current (plist-get transition :current)))
       (unless (eq previous current)
         (agent-repl--info (plist-get transition :scope)
                           "elisp.roster.drawn-status: id={id} from={from} to={to}"
                           :id (plist-get transition :id) :from previous :to current))))))

(defun agent-repl-roster--run-status-changes (entries)
  "Run `agent-repl-roster-status-change-functions' per changed arm in ENTRIES.

A CHANGE is a row whose arm differs from the arm the last accepted push
carried for it.  A FIRST SIGHTING is not a change — there is no previous
status for the row to have moved away from — which is exactly the rule
the daemon applies to the same edge, so the two cannot disagree about
what counts as new activity.

Returns the workspaces whose status changed."
  (let ((changed nil))
    (agent-repl-roster--walk-status-transitions
     entries
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
                                 "elisp.roster.viewed-cleared: no tab id={id}" :id id)
              (agent-repl--log ws "elisp.roster.viewed-cleared: ws={ws} id={id}" :ws ws :id id)
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

(defun agent-repl-roster-echo-finished (ws)
  "Reaction (1): the cross-workspace echo for WS.
Only when WS is NOT the selected tab: a user standing in the workspace
already has the footer's activity line telling them."
  (if (agent-repl--current-ws-p ws)
      (agent-repl--log ws "elisp.roster.echo: skipped ws={ws} reason=selected" :ws ws)
    (agent-repl--log ws "elisp.roster.echo: ws={ws}" :ws ws)
    (message "Agent finished in workspace: %s" ws)))

(defun agent-repl-roster-refresh-magit (ws)
  "Reaction (2): refresh any magit-status buffer on WS's directory.
The turn ended, so the worktree the user is looking at has probably
changed underneath their status buffer."
  (agent-repl--refresh-magit-status-for-dir (agent-repl--ws-get ws :dir) ws))

(add-hook 'agent-repl-roster-finish-functions #'agent-repl-roster-echo-finished)
(add-hook 'agent-repl-roster-finish-functions #'agent-repl-roster-refresh-magit)

;;;; ---- The push ---------------------------------------------------------

(defun agent-repl-roster--index (entries)
  "Replace the row index from ENTRIES, keyed by ref id."
  (clrhash agent-repl-roster--rows-by-id)
  (dolist (entry entries)
    (let ((row (plist-get entry :row)))
      (puthash (agent-repl-roster-row-id row) row agent-repl-roster--rows-by-id))))

(defun agent-repl-roster--held-revision ()
  "Return the revision of the view the STANDING stream served, or nil.
Nil when no view is held, when no stream stands, or when the view came
from a stream that is no longer the standing one
\(`agent-repl-roster--view-stream'): revisions are one daemon\='s count,
so another stream\='s revision says nothing about this one\='s.  The one
place a held revision is read, for the selection
\(`agent-repl-roster-apply-current') and the answer rows
\(`agent-repl-roster-note-row') alike."
  (and agent-repl-roster-view
       agent-repl-roster--stream
       (eq agent-repl-roster--view-stream agent-repl-roster--stream)
       (or (plist-get agent-repl-roster-view :revision) 0)))

(defun agent-repl-roster-note-row (row revision)
  "Index ROW, taken from the daemon's roster REVISION, when it is newer.
An OpenTabs answer carries the row as of the answer (open-tabs.el), so the
tab it opens is drawn with its true status rather than the last push's.
The answer and the roster ride separate streams: a row from a revision the
last applied roster already reached is older or the same, and indexing it
would roll a status back, so it is kept out.  A view the standing stream
did not serve reached nothing (`agent-repl-roster--held-revision').
Answers t when indexed."
  (let ((held (or (agent-repl-roster--held-revision) 0)))
    (if (<= revision held)
        (progn
          (agent-repl--log '(:agent-repl-central "roster reconciliation spans every workspace")
                           "elisp.roster.answer-row-kept: id={id} revision={revision} held={held}"
                           :id (agent-repl-roster-row-id row) :revision revision :held held)
          nil)
      (puthash (agent-repl-roster-row-id row) row agent-repl-roster--rows-by-id)
      (agent-repl--info '(:agent-repl-central "roster reconciliation spans every workspace")
                        "elisp.roster.answer-row-indexed: id={id} revision={revision} held={held}"
                        :id (agent-repl-roster-row-id row) :revision revision :held held)
      t)))

(defun agent-repl-roster--record-statuses (entries)
  "Replace the previous-status table from ENTRIES, keyed by ref id."
  (clrhash agent-repl-roster--status-by-id)
  (dolist (entry entries)
    (let ((row (plist-get entry :row)))
      (puthash (agent-repl-roster-row-id row)
               (agent-repl-roster-row-status row)
               agent-repl-roster--status-by-id))))

(defun agent-repl-roster-apply (roster &optional stream)
  "Apply ROSTER: index it, reconcile the tabs, fire the edges, run the hooks.
STREAM is the stream that served ROSTER, nil naming the standing one; it
is recorded as the view\='s (`agent-repl-roster--view-stream').
A push that DECODES but violates an invariant this file relies on — two
rows sharing one ref id, or a tab numbered out of its place
\(`agent-repl-roster--misnumbered') — is logged at ERROR and DROPPED
whole: the tab identity or its number would be ambiguous, and
half-applying it would be worse than ignoring it.  Malformed pushes
never reach here at all; rpc.el drops those.  Returns the tab names in
order, or nil when the push was dropped."
  (let* ((entries (agent-repl-roster-walk roster))
         (duplicate (agent-repl-roster--duplicate-id entries))
         (misnumbered (and (not duplicate) (agent-repl-roster--misnumbered entries))))
    (cond
     (duplicate
      (agent-repl--error '(:agent-repl-central
                           "duplicate roster identity is ambiguous across workspaces")
                         "elisp.roster.push: dropped reason=duplicate-ref-id id={id} rows={rows}"
                         :id duplicate :rows (length entries))
      nil)
     (misnumbered
      (agent-repl--error '(:agent-repl-central
                           "a tab numbered out of its place would number one workspace two ways")
                         "elisp.roster.push: dropped reason=misnumbered-tab id={id} number={number}"
                         :id (agent-repl-roster-row-id (plist-get misnumbered :row))
                         :number (agent-repl-roster-tab-number (plist-get misnumbered :tab)))
      nil)
     (t
      (agent-repl-roster--index entries)
      (setq agent-repl-roster-view roster
            agent-repl-roster--view-stream (or stream agent-repl-roster--stream)
            agent-repl-roster--view-address (and agent-repl-roster--view-stream
                                                 (gethash agent-repl-roster--view-stream
                                                          agent-repl-roster--stream-addresses))
            agent-repl-roster--pushed-at (float-time))
      (let ((order (agent-repl-roster-reconcile roster)))
        ;; THE SELECTION FIRST, right after the tabs it needs exist: the edge
        ;; and status hooks below repaint and notify, and a switch the user
        ;; is waiting on must not queue behind them.
        (agent-repl-roster-apply-current)
        (agent-repl-roster--run-finish-edges entries)
        ;; BEFORE the statuses are recorded: this compares against the arms
        ;; the LAST push left behind, which the record below replaces.
        (agent-repl-roster--run-status-changes entries)
        (agent-repl-roster--record-drawn-statuses entries)
        (agent-repl-roster--record-statuses entries)
        ;; Same rule for the viewed marker: edge first, then record.
        (agent-repl-roster--run-viewed-clears entries)
        (agent-repl-roster--record-viewed entries)
        (run-hook-with-args 'agent-repl-roster-update-functions roster)
        (agent-repl--log '(:agent-repl-central "a roster push spans every workspace")
                         "elisp.roster.push: applied rows={rows} tabs={tabs}"
                         :rows (length entries) :tabs (length order))
        order)))))

(defun agent-repl-roster--assemble (streams)
  "Return the one roster plist STREAMS, `agent-repl-roster--streams', assemble.
The workspaces stream\='s keys, then `:closed', `:merge-queue',
`:recently-merged' and the push\='s `:revision' (`agent-repl-roster-view')."
  (append (list :revision (plist-get streams :revision)
                :closed (plist-get streams :closed)
                :merge-queue (plist-get streams :merge-queue)
                :recently-merged (plist-get streams :recently-merged))
          (plist-get streams :workspaces)))

(defun agent-repl-roster-apply-push (push &optional stream)
  "Apply PUSH, a decoded `RosterPush' received on STREAM (nil: the standing one).
NEVER A DIFF: each stream PUSH carries REPLACES the held snapshot whole,
and a stream it leaves out is UNCHANGED, never empty.  The held snapshots
\(`agent-repl-roster--streams') are this subscription\='s, so a push on a
subscription that has delivered none yet is its FIRST, and the contract
says a first push carries every stream.  A first push missing one is a
contract breach: it is logged at ERROR and REFUSED whole, the snapshots
and the view untouched, never half-applied.

A complete push replaces the snapshots, and their assembly
\(`agent-repl-roster--assemble') is then applied as one roster
\(`agent-repl-roster-apply'), whose own invariants may still drop it.
Returns what `agent-repl-roster-apply' returns, or nil when refused."
  (let* ((stream (or stream agent-repl-roster--stream))
         (held (and agent-repl-roster--streams
                    (eq (plist-get agent-repl-roster--streams :stream) stream)
                    agent-repl-roster--streams))
         (missing (and (null held)
                       (seq-remove (lambda (key) (plist-get push key))
                                   agent-repl-roster-stream-keys))))
    (if missing
        (progn
          (agent-repl--error '(:agent-repl-central
                               "a subscription's first roster push must carry every stream")
                             "elisp.roster.push: refused reason=first-push-incomplete missing={missing} revision={revision}"
                             :missing (prin1-to-string missing) :revision (plist-get push :revision))
          nil)
      (let ((streams (list :stream stream :revision (plist-get push :revision))))
        (dolist (key agent-repl-roster-stream-keys)
          (setq streams (plist-put streams key (or (plist-get push key)
                                                   (plist-get held key)))))
        (setq agent-repl-roster--streams streams)
        (agent-repl--log '(:agent-repl-central "a roster push spans every workspace")
                         "elisp.roster.push: streams revision={revision} carried={carried}"
                         :revision (plist-get push :revision) :carried (prin1-to-string (seq-filter (lambda (key) (plist-get push key))
                                     agent-repl-roster-stream-keys)))
        (agent-repl-roster-apply (agent-repl-roster--assemble streams) stream)))))

(defvar agent-repl-roster--ending-stream nil
  "The roster stream that carried the planned ending, or nil.
Its daemon is standing down in a PLANNED exit and the frame was the
stream\='s last (`DaemonStreamEnding'), so the clean end that follows is
recorded at INFO rather than as a stream the producer dropped.")

(defun agent-repl-roster-on-push (push &optional stream)
  "Handle one decoded WatchWorkspaceRoster PUSH received on STREAM.
THE ARM IS WHAT THE FRAME CARRIES: `:streams' is a push of the roster\='s
streams (`agent-repl-roster-apply-push'), and `:ending' marks STREAM (nil:
the standing one) as ending on purpose.  Any other arm is a contract
breach, recorded at ERROR and not applied."
  (pcase (plist-get push :arm)
    (:streams
     (let ((request-id (agent-repl--next-log-request-id)))
       (agent-repl--with-log-context
        agent-repl--global-log-scope request-id
        (lambda () (agent-repl-roster-apply-push (plist-get push :value) stream)))))
    (:ending
     (setq agent-repl-roster--ending-stream (or stream agent-repl-roster--stream))
     (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                       "elisp.roster.stream-ending"))
    (arm
     (agent-repl--error '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.unknown-push arm={arm} push={push}" :arm (prin1-to-string arm) :push (prin1-to-string push)))))

(defvar agent-repl-roster--accepted nil
  "The roster stream whose subscription the daemon ACCEPTED, or nil.
Only a stream that was accepted and then lost is re-subscribed at once:
one that died before its acceptance met a daemon that is not answering,
and re-dialing it from its own close would loop until the link\='s own
edge said so.")

(defun agent-repl-roster-on-close (reason &optional stream)
  "Handle the roster STREAM closing for REASON.
A cancel is Emacs's own graceful close.  A clean end after the stream
carried the planned ending (`agent-repl-roster--ending-stream') is the
daemon standing down on purpose: it is recorded at INFO and the roster
follows the live daemon.  Anything else is the producer dropping a
STANDING stream: it is recorded at ERROR, and the roster FOLLOWS THE
LIVE DAEMON (`agent-repl-roster--follow-live-daemon').

A close of a stream that is no longer the standing one is STALE: a
re-subscription already replaced it, and forgetting the stream that
stands now would leave the tabs with no roster at all.  STREAM nil means
the caller did not name the stream."
  (cond
   ((and stream (not (eq stream agent-repl-roster--stream)))
    (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                     "elisp.roster.stale-stream-close: reason={reason}" :reason (prin1-to-string reason)))
   (t
    (let ((closing (or stream agent-repl-roster--stream))
          (departing agent-repl-roster--stream-conn))
      (setq agent-repl-roster--stream nil
            agent-repl-roster--stream-conn nil)
      (agent-repl-host-forget-confirming "the roster stream closed")
      (pcase (car-safe reason)
        (:cancelled (agent-repl--log '(:agent-repl-central "the roster stream spans workspaces")
                                      "elisp.roster.stream-close: reason=cancelled"))
        ((and :ended (guard (and closing (eq closing agent-repl-roster--ending-stream))))
         ;; THE DAEMON SAID SO FIRST: the stream's last frame was the planned
         ;; ending, so this is a stand-down, not a fault -- INFO, and the same
         ;; follow of the live daemon a loss takes.
         (setq agent-repl-roster--ending-stream nil)
         (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                           "elisp.roster.stream-close: reason=planned-ending")
         (agent-repl-roster--follow-live-daemon stream departing))
        (:ended (agent-repl--error '(:agent-repl-central "the roster stream spans workspaces")
                                    "elisp.roster.stream-close: reason=ended-without-cancel — a standing stream the producer ended")
                (agent-repl-roster--follow-live-daemon stream))
        (_ (agent-repl--error '(:agent-repl-central "the roster stream spans workspaces")
                               "elisp.roster.stream-close: reason={reason}" :reason (prin1-to-string reason))
           (agent-repl-roster--follow-live-daemon stream)))))))

(defun agent-repl-roster--follow-live-daemon (lost &optional planned-on)
  "Re-subscribe the roster on the LIVE daemon after the stream LOST died.
PLANNED-ON is the connection LOST rode when it ended PLANNED, else nil.
Regression, 2026-09-27: a daemon exited under a standing roster stream
and nothing re-subscribed it unless the link itself went down or was
promoted -- the roster was left to those edges alone.  The live daemon is
the link\='s (`agent-repl-link-live'), never an address this file keeps:

  - no link stands: the link-up edge subscribes the roster
    (`agent-repl-roster-on-link-up'), and nothing polls meanwhile;
  - LOST was never accepted: the daemon it dialed is not answering, and
    the link\='s own edge (down then up, or a promotion) re-subscribes;
  - the live daemon announced its OWN ending -- LOST ended planned on it,
    or the link\='s `WatchDaemon' carried the ending
    (`agent-repl-link-departing-p'): it is leaving, and the link\='s own edge
    re-subscribes on whatever serves next;
  - otherwise the roster is re-subscribed on the live daemon now.

Regression, 2026-10-03: a bounce\='s announced stand-down ended the roster
stream before the link\='s own, and the roster re-subscribed on the very
daemon that had just said it was leaving: `elisp.connect.dial-failed',
`elisp.connect.stream-error' and `elisp.roster.stream-close' at ERROR."
  (let ((live (and (fboundp 'agent-repl-link-live) (agent-repl-link-live))))
    (cond
     ((null live)
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribe-awaiting-link"))
     ((not (and lost (eq lost agent-repl-roster--accepted)))
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribe-awaiting-link-edge reason=never-accepted address={address}"
                        :address (prin1-to-string (agent-repl-connect-connection-address live))))
     ((and (fboundp 'agent-repl-link-departing-p)
           (agent-repl-link-departing-p live planned-on))
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribe-awaiting-link-edge reason=daemon-departing address={address}"
                        :address (prin1-to-string (agent-repl-connect-connection-address live))))
     (t
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.resubscribed-on-loss address={address}"
                        :address (prin1-to-string (agent-repl-connect-connection-address live)))
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
           conn (lambda (push) (agent-repl-roster-on-push push stream))
           (lambda (reason) (agent-repl-roster-on-close reason stream))
           (lambda ()
             (setq agent-repl-roster--accepted stream)
             ;; THE STREAM IS KEYED TO ITS DAEMON AT ITS ACCEPTANCE, before
             ;; any push it carries (`agent-repl-roster--view-address').
             (puthash stream (prin1-to-string (agent-repl-connect-connection-address conn))
                      agent-repl-roster--stream-addresses)
             (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                               "elisp.roster.subscribed method={method} address={address}"
                               :method (prin1-to-string "WatchWorkspaceRoster")
                               :address (prin1-to-string (agent-repl-connect-connection-address conn))))))
    (setq agent-repl-roster--stream stream
          agent-repl-roster--stream-conn conn))
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
  (setq agent-repl-roster--stream nil
        agent-repl-roster--stream-conn nil)
  (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                    "elisp.roster.link-down: stream forgotten view-kept={view_kept}"
                    :view_kept (if agent-repl-roster-view "t" "nil")))

(defun agent-repl-roster-on-link-handover (_old new)
  "Move the roster onto NEW, the successor daemon, the moment it is ACCEPTED.
Registered on `agent-repl-link-handover-functions'.

THE ROSTER IS DAEMON-WIDE, SO IT FOLLOWS THE NEWEST DAEMON, NOT THE LAST
ONE TO EXIT.  Per-workspace streams stay on whichever daemon still serves
each workspace, but the roster rides one connection, and it used to stay
on the outgoing daemon until that daemon dropped its stream.  A handover
that never finished never dropped it: on 2026-10-12 the outgoing daemon
kept one workspace for nine hours, a relaunched Emacs attached to it, and
every roster it pushed carried a field the deploy\='s own elisp had
retired -- each push was refused whole, and no workspace loaded.  The
successor is the daemon the deploy reloaded this Emacs for, and both
read the one registry, so its roster is the one this Emacs can read."
  (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                    "elisp.roster.resubscribed-on-handover address={address}"
                    :address (prin1-to-string (agent-repl-connect-connection-address new)))
  (agent-repl-roster-subscribe new))

(defun agent-repl-roster-on-link-promote (_old new)
  "Re-subscribe the roster on NEW when the successor is PROMOTED to primary.
A promotion fires no up hooks -- by design, because every workspace was
already adopted onto the successor and re-registering the fleet would be
a lie.  The ROSTER is not adopted, though: a stream that rode the OLD
connection dies with it, so without this re-subscription Emacs comes out
of every blue-green rollout with no roster stream at all and the tabs stop
reconciling.  A roster already moved onto NEW at the handover
\=(`agent-repl-roster-on-link-handover') is left standing."
  (if (and agent-repl-roster--stream (eq agent-repl-roster--stream-conn new))
      (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                        "elisp.roster.promotion-kept-stream address={address}"
                        :address (prin1-to-string (agent-repl-connect-connection-address new)))
    (agent-repl--info '(:agent-repl-central "the roster stream spans workspaces")
                      "elisp.roster.resubscribed-on-promotion address={address}"
                      :address (prin1-to-string (agent-repl-connect-connection-address new)))
    (agent-repl-roster-subscribe new)))

(add-hook 'agent-repl-link-up-functions #'agent-repl-roster-on-link-up)
(add-hook 'agent-repl-link-promote-functions #'agent-repl-roster-on-link-promote)
(add-hook 'agent-repl-link-handover-functions #'agent-repl-roster-on-link-handover)
(add-hook 'agent-repl-link-down-functions #'agent-repl-roster-on-link-down)

(provide 'agent-repl-roster)
;;; roster.el ends here
