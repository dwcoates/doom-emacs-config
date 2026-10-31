;;; wire-roster.el --- protojson codec for the workspace roster -*- lexical-binding: t; -*-

;;; Commentary:

;; THE SIDEBAR'S STREAM, DECODED.  WatchWorkspaceRoster is THE ONE GLOBAL
;; STREAM: it carries no workspace, every client watches the same roster, and
;; the daemon's stream order is the only order.  Its response wraps
;; `frontend.v1.WorkspaceRoster' whole.
;;
;; THE MESSAGE TREE IS THE UI TREE, and the decode preserves it exactly:
;; every drawn box is one message and every message is one plist, nesting
;; equal.  Nothing is flattened for convenience — a consumer that wants the
;; row's `WorkspaceRef' reaches through `RosterRowWorkspace', because that is
;; where the contract puts it, and a flattening here would be a second,
;; drifting spelling of the tree.
;;
;; Emacs decodes this stream but NEVER builds one: the sidebar is
;; daemon-resolved and Emacs's contribution to it is two verbs elsewhere
;; (RegisterWorkspace, SelectWorkspace).
;;
;; The shared primitives and the elisp shape live in `wire-common.el'.

;;; Code:

;; Cross-file forward declarations.  These sources load in the dependency
;; order config.el establishes and resolve each other's calls at call time,
;; so the declarations below exist for the byte-compiler alone.
(declare-function agent-repl-wire--check-keys "wire-common")
(declare-function agent-repl-wire--decode-bool "wire-common")
(declare-function agent-repl-wire--decode-empty "wire-common")
(declare-function agent-repl-wire--decode-int64 "wire-common")
(declare-function agent-repl-wire--decode-uint32 "wire-common")
(declare-function agent-repl-wire--decode-uint64 "wire-common")
(declare-function agent-repl-wire--decode-message "wire-common")
(declare-function agent-repl-wire--decode-oneof "wire-common")
(declare-function agent-repl-wire--decode-optional-message "wire-common")
(declare-function agent-repl-wire--decode-optional-string "wire-common")
(declare-function agent-repl-wire--decode-optional-int64 "wire-common")
(declare-function agent-repl-wire--decode-repeated "wire-common")
(declare-function agent-repl-wire--decode-string "wire-common")
(declare-function agent-repl-wire--decoded "wire-common")
(declare-function agent-repl-wire-decode-daemon-stream-ending "wire-common")
(declare-function agent-repl-wire-decode-feed-id "wire-common")
(declare-function agent-repl-wire--encode-empty "wire-common")
(declare-function agent-repl-wire--encoded "wire-common")
(declare-function agent-repl-wire--fail "wire-common")
(declare-function agent-repl-wire--object "wire-common")
(declare-function agent-repl-wire--present-p "wire-common")
(declare-function agent-repl-wire--raw "wire-common")
(declare-function agent-repl-wire-decode-repository-ref "wire-common")
(declare-function agent-repl-wire-decode-workspace-ref "wire-common")

;; wire-common.el is loaded immediately before this file by config.el's
;; module list; that ordering is the dependency, as it is everywhere else in
;; this module (no file here `require's a sibling).

;;;; ---- Leaves ----

(defun agent-repl-wire-decode-roster-label (value)
  "Decode VALUE as `RosterLabel', a plist `(:text)'.
DISPLAY ONLY — the repo key or task id beside the header is the identity."
  (let ((object (agent-repl-wire--object "RosterLabel" value)))
    (agent-repl-wire--check-keys "RosterLabel" object '(text))
    (agent-repl-wire--decoded
     "RosterLabel"
     (list :text (agent-repl-wire--decode-string "RosterLabel" 'text object)))))

(defun agent-repl-wire-decode-roster-section-count (value)
  "Decode VALUE as `RosterSectionCount', a plist `(:workspaces)'.
How many workspaces the section holds; the webview draws it while folded."
  (let ((object (agent-repl-wire--object "RosterSectionCount" value)))
    (agent-repl-wire--check-keys "RosterSectionCount" object '(workspaces))
    (agent-repl-wire--decoded
     "RosterSectionCount"
     (list :workspaces (agent-repl-wire--decode-uint32
                        "RosterSectionCount" 'workspaces object)))))

(defun agent-repl-wire-decode-roster-row-name (value)
  "Decode VALUE as `RosterRowName', a plist `(:text)'.
Never an identity or a join key: names collide across repos."
  (let ((object (agent-repl-wire--object "RosterRowName" value)))
    (agent-repl-wire--check-keys "RosterRowName" object '(text))
    (agent-repl-wire--decoded
     "RosterRowName"
     (list :text (agent-repl-wire--decode-string "RosterRowName" 'text object)))))

(defun agent-repl-wire-decode-roster-row-current (value)
  "Decode VALUE as `RosterRowCurrent', a plist `(:current)'.
Redundant with a comparison against the roster's `current' and
deliberately so: the resolver states it once."
  (let ((object (agent-repl-wire--object "RosterRowCurrent" value)))
    (agent-repl-wire--check-keys "RosterRowCurrent" object '(current))
    (agent-repl-wire--decoded
     "RosterRowCurrent"
     (list :current (agent-repl-wire--decode-bool
                     "RosterRowCurrent" 'current object)))))

(defun agent-repl-wire-decode-roster-row-closed (value)
  "Decode VALUE as `RosterRowClosed', a plist `(:closed)'.
Closed is ORTHOGONAL to the lifecycle, which is why it is not a status arm."
  (let ((object (agent-repl-wire--object "RosterRowClosed" value)))
    (agent-repl-wire--check-keys "RosterRowClosed" object '(closed))
    (agent-repl-wire--decoded
     "RosterRowClosed"
     (list :closed (agent-repl-wire--decode-bool
                    "RosterRowClosed" 'closed object)))))

(defun agent-repl-wire-decode-roster-row-priority-badge (value)
  "Decode VALUE as `RosterRowPriorityBadge', a plist `(:label)'.
The DRAWN label only — ordering is already the resolver's."
  (let ((object (agent-repl-wire--object "RosterRowPriorityBadge" value)))
    (agent-repl-wire--check-keys "RosterRowPriorityBadge" object '(label))
    (agent-repl-wire--decoded
     "RosterRowPriorityBadge"
     (list :label (agent-repl-wire--decode-string
                   "RosterRowPriorityBadge" 'label object)))))

(defun agent-repl-wire-decode-roster-row-attention (value)
  "Decode VALUE as `RosterRowAttention' and return t.
The message is EMPTY and PRESENCE IS THE FACT, so the decoded value must
be something other than nil — nil is how the optional field's absence is
spelled, and an empty message decodes to nil everywhere else."
  (agent-repl-wire--decode-empty "RosterRowAttention" value)
  t)

(defun agent-repl-wire-decode-roster-row-viewed (value)
  "Decode VALUE as `RosterRowViewed' and return t.
The DISPLAY MODE marker: present is PARTIAL and absent is FULL.  The
message is EMPTY and PRESENCE IS THE FACT, exactly as for the attention
marker above, so the decoded value must be something other than nil.

EMACS DRAWS FROM IT.  The daemon is the single source of the mode: the
tab bar reads it back (`agent-repl-roster-viewed-for-ws') exactly as the
webapp sidebar does, so the two surfaces cannot disagree.  Emacs only
REPORTS the dwell that raises it (`agent-repl--tab-view-partial'); the
daemon also raises it by itself when a /clear or compaction completes."
  (agent-repl-wire--decode-empty "RosterRowViewed" value)
  t)

(defconst agent-repl-wire-roster-row-idle-async-unread-arms
  '((done :done agent-repl-wire-decode-roster-row-status-done)
    (interrupted :interrupted agent-repl-wire-decode-roster-row-status-interrupted)
    (turnFailed :turn-failed agent-repl-wire-decode-roster-row-status-turn-failed))
  "`RosterRowIdleAsyncUnread.turn_end''s arm table: (WIRE-KEY ARM-KEYWORD DECODER).
Each arm is the turn end's own status arm message, so its keyword is the
status arm keyword the tab bar\='s palette is keyed by.")

(defun agent-repl-wire-decode-roster-row-idle-async-unread (value)
  "Decode VALUE as `RosterRowIdleAsyncUnread' and return the turn end\='s arm.
The UNREAD-TURN-END marker of an `idle_async' row: the result of that turn
end stands unread under the background.  Returns `:done', `:interrupted'
or `:turn-failed', the status arm keyword the turn end draws once the
background ends.  An unset oneof is refused: the producer always names
the turn end, and a color guessed here would be a second answer to a
question only the daemon answers."
  (let ((object (agent-repl-wire--object "RosterRowIdleAsyncUnread" value)))
    (agent-repl-wire--check-keys
     "RosterRowIdleAsyncUnread" object
     (mapcar #'car agent-repl-wire-roster-row-idle-async-unread-arms))
    (plist-get (agent-repl-wire--decode-oneof
                "RosterRowIdleAsyncUnread" 'turn_end object
                agent-repl-wire-roster-row-idle-async-unread-arms)
               :arm)))

(defun agent-repl-wire-decode-roster-row-reviving (value)
  "Decode VALUE as `RosterRowReviving' and return t.
The REVIVING marker: the daemon is bringing this parked workspace's
session back up.  EMPTY, and PRESENCE IS THE FACT, as for the markers
above.

EMACS DECODES IT AND DRAWS NOTHING FROM IT.  The marker is the webapp
sidebar's shimmer and not a status arm, so the tab-bar's status (which
is the arm) is unaffected.  It is decoded because the codec refuses
unknown fields, and refusing this one would drop every roster push."
  (agent-repl-wire--decode-empty "RosterRowReviving" value)
  t)

(defun agent-repl-wire-decode-roster-row-detached-live (value)
  "Decode VALUE as `RosterRowDetachedLive' and return t.
The DETACHED-WORK marker: detached (background) work runs in this
workspace right now, on whatever status arm the row stands.  EMPTY, and
PRESENCE IS THE FACT, as for the markers above.  Emacs draws nothing
from it: it is decoded because the codec refuses unknown fields."
  (agent-repl-wire--decode-empty "RosterRowDetachedLive" value)
  t)

(defun agent-repl-wire-decode-roster-row-one-shot (value)
  "Decode VALUE as `RosterRowOneShot' and return t.
The ONE-SHOT mark: the workspace was created as a one-shot.  EMPTY, and
PRESENCE IS THE FACT, as for the markers above.  Which status arms draw
its glyph is the shared vocabulary's `one_shot_glyph_arms'."
  (agent-repl-wire--decode-empty "RosterRowOneShot" value)
  t)

(defun agent-repl-wire-decode-roster-row-async-merging (value)
  "Decode VALUE as `RosterRowAsyncMerging' and return t.
The ASYNC-MERGING marker: the workspace\='s own branch merges async in a
hidden merge workspace, on whatever status arm the row stands.  EMPTY,
and PRESENCE IS THE FACT, as for the markers above.  The tab bar paints
its [N] run with the shared vocabulary\='s `async_merge_mark\='."
  (agent-repl-wire--decode-empty "RosterRowAsyncMerging" value)
  t)

(defun agent-repl-wire-decode-roster-row-merge-paused (value)
  "Decode VALUE as `RosterRowMergePaused' and return t.
The MERGE-PAUSED marker: the merge queue holding the row\='s merge is
paused.  EMPTY, and PRESENCE IS THE FACT, as for the markers above.

EMACS DECODES IT AND DRAWS NOTHING FROM IT.  The marker inks amber the
webapp sidebar\='s merge indicators, and the tab bar is outside that
ruling.  It is decoded because the codec refuses unknown fields, and
refusing this one would drop every roster push."
  (agent-repl-wire--decode-empty "RosterRowMergePaused" value)
  t)

(defun agent-repl-wire-decode-roster-row-opening (value)
  "Decode VALUE as `RosterRowOpening' and return t.
The OPENING marker: the editor is opening this workspace, whose status
is `inactive' while it stands.  EMPTY, and PRESENCE IS THE FACT, as for
the markers above.  Emacs draws nothing from it: it is the webapp
sidebar's not-selectable row, decoded because the codec refuses unknown
fields."
  (agent-repl-wire--decode-empty "RosterRowOpening" value)
  t)

(defun agent-repl-wire-decode-roster-row-availability-pending (value)
  "Decode VALUE as the empty `RosterRowAvailabilityPending'.
The daemon is still bringing the session up: the workspace is not opened."
  (agent-repl-wire--decode-empty "RosterRowAvailabilityPending" value))

(defun agent-repl-wire-decode-roster-row-availability-available (value)
  "Decode VALUE as the empty `RosterRowAvailabilityAvailable'.
The workspace's shim link has connected: the workspace is opened."
  (agent-repl-wire--decode-empty "RosterRowAvailabilityAvailable" value))

(defun agent-repl-wire-decode-roster-row-availability-unavailable (value)
  "Decode VALUE as the empty `RosterRowAvailabilityUnavailable'.
The bring-up ended without a session: the workspace is opened and draws
its start failure."
  (agent-repl-wire--decode-empty "RosterRowAvailabilityUnavailable" value))

(defconst agent-repl-wire-roster-row-availability-arms
  '((pending :pending agent-repl-wire-decode-roster-row-availability-pending)
    (available :available agent-repl-wire-decode-roster-row-availability-available)
    (unavailable :unavailable agent-repl-wire-decode-roster-row-availability-unavailable))
  "`RosterRowAvailability.availability''s arm table.
Each entry is (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-roster-row-availability (value)
  "Decode VALUE as `RosterRowAvailability', a plist `(:arm ARM :value nil)'.
Whether the daemon has this workspace's session to offer: the roster opens
a workspace's tab only once its row leaves `:pending'.  AN UNSET ONEOF IS
INVALID and is rejected loudly, as for the status."
  (let ((object (agent-repl-wire--object "RosterRowAvailability" value)))
    (agent-repl-wire--check-keys "RosterRowAvailability" object
                                 (mapcar #'car agent-repl-wire-roster-row-availability-arms))
    (agent-repl-wire--decoded
     "RosterRowAvailability"
     (agent-repl-wire--decode-oneof
      "RosterRowAvailability" 'availability object
      agent-repl-wire-roster-row-availability-arms))))

(defun agent-repl-wire-roster--decode-instant (message value)
  "Decode VALUE as MESSAGE, an instant message whose only field is `atMs'.
Answers the plist `(:at-ms)'.  Every roster instant (the durable last
selection, the durable last turn end and each when-column arm) shares
this one shape."
  (let ((object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(atMs))
    (agent-repl-wire--decoded
     message
     (list :at-ms (agent-repl-wire--decode-int64 message 'atMs object)))))

(defun agent-repl-wire-decode-roster-row-last-selected (value)
  "Decode VALUE as `RosterRowLastSelected', a plist `(:at-ms)'.
The DURABLE instant the user last selected the workspace, epoch
MILLISECONDS, read from the daemon's record so it survives an Emacs
restart.  Emacs orders by it and never draws it
\(`agent-repl-roster-selection-recency-order'); the when-column is
`RosterRowWhen'."
  (agent-repl-wire-roster--decode-instant "RosterRowLastSelected" value))

(defun agent-repl-wire-decode-roster-row-last-turn-ended (value)
  "Decode VALUE as `RosterRowLastTurnEnded', a plist `(:at-ms)'.
The DURABLE instant the workspace's most recent turn closed, epoch
MILLISECONDS, read from the daemon's turn record so it survives an Emacs
restart.  Emacs orders by it and never draws it
\(`agent-repl-roster-last-finished-name')."
  (agent-repl-wire-roster--decode-instant "RosterRowLastTurnEnded" value))

(defun agent-repl-wire-decode-roster-row-last-updated (value)
  "Decode VALUE as `RosterRowLastUpdated', a plist `(:at-ms)'.
The instant the workspace was LAST UPDATED, epoch MILLISECONDS: the
latest of its lifecycle instants and of its last status change, resolved
by the daemon.  Every row carries it.  Emacs orders by it and never
draws it \(`agent-repl--switch-curated')."
  (agent-repl-wire-roster--decode-instant "RosterRowLastUpdated" value))

;;;; ---- The close record ----

(defun agent-repl-wire-decode-roster-row-closure-by-user (value)
  "Decode VALUE as the empty `RosterRowClosureByUser': the user closed it."
  (agent-repl-wire--decode-empty "RosterRowClosureByUser" value))

(defun agent-repl-wire-decode-roster-row-closure-by-agent (value)
  "Decode VALUE as the empty `RosterRowClosureByAgent': an agent closed it."
  (agent-repl-wire--decode-empty "RosterRowClosureByAgent" value))

(defun agent-repl-wire-decode-roster-row-closure-by-merge (value)
  "Decode VALUE as the empty `RosterRowClosureByMerge': its merge closed it."
  (agent-repl-wire--decode-empty "RosterRowClosureByMerge" value))

(defun agent-repl-wire-decode-roster-row-closure-by-daemon (value)
  "Decode VALUE as the empty `RosterRowClosureByDaemon': the daemon closed it."
  (agent-repl-wire--decode-empty "RosterRowClosureByDaemon" value))

(defconst agent-repl-wire-roster-row-closure-by-arms
  '((user :user agent-repl-wire-decode-roster-row-closure-by-user)
    (agent :agent agent-repl-wire-decode-roster-row-closure-by-agent)
    (merge :merge agent-repl-wire-decode-roster-row-closure-by-merge)
    (daemon :daemon agent-repl-wire-decode-roster-row-closure-by-daemon))
  "`RosterRowClosure.by''s arm table: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-roster-row-closure (value)
  "Decode VALUE as `RosterRowClosure', a plist `(:at-ms MS :by ONEOF)'.
The DURABLE instant the workspace was closed, epoch MILLISECONDS, and who
closed it, read from the daemon's record so it survives an Emacs restart.
Emacs filters and orders by it and never draws it (the `SPC p p'
switcher offers the workspaces the user most recently closed).  AN UNSET
`by' IS INVALID and is rejected loudly."
  (let ((object (agent-repl-wire--object "RosterRowClosure" value)))
    (agent-repl-wire--check-keys
     "RosterRowClosure" object
     (cons 'atMs (mapcar #'car agent-repl-wire-roster-row-closure-by-arms)))
    (agent-repl-wire--decoded
     "RosterRowClosure"
     (list :at-ms (agent-repl-wire--decode-int64 "RosterRowClosure" 'atMs object)
           :by (agent-repl-wire--decode-oneof
                "RosterRowClosure" 'by object
                agent-repl-wire-roster-row-closure-by-arms)))))

;;;; ---- The when column ----
;;
;; REGRESSION WATCH (2026-09-15): this strict decoder rejects the whole
;; WatchWorkspaceRoster push on an unknown `shown' arm, which blanks the roster
;; and drops every tab to a stale blue status.  When the daemon adds a `when'
;; arm, it MUST be added to `agent-repl-wire-roster-row-when-arms' in the same
;; change, or the tab bar goes dark.  The decode and the allow-list both read
;; that one table, so they cannot disagree with each other.

(defun agent-repl-wire-decode-roster-row-when-merged (value)
  "Decode VALUE as `RosterRowWhenMerged', a plist `(:at-ms)'."
  (agent-repl-wire-roster--decode-instant "RosterRowWhenMerged" value))

(defun agent-repl-wire-roster--decode-started-at (message value)
  "Decode VALUE as MESSAGE, a start message whose only field is `startedAtMs'.
Answers the plist `(:started-at-ms)'.  Every counting when-column arm (a
sync merge and a running turn) shares this one shape."
  (let ((object (agent-repl-wire--object message value)))
    (agent-repl-wire--check-keys message object '(startedAtMs))
    (agent-repl-wire--decoded
     message
     (list :started-at-ms (agent-repl-wire--decode-int64 message 'startedAtMs object)))))

(defun agent-repl-wire-decode-roster-row-when-turn-running (value)
  "Decode VALUE as `RosterRowWhenTurnRunning', a plist `(:started-at-ms)'.
The instant the running turn started, exactly the footer clock's."
  (agent-repl-wire-roster--decode-started-at "RosterRowWhenTurnRunning" value))

(defun agent-repl-wire-decode-roster-row-when-sync-merging (value)
  "Decode VALUE as `RosterRowWhenSyncMerging', a plist `(:started-at-ms)'.
The instant the workspace's sync merge joined its repository's queue."
  (agent-repl-wire-roster--decode-started-at "RosterRowWhenSyncMerging" value))

(defun agent-repl-wire-decode-roster-row-when-no-turn-running (value)
  "Decode VALUE as `RosterRowWhenNoTurnRunning'.
A plist `(:last-turn-ended-at-ms)': the durable instant the workspace's
last turn ended, epoch MILLISECONDS,
nil when no turn of the workspace has ended."
  (let ((object (agent-repl-wire--object "RosterRowWhenNoTurnRunning" value)))
    (agent-repl-wire--check-keys "RosterRowWhenNoTurnRunning" object '(lastTurnEndedAtMs))
    (agent-repl-wire--decoded
     "RosterRowWhenNoTurnRunning"
     (list :last-turn-ended-at-ms
           (agent-repl-wire--decode-optional-int64
            "RosterRowWhenNoTurnRunning" 'lastTurnEndedAtMs object)))))

(defun agent-repl-wire-decode-roster-row-when-hidden-cache (value)
  "Decode VALUE as `RosterRowWhenHiddenCache', a plist `(:lapses-at-ms)'.
When the hidden workspace's prompt cache lapses, epoch MILLISECONDS."
  (let ((object (agent-repl-wire--object "RosterRowWhenHiddenCache" value)))
    (agent-repl-wire--check-keys "RosterRowWhenHiddenCache" object '(lapsesAtMs))
    (agent-repl-wire--decoded
     "RosterRowWhenHiddenCache"
     (list :lapses-at-ms (agent-repl-wire--decode-int64
                          "RosterRowWhenHiddenCache" 'lapsesAtMs object)))))

(defun agent-repl-wire-decode-roster-row-when-hidden (value)
  "Decode VALUE as `RosterRowWhenHidden', a plist `(:at-ms :cache)'.
The instant the workspace was moved into the hidden section, and the lapse
of its prompt cache when the daemon knows it (nil otherwise).  Emacs draws
no hidden row, but the roster push carries them, so this decodes them whole."
  (let ((object (agent-repl-wire--object "RosterRowWhenHidden" value)))
    (agent-repl-wire--check-keys "RosterRowWhenHidden" object '(atMs cache))
    (agent-repl-wire--decoded
     "RosterRowWhenHidden"
     (list :at-ms (agent-repl-wire--decode-int64 "RosterRowWhenHidden" 'atMs object)
           :cache (agent-repl-wire--decode-optional-message
                   "RosterRowWhenHidden" 'cache object
                   #'agent-repl-wire-decode-roster-row-when-hidden-cache)))))

(defconst agent-repl-wire-roster-row-when-arms
  '((syncMerging :sync-merging agent-repl-wire-decode-roster-row-when-sync-merging)
    (turnRunning :turn-running agent-repl-wire-decode-roster-row-when-turn-running)
    (noTurnRunning :no-turn-running agent-repl-wire-decode-roster-row-when-no-turn-running)
    (merged :merged agent-repl-wire-decode-roster-row-when-merged)
    (hidden :hidden agent-repl-wire-decode-roster-row-when-hidden))
  "`RosterRowWhen.shown''s arm table: (WIRE-KEY ARM-KEYWORD DECODER).
The ONE list of the when-column's arms: the oneof decode and the key
allow-list both read it.")

(defun agent-repl-wire-decode-roster-row-when-shown (value)
  "Decode `RosterRowWhen''s `shown' oneof from the object VALUE.
NEVER UNSET: every row has an arm to draw, `noTurnRunning' at the least."
  (agent-repl-wire--decode-oneof
   "RosterRowWhen" 'shown value agent-repl-wire-roster-row-when-arms))

(defun agent-repl-wire-decode-roster-row-when (value)
  "Decode VALUE as `RosterRowWhen': the oneof plist."
  (let ((object (agent-repl-wire--object "RosterRowWhen" value)))
    (agent-repl-wire--check-keys "RosterRowWhen" object
                                 (mapcar #'car agent-repl-wire-roster-row-when-arms))
    (agent-repl-wire--decoded
     "RosterRowWhen" (agent-repl-wire-decode-roster-row-when-shown object))))

;;;; ---- The detail panel ----

(defun agent-repl-wire-decode-roster-row-detail-branch (value)
  "Decode VALUE as `RosterRowDetailBranch', a plist `(:name)'."
  (let ((object (agent-repl-wire--object "RosterRowDetailBranch" value)))
    (agent-repl-wire--check-keys "RosterRowDetailBranch" object '(name))
    (agent-repl-wire--decoded
     "RosterRowDetailBranch"
     (list :name (agent-repl-wire--decode-string
                  "RosterRowDetailBranch" 'name object)))))

(defun agent-repl-wire-decode-roster-row-detail-parent-branch (value)
  "Decode VALUE as `RosterRowDetailParentBranch', a plist `(:name)'."
  (let ((object (agent-repl-wire--object "RosterRowDetailParentBranch" value)))
    (agent-repl-wire--check-keys "RosterRowDetailParentBranch" object '(name))
    (agent-repl-wire--decoded
     "RosterRowDetailParentBranch"
     (list :name (agent-repl-wire--decode-string
                  "RosterRowDetailParentBranch" 'name object)))))

(defun agent-repl-wire-decode-roster-row-detail-summary (value)
  "Decode VALUE as `RosterRowDetailSummary', a plist `(:text)'."
  (let ((object (agent-repl-wire--object "RosterRowDetailSummary" value)))
    (agent-repl-wire--check-keys "RosterRowDetailSummary" object '(text))
    (agent-repl-wire--decoded
     "RosterRowDetailSummary"
     (list :text (agent-repl-wire--decode-string
                  "RosterRowDetailSummary" 'text object)))))

(defun agent-repl-wire-decode-roster-row-detail-commits (value)
  "Decode VALUE as `RosterRowDetailCommits', a plist `(:count)'.
The count is how many commits the workspace's branch holds beyond its
merge-base with its parent branch; zero is a real count, which protojson
omits."
  (let ((object (agent-repl-wire--object "RosterRowDetailCommits" value)))
    (agent-repl-wire--check-keys "RosterRowDetailCommits" object '(count))
    (agent-repl-wire--decoded
     "RosterRowDetailCommits"
     (list :count (agent-repl-wire--decode-uint32
                   "RosterRowDetailCommits" 'count object)))))

(defun agent-repl-wire-decode-roster-row-detail-child-receded (value)
  "Decode VALUE as `RosterRowDetailChildReceded' and return t.
EMPTY, and PRESENCE IS THE FACT: the child's own row recedes."
  (agent-repl-wire--decode-empty "RosterRowDetailChildReceded" value)
  t)

(defun agent-repl-wire-decode-roster-row-detail-child (value)
  "Decode VALUE as `RosterRowDetailChild', a plist `(:workspace :name :receded)'.
The workspace ref is required; `receded' is PRESENT OR ABSENT, presence
being the fact that the child's own row recedes."
  (let ((object (agent-repl-wire--object "RosterRowDetailChild" value)))
    (agent-repl-wire--check-keys "RosterRowDetailChild" object '(workspace name receded))
    (agent-repl-wire--decoded
     "RosterRowDetailChild"
     (list :workspace (agent-repl-wire--decode-message
                       "RosterRowDetailChild" 'workspace object
                       #'agent-repl-wire-decode-workspace-ref)
           :name (agent-repl-wire--decode-string
                  "RosterRowDetailChild" 'name object)
           :receded (agent-repl-wire--decode-optional-message
                     "RosterRowDetailChild" 'receded object
                     #'agent-repl-wire-decode-roster-row-detail-child-receded)))))

(defun agent-repl-wire-decode-roster-row-detail (value)
  "Decode VALUE as `RosterRowDetail', a plist of its lines.
Each line is PRESENT OR ABSENT BY MESSAGE PRESENCE rather than by an empty
string — the proto says so at the message — so an absent line decodes to
nil and is omitted, never rendered blank.  This is the one place in the
roster where an absent message field is not a breach.  `children' is a
repeated field: absent (protojson omits an empty list) decodes to nil."
  (let ((object (agent-repl-wire--object "RosterRowDetail" value)))
    (agent-repl-wire--check-keys "RosterRowDetail" object
                                 '(branch parentBranch summary commits children))
    (agent-repl-wire--decoded
     "RosterRowDetail"
     (list :branch (agent-repl-wire--decode-optional-message
                    "RosterRowDetail" 'branch object
                    #'agent-repl-wire-decode-roster-row-detail-branch)
           :parent-branch (agent-repl-wire--decode-optional-message
                           "RosterRowDetail" 'parentBranch object
                           #'agent-repl-wire-decode-roster-row-detail-parent-branch)
           :summary (agent-repl-wire--decode-optional-message
                     "RosterRowDetail" 'summary object
                     #'agent-repl-wire-decode-roster-row-detail-summary)
           :commits (agent-repl-wire--decode-optional-message
                     "RosterRowDetail" 'commits object
                     #'agent-repl-wire-decode-roster-row-detail-commits)
           :children (agent-repl-wire--decode-repeated
                      "RosterRowDetail" 'children object
                      #'agent-repl-wire-decode-roster-row-detail-child)))))

;;;; ---- The 23 status arms ----
;;
;; Each is EMPTY BY DESIGN: being set is the entire assertion, and a payload
;; would be a second place for the status to disagree with itself.

(defun agent-repl-wire-decode-roster-row-status-submitting (value)
  "Decode VALUE as the empty `RosterRowStatusSubmitting'."
  (agent-repl-wire--decode-empty "RosterRowStatusSubmitting" value))

(defun agent-repl-wire-decode-roster-row-status-thinking (value)
  "Decode VALUE as the empty `RosterRowStatusThinking'."
  (agent-repl-wire--decode-empty "RosterRowStatusThinking" value))

(defun agent-repl-wire-decode-roster-row-status-clearing (value)
  "Decode VALUE as the empty `RosterRowStatusClearing'."
  (agent-repl-wire--decode-empty "RosterRowStatusClearing" value))

(defun agent-repl-wire-decode-roster-row-status-compacting (value)
  "Decode VALUE as the empty `RosterRowStatusCompacting'."
  (agent-repl-wire--decode-empty "RosterRowStatusCompacting" value))

(defun agent-repl-wire-decode-roster-row-status-permission (value)
  "Decode VALUE as the empty `RosterRowStatusPermission'."
  (agent-repl-wire--decode-empty "RosterRowStatusPermission" value))

(defun agent-repl-wire-decode-roster-row-status-question (value)
  "Decode VALUE as the empty `RosterRowStatusQuestion'.
A question batch waits on the user's answers: a question gate."
  (agent-repl-wire--decode-empty "RosterRowStatusQuestion" value))

(defun agent-repl-wire-decode-roster-row-status-done (value)
  "Decode VALUE as the empty `RosterRowStatusDone'."
  (agent-repl-wire--decode-empty "RosterRowStatusDone" value))

(defun agent-repl-wire-decode-roster-row-status-interrupted (value)
  "Decode VALUE as the empty `RosterRowStatusInterrupted'."
  (agent-repl-wire--decode-empty "RosterRowStatusInterrupted" value))

(defun agent-repl-wire-decode-roster-row-status-turn-failed (value)
  "Decode VALUE as the empty `RosterRowStatusTurnFailed'.
The last turn ended by failing: a turn end like done and interrupted,
with the same read rule, drawn blue."
  (agent-repl-wire--decode-empty "RosterRowStatusTurnFailed" value))

(defun agent-repl-wire-decode-roster-row-status-ready (value)
  "Decode VALUE as the empty `RosterRowStatusReady'.
BOTH the idle and ready render states resolve here."
  (agent-repl-wire--decode-empty "RosterRowStatusReady" value))

(defun agent-repl-wire-decode-roster-row-status-idle-async (value)
  "Decode VALUE as the empty `RosterRowStatusIdleAsync'."
  (agent-repl-wire--decode-empty "RosterRowStatusIdleAsync" value))

(defun agent-repl-wire-decode-roster-row-status-vendor-blocked (value)
  "Decode VALUE as the empty `RosterRowStatusVendorBlocked'."
  (agent-repl-wire--decode-empty "RosterRowStatusVendorBlocked" value))

(defun agent-repl-wire-decode-roster-row-status-turn-died (value)
  "Decode VALUE as the empty `RosterRowStatusTurnDied'.
The last turn died with agent-repl's own machinery (its vendor query, or
the agent process): an agent-repl fault, standing until the next turn."
  (agent-repl-wire--decode-empty "RosterRowStatusTurnDied" value))

(defun agent-repl-wire-decode-roster-row-status-vendor-fault (value)
  "Decode VALUE as the empty `RosterRowStatusVendorFault'.
The vendor will not start while agent-repl serves: a vendor fault."
  (agent-repl-wire--decode-empty "RosterRowStatusVendorFault" value))

(defun agent-repl-wire-decode-roster-row-status-network-fault (value)
  "Decode VALUE as the empty `RosterRowStatusNetworkFault'.
This machine cannot reach the network: a network fault."
  (agent-repl-wire--decode-empty "RosterRowStatusNetworkFault" value))

(defun agent-repl-wire-decode-roster-row-status-api-retrying (value)
  "Decode VALUE as the empty `RosterRowStatusApiRetrying'."
  (agent-repl-wire--decode-empty "RosterRowStatusApiRetrying" value))

(defun agent-repl-wire-decode-roster-row-status-closing (value)
  "Decode VALUE as the empty `RosterRowStatusClosing'.
A close of the workspace was refused: the footer's `closing' status."
  (agent-repl-wire--decode-empty "RosterRowStatusClosing" value))

(defun agent-repl-wire-decode-roster-row-status-daemon-impaired (value)
  "Decode VALUE as the empty `RosterRowStatusDaemonImpaired'.
The daemon itself cannot serve the workspace: an agent-repl fault."
  (agent-repl-wire--decode-empty "RosterRowStatusDaemonImpaired" value))

(defun agent-repl-wire-decode-roster-row-status-waiting (value)
  "Decode VALUE as the empty `RosterRowStatusWaiting'.
The session waits on an interrupt landing; a permission, a question or
a cold gate has its own arm."
  (agent-repl-wire--decode-empty "RosterRowStatusWaiting" value))

(defun agent-repl-wire-decode-roster-row-status-init (value)
  "Decode VALUE as the empty `RosterRowStatusInit'."
  (agent-repl-wire--decode-empty "RosterRowStatusInit" value))

(defun agent-repl-wire-decode-roster-row-status-severed (value)
  "Decode VALUE as the empty `RosterRowStatusSevered'."
  (agent-repl-wire--decode-empty "RosterRowStatusSevered" value))

(defun agent-repl-wire-decode-roster-row-status-start-failed (value)
  "Decode VALUE as the empty `RosterRowStatusStartFailed'."
  (agent-repl-wire--decode-empty "RosterRowStatusStartFailed" value))

(defun agent-repl-wire-decode-roster-row-status-degraded (value)
  "Decode VALUE as the empty `RosterRowStatusDegraded'."
  (agent-repl-wire--decode-empty "RosterRowStatusDegraded" value))

(defun agent-repl-wire-decode-roster-row-status-dead (value)
  "Decode VALUE as the empty `RosterRowStatusDead'."
  (agent-repl-wire--decode-empty "RosterRowStatusDead" value))

(defun agent-repl-wire-decode-roster-row-status-merging (value)
  "Decode VALUE as the empty `RosterRowStatusMerging'."
  (agent-repl-wire--decode-empty "RosterRowStatusMerging" value))

(defun agent-repl-wire-decode-roster-row-status-merge-queued (value)
  "Decode VALUE as the empty `RosterRowStatusMergeQueued'."
  (agent-repl-wire--decode-empty "RosterRowStatusMergeQueued" value))

(defun agent-repl-wire-decode-roster-row-status-merge-failed (value)
  "Decode VALUE as the empty `RosterRowStatusMergeFailed'."
  (agent-repl-wire--decode-empty "RosterRowStatusMergeFailed" value))

(defun agent-repl-wire-decode-roster-row-status-merged (value)
  "Decode VALUE as the empty `RosterRowStatusMerged'."
  (agent-repl-wire--decode-empty "RosterRowStatusMerged" value))

(defun agent-repl-wire-decode-roster-row-status-none (value)
  "Decode VALUE as the empty `RosterRowStatusNone'.
Distinct from an unset oneof: \"the resolver looked and there is none\" is
an assertion, where an unset oneof is the absence of one."
  (agent-repl-wire--decode-empty "RosterRowStatusNone" value))

(defun agent-repl-wire-decode-roster-row-status-inactive (value)
  "Decode VALUE as the empty `RosterRowStatusInactive'."
  (agent-repl-wire--decode-empty "RosterRowStatusInactive" value))

(defun agent-repl-wire-decode-roster-row-status-cold-gate (value)
  "Decode VALUE as the empty `RosterRowStatusColdGate'.
The session is parked on its cold gate's answer."
  (agent-repl-wire--decode-empty "RosterRowStatusColdGate" value))

(defconst agent-repl-wire-roster-row-status-arms
  '((submitting :submitting agent-repl-wire-decode-roster-row-status-submitting)
    (thinking :thinking agent-repl-wire-decode-roster-row-status-thinking)
    (clearing :clearing agent-repl-wire-decode-roster-row-status-clearing)
    (compacting :compacting agent-repl-wire-decode-roster-row-status-compacting)
    (permission :permission agent-repl-wire-decode-roster-row-status-permission)
    (question :question agent-repl-wire-decode-roster-row-status-question)
    (done :done agent-repl-wire-decode-roster-row-status-done)
    (interrupted :interrupted agent-repl-wire-decode-roster-row-status-interrupted)
    (turnFailed :turn-failed agent-repl-wire-decode-roster-row-status-turn-failed)
    (ready :ready agent-repl-wire-decode-roster-row-status-ready)
    (idleAsync :idle-async agent-repl-wire-decode-roster-row-status-idle-async)
    (vendorBlocked :vendor-blocked
                   agent-repl-wire-decode-roster-row-status-vendor-blocked)
    (vendorFault :vendor-fault
                 agent-repl-wire-decode-roster-row-status-vendor-fault)
    (networkFault :network-fault
                  agent-repl-wire-decode-roster-row-status-network-fault)
    (apiRetrying :api-retrying
                 agent-repl-wire-decode-roster-row-status-api-retrying)
    (closing :closing agent-repl-wire-decode-roster-row-status-closing)
    (daemonImpaired :daemon-impaired
                    agent-repl-wire-decode-roster-row-status-daemon-impaired)
    (waiting :waiting agent-repl-wire-decode-roster-row-status-waiting)
    (coldGate :cold-gate agent-repl-wire-decode-roster-row-status-cold-gate)
    (init :init agent-repl-wire-decode-roster-row-status-init)
    (severed :severed agent-repl-wire-decode-roster-row-status-severed)
    (startFailed :start-failed agent-repl-wire-decode-roster-row-status-start-failed)
    (degraded :degraded agent-repl-wire-decode-roster-row-status-degraded)
    (dead :dead agent-repl-wire-decode-roster-row-status-dead)
    (turnDied :turn-died agent-repl-wire-decode-roster-row-status-turn-died)
    (merging :merging agent-repl-wire-decode-roster-row-status-merging)
    (mergeQueued :merge-queued agent-repl-wire-decode-roster-row-status-merge-queued)
    (mergeFailed :merge-failed agent-repl-wire-decode-roster-row-status-merge-failed)
    (merged :merged agent-repl-wire-decode-roster-row-status-merged)
    (none :none agent-repl-wire-decode-roster-row-status-none)
    (inactive :inactive agent-repl-wire-decode-roster-row-status-inactive))
  "`RosterRow.status''s arm table: (WIRE-KEY ARM-KEYWORD DECODER).
The whole declared vocabulary, in the proto's own order.  A test pins it
against the checked-in Go bindings, so an arm landed in the contract
without a decoder here fails loudly instead of decoding as an unknown
field at some later push.")

(defconst agent-repl-wire-roster-row-status-keywords
  (mapcar #'cadr agent-repl-wire-roster-row-status-arms)
  "Every `RosterRow.status' arm keyword, in the proto's order.
status.el's color table is asserted row for row against this list.")

;;;; ---- RosterRow ----

(defun agent-repl-wire-decode-roster-row-workspace-workspace (value)
  "Decode `RosterRowWorkspace''s `workspace' field VALUE as a WorkspaceRef."
  (agent-repl-wire-decode-workspace-ref value))

(defun agent-repl-wire-decode-roster-row-workspace (value)
  "Decode VALUE as `RosterRowWorkspace', a plist `(:workspace REF)'.
The join key every gesture on the row travels back with."
  (let ((object (agent-repl-wire--object "RosterRowWorkspace" value)))
    (agent-repl-wire--check-keys "RosterRowWorkspace" object '(workspace))
    (agent-repl-wire--decoded
     "RosterRowWorkspace"
     (list :workspace (agent-repl-wire--decode-message
                       "RosterRowWorkspace" 'workspace object
                       #'agent-repl-wire-decode-roster-row-workspace-workspace)))))

(defun agent-repl-wire-decode-roster-row-status (value)
  "Decode `RosterRow''s `status' oneof from the object VALUE.
AN UNSET ONEOF IS INVALID and is rejected LOUDLY: a row with no lifecycle
is a contract breach, not a default dot.  Two arms set is the same breach
from the other side, and an arm this build does not know is refused as an
unknown field by the row's key check."
  (agent-repl-wire--decode-oneof
   "RosterRow" 'status value agent-repl-wire-roster-row-status-arms))

(defconst agent-repl-wire--roster-row-keys
  (append '(workspace attention priority viewed idleAsyncUnread reviving detachedLive oneShot opening asyncMerging mergePaused lastSelected lastTurnEnded closure lastUpdated availability name current when detail closed)
          (mapcar #'car agent-repl-wire-roster-row-status-arms))
  "Every key `RosterRow' may carry: its own fields plus the 22 status arms.")

(defun agent-repl-wire-decode-roster-row (value)
  "Decode VALUE as `RosterRow'.
Returns `(:workspace W :attention A :priority P :viewed V
:idle-async-unread IU :reviving R
:detached-live DL :one-shot OS :opening O :async-merging AM
:merge-paused MP :last-selected L :last-turn-ended LT :closure CL
:last-updated LU
:availability AV
:name N :status
S :current C :when WHEN :detail D :closed CLOSED)', with the message tree
preserved as the contract spells it.  A row carries no family: each list
nests its rows in its own row type (`RosterOpenRow', `RosterHiddenRow')."
  (let ((object (agent-repl-wire--object "RosterRow" value)))
    (agent-repl-wire--check-keys "RosterRow" object agent-repl-wire--roster-row-keys)
    (agent-repl-wire--decoded
     "RosterRow"
     (agent-repl-wire--check-roster-row-idle-async-unread
     (list :workspace (agent-repl-wire--decode-message
                       "RosterRow" 'workspace object
                       #'agent-repl-wire-decode-roster-row-workspace)
           :attention (agent-repl-wire--decode-optional-message
                       "RosterRow" 'attention object
                       #'agent-repl-wire-decode-roster-row-attention)
           :priority (agent-repl-wire--decode-optional-message
                      "RosterRow" 'priority object
                      #'agent-repl-wire-decode-roster-row-priority-badge)
           :viewed (agent-repl-wire--decode-optional-message
                    "RosterRow" 'viewed object
                    #'agent-repl-wire-decode-roster-row-viewed)
           :idle-async-unread (agent-repl-wire--decode-optional-message
                               "RosterRow" 'idleAsyncUnread object
                               #'agent-repl-wire-decode-roster-row-idle-async-unread)
           :reviving (agent-repl-wire--decode-optional-message
                      "RosterRow" 'reviving object
                      #'agent-repl-wire-decode-roster-row-reviving)
           :detached-live (agent-repl-wire--decode-optional-message
                           "RosterRow" 'detachedLive object
                           #'agent-repl-wire-decode-roster-row-detached-live)
           :one-shot (agent-repl-wire--decode-optional-message
                      "RosterRow" 'oneShot object
                      #'agent-repl-wire-decode-roster-row-one-shot)
           :opening (agent-repl-wire--decode-optional-message
                     "RosterRow" 'opening object
                     #'agent-repl-wire-decode-roster-row-opening)
           :async-merging (agent-repl-wire--decode-optional-message
                           "RosterRow" 'asyncMerging object
                           #'agent-repl-wire-decode-roster-row-async-merging)
           :merge-paused (agent-repl-wire--decode-optional-message
                          "RosterRow" 'mergePaused object
                          #'agent-repl-wire-decode-roster-row-merge-paused)
           :last-selected (agent-repl-wire--decode-optional-message
                           "RosterRow" 'lastSelected object
                           #'agent-repl-wire-decode-roster-row-last-selected)
           :last-turn-ended (agent-repl-wire--decode-optional-message
                             "RosterRow" 'lastTurnEnded object
                             #'agent-repl-wire-decode-roster-row-last-turn-ended)
           :closure (agent-repl-wire--decode-optional-message
                     "RosterRow" 'closure object
                     #'agent-repl-wire-decode-roster-row-closure)
           :last-updated (agent-repl-wire--decode-message
                          "RosterRow" 'lastUpdated object
                          #'agent-repl-wire-decode-roster-row-last-updated)
           :availability (agent-repl-wire--decode-message
                          "RosterRow" 'availability object
                          #'agent-repl-wire-decode-roster-row-availability)
           :name (agent-repl-wire--decode-message
                  "RosterRow" 'name object
                  #'agent-repl-wire-decode-roster-row-name)
           :status (agent-repl-wire-decode-roster-row-status object)
           :current (agent-repl-wire--decode-message
                     "RosterRow" 'current object
                     #'agent-repl-wire-decode-roster-row-current)
           :when (agent-repl-wire--decode-message
                  "RosterRow" 'when object
                  #'agent-repl-wire-decode-roster-row-when)
           :detail (agent-repl-wire--decode-message
                    "RosterRow" 'detail object
                    #'agent-repl-wire-decode-roster-row-detail)
           :closed (agent-repl-wire--decode-message
                    "RosterRow" 'closed object
                    #'agent-repl-wire-decode-roster-row-closed))))))

(defun agent-repl-wire--check-roster-row-idle-async-unread (row)
  "Return the decoded ROW, refusing an unread-turn-end marker off `idle_async'.
`RosterRowIdleAsyncUnread' stands only on an `idle_async' row; one on any
other arm is a contract breach, refused loudly rather than drawn."
  (let ((end (plist-get row :idle-async-unread))
        (arm (plist-get (plist-get row :status) :arm)))
    (when (and end (not (eq arm :idle-async)))
      (agent-repl-wire--fail
       "RosterRow" 'idle_async_unread
       (format "the marker (turn end %s) stands on status %s, and only an idle_async row may carry it"
               end arm)))
    row))

;;;; ---- The lists' rows ----
;;
;; EACH LIST WRAPS THE SHARED ROW IN ITS OWN ROW TYPE: an open row states its
;; tab and nests its open family, a hidden row nests its hidden family, and a
;; merged or a closed row stands alone.  The roster lists every workspace in
;; exactly one of them.

(defun agent-repl-wire--decode-row-list (message-name value decode-row)
  "Decode VALUE as MESSAGE-NAME, one list's rows: a plist `(:rows LIST)'.
Every such message (`RosterOpenRows', `RosterHiddenRows', `RosterMergedRows',
`RosterClosedRows') is one repeated `rows' field in drawn order, each
element decoded by DECODE-ROW."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object '(rows))
    (agent-repl-wire--decoded
     message-name
     (list :rows (agent-repl-wire--decode-repeated message-name 'rows object decode-row)))))

(defun agent-repl-wire-decode-roster-open-row-tab-numbered (value)
  "Decode VALUE as `RosterOpenRowTabNumbered', a plist `(:number N)'.
N counts from 1 in the Open section's walk, with no gap."
  (let ((object (agent-repl-wire--object "RosterOpenRowTabNumbered" value)))
    (agent-repl-wire--check-keys "RosterOpenRowTabNumbered" object '(number))
    (agent-repl-wire--decoded
     "RosterOpenRowTabNumbered"
     (list :number (agent-repl-wire--decode-uint32 "RosterOpenRowTabNumbered" 'number object)))))

(defun agent-repl-wire-decode-roster-open-row-tab-folded (value)
  "Decode VALUE as the empty message `RosterOpenRowTabFolded'."
  (agent-repl-wire--decode-empty "RosterOpenRowTabFolded" value))

(defun agent-repl-wire-decode-roster-open-row-tab (value)
  "Decode VALUE as `RosterOpenRowTab', a plist `(:tab ARM)'.
THE ARM IS THE TAB: `:numbered' is drawn on the bar at its number, and
`:folded' (its repository or subsection collapsed) is open but undrawn.
It is never unset: every open row has a tab."
  (let ((object (agent-repl-wire--object "RosterOpenRowTab" value)))
    (agent-repl-wire--check-keys "RosterOpenRowTab" object '(numbered folded))
    (agent-repl-wire--decoded
     "RosterOpenRowTab"
     (list :tab (agent-repl-wire--decode-oneof
                 "RosterOpenRowTab" 'tab object
                 '((numbered :numbered agent-repl-wire-decode-roster-open-row-tab-numbered)
                   (folded :folded agent-repl-wire-decode-roster-open-row-tab-folded)))))))

(defun agent-repl-wire-decode-roster-open-row-repository-divider (value)
  "Decode VALUE as `RosterOpenRowRepositoryDivider', a plist `(:name NAME)'."
  (let ((object (agent-repl-wire--object "RosterOpenRowRepositoryDivider" value)))
    (agent-repl-wire--check-keys "RosterOpenRowRepositoryDivider" object '(name))
    (agent-repl-wire--decoded
     "RosterOpenRowRepositoryDivider"
     (list :name (agent-repl-wire--decode-string
                  "RosterOpenRowRepositoryDivider" 'name object)))))

(defun agent-repl-wire-decode-roster-open-row (value)
  "Decode VALUE as `RosterOpenRow'.
The plist is `(:row ROW :tab TAB :children ROWS :repository-divider DIVIDER)'.
CHILDREN are the row's open family, each itself an open row, in drawn order.
DIVIDER names the repository the row begins within its family, nil when it
is of the same repository as the row drawn before it."
  (let ((object (agent-repl-wire--object "RosterOpenRow" value)))
    (agent-repl-wire--check-keys "RosterOpenRow" object '(row tab children repositoryDivider))
    (agent-repl-wire--decoded
     "RosterOpenRow"
     (list :row (agent-repl-wire--decode-message
                 "RosterOpenRow" 'row object #'agent-repl-wire-decode-roster-row)
           :tab (agent-repl-wire--decode-message
                 "RosterOpenRow" 'tab object #'agent-repl-wire-decode-roster-open-row-tab)
           :children (agent-repl-wire--decode-repeated
                      "RosterOpenRow" 'children object #'agent-repl-wire-decode-roster-open-row)
           :repository-divider (agent-repl-wire--decode-optional-message
                                "RosterOpenRow" 'repositoryDivider object
                                #'agent-repl-wire-decode-roster-open-row-repository-divider)))))

(defun agent-repl-wire-decode-roster-open-rows (value)
  "Decode VALUE as `RosterOpenRows', a plist `(:rows OPEN-ROWS)'."
  (agent-repl-wire--decode-row-list "RosterOpenRows" value #'agent-repl-wire-decode-roster-open-row))

(defun agent-repl-wire-decode-roster-hidden-row (value)
  "Decode VALUE as `RosterHiddenRow', a plist `(:row ROW :children ROWS)'.
CHILDREN are the row's hidden family, each itself a hidden row."
  (let ((object (agent-repl-wire--object "RosterHiddenRow" value)))
    (agent-repl-wire--check-keys "RosterHiddenRow" object '(row children))
    (agent-repl-wire--decoded
     "RosterHiddenRow"
     (list :row (agent-repl-wire--decode-message
                 "RosterHiddenRow" 'row object #'agent-repl-wire-decode-roster-row)
           :children (agent-repl-wire--decode-repeated
                      "RosterHiddenRow" 'children object #'agent-repl-wire-decode-roster-hidden-row)))))

(defun agent-repl-wire-decode-roster-hidden-rows (value)
  "Decode VALUE as `RosterHiddenRows', a plist `(:rows HIDDEN-ROWS)'."
  (agent-repl-wire--decode-row-list "RosterHiddenRows" value #'agent-repl-wire-decode-roster-hidden-row))

(defun agent-repl-wire-decode-roster-merged-row (value)
  "Decode VALUE as `RosterMergedRow', a plist `(:row ROW)'.  It nests nothing."
  (let ((object (agent-repl-wire--object "RosterMergedRow" value)))
    (agent-repl-wire--check-keys "RosterMergedRow" object '(row))
    (agent-repl-wire--decoded
     "RosterMergedRow"
     (list :row (agent-repl-wire--decode-message
                 "RosterMergedRow" 'row object #'agent-repl-wire-decode-roster-row)))))

(defun agent-repl-wire-decode-roster-merged-rows (value)
  "Decode VALUE as `RosterMergedRows', a plist `(:rows MERGED-ROWS)'."
  (agent-repl-wire--decode-row-list "RosterMergedRows" value #'agent-repl-wire-decode-roster-merged-row))

(defun agent-repl-wire-decode-roster-closed-row (value)
  "Decode VALUE as `RosterClosedRow', a plist `(:row ROW)'.  It nests nothing."
  (let ((object (agent-repl-wire--object "RosterClosedRow" value)))
    (agent-repl-wire--check-keys "RosterClosedRow" object '(row))
    (agent-repl-wire--decoded
     "RosterClosedRow"
     (list :row (agent-repl-wire--decode-message
                 "RosterClosedRow" 'row object #'agent-repl-wire-decode-roster-row)))))

(defun agent-repl-wire-decode-roster-closed-rows (value)
  "Decode VALUE as `RosterClosedRows', a plist `(:rows CLOSED-ROWS)'.
Every closed workspace that is not a recent merge, which no section draws
but the editor's switcher reaches."
  (agent-repl-wire--decode-row-list "RosterClosedRows" value #'agent-repl-wire-decode-roster-closed-row))

;;;; ---- Sections ----

(defun agent-repl-wire-decode-roster-section-header-label (value)
  "Decode `RosterSectionHeader''s `label' field VALUE as a RosterLabel."
  (agent-repl-wire-decode-roster-label value))

(defun agent-repl-wire-decode-roster-section-header-count (value)
  "Decode `RosterSectionHeader''s `count' field VALUE as a RosterSectionCount."
  (agent-repl-wire-decode-roster-section-count value))

(defun agent-repl-wire-decode-roster-section-header (value)
  "Decode VALUE as `RosterSectionHeader', a plist `(:label :count)'.
Fold state is WEBVIEW-LOCAL and is no element of this view."
  (let ((object (agent-repl-wire--object "RosterSectionHeader" value)))
    (agent-repl-wire--check-keys "RosterSectionHeader" object '(label count))
    (agent-repl-wire--decoded
     "RosterSectionHeader"
     (list :label (agent-repl-wire--decode-message
                   "RosterSectionHeader" 'label object
                   #'agent-repl-wire-decode-roster-section-header-label)
           :count (agent-repl-wire--decode-message
                   "RosterSectionHeader" 'count object
                   #'agent-repl-wire-decode-roster-section-header-count)))))

(defun agent-repl-wire-decode-roster-repo-key-repository (value)
  "Decode `RosterRepoKey''s `repository' field VALUE as a RepositoryRef."
  (agent-repl-wire-decode-repository-ref value))

(defun agent-repl-wire-decode-roster-repo-key (value)
  "Decode VALUE as `RosterRepoKey', a plist `(:repository REF)'.
The fold key and the join key — and the echo token CreateWorkspace takes
back."
  (let ((object (agent-repl-wire--object "RosterRepoKey" value)))
    (agent-repl-wire--check-keys "RosterRepoKey" object '(repository))
    (agent-repl-wire--decoded
     "RosterRepoKey"
     (list :repository (agent-repl-wire--decode-message
                        "RosterRepoKey" 'repository object
                        #'agent-repl-wire-decode-roster-repo-key-repository)))))

(defun agent-repl-wire-decode-roster-repo-section-key (value)
  "Decode `RosterRepoSection''s `key' field VALUE as a RosterRepoKey."
  (agent-repl-wire-decode-roster-repo-key value))

(defun agent-repl-wire-decode-roster-repo-section-header (value)
  "Decode `RosterRepoSection''s `header' field VALUE."
  (agent-repl-wire-decode-roster-section-header value))

(defun agent-repl-wire-decode-roster-repo-section-collapsible (value)
  "Decode VALUE as the empty message `RosterRepoSectionCollapsible'."
  (agent-repl-wire--decode-empty "RosterRepoSectionCollapsible" value))

(defun agent-repl-wire-decode-roster-repo-section-holds-current (value)
  "Decode VALUE as the empty message `RosterRepoSectionHoldsCurrent'."
  (agent-repl-wire--decode-empty "RosterRepoSectionHoldsCurrent" value))

(defun agent-repl-wire--decode-expanded-collapse (message-name value collapsible holds-current
                                                                &optional never-collapses)
  "Decode VALUE as MESSAGE-NAME, an expanded fold, a plist `(:collapse)'.
Whether the sidebar's fold gesture may collapse the repository section or
subsection: never while it holds the current workspace.  COLLAPSIBLE and
HOLDS-CURRENT decode the two arms, each the message's own empty arm.
NEVER-COLLAPSES, when given, decodes the third arm only a subsection has:
the one-shot subsection, which never collapses.  Emacs folds nothing, but
the codec refuses unknown fields, so it is decoded and validated whole.
Never unset."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object
                                 (if never-collapses
                                     '(collapsible holdsCurrent neverCollapses)
                                   '(collapsible holdsCurrent)))
    (agent-repl-wire--decoded
     message-name
     (list :collapse
           (agent-repl-wire--decode-oneof
            message-name 'collapse object
            `((collapsible :collapsible ,collapsible)
              (holdsCurrent :holds-current ,holds-current)
              ,@(and never-collapses
                     `((neverCollapses :never-collapses ,never-collapses)))))))))

(defun agent-repl-wire-decode-roster-repo-section-expanded (value)
  "Decode VALUE as `RosterRepoSectionExpanded', a plist `(:collapse)'.
See `agent-repl-wire--decode-expanded-collapse'."
  (agent-repl-wire--decode-expanded-collapse
   "RosterRepoSectionExpanded" value
   'agent-repl-wire-decode-roster-repo-section-collapsible
   'agent-repl-wire-decode-roster-repo-section-holds-current))

(defun agent-repl-wire-decode-roster-repo-section-collapsed (value)
  "Decode VALUE as the empty message `RosterRepoSectionCollapsed'."
  (agent-repl-wire--decode-empty "RosterRepoSectionCollapsed" value))

(defun agent-repl-wire-decode-roster-repo-section-fold (object)
  "Decode `RosterRepoSection''s `fold' oneof from OBJECT.
THE ARM IS THE FOLD, and it is never unset: a collapsed repository's
workspaces have no tabs on the bar."
  (agent-repl-wire--decode-oneof
   "RosterRepoSection" 'fold object
   '((expanded :expanded agent-repl-wire-decode-roster-repo-section-expanded)
     (collapsed :collapsed agent-repl-wire-decode-roster-repo-section-collapsed))))

(defun agent-repl-wire-decode-roster-repo-divergence-pending (value)
  "Decode VALUE as the empty message `RosterRepoDivergencePending'."
  (agent-repl-wire--decode-empty "RosterRepoDivergencePending" value))

(defun agent-repl-wire-decode-roster-repo-divergence-counted (value)
  "Decode VALUE as `RosterRepoDivergenceCounted'.
A plist `(:branch :upstream :ahead :behind)'."
  (let ((object (agent-repl-wire--object "RosterRepoDivergenceCounted" value)))
    (agent-repl-wire--check-keys "RosterRepoDivergenceCounted" object
                                 '(branch upstream ahead behind))
    (agent-repl-wire--decoded
     "RosterRepoDivergenceCounted"
     (list :branch (agent-repl-wire--decode-string
                    "RosterRepoDivergenceCounted" 'branch object)
           :upstream (agent-repl-wire--decode-string
                      "RosterRepoDivergenceCounted" 'upstream object)
           :ahead (agent-repl-wire--decode-uint32
                   "RosterRepoDivergenceCounted" 'ahead object)
           :behind (agent-repl-wire--decode-uint32
                    "RosterRepoDivergenceCounted" 'behind object)))))

(defun agent-repl-wire-decode-roster-repo-divergence-no-upstream (value)
  "Decode VALUE as `RosterRepoDivergenceNoUpstream', a plist `(:branch)'."
  (let ((object (agent-repl-wire--object "RosterRepoDivergenceNoUpstream" value)))
    (agent-repl-wire--check-keys "RosterRepoDivergenceNoUpstream" object '(branch))
    (agent-repl-wire--decoded
     "RosterRepoDivergenceNoUpstream"
     (list :branch (agent-repl-wire--decode-string
                    "RosterRepoDivergenceNoUpstream" 'branch object)))))

(defun agent-repl-wire-decode-roster-repo-divergence-failed (value)
  "Decode VALUE as `RosterRepoDivergenceFailed', a plist `(:reason)'."
  (let ((object (agent-repl-wire--object "RosterRepoDivergenceFailed" value)))
    (agent-repl-wire--check-keys "RosterRepoDivergenceFailed" object '(reason))
    (agent-repl-wire--decoded
     "RosterRepoDivergenceFailed"
     (list :reason (agent-repl-wire--decode-string
                    "RosterRepoDivergenceFailed" 'reason object)))))

(defun agent-repl-wire-decode-roster-repo-divergence (value)
  "Decode VALUE as `RosterRepoDivergence', a plist `(:state)'.
The repository's primary branch against its upstream, which the webview
draws as the section label's hover.  Emacs draws nothing from it, but the
codec refuses unknown fields, so it is decoded and validated whole.  THE
ARM IS THE MEASUREMENT'S OUTCOME, and it is never unset."
  (let ((object (agent-repl-wire--object "RosterRepoDivergence" value)))
    (agent-repl-wire--check-keys "RosterRepoDivergence" object
                                 '(pending counted noUpstream failed))
    (agent-repl-wire--decoded
     "RosterRepoDivergence"
     (list :state
           (agent-repl-wire--decode-oneof
            "RosterRepoDivergence" 'state object
            '((pending :pending agent-repl-wire-decode-roster-repo-divergence-pending)
              (counted :counted agent-repl-wire-decode-roster-repo-divergence-counted)
              (noUpstream :no-upstream agent-repl-wire-decode-roster-repo-divergence-no-upstream)
              (failed :failed agent-repl-wire-decode-roster-repo-divergence-failed)))))))

(defun agent-repl-wire-decode-roster-subsection-key (value)
  "Decode VALUE as `RosterSubsectionKey', a plist `(:subsection-id)'.
The ID, NOT the name: a subsection can be renamed without becoming another."
  (let ((object (agent-repl-wire--object "RosterSubsectionKey" value)))
    (agent-repl-wire--check-keys "RosterSubsectionKey" object '(subsectionId))
    (agent-repl-wire--decoded
     "RosterSubsectionKey"
     (list :subsection-id (agent-repl-wire--decode-string
                           "RosterSubsectionKey" 'subsectionId object)))))

(defun agent-repl-wire-decode-roster-subsection-named (value)
  "Decode VALUE as `RosterSubsectionNamed', a plist `(:key)'.
A subsection the user made and named, which carries its stored identity."
  (let ((object (agent-repl-wire--object "RosterSubsectionNamed" value)))
    (agent-repl-wire--check-keys "RosterSubsectionNamed" object '(key))
    (agent-repl-wire--decoded
     "RosterSubsectionNamed"
     (list :key (agent-repl-wire--decode-message
                 "RosterSubsectionNamed" 'key object
                 #'agent-repl-wire-decode-roster-subsection-key)))))

(defun agent-repl-wire-decode-roster-subsection-one-shot (value)
  "Decode VALUE as the empty message `RosterSubsectionOneShot'.
The repository's one-shot subsection, derived from its workspaces' one-shot
marks and always the section's last entry."
  (agent-repl-wire--decode-empty "RosterSubsectionOneShot" value))

(defun agent-repl-wire-decode-roster-subsection-never-collapses (value)
  "Decode VALUE as the empty message `RosterSubsectionNeverCollapses'."
  (agent-repl-wire--decode-empty "RosterSubsectionNeverCollapses" value))

(defun agent-repl-wire-decode-roster-subsection-collapsible (value)
  "Decode VALUE as the empty message `RosterSubsectionCollapsible'."
  (agent-repl-wire--decode-empty "RosterSubsectionCollapsible" value))

(defun agent-repl-wire-decode-roster-subsection-holds-current (value)
  "Decode VALUE as the empty message `RosterSubsectionHoldsCurrent'."
  (agent-repl-wire--decode-empty "RosterSubsectionHoldsCurrent" value))

(defun agent-repl-wire-decode-roster-subsection-expanded (value)
  "Decode VALUE as `RosterSubsectionExpanded', a plist `(:collapse)'.
See `agent-repl-wire--decode-expanded-collapse'."
  (agent-repl-wire--decode-expanded-collapse
   "RosterSubsectionExpanded" value
   'agent-repl-wire-decode-roster-subsection-collapsible
   'agent-repl-wire-decode-roster-subsection-holds-current
   'agent-repl-wire-decode-roster-subsection-never-collapses))

(defun agent-repl-wire-decode-roster-subsection-collapsed (value)
  "Decode VALUE as the empty message `RosterSubsectionCollapsed'."
  (agent-repl-wire--decode-empty "RosterSubsectionCollapsed" value))

(defun agent-repl-wire-decode-roster-subsection (value)
  "Decode VALUE as `RosterSubsection'.
A plist `(:kind :header :open-rows :fold)'.
One subsection of a repository's section: `:kind' is `:named', a subsection
the user made, or `:one-shot', the repository's one-shot subsection.  Emacs
draws nothing from it but walks its rows, because a workspace in a
subsection has its row there and nowhere else in the repository grouping.
A collapsed subsection's rows carry folded tabs, which is all the tab bar
reads of its fold."
  (let ((object (agent-repl-wire--object "RosterSubsection" value)))
    (agent-repl-wire--check-keys "RosterSubsection" object '(named oneShot header openRows expanded collapsed))
    (agent-repl-wire--decoded
     "RosterSubsection"
     (list :kind (agent-repl-wire--decode-oneof
                  "RosterSubsection" 'kind object
                  '((named :named agent-repl-wire-decode-roster-subsection-named)
                    (oneShot :one-shot agent-repl-wire-decode-roster-subsection-one-shot)))
           :header (agent-repl-wire--decode-message
                    "RosterSubsection" 'header object
                    #'agent-repl-wire-decode-roster-section-header)
           :open-rows (agent-repl-wire--decode-message
                       "RosterSubsection" 'openRows object
                       #'agent-repl-wire-decode-roster-open-rows)
           :fold (agent-repl-wire--decode-oneof
                  "RosterSubsection" 'fold object
                  '((expanded :expanded agent-repl-wire-decode-roster-subsection-expanded)
                    (collapsed :collapsed agent-repl-wire-decode-roster-subsection-collapsed)))))))

(defun agent-repl-wire-decode-roster-repo-entry (value)
  "Decode VALUE as `RosterRepoEntry', a plist `(:entry ARM)'.
THE ARM IS WHAT STANDS AT THIS POINT OF THE SECTION: `:row', a top-level
open row in no subsection, or `:subsection'.  It is never unset."
  (let ((object (agent-repl-wire--object "RosterRepoEntry" value)))
    (agent-repl-wire--check-keys "RosterRepoEntry" object '(row subsection))
    (agent-repl-wire--decoded
     "RosterRepoEntry"
     (list :entry (agent-repl-wire--decode-oneof
                   "RosterRepoEntry" 'entry object
                   '((row :row agent-repl-wire-decode-roster-open-row)
                     (subsection :subsection agent-repl-wire-decode-roster-subsection)))))))

;;;; ---- frontend.v1.RosterRepoMergeQueue ----
;; A repository's merge queue, which the webview draws in the merge queue
;; section (`RosterMergedSection''s `queues').  Emacs draws nothing from it,
;; but the codec refuses unknown fields, so it is decoded and validated
;; whole.

(defun agent-repl-wire-decode-roster-merge-queue-mode (value)
  "Decode VALUE as `RosterMergeQueueMode', a plist `(:arm ARM)'.
ARM is `:sync' or `:async'.  It is never unset."
  (let ((object (agent-repl-wire--object "RosterMergeQueueMode" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueMode" object '(sync async))
    (agent-repl-wire--decoded
     "RosterMergeQueueMode"
     (list :arm (plist-get
                 (agent-repl-wire--decode-oneof
                  "RosterMergeQueueMode" 'mode object
                  `((sync :sync ,(lambda (v) (agent-repl-wire--decode-empty "RosterMergeQueueModeSync" v)))
                    (async :async ,(lambda (v) (agent-repl-wire--decode-empty "RosterMergeQueueModeAsync" v)))))
                 :arm)))))

(defun agent-repl-wire-decode-roster-merge-queue-subject (value)
  "Decode VALUE as `RosterMergeQueueSubject'.
A plist `(:workspace REF :name NAME :reveal FEED-ID :mode MODE)': the
workspace the merge is credited to, its name, the bubble a click reveals
\(nil when none), and how the merge runs."
  (let ((object (agent-repl-wire--object "RosterMergeQueueSubject" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueSubject" object '(workspace name reveal mode))
    (agent-repl-wire--decoded
     "RosterMergeQueueSubject"
     (list :workspace (agent-repl-wire--decode-message
                       "RosterMergeQueueSubject" 'workspace object #'agent-repl-wire-decode-workspace-ref)
           :name (agent-repl-wire--decode-string "RosterMergeQueueSubject" 'name object)
           :reveal (agent-repl-wire--decode-optional-message
                    "RosterMergeQueueSubject" 'reveal object #'agent-repl-wire-decode-feed-id)
           :mode (agent-repl-wire--decode-message
                  "RosterMergeQueueSubject" 'mode object #'agent-repl-wire-decode-roster-merge-queue-mode)))))

(defconst agent-repl-wire--roster-merge-queue-instants
  '((startedAtMs . :started-at-ms) (endedAtMs . :ended-at-ms) (queuedAtMs . :queued-at-ms))
  "Each epoch-ms field a merge queue's span messages carry, and its keyword.")

(defun agent-repl-wire--decode-roster-merge-queue-span (message-name value fields)
  "Decode VALUE as MESSAGE-NAME, a message of the epoch-ms FIELDS alone.
Returns a plist keyed by each field's keyword
\(`agent-repl-wire--roster-merge-queue-instants')."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object fields)
    (agent-repl-wire--decoded
     message-name
     (mapcan (lambda (field)
               (list (cdr (assq field agent-repl-wire--roster-merge-queue-instants))
                     (agent-repl-wire--decode-int64 message-name field object)))
             fields))))

(defun agent-repl-wire-decode-roster-merge-queue-stage (value)
  "Decode VALUE as `RosterMergeQueueStage'.
A plist `(:label LABEL :round ROUND :state (:arm ARM :value SPAN))', ARM
`:live', `:succeeded' or `:failed'."
  (let ((object (agent-repl-wire--object "RosterMergeQueueStage" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueStage" object '(label round live succeeded failed))
    (agent-repl-wire--decoded
     "RosterMergeQueueStage"
     (list :label (agent-repl-wire--decode-string "RosterMergeQueueStage" 'label object)
           :round (agent-repl-wire--decode-uint32 "RosterMergeQueueStage" 'round object)
           :state (agent-repl-wire--decode-oneof
                   "RosterMergeQueueStage" 'state object
                   `((live :live ,(lambda (v) (agent-repl-wire--decode-roster-merge-queue-span
                                               "RosterMergeQueueStageLive" v '(startedAtMs))))
                     (succeeded :succeeded ,(lambda (v) (agent-repl-wire--decode-roster-merge-queue-span
                                                         "RosterMergeQueueStageSucceeded" v '(startedAtMs endedAtMs))))
                     (failed :failed ,(lambda (v) (agent-repl-wire--decode-roster-merge-queue-span
                                                   "RosterMergeQueueStageFailed" v '(startedAtMs endedAtMs))))))))))

(defun agent-repl-wire-decode-roster-merge-queue-head-merging (value)
  "Decode VALUE as `RosterMergeQueueHeadMerging'.
A plist `(:step STEP :suspended SUSPENDED)': STEP is the daemon's words
for the step, nil while none is named, and SUSPENDED is t while nothing
of the merge's work runs."
  (let ((object (agent-repl-wire--object "RosterMergeQueueHeadMerging" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueHeadMerging" object '(step suspended))
    (agent-repl-wire--decoded
     "RosterMergeQueueHeadMerging"
     (list :step (agent-repl-wire--decode-optional-string "RosterMergeQueueHeadMerging" 'step object)
           :suspended (and (agent-repl-wire--present-p object 'suspended)
                           (progn (agent-repl-wire--decode-empty
                                   "RosterMergeQueueHeadSuspended" (agent-repl-wire--raw object 'suspended))
                                  t))))))

(defun agent-repl-wire-decode-roster-merge-queue-head (value)
  "Decode VALUE as `RosterMergeQueueHead'.
A plist `(:subject SUBJECT :head-since-ms MS :stages STAGES :standing
\(:arm ARM :value V))', ARM `:waiting' or `:merging', MS when the merge
reached the head.  The standing is never unset."
  (let ((object (agent-repl-wire--object "RosterMergeQueueHead" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueHead" object '(subject stages waiting merging headSinceMs))
    (agent-repl-wire--decoded
     "RosterMergeQueueHead"
     (list :subject (agent-repl-wire--decode-message
                     "RosterMergeQueueHead" 'subject object #'agent-repl-wire-decode-roster-merge-queue-subject)
           :head-since-ms (agent-repl-wire--decode-int64 "RosterMergeQueueHead" 'headSinceMs object)
           :stages (agent-repl-wire--decode-repeated
                    "RosterMergeQueueHead" 'stages object #'agent-repl-wire-decode-roster-merge-queue-stage)
           :standing (agent-repl-wire--decode-oneof
                      "RosterMergeQueueHead" 'standing object
                      `((waiting :waiting ,(lambda (v) (agent-repl-wire--decode-roster-merge-queue-span
                                                        "RosterMergeQueueHeadWaiting" v '(queuedAtMs))))
                        (merging :merging agent-repl-wire-decode-roster-merge-queue-head-merging)))))))

(defun agent-repl-wire-decode-roster-merge-queue-waiting (value)
  "Decode VALUE as `RosterMergeQueueWaiting'.
A plist `(:subject SUBJECT :queued-at-ms MS :placed-at-front BOOL)'."
  (let ((object (agent-repl-wire--object "RosterMergeQueueWaiting" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueWaiting" object '(subject queuedAtMs placedAtFront))
    (agent-repl-wire--decoded
     "RosterMergeQueueWaiting"
     (list :subject (agent-repl-wire--decode-message
                     "RosterMergeQueueWaiting" 'subject object #'agent-repl-wire-decode-roster-merge-queue-subject)
           :queued-at-ms (agent-repl-wire--decode-int64 "RosterMergeQueueWaiting" 'queuedAtMs object)
           :placed-at-front (agent-repl-wire--decode-bool "RosterMergeQueueWaiting" 'placedAtFront object)))))

(defun agent-repl-wire-decode-roster-merge-queue-pauser (value)
  "Decode VALUE as `RosterMergeQueuePauser', `(:arm ARM :value V)'.
ARM is `:user' (V t) or `:workspace' (V the workspace's name)."
  (let ((object (agent-repl-wire--object "RosterMergeQueuePauser" value)))
    (agent-repl-wire--check-keys "RosterMergeQueuePauser" object '(user workspace))
    (agent-repl-wire--decode-oneof
     "RosterMergeQueuePauser" 'asker object
     `((user :user ,(lambda (v) (agent-repl-wire--decode-empty "RosterMergeQueuePauserUser" v) t))
       (workspace :workspace
                  ,(lambda (v)
                     (let ((ws (agent-repl-wire--object "RosterMergeQueuePauserWorkspace" v)))
                       (agent-repl-wire--check-keys "RosterMergeQueuePauserWorkspace" ws '(name))
                       (agent-repl-wire--decode-string "RosterMergeQueuePauserWorkspace" 'name ws))))))))

(defun agent-repl-wire-decode-roster-merge-queue-paused (value)
  "Decode VALUE as `RosterMergeQueuePaused'.
A plist `(:paused-at-ms MS :by BY :hard HARD)': when the queue was paused
and who paused it (each nil for a pause recorded before the daemon kept
them), and t for a hard stop."
  (let ((object (agent-repl-wire--object "RosterMergeQueuePaused" value)))
    (agent-repl-wire--check-keys "RosterMergeQueuePaused" object '(pausedAtMs by hard))
    (agent-repl-wire--decoded
     "RosterMergeQueuePaused"
     (list :paused-at-ms (agent-repl-wire--decode-optional-int64 "RosterMergeQueuePaused" 'pausedAtMs object)
           :by (agent-repl-wire--decode-optional-message
                "RosterMergeQueuePaused" 'by object #'agent-repl-wire-decode-roster-merge-queue-pauser)
           :hard (agent-repl-wire--decode-bool "RosterMergeQueuePaused" 'hard object)))))

(defun agent-repl-wire-decode-roster-repo-merge-queue (value)
  "Decode VALUE as `RosterRepoMergeQueue'.
A plist `(:head HEAD :waiting WAITING :paused PAUSED :remediating
REMEDIATING)': the merge at the head (nil only while the line is empty and a
merge remediates), the merges waiting behind it, the queue's pause
\(`agent-repl-wire-decode-roster-merge-queue-paused'), nil while it runs,
and the merges remediating out of line
\(`agent-repl-wire-decode-roster-merge-queue-remediating')."
  (let ((object (agent-repl-wire--object "RosterRepoMergeQueue" value)))
    (agent-repl-wire--check-keys "RosterRepoMergeQueue" object '(head waiting paused remediating))
    (agent-repl-wire--decoded
     "RosterRepoMergeQueue"
     (list :head (agent-repl-wire--decode-optional-message
                  "RosterRepoMergeQueue" 'head object #'agent-repl-wire-decode-roster-merge-queue-head)
           :waiting (agent-repl-wire--decode-repeated
                     "RosterRepoMergeQueue" 'waiting object #'agent-repl-wire-decode-roster-merge-queue-waiting)
           :paused (agent-repl-wire--decode-optional-message
                    "RosterRepoMergeQueue" 'paused object
                    #'agent-repl-wire-decode-roster-merge-queue-paused)
           :remediating (agent-repl-wire--decode-repeated
                         "RosterRepoMergeQueue" 'remediating object
                         #'agent-repl-wire-decode-roster-merge-queue-remediating)))))

(defun agent-repl-wire-decode-roster-merge-queue-remediating (value)
  "Decode VALUE as `RosterMergeQueueRemediating'.
A plist `(:subject SUBJECT :since-ms MS)': a merge remediating its failing
tests out of line, and when it stepped aside."
  (let ((object (agent-repl-wire--object "RosterMergeQueueRemediating" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueRemediating" object '(subject sinceMs))
    (agent-repl-wire--decoded
     "RosterMergeQueueRemediating"
     (list :subject (agent-repl-wire--decode-message
                     "RosterMergeQueueRemediating" 'subject object
                     #'agent-repl-wire-decode-roster-merge-queue-subject)
           :since-ms (agent-repl-wire--decode-int64 "RosterMergeQueueRemediating" 'sinceMs object)))))

(defun agent-repl-wire-decode-roster-repo-section (value)
  "Decode VALUE as `RosterRepoSection'.
A plist `(:key :header :fold :divergence :entries)'.  ENTRIES is the
section's body in drawn order: its rows in no subsection and its
subsections, interleaved (`agent-repl-wire-decode-roster-repo-entry')."
  (let ((object (agent-repl-wire--object "RosterRepoSection" value)))
    (agent-repl-wire--check-keys "RosterRepoSection" object
                                 '(key header expanded collapsed divergence entries))
    (agent-repl-wire--decoded
     "RosterRepoSection"
     (list :key (agent-repl-wire--decode-message
                 "RosterRepoSection" 'key object
                 #'agent-repl-wire-decode-roster-repo-section-key)
           :header (agent-repl-wire--decode-message
                    "RosterRepoSection" 'header object
                    #'agent-repl-wire-decode-roster-repo-section-header)
           :fold (agent-repl-wire-decode-roster-repo-section-fold object)
           :divergence (agent-repl-wire--decode-message
                        "RosterRepoSection" 'divergence object
                        #'agent-repl-wire-decode-roster-repo-divergence)
           :entries (agent-repl-wire--decode-repeated
                     "RosterRepoSection" 'entries object
                     #'agent-repl-wire-decode-roster-repo-entry)))))

(defun agent-repl-wire-decode-roster-merged-section-header (value)
  "Decode `RosterMergedSection''s `header' field VALUE, a plist `(:label L)'.
The Merge Queue section's header carries NO count: each queue group
counts its own line."
  (let ((object (agent-repl-wire--object "RosterSectionHeader" value)))
    (agent-repl-wire--check-keys "RosterSectionHeader" object '(label))
    (agent-repl-wire--decoded
     "RosterSectionHeader"
     (list :label (agent-repl-wire--decode-message
                   "RosterSectionHeader" 'label object
                   #'agent-repl-wire-decode-roster-section-header-label)))))

(defun agent-repl-wire-decode-roster-merged-repo-group-key (value)
  "Decode `RosterMergedRepoGroup''s `key' field VALUE as a RosterRepoKey."
  (agent-repl-wire-decode-roster-repo-key value))

(defun agent-repl-wire-decode-roster-merged-repo-group-header-label (value)
  "Decode `RosterMergedRepoGroupHeader''s `label' field VALUE."
  (agent-repl-wire-decode-roster-label value))

(defun agent-repl-wire-decode-roster-merged-repo-group-header (value)
  "Decode VALUE as `RosterMergedRepoGroupHeader', a plist `(:label L)'.
A group's header is its repository's label and nothing to toggle."
  (let ((object (agent-repl-wire--object "RosterMergedRepoGroupHeader" value)))
    (agent-repl-wire--check-keys "RosterMergedRepoGroupHeader" object '(label))
    (agent-repl-wire--decoded
     "RosterMergedRepoGroupHeader"
     (list :label (agent-repl-wire--decode-message
                   "RosterMergedRepoGroupHeader" 'label object
                   #'agent-repl-wire-decode-roster-merged-repo-group-header-label)))))

(defun agent-repl-wire-decode-roster-merged-repo-group-shown (value)
  "Decode VALUE as the empty message `RosterMergedRepoGroupShown'."
  (agent-repl-wire--decode-empty "RosterMergedRepoGroupShown" value))

(defun agent-repl-wire-decode-roster-merged-repo-group-repository-collapsed (value)
  "Decode VALUE as the empty message `RosterMergedRepoGroupRepositoryCollapsed'."
  (agent-repl-wire--decode-empty "RosterMergedRepoGroupRepositoryCollapsed" value))

(defun agent-repl-wire-decode-roster-merged-repo-group-drawn (object)
  "Decode `RosterMergedRepoGroup''s `drawn' oneof from OBJECT.
THE ARM IS THE REPOSITORY'S FOLD, as the daemon resolved it; it is never
unset.  A `:repository-collapsed' group's workspaces are off the tab bar,
as every workspace of a collapsed repository is."
  (agent-repl-wire--decode-oneof
   "RosterMergedRepoGroup" 'drawn object
   '((shown :shown agent-repl-wire-decode-roster-merged-repo-group-shown)
     (repositoryCollapsed :repository-collapsed
                          agent-repl-wire-decode-roster-merged-repo-group-repository-collapsed))))

(defun agent-repl-wire-decode-roster-merged-repo-group (value)
  "Decode VALUE as `RosterMergedRepoGroup'.
A plist `(:key :header :merged-rows :drawn)'."
  (let ((object (agent-repl-wire--object "RosterMergedRepoGroup" value)))
    (agent-repl-wire--check-keys "RosterMergedRepoGroup" object
                                 '(key header mergedRows shown repositoryCollapsed))
    (agent-repl-wire--decoded
     "RosterMergedRepoGroup"
     (list :key (agent-repl-wire--decode-message
                 "RosterMergedRepoGroup" 'key object
                 #'agent-repl-wire-decode-roster-merged-repo-group-key)
           :header (agent-repl-wire--decode-message
                    "RosterMergedRepoGroup" 'header object
                    #'agent-repl-wire-decode-roster-merged-repo-group-header)
           :merged-rows (agent-repl-wire--decode-message
                         "RosterMergedRepoGroup" 'mergedRows object
                         #'agent-repl-wire-decode-roster-merged-rows)
           :drawn (agent-repl-wire-decode-roster-merged-repo-group-drawn object)))))

(defun agent-repl-wire-decode-roster-merged-section-expanded (value)
  "Decode VALUE as the empty message `RosterMergedSectionExpanded'."
  (agent-repl-wire--decode-empty "RosterMergedSectionExpanded" value))

(defun agent-repl-wire-decode-roster-merged-section-collapsed (value)
  "Decode VALUE as the empty message `RosterMergedSectionCollapsed'."
  (agent-repl-wire--decode-empty "RosterMergedSectionCollapsed" value))

(defun agent-repl-wire-decode-roster-merged-section-fold (object)
  "Decode `RosterMergedSection''s `fold' oneof from OBJECT.
THE ARM IS THE FOLD, the daemon's view state; it is never unset."
  (agent-repl-wire--decode-oneof
   "RosterMergedSection" 'fold object
   '((expanded :expanded agent-repl-wire-decode-roster-merged-section-expanded)
     (collapsed :collapsed agent-repl-wire-decode-roster-merged-section-collapsed))))

(defun agent-repl-wire-decode-roster-merge-queue-repo-group (value)
  "Decode VALUE as `RosterMergeQueueRepoGroup', `(:key :header :queue :count)'.
One repository's merge queue inside the merge queue section, under the
repository's label, with the merges in its line.  Emacs draws nothing
from it."
  (let ((object (agent-repl-wire--object "RosterMergeQueueRepoGroup" value)))
    (agent-repl-wire--check-keys "RosterMergeQueueRepoGroup" object '(key header queue count))
    (agent-repl-wire--decoded
     "RosterMergeQueueRepoGroup"
     (list :key (agent-repl-wire--decode-message
                 "RosterMergeQueueRepoGroup" 'key object
                 #'agent-repl-wire-decode-roster-repo-key)
           :header (agent-repl-wire--decode-message
                    "RosterMergeQueueRepoGroup" 'header object
                    #'agent-repl-wire-decode-roster-merged-repo-group-header)
           :queue (agent-repl-wire--decode-message
                   "RosterMergeQueueRepoGroup" 'queue object
                   #'agent-repl-wire-decode-roster-repo-merge-queue)
           :count (agent-repl-wire--decode-message
                   "RosterMergeQueueRepoGroup" 'count object
                   #'agent-repl-wire-decode-roster-section-count)))))

(defun agent-repl-wire-decode-roster-merged-section (value)
  "Decode VALUE as `RosterMergedSection'.
A plist `(:header :groups :queues :queue-label :merged-label :fold)'.
The Deploys section has no key of its own — the roster carries
exactly one.  Its queues sit in one `RosterMergeQueueRepoGroup' per
repository holding a merge, and its merged rows in one
`RosterMergedRepoGroup' per repository."
  (let ((object (agent-repl-wire--object "RosterMergedSection" value)))
    (agent-repl-wire--check-keys "RosterMergedSection" object
                                 '(header groups queues queueLabel mergedLabel expanded collapsed))
    (agent-repl-wire--decoded
     "RosterMergedSection"
     (list :header (agent-repl-wire--decode-message
                    "RosterMergedSection" 'header object
                    #'agent-repl-wire-decode-roster-merged-section-header)
           :groups (agent-repl-wire--decode-repeated
                    "RosterMergedSection" 'groups object
                    #'agent-repl-wire-decode-roster-merged-repo-group)
           :queues (agent-repl-wire--decode-repeated
                    "RosterMergedSection" 'queues object
                    #'agent-repl-wire-decode-roster-merge-queue-repo-group)
           :queue-label (agent-repl-wire--decode-message
                         "RosterMergedSection" 'queueLabel object
                         #'agent-repl-wire-decode-roster-label)
           :merged-label (agent-repl-wire--decode-message
                          "RosterMergedSection" 'mergedLabel object
                          #'agent-repl-wire-decode-roster-label)
           :fold (agent-repl-wire-decode-roster-merged-section-fold object)))))

;;;; ---- The hidden-workspaces section ----

(defun agent-repl-wire-decode-roster-hidden-section-header (value)
  "Decode `RosterHiddenSection''s `header' field VALUE."
  (agent-repl-wire-decode-roster-section-header value))

(defun agent-repl-wire-decode-roster-hidden-section-expanded (value)
  "Decode VALUE as the empty message `RosterHiddenSectionExpanded'."
  (agent-repl-wire--decode-empty "RosterHiddenSectionExpanded" value))

(defun agent-repl-wire-decode-roster-hidden-section-collapsed (value)
  "Decode VALUE as the empty message `RosterHiddenSectionCollapsed'."
  (agent-repl-wire--decode-empty "RosterHiddenSectionCollapsed" value))

(defun agent-repl-wire-decode-roster-hidden-section-fold (object)
  "Decode `RosterHiddenSection''s `fold' oneof from OBJECT.
THE ARM IS THE FOLD, the daemon's view state; it is never unset.  It folds
sidebar rows only: a hidden workspace is off the tab bar either way."
  (agent-repl-wire--decode-oneof
   "RosterHiddenSection" 'fold object
   '((expanded :expanded agent-repl-wire-decode-roster-hidden-section-expanded)
     (collapsed :collapsed agent-repl-wire-decode-roster-hidden-section-collapsed))))

(defun agent-repl-wire-decode-roster-hidden-section (value)
  "Decode VALUE as `RosterHiddenSection', a plist `(:header :hidden-rows :fold)'.
It has no key of its own — the roster carries exactly one — so a `key' on
the wire is an unknown field."
  (let ((object (agent-repl-wire--object "RosterHiddenSection" value)))
    (agent-repl-wire--check-keys "RosterHiddenSection" object
                                 '(header hiddenRows expanded collapsed))
    (agent-repl-wire--decoded
     "RosterHiddenSection"
     (list :header (agent-repl-wire--decode-message
                    "RosterHiddenSection" 'header object
                    #'agent-repl-wire-decode-roster-hidden-section-header)
           :hidden-rows (agent-repl-wire--decode-message
                         "RosterHiddenSection" 'hiddenRows object
                         #'agent-repl-wire-decode-roster-hidden-rows)
           :fold (agent-repl-wire-decode-roster-hidden-section-fold object)))))

;;;; ---- The open-workspaces section ----

(defun agent-repl-wire-decode-roster-open-section-label (value)
  "Decode `RosterOpenSection''s `label' field VALUE."
  (agent-repl-wire-decode-roster-label value))

(defun agent-repl-wire-decode-roster-open-section-expanded (value)
  "Decode VALUE as the empty message `RosterOpenSectionExpanded'."
  (agent-repl-wire--decode-empty "RosterOpenSectionExpanded" value))

(defun agent-repl-wire-decode-roster-open-section-collapsed (value)
  "Decode VALUE as the empty message `RosterOpenSectionCollapsed'."
  (agent-repl-wire--decode-empty "RosterOpenSectionCollapsed" value))

(defun agent-repl-wire-decode-roster-open-section (value)
  "Decode VALUE as `RosterOpenSection'.
The result is a plist `(:label :fold)'.
The heading over the repository sections; its fold folds sidebar rows
only, so the tab bar reads nothing off it.  It is decoded because the
codec refuses unknown fields."
  (let ((object (agent-repl-wire--object "RosterOpenSection" value)))
    (agent-repl-wire--check-keys "RosterOpenSection" object
                                 '(label expanded collapsed))
    (agent-repl-wire--decoded
     "RosterOpenSection"
     (list :label (agent-repl-wire--decode-message
                   "RosterOpenSection" 'label object
                   #'agent-repl-wire-decode-roster-open-section-label)
           :fold (agent-repl-wire--decode-oneof
                  "RosterOpenSection" 'fold object
                  '((expanded :expanded agent-repl-wire-decode-roster-open-section-expanded)
                    (collapsed :collapsed agent-repl-wire-decode-roster-open-section-collapsed)))))))

;;;; ---- The views and the roster ----

(defun agent-repl-wire-decode-roster-repository-view-sections (value)
  "Decode one element of `RosterRepositoryView''s repeated `sections'."
  (agent-repl-wire-decode-roster-repo-section value))

(defun agent-repl-wire-decode-roster-open-count (value)
  "Decode VALUE as `RosterOpenCount', a plist `(:workspaces)'.
How many rows the open-workspaces section lists; the webview draws it.
Decoded because the codec refuses unknown fields."
  (let ((object (agent-repl-wire--object "RosterOpenCount" value)))
    (agent-repl-wire--check-keys "RosterOpenCount" object '(workspaces))
    (agent-repl-wire--decoded
     "RosterOpenCount"
     (list :workspaces (agent-repl-wire--decode-uint32
                        "RosterOpenCount" 'workspaces object)))))

(defun agent-repl-wire-decode-roster-repository-view (value)
  "Decode VALUE as `RosterRepositoryView', `(:sections LIST :open-count C)'.
The rail's one grouping: the open workspaces, one section per repository.
Order is the resolver's; clients do not re-sort."
  (let ((object (agent-repl-wire--object "RosterRepositoryView" value)))
    (agent-repl-wire--check-keys "RosterRepositoryView" object '(sections openCount))
    (agent-repl-wire--decoded
     "RosterRepositoryView"
     (list :sections (agent-repl-wire--decode-repeated
                      "RosterRepositoryView" 'sections object
                      #'agent-repl-wire-decode-roster-repository-view-sections)
           :open-count (agent-repl-wire--decode-message
                        "RosterRepositoryView" 'openCount object
                        #'agent-repl-wire-decode-roster-open-count)))))

(defun agent-repl-wire-decode-roster-current-workspace-workspace (value)
  "Decode `RosterCurrentWorkspace''s `workspace' field VALUE."
  (agent-repl-wire-decode-workspace-ref value))

(defun agent-repl-wire-decode-roster-current-workspace (value)
  "Decode VALUE as `RosterCurrentWorkspace', a plist `(:workspace REF)'.
Compared against a row's workspace by IDENTITY, never against a display
name, which is not unique."
  (let ((object (agent-repl-wire--object "RosterCurrentWorkspace" value)))
    (agent-repl-wire--check-keys "RosterCurrentWorkspace" object '(workspace))
    (agent-repl-wire--decoded
     "RosterCurrentWorkspace"
     (list :workspace (agent-repl-wire--decode-message
                       "RosterCurrentWorkspace" 'workspace object
                       #'agent-repl-wire-decode-roster-current-workspace-workspace)))))

(defun agent-repl-wire-decode-roster-version (value)
  "Decode VALUE as `RosterVersion', a plist `(:text)'.
The agent-repl release the webview draws in the rail's header; the tab
bar reads nothing off it.  Decoded because the codec refuses unknown
fields."
  (let ((object (agent-repl-wire--object "RosterVersion" value)))
    (agent-repl-wire--check-keys "RosterVersion" object '(text))
    (agent-repl-wire--decoded
     "RosterVersion"
     (list :text (agent-repl-wire--decode-string "RosterVersion" 'text object)))))

(defun agent-repl-wire-decode-workspace-roster-repository (value)
  "Decode `WorkspaceRoster''s `repository' field VALUE."
  (agent-repl-wire-decode-roster-repository-view value))

(defun agent-repl-wire-decode-workspace-roster-recently-merged (value)
  "Decode `WorkspaceRoster''s `recently_merged' field VALUE."
  (agent-repl-wire-decode-roster-merged-section value))

(defun agent-repl-wire-decode-workspace-roster-hidden (value)
  "Decode `WorkspaceRoster''s `hidden' field VALUE."
  (agent-repl-wire-decode-roster-hidden-section value))

(defun agent-repl-wire-decode-workspace-roster-open (value)
  "Decode `WorkspaceRoster''s `open' field VALUE."
  (agent-repl-wire-decode-roster-open-section value))

(defun agent-repl-wire-decode-workspace-roster-closed (value)
  "Decode `WorkspaceRoster''s `closed' field VALUE."
  (agent-repl-wire-decode-roster-closed-rows value))

;;;; ---- The sidebar's apps: the switcher and the thread section ----
;;
;; Emacs draws neither (the webapp's rail does), but it reads both: the
;; thread section is where Emacs learns the threads it opens workspaces for
;; and picks among (threads.el), and the switcher is decoded whole so a
;; roster carrying it is a roster this codec reads.

(defun agent-repl-wire-decode-roster-text (message-name value)
  "Decode VALUE as MESSAGE-NAME, a message whose one field is `text'."
  (let ((object (agent-repl-wire--object message-name value)))
    (agent-repl-wire--check-keys message-name object '(text))
    (agent-repl-wire--decode-string message-name 'text object)))

(defun agent-repl-wire-decode-sidebar-app (value)
  "Decode VALUE as `SidebarApp', the arm keyword `:workspaces' or `:chat'."
  (let ((object (agent-repl-wire--object "SidebarApp" value)))
    (agent-repl-wire--check-keys "SidebarApp" object '(workspaces chat))
    (plist-get
     (agent-repl-wire--decode-oneof
      "SidebarApp" 'app object
      '((workspaces :workspaces agent-repl-wire-decode-sidebar-app-workspaces)
        (chat :chat agent-repl-wire-decode-sidebar-app-chat)))
     :arm)))

(defun agent-repl-wire-decode-sidebar-app-workspaces (value)
  "Decode VALUE as the empty `SidebarAppWorkspaces'."
  (agent-repl-wire--decode-empty "SidebarAppWorkspaces" value))

(defun agent-repl-wire-decode-sidebar-app-chat (value)
  "Decode VALUE as the empty `SidebarAppChat'."
  (agent-repl-wire--decode-empty "SidebarAppChat" value))

(defun agent-repl-wire-decode-roster-app-switcher-title (value)
  "Decode `RosterAppSwitcher''s `title' field VALUE as its text."
  (agent-repl-wire-decode-roster-text "RosterAppSwitcherTitle" value))

(defun agent-repl-wire-decode-roster-app-switcher-option-label (value)
  "Decode `RosterAppSwitcherOption''s `label' field VALUE as its text."
  (agent-repl-wire-decode-roster-text "RosterAppSwitcherOptionLabel" value))


(defun agent-repl-wire-decode-roster-app-switcher-option-shown (value)
  "Decode VALUE as the empty `RosterAppSwitcherOptionShown' and return t."
  (agent-repl-wire--decode-empty "RosterAppSwitcherOptionShown" value)
  t)

(defun agent-repl-wire-decode-roster-app-switcher-option (value)
  "Decode VALUE as `RosterAppSwitcherOption', `(:label :app :shown)'."
  (let ((object (agent-repl-wire--object "RosterAppSwitcherOption" value)))
    (agent-repl-wire--check-keys "RosterAppSwitcherOption" object '(label app shown))
    (agent-repl-wire--decoded
     "RosterAppSwitcherOption"
     (list :label (agent-repl-wire--decode-message
                   "RosterAppSwitcherOption" 'label object
                   #'agent-repl-wire-decode-roster-app-switcher-option-label)
           :app (agent-repl-wire--decode-message
                 "RosterAppSwitcherOption" 'app object #'agent-repl-wire-decode-sidebar-app)
           :shown (agent-repl-wire--decode-optional-message
                   "RosterAppSwitcherOption" 'shown object
                   #'agent-repl-wire-decode-roster-app-switcher-option-shown)))))

(defun agent-repl-wire-decode-roster-app-switcher (value)
  "Decode VALUE as `RosterAppSwitcher', `(:title :shown :options)'.
SHOWN is the app keyword the rail shows, which follows the selection."
  (let ((object (agent-repl-wire--object "RosterAppSwitcher" value)))
    (agent-repl-wire--check-keys "RosterAppSwitcher" object '(title shown options))
    (agent-repl-wire--decoded
     "RosterAppSwitcher"
     (list :title (agent-repl-wire--decode-message
                   "RosterAppSwitcher" 'title object
                   #'agent-repl-wire-decode-roster-app-switcher-title)
           :shown (agent-repl-wire--decode-message
                   "RosterAppSwitcher" 'shown object #'agent-repl-wire-decode-sidebar-app)
           :options (agent-repl-wire--decode-repeated
                     "RosterAppSwitcher" 'options object
                     #'agent-repl-wire-decode-roster-app-switcher-option)))))

(defun agent-repl-wire-decode-roster-thread-new-button (value)
  "Decode VALUE as `RosterThreadNewButton', its label."
  (let ((object (agent-repl-wire--object "RosterThreadNewButton" value)))
    (agent-repl-wire--check-keys "RosterThreadNewButton" object '(label))
    (agent-repl-wire--decode-string "RosterThreadNewButton" 'label object)))

(defun agent-repl-wire-decode-roster-thread-name (value)
  "Decode `RosterThreadRow''s `name' field VALUE as its text."
  (agent-repl-wire-decode-roster-text "RosterThreadName" value))

(defun agent-repl-wire-decode-roster-thread-summary (value)
  "Decode `RosterThreadRow''s `summary' field VALUE as its text."
  (agent-repl-wire-decode-roster-text "RosterThreadSummary" value))

(defun agent-repl-wire-decode-roster-thread-current (value)
  "Decode VALUE as the empty `RosterThreadCurrent' and return t."
  (agent-repl-wire--decode-empty "RosterThreadCurrent" value)
  t)

(defun agent-repl-wire-decode-roster-thread-open (value)
  "Decode VALUE as the empty `RosterThreadOpen'."
  (agent-repl-wire--decode-empty "RosterThreadOpen" value))

(defun agent-repl-wire-decode-roster-thread-archived (value)
  "Decode VALUE as the empty `RosterThreadArchived'."
  (agent-repl-wire--decode-empty "RosterThreadArchived" value))

(defun agent-repl-wire-decode-roster-thread-standing (value)
  "Decode VALUE as `RosterThreadStanding', `(:arm :open|:archived :value nil)'.
THE ARM IS THE STANDING, so an unset one is a contract breach."
  (let ((object (agent-repl-wire--object "RosterThreadStanding" value)))
    (agent-repl-wire--check-keys "RosterThreadStanding" object '(open archived))
    (agent-repl-wire--decoded
     "RosterThreadStanding"
     (agent-repl-wire--decode-oneof
      "RosterThreadStanding" 'standing object
      '((open :open agent-repl-wire-decode-roster-thread-open)
        (archived :archived agent-repl-wire-decode-roster-thread-archived))))))

;;;; ---- A thread's outline ----
;;
;; The webapp's rail draws it; Emacs reads it whole, so a roster carrying it
;; is a roster this codec reads.

(defun agent-repl-wire-decode-zettel-node-id (value)
  "Decode VALUE as `zettel.v1.NodeId', the node's id string."
  (let ((object (agent-repl-wire--object "NodeId" value)))
    (agent-repl-wire--check-keys "NodeId" object '(id))
    (agent-repl-wire--decode-string "NodeId" 'id object)))

(defun agent-repl-wire-decode-roster-thread-outline-label (value)
  "Decode VALUE as `RosterThreadOutlineLabel', `(:text S :question Q)'.
TEXT is what a row draws (the question's abbreviation, or the question
while it has none); QUESTION is the question whole."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineLabel" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineLabel" object '(text question))
    (list :text (agent-repl-wire--decode-string "RosterThreadOutlineLabel" 'text object)
          :question (agent-repl-wire--decode-string "RosterThreadOutlineLabel" 'question object))))

(defun agent-repl-wire-decode-roster-thread-outline-crumb (value)
  "Decode VALUE as `RosterThreadOutlineCrumb', `(:node ID :label L)'.
LABEL is a decoded `RosterThreadOutlineLabel'."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineCrumb" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineCrumb" object '(node label))
    (list :node (agent-repl-wire--decode-message
                 "RosterThreadOutlineCrumb" 'node object #'agent-repl-wire-decode-zettel-node-id)
          :label (agent-repl-wire--decode-message
                  "RosterThreadOutlineCrumb" 'label object
                  #'agent-repl-wire-decode-roster-thread-outline-label))))

(defun agent-repl-wire-decode-roster-thread-outline-breadcrumb-whole (value)
  "Decode VALUE as `RosterThreadOutlineBreadcrumbWhole', its crumbs."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineBreadcrumbWhole" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineBreadcrumbWhole" object '(crumbs))
    (agent-repl-wire--decode-repeated "RosterThreadOutlineBreadcrumbWhole" 'crumbs object
                                      #'agent-repl-wire-decode-roster-thread-outline-crumb)))

(defun agent-repl-wire-decode-roster-thread-outline-breadcrumb-elided (value)
  "Decode VALUE as `RosterThreadOutlineBreadcrumbElided'.
The result is `(:first C :omitted N :last C)'."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineBreadcrumbElided" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineBreadcrumbElided" object '(first omitted last))
    (list :first (agent-repl-wire--decode-message
                  "RosterThreadOutlineBreadcrumbElided" 'first object
                  #'agent-repl-wire-decode-roster-thread-outline-crumb)
          :omitted (agent-repl-wire--decode-uint32 "RosterThreadOutlineBreadcrumbElided" 'omitted object)
          :last (agent-repl-wire--decode-message
                 "RosterThreadOutlineBreadcrumbElided" 'last object
                 #'agent-repl-wire-decode-roster-thread-outline-crumb))))

(defconst agent-repl-wire-roster-thread-outline-breadcrumb-arms
  '((whole :whole agent-repl-wire-decode-roster-thread-outline-breadcrumb-whole)
    (elided :elided agent-repl-wire-decode-roster-thread-outline-breadcrumb-elided))
  "`RosterThreadOutlineBreadcrumb.extent''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-roster-thread-outline-breadcrumb (value)
  "Decode VALUE as `RosterThreadOutlineBreadcrumb', its extent oneof."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineBreadcrumb" value)))
    (agent-repl-wire--check-keys
     "RosterThreadOutlineBreadcrumb" object
     (mapcar #'car agent-repl-wire-roster-thread-outline-breadcrumb-arms))
    (agent-repl-wire--decode-oneof "RosterThreadOutlineBreadcrumb" 'extent object
                                   agent-repl-wire-roster-thread-outline-breadcrumb-arms)))

(defun agent-repl-wire-decode-roster-thread-outline-folded (value)
  "Decode VALUE as `RosterThreadOutlineFolded', the folded runs' count."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineFolded" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineFolded" object '(runs))
    (agent-repl-wire--decode-uint32 "RosterThreadOutlineFolded" 'runs object)))

(defconst agent-repl-wire-roster-thread-outline-place-arms
  '((off :off (lambda (v) (agent-repl-wire--decode-empty "RosterThreadOutlineOff" v)))
    (onPath :on-path (lambda (v) (agent-repl-wire--decode-empty "RosterThreadOutlineOnPath" v)))
    (shown :shown (lambda (v) (agent-repl-wire--decode-empty "RosterThreadOutlineShown" v))))
  "`RosterThreadOutlinePlace.place''s arms: (WIRE-KEY ARM-KEYWORD DECODER).")

(defun agent-repl-wire-decode-roster-thread-outline-place (value)
  "Decode VALUE as `RosterThreadOutlinePlace', its place arm's keyword."
  (let ((object (agent-repl-wire--object "RosterThreadOutlinePlace" value)))
    (agent-repl-wire--check-keys
     "RosterThreadOutlinePlace" object (mapcar #'car agent-repl-wire-roster-thread-outline-place-arms))
    (plist-get (agent-repl-wire--decode-oneof "RosterThreadOutlinePlace" 'place object
                                              agent-repl-wire-roster-thread-outline-place-arms)
               :arm)))

(defun agent-repl-wire-decode-roster-thread-outline-row (value)
  "Decode VALUE as `RosterThreadOutlineRow',
`(:node ID :label L :depth N :turns N :folded N :place P)', LABEL a
decoded `RosterThreadOutlineLabel', FOLDED nil when nothing folds below
the row."
  (let ((object (agent-repl-wire--object "RosterThreadOutlineRow" value)))
    (agent-repl-wire--check-keys "RosterThreadOutlineRow" object '(node label depth turns folded place))
    (list :node (agent-repl-wire--decode-message
                 "RosterThreadOutlineRow" 'node object #'agent-repl-wire-decode-zettel-node-id)
          :label (agent-repl-wire--decode-message
                  "RosterThreadOutlineRow" 'label object #'agent-repl-wire-decode-roster-thread-outline-label)
          :depth (agent-repl-wire--decode-uint32 "RosterThreadOutlineRow" 'depth object)
          :turns (agent-repl-wire--decode-uint32 "RosterThreadOutlineRow" 'turns object)
          :folded (agent-repl-wire--decode-optional-message
                   "RosterThreadOutlineRow" 'folded object #'agent-repl-wire-decode-roster-thread-outline-folded)
          :place (agent-repl-wire--decode-message
                  "RosterThreadOutlineRow" 'place object #'agent-repl-wire-decode-roster-thread-outline-place))))

(defun agent-repl-wire-decode-roster-thread-outline (value)
  "Decode VALUE as `RosterThreadOutline', `(:breadcrumb B :rows ROWS)'."
  (let ((object (agent-repl-wire--object "RosterThreadOutline" value)))
    (agent-repl-wire--check-keys "RosterThreadOutline" object '(breadcrumb rows))
    (list :breadcrumb (agent-repl-wire--decode-optional-message
                       "RosterThreadOutline" 'breadcrumb object
                       #'agent-repl-wire-decode-roster-thread-outline-breadcrumb)
          :rows (agent-repl-wire--decode-repeated "RosterThreadOutline" 'rows object
                                                  #'agent-repl-wire-decode-roster-thread-outline-row))))

(defun agent-repl-wire-decode-roster-thread-row (value)
  "Decode VALUE as `RosterThreadRow',
`(:thread REF :name :summary :current :standing S :outline O)'.
SUMMARY is nil when none is set; CURRENT is t on the selected thread;
STANDING is `agent-repl-wire-decode-roster-thread-standing''s; OUTLINE is
nil on every row but the current thread's."
  (let ((object (agent-repl-wire--object "RosterThreadRow" value)))
    (agent-repl-wire--check-keys "RosterThreadRow" object '(thread name summary current standing outline))
    (agent-repl-wire--decoded
     "RosterThreadRow"
     (list :thread (agent-repl-wire--decode-message
                    "RosterThreadRow" 'thread object #'agent-repl-wire-decode-workspace-ref)
           :name (agent-repl-wire--decode-message
                  "RosterThreadRow" 'name object #'agent-repl-wire-decode-roster-thread-name)
           :summary (agent-repl-wire--decode-optional-message
                     "RosterThreadRow" 'summary object
                     #'agent-repl-wire-decode-roster-thread-summary)
           :current (agent-repl-wire--decode-optional-message
                     "RosterThreadRow" 'current object
                     #'agent-repl-wire-decode-roster-thread-current)
           :standing (agent-repl-wire--decode-message
                      "RosterThreadRow" 'standing object
                      #'agent-repl-wire-decode-roster-thread-standing)
           :outline (agent-repl-wire--decode-optional-message
                     "RosterThreadRow" 'outline object
                     #'agent-repl-wire-decode-roster-thread-outline)))))

(defun agent-repl-wire-decode-roster-thread-section (value)
  "Decode VALUE as `RosterThreadSection', `(:new-thread LABEL :rows ROWS)'.
ROWS are every open thread and then every archived one, each the one last
interacted with first."
  (let ((object (agent-repl-wire--object "RosterThreadSection" value)))
    (agent-repl-wire--check-keys "RosterThreadSection" object '(newThread rows))
    (agent-repl-wire--decoded
     "RosterThreadSection"
     (list :new-thread (agent-repl-wire--decode-message
                        "RosterThreadSection" 'newThread object
                        #'agent-repl-wire-decode-roster-thread-new-button)
           :rows (agent-repl-wire--decode-repeated
                  "RosterThreadSection" 'rows object
                  #'agent-repl-wire-decode-roster-thread-row)))))

(defun agent-repl-wire-decode-workspace-roster-current (value)
  "Decode `WorkspaceRoster''s optional `current' field VALUE."
  (agent-repl-wire-decode-roster-current-workspace value))

(defun agent-repl-wire-decode-workspace-roster-version (value)
  "Decode `WorkspaceRoster''s optional `version' field VALUE."
  (agent-repl-wire-decode-roster-version value))

(defun agent-repl-wire-decode-roster-cold-gate (value)
  "Decode VALUE as `RosterColdGate'.
Returns `(:workspace REF :row ID)'.  THE CARD IS THE WEBHOST\='S TO DRAW
\(webapp/src/host/gate.ts): Emacs hides the input from the host view\='s
gate, never from this one, so the card is checked to be a message and not
read further."
  (let ((object (agent-repl-wire--object "RosterColdGate" value)))
    (agent-repl-wire--check-keys "RosterColdGate" object '(workspace row gate))
    (agent-repl-wire--decode-message "RosterColdGate" 'gate object
                                     (lambda (gate) (agent-repl-wire--object "RosterColdGate.gate" gate)))
    (agent-repl-wire--decoded
     "RosterColdGate"
     (list :workspace (agent-repl-wire--decode-message
                       "RosterColdGate" 'workspace object
                       #'agent-repl-wire-decode-workspace-ref)
           :row (agent-repl-wire--decode-message
                 "RosterColdGate" 'row object
                 #'agent-repl-wire-decode-feed-id)))))

(defun agent-repl-wire-decode-workspace-roster (value)
  "Decode VALUE as `WorkspaceRoster'.
Returns `(:repository V :open O :hidden H :recently-merged S :closed C
:current C-or-nil :revision N :version V-or-nil :app-switcher A
:threads T :cold-gates G)'.  THERE IS NO TAB BAR
MESSAGE: the Open section's rows are the tab bar, each stating its own
tab."
  (let ((object (agent-repl-wire--object "WorkspaceRoster" value)))
    (agent-repl-wire--check-keys
     "WorkspaceRoster" object
     '(repository open hidden recentlyMerged closed current revision version appSwitcher threads coldGates))
    (agent-repl-wire--decoded
     "WorkspaceRoster"
     (list :repository (agent-repl-wire--decode-message
                        "WorkspaceRoster" 'repository object
                        #'agent-repl-wire-decode-workspace-roster-repository)
           :open (agent-repl-wire--decode-message
                  "WorkspaceRoster" 'open object
                  #'agent-repl-wire-decode-workspace-roster-open)
           :hidden (agent-repl-wire--decode-message
                    "WorkspaceRoster" 'hidden object
                    #'agent-repl-wire-decode-workspace-roster-hidden)
           :recently-merged (agent-repl-wire--decode-message
                             "WorkspaceRoster" 'recentlyMerged object
                             #'agent-repl-wire-decode-workspace-roster-recently-merged)
           :closed (agent-repl-wire--decode-message
                    "WorkspaceRoster" 'closed object
                    #'agent-repl-wire-decode-workspace-roster-closed)
           :current (agent-repl-wire--decode-optional-message
                     "WorkspaceRoster" 'current object
                     #'agent-repl-wire-decode-workspace-roster-current)
           :revision (agent-repl-wire--decode-uint64 "WorkspaceRoster" 'revision object)
           :version (agent-repl-wire--decode-optional-message
                     "WorkspaceRoster" 'version object
                     #'agent-repl-wire-decode-workspace-roster-version)
           :app-switcher (agent-repl-wire--decode-message
                          "WorkspaceRoster" 'appSwitcher object
                          #'agent-repl-wire-decode-roster-app-switcher)
           :threads (agent-repl-wire--decode-message
                     "WorkspaceRoster" 'threads object
                     #'agent-repl-wire-decode-roster-thread-section)
           :cold-gates (agent-repl-wire--decode-repeated
                        "WorkspaceRoster" 'coldGates object
                        #'agent-repl-wire-decode-roster-cold-gate)))))

;;;; ---- The rpc ----

(defun agent-repl-wire-encode-watch-workspace-roster-request (value)
  "Encode the WatchWorkspaceRosterRequest from VALUE — empty: the roster is global."
  (agent-repl-wire--encoded
   "WatchWorkspaceRosterRequest"
   (agent-repl-wire--encode-empty "WatchWorkspaceRosterRequest" value)))

(defun agent-repl-wire-decode-watch-workspace-roster-response-roster (value)
  "Decode `WatchWorkspaceRosterResponse''s `roster' field VALUE."
  (agent-repl-wire-decode-workspace-roster value))

(defun agent-repl-wire-decode-watch-workspace-roster-response-ending (value)
  "Decode `WatchWorkspaceRosterResponse''s `ending' push arm VALUE."
  (agent-repl-wire-decode-daemon-stream-ending value))

(defun agent-repl-wire-decode-watch-workspace-roster-response (value)
  "Decode VALUE as `WatchWorkspaceRosterResponse', `(:arm ARM :value V)'.
THE ARM IS WHAT THE FRAME CARRIES: `:roster' is the roster, always whole
and never a delta -- a partially-applied roster is unrepresentable by
construction -- and `:ending' (value nil) is the planned end of the
stream, its last frame before a clean end.  An unset oneof is a breach,
never an empty sidebar."
  (let ((object (agent-repl-wire--object "WatchWorkspaceRosterResponse" value)))
    (agent-repl-wire--check-keys "WatchWorkspaceRosterResponse" object '(roster ending))
    (agent-repl-wire--decoded
     "WatchWorkspaceRosterResponse"
     (agent-repl-wire--decode-oneof
      "WatchWorkspaceRosterResponse" 'push object
      '((roster :roster agent-repl-wire-decode-watch-workspace-roster-response-roster)
        (ending :ending agent-repl-wire-decode-watch-workspace-roster-response-ending))))))

(provide 'wire-roster)

;;; wire-roster.el ends here
