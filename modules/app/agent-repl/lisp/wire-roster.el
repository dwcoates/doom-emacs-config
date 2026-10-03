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
(declare-function agent-repl-wire--decode-message "wire-common")
(declare-function agent-repl-wire--decode-oneof "wire-common")
(declare-function agent-repl-wire--decode-optional-message "wire-common")
(declare-function agent-repl-wire--decode-repeated "wire-common")
(declare-function agent-repl-wire--decode-string "wire-common")
(declare-function agent-repl-wire--decoded "wire-common")
(declare-function agent-repl-wire-decode-daemon-stream-ending "wire-common")
(declare-function agent-repl-wire--encode-empty "wire-common")
(declare-function agent-repl-wire--encoded "wire-common")
(declare-function agent-repl-wire--object "wire-common")
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

(defun agent-repl-wire-decode-roster-task-done (value)
  "Decode VALUE as `RosterTaskDone', a plist `(:done)'."
  (let ((object (agent-repl-wire--object "RosterTaskDone" value)))
    (agent-repl-wire--check-keys "RosterTaskDone" object '(done))
    (agent-repl-wire--decoded
     "RosterTaskDone"
     (list :done (agent-repl-wire--decode-bool "RosterTaskDone" 'done object)))))

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
PRESENCE IS THE FACT, as for the markers above.

Emacs reads it for ONE decision: the view dwell's threshold
\(`agent-repl--tab-dwell-seconds'), because an unread `done' outranks
`idle_async' and so the status alone cannot say that background work runs
beside it."
  (agent-repl-wire--decode-empty "RosterRowDetachedLive" value)
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

(defun agent-repl-wire-decode-roster-row-last-selected (value)
  "Decode VALUE as `RosterRowLastSelected', a plist `(:at-ms)'.
The DURABLE instant the user last selected the workspace, epoch
MILLISECONDS, read from the daemon's record so it survives an Emacs
restart.  Emacs orders by it and never draws it
\(`agent-repl-roster-selection-recency-order'); the when-column is
`RosterRowWhen'."
  (let ((object (agent-repl-wire--object "RosterRowLastSelected" value)))
    (agent-repl-wire--check-keys "RosterRowLastSelected" object '(atMs))
    (agent-repl-wire--decoded
     "RosterRowLastSelected"
     (list :at-ms (agent-repl-wire--decode-int64
                   "RosterRowLastSelected" 'atMs object)))))

;;;; ---- The when column ----
;;
;; REGRESSION WATCH (2026-09-15): this strict decoder rejects the whole
;; WatchWorkspaceRoster push on an unknown `shown' arm, which blanks the roster
;; and drops every tab to a stale blue status.  When the daemon adds a `when'
;; arm (it added `active' and `created' here), it MUST be added to both the
;; oneof list and the check-keys allow-list below in the same change, or the
;; tab bar goes dark.  Watch for a new arm outrunning this file again.

(defun agent-repl-wire-decode-roster-row-when-merged (value)
  "Decode VALUE as `RosterRowWhenMerged', a plist `(:at-ms)'."
  (let ((object (agent-repl-wire--object "RosterRowWhenMerged" value)))
    (agent-repl-wire--check-keys "RosterRowWhenMerged" object '(atMs))
    (agent-repl-wire--decoded
     "RosterRowWhenMerged"
     (list :at-ms (agent-repl-wire--decode-int64
                   "RosterRowWhenMerged" 'atMs object)))))

(defun agent-repl-wire-decode-roster-row-when-active (value)
  "Decode VALUE as `RosterRowWhenActive', a plist `(:at-ms)'.
The workspace's last-activity instant; the client renders the age and ticks."
  (let ((object (agent-repl-wire--object "RosterRowWhenActive" value)))
    (agent-repl-wire--check-keys "RosterRowWhenActive" object '(atMs))
    (agent-repl-wire--decoded
     "RosterRowWhenActive"
     (list :at-ms (agent-repl-wire--decode-int64
                   "RosterRowWhenActive" 'atMs object)))))

(defun agent-repl-wire-decode-roster-row-when-created (value)
  "Decode VALUE as `RosterRowWhenCreated', a plist `(:at-ms)'."
  (let ((object (agent-repl-wire--object "RosterRowWhenCreated" value)))
    (agent-repl-wire--check-keys "RosterRowWhenCreated" object '(atMs))
    (agent-repl-wire--decoded
     "RosterRowWhenCreated"
     (list :at-ms (agent-repl-wire--decode-int64
                   "RosterRowWhenCreated" 'atMs object)))))

(defun agent-repl-wire-decode-roster-row-when-shown (value)
  "Decode `RosterRowWhen''s `shown' oneof from the object VALUE.
UNSET IS LEGAL and means nothing to show — never active, not created, not
merged — so the column is empty rather than \"0ms ago\"."
  (agent-repl-wire--decode-oneof
   "RosterRowWhen" 'shown value
   '((active :active agent-repl-wire-decode-roster-row-when-active)
     (created :created agent-repl-wire-decode-roster-row-when-created)
     (merged :merged agent-repl-wire-decode-roster-row-when-merged))
   t))

(defun agent-repl-wire-decode-roster-row-when (value)
  "Decode VALUE as `RosterRowWhen': the oneof plist, or nil when unset."
  (let ((object (agent-repl-wire--object "RosterRowWhen" value)))
    (agent-repl-wire--check-keys "RosterRowWhen" object
                                 '(active created merged))
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

(defun agent-repl-wire-decode-roster-row-detail (value)
  "Decode VALUE as `RosterRowDetail', a plist of its three lines.
Each line is PRESENT OR ABSENT BY MESSAGE PRESENCE rather than by an empty
string — the proto says so at the message — so an absent line decodes to
nil and is omitted, never rendered blank.  This is the one place in the
roster where an absent message field is not a breach."
  (let ((object (agent-repl-wire--object "RosterRowDetail" value)))
    (agent-repl-wire--check-keys "RosterRowDetail" object '(branch parentBranch summary))
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
                     #'agent-repl-wire-decode-roster-row-detail-summary)))))

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

(defconst agent-repl-wire-roster-row-status-arms
  '((submitting :submitting agent-repl-wire-decode-roster-row-status-submitting)
    (thinking :thinking agent-repl-wire-decode-roster-row-status-thinking)
    (clearing :clearing agent-repl-wire-decode-roster-row-status-clearing)
    (compacting :compacting agent-repl-wire-decode-roster-row-status-compacting)
    (permission :permission agent-repl-wire-decode-roster-row-status-permission)
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
    (init :init agent-repl-wire-decode-roster-row-status-init)
    (severed :severed agent-repl-wire-decode-roster-row-status-severed)
    (startFailed :start-failed agent-repl-wire-decode-roster-row-status-start-failed)
    (degraded :degraded agent-repl-wire-decode-roster-row-status-degraded)
    (dead :dead agent-repl-wire-decode-roster-row-status-dead)
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

(defun agent-repl-wire-decode-roster-row-children (value)
  "Decode one element of `RosterRow''s repeated `children' field VALUE.
Nested workspaces — a spawned family under its parent — in render order."
  (agent-repl-wire-decode-roster-row value))

(defconst agent-repl-wire--roster-row-keys
  (append '(workspace attention priority viewed reviving detachedLive lastSelected availability name current children when detail closed)
          (mapcar #'car agent-repl-wire-roster-row-status-arms))
  "Every key `RosterRow' may carry: its own fields plus the 22 status arms.")

(defun agent-repl-wire-decode-roster-row (value)
  "Decode VALUE as `RosterRow'.
Returns `(:workspace W :attention A :priority P :viewed V :reviving R
:detached-live DL :last-selected L :availability AV :name N :status
S :current C :children ROWS :when WHEN :detail D :closed CLOSED)', with
the message tree preserved as the contract spells it."
  (let ((object (agent-repl-wire--object "RosterRow" value)))
    (agent-repl-wire--check-keys "RosterRow" object agent-repl-wire--roster-row-keys)
    (agent-repl-wire--decoded
     "RosterRow"
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
           :reviving (agent-repl-wire--decode-optional-message
                      "RosterRow" 'reviving object
                      #'agent-repl-wire-decode-roster-row-reviving)
           :detached-live (agent-repl-wire--decode-optional-message
                           "RosterRow" 'detachedLive object
                           #'agent-repl-wire-decode-roster-row-detached-live)
           :last-selected (agent-repl-wire--decode-optional-message
                           "RosterRow" 'lastSelected object
                           #'agent-repl-wire-decode-roster-row-last-selected)
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
           :children (agent-repl-wire--decode-repeated
                      "RosterRow" 'children object
                      #'agent-repl-wire-decode-roster-row-children)
           :when (agent-repl-wire--decode-message
                  "RosterRow" 'when object
                  #'agent-repl-wire-decode-roster-row-when)
           :detail (agent-repl-wire--decode-message
                    "RosterRow" 'detail object
                    #'agent-repl-wire-decode-roster-row-detail)
           :closed (agent-repl-wire--decode-message
                    "RosterRow" 'closed object
                    #'agent-repl-wire-decode-roster-row-closed)))))

;;;; ---- Sections ----

(defun agent-repl-wire-decode-roster-rows-rows (value)
  "Decode one element of `RosterRows''s repeated `rows' field VALUE."
  (agent-repl-wire-decode-roster-row value))

(defun agent-repl-wire-decode-roster-rows (value)
  "Decode VALUE as `RosterRows', a plist `(:rows LIST)' in render order."
  (let ((object (agent-repl-wire--object "RosterRows" value)))
    (agent-repl-wire--check-keys "RosterRows" object '(rows))
    (agent-repl-wire--decoded
     "RosterRows"
     (list :rows (agent-repl-wire--decode-repeated
                  "RosterRows" 'rows object
                  #'agent-repl-wire-decode-roster-rows-rows)))))

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

(defun agent-repl-wire-decode-roster-task-section-header-label (value)
  "Decode `RosterTaskSectionHeader''s `label' field VALUE."
  (agent-repl-wire-decode-roster-label value))

(defun agent-repl-wire-decode-roster-task-section-header-done (value)
  "Decode `RosterTaskSectionHeader''s `done' field VALUE."
  (agent-repl-wire-decode-roster-task-done value))

(defun agent-repl-wire-decode-roster-task-section-header (value)
  "Decode VALUE as `RosterTaskSectionHeader', a plist `(:label :done)'.
Its own message because the done axis exists ONLY for tasks."
  (let ((object (agent-repl-wire--object "RosterTaskSectionHeader" value)))
    (agent-repl-wire--check-keys "RosterTaskSectionHeader" object '(label done))
    (agent-repl-wire--decoded
     "RosterTaskSectionHeader"
     (list :label (agent-repl-wire--decode-message
                   "RosterTaskSectionHeader" 'label object
                   #'agent-repl-wire-decode-roster-task-section-header-label)
           :done (agent-repl-wire--decode-message
                  "RosterTaskSectionHeader" 'done object
                  #'agent-repl-wire-decode-roster-task-section-header-done)))))

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

(defun agent-repl-wire-decode-roster-task-key (value)
  "Decode VALUE as `RosterTaskKey', a plist `(:task-id)'.
The ID, NOT the title: a task can be renamed without becoming another."
  (let ((object (agent-repl-wire--object "RosterTaskKey" value)))
    (agent-repl-wire--check-keys "RosterTaskKey" object '(taskId))
    (agent-repl-wire--decoded
     "RosterTaskKey"
     (list :task-id (agent-repl-wire--decode-string
                     "RosterTaskKey" 'taskId object)))))

(defun agent-repl-wire-decode-roster-repo-section-key (value)
  "Decode `RosterRepoSection''s `key' field VALUE as a RosterRepoKey."
  (agent-repl-wire-decode-roster-repo-key value))

(defun agent-repl-wire-decode-roster-repo-section-header (value)
  "Decode `RosterRepoSection''s `header' field VALUE."
  (agent-repl-wire-decode-roster-section-header value))

(defun agent-repl-wire-decode-roster-repo-section-rows (value)
  "Decode `RosterRepoSection''s `rows' field VALUE as RosterRows."
  (agent-repl-wire-decode-roster-rows value))

(defun agent-repl-wire-decode-roster-repo-section-expanded (value)
  "Decode VALUE as the empty message `RosterRepoSectionExpanded'."
  (agent-repl-wire--decode-empty "RosterRepoSectionExpanded" value))

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

(defun agent-repl-wire-decode-roster-repo-section (value)
  "Decode VALUE as `RosterRepoSection', a plist `(:key :header :rows :fold)'."
  (let ((object (agent-repl-wire--object "RosterRepoSection" value)))
    (agent-repl-wire--check-keys "RosterRepoSection" object
                                 '(key header rows expanded collapsed))
    (agent-repl-wire--decoded
     "RosterRepoSection"
     (list :key (agent-repl-wire--decode-message
                 "RosterRepoSection" 'key object
                 #'agent-repl-wire-decode-roster-repo-section-key)
           :header (agent-repl-wire--decode-message
                    "RosterRepoSection" 'header object
                    #'agent-repl-wire-decode-roster-repo-section-header)
           :rows (agent-repl-wire--decode-message
                  "RosterRepoSection" 'rows object
                  #'agent-repl-wire-decode-roster-repo-section-rows)
           :fold (agent-repl-wire-decode-roster-repo-section-fold object)))))

(defun agent-repl-wire-decode-roster-task-section-key (value)
  "Decode `RosterTaskSection''s `key' field VALUE as a RosterTaskKey."
  (agent-repl-wire-decode-roster-task-key value))

(defun agent-repl-wire-decode-roster-task-section-rows (value)
  "Decode `RosterTaskSection''s `rows' field VALUE as RosterRows."
  (agent-repl-wire-decode-roster-rows value))

(defun agent-repl-wire-decode-roster-task-section (value)
  "Decode VALUE as `RosterTaskSection', a plist `(:key :header :rows)'."
  (let ((object (agent-repl-wire--object "RosterTaskSection" value)))
    (agent-repl-wire--check-keys "RosterTaskSection" object '(key header rows))
    (agent-repl-wire--decoded
     "RosterTaskSection"
     (list :key (agent-repl-wire--decode-message
                 "RosterTaskSection" 'key object
                 #'agent-repl-wire-decode-roster-task-section-key)
           :header (agent-repl-wire--decode-message
                    "RosterTaskSection" 'header object
                    #'agent-repl-wire-decode-roster-task-section-header)
           :rows (agent-repl-wire--decode-message
                  "RosterTaskSection" 'rows object
                  #'agent-repl-wire-decode-roster-task-section-rows)))))

(defun agent-repl-wire-decode-roster-merged-section-header (value)
  "Decode `RosterMergedSection''s `header' field VALUE."
  (agent-repl-wire-decode-roster-section-header value))

(defun agent-repl-wire-decode-roster-merged-section-rows (value)
  "Decode `RosterMergedSection''s `rows' field VALUE as RosterRows."
  (agent-repl-wire-decode-roster-rows value))

(defun agent-repl-wire-decode-roster-merged-section (value)
  "Decode VALUE as `RosterMergedSection', a plist `(:header :rows)'.
It has no key of its own — the roster carries exactly one."
  (let ((object (agent-repl-wire--object "RosterMergedSection" value)))
    (agent-repl-wire--check-keys "RosterMergedSection" object '(header rows))
    (agent-repl-wire--decoded
     "RosterMergedSection"
     (list :header (agent-repl-wire--decode-message
                    "RosterMergedSection" 'header object
                    #'agent-repl-wire-decode-roster-merged-section-header)
           :rows (agent-repl-wire--decode-message
                  "RosterMergedSection" 'rows object
                  #'agent-repl-wire-decode-roster-merged-section-rows)))))

;;;; ---- The two views and the roster ----

(defun agent-repl-wire-decode-roster-repository-view-sections (value)
  "Decode one element of `RosterRepositoryView''s repeated `sections'."
  (agent-repl-wire-decode-roster-repo-section value))

(defun agent-repl-wire-decode-roster-repository-view (value)
  "Decode VALUE as `RosterRepositoryView', a plist `(:sections LIST)'.
Order is the resolver's; clients do not re-sort."
  (let ((object (agent-repl-wire--object "RosterRepositoryView" value)))
    (agent-repl-wire--check-keys "RosterRepositoryView" object '(sections))
    (agent-repl-wire--decoded
     "RosterRepositoryView"
     (list :sections (agent-repl-wire--decode-repeated
                      "RosterRepositoryView" 'sections object
                      #'agent-repl-wire-decode-roster-repository-view-sections)))))

(defun agent-repl-wire-decode-roster-task-view-sections (value)
  "Decode one element of `RosterTaskView''s repeated `sections'."
  (agent-repl-wire-decode-roster-task-section value))

(defun agent-repl-wire-decode-roster-task-view (value)
  "Decode VALUE as `RosterTaskView', a plist `(:sections LIST)'."
  (let ((object (agent-repl-wire--object "RosterTaskView" value)))
    (agent-repl-wire--check-keys "RosterTaskView" object '(sections))
    (agent-repl-wire--decoded
     "RosterTaskView"
     (list :sections (agent-repl-wire--decode-repeated
                      "RosterTaskView" 'sections object
                      #'agent-repl-wire-decode-roster-task-view-sections)))))

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

(defun agent-repl-wire-decode-workspace-roster-repository (value)
  "Decode `WorkspaceRoster''s `repository' field VALUE."
  (agent-repl-wire-decode-roster-repository-view value))

(defun agent-repl-wire-decode-workspace-roster-task (value)
  "Decode `WorkspaceRoster''s `task' field VALUE."
  (agent-repl-wire-decode-roster-task-view value))

(defun agent-repl-wire-decode-workspace-roster-recently-merged (value)
  "Decode `WorkspaceRoster''s `recently_merged' field VALUE."
  (agent-repl-wire-decode-roster-merged-section value))

(defun agent-repl-wire-decode-workspace-roster-current (value)
  "Decode `WorkspaceRoster''s optional `current' field VALUE."
  (agent-repl-wire-decode-roster-current-workspace value))

(defun agent-repl-wire-decode-workspace-roster (value)
  "Decode VALUE as `WorkspaceRoster'.
Returns `(:repository V :task V :recently-merged S :current C-or-nil)'.
BOTH groupings arrive fully resolved: which one is drawn is client-local
preference, a selection between two resolved views, never a derivation."
  (let ((object (agent-repl-wire--object "WorkspaceRoster" value)))
    (agent-repl-wire--check-keys
     "WorkspaceRoster" object '(repository task recentlyMerged current))
    (agent-repl-wire--decoded
     "WorkspaceRoster"
     (list :repository (agent-repl-wire--decode-message
                        "WorkspaceRoster" 'repository object
                        #'agent-repl-wire-decode-workspace-roster-repository)
           :task (agent-repl-wire--decode-message
                  "WorkspaceRoster" 'task object
                  #'agent-repl-wire-decode-workspace-roster-task)
           :recently-merged (agent-repl-wire--decode-message
                             "WorkspaceRoster" 'recentlyMerged object
                             #'agent-repl-wire-decode-workspace-roster-recently-merged)
           :current (agent-repl-wire--decode-optional-message
                     "WorkspaceRoster" 'current object
                     #'agent-repl-wire-decode-workspace-roster-current)))))

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
