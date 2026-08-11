;;; frontend-state.el --- Apply daemon-pushed frontend.v1 state -*- lexical-binding: t; -*-

;;; Commentary:

;; The interpretation half of the Emacs dumb renderer (design §5.4, §10).
;; `frontend-uds.el' owns the transport and hands decoded `frontend.v1'
;; frames to handlers registered by oneof field name; this file supplies
;; the three state-bearing handlers and registers them:
;;
;;   - `workspaceState'  -> map the pushed RenderState enum to the existing
;;                          render-state keyword vocabulary and store it as
;;                          the pushed-state source of truth (via workspace.el
;;                          wrappers) that the renderer reads at stitch.
;;   - `snapshot'        -> full resync: apply every WorkspaceState in the
;;                          StateSnapshot on (re)connect.
;;   - `sessionView'     -> the session store, plus the FIRST reader
;;                          `SessionView.death' ever had (F4).
;;   - `shutdownSchedule'-> record the daemon-global drain lease.  Rendered by
;;                          nothing here; kept because the cancel command
;;                          Emacs sends needs the live schedule id.
;;
;; The daemon is the SINGLE SOURCE OF TRUTH for render-state (SSM-resolved);
;; Emacs never re-derives it.  Per §10 the local precedence `cond' in
;; `agent-repl--ws-render-status' is replaced by a lookup of the pushed
;; keyword this file stores under the `:pushed-render-state' plist key.
;;
;; No-Silent-Fallbacks (AGENTS.md): an unmappable RenderState (the proto
;; zero value RENDER_STATE_UNSPECIFIED, or any value absent from the closed
;; map), a WorkspaceState missing its `workspace', or a DegradedNotice
;; missing its `component' all fail loudly (log + `error') — never a
;; defaulted value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; `agent-repl--latch-and-maybe-fire-loaded' lives in sentinel.el; both load
;; as part of the same module, so the symbol is resolved at call time.
(declare-function agent-repl--latch-and-maybe-fire-loaded "sentinel" (ws key &optional marker))
(declare-function agent-repl--ws-dir-owner "workspace" (dir &optional except))
(declare-function agent-repl--ws-registered-dir-owner "workspace" (dir &optional except))
(declare-function agent-repl--ws-tombstoned-p "workspace" (ws))
(declare-function agent-repl--ws-known-p "workspace" (ws))
(declare-function agent-repl--workspace-create-handle-available
                  "workspace-create-client" (available))
(declare-function agent-repl--workspace-create-handle-host-action
                  "workspace-create-client" (action))
;; `agent-repl--frontend-note-boot-id' lives in frontend-client.el (it owns the
;; reattach give-up state the boot-id change resets); resolved at call time.
(declare-function agent-repl--frontend-note-boot-id "frontend-client" (boot-id))
(declare-function agent-repl--uds-run-snapshot-applied-hook "frontend-uds" ())
;; The merge-failed resurrection (below) reuses snapshot-load's promotion
;; primitives; both live outside this module and resolve at call time.
(declare-function agent-repl--establish-workspace "commands" (ws dir))
(declare-function agent-repl--reorder-workspace-to-front "workspace" (ws))
(declare-function agent-repl--ws-open-p "workspace" (ws))

;;;; ---- State-transition hook -------------------------------------------

(defvar agent-repl-ws-state-transition-functions nil
  "Abnormal hook run each time a daemon-pushed `WorkspaceState' is applied.
Each function is called with three arguments: (WORKSPACE NEW-KEYWORD
PREVIOUS-KEYWORD), where NEW-KEYWORD is the render keyword just stored
under `:pushed-render-state' and PREVIOUS-KEYWORD is what it replaced
\(nil on the first push for a workspace).

This is the single subscription point the agent-shim cutover (design §4.6,
§9.3) re-anchors the merge reactive consequences onto: the daemon now owns
merge STATE (it publishes `:merging'/`:merge-conflict'/`:merged'/… over the
frontend surface), so the magit conflict popup, merged-teardown, and
close-after-merge — all previously keyed off LOCAL `:merge-*' plist flips in
worktree.el — key off THIS hook instead
\(see `agent-repl--merge-react-to-pushed-state' in worktree.el).

Handlers run via `run-hook-wrapped' inside `condition-case', so a broken
subscriber cannot prevent state application or later subscribers from
running — the error is loud-logged, never swallowed silently.")

(defun agent-repl--frontend-run-state-transition-hook (workspace new previous)
  "Run `agent-repl-ws-state-transition-functions' for WORKSPACE.
NEW and PREVIOUS are render keywords (PREVIOUS nil on first push).  Each
handler is wrapped so a signal is caught + logged rather than aborting
state application (the log keeps the No-Silent-Fallbacks contract: the
failure is surfaced, not hidden)."
  (run-hook-wrapped 'agent-repl-ws-state-transition-functions
                    (lambda (fn ws n p)
                      (condition-case err
                          (funcall fn ws n p)
                        (error
                         (agent-repl--log ws
                                          "ws-state-transition-hook fn=%s err=%S"
                                          fn err)))
                      nil)
                    workspace new previous))

;;;; ---- RenderState → keyword mapping -----------------------------------

(defconst agent-repl--frontend-render-state-map
  '(("RENDER_STATE_INIT"           . :init)
    ("RENDER_STATE_IDLE"           . :idle)
    ("RENDER_STATE_IDLE_ASYNC"     . :idle-async)
    ;; RED like :thinking — a turn is in flight and a prompt cannot land yet.
    ;; The phase word is the whole distinction: the daemon has committed to the
    ;; submit and the shim has not acked it, so the agent is not holding
    ;; anything yet.
    ("RENDER_STATE_SUBMITTING"     . :submitting)
    ("RENDER_STATE_THINKING"       . :thinking)
    ("RENDER_STATE_PERMISSION"     . :permission)
    ("RENDER_STATE_DONE"           . :done)
    ("RENDER_STATE_READY"          . :ready)
    ;; GREEN like :done — an interrupted turn is a CONCLUDED turn: the user
    ;; asked for the stop, got it, and can prompt again immediately.  The
    ;; word carries the distinction; the color claim is the same.
    ("RENDER_STATE_INTERRUPTED"    . :interrupted)
    ;; RED like :thinking — the agent is busy and a prompt cannot land yet.
    ;; The two context cuts differ from thinking only in WHAT the agent is
    ;; busy with, which the word carries; the color claim is the same.
    ;; THE CLOSED HALF OF THE legacy connectivity projection IS TWO STATES, and it used to be one.
    ;; A single RENDER_STATE_DORMANT meant both "we put this session to sleep on
    ;; purpose to reclaim its ~500MB" and "the backend substrate is broken", so
    ;; the most ordinary event in the system painted a tab exactly like a dead
    ;; shim did.  A color that fires on both means neither.
    ;;
    ;; BLUE like :init, and the opposite claim: :init says a bring-up is in
    ;; flight, :severed says nothing is wired, nothing is coming, and something
    ;; on our side broke — a bring-up that failed, or a session controller that died on a
    ;; terminal protocol error.
    ("RENDER_STATE_SEVERED"        . :severed)
    ;; TEAL, and pointedly not blue.  Nothing is wired here and nothing is
    ;; wrong: the shim was SIGTERMed on purpose, or nothing was ever wired to
    ;; this workspace at all.  Its PRECEDENCE is still the blue band's, because
    ;; the actionability claim is identical — you cannot interact without paying
    ;; a bring-up — and only the reason is benign.
    ("RENDER_STATE_HIBERNATED"     . :hibernated)
    ("RENDER_STATE_CLEARING"       . :clearing)
    ("RENDER_STATE_COMPACTING"     . :compacting)
    ("RENDER_STATE_VENDOR_BLOCKED" . :vendor-blocked)
    ;; DEPRECATED upstream and no longer resolved by the SSM, but still
    ;; mapped: an old daemon binary, or a state log written before the
    ;; remap, can still push it, and erroring on a state we know how to
    ;; render would be worse than rendering it.  It resolves to the same
    ;; purple `:vendor-blocked' means, because that is what it always was —
    ;; a turn that ended on something only a human or the vendor can fix.
    ("RENDER_STATE_STOP_FAILED"    . :vendor-blocked)
    ;; The merge pipeline's FIRST mark, emitted by the daemon's command
    ;; handler the instant a merge command arrives and before anything
    ;; durable exists for it.  Transient by construction: it is superseded
    ;; by `:merge-queued' or `:merging' within milliseconds, or by
    ;; `:merge-failed' when the enqueue is refused.
    ("RENDER_STATE_MERGE_ENQUEUING" . :merge-enqueuing)
    ("RENDER_STATE_MERGING"        . :merging)
    ("RENDER_STATE_MERGE_QUEUED"   . :merge-queued)
    ("RENDER_STATE_MERGE_CONFLICT" . :merge-conflict)
    ("RENDER_STATE_MERGE_FAILED"   . :merge-failed)
    ("RENDER_STATE_MERGED"         . :merged)
    ("RENDER_STATE_DEAD"           . :dead)
    ("RENDER_STATE_DEGRADED"       . :degraded))
  "Map every `RenderState' enum NAME (protojson string) to a render keyword.
The keyword half is the existing closed render-state vocabulary that
renderers look up in `agent-repl-ws-state-icons' (see
`agent-repl--ws-render-status').  Deliberately omits the proto zero
value RENDER_STATE_UNSPECIFIED: receiving it means the daemon pushed an
unresolved state, which is an invariant violation, not a state to
render — `agent-repl--frontend-state->keyword' errors on it.

`:degraded' carries the 📡 glyph in `agent-repl-ws-state-icons'
\(workspace.el); `test-render-colors.el' asserts it exists.")

(defun agent-repl--frontend-state->keyword (state &optional workspace)
  "Map RenderState enum NAME STATE (a string) to a render keyword.
Signals `error' (after a loud log) for RENDER_STATE_UNSPECIFIED, a nil
STATE (protojson omits a default-valued enum, i.e. UNSPECIFIED), or any
value outside `agent-repl--frontend-render-state-map' — there is no
fallback keyword (AGENTS.md No-Silent-Fallbacks).  WORKSPACE, when known,
threads per-workspace log metadata through a state-frame application."
  (let ((kw (and (stringp state)
                 (cdr (assoc state agent-repl--frontend-render-state-map)))))
    (unless kw
      (agent-repl--log workspace
                       "frontend-state->keyword: UNMAPPABLE RenderState=%S (UNSPECIFIED/unknown) — no fallback"
                       state)
      (error "agent-repl frontend: unmappable RenderState %S" state))
    ;; WorkspaceState frames can arrive on each state transition, so retain
    ;; the exact accepted mapping only in verbose traces.
    (agent-repl--log-verbose workspace
                             "frontend-state->keyword: state=%s keyword=%s"
                             state kw)
    kw))

(defconst agent-repl--frontend-connectivity-map
  '(("SESSION_CONNECTIVITY_HIBERNATED" . :hibernated)
    ("SESSION_CONNECTIVITY_CONNECTING" . :connecting)
    ("SESSION_CONNECTIVITY_OPERATIONAL" . :operational)
    ("SESSION_CONNECTIVITY_DEGRADED" . :degraded)
    ("SESSION_CONNECTIVITY_UNAVAILABLE" . :unavailable))
  "Closed daemon-pushed session-connectivity vocabulary.")

(defconst agent-repl--frontend-session-status-map
  '(("SESSION_STATUS_READY" . :ready)
    ("SESSION_STATUS_SUBMITTING" . :submitting)
    ("SESSION_STATUS_THINKING" . :thinking)
    ("SESSION_STATUS_PERMISSION" . :permission)
    ("SESSION_STATUS_DONE" . :done)
    ("SESSION_STATUS_INTERRUPTED" . :interrupted)
    ("SESSION_STATUS_VENDOR_BLOCKED" . :vendor-blocked)
    ("SESSION_STATUS_MONITORING" . :monitoring))
  "Closed daemon-pushed session-status vocabulary.")

(defun agent-repl--frontend-enum->keyword
    (value mapping field workspace &optional allow-unspecified)
  "Map enum VALUE through MAPPING for FIELD and WORKSPACE.
ALLOW-UNSPECIFIED returns nil only for a nil or explicitly unspecified
session-status value.  Every other missing or unknown enum fails loudly."
  (let ((keyword (and (stringp value) (cdr (assoc value mapping)))))
    (cond
     (keyword
      (agent-repl--log-verbose
       workspace "frontend-enum->keyword: field=%s value=%s keyword=%s"
       field value keyword)
      keyword)
     ((and allow-unspecified
           (or (null value)
               (equal value "SESSION_STATUS_UNSPECIFIED")))
      nil)
     (t
      (agent-repl--log
       workspace
       "frontend-enum->keyword: UNMAPPABLE field=%s value=%S allow-unspecified=%S"
       field value allow-unspecified)
      (error "agent-repl frontend: unmappable %s %S" field value)))))

(defun agent-repl--frontend-validate-runtime-faults (workspace faults)
  "Validate daemon-pushed runtime FAULTS for WORKSPACE and return them."
  (dolist (fault faults)
    (let ((component (plist-get fault :component))
          (fault-type (plist-get fault :faultType))
          (impact (plist-get fault :impact)))
      (when (or (not (stringp component)) (string-empty-p component)
                (not (stringp fault-type)) (string-empty-p fault-type)
                (not (member impact
                             '("connectivity" "feature" "command"
                               "turn-terminal"))))
        (agent-repl--log
         workspace
         "frontend-runtime-fault: INVALID component=%S fault-type=%S impact=%S fault=%S"
         component fault-type impact fault)
        (error "agent-repl frontend: invalid RuntimeFault %S" fault))))
  faults)

;;;; ---- WorkspaceState application --------------------------------------

(defvar agent-repl--frontend-workspace-state-views
  (make-hash-table :test 'equal)
  "Latest raw `WorkspaceState' plist keyed by its daemon workspace path.
Unlike render-state application, this store retains states for workspaces
that Emacs has not restored yet.  Startup shutdown safety reads it so an
in-flight turn from the previous Emacs cannot be killed merely because its
perspective has not been recreated.")

(defun agent-repl--frontend-workspace-state-views-all ()
  "Return every latest raw `WorkspaceState' plist pushed by the daemon."
  (hash-table-values agent-repl--frontend-workspace-state-views))

;;;; ---- The inbound workspace key ---------------------------------------
;;
;; Every daemon frame names its workspace by the session CWD, because that is
;; what the daemon keys workspaces by (SessionLocator, the SSM, sessioncontroller).
;; Emacs keys workspaces by their persp NAME ("doom").
;;
;; Feeding the daemon's path straight to `agent-repl--ws-put' does not fail —
;; it STUB-CREATES a hash entry under the path.  The pushed render state then
;; lands on that stub while the tab-bar keeps reading the real, name-keyed
;; workspace, which never changes: every workspace sat at its disconnected
;; colour while the GUI showed the session working normally.  The stubs are
;; visible in the log as `sidebar-entries: skipping live ws=/Users/... with no
;; :project-dir'.
;;
;; So every inbound handler resolves the path to the workspace NAME first.  A
;; session-scoped live frame whose path resolves to nothing violates the
;; creation handshake: WorkspaceAvailable must materialize the host workspace
;; before the daemon publishes session state.  This resolver remains LIVE-only
;; because permission and UDS callers must never target a tombstoned workspace.
;; State handlers classify tombstoned owners separately for retain/drop logic.

(defun agent-repl--frontend-ws-name (workspace)
  "Return the Emacs workspace NAME the daemon's WORKSPACE string refers to.
WORKSPACE is a session CWD (how the daemon names workspaces).  Resolves it
against the live workspaces' `:project-dir'.  A WORKSPACE that is already a
known workspace name is returned as-is, so a frame that carries a name
still works.  Returns nil when nothing owns it — the caller must then drop
or reject the frame rather than key state under an unresolvable id."
  (when (and workspace (not (string-empty-p workspace)))
    (let* ((owner (agent-repl--ws-dir-owner workspace))
           (known (and (not owner)
                       (agent-repl--ws-known-p workspace)))
           (resolved (or owner (and known workspace))))
      ;; This resolver runs for every WorkspaceState frame.  A verbose record
      ;; preserves the decisive path-vs-name outcome without flooding normal
      ;; lifecycle logs.
      (agent-repl--log-verbose
       resolved
       "frontend-ws-name: wire-workspace=%s resolution=%s result=%s"
       workspace (cond (owner :dir-owner) (known :known-name) (t :unowned))
       resolved)
      resolved)))

(defvar agent-repl--frontend-applying-snapshot-state nil
  "Non-nil only while snapshot replay applies `WorkspaceState' records.
An unregistered state in a reconnect snapshot is retained for restart safety.
An unregistered live push is an impossible pre-WorkspaceAvailable frame and
must reject loudly.")

(defun agent-repl--frontend-reject-unmaterialized-session-frame
    (frame job-id path identity)
  "Reject a session FRAME that arrived before host materialization.
JOB-ID is the creation job identity when the frame protocol carries one;
the current session-state frames do not, so callers must pass the explicit
wire-contract marker `unavailable'.  PATH and IDENTITY identify the
impossible frame in the canonical global log.  This function never mutates
workspace or frontend state.

IDENTITY is whatever the arm names itself by: a HOST frame passes its
`session_id', a FENCED push its opaque `fence' (`session_id' is reserved on
every one of those).  It is a diagnostic here — logged, never parsed."
  (agent-repl--log
   nil
   "frontend-session-frame: REJECTED pre-materialization frame=%s job-id=%s path=%S identity=%S"
   frame job-id path identity)
  (user-error
   "agent-repl frontend: %s arrived before WorkspaceAvailable materialized path %s (job %s identity %s)"
   frame path job-id identity))

(defun agent-repl--frontend-tombstoned-dir-owner (path)
  "Return PATH's tombstoned workspace owner, or nil for live and unknown paths."
  (let ((owner (agent-repl--ws-registered-dir-owner path)))
    (and owner (agent-repl--ws-tombstoned-p owner) owner)))

(defun agent-repl--frontend-int64 (raw)
  "Return protojson int64 field RAW as an Emacs number, or nil.
protojson encodes int64/uint64 as a JSON STRING, so the wire value for a
numeric field arrives as either a string of digits (the daemon's own
encoder) or a number (a hand-built frame in a test).  Anything else — an
absent field, a non-numeric string — answers nil, so the caller decides
what an unusable value means rather than inheriting a guessed 0."
  (cond ((numberp raw) raw)
        ((and (stringp raw) (string-match-p "\\`-?[0-9]+\\'" raw))
         (string-to-number raw))))

(defun agent-repl--frontend-retain-merged-at (workspace ws-state)
  "Retain WS-STATE's `mergedAtMs' as WORKSPACE's `:merge-completed-at'.
Returns the retained epoch-seconds float, or nil when the frame carries no
merge instant.

The daemon persists the instant a workspace's merge landed
\(`ssm/merged.go') and rides it on EVERY WorkspaceState from then on, so
this is the durable fact the sidebar's Recently Merged section orders on
\(`agent-repl--sidebar-merged-at').  It is stored in epoch SECONDS to match
every other Emacs-side time value; the wire field is millis.

Retention is MONOTONE — a positive instant is adopted, and a zero or
absent one leaves an already-known instant alone.  protojson omits a
zero-valued int64, so \"not merged\" and \"field absent\" are the same wire
shape, and clearing on it would erase a merge Emacs restored from its own
session state.  A merge never un-happens, so there is no honest reading of
that shape that should destroy the fact.

A DIFFERENT positive instant is adopted with a loud log: the daemon holds
the first landing as the fact every frontend orders on, so a disagreement
is the daemon correcting Emacs, and one that goes unrecorded would leave
the section's ordering unexplainable."
  (let ((ms (agent-repl--frontend-int64 (plist-get ws-state :mergedAtMs)))
        (known (agent-repl--ws-get workspace :merge-completed-at)))
    (cond
     ((or (null ms) (<= ms 0))
      (agent-repl--log-verbose workspace
                               "frontend-retain-merged-at: ws=%s no merge instant on frame (raw=%S) — keeping known=%S"
                               workspace (plist-get ws-state :mergedAtMs) known)
      nil)
     (t
      (let ((at (/ ms 1000.0)))
        (if (and known (/= (float-time known) at))
            (agent-repl--log workspace
                             "frontend-retain-merged-at: ws=%s daemon merged_at_ms=%d (%.3f) SUPERSEDES known=%.3f — the daemon's first landing is the ordering fact"
                             workspace ms at (float-time known))
          (agent-repl--log-verbose workspace
                                   "frontend-retain-merged-at: ws=%s merged_at_ms=%d -> :merge-completed-at=%.3f"
                                   workspace ms at))
        (agent-repl--ws-put workspace :merge-completed-at at)
        at)))))

;;;; ---- MergeStatus (the merge pipeline's own structured report) --------
;;
;; `WorkspaceState.merge_status' carries what the flat `merge_phase' string
;; never could: which merge run this is, how far into the cherry-pick it
;; got, which commit is on the table right now, and why a merge ended the
;; way it did.  The daemon owns the whole pipeline (it runs the
;; before/after actions too); Emacs only renders what lands here.
;;
;; EXACTLY ONE oneof arm is set per push, and WHICH arm it is IS the phase.
;; Deriving the phase keyword from the arm — rather than reading a second,
;; parallel phase field — means the two can never disagree, because there
;; is only one of them.

(defconst agent-repl--frontend-merge-status-arms
  '((:enqueued      . :enqueued)
    (:beforeAction  . :before-action)
    (:cherryPicking . :cherry-picking)
    (:testing       . :testing)
    (:conflict      . :conflict)
    (:afterAction   . :after-action)
    (:merged        . :merged)
    (:failed        . :failed))
  "Map each `MergeStatus' oneof arm (its protojson key) to a phase keyword.
Closed by construction: the arm set IS the phase vocabulary.  A status
carrying no arm, or more than one, is a malformed frame rather than a
phase to render — `agent-repl--frontend-merge-status-arm' errors on it
\(AGENTS.md No-Silent-Fallbacks).")

(defconst agent-repl--frontend-merge-status-fields
  '((:position          . :position)
    (:depth             . :depth)
    (:prompt            . :prompt)
    (:commitsTotal      . :commits-total)
    (:commitsLanded     . :commits-landed)
    (:currentSha        . :current-sha)
    (:currentSubject    . :current-subject)
    (:conflictedSha     . :conflicted-sha)
    (:conflictedSubject . :conflicted-subject)
    (:failingSha        . :failing-sha)
    (:failingSubject    . :failing-subject)
    (:cause             . :cause)
    (:failedJson        . :failed-json)
    (:afterActionError  . :after-action-error))
  "Every field a `MergeStatus' oneof arm can carry: wire key to plist key.
The union across all arms, deliberately flat: a caller asks for
`:commits-landed' without first knowing which arm produced it.  A field
outside this table is a wire addition Emacs has not been taught, which
fails loudly for the same reason an unknown frame arm does.

`:failedJson' is the `failed' arm serialized as JSON BY THE DAEMON, using
proto3's own JSON mapping — it is kept verbatim, and Emacs never
assembles or re-serializes a record of its own from the sibling fields
beside it.  `merge-handlers.el' reports it as a field of the merge error,
which is how a failure that fits on one echo line still hands the reader
every field the arm carried.")

(defconst agent-repl--frontend-merge-status-numeric-fields
  '(:position :depth :commits-total :commits-landed)
  "The `MergeStatus' arm fields read as NUMBERS rather than kept verbatim.
Routed through `agent-repl--frontend-int64' because protojson encodes a
64-bit field as a JSON string while a 32-bit one arrives as a number, and
a renderer that formatted the raw value would print counts two ways.")

(defun agent-repl--frontend-merge-status-arm (status workspace)
  "Return (PHASE . ARM-PLIST) for STATUS's single present oneof arm.
STATUS is a decoded `MergeStatus' plist and WORKSPACE threads log
metadata.  Signals (after a loud log) when the count of present arms is
not exactly one: a merge with no phase, or with two, is not a state to
render and there is no arm to prefer."
  (let ((present (cl-remove-if-not
                  (lambda (cell) (plist-member status (car cell)))
                  agent-repl--frontend-merge-status-arms)))
    (unless (= 1 (length present))
      (agent-repl--log workspace
                       "frontend-merge-status-arm: EXPECTED exactly one oneof arm, got %d (%S) in %S — no fallback"
                       (length present) (mapcar #'car present) status)
      (error "agent-repl frontend: MergeStatus carries %d oneof arms"
             (length present)))
    (cons (cdr (car present)) (plist-get status (car (car present))))))

(defun agent-repl--frontend-parse-merge-status (status workspace)
  "Decode `MergeStatus' plist STATUS into Emacs's merge-status plist.
Returns nil when STATUS is absent — the daemon rides the submessage only
while it has a merge to report, and inventing an idle phase for its
absence would narrate a merge nobody asked for.

The result is `(:phase KW :run-id S :phase-started-at-ms N
:updated-at-ms N ...)' plus the present arm's own fields, renamed through
`agent-repl--frontend-merge-status-fields' and, for the counts, parsed
through `agent-repl--frontend-int64'.  An arm field outside that table
fails loudly."
  (when status
    (let* ((arm (agent-repl--frontend-merge-status-arm status workspace))
           (phase (car arm))
           (body (cdr arm))
           (parsed (list :phase phase
                         :run-id (plist-get status :runId)
                         :phase-started-at-ms
                         (agent-repl--frontend-int64
                          (plist-get status :phaseStartedAtMs))
                         :updated-at-ms
                         (agent-repl--frontend-int64
                          (plist-get status :updatedAtMs)))))
      (cl-loop for (wire value) on body by #'cddr
               do (let ((key (cdr (assq wire
                                        agent-repl--frontend-merge-status-fields))))
                    (unless key
                      (agent-repl--log workspace
                                       "frontend-parse-merge-status: UNKNOWN field=%S in phase=%s arm=%S — no fallback"
                                       wire phase body)
                      (error "agent-repl frontend: unknown MergeStatus field %S" wire))
                    (setq parsed
                          (plist-put parsed key
                                     (if (memq key agent-repl--frontend-merge-status-numeric-fields)
                                         (agent-repl--frontend-int64 value)
                                       value)))))
      ;; A merge pushes a status on every tick of its cherry-pick, so the
      ;; decisive per-frame record stays verbose; the narration in
      ;; merge-handlers.el logs the transitions at normal volume.
      (agent-repl--log-verbose workspace
                               "frontend-parse-merge-status: phase=%s run-id=%s parsed=%S"
                               phase (plist-get status :runId) parsed)
      parsed)))

(defconst agent-repl--frontend-merge-dequeue-standings
  '((:waiting . :waiting)
    (:running . :running))
  "Map each `MergeDequeueOffer' oneof arm (its protojson key) to a keyword.
Closed by construction, exactly as
`agent-repl--frontend-merge-status-arms' is: the arm set IS the standing
vocabulary, so an offer carrying no arm or more than one is a malformed
frame rather than a standing to narrate.")

(defun agent-repl--frontend-parse-merge-dequeue-offer (offer workspace)
  "Decode `MergeDequeueOffer' plist OFFER into Emacs's offer plist.
Returns nil when OFFER is absent — the daemon rides the submessage only
while a question stands, and inventing one for its absence would narrate
a card that is not on screen.

The result is `(:offer-id S :run-id S :standing KW :ahead N :position N
:depth N)', with the queue figures present only on the `:waiting'
standing.  WORKSPACE threads log metadata.

EMACS NARRATES THE QUESTION, IT DOES NOT ASK IT.  The card and its two
buttons are the webapp's (design: the host draws no cards), so the
`:running' arm's nested `MergeStatus' is deliberately NOT decoded here —
Emacs already holds that run's status on `:pushed-merge-status', and a
second copy of it would be a second thing to keep in step."
  (when offer
    (let ((present (cl-remove-if-not
                    (lambda (cell) (plist-member offer (car cell)))
                    agent-repl--frontend-merge-dequeue-standings)))
      (unless (= 1 (length present))
        (agent-repl--log workspace
                         "frontend-parse-merge-dequeue-offer: EXPECTED exactly one standing arm, got %d (%S) — no fallback"
                         (length present) (mapcar #'car present))
        (error "agent-repl frontend: MergeDequeueOffer carries %d oneof arms"
               (length present)))
      (let* ((wire (car (car present)))
             (standing (cdr (car present)))
             (body (plist-get offer wire))
             (parsed (list :offer-id (plist-get offer :offerId)
                           :run-id (plist-get offer :runId)
                           :standing standing)))
        (when (eq standing :waiting)
          (setq parsed
                (append parsed
                        (list :ahead (agent-repl--frontend-int64
                                      (plist-get body :ahead))
                              :position (agent-repl--frontend-int64
                                         (plist-get body :position))
                              :depth (agent-repl--frontend-int64
                                      (plist-get body :depth))))))
        (agent-repl--log-verbose workspace
                                 "frontend-parse-merge-dequeue-offer: offer=%s standing=%s parsed=%S"
                                 (plist-get offer :offerId) standing parsed)
        parsed))))

(declare-function agent-repl--recovery-slo-note-emacs "agent-repl-recovery-slo" (ws))

(defun agent-repl--frontend-apply-workspace-state (ws-state)
  "Apply a `WorkspaceState' frame WS-STATE (a plist).
Handler for the `workspaceState' oneof arm.  Maps the pushed RenderState
to a render keyword and stores it under the `:pushed-render-state'
workspace key (via `agent-repl--ws-put') as the pushed-state source of
truth the renderer reads.  The resolution inputs (turn-active, live-task
count, merge phase, cause kind/seq) are stored under
`:pushed-render-state-meta' for debuggability and logged as an old->new
transition.  The frame's `mergedAtMs' is retained separately, on the
durable `:merge-completed-at' key
\(`agent-repl--frontend-retain-merged-at'), and its structured
`mergeStatus' lands on `:pushed-merge-status'
\(`agent-repl--frontend-parse-merge-status').  Its `mergeDequeueOffer' —
the question an interrupt raised over a queued merge — lands on
`:pushed-merge-dequeue-offer'
\(`agent-repl--frontend-parse-merge-dequeue-offer').  Returns the applied
keyword.

Fails loudly on a missing/blank `workspace' (invariant violation)."
  (let ((raw-workspace (plist-get ws-state :workspace))
        (state (plist-get ws-state :state)))
    (when (or (null raw-workspace) (string-empty-p raw-workspace))
      (agent-repl--log nil
                       "frontend-apply-workspace-state: MISSING workspace in %S — no fallback"
                       ws-state)
      (error "agent-repl frontend: WorkspaceState missing workspace"))
    (let* ((workspace (agent-repl--frontend-ws-name raw-workspace))
           (diagnostic-workspace (or workspace raw-workspace))
           (keyword
            (agent-repl--frontend-state->keyword
             state diagnostic-workspace))
           (connectivity
            (agent-repl--frontend-enum->keyword
             (plist-get ws-state :connectivity)
             agent-repl--frontend-connectivity-map
             "SessionConnectivity" diagnostic-workspace))
           (session-status
            (agent-repl--frontend-enum->keyword
             (plist-get ws-state :status)
             agent-repl--frontend-session-status-map
             "SessionStatus" diagnostic-workspace t))
           (session-id (plist-get ws-state :sessionId))
           (generation-id (plist-get ws-state :controllerGenerationId))
           (tombstoned-owner
            (and (not workspace)
                 (agent-repl--frontend-tombstoned-dir-owner raw-workspace)))
           (faults
            (agent-repl--frontend-validate-runtime-faults
             diagnostic-workspace (plist-get ws-state :activeFaults)))
           ;; Decoded with the other preconditions, before anything is
           ;; retained: a malformed MergeStatus must reject the whole frame
           ;; rather than land a render state whose merge half was dropped.
           (merge-status
            (agent-repl--frontend-parse-merge-status
             (plist-get ws-state :mergeStatus) diagnostic-workspace))
           ;; Decoded beside the status and for the same reason: a malformed
           ;; offer must reject the whole frame rather than land a render
           ;; state whose outstanding question was dropped.
           (dequeue-offer
            (agent-repl--frontend-parse-merge-dequeue-offer
             (plist-get ws-state :mergeDequeueOffer) diagnostic-workspace)))
      ;; Validate every precondition before retaining the raw frame or mutating
      ;; workspace state, so a malformed composite verdict cannot partially
      ;; land.
      (when (and (not (eq connectivity :hibernated))
                 (or (not (stringp session-id)) (string-empty-p session-id)
                     (not (stringp generation-id))
                     (string-empty-p generation-id)))
        (agent-repl--log
         diagnostic-workspace
         "frontend-apply-workspace-state: INCOMPLETE controller identity connectivity=%s session=%S generation=%S"
         connectivity session-id generation-id)
        (error "agent-repl frontend: incomplete session-controller identity"))
      (if (not workspace)
          ;; A merge failure is the explicit closed-workspace exception: its
          ;; established recovery path re-materializes the workspace before
          ;; recursively applying this frame.  Every other unowned frame is
          ;; an impossible pre-WorkspaceAvailable publication.
          (cond
           (tombstoned-owner
            (puthash raw-workspace ws-state agent-repl--frontend-workspace-state-views)
            (agent-repl--log-verbose
             nil
             "frontend-apply-workspace-state: retained tombstoned workspace=%s path=%S session-id=%S"
             tombstoned-owner raw-workspace session-id)
            nil)
           (agent-repl--frontend-applying-snapshot-state
            (puthash raw-workspace ws-state agent-repl--frontend-workspace-state-views)
            (agent-repl--log-verbose
             nil
             "frontend-apply-workspace-state: retained unowned snapshot workspace path=%S session-id=%S"
             raw-workspace session-id)
            nil)
           ((eq keyword :merge-failed)
            (agent-repl--frontend-resurrect-merge-failed raw-workspace ws-state))
           (t
            (agent-repl--frontend-reject-unmaterialized-session-frame
             "WorkspaceState" "unannounced" raw-workspace session-id)))
        ;; The workspace is materialized, so retaining its daemon view is safe
        ;; for reconnect processing.
        (puthash raw-workspace ws-state agent-repl--frontend-workspace-state-views)
        (progn
    (let* ((previous (agent-repl--ws-get workspace :pushed-render-state))
           ;; live_task_count / cause_seq / at_ms are proto int64/uint64,
           ;; which protojson encodes as JSON strings — stored verbatim.
           (turn-active (plist-get ws-state :turnActive))
           (live-tasks (plist-get ws-state :liveTaskCount))
           (cause-kind (plist-get ws-state :causeKind))
           (cause-seq (plist-get ws-state :causeSeq))
           ;; The merge instant is a DURABLE workspace fact rather than a
           ;; resolution input, so it lands on its own key: the render state
           ;; moves on after a merge (the daemon hibernates the session, and
           ;; the pushed state becomes `:hibernated'), and a section keyed on
           ;; the transient state loses the row the moment that happens.
           (merged-at (agent-repl--frontend-retain-merged-at workspace ws-state)))
      (agent-repl--ws-put workspace :pushed-render-state keyword)
      (agent-repl--ws-put workspace :pushed-session-connectivity connectivity)
      (agent-repl--ws-put workspace :pushed-session-status session-status)
      (agent-repl--ws-put workspace :pushed-render-state-meta
                          (list :turn-active turn-active
                                :live-task-count live-tasks
                                :cause-kind cause-kind
                                :cause-seq cause-seq
                                :at-ms (plist-get ws-state :atMs)
                                :session-id session-id
                                :controller-generation-id generation-id
                                :active-faults faults))
      ;; Stored VERBATIM, absence included.  The daemon rides the submessage
      ;; only while it has a merge to report, so retaining the last one it
      ;; sent would leave the narrator (merge-handlers.el) describing a phase
      ;; the daemon has already moved past.
      (agent-repl--ws-put workspace :pushed-merge-status merge-status)
      ;; Stored VERBATIM, absence included, exactly as the status above is.
      ;; The daemon clears the offer to take the card down, so retaining the
      ;; last one it sent would leave the narrator announcing a question the
      ;; user has already answered.
      (agent-repl--ws-put workspace :pushed-merge-dequeue-offer dequeue-offer)
      (agent-repl--log workspace
                       "frontend-apply-workspace-state: %s -> %s connectivity=%s status=%s session=%S generation=%S faults=%S cause=%s seq=%s turn-active=%S live-tasks=%s merged-at=%s merge-status-phase=%s merge-status-run=%s"
                       previous keyword connectivity session-status session-id
                       generation-id faults cause-kind cause-seq turn-active
                       live-tasks merged-at
                       (plist-get merge-status :phase)
                       (plist-get merge-status :run-id))
      (agent-repl--log-verbose workspace
                               "frontend-apply-workspace-state: merge-dequeue-offer=%s standing=%s"
                               (or (plist-get dequeue-offer :offer-id) "none")
                               (plist-get dequeue-offer :standing))
      ;; Session-ready latch (design §10 cutover gap): the SessionStart
      ;; managed hook that used to set the `:agent-ready' half of the
      ;; ws-fully-loaded latch was deleted in S2, orphaning
      ;; `agent-repl-ws-fully-loaded-functions'.  The daemon now owns
      ;; session-ready reporting, so the FIRST pushed WorkspaceState for a
      ;; workspace (any state) is the ready signal.  One-shot per workspace,
      ;; guarded by `:agent-ready-latched' (cleared when the ws plist resets
      ;; on kill/relaunch, so the next session re-latches).
      (agent-repl--frontend-maybe-latch-agent-ready workspace)
      ;; THE EMACS HALF OF THE RECOVERY SLO (lisp/recovery-slo.el), stamped
      ;; HERE and not at decode: what the SLO asks is whether this end's
      ;; RENDERED view is the new daemon's, and that is true only once the
      ;; pushed state has actually been stored — which is the line above.
      (agent-repl--recovery-slo-note-emacs workspace)
      ;; Re-key point for the merge reactive consequences (design §4.6/§9.3):
      ;; run AFTER the pushed state is stored so subscribers observe it.
      (agent-repl--frontend-run-state-transition-hook workspace keyword previous)
      keyword))))))

(defun agent-repl--merge-resurrect-on-failure (ws new _previous)
  "Re-open WS's tab when a pushed `:merge-failed' finds it closed.
Subscriber for `agent-repl-ws-state-transition-functions'.  The COMPLEMENT
of `agent-repl--frontend-resurrect-merge-failed': that path covers a cwd no
live workspace owns at all, while this one covers the data-only entry a
completed merge leaves behind (registered in `agent-repl--workspaces' with
no persp tab — see snapshot-load's register-merged case).  Both converge on
the same promotion: establish + front-reorder, so the failure is the
leftmost tab rather than invisible state.

Idempotent: once the tab is open `agent-repl--ws-open-p' is non-nil and a
re-pushed `:merge-failed' changes nothing.  A worktree gone from disk is
loud-logged and left alone."
  (when (and (eq new :merge-failed)
             (not (agent-repl--ws-open-p ws)))
    (let ((dir (agent-repl--ws-get ws :project-dir)))
      (if (and dir (file-directory-p dir))
          (progn
            (agent-repl--log ws
                             "merge-resurrect-on-failure: merge_failed pushed for tab-less workspace ws=%s dir=%s — re-establishing its tab"
                             ws dir)
            (agent-repl--establish-workspace ws dir)
            (agent-repl--reorder-workspace-to-front ws)
            (agent-repl--ws-put ws :merge-failed t))
        (agent-repl--log ws
                         "merge-resurrect-on-failure: ws=%s dir=%S MISSING on disk — cannot resurrect"
                         ws dir)))))

;; Registered like the sidebar/death reactors: `add-hook' auto-vivifies the
;; hook variable, and its `defvar ... nil' above does not reset a bound one.
(add-hook 'agent-repl-ws-state-transition-functions
          #'agent-repl--merge-resurrect-on-failure)

(defun agent-repl--frontend-resurrect-merge-failed (raw-workspace ws-state)
  "Re-establish the closed workspace at RAW-WORKSPACE and re-apply WS-STATE.
Called from `agent-repl--frontend-apply-workspace-state' when a pushed
`:merge-failed' names a cwd no live Emacs workspace owns.  Reuses
snapshot-load's merge-failure promotion (`agent-repl--establish-workspace'
plus `agent-repl--reorder-workspace-to-front', commands.el): a failed
cherry-pick must not hide as retained-but-unrendered state — surfacing it
as the leftmost tab forces the user to notice and act.

A worktree missing on disk cannot be resurrected; that case is loud-logged
and the frame stays retained-only.  Returns the re-applied render keyword,
or nil when resurrection was impossible."
  (let ((ws (file-name-nondirectory (directory-file-name raw-workspace))))
    (cond
     ((not (file-directory-p raw-workspace))
      (agent-repl--log nil
                       "frontend-resurrect-merge-failed: ws=%s dir=%s MISSING on disk — cannot resurrect; state retained only"
                       ws raw-workspace)
      nil)
     (t
      (agent-repl--log ws
                       "frontend-resurrect-merge-failed: merge_failed pushed for closed workspace ws=%s dir=%s — re-establishing its tab"
                       ws raw-workspace)
      (agent-repl--establish-workspace ws raw-workspace)
      (agent-repl--reorder-workspace-to-front ws)
      (agent-repl--ws-put ws :merge-failed t)
      (if (agent-repl--frontend-ws-name raw-workspace)
          ;; A live workspace owns the cwd now, so the ordinary apply path
          ;; stores the pushed state and runs the transition hook (sidebar
          ;; repaint, minibuffer narration).  The ownership check above is
          ;; the recursion bound: an establish that did not register the
          ;; dir would loop here forever, so it fails loudly instead.
          (agent-repl--frontend-apply-workspace-state ws-state)
        (agent-repl--log ws
                         "frontend-resurrect-merge-failed: ws=%s establish did NOT register dir=%s — state not re-applied"
                         ws raw-workspace)
        nil)))))

(defun agent-repl--frontend-maybe-latch-agent-ready (workspace)
  "Set the `:agent-ready' latch bit for WORKSPACE on its FIRST pushed state.
One-shot per workspace: guarded by the `:agent-ready-latched' plist key so
only the first `WorkspaceState' push arms the latch.  Loud-logged.  See
`agent-repl-ws-fully-loaded-functions'."
  (if (agent-repl--ws-get workspace :agent-ready-latched)
      ;; This check is reached on every later WorkspaceState frame.
      (agent-repl--log-verbose workspace
                               "frontend-latch-agent-ready: already latched; skip :agent-ready")
    (agent-repl--ws-put workspace :agent-ready-latched t)
    (agent-repl--log workspace
                     "frontend-latch-agent-ready: first pushed state for ws=%s — setting :agent-ready"
                     workspace)
    (agent-repl--latch-and-maybe-fire-loaded workspace :agent-ready)))

;;;; ---- StateSnapshot resync --------------------------------------------

(defun agent-repl--frontend-snapshot-item-id (item id-keys)
  "Return a diagnostic identity string for snapshot ITEM from ID-KEYS.
ID-KEYS is a list of plist keywords whose values identify the item on the
wire.  A non-plist ITEM still yields a printable form rather than erroring
— this runs on the failure path, where an unusable item is exactly what is
being reported."
  (if (not (keywordp (car-safe item)))
      (format "item=%S" item)
    (mapconcat (lambda (key)
                 (format "%s=%S" (substring (symbol-name key) 1)
                         (plist-get item key)))
               id-keys " ")))

(declare-function agent-repl--recovery-slo-note-wire-frame
                  "agent-repl-recovery-slo" (field ws))

(defun agent-repl--frontend-snapshot-note-wire (field items)
  "Stamp the recovery SLO's wire signal for every workspace ITEMS names.

THE SNAPSHOT ARM ATTRIBUTES ITS RECORDS.  A `StateSnapshot' carries no
top-level `workspace' — it carries a LIST of per-workspace records — so
the dispatch point that stamps a live `workspaceState'/`sessionView' has
nothing to attribute here and used to stamp nothing at all, while the very
same records went on to stamp the emacs signal one by one
\(`agent-repl--recovery-slo-note-emacs').  That is the `wire_ms=-1' beside
a real `emacs_ms' that made the whole connect path look wireless.

FIELD names the arm each record IS (`workspaceState' for `:workspaces',
`sessionView' for `:sessions'), and the decision about whether that arm is
evidence stays in `agent-repl--recovery-slo-note-wire-frame' — this
function attributes, it does not judge.

Called BEFORE the batch is applied, for the whole batch: what the wire
signal answers is that the frame carrying this workspace arrived, which is
already true of every record in it, and making the last record of a batch
wait for the first record's apply would put the emacs signal's question
into the wire signal's answer.  A record naming a workspace this host does
not own resolves to nil and is skipped — there is no attempt to stamp."
  (dolist (item items)
    (when (keywordp (car-safe item))
      (let ((ws (agent-repl--frontend-ws-name (plist-get item :workspace))))
        (when ws
          (agent-repl--recovery-slo-note-wire-frame field ws))))))

(defun agent-repl--frontend-apply-snapshot-items (kind items id-keys apply-fn)
  "Apply each of ITEMS through APPLY-FN, containing per-item failures.
KIND names the snapshot list for the log; ID-KEYS identifies an item within
it (see `agent-repl--frontend-snapshot-item-id').  A signal from APPLY-FN is
loud-logged with the item's identity and the error, then swallowed FOR THIS
ITEM ONLY so the resync continues — the aggregate is re-surfaced by
`agent-repl--frontend-apply-snapshot', which is what keeps this from being a
silent fallback.  Returns the number of items that failed."
  (let ((failures 0))
    (dolist (item items)
      (condition-case err
          (funcall apply-fn item)
        (error
         (setq failures (1+ failures))
         (agent-repl--warn
          nil
          "frontend-apply-snapshot: %s item FAILED — %s err-type=%s err=%s; CONTAINED, resync continues"
          kind (agent-repl--frontend-snapshot-item-id item id-keys)
          (car err) (error-message-string err)))))
    failures))

;;;; ---- Batched connect delivery: completeness accounting -----------------
;;
;; A connect snapshot may arrive as SEVERAL `StateSnapshot' frames — the fleet's
;; `:workspaces' split into batches so this host can APPLY as they land instead
;; of applying nothing until the last workspace of one huge frame has been
;; decoded.  Applying a workspace's state is the expensive half (perspective,
;; bookkeeping, readiness latches; ~18ms each at fleet scale) and decoding is
;; not (~3ms for the whole 299KB frame), so batching is what makes ONE
;; workspace's recovery independent of how many other workspaces exist.
;;
;; The daemon states the same `:workspaceTotal' — the count the WHOLE delivery
;; carries — on every batch, and numbers the batches with `:workspaceBatchIndex'
;; (0 is the LEAD batch, the only one carrying the wholesale fields).  An older
;; daemon sends neither, which reads as "this frame is the whole delivery" and
;; is exactly the pre-batching behavior.
;;
;; THE VIEW IS PARTIAL UNTIL IT IS NOT.  A partial view is never reported as
;; complete: `agent-repl--frontend-snapshot-complete-p' is nil from the lead
;; batch until this host has been handed `:workspaceTotal' DISTINCT workspaces,
;; the snapshot-applied hook (the reconnect edge every recovery subscriber runs
;; off) fires only at that instant, and any disagreement between batches about
;; the total abandons the delivery LOUDLY rather than completing it.

(defvar agent-repl--frontend-snapshot-expected-workspaces nil
  "Workspaces the in-flight connect delivery will carry, or nil when none is open.")

(defvar agent-repl--frontend-snapshot-delivered-workspaces
  (make-hash-table :test 'equal)
  "Workspace keys this connect delivery has handed to the apply path.
A key is recorded whether or not its apply SUCCEEDED: a per-item failure is
loud-logged and surfaced on its own, and it must not strand the delivery
short of its total forever.  This counts DELIVERY, not correctness.")

(defvar agent-repl--frontend-snapshot-complete-p nil
  "Non-nil once the current connect delivery has landed every workspace.
Read by anything that must not mistake a partial fleet for the fleet.")

(defun agent-repl--frontend-snapshot-view-complete-p ()
  "Return non-nil when the applied view covers the daemon's whole fleet."
  agent-repl--frontend-snapshot-complete-p)

(defun agent-repl--frontend-snapshot-invalidate ()
  "Forget any connect delivery: the view is PARTIAL until a new one lands."
  (setq agent-repl--frontend-snapshot-expected-workspaces nil)
  (clrhash agent-repl--frontend-snapshot-delivered-workspaces)
  (setq agent-repl--frontend-snapshot-complete-p nil))

(defun agent-repl--frontend-snapshot-reset-delivery (total)
  "Open a fresh connect delivery expecting TOTAL workspaces."
  (setq agent-repl--frontend-snapshot-expected-workspaces total)
  (clrhash agent-repl--frontend-snapshot-delivered-workspaces)
  (setq agent-repl--frontend-snapshot-complete-p nil))

(defun agent-repl--frontend-snapshot-abandon-delivery (reason)
  "Abandon the in-flight delivery for REASON, leaving the view INCOMPLETE.
The applied workspaces are kept — they are real state that really landed —
but the view stops being completable, because a delivery whose own account
of itself is inconsistent cannot be shown to have covered the fleet."
  (setq agent-repl--frontend-snapshot-expected-workspaces nil)
  (setq agent-repl--frontend-snapshot-complete-p nil)
  (agent-repl--warn
   nil
   "frontend-apply-snapshot: connect delivery ABANDONED — %s; the applied view is PARTIAL and is not reported complete"
   reason))

(defun agent-repl--frontend-snapshot-note-delivered (workspaces)
  "Record WORKSPACES as delivered; return non-nil when the fleet is now whole."
  (dolist (item workspaces)
    (let ((key (and (keywordp (car-safe item)) (plist-get item :workspace))))
      (when (stringp key)
        (puthash key t agent-repl--frontend-snapshot-delivered-workspaces))))
  (let ((expected agent-repl--frontend-snapshot-expected-workspaces)
        (have (hash-table-count agent-repl--frontend-snapshot-delivered-workspaces)))
    (when (and expected (>= have expected))
      (setq agent-repl--frontend-snapshot-complete-p t))
    agent-repl--frontend-snapshot-complete-p))

(defun agent-repl--frontend-apply-snapshot-continuation (snapshot total)
  "Apply a CONTINUATION batch SNAPSHOT of a connect delivery of TOTAL workspaces.
A continuation carries `:workspaces' and nothing else: the wholesale
rebuilds and the daemon-global views belong to the lead batch and are
stated exactly once per connect.  Returns the count of states applied."
  (let ((workspaces (plist-get snapshot :workspaces))
        (index (or (plist-get snapshot :workspaceBatchIndex) 0)))
    (cond
     ((null agent-repl--frontend-snapshot-expected-workspaces)
      ;; A continuation with no lead is a delivery this host never saw the
      ;; start of.  Its states are applied — they are the daemon's truth — but
      ;; the view cannot be called complete, and the gap is loud.
      (agent-repl--frontend-snapshot-abandon-delivery
       (format "batch %d arrived with no lead batch open" index)))
     ((/= total agent-repl--frontend-snapshot-expected-workspaces)
      (agent-repl--frontend-snapshot-abandon-delivery
       (format "batch %d states workspaceTotal=%d, the lead batch stated %d"
               index total agent-repl--frontend-snapshot-expected-workspaces))))
    (agent-repl--log nil
                     "frontend-apply-snapshot: connect batch %d — %d workspace(s), total=%d delivered-before=%d"
                     index (length workspaces) total
                     (hash-table-count agent-repl--frontend-snapshot-delivered-workspaces))
    ;; Wire evidence for this batch's whole slice, before any of it is applied.
    (agent-repl--frontend-snapshot-note-wire "workspaceState" workspaces)
    (let ((failures
           (let ((agent-repl--frontend-applying-snapshot-state t))
             (agent-repl--frontend-apply-snapshot-items
              "workspace-state" workspaces '(:workspace :state)
              #'agent-repl--frontend-apply-workspace-state))))
      (when (> failures 0)
        (agent-repl--warn
         nil
         "frontend-apply-snapshot: %d item(s) FAILED in connect batch %d — see the per-item lines above"
         failures index)))
    (when (agent-repl--frontend-snapshot-note-delivered workspaces)
      (agent-repl--log nil
                       "frontend-apply-snapshot: connect delivery COMPLETE at batch %d — %d workspace(s)"
                       index (hash-table-count agent-repl--frontend-snapshot-delivered-workspaces))
      (agent-repl--uds-run-snapshot-applied-hook))
    (length workspaces)))

(defun agent-repl--frontend-apply-snapshot (snapshot)
  "Apply a `StateSnapshot' frame SNAPSHOT (a plist) — full resync.
Handler for the `snapshot' oneof arm.  Applies every `WorkspaceState' in
the snapshot's `:workspaces' list via
`agent-repl--frontend-apply-workspace-state', REBUILDS the SessionView
store wholesale from `:sessions' (the daemon's full roster, terminal
sessions included — a wholesale rebuild drops entries a bounced daemon no
longer knows, unlike an upsert), and applies `:daemon' (DaemonView) for
boot detection.  The `:catalogs' array belongs to the webapp's detached-task
roster; Emacs counts it for diagnostics but deliberately applies none of it.
Returns the count of workspace states applied.

CONTAINMENT CONTRACT: a failure applying ONE snapshot item is loud-logged
with the item's identity, counted, and surfaced afterwards both in the log
and via `message' — but it NEVER aborts the resync.  Readiness (the
DaemonView) and every remaining item must land regardless, because this is
the only frame that establishes them: a retained host action whose handler
signals (the executor in workspace-create-client.el acknowledges failures to
the daemon and then deliberately RE-SIGNALS) used to propagate out of here
and leave `agent-repl--frontend-daemon-view' nil forever, so every later
readiness read failed with \"daemon never became ready\".  Containment is
per-item only; a host action arriving as its own live frame keeps the
executor's re-signal behavior untouched.  This is not a silent fallback: no
error is swallowed without a log line, and an aggregate failure count is
pushed to the user.

On the scoped per-session webapp connection the daemon omits `:daemon' and
retains only that session's catalog; a nil `:daemon' is therefore skipped,
not an error.  This handler runs on the UNSCOPED Emacs connection, which
receives every catalog but has no per-task roster."
  (let* ((batch-index (or (plist-get snapshot :workspaceBatchIndex) 0))
         ;; An unset total is an older daemon, or a snapshot that was never
         ;; batched (a resync, a GUI lease renewal): the frame IS the delivery.
         (declared-total (or (plist-get snapshot :workspaceTotal) 0))
         (total (if (> declared-total 0)
                    declared-total
                  (length (plist-get snapshot :workspaces)))))
    (if (> batch-index 0)
        (agent-repl--frontend-apply-snapshot-continuation snapshot total)
      (agent-repl--frontend-snapshot-reset-delivery total)
      (agent-repl--frontend-apply-snapshot-lead snapshot))))

(defun agent-repl--frontend-apply-snapshot-lead (snapshot)
  "Apply the LEAD batch of a connect delivery, SNAPSHOT.
This is the whole of the pre-batching resync: the wholesale roster rebuilds,
the daemon-global views, and the lead batch\\='s share of `:workspaces'.  See
`agent-repl--frontend-apply-snapshot' for the containment contract."
  (let ((workspaces (plist-get snapshot :workspaces))
        (sessions (plist-get snapshot :sessions))
        (catalogs (plist-get snapshot :catalogs))
        (inits (plist-get snapshot :inits))
        (available (plist-get snapshot :workspaceAvailable))
        (host-actions (plist-get snapshot :hostActions))
        (daemon (plist-get snapshot :daemon))
        ;; `plist-member', not `plist-get': an `idle' view is `{}' and
        ;; decodes to nil, so presence and emptiness are different facts.
        (shutdown-schedule (plist-member snapshot :shutdownSchedule)))
    (agent-repl--log nil
                     "frontend-apply-snapshot: resync — %d workspace(s), %d session(s), %d webapp-only catalog(s), %d init(s), %d workspace-available, %d host-action(s), daemon=%S shutdown-schedule-present=%s"
                     (length workspaces) (length sessions) (length catalogs)
                     (length inits) (length available) (length host-actions)
                     (and daemon t) (if shutdown-schedule "t" "nil"))
    ;; Rebuild the session roster from scratch: the snapshot is authoritative,
    ;; so a session absent from it (a bounced daemon never heard of) must not
    ;; linger in the store where the orphan/live-p reads would still see it.
    (clrhash agent-repl--frontend-session-views)
    (clrhash agent-repl--frontend-workspace-state-views)
    ;; Rebuild the retained-SystemInit roster wholesale too (same rationale):
    ;; the slash-command menu source must not carry a bounced daemon's stale
    ;; session inits.  Cleared HERE, with the other wholesale clears and ahead
    ;; of everything, so no reader can see a bounced daemon's roster no matter
    ;; where in this function it runs.
    (clrhash agent-repl--frontend-session-inits)
    ;; Wire evidence for the lead batch's slice, before any of it is applied.
    (agent-repl--frontend-snapshot-note-wire "workspaceState" workspaces)
    (agent-repl--frontend-snapshot-note-wire "sessionView" sessions)
    (let ((failures 0))
      ;; ---- THE RECOVERY-CRITICAL PREFIX ---------------------------------
      ;;
      ;; ORDER IS LATENCY HERE, and this prefix is ordered by what a recovering
      ;; workspace waits on rather than by what reads well as a list.
      ;;
      ;; A workspace's emacs-side recovery signal is stamped when ITS
      ;; `WorkspaceState' has been stored (`agent-repl--recovery-slo-note-emacs'
      ;; at the end of `agent-repl--frontend-apply-workspace-state').  The
      ;; daemon already went to the trouble of putting the live workspaces in
      ;; the LEAD batch and shipping it first (frontend/snapshotbatch.go), for
      ;; exactly that reason — but this end then made that batch's workspaces
      ;; queue behind the whole fleet's session views and SystemInits, which
      ;; are wholesale rebuilds that ride the lead batch and scale with the
      ;; ROSTER, not with the workspace.  The lead batch's own workspaces
      ;; therefore paid the full fleet cost the batching was introduced to
      ;; remove, which is the `emacs_ms' that moved from ~5ms to ~2.3s.
      ;;
      ;; So the per-workspace applies go FIRST, behind only the two things they
      ;; genuinely depend on: the DaemonView (identity/readiness, which nothing
      ;; here can fail into) and WorkspaceAvailable (which materializes the
      ;; local owner a newly-created path's render state needs).  Nothing in
      ;; `agent-repl--frontend-apply-workspace-state' reads the session-view or
      ;; SystemInit stores, so nothing it does depends on their rebuilds having
      ;; run — and both stores were CLEARED above, so a subscriber that reaches
      ;; for one sees an honestly empty roster rather than a bounced daemon's.
      ;;
      ;; Identity/readiness lands BEFORE any item that can fail on side
      ;; effects: `agent-repl--frontend-daemon-ready-p' reads the DaemonView,
      ;; and nothing about the daemon's own identity depends on a workspace,
      ;; an init, or a host action having applied cleanly.
      (when daemon
        (agent-repl--frontend-apply-daemon-view daemon))
      ;; Replayed daemon-owned creation jobs materialize before their ACK.  The
      ;; handlers are defined later in load order by workspace-create-client.el;
      ;; snapshots arrive only after the full module has loaded and connected.
      (setq failures
            (+ failures
               (agent-repl--frontend-apply-snapshot-items
                "workspace-available" available '(:jobId :finalName :worktreePath)
                #'agent-repl--workspace-create-handle-available)))
      ;; Apply render state only AFTER WorkspaceAvailable has established the
      ;; local perspective/bookkeeping owner for a newly-created path.
      (setq failures
            (+ failures
               (let ((agent-repl--frontend-applying-snapshot-state t))
                 (agent-repl--frontend-apply-snapshot-items
                  "workspace-state" workspaces '(:workspace :state)
                  #'agent-repl--frontend-apply-workspace-state))))
      ;; ---- THE WHOLESALE REBUILDS ---------------------------------------
      ;;
      ;; Roster-scaled work, deliberately AFTER the per-workspace prefix: these
      ;; are read by menus and liveness queries rather than by the render this
      ;; connect is racing to restore.
      (setq failures
            (+ failures
               (agent-repl--frontend-apply-snapshot-items
                "session-view" sessions '(:workspace)
                #'agent-repl--frontend-apply-session-view)))
      (setq failures
            (+ failures
               (agent-repl--frontend-apply-snapshot-items
                "session-init" inits '(:workspace)
                #'agent-repl--frontend-apply-session-init)))
      ;; The drain lease, when this daemon carries the field at all.  Routed
      ;; through the per-item container so a malformed view is loud AND
      ;; counted without costing the resync every other item — the lease is
      ;; not rendered here, so it is the last thing that should abort a
      ;; reconnect.  An absent field is an older daemon, not a failure, and
      ;; leaves the recorded lease at its honest "unknown".
      (setq failures
            (+ failures
               (agent-repl--frontend-apply-snapshot-items
                "shutdown-schedule"
                (when shutdown-schedule (list (plist-get snapshot :shutdownSchedule)))
                '(:draining)
                #'agent-repl--frontend-apply-shutdown-schedule)))
      ;; Last, and deliberately after the DaemonView: the host-action executor
      ;; re-signals handler failures by contract, and a retained action for a
      ;; dir with no live workspace must not cost the resync its readiness.
      (setq failures
            (+ failures
               (agent-repl--frontend-apply-snapshot-items
                "host-action" host-actions '(:actionId)
                #'agent-repl--workspace-create-handle-host-action)))
      (agent-repl--log nil
                       "frontend-apply-snapshot: applied %d workspace state(s), %d session(s), %d init(s), %d workspace-available, %d host-action(s); ignored %d webapp-only catalog(s); %d item failure(s)"
                       (length workspaces) (length sessions) (length inits)
                       (length available) (length host-actions)
                       (length catalogs) failures)
      (when (> failures 0)
        ;; Loud on BOTH channels: a contained failure that only reached the log
        ;; would be a silent fallback from the user's seat.
        (agent-repl--warn nil
                         "frontend-apply-snapshot: %d item(s) FAILED during snapshot resync — see the per-item lines above; resync completed for the rest"
                         failures)
        (agent-repl--user-message
         nil "%d item(s) failed to resync — see the agent-repl log for detail"
         (list failures)
         :detail (format (concat "frontend-apply-snapshot failures=%d workspaces=%d "
                                 "sessions=%d inits=%d available=%d host-actions=%d")
                         failures (length workspaces) (length sessions)
                         (length inits) (length available) (length host-actions)))))
    ;; THE RECONNECT IS DONE HERE, and nowhere earlier.  Every subscriber that
    ;; needs the state of the world as of reconnection — the recovery sweep
    ;; that re-ensures each workspace, the retraction that takes the outage
    ;; notices down — runs off this edge rather than off the socket's open
    ;; transition, where the roster it would read is still empty.
    ;;
    ;; It runs even when items failed: a partial resync is still a live link,
    ;; and leaving the outage notices standing over it would report an outage
    ;; that is over.  The per-item failures were surfaced above on their own.
    ;;
    ;; It does NOT run while the connect delivery is still arriving in batches.
    ;; The edge means "the state of the world as of reconnection", and half a
    ;; fleet is not that world — the sweep that runs off it would re-ensure
    ;; workspaces it has not been told about yet.  The remaining batches fire
    ;; it (see `agent-repl--frontend-apply-snapshot-continuation'); a delivery
    ;; that never completes leaves it unfired, which is exactly what a
    ;; never-delivered snapshot does today.
    (if (agent-repl--frontend-snapshot-note-delivered workspaces)
        (agent-repl--uds-run-snapshot-applied-hook)
      (agent-repl--log nil
                       "frontend-apply-snapshot: connect delivery PARTIAL after the lead batch — %d of %s workspace(s); snapshot-applied hook held"
                       (hash-table-count agent-repl--frontend-snapshot-delivered-workspaces)
                       agent-repl--frontend-snapshot-expected-workspaces))
    (length workspaces)))

;;;; ---- DegradedNotice: RETIRED (F4, wire removed in step 11) -----------
;;
;; `agent-repl--frontend-apply-degraded-notice' lived here.  It echoed a raw
;; component/reason pair the daemon had already classified, carried no
;; correlation between a report and its all-clear, and said nothing about
;; how much conversation the outage had cost.
;;
;; Degradation is now a self-resolving failure CARD on the conversation
;; plane, plus a move on the SSM's legacy impairment projection that colors the workspace.
;; The frame arm itself is gone (reserved 8/"degraded_notice" in
;; frontend.proto, step 11): `degradedNotice' is no longer in
;; `agent-repl--uds-known-frame-fields', so a push from a daemon old enough
;; to still send it is now the loud unknown-field signal rather than a
;; settled no-op — the expected, approved shape of a breaking wire change.

;;;; ---- SessionView store -----------------------------------------------
;;
;; The daemon pushes a `SessionView' when a session is created/deleted (and
;; carries the full roster — terminal sessions included — in the connect
;; snapshot's `:sessions').  This is the pushed-frame replacement for the
;; Emacs-side GET /sessions poller: the session-CRUD reads that used to hit
;; the daemon (live-p, the create→id correlation, turn-active gating, orphan
;; reap roster) key off THIS store instead.  Keyed by session id; each value
;; is the decoded SessionView plist.

(defvar agent-repl--frontend-session-views (make-hash-table :test 'equal)
  "Hash of WORKSPACE -> the decoded `SessionView' plist of its CURRENT session.

ONE ENTRY PER WORKSPACE, and it is the workspace\='s current session: a
superseded predecessor REPLACES nothing and a successor replaces its
predecessor, rather than the two accumulating side by side.  A workspace
has at most one live session — the daemon supersedes every older session
on the same cwd at create time (supersede.go), so a workspace\='s session
is a well-defined single thing and this is the store that says so.

Populated by `agent-repl--frontend-apply-session-view' (per-session
pushes) and rebuilt wholesale from the connect snapshot\='s `:sessions'.
The single source of truth for daemon session metadata; Emacs does not
poll for it.")

(defun agent-repl--frontend-session-view (workspace)
  "Return the stored `SessionView' plist for WORKSPACE, or nil when unknown.
WORKSPACE is the absolute cwd the daemon routes by — WS\='s
`agent-repl--frontend-ws-command-key', not a workspace NAME."
  (and workspace (gethash workspace agent-repl--frontend-session-views)))

(defun agent-repl--frontend-session-views-all ()
  "Return every stored `SessionView' plist (one per known workspace)."
  (hash-table-values agent-repl--frontend-session-views))

(defun agent-repl--frontend-workspace-session-live-p (workspace)
  "Return non-nil when WORKSPACE\='s current `SessionView' is not terminal.
The pushed-frame replacement for a per-session liveness probe: liveness
is a fact about the workspace, answered from the view the daemon last
pushed for it."
  (let ((view (agent-repl--frontend-session-view workspace)))
    (and view (not (eq (plist-get view :terminal) t)))))

(defun agent-repl--frontend-session-view-supersedes-p (incoming stored)
  "Return non-nil when INCOMING should replace STORED for their workspace.

A LIVE view always wins: it is the workspace\='s current session by
definition.  A TERMINAL view wins only over nothing, over another
terminal view, or over the SAME session it reports the death of —
otherwise it is a superseded predecessor arriving after its successor,
and letting it land would retire a session that is running.

Both ids read here come from the daemon\='s own views; neither is a key
this store is indexed by."
  (cond
   ((null stored) t)
   ((not (eq (plist-get incoming :terminal) t)) t)
   ((eq (plist-get stored :terminal) t) t)
   (t (equal (plist-get incoming :sessionId) (plist-get stored :sessionId)))))

(defun agent-repl--frontend-store-session-view (view)
  "Upsert VIEW (a decoded `SessionView' plist) as its workspace\='s current one.
A view with no `:workspace' is an invariant violation and fails loudly
\(No-Silent-Fallbacks) — the daemon always stamps the routing key it
delivers the view under.  Returns the workspace."
  (let ((workspace (plist-get view :workspace)))
    (when (or (null workspace) (and (stringp workspace) (string-empty-p workspace)))
      (agent-repl--log nil
                       "frontend-store-session-view: MISSING workspace in %S — no fallback"
                       view)
      (error "agent-repl frontend: SessionView missing workspace"))
    (let ((stored (gethash workspace agent-repl--frontend-session-views)))
      (if (agent-repl--frontend-session-view-supersedes-p view stored)
          (puthash workspace view agent-repl--frontend-session-views)
        (agent-repl--log-verbose
         (agent-repl--frontend-ws-name workspace)
         "frontend-store-session-view: DROPPED superseded terminal view workspace=%S"
         workspace)))
    workspace))

(defvar agent-repl--frontend-surfaced-deaths (make-hash-table :test 'equal)
  "Hash of SESSION ID -> the classified death already surfaced for it.

A terminal SessionView is re-pushed on every snapshot and on any later
write to its record, so without this latch a dead session would announce
its death again on every reconnect — turning one honest report into
recurring noise about something the user already knows.

KEYED BY SESSION, NOT BY WORKSPACE, because a workspace has as many
terminal records as it has had sessions and the daemon pushes a
SessionView for every one of them.  A single workspace-wide slot held
whichever death arrived last, so two records on one cwd dying
differently — a supersede and a delete, say — evicted each other's latch
on every snapshot and re-announced BOTH, forever.  One session dies once,
so the session is the identity that makes the report once-only.

The DEATH ITSELF is the latch value: what must not repeat is the report,
and a session whose death is later re-pushed SETTLED has something new to
say (the surfacing path logs that close rather than warning about it).")

(defun agent-repl--frontend-apply-session-view (view)
  "Apply a `SessionView' frame VIEW (a plist).  Handler for `sessionView'.
Upserts it into `agent-repl--frontend-session-views', logs the parity
fields the reattach/orphan/turn-active reads consume, and surfaces a
session DEATH the first time one is seen.  Returns the id.

The death reader is new (F4).  `death_reason' had two producers and ZERO
readers — this file never read it at all, despite a comment elsewhere
claiming the detail rides the pushed `SessionView' — because a free string
gave no way to know what class of failure it described.  `:death' is that
same fact classified, so it can finally be shown."
  (let ((workspace (agent-repl--frontend-store-session-view view)))
    ;; A `SessionView''s `:workspace' is the wire's session CWD, not a
    ;; workspace name, so it can never index the workspace hash.  Route through
    ;; the file's own resolver, which also passes a frame that already carries
    ;; a name.  The raw wire value stays in the message text, since that is the
    ;; field an operator correlates against the daemon.
    (agent-repl--log (agent-repl--frontend-ws-name workspace)
                     ;; `token_utilization' left SessionView with the component
                     ;; reshape — it was a persistence record carried verbatim
                     ;; to a renderer that had to digest it — so the trace no
                     ;; longer claims to report a field the wire cannot carry.
                     "frontend-apply-session-view: ws=%s terminal=%S claude-id=%s pending=%s"
                     workspace (plist-get view :terminal)
                     (or (plist-get view :claudeSessionId) "nil")
                     (or (plist-get view :pendingPermissions) "0"))
    (agent-repl--frontend-surface-session-death workspace view)
    workspace))

(defun agent-repl--frontend-surface-session-death (key view)
  "Surface VIEW's classified death for workspace KEY, at most once per session.
KEY is the wire cwd the view arrived under.

Returns the surfaced text, or nil when the session is alive, carries no
classified death, or has already been reported.

A view that carries a death and no session id SIGNALS.  The death is
latched per session, so an anonymous one has no identity to latch and
would re-announce on every snapshot — the very defect this latch exists
for — and the wire cannot produce one: `SessionView.session_id' is what
every other reader keys on too."
  ;; Resolved to a workspace NAME once, up front.  This runs for every
  ;; replayed SessionView frame — including the no-death verbose path — so
  ;; carrying the wire CWD any further would make the chattiest branch in the
  ;; frame handler signal inside the connection's process filter.
  (let* ((workspace (agent-repl--frontend-ws-name key))
         (item (plist-get view :death))
         (session (plist-get view :sessionId)))
    (cond
     ((null item)
      ;; SessionView frames are replayed in every snapshot; absence of death
      ;; is useful only while tracing that chatty stream.
      (agent-repl--log-verbose workspace
                               "frontend-surface-session-death: ws=%s outcome=no-death"
                               key)
      nil)
     ((or (null session) (equal session ""))
      (agent-repl--log workspace
                       "frontend-surface-session-death: MALFORMED ws=%s — a death with no session id has nothing to latch on; no fallback"
                       key)
      (error "agent-repl frontend: SessionView death missing session id"))
     ((equal item (gethash session agent-repl--frontend-surfaced-deaths))
      (agent-repl--log-verbose workspace
                               "frontend-surface-session-death: ws=%s session=%s outcome=already-surfaced"
                               key session)
      nil)
     (t
      (puthash session item agent-repl--frontend-surfaced-deaths)
      ;; `death' is a `FailureCardView' carried OUTSIDE the feed, so it was
      ;; never filed under a ConversationItem and has no uuid to pass on.
      (let ((failure (agent-repl-failure-from-wire item)))
        (agent-repl--log workspace
                         "frontend-surface-session-death: ws=%s session=%s outcome=surface class=%s kind=%s resolved=%S"
                         key session (plist-get failure :class)
                         (plist-get failure :type) (plist-get failure :resolved))
        (agent-repl-failure-surface workspace failure))))))

;;;; ---- SessionInit store (slash-command menu source) -------------------
;;
;; The daemon pushes a `SessionInitView' (retained `SystemInit') on attach
;; and carries the full roster in the connect snapshot's `:inits'.  This is
;; the pushed-frame replacement for the deleted GET /commands HTTP menu: the
;; input buffer's slash-command completion (input.el) reads the retained
;; `SystemInit''s `slashCommands' off THIS store instead of fetching.  Keyed
;; by workspace; each value is the decoded `SystemInit' plist.

(defvar agent-repl--frontend-session-inits (make-hash-table :test 'equal)
  "Hash of WORKSPACE -> decoded `SystemInit' plist (from `SessionInitView').

ONE ENTRY PER WORKSPACE, for its current session: a successor's init
replaces its predecessor's rather than accumulating beside it, so the
menu a workspace offers is the one its running session announced.

Populated by `agent-repl--frontend-apply-session-init' (per-session
pushes) and rebuilt wholesale from the connect snapshot's `:inits'.  The
source of truth for the slash-command menu; Emacs does not poll for it.")

(defun agent-repl--frontend-session-init (workspace)
  "Return the stored `SystemInit' plist for WORKSPACE, or nil when unknown.
WORKSPACE is the absolute cwd the daemon routes by — WS's
`agent-repl--frontend-ws-command-key', not a workspace NAME."
  (and workspace (gethash workspace agent-repl--frontend-session-inits)))

(defun agent-repl--frontend-store-session-init (view)
  "Upsert a `SessionInitView' VIEW's `SystemInit' under its workspace.
A view with no `:workspace' is an invariant violation and fails loudly
\(No-Silent-Fallbacks).  Returns the workspace."
  (let ((workspace (plist-get view :workspace))
        (init (plist-get view :init)))
    (when (or (null workspace) (and (stringp workspace) (string-empty-p workspace)))
      (agent-repl--log nil
                       "frontend-store-session-init: MISSING workspace in %S — no fallback"
                       view)
      (error "agent-repl frontend: SessionInitView missing workspace"))
    (puthash workspace init agent-repl--frontend-session-inits)
    workspace))

(defun agent-repl--frontend-apply-session-init (view)
  "Apply a `SessionInitView' frame VIEW (a plist).  Handler for `sessionInit'.
Upserts its retained `SystemInit' into `agent-repl--frontend-session-inits'
and logs the slash-command count the completion menu consumes.  Returns the
workspace."
  (let ((workspace (agent-repl--frontend-store-session-init view)))
    ;; `:workspace' is a session CWD on the wire — see
    ;; `agent-repl--frontend-apply-session-view' for why it must be resolved
    ;; to a name before it can select a workspace log sink.
    (agent-repl--log (agent-repl--frontend-ws-name workspace)
                     "frontend-apply-session-init: ws=%s slash-commands=%d skills=%d model=%s"
                     workspace
                     (length (plist-get (plist-get view :init) :slashCommands))
                     (length (plist-get (plist-get view :init) :skills))
                     (or (plist-get (plist-get view :init) :model) "nil"))
    workspace))

;;;; ---- DaemonView (boot/version/binary mtime) --------------------------
;;
;; The daemon pushes a `DaemonView' in the connect snapshot's `:daemon' (and
;; as its own frame).  It is the pushed-frame replacement for the whole
;; daemon-IDENTITY half of the deleted GET /sessions envelope: the boot id
;; (reattach give-up reset) and `daemon_binary_mtime_ms' (the startup
;; staleness bounce in daemon.el), plus the protocol/daemon version strings.

(defvar agent-repl--frontend-last-daemon-view nil
  "The most recently applied `DaemonView' plist, or nil before the first frame.
Read through `agent-repl--frontend-daemon-view', which is the source of
truth for daemon identity now that Emacs no longer polls GET /sessions.")

(defun agent-repl--frontend-daemon-view ()
  "Return the last-pushed `DaemonView' plist, or nil before the first frame.
Trustworthy only while the UDS link is LIVE: a dropped link leaves the last
view behind, and after a daemon bounce it describes the PREVIOUS instance
until the reconnect snapshot lands.  Callers that must not act on a stale
view (readiness, binary-staleness) therefore gate on
`agent-repl--uds-connected-p' — a disconnected link reads as \"unknown\",
which is exactly how the old HTTP probes treated an unreachable daemon."
  agent-repl--frontend-last-daemon-view)

(defun agent-repl--frontend-invalidate-daemon-view (reason)
  "Invalidate the retained daemon identity before a new UDS connection.
REASON names the connection boundary that made the prior view stale.  The
next `agent-repl--frontend-after-ready' must then poll until a fresh snapshot
applies a new `DaemonView'; merely opening a socket can no longer satisfy
readiness with the previous daemon instance's identity."
  (let ((prior agent-repl--frontend-last-daemon-view))
    (setq agent-repl--frontend-last-daemon-view nil)
    (agent-repl--log
     nil
     "frontend-daemon-view invalidated: reason=%s prior-present=%s prior-boot-id=%S result=nil"
     reason (if prior "t" "nil") (plist-get prior :bootId)))
  nil)

(defun agent-repl--frontend-daemon-view-binary-mtime-seconds ()
  "Return the pushed daemon binary mtime as integer Unix SECONDS, or nil.
Reads `:daemonBinaryMtimeMs' off the stored `DaemonView'.  The proto field
is an int64, which protojson encodes as a JSON STRING, so a string is
parsed as well as a number.  Nil when no view has been pushed, when the
field is absent (a daemon predating it), or when the value is non-positive
\(a daemon whose boot-time self-stat failed) — a daemon that cannot name
its own binary is never judged stale on a guess."
  (let* ((raw (plist-get (agent-repl--frontend-daemon-view) :daemonBinaryMtimeMs))
         (ms (cond ((numberp raw) raw)
                   ((and (stringp raw) (string-match-p "\\`-?[0-9]+\\'" raw))
                    (string-to-number raw)))))
    (and ms (> ms 0) (floor (/ ms 1000)))))

(defun agent-repl--frontend-apply-daemon-view (view)
  "Apply a `DaemonView' frame VIEW (a plist).  Handler for `daemonView'.
Stores VIEW as `agent-repl--frontend-last-daemon-view' (the daemon-identity
store the readiness + binary-staleness reads consume) and routes the daemon
`:bootId' into `agent-repl--frontend-note-boot-id', so a daemon-instance
change still resets the reattach give-ups.  Returns the boot id."
  (let ((boot-id (plist-get view :bootId)))
    (setq agent-repl--frontend-last-daemon-view view)
    (agent-repl--log nil
                     "frontend-apply-daemon-view: boot-id=%s protocol=%s version=%s mtime-ms=%s"
                     (or boot-id "nil") (plist-get view :protocolVersion)
                     (plist-get view :daemonVersion)
                     (or (plist-get view :daemonBinaryMtimeMs) "nil"))
    (agent-repl--frontend-note-boot-id boot-id)
    boot-id))

;;;; ---- ShutdownScheduleView: the daemon-global drain lease -------------
;;
;; The daemon broadcasts this on EVERY change and carries it in the connect
;; snapshot, so a client that joins mid-drain sees the lease without waiting
;; for an edge.  Emacs RENDERS NONE OF IT — the drain banner is the webapp's,
;; by the same division that keeps the queue and progress arms webapp-only.
;;
;; It is still recorded here rather than ignored, because Emacs is a SENDER on
;; this contract: `CancelScheduledShutdownCmd' needs the live `schedule_id',
;; and the proto is explicit that a stale id is a loud daemon nack.  The only
;; place that id exists on this side is the pushed view, so the frame is the
;; cancel command's sole input, not decoration.

(defvar agent-repl--frontend-shutdown-schedule nil
  "The daemon's last-pushed `ShutdownScheduleView', normalized, or nil.

nil means NO VIEW HAS EVER BEEN RECEIVED on this connection — genuinely
unknown, which is NOT the same fact as `idle' and must never be read as
one.  A daemon too old to carry the arm leaves this nil forever, and
every reader treats unknown as \"cannot act\", never as \"no lease\".

Otherwise a plist naming exactly one arm of the proto oneof:

  (:state :idle)
  (:state :draining :schedule-id S :scheduled-at-ms N :cause C
   :stop-shims BOOL :holds HOLDS)

HOLDS is the decoded `ShutdownHold' list verbatim; Emacs counts it for
the log and interprets none of it.")

(defun agent-repl-frontend-shutdown-schedule ()
  "Return the recorded drain-lease state plist, or nil when unknown.
The single reader of `agent-repl--frontend-shutdown-schedule', so the
unknown/idle distinction stays in one place."
  agent-repl--frontend-shutdown-schedule)

(defun agent-repl-frontend-scheduled-shutdown-id ()
  "Return the live schedule id when a shutdown is scheduled, else nil.
Nil for BOTH `idle' and never-received: neither can name a schedule, and
a caller that needs one must fail loudly rather than invent an id."
  (let ((schedule agent-repl--frontend-shutdown-schedule))
    (when (eq (plist-get schedule :state) :draining)
      (plist-get schedule :scheduleId))))

(defun agent-repl--frontend-shutdown-schedule-summary (schedule)
  "Return a one-line log summary of normalized SCHEDULE."
  (pcase (plist-get schedule :state)
    ('nil "unknown")
    (:idle "idle")
    (:draining (format "draining id=%s cause=%S stop-shims=%s holds=%d"
                       (plist-get schedule :scheduleId)
                       (plist-get schedule :cause)
                       (if (plist-get schedule :stopShims) "t" "nil")
                       (length (plist-get schedule :holds))))
    (state (format "unrecognized:%S" state))))

(defun agent-repl--frontend-apply-shutdown-schedule (view)
  "Apply a pushed `ShutdownScheduleView' VIEW (a plist) — record the lease.
Handler for the `shutdownSchedule' oneof arm and for the connect
snapshot's field of the same name.  Returns the normalized plist now in
`agent-repl--frontend-shutdown-schedule'.

Exactly one arm is set on the wire by contract, so a view with neither or
both fails loudly (log + `error') — no defaulting to `idle', which would
let a malformed frame silently cancel a real drain from Emacs's seat.  A
`draining' arm with no `schedule_id' fails the same way: an unidentifiable
schedule cannot be cancelled, and recording it would only defer the
failure to the cancel.

NOTE the empty-message decode: `ShutdownScheduleIdle' is `{}' on the
wire, which `json-parse-string' renders as a nil plist value, so arm
presence is tested with `plist-member' rather than `plist-get'."
  (unless (listp view)
    (agent-repl--log nil
                     "frontend-shutdown-schedule: MALFORMED view type=%s — expected a plist"
                     (type-of view))
    (error "agent-repl: malformed ShutdownScheduleView (not a plist)"))
  (let ((idle (plist-member view :idle))
        (draining (plist-member view :draining)))
    (when (and idle draining)
      (agent-repl--log nil
                       "frontend-shutdown-schedule: MALFORMED view sets BOTH oneof arms view=%S"
                       view)
      (error "agent-repl: ShutdownScheduleView sets both idle and draining"))
    (unless (or idle draining)
      (agent-repl--log nil
                       "frontend-shutdown-schedule: MALFORMED view sets NEITHER oneof arm view=%S"
                       view)
      (error "agent-repl: ShutdownScheduleView sets no state arm"))
    (let* ((body (and draining (plist-get view :draining)))
           (schedule-id (plist-get body :scheduleId)))
      (when (and draining (not (and (stringp schedule-id)
                                    (not (string-empty-p schedule-id)))))
        (agent-repl--log nil
                         "frontend-shutdown-schedule: MALFORMED draining arm carries no schedule_id view=%S"
                         view)
        (error "agent-repl: ShutdownScheduleDraining carries no schedule_id"))
      (let ((previous agent-repl--frontend-shutdown-schedule)
            (next (if idle
                      (list :state :idle)
                    (list :state :draining
                          :scheduleId schedule-id
                          :scheduledAtMs (plist-get body :scheduledAtMs)
                          :cause (plist-get body :cause)
                          :stopShims (and (plist-get body :stopShims) t)
                          :holds (plist-get body :holds)))))
        (setq agent-repl--frontend-shutdown-schedule next)
        (agent-repl--log nil
                         "frontend-shutdown-schedule: %s -> %s"
                         (agent-repl--frontend-shutdown-schedule-summary previous)
                         (agent-repl--frontend-shutdown-schedule-summary next))
        next))))

;;;; ---- Handler registration --------------------------------------------
;;
;; Loaded after `frontend-uds.el' (config.el load order / the test files),
;; so `agent-repl--uds-register-handler' is defined here.

;; A NEW LINK HOLDS NO DELIVERY.  Whatever the previous connection converged
;; on, this one has landed nothing yet, so the completeness verdict resets to
;; "partial" at link open rather than carrying a dead daemon's "complete"
;; across the outage.
(add-hook 'agent-repl-uds-connected-functions
          #'agent-repl--frontend-snapshot-invalidate)

(agent-repl--uds-register-handler "workspaceState"
                                  #'agent-repl--frontend-apply-workspace-state)
(agent-repl--uds-register-handler "snapshot"
                                  #'agent-repl--frontend-apply-snapshot)
(agent-repl--uds-register-handler "sessionView"
                                  #'agent-repl--frontend-apply-session-view)
(agent-repl--uds-register-handler "daemonView"
                                  #'agent-repl--frontend-apply-daemon-view)
(agent-repl--uds-register-handler "sessionInit"
                                  #'agent-repl--frontend-apply-session-init)
(agent-repl--uds-register-handler "shutdownSchedule"
                                  #'agent-repl--frontend-apply-shutdown-schedule)
;; The handler itself lives in `daemon.el', alongside the expected-restart
;; window it opens, but it is REGISTERED here: `daemon.el' loads before
;; `frontend-uds.el' and cannot call `agent-repl--uds-register-handler' at its
;; own load time.  Registering it here keeps every UDS handler registration in
;; the one module that runs after the dispatcher exists.
(agent-repl--uds-register-handler "restartPending"
                                  #'agent-repl--frontend-apply-restart-pending)

;;;; ---- Module init: registration only ---------------------------------
;;
;; WHY: daemon startup is intentionally lazy (daemon.el) and the first
;; session-open path owns build, launch, and readiness.  Dialing here ran
;; before that owner and misclassified every cold Emacs start as a daemon
;; outage.  Registration remains a load-time operation; the first session
;; open connects only after `agent-repl--ensure-frontend-daemon' has run.

(agent-repl--log
 nil
 "frontend-state init: handlers registered; UDS dial deferred to lazy daemon readiness")

(provide 'frontend-state)

;;; frontend-state.el ends here
