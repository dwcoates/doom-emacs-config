package sessioncontroller

import (
	"errors"
	"fmt"
	"sort"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
	"claude-repld/internal/tokenusage"
	"claude-repld/internal/tokenutilization"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// Pusher is the slice of the frontend server the session controller pushes to. Satisfied by
// *frontend.Server; an interface so the sink translation is unit-testable with
// a recording fake.
type Pusher interface {
	PushConversationDelta(*frontendv1.ConversationDelta)
	// PushAsyncBubbleDelta publishes one event's whole async effect: the
	// bubbles it opened and the updates it folded, in one fenced frame.
	PushAsyncBubbleDelta(*frontendv1.AsyncBubbleDelta)
	PushTypingDelta(*frontendv1.TypingDelta)
	PushTaskCatalog(*frontendv1.TaskCatalog)
	PushWorkspaceState(*frontendv1.WorkspaceState)
	PushSessionInitView(*frontendv1.SessionInitView)
	PushHeartbeatView(*frontendv1.HeartbeatView)
	PushQueueView(*frontendv1.QueueView)
	PushProgressView(*frontendv1.ProgressView)
}

// StateApplier is the slice of the SSM the session controller feeds lifecycle events to.
// Satisfied by *ssm.Manager.
type StateApplier interface {
	Apply(ev *corev1.Event) error
	// ApplyTurnBoundary is the ONE destination of a STREAM turn boundary. It
	// moves the durable turn ledger, derives turn liveness from it, and paints
	// the session-status axis from that SAME derivation, in one transaction.
	//
	// It replaced a pair — a ledger resolve followed by a general Apply — whose
	// two idempotency rules could disagree about whether a turn was in flight.
	// The color and the queue now read one value; see ssm.TurnLiveness.
	ApplyTurnBoundary(workspace, claimantSessionID, liveQueryInstanceID string, ev *corev1.Event) (ssm.TurnBoundary, error)
	// ResolveTurnClaimBridge persists cross-session correlation proof without
	// applying lifecycle state. This is the only route for TurnClaimBridge.
	ResolveTurnClaimBridge(workspace, claimantSessionID string, ev *corev1.Event) (replayed bool, err error)
	// ReconcileTurnHandshake validates/persists the shim's active-turn snapshot
	// before DaemonHello opens its standing store subscription. closed names the
	// PHANTOM claims it synthesized an end for — turns the returning shim says
	// do not exist — which is what the caller releases its prompt queue on.
	//
	// durablyEnded names turns the caller has PROVED already carry a terminal
	// event in the store. Those are never cut: the store's record outranks the
	// hello, and their own replayed boundary settles them COMPLETED.
	ReconcileTurnHandshake(workspace, claimantSessionID string, ids []string, legacyActive bool, durablyEnded []string) (before, after, closed []string, err error)
	// ActiveTurnIDs names every turn claim the session still holds OPEN in the
	// durable ledger. It is how a drain hold whose prompt was ACCEPTED but whose
	// TurnStarted has not been observed yet still names the turn it is waiting
	// on: process memory cannot answer that, and the ledger can.
	ActiveTurnIDs(workspace, claimantSessionID string) ([]string, error)
	// TurnClaimExists answers whether the ledger ever opened a claim under one
	// turn identity, open or closed. It is what an UNKNOWN-FATE submit is
	// reconciled against before the queue is allowed to redeliver it.
	TurnClaimExists(workspace, turnID string) (bool, error)
	// SynthesizeTurnClose ends every durable turn claim held by the session
	// WITHOUT a TurnEnded, for a live shim observation that contradicts it. It
	// is the turn-lifecycle half of ReconcileAlreadyComplete's status-axis
	// reconciliation: the Ack says no foreground turn exists, so the claim the
	// queue holds prompts behind must not outlive it.
	SynthesizeTurnClose(workspace, claimantSessionID, cause string) (closed []string, err error)
	// ReconcileTasks adopts an AUTHORITATIVE live-task set for the session's
	// workspace, so live_task_count becomes exactly len(liveTaskIDs). Fed from
	// data.BackgroundTasksChanged, the only event carrying the whole live set.
	ReconcileTasks(sessionID string, liveTaskIDs []string) error
	// ApplyBackfillState records the workspace's transcript-backfill outcome
	// on the SSM's backfill axis. A FAILED backfill compromises the route
	// and resolves the workspace blue: an incomplete history cannot be the
	// basis of a "ready" claim.
	ApplyBackfillState(workspace, state string) error
	// ApplySessionConnectivity records the current session-controller
	// generation's establishment lifecycle.
	ApplySessionConnectivity(workspace, sessionID, generationID string, state ssm.SessionConnectivity, causeKind string) error
	// ApplyRuntimeFault opens or closes one typed, component-scoped fault
	// window owned by the named controller generation.
	ApplyRuntimeFault(workspace, sessionID, generationID, component, faultType string, impact ssm.FaultImpact, open bool, causeKind string) error
	// MarkPromptAccepted appends the daemon-local `submitting` edge as the daemon
	// commits to submitting an immediately delivered prompt — BEFORE the shim
	// Acks it, so the status does not wait on a shim round-trip — and
	// synchronously publishes that state through PUBLISH. The durable
	// TurnStarted follows over the store stream.
	// ADMISSION says whose prompt it is, and is the ONE thing that changes
	// which resolved states the edge may be published over: the daemon's own
	// idle machinery is admitted over the merge phases that leave the session
	// unowned (ssm/mergepromptgate.go), and a user prompt is not.
	MarkPromptAccepted(workspace, sessionID, requestID string, admission ssm.PromptAdmission, publish func(*frontendv1.WorkspaceState)) error
	// MarkPromptDelivered advances that edge from `submitting` to `thinking`
	// when the shim ACKS the prompt, which is the first moment the agent
	// genuinely holds it. Reports whether it wrote the row.
	MarkPromptDelivered(workspace, sessionID, requestID string) (advanced bool, err error)
	// MarkPromptRejected withdraws that edge when the submit it was published
	// for then FAILS, and synchronously publishes the retraction. It reports
	// whether it wrote the closing row: false with a nil error means something
	// more authoritative already owns the axis and nothing was retracted.
	MarkPromptRejected(workspace, sessionID, requestID string, publish func(*frontendv1.WorkspaceState)) (retracted bool, err error)
	// ReconcileAlreadyComplete makes an ALREADY_COMPLETE interrupt Ack agree
	// with the session-status lifecycle before the progress footer may publish
	// "already finished". It closes a still-standing
	// `submitting`/`thinking`/`permission`
	// row owned by this session and preserves already-settled turn outcomes.
	// PUBLISH is the synchronous frontend ordering barrier. The reconciled
	// settled WorkspaceState must be offered before the progress resolver may
	// publish the mutually exclusive ALREADY_COMPLETE window.
	ReconcileAlreadyComplete(workspace, sessionID string, publish func(*frontendv1.WorkspaceState)) (closed bool, err error)
	// SettleTurnFromTerminalResult reconciles the SAME two halves from the
	// vendor's own `result` message, which is the turn's end rather than a
	// report of it. It retires this session's claims, paints the axis settled
	// and publishes the re-derived state, all attributed to the terminal
	// result. See terminalsettlement.go.
	SettleTurnFromTerminalResult(workspace, sessionID string, publish func(*frontendv1.WorkspaceState)) (settled bool, err error)
	// MarkTurnInterrupted records that a USER-COMMANDED stop was delivered to
	// the workspace's running turn, so that turn's own TurnEnded reports
	// `interrupted` instead of `done` or `vendor_blocked` (I1). Fed ONLY from
	// the frontend interrupt command path — the queue's interject sends the
	// same Interrupt as machinery and must paint no outcome.
	MarkTurnInterrupted(workspace string) error
	// ApplyClearing opens or closes the CLEARING axis. The daemon owns the
	// opening edge (nothing announces a clear as it begins), and the
	// first-class ContextCleared closes it.
	ApplyClearing(workspace string, clearing bool, reason string) error
	// ApplyCompacting opens or closes the COMPACTING axis, from the vendor's
	// own status ticker and the first-class ContextCompacted.
	ApplyCompacting(workspace string, compacting bool, reason string) error
	// NoteCompactionCompleted records that the workspace's conversation was
	// COMPACTED, so a daemon-initiated compaction is not run a second time
	// against a conversation nothing has been added to since. It is fed from
	// the first-class ContextCompacted alone — the compacting axis's other
	// closing edges also fire for a compaction that died. See
	// ssm/compactiongate.go.
	NoteCompactionCompleted(workspace string) error
	// CompactionGateOf reads what the log knows about the workspace's
	// compaction history: when a compaction last completed, and when the
	// conversation last received material a compaction would summarize. Its
	// Redundant method is the whole policy.
	CompactionGateOf(workspace string) (ssm.CompactionGate, error)
	// MergeLeaseHeld reports whether merge.Coordinator currently owns the
	// workspace's shim. While it does, the merge is the ONLY party allowed to
	// submit, and every conversation item the session produces is a merge's
	// rather than a user's.
	MergeLeaseHeld(workspace string) bool
	// ConversationSourceAt returns the PERSISTED provenance verdict for an item
	// produced in workspace at tsMs. It is read off the merge lease's durable
	// ledger rather than off the lease's current state, which is what makes a
	// resync of a finished merge replay CONVERSATION_SOURCE_MERGE instead of
	// rewriting the history as the user's.
	ConversationSourceAt(workspace string, tsMs int64) (frontendv1.ConversationSource, error)
	// ApplySessionRotated reconciles the session-status lifecycle across a VENDOR SESSION
	// UUID ROTATION: the turn in flight when the uuid changed can never report
	// its end under the retired identity, so a `thinking` row held for it has
	// nothing arriving to supersede it. Fed only from the shim handshake that
	// announces the new uuid.
	ApplySessionRotated(workspace, previous, next string) error
	// ApplyPermission opens or closes the workspace's PERMISSION row: the agent
	// asked the user a canUseTool question and is parked until it is answered.
	// PENDING is this workspace's pending-permission count folded to a boolean,
	// so the opening edge is the first pending request and the closing edge is
	// the count returning to zero — grant, deny and abandonment alike.
	ApplyPermission(workspace string, pending bool, reason string) error
	// Current resolves state for teardown reconciliation paths that need the
	// SSM's authoritative verdict without acquiring turn-admission ownership.
	Current(workspace string) (*frontendv1.WorkspaceState, bool, error)
	// AcquireHibernationLease atomically snapshots the workspace and excludes
	// every new prompt/turn start until release. The returned snapshot includes
	// durable turn claims, so a lifecycle boundary accepted immediately before
	// lease acquisition cannot be mistaken for a settled workspace.
	AcquireHibernationLease(workspace string) (state *frontendv1.WorkspaceState, found bool, release func(), err error)
	// AcquireControllerRegistration excludes hibernation from the beginning of
	// a controller generation's bring-up through its operational edge.
	AcquireControllerRegistration(workspace, sessionID, generationID string) (release func(), err error)
	// CloseStaleTurn closes a workspace's standing `thinking` when the daemon
	// has just STOPPED the shim that promised to report that turn's end. The
	// session-status lifecycle retires `thinking` on a `TurnEnded` and on nothing else, so a
	// stop landing mid-turn kills the only process that could ever produce
	// one and latches the axis forever.
	//
	// It reports whether it WROTE the closing row: false with a nil error is
	// the good outcome, meaning the shim's own end reached the log first and
	// closed the axis honestly. See turnstop.go, which is the only caller.
	CloseStaleTurn(workspace, sessionID, reason string, soleSessionController bool) (closed bool, err error)
	// CloseOrphanedTurn reconciles a workspace whose turn NOTHING CAN EVER END:
	// the caller has proved that no controller drives it and no process holds
	// its lock, so every claim standing in it — under ANY claimant — was opened
	// by a shim that is gone. Unlike CloseStaleTurn it therefore retires the
	// whole workspace's ledger rather than one session's, which is what makes it
	// converge instead of re-running against the same untouched claim forever.
	//
	// It reports whether it reconciled anything. See connectivitystate.go's
	// reconcileOrphanedTurn, which is the only caller and which owns the proof.
	CloseOrphanedTurn(workspace, sessionID, reason string) (closed bool, err error)
	// CloseOriginTurns ends the NAMED turns' claims because the ORIGIN that
	// submitted them reached its own terminal. It is the closing edge for the
	// turns the daemon opens on a machine's behalf — a keep-alive ping, a
	// workspace-create prompt, a merge run's resolution — none of which has
	// anybody watching for its end once the origin is done.
	//
	// It reports the identities it closed. A turn whose own end already retired
	// its claim is left exactly as it was and is not among them.
	CloseOriginTurns(workspace string, turnIDs []string, cause string) (closed []string, err error)
	// UnansweredInterruptAgeMs reports how long a stop the shim acked as
	// INTERRUPTED has gone without the terminal that ack owed, and whether such
	// a stop stands at all. It is the one piece of evidence a LIVE shim
	// produces against its own turn claim, and it is what narrows the
	// live-controller decline (unsubstantiatedturn.go).
	UnansweredInterruptAgeMs(workspace string) (ageMs int64, marked bool)
	// LastActivityMs is when anything last happened on the workspace. The state
	// log is already the activity record, so this needs no bookkeeping of its
	// own; ok=false means the workspace has no history at all, which is UNKNOWN
	// rather than idle.
	LastActivityMs(workspace string) (atMs int64, ok bool, err error)
}

// ProgressResolver is the slice of the progress-footer resolver (F1) the session controller
// feeds. Satisfied by *progress.Manager.
//
// The controller feeds it the SAME event stream it feeds the SSM, plus the two
// daemon-local facts no store event carries: how many permission prompts are
// waiting on the user, and how deep the held-prompt queue is. Its Apply takes
// the workspace explicitly because a progress view is workspace-keyed and this
// resolver, unlike the SSM, holds no session→workspace binding of its own.
//
// Apply takes the CANONICAL DAEMON session id alongside the workspace, and not
// because the resolver could not read one off the event: it could, and that is
// exactly the defect. Store events are filed under the vendor conversation
// uuid, so a view stamped from the event carried an identity the frontend's
// exact agent-session scope filter drops, and the footer never saw the token
// ticks. The controller is the authority on which daemon session drives the
// workspace, so it names it here.
type ProgressResolver interface {
	Apply(workspace, sessionID string, ev *corev1.Event) error
	SetCounts(workspace string, pendingPermissions, queueDepth int64)
	// NoteTurnAccepted returns the exact cleared interrupt view so the prompt
	// path can offer it synchronously before the active WorkspaceState. Nil is
	// reserved for the explicitly optional noopProgress implementation.
	NoteTurnAccepted(workspace, sessionID string) *frontendv1.ProgressView
	// NoteTurnRejected closes the clock NoteTurnAccepted opened when the submit
	// it was opened for never reached the shim. Fed only after the SSM confirms
	// it retracted the matching state edge, so a turn that started for another
	// reason in that window keeps its clock.
	NoteTurnRejected(workspace, sessionID string)
	// NoteInterrupt opens the interrupt window on the shim's ack of a
	// USER-COMMANDED stop (I1), carrying the ack's outcome verbatim. Fed ONLY
	// from the frontend interrupt command path, for the same reason
	// MarkTurnInterrupted is: an interject's stop is machinery, not a user
	// action, and opening a window for it would report a stop nobody asked
	// for.
	NoteInterrupt(workspace, sessionID string, outcome corev1.InterruptOutcome)
	// NoteTurnAccounting hands the resolver one SETTLED turn's reconciliation,
	// from which it resolves the footer's accounting cell. It is fed from the
	// single settlement path below, which is the only place that holds a
	// turn's resolved record, so the cell cannot be produced from a
	// half-settled turn.
	NoteTurnAccounting(workspace, sessionID string, accounting *frontendv1.TurnAccounting) error
}

// ClearCompactStore persists the newest CLEAR-OR-COMPACTION seq per
// conversation — the frontend REPLAY FLOOR. Satisfied by
// *server.RegistrySeqStore, the same registry adapter that persists
// last_seen_seq, because the floor mark is the same kind of fact: a
// per-conversation seq high-water that must outlive the daemon.
//
// It is a store rather than a field on the live session controller on purpose. The daemon
// re-Subscribes each shim from its high-water mark, so a clear or compaction
// observed before a restart is never re-delivered; an in-memory floor would
// quietly reset to "nothing seen" on every restart and hand the frontend back
// the entire conversation that clear or compaction discarded, which is the
// class of bug the floor exists to end.
//
// It deliberately does NOT reach into the shim-store. The store's API is
// contractually tiny — schema, seq, dedup, fan-out — and the daemon already
// sees every clear and every compaction as it flows, so tracking the newest one
// costs an existing registry write and asks the store for nothing.
type ClearCompactStore interface {
	// NewestClearOrCompactSeq returns the newest clear-or-compaction seq for the
	// session's conversation, or 0 when neither has ever been observed on it.
	NewestClearOrCompactSeq(sessionID string) uint64
	// SetNewestClearOrCompactSeq records seq as the newest clear or compaction.
	// Monotonic: an older seq never lowers a conversation's floor.
	SetNewestClearOrCompactSeq(sessionID string, seq uint64)
}

// PromptReceiptStore persists the DURABLE half of a prompt receipt: one row per
// prompt this daemon accepted and has not yet seen the conversation carry.
// Satisfied by *statedb.PromptReceipts.
//
// It is the only thing standing between a user and a silently lost prompt. A
// receipt lived only in daemon memory before this, so a prompt accepted and not
// yet durable when the daemon died left no trace anywhere — the shim-store had
// never received the turn, and the bubble the user had already seen died with
// the process.
// WiringApplier moves a workspace's wired axis — the generation-less
// connectivity projection that says whether ANYTHING is attached to a
// workspace. Satisfied by *ssm.Manager.
//
// It is an OPTIONAL facet of the StateApplier rather than a member of it, for
// the reason WorkspaceStateReader is: a focused harness supplies an applier
// that answers only what its subject exercises, and widening the required
// interface would make every such harness carry a method it never calls. The
// one production site that needs it (durablereplay.go) says out loud when the
// facet is absent rather than assuming it moved the axis.
type WiringApplier interface {
	ApplyWired(workspace string, wiring ssm.Wiring, reason string) error
}

// TerminalFailureCardStore persists a session's STANDING terminal failure card
// — one row per session whose bring-up is fenced on a verdict that cannot heal
// on its own. Satisfied by *statedb.TerminalFailureCards.
//
// It is what makes the fence's card outlive the instant it was pushed at. The
// card used to be a live push only, so a client that connected after the
// refusal replayed the conversation from durable history and found no account
// of why nothing drives it — the durable history is the vendor conversation,
// and a session that never came up wrote nothing into it.
type TerminalFailureCardStore interface {
	// Record writes the session's standing card, REPLACING any prior one, so a
	// fence re-established after a hard restart restates one claim rather than
	// stacking a second.
	Record(rec statedb.TerminalFailureCard) error
	// Standing reads the session's card, if one stands.
	Standing(sessionID string) (statedb.TerminalFailureCard, bool, error)
	// Withdraw deletes the session's card, reporting whether one stood. It is
	// the closing edge of the standing claim, run when the fence is cleared.
	Withdraw(sessionID string) (bool, error)
}

type PromptReceiptStore interface {
	// Record persists one accepted prompt. It runs BEFORE the receipt bubble is
	// pushed, so a receipt on a user's screen always implies a durable record.
	Record(r statedb.PromptReceipt) error
	// Retire discards a request's receipt, reporting whether one was
	// outstanding. Retiring an already-retired receipt is a no-op, never an
	// error: the retirement points are several and any may run second.
	Retire(requestID string) (bool, error)
	// RetireWorkspace discards every receipt for a workspace accepted at or
	// before throughMs — the context cut's sweep — reporting how many went.
	RetireWorkspace(workspace string, throughMs int64) (int, error)
	// Outstanding lists a workspace's un-retired receipts, oldest first.
	// A PENDING RESUMPTION IS NEVER AMONG THEM: this is the render path, and a
	// re-drive is not the user's prompt (turnresumption.go).
	Outstanding(workspace string) ([]statedb.PromptReceipt, error)

	// --- the interrupted-turn resumption (turnresumption.go) ---

	// RecordPendingResumption durably records a turn a teardown is about to
	// interrupt, so the successor daemon can re-drive it. It runs BEFORE the
	// interrupt is delivered.
	RecordPendingResumption(r statedb.PendingResumption) error
	// PendingResumptions lists what a workspace is still owed AND UNCLAIMED,
	// oldest interruption first. It is the LEVEL the re-drive is triggered off,
	// which is what makes the resumption survive a bounce mid-resumption.
	PendingResumptions(workspace string) ([]statedb.PendingResumption, error)
	// UndischargedResumptions lists every resumption row a workspace carries,
	// claimed or not. It is the PREEMPTION's reading: a user who moved on
	// abandons the turn whether or not a re-drive already claimed it.
	UndischargedResumptions(workspace string) ([]statedb.PendingResumption, error)
	// ClaimResumptionForDelivery takes one owed resumption for a re-drive about
	// to submit, reporting whether this caller got it. It is the fence that
	// makes one interrupted turn re-driven once (turnresumption.go).
	ClaimResumptionForDelivery(requestID string, atMs int64) (bool, error)
	// DischargeResumption discards one resumption, claimed or not, reporting
	// whether one was there. The re-drive's instruction reaching the vendor
	// conversation and the user preempting it both discharge through here.
	DischargeResumption(requestID string) (bool, error)
}

// TurnAccountingStore durably records the evidence required to compare a
// completed turn with another client. Every consumer receives one at
// construction, before it can accept any event.
type TurnAccountingStore interface {
	Record(sessionID string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error)
	List(sessionID string) ([]*frontendv1.TurnAccounting, error)
}

// HistoricalTokenUtilizationStore durably normalizes file-plane response
// usage that cannot prove an enclosing turn or stream timing. Its identity is
// the stable API message id within the agent-repl session.
type HistoricalTokenUtilizationStore interface {
	RecordHistorical(*frontendv1.TokenUtilization) (bool, error)
}

// noopProgress is the ProgressResolver a session controller built without one falls back
// to. It exists so the progress feed is OPTIONAL for a test harness that does
// not care about it, without every feed site growing a nil check.
type noopProgress struct{}

func (noopProgress) Apply(string, string, *corev1.Event) error                { return nil }
func (noopProgress) SetCounts(string, int64, int64)                           {}
func (noopProgress) NoteTurnAccepted(string, string) *frontendv1.ProgressView { return nil }
func (noopProgress) NoteTurnRejected(string, string)                          {}
func (noopProgress) NoteInterrupt(string, string, corev1.InterruptOutcome)    {}
func (noopProgress) NoteTurnAccounting(string, string, *frontendv1.TurnAccounting) error {
	return nil
}

// ringCap bounds the per-session retained event ring the daemon keeps for the
// live TaskCatalog rebuild and for resync replay. It is a bounded window: older
// history is served by the store-backed replay on the next Subscribe (the store
// is the durable record), so dropping the oldest here loses nothing durable.
const ringCap = 4096

// consumer is one session's translation of the merged shim event stream into
// frontend pushes and SSM state. It implements shimclient's StateSink,
// FrameSink and DegradedReporter for a single session bound to one workspace.
// All three sink methods run on the shimclient demux goroutine in strict
// arrival order; the ring mutex guards only the retained-events slice (touched
// by both the demux and a concurrent resync).
type consumer struct {
	workspace string
	sessionID string
	// generationID scopes every runtime fault to this exact controller
	// incarnation. A delayed edge from a retired consumer is rejected by the
	// SSM instead of repainting its replacement.
	generationID string
	push         Pusher
	ssm          StateApplier
	// prog is the progress-footer resolver, fed the same stream as the SSM.
	// Never nil (noopProgress stands in), so the feed sites stay unconditional.
	prog ProgressResolver
	// floors persists the newest clear-or-compaction seq (the replay floor).
	// Required: Config validation rejects a Manager built without one, so a
	// clear or a compaction can never be observed and silently forgotten.
	floors ClearCompactStore
	// receipts persists prompt receipts durably. Assigned after construction
	// (bindReceipts) rather than taken as yet another positional constructor
	// argument. Nil is a session controller built without one, and every use
	// site says so out loud rather than silently skipping the write.
	receipts PromptReceiptStore
	// onPushedConversation observes each translated ConversationDelta at the
	// moment it is pushed. Assigned only by the throwaway consumer a DURABLE
	// replay runs through, which needs to know which prompts the store's own
	// events just served so it does not serve a receipt for one of them twice
	// (durablereplay.go). Nil on every live consumer.
	onPushedConversation func(*frontendv1.ConversationDelta)
	logf                 func(string, ...any)
	// warnf is the WARN channel for records that accompany a regression the
	// user can see — degraded accounting, a failure card, a rejected event.
	// At info those are indistinguishable from routine progress and invisible
	// to a level filter. Reached through warn, never called directly.
	//
	// Assigned after construction for the same reason as onVendorSessionID
	// below: newConsumer's positional list is already long enough that one
	// more argument would be a hazard rather than a clarification. Unset it
	// falls back to logf, so a record is never lost — only its severity.
	warnf func(string, ...any)
	now   func() int64
	// onSessionStarted fires when a SessionStarted event arrives, letting the
	// controller adopt the vendor session uuid the start announced.
	onSessionStarted func(*corev1.SessionStarted)
	// onVendorSessionID reports the VENDOR session uuid observed on a
	// PERSISTENT store event's envelope. Assigned after construction (the
	// controller binds it before any event can flow) rather than taken as a
	// constructor argument, which is already long enough that one more
	// positional nil would be a hazard rather than a clarification.
	// PERSISTENT store event's envelope, so the durable record tracks the
	// conversation this session is actually filing under.
	//
	// The envelope's session_id IS that uuid for anything the store stamped a
	// seq onto — the shim reads it off the SDK message and the sidecar derives
	// it from `<uuid>.jsonl`, and the two must agree or the store's dedup could
	// not merge them. EPHEMERAL events (seq 0) carry the daemon's own s_ id
	// instead, which is why the seq is the discriminator rather than the
	// payload kind.
	//
	// WHY THE REGISTRY NEEDS THE LIVE STREAM AS A SOURCE. The uuid is what
	// binds a rotated conversation's events to a workspace, and what a shim
	// (re)handshake's announcement is COMPARED AGAINST to notice a rotation at
	// all. Learning it only from a discovered transcript leaves the record
	// empty for a session that has not been rehydrated, and an empty record has
	// nothing for the announcement to differ from.
	onVendorSessionID func(vendorSessionID string)
	// durableTurnEnds reads the STORE for the terminal events of the turn claims
	// a returning shim's hello contradicts. It is the only authority that can
	// tell a turn CUT by a daemon gap from one that FINISHED inside it, and it
	// is consulted before any claim is cut (durableturnevidence.go). Nil on a
	// consumer built without a durable history source, and the judgment site
	// says so out loud rather than silently assuming interruption.
	durableTurnEnds durableTurnEndProbe
	// onDegraded reports a shim-sourced DegradedState to the session controller, which is
	// what lets a bring-up still waiting on the handshake learn that the shim
	// has already given up. Assigned after construction, like the hook above.
	onDegraded func(*corev1.DegradedState)
	// onQueryTermination reports the typed lifecycle evidence before its paired
	// degraded wake-up. The bring-up gate retains it so an exact resume failure
	// can retain the SDK's reason and identity through the command boundary.
	onQueryTermination func(*frontendv1.QueryTerminationFailure)
	// resumeIdentity proves a resumed query's first observed vendor id before
	// any envelope identity can update the registry.
	resumeIdentity *resumeIdentityTracker
	// unexpectedQueryTerminationSurfaced deduplicates the ordered live pair:
	// QueryLifecycle.Terminated provides the immediate fault while the following
	// persistent DegradedState is the replay authority. A fresh replay consumer
	// starts false and therefore surfaces the persistent record exactly once.
	unexpectedQueryTerminationSurfaced bool
	// historicalTerminationPairs is the HISTORICAL mirror of the latch above,
	// keyed by the RETIRED query the replayed pair belongs to.
	//
	// It exists because the pair's two halves reach two different sinks — the
	// QueryLifecycle row through surfaceUnexpectedQueryTermination, the
	// DegradedState through Degraded — and both derive the SAME card identity
	// (degradedUUID("claude-shim-sdk")). The live path already collapses that to
	// one push through unexpectedQueryTerminationSurfaced; the withhold path had
	// no equivalent, so one replayed death pushed the card twice and recorded
	// "system failure … resolved=false" twice in the same microsecond.
	//
	// It is deliberately SEPARATE from the live latch, and keyed rather than a
	// bool, for the reason historicalquerydegradation_test.go pins: arming the
	// live latch on history would make it swallow a genuine live termination
	// arriving afterwards.
	historicalTerminationPairs map[string]struct{}
	// onTurn reports an observed turn boundary (true = TurnStarted, false =
	// TurnEnded). It drives the prompt queue's interception and drain (E4).
	// Called on the shim read-loop goroutine, so the handler must not block on
	// anything that needs that loop to make progress — notably an Ack-awaiting
	// send back to the same shim.
	//
	// atMs is the BOUNDARY'S OWN INSTANT — the event's produced_at_ms — not the
	// moment the handler runs. Anything the queue stamps from a boundary (the
	// keep-alive window's closing edge) has to agree with the timestamps the
	// vendor wrote on the transcript records that boundary bounds, and a clock
	// read taken at handling time is a different, later, unrelated instant.
	onTurn func(active bool, atMs int64)
	// onTurnEvent reports EVERY accepted turn boundary WITH THE TURN'S OWN ID,
	// where onTurn above reports only the active/idle EDGES the queue drains on.
	// Assigned after construction, like onVendorSessionID.
	//
	// The distinction is what makes a per-turn wait correlatable. A merge's
	// conflict-resolution turn has to be waited on SPECIFICALLY (mergeresolve.go):
	// the coordinator resumes the cherry-pick the moment that turn ends, so the
	// end of some OTHER turn — the user's interrupted one, whose TurnEnded can
	// still be in flight when the lease is taken — must never be mistaken for it.
	// Edges cannot express that; turn ids can.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onTurnEvent func(started bool, turnID string, outcome turnOutcome)
	// onTurnLiveness hands the controller the SSM's ONE derived turn liveness
	// the instant the boundary transaction that produced it committed. It is
	// what binds the controller's turn record to the turn's own id
	// (turnrecord.go).
	//
	// IT CARRIES THE VALUE, NOT A COPY OF ITS INPUTS. The same ssm.TurnLiveness
	// the workspace color was painted from is the one the prompt queue then acts
	// on, so "the color says green" and "the queue says a turn is in flight" are
	// not two computations that must agree — they are one value read twice.
	//
	// It is called ONLY when the derivation holds a live turn: a boundary that
	// binds or renames a hold may run ahead of the frontend publication (it can
	// only make the daemon hold MORE), while the one that RELEASES the last claim
	// rides the queue's own end edge (onTurn) instead.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onTurnLiveness func(l ssm.TurnLiveness)
	// onTurnEnded reports the instant an accepted turn END landed, so the
	// controller can persist it as the keep-alive policy's measuring point.
	// Assigned after construction, like onVendorSessionID.
	//
	// It is a SEPARATE callback from onTurn's idle edge because it fires on
	// every accepted end rather than only on the active-to-idle transition: the
	// policy measures from the newest end, and an end that produced no edge
	// (one turn ending while another is already running) is still the newest.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onTurnEnded func(atMs int64)
	// onTurnStarted reports an accepted turn START with the boundary's OWN
	// instant, so the controller can re-stamp a keep-alive window's lower bound
	// onto the clock that stamps conversation items (keepalivesubmit.go).
	//
	// IT CARRIES BOTH THE NAME AND THE INSTANT because the re-stamp needs both
	// and neither existing start-side hook has the pair: onTurn has the instant
	// but no name and fires only on the idle-to-active edge, and onTurnClaims
	// has the names but no instant. Assigned after construction, like
	// onVendorSessionID.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onTurnStarted func(turnID string, atMs int64)
	// onTurnResultCost reports what one turn's terminal result measured, named
	// by the turn the accounting reducer just attributed that result to.
	//
	// IT MATTERS FOR EXACTLY TWO KINDS OF TURN, and ONE reduction serves both so
	// they can never disagree about one result. A keep-alive ping is a dozen
	// tokens of prompt, so a ping that paid for the whole conversation is the
	// cache's absence MEASURED rather than predicted, and that measurement is
	// what puts the session to sleep (keepalivecold.go). A DAEMON-INITIATED
	// COMPACTION that paid the same way is a pure cost defect and trips the
	// cold-read alarm (compactioncold.go). Every other turn's cost is a cost
	// report and is rendered as one by the progress footer.
	//
	// THE CONVERSATION'S SIZE IS NOT ONE OF THEM, and used to be. A result's
	// usage sums every model call the turn made, so it measures the turn's work
	// and not the context's occupancy; onMainAgentContextSize carries the size
	// instead, off evidence that can answer that question.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onTurnResultCost func(cost turnResultCost)
	// onMainAgentContextSize reports how big the standing conversation is, as of
	// one LIVE main-agent response's own Messages-API usage.
	//
	// It fires per response rather than per turn because that is the granularity
	// the question has an answer at: one request's input buckets name exactly the
	// prompt that request presented, and the latest of them is the occupancy
	// right now. The warm-compaction size floor is the only reader
	// (warmcompact.go, contextsize.go).
	//
	// HISTORICAL RECORDS ARE NOT DELIVERED. A replayed transcript row describes
	// how big the conversation was at some past instant, and handing that to a
	// floor judging the present would compact against history.
	//
	// Called on the shim read-loop goroutine, with the same non-blocking
	// obligation onTurn carries.
	onMainAgentContextSize func(record *frontendv1.TokenUtilization)
	// compactedWaiter reports that a compaction COMPLETED — the compacting
	// axis closing, which is the only first-class report the vendor gives.
	// A compact-first revival waits on it before it will accept prompts.
	// Assigned after construction, like onVendorSessionID.
	compactedWaiter cutWaiter
	// clearedWaiter is compactedWaiter's counterpart for the OTHER context
	// cut: the clearing axis closing, which is the only first-class report
	// that a `/clear` actually discarded the conversation. A clear revival
	// waits on it on exactly the terms a compact-first revival waits on the
	// compaction, and it is a SEPARATE field so a cut of one kind can never
	// release a revival that asked for the other.
	clearedWaiter cutWaiter
	// keepAliveWindows is the durable ledger the keep-alive exclusion reads
	// (keepaliveexclude.go). Nil is the exclusion OFF.
	keepAliveWindows KeepAliveWindowLedger
	// turnSuperseders closes the claims of turns a rewind discarded
	// (sessionrewound.go).
	turnSuperseders TurnClaimSuperseder
	// onBackfill reports a never-blue backfill transition (F2), once per
	// distinct state. The controller persists it and re-pushes the SessionView.
	onBackfill func(state string)
	// onSystemInit reports the session metadata the SDK announces at init and
	// RE-ANNOUNCES on every submit, which is what makes it a live signal rather
	// than a start-of-life one. The controller persists the model off it, so a
	// respawn stops replaying whatever model was requested at create.
	// seq is the file-plane seq the SystemInit was carried on, which is what
	// orders the model it announces against a shim confirmation
	// (registry.ModelObservation).
	onSystemInit func(si *datav1.SystemInit, seq uint64)
	// onSessionEnded reports that the session's shim reported it is over (F4).
	// Nothing marked a record terminal on shim death before this, so
	// RENDER_STATE_DEAD and death_reason sat on two disconnected axes: the
	// workspace went dead with no account of why, and the one death reason the
	// registry documented was never written by anything.
	onSessionEnded func()

	// skills correlates a launched skill's SKILL.md body back to the Skill
	// call that launched it (skillbody.go). Locked internally, so it sits
	// outside the ring's mutex rather than under it.
	skills *skillCorrelator
	// bubbles is this consumer's detached-work apparatus: every async bubble
	// the session has opened, and the routing that folds each work kind's
	// output into the right one (asyncbubbles.go).
	bubbles *asyncBubbleStore
	// turns is the single lifecycle authority gate. Every turn boundary passes
	// it before the queue, SSM, or progress resolver can mutate.
	turns turnLifecycle

	mu   sync.Mutex
	ring []*corev1.Event
	// systemInit is the last SDK system:init snapshot seen on this session's
	// stream (a data.v1 SystemInit inside a vendor event). It backs the daemon's
	// HTTP /status and /commands routes now that the L2 translator that used to
	// cache it is gone. Nil until the first init lands (honest empty).
	systemInit *datav1.SystemInit
	// streamSeq is the HIGHEST file-plane seq this consumer has taken in.
	//
	// It is the instant a shim confirmation is true AS OF: everything already
	// consumed is by construction older than an answer the shim gave after it,
	// so a `SystemInit` from at-or-below the mark cannot outrank that
	// confirmation (registry.ModelObservation). Read and written under c.mu.
	streamSeq uint64
	// permItems retains the LATEST permission ConversationItem per request_id,
	// in first-seen order, so a resync replays each permission's current
	// resolution (S8). The retained ring holds core.v1.Events; a permission item
	// is a daemon-composed frontend item with no store seq, so it lives here
	// beside the ring and is replayed on every resync.
	permItems map[string]*frontendv1.ConversationItem
	permOrder []string
	// failItems retains the LATEST system-failure ConversationItem per uuid,
	// in first-seen order, on the same footing as permItems and for the same
	// reason (F4). A WINDOW-shaped failure is re-sent under its opening uuid
	// with resolved_at_ms set, so the retained copy is the SETTLED card and a
	// resync replays that rather than re-opening the alarm.
	failItems map[string]*frontendv1.ConversationItem
	failOrder []string
	// withheldCards are the failItems uuids the WITHHOLD arms put up — cards
	// about a RETIRED query, replayed off durable history. They are settled at
	// the bring-up gate by resolveWithheldDegradations, because an unresolved
	// degradation for a query that is already dead misrepresents a session whose
	// LIVE query is up and driveable. Guarded by mu, beside failItems.
	withheldCards map[string]struct{}
	// echoes are the prompt receipts this daemon has pushed and the durable
	// transcript has not yet claimed, OLDEST FIRST. See pushUserEcho.
	echoes []*promptEcho
	// cmdItems retains the session-command invocation items this daemon has
	// pushed, in first-seen order, on the same footing as permItems and
	// failItems and for the same reason (sessioncommand.go). They are the ONLY
	// account a frontend gets of an invocation — the command earns no prompt
	// bubble and the CLI's own transcript bookkeeping for it is withheld as
	// machinery — so a resync that could not replay them would leave the feed
	// silent about a command the user ran.
	cmdItems map[string]*frontendv1.ConversationItem
	cmdOrder []string
	// backfill is the last never-blue state reported for this session (F2).
	// In-memory latch: it is what keeps a long transcript from writing the
	// registry record once per line.
	backfill string
	// accounting owns the ordered query, usage, response, and turn evidence.
	accounting           *turnAccountingReducer
	accountingStore      TurnAccountingStore
	historicalUsageStore HistoricalTokenUtilizationStore
	// pendingTerminal names each turn whose ACCOUNTING STAMP is still
	// outstanding, under the terminal result that turn produced. It is no longer
	// a parking lot: the result it records was published on arrival, and what is
	// outstanding is the token ledger alone (terminalsettlement.go). Reached
	// ONLY through noteTerminalResult, heldTerminalResult,
	// takeHeldTerminalResult and heldTerminalTurnIDs, all of which take mu: the
	// stream writes it from the consumer's own event path while teardowns and
	// interrupt acks read and drain it from their goroutines, and an unguarded
	// range over a map another goroutine is writing kills the process outright.
	pendingTerminal map[string]*corev1.Event
	// terminalSeqByTurn is the PERMANENT receipt of which stream coordinate
	// carried each turn's terminal result. It outlives pendingTerminal's
	// discharge because a LATE correction revises a stamp long after it
	// settled, and the revision has to reach the same seq the first settlement
	// indexed. Guarded by mu, beside pendingTerminal.
	terminalSeqByTurn map[string]uint64
	// settledStamps names every turn whose accounting stamp has been published
	// at least once. It is what makes a second settlement authority — the
	// shim's `TurnEnded` arriving after the enrichment already settled the
	// stamp — the quiet no-op it should be, without weakening the loud
	// retired-turn report that a genuine collision still takes. Guarded by mu.
	settledStamps map[string]struct{}
	// announcedTurnEnds names every turn whose END this consumer has already
	// announced to the daemon's own turn machinery — the queue's drain, the
	// keep-alive claim, the idle clock. The vendor's terminal result and the
	// shim's later `TurnEnded` are two statements of ONE end, so announcing
	// both would fire that machinery twice for one turn; the second edge then
	// lands after the NEXT turn has taken the record and retires a claim it
	// does not own. A turnLatch, so the test-and-set is one step.
	announcedTurnEnds *turnLatch
	// stoppedTurns names every turn a TEARDOWN STOP is in flight for. A turn
	// interrupted so the daemon can take its shim away was DISPLACED, not
	// finished, and the successor daemon owes it (turnresumption.go); settling
	// it from the vendor's result would record it as a turn that ended of its
	// own accord. The stop's own path closes it instead, so the ledger keeps
	// saying the shim was stopped over a live turn.
	stoppedTurns           *turnLatch
	replayedAccounting     map[string]*frontendv1.TurnAccounting
	replayedResponses      map[string]*frontendv1.TokenUtilization
	completedTerminalBySeq map[uint64]*frontendv1.TurnAccounting
	completedResponses     map[string]*frontendv1.TokenUtilization
	responseDiagnostics    *diagnosticDeduper
	// onTerminalAccountingPersisted republishes the SessionView from the
	// durable aggregate only after the terminal conversation delta is visible.
	onTerminalAccountingPersisted func()
	// onHistoricalUsagePersisted republishes the SessionView only when a
	// file-plane record inserted a normalized response row.
	onHistoricalUsagePersisted func()
}

// warn emits through the consumer's WARN channel. An unwired channel still
// records the event through logf rather than dropping it, because losing the
// record entirely would be strictly worse than logging it at the wrong level.
func (c *consumer) warn(format string, args ...any) {
	if c.warnf != nil {
		c.warnf(format, args...)
		return
	}
	c.logf(format, args...)
}

func newConsumer(workspace, sessionID string, push Pusher, applier StateApplier, prog ProgressResolver, floors ClearCompactStore, accountingStore TurnAccountingStore, logf func(string, ...any), onSessionStarted func(*corev1.SessionStarted), onTurn func(active bool, atMs int64), onBackfill func(state string), onSystemInit func(si *datav1.SystemInit, seq uint64), onSessionEnded func()) *consumer {
	if accountingStore == nil {
		panic("session-controller: newConsumer needs a TurnAccountingStore")
	}
	if logf == nil {
		logf = func(string, ...any) {}
	}
	if prog == nil {
		prog = noopProgress{}
	}
	return &consumer{
		workspace:              workspace,
		sessionID:              sessionID,
		push:                   push,
		ssm:                    applier,
		prog:                   prog,
		floors:                 floors,
		accountingStore:        accountingStore,
		logf:                   logf,
		now:                    func() int64 { return time.Now().UnixMilli() },
		onSessionStarted:       onSessionStarted,
		onTurn:                 onTurn,
		onBackfill:             onBackfill,
		onSystemInit:           onSystemInit,
		onSessionEnded:         onSessionEnded,
		skills:                 newSkillCorrelator(),
		bubbles:                newAsyncBubbleStore(workspace, dlog.Tag(dlog.Logf(logf), "session", sessionID, "ws", workspace)),
		turns:                  newTurnLifecycle(applier, workspace, sessionID),
		accounting:             newTurnAccountingReducer(dlog.Tag(dlog.Logf(logf), "session", sessionID, "ws", workspace)),
		resumeIdentity:         newResumeIdentityTracker(),
		pendingTerminal:        map[string]*corev1.Event{},
		terminalSeqByTurn:      map[string]uint64{},
		settledStamps:          map[string]struct{}{},
		announcedTurnEnds:      newTurnLatch(),
		stoppedTurns:           newTurnLatch(),
		replayedAccounting:     map[string]*frontendv1.TurnAccounting{},
		replayedResponses:      map[string]*frontendv1.TokenUtilization{},
		completedTerminalBySeq: map[uint64]*frontendv1.TurnAccounting{},
		completedResponses:     map[string]*frontendv1.TokenUtilization{},
		responseDiagnostics:    newDiagnosticDeduper(responseDiagnosticDedupeCapacity, responseDiagnosticRepeatLimit),
	}
}

// retain appends ev to the bounded ring, dropping the oldest past ringCap.
// observeVendorSessionID reports a PERSISTENT event's envelope session id as
// the conversation's vendor uuid. Seq 0 (ephemeral) carries the daemon's own
// id and is skipped; see onVendorSessionID.
func (c *consumer) observeVendorSessionID(ev *corev1.Event) {
	if c.onVendorSessionID == nil || ev.GetSeq() == 0 {
		return
	}
	if sid := ev.GetSessionId(); sid != "" {
		c.onVendorSessionID(sid)
	}
}

func (c *consumer) retain(ev *corev1.Event) {
	c.mu.Lock()
	c.ring = append(c.ring, ev)
	if len(c.ring) > ringCap {
		// Drop the oldest quarter in one shift so the trim is amortized O(1).
		drop := ringCap / 4
		c.ring = append(c.ring[:0], c.ring[drop:]...)
	}
	c.mu.Unlock()
}

// purgeRetained drops every retained event and reports how many were dropped
// plus the seq ceiling that went with them.
//
// IT IS THE ROTATION'S PURGE, and it exists because the ring is a window onto
// ONE store seq space. When the vendor retires a session uuid the conversation
// starts a fresh space at 1, and every seq the ring holds becomes a number with
// no meaning in the space this session now serves. Three things read those
// numbers and all three were wrong until they were dropped:
//
//   - newestRetainedSeq, the CEILING an honest client mark is checked against
//     (Manager.lastSeenSeq). Left standing it reports the retired space's high
//     water, so a post-rotation mark in the new space reads as ordinary rather
//     than as the retired-space mark it is.
//   - ringFloor, which becomes the re-pull's stop_at. Left standing it bounds a
//     re-pull of the NEW space at a retired-space seq — the observed
//     `stop_at=1122` against a space that had reached 12.
//   - resync itself, which would replay the retired conversation's items to a
//     frontend that just discarded them.
//
// After this the ring is empty until new-space events arrive, which is exactly
// the state the cursor and the replay floor are reset to in the same act. The
// events are not lost: they are in the store under the retired key, and nothing
// in the new space refers to them.
func (c *consumer) purgeRetained() (dropped int, ceiling uint64) {
	c.mu.Lock()
	defer c.mu.Unlock()
	dropped = len(c.ring)
	for i := len(c.ring) - 1; i >= 0; i-- {
		if seq := c.ring[i].GetSeq(); seq > 0 {
			ceiling = seq
			break
		}
	}
	c.ring = nil
	// The correlator names records in the RETIRED conversation's uuid space,
	// which nothing in the new one refers to. Left standing it could only
	// answer a new-space parent uuid by coincidence.
	c.skills.reset()
	return dropped, ceiling
}

// snapshotRing returns a shallow copy of the retained events for catalog
// rebuilds and resync, taken under the lock so a concurrent retain cannot race
// the read.
func (c *consumer) snapshotRing() []*corev1.Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*corev1.Event, len(c.ring))
	copy(out, c.ring)
	return out
}

// newestRetainedSeq is the highest store seq the retained ring holds, or 0 when
// it holds no PERSISTENT event at all.
//
// It exists so a client's replay mark can be checked against the seq space this
// conversation is actually in (Manager.replayFloor). The durable last_seen_seq
// is the same fact for a session whose events came through shimclient, but the
// ring is the one place that is true even before the durable mark is written,
// so the two are read together and the higher wins.
//
// The scan runs BACKWARDS and stops at the first positive seq: the ring is in
// arrival order and seq is monotonic across PERSISTENT events, while ephemeral
// ones (seq 0, the daemon's own id on the envelope) carry no position at all.
func (c *consumer) newestRetainedSeq() uint64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	for i := len(c.ring) - 1; i >= 0; i-- {
		if seq := c.ring[i].GetSeq(); seq > 0 {
			return seq
		}
	}
	return 0
}

// noteStreamSeq advances the consumed-seq high-water mark. Monotone: an
// out-of-order or re-delivered event never lowers it, because the mark states
// what has been SEEN rather than what arrived last.
func (c *consumer) noteStreamSeq(seq uint64) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if seq > c.streamSeq {
		c.streamSeq = seq
	}
}

// consumedStreamSeq is the highest file-plane seq this consumer has taken in.
func (c *consumer) consumedStreamSeq() uint64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.streamSeq
}

// latestSystemInit returns the last SDK system:init snapshot seen on this
// session's stream, or nil before the first init lands.
func (c *consumer) latestSystemInit() *datav1.SystemInit {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.systemInit
}

// systemInitFromVendor decodes a vendor event's Any into its SystemInit arm, or
// nil when the Any is not a ClaudeStreamMessage carrying a system:init (every
// vendor event shares the same Any type URL; the inner oneof is the
// discriminator).
func systemInitFromVendor(a *anypb.Any) *datav1.SystemInit {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return csm.GetSystemInit()
}

// statusFromVendor decodes a vendor event's Any into its StatusMessage arm.
// ok is false for every other payload, which is what keeps "no status here"
// distinct from "a status whose text is empty" — the vendor's empty status is
// its null, and it is the edge that CLOSES the compaction window.
func statusFromVendor(a *anypb.Any) (status string, ok bool) {
	if a == nil {
		return "", false
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return "", false
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return "", false
	}
	sm := csm.GetStatus()
	if sm == nil {
		return "", false
	}
	return sm.GetStatus(), true
}

// commandsChangedFromVendor decodes a vendor event's Any into its
// CommandsChanged arm, or nil when it is anything else.
func commandsChangedFromVendor(a *anypb.Any) *datav1.CommandsChanged {
	if a == nil {
		return nil
	}
	msg, err := a.UnmarshalNew()
	if err != nil {
		return nil
	}
	csm, ok := msg.(*datav1.ClaudeStreamMessage)
	if !ok {
		return nil
	}
	return csm.GetCommandsChanged()
}

// applyCommandsChanged folds a system/commands_changed push into the retained
// SystemInit and returns the refreshed snapshot to re-publish, or nil when
// there is nothing to fold it into yet.
//
// The SDK's contract for this message is REPLACE, not merge: the payload is
// the complete current command list after a mid-session change (a skill
// discovered as the agent moves into a subdirectory, a plugin installed). So
// the retained slash-command list is overwritten wholesale.
//
// The retained SystemInit is CLONED rather than mutated: the very same pointer
// was already handed to frontends in an earlier SessionInitView, and editing
// it in place would retroactively rewrite a snapshot they are still holding.
//
// A push arriving before any init has nothing to update. That is not an error
// worth failing on — the init that follows carries the current list anyway —
// but it IS worth saying out loud rather than silently discarding a command
// list the user's menu is waiting for.
func (c *consumer) applyCommandsChanged(cc *datav1.CommandsChanged) *datav1.SystemInit {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.systemInit == nil {
		c.logf("session-controller: commands_changed before system:init session=%s commands=%d; dropped (the next init carries the current list)",
			c.sessionID, len(cc.GetCommands()))
		return nil
	}
	names := make([]string, 0, len(cc.GetCommands()))
	for _, cmd := range cc.GetCommands() {
		names = append(names, cmd.GetName())
	}
	refreshed, _ := proto.Clone(c.systemInit).(*datav1.SystemInit)
	refreshed.SlashCommands = names
	c.systemInit = refreshed
	return refreshed
}

// Apply feeds a lifecycle event to the SSM and refreshes the TaskCatalog on
// task-lifecycle transitions (design step 1). It also fires onSessionStarted so
// the session controller sees the start. A lifecycle rejection or SSM
// apply error is loud-logged and aborts this delivery, so shimclient cannot
// advance last_seen_seq past state the daemon did not accept.
//
// ACCOUNTING IS BOOKKEEPING AND MAY NOT GATE THIS DELIVERY. A failure inside
// c.accounting.observe is, by construction, about the TOKEN-UTILIZATION
// LEDGER alone — a malformed usage observation, an unattributed response, a
// response whose embedded session id cannot be reconciled — never about
// whether this event is a valid turn-lifecycle or SSM boundary. Before this,
// every one of those failures returned from Apply exactly like a genuine
// protocol violation (a seq regression, a rejected handshake), which
// shimclient's dispatch treats identically: the shim connection is torn down
// as terminal, the session controller exits, and if this happens during
// bring-up the workspace never establishes at all — a bookkeeping
// disagreement about one turn's token accounting denying the user their
// entire conversation. See degradeAccountingObservation.
func (c *consumer) Apply(ev *corev1.Event) error {
	// The reducer consumes every durable lifecycle and observation fact before
	// derived state can publish a terminal result. Its failure DEGRADES this
	// turn's accounting — loudly, below — and does not stop the boundary
	// itself from reaching the turn ledger, the SSM, or the frontend.
	if err := c.accounting.observe(ev, c.sessionID); err != nil {
		if fatal := c.degradeAccountingObservation(ev, err); fatal != nil {
			return fmt.Errorf("session-controller: observe turn accounting: %w", fatal)
		}
	}
	applyState := true
	var turnResult *turnResolution
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnClaimBridge:
		err := fmt.Errorf("session-controller: TurnClaimBridge must use ApplyTurnClaimBridge, never lifecycle Apply")
		c.logf("session-controller: turn bridge decision=reject_misroute session=%s seq=%d turn_id=%q request_id=%q error=%v",
			c.sessionID, ev.GetSeq(), ev.GetTurnClaimBridge().GetTurnId(), ev.GetRequestId(), err)
		return err
	case *corev1.Event_TurnStarted, *corev1.Event_TurnEnded:
		res, turnErr := c.turns.resolve(ev, c.accounting.queryID)
		c.logf("session-controller: turn lifecycle plane=%s kind=%s session=%s seq=%d turn_id=%q request_id=%q dedup_key=%q active_before=%s active_after=%s decision=%s apply=%v notify=%v replayed=%v error=%v",
			ev.GetPlane().String(), stateKind(ev), c.sessionID, ev.GetSeq(),
			res.correlation, ev.GetRequestId(), ev.GetDedupKey(), res.before,
			res.after, res.decision, res.apply, res.notify, res.replayed, turnErr)
		if turnErr != nil {
			if errors.Is(turnErr, ssm.ErrTurnStartConflict) {
				// THE REFUSAL IS ABOUT ONE TURN, so it is declared as such to
				// the demux instead of ending the session. Two live starts
				// contend for a single identity: that turn is unusable and says
				// so loudly, while the conversation around it is untouched. The
				// alternative killed the controller, and — because the offending
				// start is durable in the vendor stream — killed it again on
				// every subsequent resume.
				c.warn("session-controller: turn lifecycle CONFLICT SCOPED TO THE TURN session=%s seq=%d turn_id=%q request_id=%q: %v — the session keeps running and the durable mark advances past this start rather than replaying it into the same refusal forever",
					c.sessionID, ev.GetSeq(), res.correlation, ev.GetRequestId(), turnErr)
				return fmt.Errorf("%w: session-controller: turn lifecycle rejected: %v",
					shimclient.ErrTurnScopedRejection, turnErr)
			}
			return fmt.Errorf("session-controller: turn lifecycle rejected: %w", turnErr)
		}
		turnResult = &res
		// THE BOUNDARY ALREADY REACHED THE SSM. ApplyTurnBoundary moved the
		// ledger and painted the session-status axis from one derivation in one
		// transaction, so there is no second state apply to make here and no
		// `apply` flag deciding whether the color gets to move. What is left is
		// binding the controller's own record to the very value the color was
		// painted from.
		applyState = res.apply
		// THE NAME BINDS FIRST, on the durable ledger's own acceptance and
		// before this delivery touches the frontend or the progress footer. See
		// onTurnLiveness for why the release direction does not.
		if res.liveness.Active() && c.onTurnLiveness != nil {
			c.onTurnLiveness(res.liveness)
		}
	}
	if turnResult == nil && applyState {
		if err := c.ssm.Apply(ev); err != nil {
			c.logf("session-controller: ssm apply failed session=%s seq=%d kind=%s: %v",
				c.sessionID, ev.GetSeq(), stateKind(ev), err)
			return fmt.Errorf("session-controller: ssm apply failed: %w", err)
		}
	}
	// Nothing user-visible or replay-retained mutates before both the durable
	// turn ledger and the SSM accept the event.
	c.retain(ev)
	c.observeVendorSessionID(ev)
	if ss := ev.GetSessionStarted(); ss != nil && c.onSessionStarted != nil {
		c.onSessionStarted(ss)
	}
	// A turn END is announced ONCE. The vendor's terminal result already made
	// this announcement for a turn it settled (terminalsettlement.go), and the
	// shim's `TurnEnded` for that same turn is a replay of it.
	announce := turnResult != nil && turnResult.notify
	if announce && ev.GetTurnEnded() != nil {
		announce = c.claimTurnEndAnnouncement(ev.GetTurnEnded().GetTurnId())
	}
	if announce && c.onTurn != nil {
		c.onTurn(turnResult.active, c.boundaryInstant(ev))
	}
	// THE KEEP-ALIVE POLICY'S ONE INPUT, persisted on the accepted turn END and
	// nowhere else. It is written HERE, after the durable ledger and the SSM
	// have both accepted the boundary, so the instant the policy measures from
	// is one the rest of the daemon also agrees a turn ended at.
	//
	// A turn ending STARTS the clock; nothing arms a timer. See
	// registry.Record.LastTurnEndMs for why the timestamp rather than a timer
	// is what survives a laptop sleep and a daemon bounce.
	if te := ev.GetTurnEnded(); te != nil && c.onTurnEnded != nil && announce {
		c.onTurnEnded(c.boundaryInstant(ev))
	}
	// THE KEEP-ALIVE WINDOW'S LOWER BOUND, taken from the START boundary for the
	// exact reason its upper bound is taken from the END boundary (queue.go):
	// both are compared against timestamps the VENDOR wrote, so both must come
	// from the vendor's clock or the comparison is decided by clock agreement
	// rather than by evidence. Placed with the end hook, after the durable
	// ledger and the SSM have accepted the boundary, so the instant re-stamped
	// is one the rest of the daemon also agrees the turn began at.
	if ts := ev.GetTurnStarted(); ts != nil && ts.GetTurnId() != "" && c.onTurnStarted != nil {
		c.onTurnStarted(ts.GetTurnId(), c.boundaryInstant(ev))
	}
	// EVERY accepted boundary, edge or not: a turn that starts while another is
	// still ending produces no edge, and a wait correlated on that turn's id
	// would otherwise never see it begin.
	if turnResult != nil && c.onTurnEvent != nil {
		_, started := ev.GetPayload().(*corev1.Event_TurnStarted)
		// The end's own verdict rides with it. A waiter that only learned a turn
		// ended could not tell a completed merge action from an errored one.
		var outcome turnOutcome
		if ended := ev.GetTurnEnded(); ended != nil {
			outcome = turnOutcome{isError: ended.GetIsError(), stopReason: ended.GetStopReason()}
		}
		c.onTurnEvent(started, turnResult.correlation, outcome)
	}
	if applyState {
		c.applyProgress(ev)
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted, *corev1.Event_TaskEnded:
		// The SAME lifecycle event that moves the task catalog opens and settles
		// the detachment's bubble. One event, both surfaces, so a task the
		// footer shows as running and a bubble that says it settled cannot come
		// from two different readings of the stream.
		c.pushAsync(c.observeAsyncTask(ev), ev)
		catalog := frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.fence(), c.snapshotRing(), c.logf)
		c.logf("session-controller: task catalog push session=%s ws=%s seq=%d event=%s tasks=%d",
			c.sessionID, c.workspace, ev.GetSeq(), stateKind(ev), len(catalog.GetTasks()))
		c.push.PushTaskCatalog(catalog)
	case *corev1.Event_TaskProgress:
		// TaskProgress can fire hundreds of times per second, but TaskCatalog has
		// no progress fields and BuildTaskCatalog deliberately ignores it. Do not
		// log or broadcast this hot no-change path.
	case *corev1.Event_SessionEnded:
		// The SAME event the SSM resolves to RENDER_STATE_DEAD also records
		// WHY, so the color and its account cannot disagree. Before this the
		// SSM went dead and the record stayed silent.
		if c.onSessionEnded != nil {
			c.onSessionEnded()
		}
	}
	if ended := ev.GetTurnEnded(); ended != nil && ended.GetTurnId() != "" {
		// A settlement failure DEGRADES this turn's accounting only. The turn
		// itself already reached the durable ledger, the SSM, and the frontend
		// above — establishment and the conversation are unaffected. See the
		// comment on Apply's own accounting.observe call for why this may not
		// be allowed to abort the delivery.
		if err := c.settleTurnAccounting(ended.GetTurnId()); err != nil {
			c.warn("session-controller: ACCOUNTING DEGRADED session=%s turn_id=%s seq=%d — terminal settlement FAILED and this turn's accounting is unavailable, but the turn boundary itself was already accepted and the session establishes normally: %v",
				c.sessionID, ended.GetTurnId(), ev.GetSeq(), err)
		} else if c.onTerminalAccountingPersisted != nil {
			c.onTerminalAccountingPersisted()
			c.logf("session-controller: terminal accounting SessionView republished session=%s turn_id=%s", c.sessionID, ended.GetTurnId())
		}
	}
	return nil
}

// degradeAccountingObservation records an accounting-observation failure
// LOUDLY, through the same structured helper the old fatal path used, and
// reports whether the caller may still continue.
//
// EVERY FAILURE EXCEPT ONE DEGRADES: the boundary this event carries still
// reaches the turn ledger, the SSM, and the frontend, because a
// token-utilization bookkeeping failure is never a reason to withhold a
// user's conversation or deny a workspace's establishment. See the comment
// on Apply. The one exception —
// ErrAccountingQueryIdentityContradiction — is not bookkeeping: it is the
// live query() invocation disagreeing with itself about its own identity,
// which stays exactly as fatal as it always was and is returned unchanged
// for the caller to propagate.
//
// SEVERITY, AND ONLY SEVERITY, SPLITS ON EPOCH. A rejection whose row was
// written by a RETIRED query is replayed history: the store serves it again on
// every bring-up, so leaving it at warn meant one poisoned durable row alarming
// forever about a fault surfaced once and already fixed at its producer. Such a
// rejection records in full at info under a decision field. A LIVE rejection is
// byte-identical to what it always was.
func (c *consumer) degradeAccountingObservation(ev *corev1.Event, cause error) error {
	// ONE classification for the whole rejection, made here and threaded down, so
	// the three records this path emits cannot disagree about the row's epoch.
	historical := rejectionIsHistorical(c.accounting, ev)
	c.logRejectedAccountingObservation(ev, cause, historical)
	if errors.Is(cause, ErrAccountingQueryIdentityContradiction) {
		return cause
	}
	if historical {
		// THE DEGRADATION ITSELF IS UNCHANGED — the evidence is still unavailable,
		// the event is still applied, nothing is swallowed. Only the severity of
		// the record moves, for the same reason its two companion records moved:
		// a durable row poisoned once by a since-fixed producer would otherwise
		// re-alarm on every boot forever, about a failure already surfaced.
		c.logf("session-controller: ACCOUNTING DEGRADED session=%s seq=%d kind=%s historical=true decision=%s — this event's token-utilization evidence is unavailable, but the event itself is still applied and the session establishes normally: %v",
			c.sessionID, ev.GetSeq(), stateKind(ev), historicalRejectionDecision, cause)
		return nil
	}
	c.warn("session-controller: ACCOUNTING DEGRADED session=%s seq=%d kind=%s — this event's token-utilization evidence is unavailable, but the event itself is still applied and the session establishes normally: %v",
		c.sessionID, ev.GetSeq(), stateKind(ev), cause)
	return nil
}

// noteTerminalResult records the terminal result a turn produced, so its
// accounting stamp can be attributed to the emission it belongs to and so the
// release authorities can name the turns whose stamps are still outstanding.
//
// IT WITHHOLDS NOTHING. The event it records is published by the caller in the
// same breath; this map used to be a parking lot and is now a receipt. See
// pendingTerminal for why it is only ever reached under mu.
func (c *consumer) noteTerminalResult(turnID string, ev *corev1.Event) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.pendingTerminal[turnID] = ev
	c.terminalSeqByTurn[turnID] = ev.GetSeq()
}

// noteTurnStopInFlight records that a teardown is about to interrupt this turn
// so the daemon can stop its shim. It is called BEFORE the interrupt, for the
// same reason the owed-resumption row is written before it: the result the
// interrupt provokes can be back before the call that sent it returns.
func (c *consumer) noteTurnStopInFlight(turnID string) {
	c.stoppedTurns.mark(turnID)
}

// turnStopInFlight reports whether a teardown stop is outstanding for a turn.
func (c *consumer) turnStopInFlight(turnID string) bool {
	return c.stoppedTurns.marked(turnID)
}

// claimTurnEndAnnouncement records that this turn's end is being announced and
// reports whether THIS caller is the one making the announcement. The second
// authority for the same turn gets false and announces nothing.
func (c *consumer) claimTurnEndAnnouncement(turnID string) bool {
	if turnID == "" {
		// An END NOBODY ATTRIBUTED cannot be matched against a prior claim, so
		// it announces rather than be silently dropped by a latch it can never
		// be a member of.
		return true
	}
	return c.announcedTurnEnds.claim(turnID)
}

// stampAlreadySettled reports whether this turn's accounting stamp has already
// been published once.
func (c *consumer) stampAlreadySettled(turnID string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	_, settled := c.settledStamps[turnID]
	return settled
}

// heldTerminalResult reports the terminal result parked for a turn without
// discharging the hold. Nil means nothing is held for that turn.
func (c *consumer) heldTerminalResult(turnID string) *corev1.Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.pendingTerminal[turnID]
}

// takeHeldTerminalResult discharges a turn's hold and hands back what it held,
// in one step so two settlements of the same turn cannot both publish it. Nil
// means nothing was held.
func (c *consumer) takeHeldTerminalResult(turnID string) *corev1.Event {
	c.mu.Lock()
	defer c.mu.Unlock()
	ev := c.pendingTerminal[turnID]
	delete(c.pendingTerminal, turnID)
	return ev
}

// heldTerminalTurnIDs reports every turn whose terminal result this consumer
// still holds, in unspecified order.
//
// A turn appears here only between the arrival of its result and the settlement
// of its accounting, so the set is exactly "the turns this consumer owes the
// frontend an answer for and has no end boundary from".
func (c *consumer) heldTerminalTurnIDs() []string {
	c.mu.Lock()
	defer c.mu.Unlock()
	turnIDs := make([]string, 0, len(c.pendingTerminal))
	for turnID := range c.pendingTerminal {
		turnIDs = append(turnIDs, turnID)
	}
	return turnIDs
}

// settleTurnAccounting resolves, persists, and RELEASES one turn's accounting.
//
// IT IS THE SINGLE RELEASE PATH FOR A RETAINED TERMINAL RESULT, and that is the
// whole point. A terminal result is parked in pendingTerminal until its turn's
// end is known; the only stream event that supplies that end is `TurnEnded`.
// When the daemon closes a claim ITSELF — SynthesizeTurnClose, which writes
// end_seq=0 because no event produced the close — no `TurnEnded` ever arrives,
// and a result with no route to this function is stranded permanently: the user
// never sees the turn's answer, hibernation is refused every 30 seconds, and
// daemon restarts are refused.
//
// Every authority on "this turn ended" settles through here, so a close cannot
// happen without the release that belongs to it:
//
//   - a stream `TurnEnded` settles the turn it closes;
//   - a phantom close settles the claims SynthesizeTurnClose reports
//     (phantomturn.go); and
//   - a teardown settles everything the consumer still holds, before the
//     session controller context is cancelled (turnstop.go,
//     releaseHeldTerminalResults).
//
// The teardown's axis close (settleTurnAfterStop) is deliberately NOT one of
// them and cannot be: it runs after the eviction and the cancel, with neither
// the session controller nor its consumer in reach.
//
// A TURN CAN BE NAMED BY TWO OF THEM AT ONCE. Each authority snapshots the held
// set and then settles it, and nothing serializes one snapshot against the
// other's discharge: a synthesized close and a teardown observed the same turn
// held, and the second to run reached a reducer entry the first had already
// retired. That second settlement is served from the durable store by
// serveRetiredTurnAccounting rather than recomputed — the recompute would be
// evidence-free, and it used to kill the daemon outright.
func (c *consumer) settleTurnAccounting(turnID string) error {
	// THE SECOND AUTHORITY IS NOW THE ORDINARY CASE, not a collision. A turn's
	// stamp settles as soon as its corrections are on file
	// (terminalsettlement.go), and the shim's `TurnEnded` arrives afterwards
	// naming the same turn on every healthy turn there is. Answering it through
	// serveRetiredTurnAccounting would file a WARN about a settled turn once per
	// turn, which is noise that would bury the real collisions that path exists
	// to report. The already-settled answer is stated once, at info.
	if c.stampAlreadySettled(turnID) {
		c.logf("session-controller: turn accounting stamp ALREADY SETTLED session=%s turn_id=%s decision=skip_second_settlement — this turn's stamp settled when its corrections came in, and this later authority names the same turn rather than a different outcome",
			c.sessionID, turnID)
		return nil
	}
	if !c.accounting.hasTurn(turnID) {
		return c.serveRetiredTurnAccounting(turnID)
	}
	accounting := c.accounting.resolveTurn(turnID, c.now())
	accounting, err := c.accountingStore.Record(c.sessionID, accounting)
	if err != nil {
		c.logf("session-controller: terminal accounting persistence FAILED session=%s turn_id=%s: %v", c.sessionID, turnID, err)
		persisted, found, lookupErr := c.persistedTurnAccounting(turnID)
		if lookupErr != nil {
			c.logf("session-controller: terminal accounting persistence FAILED session=%s turn_id=%s and the already-persisted settlement could NOT be read either: %v", c.sessionID, turnID, lookupErr)
			return fmt.Errorf("session-controller: persist terminal accounting: %w", err)
		}
		if !found {
			return fmt.Errorf("session-controller: persist terminal accounting: %w", err)
		}
		// A SETTLEMENT ALREADY EXISTS FOR THIS TURN, so the failure above is a
		// disagreement about a turn the store has already accounted — not the
		// absence of an account. The persisted row was written by the
		// generation that actually OBSERVED the turn's evidence, which is
		// strictly more evidence than this attempt could have: a replay of a
		// retired query's stream admits none of that generation's responses,
		// usage boundaries, or vendor result (see
		// turnAccountingReducer.liveEvidenceFor), so the record it recomputes
		// is evidence-free by construction and can never match.
		//
		// Serving the persisted row is therefore the correct degradation: the
		// user sees the accounting that was actually measured instead of the
		// "INVALID ACCOUNTING"/"INCOMPLETE ACCOUNTING" a withheld settlement
		// renders as. The failure itself stays LOUD — logged immediately above
		// and again here — because a divergence between a live recompute and a
		// persisted settlement is still an invariant violation worth the record
		// even when the user is served correctly.
		c.logf("session-controller: terminal accounting SERVED FROM PERSISTED SETTLEMENT session=%s turn_id=%s — the recomputed record could not be persisted (see the failure above) and the already-persisted settlement for this turn is authoritative, so the turn's accounting is served from it rather than withheld",
			c.sessionID, turnID)
		accounting = persisted
	}
	if err := c.accounting.commitResolved(turnID); err != nil {
		// The precheck above already answered this question, so reaching here
		// means the reducer changed underneath a settlement that owns it. Report
		// it rather than absorb it: the record was persisted and the turn must
		// still be published, but the ledger and the settlement disagreeing is an
		// invariant violation in its own right.
		c.warn("session-controller: ACCOUNTING LEDGER RETIRED UNDER SETTLEMENT session=%s ws=%s turn_id=%s decision=publish_persisted_record — the turn's accounting record persisted, but its reducer entry was already retired when the commit ran: %v",
			c.sessionID, c.workspace, turnID, err)
	}
	c.publishTurnAccountingStamp(turnID, accounting)
	return nil
}

// serveRetiredTurnAccounting answers a settlement naming a turn the reducer
// holds no unsettled entry for.
//
// THE REDUCER IS NOT THE AUTHORITY HERE, the store is. A retired entry means
// this turn's accounting was already resolved and persisted — by an earlier
// release authority in this process, or by the generation that actually
// observed the turn before a replay carried its rows past a fresh reducer.
// Recomputing would produce an evidence-free record, so the persisted
// settlement is served instead, on exactly the terms settleTurnAccounting
// serves one on a divergent-replay persistence failure.
//
// THE CONDITION STAYS LOUD. Every branch records through the consumer's WARN
// channel with the session, workspace, turn, the held result's seq, and the
// branch decision, because a second settlement for one turn is a collision
// worth the record even when the user is served correctly.
func (c *consumer) serveRetiredTurnAccounting(turnID string) error {
	held := c.heldTerminalResult(turnID)
	seq := held.GetSeq()
	persisted, found, err := c.persistedTurnAccounting(turnID)
	if err != nil {
		c.warn("session-controller: ACCOUNTING SETTLEMENT NAMES RETIRED TURN session=%s ws=%s turn_id=%s seq=%d decision=persisted_lookup_failed — this turn's reducer entry was already retired by an earlier settlement and the persisted settlement could NOT be read either: %v",
			c.sessionID, c.workspace, turnID, seq, err)
		return err
	}
	if !found {
		c.warn("session-controller: ACCOUNTING SETTLEMENT NAMES RETIRED TURN session=%s ws=%s turn_id=%s seq=%d decision=no_persisted_settlement — this turn has neither a reducer entry nor a persisted settlement, so its accounting is unavailable and any retained terminal result stays unpublished",
			c.sessionID, c.workspace, turnID, seq)
		return fmt.Errorf("%w %q", ErrAccountingCommitUnknownTurn, turnID)
	}
	c.warn("session-controller: ACCOUNTING SETTLEMENT NAMES RETIRED TURN session=%s ws=%s turn_id=%s seq=%d decision=served_persisted_settlement held_terminal=%v — an earlier release authority already settled and retired this turn, so its already-persisted settlement is authoritative and is served rather than recomputed",
		c.sessionID, c.workspace, turnID, seq, held != nil)
	c.publishTurnAccountingStamp(turnID, persisted)
	return nil
}

// publishTurnAccountingStamp discharges a turn's outstanding stamp and publishes
// the settled accounting record.
//
// IT PUBLISHES AN ACCOUNTING STAMP AND NOTHING ELSE. It used to also push the
// turn's terminal result, which is what made the conversation wait on the token
// ledger; the result is published on arrival now (terminalsettlement.go) and
// this feeds the footer cell and the indexes a resync replays response stamps
// from. Discharge and publication remain one step so two settlements naming the
// same turn cannot both claim to be the first.
func (c *consumer) publishTurnAccountingStamp(turnID string, accounting *frontendv1.TurnAccounting) {
	// THE FOOTER'S ACCOUNTING CELL IS RESOLVED FROM THIS RECORD, and it is fed
	// FIRST: a turn whose stamp was already discharged still settled, and its
	// accounting is still the newest the footer has. A failure to feed it is
	// reported, never swallowed — a cell left one turn stale is a figure the
	// user reads as current.
	if err := c.prog.NoteTurnAccounting(c.workspace, c.sessionID, accounting); err != nil {
		c.warn("session-controller: turn accounting cell NOT RESOLVED session=%s ws=%s turn_id=%s — the footer keeps the previous turn's cell: %v",
			c.sessionID, c.workspace, turnID, err)
	}
	c.takeHeldTerminalResult(turnID)
	c.mu.Lock()
	c.settledStamps[turnID] = struct{}{}
	// THE RESPONSE-STAMP INDEXES ARE FED ON EVERY SETTLEMENT, including a LATE
	// REVISION, so a resync replaying this conversation renders the vendor's
	// final figures rather than the ones that happened to be on file when the
	// turn first settled. A turn that produced no terminal result of its own —
	// an interrupted turn settled by a synthesized close — has no seq to index
	// and only feeds the footer above.
	if seq, produced := c.terminalSeqByTurn[turnID]; produced {
		c.completedTerminalBySeq[seq] = accounting
	}
	for _, response := range accounting.GetResponses() {
		c.completedResponses[response.GetApiMessageId()] = response
	}
	c.mu.Unlock()
}

// persistedTurnAccounting reports the settlement the store already holds for
// one turn of this session, and whether it holds one at all.
//
// It reads through List rather than a dedicated point lookup because List is
// already the store's answer to "what settlements exist for this session", and
// this is a failure path taken once per unpersistable turn: a second query
// shape would be one more thing that could disagree with the row Record
// compares against.
func (c *consumer) persistedTurnAccounting(turnID string) (*frontendv1.TurnAccounting, bool, error) {
	accountings, err := c.accountingStore.List(c.sessionID)
	if err != nil {
		return nil, false, fmt.Errorf("session-controller: read persisted turn accounting: %w", err)
	}
	for _, accounting := range accountings {
		if accounting.GetTurnId() == turnID {
			return accounting, true, nil
		}
	}
	return nil, false, nil
}

// ReleaseSynthesizedTurnClose settles every turn whose claim the daemon closed
// without a `TurnEnded`, so a synthesized close carries the same release a real
// end does. Failures are reported per turn and never abort the others: each
// retained result is independent, and one unpersistable accounting record must
// not strand the rest.
func (c *consumer) ReleaseSynthesizedTurnClose(turnIDs []string, cause string) {
	for _, turnID := range turnIDs {
		if turnID == "" {
			continue
		}
		retained := c.heldTerminalResult(turnID) != nil
		if err := c.settleTurnAccounting(turnID); err != nil {
			c.logf("session-controller: synthesized turn close could NOT settle accounting session=%s turn_id=%s cause=%s retained_terminal=%v: %v — the turn's result stays unpublished and this is the record of why",
				c.sessionID, turnID, cause, retained, err)
			continue
		}
		if retained {
			c.logf("session-controller: synthesized turn close RELEASED a retained terminal result session=%s turn_id=%s cause=%s — no TurnEnded will ever arrive for this turn, so the close is what publishes its result",
				c.sessionID, turnID, cause)
		}
	}
}

// ApplyTurnClaimBridge is the sole consumer route for non-lifecycle rotation
// proof. It touches the durable claim ledger and the private accounting
// correlation reducer: no retain, SSM Apply, lifecycle edge, onTurn, progress,
// task catalog, or frontend push occurs on this path.
func (c *consumer) ApplyTurnClaimBridge(ev *corev1.Event) error {
	replayed, err := c.ssm.ResolveTurnClaimBridge(c.workspace, c.sessionID, ev)
	// ONE record per bridge, carrying the consumer's own outcome. The SSM owns
	// the refusal itself and has already logged it with its full context; this
	// line reports what the CONSUMER did about it, which is a different fact.
	// It used to be emitted here AND again inside the dead-claim branch, so a
	// single refusal produced three records across two layers.
	outcome := "correlation_retained=false session_survives=true"
	if err == nil {
		outcome = "correlation_retained=true session_survives=true"
	} else if errors.Is(err, ssm.ErrTurnBridgeDeadClaim) {
		// The claim is closed, so no durable row is written — but the accounting
		// correlation IS kept, or the response usage that follows this bridge
		// would have no root turn to name and would be fatal.
		outcome = "correlation_retained=true session_survives=true durable_row=none"
	} else {
		outcome = "correlation_retained=false session_survives=false"
	}
	c.logf("session-controller: turn bridge plane=%s session=%s seq=%d turn_id=%q previous_session=%q request_id=%q decision=%s replayed=%v %s error=%v",
		ev.GetPlane().String(), c.sessionID, ev.GetSeq(),
		ev.GetTurnClaimBridge().GetTurnId(),
		ev.GetTurnClaimBridge().GetPreviousSessionId(), ev.GetRequestId(),
		turnBridgeDecision(err), replayed, outcome, err)
	if err != nil {
		// The dead-claim refusal is the one bridge failure the transport must
		// SURVIVE. The ledger already refused it (nothing was written), and the
		// turn it names ended before this bridge arrived, so there is no live
		// correlation left to protect by severing the link. Escalating it did
		// protect nothing and cost everything: the shim replays from
		// last_seen_seq, so a bridge that kills the session is redelivered on
		// every reattach and kills it again, forever.
		//
		// Returning nil here lets last_seen_seq advance PAST the dead bridge,
		// which is what ends the loop. It is not a swallow: the refusal is
		// recorded twice above — once by the SSM as decision=refuse_dead_claim,
		// once on the line below — and the bridge is still refused. Only the
		// session's life is spared.
		if errors.Is(err, ssm.ErrTurnBridgeDeadClaim) {
			// THE DURABLE REFUSAL AND THE IN-MEMORY CORRELATION ARE TWO DIFFERENT
			// CONCERNS, and conflating them is what made a survivable refusal fatal
			// one event later.
			//
			// The ledger is right to refuse: its row for a closed claim is final,
			// and a bridge arriving after the close is proof about a retired epoch.
			// But the accounting reducer's correlation is not a durable record — it
			// is the ONLY thing that lets the response usage FOLLOWING this bridge
			// name its root turn (see observeTurnClaimBridge). Skipping it left
			// activeTurnID unset, so the very next event raised
			// unattributedResponseUsageError with reason=no_active_turn, which IS
			// terminal. The session survived the bridge and died on its successor.
			//
			// Correlating a retired turn is safe: the ids that follow belong to that
			// turn, and the next live TurnStarted replaces the correlation outright.
			c.accounting.observeTurnClaimBridge(ev)
			return nil
		}
		return fmt.Errorf("session-controller: turn bridge rejected: %w", err)
	}
	c.accounting.observeTurnClaimBridge(ev)
	return nil
}

func turnBridgeDecision(err error) string {
	switch {
	case err == nil:
		return "accept_durable_bridge_without_lifecycle_edge"
	case errors.Is(err, ssm.ErrTurnBridgeDeadClaim):
		return "refuse_dead_claim_bridge_session_survives"
	default:
		return "reject_durable_bridge"
	}
}

// Consume translates a data/ephemeral event into a frontend push (design step
// 1): complete vendor messages become a ConversationDelta stamped with
// through_seq; ContentDelta and HeartbeatProgress become ephemeral TypingDelta
// relays. A vendor payload that cannot be translated is a loud error, never a
// silent drop.
func (c *consumer) Consume(ev *corev1.Event) error {
	// THE HIGH-WATER MARK, advanced before anything can be decided from it and
	// unconditionally: an event this consumer refuses further down was still
	// PRODUCED, and a confirmation taken after it is still newer than it.
	c.noteStreamSeq(ev.GetSeq())
	identityMismatch, identityAdopted, identityErr := c.resumeIdentity.observe(ev)
	if identityAdopted != nil {
		// A ROTATION THIS DAEMON ASKED FOR. Logged at the same volume the fatal
		// case is: the query is now filing under a conversation other than the
		// one it was asked to resume, and that substitution must be readable
		// from the log even though it is the `/clear` working as intended.
		c.logf("session-controller: RESUME IDENTITY ADOPTED session=%s query_instance_id=%s requested_vendor_session_id=%s adopted_vendor_session_id=%s seq=%d outcome=clear_rotation_adopted — a /clear this daemon dispatched discharged the resume commitment, so the rotated conversation is this query's identity from here",
			c.sessionID, identityAdopted.queryInstanceID, identityAdopted.requestedVendorSessionID, identityAdopted.adoptedVendorSessionID, ev.GetSeq())
	}
	if identityErr != nil {
		c.logf("session-controller: query identity observation REJECTED before mutation session=%s seq=%d error=%v", c.sessionID, ev.GetSeq(), identityErr)
		return fmt.Errorf("session-controller: observe query identity before frame mutation: %w", identityErr)
	}
	if identityMismatch != nil {
		mismatchErr := newResumeIdentityMismatchError(c.sessionID, identityMismatch)
		c.logf("session-controller: RESUME IDENTITY MISMATCH session=%s query_instance_id=%s requested_vendor_session_id=%s observed_vendor_session_id=%s seq=%d outcome=fatal_invalid_controller",
			c.sessionID, identityMismatch.queryInstanceID, identityMismatch.requestedVendorSessionID, identityMismatch.observedVendorSessionID, ev.GetSeq())
		c.pushFailure("resume-identity-"+identityMismatch.queryInstanceID, errclass.Command(nil, mismatchErr))
		return fmt.Errorf("session-controller: resumed query identity mismatch before frame mutation: %w", mismatchErr)
	}
	historicalQueryID, historicalQueryLifecycle := c.accounting.HistoricalQueryLifecycle(ev)
	// ACCOUNTING IS BOOKKEEPING AND MAY NOT GATE CONVERSATION DELIVERY EITHER.
	// See the comment on consumer.Apply: a token-utilization ledger failure is
	// never a reason to withhold this event's conversation content from the
	// user, so it degrades this event's accounting, loudly, rather than
	// aborting the frame translation below.
	if err := c.accounting.observe(ev, c.sessionID); err != nil {
		if fatal := c.degradeAccountingObservation(ev, err); fatal != nil {
			return fmt.Errorf("session-controller: observe turn accounting before frame mutation: %w", fatal)
		}
	}
	// THE TERMINAL RESULT'S COST, reported against the SAME turn the accounting
	// reducer just attributed the result to. Placed immediately after that
	// attribution — and before anything is pushed — so the durable ledger and
	// this report can never name different turns for one result.
	//
	// Only the DATA plane can carry a result, which is why this is here and not
	// in Apply: Apply's events are lifecycle boundaries, and asking them for a
	// vendor result would be asking a question no lifecycle event can answer.
	if result := resultFromVendor(ev.GetVendor()); result != nil && c.onTurnResultCost != nil {
		if usage := result.GetUsage(); usage != nil {
			// THE RESULT'S OWN USAGE, never an assistant record's. A compaction's
			// synthetic assistant message carries a zero usage by construction;
			// the real figures ride the terminal result, which is what this
			// reduces and what every consumer of the hook is judged on.
			// A COUNTER THE VENDOR REPORTED NEGATIVE IS SURFACED, NOT JUDGED.
			// The canonical shape is unsigned, so converting one would hand the
			// hibernation policy and the cold-compaction tripwire a turn costing
			// nearly 2^64 tokens. The report is dropped loudly instead — the
			// same degradation the accounting ledger takes above, for the same
			// reason: bookkeeping may not gate conversation delivery.
			cost, err := newTurnResultCost(c.accounting.activeTurn(), usage)
			if err != nil {
				c.warn("session-controller: turn result cost REJECTED session=%s seq=%d turn_id=%s error=%v — the vendor reported a negative token counter, so no cold-ping, cold-compaction, or conversation-size verdict is taken from this result", c.sessionID, ev.GetSeq(), c.accounting.activeTurn(), err)
			} else {
				c.onTurnResultCost(cost)
			}
		}
	}
	if historicalQueryLifecycle {
		c.logf("session-controller: historical query lifecycle ACCEPTED without accounting rebind session=%s seq=%d historical_query_instance_id=%q live_query_instance_id=%q lifecycle=%T decision=retain_history_keep_live_handshake_authority",
			c.sessionID, ev.GetSeq(), historicalQueryID, c.accounting.queryID, ev.GetQueryLifecycle().GetEvent())
	}
	terminationFailure, err := frontend.FailureCardFromQueryTermination(c.sessionID, ev.GetQueryLifecycle(), ev.GetQueryLifecycle().GetObservedAtMs())
	if err != nil {
		c.warn("session-controller: typed query termination REJECTED before mutation session=%s seq=%d query_instance_id=%q vendor_session_id=%q vendor_identity_unavailable=%v observed_at_ms=%d error=%v", c.sessionID, ev.GetSeq(), ev.GetQueryLifecycle().GetQueryInstanceId(), ev.GetQueryLifecycle().GetTerminated().GetVendorSessionId(), ev.GetQueryLifecycle().GetTerminated().GetVendorSessionIdentityUnavailable() != nil, ev.GetQueryLifecycle().GetObservedAtMs(), err)
		return fmt.Errorf("session-controller: translate typed query termination before frame mutation: %w", err)
	}
	observation, utilizationErr := tokenUtilizationObservationFromEvent(ev, c.sessionID, c.accounting.isKnownVendorSession)
	if utilizationErr != nil {
		historical := rejectionIsHistorical(c.accounting, ev)
		c.logRejectedTokenUtilization(ev, utilizationErr, historical)
		if historical {
			// Replayed row: same demotion as the rejection record it accompanies,
			// so one row's three records never split across two severities. The
			// degradation is unchanged — the observation is still discarded below.
			c.logf("session-controller: token utilization translation DEGRADED session=%s seq=%d kind=%s historical=true decision=%s — invalid accounting was rejected before accounting mutation, but conversation delivery continues: %v",
				c.sessionID, ev.GetSeq(), stateKind(ev), historicalRejectionDecision, utilizationErr)
		} else {
			c.warn("session-controller: token utilization translation DEGRADED session=%s seq=%d kind=%s — invalid accounting was rejected before accounting mutation, but conversation delivery continues: %v",
				c.sessionID, ev.GetSeq(), stateKind(ev), utilizationErr)
		}
		observation = nil
	}
	var utilization *frontendv1.TokenUtilization
	historicalInserted := false
	if observation != nil {
		utilization = observation.record
	}
	if observation != nil && observation.historical {
		if c.historicalUsageStore == nil {
			return fmt.Errorf("session-controller: historical token utilization store is not wired (session=%s seq=%d api_message_id=%s)", c.sessionID, ev.GetSeq(), utilization.GetApiMessageId())
		}
		inserted, err := c.historicalUsageStore.RecordHistorical(utilization)
		if err != nil {
			return fmt.Errorf("session-controller: persist historical token utilization before frame mutation (session=%s seq=%d api_message_id=%s): %w", c.sessionID, ev.GetSeq(), utilization.GetApiMessageId(), err)
		}
		historicalInserted = inserted
		usage := utilization.GetUsage()
		c.logf("session-controller: historical token utilization persisted session=%s seq=%d api_message_id=%s inserted=%t input_tokens=%d output_tokens=%d cache_read_input_tokens=%d cache_creation_input_tokens=%d root_turn_id=absent response_timing=absent", c.sessionID, ev.GetSeq(), utilization.GetApiMessageId(), inserted, usage.GetInputTokens(), usage.GetOutputTokens(), usage.GetCacheReadInputTokens(), usage.GetCacheCreationInputTokens())
	}
	// HOW BIG THE CONVERSATION IS RIGHT NOW, taken from the same record the
	// ledger just accepted rather than measured again from the raw event: one
	// reduction, one identity, one set of invariants already enforced above.
	//
	// LIVE ONLY. The historical arm above persists replayed rows describing a
	// past instant, and the warm-compaction floor judges the present.
	if observation != nil && !observation.historical && c.onMainAgentContextSize != nil {
		c.onMainAgentContextSize(utilization)
	}
	c.retain(ev)
	c.observeVendorSessionID(ev)
	if terminationFailure != nil {
		c.surfaceUnexpectedQueryTermination(ev, terminationFailure, historicalQueryLifecycle)
	}
	if utilization != nil && utilization.GetUsage().GetUnmodeledUsage() != nil && len(utilization.GetUsage().GetUnmodeledUsage().GetFields()) > 0 {
		decision, err := c.responseDiagnostics.observe(utilization.GetApiMessageId(), utilization.GetUsage().GetUnmodeledUsage())
		if err != nil {
			c.logf("session-controller: API usage diagnostic REJECTED session=%s api_message_id=%s error=%v", c.sessionID, utilization.GetApiMessageId(), err)
			return err
		}
		if decision.Emit {
			kind := "first"
			if decision.Summary {
				kind = "repeat-summary"
			}
			c.logf("session-controller: API usage contains unmodeled fields session=%s api_message_id=%s payload_fingerprint=%s diagnostic_kind=%s repeat_count=%d repeat_limit=%d field_count=%d", c.sessionID, utilization.GetApiMessageId(), decision.Fingerprint, kind, decision.RepeatCount, responseDiagnosticRepeatLimit, len(utilization.GetUsage().GetUnmodeledUsage().GetFields()))
		}
	}
	c.applyProgress(ev)
	c.observeBackfill(ev)
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_ContentDelta:
		if td := frontend.TypingDeltaFromContentDelta(c.workspace, c.sessionID, p.ContentDelta); td != nil {
			c.push.PushTypingDelta(td)
		}
	case *corev1.Event_HeartbeatProgress:
		// E4: relayed as HeartbeatView. Under S9 this was a schema-forced DROP —
		// TypingDelta carries only a ContentDelta and there was no other arm to
		// put a tool-progress heartbeat in. HeartbeatView is that arm, so the
		// liveness signal now reaches the frontend instead of being logged away.
		if hv := frontend.HeartbeatViewFromProgress(c.workspace, c.sessionID, p.HeartbeatProgress); hv != nil {
			c.push.PushHeartbeatView(hv)
		}
	case *corev1.Event_Vendor:
		if si := systemInitFromVendor(p.Vendor); si != nil {
			c.mu.Lock()
			c.systemInit = si
			c.mu.Unlock()
			// THE LIVE SESSION METADATA, handed to the session controller so it can be
			// persisted. The SDK re-emits init on every submit, so this is the
			// only place the daemon learns that a running session's model
			// changed out from under the model it was spawned with.
			if c.onSystemInit != nil {
				c.onSystemInit(si, ev.GetSeq())
			}
			// The session's retained SystemInit just became available (attach or a
			// fresh init): push it as a SessionInitView so frontends can source
			// their slash-command/tools/model menus from it (S9), replacing the
			// Emacs GET /commands HTTP menu.
			c.push.PushSessionInitView(&frontendv1.SessionInitView{
				Workspace: c.workspace,
				Fence:     c.fence(),
				Init:      si,
			})
		}
		// system/commands_changed is the SDK's mid-session command-list push
		// (a skill discovered as the agent moves, a plugin installed). Before
		// this arm existed the menu froze at whatever the session's one init
		// reported, and only an explicit refresh could unstick it. Folding it
		// into the retained SystemInit and re-publishing means the frontend's
		// menu simply follows, through the SessionInitView it already consumes.
		if cc := commandsChangedFromVendor(p.Vendor); cc != nil {
			if refreshed := c.applyCommandsChanged(cc); refreshed != nil {
				c.push.PushSessionInitView(&frontendv1.SessionInitView{
					Workspace: c.workspace,
					Fence:     c.fence(),
					Init:      refreshed,
				})
			}
		}
		// The SDK's full live-task set. It is the one event that can CLOSE the
		// ghost class rather than adding to it, so it drives both task planes
		// (the SSM counter and the roster) as an authority.
		if btc := frontend.BackgroundTasksFromVendor(p.Vendor); btc != nil {
			c.reconcileTasks(ev, btc)
		}
		// The vendor's compaction ticker. It reached the progress footer's
		// window and nothing else, so the SSM had no way to know the agent was
		// folding the context rather than answering.
		c.noteCompactingStatus(ev, p.Vendor)
		// A terminal result SETTLES ITS TURN, here, on arrival. It is published
		// unconditionally and the turn's durable claim is closed from it; only
		// the accounting stamp is enriched afterwards. See terminalsettlement.go
		// for the ten-minute wedge the old retain-until-accounting-settles
		// behaviour produced. The same path is used for replay because replay
		// also drives this consumer.
		if resultFromVendor(p.Vendor) != nil && c.accounting.activeTurnID != "" {
			c.settleTurnOnTerminalResult(c.accounting.activeTurnID, ev)
			return nil
		}
		c.pushConversation(ev, true)
	case *corev1.Event_ContextCleared, *corev1.Event_ContextCompacted:
		// A clear and a compaction each do two things at once: they render as
		// their own bubble, and they RAISE this conversation's replay floor so no
		// reconnecting frontend is ever served the history they discarded. The
		// floor is recorded FIRST — one pushed but not recorded would be drawn
		// once and then buried under a replay of everything above it.
		c.noteClearOrCompact(ev)
		c.noteCutCompleted(ev)
		c.pushConversation(ev, true)
	default:
		// UnparsedEvent / empty payloads carry no conversation content of their
		// own; the demux already loud-logged them. Nothing to push.
	}
	if historicalInserted && c.onHistoricalUsagePersisted != nil {
		c.onHistoricalUsagePersisted()
		c.logf("session-controller: historical token utilization SessionView republished session=%s api_message_id=%s", c.sessionID, utilization.GetApiMessageId())
	}
	return nil
}

// rejectionIsHistorical answers the rejection log's "was this replayed
// history?" question through the same classifier the reducer used, so the log
// can never disagree with the decision it is reporting.
//
// It reads the ENVELOPE, exactly as the reducer does: one comparison of the
// producer-stamped query against the bound live query. An empty stamp is live
// (fail closed), so a rejection from an unstamped producer is reported as the
// live contradiction it is.
//
// IT ASKS ABOUT THE EVENT, NOT ABOUT ONE PAYLOAD ARM. An earlier version
// short-circuited to false whenever the event carried no AccountUsageObservation
// — but the rejection path is reached by ANY accounting failure, and the most
// common one is a token-utilization record riding a plain stream response. Every
// such rejection therefore printed historical=false no matter which query wrote
// the row, which is how one poisoned durable row (blank model, written by a
// retired query and fixed shim-side long ago) kept re-announcing itself at warn
// on every boot while claiming to be live. The event's epoch is a property of
// its envelope; no payload arm can make a replayed row live.
func rejectionIsHistorical(r *turnAccountingReducer, ev *corev1.Event) bool {
	_, historical := r.liveEvidenceFor(ev)
	return historical
}

// historicalRejectionDecision names the branch a replayed rejection took, for
// the record that would otherwise be a warn. It is the rejection-path sibling of
// retain_history_no_bring_up_fault: the record is kept in full, only its
// severity is withheld.
const historicalRejectionDecision = "retain_history_no_live_warn"

func (c *consumer) logRejectedAccountingObservation(ev *corev1.Event, cause error, historical bool) {
	c.logRejectedTokenUtilization(ev, cause, historical)
	observation := ev.GetAccountUsageObservation()
	queryID, turnID, boundary := "", "", "unspecified"
	if observation != nil {
		queryID, turnID = observation.GetQueryInstanceId(), observation.GetTurnId()
		switch observation.GetBoundary().(type) {
		case *corev1.AccountUsageObservation_TurnStart:
			boundary = "turn_start"
		case *corev1.AccountUsageObservation_TurnEnd:
			boundary = "turn_end"
		}
	}
	// event_query_instance_id is the ENVELOPE stamp — the query whose producer
	// built this row — and it is the whole basis of the historical verdict. It
	// is printed beside the payload's own id because the two answer different
	// questions: the payload says which query the OBSERVATION is about, the
	// envelope says which query WROTE it. A rejection cannot be told apart from
	// a genuine contradiction without the latter.
	const record = "session-controller: turn accounting observation REJECTED before mutation session=%s authoritative_query_instance_id=%q query_instance_id=%q event_query_instance_id=%q seq=%d historical=%v request_id=%q turn_id=%q boundary=%s cause=%q kind=%s"
	args := []any{
		c.sessionID, c.accounting.queryID, queryID, ev.GetQueryInstanceId(), ev.GetSeq(),
		historical,
		ev.GetRequestId(), turnID, boundary, cause.Error(), stateKind(ev),
	}
	if historical {
		// INFO, NOT WARN, AND ONLY ON THIS ARM — the same call the typed
		// termination and the shim degradation already make. A replayed rejection
		// is a durable row being classified, not a fresh anomaly; the anomaly was
		// surfaced at warn when it first occurred. The record is unchanged and
		// complete, only its severity moved, and the decision field names the
		// branch that moved it.
		c.logf(record+" decision=%s", append(args, historicalRejectionDecision)...)
		return
	}
	c.warn(record, args...)
}

// logRejectedTokenUtilization records canonical ingress diagnostics when a
// token record violates a field-level evidence invariant before frame or
// durable-state mutation.
//
// historical is the ONE classifier's verdict for this event, passed in rather
// than re-derived so this record and the observation record beneath it can never
// disagree about the same row's epoch.
func (c *consumer) logRejectedTokenUtilization(ev *corev1.Event, cause error, historical bool) {
	var invalid *tokenutilization.ValidationError
	if !errors.As(cause, &invalid) {
		return
	}
	const record = "session-controller: token utilization REJECTED before mutation field_path=%q api_message_id=%q model=%q source_plane=%s agent_repl_session_id=%q claude_session_id=%q session=%q seq=%d error=%v"
	args := []any{
		invalid.FieldPath, invalid.APIMessageID, invalid.Model, ev.GetPlane().String(), invalid.AgentReplSessionID, invalid.ClaudeSessionID, c.sessionID, ev.GetSeq(), cause,
	}
	if historical {
		// See the observation record above: full identity retained, severity
		// withheld, because a replayed row's rejection is history being replayed
		// rather than news.
		c.logf(record+" historical=true decision=%s", append(args, historicalRejectionDecision)...)
		return
	}
	c.warn(record, args...)
}

// surfaceUnexpectedQueryTermination reports an SDK query termination read off
// the durable sequence.
//
// historical is the single classifier's verdict (turnAccountingReducer.
// liveEvidenceFor): the row was stamped by a query OTHER than the live one, so
// it is a termination the store replays from a RETIRED shim invocation. Such a
// row stays authoritative history and keeps its card, but it may not be spoken
// of in the present tense: it did not happen to this process.
//
// THAT DISTINCTION IS LOAD-BEARING, NOT COSMETIC. The termination pair is
// persisted, so every later bring-up replays it; feeding a retired query's
// death to the bring-up gate made the workspace kill each fresh attempt on a
// fault that predated it, before the handshake could ever land. The park
// cooldown then expired, retried, and died on the same seq — a latch no restart
// could clear, because the evidence was durable. A bring-up may only be failed
// by a fault its OWN query produced.
//
// A historical row therefore skips three things and only three: the bring-up
// gate (onQueryTermination/onDegraded), the runtime fault (which would open a
// turn-terminal degradation on a session that never saw it), and the
// duplicate-suppression latch (which would swallow a genuine LIVE termination
// arriving later through Degraded). The card is still pushed, under the same
// stable identity, so nothing the user could see is lost.
func (c *consumer) surfaceUnexpectedQueryTermination(ev *corev1.Event, item *frontendv1.FailureCardView, historical bool) {
	lifecycle := ev.GetQueryLifecycle()
	terminated := lifecycle.GetTerminated()
	detail := item.GetKind().GetQueryTermination().GetDetail()
	reason, cause := "unexpected_eof", ""
	if failure := terminated.GetIteratorFailure(); failure != nil {
		reason, cause = "iterator_failure", failure.GetCause()
	} else if failure := terminated.GetStartupFailure(); failure != nil {
		reason, cause = "startup_failure", failure.GetCause()
	}
	if historical {
		// INFO, NOT WARN, AND ONLY ON THIS ARM. The anomaly was surfaced loudly
		// when it actually happened; this row is its durable history being
		// replayed, and classifying a replay correctly is ordinary branch
		// selection rather than a new anomaly. Leaving it at warn made a single
		// durable Aug-6 row re-alarm on EVERY subsequent boot, forever, about a
		// query that had died once. The record is unchanged and complete — same
		// channel, same identity context — only its severity moved. The LIVE arm
		// below stays at warn, because a live termination IS new news.
		c.logf("session-controller: typed query termination WITHHELD from the bring-up gate session=%s replayed_query_instance_id=%s live_query_instance_id=%q vendor_session_id=%s observed_at_ms=%d termination_kind=%s cause=%q seq=%d decision=retain_history_no_bring_up_fault",
			c.sessionID, detail.GetQueryInstanceId(), c.accounting.queryID, detail.GetVendorSessionId(), detail.GetObservedAtMs(), reason, cause, ev.GetSeq())
		c.pushHistoricalTerminationCard(detail.GetQueryInstanceId(), item)
		return
	}
	c.unexpectedQueryTerminationSurfaced = true
	c.warn("session-controller: typed query termination surfaced directly session=%s query_instance_id=%s vendor_session_id=%s vendor_identity_unavailable=%v observed_at_ms=%d termination_kind=%s cause=%q seq=%d replay_authority=query_lifecycle", c.sessionID, detail.GetQueryInstanceId(), detail.GetVendorSessionId(), detail.GetVendorSessionIdentityUnavailable() != nil, detail.GetObservedAtMs(), reason, cause, ev.GetSeq())
	if c.onQueryTermination != nil {
		c.onQueryTermination(proto.Clone(detail).(*frontendv1.QueryTerminationFailure))
	}
	queryID := detail.GetQueryInstanceId()
	ds := &corev1.DegradedState{Component: "claude-shim-sdk", Reason: "unexpected_query_termination", QueryInstanceId: &queryID}
	if c.onDegraded != nil {
		c.onDegraded(ds)
	}
	classification := faultClassifications["claude-shim-sdk"]
	c.applyRuntimeFault("claude-shim-sdk", classification, true, "unexpected_query_termination")
	c.pushFailure(c.degradedUUID("claude-shim-sdk"), item)
}

// noteClearOrCompact records a clear or a compaction as the conversation's
// newest replay floor.
//
// A seq-less one is NOT recorded: seq 0 means the event never reached the store
// (producer → store, pre-ingest), and a floor derived from it would be no floor
// at all. It is loud-logged instead of being taken as "nothing happened", since
// a clear or compaction the daemon saw but cannot position is a real anomaly.
func (c *consumer) noteClearOrCompact(ev *corev1.Event) {
	seq := ev.GetSeq()
	logf := dlog.Tag(dlog.Logf(c.logf),
		"session", c.sessionID, "ws", c.workspace, "kind", stateKind(ev),
		"seq", seq, "dedup_key", ev.GetDedupKey())
	if seq == 0 {
		logf("session-controller: replay floor NOT raised — this clear or compaction carries no store seq, so it has no position to floor at")
		return
	}
	logf("session-controller: replay floor raised to this clear or compaction")
	c.floors.SetNewestClearOrCompactSeq(c.sessionID, seq)
	// Outstanding prompt receipts go with it. They carry no seq, so nothing
	// else would ever floor them, and a receipt for a prompt the clear just
	// discarded would replay pre-clear text back above the floor.
	if dropped := c.dropEchoes(); dropped > 0 {
		logf("session-controller: dropped %d unclaimed prompt receipt(s) with the history this floor hides", dropped)
	}
	// The session-command invocations go with them, for the identical reason:
	// they carry no seq either, and an invocation from below the cut replayed
	// above it would sit in a feed the cut exists to open.
	if dropped := c.dropCommandItems(); dropped > 0 {
		logf("session-controller: dropped %d session-command invocation item(s) with the history this floor hides", dropped)
	}
	// And their DURABLE records, or the very next replay would put the
	// pre-cut prompts back above the floor this event just raised.
	c.retireDurableReceiptsThrough(c.now(), "replay_floor_raised:"+stateKind(ev))
}

// noteCutCompleted closes the SSM axis the arrived context cut was the
// completion of, so the footer's phase word stops naming work that is done.
//
// The two events are the ONLY first-class report that a cut finished (the
// vendor announces neither as it begins), which is what makes them the natural
// closing edge for the two axes the daemon and the vendor status opened.
//
// A close for an axis that was never open is not an error — a compaction can
// be reported by the file plane on a daemon that never saw its status ticker —
// and the SSM logs that case rather than acting on it.
func (c *consumer) noteCutCompleted(ev *corev1.Event) {
	var err error
	switch ev.GetPayload().(type) {
	case *corev1.Event_ContextCleared:
		err = c.ssm.ApplyClearing(c.workspace, false, "context_cleared")
		// A CLEAR revival is waiting on exactly this event, on the compaction's
		// terms below: the clearing axis closing is the only first-class report
		// that the conversation was actually discarded, so the gate is released
		// from here rather than from a turn end that would also fire for a
		// `/clear` the CLI never carried out.
		c.fireCutWaiter(&c.clearedWaiter)
	case *corev1.Event_ContextCompacted:
		err = c.ssm.ApplyCompacting(c.workspace, false, "context_compacted")
		// THE COMPACTION GATE CLOSES HERE, on the same event and for the same
		// reason the revival's completion gate opens here: this is the only
		// first-class report that a compaction actually finished. A compaction
		// the daemon initiates is declined from now until the conversation is
		// given something new to summarize (ssm/compactiongate.go).
		//
		// It is a SEPARATE failure from the axis close above, and is reported
		// separately: a gate that failed to close permits a duplicate
		// compaction, where an axis that failed to close holds a phase word.
		if gateErr := c.ssm.NoteCompactionCompleted(c.workspace); gateErr != nil {
			c.logf("session-controller: closing the compaction gate FAILED session=%s ws=%s seq=%d: %v (a daemon-initiated compaction may run a second time against this same conversation)",
				c.sessionID, c.workspace, ev.GetSeq(), gateErr)
		}
		// A compact-first revival is waiting on exactly this event. The
		// compacting axis closing IS the completion signal — there is no other
		// first-class report that a compaction finished — so the revival's gate
		// is released from here rather than from a turn end, which would also
		// fire for a compaction that failed.
		c.fireCutWaiter(&c.compactedWaiter)
	}
	if err != nil {
		c.logf("session-controller: closing the context-cut axis FAILED session=%s ws=%s seq=%d kind=%s: %v (the workspace may hold its phase word until the next bounding edge)",
			c.sessionID, c.workspace, ev.GetSeq(), stateKind(ev), err)
	}
}

// noteCompactingStatus moves the SSM's compacting axis from the vendor's own
// status ticker — the same signal the progress footer's compaction window is
// folded from, which until now reached only the footer.
//
// An EMPTY status is the vendor's null and closes the window; that is the
// contract progress.applyStreamLocked already reads it under, and the two must
// agree or the footer's window and the phase word would disagree about the
// same fact.
func (c *consumer) noteCompactingStatus(ev *corev1.Event, a *anypb.Any) {
	status, ok := statusFromVendor(a)
	if !ok {
		return
	}
	if err := c.ssm.ApplyCompacting(c.workspace, status == "compacting", "vendor_status:"+status); err != nil {
		c.logf("session-controller: applying the vendor compaction status FAILED session=%s ws=%s seq=%d status=%q: %v",
			c.sessionID, c.workspace, ev.GetSeq(), status, err)
	}
}

// reconcileTasks adopts a `BackgroundTasksChanged` snapshot as the
// authoritative live-task set on BOTH task planes:
//
//   - the SSM's live_task_count, which appends reconciliation rows so the
//     derived count equals the list exactly (it is the only thing that can
//     settle the `IMPOSSIBLE live_task_count=-N` class, where replayed
//     historical task_ended events arrive with no logged task_started); and
//   - the frontend TaskCatalog, rebuilt from the ring — which already holds
//     THIS event, since Consume retains before it translates — so the roster
//     sweeps its ghosts in the same push.
//
// An SSM failure is loud-logged and does not stop the roster refresh: the two
// planes are independent, and losing both over one failure would be worse.
func (c *consumer) reconcileTasks(ev *corev1.Event, btc *datav1.BackgroundTasksChanged) {
	ids := make([]string, 0, len(btc.GetTasks()))
	for _, ref := range btc.GetTasks() {
		if ref.GetTaskId() != "" {
			ids = append(ids, ref.GetTaskId())
		}
	}
	c.logf("session-controller: authoritative live-task set session=%s ws=%s seq=%d tasks=%d",
		c.sessionID, c.workspace, ev.GetSeq(), len(ids))
	// Keyed by the EVENT's session id: the SSM resolves a workspace from
	// whichever identity the event carries, and a store event carries the
	// vendor uuid rather than the daemon's s_ id.
	if err := c.ssm.ReconcileTasks(ev.GetSessionId(), ids); err != nil {
		c.logf("session-controller: task reconciliation failed session=%s seq=%d: %v", c.sessionID, ev.GetSeq(), err)
	}
	c.push.PushTaskCatalog(frontend.BuildTaskCatalog(c.workspace, c.sessionID, c.fence(), c.snapshotRing(), c.logf))
}

// Backfill states persisted on the registry record and mapped onto
// frontendv1.BackfillState by the server. Strings (not the enum) because the
// registry is JSON on disk and a numeric enum there would be unreadable.
const (
	BackfillPending = "pending"
	BackfillDone    = "done"
	BackfillFailed  = "failed"
)

// sidecarProducer is the UnparsedEvent producer tag the shim-claude-sidecar
// stamps on a transcript line it could not read. It is the ONLY sidecar
// failure that reaches the daemon durably (see frontendv1.BackfillState).
const sidecarProducer = "shim-claude-sidecar"

// noteBackfill records a backfill transition for this session, ONCE.
//
// Called on every qualifying event, so the in-memory latch is what keeps a
// long transcript from writing the registry record per line. FAILED is
// terminal for the session: a transcript the sidecar could not fully read does
// not become readable by reading more of it, and letting a later good line
// flip it back to DONE would hide exactly the partial-history case this signal
// exists to surface.
func (c *consumer) noteBackfill(state string) {
	c.mu.Lock()
	if c.backfill == state || c.backfill == BackfillFailed {
		c.mu.Unlock()
		return
	}
	c.backfill = state
	c.mu.Unlock()
	c.logf("session-controller: backfill %s session=%s ws=%s", state, c.sessionID, c.workspace)
	if c.onBackfill != nil {
		c.onBackfill(state)
	}
}

// settleBackfillFromStore settles the backfill for a session whose history is
// ALREADY in the store, using the durable high-water as the evidence.
//
// THE REOPEN WEDGE this exists to close: observeBackfill can only ever see a
// backfill happen — it flips DONE when a TranscriptLine ARRIVES. On reopening
// a session whose transcript was fully ingested in an earlier run, the
// sidecar's cursor already sits at the tail of that file, so no new line is
// ever written and no new event ever arrives. The backfill was complete before
// this daemon started, and waiting for evidence of it happening again would
// wait forever — leaving a workspace with a complete, replayable history stuck
// reporting "still starting" and therefore stuck BLUE, permanently, on the
// most ordinary action there is.
//
// A nonzero high-water is proof of exactly the right fact: the daemon has
// durably observed store events for this session, which means the file plane
// really did deliver into the store. It is the same conclusion
// observeBackfill draws, read from the accumulated record rather than from a
// live arrival.
//
// It never overwrites a state already known — in particular never a FAILED,
// which is terminal (a transcript the sidecar could not fully read does not
// become readable by reopening it).
func (c *consumer) settleBackfillFromStore(highWater uint64) {
	if highWater == 0 {
		// No durable events for this session: genuinely nothing ingested
		// yet, so the live path is the one that will settle it.
		return
	}
	c.mu.Lock()
	already := c.backfill != ""
	c.mu.Unlock()
	if already {
		return
	}
	c.logf("session-controller: backfill settled from store high-water session=%s ws=%s seq=%d (history already ingested; no new line will arrive)",
		c.sessionID, c.workspace, highWater)
	c.noteBackfill(BackfillDone)
}

// observeBackfill derives the never-blue backfill signal from the two pieces
// of evidence that actually reach the daemon (the sidecar reports nothing of
// its own — see frontendv1.BackfillState):
//
//   - a data.v1.TranscriptLine vendor event means the FILE plane delivered
//     into the store, which is what "backfilled" means;
//   - an UnparsedEvent stamped by the sidecar means it hit a line of that
//     transcript it could not read.
//
// A stream-plane event proves nothing about the file plane and is ignored
// here: a live turn writes ClaudeStreamMessage events for a session whose
// history never arrived, which is precisely the blue-but-live case.
//
// This sees only backfills that HAPPEN. A session reopened with its history
// already ingested is settled by settleBackfillFromStore instead.
func (c *consumer) observeBackfill(ev *corev1.Event) {
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_Unparsed:
		if p.Unparsed.GetProducer() == sidecarProducer {
			c.logf("session-controller: sidecar could not read transcript line session=%s path=%s offset=%d: %s",
				c.sessionID, p.Unparsed.GetSourcePath(), p.Unparsed.GetByteOffset(), p.Unparsed.GetError())
			c.noteBackfill(BackfillFailed)
		}
	case *corev1.Event_Vendor:
		if isTranscriptLine(p.Vendor) {
			c.noteBackfill(BackfillDone)
		}
	}
}

// isTranscriptLine reports whether a vendor Any carries a file-plane
// TranscriptLine. Matched on the Any's type identity rather than by
// unmarshaling it: this runs on every vendor event, and which ARM it is is the
// entire question being asked.
func isTranscriptLine(a *anypb.Any) bool {
	return a != nil && a.MessageIs((*datav1.TranscriptLine)(nil))
}

// applyProgress folds an event into the progress-footer resolver. Both event
// planes route through here — the lifecycle plane (Apply) carries the turn
// boundaries and the data plane (Consume) carries the tickers and windows — so
// the resolver sees the whole stream exactly once per event.
//
// A fold failure is loud-logged, never swallowed, and never stops the stream:
// the footer degrading is not a reason to stop delivering conversation.
func (c *consumer) applyProgress(ev *corev1.Event) {
	if err := c.prog.Apply(c.workspace, c.sessionID, ev); err != nil {
		c.logf("session-controller: progress apply failed session=%s seq=%d kind=%s: %v",
			c.sessionID, ev.GetSeq(), stateKind(ev), err)
	}
}

// userTurnReceipt extracts the round-trip receipt a pushed delta carries for a
// USER PROMPT: the request id and total prompt-text length across its
// user_message items. textLen 0 means the delta carries no prompt — a pure
// tool-result feedback message rides the user_message arm too, and logging
// every tool result would bury the one receipt per prompt this exists for.
func userTurnReceipt(cd *frontendv1.ConversationDelta) (requestID string, textLen int) {
	for _, it := range cd.GetItems() {
		um := it.GetUserMessage()
		if um == nil {
			continue
		}
		n := len(userMessageText(um))
		if n > 0 {
			requestID = it.GetRequestId()
			textLen += n
		}
	}
	return requestID, textLen
}

// pushConversation converts a vendor event to a ConversationDelta and pushes it,
// loud-logging (never swallowing) a translation failure.
func (c *consumer) pushConversation(ev *corev1.Event, live bool) {
	c.pushConversationAttributed(ev, live, "")
}

// pushConversationAttributed is pushConversation for the one caller that has
// ATTRIBUTED this very event to a turn's terminal result.
//
// The attribution is carried as an argument rather than looked up from a
// stream coordinate. A seq index would answer "was some event with this seq a
// terminal result", and a session that outlives a shim generation renumbers
// from one, so seq 10 of the second generation would inherit the verdict of
// seq 10 of the first. Passing the turn id along the one call that makes the
// attribution makes that confusion unrepresentable: the assertion below can
// only ever be asked of the event it was made about.
func (c *consumer) pushConversationAttributed(ev *corev1.Event, live bool, terminalTurnID string) {
	observation, err := tokenUtilizationObservationFromEvent(ev, c.sessionID, c.accounting.isKnownVendorSession)
	if err != nil {
		historical := rejectionIsHistorical(c.accounting, ev)
		c.logRejectedTokenUtilization(ev, err, historical)
		if historical {
			// See degradeAccountingObservation: a replayed row's rejection keeps its
			// full record and loses only its severity.
			c.logf("session-controller: conversation token utilization DEGRADED session=%s seq=%d historical=true decision=%s — invalid accounting attachment was rejected, but conversation translation continues: %v", c.sessionID, ev.GetSeq(), historicalRejectionDecision, err)
		} else {
			c.warn("session-controller: conversation token utilization DEGRADED session=%s seq=%d — invalid accounting attachment was rejected, but conversation translation continues: %v", c.sessionID, ev.GetSeq(), err)
		}
		observation = nil
	}
	var historicalUsage *frontendv1.TokenUtilization
	if observation != nil && observation.historical {
		historicalUsage = observation.record
	}
	// THE ONE CURATION POINT. CurateEvent decides, for this event and every
	// route that replays it, which content is the top-level conversation and
	// which belongs inside a detached agent's bubble. Nothing below re-asks
	// that question: a detached agent's emissions are already gone from cd.
	curated, err := frontend.CurateEvent(c.workspace, c.fence(), ev)
	if err != nil {
		c.logf("session-controller: conversation translate failed session=%s seq=%d: %v", c.sessionID, ev.GetSeq(), err)
		return
	}
	// THE RE-DRIVE'S DELIVERY IS CONFIRMED BY ITS OWN SUPPRESSION. The curator
	// just removed the daemon's internal instruction from this event, which is
	// the evidence — and the only evidence — that the re-drive carrying it
	// reached the vendor conversation (turnresumption.go).
	//
	// It runs BEFORE the nil-feed return, because an event carrying only the
	// instruction curates to no feed at all and is exactly the ordinary case.
	// It runs on replayed events too: a replay re-establishes the same fact,
	// and the discharge is idempotent.
	c.dischargeDeliveredResumptions(curated.SuppressedInternalResumes)
	cd, envs := curated.Feed, curated.Envelopes
	if cd == nil {
		return // known-but-non-conversational vendor payload
	}
	// The detached half, folded into its bubbles and pushed on the async plane.
	// It runs BEFORE the feed push so that a bubble a frontend is about to be
	// told about by a stamped tool card has already been opened for it.
	//
	// A WINDOW's opening edge rides the same push, and for the same reason: the
	// Skill call's card is in this very delta, and the bubble has to exist
	// before the stamp below resolves it (asyncwindows.go).
	detached := c.observeAsync(curated, ev)
	detached.absorb(c.observeSkillSpawn(curated, ev))
	c.pushAsync(detached, ev)
	// THE CLASSIFICATION VERDICT ON THE TOOL CARD, from the same store the
	// bubbles live in — so the card names a bubble the frontend has, and both
	// spawned_bubble_id fields carry the one string that store resolved.
	frontend.StampSpawnedBubbleIDs(cd.GetItems(), c.bubbles.spawnedBubbleID)
	for _, item := range cd.GetItems() {
		response := item.GetAgent().GetResponse()
		assistant := response.GetBody()
		if assistant == nil {
			continue
		}
		var utilization *frontendv1.TokenUtilization
		c.mu.Lock()
		completed := c.completedResponses[assistant.GetId()]
		c.mu.Unlock()
		if completed != nil {
			utilization = completed
		} else if replayed := c.replayedResponses[assistant.GetId()]; replayed != nil {
			utilization = replayed
		} else if historicalUsage != nil && historicalUsage.GetApiMessageId() == assistant.GetId() {
			utilization = historicalUsage
		} else if live {
			// Replayed history may use only stable completed/persisted indexes.
			// It must never infer identity from the reducer's mutable live turn.
			utilization = c.accounting.response(assistant.GetId())
		}
		if utilization == nil {
			continue
		}
		// The DURABLE record no longer rides the feed item; the bubble carries
		// the RESOLVED figures its corner renders, derived once here. The
		// evidence layer is untouched — it is still persisted and still
		// vendor-faithful; what changed is that a renderer is handed the answer
		// rather than the arithmetic.
		stamp, err := tokenusage.ResponseStamp(utilization)
		if err != nil {
			// Never swallowed and never zero-filled: a stamp the daemon cannot
			// derive is a defect in the durable record, and a fabricated zero
			// would read as a free response.
			c.warn("session-controller: response usage stamp REJECTED session=%s seq=%d api_message_id=%s branch=stamp_omitted error=%v",
				c.sessionID, ev.GetSeq(), utilization.GetApiMessageId(), err)
			continue
		}
		response.UsageStamp = stamp
		if utilization == historicalUsage {
			c.logf("session-controller: historical token utilization attached session=%s seq=%d api_message_id=%s root_turn_id=absent response_timing=absent", c.sessionID, ev.GetSeq(), utilization.GetApiMessageId())
		}
	}
	// TURN ACCOUNTING NO LONGER RIDES THE FEED ITEM. The contract removed the
	// durable turn_accounting record from ConversationItem: a turn's verdict is
	// the session's ledger rather than the agent's utterance, and it reaches a
	// frontend RESOLVED, on FooterAccountingCell. The reconciliation that
	// produces that cell is not wired yet, so the accounting is consumed here
	// and rendered nowhere.
	//
	// The PRESENCE CHECK stays, still fatal, and now runs at the moment the
	// attribution is MADE rather than at the moment its stamp settles. It never
	// asserted anything about the wire — it asserts that a turn the daemon has
	// attributed a terminal result to really did emit one, which is the
	// invariant that catches an accounting attributed to the wrong turn. Since
	// a terminal result is published on arrival now, this is the one push where
	// that question can be asked, and every terminal reaches it rather than
	// only the ones whose accounting settled.
	if terminalTurnID != "" && !hasTurnResult(cd) {
		panic(fmt.Sprintf("session-controller: accounting terminal seq=%d turn_id=%s had no result item", ev.GetSeq(), terminalTurnID))
	}
	// The harness's isMeta records — a launched skill's body and the notices
	// around it — become the skill's own bubble body, the skill card they belong
	// to, or nothing at all (skillbody.go). A body delivered to a bubble is async
	// traffic, and rides out with the window's own push below.
	windows := c.curateMetaRecords(cd, envs)
	// The CLI's own slash-command bookkeeping, which it writes as unflagged
	// "user" transcript records, goes no further than this (machinery.go).
	// FIRST, before attribution: a machinery record claiming a real prompt's
	// receipt would misattribute both.
	c.withholdMachinery(cd)
	// The harness's detached-work completion notices, which it writes as
	// unflagged "user" records addressed to the model (tasknotification.go).
	// Beside withholdMachinery and before attribution for the same reason: a
	// record nobody typed must never claim a real prompt's receipt.
	c.withholdTaskNotifications(cd, envs)
	// The CLI's own "No response requested." record — a synthetic assistant
	// line closing a turn nothing was asked of — goes no further either
	// (noresponse.go).
	c.withholdNoResponsePlaceholders(cd)
	// The daemon's own cache keep-alive turns, withheld from every rendering
	// off the durable window ledger (keepaliveexclude.go). Same "withheld, not
	// deleted" discipline as the two above, and placed with them so all three
	// exclusions run at the one chokepoint every replay route funnels through.
	//
	// A delta emptied by this exclusion is still PUSHED, exactly as one emptied
	// by the two above is. The frame carries through_seq, which is the
	// frontend's replay cursor: swallowing it would leave every client's cursor
	// stuck behind the ping forever, and the next resync would re-deliver the
	// whole conversation from before it.
	c.withholdKeepAlive(cd)
	// The daemon's own `/compact` turns — the warm compaction and the
	// compact-first revival — whose plumbing the feed drew as an orphan duration
	// chip (contextcutexclude.go). Same "withheld, not deleted" discipline as
	// the exclusions above, and placed with them for the same reason: this is
	// the one chokepoint every replay route funnels through. The compaction's
	// own divider is untouched, because the vendor's `compact_boundary` record
	// carries no turn id for this to match on.
	c.withholdDaemonContextCut(cd)
	// PROVENANCE, AFTER EVERY CURATION AND BEFORE THE PUSH. The curators above
	// rebuild items (skillbody.go mints a fresh SkillBodyItem from the record it
	// consumed), so stamping earlier would leave a rebuilt item carrying the
	// proto3 zero — the malformed frame a receiver must reject.
	//
	// The verdict comes from the merge lease's DURABLE LEDGER keyed on the
	// item's own instant, never from whether the lease happens to be held right
	// now: this same path runs on a resync replaying history, and a released
	// lease would rewrite a merge's conversation as the user's.
	if !c.stampConversationProvenance(cd) {
		return
	}
	// LIVE ONLY. A durable user turn arriving now may be the transcript's
	// account of a submit this daemon made moments ago, and stamping it with
	// that submit's request id is what lets the frontend reconcile it onto the
	// receipt already on screen (promptecho.go). Replayed history has no live
	// submit behind it, and claiming a receipt for one would misattribute both.
	if live {
		c.attributeUserTurn(cd)
	}
	// THE WINDOW DIVERSION, LAST. While a merge run or a skill invocation owns
	// the session, its emissions belong to that bubble rather than to this delta
	// — and running here, after every curator and every stamp, means the items it
	// diverts are exactly the ones that would otherwise have been pushed
	// (asyncwindows.go). The delta is still pushed when the diversion empties it,
	// because it carries the frontend's replay cursor.
	//
	// The body deliveries absorbed above go out FIRST in the same frame: a
	// skill's body is what the bubble opens with, and its emissions follow.
	windows.absorb(c.foldWindows(cd, ev))
	c.push.PushConversationDelta(cd)
	// AFTER the feed push: the bubble's new content is the content that just
	// left this delta, and a client applies the removal before the fold that
	// explains it.
	c.pushAsync(windows, ev)
	// A DURABLE replay watches what its own store events carried, so an
	// un-retired receipt for a prompt those events already drew is suppressed
	// rather than drawn a second time (durablereplay.go). Nil on every live
	// consumer, where the ring and attributeUserTurn cover the same ground.
	if c.onPushedConversation != nil {
		c.onPushedConversation(cd)
	}
	// The prompt round-trip receipt: one line per LIVE user prompt reaching
	// the frontend push, closing the gap between "control request acked" (the
	// shim took the prompt) and the webapp's own mount log. Live only — a
	// resync/repull replays thousands of historical user turns, and a receipt
	// per replayed prompt would bury the one that answers "did MY prompt come
	// back".
	if live {
		if requestID, textLen := userTurnReceipt(cd); textLen > 0 {
			c.logf("session-controller: user turn pushed ws=%q session=%s seq=%d request_id=%s len=%d",
				c.workspace, c.sessionID, ev.GetSeq(), requestID, textLen)
		}
	}
}

// connectionComponent names the daemon's own transport in a degraded card.
const connectionComponent = "shim-connection"

type faultClassification struct {
	faultType string
	impact    ssm.FaultImpact
}

// faultClassifications is the CLOSED set of components whose degradations move
// workspace health. A component missing from it is not silently tolerated — it
// takes the unknown_component branch in Degraded and applies nothing at all —
// so every producer of a DegradedState has to appear here to be believed.
//
// claude-shim-turn-lifecycle is the shim reporting that a turn reached its
// terminal by some route other than the SDK's own result: a synthesized end, a
// late result for a turn already closed, a result correlating to no open turn.
// Each is a statement about whether the turn terminal can be trusted, which is
// the impact the SDK stream itself carries.
var faultClassifications = map[string]faultClassification{
	"shim-store-client":              {faultType: "store-link", impact: ssm.FaultImpactConnectivity},
	"shim-store":                     {faultType: "store-link", impact: ssm.FaultImpactConnectivity},
	"store-client":                   {faultType: "store-link", impact: ssm.FaultImpactConnectivity},
	"store":                          {faultType: "store-link", impact: ssm.FaultImpactConnectivity},
	connectionComponent:              {faultType: "heartbeat", impact: ssm.FaultImpactConnectivity},
	"claude-shim-model-catalog":      {faultType: "model-catalog", impact: ssm.FaultImpactFeature},
	"daemon-model-catalog":           {faultType: "model-catalog", impact: ssm.FaultImpactFeature},
	"shim-claude-sidecar-store-link": {faultType: "transcript-file-plane", impact: ssm.FaultImpactFeature},
	"claude-shim-sdk":                {faultType: "sdk-stream", impact: ssm.FaultImpactTurnTerminal},
	"claude-shim-turn-lifecycle":     {faultType: "turn-lifecycle", impact: ssm.FaultImpactTurnTerminal},
	"claude-shim-interrupt":          {faultType: "interrupt", impact: ssm.FaultImpactCommand},
	"claude-shim-permission-mode":    {faultType: "permission-mode", impact: ssm.FaultImpactCommand},
	"claude-shim":                    {faultType: "shim-capability", impact: ssm.FaultImpactFeature},
}

// Degraded surfaces a shim-sourced DegradedState as a self-resolving failure
// card (F4).
//
// It used to push a DegradedNotice: chrome that scrolled away, carried no
// correlation id, and threw dropped_count away in translation. A user whose
// workspace changed color needs to find out why from the conversation itself,
// so the account lives there now.
//
// It returns the disposition it classified ds under, because the shim client's
// own relay record for this event takes its severity from that verdict and has
// no way to compute it (shimclient.DegradedReporter).
func (c *consumer) Degraded(_ string, ev *corev1.Event, ds *corev1.DegradedState) shimclient.Disposition {
	// THE SAME EPOCH TEST THE TYPED TERMINATION CHANNEL ALREADY MAKES, asked of
	// the ONE classifier, on the envelope. The shim writes an unexpected
	// termination as an ACKNOWLEDGED PAIR — the QueryLifecycle row and this
	// DegradedState, adjacent in the store — so every bring-up replays both.
	// surfaceUnexpectedQueryTermination withheld the first from the bring-up
	// gate; nothing withheld the second, and the second alone was enough to kill
	// each fresh attempt on a fault written by a query that had already died.
	// A bring-up may only be failed by a fault its OWN query produced.
	if _, historical := c.accounting.liveEvidenceFor(ev); historical {
		c.withholdHistoricalDegradation(ev, ds)
		return shimclient.DegradationHistorical
	}
	if !ds.GetRecovered() && ds.GetComponent() == "claude-shim-sdk" && ds.GetReason() == "unexpected_query_termination" {
		if ds.QueryInstanceId == nil || ds.GetQueryInstanceId() == "" {
			panic(fmt.Sprintf("session-controller: unexpected query termination degradation has no query_instance_id session=%s", c.sessionID))
		}
		if c.unexpectedQueryTerminationSurfaced {
			c.logf("session-controller: duplicate unexpected query termination suppressed session=%s component=%s reason=%s", c.sessionID, ds.GetComponent(), ds.GetReason())
			return shimclient.DegradationLive
		}
		c.unexpectedQueryTerminationSurfaced = true
	}
	// A shim reporting its SDK dead BEFORE the bring-up gate closed is the one
	// account the daemon ever gets of why a resume died (bringupescape.go).
	if c.onDegraded != nil {
		c.onDegraded(ds)
	}
	classification, ok := faultClassifications[ds.GetComponent()]
	if !ok {
		c.logf("session-controller: runtime fault REJECTED ws=%q session=%q generation=%q component=%q reason=%q recovered=%v branch=unknown_component",
			c.workspace, c.sessionID, c.generationID, ds.GetComponent(), ds.GetReason(), ds.GetRecovered())
	} else {
		c.applyRuntimeFault(ds.GetComponent(), classification, !ds.GetRecovered(), faultCauseKind(ds))
	}
	item := frontend.FailureCardFromDegradedState(ds, c.now())
	if item == nil {
		return shimclient.DegradationLive
	}
	c.pushFailure(c.degradedUUID(ds.GetComponent()), item)
	return shimclient.DegradationLive
}

// withholdHistoricalDegradation retains a replayed degradation as history while
// keeping it out of everything that speaks in the present tense.
//
// It skips exactly what surfaceUnexpectedQueryTermination's historical arm
// skips, and for the identical reasons:
//
//   - the BRING-UP GATE (onDegraded → noteBringUpFault), because a retired
//     query's death did not happen to the attempt now in flight;
//   - the RUNTIME FAULT, because opening a fault window would colour a
//     workspace over a degradation that has already been superseded — and a
//     replayed recovery, stamped by the same retired query, is withheld here
//     too, so the pair still cancels rather than half-applying;
//   - the DUPLICATE-SUPPRESSION LATCH, because arming it on history would make
//     the latch swallow a genuine LIVE termination arriving afterwards.
//
// The failure CARD is still pushed, under the same stable per-component
// identity, so nothing the user could see is lost — this is a retention
// decision, not a drop. It is pushed ONCE per replayed pair, through
// pushHistoricalTerminationCard, because the QueryLifecycle half derives the
// same card identity from the other sink.
func (c *consumer) withholdHistoricalDegradation(ev *corev1.Event, ds *corev1.DegradedState) {
	// INFO, NOT WARN, for the same reason the typed termination's historical arm
	// is at info (surfaceUnexpectedQueryTermination): a replayed row is history
	// being classified, not a fresh degradation. The live arm in Degraded keeps
	// its warn/error severity untouched.
	c.logf("session-controller: shim degradation WITHHELD from the bring-up gate session=%s ws=%q replayed_query_instance_id=%s live_query_instance_id=%q component=%s reason=%q recovered=%v dropped_count=%d seq=%d decision=retain_history_no_bring_up_fault",
		c.sessionID, c.workspace, ev.GetQueryInstanceId(), c.accounting.queryID,
		ds.GetComponent(), ds.GetReason(), ds.GetRecovered(), ds.GetDroppedCount(), ev.GetSeq())
	item := frontend.FailureCardFromDegradedState(ds, c.now())
	if item == nil {
		return
	}
	// The DegradedState half of a replayed unexpected-termination PAIR derives
	// the very same card identity as the QueryLifecycle half, so it goes through
	// the pair latch rather than pushing a second, identical card.
	if ds.GetComponent() == shimSDKComponent && ds.GetReason() == "unexpected_query_termination" {
		c.pushHistoricalTerminationCard(ds.GetQueryInstanceId(), item)
		return
	}
	c.pushWithheldFailure(c.degradedUUID(ds.GetComponent()), item)
}

// pushHistoricalTerminationCard pushes the failure card for a REPLAYED
// unexpected-termination pair exactly once, keyed by the retired query the pair
// belongs to.
//
// The shim persists such a termination as an acknowledged pair — the
// QueryLifecycle row and the confirming DegradedState, adjacent in the store —
// and the two halves reach two different sinks that both derive
// degradedUUID("claude-shim-sdk"). Pushing both meant one replayed death
// recorded "system failure … resolved=false" TWICE per boot for one event. The
// live path has collapsed this to a single push since it was written
// (unexpectedQueryTerminationSurfaced); this is the same discipline on the
// withhold path.
//
// FIRST WINS, and the store's own write order makes that the RICHER half: the
// lifecycle row is written before its confirmation, and only the lifecycle row
// carries the typed QueryTerminationFailure detail.
//
// A pair whose retired query id is EMPTY is pushed unlatched. An unkeyed latch
// would collapse every unidentified replay into one card, and losing a card is
// worse than logging one twice.
func (c *consumer) pushHistoricalTerminationCard(retiredQueryID string, item *frontendv1.FailureCardView) {
	if retiredQueryID != "" {
		if _, seen := c.historicalTerminationPairs[retiredQueryID]; seen {
			c.logf("session-controller: replayed termination pair already carded session=%s retired_query_instance_id=%s uuid=%s decision=single_card_per_replayed_pair",
				c.sessionID, retiredQueryID, c.degradedUUID(shimSDKComponent))
			return
		}
		if c.historicalTerminationPairs == nil {
			c.historicalTerminationPairs = map[string]struct{}{}
		}
		c.historicalTerminationPairs[retiredQueryID] = struct{}{}
	}
	c.pushWithheldFailure(c.degradedUUID(shimSDKComponent), item)
}

// Canonical cause kinds for a fault edge whose DegradedState carried no
// reason of its own. The SSM requires a non-empty cause kind on every edge,
// and the reason is a proto3 string a reporting peer may leave unset — a
// recovery that arrived without one used to be REJECTED for the empty cause,
// which left the fault window open, the workspace blue, and only a "runtime
// fault FAILED" line to say why.
//
// Deriving the cause here rather than demanding the peer populate it is the
// daemon owning its OWN vocabulary: cause_kind is an SSM concept, and
// ConnectionRecovered has always supplied one ("heartbeat_resumed") instead of
// forwarding free text.
const (
	componentDegradedCause  = "component_degraded"
	componentRecoveredCause = "component_recovered"
)

// faultCauseKind is the cause kind for the fault edge ds describes: its own
// reason when it carries one, else this daemon's canonical name for the edge.
func faultCauseKind(ds *corev1.DegradedState) string {
	if reason := ds.GetReason(); reason != "" {
		return reason
	}
	if ds.GetRecovered() {
		return componentRecoveredCause
	}
	return componentDegradedCause
}

// ConnectionDegraded opens the transport heartbeat's typed connectivity fault
// and the matching failure card.
func (c *consumer) ConnectionDegraded(_ string, reason string) {
	c.applyRuntimeFault(connectionComponent, faultClassifications[connectionComponent], true, reason)
	c.pushFailure(c.degradedUUID(connectionComponent), errclass.ConnectionDegraded(connectionComponent, reason))
}

// ConnectionRecovered closes exactly that heartbeat fault window and re-sends
// the SAME card with resolved_at_ms stamped.
//
// Re-sending under the opening card's uuid is what makes it ONE card that
// settles rather than two cards that accumulate — the recovery report used to
// carry neither a reason nor any correlation to what it was recovering from.
func (c *consumer) ConnectionRecovered(_ string) {
	c.applyRuntimeFault(connectionComponent, faultClassifications[connectionComponent], false, "heartbeat_resumed")
	item := errclass.ConnectionDegraded(connectionComponent, "")
	errclass.Resolve(item, c.now())
	c.pushFailure(c.degradedUUID(connectionComponent), item)
}

// applyRuntimeFault records one typed, generation-scoped fault edge and logs
// the complete identity and branch verdict.
func (c *consumer) applyRuntimeFault(component string, classification faultClassification, open bool, causeKind string) {
	if err := c.ssm.ApplyRuntimeFault(
		c.workspace, c.sessionID, c.generationID,
		component, classification.faultType, classification.impact, open, causeKind,
	); err != nil {
		c.logf("session-controller: runtime fault FAILED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%v cause=%q branch=ssm_rejected error=%v",
			c.workspace, c.sessionID, c.generationID, component, classification.faultType, classification.impact, open, causeKind, err)
		return
	}
	c.logf("session-controller: runtime fault APPLIED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%v cause=%q branch=accepted",
		c.workspace, c.sessionID, c.generationID, component, classification.faultType, classification.impact, open, causeKind)
}

// degradedUUID is the STABLE card identity for one component's degraded
// window on this session. Both edges of the window derive the same id, which
// is what lets the closing edge reconcile the opening card in place.
func (c *consumer) degradedUUID(component string) string {
	if component == "" {
		component = connectionComponent
	}
	return "degraded:" + c.sessionID + ":" + component
}

// startFailedUUID is the stable card identity for this session's start-failed
// report. One per session, so a retried-and-failed-again bring-up updates the
// same card instead of stacking a second account of one failure.
func (c *consumer) startFailedUUID() string {
	return startFailedCardUUID(c.sessionID)
}

// boundaryInstant is the instant a lifecycle event says it happened at, which
// is the ONE instant every consumer of that boundary must agree on: the
// keep-alive policy's clock, the keep-alive window's closing edge, and the
// durable last-turn-end stamp all measure from the same fact.
//
// The fallback to now() is the honest reading of an event that carried no
// instant — the boundary really was observed, and dropping it because a field
// was unset would lose the fact entirely — and it is the same fallback the
// last-turn-end stamp has always used.
func (c *consumer) boundaryInstant(ev *corev1.Event) int64 {
	if at := ev.GetProducedAtMs(); at != 0 {
		return at
	}
	return c.now()
}

// keepAliveWindowUnclosedUUID is the stable card identity for ONE ping's
// unclosed window. Keyed by turn id rather than by session so two different
// stranded windows are two different cards: each names a distinct row that has
// to be repaired, and collapsing them would hide the second one.
func (c *consumer) keepAliveWindowUnclosedUUID(turnID string) string {
	return "keep_alive_window_unclosed:" + c.sessionID + ":" + turnID
}

// keepAliveWindowInvertedUUID is the stable card identity for ONE ping's
// refused, clamped close. Keyed like the unclosed card above, and DISTINCT from
// it: the two report different faults about the same row, and sharing an
// identity would let one replace the other on screen.
func (c *consumer) keepAliveWindowInvertedUUID(turnID string) string {
	return "keep_alive_window_inverted:" + c.sessionID + ":" + turnID
}

// coldCompactionUUID is the stable card identity for ONE daemon compaction that
// read the conversation at the uncached rate. Keyed by the compaction's own turn
// id so a session that pays this cost twice shows two cards: each is a separate
// charge, and collapsing them under one identity would hide the second.
func (c *consumer) coldCompactionUUID(turnID string) string {
	return "compaction_cold_read:" + c.sessionID + ":" + turnID
}

// resync replays the retained conversation deltas from fromSeq (0 = from the
// start of the retained window) via the normal PushConversationDelta path. It
// is idempotent by construction: the frontends reconcile by through_seq/uuid,
// so re-pushing already-seen items REPLACES rather than duplicates them.
//
// It returns the ring's FLOOR: the oldest seq this replay could possibly have
// covered. A caller asking below the floor was answered incompletely, and the
// floor is what tells it so — the bounded store re-pull (repull.go) closes the
// remainder. Before the floor was reported, "older than the retained window"
// was answered with silence, which is what left a freshly-mounted GUI blank
// after a daemon restart (the ring is empty then, so EVERY request is
// below-floor).
func (c *consumer) resync(fromSeq uint64) (floor uint64, haveFloor bool) {
	for _, ev := range c.snapshotRing() {
		if ev.GetSeq() < fromSeq {
			continue
		}
		// INCLUSIVE of fromSeq. When the caller raised it to a clear or a
		// compaction, that event is the FIRST thing replayed: a frontend that
		// discards its history at the floor and never receives the clear or the
		// compaction has nothing to draw and no reason to have discarded.
		//
		// Every retained event is offered to the curator, which decides what
		// carries conversation content. Filtering to vendor payloads here is
		// what kept the first-class clear and compaction out of every replay.
		c.pushConversation(ev, false)
	}
	// Replay the retained permission items too: they carry no store seq (they
	// are daemon-composed, not store events), so a pending or resolved
	// permission is re-presented on reconnect regardless of fromSeq. Idempotent
	// by uuid (the permission request_id) — a re-push REPLACES.
	for _, item := range c.snapshotPermItems() {
		c.pushLocalItem(item)
	}
	// Same for the retained failure cards (F4): they carry no store seq
	// either, and a reconnecting frontend that could not see WHY its
	// workspace is off-green is the gap the cards exist to close.
	for _, item := range c.snapshotFailItems() {
		c.pushLocalItem(item)
	}
	// And the prompt receipts the durable transcript has not claimed yet
	// (promptecho.go). Same reasoning once more: no store seq, so no fromSeq
	// covers them — and a frontend that reconnects between a submit and its
	// transcript line would otherwise find the user's own prompt missing.
	for _, item := range c.snapshotEchoes() {
		c.pushLocalItem(item)
	}
	// And the session-command invocations (sessioncommand.go). Same reasoning a
	// third time, with one addition: a session command earns no prompt receipt
	// by design, so this item is the ONLY thing that will ever tell a
	// reconnecting frontend the command was run.
	for _, item := range c.snapshotCommandItems() {
		c.pushLocalItem(item)
	}
	return c.ringFloor()
}

// ringFloor reports the oldest store seq the retained ring still holds — the
// first seq a resync replay can cover — and whether the ring holds one at all.
//
// ok=false means the ring carries no seq-bearing event: it was emptied by the
// cap, or (the case that matters) this is a freshly restarted daemon whose ring
// has not filled yet. A caller cannot derive the floor from the ring then, and
// must fall back to the DURABLE last_seen_seq — see Manager.Resync.
//
// Reporting 0 in that case would claim the ring covers all of history, which is
// the silent answer this whole mechanism exists to replace.
func (c *consumer) ringFloor() (uint64, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, ev := range c.ring {
		if seq := ev.GetSeq(); seq > 0 {
			return seq, true
		}
	}
	return 0, false
}

// pushPermission retains and pushes a permission ConversationItem, keyed by its
// uuid (the permission request_id) so a resync replays the latest resolution.
// A same-uuid push REPLACES the retained item, tracking the resolution
// lifecycle (PENDING -> ALLOWED/DENIED/ABANDONED). This is the S8 permission
// surface pushed through the NORMAL retained pusher path so resync replays it.
func (c *consumer) pushPermission(item *frontendv1.ConversationItem) {
	c.mu.Lock()
	if c.permItems == nil {
		c.permItems = map[string]*frontendv1.ConversationItem{}
	}
	if _, seen := c.permItems[item.GetUuid()]; !seen {
		c.permOrder = append(c.permOrder, item.GetUuid())
	}
	c.permItems[item.GetUuid()] = item
	c.mu.Unlock()
	c.pushLocalItem(item)
}

// pushLocalItem wraps a single DAEMON-COMPOSED item (a permission, a failure
// card, a prompt receipt) in a ConversationDelta and pushes it. No store seq:
// through_seq stays 0, because nothing in the store produced it.
//
// PROVENANCE IS THE LIVE VERDICT HERE, not a ledger lookup, and the difference
// from pushConversation is the item's origin rather than an inconsistency. A
// daemon-composed item is composed NOW, by this daemon, so whether the merge
// owns the shim at this instant IS its provenance — and a permission card
// carries no timestamp to look one up with anyway. Every caller retains the
// stamped item (permItems, failItems, echoes), so a resync replays the verdict
// that was made rather than deriving a new one.
func (c *consumer) pushLocalItem(item *frontendv1.ConversationItem) {
	if !c.stampLocalItemProvenance(item) {
		return
	}
	c.push.PushConversationDelta(&frontendv1.ConversationDelta{
		Workspace: c.workspace,
		Fence:     c.fence(),
		Items:     []*frontendv1.ConversationItem{item},
	})
}

// pushReplayedItem pushes ONE daemon-composed durable item during a durable
// replay — a prompt receipt, or a fenced session's standing terminal failure
// card — and reports whether it went.
//
// IT IS NOT pushLocalItem, and the difference is the provenance rule. A local
// item is composed NOW, so the live lease state is its provenance; a replayed
// item was composed by a daemon that no longer exists, at an instant the
// record remembers, so its verdict must come from the merge lease's DURABLE
// LEDGER at that instant — the same rule pushConversation applies to every
// other replayed item. Reading the live lease instead would rewrite a merge's
// prompt as the user's, or the reverse, purely on the basis of what happens to
// be leased when the frontend reconnects.
//
// Nothing is retained: this consumer is the throwaway one a durable replay runs
// through, and the record in the state store is the thing that persists.
func (c *consumer) pushReplayedItem(item *frontendv1.ConversationItem) bool {
	cd := &frontendv1.ConversationDelta{
		Workspace: c.workspace,
		Fence:     c.fence(),
		Items:     []*frontendv1.ConversationItem{item},
	}
	if !c.stampConversationProvenance(cd) {
		return false
	}
	c.push.PushConversationDelta(cd)
	return true
}

// pushFailure retains and pushes a system-failure ConversationItem under uuid,
// on the same retained-and-replayed path permissions use (F4).
//
// Keying by uuid is what makes a WINDOW-shaped failure one card: the closing
// edge REPLACES the retained item with its resolved twin, so a resync replays
// the settled card rather than re-opening an alarm about something that
// already ended.
func (c *consumer) pushFailure(uuid string, failure *frontendv1.FailureCardView) {
	c.retainFailure(uuid, failure)
	// THE TWO EDGES ARE NOT THE SAME NEWS, and recording both at warn made the
	// good one as loud as the bad one: every store bounce that resolved itself
	// still left a "system failure ... resolved=true" warn per session, which is
	// how a self-healing fleet-wide event read as a fleet-wide alarm. Opening a
	// card is a warning; SETTLING one is the report that it ended.
	//
	// The record itself is unchanged in wording and in identity, so the open
	// edge is byte-identical to what it always was.
	emit := c.warn
	if errclass.IsResolved(failure) {
		emit = c.logf
	}
	emit("session-controller: system failure session=%s uuid=%s type=%s resolved=%v: %s",
		c.sessionID, uuid, failureType(failure), errclass.IsResolved(failure), failure.GetDetail())
	c.pushLocalItem(c.retainedFailure(uuid))
}

// retainFailure is pushFailure without the record: it retains the card and
// nothing else.
//
// It is split out because a card's edges are not all the same NEWS. Opening one
// is the warn pushFailure emits; closing a WITHHELD one because the live
// successor wired is ordinary progress, and routing that through pushFailure
// would put a second, differently-worded record on the same edge — the very
// double-record shape the replayed pair already had.
func (c *consumer) retainFailure(uuid string, failure *frontendv1.FailureCardView) {
	// The ENVELOPE is the card's only address now. It used to be repeated onto
	// the failure itself so an out-of-feed surface could name the card; the
	// contract carries that address as FailureCardRef instead, so there is one
	// copy of it and nothing to keep in step with the envelope.
	item := &frontendv1.ConversationItem{
		Uuid: uuid,
		TsMs: c.now(),
		Item: &frontendv1.ConversationItem_FailureCard{FailureCard: failure},
	}

	c.mu.Lock()
	if c.failItems == nil {
		c.failItems = map[string]*frontendv1.ConversationItem{}
	}
	if _, seen := c.failItems[uuid]; !seen {
		c.failOrder = append(c.failOrder, uuid)
	}
	c.failItems[uuid] = item
	// A card pushed on ANY other path supersedes a withheld one under the same
	// uuid, so the live successor's readiness may no longer resolve it. The
	// withhold path re-arms this immediately afterwards, which is why the delete
	// is unconditional here rather than conditional on the caller.
	delete(c.withheldCards, uuid)
	c.mu.Unlock()
}

// retainedFailure reads back the retained ConversationItem for uuid under the
// lock, so a push cannot race a concurrent retention of the same card.
func (c *consumer) retainedFailure(uuid string) *frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.failItems[uuid]
}

// pushWithheldFailure retains and pushes a card the WITHHOLD arms produced,
// recording it as those arms record everything: on the ordinary channel, as a
// replay being classified rather than a fresh anomaly.
//
// The uuid is remembered so the bring-up gate can settle the card once the LIVE
// query for this session is genuinely up. Without that, an unresolved
// degradation card for a RETIRED query sat on a healthy session forever — the
// row is durable, so it came back at every boot and never had a closing edge,
// which misrepresents a session that is working.
func (c *consumer) pushWithheldFailure(uuid string, failure *frontendv1.FailureCardView) {
	c.retainFailure(uuid, failure)
	c.logf("session-controller: system failure session=%s uuid=%s type=%s resolved=%v origin=withheld_replay: %s",
		c.sessionID, uuid, failureType(failure), errclass.IsResolved(failure), failure.GetDetail())
	c.mu.Lock()
	if c.withheldCards == nil {
		c.withheldCards = map[string]struct{}{}
	}
	c.withheldCards[uuid] = struct{}{}
	c.mu.Unlock()
	c.pushLocalItem(c.retainedFailure(uuid))
}

// resolveWithheldDegradations settles every card the WITHHOLD arms retained,
// now that the LIVE query for this session has reached ShimReady and wired.
//
// THIS IS THE SAME SHAPE AS THE SUPERSEDED-DEATH RESOLUTION (server/
// deathresolve.go), and for the same reason. Both cards are WINDOW-shaped
// and were recorded as if they were EVENT-shaped: a retired query's death is a
// true account of something that happened, and it stops describing this session
// the moment a live query genuinely has it — not one edge before. The supersede
// resolves at SessionOperational because the successor does not exist any
// earlier; this resolves at the same edge because "the live query is up" is not
// a fact until the bring-up gate closes.
//
// It is a RESOLUTION, never a deletion: the card keeps its uuid, its type and
// its whole source detail, and is re-sent with resolved_at_ms stamped, so the
// history stays queryable and a resync replays the settled card rather than
// re-opening the alarm. A LIVE degradation is untouched — pushing one on any
// other path drops the uuid from the withheld set (retainFailure), so this can
// only ever settle a card the withhold arms themselves put up.
func (c *consumer) resolveWithheldDegradations(reason string) {
	c.mu.Lock()
	uuids := make([]string, 0, len(c.withheldCards))
	for uuid := range c.withheldCards {
		uuids = append(uuids, uuid)
	}
	items := make(map[string]*frontendv1.FailureCardView, len(uuids))
	for _, uuid := range uuids {
		items[uuid] = c.failItems[uuid].GetFailureCard()
	}
	c.withheldCards = nil
	c.mu.Unlock()
	// Sorted so a session holding more than one withheld card records and
	// pushes them in a stable order rather than Go's map order.
	sort.Strings(uuids)
	for _, uuid := range uuids {
		failure := items[uuid]
		if failure == nil {
			// The retained item went away under us (a rotation purge). Loud,
			// because a withheld uuid with no card behind it means the two
			// structures disagreed, and silently skipping would hide that.
			c.warn("session-controller: withheld degradation card RESOLUTION SKIPPED session=%s uuid=%s reason=%s branch=no_retained_card — the card is gone, so nothing could be settled",
				c.sessionID, uuid, reason)
			continue
		}
		c.settleRetainedCard(uuid, failure, reason, "withheld degradation", "live_successor_healthy")
	}
}

// settleRetainedCard stamps ONE retained card resolved and re-publishes it.
//
// IT IS THE ONLY WAY A CARD IS SETTLED IN THIS PACKAGE, because every
// resolution edge owes the same four things and a second copy of them would
// drift: the card keeps its uuid, its type and its whole source detail; it is
// re-sent with resolved_at_ms stamped rather than deleted, so the history stays
// queryable and a resync replays the SETTLED card instead of re-opening the
// alarm; the retention is updated so a later snapshot carries the settled copy;
// and the settlement is logged with the reason that closed it.
//
// An ALREADY-RESOLVED card is a no-op. Two recovery events can legitimately
// race for one card, and the loser must not re-stamp a later instant onto a
// settlement that already happened.
func (c *consumer) settleRetainedCard(uuid string, failure *frontendv1.FailureCardView, reason, label, decision string) {
	if errclass.IsResolved(failure) {
		return
	}
	resolved := proto.Clone(failure).(*frontendv1.FailureCardView)
	errclass.Resolve(resolved, c.now())
	c.retainFailure(uuid, resolved)
	c.logf("session-controller: %s card RESOLVED session=%s uuid=%s type=%s resolved_at_ms=%d reason=%s decision=%s",
		label, c.sessionID, uuid, failureType(resolved), errclass.ResolvedAtMs(resolved), reason, decision)
	c.pushLocalItem(c.retainedFailure(uuid))
}

// bounceWindowCardTypes are the card classes a planned bounce mints and a
// successful bring-up disproves.
//
// EACH ONE IS WINDOW-SHAPED AND WAS RECORDED AS IF IT WERE EVENT-SHAPED. "The
// conversation could not be resumed" and "the session controller was
// unreachable" are true accounts of something that happened during a bounce,
// and they stop describing this session the moment a live shim wires. Left
// unresolved they lingered with resolved_at_ms=0 and resurfaced hours later on
// every reconnect, because a durable card with no closing edge never acquires
// one.
//
// It is a CLOSED list rather than "resolve everything". A card that a healthy
// bring-up does not disprove — a tool failure, a permission denial, a vendor
// error inside a turn — is still true afterwards, and settling it because the
// shim came up would be the daemon claiming something it did not verify.
var bounceWindowCardTypes = []errclass.Type{
	// "the Claude conversation could not be resumed" — the bring-up this card
	// describes is the one that has now succeeded.
	errclass.TypeSessionResumeFailed,
	// "a bring-up ended without wiring, and the fresh retry ended the same
	// way" — disproved by a wire, which is the event this resolution runs on.
	errclass.TypeSessionStartFailed,
	// "the establishment deadline elapsed with no verdict at all" — a wait that
	// ran out, whose verdict has now arrived.
	errclass.TypeSessionNotEstablished,
}

// resolveBounceWindowCards settles the cards a bounce put up, now that this
// session has a live shim behind it.
//
// THE EDGE IS THE RECOVERY EVENT, which is the whole point of goal E: a card
// minted because the daemon was going away is closed by the daemon having come
// back, rather than by a timer or by the user dismissing it.
func (c *consumer) resolveBounceWindowCards(reason string) {
	wanted := make(map[errclass.Type]struct{}, len(bounceWindowCardTypes))
	for _, t := range bounceWindowCardTypes {
		wanted[t] = struct{}{}
	}
	c.mu.Lock()
	// Snapshot in FIRST-SEEN order, so a session holding several settles them
	// in the order they were raised rather than in Go's map order.
	type candidate struct {
		uuid    string
		failure *frontendv1.FailureCardView
	}
	var candidates []candidate
	for _, uuid := range c.failOrder {
		failure := c.failItems[uuid].GetFailureCard()
		if failure == nil {
			continue
		}
		kind, ok := errclass.TypeOf(failure.GetKind())
		if !ok {
			continue
		}
		if _, want := wanted[kind]; !want {
			continue
		}
		candidates = append(candidates, candidate{uuid: uuid, failure: failure})
	}
	c.mu.Unlock()
	for _, item := range candidates {
		c.settleRetainedCard(item.uuid, item.failure, reason, "bounce window", "live_shim_wired")
	}
}

// snapshotFailItems returns the retained failure items in first-seen order,
// taken under the lock so a concurrent pushFailure cannot race the read.
func (c *consumer) snapshotFailItems() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.failOrder))
	for _, id := range c.failOrder {
		out = append(out, c.failItems[id])
	}
	return out
}

// snapshotPermItems returns the retained permission items in first-seen order,
// taken under the lock so a concurrent pushPermission cannot race the read.
func (c *consumer) snapshotPermItems() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.permOrder))
	for _, id := range c.permOrder {
		out = append(out, c.permItems[id])
	}
	return out
}

// permissionItem composes a permission ConversationItem: the request plus its
// resolution, keyed by the request_id as the item uuid (the reconciliation key
// frontends replace on). denyMessage is set only on RESOLUTION_DENIED.
func permissionItem(req *corev1.PermissionRequest, res corev1.PermissionItem_Resolution, denyMessage string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{
		Uuid: req.GetRequestId(),
		Item: &frontendv1.ConversationItem_Permission{Permission: &corev1.PermissionItem{
			Request:     req,
			Resolution:  res,
			DenyMessage: denyMessage,
		}},
	}
}

// stateKind names a lifecycle event's payload for logging.
func stateKind(ev *corev1.Event) string {
	switch ev.GetPayload().(type) {
	case *corev1.Event_SessionStarted:
		return "session_started"
	case *corev1.Event_SessionEnded:
		return "session_ended"
	case *corev1.Event_TurnStarted:
		return "turn_started"
	case *corev1.Event_TurnEnded:
		return "turn_ended"
	case *corev1.Event_TurnClaimBridge:
		return "turn_claim_bridge"
	case *corev1.Event_TaskStarted:
		return "task_started"
	case *corev1.Event_TaskProgress:
		return "task_progress"
	case *corev1.Event_TaskEnded:
		return "task_ended"
	default:
		return "other"
	}
}
