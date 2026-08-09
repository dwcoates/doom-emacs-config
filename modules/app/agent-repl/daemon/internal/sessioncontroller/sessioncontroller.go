// Package sessioncontroller owns the daemon's session controllers. Each session
// controller binds a session's UDS claude-shim (via internal/shimclient) to the daemon's resolved
// surfaces — lifecycle events to the SSM, conversation/typing/task/degraded
// events to the frontend push channel, and the canUseTool permission round-trip
// to the frontend command surface. It is the consumption + prompt half of the
// agent-shim cutover (design §4, §9.1): the daemon consumes exactly one
// totally-ordered stream per session and renders nothing itself.
//
// Bring-up is LAZY and reattach-first: the first frontend command for a
// workspace resolves it to a live session, reattaches to that session's shim if
// it is still listening (the shim outlives a dead daemon, §4.4) or spawns a
// fresh UDS shim, then runs a shimclient whose sinks are this package's
// per-session consumer. A UDS disconnect never ends the turn; the client
// reattaches and replays from the daemon-tracked last_seen_seq.
//
// # The Locked naming convention
//
// A `…Locked` suffix on a Manager method means the CALLER ALREADY HOLDS m.mu
// and the method must not touch it — the requires-held reading, which is the
// only one used here. A method that takes m.mu itself carries NO suffix.
//
// The distinction matters because both kinds exist on the same receiver and
// mixing the readings is a self-deadlock or an unguarded read, neither of which
// the compiler catches. So the rule is one-directional and absolute: seeing
// `Locked` means "I am inside the critical section", never "I will enter one".
package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"

	"google.golang.org/protobuf/types/known/structpb"
)

// SessionRegistrar persists the durable CLI session uuid a session's
// SessionStarted carries (vendor_session_id), so --resume and cross-restart
// rehydration keep working after the L2 stdio plane that used to write it is
// gone. Bound in main to a registry-writing adapter; nil disables the write.
// # ADOPTION IS EAGER, AND THE DISK IS CHECKED AT USE
//
// These writes used to take a `durable` flag and REFUSE a first adoption
// without it — the daemon's evidence that a turn had run and the vendor had
// therefore written the transcript this uuid names. The incident behind that
// gate was real: a session created, never prompted, then dead, leaving a record
// pointing at a transcript that never existed, so every later bring-up ran
// `claude --resume <uuid>`, the CLI exited 1, and the workspace sat in
// `starting` with nothing to explain it.
//
// THE GATE MOVED RATHER THAN DISAPPEARED. server.ConversationResolver now stats
// the transcript when it resolves a resume target and skips any conversation
// the vendor never wrote, so the same disk is consulted — at the moment the
// answer is used, instead of the moment it was first guessed. That ordering is
// strictly stronger, because a uuid can stop being resumable after adoption and
// only a check at use will notice.
//
// Holding also had a cost that was not obvious until it bit: while the registry
// withheld the uuid, the shim and the webapp both knew it, so every client log
// failed identity validation against the registry and nacked. A workspace
// opened and left unprompted produced tens of thousands of rejected log
// records. An authority that knowingly reports something it believes to be
// false, even temporarily, will be disagreed with by everyone who knows better.
type SessionRegistrar interface {
	// ClaudeSessionIDChanged persists claudeSessionID, reporting whether the
	// write landed.
	ClaudeSessionIDChanged(sessionID, claudeSessionID string) (adopted bool)
	// AdoptVendorSessionID adopts claudeSessionID as the session's vendor uuid
	// and reports whether that ROTATED an already-adopted, DIFFERENT one, plus
	// what it replaced and whether anything was written at all.
	//
	// A rotation retires the conversation's store seq space and starts a fresh
	// one at 1, so the adoption and the CURSOR RESET that must accompany it are
	// one indivisible act rather than two writes. Splitting them is not merely
	// untidy: the registry hydrates a record's cursors up from the checkpoint
	// filed under its CURRENT uuid on every write, so a reset landing while the
	// old uuid still stood would be undone before the new uuid was recorded.
	//
	// Idempotent for an unchanged uuid: rotated=false, nothing reset.
	AdoptVendorSessionID(sessionID, claudeSessionID string) (rotated bool, previous string, adopted bool)
	// QueuedPromptsChanged persists the prompts the daemon is currently
	// HOLDING for a session (E4). A daemon that dies mid-queue would otherwise
	// lose them with no trace; the record is the honest one.
	QueuedPromptsChanged(sessionID string, queued []registry.QueuedPrompt)
	// BackfillStateChanged persists the never-blue backfill signal (F2) and
	// re-pushes the session's SessionView. Called only on a real transition,
	// so a steady stream of transcript lines writes the record once.
	BackfillStateChanged(sessionID, state string)
	// SessionDied marks the session's record terminal with the reason its
	// death carried (F4), and re-pushes the SessionView so the dead-state
	// card gets its account. Before this the shim-death path wrote nothing,
	// leaving the SSM's dead state and the record's death reason on two
	// disconnected axes.
	SessionDied(sessionID, reason string)
	// SessionOperational reports that workspace's sessionID has reached
	// OPERATIONAL — the bring-up gate closed, the session is genuinely
	// driveable. It is the resolving edge of every WINDOW-shaped death already
	// recorded against that workspace's earlier sessions: a supersede says "a
	// newer session took this workspace", and this is the moment that newer
	// session demonstrably has it. The registrar stamps and re-pushes; nothing
	// here waits on it.
	SessionOperational(workspace, sessionID string)
	// SessionModelObserved persists the model a LIVE session reports itself to
	// be running, which is the only model a respawn should ever be pinned to.
	// The requested-at-create model is a seed and nothing more.
	SessionModelObserved(sessionID, model string)
}

// ModelCatalogRegistrar receives the live SDK's model menu. It is separate
// from SessionRegistrar because query capability is not transcript identity.
type ModelCatalogRegistrar interface {
	SessionModelCatalogObserved(sessionID string, models []*corev1.ModelOption) error
}

// TerminalAccountingObserver receives the durable terminal-accounting edge.
// It is separate from SessionRegistrar because token aggregates are derived
// from the accounting store rather than registry state.
type TerminalAccountingObserver interface {
	TerminalAccountingPersisted(sessionID string)
}

type HistoricalTokenUtilizationObserver interface {
	HistoricalTokenUtilizationPersisted(sessionID string)
}

// SpawnResult reports the vendor conversation a successful spawn resumed.
// The spawner owns durable identity validation before it returns this result:
// a recorded conversation must resume exactly or the bring-up fails.
type SpawnResult struct {
	// Resumed is the vendor conversation uuid this spawn actually resumed, or
	// "" for a fresh start.
	Resumed string
}

// Spawner makes sure a session has exactly one live shim: it leaves an existing
// one alone (connected, or merely holding its session lock — the shim outlives
// a dead daemon, §4.4) or spawns a fresh one via ShimUDSArgv. The
// concrete impl lives in the server package (it owns the liveness checks and
// the spawn plumbing); injected here so the session controller stays IO-narrow and testable.
type Spawner interface {
	EnsureShim(ctx context.Context, sessionID string) (SpawnResult, error)
	// StopShim asks the session's shim to stop cleanly (the daemon SIGTERMs
	// its child shim on hibernation, §4.4 redefined). A stop failure is
	// surfaced, never swallowed.
	//
	// hintPID is the pid the shim announced on its ShimHello, or 0 when this
	// daemon has never seen one. It is what makes a shim the daemon did NOT
	// spawn stoppable at all: a survivor of a previous daemon leaves no
	// process handle behind, so StopShim was a permanent no-op for exactly the
	// shims a restart-driven bounce needs to reach. It is consulted ONLY when
	// there is no handle, and only while the connection that carried it is
	// live — which is what makes killing by pid safe here rather than a
	// pid-reuse hazard.
	//
	// by NAMES THE STOP, and it is a required argument rather than a
	// convention: the implementation refuses an unattributed one outright. It
	// is rendered from the closed cause vocabulary (stopcause.go) at the one
	// funnel that reaches this method, so the shim's own death record and the
	// daemon's log line come from a single table.
	StopShim(sessionID string, hintPID int32, by shim.Stop) error
}

// SessionLocator maps a workspace to the live session id bound to it. The
// concrete impl reads the daemon's session registry (the non-terminal record
// whose cwd is the workspace). ok=false when the workspace has no live session.
type SessionLocator interface {
	Locate(workspace string) (sessionID string, ok bool)
}

// FileDiagnosticPersister owns workspace-specific sidecar diagnostics after
// the session controller resolves the session to its authoritative workspace.
type FileDiagnosticPersister interface {
	PersistFileDiagnostic(workspace, agentReplSessionID string, ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error
}

// sessionClient is the slice of *shimclient.Client the session controller drives. An
// interface so the manager's routing is unit-testable with a fake.
type sessionClient interface {
	Run(ctx context.Context) error
	AwaitReady(ctx context.Context) error
	// Health proves the already handshaked shim's own dependency boundary.
	// It MUST NOT cause a lazy bring-up; session readiness is false until the
	// existing live session controller can answer this probe.
	Health(ctx context.Context, requestID string) (*corev1.HealthStatus, error)
	// SubmitPrompt hands one prompt to the shim under requestID, which the
	// shim adopts as the turn_id of every boundary the prompt produces. A
	// caller whose own bookkeeping is keyed by that identity — the keep-alive
	// ping — passes it; an empty id is minted by the client.
	SubmitPrompt(ctx context.Context, requestID, text, origin, permissionMode string, promptOrigin corev1.PromptOrigin) error
	// Interrupt returns the shim's own verdict on what the stop did, which is
	// the only place that verdict is observable.
	//
	// originRequestID names WHO ORDERED the stop, in that caller's own
	// vocabulary, and travels only so the log can correlate the exchange back
	// to it: the wire carries a daemon-minted control id that appears nowhere
	// in any caller's records.
	Interrupt(ctx context.Context, originRequestID string) (corev1.InterruptOutcome, error)
	// UnpinAccountingTurn releases the durable-cursor hold a turn's start took,
	// for a turn the daemon closed WITHOUT a TurnEnded. Only a stream TurnEnded
	// releases a pin otherwise, so a synthesized close would freeze the cursor
	// at that turn forever.
	UnpinAccountingTurn(turnIDs ...string)
	SetModel(ctx context.Context, model string) (string, error)
	// Replay asks the shim for a bounded slice of persisted history, streaming
	// it to onEvent. Its events arrive over the wire as ReplayEvent, a
	// different type from live Events, which is what keeps replayed history
	// out of the SSM/task/progress planes structurally (repull.go).
	Replay(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (shimclient.ReplayResult, error)
}

// Config assembles a Manager. Every collaborator is injected so the session controller is
// testable and free of the daemon's HTTP surface.
type Config struct {
	// Push is the frontend server (conversation/typing/task/degraded/state).
	Push Pusher
	// SSM applies lifecycle events. Required.
	SSM StateApplier
	// Progress resolves the progress footer (F1) from the same event stream
	// plus the daemon-local pending-permission and queue counts. Nil disables
	// the feed (the footer simply never populates) rather than nil-derefing.
	Progress ProgressResolver
	// Spawner reattaches-or-spawns a session's UDS shim. Required.
	Spawner Spawner
	// Locator resolves a workspace to its live session id. Required.
	Locator SessionLocator
	// SeqStore persists last_seen_seq (RegistrySeqStore). Required.
	SeqStore shimclient.SeqStore
	// PermissionModes reads each session's stored permission posture, which
	// the bring-up gate carries to the shim on DaemonHello (RegistryModeStore).
	// Nil resolves every session to protocol.DefaultSessionPermissionMode —
	// never to an ungated mode — so the omission is safe rather than silent.
	PermissionModes shimclient.ModeStore
	// ClearCompactStore persists the newest clear-or-compaction seq — the
	// frontend replay floor (RegistrySeqStore again). Required: a session controller that
	// observed a clear or a compaction and had nowhere to record it would serve
	// the next resync the very history that clear or compaction discarded, which
	// is the failure the floor exists to prevent.
	ClearCompactStore ClearCompactStore
	// DurableHistory serves a resync for a workspace with NO live session
	// controller, straight from the store the shim itself reads
	// (durablereplay.go). Nil makes such a resync a LOUD failure rather than
	// the quiet empty feed it used to be: a frontend cannot tell silence from
	// an empty conversation, so the daemon must say which one it means.
	DurableHistory DurableHistorySource
	// PromptReceipts persists the DURABLE half of every prompt receipt: the
	// record written at acceptance, before the receipt bubble is pushed, and
	// retired once the conversation itself carries the prompt (promptecho.go,
	// durablereplay.go).
	//
	// Nil is a session controller with the durable receipt guarantee OFF — a
	// focused harness that does not exercise it. Production always wires one,
	// and every site that would have written or read a record says out loud
	// that it did not, so the absence is never mistaken for "there were no
	// receipts". It is deliberately not Required: a Manager built without one
	// still serves prompts correctly, it merely cannot testify to a prompt
	// across its own death.
	PromptReceipts PromptReceiptStore
	// TurnAccountings durably records resolved terminal accounting before the
	// terminal ResultMessage reaches any frontend. Production always supplies it.
	TurnAccountings TurnAccountingStore
	// HistoricalUsage normalizes rootless, untimed file-plane usage
	// into the session aggregate ledger before its conversation item is visible.
	HistoricalUsage HistoricalTokenUtilizationStore
	// DaemonVersion / ProtocolVersion travel in DaemonHello; ProtocolVersion
	// must equal the shim's ("1").
	DaemonVersion   string
	ProtocolVersion string
	// MergeResolutionTurnBound bounds one shim-driven merge-conflict resolution
	// turn (mergeresolve.go). Zero takes mergeResolutionTurnBound, which is the
	// only value production uses; it is injectable so a harness can prove the
	// unfinished-turn error without waiting out a bound sized for real conflict
	// resolution.
	MergeResolutionTurnBound time.Duration
	// MergeResolutionTurnBindBound bounds the FIRST phase of that wait: how long
	// a submitted merge prompt may go without a turn STARTING for it
	// (mergeresolve.go). Zero takes mergeResolutionTurnBindBound. It is a
	// separate knob from MergeResolutionTurnBound because a harness proving the
	// never-started failure and one proving the never-ended failure must shrink
	// different clocks — collapsing them would make the two indistinguishable in
	// exactly the tests that exist to tell them apart.
	MergeResolutionTurnBindBound time.Duration
	// ShimBuildSHA reports the build identity of the shim bundle this daemon
	// would spawn TODAY — the `dist/.built-sha` stamp beside the entrypoint.
	// A shim announcing a different identity is running superseded code and is
	// bounced onto the current bundle (buildrefresh.go).
	//
	// Nil, or an empty return, disables the refresh: an unknown identity is
	// not a mismatch, and treating it as one turns a missing stamp into a
	// bounce loop.
	ShimBuildSHA func() string
	// Registrar persists SessionStarted.vendor_session_id (the CLI session uuid)
	// through to the durable registry record; nil disables the write.
	Registrar SessionRegistrar
	// ModelCatalogs records the live query-owned menu for frontend rendering.
	// Nil is allowed only in focused controller tests.
	ModelCatalogs ModelCatalogRegistrar
	// Hibernations is the durable half of hibernation and of the keep-alive
	// policy's one input (hibernation.go). Nil makes HibernateWithCause a loud
	// refusal rather than a sleep nothing records — an unrecorded sleep is
	// revived implicitly by the next daemon, which is precisely the silent
	// un-sleeping the durable flag exists to prevent.
	Hibernations HibernationRegistrar
	// LegacyTurnEnds resolves the durable last-turn-end for a session that
	// predates the keep-alive branch and therefore has none, by the SAME rule
	// the idle sweeper applies (server.stampLegacyTurnEnd). It exists so the
	// staleness check taken at prompt acceptance and at bring-up
	// (hibernateIfStale) measures such a session exactly as the sweeper does,
	// rather than carrying a second copy of the stamping policy.
	//
	// Nil leaves an undated session outside that check — loudly — and with the
	// sweeper alone, which is the pre-existing behavior rather than a new one.
	LegacyTurnEnds LegacyTurnEndStamper
	// KeepAlive is the resolved cache keep-alive policy. The zero value takes
	// keepalive.DefaultConfig, because a zero TTL would read every session as
	// already cache-expired.
	KeepAlive keepalive.Config
	// KeepAliveWindows is the durable ledger of when the daemon was pinging,
	// and the sole evidence the conversation exclusion decides on
	// (keepaliveexclude.go). Nil is the exclusion OFF: keep-alive turns would
	// render as ordinary conversation, which is why every site that would have
	// used it says so rather than failing quietly.
	KeepAliveWindows KeepAliveWindowLedger
	// VendorSessions performs the rewind's ATOMIC registry flip. It is the same
	// object as Registrar in production; it is a separate field because the
	// rewind needs exactly one method and a harness should be able to supply
	// just that one.
	VendorSessions VendorSessionAdopter
	// VendorSessionOf reads the vendor conversation uuid a session currently
	// resumes, which is the transcript the rewind truncates. Nil makes a rewind
	// impossible rather than guessed at.
	VendorSessionOf func(sessionID string) (string, bool)
	// Logf is the daemon logger. Nil discards.
	Logf func(string, ...any)
	// Warnf is the daemon logger's WARN channel, for a record that accompanies
	// a regression the user can see — degraded accounting, a failure card, a
	// dropped or rejected event — but that is not itself a hard fault. At info
	// such a record is indistinguishable from routine progress and invisible
	// to a level filter, which is the whole reason this channel exists beside
	// Logf.
	//
	// Nil falls back to Logf, so an unwired warn channel still records the
	// event (at Logf's level) rather than losing it.
	Warnf func(string, ...any)
	// Errorf is the daemon logger's ERROR channel, for a hard failure rather
	// than a degradation. Nil falls back to Warnf.
	Errorf func(string, ...any)
	// Classifier judges prompts queued during a running turn (E4). Nil leaves
	// the queue unclassified: entries are marked ERROR with that stated
	// reason and still delivered by the ordinary turn-end drain, so the
	// feature degrades to plain FIFO rather than silently pretending to have
	// judged anything.
	Classifier Classifier
	// ShutdownHolds is the durable ledger of prompts parked by a scheduled
	// shutdown's drain lease. Optional: a nil store means a parked prompt does
	// not survive the bounce that parked it, which is loud-logged at every
	// parking site rather than silently tolerated. Satisfied by
	// *statedb.ShutdownSchedules.
	ShutdownHolds ShutdownHoldStore
	// SessionConfigDir resolves a session's CLAUDE_CONFIG_DIR so the
	// classifier runs under the same account as the session it is about. Nil
	// leaves it empty, which inherits the daemon's own environment.
	SessionConfigDir func(sessionID string) string
	// Now is THE fleet's single clock authority, in unix milliseconds. It
	// stamps the queue (queued_at_ms), the keep-alive window ledger, and — the
	// reason it is exported — the hibernation transition's staleness re-check.
	//
	// That re-check re-derives idleness for a decision the SWEEPER took against
	// server.Config.Now. Two clocks for one policy is two authorities: under an
	// injected clock the sweeper measures hours idle while the gate measures
	// milliseconds and refuses every automatic hibernation as stale. Production
	// and every harness must therefore thread ONE clock into both fields.
	//
	// Nil defaults to wall clock.
	Now func() int64

	// WorkspaceLockHeld probes the kernel-enforced claim a live shim holds on a
	// WORKSPACE, which is the one fact that distinguishes "no shim" from "a
	// shim that has not dialled in yet" before anything is spawned
	// (survivingshim.go). An error from it means "I could not tell", and is
	// never read as free.
	//
	// Default = sessionlock.WorkspaceLockHeld
	WorkspaceLockHeld func(cwd string) (bool, error)

	// Source yields each session's shim connection: shims dial the daemon's
	// listening socket and the listener routes each connection to the client
	// that owns that session. Required.
	Source shimclient.ConnSource
	// FileDiagnostics persists sidecar file-plane evidence. It is required in
	// production and a received diagnostic fails loudly if it is absent.
	FileDiagnostics FileDiagnosticPersister

	// newClient is injected only by tests; production uses a real shimclient.
	newClient func(cfg shimclient.Config) sessionClient
	// newControllerGenerationID is injected only by tests. Production uses a
	// cryptographically random process-independent identity because generation
	// rows survive daemon restarts.
	newControllerGenerationID func() (string, error)
}

// Manager is the fleet of session controllers. It implements the frontend
// PromptRouter (SubmitPrompt/Interrupt/AnswerPermission) plus Resync.
type Manager struct {
	cfg  Config
	logf func(string, ...any)
	// warnf is the WARN channel described on Config.Warnf. It is never nil
	// after New: an unset Config.Warnf resolves to logf.
	warnf func(string, ...any)
	// errorf is the ERROR channel described on Config.Errorf. Never nil after
	// New: an unset Config.Errorf resolves to warnf.
	errorf func(string, ...any)
	reg    *permRegistry

	// shimStops is the SOLE holder of the wired spawner's stop half, taken off
	// it in New. cfg.Spawner keeps the bring-up half and refuses stops, so the
	// only way to kill a shim from this package is the teardown funnel that
	// reaches this gate (turnstop.go).
	shimStops *shimStopGate

	newClient func(cfg shimclient.Config) sessionClient
	// newControllerGenerationID mints the identity persisted on every
	// connectivity lifecycle and runtime-fault edge.
	newControllerGenerationID func() (string, error)
	// now is the queue's clock (queued_at_ms), injected by tests.
	now func() int64
	// workspaceLockHeld is the pre-spawn workspace-ownership probe
	// (survivingshim.go).
	workspaceLockHeld func(cwd string) (bool, error)

	// shutdownLease binds the daemon-global scheduled-shutdown drain lease
	// (shutdownlease.go). Late-bound because the engine takes this fleet as a
	// dependency, so it cannot be a Config field.
	shutdownLease shutdownLeaseBinding

	mu   sync.Mutex
	byWS map[string]*sessionController // workspace -> live session controller
	// parked is the boot-materialized drain-park ledger: workspace -> the
	// prompts a previous daemon parked whose session has NOT wired to this one
	// (parkedledger.go). A workspace is in exactly one of the two maps, and the
	// entries move from this one to the controller's own queue at wire time.
	parked map[string]*parkedSession
	// restoreTombstones is the entry ids cancelled while a drain-hold restore
	// is mid-flight, per workspace, so the restore's apply loop cannot re-add a
	// prompt the user took back from under its own stale row snapshot
	// (shutdownlease.go). Written and read only under mu; the zero value is
	// usable, so nothing has to construct it.
	restoreTombstones restoreTombstones
	// hibernating is the exclusive per-workspace claim one hibernation
	// transition holds (hibernation.go). It is what makes two racing causes
	// produce one transition and one durable account instead of two.
	hibernating map[string]bool
	// reviving is the exclusive per-workspace claim one revival holds
	// (revive.go). It is hibernation's claim mirrored: without it two
	// concurrent ReviveSessionCmds both submit `/compact` under one request id
	// and the second overwrites the first's completion waiter.
	reviving map[string]bool
	// keepAliveRewinds names, per WORKSPACE, the keep-alive ping turn whose
	// aftermath — the transcript rewind and the respawn behind it — is still
	// running. It is the SECOND half of the ping's continuous hold: the ping's
	// own claim (sessionController.keepAliveTurnID) is cleared at the turn's
	// end, and without this a prompt arriving in the gap between that clear and
	// the rewind's stop would start a real turn the rewind then SIGTERMs and
	// truncates away.
	//
	// It is keyed by workspace rather than held on the sessionController for
	// the reason the rewind exists at all: the rewind REPLACES the controller,
	// so a claim living on the retired one would evaporate exactly when the
	// respawned session starts accepting prompts again.
	keepAliveRewinds map[string]string
	lastCSID         map[string]string // session id -> last-persisted claude session uuid
	// shimPID is the pid each session's shim announced on its ShimHello. It is
	// the ONLY way to stop a shim this daemon did not spawn, and it is kept in
	// memory rather than persisted deliberately: it is trustworthy exactly
	// while the connection that carried it is live, and a pid outliving its
	// connection is a pid-reuse hazard rather than a stop handle.
	shimPID map[string]int32
	// bringUpFailures tracks each session's CONSECUTIVE resolved bring-up
	// failures and, once the give-up bound is reached, the PARK that bound
	// imposes (bringupescape.go). The park is a cooldown, never a wall: it
	// expires on its own so no workspace can be dead-ended by it.
	bringUpFailures map[string]*bringUpStreak
	// buildBounced remembers the sessions already bounced for a stale bundle,
	// so a shim that comes back still reporting a mismatched build (a bundle
	// whose identity cannot move, a stamp that is wrong) is loud ONCE instead
	// of bouncing forever. A session is entered here only after a bounce
	// SUCCEEDED: the latch means "this session has been refreshed", never "a
	// refresh was attempted for it".
	buildBounced map[string]bool
	// buildRefresh records the one stale-build restart for a session. Health
	// probes that were already bound to the retiring controller use this
	// generation-specific rendezvous to follow the intentional replacement;
	// arbitrary transport failures never acquire retry semantics.
	buildRefresh map[string]*buildRefreshState
	// interruptDrain overrides the teardown drain's interrupt bound. Zero means
	// the production constant; only a test assigns one, so the timeout branch
	// can be driven without a ten-second wait (see turnstop.go).
	interruptDrain time.Duration
	// repullWaitGraceOverride overrides the grace a serialized re-pull request
	// allows the in-flight one to unwind in, once that one's own deadline has
	// tripped. Zero means the production constant; only a test assigns one, so
	// the wedged-pull branch can be driven without a minute-long wait (see
	// repull.go).
	repullWaitGraceOverride time.Duration
	// survivingShimWaitOverride and survivingShimPollOverride override the
	// pre-spawn wait for a lock-holding shim to dial in (survivingshim.go).
	// Zero means the production constants; only a test assigns them, so the
	// expiry branch can be driven without a ten-second wait.
	survivingShimWaitOverride time.Duration
	survivingShimPollOverride time.Duration
	// reviveCompactBoundOverride overrides how long a compact-first revival's
	// detached completion wait allows the compaction. Zero means the production
	// constant; only a test assigns one, so the timeout branch can be driven
	// without a ten-minute wait (see revive.go).
	reviveCompactBoundOverride time.Duration
	closed                     bool
	rootCtx                    context.Context
	rootStop                   context.CancelFunc
	// exits counts every session-controller-exit goroutine (the tail of bringUp's `go
	// func`), so Close can JOIN them. Unjoined, that tail — which drains the
	// queue, publishes the empty view, and persists queued_prompts through the
	// registry — outlives Close and races whatever tears down after it; in the
	// e2e suite it recreated registry files inside a t.TempDir mid-RemoveAll,
	// which was the origin of the roving "directory not empty" teardown flake.
	exits sync.WaitGroup
}

// sessionController is one live session's in-memory control state.
type sessionController struct {
	sessionID string
	workspace string
	// generationID distinguishes this in-memory controller from every retired
	// controller for the same workspace and session.
	generationID string
	// resumedVendorSessionID records the exact durable conversation this
	// generation asked the spawner to resume. A transport failure may retry
	// only while preserving this identity.
	resumedVendorSessionID string
	client                 sessionClient
	consumer               *consumer
	cancel                 context.CancelFunc
	// controllerRegistrationRelease relinquishes the SSM-owned reservation
	// that excludes hibernation until this generation reaches operational or
	// exits. The closure is idempotent.
	controllerRegistrationRelease func()

	// Bring-up gate state (bringupescape.go), guarded by the manager mutex
	// except for faulted, which is a one-shot broadcast channel.
	//
	// wired is set the moment the handshake lands, and is what distinguishes a
	// shim that died BEFORE it was ever driveable — an escapable bring-up
	// failure — from one that degrades mid-session, which is an ordinary
	// degraded card and no business of the ladder's.
	wired bool
	// faultReason is the shim's own account of why its SDK died during
	// bring-up. It is the ONLY detail the daemon ever learns about that death:
	// the shim's exit carries nothing on the wire, and this DegradedState is
	// sent immediately before it.
	faultReason string
	// faultTermination preserves the typed lifecycle record that preceded the
	// degraded wake-up for an unexpected query termination. It is empty when
	// the shim could only report a generic pre-readiness SDK failure.
	faultTermination *frontendv1.QueryTerminationFailure
	// faulted is closed once, by the first bring-up fault, to wake the wait.
	faulted chan struct{}
	// buildRefreshStarted closes when this generation's ShimReady proves that
	// its bundle is stale and transfers bring-up ownership to a replacement.
	// Health probes select on this edge beside AwaitReady, so the readiness that
	// is deliberately withheld from the retired generation cannot strand them.
	buildRefreshStarted chan struct{}

	// Prompt-queue state (E4), guarded by the manager mutex.
	//
	// turn is the ONE record of whether a turn is in flight and which turn it
	// is (turnrecord.go). It tracks the OBSERVED boundary — the daemon's own
	// accept edge and the durable turn ledger's claims — rather than the SSM's
	// derived turn_active: the queue must act on what the session really
	// reported, at the moment it reported it, not on a resolved view of it.
	//
	// It is ONE field rather than an active flag beside an id because those two
	// were written at different edges, and every schedule that landed between
	// them broadcast a drain hold that said a turn was running and could not say
	// which. Only the validating transitions in turnrecord.go write it.
	turn  turnRecord
	queue promptQueue
	// paused stops the queue DRAINING while retaining every entry (I1). Set by
	// a user-commanded interrupt: the user asked for work to stop, and
	// delivering the next held prompt the moment the stopped turn ends would
	// start exactly the work they just stopped.
	//
	// NOT PERSISTED, matching what the queue already persists: the registry
	// record carries the held prompts themselves and nothing else about the
	// queue's posture (see publishQueueLocked). A daemon restart therefore
	// resumes draining, which is the same answer it gives for turnActive.
	paused bool
	// interruptedTurn marks the turn now in flight as one a user-commanded
	// stop was delivered to. Consumed at that turn's boundary, where it
	// decides whether a lone paused runner resumes the drain.
	interruptedTurn bool
	// pausedRunner marks the turn now in flight as the prompt that jumped the
	// paused queue and is running ALONE. Its clean end resumes the drain; its
	// interrupted end leaves the queue paused.
	pausedRunner bool
	// keepAliveTurnID is the in-flight cache keep-alive ping's turn id, empty
	// when none is running. It is the claim taken under the manager mutex by
	// the same acquisition that decided to ping, and it is what a real prompt
	// arriving mid-ping is held behind (the queue's keep-alive hold). Read and
	// written only under Manager.mu.
	keepAliveTurnID string
	// keepAlivePing is what the in-flight ping measured about the cache it was
	// sent to refresh, nil when no ping is running. It is acquired and released
	// with keepAliveTurnID, by the same acquisitions, because it has no meaning
	// apart from the claim: a measurement outliving its ping would be read at
	// the next ping's turn end as though it were that ping's own. Read and
	// written only under Manager.mu (keepalivecold.go).
	keepAlivePing *keepAlivePingMeasurement
	// daemonCompaction is the DAEMON-INITIATED compaction in flight for this
	// session — a warm pre-expiry one or a compact-first revival's own — and nil
	// when none is. It is what the cold-read alarm matches a terminal result
	// against, and what stops a warm compaction being submitted twice for one
	// cache window. Read and written only under Manager.mu (compactioncold.go).
	daemonCompaction *daemonCompaction
	// warmCompactAnchorMs is the durable last-turn-end the most recent warm
	// compaction ATTEMPT was decided against, zero when none has been made.
	//
	// It is the exactly-once guarantee for one cache window. The warm-compaction
	// arm is due across a whole span of elapsed idleness, which the idle sweeper
	// crosses many times; a successful compaction ends a turn and moves the
	// anchor forward on its own, but a failed one does not, and without this the
	// same failure would be re-attempted every tick until the cache died. Read
	// and written only under Manager.mu (warmcompact.go).
	warmCompactAnchorMs int64
	// lastContextInputTokens is the TOTAL input the session's most recent
	// terminal result presented to the model: the uncached buckets plus the
	// cache read. It is the only figure this daemon holds about how big the
	// standing conversation actually is, and it is what the warm-compaction size
	// floor is judged against — compacting a small conversation costs a
	// full-history model call and buys back nothing (warmcompact.go).
	//
	// Zero means NO RESULT HAS BEEN OBSERVED in this daemon's lifetime for this
	// session, which is an unknown and not a small conversation. Every unknown
	// answers none. Read and written only under Manager.mu.
	lastContextInputTokens int64
	// runningText is the prompt that started the turn now in flight, as far as
	// this daemon saw it. It is the classifier's "what is already running"
	// context, and is empty when the turn predates this daemon.
	runningText string
	// runningPermissionMode is the mode that turn was submitted under, kept
	// beside runningText because both describe the SAME turn and a resume that
	// carried only the text would put the turn back under a different mode
	// than the one it was cut from (mergelease.go, ResumeDisplacedTurn).
	runningPermissionMode string
	// queueMigrating marks this controller's queue as OWNED BY THE REWIND
	// ORCHESTRATOR rather than by this controller. It is set under the manager
	// mutex by the same acquisition that empties the queue into the
	// orchestrator's own slice, BEFORE the rewind stops the shim, so the exit
	// tail that stop causes cannot drop entries or persist nil over the durable
	// record of what is still owed. Cleared when the entries are re-parked.
	queueMigrating bool
	// phantomTurnClosed names the durable turn claims the shim handshake just
	// contradicted and the SSM synthesized an end for (phantomturn.go). It is
	// carried from the handshake to ShimReady, where the queue is released on
	// that synthesized boundary — the boundary no process is left to send.
	// Empty whenever nothing is owed.
	phantomTurnClosed []string

	// turnWaiters are the waits for a SPECIFIC turn to end (mergeresolve.go),
	// guarded by the manager mutex. Only merge.Coordinator's conflict-resolution
	// prompt arms one today: it is the one caller whose next action depends on
	// the agent having finished, rather than on the queue's active/idle edge.
	turnWaiters []*turnWaiter

	// repull is the below-floor history re-pull now running for this workspace,
	// or nil. Guarded by the manager mutex; it is what keeps two frontends
	// mounting at once from pulling the same history twice (repull.go).
	repull *repullState

	// rotEpoch counts the VENDOR SESSION ROTATIONS this session controller has seen. It is
	// the seq space's generation number: every seq the daemon holds for this
	// session is only comparable with another seq of the same epoch.
	//
	// It exists for the re-pull's coalescing rule. Two requests are coalesced
	// when the in-flight one's range covers the newcomer's, and that comparison
	// is arithmetic on seqs — which is meaningless across a rotation, where the
	// in-flight bounds were computed in a space that no longer exists.
	// Guarded by the manager mutex.
	rotEpoch uint64

	// pendingResync is a frontend resync whose store re-pull was interrupted by
	// the shim link going away, re-armed to run again once the shim reattaches.
	// nil when nothing is pending. Guarded by the manager mutex.
	pendingResync *pendingResync
	// resyncRetried is the ONE-SHOT budget for that re-arm: a second
	// consecutive lost link is reported to the frontend as the failure it is
	// rather than retried forever. Cleared by a resync that completes, and by a
	// rotation (which is a legitimate reason for a second interruption).
	// Guarded by the manager mutex.
	resyncRetried bool
}

func (d *sessionController) releaseControllerRegistration() {
	if d != nil && d.controllerRegistrationRelease != nil {
		d.controllerRegistrationRelease()
	}
}

func controllerSessionID(d *sessionController) string {
	if d == nil {
		return ""
	}
	return d.sessionID
}

func controllerGenerationID(d *sessionController) string {
	if d == nil {
		return ""
	}
	return d.generationID
}

// pendingResync is a frontend resync waiting for the shim to reattach.
type pendingResync struct {
	// fromSeq is the CLIENT's original mark, not the floored replay position:
	// the re-armed attempt re-derives the floor against whatever the
	// conversation looks like after the reattach, which after a rotation is a
	// different seq space entirely.
	fromSeq uint64
	// sessionID and generationID bind the deferred re-pull to the same
	// authoritative workspace snapshot that admitted the original command.
	// A reconnect or replacement changes that snapshot, so it must not replay
	// the old client's request into the new controller generation.
	sessionID    string
	generationID string
}

// WorkspaceStateReader exposes the authoritative workspace identity carried
// by frontend snapshots.  The durable-history branch has no in-memory session
// controller, so this is the only source that can validate a resync before it
// opens the store-backed replay.
type WorkspaceStateReader interface {
	Current(workspace string) (*frontendv1.WorkspaceState, bool, error)
}

type fileDiagnosticSink struct {
	persister          FileDiagnosticPersister
	workspace          string
	agentReplSessionID string
}

func (s fileDiagnosticSink) PersistFileDiagnostic(ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error {
	if s.persister == nil {
		return fmt.Errorf("session-controller: file-plane diagnostic persister is not wired for workspace %q", s.workspace)
	}
	return s.persister.PersistFileDiagnostic(s.workspace, s.agentReplSessionID, ev, diagnostic)
}

// New builds a Manager. Required collaborators missing is a construction error
// (surfaced, never a nil-deref at dispatch).
func New(cfg Config) (*Manager, error) {
	switch {
	case cfg.Push == nil:
		return nil, fmt.Errorf("session-controller: New needs a Pusher")
	case cfg.SSM == nil:
		return nil, fmt.Errorf("session-controller: New needs an SSM StateApplier")
	case cfg.Spawner == nil:
		return nil, fmt.Errorf("session-controller: New needs a Spawner")
	case cfg.Locator == nil:
		return nil, fmt.Errorf("session-controller: New needs a SessionLocator")
	case cfg.SeqStore == nil:
		return nil, fmt.Errorf("session-controller: New needs a SeqStore")
	case cfg.ClearCompactStore == nil:
		return nil, fmt.Errorf("session-controller: New needs a ClearCompactStore (without it an observed clear or compaction cannot floor a later resync)")
	case cfg.TurnAccountings == nil:
		return nil, fmt.Errorf("session-controller: New needs a TurnAccountingStore (without it terminal accounting cannot precede frontend delivery)")
	case cfg.Source == nil:
		return nil, fmt.Errorf("session-controller: New needs a ConnSource (shims dial the daemon; without it no session controller can connect)")
	case cfg.FileDiagnostics == nil:
		return nil, fmt.Errorf("session-controller: New needs a FileDiagnosticPersister")
	}
	logf := cfg.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	warnf := cfg.Warnf
	if warnf == nil {
		warnf = logf
	}
	errorf := cfg.Errorf
	if errorf == nil {
		errorf = warnf
	}
	newClient := cfg.newClient
	if newClient == nil {
		newClient = func(c shimclient.Config) sessionClient { return shimclient.New(c) }
	}
	newControllerGenerationID := cfg.newControllerGenerationID
	if newControllerGenerationID == nil {
		newControllerGenerationID = newSecureControllerGenerationID
	}
	rootCtx, rootStop := context.WithCancel(context.Background())
	now := cfg.Now
	if now == nil {
		now = func() int64 { return time.Now().UnixMilli() }
	}
	workspaceLockHeld := cfg.WorkspaceLockHeld
	if workspaceLockHeld == nil {
		workspaceLockHeld = sessionlock.WorkspaceLockHeld
	}
	// THE STOP HALF IS TAKEN OFF THE SPAWNER HERE and never put back: the gate
	// below is its only holder, and the spawner the Manager retains REFUSES
	// stops (turnstop.go). That is what keeps stopShimSettlingTurn the sole
	// route from this package to a shim's death.
	stops := newShimStopGate(cfg.Spawner)
	cfg.Spawner = sealedSpawner{ensure: cfg.Spawner, logf: logf}
	return &Manager{
		cfg:                       cfg,
		shimStops:                 stops,
		logf:                      logf,
		warnf:                     warnf,
		errorf:                    errorf,
		reg:                       newPermRegistry(logf),
		newClient:                 newClient,
		newControllerGenerationID: newControllerGenerationID,
		now:                       now,
		workspaceLockHeld:         workspaceLockHeld,
		byWS:                      make(map[string]*sessionController),
		parked:                    make(map[string]*parkedSession),
		lastCSID:                  make(map[string]string),
		shimPID:                   make(map[string]int32),
		bringUpFailures:           make(map[string]*bringUpStreak),
		buildBounced:              make(map[string]bool),
		buildRefresh:              make(map[string]*buildRefreshState),
		rootCtx:                   rootCtx,
		rootStop:                  rootStop,
	}, nil
}

// Ensure STARTS the workspace's session (lazily, reattach-first) without
// submitting a prompt — the eager bring-up the create path uses so a freshly
// created session's shim is live (and its stream consumed onto the frontend +
// SSM) before the first prompt. A workspace with no live session is a loud
// error, same as SubmitPrompt.
//
// It deliberately does NOT wait for the shim connection to finish handshaking:
// its callers only want the process running early, and every path that
// actually SENDS to the shim waits for readiness itself (see ensure). Blocking
// here would serialize a whole workspace restore behind N handshakes for no
// benefit.
func (m *Manager) Ensure(workspace string) error {
	_, err := m.bringUp(workspace)
	return err
}

// EnsureDriveable is Ensure plus the WAIT: it returns only once the workspace's
// shim has connected and handshaked, so the caller's very next SEND cannot lose
// the race against the shim's boot.
//
// It exists because Ensure deliberately does not wait (see above) and a merge
// run's first act after the bring-up is a send — the lease's interrupt. Against
// a workspace the idle sweeper had hibernated, Ensure returned while the
// respawned shim was still handshaking and the interrupt failed with "no live
// shim connection" a few tens of milliseconds before the link came up, which
// failed the merge for a session that was in fact coming back.
//
// The wait is on the connection EVENT and bounded by the CALLER'S context, so
// nothing here sleeps or polls, and a caller with a deadline gets its own
// deadline back as a loud error rather than a hang.
func (m *Manager) EnsureDriveable(ctx context.Context, workspace string) error {
	_, err := m.ensure(ctx, workspace)
	return err
}

// Live reports whether this manager holds a live session controller for workspace — i.e.
// whether Ensure would be a no-op.
//
// It is the ONE session fact that does not survive a daemon restart, which is
// exactly why a frontend has to be told it rather than infer it. Every durable
// field a frontend can see (non-terminal, backfilled) is equally true of a
// workspace this daemon has never brought up, so a frontend judging "already
// up?" from the record alone answers YES about a workspace with no session controller at
// all.
func (m *Manager) Live(workspace string) bool {
	if workspace == "" {
		return false
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	_, ok := m.byWS[workspace]
	return ok
}

// snapshotSessionControllers copies the currently live session controller set without holding the
// manager lock while callers inspect per-controller consumer state.
func (m *Manager) snapshotSessionControllers() []*sessionController {
	m.mu.Lock()
	controllers := make([]*sessionController, 0, len(m.byWS))
	for _, d := range m.byWS {
		controllers = append(controllers, d)
	}
	m.mu.Unlock()
	return controllers
}

// SessionInits returns a SessionInitView for every live session whose SystemInit
// has landed, sorted by workspace for a stable connect snapshot (task step 4:
// StateSnapshot.inits). A session with no init yet contributes nothing.
func (m *Manager) SessionInits() []*frontendv1.SessionInitView {
	var out []*frontendv1.SessionInitView
	for _, d := range m.snapshotSessionControllers() {
		if si := d.consumer.latestSystemInit(); si != nil {
			out = append(out, &frontendv1.SessionInitView{
				Workspace: d.workspace,
				Fence:     d.consumer.fence(),
				Init:      si,
			})
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].GetWorkspace() < out[j].GetWorkspace() })
	return out
}

// taskCatalogForSessionController rebuilds one live session controller's complete detached-task roster
// from its retained event ring.
func taskCatalogForSessionController(d *sessionController) *frontendv1.TaskCatalog {
	return frontend.BuildTaskCatalog(d.workspace, d.sessionID, d.consumer.fence(), d.consumer.snapshotRing(), d.consumer.logf)
}

// TaskCatalogs returns the complete detached-task roster for every live
// session, sorted by workspace for deterministic connect/resync snapshots. An
// idle session contributes an empty catalog so a reconnecting frontend is told
// authoritatively that its previous roster is clear.
func (m *Manager) TaskCatalogs() []*frontendv1.TaskCatalog {
	controllers := m.snapshotSessionControllers()
	catalogs := make([]*frontendv1.TaskCatalog, 0, len(controllers))
	taskCount := 0
	for _, d := range controllers {
		catalog := taskCatalogForSessionController(d)
		catalogs = append(catalogs, catalog)
		taskCount += len(catalog.GetTasks())
	}
	sort.Slice(catalogs, func(i, j int) bool {
		return catalogs[i].GetWorkspace() < catalogs[j].GetWorkspace()
	})
	m.logf("session-controller: task catalog snapshot catalogs=%d tasks=%d", len(catalogs), taskCount)
	return catalogs
}

// AsyncBubbles returns every async bubble every live session still holds,
// folded to date, for the connect/resync snapshot.
//
// The bubbles come from the SAME store the deltas were produced from, so a
// reconnecting client is handed exactly the fold its pushes had been building —
// never a second, separately derived account of the same detached work. Order
// is by workspace and then by launch, so two consecutive snapshots of an
// unchanged session are byte-identical.
func (m *Manager) AsyncBubbles() []*frontendv1.AsyncBubble {
	controllers := m.snapshotSessionControllers()
	sort.Slice(controllers, func(i, j int) bool { return controllers[i].workspace < controllers[j].workspace })
	var out []*frontendv1.AsyncBubble
	for _, d := range controllers {
		out = append(out, d.consumer.bubbles.snapshot()...)
	}
	m.logf("session-controller: async bubble snapshot sessions=%d bubbles=%d", len(controllers), len(out))
	return out
}

// TaskEntry returns the frontend TaskEntry (including its output_path) for a
// detached task on the workspace's live session, rebuilt from the retained
// event ring. ok=false when the workspace has no live session controller or no such task.
// The caller enforces the path-confinement predicates before reading the file.
func (m *Manager) TaskEntry(workspace, taskID string) (*frontendv1.TaskEntry, bool) {
	d, err := m.existing(workspace)
	if err != nil {
		return nil, false
	}
	cat := taskCatalogForSessionController(d)
	for _, e := range cat.GetTasks() {
		if e.GetTaskId() == taskID {
			return e, true
		}
	}
	return nil, false
}

// persistVendorSessionID offers a session's CLI uuid to the registry via the
// injected Registrar, deduped per session so a repeated SessionStarted (a
// reattach replay) does not re-write the same value. No-op when no registrar
// is wired or the uuid is empty.
//
// The uuid is written the moment the session announces it. Whether the vendor
// has actually written the transcript it names is checked where it matters, by
// server.ConversationResolver at resume time; see SessionRegistrar.
func (m *Manager) persistVendorSessionID(sessionID, csid string) {
	if m.cfg.Registrar == nil || csid == "" {
		return
	}
	m.mu.Lock()
	if m.lastCSID[sessionID] == csid {
		m.mu.Unlock()
		return
	}
	m.mu.Unlock()

	if !m.cfg.Registrar.ClaudeSessionIDChanged(sessionID, csid) {
		return
	}
	m.mu.Lock()
	m.lastCSID[sessionID] = csid
	m.mu.Unlock()
	m.logf("session-controller: persisting claude_session_id session=%s uuid=%s", sessionID, csid)
}

// SubmitPrompt brings the workspace's session up (lazily, reattach-first) and
// submits text to its shim, VERBATIM: the daemon rewrites no prompt, because
// the session's guidelines ride in its system prompt (metaprompt.go) rather
// than in anything folded into the conversation.
// A prompt submitted while the session's turn is ALREADY RUNNING is not
// forwarded at all: the daemon queues it (E4) and this returns nil, because
// the command was accepted — it was accepted into the queue. The queue's own
// pushed QueueView is what tells the frontend where the prompt went.
// requestID is the frontend command's own id. It is what the daemon's prompt
// RECEIPT is keyed on (promptecho.go), what the durable transcript line is
// later stamped with, and the authoritative turn id used for terminal
// accounting. It must therefore be nonempty before any session state changes.
func (m *Manager) SubmitPrompt(ctx context.Context, workspace, requestID, text, permissionMode string, promptOrigin corev1.PromptOrigin) error {
	if strings.TrimSpace(requestID) == "" {
		return fmt.Errorf("session-controller: submit prompt for workspace %q needs a non-empty request id", workspace)
	}
	return m.submitPrompt(ctx, workspace, requestID, text, permissionMode, "frontend", promptOrigin)
}

// SetModel forwards a deliberate model request to the live shim, then persists
// the shim-confirmed selection so the next respawn preserves the choice.
func (m *Manager) SetModel(ctx context.Context, workspace, model string) (string, error) {
	requested := registry.NormalizeModel(model)
	if requested == "" {
		return "", fmt.Errorf("session-controller: set model for workspace %q needs a non-empty model id", workspace)
	}
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return "", err
	}
	selected, err := d.client.SetModel(ctx, requested)
	selected = registry.NormalizeModel(selected)
	if err != nil {
		if selected != "" {
			m.persistObservedModel(d.sessionID, selected)
			m.logf("session-controller: model request REJECTED session=%s ws=%q requested=%q shim_selected=%q: %v", d.sessionID, workspace, requested, selected, err)
			return selected, err
		}
		m.logf("session-controller: model request FAILED session=%s ws=%q requested=%q without a shim-selected model: %v", d.sessionID, workspace, requested, err)
		return "", err
	}
	if selected == "" {
		return "", fmt.Errorf("session-controller: shim acknowledged model request for %q without a selected model", workspace)
	}
	m.logf("session-controller: model request CONFIRMED session=%s ws=%q requested=%q selected=%q", d.sessionID, workspace, requested, selected)
	m.persistObservedModel(d.sessionID, selected)
	return selected, nil
}

// SubmitWorkspaceInitialPrompt submits a durable workspace-create job's initial
// prompt.  JobID is carried as the vendor-visible origin for traceability, not
// as an exact-once claim: after a shim accepts the prompt but before the job
// store checkpoints it, a daemon crash may submit it again.  The creation
// manager therefore deliberately provides at-least-once delivery and never
// marks delivery before this call returns successfully.
func (m *Manager) SubmitWorkspaceInitialPrompt(ctx context.Context, workspace, jobID, text, permissionMode string) error {
	if jobID == "" {
		return fmt.Errorf("session-controller: workspace initial prompt needs a job id")
	}
	return m.submitPrompt(ctx, workspace, "workspace-create:"+jobID, text, permissionMode, "workspace-create:"+jobID,
		corev1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED)
}

func (m *Manager) submitPrompt(ctx context.Context, workspace, requestID, text, permissionMode, origin string, promptOrigin corev1.PromptOrigin) error {
	_, err := m.submitPromptAs(ctx, workspace, requestID, text, permissionMode, origin, promptOrigin, submitterUser)
	return err
}

// promptDisposition is WHAT BECAME OF an accepted prompt: forwarded straight to
// the shim, or parked on the queue behind work already in flight.
//
// IT EXISTS BECAUSE "THE SUBMIT SUCCEEDED" IS NOT "A TURN IS BEGINNING". Both
// outcomes return a nil error, and for a user's prompt that is the whole point —
// a queue chip is a promise of later delivery. For a MERGE prompt it is not: the
// merge holds the workspace's lease and is waiting for the turn its submit was
// supposed to start, so a prompt that only reached the queue means no turn is
// coming until whatever is in front of it ends. That distinction used to be
// invisible above the submit, and a merge resolution parked behind a turn whose
// end never arrived waited out its entire 30-minute bound on a prompt the shim
// had never even been handed (mergeresolve.go).
type promptDisposition struct {
	// queuedEntryID is the queue entry the prompt was parked as. Empty means it
	// was forwarded to the shim instead.
	queuedEntryID string
}

// queued reports whether the prompt was parked rather than forwarded.
func (p promptDisposition) queued() bool { return p.queuedEntryID != "" }

// String is the phrase that goes onto a failure cause, so the record states the
// submit's fate rather than leaving it to be inferred from the log.
func (p promptDisposition) String() string {
	if p.queued() {
		return "the prompt was QUEUED as entry " + p.queuedEntryID + " behind a turn already in flight, never handed to the shim"
	}
	return "the prompt was FORWARDED to the shim"
}

// submitPromptAs is submitPrompt with the SUBMITTER named, which is what the
// merge exclusivity lease is decided against (mergelease.go).
//
// The lease is checked HERE, before the session is even brought up, so a
// refused prompt neither spawns a shim nor lands on the queue. A user prompt
// parked on the queue of a leased session would be delivered into the middle of
// merge.Coordinator's conflict resolution the moment its turn ended, which is
// the silent drop-shaped failure the loud refusal replaces.
func (m *Manager) submitPromptAs(ctx context.Context, workspace, requestID, text, permissionMode, origin string, promptOrigin corev1.PromptOrigin, who submitter) (promptDisposition, error) {
	if err := validatePromptOrigin(promptOrigin); err != nil {
		m.logf("session-controller: prompt REFUSED ws=%q request_id=%s origin=%q prompt_origin=%d error=%v — no session or queue state was touched", workspace, requestID, origin, promptOrigin, err)
		return promptDisposition{}, err
	}
	if err := m.guardMergeLease(workspace, who, requestID, origin); err != nil {
		return promptDisposition{}, err
	}
	// THE REVIVAL GATE, ahead of ensure() on purpose: ensure() brings a stopped
	// shim back up, so asking after it would have already paid the bring-up and
	// silently un-slept the session the gate exists to hold (hibernation.go).
	//
	// IT IS ASKED ONLY WHERE A REFUSAL IS THE RIGHT ANSWER. A session that is
	// hibernated with no revival decision behind it refuses prompts outright and
	// that is unchanged. A session whose user HAS chosen compact-first is a
	// different question: its record stays hibernated on purpose while the
	// compaction runs, and the contract for that window is delayed-never-dropped
	// — so the prompt is admitted to the queue as a PARKED entry instead
	// (revive.go, queue.go). Skipping the gate here is safe because the entry is
	// parked rather than forwarded, and forwardPrompt's own copy of the gate
	// still stands behind every delivery path.
	if !m.revivalParkAdmits(workspace, who) {
		if err := m.guardHibernation(workspace, requestID, origin, who); err != nil {
			return promptDisposition{}, err
		}
	}
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return promptDisposition{}, err
	}

	// THE DRAIN LEASE IS READ BEFORE THE MUTEX, never under it: the lease engine
	// calls back into this fleet to recompute its holds, and reading it here
	// keeps the two locks in one order.
	leaseScheduleID, _ := m.heldSchedule()

	m.mu.Lock()
	entry, queued, err := m.queueSubmitLocked(d, requestID, text, permissionMode, promptOrigin, leaseScheduleID)
	if err != nil {
		// A REFUSED submit is refused whole: nothing was queued, nothing is
		// forwarded, and the caller's ack carries the reason. The only refusal
		// reachable here is a drain park the daemon could not make durable, and
		// keeping the prompt anyway would be the daemon promising a delivery it
		// has no way to make.
		m.mu.Unlock()
		return promptDisposition{}, err
	}
	if !queued {
		m.mu.Unlock()
		// The reading of the prompt, the receipt, and the forward all happen
		// together in forwardPrompt (promptdispatch.go) — a `/clear` must not be
		// echoed as a bubble, and deciding that anywhere but where the text is
		// read is how the two came apart.
		//
		// A QUEUED prompt deliberately reaches none of this: it renders as a
		// queue chip until it is DELIVERED, and a bubble drawn now would claim
		// an execution order the session is not going to follow. Its receipt is
		// pushed at the delivery site instead (queue.go, deliver).
		if err := m.forwardPrompt(ctx, d, requestID, text, origin, permissionMode, promptOrigin, who); err != nil {
			// The prompt never reached the shim, so no turn is beginning — and
			// with a paused queue that matters: the lone-runner flag set on the
			// way in would otherwise leave the pause waiting for a turn end
			// that can never arrive.
			m.mu.Lock()
			d.pausedRunner = false
			m.mu.Unlock()
			return promptDisposition{}, err
		}
		return promptDisposition{}, nil
	}
	parked := promptDisposition{queuedEntryID: entry.id}
	running := d.runningText
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	// THE CLASSIFIER NEVER RUNS ON A DRAIN-HELD ENTRY. It answers exactly one
	// question — should this prompt interrupt the turn in front of it — and a
	// prompt parked by a scheduled bounce has no turn in front of it to
	// interrupt. Asking anyway would spend a model call to produce a verdict
	// that could only be wrong: an INTERJECT would demand an interrupt on
	// behalf of a prompt the lease exists to hold back.
	if entry.drainHeld() {
		m.logf("session-controller: queued prompt entry=%s session=%s ws=%q origin=%q schedule=%s classifier=SKIPPED (parked by the drain lease)",
			entry.id, d.sessionID, workspace, origin, entry.shutdownHoldScheduleID)
		m.publish(d.sessionID, view, recs)
		return parked, nil
	}
	// THE CLASSIFIER NEVER RUNS ON A KEEP-ALIVE-HELD ENTRY EITHER, and the
	// reason is sharper than the drain lease's. The classifier answers exactly
	// one question — should this prompt interrupt the turn in front of it —
	// and the turn in front of this one is a machine-generated ping. Spending a
	// model call to judge whether the user's prompt should interrupt the
	// daemon's own cache refresh would produce a verdict that could only be
	// wrong: INTERJECT would demand an interrupt whose whole effect is to leave
	// a half-finished ping in the transcript the rewind is about to clean up.
	if entry.keepAliveHeld() {
		m.logf("session-controller: queued prompt entry=%s session=%s ws=%q origin=%q keep_alive_turn=%s classifier=SKIPPED (held behind a cache keep-alive turn)",
			entry.id, d.sessionID, workspace, origin, entry.keepAliveHoldTurnID)
		m.publish(d.sessionID, view, recs)
		return parked, nil
	}
	// THE CLASSIFIER NEVER RUNS ON A REVIVAL-PARKED ENTRY EITHER. The turn in
	// front of it is the revival's own `/compact`, and the only verdict the
	// classifier could return that changes anything — INTERJECT — would demand
	// an interrupt of the compaction the user chose to pay for. Spending a model
	// call to ask for that is spending it to be wrong.
	if entry.revivalHeld() {
		m.logf("session-controller: queued prompt entry=%s session=%s ws=%q origin=%q revival_session=%s classifier=SKIPPED (parked by an in-flight compact-first revival)",
			entry.id, d.sessionID, workspace, origin, entry.revivalHoldSessionID)
		m.publish(d.sessionID, view, recs)
		return parked, nil
	}
	m.logf("session-controller: queued prompt entry=%s session=%s ws=%q origin=%q (turn in flight)",
		entry.id, d.sessionID, workspace, origin)
	m.publish(d.sessionID, view, recs)
	go m.classify(d, entry.id, running, text)
	return parked, nil
}

// persistBackfillState writes the never-blue backfill signal (F2) through to
// the durable registry record. No-op without a registrar (a test harness).
//
// Persisted rather than kept in memory because the evidence it was derived
// from does not survive a daemon restart: the re-Subscribe starts from
// LastSeq, so the file-plane events that proved the backfill landed are never
// re-delivered. See registry.Record.BackfillState.
func (m *Manager) persistBackfillState(sessionID, state string) {
	if m.cfg.Registrar == nil {
		return
	}
	m.cfg.Registrar.BackfillStateChanged(sessionID, state)
}

// persistObservedModel writes the model a live session reports through to its
// registry record, so the next respawn is pinned to what the session IS rather
// than what was asked for when it was created.
//
// THE RECORD USED TO FREEZE AT CREATE. rec.Model was written once, from the
// frontend's CreateSessionCmd, and read back on every respawn forever after.
// A session whose model changed mid-life was therefore relaunched as the
// original model after each hibernation, silently undoing the change.
//
// THE PLACEHOLDER MEANS EMPTY. The CLI reports `<synthetic>` when it has no
// real model to name. Normalize it to the same empty representation and do not
// overwrite a record that may already hold the last real observed model.
//
// No-op without a registrar (a test harness).
func (m *Manager) persistObservedModel(sessionID, model string) {
	if m.cfg.Registrar == nil {
		return
	}
	normalized := registry.NormalizeModel(model)
	if normalized == "" {
		if model != "" {
			m.logf("session-controller: session %s reported model marker %q — normalized to empty; leaving the record's model alone", sessionID, model)
		}
		return
	}
	m.cfg.Registrar.SessionModelObserved(sessionID, normalized)
}

// modelCatalogReporter is the shimclient boundary for query-supported model
// menus. It validates the session binding before the registrar republishes a
// SessionView, so one shim cannot alter another workspace's picker.
type modelCatalogReporter struct{ m *Manager }

func (r modelCatalogReporter) ModelCatalog(sessionID string, catalog *corev1.ModelCatalog) error {
	if catalog.GetSessionId() != sessionID {
		return fmt.Errorf("session-controller: refusing model catalog frame_session=%s expected_session=%s", catalog.GetSessionId(), sessionID)
	}
	if r.m.cfg.ModelCatalogs == nil {
		return fmt.Errorf("session-controller: model catalog session=%s models=%d has no registrar", sessionID, len(catalog.GetModels()))
	}
	r.m.logf("session-controller: model catalog observed session=%s models=%d", sessionID, len(catalog.GetModels()))
	if err := r.m.cfg.ModelCatalogs.SessionModelCatalogObserved(sessionID, catalog.GetModels()); err != nil {
		return fmt.Errorf("session-controller: model catalog session=%s registrar rejected it: %w", sessionID, err)
	}
	return nil
}

// persistSessionDeath marks the session's record terminal with the reason its
// death carried (F4).
//
// Nothing did this before, which is why the registry documented a "shim_died"
// reason that no code path ever wrote: a shim death resolved the workspace
// RENDER_STATE_DEAD through the SSM and left the record claiming the session
// was alive with no reason recorded. The dead-state card had nothing to show.
func (m *Manager) persistSessionDeath(sessionID, reason string) {
	if m.cfg.Registrar == nil {
		return
	}
	m.logf("session-controller: session %s ended — marking the record terminal (reason=%s)", sessionID, reason)
	m.cfg.Registrar.SessionDied(sessionID, reason)
}

// progress returns the configured progress resolver, or the no-op stand-in when
// the session controller was built without one.
func (m *Manager) progress() ProgressResolver {
	if m.cfg.Progress == nil {
		return noopProgress{}
	}
	return m.cfg.Progress
}

// noteProgressCounts republishes the workspace's two daemon-local ephemeral
// counters to the progress footer: the permission prompts waiting on the user
// and the depth of the held-prompt queue. Neither is a store fact, so nothing
// else would ever tell the footer they moved.
//
// Must be called with m.mu RELEASED (it takes the permission registry's lock and
// then the resolver's).
func (m *Manager) noteProgressCounts(workspace string, queueDepth int64) {
	pending := int64(len(m.reg.idsForWorkspace(workspace)))
	m.progress().SetCounts(workspace, pending, queueDepth)
}

// Interrupt interrupts the workspace's live turn. A workspace with no live
// session is a loud error (the frontend renders the failed CommandAck) UNLESS
// the log says a turn is still in flight behind it, in which case the bring-up
// is paid first and the stop is delivered — see recoverSessionControllerForInterrupt.
//
// The shim's OUTCOME decides whether the stop failed, not the absence of an
// error: an undeliverable stop is a failure, while a stop that arrived after
// the turn had already finished is a success the user explicitly asked for.
// Those two used to be indistinguishable from here, and the second was
// reported as the first.
//
// THIS IS THE USER-COMMANDED STOP, and the only one. It is reached from
// exactly one place — the frontend interrupt command handler, via
// PromptRouter — which is what makes it the right and sufficient place to
// route the three consequences a user's stop has and an interject's stop must
// not have: the interrupt window, the `interrupted` turn outcome, and the
// queue pause. The queue's own interject calls d.client.Interrupt DIRECTLY
// (see beginInterject) and therefore reaches none of them structurally,
// rather than by remembering to pass a flag.
// requestID is the FRONTEND COMMAND'S OWN id, carried through for the same
// reason SubmitPrompt's is: it is the only id the user's client, the daemon's
// command log and the shim exchange can be reconciled on. Without it a stop was
// unfindable end to end — the command arrived under `fe-276-1074`, the wire
// exchange went out under a daemon-minted `daemon-interrupt-3-…`, and nothing
// joined them — so an interrupt that WAS delivered and acked INTERRUPTED within
// two milliseconds read exactly like one the daemon had silently swallowed.
func (m *Manager) Interrupt(ctx context.Context, workspace, requestID string) error {
	d, err := m.existing(workspace)
	if err != nil {
		d, err = m.recoverSessionControllerForInterrupt(ctx, workspace, err)
		if err != nil {
			return err
		}
	}
	outcome, err := d.client.Interrupt(ctx, requestID)
	if err != nil {
		return err
	}
	if err := m.noteUserInterrupt(d, outcome); err != nil {
		return err
	}
	if failed := errclass.InterruptError(outcome); failed != nil {
		m.logf("session-controller: interrupt undeliverable ws=%s session=%s request_id=%s outcome=%s", workspace, d.sessionID, requestID, outcome)
		return failed
	}
	m.logf("session-controller: interrupt ws=%s session=%s request_id=%s outcome=%s", workspace, d.sessionID, requestID, outcome)
	return nil
}

// recoverSessionControllerForInterrupt brings a workspace up so a user's stop can reach a
// turn that is still running behind a session controller that is gone, returning ABSENT
// unchanged when there is nothing to stop.
//
// THE STATE IT RECOVERS FROM SHOULD NOT EXIST. `hibernate()` refuses any
// workspace that has not settled, so a live turn with no session controller behind it means
// some writer closed the axis behind work in flight. The user, meanwhile, is
// looking at a tab that says nothing is happening and pressing stop on it, and
// the old behavior answered with "no live session for this workspace" — a nack
// about the interrupt, which tells them nothing about the turn that is still
// burning tokens somewhere.
//
// THE ORDER IS BRING UP, THEN INTERRUPT, and it is the order the user asked for
// rather than a convenience. Bringing the session up first re-establishes the
// stream, so the moment the stop lands its consequences — the turn's `interrupted`
// outcome, the queue pause, the footer's window — flow to a frontend that can
// see them. Interrupting first would deliver a stop into a route with nothing on
// the other end.
//
// WHAT IT REFUSES TO DO is spawn a shim to stop nothing. A genuinely settled
// workspace with no session controller — hibernated after a clean turn, never opened, merely
// severed — keeps the ORIGINAL ErrNoLiveSessionController, because paying a 500MB bring-up
// and a several-hundred-millisecond handshake to deliver a stop to an idle
// session is a worse answer than the honest error. `turn_active` is what
// separates the two, and it is deliberately read INSTEAD of the resolved state:
// the violated case resolves `hibernated`, since teal outranks the session-status lifecycle by
// design, so "reads red" would never fire on the very state this exists for. The
// red band is accepted too, for a workspace without a session controller whose log never got the
// hibernation row at all.
//
// A STATE READ FAILURE KEEPS THE ORIGINAL ERROR. The absence of a session controller is a
// fact we already have; the recovery is a discretionary extra that needs positive
// evidence of a turn, and spawning a shim on an unreadable log would be acting on
// a guess. Logged loudly, never swallowed.
func (m *Manager) recoverSessionControllerForInterrupt(ctx context.Context, workspace string, absent error) (*sessionController, error) {
	if !errors.Is(absent, ErrNoLiveSessionController) {
		return nil, absent
	}
	st, found, err := m.cfg.SSM.Current(workspace)
	if err != nil {
		m.logf("session-controller: interrupt recovery state read FAILED ws=%s: %v — keeping the no-live-session-controller refusal rather than spawning a shim on a guess",
			workspace, err)
		return nil, absent
	}
	if !found || (!st.GetTurnActive() && !redStates[st.GetState()]) {
		return nil, absent
	}
	m.logf("session-controller: INVARIANT VIOLATION RECOVERY ws=%s — a user-commanded stop arrived for a workspace with NO live session controller whose log still shows a turn in flight (state=%s turn_active=%v). Bringing the session up FIRST so the stop has somewhere to land, then interrupting",
		workspace, st.GetState(), st.GetTurnActive())
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		m.logf("session-controller: interrupt recovery bring-up FAILED ws=%s: %v — the turn in flight cannot be reached", workspace, err)
		return nil, err
	}
	m.logf("session-controller: interrupt recovery brought up ws=%s session=%s; delivering the stop", workspace, d.sessionID)
	return d, nil
}

// noteUserInterrupt applies a user-commanded stop's consequences from the
// shim's ack.
//
// The WINDOW opens on every outcome: two of the three move no workspace phase,
// so the footer's window is the only place they are reported at all — a FAILED
// stop keeps its ordinary errclass failure path unchanged on top of it.
//
// The TURN OUTCOME is marked only on INTERRUPTED, because that is the only
// outcome under which a turn was actually stopped; the other two stopped
// nothing and have no turn to name.
//
// The PAUSE holds on both successful outcomes. ALREADY_COMPLETE pauses too:
// the turn being over already does not make the user's "stop" mean less, and
// the queue would otherwise deliver the next held prompt into the silence they
// just asked for. FAILED changes nothing — no stop was delivered, so nothing
// about the session moved.
func (m *Manager) noteUserInterrupt(d *sessionController, outcome corev1.InterruptOutcome) error {
	// ALREADY_COMPLETE is a shim-side assertion that no foreground turn
	// exists. Its durable TurnEnded can still be traversing the store while
	// this control Ack arrives. Reconcile the SSM FIRST and publish the footer
	// window only after that succeeds; reversing these calls is the exact race
	// that rendered "already finished" beside `thinking`.
	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE {
		publish := func(state *frontendv1.WorkspaceState) {
			d.consumer.push.PushWorkspaceState(state)
		}
		closed, err := m.cfg.SSM.ReconcileAlreadyComplete(d.workspace, d.sessionID, publish)
		if err != nil {
			m.logf("session-controller: already-complete reconciliation FAILED ws=%s session=%s outcome=%s: %v — withholding the interrupt window so mutually exclusive footer/state claims cannot be published",
				d.workspace, d.sessionID, outcome, err)
			return err
		}
		m.mu.Lock()
		turnBefore, _ := d.noteTurnIdleLocked()
		m.mu.Unlock()
		// The DURABLE half of the same statement. The status axis above is what
		// the footer renders; the turn claim is what the queue holds prompts
		// behind, and a claim left standing against an Ack that says no turn
		// exists queues every later prompt behind a boundary that is not coming.
		m.closeTurnClaimsOnAlreadyComplete(d)
		m.logf("session-controller: already-complete reconciliation CONFIRMED ws=%s session=%s outcome=%s ssm_closed=%v session_controller_turn_before=%s session_controller_turn_after=idle",
			d.workspace, d.sessionID, outcome, closed, turnBefore)
	}

	m.progress().NoteInterrupt(d.workspace, d.sessionID, outcome)

	// A USER-COMMANDED STOP CLOSES AN OPEN MERGE WINDOW. It is one of the two
	// boundaries async-bubble.proto names for a Merge bubble, and this is the
	// one place a user's stop is known — an interject's machinery stop calls
	// d.client.Interrupt directly and therefore cannot reach it, which is
	// exactly right: machinery did not take the session back from the merge.
	//
	// A FAILED stop delivered nothing and moves no window; the other two both
	// mean the user has the session again.
	if errclass.InterruptError(outcome) == nil {
		d.consumer.settleMergeWindowOnInterrupt(fmt.Sprintf("user interrupt (%s)", outcome))
	}

	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		if err := m.cfg.SSM.MarkTurnInterrupted(d.workspace); err != nil {
			// Loud, never swallowed: the turn's end will report `done` instead
			// of `interrupted`, and this line is the only account of why.
			m.logf("session-controller: marking the interrupted turn on the SSM FAILED ws=%s session=%s: %v (the stopped turn will report `done`)",
				d.workspace, d.sessionID, err)
		}
	}
	if errclass.InterruptError(outcome) != nil {
		return nil
	}

	m.mu.Lock()
	d.paused = true
	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		d.interruptedTurn = true
	}
	held := len(d.queue.entries)
	m.mu.Unlock()
	m.logf("session-controller: queue PAUSED by a user interrupt ws=%s session=%s outcome=%s held=%d (every entry retained; a newly submitted prompt runs alone and its clean end resumes the drain)",
		d.workspace, d.sessionID, outcome, held)
	return nil
}

// TurnActive reports whether the workspace's session has a turn IN FLIGHT, as
// the session controller observed it off the shim's own TurnStarted/TurnEnded stream.
//
// It is the same fact the queue acts on (sessionController.turn), deliberately
// rather than the SSM's resolved turn_active: the interrupt confirm gate is
// deciding whether there is a turn to stop RIGHT NOW, which is a question
// about what the session reported, not about how a workspace resolves.
//
// A workspace with no live session controller is a loud error, the same one Interrupt
// itself would return a moment later.
func (m *Manager) TurnActive(workspace string) (bool, error) {
	d, err := m.existing(workspace)
	if err != nil {
		return false, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return d.turn.active(), nil
}

// Health proves one named live session is connected to this daemon and that
// its shim has passed its own dependency health check.  It deliberately uses
// existing rather than ensure: a restore must not render because a probe
// happened to create a new shim; it may render only after the live session controller has
// handshaked and answered the correlated check.
//
// IT WAITS ON A BRING-UP ALREADY IN MOTION, under the PROBE'S OWN context.
// AwaitReady resolves only on ShimReady, i.e. once the shim has completed ALL
// of its wiring (lock, query, producer, standing store subscription), so
// waiting on it is what makes a probe issued the instant after a spawn answer
// the question it was asking instead of failing against a connection that was
// milliseconds away. This is also the gate createSession's ack rides on.
//
// The wait creates NOTHING: a workspace with no session controller still fails loudly and
// immediately (existing, above), and the deadline stays the caller's — a probe
// whose context ends before readiness surfaces that deadline as its own loud
// error rather than hanging.
//
// Each failure carries the LINK it stopped at as a sentinel (ErrNoLiveSessionController,
// ErrShimNotReady, or the shimclient sentinels the round-trip returns), which
// is what lets a create nack name the deepest hop rather than the whole path.
func (m *Manager) Health(ctx context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error) {
	if workspace == "" {
		return nil, fmt.Errorf("session-controller: health requires a workspace")
	}
	if sessionID == "" {
		return nil, fmt.Errorf("session-controller: health ws=%q requires a session_id", workspace)
	}
	if requestID == "" {
		return nil, fmt.Errorf("session-controller: health ws=%q session=%s requires a request_id", workspace, sessionID)
	}
	d, err := m.existing(workspace)
	if err != nil {
		return nil, err
	}
	if d.sessionID != sessionID {
		return nil, fmt.Errorf("session-controller: health ws=%q names session=%s but live session controller owns session=%s", workspace, sessionID, d.sessionID)
	}
	m.logf("session-controller: health probe ws=%q session=%s request_id=%s", workspace, sessionID, requestID)
	// A stale-build verdict deliberately withholds the source generation's
	// readiness. Race that wait against the verdict's own transition edge so a
	// probe transfers to the replacement at the instant ownership moves,
	// independent of the source transport's later teardown.
	type healthAnswer struct {
		status *corev1.HealthStatus
		err    error
	}
	sourceCtx, cancelSource := context.WithCancel(ctx)
	defer cancelSource()
	sourceAnswer := make(chan healthAnswer, 1)
	go func() {
		status, err := m.healthController(sourceCtx, d, requestID)
		sourceAnswer <- healthAnswer{status: status, err: err}
	}()

	var status *corev1.HealthStatus
	var sourceErr error
	select {
	case answer := <-sourceAnswer:
		status, sourceErr = answer.status, answer.err
	case <-d.buildRefreshStarted:
		cancelSource()
		sourceErr = fmt.Errorf("source generation retired before readiness")
	}
	replacement, followErr := m.followBuildRefresh(ctx, workspace, sessionID, d)
	if followErr != nil {
		return nil, fmt.Errorf("session-controller: health ws=%q session=%s request_id=%s: stale-build replacement failed after the source generation retired: %w (source probe: %v)",
			workspace, sessionID, requestID, followErr, sourceErr)
	}
	if replacement == nil {
		return status, sourceErr
	}
	m.logf("session-controller: health probe ws=%q session=%s request_id=%s following intentional stale-build replacement generation=%s->%s",
		workspace, sessionID, requestID, d.generationID, replacement.generationID)
	return m.healthController(ctx, replacement, requestID)
}

func (m *Manager) healthController(ctx context.Context, d *sessionController, requestID string) (*corev1.HealthStatus, error) {
	workspace, sessionID := d.workspace, d.sessionID
	if err := d.client.AwaitReady(ctx); err != nil {
		// BOTH causes stay in the chain: the link (which hop is pending) and the
		// transport error (why the wait ended). Dropping either would cost a
		// caller one of the two questions it has to answer.
		return nil, fmt.Errorf("session-controller: health ws=%q session=%s request_id=%s: %w within the probe's deadline: %w",
			workspace, sessionID, requestID, ErrShimNotReady, err)
	}
	status, err := d.client.Health(ctx, requestID)
	if err != nil {
		return nil, fmt.Errorf("session-controller: health ws=%q session=%s request_id=%s: %w", workspace, sessionID, requestID, err)
	}
	if status.GetRequestId() != requestID {
		return nil, fmt.Errorf("session-controller: health ws=%q session=%s request_id mismatch got=%q want=%q", workspace, sessionID, status.GetRequestId(), requestID)
	}
	return status, nil
}

// AnswerPermission delivers a frontend permission answer to the parked
// canUseTool round-trip (keyed by permissionRequestID). A stale/duplicate
// answer is a loud error, never swallowed.
func (m *Manager) AnswerPermission(_ context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error {
	m.logf("session-controller: permission answer ws=%s request_id=%s allow=%v", workspace, permissionRequestID, allow)
	return m.reg.answer(permissionRequestID, allow, denyMessage, updatedInput)
}

// Resync replays the workspace session's retained conversation deltas from
// fromSeq (task step 5), then closes whatever the ring could not cover.
//
// THE REPLAY FLOOR. What is actually replayed starts at
// max(fromSeq, newestClearOrCompactSeq) — never at the raw client mark. A clear
// and a compaction are each the point at which the conversation stopped
// informing the agent, so history above one is history the frontend would only
// discard, and serving it is both wasted work and an invitation for the
// frontend to invent its own rule for FINDING the clear or the compaction (the
// webapp's retired string-match on prompt text). Flooring here makes replay
// idempotent by construction: the frontend scans for nothing, and a clear or a
// compaction it receives is always live.
//
// The floor is INCLUSIVE of the clear or compaction itself — see replayFloor.
//
// The retained ring is a bounded live window (4,096 events) and is EMPTY after
// a daemon restart, so a frontend asking from a seq below the ring's floor used
// to be answered with silence — the blank-feed bug. When fromSeq falls below
// the floor, the remainder is served by a bounded, frontend-initiated re-pull
// straight from the store (repull.go), which feeds CONVERSATION TRANSLATION
// ONLY.
//
// The floor comes from the ring when the ring holds anything, and otherwise
// from the DURABLE last_seen_seq: everything up to that mark was consumed by
// some daemon and is no longer held here, so the first seq the live window
// covers is one past it.
//
// A workspace with NO live session controller is served from DURABLE history
// instead (durablereplay.go). It used to be skipped quietly one layer up, which
// is what left a webview reloaded after a daemon bounce showing a correct
// footer over an empty feed: the conversation was in the store the whole time,
// and every read path required a shim.
// ResyncForGeneration admits a frontend replay only when the identity copied
// from its authoritative WorkspaceState is still current.  The comparison is
// linearized under the controller lock before ring replay, replay allocation,
// store reads, or shim reads can begin.  The durable-history branch compares
// against the SSM snapshot because it deliberately has no live controller, and
// keeps the full identity requirement.
//
// Against a LIVE controller the non-empty controller generation is the identity
// that decides: a request carrying it is current even when its session field
// names a superseded session (decision=current_generation_session_rebound).
// A generation mismatch still rejects.
func (m *Manager) ResyncForGeneration(workspace, expectedSessionID, expectedGenerationID string, fromSeq uint64) error {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	if m.hibernating[workspace] {
		liveSessionID, liveGenerationID := "", ""
		if live {
			liveSessionID, liveGenerationID = d.sessionID, d.generationID
		}
		m.mu.Unlock()
		return m.rejectSupersededResync(workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "hibernation_transition", "eligibility_revoked")
	}
	if live {
		liveSessionID, liveGenerationID := d.sessionID, d.generationID
		m.mu.Unlock()
		if expectedGenerationID != liveGenerationID {
			return m.rejectSupersededResync(workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "live_controller", "identity_mismatch")
		}
		if expectedSessionID != liveSessionID {
			// A NON-EMPTY controller generation uniquely identifies THIS live
			// controller, so a client carrying it is current on the pushed plane and
			// only its session field is stale — the exact shape a webview ends up in
			// when a session id rotates underneath a store that already took the new
			// generation. Rejecting it deadlocked the view: resync is the only
			// recovery mechanism, so a rejected resync is a permanent stale banner.
			// The replay is served from the live controller under ITS identity, which
			// is also what any re-arm is bound to.
			if expectedGenerationID == "" {
				return m.rejectSupersededResync(workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "live_controller", "identity_mismatch")
			}
			m.logf("session-controller: resync eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=current_generation_session_rebound",
				workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "live_controller")
			return m.resyncFromController(d, fromSeq, liveSessionID, liveGenerationID)
		}
		m.logf("session-controller: resync eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=current_live_controller",
			workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "live_controller")
		return m.resyncFromController(d, fromSeq, expectedSessionID, expectedGenerationID)
	}

	reader, ok := m.cfg.SSM.(WorkspaceStateReader)
	if !ok {
		m.mu.Unlock()
		err := fmt.Errorf("session-controller: resync ws=%q has no workspace-state reader for durable-history identity validation", workspace)
		m.logf("session-controller: resync eligibility REJECTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=missing_workspace_state_reader error=%v",
			workspace, expectedSessionID, expectedGenerationID, "", "", fromSeq, "durable_history", err)
		return err
	}
	state, found, err := reader.Current(workspace)
	if err != nil {
		m.mu.Unlock()
		m.logf("session-controller: resync eligibility FAILED ws=%q request_session=%q request_generation=%q from_seq=%d replay_source=%q decision=workspace_state_read_failed error=%v",
			workspace, expectedSessionID, expectedGenerationID, fromSeq, "durable_history", err)
		return fmt.Errorf("session-controller: read authoritative workspace state for durable resync ws %q: %w", workspace, err)
	}
	if !found || state == nil {
		m.mu.Unlock()
		err := fmt.Errorf("session-controller: no authoritative workspace state for durable resync ws %q", workspace)
		m.logf("session-controller: resync eligibility REJECTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=missing_workspace_state error=%v",
			workspace, expectedSessionID, expectedGenerationID, "", "", fromSeq, "durable_history", err)
		return err
	}
	liveSessionID, liveGenerationID := state.GetSessionId(), state.GetControllerGenerationId()
	if expectedSessionID != liveSessionID || expectedGenerationID != liveGenerationID {
		m.mu.Unlock()
		return m.rejectSupersededResync(workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "durable_history", "identity_mismatch")
	}
	m.logf("session-controller: resync eligibility ACCEPTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=current_durable_snapshot",
		workspace, expectedSessionID, expectedGenerationID, liveSessionID, liveGenerationID, fromSeq, "durable_history")
	// Keep m.mu through the bounded durable replay.  bringUp installs the next
	// controller under this same mutex, so no controller generation can appear
	// after the no-controller snapshot check and before the store replay ends.
	// resyncFromDurableHistory never re-enters m.mu; preserve that lock-order
	// invariant whenever its implementation changes.
	err = m.resyncFromDurableHistory(workspace, fromSeq)
	m.mu.Unlock()
	return err
}

func (m *Manager) rejectSupersededResync(workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID string, fromSeq uint64, replaySource, rejectionCause string) error {
	err := fmt.Errorf("%w: resync ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q rejection_cause=%q",
		errclass.ErrSessionSuperseded, workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID, fromSeq, replaySource, rejectionCause)
	m.logf("session-controller: resync eligibility REJECTED ws=%q request_session=%q request_generation=%q live_session=%q live_generation=%q from_seq=%d replay_source=%q decision=superseded rejection_cause=%q error=%v",
		workspace, requestSessionID, requestGenerationID, liveSessionID, liveGenerationID, fromSeq, replaySource, rejectionCause, err)
	return err
}

// resyncFromController performs the replay only after its caller selected the
// exact current controller generation.  It deliberately owns no eligibility
// lookup, which keeps every store or shim read below that admission boundary.
func (m *Manager) resyncFromController(d *sessionController, fromSeq uint64, expectedSessionID, expectedGenerationID string) error {
	replayFrom := m.replayFloor(d, fromSeq)
	ringFloor, haveRingFloor := d.consumer.resync(replayFrom)
	if !haveRingFloor {
		ringFloor = m.cfg.SeqStore.LastSeq(d.sessionID) + 1
	}
	if replayFrom >= ringFloor {
		m.noteResyncSettled(d)
		return nil // the ring covered the whole request
	}
	m.logf("session-controller: resync ws=%q replay_from=%d is below the retained floor %d; re-pulling the gap from the store",
		d.workspace, replayFrom, ringFloor)
	err := m.startRepull(d, exclusiveLowerBound(replayFrom), ringFloor)
	if errors.Is(err, shimclient.ErrReplayLinkLost) {
		return m.rearmResyncAfterReattach(d, fromSeq, expectedSessionID, expectedGenerationID, err)
	}
	if err == nil {
		m.noteResyncSettled(d)
	}
	return err
}

// rearmResyncAfterReattach holds a resync whose re-pull was cut short by the
// shim link going away, to be served again the moment the shim reattaches.
//
// WHY THIS IS NOT A FAILURE. The shim bounces the daemon link DELIBERATELY when
// the vendor rotates its session uuid, and a re-pull in flight across that
// bounce comes back with zero events and a lost link. Reporting it as a
// truncation put a red failure card in a feed that the rotation had just
// emptied, which is the exact pair of symptoms the user saw: nothing to read,
// and an alarm about it. The client's question was never answered, so it is
// re-asked rather than answered wrongly.
//
// WHY IT RE-ARMS RATHER THAN LOOPING. There is no sleep and no retry timer: the
// standing subscription is replayed on reattach, and onConnected is the event
// that says the link is back. Re-running the resync there is the same request
// against a connection that can serve it.
//
// THE BUDGET IS ONE. A second CONSECUTIVE lost link is reported to the frontend
// as the failure it is — a link that will not stay up long enough to serve
// history is a real outage, not a rotation, and hiding it behind endless
// retries is what a fallback looks like. The budget refreshes when a resync
// completes, and when a rotation happens (which is itself a legitimate reason
// for the second interruption).
func (m *Manager) rearmResyncAfterReattach(d *sessionController, fromSeq uint64, expectedSessionID, expectedGenerationID string, cause error) error {
	m.mu.Lock()
	if closed, retried := m.closed, d.resyncRetried; closed || retried {
		m.mu.Unlock()
		m.logf("session-controller: resync ws=%q session=%s from_seq=%d lost the shim link with no re-arm left (already retried=%v, manager closed=%v) — surfacing it rather than retrying further",
			d.workspace, d.sessionID, fromSeq, retried, closed)
		return cause
	}
	d.resyncRetried = true
	d.pendingResync = &pendingResync{fromSeq: fromSeq, sessionID: expectedSessionID, generationID: expectedGenerationID}
	m.mu.Unlock()
	m.logf("session-controller: resync ws=%q session=%s from_seq=%d was INTERRUPTED by a shim-link bounce and is RE-ARMED — it will be served again as soon as the shim reattaches; this is not a truncation and no failure card is pushed: %v",
		d.workspace, d.sessionID, fromSeq, cause)
	return nil
}

// noteResyncSettled refreshes the one-shot re-arm budget after a resync that
// actually completed.
func (m *Manager) noteResyncSettled(d *sessionController) {
	m.mu.Lock()
	d.resyncRetried = false
	m.mu.Unlock()
}

// runPendingResync re-runs a resync the shim link interrupted, now that the
// shim has reattached. It runs on its OWN goroutine because this is called from
// the shimclient's connection goroutine before its read loop starts, and a
// re-pull cannot complete without that loop delivering the replayed events.
func (m *Manager) runPendingResync(workspace, sessionID string) {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if m.closed || !ok || d.sessionID != sessionID || d.pendingResync == nil {
		m.mu.Unlock()
		return
	}
	pending := d.pendingResync
	d.pendingResync = nil
	// Registered with the same WaitGroup Close joins, so this cannot outlive the
	// manager and race whatever tears down after it.
	m.exits.Add(1)
	m.mu.Unlock()
	go func() {
		defer m.exits.Done()
		m.logf("session-controller: re-serving the resync a shim-link bounce interrupted ws=%q session=%s from_seq=%d (the shim has reattached)",
			workspace, sessionID, pending.fromSeq)
		var err error
		err = m.ResyncForGeneration(workspace, pending.sessionID, pending.generationID, pending.fromSeq)
		if err != nil {
			m.logf("session-controller: the re-armed resync ws=%q session=%s from_seq=%d FAILED: %v",
				workspace, sessionID, pending.fromSeq, err)
		}
	}()
}

// replayFloor is the first seq a frontend replay may start at:
// max(clientLastSeq, newestClearOrCompactSeq), INCLUSIVE.
//
// Inclusive is the whole point. The clear or the compaction IS the bubble the
// frontend draws and the rule it discards above, so a floor of
// newestClearOrCompactSeq+1 would tell a frontend to throw away everything it
// holds and hand it nothing to show for it. Both consumers of this value honor
// that: consumer.resync replays every ring event with seq >= this, and the
// store re-pull converts it to the shim's EXCLUSIVE lower bound
// (exclusiveLowerBound) rather than passing it straight through.
//
// A CLEAR AND A COMPACTION BOTH FLOOR A REPLAY. A compaction discards the
// history that preceded it just as a clear does — the summary is what stands in
// for it — so there is no reason to floor on one and not the other. The store
// mark is a single seq for that reason: whichever came last wins, and the older
// one is already below the floor the newer one sets.
//
// A MARK FROM A RETIRED SEQ SPACE IS NOT TRUSTED. When the vendor rotates its
// session uuid (a `/clear`), the conversation starts a NEW store seq space at 1
// and every mark counted in the old one becomes a number with no meaning here —
// a client still holding 1060 while this space has reached 12 would otherwise
// read as "already past everything" and be served nothing at all, the clear
// that caused the rotation included. Such a mark is impossible in-space: the
// daemon records last_seen_seq BEFORE forwarding an event, so no frontend can
// hold a seq this conversation has not seen. It is therefore floored at the
// newest clear or compaction (or at zero, replaying what is retained), loudly,
// rather than believed.
func (m *Manager) replayFloor(d *sessionController, fromSeq uint64) uint64 {
	return m.replayFloorAt(d.workspace, d.sessionID, m.lastSeenSeq(d), fromSeq)
}

// replayFloorAt is replayFloor's rule expressed against a workspace, its
// session, and the ceiling on an honest client mark, so the UNWIRED durable
// replay (durablereplay.go) floors identically without a session controller to
// read the ceiling off. Its ceiling is the DURABLE last_seen_seq alone, because
// a workspace with no controller has no retained ring to raise it.
func (m *Manager) replayFloorAt(workspace, sessionID string, lastSeen, fromSeq uint64) uint64 {
	floorSeq := m.cfg.ClearCompactStore.NewestClearOrCompactSeq(sessionID)
	logf := dlog.Tag(dlog.Logf(m.logf),
		"ws", workspace, "session", sessionID,
		"from_seq", fromSeq, "newest_clear_or_compact_seq", floorSeq,
		"last_seen_seq", lastSeen)
	if fromSeq > lastSeen {
		logf("session-controller: replay mark from a RETIRED seq space — from_seq is above every seq this conversation has produced, so it was counted under a vendor session uuid that has since rotated; replay_from=%d (the mark is NOT trusted as 'already past everything')", floorSeq)
		return floorSeq
	}
	if floorSeq <= fromSeq {
		logf("session-controller: replay floor left at the client mark replay_from=%d (the client is already at or past the newest clear or compaction)", fromSeq)
		return fromSeq
	}
	logf("session-controller: replay floor RAISED to the newest clear or compaction replay_from=%d (inclusive: that event is itself replayed; the history above it is never sent)", floorSeq)
	return floorSeq
}

// lastSeenSeq is the highest store seq this conversation has produced in its
// CURRENT vendor seq space: the durable last_seen_seq, or the retained ring's
// newest position when that is higher.
//
// Both are read because they are true at different moments. The durable mark is
// written by shimclient before an event is forwarded anywhere, so it covers
// everything a frontend can possibly have seen — including after a restart, when
// the ring is empty. The ring covers a session controller whose events did not come through
// that path at all (the resync tests' direct Consume, a re-pull), where the
// durable mark can legitimately still read zero.
//
// The value is a CEILING on any honest client mark, which is what makes a mark
// above it evidence of a retired seq space rather than of a fast frontend.
func (m *Manager) lastSeenSeq(d *sessionController) uint64 {
	durable := m.cfg.SeqStore.LastSeq(d.sessionID)
	if retained := d.consumer.newestRetainedSeq(); retained > durable {
		return retained
	}
	return durable
}

// exclusiveLowerBound converts an INCLUSIVE first-seq-to-replay into the
// EXCLUSIVE from_seq a core.v1 ReplayRequest carries (core.proto: "EXCLUSIVE
// lower bound, matching Subscribe.from_seq").
//
// Without the conversion a re-pull floored at a clear or a compaction would
// serve everything AFTER it and not the event itself — precisely the bubble the
// frontend needs, missing in exactly the case that matters most: a restarted
// daemon, whose ring is empty, so every replay goes through the store.
//
// A floor of 0 stays 0: there is no seq below it to be exclusive of, and
// underflowing to MaxUint64 would ask the store to replay nothing at all.
func exclusiveLowerBound(inclusive uint64) uint64 {
	if inclusive == 0 {
		return 0
	}
	return inclusive - 1
}

// PendingPermissions lists the request ids of the workspace's unresolved
// permission prompts (GET /sessions pending_permissions, SUPERSEDED S7). A
// workspace with no live session controller has none.
func (m *Manager) PendingPermissions(workspace string) []string {
	return m.reg.idsForWorkspace(workspace)
}

// ErrNotLiveSession reports that the workspace IS controlled, but by a DIFFERENT
// session than the one the caller asked to stand down. Distinct from the "no
// live session" error so a caller can tell "nothing to stop" (benign) from
// "that shim belongs to someone else — do not touch it". Its value lives in
// internal/errclass beside its classification; this is the historic name.
var ErrNotLiveSession = errclass.ErrNotLiveSession

// The two establishment-link anchors this package is the deepest holder of:
// there is no session controller at all for the workspace, and there is one whose
// connection never completed the handshake. Together with the shimclient
// sentinels and the shim's own health verdict they let a createSession nack
// name the deepest link that failed instead of reporting a bare bring-up
// error. Their values live in internal/errclass beside their classifications.
var (
	ErrNoLiveSessionController = errclass.ErrNoLiveSessionController
	ErrShimNotReady            = errclass.ErrShimNotReady
)

// Hibernate suspends the workspace's live session, WHICHEVER session that is:
// it stops consuming the stream and SIGTERMs the child shim (the redefined
// hibernation, §4.4). The registry record stays non-terminal (the caller owns
// that), so the next act revives it via a fresh reattach-first Ensure. A
// workspace with no live session controller is a loud error (nothing to hibernate). A
// workspace that has not SETTLED is refused with ErrNotSettled — the guard used
// to be "NEVER call this while a turn is active", left to each caller, and it is
// now mechanical inside hibernate().
//
// Use this only when the intent is workspace-scoped (the idle sweep or daemon
// shutdown). A terminal lifecycle operation standing down one specific record
// uses StopSession instead.
// cause NAMES THE CALLER, and every caller names a different one: the idle
// sweep, a merged teardown, an account switch, a drain execution and an
// ordinary shutdown are five different reasons a workspace's shim died, and the
// stop record has to be able to tell them apart (stopcause.go).
func (m *Manager) Hibernate(workspace string, cause StopCause) error {
	return m.hibernate(workspace, "", cause)
}

// StopSession terminates sessionID's shim without disturbing any different
// session driving workspace. It serves terminal lifecycle operations such as
// deletion and supersession, not hibernation: it may stop an active turn, and
// it never publishes the benign HIBERNATED state. The caller must make the
// session terminal before invoking it.
//
// If the matching controller is already evicted after a terminal client error,
// or a replacement owns workspace, the session-scoped stop reaches the spawner
// directly. Process handles are keyed by session id and must not become
// unreachable through byWS churn.
//
// Several registry records can share one cwd — a stale duplicate, a superseded
// resume, an orphan awaiting reap — so "stop THIS record's shim" is not the same
// question as "stop the workspace's shim". Answering it with the workspace-keyed
// Hibernate SIGTERMs whichever shim happens to be live, which on 2026-07-25
// meant reaping an orphan killed the healthy session created 175ms earlier for
// the same workspace, leaving the user with nothing to drive.
// HibernateSession is the compatibility name for an exact terminal stop. It
// intentionally does not publish HIBERNATED: the durable record is terminal,
// not a resumable sleeping session.
func (m *Manager) HibernateSession(workspace, sessionID string, cause StopCause) error {
	return m.stopSessionController(workspace, sessionID, cause)
}

func (m *Manager) StopSession(workspace, sessionID string) error {
	return m.stopSessionController(workspace, sessionID, StopCauseSessionDeleted())
}

// StopSessionForReplacement stops one exact session process so the same
// durable record can be relaunched under different process configuration. It
// is not hibernation and never publishes HIBERNATED.
func (m *Manager) StopSessionForReplacement(workspace, sessionID string) error {
	return m.stopSessionController(workspace, sessionID, StopCauseAccountSwitch())
}

// stopSessionController is the exact-session teardown shared by terminal
// lifecycle operations and intentional process replacement. It never reports
// HIBERNATED: the caller owns the terminal or replacement state.
func (m *Manager) stopSessionController(workspace, sessionID string, cause StopCause) error {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if ok && d.sessionID != sessionID {
		live := d.sessionID
		m.mu.Unlock()
		m.logf("session-controller: session-scoped hibernate ws=%q requested=%s live=%s; preserving live session controller and stopping requested shim only",
			workspace, sessionID, live)
		// NOT the sole controller, and no interrupt. The requested record's own
		// connection is not the one in hand — a DIFFERENT session drives this
		// workspace — so there is nothing here to interrupt over, and an
		// unattributed `thinking` may belong to the replacement rather than to
		// the shim being stopped. A claim naming the requested session is still
		// closed; anything else is left for the live session controller to report.
		//
		// NO RETAINED-RESULT RELEASE EITHER, and for the same reason. The only
		// consumer in reach belongs to the LIVE session, whose held results
		// belong to turns that session is still running and will still close;
		// publishing them from a stop aimed at a different record would answer
		// a live turn on behalf of a shim that is not being stopped. The
		// requested record's own consumer, if it ever had one, went with the
		// controller the replacement displaced.
		//
		// The caller's cause is REFINED rather than replaced: the record names
		// both what was asked for (a delete, a supersede) and what the daemon
		// found when it got here (a record a replacement had already taken).
		return m.stopShimSettlingTurn(workspace, sessionID, cause.supersededRecord(), false)
	}
	if ok {
		delete(m.byWS, workspace)
	}
	m.mu.Unlock()
	// THE DRAIN COMES BEFORE THE CANCEL. A terminal delete or supersession stands
	// a record down even when it is mid-turn, so the terminal stop must retire
	// the turn that can no longer emit its own completion.
	if ok {
		m.drainAndCancelSessionController(workspace, d, cause)
	}
	m.logf("session-controller: exact session stop ws=%q session=%s path=%s session_controller_present=%v (SIGTERM child shim)",
		workspace, sessionID, cause.path(), ok)
	if err := m.stopShimSettlingTurn(workspace, sessionID, cause, true); err != nil {
		return err
	}
	if ok {
		d.releaseControllerRegistration()
	}
	return nil
}

// hibernate is the shared teardown. An empty wantSession means "whichever
// session is live"; a non-empty one gates the teardown on identity. cause
// travels from the caller to both the funnel's log line and the shim's own stop
// record.
func (m *Manager) hibernate(workspace, wantSession string, cause StopCause) error {
	// THE SETTLED GUARD LIVES HERE, not in the idle sweeper, and the placement is
	// the point. "Never hibernate a workspace mid-turn" was a rule each caller
	// had to remember, which means it held only for the callers that did — the
	// sweeper's elapsed-quiet gate enforces it for the sweeper alone, and every
	// frontend command and future caller entered the same teardown ungated.
	// Inside the shared teardown it is mechanical: there is no path to a
	// hibernation of a working workspace left to find.
	//
	// It also protects the vocabulary. `hibernated` is ranked in the blue band
	// precisely so it cannot be masked by a stale agent row, which means a teal
	// tab OVER a live turn would be indistinguishable from a teal tab over a
	// settled one — the user would see "asleep" while the agent worked, with no
	// color anywhere to correct it. Refusing here is what makes that combination
	// unreachable by construction rather than merely unlikely.
	releaseHibernationLease, err := m.acquireSettledHibernationLease(workspace)
	if err != nil {
		return err
	}
	defer releaseHibernationLease()
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if !ok {
		m.mu.Unlock()
		return fmt.Errorf("%w: workspace %q", errNoLiveSessionToHibernate, workspace)
	}
	if wantSession != "" && d.sessionID != wantSession {
		live := d.sessionID
		m.mu.Unlock()
		return fmt.Errorf("%w: workspace %q is controlled by session %s, not %s",
			ErrNotLiveSession, workspace, live, wantSession)
	}
	delete(m.byWS, workspace)
	m.mu.Unlock()
	// THE DRAIN COMES BEFORE THE CANCEL, which ends client.Run and takes the
	// connection an interrupt would travel over with it. The hibernation lease
	// proves no turn is active and prevents a new one from beginning throughout
	// this stop sequence; the drain therefore reconciles only already-settled
	// transport bookkeeping.
	//
	// A settled turn can still be one whose result this daemon holds — a hold
	// outlives the answer's arrival and is discharged by the turn's end
	// boundary, which a hibernation is precisely what prevents from ever
	// arriving — so the release rides the same prologue (turnstop.go).
	m.drainAndCancelSessionController(workspace, d, cause)
	m.logf("session-controller: hibernating ws=%q session=%s (SIGTERM child shim)", workspace, d.sessionID)
	if err := m.stopShimSettlingTurn(workspace, d.sessionID, cause, true); err != nil {
		return err
	}
	// HIBERNATED means StopShim has completed, including release of the
	// session lock that gates a following restoration. The controller exit tail
	// is silent for this retired generation, so no later connectivity edge can
	// overwrite this completed teardown.
	m.noteConnectivity(workspace, d.sessionID, d.generationID, ssm.SessionConnectivityHibernated, "hibernated")
	// A deliberate stand-down retires whatever streak of bring-up failures the
	// session had accumulated, so a revival climbs the ladder from the bottom
	// rather than inheriting a park (bringupescape.go).
	m.clearBringUpFailures(d.sessionID)
	return nil
}

// bringUpTimeout bounds how long ensure waits for a spawned shim to connect
// and handshake. It is a FAILURE bound, not a tuned delay: ensure returns the
// instant the connection is ready, and this only decides how long we wait
// before declaring the shim genuinely dead. Generous enough that a loaded
// machine never trips it spuriously.
//
// IT IS A BOUND ON SILENCE, not on elapsed time (shimclient.Config.BringUpStall
// carries the full account). A shim streaming its replayed backlog is working,
// however long the backlog takes; a shim that has said nothing for this long is
// not.
const bringUpTimeout = 30 * time.Second

// bringUpProgressCap is the ABSOLUTE ceiling on one bring-up, whatever the shim
// is doing. The silence bound above is what normally resolves a bring-up, and
// it cannot bound a shim that trickles a frame every few seconds forever, so
// this caps the whole attempt.
//
// It is deliberately far larger than bringUpTimeout: the case it exists for is
// pathological, and the case it must NOT cut short — a genuinely large
// conversation replaying into the daemon's sinks — is ordinary.
const bringUpProgressCap = 10 * time.Minute

// ensure returns a session controller that is READY TO DRIVE: the shim is running and its
// connection has completed the handshake, so a control send will not fail with
// ErrNotConnected.
//
// This is the contract every caller already assumed and the code did not keep.
// bringUp only starts the connect loop, so sending immediately after it raced
// the shim's boot — and on a cold workspace (the idle sweep hibernates
// everything not mid-turn) the send lost that race and the user's prompt was
// rejected with "no live shim connection" about 500ms before the connection
// came up.
//
// The wait is on the connection EVENT, never on a duration: AwaitReady returns
// when the handshake lands. It also covers the RECONNECT window, because a
// workspace already in byWS can still be mid-reconnect with no live connection.
func (m *Manager) ensure(ctx context.Context, workspace string) (*sessionController, error) {
	d, err := m.bringUp(workspace)
	if err != nil {
		return nil, err
	}
	if err := m.awaitDriveable(ctx, d); err != nil {
		// EVERY bring-up resolves — see bringupescape.go. The ladder either
		// returns a session controller that really is wired, or a loud error whose failure
		// card and closed axis have already been published.
		return m.escapeFailedBringUp(ctx, workspace, d, err)
	}
	return d, nil
}

// existing returns the live session controller for workspace, or a loud error when there is
// none (no lazy bring-up: interrupt/resync/answer for an unbrought-up workspace
// is a caller error, distinct from a first prompt which brings it up).
func (m *Manager) existing(workspace string) (*sessionController, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if d, ok := m.byWS[workspace]; ok {
		return d, nil
	}
	return nil, fmt.Errorf("session-controller: no live session for workspace %q: %w", workspace, ErrNoLiveSessionController)
}

// ensure returns the live session controller for workspace, bringing it up (reattach-first
// spawn + shimclient) on first use.
//
// bringUp STARTS the session: it spawns the shim if needed and launches the
// client's connect loop in a goroutine. It returns as soon as that is under
// way, so the returned controller is NOT yet driveable — `d.client` has no
// connection for the few hundred milliseconds the shim takes to boot, listen,
// and handshake. Anything about to SEND must use ensure instead.
func (m *Manager) bringUp(workspace string) (*sessionController, error) {
	d, _, err := m.bringUpTracked(workspace)
	return d, err
}

// bringUpTracked is bringUp plus the ownership fact recovery needs. created is
// false when a concurrent caller already owns the workspace, so a failed old
// generation can observe that winner without applying retry policy to it.
func (m *Manager) bringUpTracked(workspace string) (*sessionController, bool, error) {
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		return nil, false, fmt.Errorf("session-controller: manager closed")
	}
	if d, ok := m.byWS[workspace]; ok {
		m.mu.Unlock()
		return d, false, nil
	}
	m.mu.Unlock()

	// THE WORKSPACE-OWNERSHIP GATE, BEFORE ANY IDENTITY IS MINTED. A live shim
	// can own this workspace under a session id this daemon knows nothing about
	// — a survivor of a previous daemon, or a second id minted for the same
	// workspace — and every session-keyed probe downstream answers "free" for
	// it. Asked here because this is the line that leads to a spawn, and the
	// answer decides between adopting the survivor, waiting for it, and
	// refusing (survivingshim.go).
	if survivor, err := m.awaitSurvivingShim(workspace); err != nil {
		return nil, false, err
	} else if survivor != nil {
		return survivor, false, nil
	}

	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return nil, false, fmt.Errorf("session-controller: workspace %q has no live session to drive", workspace)
	}
	// THE STALENESS CHECK, BEFORE ANYTHING IS SPAWNED. A record restored at
	// boot can be long past the keep-alive thresholds while its flag still says
	// awake, and bringing it up would put a warm-looking session in front of
	// the user until the next sweep noticed. Asked here rather than only in the
	// prompt gate because this is the line that spawns the process: a stale
	// session sleeps without one ever being started, and the caller gets the
	// SAME typed refusal the gate produces, so no route can turn the sleep into
	// a silent skip (hibernation.go).
	if detail, took := m.hibernateIfStale(workspace, sessionID); took {
		m.logf("session-controller: bring-up REFUSED by the revival gate ws=%q session=%s cause=%s since_ms=%d — the record was found stale at bring-up and hibernated instead; no shim was spawned",
			workspace, sessionID, detail.Cause, detail.SinceMs)
		return nil, false, fmt.Errorf("%w: workspace %q session %s has been asleep since %d (%s)",
			ErrHibernated, workspace, sessionID, detail.SinceMs, detail.Cause)
	}
	// THE GIVE-UP BOUND, BEFORE ANYTHING IS SPAWNED. A session that has already
	// failed bring-up bringUpGiveUpAfter times in a row is parked: its failure
	// card is standing, and every further attempt would cost another
	// bring-up window during which the daemon dispatches nothing.
	//
	// THE PARK EXPIRES ON ITS OWN (bringupescape.go). This call is what releases
	// it, so the refusal below is only ever "not yet", never "not again" — and
	// it says how long "not yet" is, plus the two things that end the park
	// immediately, because a refusal the user cannot act on is a dead workspace
	// with extra steps.
	if remaining, parked := m.bringUpParkRemaining(sessionID); parked {
		m.logf("session-controller: bring-up REFUSED by the give-up bound ws=%q session=%s consecutive_failures=%d bound=%d retry_in=%s — the session is parked on its standing failure card and no shim was spawned; the park expires on its own, and a hard restart or a hibernation ends it now",
			workspace, sessionID, m.bringUpFailuresFor(sessionID), bringUpGiveUpAfter, remaining.Round(time.Second))
		return nil, false, fmt.Errorf("%w: workspace %q session %s failed bring-up %d times in a row, so it is resting for another %s — opening it again after that retries automatically, and a hard restart (RestartSession) retries right now",
			ErrBringUpGaveUp, workspace, sessionID, m.bringUpFailuresFor(sessionID), remaining.Round(time.Second))
	}
	generationID, err := m.newControllerGenerationID()
	if err != nil {
		m.logf("session-controller: controller generation mint FAILED ws=%q session=%s decision=abort_before_spawn error=%v",
			workspace, sessionID, err)
		return nil, false, err
	}
	if generationID == "" {
		err := fmt.Errorf("session-controller: controller generation factory returned an empty id")
		m.logf("session-controller: controller generation mint FAILED ws=%q session=%s decision=abort_before_spawn error=%v",
			workspace, sessionID, err)
		return nil, false, err
	}
	m.logf("session-controller: controller generation minted ws=%q session=%s generation=%s decision=begin_bring_up",
		workspace, sessionID, generationID)
	rawReleaseRegistration, err := m.cfg.SSM.AcquireControllerRegistration(workspace, sessionID, generationID)
	if err != nil {
		return nil, false, fmt.Errorf("session-controller: reserve controller registration for workspace %q: %w", workspace, err)
	}
	var releaseRegistrationOnce sync.Once
	releaseRegistration := func() { releaseRegistrationOnce.Do(rawReleaseRegistration) }
	registrationTransferred := false
	defer func() {
		if !registrationTransferred {
			releaseRegistration()
		}
	}()
	spawn, err := m.cfg.Spawner.EnsureShim(m.rootCtx, sessionID)
	if err != nil {
		return nil, false, fmt.Errorf("session-controller: ensure shim for session %s (ws %q): %w", sessionID, workspace, err)
	}

	d := &sessionController{
		sessionID: sessionID, workspace: workspace,
		generationID: generationID, resumedVendorSessionID: spawn.Resumed,
		faulted:                       make(chan struct{}),
		buildRefreshStarted:           make(chan struct{}),
		controllerRegistrationRelease: releaseRegistration,
	}
	cons := newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, m.cfg.Progress, m.cfg.ClearCompactStore, m.cfg.TurnAccountings, m.logf, func(ss *corev1.SessionStarted) {
		m.persistVendorSessionID(sessionID, ss.GetVendorSessionId())
	}, func(active bool, atMs int64) {
		m.onTurnBoundary(d, active, atMs)
	}, func(state string) {
		m.persistBackfillState(sessionID, state)
		// The SSM composes green from this: a failed backfill is blue, and
		// a settled one releases the axis so the workspace can be ready.
		if err := m.cfg.SSM.ApplyBackfillState(workspace, state); err != nil {
			m.logf("session-controller: applying backfill %s to the SSM (ws %q): %v", state, workspace, err)
		}
	}, func(si *datav1.SystemInit) {
		m.persistObservedModel(sessionID, si.GetModel())
	}, func() {
		m.persistSessionDeath(sessionID, errclass.DeathReasonShimDied)
	})
	// The WARN channel for the consumer's user-visible degradations (degraded
	// accounting, failure cards, rejected events). Bound before Run so no such
	// record can be emitted at info.
	cons.warnf = m.warnf
	cons.historicalUsageStore = m.cfg.HistoricalUsage
	cons.generationID = generationID
	// The durable receipt ledger. Bound before Run, so no durable user line can
	// reach attributeUserTurn — the retirement point — with this unset.
	cons.receipts = m.cfg.PromptReceipts
	// The keep-alive exclusion's evidence. Bound before Run, so no conversation
	// item can reach the curation block with the ledger unset and be rendered
	// as though the user had typed it.
	cons.keepAliveWindows = m.cfg.KeepAliveWindows
	// The keep-alive window's LOWER bound, moved onto the vendor's clock by the
	// ping's own start boundary. Bound before Run for the same reason the ledger
	// itself is: the very first boundary a ping produces is the one that carries
	// the instant, and there is no second chance to observe it.
	cons.onTurnStarted = func(turnID string, atMs int64) {
		m.restampKeepAliveWindowStart(d, turnID, atMs)
	}
	// A rewind's discarded turns hold claims in the seq space it retires, and
	// nothing in the new space will ever deliver their ends (sessionrewound.go).
	if superseder, ok := m.cfg.SSM.(TurnClaimSuperseder); ok {
		cons.turnSuperseders = superseder
	}
	// The per-turn wait (mergeresolve.go) rides the SAME stream the queue's
	// edges do, but correlated by turn id rather than by edge. Bound before Run,
	// so no boundary can reach the consumer with this unset.
	cons.onTurnEvent = func(started bool, turnID string, outcome turnOutcome) {
		// THE WAITERS ONLY. This hook used to bind the drain hold's turn id too,
		// which put the id's write AFTER the boundary latch and after everything
		// the SSM apply publishes; the record is now bound from the durable claim
		// set instead (onTurnClaims, below), and this is left to the one thing it
		// is for.
		m.onTurnEvent(d, started, turnID, outcome)
	}
	// The turn record's binding edge: the SSM's durable claim ledger has just
	// accepted a boundary, and nothing user-visible has moved yet.
	cons.onTurnLiveness = func(l ssm.TurnLiveness) {
		m.noteTurnLiveness(d, l)
	}
	// The keep-alive policy's measuring point. Persisted per accepted turn end,
	// which is what makes every later decision a time-since check against a
	// durable instant rather than a timer nothing can restore (hibernation.go).
	if m.cfg.Hibernations != nil {
		cons.onTurnEnded = func(atMs int64) {
			m.cfg.Hibernations.TurnEndObserved(sessionID, atMs)
		}
	}
	// WHAT EVERY TURN'S TERMINAL RESULT MEASURED, routed to the three decisions
	// that read it: the keep-alive ping's own verdict on the cache it was sent
	// to refresh (keepalivecold.go), the daemon compaction's cold-read alarm
	// (compactioncold.go), and the conversation size the warm-compaction floor
	// is judged against (warmcompact.go). Bound before Run, so a result can
	// never reach the consumer with nothing to latch it in: for the ping it is
	// the ONE observation the feature makes about its own premise, and it
	// arrives exactly once.
	cons.onTurnResultCost = func(cost turnResultCost) {
		m.noteTurnResultCost(d, cost)
	}
	// Every PERSISTENT store event names the conversation it belongs to.
	// Keeping the record current off the live stream is what gives a later
	// handshake's announcement something to DIFFER from — a rotation is
	// invisible against an empty record. Bound before Run, so no event can
	// reach the consumer with this unset.
	cons.onVendorSessionID = func(vendorSessionID string) {
		m.persistVendorSessionID(sessionID, vendorSessionID)
	}
	if observer, ok := m.cfg.Registrar.(TerminalAccountingObserver); ok {
		cons.onTerminalAccountingPersisted = func() { observer.TerminalAccountingPersisted(sessionID) }
	}
	if observer, ok := m.cfg.Registrar.(HistoricalTokenUtilizationObserver); ok {
		cons.onHistoricalUsagePersisted = func() { observer.HistoricalTokenUtilizationPersisted(sessionID) }
	}
	cons.onQueryTermination = func(detail *frontendv1.QueryTerminationFailure) {
		m.noteBringUpTermination(d, detail)
	}
	cons.onDegraded = func(ds *corev1.DegradedState) { m.noteBringUpFault(d, ds) }
	d.consumer = cons
	// Settle the backfill for a REOPENED session before any event flows.
	//
	// The live derivation can only witness a backfill happening; a session
	// whose transcript was ingested in an earlier run produces no new line to
	// witness, because the sidecar's cursor already sits at that file's tail.
	// Reading the durable high-water instead answers the same question from
	// the record, so a fully-backfilled workspace does not sit in "starting"
	// forever waiting for evidence that will never come again.
	cons.settleBackfillFromStore(m.cfg.SeqStore.LastSeq(sessionID))
	// onPermsChanged republishes the footer's pending-permission badge on both
	// edges of a permission's life, and moves the SSM's permission row off the
	// same count. The queue depth is read back off the live queue so the two
	// counters are always reported together and neither can go stale behind the
	// other.
	ph := permHandler{reg: m.reg, cons: cons, logf: m.logf, onPermsChanged: func() {
		m.mu.Lock()
		depth := int64(len(d.queue.entries))
		m.mu.Unlock()
		m.noteProgressCounts(workspace, depth)
		m.notePermissionState(workspace)
	}}

	runCtx, cancel := context.WithCancel(m.rootCtx)
	d.cancel = cancel
	client := m.newClient(shimclient.Config{
		SessionID:       sessionID,
		Source:          m.cfg.Source,
		DaemonVersion:   m.cfg.DaemonVersion,
		ProtocolVersion: m.cfg.ProtocolVersion,
		SeqStore:        m.cfg.SeqStore,
		// The bring-up gate fails on SILENCE from this shim, not on elapsed
		// time, so a long conversation's replay cannot time its own session out.
		BringUpStall: bringUpTimeout,
		// The durable authority on what is in flight, so a reconnect rebuilds
		// the accounting pin set instead of discarding it.
		OpenTurnClaims:  m.cfg.SSM,
		Workspace:       workspace,
		PermissionModes: m.cfg.PermissionModes,
		StateSink:       cons,
		TurnClaims:      cons,
		Rewinds:         cons,
		FrameSink:       cons,
		Models:          modelCatalogReporter{m: m},
		FileDiagnostics: fileDiagnosticSink{persister: m.cfg.FileDiagnostics, workspace: workspace, agentReplSessionID: sessionID},
		Degraded:        cons,
		Permissions:     ph,
		OnHandshake: func(hello *corev1.ShimHello) error {
			return m.onHandshakeForGeneration(workspace, sessionID, generationID, hello)
		},
		OnConnected: func(hello *corev1.ShimHello) bool {
			return m.onConnectedForGeneration(workspace, sessionID, generationID, hello)
		},
		OnLinkLost: func(cause error) { m.onLinkLostForGeneration(workspace, sessionID, generationID, cause) },
		Logf:       m.logf,
		Warnf:      m.warnf,
		Errorf:     m.errorf,
	})
	d.client = client

	// Race: two concurrent first-prompts for the same workspace. Re-check under
	// the lock; if another goroutine won, tear ours down and use theirs.
	m.mu.Lock()
	if existing, ok := m.byWS[workspace]; ok {
		m.mu.Unlock()
		cancel()
		// The winner owns the axis: this loser's teardown must not report the
		// workspace unwired while a live session controller holds it.
		return existing, false, nil
	}
	m.byWS[workspace] = d
	m.mu.Unlock()

	// A BRING-UP IS NOW IN FLIGHT, and that is what `starting` means — not that
	// a session is wanted, which is a wish rather than a fact.
	//
	// Written AFTER the registration above rather than before the spawn, and
	// that placement is the whole guard on the concurrent-first-prompt race: the
	// loser of that race returns early with the winner's controller and never
	// touches the axis, so a `starting` can never land on top of a `wired` the
	// winner has already earned. What is left uncovered is the spawn itself,
	// which is a process exec; the window the user actually waits through is the
	// handshake, and that is entirely after this point.
	if err := m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityConnecting, "bring_up"); err != nil {
		m.mu.Lock()
		if current, exists := m.byWS[workspace]; exists && current == d {
			delete(m.byWS, workspace)
		}
		m.mu.Unlock()
		// The shared prologue, because this abort owns the whole teardown: no
		// exit tail runs for it (the Run goroutine below is not launched yet),
		// so this is the only place its shim can be interrupted and the only
		// place its consumer's held results can still be published.
		m.drainAndCancelSessionController(workspace, d, StopCauseBringUpFailed())
		stopErr := m.stopShimSettlingTurn(workspace, sessionID, StopCauseBringUpFailed(), true)
		if stopErr != nil {
			return nil, false, fmt.Errorf("%w; stopping rejected generation: %v", err, stopErr)
		}
		return nil, false, err
	}

	// The Add is taken UNDER m.mu with a closed re-check, exactly like
	// runPendingResync's: Close sets `closed` under this lock and then waits on
	// `exits`, so a bare Add here could land while that Wait is already running
	// on a zero counter — the WaitGroup reuse panic. Once closed, no new exit
	// goroutine may start; the bring-up aborts and mirrors the exit tail's
	// manager-close path instead (evict, drop the queue, preserve the shim).
	m.mu.Lock()
	if m.closed {
		wasCurrent := false
		if cur, ok := m.byWS[workspace]; ok && cur == d {
			delete(m.byWS, workspace)
			wasCurrent = true
		}
		dropped := d.queue.drainAll()
		view := d.queue.view(workspace, sessionID)
		m.mu.Unlock()
		if len(dropped) > 0 {
			m.logf("session-controller: session %s bring-up aborted with %d queued prompt(s) undelivered ws=%q",
				sessionID, len(dropped), workspace)
		}
		m.publish(sessionID, view, nil)
		if wasCurrent {
			m.logf("session-controller: session %s bring-up aborted by a manager close ws=%q; PRESERVING the shim for the next daemon to reattach",
				sessionID, workspace)
		}
		m.logf("session-controller: session %s bring-up FAILED ws=%q generation=%s reason=manager_closed was_current=%t dropped_prompts=%d decision=abort",
			sessionID, workspace, generationID, wasCurrent, len(dropped))
		return nil, false, fmt.Errorf("session-controller: manager closed during bring-up of workspace %q", workspace)
	}
	m.exits.Add(1)
	registrationTransferred = true
	m.mu.Unlock()
	go func() {
		defer m.exits.Done()
		defer d.releaseControllerRegistration()
		runErr := client.Run(runCtx)
		if runErr != nil {
			m.logf("session-controller: session %s session controller ended: %v", sessionID, runErr)
		}
		m.mu.Lock()
		wasCurrent := false
		if cur, ok := m.byWS[workspace]; ok && cur == d {
			delete(m.byWS, workspace)
			wasCurrent = true
		}
		// Read under the SAME lock the eviction took: a manager close is the
		// difference between "this session controller died" and "the daemon is going away",
		// and the shim-stop decision below turns entirely on which it is.
		managerClosing := m.closed
		// The session is gone, so its held prompts can never be delivered.
		// Empty the queue and PUSH the empty view: a frontend that keeps
		// rendering chips for a dead session is offering the user controls
		// that do nothing.
		//
		// UNLESS THE QUEUE IS NOT THIS TAIL'S TO EMPTY. A rewind takes the
		// entries into its own ownership before it stops the shim, so the exit
		// this stop causes finds nothing here — and the publish is skipped with
		// it, because pushing the empty view and persisting nil records is
		// exactly how the durable evidence of prompts still owed was lost.
		migrating := d.queueMigrating
		dropped := m.drainQueueForExitLocked(d)
		view := d.queue.view(workspace, sessionID)
		m.mu.Unlock()
		if len(dropped) > 0 {
			m.logf("session-controller: session %s ended with %d queued prompt(s) undelivered ws=%q",
				sessionID, len(dropped), workspace)
		}
		// THE LAST CHANCE ANY HELD RESULT GETS. Run has returned, so no further
		// event can reach this consumer and no `TurnEnded` can discharge a hold
		// it is still carrying. Unconditional, because the reason Run ended
		// changes nothing about a turn whose answer is already in hand, and it
		// covers two distinct arrivals:
		//
		//   - a shim that DIED on its own went through no teardown prologue at
		//     all, so this is the only edge its held results ever get; and
		//   - a teardown's own interrupt is waited on to the ACK, not to the
		//     terminal event that ack causes, so a result landing between the
		//     prologue's release and the cancel is held with nothing left to
		//     free it. This frees it.
		//
		// What no edge can reach is a result the shim never managed to send
		// before the stop, because it is not evidence this daemon ever held.
		m.releaseHeldTerminalResults(d, StopCauseControllerExit())
		if !migrating {
			m.publish(sessionID, view, nil)
		}
		// THE WIRING IS GONE with the session controller, and `runErr` is what says whether
		// that is a FAULT or a teardown we asked for. Only the CURRENT controller
		// reports it at all: a superseded one exiting says nothing about the
		// replacement that now owns the workspace, and unwiring that replacement
		// would be a lie about a live session.
		//
		// A NON-NIL runErr IS THE ONLY THING THAT MEANS BROKEN. `client.Run`
		// loops forever across benign disconnects and returns non-nil only for a
		// terminal protocol error (see internal/shimclient/client.go), so a
		// non-nil answer here is genuine evidence the substrate failed.
		//
		// A NIL runErr WRITES NOTHING, and the silence is load-bearing rather
		// than an omission. This tail fires on the SAME workspace milliseconds
		// after a hibernation, because the hibernation's own cancel is what ended
		// Run — so a tail that wrote `severed` unconditionally would repaint
		// every hibernation blue the instant after it went teal, which is the
		// whole split undone. Every clean cancel of a session controller ctx has already
		// recorded a truer answer than this tail could:
		//
		//   - HIBERNATION (Manager.hibernate) writes `hibernated` at the instant
		//     where the benign reason is known. StopSession belongs only to a
		//     registry record already marked terminal, whose terminal SessionView
		//     is the authoritative state.
		//   - A FAILED BRING-UP's escape (bringupescape.go) tears the session controller down
		//     and writes `severed` itself, naming the bring-up that failed.
		//   - A MANAGER CLOSE cancels the root ctx. The axis it leaves behind is
		//     rewritten for every workspace by the next boot's
		//     hibernateEveryWorkspaceLocked, so nothing this tail wrote would
		//     survive to be read.
		//   - THE CONCURRENT-FIRST-PROMPT LOSER cancels before this goroutine is
		//     ever launched, so it produces no tail at all.
		//
		// That enumeration is exhaustive over the cancels of a session controller ctx, which
		// is why no `hibernating` flag is needed to discriminate here: the axis is
		// already correct on every clean path, and re-stating it can only be
		// wrong.
		if wasCurrent && runErr != nil {
			m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityUnavailable, "session_controller_exit")
		}
		if wasCurrent && runErr == nil {
			m.logf("session-controller: session %s session controller exited CLEANLY ws=%q; leaving the legacy connectivity projection to whoever asked for the teardown (a hibernation already recorded `hibernated`, a failed bring-up already recorded `severed`)",
				sessionID, workspace)
		}
		// A MANAGER CLOSE IS NOT A DEAD SESSION CONTROLLER, and stopping the shim here on
		// one would silently defeat the daemon's preserve-on-shutdown contract:
		// the whole point of a preserved shim is that it outlives this process,
		// redials the daemon socket and parks for the next boot to reattach.
		// The teardown that closed the manager owns that decision (see
		// server.ShutdownAll), and it has already acted on it by the time this
		// runs, so re-deciding it here could only ever contradict it.
		if wasCurrent && managerClosing {
			m.logf("session-controller: session %s session controller exiting on a manager close ws=%q; PRESERVING the shim for the next daemon to reattach",
				sessionID, workspace)
		}
		// A terminal protocol error ends Run while this session controller is still current,
		// without going through Hibernate. That used to orphan the spawned shim
		// and its stop handle after the byWS eviction above. A non-current Run
		// exit was initiated by a teardown that already owns StopShim.
		//
		// NO DRAIN HERE, and its absence is not an omission. This tail runs
		// BECAUSE client.Run ended, so the connection an interrupt would travel
		// over is already gone; asking for one could only produce a nack. The
		// stop still routes through the funnel, so the axis is closed either
		// way — which is what makes a shim that DIED mid-turn, rather than one
		// this daemon stopped, unable to latch the workspace in `thinking`.
		if wasCurrent && !managerClosing {
			if stopErr := m.stopShimSettlingTurn(workspace, sessionID, StopCauseControllerExit(), true); stopErr != nil {
				m.logf("session-controller: session %s unexpected session-controller-exit shim stop FAILED ws=%q run_err=%v: %v",
					sessionID, workspace, runErr, stopErr)
			} else {
				m.logf("session-controller: session %s unexpected session-controller-exit shim stop complete ws=%q run_err=%v",
					sessionID, workspace, runErr)
			}
		}
		// A MANAGER CLOSE DELIBERATELY LEAVES THE AXIS ALONE. The shim is
		// PRESERVED there, so it is still running the turn and will report that
		// turn's end to whichever daemon it redials — closing the axis would be
		// a lie about a live turn, which is the one thing worse than a stale
		// claim. The workspace's own reattach handshake settles the axis if the
		// turn really did finish in the gap (ssm.ReconcileTurnHandshake).
	}()
	m.logf("session-controller: brought up session=%s ws=%q (reattach-first)", sessionID, workspace)
	return d, true, nil
}

// onHandshake adopts the vendor session uuid a (re)handshaking shim announces,
// and reconciles the daemon across a ROTATION of it.
//
// It runs BEFORE the shimclient reads its Subscribe position (shimclient
// Config.OnHandshake), which is what makes a rotation survivable at all: the
// shim minted a new store seq space starting at 1, and a Subscribe resuming
// from the retired space's high-water mark would ask for events that will never
// come and then read seq=1 as a terminal regression.
//
// Three things move together on a rotation, in this order:
//
//  1. THE REGISTRY. The new uuid is persisted and the conversation's cursors
//     (last_seq, and the replay floor that counts in the same space) are reset
//     to zero — one indivisible write, because the registry re-hydrates a
//     record's cursors from the checkpoint filed under its CURRENT uuid.
//  2. THE RETAINED CONVERSATION RING. Every item it holds is keyed to the
//     RETIRED seq space, and so is every ceiling derived from it. See
//     purgeRetainedOnRotation and consumer.purgeRetained.
//  3. THE QUEUE'S TURN OBSERVATION. The turn in flight when the uuid changed
//     will report its end under the NEW identity, so the daemon's turn-active
//     flag is cleared rather than left standing on a boundary nothing will
//     close. An interrupt mark riding that turn is dropped with it.
//  4. THE SSM. Same reconciliation on the render axis (ApplySessionRotated).
//
// The events themselves are not lost — they are in the store under the retired
// key, and the Subscribe this precedes replays the NEW space from zero,
// ContextCleared included.
//
// ---------------------------------------------------------------------------
// THE SEQ-HOLDER INVENTORY — the checklist a new one must join
// ---------------------------------------------------------------------------
//
// A store seq is only meaningful inside ONE vendor seq space. Every per-session
// place the daemon holds, compares, or persists one is listed here, and each is
// either RESET on a rotation or carries a stated reason why a stale value is
// harmless. Adding a new seq holder means adding a line here.
//
// RESET ON ROTATION:
//
//   - registry Record.LastSeq — the Subscribe high-water. Zeroed inside
//     RegistryRegistrar.AdoptVendorSessionID, in the same write that adopts the
//     new uuid.
//   - registry Record.NewestClearOrCompactSeq — the durable replay floor.
//     Zeroed in that same write.
//   - registry ConversationCheckpoint (both cursors) — re-filed under the NEW
//     uuid by that write, so the hydrate-up on the next mutation reads a fresh
//     checkpoint at zero rather than the retired one.
//   - shimclient Client.lastSeen — re-read from the SeqStore on every
//     runOnce, which is why OnHandshake (this hook) must run BEFORE the
//     Subscribe reads it. It is ALSO reset outside this hook: a rotation is not
//     the only way a seq space is retired, and a shim that restarts under an
//     unchanged vendor uuid announces nothing here while still renumbering
//     from 1. The mark carries the shim generation that advanced it and is
//     rebased on the new generation's first event — shimclient
//     seqgeneration.go.
//   - consumer.ring — purged here (purgeRetainedOnRotation). NAMED
//     CONSEQUENCE: the frontend TaskCatalog is rebuilt from this same ring, so
//     a detached task whose start was only ever seen in the retired space drops
//     off the roster until the next `BackgroundTasksChanged` — which is the
//     AUTHORITATIVE live set and re-establishes the whole roster when it lands.
//     Erring toward an empty roster is the right way round: the alternative is
//     a roster keyed to seqs from a conversation that no longer exists.
//   - consumer.newestRetainedSeq() — derived from the ring; empty after the
//     purge, so the ceiling in Manager.lastSeenSeq falls back to the durable
//     mark, which the registry just zeroed.
//   - consumer.ringFloor() — derived from the ring, and it is the re-pull's
//     stop_at. Empty after the purge, so Resync takes the floor from the
//     durable last_seen_seq instead (repull.go).
//   - sessionController.repull{fromSeq,stopAt} — an in-flight re-pull's bounds. Stamped
//     with sessionController.rotEpoch, which this bumps, so a post-rotation request can
//     never be coalesced onto a re-pull bounded in the retired space.
//   - sessionController.resyncRetried — the one-shot re-arm budget, refreshed here so a
//     re-pull that a SECOND bounce interrupts is still retried once.
//
// DELIBERATELY NOT RESET, and why stale values are harmless:
//
//   - consumer.permItems / failItems — daemon-composed items keyed by
//     request_id and card uuid; they carry NO store seq (through_seq stays 0)
//     and are replayed on every resync regardless of fromSeq, so no ceiling can
//     hide them. A pending permission also survives the shim bounce and is
//     re-asked on reattach, so dropping these would erase a live prompt.
//   - ssm workspace_state.cause_seq (the (session_id, cause_seq) idempotency
//     key) — the key is keyed by the event's session id, which IS the vendor
//     uuid, so the new space gets a fresh key space for free. (The paint
//     watermark that used to be the KNOWN residual here is gone with the whole
//     attestation model; nothing per-workspace holds a seq across a rotation
//     any more.)
//   - sessionController.metaCwd / backfill / systemInit / queue entries — carry no seq at
//     all; a rotation does not change what they describe.
func (m *Manager) onHandshakeForGeneration(workspace, sessionID, generationID string, hello *corev1.ShimHello) error {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	m.mu.Unlock()
	if !ok || d.sessionID != sessionID || d.generationID != generationID {
		err := fmt.Errorf("turn handshake has no matching live session controller")
		m.logf("session-controller: turn handshake decision=reject_no_matching_session_controller ws=%q session=%q generation=%q current_session=%q current_generation=%q active_turn_ids=%v error=%v",
			workspace, sessionID, generationID, controllerSessionID(d), controllerGenerationID(d), hello.GetActiveTurnIds(), err)
		return err
	}
	if err := d.consumer.accounting.bindHandshakeIdentity(hello); err != nil {
		m.logf("session-controller: query handshake decision=reject_identity ws=%q session=%q generation=%q query_instance_id=%q vendor_session_id=%q runtime_snapshot=%t error=%v",
			workspace, sessionID, generationID, hello.GetQueryInstanceId(), hello.GetVendorSessionId(), hello.GetQueryRuntimeIdentity() != nil, err)
		return fmt.Errorf("query handshake identity failed: %w", err)
	}
	active, phantomClosed, err := d.consumer.reconcileTurnHandshake(hello)
	if err != nil {
		reason := fmt.Sprintf("turn handshake correlation failed: %v", err)
		m.logf("session-controller: turn handshake decision=reject_correlation ws=%q session=%q generation=%q active_turn_ids=%v error=%v",
			workspace, sessionID, generationID, hello.GetActiveTurnIds(), err)
		m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityUnavailable, "turn_handshake_correlation_failed")
		return fmt.Errorf("%s", reason)
	}
	m.reconcileTurnSnapshot(d, active, hello)
	// A claim the returning shim contradicted has just been ended durably, so
	// the queue is owed the boundary that ending stands for. It is released at
	// ShimReady rather than here — see notePhantomTurnClosed.
	m.notePhantomTurnClosed(d, phantomClosed)
	// The pid rides EVERY hello, so a reconnect refreshes it and a bounce onto
	// a fresh process never carries the retired one's number forward.
	m.noteShimPID(sessionID, hello.GetPid())
	csid := hello.GetVendorSessionId()
	if csid == "" {
		// A fresh session whose shim has not learned its uuid yet. Announcing
		// nothing is the honest shape; the SessionStarted that follows carries
		// it (persistVendorSessionID).
		return nil
	}
	if m.cfg.Registrar == nil {
		m.logf("session-controller: shim announced vendor_session_id=%s ws=%q session=%s with NO registrar bound — a rotation cannot reset the store cursor and would be read as a seq regression",
			csid, workspace, sessionID)
		return nil
	}
	rotated, previous, adopted := m.cfg.Registrar.AdoptVendorSessionID(sessionID, csid)
	if !adopted {
		// The write itself failed (no record, or a registry error the adapter
		// already logged loudly). Never a deliberate refusal: adoption is
		// eager now, and whether the transcript exists is checked at resume.
		return nil
	}
	m.mu.Lock()
	m.lastCSID[sessionID] = csid
	m.mu.Unlock()
	if !rotated {
		return nil
	}
	m.logf("session-controller: VENDOR SESSION ROTATION ws=%q session=%s %s -> %s — the vendor retired one transcript identity mid-stream; store cursor and replay floor reset to zero and the subscription resumes from the new seq space's beginning",
		workspace, sessionID, previous, csid)
	// THE ROTATION IS A BOUNCE, so the wiring is genuinely absent for the window
	// between this announcement and the new ShimReady. Reporting it closes the
	// axis honestly rather than letting a rotating workspace keep claiming a
	// substrate that is mid-re-handshake; the ShimReady that follows re-opens it
	// through onConnected.
	m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityConnecting, "session_rotating")
	m.purgeRetainedOnRotation(workspace, sessionID, previous, csid)
	m.clearTurnOnRotation(workspace, sessionID, previous, csid)
	if err := m.cfg.SSM.ApplySessionRotated(workspace, previous, csid); err != nil {
		m.logf("session-controller: reconciling the SSM across the rotation FAILED ws=%q session=%s %s -> %s: %v (the workspace may stay in THINKING until the next turn)",
			workspace, sessionID, previous, csid, err)
	}
	return nil
}

func (m *Manager) onHandshake(workspace, sessionID string, hello *corev1.ShimHello) error {
	return m.onHandshakeForGeneration(
		workspace, sessionID, m.currentControllerGeneration(workspace, sessionID), hello,
	)
}

// reconcileTurnSnapshot restores the queue's process-local turn latch from the
// durable handshake result without pretending the snapshot is a TurnEnded
// edge. Sending it through onTurnBoundary would drain a queued prompt every
// time an already-idle shim reconnected; merely assigning it leaves the next
// real boundary as the sole drain trigger.
func (m *Manager) reconcileTurnSnapshot(d *sessionController, active bool, hello *corev1.ShimHello) {
	m.mu.Lock()
	before, changed := d.noteTurnAdoptedLocked(active)
	after := d.turn
	queueDepth := len(d.queue.entries)
	paused := d.paused
	m.mu.Unlock()
	m.logf("session-controller: turn snapshot reconciled ws=%q session=%s process_before=%s process_after=%s changed=%v hello_turn_in_flight=%v hello_turn_ids=%v queue_depth=%d paused=%v decision=set_without_boundary_effects",
		d.workspace, d.sessionID, before, after, changed, hello.GetTurnInFlight(),
		hello.GetActiveTurnIds(), queueDepth, paused)
}

// purgeRetainedOnRotation drops the retained conversation window belonging to
// the RETIRED seq space, and every ceiling derived from it.
//
// THE DEFECT THIS CLOSES. The rotation reset the registry's cursors and left
// the ring alone, so the daemon went on holding the old conversation's items
// with their old seqs. A frontend that rebased and asked to resync from zero
// was then served a re-pull bounded at `stop_at=1122` — a ceiling read straight
// out of the retired space, against a space that had reached 12 — which
// delivered nothing and reported a truncation. The feed was empty and the only
// thing in it was a failure card.
//
// The epoch bump is the other half: an in-flight re-pull's bounds were computed
// in the retired space, so a request arriving after the rotation must not be
// coalesced onto it (startRepull), and the one-shot re-arm budget is refreshed
// because a rotation is a legitimate reason for a second interruption.
func (m *Manager) purgeRetainedOnRotation(workspace, sessionID, previous, next string) {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if !ok || d.sessionID != sessionID {
		m.mu.Unlock()
		return
	}
	d.rotEpoch++
	epoch := d.rotEpoch
	d.resyncRetried = false
	inflight := d.repull
	m.mu.Unlock()

	dropped, ceiling := d.consumer.purgeRetained()
	logf := dlog.Tag(dlog.Logf(m.logf),
		"ws", workspace, "session", sessionID, "previous", previous, "next", next,
		"purged", dropped, "retired_ceiling", ceiling, "rotation_epoch", epoch)
	logf("session-controller: retained conversation ring PURGED across the vendor session rotation — every item and every seq ceiling it carried counted in the retired space; the ring is empty until the new space's events arrive")
	if inflight != nil {
		logf("session-controller: a history re-pull is IN FLIGHT across the rotation (from_seq=%d stop_at=%d, both retired-space numbers) — its bounds cannot cover the new space, so no later request will be coalesced onto it; a new-space request waits it out and then pulls its own range",
			inflight.fromSeq, inflight.stopAt)
	}
}

// clearTurnOnRotation drops the daemon's turn-in-flight observation for the
// rotated session, and with it any interrupt mark riding that turn.
//
// It does NOT go through onTurnBoundary. That path is the TURN-END DRAIN: it
// delivers the next held prompt, resumes a paused queue, and decides whether a
// lone runner finished cleanly — all of it reasoning about a turn that ENDED.
// A rotation is not an end, it is a loss of the identity the end will be
// reported under, and the real TurnEnded is moments away in the replay the
// Subscribe is about to open. Draining here would submit into a session
// mid-re-handshake and then drain a second time when that end lands.
func (m *Manager) clearTurnOnRotation(workspace, sessionID, previous, next string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS[workspace]
	if !ok || d.sessionID != sessionID {
		return
	}
	if d.interruptedTurn {
		d.interruptedTurn = false
		m.logf("session-controller: interrupt mark DROPPED as stale ws=%q session=%s (vendor session rotated %s -> %s) — the stopped turn's end belongs to the retired identity",
			workspace, sessionID, previous, next)
	}
	before, changed := d.noteTurnIdleLocked()
	if !changed {
		return
	}
	m.logf("session-controller: turn-in-flight observation CLEARED ws=%q session=%s before=%s (vendor session rotated %s -> %s) — the running turn's end will be reported under the new identity, and the record drops its phase and its id in ONE assignment so no name outlives the identity it was minted under",
		workspace, sessionID, before, previous, next)
}

// onConnected reconciles SSM turn state on a mid-turn reattach (task step 1):
// when the shim reports a turn in flight, the SSM must not read idle. The store
// replays events from last_seen_seq on Subscribe, so the SSM re-derives turn
// state from the replayed TurnStarted; this hook loud-logs the observation so a
// reconciliation gap is visible rather than silent.
func (m *Manager) onConnectedForGeneration(workspace, sessionID, generationID string, hello *corev1.ShimHello) bool {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	current := ok && d.sessionID == sessionID && d.generationID == generationID
	m.mu.Unlock()
	if !current {
		m.logf("session-controller: stale ShimReady ignored ws=%q session=%q generation=%q current_session=%q current_generation=%q branch=retired_controller",
			workspace, sessionID, generationID, controllerSessionID(d), controllerGenerationID(d))
		return true
	}
	// Build identity is a PRE-READINESS transition. A mismatched source is
	// registered for exact-generation replacement before the shim client may
	// release AwaitReady, and it never paints operational or releases queued
	// work on the generation being retired.
	m.noteShimPID(sessionID, hello.GetPid())
	if m.refreshStaleShim(workspace, sessionID, hello.GetBuildSha()) {
		return true
	}
	// THE BRING-UP GATE CLOSED. This hook fires from the shim's ShimReady, the
	// same frame AwaitReady resolves on, so "wired" here means exactly what
	// "driveable" means everywhere else: session lock held, SDK query built,
	// store producer link up, standing subscription open. It is the ONE opening
	// edge of the axis, and nothing weaker may write it.
	if err := m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityOperational, "shim_ready"); err != nil {
		m.logf("session-controller: operational connectivity edge rejected ws=%q session=%s generation=%s decision=cancel_generation error=%v",
			workspace, sessionID, generationID, err)
		d.cancel()
		return true
	}
	// THE BRING-UP GATE IS CLOSED. Anything that fails from here is a
	// mid-session fault, never an escapable bring-up failure.
	m.noteWired(workspace, sessionID)
	// The handover this session completes is now FACT, so the predecessors it
	// displaced stop being an open failure. Resolving here rather than at the
	// supersede itself is the whole point: at supersede time the successor did
	// not exist yet, and a card that closed then would have closed on a promise.
	if m.cfg.Registrar != nil {
		m.cfg.Registrar.SessionOperational(workspace, sessionID)
	} else {
		m.logf("session-controller: operational reached with NO registrar ws=%q session=%s — superseded predecessors keep their open death cards until a boot reconciliation stamps them",
			workspace, sessionID)
	}
	// And the SAME edge settles the degradation cards the withhold arms put up
	// from durable history, for the identical reason. A retired query's death is
	// a true account of something that happened, and it stops describing this
	// session the moment a live query genuinely has it. Because that row is
	// DURABLE it replays at every boot and would otherwise never acquire a
	// closing edge, leaving an unresolved failure card on a healthy session
	// forever. Resolution, not deletion — the card keeps its identity and its
	// detail and is re-sent with resolved_at_ms stamped (sinks.go).
	d.consumer.resolveWithheldDegradations("shim_ready")
	// The pid and the build identity are BOTH only trustworthy on a live
	// connection, and this is the moment the connection is proven usable. A
	// shim running a superseded bundle is bounced from here onto the current
	// one (buildrefresh.go); everything below still runs, because the bounce is
	// asynchronous and this connection remains the live one until it lands.
	if hello.GetTurnInFlight() {
		m.logf("session-controller: reattached mid-turn ws=%s session=%s turn_in_flight=true active_turn_ids=%v; SSM state is durable and replay closes any unseen boundary", workspace, sessionID, hello.GetActiveTurnIds())
	}
	// A resync whose store re-pull this very reattach interrupted is served
	// again here — the link being back IS the event it was waiting for, which is
	// why nothing sleeps or polls for it.
	m.runPendingResync(workspace, sessionID)
	// The queue owed a boundary by this connection's handshake — a turn cut by
	// a restart, whose claim the handshake ended durably — is released now that
	// the session is genuinely driveable (phantomturn.go). No-op when nothing
	// is owed, which is every ordinary reattach.
	m.releasePhantomTurn(d)
	// Prompts a PREVIOUS daemon parked behind a scheduled shutdown are put back
	// on this session's queue now, for the same reason the phantom release
	// waits for this frame: the session is genuinely driveable, so a restored
	// prompt can actually be delivered rather than parked a second time
	// (shutdownlease.go).
	m.restoreShutdownHolds(d)
	// This session is live, so it may be holding the drain open — a reattach
	// mid-turn does exactly that.
	m.noteDrainActivity()
	d.releaseControllerRegistration()
	return false
}

func (m *Manager) onConnected(workspace, sessionID string, hello *corev1.ShimHello) {
	_ = m.onConnectedForGeneration(
		workspace, sessionID, m.currentControllerGeneration(workspace, sessionID), hello,
	)
}

// Close stops every controller, abandons pending permissions (no fabricated
// answers), and JOINS every session-controller-exit goroutine before returning, so no
// teardown work of this manager's — queue drain, empty-view publish, the
// registry's queued_prompts persist, the orphan-shim stop — can outlive it.
// Idempotent.
func (m *Manager) Close() {
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		return
	}
	m.closed = true
	m.mu.Unlock()
	m.rootStop()
	m.reg.fail("manager closed")
	m.exits.Wait()
}

// permHandler bridges a session's canUseTool round-trip to the frontend: it
// pushes the permission ConversationItem (its resolution lifecycle), surfaces a
// permission render-state, and blocks on the rendezvous until the frontend
// answers (or teardown abandons it).
type permHandler struct {
	reg  *permRegistry
	cons *consumer
	logf func(string, ...any)
	// onPermsChanged fires whenever the workspace's pending-permission set
	// moves, so the progress footer's badge tracks it. Nil-safe.
	onPermsChanged func()
}

// permsChanged fires the pending-permission notification, if one is bound.
func (h permHandler) permsChanged() {
	if h.onPermsChanged != nil {
		h.onPermsChanged()
	}
}

func (h permHandler) HandlePermission(sessionID string, req *corev1.PermissionRequest) *corev1.PermissionResponse {
	h.logf("session-controller: permission prompt ws=%s session=%s request_id=%s tool=%s (awaiting frontend answer)",
		h.cons.workspace, sessionID, req.GetRequestId(), req.GetToolName())
	// Push the pending permission ConversationItem (uuid = request_id) through
	// the retained-ring pusher so a resync replays it (S8). It supersedes the
	// earlier WorkspaceState-only decision but does NOT replace the PERMISSION
	// render-state, which stays alongside.
	h.cons.pushPermission(permissionItem(req, corev1.PermissionItem_RESOLUTION_PENDING, ""))
	// THE PERMISSION RENDER-STATE IS NOT PUSHED FROM HERE. A hand-built
	// WorkspaceState carrying only a render state is a frame that cannot say
	// its session connectivity, status, controller generation or revision, and
	// the frontend contract has no reading for an UNSPECIFIED connectivity: the
	// webapp's validating decoder refuses the whole frame, so this shortcut
	// bought nothing and cost the frame it was trying to deliver.
	//
	// The authority is the SSM's permission row (ssm.ApplyPermission, THE only
	// producer of RENDER_STATE_PERMISSION), which the count edge below reaches
	// through onPermsChanged the moment this waiter parks. That push is
	// resolved, fully stamped and monotonically revisioned like every other
	// WorkspaceState this daemon emits.
	ch, release := h.reg.await(req.GetRequestId(), h.cons.workspace)
	// The waiter is parked, so the workspace's pending count just went up; and
	// however this returns, releasing it brings the count back down.
	h.permsChanged()
	defer func() {
		release()
		h.permsChanged()
	}()
	resp := <-ch
	if resp == nil {
		// Teardown abandoned the request (no response sent; the shim re-asks on
		// reattach). Push the ABANDONED resolution on the same uuid.
		h.cons.pushPermission(permissionItem(req, corev1.PermissionItem_RESOLUTION_ABANDONED, ""))
		return nil
	}
	res := corev1.PermissionItem_RESOLUTION_ALLOWED
	if resp.GetDecision() == corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		res = corev1.PermissionItem_RESOLUTION_DENIED
	}
	h.cons.pushPermission(permissionItem(req, res, resp.GetDenyMessage()))
	return resp
}
