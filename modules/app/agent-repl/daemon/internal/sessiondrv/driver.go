// Package sessiondrv is the daemon's per-session shim-driver: it binds a
// session's UDS claude-shim (via internal/shimclient) to the daemon's resolved
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
package sessiondrv

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strings"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/structpb"
)

// SessionRegistrar persists the durable CLI session uuid a session's
// SessionStarted carries (vendor_session_id), so --resume and cross-restart
// rehydration keep working after the L2 stdio plane that used to write it is
// gone. Bound in main to a registry-writing adapter; nil disables the write.
type SessionRegistrar interface {
	ClaudeSessionIDChanged(sessionID, claudeSessionID string)
	// AdoptVendorSessionID adopts claudeSessionID as the session's vendor uuid
	// and reports whether that ROTATED an already-adopted, DIFFERENT one, plus
	// what it replaced.
	//
	// A rotation retires the conversation's store seq space and starts a fresh
	// one at 1, so the adoption and the CURSOR RESET that must accompany it are
	// one indivisible act rather than two writes. Splitting them is not merely
	// untidy: the registry hydrates a record's cursors up from the checkpoint
	// filed under its CURRENT uuid on every write, so a reset landing while the
	// old uuid still stood would be undone before the new uuid was recorded.
	//
	// Idempotent for an unchanged uuid: rotated=false, nothing reset.
	AdoptVendorSessionID(sessionID, claudeSessionID string) (rotated bool, previous string)
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
}

// Spawner makes sure a session has exactly one live shim: it leaves an existing
// one alone (connected, or merely holding its session lock — the shim outlives
// a dead daemon, §4.4) or spawns a fresh one via ShimUDSArgv. The
// concrete impl lives in the server package (it owns the liveness checks and
// the spawn plumbing); injected here so the driver stays IO-narrow and testable.
type Spawner interface {
	EnsureShim(ctx context.Context, sessionID string) error
	// StopShim asks the session's shim to stop cleanly (the daemon SIGTERMs
	// its child shim on hibernation, §4.4 redefined). A shim the daemon never
	// spawned (a reattached one that outlived a prior daemon) is a no-op. A
	// stop failure is surfaced, never swallowed.
	StopShim(sessionID string) error
}

// SessionLocator maps a workspace to the live session id bound to it. The
// concrete impl reads the daemon's session registry (the non-terminal record
// whose cwd is the workspace). ok=false when the workspace has no live session.
type SessionLocator interface {
	Locate(workspace string) (sessionID string, ok bool)
}

// sessionClient is the slice of *shimclient.Client the driver drives. An
// interface so the manager's routing is unit-testable with a fake.
type sessionClient interface {
	Run(ctx context.Context) error
	AwaitReady(ctx context.Context) error
	// Health proves the already handshaked shim's own dependency boundary.
	// It MUST NOT cause a lazy bring-up; session readiness is false until the
	// existing live driver can answer this probe.
	Health(ctx context.Context, requestID string) (*corev1.HealthStatus, error)
	SubmitPrompt(ctx context.Context, text, origin, permissionMode string) error
	// Interrupt returns the shim's own verdict on what the stop did, which is
	// the only place that verdict is observable.
	Interrupt(ctx context.Context) (corev1.InterruptOutcome, error)
	// Replay asks the shim for a bounded slice of persisted history, streaming
	// it to onEvent. Its events arrive over the wire as ReplayEvent, a
	// different type from live Events, which is what keeps replayed history
	// out of the SSM/task/progress planes structurally (repull.go).
	Replay(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (shimclient.ReplayResult, error)
}

// Config assembles a Manager. Every collaborator is injected so the driver is
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
	// ClearCompactStore persists the newest clear-or-compaction seq — the
	// frontend replay floor (RegistrySeqStore again). Required: a driver that
	// observed a clear or a compaction and had nowhere to record it would serve
	// the next resync the very history that clear or compaction discarded, which
	// is the failure the floor exists to prevent.
	ClearCompactStore ClearCompactStore
	// DaemonVersion / ProtocolVersion travel in DaemonHello; ProtocolVersion
	// must equal the shim's ("1").
	DaemonVersion   string
	ProtocolVersion string
	// Registrar persists SessionStarted.vendor_session_id (the CLI session uuid)
	// through to the durable registry record; nil disables the write.
	Registrar SessionRegistrar
	// Logf is the daemon logger. Nil discards.
	Logf func(string, ...any)
	// Classifier judges prompts queued during a running turn (E4). Nil leaves
	// the queue unclassified: entries are marked ERROR with that stated
	// reason and still delivered by the ordinary turn-end drain, so the
	// feature degrades to plain FIFO rather than silently pretending to have
	// judged anything.
	Classifier Classifier
	// SessionConfigDir resolves a session's CLAUDE_CONFIG_DIR so the
	// classifier runs under the same account as the session it is about. Nil
	// leaves it empty, which inherits the daemon's own environment.
	SessionConfigDir func(sessionID string) string
	// now is the queue's clock, injected only by tests.
	now func() int64

	// Source yields each session's shim connection: shims dial the daemon's
	// listening socket and the listener routes each connection to the client
	// that owns that session. Required.
	Source shimclient.ConnSource

	// newClient is injected only by tests; production uses a real shimclient.
	newClient func(cfg shimclient.Config) sessionClient
}

// Manager is the fleet of per-session drivers. It implements the frontend
// PromptRouter (SubmitPrompt/Interrupt/AnswerPermission) plus Resync.
type Manager struct {
	cfg  Config
	logf func(string, ...any)
	reg  *permRegistry

	newClient func(cfg shimclient.Config) sessionClient
	// now is the queue's clock (queued_at_ms), injected by tests.
	now func() int64

	mu       sync.Mutex
	byWS     map[string]*driven // workspace -> live driver
	lastCSID map[string]string  // session id -> last-persisted claude session uuid
	closed   bool
	rootCtx  context.Context
	rootStop context.CancelFunc
	// exits counts every driver-exit goroutine (the tail of bringUp's `go
	// func`), so Close can JOIN them. Unjoined, that tail — which drains the
	// queue, publishes the empty view, and persists queued_prompts through the
	// registry — outlives Close and races whatever tears down after it; in the
	// e2e suite it recreated registry files inside a t.TempDir mid-RemoveAll,
	// which was the origin of the roving "directory not empty" teardown flake.
	exits sync.WaitGroup
}

// driven is one live session's driver state.
type driven struct {
	sessionID string
	workspace string
	client    sessionClient
	consumer  *consumer
	cancel    context.CancelFunc

	// metaprompt re-fire state, guarded by the manager mutex: armed when a
	// RESUME/COMPACT_CONTINUE SessionStarted arrived; fired once the directive
	// has been folded into a prompt.
	metaArmed bool
	metaFired bool
	metaCwd   string

	// Prompt-queue state (E4), guarded by the manager mutex.
	//
	// turnActive tracks the OBSERVED turn boundary (TurnStarted/TurnEnded off
	// the shim stream) rather than the SSM's derived turn_active: the queue
	// must act on what the session really reported, at the moment it reported
	// it, not on a resolved view of it.
	turnActive bool
	queue      promptQueue
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
	// runningText is the prompt that started the turn now in flight, as far as
	// this daemon saw it. It is the classifier's "what is already running"
	// context, and is empty when the turn predates this daemon.
	runningText string

	// repull is the below-floor history re-pull now running for this workspace,
	// or nil. Guarded by the manager mutex; it is what keeps two frontends
	// mounting at once from pulling the same history twice (repull.go).
	repull *repullState

	// rotEpoch counts the VENDOR SESSION ROTATIONS this driver has seen. It is
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

// pendingResync is a frontend resync waiting for the shim to reattach.
type pendingResync struct {
	// fromSeq is the CLIENT's original mark, not the floored replay position:
	// the re-armed attempt re-derives the floor against whatever the
	// conversation looks like after the reattach, which after a rotation is a
	// different seq space entirely.
	fromSeq uint64
}

// New builds a Manager. Required collaborators missing is a construction error
// (surfaced, never a nil-deref at dispatch).
func New(cfg Config) (*Manager, error) {
	switch {
	case cfg.Push == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a Pusher")
	case cfg.SSM == nil:
		return nil, fmt.Errorf("sessiondrv: New needs an SSM StateApplier")
	case cfg.Spawner == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a Spawner")
	case cfg.Locator == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a SessionLocator")
	case cfg.SeqStore == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a SeqStore")
	case cfg.ClearCompactStore == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a ClearCompactStore (without it an observed clear or compaction cannot floor a later resync)")
	case cfg.Source == nil:
		return nil, fmt.Errorf("sessiondrv: New needs a ConnSource (shims dial the daemon; without it no session can be driven)")
	}
	logf := cfg.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	newClient := cfg.newClient
	if newClient == nil {
		newClient = func(c shimclient.Config) sessionClient { return shimclient.New(c) }
	}
	rootCtx, rootStop := context.WithCancel(context.Background())
	now := cfg.now
	if now == nil {
		now = func() int64 { return time.Now().UnixMilli() }
	}
	return &Manager{
		cfg:       cfg,
		logf:      logf,
		reg:       newPermRegistry(logf),
		newClient: newClient,
		now:       now,
		byWS:      make(map[string]*driven),
		lastCSID:  make(map[string]string),
		rootCtx:   rootCtx,
		rootStop:  rootStop,
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

// snapshotDrivers copies the currently live driver set without holding the
// manager lock while callers inspect per-driver consumer state.
func (m *Manager) snapshotDrivers() []*driven {
	m.mu.Lock()
	drivers := make([]*driven, 0, len(m.byWS))
	for _, d := range m.byWS {
		drivers = append(drivers, d)
	}
	m.mu.Unlock()
	return drivers
}

// SessionInits returns a SessionInitView for every live session whose SystemInit
// has landed, sorted by workspace for a stable connect snapshot (task step 4:
// StateSnapshot.inits). A session with no init yet contributes nothing.
func (m *Manager) SessionInits() []*frontendv1.SessionInitView {
	var out []*frontendv1.SessionInitView
	for _, d := range m.snapshotDrivers() {
		if si := d.consumer.latestSystemInit(); si != nil {
			out = append(out, &frontendv1.SessionInitView{
				Workspace: d.workspace,
				SessionId: d.sessionID,
				Init:      si,
			})
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].GetWorkspace() < out[j].GetWorkspace() })
	return out
}

// taskCatalogForDriven rebuilds one live driver's complete detached-task roster
// from its retained event ring.
func taskCatalogForDriven(d *driven) *frontendv1.TaskCatalog {
	return frontend.BuildTaskCatalog(d.workspace, d.sessionID, d.consumer.snapshotRing())
}

// TaskCatalogs returns the complete detached-task roster for every live
// session, sorted by workspace for deterministic connect/resync snapshots. An
// idle session contributes an empty catalog so a reconnecting frontend is told
// authoritatively that its previous roster is clear.
func (m *Manager) TaskCatalogs() []*frontendv1.TaskCatalog {
	drivers := m.snapshotDrivers()
	catalogs := make([]*frontendv1.TaskCatalog, 0, len(drivers))
	taskCount := 0
	for _, d := range drivers {
		catalog := taskCatalogForDriven(d)
		catalogs = append(catalogs, catalog)
		taskCount += len(catalog.GetTasks())
	}
	sort.Slice(catalogs, func(i, j int) bool {
		return catalogs[i].GetWorkspace() < catalogs[j].GetWorkspace()
	})
	m.logf("sessiondrv: task catalog snapshot catalogs=%d tasks=%d", len(catalogs), taskCount)
	return catalogs
}

// TaskEntry returns the frontend TaskEntry (including its output_path) for a
// detached task on the workspace's live session, rebuilt from the retained
// event ring. ok=false when the workspace has no live driver or no such task.
// The caller enforces the path-confinement predicates before reading the file.
func (m *Manager) TaskEntry(workspace, taskID string) (*frontendv1.TaskEntry, bool) {
	d, err := m.existing(workspace)
	if err != nil {
		return nil, false
	}
	cat := taskCatalogForDriven(d)
	for _, e := range cat.GetTasks() {
		if e.GetTaskId() == taskID {
			return e, true
		}
	}
	return nil, false
}

// persistVendorSessionID writes a session's CLI uuid through to the registry
// via the injected Registrar, deduped per session so a repeated SessionStarted
// (a reattach replay) does not re-write the same value. No-op when no registrar
// is wired or the uuid is empty.
func (m *Manager) persistVendorSessionID(sessionID, csid string) {
	if m.cfg.Registrar == nil || csid == "" {
		return
	}
	m.mu.Lock()
	if m.lastCSID[sessionID] == csid {
		m.mu.Unlock()
		return
	}
	m.lastCSID[sessionID] = csid
	m.mu.Unlock()
	m.logf("sessiondrv: persisting claude_session_id session=%s uuid=%s", sessionID, csid)
	m.cfg.Registrar.ClaudeSessionIDChanged(sessionID, csid)
}

// SubmitPrompt brings the workspace's session up (lazily, reattach-first) and
// submits text to its shim. On the FIRST prompt after a RESUME/COMPACT_CONTINUE
// session start, the metaprompt read-directive is prepended once (task step 4).
// A prompt submitted while the session's turn is ALREADY RUNNING is not
// forwarded at all: the daemon queues it (E4) and this returns nil, because
// the command was accepted — it was accepted into the queue. The queue's own
// pushed QueueView is what tells the frontend where the prompt went.
// requestID is the frontend command's own id. It is what the daemon's prompt
// RECEIPT is keyed on (promptecho.go) and what the durable transcript line is
// later stamped with, so the two reconcile onto one bubble. An empty one (a
// caller with no frontend request behind it) simply pushes no receipt.
func (m *Manager) SubmitPrompt(ctx context.Context, workspace, requestID, text, permissionMode string) error {
	return m.submitPrompt(ctx, workspace, requestID, text, permissionMode, "frontend")
}

// SubmitWorkspaceInitialPrompt submits a durable workspace-create job's initial
// prompt.  JobID is carried as the vendor-visible origin for traceability, not
// as an exact-once claim: after a shim accepts the prompt but before the job
// store checkpoints it, a daemon crash may submit it again.  The creation
// manager therefore deliberately provides at-least-once delivery and never
// marks delivery before this call returns successfully.
func (m *Manager) SubmitWorkspaceInitialPrompt(ctx context.Context, workspace, jobID, text, permissionMode string) error {
	if jobID == "" {
		return fmt.Errorf("sessiondrv: workspace initial prompt needs a job id")
	}
	return m.submitPrompt(ctx, workspace, "workspace-create:"+jobID, text, permissionMode, "workspace-create:"+jobID)
}

func (m *Manager) submitPrompt(ctx context.Context, workspace, requestID, text, permissionMode, origin string) error {
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return err
	}

	m.mu.Lock()
	entry, queued := m.queueSubmitLocked(d, requestID, text, permissionMode)
	if !queued {
		m.mu.Unlock()
		// THE RECEIPT, pushed before the forward rather than after it: the send
		// is what the bubble reports, and the shim's Ack can be hundreds of
		// milliseconds away. A submit that then FAILS keeps its bubble — the
		// user did send that text, the failure surfaces as its own card, and
		// silently retracting what they typed would be the worse report.
		//
		// A QUEUED prompt deliberately reaches none of this: it renders as a
		// queue chip until it is DELIVERED, and a bubble drawn now would claim
		// an execution order the session is not going to follow. Its receipt is
		// pushed at the delivery site instead (queue.go, deliver).
		m.echo(d, requestID, text)
		text = m.applyMetaprompt(d, text)
		if err := d.client.SubmitPrompt(ctx, text, origin, permissionMode); err != nil {
			// The prompt never reached the shim, so no turn is beginning — and
			// with a paused queue that matters: the lone-runner flag set on the
			// way in would otherwise leave the pause waiting for a turn end
			// that can never arrive.
			m.mu.Lock()
			d.pausedRunner = false
			m.mu.Unlock()
			return err
		}
		// The submit was ACCEPTED, so a turn is beginning. This is the earliest
		// turn-start signal the daemon actually observes today (live TurnStarted
		// events do not currently reach it), and it is what starts the footer's
		// turn clock and resets its turn-scoped token figure. The resolver's open
		// is idempotent, so the real TurnStarted arriving later changes nothing.
		// A QUEUED prompt deliberately does not reach here: the turn it would
		// report is the one already running.
		m.progress().NoteTurnAccepted(workspace, d.sessionID)
		m.noteClearDispatched(workspace, text)
		return nil
	}
	running := d.runningText
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	m.logf("sessiondrv: queued prompt entry=%s session=%s ws=%q origin=%q (turn in flight)",
		entry.id, d.sessionID, workspace, origin)
	m.publish(d.sessionID, view, recs)
	go m.classify(d, entry.id, running, text)
	return nil
}

// clearSessionCommand is the session command that discards a conversation's
// context. Matched on the SUBMITTED text, which is where the daemon sees it:
// the harness recognizes `/clear` itself and expands it into a command
// envelope inside the CLI, so by the time anything is on the file plane the
// clear has already happened.
const clearSessionCommand = "/clear"

// noteClearDispatched opens the SSM's clearing axis for a `/clear` the daemon
// just handed to a shim.
//
// THIS IS THE ONLY PLACE THAT KNOWS. Nothing in the event stream announces a
// clear as it BEGINS — the first-class ContextCleared reports one that already
// finished — so a footer that waited for an event would say `thinking` through
// the entire cut and then jump straight to the cleared bubble.
//
// An argument after the command means the user asked for something else, so
// the text must be exactly the command. A failure is loud-logged and does not
// fail the submit: the prompt was accepted, and losing it over a footer word
// would be the larger harm.
func (m *Manager) noteClearDispatched(workspace, text string) {
	if strings.TrimSpace(text) != clearSessionCommand {
		return
	}
	m.logf("sessiondrv: /clear dispatched ws=%q — opening the SSM's clearing axis until its ContextCleared lands", workspace)
	if err := m.cfg.SSM.ApplyClearing(workspace, true, "clear_dispatched"); err != nil {
		m.logf("sessiondrv: opening the clearing axis FAILED ws=%q: %v (the cut will render as an ordinary turn)", workspace, err)
	}
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
	m.logf("sessiondrv: session %s ended — marking the record terminal (reason=%s)", sessionID, reason)
	m.cfg.Registrar.SessionDied(sessionID, reason)
}

// progress returns the configured progress resolver, or the no-op stand-in when
// the driver was built without one.
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

// applyMetaprompt folds the metaprompt directive into text once for an armed
// session, loud-logging the fold.
//
// TAKES m.mu itself, which is why it carries no `Locked` suffix (see the
// package doc): callers must NOT hold the mutex.
func (m *Manager) applyMetaprompt(d *driven, text string) string {
	m.mu.Lock()
	defer m.mu.Unlock()
	if !d.metaArmed || d.metaFired {
		return text
	}
	directive, ok := metapromptDirective(d.metaCwd)
	if !ok {
		// Armed but the file is absent under this cwd: nothing to prepend. Mark
		// fired so we do not re-stat on every prompt, and log the honest miss.
		d.metaFired = true
		m.logf("sessiondrv: metaprompt re-fire armed for session=%s but %s/%s absent; skipping",
			d.sessionID, d.metaCwd, metapromptRelPath)
		return text
	}
	d.metaFired = true
	m.logf("sessiondrv: metaprompt re-fire folding read-directive into next prompt session=%s ws=%s", d.sessionID, d.workspace)
	return prependMetaprompt(directive, text)
}

// Interrupt interrupts the workspace's live turn. A workspace with no live
// session is a loud error (the frontend renders the failed CommandAck).
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
func (m *Manager) Interrupt(ctx context.Context, workspace string) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	outcome, err := d.client.Interrupt(ctx)
	if err != nil {
		return err
	}
	m.noteUserInterrupt(d, outcome)
	if failed := errclass.InterruptError(outcome); failed != nil {
		m.logf("sessiondrv: interrupt undeliverable ws=%s session=%s outcome=%s", workspace, d.sessionID, outcome)
		return failed
	}
	m.logf("sessiondrv: interrupt ws=%s session=%s outcome=%s", workspace, d.sessionID, outcome)
	return nil
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
func (m *Manager) noteUserInterrupt(d *driven, outcome corev1.InterruptOutcome) {
	m.progress().NoteInterrupt(d.workspace, d.sessionID, outcome)

	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		if err := m.cfg.SSM.MarkTurnInterrupted(d.workspace); err != nil {
			// Loud, never swallowed: the turn's end will report `done` instead
			// of `interrupted`, and this line is the only account of why.
			m.logf("sessiondrv: marking the interrupted turn on the SSM FAILED ws=%s session=%s: %v (the stopped turn will report `done`)",
				d.workspace, d.sessionID, err)
		}
	}
	if errclass.InterruptError(outcome) != nil {
		return
	}

	m.mu.Lock()
	d.paused = true
	if outcome == corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		d.interruptedTurn = true
	}
	held := len(d.queue.entries)
	m.mu.Unlock()
	m.logf("sessiondrv: queue PAUSED by a user interrupt ws=%s session=%s outcome=%s held=%d (every entry retained; a newly submitted prompt runs alone and its clean end resumes the drain)",
		d.workspace, d.sessionID, outcome, held)
}

// TurnActive reports whether the workspace's session has a turn IN FLIGHT, as
// the driver observed it off the shim's own TurnStarted/TurnEnded stream.
//
// It is the same fact the queue acts on (driven.turnActive), deliberately
// rather than the SSM's resolved turn_active: the interrupt confirm gate is
// deciding whether there is a turn to stop RIGHT NOW, which is a question
// about what the session reported, not about how a workspace resolves.
//
// A workspace with no live driver is a loud error, the same one Interrupt
// itself would return a moment later.
func (m *Manager) TurnActive(workspace string) (bool, error) {
	d, err := m.existing(workspace)
	if err != nil {
		return false, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return d.turnActive, nil
}

// Health proves one named live session is connected to this daemon and that
// its shim has passed its own dependency health check.  It deliberately uses
// existing rather than ensure: a restore must not render because a probe
// happened to create a new shim; it may render only after the live driver has
// handshaked and answered the correlated check.
//
// IT WAITS ON A BRING-UP ALREADY IN MOTION, under the PROBE'S OWN context.
// AwaitReady resolves only on ShimReady, i.e. once the shim has completed ALL
// of its wiring (lock, query, producer, standing store subscription), so
// waiting on it is what makes a probe issued the instant after a spawn answer
// the question it was asking instead of failing against a connection that was
// milliseconds away. This is also the gate createSession's ack rides on.
//
// The wait creates NOTHING: a workspace with no driver still fails loudly and
// immediately (existing, above), and the deadline stays the caller's — a probe
// whose context ends before readiness surfaces that deadline as its own loud
// error rather than hanging.
//
// Each failure carries the LINK it stopped at as a sentinel (ErrNoLiveDriver,
// ErrShimNotReady, or the shimclient sentinels the round-trip returns), which
// is what lets a create nack name the deepest hop rather than the whole path.
func (m *Manager) Health(ctx context.Context, workspace, sessionID, requestID string) (*corev1.HealthStatus, error) {
	if workspace == "" {
		return nil, fmt.Errorf("sessiondrv: health requires a workspace")
	}
	if sessionID == "" {
		return nil, fmt.Errorf("sessiondrv: health ws=%q requires a session_id", workspace)
	}
	if requestID == "" {
		return nil, fmt.Errorf("sessiondrv: health ws=%q session=%s requires a request_id", workspace, sessionID)
	}
	d, err := m.existing(workspace)
	if err != nil {
		return nil, err
	}
	if d.sessionID != sessionID {
		return nil, fmt.Errorf("sessiondrv: health ws=%q names session=%s but live driver owns session=%s", workspace, sessionID, d.sessionID)
	}
	m.logf("sessiondrv: health probe ws=%q session=%s request_id=%s", workspace, sessionID, requestID)
	if err := d.client.AwaitReady(ctx); err != nil {
		// BOTH causes stay in the chain: the link (which hop is pending) and the
		// transport error (why the wait ended). Dropping either would cost a
		// caller one of the two questions it has to answer.
		return nil, fmt.Errorf("sessiondrv: health ws=%q session=%s request_id=%s: %w within the probe's deadline: %w",
			workspace, sessionID, requestID, ErrShimNotReady, err)
	}
	status, err := d.client.Health(ctx, requestID)
	if err != nil {
		return nil, fmt.Errorf("sessiondrv: health ws=%q session=%s request_id=%s: %w", workspace, sessionID, requestID, err)
	}
	if status.GetRequestId() != requestID {
		return nil, fmt.Errorf("sessiondrv: health ws=%q session=%s request_id mismatch got=%q want=%q", workspace, sessionID, status.GetRequestId(), requestID)
	}
	return status, nil
}

// AnswerPermission delivers a frontend permission answer to the parked
// canUseTool round-trip (keyed by permissionRequestID). A stale/duplicate
// answer is a loud error, never swallowed.
func (m *Manager) AnswerPermission(_ context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error {
	m.logf("sessiondrv: permission answer ws=%s request_id=%s allow=%v", workspace, permissionRequestID, allow)
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
// A workspace with no live session is a loud error.
func (m *Manager) Resync(workspace string, fromSeq uint64) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	replayFrom := m.replayFloor(d, fromSeq)
	ringFloor, haveRingFloor := d.consumer.resync(replayFrom)
	if !haveRingFloor {
		ringFloor = m.cfg.SeqStore.LastSeq(d.sessionID) + 1
	}
	if replayFrom >= ringFloor {
		m.noteResyncSettled(d)
		return nil // the ring covered the whole request
	}
	m.logf("sessiondrv: resync ws=%q replay_from=%d is below the retained floor %d; re-pulling the gap from the store",
		workspace, replayFrom, ringFloor)
	err = m.startRepull(d, exclusiveLowerBound(replayFrom), ringFloor)
	if errors.Is(err, shimclient.ErrReplayLinkLost) {
		return m.rearmResyncAfterReattach(d, fromSeq, err)
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
func (m *Manager) rearmResyncAfterReattach(d *driven, fromSeq uint64, cause error) error {
	m.mu.Lock()
	if closed, retried := m.closed, d.resyncRetried; closed || retried {
		m.mu.Unlock()
		m.logf("sessiondrv: resync ws=%q session=%s from_seq=%d lost the shim link with no re-arm left (already retried=%v, manager closed=%v) — surfacing it rather than retrying further",
			d.workspace, d.sessionID, fromSeq, retried, closed)
		return cause
	}
	d.resyncRetried = true
	d.pendingResync = &pendingResync{fromSeq: fromSeq}
	m.mu.Unlock()
	m.logf("sessiondrv: resync ws=%q session=%s from_seq=%d was INTERRUPTED by a shim-link bounce and is RE-ARMED — it will be served again as soon as the shim reattaches; this is not a truncation and no failure card is pushed: %v",
		d.workspace, d.sessionID, fromSeq, cause)
	return nil
}

// noteResyncSettled refreshes the one-shot re-arm budget after a resync that
// actually completed.
func (m *Manager) noteResyncSettled(d *driven) {
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
		m.logf("sessiondrv: re-serving the resync a shim-link bounce interrupted ws=%q session=%s from_seq=%d (the shim has reattached)",
			workspace, sessionID, pending.fromSeq)
		if err := m.Resync(workspace, pending.fromSeq); err != nil {
			m.logf("sessiondrv: the re-armed resync ws=%q session=%s from_seq=%d FAILED: %v",
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
func (m *Manager) replayFloor(d *driven, fromSeq uint64) uint64 {
	floorSeq := m.cfg.ClearCompactStore.NewestClearOrCompactSeq(d.sessionID)
	lastSeen := m.lastSeenSeq(d)
	logf := dlog.Tag(dlog.Logf(m.logf),
		"ws", d.workspace, "session", d.sessionID,
		"from_seq", fromSeq, "newest_clear_or_compact_seq", floorSeq,
		"last_seen_seq", lastSeen)
	if fromSeq > lastSeen {
		logf("sessiondrv: replay mark from a RETIRED seq space — from_seq is above every seq this conversation has produced, so it was counted under a vendor session uuid that has since rotated; replay_from=%d (the mark is NOT trusted as 'already past everything')", floorSeq)
		return floorSeq
	}
	if floorSeq <= fromSeq {
		logf("sessiondrv: replay floor left at the client mark replay_from=%d (the client is already at or past the newest clear or compaction)", fromSeq)
		return fromSeq
	}
	logf("sessiondrv: replay floor RAISED to the newest clear or compaction replay_from=%d (inclusive: that event is itself replayed; the history above it is never sent)", floorSeq)
	return floorSeq
}

// lastSeenSeq is the highest store seq this conversation has produced in its
// CURRENT vendor seq space: the durable last_seen_seq, or the retained ring's
// newest position when that is higher.
//
// Both are read because they are true at different moments. The durable mark is
// written by shimclient before an event is forwarded anywhere, so it covers
// everything a frontend can possibly have seen — including after a restart, when
// the ring is empty. The ring covers a driver whose events did not come through
// that path at all (the resync tests' direct Consume, a re-pull), where the
// durable mark can legitimately still read zero.
//
// The value is a CEILING on any honest client mark, which is what makes a mark
// above it evidence of a retired seq space rather than of a fast frontend.
func (m *Manager) lastSeenSeq(d *driven) uint64 {
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
// workspace with no live driver has none.
func (m *Manager) PendingPermissions(workspace string) []string {
	return m.reg.idsForWorkspace(workspace)
}

// ErrNotLiveSession reports that the workspace IS driven, but by a DIFFERENT
// session than the one the caller asked to stand down. Distinct from the "no
// live session" error so a caller can tell "nothing to stop" (benign) from
// "that shim belongs to someone else — do not touch it". Its value lives in
// internal/errclass beside its classification; this is the historic name.
var ErrNotLiveSession = errclass.ErrNotLiveSession

// The two establishment-link anchors this package is the deepest holder of:
// there is no driver at all for the workspace, and there is one whose
// connection never completed the handshake. Together with the shimclient
// sentinels and the shim's own health verdict they let a createSession nack
// name the deepest link that failed instead of reporting a bare bring-up
// error. Their values live in internal/errclass beside their classifications.
var (
	ErrNoLiveDriver = errclass.ErrNoLiveDriver
	ErrShimNotReady = errclass.ErrShimNotReady
)

// Hibernate suspends the workspace's live session, WHICHEVER session that is:
// it stops consuming the stream and SIGTERMs the child shim (the redefined
// hibernation, §4.4). The registry record stays non-terminal (the caller owns
// that), so the next act revives it via a fresh reattach-first Ensure. A
// workspace with no live driver is a loud error (nothing to hibernate). NEVER
// call this while a turn is active — the SSM is the caller's guard.
//
// Use this ONLY when the intent really is workspace-scoped (the idle sweep,
// daemon shutdown). A caller standing down one SPECIFIC record must use
// HibernateSession — see the warning there.
func (m *Manager) Hibernate(workspace string) error {
	return m.hibernate(workspace, "")
}

// HibernateSession suspends sessionID's shim without disturbing any different
// session currently driving workspace. If the matching driver is already
// evicted after a terminal client error, or a replacement now owns workspace,
// the session-scoped stop still reaches the spawner directly: process handles
// are keyed by session id and must not become unreachable through byWS churn.
//
// Several registry records can share one cwd — a stale duplicate, a superseded
// resume, an orphan awaiting reap — so "stop THIS record's shim" is not the same
// question as "stop the workspace's shim". Answering it with the workspace-keyed
// Hibernate SIGTERMs whichever shim happens to be live, which on 2026-07-25
// meant reaping an orphan killed the healthy session created 175ms earlier for
// the same workspace, leaving the user with nothing to drive.
func (m *Manager) HibernateSession(workspace, sessionID string) error {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if ok && d.sessionID != sessionID {
		live := d.sessionID
		m.mu.Unlock()
		m.logf("sessiondrv: session-scoped hibernate ws=%q requested=%s live=%s; preserving live driver and stopping requested shim only",
			workspace, sessionID, live)
		return m.cfg.Spawner.StopShim(sessionID)
	}
	if ok {
		delete(m.byWS, workspace)
	}
	m.mu.Unlock()
	if ok {
		d.cancel()
	}
	m.logf("sessiondrv: hibernating session-scoped ws=%q session=%s driver_present=%v (SIGTERM child shim)",
		workspace, sessionID, ok)
	return m.cfg.Spawner.StopShim(sessionID)
}

// hibernate is the shared teardown. An empty wantSession means "whichever
// session is live"; a non-empty one gates the teardown on identity.
func (m *Manager) hibernate(workspace, wantSession string) error {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	if !ok {
		m.mu.Unlock()
		return fmt.Errorf("sessiondrv: no live session for workspace %q to hibernate", workspace)
	}
	if wantSession != "" && d.sessionID != wantSession {
		live := d.sessionID
		m.mu.Unlock()
		return fmt.Errorf("%w: workspace %q is driven by session %s, not %s",
			ErrNotLiveSession, workspace, live, wantSession)
	}
	delete(m.byWS, workspace)
	m.mu.Unlock()
	d.cancel() // stop consuming; the shimclient Run ends
	m.logf("sessiondrv: hibernating ws=%q session=%s (SIGTERM child shim)", workspace, d.sessionID)
	return m.cfg.Spawner.StopShim(d.sessionID)
}

// bringUpTimeout bounds how long ensure waits for a spawned shim to connect
// and handshake. It is a FAILURE bound, not a tuned delay: ensure returns the
// instant the connection is ready, and this only decides how long we wait
// before declaring the shim genuinely dead. Generous enough that a loaded
// machine never trips it spuriously.
const bringUpTimeout = 30 * time.Second

// ensure returns a driver that is READY TO DRIVE: the shim is running and its
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
func (m *Manager) ensure(ctx context.Context, workspace string) (*driven, error) {
	d, err := m.bringUp(workspace)
	if err != nil {
		return nil, err
	}
	waitCtx, cancel := context.WithTimeout(ctx, bringUpTimeout)
	defer cancel()
	if err := d.client.AwaitReady(waitCtx); err != nil {
		return nil, fmt.Errorf("sessiondrv: session %s for workspace %q never became driveable: %w", d.sessionID, workspace, err)
	}
	return d, nil
}

// existing returns the live driver for workspace, or a loud error when there is
// none (no lazy bring-up: interrupt/resync/answer for an unbrought-up workspace
// is a caller error, distinct from a first prompt which brings it up).
func (m *Manager) existing(workspace string) (*driven, error) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if d, ok := m.byWS[workspace]; ok {
		return d, nil
	}
	return nil, fmt.Errorf("sessiondrv: no live session for workspace %q: %w", workspace, ErrNoLiveDriver)
}

// ensure returns the live driver for workspace, bringing it up (reattach-first
// spawn + shimclient) on first use.
//
// bringUp STARTS the session: it spawns the shim if needed and launches the
// client's connect loop in a goroutine. It returns as soon as that is under
// way, so the returned driver is NOT yet driveable — `d.client` has no
// connection for the few hundred milliseconds the shim takes to boot, listen,
// and handshake. Anything about to SEND must use ensure instead.
func (m *Manager) bringUp(workspace string) (*driven, error) {
	m.mu.Lock()
	if m.closed {
		m.mu.Unlock()
		return nil, fmt.Errorf("sessiondrv: manager closed")
	}
	if d, ok := m.byWS[workspace]; ok {
		m.mu.Unlock()
		return d, nil
	}
	m.mu.Unlock()

	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return nil, fmt.Errorf("sessiondrv: workspace %q has no live session to drive", workspace)
	}
	if err := m.cfg.Spawner.EnsureShim(m.rootCtx, sessionID); err != nil {
		return nil, fmt.Errorf("sessiondrv: ensure shim for session %s (ws %q): %w", sessionID, workspace, err)
	}

	d := &driven{sessionID: sessionID, workspace: workspace}
	cons := newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, m.cfg.Progress, m.cfg.ClearCompactStore, m.logf, func(ss *corev1.SessionStarted) {
		m.armMetaprompt(d, ss)
		m.persistVendorSessionID(sessionID, ss.GetVendorSessionId())
	}, func(active bool) {
		m.onTurnBoundary(d, active)
	}, func(state string) {
		m.persistBackfillState(sessionID, state)
		// The SSM composes green from this: a failed backfill is blue, and
		// a settled one releases the axis so the workspace can be ready.
		if err := m.cfg.SSM.ApplyBackfillState(workspace, state); err != nil {
			m.logf("sessiondrv: applying backfill %s to the SSM (ws %q): %v", state, workspace, err)
		}
	}, func() {
		m.persistSessionDeath(sessionID, errclass.DeathReasonShimDied)
	})
	// Every PERSISTENT store event names the conversation it belongs to.
	// Keeping the record current off the live stream is what gives a later
	// handshake's announcement something to DIFFER from — a rotation is
	// invisible against an empty record. Bound before Run, so no event can
	// reach the consumer with this unset.
	cons.onVendorSessionID = func(vendorSessionID string) {
		m.persistVendorSessionID(sessionID, vendorSessionID)
	}
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
	// edges of a permission's life. The queue depth is read back off the live
	// queue so the two counters are always reported together and neither can go
	// stale behind the other.
	ph := permHandler{reg: m.reg, cons: cons, logf: m.logf, onPermsChanged: func() {
		m.mu.Lock()
		depth := int64(len(d.queue.entries))
		m.mu.Unlock()
		m.noteProgressCounts(workspace, depth)
	}}

	runCtx, cancel := context.WithCancel(m.rootCtx)
	d.cancel = cancel
	client := m.newClient(shimclient.Config{
		SessionID:       sessionID,
		Source:          m.cfg.Source,
		DaemonVersion:   m.cfg.DaemonVersion,
		ProtocolVersion: m.cfg.ProtocolVersion,
		SeqStore:        m.cfg.SeqStore,
		StateSink:       cons,
		FrameSink:       cons,
		Degraded:        cons,
		Permissions:     ph,
		OnHandshake:     func(hello *corev1.ShimHello) { m.onHandshake(workspace, sessionID, hello) },
		OnConnected:     func(hello *corev1.ShimHello) { m.onConnected(workspace, sessionID, hello) },
		Logf:            m.logf,
	})
	d.client = client

	// Race: two concurrent first-prompts for the same workspace. Re-check under
	// the lock; if another goroutine won, tear ours down and use theirs.
	m.mu.Lock()
	if existing, ok := m.byWS[workspace]; ok {
		m.mu.Unlock()
		cancel()
		return existing, nil
	}
	m.byWS[workspace] = d
	m.mu.Unlock()

	m.exits.Add(1)
	go func() {
		defer m.exits.Done()
		runErr := client.Run(runCtx)
		if runErr != nil {
			m.logf("sessiondrv: session %s driver ended: %v", sessionID, runErr)
		}
		m.mu.Lock()
		wasCurrent := false
		if cur, ok := m.byWS[workspace]; ok && cur == d {
			delete(m.byWS, workspace)
			wasCurrent = true
		}
		// The session is gone, so its held prompts can never be delivered.
		// Empty the queue and PUSH the empty view: a frontend that keeps
		// rendering chips for a dead session is offering the user controls
		// that do nothing.
		dropped := d.queue.drainAll()
		view := d.queue.view(workspace, sessionID)
		m.mu.Unlock()
		if len(dropped) > 0 {
			m.logf("sessiondrv: session %s ended with %d queued prompt(s) undelivered ws=%q",
				sessionID, len(dropped), workspace)
		}
		m.publish(sessionID, view, nil)
		// A terminal protocol error ends Run while this driver is still current,
		// without going through Hibernate. That used to orphan the spawned shim
		// and its stop handle after the byWS eviction above. A non-current Run
		// exit was initiated by a teardown that already owns StopShim.
		if wasCurrent {
			if stopErr := m.cfg.Spawner.StopShim(sessionID); stopErr != nil {
				m.logf("sessiondrv: session %s unexpected driver-exit shim stop FAILED ws=%q run_err=%v: %v",
					sessionID, workspace, runErr, stopErr)
			} else {
				m.logf("sessiondrv: session %s unexpected driver-exit shim stop complete ws=%q run_err=%v",
					sessionID, workspace, runErr)
			}
		}
	}()
	m.logf("sessiondrv: brought up session=%s ws=%q (reattach-first)", sessionID, workspace)
	return d, nil
}

// armMetaprompt arms the re-fire for a RESUME/COMPACT_CONTINUE session start.
func (m *Manager) armMetaprompt(d *driven, ss *corev1.SessionStarted) {
	if !wantsMetapromptRefire(ss) {
		return
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if d.metaArmed {
		return
	}
	d.metaArmed = true
	d.metaCwd = ss.GetCwd()
	m.logf("sessiondrv: metaprompt re-fire armed session=%s source=%s cwd=%s", d.sessionID, ss.GetSource(), ss.GetCwd())
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
//     Subscribe reads it.
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
//   - driven.repull{fromSeq,stopAt} — an in-flight re-pull's bounds. Stamped
//     with driven.rotEpoch, which this bumps, so a post-rotation request can
//     never be coalesced onto a re-pull bounded in the retired space.
//   - driven.resyncRetried — the one-shot re-arm budget, refreshed here so a
//     re-pull that a SECOND bounce interrupts is still retried once.
//
// DELIBERATELY NOT RESET, and why stale values are harmless:
//
//   - consumer.permItems / failItems — daemon-composed items keyed by
//     request_id and card uuid; they carry NO store seq (through_seq stays 0)
//     and are replayed on every resync regardless of fromSeq, so no ceiling can
//     hide them. A pending permission also survives the shim bounce and is
//     re-asked on reattach, so dropping these would erase a live prompt.
//   - ssm workspace_state.cause_seq (the paint watermark on the `painted` row,
//     and the (session_id, cause_seq) idempotency key) — the KNOWN residual.
//     The idempotency key is keyed by the event's session id, which IS the
//     vendor uuid, so the new space gets a fresh key space for free. The paint
//     watermark is per WORKSPACE and monotonic, so a retired-space attestation
//     survives the rotation and swallows the new space's lower acks as
//     superseded until it climbs past the old high water — the workspace reads
//     painted on an attestation about a conversation that is gone. That is a
//     KNOWN residual, deliberately left by the 045a79d8 report: it errs toward
//     green rather than toward the permanent blue a mis-reset would produce,
//     and it is listed here so the next reader finds it named rather than
//     missing.
//   - driven.metaCwd / backfill / systemInit / queue entries — carry no seq at
//     all; a rotation does not change what they describe.
func (m *Manager) onHandshake(workspace, sessionID string, hello *corev1.ShimHello) {
	csid := hello.GetVendorSessionId()
	if csid == "" {
		// A fresh session whose shim has not learned its uuid yet. Announcing
		// nothing is the honest shape; the SessionStarted that follows carries
		// it (persistVendorSessionID).
		return
	}
	if m.cfg.Registrar == nil {
		m.logf("sessiondrv: shim announced vendor_session_id=%s ws=%q session=%s with NO registrar bound — a rotation cannot reset the store cursor and would be read as a seq regression",
			csid, workspace, sessionID)
		return
	}
	rotated, previous := m.cfg.Registrar.AdoptVendorSessionID(sessionID, csid)
	m.mu.Lock()
	m.lastCSID[sessionID] = csid
	m.mu.Unlock()
	if !rotated {
		return
	}
	m.logf("sessiondrv: VENDOR SESSION ROTATION ws=%q session=%s %s -> %s — the vendor retired one transcript identity mid-stream; store cursor and replay floor reset to zero and the subscription resumes from the new seq space's beginning",
		workspace, sessionID, previous, csid)
	m.purgeRetainedOnRotation(workspace, sessionID, previous, csid)
	m.clearTurnOnRotation(workspace, sessionID, previous, csid)
	if err := m.cfg.SSM.ApplySessionRotated(workspace, previous, csid); err != nil {
		m.logf("sessiondrv: reconciling the SSM across the rotation FAILED ws=%q session=%s %s -> %s: %v (the workspace may stay in THINKING until the next turn)",
			workspace, sessionID, previous, csid, err)
	}
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
	logf("sessiondrv: retained conversation ring PURGED across the vendor session rotation — every item and every seq ceiling it carried counted in the retired space; the ring is empty until the new space's events arrive")
	if inflight != nil {
		logf("sessiondrv: a history re-pull is IN FLIGHT across the rotation (from_seq=%d stop_at=%d, both retired-space numbers) — its bounds cannot cover the new space, so no later request will be coalesced onto it",
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
		m.logf("sessiondrv: interrupt mark DROPPED as stale ws=%q session=%s (vendor session rotated %s -> %s) — the stopped turn's end belongs to the retired identity",
			workspace, sessionID, previous, next)
	}
	if !d.turnActive {
		return
	}
	d.turnActive = false
	m.logf("sessiondrv: turn-in-flight observation CLEARED ws=%q session=%s (vendor session rotated %s -> %s) — the running turn's end will be reported under the new identity",
		workspace, sessionID, previous, next)
}

// onConnected reconciles SSM turn state on a mid-turn reattach (task step 1):
// when the shim reports a turn in flight, the SSM must not read idle. The store
// replays events from last_seen_seq on Subscribe, so the SSM re-derives turn
// state from the replayed TurnStarted; this hook loud-logs the observation so a
// reconciliation gap is visible rather than silent.
func (m *Manager) onConnected(workspace, sessionID string, hello *corev1.ShimHello) {
	if hello.GetTurnInFlight() {
		m.logf("sessiondrv: reattached mid-turn ws=%s session=%s (turn_in_flight); SSM re-derives from replayed events", workspace, sessionID)
	}
	// A resync whose store re-pull this very reattach interrupted is served
	// again here — the link being back IS the event it was waiting for, which is
	// why nothing sleeps or polls for it.
	m.runPendingResync(workspace, sessionID)
}

// Close stops every driver, abandons pending permissions (no fabricated
// answers), and JOINS every driver-exit goroutine before returning, so no
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
	h.logf("sessiondrv: permission prompt ws=%s session=%s request_id=%s tool=%s (awaiting frontend answer)",
		h.cons.workspace, sessionID, req.GetRequestId(), req.GetToolName())
	// Push the pending permission ConversationItem (uuid = request_id) through
	// the retained-ring pusher so a resync replays it (S8). It supersedes the
	// earlier WorkspaceState-only decision but does NOT replace the PERMISSION
	// render-state, which stays alongside.
	h.cons.pushPermission(permissionItem(req, corev1.PermissionItem_RESOLUTION_PENDING, ""))
	// Surface a permission render-state so the frontend shows the prompt while
	// the shim's canUseTool blocks. Eventually-consistent: the SSM re-pushes
	// the resolved state as events flow, and a frontend resync corrects any lag.
	h.cons.push.PushWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: h.cons.workspace,
		SessionId: h.cons.sessionID,
		State:     frontendv1.RenderState_RENDER_STATE_PERMISSION,
	})
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
