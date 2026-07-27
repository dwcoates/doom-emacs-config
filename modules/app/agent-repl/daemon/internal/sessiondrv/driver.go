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
	"fmt"
	"sort"
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

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
	SubmitPrompt(ctx context.Context, text, origin, permissionMode string) error
	// Interrupt returns the shim's own verdict on what the stop did, which is
	// the only place that verdict is observable.
	Interrupt(ctx context.Context, hard bool) (corev1.InterruptOutcome, error)
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
	// runningText is the prompt that started the turn now in flight, as far as
	// this daemon saw it. It is the classifier's "what is already running"
	// context, and is empty when the turn predates this daemon.
	runningText string

	// repull is the below-floor history re-pull now running for this workspace,
	// or nil. Guarded by the manager mutex; it is what keeps two frontends
	// mounting at once from pulling the same history twice (repull.go).
	repull *repullState
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

// SessionInits returns a SessionInitView for every live session whose SystemInit
// has landed, sorted by workspace for a stable connect snapshot (task step 4:
// StateSnapshot.inits). A session with no init yet contributes nothing.
func (m *Manager) SessionInits() []*frontendv1.SessionInitView {
	m.mu.Lock()
	drivers := make([]*driven, 0, len(m.byWS))
	for _, d := range m.byWS {
		drivers = append(drivers, d)
	}
	m.mu.Unlock()
	var out []*frontendv1.SessionInitView
	for _, d := range drivers {
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

// TaskEntry returns the frontend TaskEntry (including its output_path) for a
// detached task on the workspace's live session, rebuilt from the retained
// event ring. ok=false when the workspace has no live driver or no such task.
// The caller enforces the path-confinement predicates before reading the file.
func (m *Manager) TaskEntry(workspace, taskID string) (*frontendv1.TaskEntry, bool) {
	d, err := m.existing(workspace)
	if err != nil {
		return nil, false
	}
	cat := frontend.BuildTaskCatalog(workspace, d.sessionID, d.consumer.snapshotRing())
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
func (m *Manager) SubmitPrompt(ctx context.Context, workspace, text, permissionMode string) error {
	d, err := m.ensure(ctx, workspace)
	if err != nil {
		return err
	}

	m.mu.Lock()
	entry, queued := m.queueSubmitLocked(d, text, permissionMode)
	if !queued {
		m.mu.Unlock()
		text = m.applyMetaprompt(d, text)
		if err := d.client.SubmitPrompt(ctx, text, "frontend", permissionMode); err != nil {
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
		return nil
	}
	running := d.runningText
	view, recs := m.publishQueueLocked(d)
	m.mu.Unlock()

	m.logf("sessiondrv: queued prompt entry=%s session=%s ws=%q (turn in flight)",
		entry.id, d.sessionID, workspace)
	m.publish(d.sessionID, view, recs)
	go m.classify(d, entry.id, running, text)
	return nil
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
func (m *Manager) Interrupt(ctx context.Context, workspace string, hard bool) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	outcome, err := d.client.Interrupt(ctx, hard)
	if err != nil {
		return err
	}
	if failed := errclass.InterruptError(outcome); failed != nil {
		m.logf("sessiondrv: interrupt undeliverable ws=%s session=%s outcome=%s", workspace, d.sessionID, outcome)
		return failed
	}
	m.logf("sessiondrv: interrupt ws=%s session=%s outcome=%s", workspace, d.sessionID, outcome)
	return nil
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
	floor, haveFloor := d.consumer.resync(fromSeq)
	if !haveFloor {
		floor = m.cfg.SeqStore.LastSeq(d.sessionID) + 1
	}
	if fromSeq >= floor {
		return nil // the ring covered the whole request
	}
	m.logf("sessiondrv: resync ws=%q from_seq=%d is below the retained floor %d; re-pulling the gap from the store",
		workspace, fromSeq, floor)
	return m.startRepull(d, fromSeq, floor)
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
	return nil, fmt.Errorf("sessiondrv: no live session for workspace %q", workspace)
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
	cons := newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, m.cfg.Progress, m.logf, func(ss *corev1.SessionStarted) {
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

	go func() {
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

// onConnected reconciles SSM turn state on a mid-turn reattach (task step 1):
// when the shim reports a turn in flight, the SSM must not read idle. The store
// replays events from last_seen_seq on Subscribe, so the SSM re-derives turn
// state from the replayed TurnStarted; this hook loud-logs the observation so a
// reconciliation gap is visible rather than silent.
func (m *Manager) onConnected(workspace, sessionID string, hello *corev1.ShimHello) {
	if hello.GetTurnInFlight() {
		m.logf("sessiondrv: reattached mid-turn ws=%s session=%s (turn_in_flight); SSM re-derives from replayed events", workspace, sessionID)
	}
}

// Close stops every driver and abandons pending permissions (no fabricated
// answers). Idempotent.
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
