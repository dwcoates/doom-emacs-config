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
	"sync"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

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
	Interrupt(ctx context.Context, hard bool) error
}

// Config assembles a Manager. Every collaborator is injected so the driver is
// testable and free of the daemon's HTTP surface.
type Config struct {
	// Push is the frontend server (conversation/typing/task/degraded/state).
	Push Pusher
	// SSM applies lifecycle events. Required.
	SSM StateApplier
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
		return d.client.SubmitPrompt(ctx, text, "frontend", permissionMode)
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
func (m *Manager) Interrupt(ctx context.Context, workspace string, hard bool) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	return d.client.Interrupt(ctx, hard)
}

// AnswerPermission delivers a frontend permission answer to the parked
// canUseTool round-trip (keyed by permissionRequestID). A stale/duplicate
// answer is a loud error, never swallowed.
func (m *Manager) AnswerPermission(_ context.Context, workspace, permissionRequestID string, allow bool, denyMessage string, updatedInput *structpb.Struct) error {
	m.logf("sessiondrv: permission answer ws=%s request_id=%s allow=%v", workspace, permissionRequestID, allow)
	return m.reg.answer(permissionRequestID, allow, denyMessage, updatedInput)
}

// Resync replays the workspace session's retained conversation deltas from
// fromSeq (task step 5). A workspace with no live session is a loud error.
func (m *Manager) Resync(workspace string, fromSeq uint64) error {
	d, err := m.existing(workspace)
	if err != nil {
		return err
	}
	d.consumer.resync(fromSeq)
	return nil
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
// "that shim belongs to someone else — do not touch it".
var ErrNotLiveSession = errors.New("sessiondrv: not the live session for this workspace")

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

// HibernateSession suspends workspace's live session ONLY when it is sessionID,
// returning ErrNotLiveSession (having stopped nothing) when a different session
// drives the workspace.
//
// Several registry records can share one cwd — a stale duplicate, a superseded
// resume, an orphan awaiting reap — so "stop THIS record's shim" is not the same
// question as "stop the workspace's shim". Answering it with the workspace-keyed
// Hibernate SIGTERMs whichever shim happens to be live, which on 2026-07-25
// meant reaping an orphan killed the healthy session created 175ms earlier for
// the same workspace, leaving the user with nothing to drive.
func (m *Manager) HibernateSession(workspace, sessionID string) error {
	return m.hibernate(workspace, sessionID)
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
	cons := newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, m.logf, func(ss *corev1.SessionStarted) {
		m.armMetaprompt(d, ss)
		m.persistVendorSessionID(sessionID, ss.GetVendorSessionId())
	}, func(active bool) {
		m.onTurnBoundary(d, active)
	})
	d.consumer = cons
	ph := permHandler{reg: m.reg, cons: cons, logf: m.logf}

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
		if err := client.Run(runCtx); err != nil {
			m.logf("sessiondrv: session %s driver ended: %v", sessionID, err)
		}
		m.mu.Lock()
		if cur, ok := m.byWS[workspace]; ok && cur == d {
			delete(m.byWS, workspace)
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
		m.cfg.Push.PushQueueView(view)
		m.persistQueue(sessionID, nil)
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
	defer release()
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
