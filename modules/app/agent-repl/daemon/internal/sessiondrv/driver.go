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
package sessiondrv

import (
	"context"
	"fmt"
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/structpb"
)

// Spawner makes a session's UDS shim reachable at its socket: it reattaches to
// a live shim (the reattach-first decision, §4.4) or spawns a fresh UDS-mode
// shim (ShimUDSArgv), returning once the shim is listening or on failure. The
// concrete impl lives in the server package (it owns ReattachDecision and the
// spawn plumbing); injected here so the driver stays IO-narrow and testable.
type Spawner interface {
	EnsureShim(ctx context.Context, sessionID, socketPath string) error
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
	// Logf is the daemon logger. Nil discards.
	Logf func(string, ...any)

	// socketPath and newClient are injected only by tests; production uses the
	// package defaults (shimclient.DefaultSocketPath / a real shimclient).
	socketPath func(sessionID string) string
	newClient  func(cfg shimclient.Config) sessionClient
}

// Manager is the fleet of per-session drivers. It implements the frontend
// PromptRouter (SubmitPrompt/Interrupt/AnswerPermission) plus Resync.
type Manager struct {
	cfg  Config
	logf func(string, ...any)
	reg  *permRegistry

	socketPath func(sessionID string) string
	newClient  func(cfg shimclient.Config) sessionClient

	mu       sync.Mutex
	byWS     map[string]*driven // workspace -> live driver
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
	}
	logf := cfg.Logf
	if logf == nil {
		logf = func(string, ...any) {}
	}
	socketPath := cfg.socketPath
	if socketPath == nil {
		socketPath = shimclient.DefaultSocketPath
	}
	newClient := cfg.newClient
	if newClient == nil {
		newClient = func(c shimclient.Config) sessionClient { return shimclient.New(c) }
	}
	rootCtx, rootStop := context.WithCancel(context.Background())
	return &Manager{
		cfg:        cfg,
		logf:       logf,
		reg:        newPermRegistry(logf),
		socketPath: socketPath,
		newClient:  newClient,
		byWS:       make(map[string]*driven),
		rootCtx:    rootCtx,
		rootStop:   rootStop,
	}, nil
}

// SubmitPrompt brings the workspace's session up (lazily, reattach-first) and
// submits text to its shim. On the FIRST prompt after a RESUME/COMPACT_CONTINUE
// session start, the metaprompt read-directive is prepended once (task step 4).
func (m *Manager) SubmitPrompt(ctx context.Context, workspace, text, permissionMode string) error {
	d, err := m.ensure(workspace)
	if err != nil {
		return err
	}
	text = m.applyMetapromptLocked(d, text)
	return d.client.SubmitPrompt(ctx, text, "frontend", permissionMode)
}

// applyMetapromptLocked folds the metaprompt directive into text once for an
// armed session, loud-logging the fold. Takes/releases the manager mutex.
func (m *Manager) applyMetapromptLocked(d *driven, text string) string {
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
func (m *Manager) ensure(workspace string) (*driven, error) {
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
	socketPath := m.socketPath(sessionID)
	if err := m.cfg.Spawner.EnsureShim(m.rootCtx, sessionID, socketPath); err != nil {
		return nil, fmt.Errorf("sessiondrv: ensure shim for session %s (ws %q): %w", sessionID, workspace, err)
	}

	d := &driven{sessionID: sessionID, workspace: workspace}
	cons := newConsumer(workspace, sessionID, m.cfg.Push, m.cfg.SSM, m.logf, func(ss *corev1.SessionStarted) {
		m.armMetaprompt(d, ss)
	})
	d.consumer = cons
	ph := permHandler{reg: m.reg, push: m.cfg.Push, workspace: workspace, sessionID: sessionID, logf: m.logf}

	runCtx, cancel := context.WithCancel(m.rootCtx)
	d.cancel = cancel
	client := m.newClient(shimclient.Config{
		SessionID:       sessionID,
		SocketPath:      m.socketPath,
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
		m.mu.Unlock()
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
// surfaces a permission render-state and blocks on the rendezvous until the
// frontend answers (or teardown abandons it).
type permHandler struct {
	reg       *permRegistry
	push      Pusher
	workspace string
	sessionID string
	logf      func(string, ...any)
}

func (h permHandler) HandlePermission(sessionID string, req *corev1.PermissionRequest) *corev1.PermissionResponse {
	h.logf("sessiondrv: permission prompt ws=%s session=%s request_id=%s tool=%s (awaiting frontend answer)",
		h.workspace, sessionID, req.GetRequestId(), req.GetToolName())
	// Surface a permission render-state so the frontend shows the prompt while
	// the shim's canUseTool blocks. Eventually-consistent: the SSM re-pushes
	// the resolved state as events flow, and a frontend resync corrects any lag.
	h.push.PushWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: h.workspace,
		SessionId: h.sessionID,
		State:     frontendv1.RenderState_RENDER_STATE_PERMISSION,
	})
	ch, release := h.reg.await(req.GetRequestId())
	defer release()
	return <-ch
}
