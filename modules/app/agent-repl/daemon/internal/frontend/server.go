package frontend

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// defaultClientBuffer bounds a single frontend connection's outbound queue.
// A consumer that cannot keep up fills this and is hard-disconnected (§6.5 /
// §5.4 fan-out contract); reconnect replays a fresh snapshot, so nothing is
// lost by construction.
const defaultClientBuffer = 256

// guiSnapshotLeaseInterval is the maximum time a live GUI stream goes without
// receiving a fresh authoritative snapshot. The browser expires its visible
// WorkspaceState after three missed leases, so a wedged writer cannot leave an
// active phase presented forever behind an apparently open WebSocket.
const guiSnapshotLeaseInterval = 15 * time.Second

// StateProvider is the SSM-like read surface the frontend server snapshots on
// every (re)connect. It is defined NARROWLY here on purpose: this package does
// NOT import daemon/internal/ssm (which does not exist in this worktree). The
// stitch phase binds the real SSM-backed implementation.
type StateProvider interface {
	// Snapshot returns the current resolved state of every workspace, session,
	// and open-task catalog, plus every durable WorkspaceAvailable and
	// HostAction still awaiting the Emacs host.  The workspace-creation manager
	// is the authority for those latter fields; stitch it into the provider
	// rather than teaching the transport package creation business logic.  It
	// must be safe for concurrent use; the server calls it while holding its
	// client-registry lock so the snapshot and subsequent delta stream cannot
	// interleave.
	Snapshot() *frontendv1.StateSnapshot
}

// Config configures a Server. Logf, State, and Handler are required.
type Config struct {
	Logf dlog.Logf
	// Warnf is the WARN channel, for a record that accompanies something the
	// user can see go wrong: a refused command, a connect served without the
	// snapshot or roster it was supposed to carry, a resync that could not be
	// marshalled. At info those sit beside the per-command chatter this server
	// emits constantly and are invisible to a level filter.
	//
	// Nil falls back to Logf, so the record is still made and only its
	// severity is lost.
	Warnf       dlog.Logf
	LogVerbosef dlog.Logf
	State       StateProvider
	Handler     CommandHandler
	// SessionPublicationAllowed is the daemon-owned materialization latch for
	// session-scoped frames.  The frontend transport invokes it at its single
	// fan-out boundary so no producer can bypass the host-ready invariant.
	SessionPublicationAllowed func(workspace, sessionID string) (bool, error)
	// BufSize is the per-client outbound buffer; <=0 uses defaultClientBuffer.
	BufSize int
	// CommandLatency persists one lifecycle-timing record per completed
	// command. Leaving it unset disables the telemetry, which New reports
	// loudly: transport-only tests legitimately run without it, a daemon does
	// not.
	CommandLatency CommandLatencyRecorder
	// AckWarnThreshold is the ack latency at which a command's record is
	// raised from debug to warn; <=0 uses DefaultAckWarnThreshold.
	AckWarnThreshold time.Duration
}

// Server serves agentshim.frontend.v1 frames as protojson over a UDS listener
// (Emacs, newline-delimited) and a WebSocket endpoint (webapp, one frame per
// message). Every connected frontend receives every broadcast frame (workspace
// entitlement is "all" for now; the fan-out list is the future filter point).
type Server struct {
	logf dlog.Logf
	// warnf is the WARN channel described on Config.Warnf. Never nil after
	// New; reached through warn.
	warnf                     dlog.Logf
	logVerbosef               dlog.Logf
	state                     StateProvider
	handler                   CommandHandler
	sessionPublicationAllowed func(workspace, sessionID string) (bool, error)
	bufSize                   int
	latency                   CommandLatencyRecorder
	ackWarn                   time.Duration
	// inflight is the daemon-wide count of frontend commands currently between
	// receipt and ack. It is the QUEUE DEPTH every latency record carries: a
	// per-connection read loop dispatches serially, so a command that waited
	// behind other work can only see that work by counting it.
	inflight atomic.Int64

	upgrader websocket.Upgrader

	mu sync.Mutex
	// publicationMu serializes the host materialization release against every
	// session-scoped live enqueue. Readers hold it from durable gate verdict
	// through enqueue; release owns the writer side while opening the decision
	// and enqueuing its authoritative snapshot.
	publicationMu sync.RWMutex
	clients       map[*client]struct{}
	listener      net.Listener
	closed        bool
	nextClientID  uint64
	// latestWorkspaceAt is the newest WorkspaceState revision that crossed the
	// delivery lock. Snapshot paths use it to detect a concurrent publication.
	latestWorkspaceAt map[string]int64
	// roster is the newest workspace roster Emacs published, or nil when none
	// has been. It lives under the DELIVERY lock rather than a lock of its own
	// so retention, fan-out and a concurrent connect are serialized against
	// each other by the same mechanism that already serializes emissions —
	// see PublishWorkspaceRoster. In-memory only, by design: the revision is
	// monotonic per Emacs BOOT, so a retained roster must not outlive its
	// publisher.
	roster *frontendv1.WorkspaceRoster
}

// client is one connected frontend's outbound state. out is never closed
// (avoids publish-after-close races with concurrent broadcasts); done signals
// the writer to stop and is closed exactly once by disconnect.
//
// scope, when non-nil, restricts this connection to one session/workspace (the
// per-session GET /sessions/{id}/stream view); nil is the unfiltered /frontend
// consumer that sees every workspace.
//
// kind names the frontend product behind the connection. It is fixed at accept
// and never reassigned — see ClientKind.
type client struct {
	id        uint64
	out       *outbox
	done      chan struct{}
	closeOnce sync.Once
	scope     *Scope
	kind      ClientKind
}

// newClient builds a client with a bounded outbox of the given size.
func newClient(bufSize int, scope *Scope, kind ClientKind) *client {
	return &client{
		out:   newOutbox(bufSize),
		done:  make(chan struct{}),
		scope: scope,
		kind:  kind,
	}
}

// warn emits through the Server's WARN channel (Config.Warnf, or Logf when
// that is unwired). It is the sole reader of warnf.
func (s *Server) warn(format string, args ...any) { s.warnf(format, args...) }

// New builds a Server. It panics on a missing required dependency: a frontend
// server with no state provider or handler is a programmer error, surfaced
// loudly rather than as a nil-deref later.
func New(cfg Config) *Server {
	if cfg.Logf == nil {
		panic("frontend: Config.Logf is required")
	}
	if cfg.LogVerbosef == nil {
		panic("frontend: Config.LogVerbosef is required")
	}
	if cfg.State == nil {
		panic("frontend: Config.State is required")
	}
	if cfg.Handler == nil {
		panic("frontend: Config.Handler is required")
	}
	buf := cfg.BufSize
	if buf <= 0 {
		buf = defaultClientBuffer
	}
	ackWarn := cfg.AckWarnThreshold
	if ackWarn <= 0 {
		ackWarn = DefaultAckWarnThreshold
	}
	warnf := cfg.Warnf
	if warnf == nil {
		warnf = cfg.Logf
	}
	return &Server{
		logf:                      cfg.Logf,
		warnf:                     warnf,
		logVerbosef:               cfg.LogVerbosef,
		state:                     cfg.State,
		handler:                   cfg.Handler,
		sessionPublicationAllowed: cfg.SessionPublicationAllowed,
		bufSize:                   buf,
		latency:                   cfg.CommandLatency,
		ackWarn:                   ackWarn,
		upgrader: websocket.Upgrader{
			// Local-loopback developer tool; the webview origin is app-scoped,
			// so origin checks are permissive by design (mirrors the existing
			// daemon server upgrader).
			CheckOrigin: func(*http.Request) bool { return true },
		},
		clients:           map[*client]struct{}{},
		latestWorkspaceAt: map[string]int64{},
	}
}

// DefaultSocketPath is the production UDS path for the Emacs frontend.
func DefaultSocketPath() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("frontend: resolve home dir: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "sock", "daemon-frontend.sock"), nil
}

// ServeUDS listens on the given unix socket path and serves frontend clients
// until Close. It removes a stale socket file, creates the parent directory,
// and blocks in the accept loop; a nil return means Close was called.
func (s *Server) ServeUDS(path string) error {
	l, err := ListenUDS(path)
	if err != nil {
		return err
	}
	return s.Serve(l)
}

// ListenUDS binds the production frontend UDS socket without entering the
// accept loop.  Startup uses it to make readiness depend on the socket being
// genuinely bound, rather than on a goroutine having merely been scheduled.
func ListenUDS(path string) (net.Listener, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return nil, fmt.Errorf("frontend: mkdir socket dir: %w", err)
	}
	// Remove a stale socket; a live one would refuse to bind.
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, fmt.Errorf("frontend: remove stale socket %s: %w", path, err)
	}
	l, err := net.Listen("unix", path)
	if err != nil {
		return nil, fmt.Errorf("frontend: listen unix %s: %w", path, err)
	}
	return l, nil
}

// Serve accepts UDS connections on l until Close. Exposed for tests (temp-dir
// listeners) and for callers that own listener creation.
func (s *Server) Serve(l net.Listener) error {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return net.ErrClosed
	}
	s.listener = l
	s.mu.Unlock()

	for {
		conn, err := l.Accept()
		if err != nil {
			s.mu.Lock()
			done := s.closed
			s.mu.Unlock()
			if done {
				return nil
			}
			return fmt.Errorf("frontend: accept: %w", err)
		}
		// The UDS endpoint is Emacs, the host frontend: it is entitled to the
		// host-only frames no GUI transport may see.
		go s.serveClient(newUDSConn(conn), nil, ClientKindHost)
	}
}

// ServeWS upgrades an HTTP request to a WebSocket and serves it as an
// UNSCOPED frontend client (the /frontend endpoint: every workspace). Mount it
// on the daemon's HTTP mux. It blocks for the connection's lifetime (the reader
// loop runs on this goroutine).
//
// Its only production consumer is the webapp's bootstrap socket, which exists
// to create a session and close before any page has rendered.
func (s *Server) ServeWS(w http.ResponseWriter, r *http.Request) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	s.serveClient(newWSConn(conn), nil, ClientKindGUIBootstrap)
}

// CommandTranslator converts one raw inbound message on a scoped connection
// into a FrontendCommand to dispatch. dispatch=false means the translator
// handled the message itself (logged it, or it is a deliberate no-op) and no
// command should be dispatched — the read loop simply reads the next message.
// A non-nil error marks a bad frame (logged; the connection continues). It
// lets the per-session /stream bridge stamp the connection's workspace onto
// each decoded command before dispatch.
type CommandTranslator func(raw []byte) (cmd *frontendv1.FrontendCommand, dispatch bool, err error)

// ServeWSScoped upgrades an HTTP request to a WebSocket and serves it as a
// client SCOPED to one session/workspace: only frames matching scope reach it
// (the connect/resync snapshot is filtered likewise). translate adapts inbound
// messages into FrontendCommands the shared handler dispatches (the /stream
// bridge uses it to stamp the connection's workspace); a nil translate parses
// frontend.v1 commands directly. This backs GET /sessions/{id}/stream.
//
// kind is the caller's declaration of which frontend product this connection
// is, fixed here at accept. The daemon's route table names it, so the client
// partition is one readable place rather than an inference from transport.
func (s *Server) ServeWSScoped(w http.ResponseWriter, r *http.Request, scope Scope, kind ClientKind, translate CommandTranslator) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	wc := newWSConn(conn)
	wc.translate = translate
	// The scoped stream logs only refusals through this handle, so it takes
	// the WARN channel rather than the plain one.
	wc.warnf = s.warnf
	s.serveClient(wc, &scope, kind)
}

// Close stops accepting, disconnects every client, and closes the listener.
func (s *Server) Close() error {
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		return nil
	}
	s.closed = true
	l := s.listener
	clients := make([]*client, 0, len(s.clients))
	for cl := range s.clients {
		clients = append(clients, cl)
	}
	s.mu.Unlock()

	for _, cl := range clients {
		s.disconnect(cl)
	}
	if l != nil {
		return l.Close()
	}
	return nil
}

// ---------------------------------------------------------------------------
// Fan-out API — the surface the stitch phase (shimclient/ssm/merge) pushes to.
// ---------------------------------------------------------------------------

// Broadcast enqueues frame to every connected client. An unscoped client gets
// the frame marshaled once (shared bytes); a scoped client gets the frame only
// if it matches its scope (a StateSnapshot is scope-filtered), marshaled per
// connection. A client whose bounded buffer is full is hard-disconnected.
//
// A WorkspaceState frame is routed through PushWorkspaceState, here rather than
// only at the convenience helper, so its one delivery path cannot be bypassed
// by reaching for the general fan-out.
// It reports HOW MANY clients the frame was actually queued to. The count is
// not decoration: a host-only frame (workspace availability, a host action) is
// addressed to exactly one kind of client, and delivering it while no client of
// that kind is connected loses it silently — the caller has no other way to
// learn that happened, and the host-only push helpers below turn a zero into a
// loud line rather than a shrug.
func (s *Server) Broadcast(frame *frontendv1.FrontendFrame) int {
	if ws := frame.GetWorkspaceState(); ws != nil {
		s.PushWorkspaceState(ws)
		return 0
	}
	_, _, scoped := frameSessionIdentity(frame)
	if scoped {
		s.publicationMu.RLock()
		defer s.publicationMu.RUnlock()
	}
	if !s.requireSessionPublication(frame) {
		return 0
	}
	var (
		unscoped     []byte
		unscopedErr  error
		unscopedDone bool
	)
	marshalUnscoped := func() ([]byte, error) {
		if !unscopedDone {
			unscoped, unscopedErr = marshalFrame(frame)
			unscopedDone = true
		}
		return unscoped, unscopedErr
	}
	s.mu.Lock()
	clients := make([]*client, 0, len(s.clients))
	for cl := range s.clients {
		clients = append(clients, cl)
	}
	s.mu.Unlock()
	delivered := 0
	for _, cl := range clients {
		if isHostOnlyFrame(frame) && !cl.kind.isHost() {
			continue
		}
		var (
			data []byte
			err  error
			sent = frame
		)
		if cl.scope == nil {
			data, err = marshalUnscoped()
		} else {
			out, keep := scopeFrame(frame, *cl.scope)
			if !keep {
				continue
			}
			sent = out
			data, err = marshalFrame(out)
		}
		if err != nil {
			s.logf("frontend: marshal frame for broadcast: %v", err)
			continue
		}
		s.enqueue(cl, outFrame{key: coalesceKey(sent), data: data})
		delivered++
	}
	return delivered
}

// PushWorkspaceState delivers a resolved state to EVERY connected frontend at
// once — same order, same content, no frontend waiting on another.
//
// It used to be sequenced: the rendering webview received it first, and the
// Emacs tab bar only once that webview acknowledged having drawn it. That
// ordering existed to stop two surfaces disagreeing about a state one of them
// had not painted yet, and it bought that at the cost of a whole viewer-based
// attestation model — which could also OMIT a workspace from an observer's
// reconnect snapshot entirely when its first emission was still held. A
// workspace's color is connection truth now; a viewer's render pace has no
// claim on it.
//
// The delivery happens under mu, so an emission, a connect and a disconnect are
// serialized against each other and every client's queue carries states in
// exactly the order the resolver produced them.
func (s *Server) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	s.publicationMu.RLock()
	defer s.publicationMu.RUnlock()
	if !s.requireSessionPublication(WorkspaceStateFrame(w)) {
		return
	}
	s.mu.Lock()
	if at := w.GetAtMs(); at > s.latestWorkspaceAt[w.GetWorkspace()] {
		s.latestWorkspaceAt[w.GetWorkspace()] = at
	}
	slow := s.deliverLocked(WorkspaceStateFrame(w), func(*client) bool { return true })
	s.mu.Unlock()
	s.disconnectAll(slow)
}

// requireSessionPublication enforces the durable creation latch at the one
// transport boundary shared by every session-scoped producer. A valid held
// decision suppresses the frame until release publishes an authoritative
// snapshot; a gate error is the invariant violation.
func (s *Server) requireSessionPublication(frame *frontendv1.FrontendFrame) bool {
	if s.sessionPublicationAllowed == nil {
		return true
	}
	workspace, sessionID, scoped := frameSessionIdentity(frame)
	if !scoped {
		return true
	}
	allowed, err := s.sessionPublicationAllowed(workspace, sessionID)
	if err != nil {
		s.logf("frontend: SESSION PUBLICATION INVARIANT VIOLATION workspace=%q session=%q frame=%T error=%v", workspace, sessionID, frame.GetFrame(), err)
		panic(fmt.Sprintf("frontend: session publication invariant workspace=%q session=%q frame=%T: %v", workspace, sessionID, frame.GetFrame(), err))
	}
	return allowed
}

func frameSessionIdentity(frame *frontendv1.FrontendFrame) (workspace, sessionID string, scoped bool) {
	switch f := frame.GetFrame().(type) {
	case *frontendv1.FrontendFrame_WorkspaceState:
		return f.WorkspaceState.GetWorkspace(), f.WorkspaceState.GetSessionId(), true
	case *frontendv1.FrontendFrame_SessionView:
		return f.SessionView.GetWorkspace(), f.SessionView.GetSessionId(), true
	case *frontendv1.FrontendFrame_ConversationDelta:
		return f.ConversationDelta.GetWorkspace(), f.ConversationDelta.GetSessionId(), true
	case *frontendv1.FrontendFrame_TypingDelta:
		return f.TypingDelta.GetWorkspace(), f.TypingDelta.GetSessionId(), true
	case *frontendv1.FrontendFrame_TaskCatalog:
		return f.TaskCatalog.GetWorkspace(), f.TaskCatalog.GetSessionId(), true
	case *frontendv1.FrontendFrame_SessionInit:
		return f.SessionInit.GetWorkspace(), f.SessionInit.GetSessionId(), true
	case *frontendv1.FrontendFrame_Heartbeat:
		return f.Heartbeat.GetWorkspace(), f.Heartbeat.GetSessionId(), true
	case *frontendv1.FrontendFrame_Queue:
		return f.Queue.GetWorkspace(), f.Queue.GetSessionId(), true
	case *frontendv1.FrontendFrame_Progress:
		return f.Progress.GetWorkspace(), f.Progress.GetSessionId(), true
	default:
		return "", "", false
	}
}

// deliverLocked enqueues frame into every client want selects, returning the
// clients whose bounded buffer overflowed so the caller can disconnect them
// after releasing mu (disconnect takes mu itself). Caller holds mu.
func (s *Server) deliverLocked(frame *frontendv1.FrontendFrame, want func(*client) bool) []*client {
	var (
		unscoped     []byte
		unscopedErr  error
		unscopedDone bool
		slow         []*client
	)
	for cl := range s.clients {
		if !want(cl) {
			continue
		}
		if isHostOnlyFrame(frame) && !cl.kind.isHost() {
			continue
		}
		var (
			data []byte
			err  error
			sent = frame
		)
		if cl.scope == nil {
			if !unscopedDone {
				unscoped, unscopedErr = marshalFrame(frame)
				unscopedDone = true
			}
			data, err = unscoped, unscopedErr
		} else {
			out, keep := scopeFrame(frame, *cl.scope)
			if !keep {
				continue
			}
			sent = out
			data, err = marshalFrame(out)
		}
		if err != nil {
			s.logf("frontend: marshal frame for delivery: %v", err)
			continue
		}
		queued, compacted := enqueueLocked(cl, outFrame{key: coalesceKey(sent), data: data})
		if !queued {
			s.logf("frontend: slow consumer (%s), outbound buffer full (%d frames) after compacting %d superseded frames, hard-disconnecting; reconnect replays snapshot",
				cl.kind, cl.out.capacity(), compacted)
			slow = append(slow, cl)
		}
	}
	return slow
}

// enqueueLocked offers a frame to a client's bounded outbox without blocking,
// compacting superseded frames first if the queue is already full. It reports
// queued=false ONLY for a live client whose queue is still full of frames
// nothing may replace; an already disconnected client is not a slow one and
// needs no second teardown. compacted is how many frames compaction removed,
// so a refusal can say what was tried.
func enqueueLocked(cl *client, f outFrame) (queued bool, compacted int) {
	select {
	case <-cl.done:
		return true, 0
	default:
	}
	return cl.out.push(f)
}

// disconnectAll tears down every client in the list. Called after mu is
// released, since disconnect takes it.
func (s *Server) disconnectAll(clients []*client) {
	for _, cl := range clients {
		s.disconnect(cl)
	}
}

// Convenience push helpers wrapping Broadcast, one per frame variant.
func (s *Server) PushSessionView(v *frontendv1.SessionView) { s.Broadcast(SessionViewFrame(v)) }
func (s *Server) PushConversationDelta(c *frontendv1.ConversationDelta) {
	s.Broadcast(ConversationDeltaFrame(c))
}
func (s *Server) PushTypingDelta(t *frontendv1.TypingDelta) { s.Broadcast(TypingDeltaFrame(t)) }
func (s *Server) PushTaskCatalog(c *frontendv1.TaskCatalog) { s.Broadcast(TaskCatalogFrame(c)) }
func (s *Server) PushSessionInitView(v *frontendv1.SessionInitView) {
	s.Broadcast(SessionInitViewFrame(v))
}
func (s *Server) PushHeartbeatView(h *frontendv1.HeartbeatView) {
	s.Broadcast(HeartbeatViewFrame(h))
}
func (s *Server) PushQueueView(q *frontendv1.QueueView) { s.Broadcast(QueueViewFrame(q)) }
func (s *Server) PushProgressView(p *frontendv1.ProgressView) {
	s.Broadcast(ProgressViewFrame(p))
}

// PushWorkspaceAvailable and PushHostAction carry HOST-ONLY work, and both
// report the number of host clients the frame reached. Zero means the frame was
// addressed to a kind of client that is not connected right now and is gone —
// the durable record behind it is the only remaining path to the host, and the
// caller is the one who has to say so.
func (s *Server) PushWorkspaceAvailable(v *frontendv1.WorkspaceAvailable) int {
	return s.Broadcast(WorkspaceAvailableFrame(v))
}
func (s *Server) PushHostAction(v *frontendv1.HostAction) int {
	return s.Broadcast(HostActionFrame(v))
}
func (s *Server) PushShutdownSchedule(v *frontendv1.ShutdownScheduleView) {
	s.Broadcast(ShutdownScheduleFrame(v))
}

// PushAuthoritativeSnapshot delivers one freshly resolved snapshot to every
// client, preserving each client's scope and host-only view.  Materialization
// release uses this existing reconnect truth to surface every held startup
// fact through one publication path.
func (s *Server) PushAuthoritativeSnapshot(snapshot *frontendv1.StateSnapshot) {
	s.mu.Lock()
	s.logf("frontend: authoritative snapshot publication clients=%d workspaces=%d sessions=%d progress=%d", len(s.clients), len(snapshot.GetWorkspaces()), len(snapshot.GetSessions()), len(snapshot.GetProgress()))
	slow := make([]*client, 0)
	for cl := range s.clients {
		view := snapshotForClient(snapshot, cl.scope, cl.kind)
		data, err := marshalFrame(SnapshotFrame(view))
		if err != nil {
			s.logf("frontend: marshal authoritative snapshot client_id=%d kind=%s: %v", cl.id, cl.kind, err)
			continue
		}
		if queued, _ := enqueueLocked(cl, outFrame{data: data}); !queued {
			s.logf("frontend: authoritative snapshot queue saturated client_id=%d kind=%s scope_workspace=%q scope_session=%q outcome=disconnect", cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope))
			slow = append(slow, cl)
		} else {
			s.logVerbosef("frontend: authoritative snapshot queued client_id=%d kind=%s scope_workspace=%q scope_session=%q", cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope))
		}
	}
	s.mu.Unlock()
	s.disconnectAll(slow)
}

// ReleaseSessionPublication makes one creation session publishable and queues
// its full authoritative snapshot before any concurrent session frame can
// pass the materialization gate.
func (s *Server) ReleaseSessionPublication(open func() error, snapshot func() *frontendv1.StateSnapshot) error {
	if open == nil || snapshot == nil {
		return fmt.Errorf("frontend: materialization release requires open and snapshot functions")
	}
	s.publicationMu.Lock()
	defer s.publicationMu.Unlock()
	if err := open(); err != nil {
		return fmt.Errorf("frontend: materialization release open: %w", err)
	}
	s.PushAuthoritativeSnapshot(snapshot())
	return nil
}

// isHostOnlyFrame marks daemon-to-host work that must never cross into either
// GUI transport.  ClientKind is the authority: every GUI connection is barred,
// bootstrap and session stream alike.
func isHostOnlyFrame(frame *frontendv1.FrontendFrame) bool {
	if frame == nil {
		return false
	}
	return frame.GetWorkspaceAvailable() != nil || frame.GetHostAction() != nil
}

// snapshotForClient applies the two independent views of a snapshot.  Scope
// limits a GUI connection to its session; host-only durable work is then
// removed from every non-host client.  The clone prevents a reconnect's
// filtering from mutating the state provider's retained snapshot.
func snapshotForClient(snapshot *frontendv1.StateSnapshot, scope *Scope, kind ClientKind) *frontendv1.StateSnapshot {
	if scope != nil {
		snapshot = filterSnapshot(snapshot, *scope)
	}
	if kind.isHost() {
		return snapshot
	}
	if snapshot == nil {
		return &frontendv1.StateSnapshot{}
	}
	filtered := proto.Clone(snapshot).(*frontendv1.StateSnapshot)
	filtered.WorkspaceAvailable = nil
	filtered.HostActions = nil
	return filtered
}

// isHostOnlyCommand identifies commands whose authority belongs exclusively
// to the Emacs UDS host.  They mutate durable creation/inbox state, so letting
// any GUI connection submit one would make an unrelated webview able to
// release prompts or consume host work.
func isHostOnlyCommand(cmd *frontendv1.FrontendCommand) bool {
	if cmd == nil {
		return false
	}
	return cmd.GetWorkspaceMaterialized() != nil || cmd.GetHostActionCompleted() != nil
}

func (s *Server) dispatchClientCommand(cl *client, cmd *frontendv1.FrontendCommand) (*frontendv1.CommandAck, *frontendv1.FrontendFrame) {
	if isHostOnlyCommand(cmd) && !cl.kind.isHost() {
		err := fmt.Errorf("frontend: host-only command rejected from client kind %s", cl.kind)
		s.warn("frontend: host-only command rejected kind=%s request_id=%s", cl.kind, cmd.GetRequestId())
		return failAck(s.logf, cmd.GetRequestId(), err), nil
	}
	return DispatchWithResponse(context.Background(), s.logf, s.handler, s, cmd)
}

// ---------------------------------------------------------------------------
// Per-connection lifecycle
// ---------------------------------------------------------------------------

func (s *Server) serveClient(c conn, scope *Scope, kind ClientKind) {
	cl := newClient(s.bufSize, scope, kind)

	// THE SNAPSHOT CARRIES EVERY WORKSPACE the provider knows about. It used to
	// be filtered down to the states a painter had settled, which could omit a
	// workspace from Emacs entirely; nothing filters it now but the connection's
	// own scope.
	var snapshot *frontendv1.StateSnapshot
	var rawSnapshot *frontendv1.StateSnapshot
	for {
		// Never call StateProvider while holding the frontend lock. Synchronous
		// SSM publication takes the locks in the opposite order. The revision
		// check under s.mu closes the capture race this lock order creates.
		rawSnapshot = s.state.Snapshot()
		snapshot = snapshotForClient(rawSnapshot, scope, kind)
		snap, err := marshalFrame(SnapshotFrame(snapshot))
		if err != nil {
			s.warn("frontend: marshal connect snapshot kind=%s scope_workspace=%q scope_session=%q: %v",
				kind, scopeWorkspace(scope), scopeSession(scope), err)
			_ = c.close()
			return
		}
		s.mu.Lock()
		if s.closed {
			s.mu.Unlock()
			_ = c.close()
			return
		}
		if workspace, snapshotAt, deliveredAt, stale := s.snapshotStaleLocked(snapshot, scope); stale {
			s.mu.Unlock()
			s.logVerbosef("frontend: connect snapshot retry kind=%s scope_workspace=%q scope_session=%q stale_workspace=%q snapshot_at_ms=%d delivered_at_ms=%d",
				kind, scopeWorkspace(scope), scopeSession(scope), workspace, snapshotAt, deliveredAt)
			continue
		}
		// Registration and enqueue remain one delivery-lock operation, so no
		// delta can slip ahead of this first FIFO frame after validation.
		s.nextClientID++
		cl.id = s.nextClientID
		// A fresh outbox is empty, so this first FIFO frame always fits; a
		// refusal here would be a programmer error, not a slow consumer.
		if queued, _ := enqueueLocked(cl, outFrame{data: snap}); !queued {
			s.mu.Unlock()
			s.warn("frontend: connect snapshot rejected by an empty outbox kind=%s scope_workspace=%q scope_session=%q",
				kind, scopeWorkspace(scope), scopeSession(scope))
			_ = c.close()
			return
		}
		// The retained roster rides the SAME delivery-lock operation as the
		// snapshot and the registration. That is what makes a connect racing a
		// publication safe: this client either sees the roster here or as the
		// broadcast that follows its registration, never neither and never the
		// older one after the newer. It is a separate frame because the roster
		// is not a StateSnapshot field — StateSnapshot is session-scoped state
		// and the roster is editor-global — and it is OMITTED entirely when
		// nothing has been published, so "no roster yet" stays distinguishable
		// from "an empty roster".
		if held := s.retainedRosterLocked(); held != nil {
			rosterData, rosterErr := marshalFrame(WorkspaceRosterFrame(held))
			if rosterErr != nil {
				s.mu.Unlock()
				s.warn("frontend: marshal connect roster kind=%s scope_workspace=%q revision=%d: %v",
					kind, scopeWorkspace(scope), held.GetRevision(), rosterErr)
				_ = c.close()
				return
			}
			if queued, _ := enqueueLocked(cl, outFrame{key: coalesceKey(WorkspaceRosterFrame(held)), data: rosterData}); !queued {
				s.mu.Unlock()
				s.warn("frontend: connect roster rejected by a near-empty outbox kind=%s scope_workspace=%q revision=%d",
					kind, scopeWorkspace(scope), held.GetRevision())
				_ = c.close()
				return
			}
		}
		s.clients[cl] = struct{}{}
		s.mu.Unlock()
		break
	}
	retainedSessions, rejectedSessions := snapshotScopeSessionAudit(rawSnapshot, scope)
	s.logf("frontend: client connected client_id=%d kind=%s scope_workspace=%q scope_session=%q snapshot_workspaces=%d snapshot_sessions_retained=%q snapshot_sessions_rejected=%q",
		cl.id, cl.kind, scopeWorkspace(scope), scopeSession(scope), len(snapshot.GetWorkspaces()), retainedSessions, rejectedSessions)

	go s.writeLoop(c, cl)
	if kind == ClientKindGUIStream {
		go s.snapshotLeaseLoop(cl)
	}
	s.readLoop(c, cl)
}

// snapshotLeaseLoop renews a GUI stream's authoritative state lease. A
// StateSnapshot is already the protocol's reconnect truth and ConnectResync
// deliberately requests conversation history only once per connection, so
// repeating this frame refreshes state without replaying conversation data.
func (s *Server) snapshotLeaseLoop(cl *client) {
	ticker := time.NewTicker(guiSnapshotLeaseInterval)
	defer ticker.Stop()
	for {
		select {
		case <-cl.done:
			return
		case <-ticker.C:
			if !s.renewSnapshotLease(cl) {
				return
			}
		}
	}
}

// renewSnapshotLease captures outside the delivery lock, validates the captured
// revision under that lock, then enqueues. Returning false means the client is
// gone or was detached, so its lease loop must stop.
func (s *Server) renewSnapshotLease(cl *client) bool {
	for {
		select {
		case <-cl.done:
			return false
		default:
		}
		// Preserve the SSM -> frontend lock order. snapshotStaleLocked closes
		// the race when a state publication lands after this capture.
		snapshot := snapshotForClient(s.state.Snapshot(), cl.scope, cl.kind)
		data, err := marshalFrame(SnapshotFrame(snapshot))
		if err != nil {
			s.logf("frontend: snapshot lease marshal failed client_id=%d kind=%s scope_workspace=%q scope_session=%q: %v",
				cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), err)
			s.disconnect(cl)
			return false
		}
		s.mu.Lock()
		if _, live := s.clients[cl]; !live {
			s.mu.Unlock()
			return false
		}
		if workspace, snapshotAt, deliveredAt, stale := s.snapshotStaleLocked(snapshot, cl.scope); stale {
			s.mu.Unlock()
			s.logVerbosef("frontend: snapshot lease retry client_id=%d kind=%s scope_workspace=%q scope_session=%q stale_workspace=%q snapshot_at_ms=%d delivered_at_ms=%d",
				cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), workspace, snapshotAt, deliveredAt)
			continue
		}
		// A snapshot supersedes nothing on the wire: the lease is the browser's
		// bounded freshness proof, and a full lease queue stays a hard
		// disconnect rather than a silent skip (AGENTS.md).
		queued, compacted := enqueueLocked(cl, outFrame{data: data})
		s.mu.Unlock()
		if !queued {
			s.logf("frontend: snapshot lease queue full client_id=%d kind=%s scope_workspace=%q scope_session=%q buffer=%d compacted=%d; hard-disconnecting",
				cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), cl.out.capacity(), compacted)
			s.disconnect(cl)
			return false
		}
		s.logVerbosef("frontend: snapshot lease renewed client_id=%d kind=%s scope_workspace=%q scope_session=%q workspaces=%d revisions=%s interval_ms=%d",
			cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), len(snapshot.GetWorkspaces()), snapshotRevisions(snapshot), guiSnapshotLeaseInterval.Milliseconds())
		return true
	}
}

// snapshotStaleLocked reports a snapshot captured before a WorkspaceState that
// has already crossed the delivery lock. Caller holds s.mu.
func (s *Server) snapshotStaleLocked(snapshot *frontendv1.StateSnapshot, scope *Scope) (string, int64, int64, bool) {
	seen := make(map[string]int64, len(snapshot.GetWorkspaces()))
	for _, state := range snapshot.GetWorkspaces() {
		seen[state.GetWorkspace()] = state.GetAtMs()
	}
	if scope != nil && scope.Workspace != "" {
		deliveredAt := s.latestWorkspaceAt[scope.Workspace]
		snapshotAt := seen[scope.Workspace]
		return scope.Workspace, snapshotAt, deliveredAt, deliveredAt > snapshotAt
	}
	for workspace, deliveredAt := range s.latestWorkspaceAt {
		if snapshotAt := seen[workspace]; deliveredAt > snapshotAt {
			return workspace, snapshotAt, deliveredAt, true
		}
	}
	return "", 0, 0, false
}

func scopeWorkspace(scope *Scope) string {
	if scope == nil {
		return ""
	}
	return scope.Workspace
}

func scopeSession(scope *Scope) string {
	if scope == nil {
		return ""
	}
	return scope.SessionID
}

// snapshotScopeSessionAudit names both sides of the scope decision that binds
// a GUI page. Counts alone hid the incident where twelve historical records
// shared one cwd and the last rejected identity rebound the live page.
func snapshotScopeSessionAudit(snapshot *frontendv1.StateSnapshot, scope *Scope) (retained, rejected string) {
	if snapshot == nil || scope == nil {
		return "", ""
	}
	var kept, dropped []string
	for _, view := range snapshot.GetSessions() {
		identity := view.GetSessionId()
		if scope.matchesAgentSession(identity, view.GetWorkspace()) {
			kept = append(kept, identity)
		} else {
			dropped = append(dropped, identity)
		}
	}
	return strings.Join(kept, ","), strings.Join(dropped, ",")
}

func snapshotRevisions(snapshot *frontendv1.StateSnapshot) string {
	parts := make([]string, 0, len(snapshot.GetWorkspaces()))
	for _, state := range snapshot.GetWorkspaces() {
		parts = append(parts, fmt.Sprintf("%s:%s:%d:%d", state.GetWorkspace(), state.GetControllerGenerationId(), state.GetAtMs(), state.GetCauseSeq()))
	}
	return strings.Join(parts, ",")
}

func (s *Server) writeLoop(c conn, cl *client) {
	defer func() {
		if err := c.close(); err != nil {
			s.logf("frontend: connection close: %v", err)
		}
	}()
	for {
		select {
		case <-cl.done:
			return
		case <-cl.out.ready:
			// ready is a wakeup, not the queue: drain everything each wake, so
			// a signal coalesced with an earlier one strands no frame.
			for {
				data, ok := cl.out.pop()
				if !ok {
					break
				}
				if err := c.writeFrame(data); err != nil {
					s.logf("frontend: write failed, disconnecting: %v", err)
					s.disconnect(cl)
					return
				}
			}
		}
	}
}

// readLoop reads this connection's commands and routes each onto its
// workspace's lane (lanes.go). Reading is deliberately NOT gated on the
// previous command finishing: a bring-up takes seconds, and a read loop that
// waited for one starved every command behind it — including commands for
// other workspaces — past the client's ack deadline.
//
// The lanes preserve every ordering the inline loop gave: one workspace's
// commands still run one at a time in arrival order, and each is still
// answered only once it has actually run.
func (s *Server) readLoop(c conn, cl *client) {
	lanes := newCommandLanes(s.logf, func(cmd *frontendv1.FrontendCommand, received time.Time, depth int64) {
		s.processCommand(cl, cmd, received, depth)
	})
	defer func() {
		// Drain first, disconnect second: a command already read still owes
		// the client an answer, exactly as it did when dispatch ran inline.
		lanes.close()
		s.disconnect(cl)
	}()
	for {
		cmd, err := c.readCommand()
		if err != nil {
			if !errors.Is(err, io.EOF) && !errors.Is(err, net.ErrClosed) && !isWSClose(err) {
				s.logf("frontend: read command: %v", err)
			}
			return
		}
		// EVERY COMMAND IS TIMED, and the clock starts the instant the frame
		// was decoded rather than inside dispatch: the interval the client
		// actually waits out is receipt through ack, and with lanes that
		// interval includes any wait in the lane's queue — instrumenting only
		// the handler would report a fast command sitting behind slow work as
		// fast. The depth is taken here, before any of this command's own work
		// runs, so it counts what was ALREADY in flight.
		received := time.Now()
		depth := s.inflight.Add(1)
		lanes.submit(cmd, received, depth)
	}
}

// processCommand performs one command and answers it. It is the body the read
// loop used to run inline, unchanged in every respect except where it runs and
// the receipt-side timing facts it reports against.
func (s *Server) processCommand(cl *client, cmd *frontendv1.FrontendCommand, received time.Time, depth int64) {
	// A resync re-sends a fresh StateSnapshot to THIS client (§5.4),
	// scope-filtered for a scoped connection. The handler covers the
	// conversation-delta replay the snapshot omits.
	if cmd.GetResync() != nil {
		s.enqueueResyncSnapshot(cl, cmd, "before_dispatch")
	}
	dispatchStart := time.Now()
	ack, response := s.dispatchClientCommand(cl, cmd)
	processing := time.Since(dispatchStart)
	// A stale generation is a request to adopt current authority, not a
	// history failure. Capture again AFTER the daemon made that decision so
	// a transition crossing the pre-dispatch capture cannot leave the client
	// holding the very identity the command just proved was retired.
	if cmd.GetResync() != nil && ack.GetFailure().GetErrorType() == string(errclass.TypeSessionReconnectSuperseded) {
		s.enqueueResyncSnapshot(cl, cmd, "after_superseded")
	}
	if !ack.GetOk() {
		s.logf("frontend: command nack {request_id=%s ws=%s}: %s", ack.GetRequestId(), cmd.GetWorkspace(), ack.GetError())
	}
	if response != nil {
		if data, err := marshalFrame(response); err != nil {
			s.logf("frontend: marshal command response request_id=%s: %v", ack.GetRequestId(), err)
		} else {
			s.enqueue(cl, outFrame{key: coalesceKey(response), data: data})
		}
	}
	if data, err := marshalFrame(CommandAckFrame(ack)); err != nil {
		s.logf("frontend: marshal command ack: %v", err)
	} else {
		s.enqueue(cl, outFrame{key: coalesceKey(response), data: data})
	}
	// The ack is on the wire's queue; that is the moment the client's wait
	// ends. The in-flight gauge is released BEFORE the record is written so
	// the telemetry never inflates the depth it reports.
	ackElapsed := time.Since(received)
	s.inflight.Add(-1)
	s.recordCommandLatency(cl, cmd, ack, depth, ackElapsed, processing)
}

// recordCommandLatency persists one completed command's lifecycle timing. A
// recorder failure is a routing or persistence invariant violation, so it is
// surfaced through the transport's own log rather than dropped.
func (s *Server) recordCommandLatency(cl *client, cmd *frontendv1.FrontendCommand, ack *frontendv1.CommandAck, depth int64, elapsed, processing time.Duration) {
	if s.latency == nil {
		return
	}
	sample := CommandLatencySample{
		Workspace:  cmd.GetWorkspace(),
		RequestID:  cmd.GetRequestId(),
		Command:    CommandFieldName(cmd),
		ClientKind: cl.kind.String(),
		QueueDepth: depth,
		Ack:        elapsed,
		Processing: processing,
		Threshold:  s.ackWarn,
		Ok:         ack.GetOk(),
	}
	if err := s.latency.RecordCommandLatency(sample); err != nil {
		s.logf("frontend: record command latency FAILED request_id=%q command=%s ws=%q ack_ms=%d: %v",
			sample.RequestID, sample.Command, sample.Workspace, sample.Ack.Milliseconds(), err)
	}
}

func (s *Server) enqueueResyncSnapshot(cl *client, cmd *frontendv1.FrontendCommand, phase string) {
	snapshot := snapshotForClient(s.state.Snapshot(), cl.scope, cl.kind)
	snap, err := marshalFrame(SnapshotFrame(snapshot))
	if err != nil {
		s.warn("frontend: marshal resync snapshot FAILED client_id=%d request_id=%q ws=%q phase=%q error=%v",
			cl.id, cmd.GetRequestId(), cmd.GetWorkspace(), phase, err)
		return
	}
	s.enqueue(cl, outFrame{data: snap})
	s.logVerbosef("frontend: resync snapshot queued client_id=%d request_id=%q ws=%q phase=%q workspaces=%d",
		cl.id, cmd.GetRequestId(), cmd.GetWorkspace(), phase, len(snapshot.GetWorkspaces()))
}

// enqueue delivers a frame to a client's bounded outbox. A full queue is first
// COMPACTED — every queued frame a later queued frame supersedes is replaced by
// that newer version — so a hidden webview that consumes slowly costs its own
// stale state rather than its connection. Only a queue still full after that,
// of frames nothing may replace, means the consumer is genuinely too slow:
// hard-disconnect it loudly (reconnect replays a fresh snapshot — no data loss
// by construction).
func (s *Server) enqueue(cl *client, f outFrame) {
	queued, compacted := enqueueLocked(cl, f)
	if queued {
		return
	}
	s.logf("frontend: slow consumer, outbound buffer full (%d frames) after compacting %d superseded frames, hard-disconnecting; reconnect replays snapshot",
		cl.out.capacity(), compacted)
	s.disconnect(cl)
}

// disconnect removes a client from the fan-out set and signals its writer to
// stop. Idempotent: safe to call from the reader, the writer, enqueue, or Close.
//
// It holds no per-connection delivery state to unwind: nothing is ever withheld
// from one frontend on account of another, so a departure strands nothing.
func (s *Server) disconnect(cl *client) {
	s.mu.Lock()
	delete(s.clients, cl)
	s.mu.Unlock()
	cl.closeOnce.Do(func() {
		close(cl.done)
		s.logf("frontend: client disconnected client_id=%d kind=%s scope_workspace=%q scope_session=%q",
			cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope))
	})
}

// clientCount reports the number of connected clients (test/introspection aid).
func (s *Server) clientCount() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.clients)
}

// ---------------------------------------------------------------------------
// protojson framing
// ---------------------------------------------------------------------------

var marshalOpts = protojson.MarshalOptions{}

func marshalFrame(frame *frontendv1.FrontendFrame) ([]byte, error) {
	b, err := marshalOpts.Marshal(frame)
	if err != nil {
		return nil, fmt.Errorf("frontend: protojson marshal: %w", err)
	}
	if frame.GetSessionView() == nil && len(frame.GetSnapshot().GetSessions()) == 0 {
		return b, nil
	}
	var wire map[string]any
	if err := json.Unmarshal(b, &wire); err != nil {
		return nil, fmt.Errorf("frontend: decode SessionView wire shape: %w", err)
	}
	if sessionView, ok := wire["sessionView"].(map[string]any); ok {
		sessionView["modelOptions"] = sessionViewModelOptions(sessionView)
	}
	if snapshot, ok := wire["snapshot"].(map[string]any); ok {
		if sessions, ok := snapshot["sessions"].([]any); ok {
			for index, raw := range sessions {
				sessionView, ok := raw.(map[string]any)
				if !ok {
					return nil, fmt.Errorf("frontend: snapshot session index=%d has invalid wire shape", index)
				}
				sessionView["modelOptions"] = sessionViewModelOptions(sessionView)
			}
		}
	}
	b, err = json.Marshal(wire)
	if err != nil {
		return nil, fmt.Errorf("frontend: encode SessionView wire shape: %w", err)
	}
	return b, nil
}

// sessionViewModelOptions preserves an SDK-published catalog and makes a valid
// empty catalog explicit. An absent field is forbidden by the web protocol.
func sessionViewModelOptions(sessionView map[string]any) any {
	if options, ok := sessionView["modelOptions"]; ok {
		return options
	}
	return []any{}
}

// ---------------------------------------------------------------------------
// conn: the two transports behind one lifecycle
// ---------------------------------------------------------------------------

// conn abstracts a single frontend connection's framing so serveClient handles
// UDS and WebSocket identically.
type conn interface {
	// writeFrame writes one protojson frame (UDS appends the newline delimiter;
	// WS sends it as one text message).
	writeFrame(data []byte) error
	// readCommand reads one inbound FrontendCommand (UDS: one newline-delimited
	// line; WS: one message). Returns io.EOF on clean close.
	readCommand() (*frontendv1.FrontendCommand, error)
	close() error
}

// udsConn frames protojson newline-delimited over a net.Conn.
type udsConn struct {
	nc net.Conn
	r  *bufio.Reader
}

func newUDSConn(nc net.Conn) *udsConn {
	return &udsConn{nc: nc, r: bufio.NewReader(nc)}
}

func (u *udsConn) writeFrame(data []byte) error {
	if _, err := u.nc.Write(data); err != nil {
		return err
	}
	_, err := u.nc.Write([]byte{'\n'})
	return err
}

func (u *udsConn) readCommand() (*frontendv1.FrontendCommand, error) {
	line, err := u.r.ReadBytes('\n')
	if err != nil {
		// A final unterminated line is still a valid frame to parse before EOF.
		if errors.Is(err, io.EOF) && len(line) > 0 {
			return unmarshalCommand(line)
		}
		return nil, err
	}
	return unmarshalCommand(line)
}

func (u *udsConn) close() error { return u.nc.Close() }

// wsConn frames protojson one-frame-per-message over a WebSocket. translate,
// when set (scoped /stream connections), adapts an inbound raw message into a
// FrontendCommand; a message the translator handles itself (dispatch=false) or
// rejects (err) is skipped and the loop reads the next one, so the shared read
// loop only ever sees dispatchable commands.
type wsConn struct {
	ws        *websocket.Conn
	translate CommandTranslator
	// warnf is the WARN channel for the one thing this handle reports: an
	// inbound command the scoped translator refused.
	warnf dlog.Logf
}

func newWSConn(ws *websocket.Conn) *wsConn { return &wsConn{ws: ws} }

func (w *wsConn) writeFrame(data []byte) error {
	return w.ws.WriteMessage(websocket.TextMessage, data)
}

func (w *wsConn) readCommand() (*frontendv1.FrontendCommand, error) {
	for {
		_, data, err := w.ws.ReadMessage()
		if err != nil {
			return nil, err
		}
		if w.translate == nil {
			return unmarshalCommand(data)
		}
		cmd, dispatch, terr := w.translate(data)
		if terr != nil {
			if w.warnf != nil {
				w.warnf("frontend: scoped stream: inbound command rejected: %v", terr)
			}
			continue
		}
		if !dispatch {
			continue
		}
		return cmd, nil
	}
}

func (w *wsConn) close() error { return w.ws.Close() }

func unmarshalCommand(data []byte) (*frontendv1.FrontendCommand, error) {
	cmd := &frontendv1.FrontendCommand{}
	if err := protojson.Unmarshal(data, cmd); err != nil {
		return nil, fmt.Errorf("frontend: protojson unmarshal command: %w", err)
	}
	return cmd, nil
}

func isWSClose(err error) bool {
	return websocket.IsCloseError(err,
		websocket.CloseNormalClosure,
		websocket.CloseGoingAway,
		websocket.CloseAbnormalClosure,
		websocket.CloseNoStatusReceived)
}

// compile-time assertion that a *FrontendFrame is a proto.Message (guards the
// marshaler against a generated-type regression).
var _ proto.Message = (*frontendv1.FrontendFrame)(nil)
