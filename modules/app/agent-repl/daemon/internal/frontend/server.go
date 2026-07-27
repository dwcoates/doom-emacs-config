package frontend

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"
)

// defaultClientBuffer bounds a single frontend connection's outbound queue.
// A consumer that cannot keep up fills this and is hard-disconnected (§6.5 /
// §5.4 fan-out contract); reconnect replays a fresh snapshot, so nothing is
// lost by construction.
const defaultClientBuffer = 256

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
	Logf    dlog.Logf
	State   StateProvider
	Handler CommandHandler
	// BufSize is the per-client outbound buffer; <=0 uses defaultClientBuffer.
	BufSize int
	// WithdrawPaint retracts a workspace's paint attestation because the
	// renderer that made it is no longer the one on screen (F5).
	//
	// It is what makes the paint axis a LATCH THAT RE-ARMS rather than a
	// one-time flag. Attestation is a claim by one renderer about one
	// connection: a painter that departs takes its claim with it, and a
	// painter that arrives has made none yet. Both edges call this, so a
	// workspace can only be green while some CURRENT renderer has said it drew
	// the history — which is what the five-color contract has always
	// documented and what nothing previously enforced.
	//
	// Called with the server's lock RELEASED, because the withdrawal resolves a
	// new state that comes straight back in through PushWorkspaceState. Nil
	// leaves the paint axis untouched, which is what every test that is not
	// about attestation wants.
	WithdrawPaint func(workspace, reason string)
}

// Server serves agentshim.frontend.v1 frames as protojson over a UDS listener
// (Emacs, newline-delimited) and a WebSocket endpoint (webapp, one frame per
// message). Every connected frontend receives every broadcast frame (workspace
// entitlement is "all" for now; the fan-out list is the future filter point).
type Server struct {
	logf     dlog.Logf
	state    StateProvider
	handler  CommandHandler
	bufSize  int
	withdraw func(workspace, reason string)

	upgrader websocket.Upgrader

	mu       sync.Mutex
	clients  map[*client]struct{}
	listener net.Listener
	closed   bool
	// gate sequences WorkspaceState delivery: painters first, observers only
	// once a painter has acknowledged (see paintgate.go). It is guarded by mu
	// rather than owning a lock, so "who is connected" and "what is held" are
	// always read at the same instant.
	gate *paintGate
}

// client is one connected frontend's outbound state. send is never closed
// (avoids send-on-closed races with concurrent broadcasts); done signals the
// writer to stop and is closed exactly once by disconnect.
//
// scope, when non-nil, restricts this connection to one session/workspace (the
// per-session GET /sessions/{id}/stream view); nil is the unfiltered /frontend
// consumer that sees every workspace.
//
// role says whether this connection PAINTS the workspace states it is sent (and
// therefore acknowledges them) or merely observes them. It is fixed at accept
// and never reassigned — see Role.
type client struct {
	send      chan []byte
	done      chan struct{}
	closeOnce sync.Once
	scope     *Scope
	role      Role
	kind      ClientKind
}

// New builds a Server. It panics on a missing required dependency: a frontend
// server with no state provider or handler is a programmer error, surfaced
// loudly rather than as a nil-deref later.
func New(cfg Config) *Server {
	if cfg.Logf == nil {
		panic("frontend: Config.Logf is required")
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
	return &Server{
		logf:     cfg.Logf,
		state:    cfg.State,
		handler:  cfg.Handler,
		bufSize:  buf,
		withdraw: cfg.WithdrawPaint,
		upgrader: websocket.Upgrader{
			// Local-loopback developer tool; the webview origin is app-scoped,
			// so origin checks are permissive by design (mirrors the existing
			// daemon server upgrader).
			CheckOrigin: func(*http.Request) bool { return true },
		},
		clients: map[*client]struct{}{},
		gate:    newPaintGate(cfg.Logf),
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
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		return fmt.Errorf("frontend: mkdir socket dir: %w", err)
	}
	// Remove a stale socket; a live one would refuse to bind.
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		s.logf("frontend: remove stale socket %s: %v", path, err)
	}
	l, err := net.Listen("unix", path)
	if err != nil {
		return fmt.Errorf("frontend: listen unix %s: %w", path, err)
	}
	return s.Serve(l)
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
		// The UDS endpoint is Emacs: it renders no conversation and draws no
		// state it must attest to, so it is an observer and the sequencer holds
		// states back from it until a painter has caught up.
		go s.serveClient(newUDSConn(conn), nil, RoleObserver, ClientKindHost)
	}
}

// ServeWS upgrades an HTTP request to a WebSocket and serves it as an
// UNSCOPED frontend client (the /frontend endpoint: every workspace). Mount it
// on the daemon's HTTP mux. It blocks for the connection's lifetime (the reader
// loop runs on this goroutine).
//
// OBSERVER, deliberately. This endpoint's only production consumer is the
// webapp's bootstrap socket, which exists to create a session and close before
// any page has rendered — it draws nothing and would acknowledge nothing.
// Counting it as a painter would hold every workspace's state back from Emacs
// for the life of a socket that was never going to answer.
func (s *Server) ServeWS(w http.ResponseWriter, r *http.Request) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	s.serveClient(newWSConn(conn), nil, RoleObserver, ClientKindGUIBootstrap)
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
// role is the caller's declaration of what this connection does with the states
// it receives; it is fixed here, at accept, and the sequencer reads it for the
// life of the connection. The daemon's route table names it, so the whole
// painter/observer partition is one readable place rather than an inference
// from transport or scope.
func (s *Server) ServeWSScoped(w http.ResponseWriter, r *http.Request, scope Scope, role Role, translate CommandTranslator) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	wc := newWSConn(conn)
	wc.translate = translate
	wc.logf = s.logf
	kind := ClientKindGUIObserver
	if role == RolePainter {
		kind = ClientKindGUIPainter
	}
	s.serveClient(wc, &scope, role, kind)
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
// A WorkspaceState frame is NOT broadcast: it is routed through the delivery
// sequencer instead, here rather than only at the convenience helper, so the
// gate cannot be bypassed by reaching for the general fan-out.
func (s *Server) Broadcast(frame *frontendv1.FrontendFrame) {
	if ws := frame.GetWorkspaceState(); ws != nil {
		s.PushWorkspaceState(ws)
		return
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
	for _, cl := range clients {
		if isHostOnlyFrame(frame) && !cl.kind.isHost() {
			continue
		}
		var (
			data []byte
			err  error
		)
		if cl.scope == nil {
			data, err = marshalUnscoped()
		} else {
			out, keep := scopeFrame(frame, *cl.scope)
			if !keep {
				continue
			}
			data, err = marshalFrame(out)
		}
		if err != nil {
			s.logf("frontend: marshal frame for broadcast: %v", err)
			continue
		}
		s.enqueue(cl, data)
	}
}

// PushWorkspaceState delivers a resolved state in sequence: to every painting
// frontend now, and to the observers only once a painter has acknowledged the
// emission (or once there is no painter left to wait for).
//
// The decision and the delivery both happen under mu, so an emission, an
// acknowledgment, a connect and a disconnect are serialized against each other
// and the observers' queues carry states in exactly the order the sequencer
// settled them.
func (s *Server) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	s.mu.Lock()
	painting := s.paintersForLocked(w.GetWorkspace(), w.GetSessionId())
	stamped, settled := s.gate.emitLocked(w, painting)
	frame := WorkspaceStateFrame(stamped)
	slow := s.deliverLocked(frame, func(cl *client) bool {
		return settled || cl.role == RolePainter
	})
	s.mu.Unlock()
	s.disconnectAll(slow)
}

// settlePaint releases a workspace's held state to the observers on a paint
// acknowledgment covering generation. A stale acknowledgment settles nothing.
func (s *Server) settlePaint(workspace string, generation uint64) {
	s.mu.Lock()
	var slow []*client
	if st := s.gate.settleLocked(workspace, generation); st != nil {
		slow = s.deliverLocked(WorkspaceStateFrame(st), func(cl *client) bool {
			return cl.role == RoleObserver
		})
	}
	s.mu.Unlock()
	s.disconnectAll(slow)
}

// paintersForLocked reports whether any connected client will paint this
// workspace's state and acknowledge it. An unscoped painter answers for every
// workspace; a scoped one only for the session/workspace it is bound to, which
// is what makes "no GUI is mounted for THIS workspace" a fact rather than an
// approximation. Caller holds mu.
func (s *Server) paintersForLocked(workspace, sessionID string) bool {
	for cl := range s.clients {
		if cl.role != RolePainter {
			continue
		}
		if cl.scope == nil || cl.scope.matches(sessionID, workspace) {
			return true
		}
	}
	return false
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
			data, err = marshalFrame(out)
		}
		if err != nil {
			s.logf("frontend: marshal frame for delivery: %v", err)
			continue
		}
		if !enqueueLocked(cl, data) {
			s.logf("frontend: slow consumer (%s), outbound buffer full (%d frames), hard-disconnecting; reconnect replays snapshot",
				cl.role, cap(cl.send))
			slow = append(slow, cl)
		}
	}
	return slow
}

// enqueueLocked offers data to a client's bounded buffer without blocking. It
// reports false ONLY for a live client whose buffer is full; an already
// disconnected client is not a slow one and needs no second teardown.
func enqueueLocked(cl *client, data []byte) bool {
	select {
	case <-cl.done:
		return true
	default:
	}
	select {
	case cl.send <- data:
		return true
	default:
		return false
	}
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
func (s *Server) PushWorkspaceAvailable(v *frontendv1.WorkspaceAvailable) {
	s.Broadcast(WorkspaceAvailableFrame(v))
}
func (s *Server) PushHostAction(v *frontendv1.HostAction) { s.Broadcast(HostActionFrame(v)) }

// isHostOnlyFrame marks daemon-to-host work that must never cross into either
// GUI transport.  ClientKind, not RoleObserver, is the authority: GUI
// bootstrap is also an observer and must not receive an Emacs action.
func isHostOnlyFrame(frame *frontendv1.FrontendFrame) bool {
	if frame == nil {
		return false
	}
	return frame.GetWorkspaceAvailable() != nil || frame.GetHostAction() != nil
}

// snapshotForClient applies the two independent views of a snapshot.  Scope
// limits a GUI painter to its session; host-only durable work is then removed
// from every non-host client regardless of whether it happens to be an
// observer.  The clone prevents a reconnect's filtering from mutating the
// state provider's retained snapshot.
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
// a GUI bootstrap or painter submit one would make an unrelated webview able
// to release prompts or consume host work.
func isHostOnlyCommand(cmd *frontendv1.FrontendCommand) bool {
	if cmd == nil {
		return false
	}
	return cmd.GetCreateWorkspace() != nil || cmd.GetWorkspaceMaterialized() != nil || cmd.GetHostActionCompleted() != nil
}

func (s *Server) dispatchClientCommand(cl *client, cmd *frontendv1.FrontendCommand) *frontendv1.CommandAck {
	if isHostOnlyCommand(cmd) && !cl.kind.isHost() {
		err := fmt.Errorf("frontend: host-only command rejected from client kind %s", cl.kind)
		s.logf("frontend: host-only command rejected kind=%s request_id=%s", cl.kind, cmd.GetRequestId())
		return failAck(s.logf, cmd.GetRequestId(), err)
	}
	return dispatch(context.Background(), s.logf, s.handler, cmd)
}

// ---------------------------------------------------------------------------
// Per-connection lifecycle
// ---------------------------------------------------------------------------

func (s *Server) serveClient(c conn, scope *Scope, role Role, kind ClientKind) {
	cl := &client{
		send:  make(chan []byte, s.bufSize),
		done:  make(chan struct{}),
		scope: scope,
		role:  role,
		kind:  kind,
	}

	// Register the client and enqueue its StateSnapshot atomically, under the
	// same lock Broadcast takes. This guarantees snapshot-then-deltas ordering:
	// no broadcast can slip a delta ahead of the snapshot, because the snapshot
	// is already first in this client's FIFO buffer before it joins the set.
	s.mu.Lock()
	if s.closed {
		s.mu.Unlock()
		_ = c.close()
		return
	}
	// The sequencer's view first, so an observer's reconnect cannot hand it a
	// state no painter has settled, then the connection's own scope filter.
	snapshot := snapshotForClient(s.gate.snapshotLocked(s.state.Snapshot(), role), scope, kind)
	snap, err := marshalFrame(SnapshotFrame(snapshot))
	if err != nil {
		s.mu.Unlock()
		s.logf("frontend: marshal connect snapshot: %v", err)
		_ = c.close()
		return
	}
	cl.send <- snap // buffer is empty here; non-blocking
	s.clients[cl] = struct{}{}
	painted := snapshotWorkspaces(snapshot)
	s.mu.Unlock()

	// A NEW painter has attested nothing. Retract whatever the previous
	// renderer claimed, so this connection has to earn green by drawing the
	// history it was just handed rather than inheriting a claim made by a
	// webview that is no longer on screen. Outside the lock: the withdrawal
	// resolves a state that re-enters through PushWorkspaceState.
	if role == RolePainter {
		s.withdrawPaint(painted, "painter_connect")
	}

	go s.writeLoop(c, cl)
	s.readLoop(c, cl)
}

// snapshotWorkspaces names every workspace a snapshot carries state for.
func snapshotWorkspaces(snap *frontendv1.StateSnapshot) []string {
	out := make([]string, 0, len(snap.GetWorkspaces()))
	for _, w := range snap.GetWorkspaces() {
		if name := w.GetWorkspace(); name != "" {
			out = append(out, name)
		}
	}
	return out
}

// withdrawPaint retracts the paint attestation of each workspace, if a
// withdrawal sink is wired. Must be called with mu RELEASED.
func (s *Server) withdrawPaint(workspaces []string, reason string) {
	if s.withdraw == nil || len(workspaces) == 0 {
		return
	}
	for _, ws := range workspaces {
		s.withdraw(ws, reason)
	}
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
		case data := <-cl.send:
			if err := c.writeFrame(data); err != nil {
				s.logf("frontend: write failed, disconnecting: %v", err)
				s.disconnect(cl)
				return
			}
		}
	}
}

func (s *Server) readLoop(c conn, cl *client) {
	defer s.disconnect(cl)
	for {
		cmd, err := c.readCommand()
		if err != nil {
			if !errors.Is(err, io.EOF) && !errors.Is(err, net.ErrClosed) && !isWSClose(err) {
				s.logf("frontend: read command: %v", err)
			}
			return
		}
		// A resync re-sends a fresh StateSnapshot to THIS client (§5.4),
		// scope-filtered for a scoped connection. The handler covers the
		// conversation-delta replay the snapshot omits.
		if cmd.GetResync() != nil {
			s.mu.Lock()
			snapshot := s.gate.snapshotLocked(s.state.Snapshot(), cl.role)
			s.mu.Unlock()
			snapshot = snapshotForClient(snapshot, cl.scope, cl.kind)
			if snap, err := marshalFrame(SnapshotFrame(snapshot)); err != nil {
				s.logf("frontend: marshal resync snapshot: %v", err)
			} else {
				s.enqueue(cl, snap)
			}
		}
		// The delivery half of a paint acknowledgment, taken BEFORE dispatch:
		// the attestation the handler applies can itself resolve a NEW state,
		// and settling the acknowledged emission first is what keeps the
		// observer's queue in generation order.
		if pa := cmd.GetPaintAck(); pa != nil && settlesDelivery(pa) {
			s.settlePaint(cmd.GetWorkspace(), pa.GetStateGeneration())
		}
		ack := s.dispatchClientCommand(cl, cmd)
		if !ack.GetOk() {
			s.logf("frontend: command nack {request_id=%s ws=%s}: %s", ack.GetRequestId(), cmd.GetWorkspace(), ack.GetError())
		}
		if data, err := marshalFrame(CommandAckFrame(ack)); err != nil {
			s.logf("frontend: marshal command ack: %v", err)
		} else {
			s.enqueue(cl, data)
		}
	}
}

// enqueue delivers data to a client's bounded buffer. A full buffer means the
// consumer is too slow: hard-disconnect it loudly (reconnect replays a fresh
// snapshot — no data loss by construction).
func (s *Server) enqueue(cl *client, data []byte) {
	select {
	case <-cl.done:
		return // already disconnected
	default:
	}
	select {
	case cl.send <- data:
	case <-cl.done:
	default:
		s.logf("frontend: slow consumer, outbound buffer full (%d frames), hard-disconnecting; reconnect replays snapshot", cap(cl.send))
		s.disconnect(cl)
	}
}

// disconnect removes a client from the fan-out set and signals its writer to
// stop. Idempotent: safe to call from the reader, the writer, enqueue, or Close.
//
// Removing a PAINTER also settles every state that was being held for it and
// for no one else. That happens under the same lock the removal does, so a
// state emitted an instant before the disconnect is neither stranded (the
// removal sees it held) nor delivered twice (the entry is cleared before the
// delivery) — the fall back to direct observer delivery is race-free by
// construction rather than by timing.
func (s *Server) disconnect(cl *client) {
	s.mu.Lock()
	_, registered := s.clients[cl]
	delete(s.clients, cl)
	var (
		slow      []*client
		unpainted []string
	)
	if registered && cl.role == RolePainter {
		for _, st := range s.gate.orphanedLocked(s.paintersForLocked) {
			slow = append(slow, s.deliverLocked(WorkspaceStateFrame(st), func(c *client) bool {
				return c.role == RoleObserver
			})...)
		}
		unpainted = s.gate.unpaintedLocked(cl.scope, s.paintersForLocked)
	}
	s.mu.Unlock()
	cl.closeOnce.Do(func() { close(cl.done) })
	s.disconnectAll(slow)
	// The renderer that attested is gone, so its claim goes with it: the next
	// webview to attach must draw the history before the workspace can be green
	// again. Outside the lock, for the same reason the connect edge is.
	s.withdrawPaint(unpainted, "painter_disconnect")
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
	return b, nil
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
	logf      dlog.Logf
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
			if w.logf != nil {
				w.logf("frontend: scoped stream: inbound command rejected: %v", terr)
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
