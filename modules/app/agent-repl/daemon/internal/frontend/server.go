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
	// and open-task catalog. It must be safe for concurrent use; the server
	// calls it while holding its client-registry lock so the snapshot and the
	// subsequent delta stream cannot interleave.
	Snapshot() *frontendv1.StateSnapshot
}

// Config configures a Server. Logf, State, and Handler are required.
type Config struct {
	Logf    dlog.Logf
	State   StateProvider
	Handler CommandHandler
	// BufSize is the per-client outbound buffer; <=0 uses defaultClientBuffer.
	BufSize int
}

// Server serves agentshim.frontend.v1 frames as protojson over a UDS listener
// (Emacs, newline-delimited) and a WebSocket endpoint (webapp, one frame per
// message). Every connected frontend receives every broadcast frame (workspace
// entitlement is "all" for now; the fan-out list is the future filter point).
type Server struct {
	logf    dlog.Logf
	state   StateProvider
	handler CommandHandler
	bufSize int

	upgrader websocket.Upgrader

	mu       sync.Mutex
	clients  map[*client]struct{}
	listener net.Listener
	closed   bool
}

// client is one connected frontend's outbound state. send is never closed
// (avoids send-on-closed races with concurrent broadcasts); done signals the
// writer to stop and is closed exactly once by disconnect.
//
// scope, when non-nil, restricts this connection to one session/workspace (the
// per-session GET /sessions/{id}/stream view); nil is the unfiltered /frontend
// consumer that sees every workspace.
type client struct {
	send      chan []byte
	done      chan struct{}
	closeOnce sync.Once
	scope     *Scope
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
		logf:    cfg.Logf,
		state:   cfg.State,
		handler: cfg.Handler,
		bufSize: buf,
		upgrader: websocket.Upgrader{
			// Local-loopback developer tool; the webview origin is app-scoped,
			// so origin checks are permissive by design (mirrors the existing
			// daemon server upgrader).
			CheckOrigin: func(*http.Request) bool { return true },
		},
		clients: map[*client]struct{}{},
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
		go s.serveClient(newUDSConn(conn), nil)
	}
}

// ServeWS upgrades an HTTP request to a WebSocket and serves it as an
// UNSCOPED frontend client (the /frontend endpoint: every workspace). Mount it
// on the daemon's HTTP mux. It blocks for the connection's lifetime (the reader
// loop runs on this goroutine).
func (s *Server) ServeWS(w http.ResponseWriter, r *http.Request) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	s.serveClient(newWSConn(conn), nil)
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
func (s *Server) ServeWSScoped(w http.ResponseWriter, r *http.Request, scope Scope, translate CommandTranslator) {
	conn, err := s.upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.logf("frontend: websocket upgrade: %v", err)
		return
	}
	wc := newWSConn(conn)
	wc.translate = translate
	wc.logf = s.logf
	s.serveClient(wc, &scope)
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
func (s *Server) Broadcast(frame *frontendv1.FrontendFrame) {
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

// Convenience push helpers wrapping Broadcast, one per frame variant.
func (s *Server) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	s.Broadcast(WorkspaceStateFrame(w))
}
func (s *Server) PushSessionView(v *frontendv1.SessionView) { s.Broadcast(SessionViewFrame(v)) }
func (s *Server) PushConversationDelta(c *frontendv1.ConversationDelta) {
	s.Broadcast(ConversationDeltaFrame(c))
}
func (s *Server) PushTypingDelta(t *frontendv1.TypingDelta) { s.Broadcast(TypingDeltaFrame(t)) }
func (s *Server) PushTaskCatalog(c *frontendv1.TaskCatalog) { s.Broadcast(TaskCatalogFrame(c)) }
func (s *Server) PushDegradedNotice(n *frontendv1.DegradedNotice) {
	s.Broadcast(DegradedNoticeFrame(n))
}
func (s *Server) PushSessionInitView(v *frontendv1.SessionInitView) {
	s.Broadcast(SessionInitViewFrame(v))
}
func (s *Server) PushHeartbeatView(h *frontendv1.HeartbeatView) {
	s.Broadcast(HeartbeatViewFrame(h))
}

// ---------------------------------------------------------------------------
// Per-connection lifecycle
// ---------------------------------------------------------------------------

func (s *Server) serveClient(c conn, scope *Scope) {
	cl := &client{
		send:  make(chan []byte, s.bufSize),
		done:  make(chan struct{}),
		scope: scope,
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
	snapshot := s.state.Snapshot()
	if scope != nil {
		snapshot = filterSnapshot(snapshot, *scope)
	}
	snap, err := marshalFrame(SnapshotFrame(snapshot))
	if err != nil {
		s.mu.Unlock()
		s.logf("frontend: marshal connect snapshot: %v", err)
		_ = c.close()
		return
	}
	cl.send <- snap // buffer is empty here; non-blocking
	s.clients[cl] = struct{}{}
	s.mu.Unlock()

	go s.writeLoop(c, cl)
	s.readLoop(c, cl)
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
			snapshot := s.state.Snapshot()
			if cl.scope != nil {
				snapshot = filterSnapshot(snapshot, *cl.scope)
			}
			if snap, err := marshalFrame(SnapshotFrame(snapshot)); err != nil {
				s.logf("frontend: marshal resync snapshot: %v", err)
			} else {
				s.enqueue(cl, snap)
			}
		}
		ack := Dispatch(context.Background(), s.handler, cmd)
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
func (s *Server) disconnect(cl *client) {
	s.mu.Lock()
	delete(s.clients, cl)
	s.mu.Unlock()
	cl.closeOnce.Do(func() { close(cl.done) })
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
