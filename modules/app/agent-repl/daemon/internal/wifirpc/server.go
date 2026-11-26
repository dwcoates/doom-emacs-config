// Package wifirpc is the persistent-wifi service's Connect surface
// (persistentwifi.v1.PersistentWifi) and the daemon's client of it.
//
// The service (cmd/agent-repl-wifi) owns the persistent-wifi controller
// (internal/persistentwifi); the daemon passes the topbar's mode change
// through to it and follows the standing it publishes. The messages carried
// are agentrepl.v1's, whole, so nothing is respelled between the topbar's
// request and the controller's answer.
package wifirpc

import (
	"context"
	"errors"
	"net"
	"net/http"
	"sync"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	persistentwifiv1 "agentrepl/proto/persistentwifi/v1"
	"agentrepl/proto/persistentwifi/v1/persistentwifiv1connect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/publish"
)

// EnvSocket overrides the service socket's default path, so a test harness
// can point the daemon and the service at a private socket without editing a
// command line. The daemon's --wifi-socket beats it.
const EnvSocket = "AGENT_REPL_PERSISTENT_WIFI_SOCKET"

// errDraining refuses a watch asked of a server that is shutting down.
var errDraining = errors.New("wifirpc: the persistent-wifi service is shutting down")

// Operations every record of this package carries.
const (
	opServe  = "daemon.wifirpc.serve"
	opUpdate = "daemon.wifirpc.update"
	opWatch  = "daemon.wifirpc.watch"
)

// readHeaderTimeout bounds a client's request headers on the unix socket. The
// headers arrive inside the measured connect-to-first-standing time (worst
// 1.31ms over 300 fresh connections, 2026-11-26, under the test runner's CPU
// governor), and five times that, rounded up to the millisecond, is 7ms. The
// only caller is the local daemon, so this bound only ever ends a connection
// that is stuck.
const readHeaderTimeout = 7 * time.Millisecond

// Controller is the slice of the persistent-wifi controller the service
// serves.
type Controller interface {
	// Update runs one validated action and answers its response whole.
	Update(ctx context.Context, update persistentwifi.ValidUpdate) *agentreplv1.UpdatePersistentWifiModeResponse
	// Topic is the standing's publication.
	Topic() *publish.Topic[*agentreplv1.PersistentWifiState]
}

// Server serves persistentwifi.v1.PersistentWifi over Connect.
//
// LOGGING OWNERSHIP. The controller records every change and read where it
// happens. This layer records only a request it refuses (at ERROR) and a
// watch's end (at DEBUG), and never re-records what the controller did.
type Server struct {
	controller Controller
	log        dlog.Logger
	http       *http.Server
	// draining is closed by Shutdown, which ends every standing watch: a
	// watch outlives its request otherwise, and the drain would wait on it.
	draining     chan struct{}
	drainingOnce sync.Once
	// conns are the connections accepted and not yet closed: the h2c handler
	// hijacks each one, so the HTTP server's own drain never sees it, and
	// Shutdown closes them itself.
	connsMu sync.Mutex
	conns   map[net.Conn]struct{}
	// peers holds the pid of every process connected.
	peers *Peers
	// peerPID reads a connection's peer pid; peerPID the function, but a
	// field so a test stands in a connection the kernel cannot name.
	peerPID func(net.Conn) (int, error)
}

var _ persistentwifiv1connect.PersistentWifiHandler = (*Server)(nil)

// NewServer builds a Server over the controller. All three are required. A
// connection whose peer pid the kernel will not name is refused and
// recorded, since the service could not then protect that peer's tree from
// the application freeze.
func NewServer(controller Controller, log dlog.Logger, peers *Peers) *Server {
	if controller == nil || log == nil || peers == nil {
		panic("wifirpc: a controller, a logger and a peer set are required")
	}
	s := &Server{controller: controller, log: log, peers: peers, peerPID: peerPID,
		draining: make(chan struct{}), conns: map[net.Conn]struct{}{}}
	mux := http.NewServeMux()
	path, handler := persistentwifiv1connect.NewPersistentWifiHandler(s)
	mux.Handle(path, handler)
	s.http = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{}), ReadHeaderTimeout: readHeaderTimeout}
	return s
}

// Serve serves on ln until Shutdown; it returns http.ErrServerClosed then.
func (s *Server) Serve(ln net.Listener) error {
	return s.http.Serve(trackingListener{Listener: ln, server: s})
}

// trackingListener hands every accepted connection to its Server, which
// closes the ones still open at Shutdown.
type trackingListener struct {
	net.Listener
	server *Server
}

func (t trackingListener) Accept() (net.Conn, error) {
	for {
		conn, err := t.Listener.Accept()
		if err != nil {
			return nil, err
		}
		if tracked, ok := t.server.track(conn); ok {
			return tracked, nil
		}
	}
}

// trackedConn forgets itself, and its peer, when it closes.
type trackedConn struct {
	net.Conn
	server *Server
	pid    int
	once   *sync.Once
}

func (c trackedConn) Close() error {
	c.once.Do(func() {
		c.server.connsMu.Lock()
		delete(c.server.conns, c)
		c.server.connsMu.Unlock()
		c.server.peers.remove(c.pid)
	})
	return c.Conn.Close()
}

// track reads conn's peer pid from the kernel and registers the connection,
// answering false for one it refused: a peer whose pid cannot be read is
// closed and recorded.
func (s *Server) track(conn net.Conn) (net.Conn, bool) {
	pid, err := s.peerPID(conn)
	if err != nil {
		s.log.Error(opServe, "refused a connection whose peer the kernel will not name", dlog.Context{"cause": err.Error()})
		_ = conn.Close()
		return nil, false
	}
	tracked := trackedConn{Conn: conn, server: s, pid: pid, once: &sync.Once{}}
	s.peers.add(pid)
	s.connsMu.Lock()
	s.conns[tracked] = struct{}{}
	s.connsMu.Unlock()
	return tracked, true
}

// Shutdown ends every standing watch in flight, whose watcher watches again,
// and drains the remaining requests within ctx.
func (s *Server) Shutdown(ctx context.Context) error {
	s.drainingOnce.Do(func() { close(s.draining) })
	err := s.http.Shutdown(ctx)
	s.connsMu.Lock()
	open := make([]net.Conn, 0, len(s.conns))
	for conn := range s.conns {
		open = append(open, conn)
	}
	s.connsMu.Unlock()
	for _, conn := range open {
		_ = conn.Close()
	}
	return err
}

// UpdateMode validates the mode change through persistentwifi.ValidateUpdate,
// the one validator, and runs it on the controller. A request naming no action
// is refused as an invalid argument and recorded, whoever sent it.
func (s *Server) UpdateMode(
	ctx context.Context,
	req *connect.Request[persistentwifiv1.UpdateModeRequest],
) (*connect.Response[persistentwifiv1.UpdateModeResponse], error) {
	valid, err := persistentwifi.ValidateUpdate(req.Msg.GetRequest())
	if err != nil {
		s.log.Error(opUpdate, "refused a mode change that is not valid", dlog.Context{"cause": err.Error()})
		return nil, connect.NewError(connect.CodeInvalidArgument, err)
	}
	return connect.NewResponse(&persistentwifiv1.UpdateModeResponse{Response: s.controller.Update(ctx, valid)}), nil
}

// WatchStanding sends the current standing, then every change, until the
// caller goes away.
func (s *Server) WatchStanding(
	ctx context.Context,
	_ *connect.Request[persistentwifiv1.WatchStandingRequest],
	stream *connect.ServerStream[persistentwifiv1.WatchStandingResponse],
) error {
	// A watch asked of a draining server is refused with Unavailable, so its
	// caller reopens on the service that replaces this one rather than reading
	// an empty stream as a service that serves nothing.
	select {
	case <-s.draining:
		return connect.NewError(connect.CodeUnavailable, errDraining)
	default:
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	go func() {
		select {
		case <-s.draining:
			cancel()
		case <-ctx.Done():
		}
	}()
	for standing := range s.controller.Topic().Subscribe(ctx) {
		if err := stream.Send(&persistentwifiv1.WatchStandingResponse{Standing: standing}); err != nil {
			s.log.Debug(opWatch, "a standing watch ended: its caller is gone", dlog.Context{"cause": err.Error()})
			return err
		}
	}
	return nil
}
