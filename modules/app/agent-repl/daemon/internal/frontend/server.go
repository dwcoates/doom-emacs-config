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
	"sort"
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

// hostBufferElasticity multiplies the HOST connection's soft bound to get its
// absolute ceiling, and hostStallGrace is how long that connection's queue may
// sit above the soft bound without a single frame being drained before it is
// treated as wedged.
//
// The host is not just another consumer. There is exactly one, it owns the UI,
// and evicting it costs a visible link-down, a reconnect, and a full snapshot
// replay. It is also SINGLE-THREADED: Emacs legitimately blocks its event loop
// for seconds at a time restoring seventeen workspaces at startup or mounting a
// webview, and every one of those blocks used to fill the flat 256-frame buffer
// with frames nothing could supersede and sever the link. Transient busyness is
// inherent to that frontend, not a defect in it, so the host's queue absorbs it
// instead of the connection paying for it.
//
// The two bounds are still hard limits, not a suggestion: 256*16 frames caps
// the memory a wedged host can pin, and a host that drains NOTHING for the
// grace period is cut loose exactly as before. Load shedding is preserved; only
// the threshold for calling a busy consumer a dead one moved.
const (
	hostBufferElasticity = 16
	hostStallGrace       = 30 * time.Second
)

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
	// AckDeadline is the point at which a command still in flight is announced
	// as overdue; <=0 uses CommandAckDeadline, the budget the Emacs client
	// itself enforces. It is a knob only so tests can reach the branch without
	// waiting out the production deadline.
	AckDeadline time.Duration
	// PaceStallGrace is how long a producer waits on a connection that has made
	// NO observable drain progress before hard-disconnecting it (pacing.go);
	// <=0 uses paceStallGrace. It exists so a test can exercise the stall
	// verdict without waiting out the production grace period; production never
	// sets it.
	PaceStallGrace time.Duration
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
	// ackDeadline is when a command still in flight is announced as overdue.
	// See Config.AckDeadline.
	ackDeadline time.Duration
	// paceGrace is how long a producer waits on a connection making NO
	// observable drain progress before giving up on it (pacing.go). Always
	// positive after New.
	paceGrace time.Duration
	// inflight is the daemon-wide count of frontend commands currently between
	// receipt and ack. It is the QUEUE DEPTH every latency record carries: a
	// per-connection read loop dispatches serially, so a command that waited
	// behind other work can only see that work by counting it.
	//
	// Every increment is paired with a commandTicket whose deferred settle
	// decrements it, so the gauge cannot leak past a panicking dispatch —
	// see ticket.go.
	inflight atomic.Int64

	// clientLogRefusals rate-limits the REPORTING of refused client_log
	// telemetry writes (clientlogrefusal.go). It never touches the refusal
	// itself. Never nil after New.
	clientLogRefusals *clientLogRefusalLimiter

	upgrader websocket.Upgrader

	mu sync.Mutex
	// publicationMu serializes the host materialization release against every
	// session-scoped live enqueue. Readers hold it from durable gate verdict
	// through enqueue; release owns the writer side only to FLIP the hold below
	// and to flush what the hold parked.
	//
	// EXACTLY ONE THING OUTSIDE THIS PACKAGE IS CALLED UNDER IT: the durable
	// publication gate, sessionPublicationAllowed. It has to be — the verdict
	// and the delivery it authorizes must be atomic against the hold below, or
	// a frame could pass the gate and then be delivered after the release's
	// snapshot. THE GATE THEREFORE MUST NEVER WAIT ON ANYTHING THAT WAITS ON
	// THIS SERVER; it is a memoized durable read, and its owner keeps the
	// release hand-off off the lock the gate takes for that reason.
	//
	// NOTHING ELSE IS. Release used to hold the writer side across its injected
	// open and snapshot functions, and the snapshot provider reaches into the
	// SSM and the workspace-view publisher — which push back through Broadcast,
	// which wants the reader side. That is a lock cycle, and it froze every
	// session controller and the merge queue drain twice in production. See
	// ReleaseSessionPublication.
	publicationMu sync.RWMutex
	// publicationHeld is engaged for the duration of a materialization release.
	// Guarded by publicationMu: engaging and clearing it take the writer side,
	// so a reader either delivers entirely before the release began or observes
	// the hold. A session-scoped frame that observes it is PARKED rather than
	// dropped — parking is what blocking on the writer used to buy, minus the
	// wait.
	publicationHeld bool
	// heldMu guards heldFrames alone. It is separate from publicationMu because
	// a parker holds the READER side (many at once) and still has to append.
	heldMu sync.Mutex
	// heldFrames is the release window's outbox, in arrival order. Flushed
	// under the writer side, so every parked frame is re-offered ahead of any
	// frame that arrives after the window closed.
	heldFrames []*frontendv1.FrontendFrame
	// releaseMu admits one materialization release at a time. Held across the
	// whole release INCLUDING the injected open/snapshot calls, which is safe
	// precisely because nothing but ReleaseSessionPublication ever takes it.
	releaseMu    sync.Mutex
	clients      map[*client]struct{}
	listener     net.Listener
	closed       bool
	nextClientID uint64
	// latestWorkspaceAt is the newest WorkspaceState revision that crossed the
	// delivery lock. Snapshot paths use it to detect a concurrent publication.
	latestWorkspaceAt map[string]int64
	// latestWorkspaceState is the frame that carried that revision, retained so
	// a connect whose snapshot was captured before it can be REPAIRED with the
	// exact frame every other client already holds, instead of re-composing the
	// snapshot until no publication races it. Retention is bounded by the
	// workspace count, which latestWorkspaceAt already carries.
	latestWorkspaceState map[string]*frontendv1.WorkspaceState
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
	// cause is WHY this connection is being torn down, recorded by disconnect
	// BEFORE done is closed so the writer's teardown — which is what actually
	// closes the socket — can name it in the log and put it on the wire as a
	// WebSocket close frame. A nil load means nothing recorded a cause, which
	// closeConn reports as the bug it is. See closecause.go.
	cause atomic.Pointer[closeCause]
	// drain is this connection's connect-snapshot drain window (bootdrain.go):
	// the interval its initial StateSnapshot spent between the outbound queue
	// and the socket. A slow ack delivered inside it is explained by the
	// bring-up rather than by the command path.
	drain snapshotDrain
}

// newClient builds a client with a bounded outbox of the given size. The HOST
// gets the elastic queue described on hostBufferElasticity; every other kind
// gets the flat bound it always had.
func newClient(bufSize int, scope *Scope, kind ClientKind) *client {
	return &client{
		out:   newClientOutbox(bufSize, kind),
		done:  make(chan struct{}),
		scope: scope,
		kind:  kind,
	}
}

// newClientOutbox selects a connection's queue policy from its kind. The
// policy is fixed at accept alongside the kind itself, so no connection can
// acquire or lose the host's headroom while it is running.
func newClientOutbox(bufSize int, kind ClientKind) *outbox {
	if kind.isHost() {
		return newElasticOutbox(bufSize, bufSize*hostBufferElasticity, hostStallGrace)
	}
	return newOutbox(bufSize)
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
	ackDeadline := cfg.AckDeadline
	if ackDeadline <= 0 {
		ackDeadline = CommandAckDeadline
	}
	paceGrace := cfg.PaceStallGrace
	if paceGrace <= 0 {
		paceGrace = paceStallGrace
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
		ackDeadline:               ackDeadline,
		paceGrace:                 paceGrace,
		upgrader: websocket.Upgrader{
			// Local-loopback developer tool; the webview origin is app-scoped,
			// so origin checks are permissive by design (mirrors the existing
			// daemon server upgrader).
			CheckOrigin: func(*http.Request) bool { return true },
		},
		clients:           map[*client]struct{}{},
		latestWorkspaceAt: map[string]int64{},

		latestWorkspaceState: map[string]*frontendv1.WorkspaceState{},
		clientLogRefusals: newClientLogRefusalLimiter(time.Now, clientLogRefusalSummaryInterval),
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
		s.disconnect(cl, causeServerShutdown)
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
	// PACE FIRST, before the publication gate's reader side is taken and before
	// any delivery lock: a wait that held either would stall a materialization
	// release, or every other connection, on one slow browser (pacing.go).
	s.paceBulkFrame(frame)
	_, _, scoped := frameSessionIdentity(frame)
	if scoped {
		s.publicationMu.RLock()
		if s.publicationHeld {
			s.parkPublication(frame)
			s.publicationMu.RUnlock()
			// A PARKED FRAME HAS BEEN DELIVERED NOWHERE YET, so it reports
			// zero. Only an UNSCOPED frame's count is load-bearing — the
			// host-only helpers below read it — and an unscoped frame never
			// parks, so no caller can mistake a park for a lost frame.
			return 0
		}
		defer s.publicationMu.RUnlock()
	}
	return s.broadcastGated(frame)
}

// broadcastGated runs the durable gate verdict and the fan-out. The caller owns
// the publication serialization: Broadcast holds the reader side, the release
// flush holds the writer side.
func (s *Server) broadcastGated(frame *frontendv1.FrontendFrame) int {
	return s.broadcastGatedTo(frame, nil)
}

// broadcastGatedTo is broadcastGated narrowed to an AUDIENCE. A nil audience
// means every client, which is the ordinary fan-out; a non-nil one admits only
// the kinds it accepts.
//
// The audience is a delivery filter layered ON TOP of the existing host-only
// and scope rules, never a way around them: a frame barred from a GUI client
// by isHostOnlyFrame stays barred no matter what audience a caller names. It
// exists so a caller that must report delivery PER CARRIER (the intentional-
// restart announcement, which tells the shutdown log which clients were told
// and which were not) can count each kind separately instead of receiving one
// undifferentiated total.
func (s *Server) broadcastGatedTo(frame *frontendv1.FrontendFrame, audience func(ClientKind) bool) int {
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
		if audience != nil && !audience(cl.kind) {
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
	if s.publicationHeld {
		s.parkPublication(WorkspaceStateFrame(w))
		s.publicationMu.RUnlock()
		return
	}
	defer s.publicationMu.RUnlock()
	s.pushWorkspaceStateGated(w)
}

// pushWorkspaceStateGated is PushWorkspaceState's body once publication
// serialization is owned by the caller — see broadcastGated.
func (s *Server) pushWorkspaceStateGated(w *frontendv1.WorkspaceState) {
	if !s.requireSessionPublication(WorkspaceStateFrame(w)) {
		return
	}
	s.mu.Lock()
	if at := w.GetAtMs(); at > s.latestWorkspaceAt[w.GetWorkspace()] {
		s.latestWorkspaceAt[w.GetWorkspace()] = at
		s.latestWorkspaceState[w.GetWorkspace()] = w
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
	// The FENCED pushes carry no session id. They are still SCOPED — the
	// materialization latch is a per-WORKSPACE decision and must hold them back
	// exactly as before — so they report their workspace and an empty session,
	// which the gate already accepts and logs as such.
	case *frontendv1.FrontendFrame_ConversationDelta:
		return f.ConversationDelta.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_TypingDelta:
		return f.TypingDelta.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_AsyncBubbleDelta:
		return f.AsyncBubbleDelta.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_TaskCatalog:
		return f.TaskCatalog.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_SessionInit:
		return f.SessionInit.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_Heartbeat:
		return f.Heartbeat.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_Queue:
		return f.Queue.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_Progress:
		return f.Progress.GetWorkspace(), "", true
	// THE THREE RESOLVED VIEWS are fenced-family frames like the ones above and
	// are latched for the same reason. They were missing here while the SNAPSHOT
	// side latched them (the snapshot provider runs all three through
	// filterPublishedWorkspaceViews), so a workspace the latch was holding back
	// had its topbar, its breakdown menu and its gate PUSHED to a client the
	// snapshot would then refuse to tell about that workspace at all. Push and
	// snapshot must answer the same question the same way.
	case *frontendv1.FrontendFrame_Topbar:
		return f.Topbar.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_TokenBreakdown:
		return f.TokenBreakdown.GetWorkspace(), "", true
	case *frontendv1.FrontendFrame_WorkspaceGate:
		return f.WorkspaceGate.GetWorkspace(), "", true
	default:
		return "", "", false
	}
}

// deliverLocked enqueues frame into every client want selects, returning the
// clients whose bounded buffer overflowed so the caller can disconnect them
// after releasing mu (disconnect takes mu itself). Caller holds mu.
func (s *Server) deliverLocked(frame *frontendv1.FrontendFrame, want func(*client) bool) []deadClient {
	var (
		unscoped     []byte
		unscopedErr  error
		unscopedDone bool
		slow         []deadClient
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
		res := enqueueLocked(cl, outFrame{key: coalesceKey(sent), data: data})
		if res.closed {
			// Already disconnected: not a slow consumer, and there is nothing
			// left to tear down a second time.
			continue
		}
		if !res.queued {
			s.recordOverflow(cl, res, "broadcast")
			slow = append(slow, deadClient{cl: cl, cause: causeOverflow(res, "broadcast")})
			continue
		}
		s.notePressure(cl, res, "broadcast")
	}
	return slow
}

// enqueueLocked offers a frame to a client's bounded outbox without blocking,
// compacting superseded frames first if the queue is already full. It reports
// queued=false ONLY for a live client whose queue is past its ceiling, or which
// has drained nothing for the whole grace period, and in both cases still full
// of frames nothing may replace; an already disconnected client is not a slow
// one and needs no second teardown. The result carries how many frames
// compaction removed and the queue's bounds, so a refusal can say what was
// tried.
func enqueueLocked(cl *client, f outFrame) pushResult {
	select {
	case <-cl.done:
		notifyFrame(f, errClientGone)
		return pushResult{closed: true, soft: cl.out.capacity(), hard: cl.out.ceiling()}
	default:
	}
	res := cl.out.push(f)
	if res.closed {
		// The teardown won the race with this push. Same disposition as the
		// branch above; the queue's own closed flag is what makes it a race
		// nothing can slip through rather than one the check above can miss.
		notifyFrame(f, errClientGone)
	}
	return res
}

// errClientGone is the disposition of a frame that will never be written
// because its connection is already gone.
var errClientGone = errors.New("frontend: connection closed before the frame was written")

// deadClient pairs a client being given up on with the CAUSE it is being given
// up on for. The two travel together because the refusal that condemned a
// connection happens under the delivery lock and the teardown happens after it
// is released: carrying only the client to the teardown is exactly how a
// per-connection reason gets replaced by a generic one.
type deadClient struct {
	cl    *client
	cause closeCause
}

// disconnectAll tears down every client in the list, each for its own recorded
// cause. Called after mu is released, since disconnect takes it.
func (s *Server) disconnectAll(dead []deadClient) {
	for _, d := range dead {
		s.disconnect(d.cl, d.cause)
	}
}

// Convenience push helpers wrapping Broadcast, one per frame variant.
func (s *Server) PushSessionView(v *frontendv1.SessionView) { s.Broadcast(SessionViewFrame(v)) }
func (s *Server) PushConversationDelta(c *frontendv1.ConversationDelta) {
	s.Broadcast(ConversationDeltaFrame(c))
}
func (s *Server) PushAsyncBubbleDelta(d *frontendv1.AsyncBubbleDelta) {
	s.Broadcast(AsyncBubbleDeltaFrame(d))
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
func (s *Server) PushTopbarView(v *frontendv1.TopbarView) { s.Broadcast(TopbarViewFrame(v)) }
func (s *Server) PushTokenBreakdownView(v *frontendv1.TokenBreakdownView) {
	s.Broadcast(TokenBreakdownViewFrame(v))
}
func (s *Server) PushWorkspaceGateView(v *frontendv1.WorkspaceGateView) {
	s.Broadcast(WorkspaceGateViewFrame(v))
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
	slow := make([]deadClient, 0)
	for cl := range s.clients {
		view := snapshotForClient(snapshot, cl.scope, cl.kind)
		data, err := marshalFrame(SnapshotFrame(view))
		if err != nil {
			s.logf("frontend: marshal authoritative snapshot client_id=%d kind=%s: %v", cl.id, cl.kind, err)
			continue
		}
		if res := enqueueLocked(cl, outFrame{data: data}); !res.queued {
			s.overflowEmit(cl)("frontend: authoritative snapshot queue saturated client_id=%d kind=%s scope_workspace=%q scope_session=%q depth=%d soft=%d hard=%d reason=%s stalled_for_ms=%d outcome=disconnect",
				cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), res.depth, res.soft, res.hard, res.reason, res.stalledFor.Milliseconds())
			slow = append(slow, deadClient{cl: cl, cause: causeOverflow(res, "authoritative_snapshot")})
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
//
// THE INJECTED FUNCTIONS RUN WITH NO SERVER LOCK HELD, and that is the whole
// shape of this method. open writes durable creation state; snapshot reaches
// through the SSM, the session registry and the workspace-view publisher, each
// of which takes its own mutex and pushes back into this server while holding
// it. Calling either under publicationMu therefore closes a cycle — this
// server's writer waiting on the SSM's mutex, the SSM's publisher waiting on
// this server's reader — and that cycle wedged every session controller and the
// merge queue drain twice in production.
//
// The ordering the method exists for is kept by a HOLD instead of by a lock
// hold. Engaging the hold parks every session-scoped frame; open and snapshot
// then run unlocked; the authoritative snapshot goes out; and only then are the
// parked frames re-offered — each re-running the durable gate, exactly as a
// frame that used to block on the writer re-ran it on waking. So no session
// frame overtakes the snapshot, and none is lost to the window either.
//
// A FAILED open STILL FLUSHES. The parked frames were held on this release's
// account, and abandoning them because the release failed would strand live
// state behind a gate that is still closed and will never re-offer them.
func (s *Server) ReleaseSessionPublication(open func() error, snapshot func() *frontendv1.StateSnapshot) error {
	if open == nil || snapshot == nil {
		return fmt.Errorf("frontend: materialization release requires open and snapshot functions")
	}
	// One release at a time: two concurrent windows would interleave their
	// parks and flushes, and the second flush would re-offer the first's frames
	// ahead of the second's snapshot.
	s.releaseMu.Lock()
	defer s.releaseMu.Unlock()

	s.holdPublications()
	defer s.flushHeldPublications()

	if err := open(); err != nil {
		return fmt.Errorf("frontend: materialization release open: %w", err)
	}
	s.PushAuthoritativeSnapshot(snapshot())
	return nil
}

// holdPublications engages the release window. The writer side is what makes
// the flip a barrier: every reader already inside a delivery finishes first,
// and every reader after it observes the hold.
func (s *Server) holdPublications() {
	s.publicationMu.Lock()
	defer s.publicationMu.Unlock()
	s.publicationHeld = true
}

// parkPublication appends one frame to the release window's outbox. The caller
// holds the READER side of publicationMu and has observed the hold, so the
// flusher — which needs the writer side — cannot be draining concurrently.
func (s *Server) parkPublication(frame *frontendv1.FrontendFrame) {
	s.heldMu.Lock()
	s.heldFrames = append(s.heldFrames, frame)
	s.heldMu.Unlock()
}

// flushHeldPublications closes the release window and re-offers every parked
// frame in arrival order, under the writer side — which is what puts all of
// them ahead of any frame that arrives after the window.
//
// Each frame runs the durable gate again rather than being delivered on the
// strength of having been parked: the gate is the authority on whether that
// session may publish, and this method's job is to have not skipped it.
func (s *Server) flushHeldPublications() {
	s.publicationMu.Lock()
	defer s.publicationMu.Unlock()
	s.publicationHeld = false
	s.heldMu.Lock()
	held := s.heldFrames
	s.heldFrames = nil
	s.heldMu.Unlock()
	if len(held) > 0 {
		s.logf("frontend: materialization release flushing held frames count=%d", len(held))
	}
	for _, frame := range held {
		if ws := frame.GetWorkspaceState(); ws != nil {
			s.pushWorkspaceStateGated(ws)
			continue
		}
		s.broadcastGated(frame)
	}
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
	// A workspace field that names nothing addressable is refused with a loud
	// ack rather than routed: the read loop already canonicalized it, so this
	// is a key the client sent that no lookup could ever resolve.
	if err := workspaceKeyError(cmd.GetWorkspace()); err != nil {
		s.warn("frontend: workspace key rejected kind=%s request_id=%s: %v", cl.kind, cmd.GetRequestId(), err)
		return failAck(s.logf, cmd.GetRequestId(), err), nil
	}
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
	//
	// THE CONNECT COMPOSES EXACTLY ONE SNAPSHOT. It used to re-compose the whole
	// snapshot and try again whenever a WorkspaceState had crossed the delivery
	// lock since the capture, which is a race against the daemon's own state and
	// therefore unbounded by construction: at 178 workspaces / 289 sessions one
	// composition costs seconds, and the deferred boot sweep mutates state for
	// ~23s after boot, so the Emacs host waited ~30s for the connect snapshot
	// that gates every downstream recovery signal. Staleness is now REPAIRED
	// rather than raced: the snapshot is served as of its capture instant and any
	// workspace the transport has published a newer state for rides immediately
	// behind it as an ordinary WorkspaceState frame, in the same delivery-lock
	// operation. The client's per-workspace end state is identical to the
	// retrying version's — WorkspaceState is a per-workspace frame the frontend
	// applies per workspace — and no view is torn, because every catch-up frame
	// is the exact frame already broadcast to every other client.
	var (
		snapshot    *frontendv1.StateSnapshot
		rawSnapshot *frontendv1.StateSnapshot
	)
	{
		// Never call StateProvider while holding the frontend lock. Synchronous
		// SSM publication takes the locks in the opposite order. The catch-up
		// pass under s.mu closes the capture race this lock order creates.
		rawSnapshot = s.state.Snapshot()
		snapshot = snapshotForClient(rawSnapshot, scope, kind)
		snap, err := marshalFrame(SnapshotFrame(snapshot))
		if err != nil {
			s.warn("frontend: marshal connect snapshot kind=%s scope_workspace=%q scope_session=%q: %v",
				kind, scopeWorkspace(scope), scopeSession(scope), err)
			s.closeConn(c, cl.id, kind, causeInternal(closeReasonSnapshotMarshal, err))
			return
		}
		s.mu.Lock()
		if s.closed {
			s.mu.Unlock()
			// This used to be the one close on the serving paths with no record
			// at all: a connection accepted into a closing server vanished
			// silently from both ends.
			s.closeConn(c, cl.id, kind, causeServerClosed)
			return
		}
		catchUp, catchUpErr := s.snapshotCatchUpLocked(snapshot, scope)
		if catchUpErr != nil {
			s.mu.Unlock()
			s.warn("frontend: marshal connect catch-up kind=%s scope_workspace=%q scope_session=%q: %v",
				kind, scopeWorkspace(scope), scopeSession(scope), catchUpErr)
			s.closeConn(c, cl.id, kind, causeInternal(closeReasonSnapshotMarshal, catchUpErr))
			return
		}
		// Registration and enqueue remain one delivery-lock operation, so no
		// delta can slip ahead of this first FIFO frame after validation.
		s.nextClientID++
		cl.id = s.nextClientID
		// THE BOOT-DRAIN WINDOW OPENS HERE and closes when the writer first
		// finds this outbox empty (bootdrain.go). Opening before the push is
		// what keeps the window from ever being closed before it was open: the
		// writer starts below and may drain this frame the instant it is
		// queued.
		cl.drain.open(time.Now())
		// A fresh outbox is empty, so this first FIFO frame always fits; a
		// refusal here would be a programmer error, not a slow consumer.
		if res := enqueueLocked(cl, outFrame{data: snap}); !res.queued {
			// The window opened and no writer will ever start on this
			// connection to close it, so close it here rather than leave a
			// window open on a connection that is being abandoned.
			cl.drain.closeAt(time.Now())
			s.mu.Unlock()
			s.warn("frontend: connect snapshot rejected by an empty outbox kind=%s scope_workspace=%q scope_session=%q",
				kind, scopeWorkspace(scope), scopeSession(scope))
			s.closeConn(c, cl.id, kind, causeInternal(closeReasonSnapshotRefused, nil))
			return
		}
		// THE CATCH-UP FRAMES RIDE THE SAME delivery-lock operation, immediately
		// behind the snapshot and ahead of registration, so this client sees
		// them in FIFO order before any broadcast that follows.
		for _, entry := range catchUp {
			if res := enqueueLocked(cl, outFrame{key: entry.key, data: entry.data}); !res.queued {
				cl.drain.closeAt(time.Now())
				s.mu.Unlock()
				s.warn("frontend: connect catch-up rejected by a near-empty outbox kind=%s scope_workspace=%q workspace=%q",
					kind, scopeWorkspace(scope), entry.workspace)
				s.closeConn(c, cl.id, kind, causeInternal(closeReasonSnapshotRefused, nil))
				return
			}
			s.logVerbosef("frontend: connect snapshot catch-up kind=%s scope_workspace=%q scope_session=%q catch_up_workspace=%q snapshot_at_ms=%d delivered_at_ms=%d",
				kind, scopeWorkspace(scope), scopeSession(scope), entry.workspace, entry.snapshotAt, entry.deliveredAt)
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
				s.closeConn(c, cl.id, kind, causeInternal(closeReasonRosterMarshal, rosterErr))
				return
			}
			if res := enqueueLocked(cl, outFrame{key: coalesceKey(WorkspaceRosterFrame(held)), data: rosterData}); !res.queued {
				s.mu.Unlock()
				s.warn("frontend: connect roster rejected by a near-empty outbox kind=%s scope_workspace=%q revision=%d",
					kind, scopeWorkspace(scope), held.GetRevision())
				s.closeConn(c, cl.id, kind, causeInternal(closeReasonRosterRefused, nil))
				return
			}
		}
		s.clients[cl] = struct{}{}
		s.mu.Unlock()
	}
	s.logSnapshotCensus(cl, snapshotPhaseConnect, snapshot)
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
			s.disconnect(cl, causeInternal(closeReasonLeaseMarshal, err))
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
		res := enqueueLocked(cl, outFrame{data: data})
		s.mu.Unlock()
		if !res.queued {
			s.overflowEmit(cl)("frontend: snapshot lease queue full client_id=%d kind=%s scope_workspace=%q scope_session=%q buffer=%d ceiling=%d compacted=%d reason=%s stalled_for_ms=%d; hard-disconnecting",
				cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), cl.out.capacity(), cl.out.ceiling(), res.compacted, res.reason, res.stalledFor.Milliseconds())
			s.disconnect(cl, causeOverflow(res, "snapshot_lease"))
			return false
		}
		s.logSnapshotCensus(cl, snapshotPhaseLease, snapshot)
		s.logVerbosef("frontend: snapshot lease renewed client_id=%d kind=%s scope_workspace=%q scope_session=%q workspaces=%d revisions=%s interval_ms=%d",
			cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), len(snapshot.GetWorkspaces()), snapshotRevisions(snapshot), guiSnapshotLeaseInterval.Milliseconds())
		return true
	}
}

// catchUpFrame is one retained WorkspaceState marshalled for a connecting
// client, with the revisions that made it necessary so the delivery can say
// WHY it was sent.
type catchUpFrame struct {
	workspace   string
	key         string
	data        []byte
	snapshotAt  int64
	deliveredAt int64
}

// snapshotCatchUpLocked returns the frames that make a captured snapshot
// current: for every workspace whose newest delivered WorkspaceState is newer
// than the one in the snapshot, the retained frame itself, scoped exactly as a
// broadcast would have scoped it. Caller holds s.mu.
//
// This is the bounded replacement for the connect retry loop. The retry could
// not terminate while anything mutated state — the deferred boot sweep mutates
// it for tens of seconds — and each attempt paid a full fleet composition. The
// repair costs one marshal per genuinely stale workspace and always terminates,
// because the set it iterates is fixed the moment the lock is taken.
//
// Order is by workspace name so a connect is reproducible and testable; the
// frames are independent per workspace, so no ordering between them is
// observable to a client anyway.
func (s *Server) snapshotCatchUpLocked(snapshot *frontendv1.StateSnapshot, scope *Scope) ([]catchUpFrame, error) {
	seen := make(map[string]int64, len(snapshot.GetWorkspaces()))
	for _, state := range snapshot.GetWorkspaces() {
		seen[state.GetWorkspace()] = state.GetAtMs()
	}
	stale := make([]string, 0, len(s.latestWorkspaceAt))
	for workspace, deliveredAt := range s.latestWorkspaceAt {
		if scope != nil && scope.Workspace != "" && workspace != scope.Workspace {
			continue
		}
		if deliveredAt > seen[workspace] {
			stale = append(stale, workspace)
		}
	}
	sort.Strings(stale)
	out := make([]catchUpFrame, 0, len(stale))
	for _, workspace := range stale {
		retained := s.latestWorkspaceState[workspace]
		if retained == nil {
			// The revision and the frame that carried it are recorded in one
			// locked step, so this is unreachable; it is still refused rather
			// than skipped, because skipping would silently serve the stale
			// view this whole path exists to prevent.
			return nil, fmt.Errorf("frontend: no retained WorkspaceState for stale workspace %q at revision %d", workspace, s.latestWorkspaceAt[workspace])
		}
		frame := WorkspaceStateFrame(retained)
		key := coalesceKey(frame)
		if scope != nil {
			scoped, keep := scopeFrame(frame, *scope)
			if !keep {
				continue
			}
			frame, key = scoped, coalesceKey(scoped)
		}
		data, err := marshalFrame(frame)
		if err != nil {
			return nil, fmt.Errorf("frontend: marshal catch-up WorkspaceState workspace=%q: %w", workspace, err)
		}
		out = append(out, catchUpFrame{
			workspace:   workspace,
			key:         key,
			data:        data,
			snapshotAt:  seen[workspace],
			deliveredAt: s.latestWorkspaceAt[workspace],
		})
	}
	return out, nil
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

// Snapshot phases. Every full state snapshot the daemon assembles is served to
// exactly one client for exactly one of these reasons, and the reason is the
// difference between "the editor reconnected" and "a webview's freshness lease
// came round again" — two facts with entirely different remedies that one
// unattributed log line could not tell apart.
const (
	snapshotPhaseConnect = "connect"
	snapshotPhaseLease   = "lease"
	snapshotPhaseResync  = "resync"
)

// logSnapshotCensus records one assembled snapshot AGAINST THE CLIENT IT WAS
// BUILT FOR.
//
// The count line used to be written by the state provider, which is handed no
// client and named none, and it called every assembly a "connect snapshot".
// Reading the log then meant reading 5350 apparent reconnects, of which 95
// actually were: the rest were GUI streams renewing their snapshot lease on a
// timer, each paying for a full 150-workspace assembly to publish the one
// workspace its scope keeps. The client id, its kind and the phase are what
// make that distinction visible without correlating against anything.
func (s *Server) logSnapshotCensus(cl *client, phase string, snapshot *frontendv1.StateSnapshot) {
	taskCount := 0
	for _, catalog := range snapshot.GetCatalogs() {
		taskCount += len(catalog.GetTasks())
	}
	s.logf("frontend: state snapshot served client_id=%d kind=%s phase=%s scope_workspace=%q scope_session=%q workspaces=%d sessions=%d catalogs=%d tasks=%d async_bubbles=%d inits=%d queues=%d progress=%d workspace_available=%d host_actions=%d daemon=%t",
		cl.id, cl.kind, phase, scopeWorkspace(cl.scope), scopeSession(cl.scope),
		len(snapshot.GetWorkspaces()), len(snapshot.GetSessions()), len(snapshot.GetCatalogs()), taskCount,
		len(snapshot.GetAsyncBubbles()), len(snapshot.GetInits()), len(snapshot.GetQueues()), len(snapshot.GetProgress()),
		len(snapshot.GetWorkspaceAvailable()), len(snapshot.GetHostActions()), snapshot.GetDaemon() != nil)
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

// writeFrameWatched writes one frame with an alarm on the write ITSELF.
//
// The two-lane outbox bounds how many frames sit AHEAD of a correlated reply,
// and that is all it can bound. It cannot preempt a write already in progress:
// there is one socket and one writer goroutine, so a consumer that stops
// reading blocks the writer inside a single writeFrame call and every queued
// ack waits behind it no matter which lane it is on.
//
// That is not hypothetical. Emacs is single-threaded and blocks its own event
// loop for seconds at a time restoring workspaces at startup; one observed boot
// blocked this writer for ~13s and then delivered eighteen acks within a
// millisecond of the host reading again. Nothing said so. The commands' own
// overdue records fired, which named eighteen slow commands and not the one
// blocked write they were all waiting on, and an operator had to infer the
// fault from a small enqueue_ms under a large duration_ms.
//
// So the write announces itself while it is stuck rather than only in
// retrospect, exactly as an overdue command does. The alarm never interferes
// with the write: it observes, and the write's own error handling is untouched.
func (s *Server) writeFrameWatched(c conn, cl *client, f outFrame) error {
	started := time.Now()
	var reported atomic.Bool
	alarm := time.AfterFunc(s.ackDeadline, func() {
		reported.Store(true)
		s.logf("frontend: OUTBOUND WRITER BLOCKED client_kind=%s lane=%s blocked_ms=%d — the consumer is not reading; every queued reply, including acks, waits behind this one write",
			cl.kind, laneName(f.control), s.ackDeadline.Milliseconds())
	})
	// Every chunk the socket accepts is drain progress. THIS is what stops the
	// stall accounting from calling a host that is steadily consuming a 31s
	// snapshot "wedged": the frame count does not move for the whole write, and
	// the byte count moves constantly.
	err := c.writeFrame(f.data, cl.out.noteWriteProgress)
	alarm.Stop()
	if reported.Load() {
		// The stall is over one way or the other, and the record that announced
		// it owes a resolution: "blocked" with no end is indistinguishable from
		// a daemon that exited still blocked.
		s.logf("frontend: outbound writer UNBLOCKED client_kind=%s lane=%s blocked_ms=%d ok=%t",
			cl.kind, laneName(f.control), time.Since(started).Milliseconds(), err == nil)
	}
	return err
}

// laneName names the lane a frame left by, for the records above. The bool is
// which queue it came off; the word is what an operator can read.
func laneName(control bool) string {
	if control {
		return "control"
	}
	return "bulk"
}

// drainOutbox writes every frame currently queued for cl, in lane order, and
// returns the first write error. It is one wake of the writer, factored out of
// the loop so the drain is a callable step rather than a shape only a goroutine
// can reach.
//
// A failed write is reported to its frame's sender BEFORE it is returned, so a
// frame that never reached the socket is never mistaken for a delivery that
// simply had not been timed yet. The caller owns the teardown.
func (s *Server) drainOutbox(c conn, cl *client) error {
	for {
		f, ok := cl.out.pop()
		if !ok {
			// THE OUTBOX RAN DRY. The first time that happens, this
			// connection's bring-up backlog — its connect snapshot and every
			// retained push that followed it — is fully written, which is the
			// closing edge of the boot-drain window (bootdrain.go). closeAt
			// keeps the first such moment and ignores every later one, so an
			// ordinary quiet period is never mistaken for a bring-up.
			cl.drain.closeAt(time.Now())
			return nil
		}
		if err := s.writeFrameWatched(c, cl, f); err != nil {
			wrapped := fmt.Errorf("frontend: write frame: %w", err)
			notifyFrame(f, wrapped)
			return wrapped
		}
		// The bytes are on the socket. This is the moment a correlated reply's
		// wait actually ends, and the only place that can say so.
		notifyFrame(f, nil)
	}
}

// writeLoop owns the socket's teardown: it is the goroutine that closes the
// connection, and it closes it exactly once, through closeConn, with the cause
// disconnect recorded. There is no other close on this path, silent or
// otherwise.
func (s *Server) writeLoop(c conn, cl *client) {
	defer func() { s.closeConn(c, cl.id, cl.kind, cl.closeCause()) }()
	for {
		select {
		case <-cl.done:
			// disconnect recorded the cause before closing done, so the
			// teardown above can name it.
			return
		case <-cl.out.ready:
			// ready is a wakeup, not the queue: drain everything each wake, so
			// a signal coalesced with an earlier one strands no frame.
			if err := s.drainOutbox(c, cl); err != nil {
				s.logf("frontend: write failed, disconnecting client_id=%d kind=%s: %v", cl.id, cl.kind, err)
				s.disconnect(cl, causeWriteFailed(err))
				return
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
	lanes := newCommandLanes(s.logf, s.logVerbosef, s.processCommand, s.answerSuperseded)
	// TELEMETRY IS NOT COMMAND WORK. client_log never reaches the lanes: it is
	// acked at ingress and written off to this connection's telemetry writer,
	// whose bounded queue and single writer goroutine are the whole of its cost
	// to the interactive path (telemetry.go).
	telemetry := newTelemetryWriter(s.warn, s.writeClientLogTelemetry)
	// inbound is why this loop stopped, and it is what the teardown reports.
	// It is written before the deferred teardown runs and read only by it, on
	// this goroutine, so it needs no synchronization. It starts UNRECORDED on
	// purpose: the loop's only exit sets it, and any future exit that forgets
	// to trips the accountability backstop in closeConn instead of inventing a
	// plausible reason.
	var inbound closeCause
	defer func() {
		// Drain first, disconnect second: a command already read still owes
		// the client an answer, exactly as it did when dispatch ran inline.
		lanes.close()
		// Second, and for the same reason: a record this connection accepted
		// was already answered with an ok ack, so it is written before the
		// connection is considered gone.
		telemetry.close()
		s.disconnect(cl, inbound)
	}()
	for {
		cmd, err := c.readCommand()
		if err != nil {
			inbound = causeInboundEnded(err)
			if !isPeerClose(err) {
				s.logf("frontend: read command client_id=%d kind=%s: %v", cl.id, cl.kind, err)
			}
			return
		}
		// THE WORKSPACE KEY IS CANONICALIZED HERE AND NOWHERE ELSE. This is the
		// single point at which a decoded command's workspace field is first
		// read, and it runs BEFORE the lane key is computed and before any
		// registry or session lookup, so "/path/ws/" and "/path/ws" are the
		// same lane, the same record, and the same session from here on down
		// (workspacekey.go).
		normalizeCommandWorkspace(cmd)
		// EVERY COMMAND IS TIMED, and the clock starts the instant the frame
		// was decoded rather than inside dispatch: the interval the client
		// actually waits out is receipt through ack, and with lanes that
		// interval includes any wait in the lane's queue — instrumenting only
		// the handler would report a fast command sitting behind slow work as
		// fast. The depth is taken here, before any of this command's own work
		// runs, so it counts what was ALREADY in flight.
		//
		// The increment and the ticket that owes its decrement are created
		// together, so there is no window in which a command counts against
		// the gauge with nothing obliged to release it.
		received := time.Now()
		depth := s.inflight.Add(1)
		t := s.newCommandTicket(cl, cmd, received, depth)
		if cmd.GetClientLog() != nil {
			s.acceptClientLog(t, telemetry)
			continue
		}
		lanes.submit(t)
	}
}

// processCommand performs one command and answers it. It is the body the read
// loop used to run inline, unchanged in every respect except where it runs and
// the receipt-side timing facts it reports against.
func (s *Server) processCommand(t *commandTicket) {
	cl, cmd := t.cl, t.cmd
	var ack *frontendv1.CommandAck
	var processing time.Duration
	// SETTLING IS DEFERRED, and that is the whole guarantee: the in-flight
	// gauge comes back and the one completion record is written on EVERY exit
	// from this function, including a panic unwinding out of a handler (which
	// still propagates — nothing here recovers it). A command that returned
	// without settling used to inflate every later command's queue_depth for
	// the rest of the daemon's life.
	defer func() { t.finish(ack, processing) }()
	// THE ONE ORDERING PAIR THE CONTROL LANE MUST NOT BREAK.
	//
	// A resync's ANSWER is the snapshot, not the ack: the client asked to be
	// made current, and an ok=true ack that arrived first would report it
	// current while it still held the state it asked to replace. The snapshot
	// cannot join the ack on the control lane either — it is a bulk state frame,
	// and a snapshot that overtook the deltas queued before it would have the
	// client adopt fresh state and then apply stale deltas onto it, which is
	// exactly the snapshot-before-delta ordering the bulk FIFO exists to hold.
	//
	// So the resync's ack follows its snapshot down the BULK lane instead. This
	// is the only command with a bulk frame of its own, and it is not one of the
	// commands the head-of-line class was observed on (open_workspace,
	// publish_workspace_roster, submit_prompt) — its ack is tied to a snapshot
	// that has to be drained regardless.
	ackLane := controlLane
	// A resync re-sends a fresh StateSnapshot to THIS client (§5.4),
	// scope-filtered for a scoped connection. The handler covers the
	// conversation-delta replay the snapshot omits.
	if cmd.GetResync() != nil {
		s.enqueueResyncSnapshot(cl, cmd, "before_dispatch")
		ackLane = bulkLane
	}
	// THE ONE COMMAND THAT IS ACKED BEFORE IT RUNS.
	//
	// A conversation page is a bounded read whose answer is a PUSH: the page
	// frame carries the request_id, so the client correlates on that rather
	// than on the ack. What the ack is needed for is the opposite thing — to
	// stop the request looking unanswered while the read is in progress.
	//
	// That matters because of what an unanswered history request has already
	// been observed to do here. A resync whose ack outran the client's deadline
	// made the client re-arm another one, whose ack was also late, and the
	// observed command queue reached 5,069 entries settling 420-550 SECONDS
	// after they were sent (connect-resync.ts, lanes.go). A page is served from
	// a store read of a conversation that may hold a quarter of a million
	// events, so it is exactly the shape that trips that cascade. Acking at
	// ACCEPTANCE removes the trigger by construction rather than by hoping the
	// read is fast: there is no window in which the client is waiting on an ack
	// at all.
	//
	// It is not a claim the page was assembled — no client reads it as one,
	// because the page itself is what carries the items. A read that then FAILS
	// still answers, with a second, failing ack under the same request id; the
	// page client tracks its own in-flight ids and is what routes that late
	// refusal to the failure sink (command-dispatch.ts).
	pageCommand := cmd.GetConversationPage() != nil
	if pageCommand {
		s.enqueuePageAccepted(t)
		// The page rides the BULK lane with the feed content it is: a page can
		// be thousands of items, and putting it on the control lane would park
		// every ack behind it.
		ackLane = bulkLane
	}
	dispatchStart := time.Now()
	var response *frontendv1.FrontendFrame
	ack, response = s.dispatchClientCommand(cl, cmd)
	processing = time.Since(dispatchStart)
	// A stale generation is a request to adopt current authority, not a
	// history failure. Capture again AFTER the daemon made that decision so
	// a transition crossing the pre-dispatch capture cannot leave the client
	// holding the very identity the command just proved was retired.
	ackType, _ := errclass.TypeOf(ack.GetFailure())
	if cmd.GetResync() != nil && ackType == errclass.TypeSessionReconnectSuperseded {
		s.enqueueResyncSnapshot(cl, cmd, "after_superseded")
	}
	if !ack.GetOk() {
		s.logf("frontend: command nack {request_id=%s ws=%s}: %s", ack.GetRequestId(), cmd.GetWorkspace(), ack.GetError())
	}
	if response != nil {
		if data, err := marshalFrame(response); err != nil {
			s.logf("frontend: marshal command response request_id=%s: %v", ack.GetRequestId(), err)
		} else {
			// The correlated reply rides the SAME lane as the ack it precedes.
			// Both are request-keyed answers to this one command, and a lane
			// preserves push order within itself, so a health view still
			// reaches the client ahead of the ack that completes it.
			s.enqueue(cl, outFrame{control: ackLane == controlLane, data: data})
		}
	}
	// A PAGE COMMAND HAS ALREADY BEEN ACKED, at acceptance. What is left is the
	// one case acceptance could not speak for: the read FAILED, so no page is
	// coming, and saying nothing would leave the client's load-more spinning
	// against a request that will never answer. The refusal goes out under the
	// same request id, and it is deliberately the only second ack this server
	// ever sends.
	if pageCommand {
		if !ack.GetOk() {
			s.enqueuePageRefusal(cl, ack, ackLane)
		}
		return
	}
	// NO COALESCE KEY. This frame used to be pushed under coalesceKey(response)
	// — the OTHER frame's key — so a command whose response was supersedable
	// could have its ack compacted away as though a later frame replaced it.
	// An ack supersedes nothing and is superseded by nothing.
	if data, err := marshalFrame(CommandAckFrame(ack)); err != nil {
		s.logf("frontend: marshal command ack: %v", err)
		// No ack frame will ever exist, so no write can ever report on one.
		// The ticket is told here, or its record would never be written.
		t.ackUndeliverable(fmt.Errorf("frontend: marshal command ack: %w", err))
	} else {
		// Declared BEFORE the push: the writer may deliver these bytes on
		// another goroutine the instant they are queued, and a disposition the
		// ticket was not yet expecting would be a record written without the
		// delivery number it exists to carry.
		t.expectAckDelivery()
		s.enqueue(cl, outFrame{control: ackLane == controlLane, data: data, notify: t.ackDisposed})
	}
	// The ack is on the CONTROL lane; the client's wait ends when the writer
	// puts those bytes on the socket, and t.ackDisposed is what observes that
	// moment. The deferred settle above releases the in-flight gauge; whichever
	// of the two happens second writes this command's one latency record.
}

// supersededAckNote is the account a coalesced resync's ack carries. It is
// prose only — no client parses it — and it exists so a captured ack explains
// itself without a reader having to correlate the daemon log.
const supersededAckNote = "resync superseded by a newer resync on this workspace's lane; the newer replay answers this request"

// answerSuperseded answers a command the lanes dropped by coalescing, and it is
// the reason dropping one is not a loss.
//
// THE SHAPE IS ok=true, deliberately, after reading what both clients do with
// each alternative:
//
//   - A NACK reaches the webapp's `onFailure` sink (command-dispatch.ts) and
//     Emacs's refusal copy (core.el), so every coalesced entry during a flood
//     would open a user-visible refusal for a request the daemon is in fact
//     honoring. `session.reconnect_superseded` is worse still: its remedy text
//     tells the user to reload the webview.
//   - An ok ack resolves the webapp's pending promise and ends Emacs's wait
//     with no card, and NEITHER client re-sends on it. ConnectResync disarms
//     before dispatching and re-arms only on an identity-mismatch rejection,
//     so an ok ack cannot feed the flood.
//
// It is not a lie about work not done: the superseding resync sits on the SAME
// lane's FIFO, ahead of every command queued behind it, and the lanes drain
// even through close(). The replay this entry asked for happens — it is simply
// performed once for all of them.
func (s *Server) answerSuperseded(t *commandTicket) {
	s.answerAccepted(t, "superseded resync", supersededAckNote)
}

// clientLogAcceptedNote is the account an ingress-acked client_log carries. It
// is prose only, and it states exactly what the ok means: the record was taken
// for writing, off every lane, and nothing waits on the write.
const clientLogAcceptedNote = "client_log accepted for telemetry writing; it is written off the workspace's command lane and nothing waits on it"

// acceptClientLog answers a client_log AT INGRESS and hands the record to the
// connection's telemetry writer.
//
// THE ACK IS THE RECEIPT, and that is the whole change: a client_log's ack used
// to mean "written", which made a client's 10s deadline depend on a lane's
// backlog for a record nothing waits on. Both clients treat this ack the same
// way they treat any ok — the pending request resolves and nothing re-sends —
// and neither has ever read a client_log ack for anything else.
//
// The write's own failures are NOT swallowed by acking early: they are surfaced
// at warn by writeClientLogTelemetry, naming the workspace, request and error,
// which is the channel an operator reads them on anyway. What is given up is a
// nack the browser only logged.
func (s *Server) acceptClientLog(t *commandTicket, telemetry *telemetryWriter) {
	s.answerAccepted(t, "client_log", clientLogAcceptedNote)
	telemetry.submit(telemetryRecord{cl: t.cl, cmd: t.cmd})
}

// writeClientLogTelemetry performs one accepted client_log's write. It is the
// SAME dispatch the lane performed — the handler, its authority checks and its
// persistence are untouched — moved onto the telemetry writer's goroutine.
func (s *Server) writeClientLogTelemetry(rec telemetryRecord) {
	ack, _ := s.dispatchClientCommand(rec.cl, rec.cmd)
	if ack.GetOk() {
		return
	}
	// The client was already told the record was accepted, so this log is the
	// only place the refusal can be reported. It is a warn, not a debug: a
	// rejected browser record is evidence lost.
	//
	// It is RATE-LIMITED, not thinned: a webview stamping a retired session id
	// refuses once per forwarded record, and reporting each one in full buried
	// the log under thousands of identical lines. The first refusal of each
	// kind carries the whole evidence and the run behind it is counted, so no
	// refusal goes uncounted and none is reported twice over.
	workspace := rec.cmd.GetWorkspace()
	decision := s.clientLogRefusals.observe(workspace, clientLogRefusalReason(ack.GetError()))
	switch {
	case decision.first:
		s.warn("frontend: client_log telemetry write REFUSED ws=%q request_id=%q: %s",
			workspace, rec.cmd.GetRequestId(), ack.GetError())
	case decision.summary:
		s.warn("frontend: client_log telemetry writes REFUSED ws=%q suppressed=%d total=%d (repeating; see the first refusal above for the reason)",
			workspace, decision.suppressed, decision.total)
	}
}

// answerAccepted answers a command with an ok ack and no execution, settling
// its ticket exactly as processCommand does. label names the case in the log.
func (s *Server) answerAccepted(t *commandTicket, label, note string) {
	cl, cmd := t.cl, t.cmd
	ack := &frontendv1.CommandAck{
		RequestId: cmd.GetRequestId(),
		Ok:        true,
		Error:     note,
	}
	// The ticket is settled on EVERY path out, exactly as processCommand does
	// it, so a coalesced command can never leak the in-flight gauge or skip its
	// one latency record. Processing is zero: this command never ran.
	defer func() { t.finish(ack, 0) }()
	data, err := marshalFrame(CommandAckFrame(ack))
	if err != nil {
		s.logf("frontend: marshal %s ack request_id=%s: %v", label, ack.GetRequestId(), err)
		t.ackUndeliverable(fmt.Errorf("frontend: marshal %s ack: %w", label, err))
		return
	}
	// Declared before the push for the same reason processCommand declares it
	// there: the writer may dispose of these bytes the instant they are queued.
	t.expectAckDelivery()
	// The CONTROL lane, unlike a performed resync's ack: this one carries no
	// snapshot, so there is no bulk frame it must stay behind.
	s.enqueue(cl, outFrame{control: true, data: data, notify: t.ackDisposed})
}

// pageAcceptedAckNote is the account an accepted-at-enqueue page ack carries.
// Prose only — no client parses it — so a captured ack explains itself without
// a reader having to correlate the daemon log.
const pageAcceptedAckNote = "conversation page accepted; the page itself arrives as a pushed frame carrying this request id"

// enqueuePageAccepted answers a conversation page command AT ACCEPTANCE, before
// the read runs. See the call site for why this one command is acked early.
//
// It owns the ticket's ack accounting exactly as processCommand's own ack does:
// the delivery this declares is the moment the CLIENT's wait ends, which for a
// page really is here, because nothing it does next is gated on the ack.
func (s *Server) enqueuePageAccepted(t *commandTicket) {
	ack := &frontendv1.CommandAck{
		RequestId: t.cmd.GetRequestId(),
		Ok:        true,
		Error:     pageAcceptedAckNote,
	}
	data, err := marshalFrame(CommandAckFrame(ack))
	if err != nil {
		s.logf("frontend: marshal conversation page acceptance ack request_id=%s: %v", ack.GetRequestId(), err)
		t.ackUndeliverable(fmt.Errorf("frontend: marshal conversation page acceptance ack: %w", err))
		return
	}
	// Declared before the push for the same reason processCommand declares it
	// there: the writer may dispose of these bytes the instant they are queued.
	t.expectAckDelivery()
	// The CONTROL lane: an acceptance carries nothing bulky, and its whole
	// purpose is to reach the client before the read it precedes.
	s.enqueue(t.cl, outFrame{control: true, data: data, notify: t.ackDisposed})
}

// enqueuePageRefusal delivers the SECOND ack a page command can receive: the
// one that says the read failed and no page is coming.
//
// It carries no ticket disposition, because the ticket's ack was already
// delivered at acceptance and a command records ONE latency sample. What this
// adds is the client-facing fact acceptance could not carry.
func (s *Server) enqueuePageRefusal(c *client, ack *frontendv1.CommandAck, ackLane lane) {
	data, err := marshalFrame(CommandAckFrame(ack))
	if err != nil {
		s.logf("frontend: marshal conversation page refusal request_id=%s: %v — the client's page request is left unanswered", ack.GetRequestId(), err)
		return
	}
	s.logf("frontend: conversation page REFUSED request_id=%s — sending the refusal under the same request id the acceptance used: %s",
		ack.GetRequestId(), ack.GetError())
	s.enqueue(c, outFrame{control: ackLane == controlLane, data: data})
}

// recordCommandLatency persists one command lifecycle sample — a completion, or
// an overdue in-flight observation. A recorder failure is a routing or
// persistence invariant violation, so it is surfaced through the transport's
// own log rather than dropped.
//
// It is called only from commandTicket, under that ticket's lock, which is what
// makes "exactly one completion record per received command" structural rather
// than a convention this function has to be trusted to honor.
func (s *Server) recordCommandLatency(rec commandLatencyRecord) {
	if s.latency == nil {
		return
	}
	t := rec.ticket
	sample := CommandLatencySample{
		Workspace:  t.cmd.GetWorkspace(),
		RequestID:  t.cmd.GetRequestId(),
		Command:    CommandFieldName(t.cmd),
		ClientKind: t.cl.kind.String(),
		QueueDepth: t.depth,
		Enqueue:    rec.enqueue,
		Delivery:   rec.delivery,
		// An overdue observation reports a delivery still PENDING, which is
		// neither a success nor a failure yet; only a settled record can say
		// the ack reached the socket.
		Delivered:  !rec.overdue && rec.deliveryErr == nil,
		Processing: rec.processing,
		Threshold:  AckWarnThresholdFor(t.cl.kind.String(), s.ackWarn),
		Ok:         rec.ack.GetOk(),
		Overdue:    rec.overdue,
	}
	// THE CLASSIFICATION IS A MEASUREMENT, taken from the two events that define
	// the window rather than from this command's own numbers: the sample's
	// delivery interval is receipt through the ack reaching the socket, and the
	// window is the connect snapshot's own enqueue-through-write. Overlap means
	// the ack was waiting while this connection was still draining its
	// bring-up. What the recorder does with that name is the recorder's policy.
	if t.cl.drain.overlaps(t.received, t.received.Add(rec.delivery)) {
		sample.Decision = BootSnapshotDrainDecision
	}
	if rec.deliveryErr != nil {
		sample.DeliveryError = rec.deliveryErr.Error()
	}
	if err := s.latency.RecordCommandLatency(sample); err != nil {
		s.logf("frontend: record command latency FAILED request_id=%q command=%s ws=%q overdue=%v enqueue_ms=%d delivery_ms=%d: %v",
			sample.RequestID, sample.Command, sample.Workspace, sample.Overdue,
			sample.Enqueue.Milliseconds(), sample.Delivery.Milliseconds(), err)
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
	// The resync snapshot is a bulk frame like any other and is paced like one:
	// a client whose queue is already deep from the replay it just asked for
	// must not be evicted by the snapshot half of the same answer.
	s.paceClient(cl)
	s.enqueue(cl, outFrame{data: snap})
	s.logSnapshotCensus(cl, snapshotPhaseResync, snapshot)
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
	res := enqueueLocked(cl, f)
	if res.queued {
		s.notePressure(cl, res, "delivery")
		return
	}
	if res.closed {
		// enqueueLocked already reported the frame undelivered; a client that
		// is already gone is not a slow one and needs no second teardown.
		return
	}
	// The refused frame never reaches the socket, and its sender is told that
	// before the connection goes: an unreported refusal is exactly how a
	// command's ack goes missing with nothing in the log naming it.
	notifyFrame(f, fmt.Errorf("frontend: outbound queue refused the frame (%s, depth %d, soft %d, hard %d)",
		res.reason, res.depth, res.soft, res.hard))
	s.recordOverflow(cl, res, "delivery")
	s.disconnect(cl, causeOverflow(res, "delivery"))
}

// notePressure records a queue that has crossed into its elastic region but is
// still accepting frames. This is the transient case — a busy consumer that
// will drain — so it is verbose, once per episode rather than once per frame.
func (s *Server) notePressure(cl *client, res pushResult, phase string) {
	if !res.entered {
		return
	}
	s.logVerbosef("frontend: outbound buffer above soft bound client_id=%d kind=%s phase=%s depth=%d soft=%d hard=%d grace_ms=%d; absorbing, no disconnect",
		cl.id, cl.kind, phase, res.depth, res.soft, res.hard, hostStallGrace.Milliseconds())
}

// recordOverflow is the single canonical record for a refused push, i.e. for
// giving up on a connection.
//
// The HOST's eviction is emitted at WARN and every other kind's at info, and
// that asymmetry is deliberate: losing the host is a user-visible service
// degradation — Emacs logs its own `uds-link: DOWN` warning for the very same
// event — whereas a backgrounded webview being shed is the load-shedding
// contract working as designed. It also names the reason, so "hit the memory
// ceiling" is never confused with "drained nothing for the whole grace period".
func (s *Server) recordOverflow(cl *client, res pushResult, phase string) {
	s.overflowEmit(cl)("frontend: slow consumer (%s), outbound buffer full (%d frames, soft %d, hard %d) after compacting %d superseded frames, client_id=%d phase=%s reason=%s stalled_for_ms=%d, hard-disconnecting; reconnect replays snapshot",
		cl.kind, res.depth, res.soft, res.hard, res.compacted, cl.id, phase, res.reason, res.stalledFor.Milliseconds())
}

// overflowEmit selects the severity every give-up-on-this-connection record is
// written at. See recordOverflow for why the host is the loud one.
func (s *Server) overflowEmit(cl *client) dlog.Logf {
	if cl.kind.isHost() {
		return s.warn
	}
	return s.logf
}

// closeCause reports the reason recorded for this connection's teardown, or the
// unrecorded backstop when nothing recorded one.
func (cl *client) closeCause() closeCause {
	if c := cl.cause.Load(); c != nil {
		return *c
	}
	return closeCause{}
}

// disconnect removes a client from the fan-out set and signals its writer to
// stop. Idempotent: safe to call from the reader, the writer, enqueue, or Close.
//
// EVERY CALLER NAMES A CAUSE, and the cause is recorded BEFORE done is closed.
// That ordering is what makes the record and the WebSocket close frame
// possible: the writer wakes on done and is the goroutine that closes the
// socket, so a cause published after the close would race the very teardown it
// is supposed to explain.
//
// It holds no per-connection delivery state to unwind: nothing is ever withheld
// from one frontend on account of another, so a departure strands nothing.
func (s *Server) disconnect(cl *client, cause closeCause) {
	s.mu.Lock()
	delete(s.clients, cl)
	s.mu.Unlock()
	cl.closeOnce.Do(func() {
		cl.cause.Store(&cause)
		close(cl.done)
		// Every frame still queued is now undeliverable. Reporting each one is
		// what keeps a command whose ack died with the connection from simply
		// never producing its record: the sender learns the ack never landed.
		stranded := cl.out.close()
		for _, f := range stranded {
			notifyFrame(f, errClientGone)
		}
		s.logf("frontend: client disconnected client_id=%d kind=%s scope_workspace=%q scope_session=%q stranded_frames=%d cause=%s ws_close_code=%d",
			cl.id, cl.kind, scopeWorkspace(cl.scope), scopeSession(cl.scope), len(stranded), cause.String(), cause.code)
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
	//
	// progress, when non-nil, is called every time the socket ACCEPTS A CHUNK of
	// this frame's bytes. That callback is the drain-progress evidence the
	// slow-consumer accounting runs on, and it exists because frame counts are
	// not evidence: a 162-workspace connect snapshot is ONE frame, the observed
	// host spent 31s consuming it, and for all 31 of those seconds a
	// frame-counting stall detector saw a consumer that had drained nothing —
	// and evicted a client that completed the write 391ms later. See outbox's
	// progress field.
	writeFrame(data []byte, progress func()) error
	// readCommand reads one inbound FrontendCommand (UDS: one newline-delimited
	// line; WS: one message). Returns io.EOF on clean close.
	readCommand() (*frontendv1.FrontendCommand, error)
	// close tears the connection down for the given reason. A WebSocket
	// transport sends that reason as a close frame with its status code before
	// closing, so a client sees a real code rather than 1005; a raw socket has
	// no such channel and relies on the caller's record (closeConn).
	close(cause closeCause) error
}

// writeChunkBytes is how much of one frame is offered to the socket per write.
// It is the resolution of the byte-level drain-progress signal above: small
// enough that a consumer reading steadily reports progress many times inside a
// single large snapshot, large enough that a normal frame is one or two writes.
const writeChunkBytes = 32 << 10

// udsConn frames protojson newline-delimited over a net.Conn.
type udsConn struct {
	nc net.Conn
	r  *bufio.Reader
}

func newUDSConn(nc net.Conn) *udsConn {
	return &udsConn{nc: nc, r: bufio.NewReader(nc)}
}

// writeFrame writes the frame in bounded chunks, reporting each chunk the
// socket accepted. Chunking changes nothing about the bytes on the wire — a UDS
// stream has no message boundaries and the delimiter is still the trailing
// newline — and it is what turns "this consumer is alive and reading" from an
// unobservable fact into a signal the stall accounting can use.
func (u *udsConn) writeFrame(data []byte, progress func()) error {
	for off := 0; off < len(data); {
		end := off + writeChunkBytes
		if end > len(data) {
			end = len(data)
		}
		n, err := u.nc.Write(data[off:end])
		off += n
		if n > 0 && progress != nil {
			progress()
		}
		if err != nil {
			return err
		}
	}
	if _, err := u.nc.Write([]byte{'\n'}); err != nil {
		return err
	}
	if progress != nil {
		progress()
	}
	return nil
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

// close ends the raw stream. A UDS peer has no in-band channel for a reason, so
// the cause is deliberately not encoded here: closeConn has already written the
// record that accounts for this teardown, and inventing a trailer frame would
// be a protocol change no client parses.
func (u *udsConn) close(closeCause) error { return u.nc.Close() }

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

// writeFrame streams the frame as ONE text message, reporting each chunk the
// message writer accepted. NextWriter rather than WriteMessage is what makes
// that reporting possible; the message framing on the wire is identical.
func (w *wsConn) writeFrame(data []byte, progress func()) error {
	mw, err := w.ws.NextWriter(websocket.TextMessage)
	if err != nil {
		return err
	}
	for off := 0; off < len(data); {
		end := off + writeChunkBytes
		if end > len(data) {
			end = len(data)
		}
		n, werr := mw.Write(data[off:end])
		off += n
		if n > 0 && progress != nil {
			progress()
		}
		if werr != nil {
			// The partial message is abandoned, and the abandonment's own
			// failure is surfaced rather than dropped: a Close error here means
			// the connection is in a state the caller must still hear about.
			if cerr := mw.Close(); cerr != nil {
				return fmt.Errorf("%w (abandoning partial message: %v)", werr, cerr)
			}
			return werr
		}
	}
	return mw.Close()
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

// close sends a real WebSocket CLOSE FRAME carrying the cause's status code and
// reason, then closes the socket.
//
// This is the fix for the user-visible `close=1005` warning: 1005 is
// "no status received", i.e. the daemon dropped the TCP connection without ever
// telling the browser why, and a bare ws.Close() produces exactly that. gorilla
// permits WriteControl concurrently with an in-flight WriteMessage, so this
// works even while the writer goroutine is parked on a large frame.
//
// A close frame that cannot be sent is REPORTED, not swallowed: the underlying
// close still happens (a failed handshake must never leak the socket), and the
// handshake failure is returned so closeConn records it.
func (w *wsConn) close(cause closeCause) error {
	code := cause.code
	if code == 0 {
		code = websocket.CloseInternalServerErr
	}
	handshake := w.ws.WriteControl(
		websocket.CloseMessage,
		websocket.FormatCloseMessage(code, cause.wireReason()),
		time.Now().Add(closeFrameBudget),
	)
	closeErr := w.ws.Close()
	switch {
	case handshake != nil && closeErr != nil:
		return fmt.Errorf("frontend: close frame (code %d): %w; close: %v", code, handshake, closeErr)
	case handshake != nil:
		return fmt.Errorf("frontend: close frame (code %d): %w", code, handshake)
	default:
		return closeErr
	}
}

func unmarshalCommand(data []byte) (*frontendv1.FrontendCommand, error) {
	cmd := &frontendv1.FrontendCommand{}
	if err := protojson.Unmarshal(data, cmd); err != nil {
		return nil, fmt.Errorf("frontend: protojson unmarshal command: %w", err)
	}
	return cmd, nil
}

// isPeerClose reports an inbound error that means THE CLIENT ENDED IT: a clean
// stream EOF, a socket this daemon already closed underneath the reader, or a
// WebSocket close frame. Everything else is a broken frame stream, and the two
// are recorded as different causes because they have different remedies.
func isPeerClose(err error) bool {
	return errors.Is(err, io.EOF) || errors.Is(err, net.ErrClosed) || isWSClose(err)
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
