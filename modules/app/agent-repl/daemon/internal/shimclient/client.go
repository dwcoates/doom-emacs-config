// Package shimclient is the daemon's client side of the agent-shim protocol:
// one connection per session to that session's shim.
//
// The shim DIALS the daemon (design-shim-transport-inversion.md); this package
// is handed the resulting connection by the daemon's listener rather than
// dialling one itself.
//
// Responsibilities:
//   - Connection lifecycle: take the next connection for this session from the
//     injected ConnSource (already identified by its ShimHello), reply
//     DaemonHello carrying the resume from_seq read off the SeqStore, and wait
//     for the shim's ShimReady ack before treating the session as driveable.
//     That one gated exchange IS the bring-up: there is no separate Subscribe
//     step, and nothing between the hello and the ack is a usable session.
//   - Heartbeats both ways with a missed-heartbeat window that surfaces a
//     degraded callback (and self-heals when traffic resumes).
//   - Reconnect: on a disconnect the client waits at the ConnSource for the
//     shim to dial back in (the shim outlives the daemon, so a disconnect never
//     ends the turn — the daemon re-attaches and replays from last_seen_seq).
//     No --resume respawn here.
//   - Control-plane sends with request_id correlation (control.go).
//   - Inbound event demux to the injected sinks (events.go).
//
// Wire format: every hop uses agent-shim's length-prefixed framing (the shared
// agentrepl/wire package). Because core.proto carries no top-level frame
// oneof, each message is wrapped in a google.protobuf.Any (the proto global
// registry is the type discriminator) and that Any's bytes are the frame
// payload. See the FINAL REPORT deviation note.
//
// This package PERSISTS nothing itself: last_seen_seq is read from and written
// to an injected SeqStore, which the stitch phase binds to the daemon's
// session registry.
package shimclient

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"net"
	"sync"
	"sync/atomic"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/protocol"
)

// Terminal (non-retryable) protocol errors: reconnecting cannot fix them, so
// Run returns them to the caller instead of looping.
var (
	// ErrVersionMismatch is returned when the shim's protocol_version does not
	// match the daemon's. A version-incompatible shim will not become
	// compatible on reconnect.
	ErrVersionMismatch = errclass.ErrShimVersionMismatch
	// ErrSeqRegression is returned when the store-assigned seq of a PERSISTENT
	// event goes backwards on a session — a protocol violation that means the
	// merged stream can no longer be trusted.
	ErrSeqRegression = errclass.ErrShimSeqRegression
	// ErrHandshakeRejected means daemon-owned reconciliation found the shim's
	// pre-subscription snapshot contradictory. Retrying the same hello cannot
	// repair durable state, so Run must fail the bring-up instead of looping.
	ErrHandshakeRejected = errors.New("shimclient: handshake rejected")
	// ErrLifecycleRejected means daemon state refused a persistent lifecycle
	// event. Reconnecting would replay the same unaccepted seq forever, so the
	// session fails loudly with its high-water still behind that event.
	ErrLifecycleRejected = errors.New("shimclient: lifecycle event rejected")
	// ErrTurnClaimRejected means the dedicated durable-ledger sink refused a
	// non-lifecycle rotation proof. It is terminal for the same replay reason,
	// but remains a distinct type so no caller can mistake proof for an SSM
	// lifecycle transition.
	ErrTurnClaimRejected = errors.New("shimclient: turn claim bridge rejected")
)

// SeqStore supplies and consumes the daemon-tracked last_seen_seq per session.
// The stitch phase binds this to the daemon's session registry so seq survives
// daemon restarts (enabling reattach replay). The client never persists on its
// own.
type SeqStore interface {
	// LastSeq returns the highest store seq the daemon has durably observed
	// for sessionID (0 if none — a fresh subscribe from the start).
	LastSeq(sessionID string) uint64
	// SetLastSeq records seq as the new high-water mark for sessionID. Called
	// in strictly increasing order per session by the demux loop.
	SetLastSeq(sessionID string, seq uint64)
}

// ModeStore supplies a session's PERMISSION POSTURE, read straight off the
// daemon's session record at handshake time and carried to the shim on
// DaemonHello.permission_mode.
//
// It is the same shape of seam as SeqStore, and for the same reason: the
// shimclient must not import the registry, and both facts the gate needs (the
// resume position and the posture) are the record's, not the client's. Reading
// it per handshake rather than per spawn is also what makes a reattach pick up
// the record's CURRENT posture instead of a stale spawn-time snapshot.
type ModeStore interface {
	// PermissionMode returns sessionID's stored mode, or "" for a session with
	// none (or no record at all). The client resolves "" through
	// protocol.ResolvePermissionMode, so an implementation must NOT invent a
	// default of its own.
	PermissionMode(sessionID string) string
}

// StateSink consumes lifecycle events (session/turn/task boundaries). The
// stitch phase binds this to the session-state manager (SSM).
type StateSink interface {
	// Apply feeds one lifecycle Event to the SSM. Called on the demux
	// goroutine in strict arrival order; must not block indefinitely.
	Apply(ev *corev1.Event) error
}

// TurnClaimSink consumes only TurnClaimBridge correlation proof. The stitch
// phase binds it directly to the durable turn ledger; it is deliberately
// separate from StateSink and FrameSink so proof cannot paint or render.
type TurnClaimSink interface {
	ApplyTurnClaimBridge(ev *corev1.Event) error
}

// FrameSink consumes every non-lifecycle, non-degraded event: the data.v1
// vendor payloads (via the Any), the ContentDelta / HeartbeatProgress /
// MessageLatency ephemerals, and UnparsedEvent evidence. The stitch phase binds
// this to the frontend translation layer.
type FrameSink interface {
	// Consume feeds one event to the frontend translator. Called on the demux
	// goroutine in strict arrival order.
	Consume(ev *corev1.Event)
}

// ModelCatalogSink receives the live query's selectable models.  A catalogue
// is session state, not conversation content a frontend may infer.
type ModelCatalogSink interface {
	ModelCatalog(sessionID string, catalog *corev1.ModelCatalog) error
}

// FileDiagnosticSink consumes a persistent file-plane diagnostic before it can
// enter frontend, retained, progress, or SSM paths.
type FileDiagnosticSink interface {
	PersistFileDiagnostic(ev *corev1.Event, diagnostic *corev1.FilePlaneDiagnostic) error
}

// DegradedReporter receives sad-path signals. DegradedState events come from
// the shim (store unreachable, converter storm, …); ConnectionDegraded /
// ConnectionRecovered are transport-level, detected by this client's
// missed-heartbeat monitor. The stitch phase binds these to a self-resolving
// SystemFailureItem conversation card (F4).
type DegradedReporter interface {
	// Degraded reports a shim-sourced DegradedState event.
	Degraded(sessionID string, ds *corev1.DegradedState)
	// ConnectionDegraded reports that the missed-heartbeat window elapsed with
	// no inbound traffic on the shim connection.
	ConnectionDegraded(sessionID, reason string)
	// ConnectionRecovered reports that inbound traffic resumed after a
	// degraded window (or a fresh connection re-attached).
	ConnectionRecovered(sessionID string)
}

// PermissionHandler answers inbound canUseTool round-trips. It may block (the
// answer typically comes from a human via a frontend); the client invokes it
// on its own goroutine and sends the returned PermissionResponse back to the
// shim. Returning nil is a protocol error and is loud-logged (no response is
// sent, and the shim's canUseTool stays blocked — honest, not papered over).
type PermissionHandler interface {
	HandlePermission(sessionID string, req *corev1.PermissionRequest) *corev1.PermissionResponse
}

// Config injects everything the stitch phase binds. Zero-value durations fall
// back to the package defaults.
type Config struct {
	// SessionID identifies the session this client attaches to.
	SessionID string

	// Source yields this session's connection. Required.
	//
	// The daemon no longer dials the shim: shims dial the daemon's one
	// listening socket and announce themselves, and the listener hands each
	// connection to the client that owns that session
	// (design-shim-transport-inversion.md). Next blocks until the shim
	// connects, so the reconnect loop needs no backoff of its own — a
	// disconnected session simply waits here for its shim to dial back in.
	Source ConnSource

	// ShimDeaths reports the death of the shim process this daemon spawned for
	// the session, so a bring-up waiting at AwaitReady fails on the process's
	// exit rather than on the caller's deadline. Optional.
	//
	// Left nil it is taken from Source when the source can also answer for the
	// process — the daemon's listener adapter and its spawn watch are the same
	// object, so binding it here costs no extra seam in the layers between.
	ShimDeaths ShimDeaths

	// DaemonVersion / ProtocolVersion travel in DaemonHello; ProtocolVersion
	// must equal the shim's or the handshake fails with ErrVersionMismatch.
	DaemonVersion   string
	ProtocolVersion string

	// PermissionModes supplies the session's stored permission posture for
	// DaemonHello.permission_mode. Optional: a nil store resolves to
	// protocol.DefaultSessionPermissionMode, which is the same answer an empty
	// record gives, so the field can never resolve to an ungated mode by
	// omission.
	PermissionModes ModeStore

	// Sinks and callbacks (all bound at stitch).
	SeqStore        SeqStore
	StateSink       StateSink
	TurnClaims      TurnClaimSink
	FrameSink       FrameSink
	Models          ModelCatalogSink
	FileDiagnostics FileDiagnosticSink
	Degraded        DegradedReporter
	Permissions     PermissionHandler

	// OnHandshake fires after the handshake completes and BEFORE the Subscribe
	// reads its from_seq off the SeqStore. Optional.
	//
	// THAT ORDERING IS THE WHOLE REASON IT EXISTS, and it is why this is not
	// folded into OnConnected. A shim announcing a ROTATED vendor session id
	// (ShimHello.vendor_session_id) is telling the daemon that the store seq
	// space its high-water mark counts in has been retired; the mark must be
	// reset before it is read, or this connection subscribes from a position
	// that means nothing in the new space and then reads its seq=1 as a
	// terminal regression. A hook that ran after the Subscribe could only
	// correct the NEXT connection.
	OnHandshake func(hello *corev1.ShimHello) error

	// OnConnected fires when the bring-up gate CLOSES — the shim's ShimReady,
	// not merely a completed handshake — carrying the ShimHello that opened it
	// (so stitch sees turn_in_flight for mid-turn reattach). Optional.
	OnConnected func(hello *corev1.ShimHello)

	// OnLinkLost fires when a connection this client was DRIVING drops while
	// the client itself lives on — the reconnect loop is about to re-run the
	// whole bring-up gate. Optional. It is the exact inverse edge of
	// OnConnected, and it exists because those two were not symmetric: the
	// gate CLOSING was reported and the gate RE-OPENING was not, so a
	// workspace whose shim link died without its session controller exiting kept claiming
	// to be fully wired for as long as the reconnect took.
	//
	// It does NOT fire for a teardown-initiated close (a cancelled run
	// context: hibernation, manager close, session controller stop). Those are not a link
	// LOSS — the session controller is going away, and its own exit is the honest edge for
	// them. Restricting the callback to a live context is what keeps the two
	// reports from racing each other over one teardown.
	OnLinkLost func(cause error)

	// Logf is the daemon's printf-style logging closure. It is required so
	// protocol and transport failures always reach the daemon's canonical log.
	Logf dlog.Logf

	// Tunables. Zero uses the defaults below.
	HeartbeatInterval time.Duration // how often we send Heartbeat
	HeartbeatTimeout  time.Duration // missed-heartbeat degraded window
	AckTimeout        time.Duration // control Ack/Nack await bound
	BackoffMin        time.Duration // initial reconnect backoff
	BackoffMax        time.Duration // reconnect backoff ceiling
}

// Defaults for the Config tunables.
const (
	DefaultHeartbeatInterval = 15 * time.Second
	DefaultHeartbeatTimeout  = 45 * time.Second
	DefaultAckTimeout        = 10 * time.Second
	DefaultBackoffMin        = 100 * time.Millisecond
	DefaultBackoffMax        = 5 * time.Second
)

// ConnSource yields a session's shim connection, already identified by the
// ShimHello the shim opened with. Next BLOCKS until that session's shim dials
// in, so a client whose shim has gone simply waits here for it to come back.
//
// Implemented by the daemon's shim listener; an interface so the client stays
// testable without a real socket.
type ConnSource interface {
	Next(ctx context.Context, sessionID string) (net.Conn, *corev1.ShimHello, error)
}

// Client is one session's shim connection. Construct with New; drive with Run.
type Client struct {
	cfg  Config
	logf dlog.Logf

	// active holds the current connection (nil while disconnected). Guarded by
	// mu. Control senders read it to write on the live connection.
	mu     sync.Mutex
	active *activeConn

	// ready is the readiness latch AwaitReady blocks on: CLOSED while `wired`
	// holds, replaced with a fresh open channel when the connection drops.
	// Guarded by the same mu as active/wired, so they can never disagree.
	//
	// This exists because bring-up is asynchronous: the daemon spawns the shim
	// process and starts connecting in a goroutine, so for a few hundred
	// milliseconds `active` is nil and every control send fails with
	// ErrNotConnected. Callers need to wait for the connection to become
	// usable, and they must wait on the EVENT rather than on a duration —
	// a timeout tuned to "probably long enough" is a guess that is wrong on
	// both sides (too short under load, needless latency otherwise).
	ready chan struct{}

	// wired is the shim's ShimReady ack: the session is fully wired, standing
	// store subscription included (core.proto ShimReady). Guarded by mu.
	//
	// IT IS A SEPARATE FACT FROM `active`, and that separation is the point. A
	// live connection means only that frames can be written; it says nothing
	// about whether the shim finished subscribing to the store. Latching
	// readiness on the connection is what let a health probe fire the instant
	// the daemon attached and be told store_subscribed=false about a session it
	// had itself just brought up.
	wired bool

	// lastSeen mirrors the SeqStore high-water mark for this session, tracked
	// in memory so the demux can detect regressions cheaply.
	lastSeen uint64

	// lastRecvNanos is the unix-nano time of the most recent inbound frame,
	// read by the heartbeat monitor.
	lastRecvNanos atomic.Int64

	// degraded tracks whether the monitor has an open degraded window, so
	// ConnectionDegraded / ConnectionRecovered fire exactly once per edge.
	degraded atomic.Bool

	// reqCounter feeds request-id generation (control.go).
	reqCounter atomic.Uint64

	// replays tracks in-flight bounded history replays by request id
	// (replay.go). Its own registry, not `pending`: a replay is a STREAM
	// closed by a ReplayDone, not a one-shot Ack.
	replays *replayRegistry
}

// activeConn is the mutable per-connection state.
type activeConn struct {
	conn net.Conn
	// hello is the ShimHello this connection opened with, kept so the
	// ShimReady that closes the gate can hand it to OnConnected.
	hello   *corev1.ShimHello
	writeMu sync.Mutex // serializes frame writes across goroutines

	pendMu  sync.Mutex
	pending map[string]chan ackResult    // request_id -> Ack/Nack waiter
	health  map[string]chan healthResult // request_id -> HealthStatus waiter
}

// ackResult carries the outcome of a correlated control request.
type ackResult struct {
	ack  *corev1.Ack  // non-nil on success; carries the interrupt outcome
	nack *corev1.Nack // non-nil = Nack; nil = Ack
	err  error        // connection lost etc.
}

// New constructs a Client, applying defaults for any zero-value Config field.
func New(cfg Config) *Client {
	if cfg.Logf == nil {
		panic("shimclient: Config.Logf is required")
	}
	if cfg.HeartbeatInterval == 0 {
		cfg.HeartbeatInterval = DefaultHeartbeatInterval
	}
	if cfg.HeartbeatTimeout == 0 {
		cfg.HeartbeatTimeout = DefaultHeartbeatTimeout
	}
	if cfg.AckTimeout == 0 {
		cfg.AckTimeout = DefaultAckTimeout
	}
	if cfg.BackoffMin == 0 {
		cfg.BackoffMin = DefaultBackoffMin
	}
	if cfg.BackoffMax == 0 {
		cfg.BackoffMax = DefaultBackoffMax
	}
	if cfg.ShimDeaths == nil {
		if deaths, ok := cfg.Source.(ShimDeaths); ok {
			cfg.ShimDeaths = deaths
		}
	}
	logf := dlog.Tag(cfg.Logf, "component", "shimclient", "session", cfg.SessionID)
	// An OPEN latch: a freshly built client has no connection yet.
	return &Client{cfg: cfg, logf: logf, ready: make(chan struct{}), replays: newReplayRegistry()}
}

// permissionMode resolves the posture this connection's DaemonHello announces:
// the record's mode when it has one, protocol.DefaultSessionPermissionMode
// otherwise. Never returns "" — an empty field on the wire means "a daemon too
// old to speak it", and this daemon is not that.
//
// A nil ModeStore takes the same branch as an empty record deliberately: the
// one thing a session must never acquire by omission is an ungated posture,
// and routing both through the single resolution site is what guarantees it.
func (c *Client) permissionMode() string {
	stored := ""
	if c.cfg.PermissionModes != nil {
		stored = c.cfg.PermissionModes.PermissionMode(c.cfg.SessionID)
	}
	return protocol.ResolvePermissionMode(stored)
}

// markReadyLocked publishes "the session is fully wired". Caller holds c.mu.
func (c *Client) markReadyLocked() {
	select {
	case <-c.ready: // already closed; nothing to publish
	default:
		close(c.ready)
	}
}

// markNotReadyLocked re-arms the latch after a disconnect. Caller holds c.mu.
// A closed channel cannot be reopened, so a fresh one replaces it — which is
// why AwaitReady re-reads the field on every pass instead of caching it.
func (c *Client) markNotReadyLocked() {
	select {
	case <-c.ready:
		c.ready = make(chan struct{})
	default: // already open
	}
}

// AwaitReady blocks until this session is FULLY WIRED, or ctx ends.
//
// WHAT IT NOW MEANS. It resolves on the shim's ShimReady — the last frame of
// the bring-up gate — so everything the gate covers is proven when it returns:
// the shim holds its session lock, its SDK query is built, its store producer
// link is up, and its standing store subscription is open at the from_seq this
// daemon asked for. It used to resolve on the CONNECTION, which proved only
// that frames could be written; a health probe issued immediately after was
// then racing the shim's own store subscription and lost.
//
// It still returns at the earliest instant that is true — an event, never a
// duration. ctx supplies the FAILURE bound: its expiry means the shim did not
// finish coming up, and the caller surfaces that loudly rather than driving a
// session that is not wired.
//
// The loop re-checks under the lock because the connection can drop again
// between the latch closing and this goroutine being scheduled.
//
// IT ALSO WATCHES THE PROCESS, not only the latch. A shim that dies between
// exec and its first frame closes no latch and dials no socket, so the only
// thing that used to end this wait was the caller's deadline — thirty seconds
// of silence naming neither the exit nor the reason. The death channel ends the
// wait at the instant of the exit and carries the process's own evidence, and
// the deadline path (still reached when the process is alive and simply never
// dialled) now says which of the two it was.
func (c *Client) AwaitReady(ctx context.Context) error {
	var died <-chan struct{}
	if c.cfg.ShimDeaths != nil {
		died = c.cfg.ShimDeaths.DiedBeforeConnect(c.cfg.SessionID)
	}
	for {
		c.mu.Lock()
		if c.active != nil && c.wired {
			c.mu.Unlock()
			return nil
		}
		ch := c.ready
		c.mu.Unlock()

		select {
		case <-ch:
			// Latch closed; loop to confirm `active` under the lock.
		case <-died:
			err := c.spawnDeathError()
			c.logf("bring-up ABORTED: %v", err)
			return err
		case <-ctx.Done():
			err := fmt.Errorf("shimclient: awaiting shim connection for session %s: %w%s",
				c.cfg.SessionID, ctx.Err(), c.spawnEvidence())
			c.logf("bring-up wait ENDED without a ready shim: %v", err)
			return err
		}
	}
}

// Run attaches to the shim and keeps the connection alive until ctx is
// cancelled, reconnecting with exponential backoff after benign disconnects.
// It returns nil on clean ctx cancellation and a terminal protocol error
// (ErrVersionMismatch, ErrSeqRegression, ErrHandshakeRejected,
// ErrLifecycleRejected, ErrTurnClaimRejected) that reconnecting cannot fix.
func (c *Client) Run(ctx context.Context) error {
	backoff := c.cfg.BackoffMin
	for {
		if ctx.Err() != nil {
			return nil
		}
		err := c.runOnce(ctx)
		switch {
		case err == nil, errors.Is(err, context.Canceled), errors.Is(err, context.DeadlineExceeded):
			if ctx.Err() != nil {
				return nil
			}
		case errors.Is(err, ErrVersionMismatch), errors.Is(err, ErrSeqRegression),
			errors.Is(err, ErrHandshakeRejected), errors.Is(err, ErrLifecycleRejected),
			errors.Is(err, ErrTurnClaimRejected):
			c.logf("terminal protocol error, not reconnecting: %v", err)
			return err
		default:
			c.logf("shim connection ended: %v (will reconnect)", err)
		}
		if ctx.Err() != nil {
			return nil
		}
		c.logf("reconnecting to live shim in %s (reattach, resume from seq=%d)", backoff, c.lastSeen)
		select {
		case <-ctx.Done():
			return nil
		case <-time.After(backoff):
		}
		backoff *= 2
		if backoff > c.cfg.BackoffMax {
			backoff = c.cfg.BackoffMax
		}
	}
}

// runOnce dials, handshakes, subscribes, then runs the read loop plus the
// heartbeat sender and monitor until the connection ends or ctx is cancelled.
// The returned error describes why the connection ended (nil never happens
// except on ctx cancel).
func (c *Client) runOnce(ctx context.Context) (retErr error) {
	if c.cfg.Source == nil {
		return errors.New("shimclient: no ConnSource configured")
	}
	c.logf("awaiting shim connection")
	conn, hello, err := c.cfg.Source.Next(ctx, c.cfg.SessionID)
	if err != nil {
		return err
	}

	ac := &activeConn{conn: conn, hello: hello, pending: make(map[string]chan ackResult), health: make(map[string]chan healthResult)}

	// GATE STAGE 1 was the ShimHello, already read by the listener to route
	// this connection here. Refuse an incompatible shim before anything else
	// happens — no registry mutation, no hello, no streaming.
	if err := c.checkVersion(hello); err != nil {
		conn.Close()
		return err
	}

	// BEFORE the high-water mark is read: a shim announcing a rotated vendor
	// session id resets it here, so the from_seq below asks the NEW seq space
	// for everything rather than resuming at a retired space's position.
	if c.cfg.OnHandshake != nil {
		if err := c.cfg.OnHandshake(hello); err != nil {
			conn.Close()
			return fmt.Errorf("%w before DaemonHello: %v", ErrHandshakeRejected, err)
		}
	}

	// GATE STAGE 2: answer with the resume position. This is what the shim
	// opens its standing store subscription at, so it is read here — after the
	// rotation reconciliation above, and before any frame goes out.
	from := c.cfg.SeqStore.LastSeq(c.cfg.SessionID)
	c.lastSeen = from
	// The session's posture travels with the resume position, resolved HERE so
	// the field is never empty on the wire (core.proto DaemonHello.
	// permission_mode). Empty is reserved for a daemon too old to speak it.
	mode := c.permissionMode()
	if err := ac.writeMsg(&corev1.DaemonHello{
		DaemonVersion:   c.cfg.DaemonVersion,
		ProtocolVersion: c.cfg.ProtocolVersion,
		FromSeq:         from,
		PermissionMode:  mode,
	}); err != nil {
		conn.Close()
		return fmt.Errorf("sending DaemonHello: %w", err)
	}
	c.logf("bring-up gate: DaemonHello sent from_seq=%d permission_mode=%s turn_in_flight=%v shim_version=%s; awaiting ShimReady",
		from, mode, hello.GetTurnInFlight(), hello.GetShimVersion())

	// Publish the live connection so the read loop and control senders can use
	// it. The READINESS latch is deliberately NOT closed here: it waits for the
	// ShimReady this connection is about to carry (dispatchShimReady).
	c.mu.Lock()
	c.active = ac
	c.mu.Unlock()

	// Seed liveness and clear any prior degraded window (fresh connection).
	c.markRecv()
	if c.degraded.CompareAndSwap(true, false) {
		c.reportRecovered()
	}

	connCtx, cancel := context.WithCancel(ctx)
	var wg sync.WaitGroup
	wg.Add(3)
	go func() { defer wg.Done(); c.heartbeatSender(connCtx, ac) }()
	go func() { defer wg.Done(); c.heartbeatMonitor(connCtx) }()
	// Shutdown watcher: a blocked readMsg on a plain net.Conn ignores ctx, so
	// closing the conn is what unblocks the read loop on cancellation.
	go func() { defer wg.Done(); <-connCtx.Done(); conn.Close() }()

	// The read loop owns this goroutine until the connection ends.
	retErr = c.readLoop(connCtx, ac)

	// Teardown: stop helpers (which closes the conn via the watcher) and fail
	// pending waiters. No silent drops: awaiting callers get a loud error.
	cancel()
	wg.Wait()
	c.mu.Lock()
	lost := false
	if c.active == ac {
		c.active = nil
		// Re-arm the latch: a later send must wait for the RECONNECT — and for
		// the whole gate it re-runs — rather than sail through on a latch left
		// closed by the dead connection.
		lost = c.wired
		c.wired = false
		c.markNotReadyLocked()
	}
	c.mu.Unlock()
	// The gate that CLOSED has re-opened. Reported only when it had actually
	// closed (`wired`) — a connection that died mid-gate never earned the
	// wiring it would now be retracting — and only when the run context is
	// still live, so a teardown's own close is left to the session controller exit that
	// follows it. See Config.OnLinkLost.
	if lost && ctx.Err() == nil && c.cfg.OnLinkLost != nil {
		c.logf("shim link LOST while the session controller lives; the reconnect loop will re-run the bring-up gate: %v", retErr)
		c.cfg.OnLinkLost(retErr)
	}
	ac.failPending(fmt.Errorf("shim connection closed: %w", retErr))
	// An in-flight replay whose shim went away will never be completed by it.
	// Telling the caller beats leaving it blocked on a ReplayDone that cannot
	// come.
	c.replays.failAll(fmt.Sprintf("shim connection closed: %v", retErr))
	return retErr
}

// checkVersion refuses an incompatible shim BEFORE any other stage of the gate
// runs: no registry reconciliation, no DaemonHello, no streaming. Reconnecting
// cannot make a version mismatch compatible, so Run treats it as terminal.
//
// The ShimHello is passed in rather than read here because the listener had to
// read it to know which session the connection belonged to — reading it twice
// would consume the first real frame instead.
func (c *Client) checkVersion(hello *corev1.ShimHello) error {
	if hello == nil {
		return fmt.Errorf("shimclient: handshake got no ShimHello")
	}
	if hello.GetProtocolVersion() != c.cfg.ProtocolVersion {
		return fmt.Errorf("%w: shim=%q daemon=%q", ErrVersionMismatch,
			hello.GetProtocolVersion(), c.cfg.ProtocolVersion)
	}
	return nil
}

// dispatchShimReady closes the bring-up gate: GATE STAGE 3, the shim's
// assertion that this session is fully wired. It is the ONLY thing that
// releases AwaitReady, and OnConnected fires from here for the same reason —
// a reattach consumer (the pending-resync re-arm) needs a shim that can serve,
// not merely one that has connected.
func (c *Client) dispatchShimReady(ac *activeConn, ready *corev1.ShimReady) {
	if got := ready.GetSessionId(); got != "" && got != c.cfg.SessionID {
		c.logf("ShimReady names session=%s on session=%s's connection; ignoring", got, c.cfg.SessionID)
		return
	}
	c.mu.Lock()
	current := c.active == ac
	if current {
		c.wired = true
		c.markReadyLocked()
	}
	c.mu.Unlock()
	if !current {
		c.logf("ShimReady arrived on a superseded connection; ignoring")
		return
	}
	c.logf("bring-up gate CLOSED: shim fully wired from_seq=%d store_key=%s; this session is now driveable",
		ready.GetFromSeq(), ready.GetVendorSessionId())
	if c.cfg.OnConnected != nil {
		c.cfg.OnConnected(ac.hello)
	}
}

// heartbeatSender emits a Heartbeat on every interval until the connection
// context is cancelled. A write failure is left for the read loop to surface.
func (c *Client) heartbeatSender(ctx context.Context, ac *activeConn) {
	t := time.NewTicker(c.cfg.HeartbeatInterval)
	defer t.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			if err := ac.writeMsg(&corev1.Heartbeat{SentAtMs: time.Now().UnixMilli()}); err != nil {
				c.logf("heartbeat send failed: %v", err)
				return
			}
		}
	}
}

// heartbeatMonitor watches inbound liveness. When no frame has arrived within
// HeartbeatTimeout it opens a degraded window (once); when traffic resumes it
// closes it. It never tears the connection down — a truly dead socket surfaces
// through the read loop; this is honest reporting, not a fallback.
func (c *Client) heartbeatMonitor(ctx context.Context) {
	interval := c.cfg.HeartbeatTimeout / 2
	if interval <= 0 {
		interval = c.cfg.HeartbeatTimeout
	}
	t := time.NewTicker(interval)
	defer t.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			since := time.Since(time.Unix(0, c.lastRecvNanos.Load()))
			if since > c.cfg.HeartbeatTimeout {
				if c.degraded.CompareAndSwap(false, true) {
					reason := fmt.Sprintf("no shim traffic for %s (>%s window)",
						since.Round(time.Millisecond), c.cfg.HeartbeatTimeout)
					c.logf("connection degraded: %s", reason)
					c.cfg.Degraded.ConnectionDegraded(c.cfg.SessionID, reason)
				}
			} else if c.degraded.CompareAndSwap(true, false) {
				c.reportRecovered()
			}
		}
	}
}

func (c *Client) reportRecovered() {
	c.logf("connection recovered: shim traffic resumed")
	c.cfg.Degraded.ConnectionRecovered(c.cfg.SessionID)
}

// markRecv records that an inbound frame just arrived.
func (c *Client) markRecv() { c.lastRecvNanos.Store(time.Now().UnixNano()) }

// newRequestID mints a request id that is unique across daemon restarts and
// vendor-session rotations, not merely within one process.
//
// The counter carries NO identity of its own: it restarts at 1 in every daemon
// process and in every fresh Client, so `daemon-prompt-2` names a different turn
// on each boot. Uniqueness rests entirely on the random suffix, which is why a
// crypto/rand failure cannot be tolerated here — a zeroed suffix would make the
// id a pure function of the counter and hand two different turns the same
// identity. These ids become durable turn-claim keys, so a collision is not a
// cosmetic correlation glitch: a new turn inherits a retired turn's ledger row
// and its bridge is refused against a claim it never owned.
//
// This mirrors newSecureControllerGenerationID in sessioncontroller: entropy
// failure is surfaced, never papered over with a weaker id. Every caller already
// returns an error, so the failure has somewhere honest to go.
func (c *Client) newRequestID(kind string) (string, error) {
	var b [6]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("shimclient: mint %s request id for session %s: %w", kind, c.cfg.SessionID, err)
	}
	return fmt.Sprintf("daemon-%s-%d-%s", kind, c.reqCounter.Add(1), hex.EncodeToString(b[:])), nil
}

// --- frame codec: one proto message per length-prefixed frame, wrapped in a
// google.protobuf.Any so the receiver can discriminate the type via the proto
// global registry. Both halves of that envelope live in `agentrepl/wire`
// (MarshalAny / ReadAny), shared with shim-store's server, the sidecar's store
// client, and the daemon's shim listener. Reads go straight through
// wire.ReadAny; only the write needs a step of its own, for the mutex below. ---

// writeMsg serializes msg into an Any and writes it as one frame, serialized
// across goroutines by the connection's write mutex.
//
// The encode deliberately happens OUTSIDE the mutex. writeMu exists so two
// goroutines cannot interleave bytes on one socket, which is a property of the
// WRITE alone; holding it across marshaling would serialize senders on CPU work
// that no ordering guarantee depends on. That is why this composes
// wire.MarshalAny + wire.WriteFrame rather than calling wire.WriteAny.
func (ac *activeConn) writeMsg(msg proto.Message) error {
	payload, err := wire.MarshalAny(msg)
	if err != nil {
		return err
	}
	ac.writeMu.Lock()
	defer ac.writeMu.Unlock()
	if err := wire.WriteFrame(ac.conn, payload); err != nil {
		return fmt.Errorf("writing %T frame: %w", msg, err)
	}
	return nil
}

// failPending resolves every outstanding control waiter with err (connection
// teardown). No silent drops: a caller awaiting an Ack gets a loud error.
func (ac *activeConn) failPending(err error) {
	ac.pendMu.Lock()
	defer ac.pendMu.Unlock()
	for id, ch := range ac.pending {
		ch <- ackResult{err: err}
		delete(ac.pending, id)
	}
	for id, ch := range ac.health {
		ch <- healthResult{err: err}
		delete(ac.health, id)
	}
}
