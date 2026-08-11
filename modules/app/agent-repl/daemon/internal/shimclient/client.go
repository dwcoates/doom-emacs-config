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
	// ErrTurnScopedRejection is the sink's own declaration that a lifecycle
	// refusal belongs to ONE TURN and not to the session.
	//
	// It is NOT terminal, and that is the whole point of it. A lifecycle
	// rejection ends the session and pins the durable mark behind the offending
	// event, so the next resume replays that event and ends the session again —
	// permanent, for any refusal whose cause is durable in the vendor stream.
	// A duplicate turn identity is exactly such a cause, so the sink marks that
	// refusal with this sentinel and the demux keeps the link, logs loudly,
	// reports the degradation, and lets the mark advance past the event.
	//
	// A sink that does not use it keeps the old terminal behavior unchanged.
	ErrTurnScopedRejection = errors.New("shimclient: lifecycle event rejected for its turn alone")
	// ErrTurnClaimRejected means the dedicated durable-ledger sink refused a
	// non-lifecycle rotation proof. It is terminal for the same replay reason,
	// but remains a distinct type so no caller can mistake proof for an SSM
	// lifecycle transition.
	ErrTurnClaimRejected = errors.New("shimclient: turn claim bridge rejected")
	// ErrReplayCursorInvariant means an event would make the durable replay
	// cursor cross an incomplete logical accounting record. Reconnecting cannot
	// change that event ordering, so Run terminates instead of skipping it.
	ErrReplayCursorInvariant = errors.New("shimclient: replay cursor invariant violated")
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

// OpenTurnClaims answers which turns are DURABLY in flight for a session.
//
// It exists because the pin set that keeps a turn's start and end atomic used
// to be remembered in process memory and thrown away on reconnect
// (pinnedAccountingTurns = nil). Discarding it is equivalent to declaring "no
// turn is in flight", which immediately unlocks the durable cursor to advance
// PAST a start whose end has not arrived. The next reconnect then replays that
// end alone and rejects it as naming an unpinned accounting turn — fatal, and
// observed in the field.
//
// Reading the ledger instead makes the reconstruction authoritative: the cursor
// can never advance past a start whose claim is still open, because the claim
// is what answers the question.
type OpenTurnClaims interface {
	// ActiveTurnIDs returns the turn ids whose durable claims are still open
	// for this workspace and claimant session. Holding nothing is an answer,
	// not a failure.
	ActiveTurnIDs(workspace, claimantSessionID string) ([]string, error)
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

// SessionRewoundSink consumes SessionRewound lineage evidence. It is separate
// from TurnClaimSink for the reason that one is separate from StateSink: a
// rewind's record must be able to close the claims of the turns it discarded,
// and a sink that could also paint or render would be able to do more than
// record.
type SessionRewoundSink interface {
	ApplySessionRewound(ev *corev1.Event, rewound *corev1.SessionRewound) error
}

// FrameSink consumes every non-lifecycle, non-degraded event: the data.v1
// vendor payloads (via the Any), the ContentDelta / HeartbeatProgress /
// MessageLatency ephemerals, and UnparsedEvent evidence. The stitch phase binds
// this to the frontend translation layer.
type FrameSink interface {
	// Consume feeds one event to the frontend translator. Called on the demux
	// goroutine in strict arrival order. A rejection prevents the persistent
	// high-water mark from advancing past an event the frontend/accounting path
	// did not accept.
	Consume(ev *corev1.Event) error
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

// Disposition is the reporter's verdict on one DegradedState: whether the row
// happened to the LIVE query or is history the store is replaying from a
// retired one.
//
// It is returned to this client because the client cannot make the call itself
// — the verdict is one comparison against the query the handshake bound, and
// only the reporter holds that binding — yet the client owns its own record of
// the event, and that record's SEVERITY depends on the verdict. Without it the
// relay had to assume every replayed degradation was present-tense news, so a
// single durable termination re-warned on every boot for the rest of the
// session's life.
type Disposition int

const (
	// DegradationLive is a degradation stamped by the query now in flight (or
	// carrying no stamp at all, which fails closed as live).
	DegradationLive Disposition = iota
	// DegradationHistorical is a degradation stamped by a RETIRED query,
	// replayed off the durable sequence.
	DegradationHistorical
)

// String names the disposition for the log record that carries it. An
// unrecognised value is reported as itself rather than defaulted to a familiar
// word, so a future arm nobody taught this method about is visible instead of
// silently reading as "live".
func (d Disposition) String() string {
	switch d {
	case DegradationLive:
		return "live"
	case DegradationHistorical:
		return "historical"
	default:
		return fmt.Sprintf("unknown(%d)", int(d))
	}
}

// DegradedReporter receives sad-path signals. DegradedState events come from
// the shim (store unreachable, converter storm, …); ConnectionDegraded /
// ConnectionRecovered are transport-level, detected by this client's
// missed-heartbeat monitor. The stitch phase binds these to a self-resolving
// SystemFailureItem conversation card (F4).
type DegradedReporter interface {
	// Degraded reports a shim-sourced DegradedState event.
	//
	// ev IS THE ENVELOPE THAT CARRIED ds, and it is a parameter rather than a
	// convenience: the envelope's query_instance_id is the ONLY thing that says
	// whether this degradation happened to the live query or is a row the store
	// is replaying from a retired one. Handing over the payload alone forced the
	// reporter to treat every replayed degradation as present-tense news, which
	// is how a retired query's death kept failing fresh bring-ups. A
	// daemon-originated degradation (one this client synthesises rather than
	// reads off the sequence) passes nil, and nil classifies as live — fail
	// closed, exactly as an unstamped event does.
	//
	// It returns the disposition it classified ds under, so this client can
	// record the same event at the severity that verdict earns instead of
	// guessing at one it has no way to compute.
	Degraded(sessionID string, ev *corev1.Event, ds *corev1.DegradedState) Disposition
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
	// ShimExits reports the death of a daemon-owned shim after it connected.
	// It is separate from ShimDeaths because bring-up failure and loss of an
	// established process have different owners and different responses.
	ShimExits ShimExits

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
	SeqStore SeqStore
	// OpenTurnClaims rebuilds the accounting pin set from durable state at
	// handshake. Nil keeps the pins that memory happens to hold, which is the
	// pre-existing behavior and cannot reconstruct anything after a generation
	// change — see OpenTurnClaims.
	OpenTurnClaims OpenTurnClaims
	// Workspace keys the durable claim lookup. Empty disables it for the same
	// reason a nil OpenTurnClaims does.
	Workspace  string
	StateSink  StateSink
	TurnClaims TurnClaimSink
	// Rewinds consumes SessionRewound lineage. Nil makes the event a LOUD
	// rejection rather than a silent fallthrough to the frame sink, where it
	// would be indistinguishable from an unhandled payload.
	Rewinds         SessionRewoundSink
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
	// The return value reports that the source generation is being retired by
	// an intentional transition. Readiness stays withheld in that case.
	OnConnected func(hello *corev1.ShimHello) (retiring bool)

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

	// Warnf is the daemon logger's WARN channel, for records that accompany a
	// regression the user can see — a shim-reported degradation, an unparsable
	// vendor event, a lost accounting pin. At info those sit beside routine
	// handshake chatter and are invisible to a level filter.
	//
	// Nil falls back to Logf, so the record is still made; only its severity
	// is lost.
	Warnf dlog.Logf

	// Errorf is the daemon logger's ERROR channel, for a hard failure of this
	// link — a broken capability channel the session cannot work around. Nil
	// falls back to Warnf, then to Logf.
	Errorf dlog.Logf

	// Tunables. Zero uses the defaults below.
	HeartbeatInterval time.Duration // how often we send Heartbeat
	HeartbeatTimeout  time.Duration // missed-heartbeat degraded window
	AckTimeout        time.Duration // control Ack/Nack await bound
	BackoffMin        time.Duration // initial reconnect backoff
	BackoffMax        time.Duration // reconnect backoff ceiling

	// BringUpStall is how long AwaitReady tolerates SILENCE from the shim
	// before the caller's expired context is allowed to end the wait.
	//
	// ShimReady is the LAST frame of the bring-up gate, and it is ordered
	// behind everything the shim wrote before it on the same stream — for a
	// workspace with a long transcript that is thousands of replayed events,
	// which this daemon's single read loop drains one sink call at a time.
	// A purely absolute deadline therefore declared "the shim never dialled
	// in" about a shim that had dialled in, handshaked, and was feeding this
	// client at full rate; the bigger the conversation, the more certainly it
	// tripped, which made it a permanent failure for exactly the workspaces
	// with the most to lose.
	//
	// So the failure bound is SILENCE, not elapsed time. A shim that never
	// connects, or that wedges mid-gate, still fails inside this window with
	// the same evidence it always carried. A shim that is demonstrably
	// working is no longer killed for taking longer than a constant. The
	// caller's context still supplies the absolute cap, so a shim that
	// trickles frames forever without ever acking is bounded too.
	//
	// Zero disables the inactivity rule and restores the pure-context bound.
	BringUpStall time.Duration
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
	// warnf is the WARN channel described on Config.Warnf. Never nil after
	// New; reached through warn.
	warnf dlog.Logf
	// errorf is the ERROR channel described on Config.Errorf. Never nil after
	// New; reached through logError.
	errorf dlog.Logf

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
	// terminal closes when Run encounters a protocol failure that reconnecting
	// cannot repair. AwaitReady selects on the same cause so a pre-readiness
	// rejection returns exact typed evidence instead of expiring generically.
	terminal     chan struct{}
	terminalErr  error
	terminalOnce sync.Once

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
	// durable cursor advancement remains pinned behind every active turn and
	// logical query-termination pair. The in-memory cursor can continue across a
	// transport reconnect; a daemon restart reads the pinned durable cursor and
	// therefore replays the complete uncommitted logical record.
	pinnedAccountingTurns map[string]struct{}
	// liveQueryInstanceID is the query() invocation this connection is bound
	// to, taken from the ShimHello that opened it. It is the ONLY thing an
	// event's envelope stamp is compared against, and it is owned by the Run
	// goroutine exactly as pinnedAccountingTurns is: runOnce writes it before
	// the read loop starts, and one runOnce's read loop has fully returned
	// before the next begins.
	liveQueryInstanceID     string
	pendingTerminationQuery string
	// pendingResumeQuery pins the durable cursor before a resumed QueryCreated
	// until runtime identity is accepted or a typed termination proves that the
	// query never established. A replacement controller must replay the resume
	// commitment before it can interpret the runtime identity that follows.
	pendingResumeQuery string
	haveVolatileCursor bool
	vendorSessionID    string

	// seqGeneration names the shim generation whose event last advanced
	// lastSeen ("" when no identified generation has: a mark just re-read from
	// the durable SeqStore, or one advanced over a connection whose hello
	// carried no pid). A seq is only comparable against a mark from the SAME
	// generation, so this is what tells a regression apart from a new
	// generation's fresh seq space. See seqgeneration.go.
	seqGeneration string

	// connGeneration names the generation of the connection currently being
	// read, taken from its ShimHello at the top of runOnce.
	//
	// Both fields are owned by the Run goroutine exactly as lastSeen is:
	// runOnce writes them before the read loop starts and the read loop is the
	// only other writer, and one runOnce's read loop has fully returned
	// (wg.Wait) before the next begins.
	connGeneration string

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

	// connectedOnce is owned by Run's goroutine. Once true, every subsequent
	// connection wait races the live process's exit event so a dead process can
	// never leave the reconnect loop waiting for a dial that cannot occur.
	connectedOnce bool
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
	if cfg.ShimExits == nil {
		if exits, ok := cfg.Source.(ShimExits); ok {
			cfg.ShimExits = exits
		}
	}
	logf := dlog.Tag(cfg.Logf, "component", "shimclient", "session", cfg.SessionID)
	warnSource := cfg.Warnf
	if warnSource == nil {
		warnSource = cfg.Logf
	}
	warnf := dlog.Tag(warnSource, "component", "shimclient", "session", cfg.SessionID)
	errorSource := cfg.Errorf
	if errorSource == nil {
		errorSource = warnSource
	}
	errorf := dlog.Tag(errorSource, "component", "shimclient", "session", cfg.SessionID)
	// An OPEN latch: a freshly built client has no connection yet.
	return &Client{cfg: cfg, logf: logf, warnf: warnf, errorf: errorf, ready: make(chan struct{}), terminal: make(chan struct{}), replays: newReplayRegistry()}
}

// warn emits through the client's WARN channel (Config.Warnf, or Logf when
// that is unwired). It is the sole reader of warnf.
//
// A Client assembled field-by-field rather than through New has no warnf at
// all; such a record still goes to logf, because losing it outright would be
// strictly worse than logging it at the wrong level.
func (c *Client) warn(format string, args ...any) {
	if c.warnf == nil {
		c.logf(format, args...)
		return
	}
	c.warnf(format, args...)
}

// logError emits through the client's ERROR channel (Config.Errorf, falling
// back to Warnf and then Logf). It is the sole reader of errorf, and degrades
// to warn for the same reason warn degrades to logf.
func (c *Client) logError(format string, args ...any) {
	if c.errorf == nil {
		c.warn(format, args...)
		return
	}
	c.errorf(format, args...)
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
// IT FAILS ON SILENCE, NOT ON ELAPSED TIME (Config.BringUpStall). See that
// field for why: ShimReady is the last frame of the gate and sits behind the
// shim's whole replayed backlog, so a busy shim and a dead one are only
// distinguishable by whether frames are still arriving.
func (c *Client) AwaitReady(ctx context.Context) error {
	var died <-chan struct{}
	if c.cfg.ShimDeaths != nil {
		died = c.cfg.ShimDeaths.DiedBeforeConnect(c.cfg.SessionID)
	}
	// The staleness reference before the first frame: a client that has never
	// received anything is silent as of NOW, not as of the unix epoch.
	started := time.Now()
	stall := c.cfg.BringUpStall
	var stallTimer *time.Timer
	var stallC <-chan time.Time
	if stall > 0 {
		stallTimer = time.NewTimer(stall)
		defer stallTimer.Stop()
		stallC = stallTimer.C
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
		case <-c.terminal:
			c.mu.Lock()
			err := c.terminalErr
			c.mu.Unlock()
			if err == nil {
				panic("shimclient: terminal readiness latch closed without an error")
			}
			return err
		case <-died:
			err := c.spawnDeathError()
			c.warn("bring-up ABORTED: %v", err)
			return err
		case <-stallC:
			// A frame may have landed since the timer was armed. Re-arm for the
			// remainder rather than failing a shim that is still feeding us.
			if remaining := stall - time.Since(c.lastActivity(started)); remaining > 0 {
				stallTimer.Reset(remaining)
				continue
			}
			err := fmt.Errorf("shimclient: awaiting shim connection for session %s: %w after %s of silence%s",
				c.cfg.SessionID, context.DeadlineExceeded, stall, c.spawnEvidence())
			c.logf("bring-up wait ENDED without a ready shim: no frame has arrived from this shim for %s: %v", stall, err)
			return err
		case <-ctx.Done():
			err := fmt.Errorf("shimclient: awaiting shim connection for session %s: %w%s",
				c.cfg.SessionID, ctx.Err(), c.spawnEvidence())
			c.warn("bring-up wait ENDED without a ready shim: %v", err)
			return err
		}
	}
}

// lastActivity is the most recent moment this client heard from the shim, with
// started standing in until the first frame arrives.
func (c *Client) lastActivity(started time.Time) time.Time {
	if nanos := c.lastRecvNanos.Load(); nanos > 0 {
		if recv := time.Unix(0, nanos); recv.After(started) {
			return recv
		}
	}
	return started
}

// Run attaches to the shim and keeps the connection alive until ctx is
// cancelled, reconnecting with exponential backoff after benign disconnects.
// It returns nil on clean ctx cancellation and a terminal protocol error
// (ErrVersionMismatch, ErrSeqRegression, ErrHandshakeRejected,
// ErrLifecycleRejected, ErrTurnClaimRejected) that reconnecting cannot fix.
func (c *Client) Run(ctx context.Context) (retErr error) {
	defer func() {
		if retErr != nil {
			c.finishTerminal(retErr)
		}
	}()
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
			errors.Is(err, ErrTurnClaimRejected), errors.Is(err, ErrReplayCursorInvariant),
			errors.Is(err, ErrShimDiedAfterConnect):
			c.warn("terminal protocol error, not reconnecting: %v", err)
			return err
		default:
			c.logf("shim connection ended: %v (will reconnect)", err)
		}
		if ctx.Err() != nil {
			return nil
		}
		c.logf("reconnecting to live shim in %s (reattach, resume from seq=%d)", backoff, c.lastSeen)
		var died <-chan ShimExit
		if c.connectedOnce && c.cfg.ShimExits != nil {
			died = c.cfg.ShimExits.DiedAfterConnect(c.cfg.SessionID)
		}
		select {
		case <-ctx.Done():
			return nil
		case exit := <-died:
			if ctx.Err() != nil {
				return nil
			}
			err := c.afterConnectDeathError(exit)
			c.warn("terminal shim process death, not reconnecting: %v", err)
			return err
		case <-time.After(backoff):
		}
		backoff *= 2
		if backoff > c.cfg.BackoffMax {
			backoff = c.cfg.BackoffMax
		}
	}
}

func (c *Client) finishTerminal(err error) {
	if err == nil {
		panic("shimclient: cannot finish terminally without an error")
	}
	c.terminalOnce.Do(func() {
		c.mu.Lock()
		c.terminalErr = err
		c.mu.Unlock()
		close(c.terminal)
	})
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
	conn, hello, err := c.nextConnection(ctx)
	if err != nil {
		return err
	}
	c.connectedOnce = true

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
			return fmt.Errorf("%w before DaemonHello: %w", ErrHandshakeRejected, err)
		}
	}

	// GATE STAGE 2: answer with the resume position. This is what the shim
	// opens its standing store subscription at, so it is read here — after the
	// rotation reconciliation above, and before any frame goes out.
	durableFrom := c.cfg.SeqStore.LastSeq(c.cfg.SessionID)
	nextGeneration := shimGenerationID(hello)
	from := durableFrom
	if c.haveVolatileCursor && c.connGeneration == nextGeneration && c.vendorSessionID == hello.GetVendorSessionId() {
		from = c.lastSeen
	} else {
		// THE PIN SET IS REBUILT FROM DURABLE STATE, NEVER DISCARDED. Clearing it
		// told advanceDurableCursor that nothing was in flight, which let the
		// cursor move past a start whose end had not arrived; the end then
		// replayed alone and was rejected as unpinned. The open claims are the
		// authority on what is actually in flight across a reconnect.
		c.pinnedAccountingTurns = c.reconstructPinnedTurns()
		c.pendingTerminationQuery = ""
		c.pendingResumeQuery = ""
	}
	// The query this connection speaks for, bound from the hello before any
	// event is read. Every provenance comparison is against this value.
	c.liveQueryInstanceID = hello.GetQueryInstanceId()
	c.lastSeen = from
	// The generation THIS connection speaks for. seqGeneration is deliberately
	// left alone: it still names the generation that earned the mark, and a
	// reconnect to the same shim must keep the regression guard fully strict.
	c.connGeneration = nextGeneration
	c.vendorSessionID = hello.GetVendorSessionId()
	c.haveVolatileCursor = true
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
	c.logf("bring-up gate: DaemonHello sent from_seq=%d permission_mode=%s turn_in_flight=%v shim_version=%s shim_generation=%q mark_generation=%q; awaiting ShimReady",
		from, mode, hello.GetTurnInFlight(), hello.GetShimVersion(), c.connGeneration, c.seqGeneration)

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

type connectionResult struct {
	conn  net.Conn
	hello *corev1.ShimHello
	err   error
}

// nextConnection waits on the transport and, after the process has connected
// once, on that process's exit event. The derived context makes the losing
// transport wait stop immediately, so the observer adds no goroutine leak.
func (c *Client) nextConnection(ctx context.Context) (net.Conn, *corev1.ShimHello, error) {
	if !c.connectedOnce || c.cfg.ShimExits == nil {
		return c.cfg.Source.Next(ctx, c.cfg.SessionID)
	}
	died := c.cfg.ShimExits.DiedAfterConnect(c.cfg.SessionID)
	if died == nil {
		return c.cfg.Source.Next(ctx, c.cfg.SessionID)
	}
	nextCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	result := make(chan connectionResult, 1)
	go func() {
		conn, hello, err := c.cfg.Source.Next(nextCtx, c.cfg.SessionID)
		result <- connectionResult{conn: conn, hello: hello, err: err}
	}()
	select {
	case <-ctx.Done():
		return nil, nil, ctx.Err()
	case exit := <-died:
		if ctx.Err() != nil {
			return nil, nil, ctx.Err()
		}
		return nil, nil, c.afterConnectDeathError(exit)
	case got := <-result:
		return got.conn, got.hello, got.err
	}
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
	c.mu.Unlock()
	if !current {
		c.logf("ShimReady arrived on a superseded connection; ignoring")
		return
	}
	// OnConnected owns every generation transition implied by this ShimReady,
	// including stale-build replacement. It must run before readiness is
	// published so AwaitReady cannot release a source generation before its
	// intentional replacement rendezvous exists.
	if c.cfg.OnConnected != nil && c.cfg.OnConnected(ac.hello) {
		c.logf("ShimReady source generation entered an intentional transition; readiness remains withheld")
		return
	}
	c.mu.Lock()
	current = c.active == ac
	if current {
		c.wired = true
		c.markReadyLocked()
	}
	c.mu.Unlock()
	if !current {
		c.logf("ShimReady source generation was retired during OnConnected; readiness remains withheld")
		return
	}
	c.logf("bring-up gate CLOSED: shim fully wired from_seq=%d store_key=%s; this session is now driveable",
		ready.GetFromSeq(), ready.GetVendorSessionId())
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
				// The sender stops here, so the link is on its way down.
				c.warn("heartbeat send failed: %v", err)
				return
			}
		}
	}
}

// heartbeatMonitor watches inbound liveness. When no frame has arrived within
// HeartbeatTimeout it opens a degraded window (once); when traffic resumes it
// closes it. It never tears the connection down — a truly dead socket surfaces
// through the read loop; this is honest reporting, not a fallback.
//
// # WHY THIS ONE LATCHES ON A DURATION AND NOT ON A PROBE COUNT
//
// The daemon's other degrade latches — the phantom sweep's live-set probe
// (sessioncontroller/phantomtask.go) and Emacs's daemon-reachability probe
// (lisp/frontend-client.el) — count CONSECUTIVE UNANSWERED PROBES, precisely
// because an elapsed window can be satisfied by wall time in which nothing was
// ever asked. That objection does not reach this monitor, and converting it
// would make it worse:
//
//   - THERE IS NO PROBE WITH AN OUTCOME TO COUNT. A Heartbeat is fire and
//     forget in both directions: the shim never acks ours (events.go, the
//     Heartbeat case: "Liveness only... No reply"), and its own heartbeats are
//     unsolicited. Nothing here is an ask, so nothing here can go unanswered.
//     Pairing them would be a wire change spanning the shim, replacing a signal
//     that works with one that does not exist yet.
//   - THE ONLY COUNT AVAILABLE WOULD BE A DURATION IN DISGUISE. "N consecutive
//     ticks that saw no new inbound frame" is measured by this same ticker,
//     which fires on wall time whether or not anything was ever sent — so it is
//     satisfied by exactly the schedules a bare timer is, while delaying a
//     genuine degrade by a factor of N.
//   - SILENCE HERE IS A FACT ABOUT THE PEER, not about our own scheduling. A
//     probe nobody issued says nothing; inbound traffic that did not arrive is
//     an observation, and after a suspend the link really has received nothing
//     and may well be dead. Reporting that is honest, and the very next frame
//     closes the window through the recovery edge below.
//
// The cry-wolf property the count buys elsewhere is already had here: the
// degrade is published on ONE edge (degraded.CompareAndSwap) and has a matching
// recovery edge, so a mute shim produces one report, not one per tick.
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
					// The workspace turns DEGRADED in the frontend on this
					// edge, so the record that explains it must not sit at
					// info beside the heartbeats it is reporting the absence
					// of.
					c.warn("connection degraded: %s", reason)
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

// reconstructPinnedTurns rebuilds the accounting pin set from the durable turn
// ledger.
//
// A read failure is LOUD and yields an empty set rather than a guess: an empty
// set is the pre-existing behavior, and inventing pins from a failed read would
// hold the durable cursor behind turns that may not exist. The log names the
// failure so a cursor that then advanced too far is explainable.
func (c *Client) reconstructPinnedTurns() map[string]struct{} {
	pinned := map[string]struct{}{}
	if c.cfg.OpenTurnClaims == nil || c.cfg.Workspace == "" {
		return pinned
	}
	ids, err := c.cfg.OpenTurnClaims.ActiveTurnIDs(c.cfg.Workspace, c.cfg.SessionID)
	if err != nil {
		c.warn("shimclient: accounting pin reconstruction FAILED session=%s workspace=%s: %v — the pin set starts empty, so the durable cursor is no longer held behind any turn this session had in flight",
			c.cfg.SessionID, c.cfg.Workspace, err)
		return pinned
	}
	for _, id := range ids {
		if id != "" {
			pinned[id] = struct{}{}
		}
	}
	if len(pinned) > 0 {
		c.logf("shimclient: accounting pins REBUILT from the durable ledger session=%s workspace=%s turns=%d — the cursor stays held behind every turn whose claim is still open",
			c.cfg.SessionID, c.cfg.Workspace, len(pinned))
	}
	return pinned
}

// UnpinAccountingTurn releases the cursor hold a turn's start took, for a turn
// the daemon closed WITHOUT a `TurnEnded`.
//
// Only a stream `TurnEnded` used to delete a pin. A synthesized close
// (SynthesizeTurnClose, which exists precisely because the turn can no longer
// produce an end) therefore left its pin standing forever, and
// advanceDurableCursor holds the durable cursor while ANY pin remains — so the
// cursor froze at that point permanently and every later reconnect replayed
// from it. That is the mirror of the unpinned-end failure: one leaves a turn
// unrepresented, the other never lets the mark move again.
//
// Unknown ids are a no-op: a close may name turns this client never pinned.
func (c *Client) UnpinAccountingTurn(turnIDs ...string) {
	if len(c.pinnedAccountingTurns) == 0 {
		return
	}
	released := 0
	for _, id := range turnIDs {
		if id == "" {
			continue
		}
		if _, ok := c.pinnedAccountingTurns[id]; ok {
			delete(c.pinnedAccountingTurns, id)
			released++
		}
	}
	if released > 0 {
		c.logf("shimclient: accounting pins RELEASED by a synthesized close session=%s turns=%d remaining=%d — no TurnEnded will arrive for these, so the close is what frees the durable cursor",
			c.cfg.SessionID, released, len(c.pinnedAccountingTurns))
	}
}

// eventIsHistorical reports whether ev was produced by a query other than the
// one this connection is bound to.
//
// ONE COMPARISON, and deliberately nothing else: the producer stamped the query
// it was running onto the envelope at construction, so the answer is a fact the
// event carries rather than something reconstructed here from delivery order,
// sequence boundaries, or a ledger lookup. A row the store serves during
// catch-up still names its producing query, so a session's own startup records
// classify as LIVE without any companion condition.
//
// EMPTY IS LIVE. FAIL CLOSED. A producer that predates query_instance_id stamps
// nothing, and every check must then apply to it exactly as it did before the
// field existed. An unbound connection (no hello query) has nothing to compare
// against and likewise admits no history.
func (c *Client) eventIsHistorical(ev *corev1.Event) bool {
	if c.liveQueryInstanceID == "" {
		return false
	}
	eventQuery := ev.GetQueryInstanceId()
	return eventQuery != "" && eventQuery != c.liveQueryInstanceID
}
