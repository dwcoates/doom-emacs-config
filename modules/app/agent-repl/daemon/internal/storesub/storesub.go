// Package storesub is the daemon's BOUNDED, READ-ONLY side channel into the
// agent-shim event store: a throwaway subscriber connection used to re-pull a
// slice of history the daemon no longer holds.
//
// # Why the daemon talks to the store directly
//
// The obvious route — send the session's shim a `Subscribe{from_seq}` with a
// low seq — is wrong, and destructively so. The shim's `onSubscribe` handler
// REOPENS its one standing store subscription at that seq
// (`shim/src/uds/uds-session.ts` -> `store-client.subscribe`), so a low
// from_seq would:
//
//   - move the daemon's standing subscribe position backwards, and
//   - replay the entire history down the SAME connection the demux feeds to
//     the SSM, the task catalog, and the progress resolver — planes that
//     consumed those events once already and would double-apply them.
//
// That second point is the exact corruption the historical-task flood is made
// of. So the re-pull rides its own connection to the store, which the store
// already serves (`Subscribe` -> replay-then-live-tail, shim-store/internal/
// server/server.go), and touches nothing the shim owns. No shim change was
// needed, and the standing subscription never learns this happened.
//
// # Bounded by construction
//
// A store subscription replays and then LIVE-TAILS forever; this reader is not
// a tail. Replay stops at the first of:
//
//   - StopAtSeq: the first seq the caller's own retained window already covers
//     (the ring floor). Reaching it means the gap is closed.
//   - MaxEvents: a hard cap on how much history one request may pull.
//   - Idle: no frame within IdleTimeout, which means the replay drained and the
//     connection has gone quiet in live-tail.
//   - ctx: the caller's deadline/cancellation.
//
// Every bound that trips is reported to the caller, never absorbed: a truncated
// re-pull is a partial answer and the caller says so.
//
// The store's `session_id` is the VENDOR session uuid (see
// agent-shim/shim-store/AGENTS.md) — subscribing under a daemon `s_` id
// registers on a channel nothing publishes to and silently returns nothing, so
// callers must pass the vendor uuid.
package storesub

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"claude-repld/internal/dlog"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// Defaults for the Config bounds.
const (
	// DefaultMaxEvents caps one re-pull. Generously above the largest observed
	// backfill burst (~1,009 events) and the retained ring (4,096), while still
	// making an unbounded replay impossible.
	DefaultMaxEvents = 20000
	// DefaultIdleTimeout is how long the reader waits for the next replayed
	// frame before concluding the replay has drained. It is a FAILURE bound, not
	// a pacing delay: a store mid-replay writes back-to-back.
	DefaultIdleTimeout = 5 * time.Second
	// DefaultDialTimeout bounds the UDS connect.
	DefaultDialTimeout = 5 * time.Second
)

// ErrTruncated reports that a re-pull hit one of its bounds before reaching
// StopAtSeq, so the caller received only part of the history it asked for.
var ErrTruncated = errors.New("storesub: re-pull truncated before reaching the retained window")

// Config binds a Client to a store socket. Zero-value bounds use the defaults.
type Config struct {
	// SocketPath is the shim-store UDS path. Required.
	SocketPath string
	// MaxEvents / IdleTimeout / DialTimeout bound one re-pull.
	MaxEvents   int
	IdleTimeout time.Duration
	DialTimeout time.Duration
	// Logf is the daemon logger. Nil discards.
	Logf dlog.Logf
	// dial is injected by tests; production dials the unix socket.
	dial func(ctx context.Context, path string) (net.Conn, error)
}

// Client re-pulls history slices from the store. Stateless between calls: each
// Replay opens and closes its own connection, so nothing about one re-pull can
// outlive it.
type Client struct {
	cfg  Config
	logf dlog.Logf
}

// New builds a Client. An empty SocketPath is a construction error rather than
// a dial failure discovered per request.
func New(cfg Config) (*Client, error) {
	if cfg.SocketPath == "" {
		return nil, fmt.Errorf("storesub: New needs a store socket path")
	}
	if cfg.MaxEvents <= 0 {
		cfg.MaxEvents = DefaultMaxEvents
	}
	if cfg.IdleTimeout <= 0 {
		cfg.IdleTimeout = DefaultIdleTimeout
	}
	if cfg.DialTimeout <= 0 {
		cfg.DialTimeout = DefaultDialTimeout
	}
	if cfg.Logf == nil {
		cfg.Logf = func(string, ...any) {}
	}
	if cfg.dial == nil {
		cfg.dial = dialUnix
	}
	return &Client{cfg: cfg, logf: dlog.Tag(cfg.Logf, "component", "storesub")}, nil
}

func dialUnix(ctx context.Context, path string) (net.Conn, error) {
	var d net.Dialer
	return d.DialContext(ctx, "unix", path)
}

// DefaultSocketPath resolves the shim-store socket the way every other
// agent-shim component does: $XDG_CACHE_HOME/agent-repl/sock/store.sock, else
// ~/.cache/agent-repl/sock/store.sock. It mirrors the shim's
// `defaultStoreSocket()` and shim-store's own `defaultCacheDir()`; the three
// must agree or the daemon dials a socket nothing is listening on.
func DefaultSocketPath() (string, error) {
	if d := os.Getenv("XDG_CACHE_HOME"); d != "" {
		return filepath.Join(d, "agent-repl", "sock", "store.sock"), nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("storesub: resolving home for the default store socket: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "sock", "store.sock"), nil
}

// Replay subscribes at fromSeq (EXCLUSIVE) and hands each replayed event to
// onEvent, stopping at the first event whose seq is >= stopAtSeq (that seq and
// everything after it is already covered by the caller's own window).
//
// vendorSessionID MUST be the vendor session uuid — the store's key space (see
// the package doc).
//
// It returns the number of events delivered. A bound tripped before reaching
// stopAtSeq wraps ErrTruncated, so a partial re-pull is reported rather than
// passed off as complete. A stopAtSeq of 0 means "no upper bound from the
// caller's window": the pull then ends on idle or the event cap.
func (c *Client) Replay(ctx context.Context, vendorSessionID string, fromSeq, stopAtSeq uint64, onEvent func(*corev1.Event)) (int, error) {
	if vendorSessionID == "" {
		return 0, fmt.Errorf("storesub: Replay needs a vendor session id (the store keys events by it)")
	}
	if onEvent == nil {
		return 0, fmt.Errorf("storesub: Replay needs an onEvent sink")
	}

	dialCtx, cancelDial := context.WithTimeout(ctx, c.cfg.DialTimeout)
	conn, err := c.cfg.dial(dialCtx, c.cfg.SocketPath)
	cancelDial()
	if err != nil {
		return 0, fmt.Errorf("storesub: dialing store at %s: %w", c.cfg.SocketPath, err)
	}
	defer conn.Close()

	// A cancelled ctx must unblock a read parked on the socket; closing the
	// connection is what does that for a plain net.Conn.
	watchDone := make(chan struct{})
	defer close(watchDone)
	go func() {
		select {
		case <-ctx.Done():
			conn.Close()
		case <-watchDone:
		}
	}()

	if err := writeMsg(conn, &corev1.Subscribe{SessionId: vendorSessionID, FromSeq: fromSeq}); err != nil {
		return 0, fmt.Errorf("storesub: sending Subscribe (session=%s from_seq=%d): %w", vendorSessionID, fromSeq, err)
	}
	c.logf("re-pull subscribed session=%s from_seq=%d stop_at_seq=%d socket=%s",
		vendorSessionID, fromSeq, stopAtSeq, c.cfg.SocketPath)

	delivered := 0
	for {
		if err := conn.SetReadDeadline(time.Now().Add(c.cfg.IdleTimeout)); err != nil {
			return delivered, fmt.Errorf("storesub: setting read deadline: %w", err)
		}
		msg, err := readMsg(conn)
		switch {
		case err == nil:
		case errors.Is(err, os.ErrDeadlineExceeded):
			c.logf("re-pull session=%s ended: no frame for %s (replay drained) after %d event(s)",
				vendorSessionID, c.cfg.IdleTimeout, delivered)
			if stopAtSeq == 0 {
				// No upper bound was asked for, so a drained replay IS the whole
				// answer — the only honest end this call could have had.
				return delivered, nil
			}
			return delivered, fmt.Errorf("%w: idle after %d event(s) (from_seq=%d stop_at_seq=%d)",
				ErrTruncated, delivered, fromSeq, stopAtSeq)
		case errors.Is(err, io.EOF), errors.Is(err, net.ErrClosed):
			if ctxErr := ctx.Err(); ctxErr != nil {
				return delivered, fmt.Errorf("%w: %w after %d event(s)", ErrTruncated, ctxErr, delivered)
			}
			c.logf("re-pull session=%s ended: store closed the subscription after %d event(s)", vendorSessionID, delivered)
			return delivered, fmt.Errorf("%w: store closed the subscription after %d event(s)", ErrTruncated, delivered)
		default:
			return delivered, fmt.Errorf("storesub: reading replay frame (session=%s): %w", vendorSessionID, err)
		}

		ev, ok := msg.(*corev1.Event)
		if !ok {
			// The store sends nothing else on a subscriber connection. A frame of
			// another type means the protocol drifted; say so and stop rather than
			// silently reading past it.
			return delivered, fmt.Errorf("storesub: replay frame is %T, expected core.v1.Event (session=%s)", msg, vendorSessionID)
		}
		if stopAtSeq != 0 && ev.GetSeq() >= stopAtSeq {
			c.logf("re-pull session=%s complete: reached the retained window at seq=%d after %d event(s)",
				vendorSessionID, ev.GetSeq(), delivered)
			return delivered, nil
		}
		onEvent(ev)
		delivered++
		if delivered >= c.cfg.MaxEvents {
			c.logf("re-pull session=%s TRUNCATED at the %d-event cap (from_seq=%d stop_at_seq=%d); older history stays unrecovered",
				vendorSessionID, c.cfg.MaxEvents, fromSeq, stopAtSeq)
			return delivered, fmt.Errorf("%w: hit the %d-event cap (from_seq=%d stop_at_seq=%d)",
				ErrTruncated, c.cfg.MaxEvents, fromSeq, stopAtSeq)
		}
	}
}

// --- Any framing (the convention every agent-shim UDS hop uses) -------------

func writeMsg(conn net.Conn, m proto.Message) error {
	a, err := anypb.New(m)
	if err != nil {
		return fmt.Errorf("storesub: wrapping %T in Any: %w", m, err)
	}
	b, err := proto.Marshal(a)
	if err != nil {
		return fmt.Errorf("storesub: marshaling Any(%T): %w", m, err)
	}
	return wire.WriteFrame(conn, b)
}

func readMsg(conn net.Conn) (proto.Message, error) {
	frame, err := wire.ReadFrame(conn)
	if err != nil {
		return nil, err
	}
	a := &anypb.Any{}
	if err := proto.Unmarshal(frame, a); err != nil {
		return nil, fmt.Errorf("storesub: decoding Any frame: %w", err)
	}
	m, err := a.UnmarshalNew()
	if err != nil {
		return nil, fmt.Errorf("storesub: resolving Any type %q: %w", a.GetTypeUrl(), err)
	}
	return m, nil
}
