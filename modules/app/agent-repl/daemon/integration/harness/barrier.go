package harness

import (
	"agent-repld/integration/grantedclock"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/rollout"
	"context"
	"fmt"
	"strconv"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// THE TEST BARRIER. A negative assertion -- "no push followed", "no StartTurn
// was sent", "the file did not change" -- waits on the daemon's own report
// that nothing it started is still in flight (AwaitIdle, internal/inflight),
// never on a window. Every stream the harness opens is TAGGED, and the
// barrier's answer names how many frames the daemon wrote on each tagged
// stream, so the assertion first waits until its stream has RECEIVED that
// many: a frame written is not yet a frame read.

// streamTagHeader is the request header a tagged stream is opened with
// (server.StreamTagHeader).
const streamTagHeader = "Agent-Repl-Stream-Tag"

// streamTagKey carries a stream's tag from runStream to the client
// interceptor that sends it.
type streamTagKey struct{}

// daemonKey carries the Daemon a stream was opened against, so a stream can
// ask its own daemon for the barrier.
type daemonKey struct{}

// streamTags mints the process-unique tags.
var streamTags atomic.Uint64

// newStreamTag mints a stream's tag.
func newStreamTag() string { return "itest-" + strconv.FormatUint(streamTags.Add(1), 10) }

// tagInterceptor sends a tagged context's stream tag as a request header.
type tagInterceptor struct{}

func (tagInterceptor) WrapUnary(next connect.UnaryFunc) connect.UnaryFunc { return next }

func (tagInterceptor) WrapStreamingClient(next connect.StreamingClientFunc) connect.StreamingClientFunc {
	return func(ctx context.Context, spec connect.Spec) connect.StreamingClientConn {
		conn := next(ctx, spec)
		if tag, ok := ctx.Value(streamTagKey{}).(string); ok {
			conn.RequestHeader().Set(streamTagHeader, tag)
		}
		return conn
	}
}

func (tagInterceptor) WrapStreamingHandler(next connect.StreamingHandlerFunc) connect.StreamingHandlerFunc {
	return next
}

// received counts one stream's frames as the harness reads them, waking
// every wait for a count.
type received struct {
	mu   sync.Mutex
	n    uint64
	next chan struct{}
}

// add counts one frame.
func (r *received) add() {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.n++
	if r.next != nil {
		close(r.next)
		r.next = nil
	}
}

// await waits until at least n frames were read, or ctx ends.
func (r *received) await(ctx context.Context, n uint64) error {
	for {
		r.mu.Lock()
		got := r.n
		if got >= n {
			r.mu.Unlock()
			return nil
		}
		if r.next == nil {
			r.next = make(chan struct{})
		}
		next := r.next
		r.mu.Unlock()
		select {
		case <-next:
		case <-ctx.Done():
			return fmt.Errorf("read %d of the %d frames the daemon wrote: %w", got, n, ctx.Err())
		}
	}
}

// AwaitIdle is the test barrier: it answers once nothing the daemon started
// is in flight, with every tagged stream's frames written, failing t when
// the daemon refuses or the wait's bound ends first.
func (d *Daemon) AwaitIdle() map[string]uint64 {
	d.t.Helper()
	wait, cancel := d.waitCtx()
	defer cancel()
	// A JOINING daemon has no harness client of its own (Client), and its
	// barrier is still its own: it is asked over a connection dialed for it.
	client := d.client
	if client == nil {
		client = d.dialJoining(wait)
	}
	return askIdle(d.t, wait, d.waitBound, client)
}

// dialJoining answers a client for a JOINING daemon, which advertises no
// daemon.addr: its address is the one it reported for the incumbent that
// spawned it (rollout.ReportJoiningAddr), which this harness waits for.
func (d *Daemon) dialJoining(wait context.Context) agentreplv1connect.AgentReplClient {
	d.t.Helper()
	path := rollout.JoiningAddrPath(d.StateDir)
	var addr string
	check := func() bool {
		got, reported, err := rollout.ReadJoiningAddr(d.StateDir)
		if err != nil {
			d.t.Fatalf("harness: read the joining daemon's address: %v", err)
		}
		addr = got
		return reported
	}
	if !awaitChange(d.t, wait, check, followFiles(path)) {
		d.t.Fatalf("harness: the joining daemon never reported its address in %s: %v", path, grantedclock.Explain(wait))
	}
	return agentreplv1connect.NewAgentReplClient(h2cclient.Client(), "http://"+addr)
}

// AwaitIdleAt is the test barrier asked of the daemon CLIENT reaches -- a
// handover's successor, which is no harness Daemon -- bounded by one wait off
// CTX.
func AwaitIdleAt(t testing.TB, ctx context.Context, client agentreplv1connect.AgentReplClient) map[string]uint64 {
	t.Helper()
	wait, cancel := grantedclock.WithBound(ctx, DefaultTimeout)
	defer cancel()
	return askIdle(t, wait, DefaultTimeout, client)
}

// askIdle asks CLIENT for the barrier within WAIT, whose bound is BOUND, and
// answers each tagged stream's frames written.
//
// THE DAEMON'S BOUND IS INSIDE THIS WAIT'S, so a barrier that cannot settle
// is answered still_busy, naming what is in flight, rather than cut off by
// this side's deadline.
func askIdle(t testing.TB, wait context.Context, bound time.Duration, client agentreplv1connect.AgentReplClient) map[string]uint64 {
	t.Helper()
	inner := bound - bound/5
	resp, err := client.AwaitIdle(wait, connect.NewRequest(&agentreplv1.AwaitIdleRequest{BoundMs: uint32(inner.Milliseconds())}))
	if err != nil {
		t.Fatalf("harness: the test barrier failed: %v", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("harness: the test barrier was refused: %v", resp.Msg.GetError())
	}
	out := make(map[string]uint64, len(success.GetStreams()))
	for _, s := range success.GetStreams() {
		out[s.GetStreamTag()] = s.GetFrames()
	}
	return out
}

// settle waits, for a stream opened against a daemon, until the daemon is
// idle and the stream has read every frame the daemon wrote on it: after it,
// whatever the stream has not yet delivered is ALL that will ever come of
// what the test did.
func (s *Stream[T]) settle(t testing.TB) {
	t.Helper()
	if s.daemon == nil {
		t.Fatalf("harness: a negative assertion on a stream opened against no daemon")
	}
	written := s.daemon.AwaitIdle()
	wait, cancel := s.daemon.waitCtx()
	defer cancel()
	if err := s.read.await(wait, written[s.tag]); err != nil {
		t.Fatalf("harness: stream %s: %v", s.tag, err)
	}
}
