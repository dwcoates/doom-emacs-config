package server

import (
	"context"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"

	"connectrpc.com/connect"
)

// openBound is how long a watch may take to become reachable. It is a FAILURE
// BOUND, never a synchronization primitive: the stream itself is what the test
// waits on, and this only stops an unopenable tail from hanging the suite.
// 1s: this package has no real subprocess or socket I/O (an in-process
// httptest.Server over a fake Store), and its observed healthy max is 0.01s
// even under -race; 1s keeps two orders of magnitude of margin for scheduler
// and GC jitter while cutting the original bound 10x.
const openBound = 1 * time.Second

// TestWatchIsReachableBeforeItsFirstLine is the deadlock this file exists to
// prevent: with nothing to replay, the tail must still open, because the
// producer of its first line cannot write until the call that opens it returns.
func TestWatchIsReachableBeforeItsFirstLine(t *testing.T) {
	// Arrange: no replay at all, and a write that will produce one page line.
	store := newFakeStore()
	store.writeResult = WriteResult{Written: 1, Lines: []LineWritten{line("a1", "p-live", 7)}}
	h := newHarness(t, store, 8)
	token := openSession(t, h, "a1")
	ctx, cancel := context.WithTimeout(context.Background(), openBound)

	// Act: the HTTP/1.1 client is the strict case — no headers, no response.
	stream, err := h.client.WatchAgentSession(ctx, connect.NewRequest(&storev1.WatchAgentSessionRequest{
		Watch: &storev1.AgentSessionToken{Value: token},
	}))
	if err != nil {
		cancel()
		t.Fatalf("WatchAgentSession before its first line = %v, want an open stream", err)
	}
	// A standing tail never ends on its own, and a Connect client's Close
	// DRAINS the body — so the cancellation has to come first or the teardown
	// would sit here until the bound expired.
	defer func() {
		cancel()
		_ = stream.Close()
	}()
	writeOne(t, h)

	// Assert: the line written after the watch opened reaches the watcher.
	if !stream.Receive() {
		t.Fatalf("the stream ended before delivering the live line: %v", stream.Err())
	}
	if got := stream.Msg().GetLine().GetAt().GetValue(); got != "p-live" {
		t.Errorf("pointer = %q, want %q", got, "p-live")
	}
}

// TestUnflushableWatchIsRefusedRatherThanHung: a transport that cannot flush
// would leave the caller blocked until its deadline, so the store refuses.
func TestUnflushableWatchIsRefusedRatherThanHung(t *testing.T) {
	// Arrange: the handler mounted with no flusher on its context at all,
	// which is what an unflushable writer leaves behind.
	store := newFakeStore()
	h := newHarness(t, store, 8)

	// Act.
	err := h.server.openStream(context.Background(), h.server.log, "store.rpc.watch-agent-session")

	// Assert.
	if got := connect.CodeOf(err); got != connect.CodeInternal {
		t.Fatalf("code = %v, want %v (err %v)", got, connect.CodeInternal, err)
	}
	if _, ok := findRecord(t, h.logs, "store.rpc.watch-agent-session", "error"); !ok {
		t.Errorf("records = %+v, want the unflushable stream recorded at error", records(t, h.logs))
	}
}
