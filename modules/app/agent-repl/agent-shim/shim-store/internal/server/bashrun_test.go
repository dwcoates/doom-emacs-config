package server

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"

	"connectrpc.com/connect"
)

// ---- bash-run fixtures ----

func bashRow(run string, terminal bool) BashRowWritten {
	frame := &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{}}}
	if terminal {
		frame = &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}}}
	}
	return BashRowWritten{
		RunID: run,
		Row: &storev1.StoreAgentBash{
			Run:   &conversationv1.AgentActivityId{Value: run},
			Frame: frame,
		},
	}
}

// bashWatcher is one WatchBashRun call driven off its own goroutine, because a
// Connect server stream's call does not return until the server writes its
// response headers.
type bashWatcher struct {
	streamc chan *connect.ServerStreamForClient[storev1.WatchBashRunResponse]
	errc    chan error
}

func startBashWatch(h *harness, ctx context.Context, run string) *bashWatcher {
	w := &bashWatcher{
		streamc: make(chan *connect.ServerStreamForClient[storev1.WatchBashRunResponse], 1),
		errc:    make(chan error, 1),
	}
	go func() {
		stream, err := h.stream.WatchBashRun(ctx, connect.NewRequest(&storev1.WatchBashRunRequest{
			Run: &conversationv1.AgentActivityId{Value: run},
		}))
		if err != nil {
			w.errc <- err
			return
		}
		w.streamc <- stream
	}()
	return w
}

func (w *bashWatcher) open(t *testing.T) *connect.ServerStreamForClient[storev1.WatchBashRunResponse] {
	t.Helper()
	select {
	case stream := <-w.streamc:
		t.Cleanup(func() { closeOrFail(t, stream) })
		return stream
	case err := <-w.errc:
		t.Fatalf("WatchBashRun = %v, want a stream", err)
		return nil
	}
}

func (w *bashWatcher) refusal(t *testing.T) error {
	t.Helper()
	select {
	case err := <-w.errc:
		return err
	case stream := <-w.streamc:
		defer closeOrFail(t, stream)
		for stream.Receive() {
		}
		return stream.Err()
	}
}

// ---- subjects ----

func TestWatchBashRunRefusesAnEmptyRunIdentity(t *testing.T) {
	// Arrange. A malformed ADDRESS is not an unknown run: the caller must fix
	// the request rather than conclude the run does not exist.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	w := startBashWatch(h, context.Background(), "")

	// Assert.
	err := w.refusal(t)
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want %v (error: %v)", connect.CodeOf(err), connect.CodeInvalidArgument, err)
	}
}

func TestWatchBashRunRefusesARunTheStoreHoldsNoRowFor(t *testing.T) {
	// Arrange. An unstored run is a refused OPEN at the transport, the store's
	// convention for every watch.
	h := newHarness(t, newFakeStore(), 0)

	// Act.
	w := startBashWatch(h, context.Background(), "run-1")

	// Assert.
	err := w.refusal(t)
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("code = %v, want %v (error: %v)", connect.CodeOf(err), connect.CodeNotFound, err)
	}
}

func TestWatchBashRunEndsAfterReplayingAnAlreadyStoredTerminal(t *testing.T) {
	// Arrange. The run is already over, so replaying it is the whole answer and
	// the stream must not stand open on a run that can never speak again.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
		{RunID: "run-1", Row: bashRow("run-1", true).Row, WriteSeq: 2},
	}, PinSeq: 2}
	h := newHarness(t, store, 0)

	// Act.
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	var rows int
	for stream.Receive() {
		rows++
	}

	// Assert.
	if err := stream.Err(); err != nil {
		t.Fatalf("stream error = %v, want a clean natural end", err)
	}
	if rows != 2 {
		t.Fatalf("rows = %d, want 2", rows)
	}
}

func TestWatchBashRunEndsAfterALiveTerminalRow(t *testing.T) {
	// Arrange. The run is open at replay time and concludes on the tail.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
	}, PinSeq: 1}
	h := newHarness(t, store, 0)
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("the replay delivered nothing: %v", stream.Err())
	}

	// Act. The terminal arrives live.
	h.server.publishBashRows(h.server.log, "producer", []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", true).Row, WriteSeq: 2},
	})

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the terminal row was not delivered: %v", stream.Err())
	}
	if stream.Receive() {
		t.Fatal("the stream delivered a row after the terminal; want it ended")
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("stream error = %v, want a clean natural end", err)
	}
}

func TestWatchBashRunDropsARowAtOrBelowItsPin(t *testing.T) {
	// Arrange. The replay already carried it; publishing it again must not
	// double it on the wire.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 5},
	}, PinSeq: 5}
	h := newHarness(t, store, 0)
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("the replay delivered nothing: %v", stream.Err())
	}

	// Act. A stale republish, then a real terminal that ends the stream.
	h.server.publishBashRows(h.server.log, "producer", []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 3},
		{RunID: "run-1", Row: bashRow("run-1", true).Row, WriteSeq: 6},
	})

	// Assert. The very next row is the terminal, so nothing came between.
	if !stream.Receive() {
		t.Fatalf("the terminal row was not delivered: %v", stream.Err())
	}
	if stream.Msg().GetRow().GetFrame().GetSuccess() == nil {
		t.Fatalf("next row = %v, want the terminal", stream.Msg().GetRow().GetFrame())
	}
}

func TestWatchBashRunIgnoresAnotherRunsRows(t *testing.T) {
	// Arrange. A run watcher must never be handed another run's row.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
	}, PinSeq: 1}
	h := newHarness(t, store, 0)
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("the replay delivered nothing: %v", stream.Err())
	}

	// Act.
	h.server.publishBashRows(h.server.log, "producer", []BashRowWritten{
		{RunID: "run-2", Row: bashRow("run-2", true).Row, WriteSeq: 2},
		{RunID: "run-1", Row: bashRow("run-1", true).Row, WriteSeq: 3},
	})

	// Assert. The next row is run-1's own terminal.
	if !stream.Receive() {
		t.Fatalf("run-1's terminal was not delivered: %v", stream.Err())
	}
	if got := stream.Msg().GetRow().GetRun().GetValue(); got != "run-1" {
		t.Fatalf("delivered row run = %q, want run-1", got)
	}
}

func TestWatchBashRunSubscribesBeforeItReplays(t *testing.T) {
	// Arrange. The gapless handoff depends on the subscription existing before
	// the replay query runs, so a row committed during the replay is either
	// replayed, delivered live, or both — never lost.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
	}, PinSeq: 1}
	store.bashRunRelease = make(chan struct{})
	h := newHarness(t, store, 0)

	// Act.
	startBashWatch(h, context.Background(), "run-1")
	<-store.bashRunEntered

	// Assert. BashRun has been entered, so the handler is already subscribed.
	if got := h.server.bashFan.subscribers(); got != 1 {
		t.Fatalf("bash subscribers at replay time = %d, want 1", got)
	}
	close(store.bashRunRelease)
}

func TestWatchBashRunEndsAnOverflowedSubscriberWithAnError(t *testing.T) {
	// Arrange. A watcher that fell behind recovers by re-opening, never by
	// being silently thinned.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
	}, PinSeq: 1}
	h := newHarness(t, store, 1)
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	if !stream.Receive() {
		t.Fatalf("the replay delivered nothing: %v", stream.Err())
	}

	// Act. Far more rows than the buffer of one can hold.
	rows := make([]BashRowWritten, 0, 8)
	for i := 0; i < 8; i++ {
		rows = append(rows, BashRowWritten{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: uint64(10 + i)})
	}
	h.server.publishBashRows(h.server.log, "producer", rows)

	// Assert.
	for stream.Receive() {
	}
	if code := connect.CodeOf(stream.Err()); code != connect.CodeResourceExhausted {
		t.Fatalf("code = %v, want %v (error: %v)", code, connect.CodeResourceExhausted, stream.Err())
	}
}

func TestWatchBashRunReplaysARowStoredAfterTheTerminal(t *testing.T) {
	// Arrange. A delta the sidecar reached only once the spool was already
	// closed is first inserted AFTER the terminal row. Ending at the terminal
	// dropped it, and the consumer concatenated a run that produced less output
	// than it did with no way to tell.
	store := newFakeStore()
	store.bashRun = BashRunReplay{Rows: []BashRowWritten{
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 1},
		{RunID: "run-1", Row: bashRow("run-1", true).Row, WriteSeq: 2},
		{RunID: "run-1", Row: bashRow("run-1", false).Row, WriteSeq: 3},
	}, PinSeq: 3}
	h := newHarness(t, store, 0)

	// Act.
	w := startBashWatch(h, context.Background(), "run-1")
	stream := w.open(t)
	var rows int
	for stream.Receive() {
		rows++
	}

	// Assert.
	if err := stream.Err(); err != nil {
		t.Fatalf("stream error = %v, want a clean natural end", err)
	}
	if rows != 3 {
		t.Fatalf("rows = %d, want every stored row including the one after the terminal", rows)
	}
}
