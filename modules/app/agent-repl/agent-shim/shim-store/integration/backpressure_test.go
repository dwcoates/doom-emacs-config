// backpressure_test.go — SUBJECT 9: a watcher the store cannot keep up with.
//
// The per-subscriber buffer is bounded. A watcher that overruns it is ENDED
// with a Connect error and a WARNING is logged: the store never silently drops
// frames into a stream the caller believes is complete. Recovery is a re-open
// with known_through, which is exactly what a dropped watcher already does.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"fmt"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// smallWatchBuffer is deliberately far below the production default so the
// overrun is reachable from a test without writing a production-sized burst.
const smallWatchBuffer = 16

// TestSlowWatcherExceedingTheBufferIsEndedWithAnError.
func TestSlowWatcherExceedingTheBufferIsEndedWithAnError(t *testing.T) {
	// Arrange: a watcher that never reads, and far more frames than it buffers.
	store := startStore(t, storeOptions{watchBuffer: smallWatchBuffer})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	seedBook(ctx, t, shim, "main", "overrun")
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)
	mark := store.logMark()

	// Act.
	overrun := smallWatchBuffer * 40
	entries := make([]*storev1.StoreEntry, 0, overrun)
	for i := 0; i < overrun; i++ {
		label := fmt.Sprintf("burst-%d", i)
		entries = append(entries, shim.agentEntry(
			"w-"+label, "u-"+label,
			frameLine(agentID("main"), responseFrame("main", "act-"+label, label)),
		))
	}
	shim.write(ctx, t, entries...)

	// Assert: the stream ends loudly, and the store said so in its log.
	assertWatchExhausted(t, awaitStreamEnd(t, stream))

	// THE WARNING MUST BE THE RIGHT ONE. `len(warnings) != 0` accepted any warn
	// the store happened to emit — a reclaimed socket, a slow query, the
	// not-implemented workflow notice — so the assertion passed without the
	// overflow record ever existing.
	overflows := recordsAtOperation(store.logRecordsAfter(mark), "store.fanout.overflow")
	if len(overflows) != 1 {
		t.Fatalf("store.fanout.overflow records = %d, want exactly 1: %v", len(overflows), overflows)
	}
	rec := overflows[0]
	if rec.Level != "warn" {
		t.Errorf("the overflow record is level %q, want warn", rec.Level)
	}
	if rec.Context["book_agent_id"] != "main" {
		t.Errorf("the overflow record names book %v, want main", rec.Context["book_agent_id"])
	}
	if hash, ok := rec.Context["watch_token_hash"].(string); !ok || hash == "" {
		t.Errorf("the overflow record carries no watch_token_hash: %v", rec.Context)
	}
	if !strings.Contains(rec.Message, "dropped=") {
		t.Errorf("the overflow record does not say how much was dropped: %q", rec.Message)
	}
}

// TestTheDefaultBufferAbsorbsALargeBurstWithoutEndingAWatcher is the other side
// of backpressure: the shipped buffer is SUBSTANTIALLY above what a daemon
// bounce can burst, because the alternative to buffering a burst is ending a
// healthy watch. A store that overflowed here would be dropping consumers
// during ordinary catch-up.
func TestTheDefaultBufferAbsorbsALargeBurstWithoutEndingAWatcher(t *testing.T) {
	// Arrange: the store's own default buffer, not a test-shrunk one.
	//
	// THE BOUNDS HERE ARE THIS SITE'S OWN. A 4096-entry WriteBatch and a
	// 4096-frame delivery are the heaviest calls in the package. Their distinct
	// bounds must not share one expiring context: the stream has to remain live
	// for the bounded write and its own bounded delivery. See burstCallTimeout.
	store := startStore(t, storeOptions{})
	setupCtx, cancelSetup := callContext(t)
	defer cancelSetup()
	cli := store.client()
	shim := streamProducer(cli)
	seedBook(setupCtx, t, shim, "main", "default-buffer")
	opened := openSession(setupCtx, t, cli, "main", 10, nil)
	streamCtx, cancelStream := callContextWithin(t, callTimeout+burstCallTimeout+burstStreamTimeout)
	defer cancelStream()
	stream := watchStream(streamCtx, t, cli, opened.GetWatch())
	defer testclose.OrFail(t, stream)
	mark := store.logMark()

	// Act. The burst is half DefaultWatchBuffer, so absorbing it is the
	// buffer's contract and not a race the test hopes to win.
	const burst = 4096
	entries := make([]*storev1.StoreEntry, 0, burst)
	for i := 0; i < burst; i++ {
		label := fmt.Sprintf("burst-%d", i)
		entries = append(entries, shim.agentEntry("w-"+label, "u-"+label,
			frameLine(agentID("main"), responseFrame("main", "act-"+label, label))))
	}
	writeCtx, cancelWrite := callContextWithin(t, burstCallTimeout)
	defer cancelWrite()
	shim.write(writeCtx, t, entries...)

	// Assert: every frame arrives, and the store never gave up on the watcher.
	got := receiveLinesWithin(t, stream, burst, burstStreamTimeout)
	if len(got) != burst {
		t.Fatalf("the watcher received %d frames, want %d", len(got), burst)
	}
	if overflows := recordsAtOperation(store.logRecordsAfter(mark), "store.fanout.overflow"); len(overflows) != 0 {
		t.Fatalf("the default buffer overflowed on a %d-line burst: %v", burst, overflows)
	}
}

// TestOverrunWatcherRecoversByReopening: the store's own recovery story holds
// after it gave up on a subscriber.
func TestOverrunWatcherRecoversByReopening(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{watchBuffer: smallWatchBuffer})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	seedBook(ctx, t, shim, "main", "reopen")
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	overrun := smallWatchBuffer * 40
	entries := make([]*storev1.StoreEntry, 0, overrun)
	for i := 0; i < overrun; i++ {
		label := fmt.Sprintf("burst-%d", i)
		entries = append(entries, shim.agentEntry(
			"w-"+label, "u-"+label,
			frameLine(agentID("main"), responseFrame("main", "act-"+label, label)),
		))
	}
	shim.write(ctx, t, entries...)
	assertWatchExhausted(t, awaitStreamEnd(t, stream))
	if err := stream.Close(); err != nil {
		t.Fatalf("closing the ended stream: %v", err)
	}

	// Act.
	reopened := openSession(ctx, t, cli, "main", 1, nil)
	recovered := watchStream(ctx, t, cli, reopened.GetWatch())
	defer testclose.OrFail(t, recovered)
	shim.write(ctx, t,
		shim.agentEntry("w-after-overrun", "u-after-overrun",
			frameLine(agentID("main"), responseFrame("main", "act-after", "after the overrun"))),
	)

	// Assert.
	assertTexts(t, "the recovered tail", receivedTexts(receiveLines(t, recovered, 1)), []string{"after the overrun"})
	assertTexts(t, "the re-opened page", pageTexts(reopened.GetPage()), []string{fmt.Sprintf("burst-%d", overrun-1)})
}

// TestASlowBashWatcherIsEndedWithResourceExhausted is the same backpressure
// contract on the run stream, which has its own registry and its own buffer.
//
// A run's rows are a spool being concatenated, so silently dropping frames into
// a stream the caller believes is complete would hand it output with a hole in
// it and no way to notice. The store ends the subscriber loudly instead, and
// the recovery is the same re-open the book's watcher makes.
func TestASlowBashWatcherIsEndedWithResourceExhausted(t *testing.T) {
	// Arrange: a run that is still open, and a watcher that never reads past
	// its replay.
	store := startStore(t, storeOptions{watchBuffer: smallWatchBuffer})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.write(ctx, t, sidecar.agentEntry("w-slow-start", "bash:run-slow:start",
		bashRun(nil, "run-slow", bashStart("make test", 1000))))
	stream := watchBashRun(ctx, t, cli, "run-slow")
	defer testclose.OrFail(t, stream)
	assertTexts(t, "the replay", receiveBashRows(t, stream, 1), []string{"start:make test"})
	mark := store.logMark()

	// Act: far more tail writes than the subscriber buffers — each supersedes
	// the run's one tail row, and each is a row the watcher is owed.
	overrun := smallWatchBuffer * 40
	entries := make([]*storev1.StoreEntry, 0, overrun)
	for i := 0; i < overrun; i++ {
		entries = append(entries, sidecar.agentEntry(
			fmt.Sprintf("w-slow-t%d", i), "bash:run-slow:tail",
			bashRun(nil, "run-slow", bashTail(fmt.Sprintf("chunk-%d", i))),
		))
	}
	sidecar.write(ctx, t, entries...)

	// Assert: the stream ends loudly, and the store recorded exactly why.
	assertWatchExhausted(t, awaitBashRunEnd(t, stream))
	overflows := recordsAtOperation(store.logRecordsAfter(mark), "store.fanout.overflow")
	if len(overflows) != 1 {
		t.Fatalf("store.fanout.overflow records = %d, want exactly 1: %v", len(overflows), overflows)
	}
	rec := overflows[0]
	if rec.Level != "warn" {
		t.Errorf("the overflow record is level %q, want warn", rec.Level)
	}
	if rec.Context["task_id"] != "run-slow" {
		t.Errorf("the overflow record names run %v, want run-slow", rec.Context["task_id"])
	}
	if !strings.Contains(rec.Message, "dropped=") {
		t.Errorf("the overflow record does not say how much was dropped: %q", rec.Message)
	}
}
