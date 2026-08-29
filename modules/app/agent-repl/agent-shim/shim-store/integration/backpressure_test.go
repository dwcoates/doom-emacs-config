// backpressure_test.go — SUBJECT 9: a watcher the store cannot keep up with.
//
// The per-subscriber buffer is bounded. A watcher that overruns it is ENDED
// with a Connect error and a WARNING is logged: the store never silently drops
// frames into a stream the caller believes is complete. Recovery is a re-open
// with known_through, which is exactly what a dropped watcher already does.
package integration

import (
	"fmt"
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
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()
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

	warnings := recordsAtLevel(store.logRecordsAfter(mark), "warn")
	if len(warnings) == 0 {
		t.Errorf("ending an overrun watcher logged no warning")
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
	defer recovered.Close()
	shim.write(ctx, t,
		shim.agentEntry("w-after-overrun", "u-after-overrun",
			frameLine(agentID("main"), responseFrame("main", "act-after", "after the overrun"))),
	)

	// Assert.
	assertTexts(t, "the recovered tail", receivedTexts(receiveLines(t, recovered, 1)), []string{"after the overrun"})
	assertTexts(t, "the re-opened page", pageTexts(reopened.GetPage()), []string{fmt.Sprintf("burst-%d", overrun-1)})
}
