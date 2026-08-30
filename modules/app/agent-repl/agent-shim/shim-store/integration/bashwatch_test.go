// bashwatch_test.go — SUBJECT: WatchBashRun, the one stream in this service
// with a NATURAL END.
//
// A detached shell run's output is written as deltas — by the sidecar copying
// the vendor's spool, and by the shim watching the SDK — so the run's history
// has to live in the entry spine under its own indexed key, exactly as a book's
// history does. WatchBashRun is the one read path through which those rows
// reach a consumer: every stored row in the run's own order, then rows as they
// are written, ending after the terminal. A book never ends; a run does, and
// the store says so by closing the stream.
package integration

import (
	"context"
	"fmt"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// writeBashRun writes a run's start plus n deltas, keyed per row the way a real
// producer keys them (`bash:<run>:start`, `bash:<run>:<from_offset>`). The
// store never parses a key.
func writeBashRun(ctx context.Context, t *testing.T, p *producer, run string, deltas int) {
	t.Helper()
	entries := []*storev1.StoreEntry{p.agentEntry("w-"+run+"-start", "bash:"+run+":start", bashRun(nil, run, bashStart("make test", 1000)))}
	for i := 0; i < deltas; i++ {
		entries = append(entries, p.agentEntry(
			fmt.Sprintf("w-%s-d%d", run, i), fmt.Sprintf("bash:%s:%d", run, i),
			bashRun(nil, run, bashDelta(fmt.Sprintf("chunk-%d", i), uint64(i))),
		))
	}
	p.write(ctx, t, entries...)
}

func TestWatchBashRunReplaysAStoredRunAndEndsAtItsTerminal(t *testing.T) {
	// Arrange: the run is already over when the watch opens, so the replay is
	// the whole answer.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-1", 2)
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-1-term", "bash:run-1:terminal",
		bashRun(nil, "run-1", bashSuccess("make test", 0))))

	// Act
	stream := watchBashRun(ctx, t, cli, "run-1")
	defer stream.Close()

	// Assert
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "delta:chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunFollowsALiveRunAndEndsWhenItConcludes(t *testing.T) {
	// Arrange: the run is open when the watch opens.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-2", 1)
	stream := watchBashRun(ctx, t, cli, "run-2")
	defer stream.Close()
	assertTexts(t, "the replay", receiveBashRows(t, stream, 2), []string{"start:make test", "delta:chunk-0"})

	// Act: more spool, then the terminal.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-2-d9", "bash:run-2:9",
		bashRun(nil, "run-2", bashDelta("chunk-live", 9))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-2-term", "bash:run-2:terminal",
		bashRun(nil, "run-2", bashSuccess("make test", 0))))

	// Assert: the tail delivers both and then ENDS on its own.
	assertTexts(t, "the live tail", drainBashRun(t, stream), []string{"delta:chunk-live", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunRefusesARunTheStoreNeverSaw(t *testing.T) {
	// Arrange: there is no failure frame on this rpc by design.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()

	// Act
	stream := watchBashRun(ctx, t, store.client(), "never-ran")
	defer stream.Close()

	// Assert
	assertBashRunRefused(t, stream)
}

func TestWatchBashRunReplaysByFirstInsertOrderNotWriteOrder(t *testing.T) {
	// Arrange: a redelivered delta upserts its own row. The run's spool order is
	// where that row has always been — not the end of the stream — because a
	// consumer concatenating output cannot have a chunk move.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-3", 2)
	// The FIRST delta is written again with a new write_id, which bumps its
	// write ordinal but must not move its position.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-3-d0-again", "bash:run-3:0",
		bashRun(nil, "run-3", bashDelta("chunk-0", 0))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-3-term", "bash:run-3:terminal",
		bashRun(nil, "run-3", bashSuccess("make test", 0))))

	// Act
	stream := watchBashRun(ctx, t, cli, "run-3")
	defer stream.Close()

	// Assert: four rows, the redelivery having replaced one in place.
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "delta:chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunStreamsARedeliveredDeltaExactlyOnce(t *testing.T) {
	// Arrange: the same upsert key written again is ONE row, and a live watcher
	// sees the supersession once — not a second row and not nothing.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-4", 1)
	stream := watchBashRun(ctx, t, cli, "run-4")
	defer stream.Close()
	assertTexts(t, "the replay", receiveBashRows(t, stream, 2), []string{"start:make test", "delta:chunk-0"})

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-4-d0-again", "bash:run-4:0",
		bashRun(nil, "run-4", bashDelta("chunk-0-corrected", 0))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-4-term", "bash:run-4:terminal",
		bashRun(nil, "run-4", bashSuccess("make test", 0))))

	// Assert
	assertTexts(t, "the live tail", drainBashRun(t, stream), []string{"delta:chunk-0-corrected", "success"})
}

func TestWatchBashRunServesTheJSONCodec(t *testing.T) {
	// Arrange: the streaming rpc is part of the JSON surface too, not only the
	// unary verbs.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	sidecar := fileProducer(store.client())
	writeBashRun(ctx, t, sidecar, "run-5", 1)
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-5-term", "bash:run-5:terminal",
		bashRun(nil, "run-5", bashSuccess("make test", 0))))

	// Act
	stream := watchBashRun(ctx, t, store.jsonClient(), "run-5")
	defer stream.Close()

	// Assert
	assertTexts(t, "the run over JSON", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "success"})
	store.assertNoErrorRecords()
}
