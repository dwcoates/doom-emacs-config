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
	"agentrepl/shim-store/internal/testclose"
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
	defer testclose.OrFail(t, stream)

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
	defer testclose.OrFail(t, stream)
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
	defer testclose.OrFail(t, stream)

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
	defer testclose.OrFail(t, stream)

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
	defer testclose.OrFail(t, stream)
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
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the run over JSON", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunReplaysADeltaStoredAfterTheTerminal(t *testing.T) {
	// Arrange: the sidecar reached the last of the spool only once the run was
	// already closed, so the late delta's row is FIRST INSERTED after the
	// terminal's. A replay that stopped at the terminal would hand the consumer
	// a run that produced less output than it did.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-late", 1)
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-late-term", "bash:run-late:terminal",
		bashRun(nil, "run-late", bashSuccess("make test", 0))))

	// Act: the late delta lands after the terminal row.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-late-d9", "bash:run-late:9",
		bashRun(nil, "run-late", bashDelta("chunk-late", 9))))
	stream := watchBashRun(ctx, t, cli, "run-late")
	defer testclose.OrFail(t, stream)

	// Assert: every stored row in first-insert order, and the natural end after
	// the LAST of them rather than at the terminal.
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "success", "delta:chunk-late"})
	store.assertNoErrorRecords()
}

func TestATerminalReUpsertAgainstAnEndedStreamIsAbsorbedSilently(t *testing.T) {
	// Arrange: a producer's retry buffer can re-send the terminal long after
	// the run's watchers are gone. The stream that already ended must not be
	// resurrected, and the row must not be doubled.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-reterm", 1)
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-reterm-term", "bash:run-reterm:terminal",
		bashRun(nil, "run-reterm", bashSuccess("make test", 0))))
	ended := watchBashRun(ctx, t, cli, "run-reterm")
	defer testclose.OrFail(t, ended)
	assertTexts(t, "the first watcher", drainBashRun(t, ended),
		[]string{"start:make test", "delta:chunk-0", "success"})

	// Act: the terminal is written again, under a NEW write_id so the ledger
	// cannot absorb it as a replay.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-reterm-term-again", "bash:run-reterm:terminal",
		bashRun(nil, "run-reterm", bashSuccess("make test", 0))))

	// Assert: the ended stream delivers nothing more.
	assertTexts(t, "the ended stream after a terminal re-upsert", drainBashRun(t, ended), nil)
}

func TestATerminalReUpsertIsServedExactlyOnceInAFreshReplay(t *testing.T) {
	// Arrange: the other half of the re-upsert — one row, not two, for a
	// watcher that opens afterwards.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-reterm-2", 1)
	sidecar.write(ctx, t, sidecar.agentEntry("w-rt2-term", "bash:run-reterm-2:terminal",
		bashRun(nil, "run-reterm-2", bashSuccess("make test", 0))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-rt2-term-again", "bash:run-reterm-2:terminal",
		bashRun(nil, "run-reterm-2", bashSuccess("make test", 0))))
	stream := watchBashRun(ctx, t, cli, "run-reterm-2")
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the fresh replay after a terminal re-upsert", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0", "success"})
	store.assertNoErrorRecords()
}

func TestInterleavedPlanesReplayInFirstInsertOrder(t *testing.T) {
	// Arrange: BOTH producers observe one run — the shim watching the SDK and
	// the sidecar copying the spool — and they interleave. The run's order is
	// the order its rows were first inserted, whichever plane inserted them.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	const run = "run-interleaved"

	// Act
	shim.write(ctx, t, shim.agentEntry("w-il-start", "bash:"+run+":start",
		bashRun(nil, run, bashStart("make test", 1000))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-il-d0", "bash:"+run+":0",
		bashRun(nil, run, bashDelta("chunk-0", 0))))
	shim.write(ctx, t, shim.agentEntry("w-il-d1", "bash:"+run+":1",
		bashRun(nil, run, bashDelta("chunk-1", 1))))
	// The file plane, authoritative for content, supersedes the shim's first
	// delta. It must land where that row has always been.
	sidecar.write(ctx, t, sidecar.agentEntry("w-il-d0-final", "bash:"+run+":0",
		bashRun(nil, run, bashDelta("chunk-0-final", 0))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-il-term", "bash:"+run+":terminal",
		bashRun(nil, run, bashSuccess("make test", 0))))
	stream := watchBashRun(ctx, t, cli, run)
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the interleaved run", drainBashRun(t, stream),
		[]string{"start:make test", "delta:chunk-0-final", "delta:chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestARefusedBashRunOpenIsRecordedExactlyOnce(t *testing.T) {
	// Arrange: the two ways a WatchBashRun open is refused. Neither has a
	// failure arm on the wire, so the log record IS the store's account of it —
	// and a reader that cannot tell one refusal from two cannot count either.
	tests := []struct {
		name     string
		run      string
		wantSite string
		wantKind string
	}{
		{name: "a run the store never saw", run: "never-ran-at-all", wantSite: "unknown_bash_run", wantKind: "invalid_request"},
		{name: "a run named by nothing", run: "", wantSite: "run_empty", wantKind: "invalid_request"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			mark := store.logMark()

			// Act.
			stream := watchBashRun(ctx, t, store.client(), tc.run)
			defer testclose.OrFail(t, stream)
			_ = awaitBashRunEnd(t, stream)

			// Assert.
			rec := assertExactlyOneNormalRecord(t, store.logRecordsAfter(mark), "a refused bash run open")
			assertRefusalKeys(t, rec, tc.wantSite, tc.wantKind)
			if rec.Context["rpc"] == nil {
				t.Errorf("the refusal record names no rpc: %v", rec.Context)
			}
		})
	}
}
