// bashwatch_test.go — SUBJECT: WatchBashRun, the one stream in this service
// with a NATURAL END.
//
// A detached shell run's output is ONE rendered-tail row every write
// supersedes (owner ruling 2026-09-23: output beyond what is rendered is not
// stored), beside its start and its terminal. WatchBashRun is the one read
// path through which those rows reach a consumer: every stored row in the
// run's own first-insert order, then every write as it lands, ending after the
// terminal. A book never ends; a run does, and the store says so by closing
// the stream.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"context"
	"fmt"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// writeBashRun writes a run's start plus one tail write per window, keyed the
// way the real producers key them (`bash:<run>:start`, `bash:<run>:tail`). The
// store never parses a key.
func writeBashRun(ctx context.Context, t *testing.T, p *producer, run string, windows ...string) {
	t.Helper()
	entries := []*storev1.StoreEntry{p.agentEntry("w-"+run+"-start", "bash:"+run+":start", bashRun(nil, run, bashStart("make test", 1000)))}
	for i, window := range windows {
		entries = append(entries, p.agentEntry(
			fmt.Sprintf("w-%s-t%d", run, i), "bash:"+run+":tail",
			bashRun(nil, run, bashTail(window)),
		))
	}
	p.write(ctx, t, entries...)
}

func TestWatchBashRunReplaysAStoredRunAndEndsAtItsTerminal(t *testing.T) {
	// Arrange: the run is already over when the watch opens, so the replay is
	// the whole answer — and the tail it replays is the NEWEST window, the
	// one row both writes superseded.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-1", "chunk-0", "chunk-0chunk-1")
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-1-term", "bash:run-1:terminal",
		bashRun(nil, "run-1", bashSuccess("make test", 0))))

	// Act
	stream := watchBashRun(ctx, t, cli, "run-1")
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "tail:chunk-0chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunStreamsEveryTailUpdateOfALiveRun(t *testing.T) {
	// Arrange: the run is open when the watch opens.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-2", "chunk-0")
	stream := watchBashRun(ctx, t, cli, "run-2")
	defer testclose.OrFail(t, stream)
	assertTexts(t, "the replay", receiveBashRows(t, stream, 2), []string{"start:make test", "tail:chunk-0"})

	// Act: the spool grows twice, then the run ends.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-2-t1", "bash:run-2:tail",
		bashRun(nil, "run-2", bashTail("chunk-0chunk-1"))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-2-t2", "bash:run-2:tail",
		bashRun(nil, "run-2", bashTail("chunk-0chunk-1chunk-2"))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-2-term", "bash:run-2:terminal",
		bashRun(nil, "run-2", bashSuccess("make test", 0))))

	// Assert: every supersession reaches the live watcher, then it ENDS.
	assertTexts(t, "the live tail", drainBashRun(t, stream),
		[]string{"tail:chunk-0chunk-1", "tail:chunk-0chunk-1chunk-2", "success"})
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
	// Arrange: the tail is superseded AFTER the terminal was written, which
	// bumps its write ordinal but must not move its position: the run's order
	// is start, tail, terminal wherever the newest write landed.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-3", "chunk-0")
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-3-term", "bash:run-3:terminal",
		bashRun(nil, "run-3", bashSuccess("make test", 0))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-3-t-again", "bash:run-3:tail",
		bashRun(nil, "run-3", bashTail("chunk-0chunk-1"))))

	// Act
	stream := watchBashRun(ctx, t, cli, "run-3")
	defer testclose.OrFail(t, stream)

	// Assert: three rows, the supersession having replaced the tail in place.
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "tail:chunk-0chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestAReplayDrawsTheSameTailTheLiveWatcherLastSaw(t *testing.T) {
	// Arrange: a live watcher follows the run to its end.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-4", "chunk-0")
	live := watchBashRun(ctx, t, cli, "run-4")
	defer testclose.OrFail(t, live)
	receiveBashRows(t, live, 2)
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-4-t1", "bash:run-4:tail",
		bashRun(nil, "run-4", bashTail("chunk-0chunk-1"))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-4-term", "bash:run-4:terminal",
		bashRun(nil, "run-4", bashSuccess("make test", 0))))
	followed := drainBashRun(t, live)

	// Act: a watcher that opens afterwards replays the run.
	replay := watchBashRun(ctx, t, cli, "run-4")
	defer testclose.OrFail(t, replay)
	replayed := drainBashRun(t, replay)

	// Assert: the replay's tail is the live watcher's last one.
	assertTexts(t, "the live watcher", followed, []string{"tail:chunk-0chunk-1", "success"})
	assertTexts(t, "the replay", replayed, []string{"start:make test", "tail:chunk-0chunk-1", "success"})
}

func TestWatchBashRunServesTheJSONCodec(t *testing.T) {
	// Arrange: the streaming rpc is part of the JSON surface too, not only the
	// unary verbs.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	sidecar := fileProducer(store.client())
	writeBashRun(ctx, t, sidecar, "run-5", "chunk-0")
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-5-term", "bash:run-5:terminal",
		bashRun(nil, "run-5", bashSuccess("make test", 0))))

	// Act
	stream := watchBashRun(ctx, t, store.jsonClient(), "run-5")
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the run over JSON", drainBashRun(t, stream),
		[]string{"start:make test", "tail:chunk-0", "success"})
	store.assertNoErrorRecords()
}

func TestWatchBashRunReplaysATailFirstStoredAfterTheTerminal(t *testing.T) {
	// Arrange: the sidecar reached the spool only once the run was already
	// closed, so the tail's row is FIRST INSERTED after the terminal's. A
	// replay that stopped at the terminal would hand the consumer a run that
	// produced less output than it did.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	writeBashRun(ctx, t, sidecar, "run-late")
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-late-term", "bash:run-late:terminal",
		bashRun(nil, "run-late", bashSuccess("make test", 0))))

	// Act: the tail lands after the terminal row.
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-late-t0", "bash:run-late:tail",
		bashRun(nil, "run-late", bashTail("chunk-late"))))
	stream := watchBashRun(ctx, t, cli, "run-late")
	defer testclose.OrFail(t, stream)

	// Assert: every stored row in first-insert order, and the natural end after
	// the LAST of them rather than at the terminal.
	assertTexts(t, "the replayed run", drainBashRun(t, stream),
		[]string{"start:make test", "success", "tail:chunk-late"})
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
	writeBashRun(ctx, t, sidecar, "run-reterm", "chunk-0")
	sidecar.write(ctx, t, sidecar.agentEntry("w-run-reterm-term", "bash:run-reterm:terminal",
		bashRun(nil, "run-reterm", bashSuccess("make test", 0))))
	ended := watchBashRun(ctx, t, cli, "run-reterm")
	defer testclose.OrFail(t, ended)
	assertTexts(t, "the first watcher", drainBashRun(t, ended),
		[]string{"start:make test", "tail:chunk-0", "success"})

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
	writeBashRun(ctx, t, sidecar, "run-reterm-2", "chunk-0")
	sidecar.write(ctx, t, sidecar.agentEntry("w-rt2-term", "bash:run-reterm-2:terminal",
		bashRun(nil, "run-reterm-2", bashSuccess("make test", 0))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-rt2-term-again", "bash:run-reterm-2:terminal",
		bashRun(nil, "run-reterm-2", bashSuccess("make test", 0))))
	stream := watchBashRun(ctx, t, cli, "run-reterm-2")
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the fresh replay after a terminal re-upsert", drainBashRun(t, stream),
		[]string{"start:make test", "tail:chunk-0", "success"})
	store.assertNoErrorRecords()
}

func TestInterleavedPlanesReplayInFirstInsertOrder(t *testing.T) {
	// Arrange: BOTH producers write one run — the shim its start and a
	// swept-up terminal, the sidecar its tail — and they interleave. The run's
	// order is the order its rows were first inserted, whichever plane
	// inserted them.
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
	sidecar.write(ctx, t, sidecar.agentEntry("w-il-t0", "bash:"+run+":tail",
		bashRun(nil, run, bashTail("chunk-0"))))
	shim.write(ctx, t, shim.agentEntry("w-il-term", "bash:"+run+":terminal",
		bashRun(nil, run, bashSuccess("make test", 0))))
	// The file plane supersedes its tail after the other plane's terminal. It
	// must land where the tail has always been.
	sidecar.write(ctx, t, sidecar.agentEntry("w-il-t1", "bash:"+run+":tail",
		bashRun(nil, run, bashTail("chunk-0chunk-1"))))
	stream := watchBashRun(ctx, t, cli, run)
	defer testclose.OrFail(t, stream)

	// Assert
	assertTexts(t, "the interleaved run", drainBashRun(t, stream),
		[]string{"start:make test", "tail:chunk-0chunk-1", "success"})
	store.assertNoErrorRecords()
}

func TestAWriteOfATailPastTheCapIsRefusedAndStoresNothing(t *testing.T) {
	// Arrange: output beyond what is rendered is never stored.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	over := strings.Repeat("y", int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)+1)
	mark := store.logMark()

	// Act
	failure := sidecar.writeExpectingFailure(ctx, t, nil, sidecar.agentEntry("w-over-t0", "bash:run-over:tail",
		bashRun(nil, "run-over", bashTail(over))))

	// Assert: refused on the tail's own field, recorded once at the named
	// site, and no run exists to watch.
	assertWriteInvalidRequest(t, failure, "entries[0].agent_update.bash.frame.tail.text")
	rec := assertExactlyOneNormalRecord(t, recordsAtOperation(store.logRecordsAfter(mark), "store.rpc.write-batch"), "the refused over-cap tail")
	assertRefusalKeys(t, rec, "bash_tail_over_cap", "invalid_request")
	stream := watchBashRun(ctx, t, cli, "run-over")
	defer testclose.OrFail(t, stream)
	assertBashRunRefused(t, stream)
}

func TestARefusedBashRunOpenIsRecordedExactlyOnce(t *testing.T) {
	// Arrange: the two ways a WatchBashRun open is refused. Neither has a
	// failure arm on the wire, so the log record IS the store's account of it —
	// and a reader that cannot tell one refusal from two cannot count either.
	tests := []struct {
		name      string
		run       string
		wantSite  string
		wantKind  string
		wantLevel string
	}{
		// A run with no row yet is the race the announcing shim waits out, so
		// it is the ordinary answer; a run named by nothing is a caller defect.
		{name: "a run the store never saw", run: "never-ran-at-all", wantSite: "unknown_bash_run", wantKind: "not_found", wantLevel: "info"},
		{name: "a run named by nothing", run: "", wantSite: "run_empty", wantKind: "invalid_request", wantLevel: "warn"},
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
			// Counted across every level, then checked for its own: the
			// exactly-once rule holds whatever severity the class carries.
			rec := assertExactlyOneNormalRecordAtLevel(t, store.logRecordsAfter(mark), "a refused bash run open", "info", "warn", "error")
			assertRefusalKeys(t, rec, tc.wantSite, tc.wantKind)
			if rec.Level != tc.wantLevel {
				t.Errorf("the refusal record's level is %q, want %q", rec.Level, tc.wantLevel)
			}
			if rec.Context["rpc"] == nil {
				t.Errorf("the refusal record names no rpc: %v", rec.Context)
			}
		})
	}
}
