package db

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// ---- what the window keeps and what it lets go ----

// TestPruneWriteLedgerKeepsWhatAbsorptionCanStillAskAbout is the retention rule
// itself: a row is free exactly when no producer can re-read the bytes behind
// it. The sidecar's boot rewind reaches at most one bounded window back from a
// file's committed cursor, so a row further behind than that names bytes
// nothing will read again.
func TestPruneWriteLedgerKeepsWhatAbsorptionCanStillAskAbout(t *testing.T) {
	const window = 1000
	tests := []struct {
		name string
		// writtenAt is the cursor offset the row's own batch advanced to.
		writtenAt int64
		// cursorNow is where that file's cursor stands when the sweep runs.
		cursorNow int64
		wantKept  bool
	}{
		{name: "inside the rewind window, still re-readable", writtenAt: 5_000, cursorNow: 5_500, wantKept: true},
		{name: "exactly the window behind, still re-readable", writtenAt: 5_000, cursorNow: 6_000, wantKept: true},
		{name: "one byte past the window", writtenAt: 5_000, cursorNow: 6_001, wantKept: false},
		{name: "a whole corpus behind the cursor", writtenAt: 5_000, cursorNow: 5_000_000, wantKept: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, window)
			writeFileBatch(t, d, "12:34", test.writtenAt, "w1", "u1")
			advanceCursor(t, d, "12:34", test.cursorNow)

			// Act
			result, err := d.PruneWriteLedger(ctx())
			if err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Assert
			kept := ledgerHas(t, d, "w1")
			if kept != test.wantKept {
				t.Fatalf("ledger row kept = %t, want %t (sweep removed %d)", kept, test.wantKept, result.Deleted)
			}
		})
	}
}

// TestPruneWriteLedgerNeverPrunesARowItCannotMeasure pins the conservative
// half. A row with no source position, or one whose file has no cursor at all,
// is exactly a row whose re-read the store cannot bound — and a file with no
// cursor is re-read FROM ZERO, which is when the ledger is doing the most work.
func TestPruneWriteLedgerNeverPrunesARowItCannotMeasure(t *testing.T) {
	const window = 1000
	tests := []struct {
		name string
		// arrange writes one ledger row under write id "w1".
		arrange func(t *testing.T, d *DB)
	}{
		{
			name: "a stream-plane write, which names no file",
			arrange: func(t *testing.T, d *DB) {
				writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
				advanceCursor(t, d, "12:34", 5_000_000)
			},
		},
		{
			name: "a file-plane write in a batch that advanced no cursor",
			arrange: func(t *testing.T, d *DB) {
				entry := pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose())))
				entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
				if _, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entry), nil); err != nil {
					t.Fatalf("WriteBatch: %v", err)
				}
				advanceCursor(t, d, "12:34", 5_000_000)
			},
		},
		{
			name: "a file whose cursor row is gone, so it is re-read from zero",
			arrange: func(t *testing.T, d *DB) {
				writeFileBatch(t, d, "12:34", 5_000, "w1", "u1")
				if _, err := d.sql.Exec(`DELETE FROM cursor WHERE file_id = '12:34'`); err != nil {
					t.Fatalf("removing the cursor row: %v", err)
				}
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, window)
			test.arrange(t, d)

			// Act
			if _, err := d.PruneWriteLedger(ctx()); err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Assert
			if !ledgerHas(t, d, "w1") {
				t.Fatal("the sweep removed a ledger row whose re-read it cannot bound")
			}
		})
	}
}

// TestARereadInsideTheWindowIsStillAbsorbedAfterASweep is the property the whole
// rule exists to preserve. A re-emitted write inside the window must still be
// ABSORBED — not re-applied, which would bump write_seq and re-deliver the row
// to every live watcher at a new ordinal.
func TestARereadInsideTheWindowIsStillAbsorbedAfterASweep(t *testing.T) {
	const window = 1000
	tests := []struct {
		name         string
		cursorNow    int64
		wantAbsorbed int
		wantWritten  int
	}{
		{name: "inside the window: absorbed", cursorNow: 5_500, wantAbsorbed: 1, wantWritten: 0},
		{name: "past the window: the row was pruned, so the replay applies again", cursorNow: 5_000_000, wantAbsorbed: 0, wantWritten: 1},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, window)
			writeFileBatch(t, d, "12:34", 5_000, "w1", "u1")
			seqBefore := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)
			advanceCursor(t, d, "12:34", test.cursorNow)
			if _, err := d.PruneWriteLedger(ctx()); err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Act: the sidecar re-reads the same bytes, minting the same write id.
			result := writeFileBatch(t, d, "12:34", 5_000, "w1", "u1")

			// Assert
			if result.Absorbed != test.wantAbsorbed || result.Written != test.wantWritten {
				t.Fatalf("replay = %d absorbed / %d written, want %d / %d",
					result.Absorbed, result.Written, test.wantAbsorbed, test.wantWritten)
			}
			seqAfter := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)
			if test.wantAbsorbed == 1 && seqAfter != seqBefore {
				t.Fatalf("write_seq moved %d -> %d on an absorbed replay; the row would be re-delivered to every watcher", seqBefore, seqAfter)
			}
		})
	}
}

// ---- the sweep and the producers it shares the write slot with ----

// TestTheSweepGivesTheWriteSlotBackBetweenBatches is what "bounded" means. The
// probe write runs from INSIDE the sweep, after a batch has committed and
// released, so it proves the slot is free mid-sweep rather than only at the
// end. If the sweep ever held the slot across its whole run this does not fail
// slowly, it never returns.
func TestTheSweepGivesTheWriteSlotBackBetweenBatches(t *testing.T) {
	tests := []struct {
		name string
		// prunable is how many ledger rows the sweep has to remove, which is
		// what decides whether it takes one turn of the slot or several.
		prunable int
	}{
		{name: "a single batch", prunable: 3},
		{name: "more rows than one batch removes", prunable: ledgerPruneBatch + 5},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, 1000)
			seedLedgerRows(t, d, "12:34", 5_000, test.prunable)
			advanceCursor(t, d, "12:34", 5_000_000)
			var probes int
			d.afterPruneBatch = func() {
				probes++
				id := "probe-" + strconv.Itoa(probes)
				writeOK(t, d, pageEntry(id, id, "agent-1", frameItem(activityFrame("agent-1", "act-"+id, prose()))))
			}

			// Act
			result, err := d.PruneWriteLedger(ctx())
			if err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Assert
			if result.Deleted != int64(test.prunable) {
				t.Fatalf("sweep removed %d rows, want %d", result.Deleted, test.prunable)
			}
			if probes != result.Batches {
				t.Fatalf("%d probe writes landed across %d sweep batches; every batch must give the slot back", probes, result.Batches)
			}
		})
	}
}

// TestASweepAndAProducerRunConcurrently is the same guarantee from the other
// side: nothing deadlocks and nobody is refused when both are writing.
func TestASweepAndAProducerRunConcurrently(t *testing.T) {
	tests := []struct {
		name    string
		writers int
	}{
		{name: "one producer beside the sweep", writers: 1},
		{name: "several producers beside the sweep", writers: 8},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, s := newPruningStore(t, 1000)
			seedLedgerRows(t, d, "12:34", 5_000, ledgerPruneBatch+5)
			advanceCursor(t, d, "12:34", 5_000_000)
			start := make(chan struct{})
			errs := make([]error, test.writers)
			var sweepErr error
			var wg sync.WaitGroup

			// Act
			wg.Add(1)
			go func() {
				defer wg.Done()
				<-start
				_, sweepErr = d.PruneWriteLedger(ctx())
			}()
			for i := 0; i < test.writers; i++ {
				wg.Add(1)
				go func(i int) {
					defer wg.Done()
					<-start
					id := fmt.Sprintf("probe-%d", i)
					_, errs[i] = d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(
						pageEntry(id, id, "agent-1", frameItem(activityFrame("agent-1", "act-"+id, prose())))), nil)
				}(i)
			}
			close(start)
			wg.Wait()

			// Assert
			if sweepErr != nil {
				t.Fatalf("PruneWriteLedger beside a producer = %v", sweepErr)
			}
			for i, err := range errs {
				if err != nil {
					t.Fatalf("producer %d beside the sweep = %v", i, err)
				}
			}
			assertNoBusyRefusal(t, s)
		})
	}
}

// TestPruneWriteLedgerAnswersItsCallersCancellation keeps the sweep out of the
// error log on an orderly shutdown, and keeps what it already committed.
func TestPruneWriteLedgerAnswersItsCallersCancellation(t *testing.T) {
	// Arrange
	d, _ := newPruningStore(t, 1000)
	writeFileBatch(t, d, "12:34", 5_000, "w1", "u1")
	advanceCursor(t, d, "12:34", 5_000_000)
	sweepCtx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err := d.PruneWriteLedger(sweepCtx)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("PruneWriteLedger = %v, want the caller's own context.Canceled", err)
	}
	if errors.Is(err, ErrStorage) {
		t.Fatalf("PruneWriteLedger = %v, want a cancellation rather than a storage failure", err)
	}
}

// TestANonPositiveRetentionWindowSweepsNothing honors an operator saying "keep
// everything" rather than reading it as "keep nothing".
func TestANonPositiveRetentionWindowSweepsNothing(t *testing.T) {
	// Arrange
	d, _ := newPruningStore(t, -1)
	writeFileBatch(t, d, "12:34", 5_000, "w1", "u1")
	advanceCursor(t, d, "12:34", 5_000_000)

	// Act
	result, err := d.PruneWriteLedger(ctx())

	// Assert
	if err != nil {
		t.Fatalf("PruneWriteLedger: %v", err)
	}
	if result.Deleted != 0 || result.Batches != 0 {
		t.Fatalf("sweep = %+v, want nothing swept", result)
	}
	if !ledgerHas(t, d, "w1") {
		t.Fatal("the disabled sweep removed a ledger row")
	}
}

// ---- harness ----

// newPruningStore opens a store whose retention window is the caller's, so a
// test states a distance in bytes rather than arranging megabytes of fixture.
func newPruningStore(t *testing.T, window int64) (*DB, *sink) {
	t.Helper()
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{
		Now:                  func() int64 { return testNow },
		LedgerRetentionBytes: window,
	})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	return d, s
}

// writeFileBatch writes one FILE-plane page line whose batch advances `fileID`
// to `offset` — the shape every sidecar batch has, and the one that stamps a
// ledger row with a source position.
func writeFileBatch(t *testing.T, d *DB, fileID string, offset int64, writeID, upsertKey string) WriteResult {
	t.Helper()
	entry := pageEntry(writeID, upsertKey, "agent-1", frameItem(activityFrame("agent-1", "act-"+upsertKey, prose())))
	entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	result, err := d.WriteBatch(ctx(), "test-sidecar", WriteInteractive, &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{entry},
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: "/t/a.jsonl", Offset: offset},
	}, nil)
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	return result
}

// advanceCursor moves a file's committed cursor with a cursor-only batch, which
// is exactly how a sidecar that read bytes yielding no entries reports progress.
func advanceCursor(t *testing.T, d *DB, fileID string, offset int64) {
	t.Helper()
	if _, err := d.WriteBatch(ctx(), "test-sidecar", WriteInteractive, &storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: "/t/a.jsonl", Offset: offset},
	}, nil); err != nil {
		t.Fatalf("advancing the cursor: %v", err)
	}
}

// seedLedgerRows inserts n ledger rows for one file directly, because the
// batching cases care about the SWEEP's loop rather than about how the rows got
// there, and driving thousands of real batches through it to prove that costs
// thousands of transactions and proves nothing extra. Every other case writes
// through the real path.
func seedLedgerRows(t *testing.T, d *DB, fileID string, offset int64, n int) {
	t.Helper()
	tx, err := d.sql.Begin()
	if err != nil {
		t.Fatalf("seeding the ledger: %v", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit
	for i := 0; i < n; i++ {
		id := "seed-" + strconv.Itoa(i)
		if _, err := tx.Exec(
			`INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms, source_file_id, source_offset) VALUES (?,?,?,?,?,?)`,
			id, id, i+1, testNow, fileID, offset); err != nil {
			t.Fatalf("seeding the ledger: %v", err)
		}
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("seeding the ledger: %v", err)
	}
}

// ledgerHas reports whether the ledger still carries a write id.
func ledgerHas(t *testing.T, d *DB, writeID string) bool {
	t.Helper()
	return scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = ?`, writeID) == 1
}

// TestASweepBatchAndAProducersBatchTogetherStayWithinTheProducersBudget is the
// sweep's side of the same 400ms budget.
//
// THE SWEEP SHARES THE WRITE SLOT, so a producer's batch can wait one sweep
// batch and then do its own work, and the number the producer is judged on is
// the SUM. Asserting the two halves separately would let a 380ms sweep batch
// and a 380ms write both pass while the producer times at 760ms, so the
// assertion here is the sum against the one budget the store would warn past.
//
// IT IS A DETERMINISTIC SUM RATHER THAN A RACE. Timing the two against each
// other and hoping they overlap proves the bound only on the runs where they
// did; the worst case a producer can meet is exactly "the slowest sweep batch,
// then my own batch", so that is what is measured. That the sweep RELEASES the
// slot between batches — which is what bounds the wait at one batch rather than
// one sweep — is pinned by TestTheSweepGivesTheWriteSlotBackBetweenBatches.
//
// THE PLAN IS WHAT THIS GUARDS. The sweep's delete drove from `write_ledger`
// and could use no index for a bound that lives on `cursor`, so every batch —
// the final empty one included — scanned the whole ledger: 111ms per batch on
// the owner's 318k-row store and 4650ms for a 21-batch sweep, all of it holding
// the slot. See the plan note in prune.go.
func TestASweepBatchAndAProducersBatchTogetherStayWithinTheProducersBudget(t *testing.T) {
	if raceEnabled {
		t.Skip("a wall-clock budget measures the race detector's instrumentation, not the store; see racedetector_on_test.go")
	}

	// Arrange: a full-sized corpus in the state a resident store is actually in
	// — swept before, so only the OLDEST few writes of each file have fallen
	// past the window since. That is the shape that costs: a sweep with a
	// backlog finds its 2000 rows immediately whatever plan it runs, while one
	// with a handful to remove is the case that either seeks to them or reads
	// the whole ledger looking (the owner's 2026-09-13 17:51 sweep removed 870
	// rows in ONE batch and took 1464ms).
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // best-effort test teardown
	spread := seedSyntheticCorpus(t, d)
	d.ledgerRetention = spread - 3*syntheticCorpusOffsetStep
	budget := DefaultBulkBase + 30*DefaultBulkPerRow

	var slowestSweepBatch time.Duration
	batchStarted := time.Now()
	d.afterPruneBatch = func() {
		if elapsed := time.Since(batchStarted); elapsed > slowestSweepBatch {
			slowestSweepBatch = elapsed
		}
		batchStarted = time.Now()
	}

	// Act
	result, err := d.PruneWriteLedger(ctx())
	if err != nil {
		t.Fatalf("PruneWriteLedger: %v", err)
	}
	writeStarted := time.Now()
	if _, err := d.WriteBatch(ctx(), "test-sidecar", WriteInteractive, thirtyRowFileBatch("live", "corpus-file-0", 1<<40), nil); err != nil {
		t.Fatalf("WriteBatch after the sweep: %v", err)
	}
	writeElapsed := time.Since(writeStarted)

	// Assert
	if result.Batches < 2 {
		t.Fatalf("the sweep took %d batches over %d deletes; the fixture must give it more than one turn of the slot",
			result.Batches, result.Deleted)
	}
	if worst := slowestSweepBatch + writeElapsed; worst > budget {
		t.Fatalf("a producer's 30-row batch behind the slowest of %d sweep batches would take %v (%v queued + %v writing), past its %v budget",
			result.Batches, worst, slowestSweepBatch, writeElapsed, budget)
	}
	assertNoBusyRefusal(t, s)
}

// TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt is the defect this
// file's latency case cannot see, stated where it IS visible.
//
// The retention bound is `c.offset - ?`: not a constant, but a column of the
// OTHER table. Driven from `write_ledger`, the bound is unknown until `c` is
// resolved, so `write_ledger_source` is usable for nothing and SQLite reads the
// whole covering index probing `cursor` per row — 111ms per batch on the
// owner's 318k-row ledger, 1464ms on the loaded box for the ONE batch that
// removed 870 rows, and paid again by every later batch of the same sweep and
// by the empty batch that ends it. All of it holds the write slot a producer's
// WriteBatch queues on.
//
// A WALL-CLOCK BOUND CANNOT GUARD THIS. That same scan measures ~200ms on a
// warm, idle box, comfortably inside the 400ms budget, so a duration assertion
// passes on both plans and only the owner's loaded store can tell them apart.
// The plan tells them apart everywhere.
func TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt(t *testing.T) {
	// Arrange
	d, _ := newPruningStore(t, DefaultLedgerRetentionBytes)

	// Act
	plan := queryPlan(t, d, ledgerPruneDeleteSQL, DefaultLedgerRetentionBytes, ledgerPruneBatch)

	// Assert: `cursor` is the one table the sweep may walk — it is the small
	// side, and the window is a per-file question so every file must be asked.
	// The ledger is reached only through write_ledger_source.
	for _, step := range strings.Split(plan, "\n") {
		step = strings.TrimSpace(step)
		if strings.HasPrefix(step, "SCAN ") && !strings.HasPrefix(step, "SCAN c") {
			t.Fatalf("the sweep's delete walks something other than the cursor table:\n%s", plan)
		}
	}
	if !strings.Contains(plan, "SEARCH l USING COVERING INDEX write_ledger_source") {
		t.Fatalf("the sweep's delete does not seek the ledger through write_ledger_source:\n%s", plan)
	}
}

