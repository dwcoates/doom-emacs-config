package db

import (
	"context"
	"errors"
	"fmt"
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
				entry.ConversionVersion = fileVersion()
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
	// PAST THE WINDOW THE REPLAY IS APPLIED AGAIN, and since it is a file-plane
	// write of exactly the content the row holds, applying it is a RESTAMP
	// (write.go sameContentBarVersion): the ledger learns the write id again
	// and no reader observes anything.
	tests := []struct {
		name          string
		cursorNow     int64
		wantAbsorbed  int
		wantWritten   int
		wantRestamped int
	}{
		{name: "inside the window: absorbed", cursorNow: 5_500, wantAbsorbed: 1},
		{name: "past the window: the row was pruned, so the replay applies again as a restamp", cursorNow: 5_000_000, wantRestamped: 1},
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
			if result.Absorbed != test.wantAbsorbed || result.Written != test.wantWritten || result.Restamped != test.wantRestamped {
				t.Fatalf("replay = %d absorbed / %d written / %d restamped, want %d / %d / %d",
					result.Absorbed, result.Written, result.Restamped, test.wantAbsorbed, test.wantWritten, test.wantRestamped)
			}
			seqAfter := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'u1'`)
			if seqAfter != seqBefore {
				t.Fatalf("write_seq moved %d -> %d on a replay of unchanged content; the row would be re-delivered to every watcher", seqBefore, seqAfter)
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
//
// ITS MONOTONIC CLOCK IS STOPPED, so a sweep batch's time bound never passes
// unless a test moves the clock itself: how many batches a sweep takes is then
// a fact about the rows and the cursors, never about how loaded the host was.
func newPruningStore(t *testing.T, window int64) (*DB, *sink) {
	t.Helper()
	return newClockedPruningStore(t, window, &fakeClock{now: time.Unix(0, 0)})
}

// newClockedPruningStore is newPruningStore on the caller's clock, for the
// cases that move it across a sweep batch's time bound.
func newClockedPruningStore(t *testing.T, window int64, clock *fakeClock) (*DB, *sink) {
	t.Helper()
	s, log := newSink(t)
	return memoryStore(t, log, Options{
		Now:                  func() int64 { return testNow },
		Clock:                clock.Now,
		LedgerRetentionBytes: window,
	}), s
}

// writeFileBatch writes one FILE-plane page line whose batch advances `fileID`
// to `offset` — the shape every sidecar batch has, and the one that stamps a
// ledger row with a source position.
func writeFileBatch(t *testing.T, d *DB, fileID string, offset int64, writeID, upsertKey string) WriteResult {
	t.Helper()
	entry := pageEntry(writeID, upsertKey, "agent-1", frameItem(activityFrame("agent-1", "act-"+upsertKey, prose())))
	entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	entry.ConversionVersion = fileVersion()
	result, err := d.WriteBatch(ctx(), "test-sidecar", WriteInteractive, &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{entry},
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: "/t/a.jsonl", Offset: offset, Conversion: currentConversion()},
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
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: "/t/a.jsonl", Offset: offset, Conversion: currentConversion()},
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
	seedLedgerRowsAs(t, d, "seed", fileID, offset, n)
}

// seedLedgerRowsAs is seedLedgerRows with the caller's write-id prefix, for
// the cases that seed more than one file and need the ids apart.
func seedLedgerRowsAs(t *testing.T, d *DB, prefix, fileID string, offset int64, n int) {
	t.Helper()
	tx, err := d.sql.Begin()
	if err != nil {
		t.Fatalf("seeding the ledger: %v", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit
	for i := 0; i < n; i++ {
		id := prefix + "-" + strconv.Itoa(i)
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

// TestASweepBatchEndsOnceItsTimeBoundPasses is the sweep's side of the 400ms
// producer budget, asserted as the bound that keeps it rather than as a
// wall-clock sum.
//
// THE SWEEP SHARES THE WRITE SLOT, so a producer's batch can wait one sweep
// batch and then do its own work. Counting rows and cursors bounded a batch's
// WORK but not its HOLD: on the owner's store single batches that removed
// nothing held the writer for 451ms to 2856ms, and interactive writes queued
// behind them (see the note on ledgerSweepCursorsPerBatch). So a batch also
// commits once the bulk time bound has passed, checked after each file — the
// longest a producer waits is the bound plus one file's delete.
//
// A WALL CLOCK CANNOT ASSERT THIS. The same batch measured 9-14ms alone and
// 442ms in a full parallel test run, which is a number about the host. The
// clock here is moved by the test after each file, so what is asserted is the
// rule itself: how many files a batch takes before the bound ends it.
func TestASweepBatchEndsOnceItsTimeBoundPasses(t *testing.T) {
	tests := []struct {
		name string
		// tick is how far the clock moves while each file is swept.
		tick time.Duration
		// want is how many files each batch asked about, in order.
		want []int
	}{
		{name: "a stopped clock: the page ends the batch", tick: 0, want: []int{10}},
		{name: "the bound passes part-way through the page", tick: 30 * time.Millisecond, want: []int{4, 4, 2}},
		{name: "one file reaches the bound exactly", tick: DefaultBulkChunkTime, want: []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1}},
		{name: "one file alone overruns the bound and still completes", tick: 3 * DefaultBulkChunkTime, want: []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, _ := newClockedPruningStore(t, 1000, clock)
			seedCursorFiles(t, d, 10)
			var files int
			var perBatch []int
			d.ledgerFileSwept = func() {
				files++
				clock.advance(test.tick)
			}
			d.afterPruneBatch = func() {
				perBatch = append(perBatch, files)
				files = 0
			}

			// Act
			result, err := d.PruneWriteLedger(ctx())

			// Assert
			if err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}
			if result.Deleted != 10 {
				t.Fatalf("sweep removed %d rows, want 10 (one per file)", result.Deleted)
			}
			if fmt.Sprint(perBatch) != fmt.Sprint(test.want) {
				t.Fatalf("files per sweep batch = %v, want %v", perBatch, test.want)
			}
		})
	}
}

// TestASweepBatchThatFillsItsRowLimitMidPageResumesAtThatFile pins the row
// limit's half of the resume: the file a batch filled its limit on may hold
// more, so the next batch asks about it again rather than past it.
func TestASweepBatchThatFillsItsRowLimitMidPageResumesAtThatFile(t *testing.T) {
	// Arrange: two files that together hold more than one batch's limit, so the
	// limit is reached on the SECOND file of the page.
	d, _ := newPruningStore(t, 1000)
	half := ledgerPruneBatch * 3 / 4
	seedLedgerRowsAs(t, d, "a", "12:34", 5_000, half)
	seedLedgerRowsAs(t, d, "b", "56:78", 5_000, half)
	advanceCursor(t, d, "12:34", 5_000_000)
	advanceCursor(t, d, "56:78", 5_000_000)

	// Act
	result, err := d.PruneWriteLedger(ctx())

	// Assert
	if err != nil {
		t.Fatalf("PruneWriteLedger: %v", err)
	}
	if result.Deleted != int64(2*half) {
		t.Fatalf("sweep removed %d rows, want %d", result.Deleted, 2*half)
	}
	if left := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger`); left != 0 {
		t.Fatalf("%d ledger rows survived the sweep", left)
	}
}

// TestASweepBatchRecordsWhatEndedIt keeps the batch's verdict in the log: a
// batch the time bound cut short is exactly what the owner's slow sweeps
// would have needed to show.
func TestASweepBatchRecordsWhatEndedIt(t *testing.T) {
	tests := []struct {
		name  string
		files int
		rows  int
		tick  time.Duration
		want  string
	}{
		{name: "the last page", files: 1, rows: 1, want: "ended_by=last_page"},
		{name: "a full page", files: ledgerSweepCursorsPerBatch, rows: 1, want: "ended_by=page"},
		{name: "the row limit", files: 1, rows: ledgerPruneBatch + 1, want: "ended_by=rows"},
		{name: "the time bound", files: 2, rows: 1, tick: DefaultBulkChunkTime, want: "ended_by=time"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			d, s := newClockedPruningStore(t, 1000, clock)
			for i := 0; i < test.files; i++ {
				fileID := fmt.Sprintf("file-%04d", i)
				seedLedgerRowsAs(t, d, fileID, fileID, 5_000, test.rows)
				advanceCursor(t, d, fileID, 5_000_000)
			}
			d.ledgerFileSwept = func() { clock.advance(test.tick) }

			// Act
			if _, err := d.PruneWriteLedger(ctx()); err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Assert
			for _, record := range s.records(t) {
				message, _ := record["message"].(string)
				if strings.HasPrefix(message, "ledger sweep transaction 1 committed") {
					if !strings.Contains(message, test.want) {
						t.Fatalf("first sweep batch record = %q, want it to name %s", message, test.want)
					}
					return
				}
			}
			t.Fatalf("no record of the first sweep batch; log was:\n%s", s.file.String())
		})
	}
}

// TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt is the defect a
// latency case cannot see, stated where it IS visible.
//
// The sweep's earlier one-statement form joined `cursor` to the ledger, and a
// bound that was a column of the OTHER table left `write_ledger_source` usable
// for nothing: SQLite read the whole covering index — 111ms per batch on the
// owner's 318k-row ledger, all of it holding the write slot. The per-file
// delete binds the file and a constant bound, so the index is a seek.
//
// A WALL-CLOCK BOUND CANNOT GUARD THIS. That same scan measures ~200ms on a
// warm, idle box, comfortably inside the 400ms budget, so a duration assertion
// passes on both plans and only the owner's loaded store can tell them apart.
// The plan tells them apart everywhere.
func TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt(t *testing.T) {
	// Arrange
	d, _ := newPruningStore(t, DefaultLedgerRetentionBytes)

	// Act
	plan := queryPlan(t, d, ledgerPruneDeleteSQL, "12:34", int64(5_000_000), ledgerPruneBatch)

	// Assert
	for _, step := range strings.Split(plan, "\n") {
		if strings.HasPrefix(strings.TrimSpace(step), "SCAN ") {
			t.Fatalf("the sweep's delete scans:\n%s", plan)
		}
	}
	if !strings.Contains(plan, "SEARCH write_ledger USING COVERING INDEX write_ledger_source (source_file_id=?") {
		t.Fatalf("the sweep's delete does not seek the ledger through write_ledger_source:\n%s", plan)
	}
}

// TestTheSweepsCursorPageSeeksTheCursorKey pins that a batch reaches its page
// of cursors by a seek past the previous batch — a scan would ask about every
// file again, which is the unbounded batch the page exists to remove.
func TestTheSweepsCursorPageSeeksTheCursorKey(t *testing.T) {
	// Arrange
	d, _ := newPruningStore(t, DefaultLedgerRetentionBytes)

	// Act
	plan := queryPlan(t, d, ledgerSweepPageSQL, "", ledgerSweepCursorsPerBatch)

	// Assert
	if !strings.Contains(plan, "SEARCH cursor USING") || !strings.Contains(plan, "(file_id>?)") {
		t.Fatalf("the sweep's page does not seek cursor past the previous page:\n%s", plan)
	}
}

// The sweep's delete selects its rows through a subquery, which is exactly the
// shape SQLite answers with an AUTOMATIC index when the column it filters on
// has no real one.
func TestTheSweepsStatementsBuildNoAutomaticIndex(t *testing.T) {
	tests := []struct {
		name      string
		statement string
		args      []any
	}{
		{name: "the delete", statement: ledgerPruneDeleteSQL,
			args: []any{"12:34", int64(5_000_000), ledgerPruneBatch}},
		{name: "the page read", statement: ledgerSweepPageSQL,
			args: []any{"", ledgerSweepCursorsPerBatch}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, DefaultLedgerRetentionBytes)

			// Act
			plan := queryPlan(t, d, test.statement, test.args...)

			// Assert
			assertNoAutomaticIndex(t, test.name, plan)
		})
	}
}

// ---- the sweep is bounded in work, and timed like any write ----

// seedCursorFiles inserts `files` cursor rows, each with one ledger row far
// enough behind its cursor to be prunable, directly — the case is about how the
// sweep walks the cursor table, not about how the rows got there.
func seedCursorFiles(t *testing.T, d *DB, files int) {
	t.Helper()
	tx, err := d.sql.Begin()
	if err != nil {
		t.Fatalf("seeding cursors: %v", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit
	for i := 0; i < files; i++ {
		fileID := fmt.Sprintf("file-%04d", i)
		if _, err := tx.Exec(`INSERT INTO cursor (file_id, path, offset, updated_at_ms) VALUES (?,?,?,?)`,
			fileID, "/t/"+fileID+".jsonl", 5_000_000, testNow); err != nil {
			t.Fatalf("seeding cursors: %v", err)
		}
		if _, err := tx.Exec(
			`INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms, source_file_id, source_offset) VALUES (?,?,?,?,?,?)`,
			"seed-"+fileID, "seed-"+fileID, i+1, testNow, fileID, 5_000); err != nil {
			t.Fatalf("seeding the ledger: %v", err)
		}
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("seeding cursors: %v", err)
	}
}

// TestTheSweepWalksTheCursorsAPageAtATime pins the bound on a batch's WORK: no
// transaction asks about more than one page of cursors, however many files the
// store tracks, and every page is still visited.
func TestTheSweepWalksTheCursorsAPageAtATime(t *testing.T) {
	tests := []struct {
		name        string
		files       int
		wantBatches int
	}{
		{name: "one file is one page", files: 1, wantBatches: 1},
		{name: "exactly one full page needs a second to learn it was the last", files: ledgerSweepCursorsPerBatch, wantBatches: 2},
		{name: "one file past a page is a second page", files: ledgerSweepCursorsPerBatch + 1, wantBatches: 2},
		{name: "two pages and one file is three", files: 2*ledgerSweepCursorsPerBatch + 1, wantBatches: 3},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newPruningStore(t, 1000)
			seedCursorFiles(t, d, test.files)

			// Act
			result, err := d.PruneWriteLedger(ctx())

			// Assert
			if err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}
			if result.Deleted != int64(test.files) {
				t.Fatalf("sweep removed %d rows, want %d (one per file)", result.Deleted, test.files)
			}
			if result.Batches != test.wantBatches {
				t.Fatalf("sweep took %d batches over %d files, want %d", result.Batches, test.files, test.wantBatches)
			}
		})
	}
}

// TestASweepBatchIsTimedAsABulkWrite pins that a sweep batch leaves the same
// per-class record a producer's batch does. A sweep that removed nothing used to
// leave no normal-level trace at all, so a sweep holding the writer for minutes
// was invisible except as other writers' queue wait.
func TestASweepBatchIsTimedAsABulkWrite(t *testing.T) {
	tests := []struct {
		name      string
		operation string
	}{
		{name: "the slow-query record names the class", operation: SlowQueryOperation},
		{name: "the per-write timing record names the class", operation: WriteTimingOperation},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			clock := &fakeClock{now: time.Unix(0, 0)}
			s, log := newSink(t)
			d := memoryStore(t, log, Options{
				Now:                  func() int64 { return testNow },
				Clock:                clock.Now,
				SlowQuery:            time.Nanosecond,
				LedgerRetentionBytes: 1000,
			})
			seedCursorFiles(t, d, 1)
			queued := make(chan struct{})
			d.queuedForWrite = func(WriteClass) {
				clock.advance(40 * time.Millisecond)
				close(queued)
			}
			release, err := d.acquireWrite(ctx(), WriteInteractive)
			if err != nil {
				t.Fatalf("acquireWrite: %v", err)
			}

			// Act: the sweep queues behind the held slot, then runs.
			done := make(chan error, 1)
			go func() {
				_, err := d.PruneWriteLedger(ctx())
				done <- err
			}()
			<-queued
			release()
			if err := <-done; err != nil {
				t.Fatalf("PruneWriteLedger: %v", err)
			}

			// Assert
			for _, record := range s.records(t) {
				context, _ := record["context"].(map[string]any)
				if record["operation"] == test.operation && context["statement"] == StatementLedgerSweep {
					if context["write_class"] != "bulk" || context["lock_wait_ms"] != float64(40) {
						t.Fatalf("sweep record write_class=%v lock_wait_ms=%v, want bulk and 40: %v",
							context["write_class"], context["lock_wait_ms"], record)
					}
					return
				}
			}
			t.Fatalf("no %s record for statement %s; log was:\n%s", test.operation, StatementLedgerSweep, s.file.String())
		})
	}
}
