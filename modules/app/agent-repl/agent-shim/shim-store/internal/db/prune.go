package db

import (
	"context"
	"time"

	"agentrepl/shim-store/internal/logging"
)

// THE WRITE LEDGER IS RETAINED ONLY AS LONG AS ABSORPTION CAN STILL ASK ABOUT
// IT, AND NOT ONE ROW LONGER.
//
// The ledger exists for exactly one question: "has this write_id already been
// applied?" — asked when a producer re-emits bytes it has already sent, so the
// store absorbs the replay instead of re-upserting the row, bumping write_seq
// and re-delivering a regression to every live watcher. It answered that
// question for EVERY write the store had ever applied, forever: 735k rows and
// 204 MB with its indexes on the owner's box, none of which any producer could
// still ask about.
//
// WHO CAN RE-EMIT, AND HOW FAR BACK
//
// The sidecar mints a DETERMINISTIC write_id from its source coordinates —
// sha256("shim-claude-sidecar|" + file_id + "|" + offset + "|" +
// discriminator) — so the same bytes at the same offset of the same file mint
// the same id and nothing else ever does. It re-reads bytes it has already sent
// in exactly two ways, and both are bounded:
//
//   - THE BOOT REWIND, once per file per boot: `tail.RewindToTurnStart` moves
//     the restored cursor back at most `tail.DefaultRewindWindow` (4 MB) from
//     the file's COMMITTED cursor, to the last turn start within that window.
//     It cannot reach a byte further back than the window, ever — the scan is
//     one bounded backward read, not a search.
//   - THE HOLD, within one poll: the cursor advances SHORT of what was read, so
//     the next read re-covers the held frame. The held frame is by definition
//     NOT yet written, and everything written is below the advanced cursor.
//
// So a ledger row whose source offset is more than the rewind window behind its
// file's committed cursor names bytes no sidecar can read again. Its absorption
// can never be asked for, and the row is free.
//
// WHY THE RECORDED OFFSET IS SAFE TO SUBTRACT FROM. The store cannot open a
// write_id — it is a hash — and `PlaneFile` carries no coordinates, so the
// source position a row is stamped with is the BATCH's `cursor_advance.offset`:
// the offset the producer's NEXT read starts at, which is strictly ABOVE every
// offset the batch's own rows came from. An upper bound on the row's true
// offset makes `cursor.offset - recorded` a LOWER bound on its true distance
// behind the cursor, so a row is pruned only when its true distance is also
// past the window. The error is entirely in the direction of keeping a row too
// long.
//
// WHAT IS NEVER PRUNED, AND WHY
//
//   - A ROW WITH NO SOURCE FILE. Stream-plane writes and any file-plane batch
//     that carried no cursor advance are stamped NULL and are kept. The shim
//     re-emits from an in-memory retry buffer whose bound the store cannot see,
//     and there is no structural argument for a cutoff, so there is no cutoff.
//   - A ROW WHOSE FILE HAS NO CURSOR. No cursor row means a sidecar would
//     re-read that file FROM ZERO, which is precisely when the ledger is doing
//     the most work; pruning there would re-write the whole file's corpus. The
//     coordinator's "the file is also gone" refinement is deliberately not
//     taken: deciding it means the store stat()ing vendor files it otherwise
//     never touches, to save rows a store reset throws away wholesale.
//   - ANYTHING, WHEN THE RETENTION WINDOW IS NON-POSITIVE. That is an operator
//     or a test saying "keep everything", and it is honored by sweeping nothing.

// DefaultLedgerRetentionBytes is how far behind a file's committed cursor a
// ledger row must fall before it is pruned.
//
// IT IS FOUR TIMES THE SIDECAR'S REWIND WINDOW, not equal to it. The two
// numbers live in different modules and are bumped by different people, and a
// retention window that merely matched today's rewind bound would start losing
// absorptions the moment somebody widened the scan. Four times over is cheap —
// the ledger row is ~100 bytes against 4 MB of transcript — and it turns a
// cross-module coupling into a margin.
const DefaultLedgerRetentionBytes int64 = 16 << 20

// DefaultLedgerSweepInterval is how often the resident store sweeps.
//
// The ledger grows with ingestion, not with time, and a sweep that finds
// nothing costs one indexed query. Sweeping often enough that a batch is small
// matters more than sweeping promptly.
const DefaultLedgerSweepInterval = 15 * time.Minute

// ledgerPruneBatch is how many rows one transaction of the sweep removes.
//
// THE SWEEP IS BATCHED BECAUSE IT SHARES THE WRITE SLOT WITH EVERY PRODUCER.
// It takes the slot per batch and gives it back between batches, so a producer's
// WriteBatch waits at most one batch of deletes rather than a whole sweep of
// them. A single unbounded DELETE would have held the slot — and, before the
// slot existed, the write lock — for as long as the backlog took.
const ledgerPruneBatch = 2000

// ledgerSweepCursorsPerBatch is how many `cursor` rows one transaction of the
// sweep asks about.
//
// THE ROW LIMIT ALONE DID NOT BOUND A BATCH. A batch that removes little walks
// EVERY cursor to learn so — one seek into write_ledger_source per file — and
// on 2026-09-23 the owner's store (4307 cursors, 674k ledger rows, a host whose
// one-row writes were taking seconds) held the one writer for 137s at the
// 18:35 sweep and 848s at the 19:05 one, with every interactive write queued
// behind it. Paging the cursors bounds the WORK a batch can do, not only what
// it can remove, so an interactive write waits for one page's seeks at most.
const ledgerSweepCursorsPerBatch = 256

// PruneResult reports what one sweep removed.
type PruneResult struct {
	// Deleted is the ledger rows removed across the whole sweep.
	Deleted int64
	// Batches is the transactions it took, each one a separate turn of the
	// write slot.
	Batches int
}

// PruneWriteLedger removes every ledger row that has fallen past the retention
// window, in bounded batches, and reports what went.
//
// IT IS INTERRUPTIBLE AND IT COMMITS AS IT GOES. A sweep cut short by shutdown
// keeps every batch it already committed and returns the caller's cancellation;
// there is nothing to roll back, because a pruned row is one nobody can ask
// about.
func (d *DB) PruneWriteLedger(ctx context.Context) (PruneResult, error) {
	var result PruneResult
	base := logging.Fields{Operation: "store.db.ledger-sweep", Table: "write_ledger"}
	if d.ledgerRetention <= 0 {
		d.log.LogVerbose(base, "ledger sweep disabled: the retention window is not positive")
		return result, nil
	}
	started := d.mono()
	// THE SWEEP WALKS `cursor` A PAGE AT A TIME, keyed by file_id. A page is
	// swept again while it keeps filling a batch, and the walk moves on once a
	// batch comes back short; it ends on the first short page.
	after := ""
	for {
		if err := ctx.Err(); err != nil {
			return result, d.refuse(base, err)
		}
		page, err := d.pruneLedgerBatch(ctx, after, result.Batches+1)
		if err != nil {
			return result, err
		}
		result.Deleted += page.deleted
		result.Batches++
		if d.afterPruneBatch != nil {
			d.afterPruneBatch()
		}
		if page.deleted >= ledgerPruneBatch {
			continue
		}
		if page.files < ledgerSweepCursorsPerBatch {
			break
		}
		after = page.last
	}
	elapsed := d.mono().Sub(started)
	if result.Deleted == 0 {
		d.log.LogVerbose(base, "ledger sweep found nothing past the retention window retention_bytes=%d duration_ms=%d",
			d.ledgerRetention, elapsed.Milliseconds())
		return result, nil
	}
	// A SWEEP THAT REMOVED SOMETHING IS A STATE CHANGE, so it is a normal-level
	// info record; one that removed nothing is the steady state and is verbose.
	d.log.Log(base, "ledger sweep removed %d write_ledger rows past the retention window batches=%d retention_bytes=%d duration_ms=%d",
		result.Deleted, result.Batches, d.ledgerRetention, elapsed.Milliseconds())
	return result, nil
}

// ledgerPruneDeleteSQL is the sweep's one statement, at package scope so the
// plan it is judged by is EXPLAINed from the statement itself rather than from
// a copy in a test that can drift away from it.
//
// The join to `cursor` is what makes the window a per-FILE question: each
// row is measured against its own file's committed position, never against
// a global one.
//
// `cursor` IS THE OUTER TABLE, AND THE `CROSS JOIN` IS WHAT FIXES IT THERE.
// The retention bound is `c.offset - ?`, which is not a constant: it is a
// column of the OTHER table. Written with the ledger outermost, SQLite can
// use `write_ledger_source` for nothing — the bound is unknown until `c` is
// resolved — so it read the whole covering index and probed `cursor` per
// row: `SCAN l USING COVERING INDEX write_ledger_source`, 318k rows and
// 111ms PER BATCH on the owner's store, paid even by the final batch that
// deletes nothing, and paid while HOLDING THE WRITE SLOT every producer's
// WriteBatch queues on. It is also why a sweep of 21 batches took 4650ms
// (2026-09-13 16:20:00) — the scan is repeated once per batch, so the sweep
// is quadratic in the ledger.
//
// Driven from `cursor` (2844 rows against 318k) the same index is an
// ordinary seek: `SEARCH l USING COVERING INDEX write_ledger_source
// (source_file_id=? AND source_offset>? AND source_offset<?)`, with the
// LIMIT stopping the outer loop as soon as a batch is full. SQLite's
// planner reorders a plain JOIN by its own row estimates and picked the
// scan; `CROSS JOIN` is the documented way to state the order and have it
// kept, which is why the order is not left to an estimate that can flip
// back the next time the table statistics move.
//
// THE OUTER SIDE IS ONE PAGE OF `cursor`, NOT THE WHOLE TABLE (see
// ledgerSweepCursorsPerBatch): a seek on the cursor primary key past the
// previous page's last file_id, so no batch asks about more files than a page
// holds however many the store tracks.
const ledgerPruneDeleteSQL = `DELETE FROM write_ledger WHERE rowid IN (
  SELECT l.rowid FROM (
      SELECT file_id, offset FROM cursor WHERE file_id > ? ORDER BY file_id LIMIT ?
    ) c
    CROSS JOIN write_ledger l
      ON l.source_file_id = c.file_id
     AND l.source_offset IS NOT NULL
     AND l.source_offset < c.offset - ?
   LIMIT ?)`

// ledgerSweepPageSQL reads the page a sweep batch covered: how many cursors it
// asked about and the last file_id among them, where the next page starts.
const ledgerSweepPageSQL = `SELECT COUNT(*), COALESCE(MAX(file_id), '') FROM (
  SELECT file_id FROM cursor WHERE file_id > ? ORDER BY file_id LIMIT ?)`

// sweptPage is what one sweep batch did.
type sweptPage struct {
	deleted int64
	files   int
	last    string
}

// pruneLedgerBatch removes at most ledgerPruneBatch rows, among the files of
// ONE page of at most ledgerSweepCursorsPerBatch cursors after `after`, in ONE
// transaction, through the same write slot every producer's batch goes
// through — and times it like one, by class.
func (d *DB) pruneLedgerBatch(ctx context.Context, after string, transaction int) (page sweptPage, err error) {
	base := logging.Fields{Operation: "store.db.ledger-sweep", Table: "write_ledger", WriteClass: WriteBulk.String()}

	started := d.mono()
	var lockWait time.Duration
	defer func() {
		fields := base
		fields.LockWait = lockWait
		d.observeQuery(StatementLedgerSweep, "write_ledger", fields, started, page.deleted)
		d.traceWriteTiming(StatementLedgerSweep, fields, started, page.deleted, transaction)
	}()

	// THE SWEEP IS BULK. It is the store's own upkeep and nobody is waiting on
	// it, so an interactive write is always taken ahead of its next batch.
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	lockWait = d.mono().Sub(started)
	if err != nil {
		if isContextError(err) {
			return sweptPage{}, d.refuse(base, err)
		}
		return sweptPage{}, d.refuse(base, storagef(err, "begin ledger sweep transaction"))
	}
	defer release()
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	if err := tx.QueryRowContext(ctx, ledgerSweepPageSQL, after, ledgerSweepCursorsPerBatch).Scan(&page.files, &page.last); err != nil {
		return sweptPage{}, d.refuse(base, storagef(err, "reading the ledger sweep's cursor page"))
	}
	res, err := tx.ExecContext(ctx, ledgerPruneDeleteSQL, after, ledgerSweepCursorsPerBatch, d.ledgerRetention, ledgerPruneBatch)
	if err != nil {
		return sweptPage{}, d.refuse(base, storagef(err, "pruning the write ledger"))
	}
	deleted, err := res.RowsAffected()
	if err != nil {
		return sweptPage{}, d.refuse(base, storagef(err, "counting the pruned write_ledger rows"))
	}
	if err := tx.Commit(); err != nil {
		return sweptPage{}, d.refuse(base, storagef(err, "commit ledger sweep transaction"))
	}
	page.deleted = deleted
	return page, nil
}

// SweepWriteLedger runs PruneWriteLedger on `interval` until ctx ends. It is
// the resident store's loop; a caller that wants one sweep calls
// PruneWriteLedger directly, which is also what every test does.
//
// IT SWEEPS ONCE AT START. A store that has just come up may be carrying the
// backlog of a long previous run, and waiting a whole interval to look at it
// serves nobody.
func (d *DB) SweepWriteLedger(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = DefaultLedgerSweepInterval
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		if _, err := d.PruneWriteLedger(ctx); err != nil && isContextError(err) {
			return
		}
		select {
		case <-ctx.Done():
			return
		case <-ticker.C:
		}
	}
}
