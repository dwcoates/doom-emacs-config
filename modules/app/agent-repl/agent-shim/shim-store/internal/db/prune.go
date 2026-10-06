package db

import (
	"context"
	"database/sql"
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

// THE ROW AND CURSOR LIMITS BOUND A BATCH'S WORK, NOT ITS HOLD. Neither can
// predict a cold page cache or a loaded host, and the owner's store showed it:
// with both limits in place, single sweep batches held the writer for 451ms,
// 1914ms and 2856ms while removing NOTHING (`store.db.slow-query`
// statement=ledger_sweep rows=0, lock_wait_ms=0, 2026-10-02 to 2026-10-06), and
// interactive write_batch records carried lock waits ending the same
// millisecond a sweep batch did (3237ms behind one at 2026-09-27 18:47:12,
// 6401ms at 2026-09-28 01:42:27). So a sweep transaction is ALSO bounded in
// time, by the same `bulkBounds.time` a producer's bulk transaction is: the
// sweep removes file by file and checks the clock after each, committing once
// the bound has passed. An interactive write therefore waits for at most the
// time bound plus ONE file's delete, whatever the host is doing. The check is
// after the file, so a transaction always sweeps at least one and a sweep
// always makes progress.

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
	// THE SWEEP WALKS `cursor` IN file_id ORDER, a page at a time. Each batch
	// resumes past the last file it finished; a file whose delete filled the
	// batch's row limit is not finished and is asked about again. The walk ends
	// on a batch that finished every file of a short page.
	after := ""
	for {
		if err := ctx.Err(); err != nil {
			return result, d.refuse(base, err)
		}
		batch, err := d.pruneLedgerBatch(ctx, after, result.Batches+1)
		if err != nil {
			return result, err
		}
		result.Deleted += batch.deleted
		result.Batches++
		if d.afterPruneBatch != nil {
			d.afterPruneBatch()
		}
		if batch.ended == sweepEndedLastPage {
			break
		}
		after = batch.resume
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

// ledgerSweepPageSQL reads the page of cursors one sweep batch asks about: a
// seek on the cursor primary key past the previous batch's last finished file,
// so no batch asks about more files than a page holds however many the store
// tracks.
const ledgerSweepPageSQL = `SELECT file_id, offset FROM cursor WHERE file_id > ? ORDER BY file_id LIMIT ?`

// ledgerPruneDeleteSQL is the sweep's delete for ONE file, at package scope so
// the plan it is judged by is EXPLAINed from the statement itself rather than
// from a copy in a test that can drift away from it.
//
// THE BOUND IS A CONSTANT, COMPUTED FROM THE FILE'S OWN CURSOR. Each row is
// measured against its own file's committed position, never against a global
// one, and the position is read from the page in the same transaction.
//
// THAT IS WHAT KEEPS IT A SEEK. The sweep's earlier one-statement form joined
// `cursor` to the ledger, and with the ledger outermost the bound `c.offset -
// ?` was unknown until `c` was resolved, so `write_ledger_source` was usable
// for nothing: SQLite read the whole covering index, 318k rows and 111ms PER
// BATCH on the owner's store, while HOLDING THE WRITE SLOT (a 21-batch sweep
// took 4650ms at 2026-09-13 16:20:00). With `source_file_id = ?` and a constant
// `source_offset < ?` the statement is `SEARCH write_ledger USING COVERING
// INDEX write_ledger_source (source_file_id=? AND ...)` whatever the table
// statistics say, and the LIMIT stops it at the batch's remaining row budget.
const ledgerPruneDeleteSQL = `DELETE FROM write_ledger WHERE rowid IN (
  SELECT rowid FROM write_ledger
   WHERE source_file_id = ?
     AND source_offset IS NOT NULL
     AND source_offset < ?
   LIMIT ?)`

// sweepEnd is why one sweep batch committed.
type sweepEnd int

const (
	// sweepEndedRows: the batch removed ledgerPruneBatch rows, and the file it
	// was on may hold more, so the next batch asks about that file again.
	sweepEndedRows sweepEnd = iota + 1
	// sweepEndedTime: the batch's time bound passed after a file.
	sweepEndedTime
	// sweepEndedPage: the batch finished every file of a full page, so there
	// may be a next one.
	sweepEndedPage
	// sweepEndedLastPage: the batch finished every file of a short page — the
	// last of the cursor table — and the sweep is done.
	sweepEndedLastPage
)

// String names the end the way the per-batch record spells it.
func (e sweepEnd) String() string {
	switch e {
	case sweepEndedRows:
		return "rows"
	case sweepEndedTime:
		return "time"
	case sweepEndedPage:
		return "page"
	case sweepEndedLastPage:
		return "last_page"
	default:
		return "unset"
	}
}

// sweptBatch is what one sweep batch did.
type sweptBatch struct {
	deleted int64
	// files is how many cursors the batch asked about.
	files int
	// resume is the file_id the next batch reads past: the last file this
	// batch FINISHED, or the batch's own starting point if it finished none.
	resume string
	ended  sweepEnd
}

// sweepCursor is one file's committed position, as a sweep batch read it.
type sweepCursor struct {
	fileID string
	offset int64
}

// pruneLedgerBatch removes, in ONE transaction and through the same write slot
// every producer's batch goes through, the ledger rows past the window of the
// files after `after` — one file at a time, stopping at the first of: the row
// limit, the bulk time bound, or the end of the page — and times it like a
// write, by class.
func (d *DB) pruneLedgerBatch(ctx context.Context, after string, transaction int) (batch sweptBatch, err error) {
	base := logging.Fields{Operation: "store.db.ledger-sweep", Table: "write_ledger", WriteClass: WriteBulk.String()}

	started := d.mono()
	var lockWait time.Duration
	defer func() {
		fields := base
		fields.LockWait = lockWait
		d.observeQuery(StatementLedgerSweep, "write_ledger", fields, started, batch.deleted)
		d.traceWriteTiming(StatementLedgerSweep, fields, started, batch.deleted, transaction)
	}()

	// THE SWEEP IS BULK. It is the store's own upkeep and nobody is waiting on
	// it, so an interactive write is always taken ahead of its next batch.
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	began := d.mono()
	lockWait = began.Sub(started)
	if err != nil {
		if isContextError(err) {
			return sweptBatch{}, d.refuse(base, err)
		}
		return sweptBatch{}, d.refuse(base, storagef(err, "begin ledger sweep transaction"))
	}
	defer release()
	defer d.endTx(tx, base)

	cursors, err := d.readSweepPage(ctx, tx, after)
	if err != nil {
		return sweptBatch{}, d.refuse(base, err)
	}
	remove, err := tx.PrepareContext(ctx, ledgerPruneDeleteSQL)
	if err != nil {
		return sweptBatch{}, d.refuse(base, storagef(err, "preparing the ledger sweep's delete"))
	}
	defer remove.Close() //nolint:errcheck // the early-return paths only; the success path closes it below and checks

	swept := sweptBatch{resume: after, ended: sweepEndedLastPage}
	if len(cursors) == ledgerSweepCursorsPerBatch {
		swept.ended = sweepEndedPage
	}
	for _, c := range cursors {
		remaining := ledgerPruneBatch - swept.deleted
		res, err := remove.ExecContext(ctx, c.fileID, c.offset-d.ledgerRetention, remaining)
		if err != nil {
			return sweptBatch{}, d.refuse(base, storagef(err, "pruning the write ledger"))
		}
		deleted, err := res.RowsAffected()
		if err != nil {
			return sweptBatch{}, d.refuse(base, storagef(err, "counting the pruned write_ledger rows"))
		}
		swept.deleted += deleted
		swept.files++
		if d.ledgerFileSwept != nil {
			d.ledgerFileSwept()
		}
		if deleted >= remaining {
			swept.ended = sweepEndedRows
			break
		}
		swept.resume = c.fileID
		if swept.files < len(cursors) && d.mono().Sub(began) >= d.bulk.time {
			swept.ended = sweepEndedTime
			break
		}
	}
	if err := remove.Close(); err != nil {
		return sweptBatch{}, d.refuse(base, storagef(err, "closing the ledger sweep's delete"))
	}
	if err := tx.Commit(); err != nil {
		return sweptBatch{}, d.refuse(base, storagef(err, "commit ledger sweep transaction"))
	}
	batch = swept
	d.log.LogVerbose(base, "ledger sweep transaction %d committed deleted=%d files=%d ended_by=%s",
		transaction, batch.deleted, batch.files, batch.ended)
	return batch, nil
}

// readSweepPage reads the page of cursors a sweep batch asks about, inside
// the batch's own transaction so each file's bound is its committed position
// as of the delete.
func (d *DB) readSweepPage(ctx context.Context, tx *sql.Tx, after string) ([]sweepCursor, error) {
	rows, err := tx.QueryContext(ctx, ledgerSweepPageSQL, after, ledgerSweepCursorsPerBatch)
	if err != nil {
		return nil, storagef(err, "reading the ledger sweep's cursor page")
	}
	defer rows.Close() //nolint:errcheck // the early-return paths only; the success path closes it below and checks
	var cursors []sweepCursor
	for rows.Next() {
		var c sweepCursor
		if err := rows.Scan(&c.fileID, &c.offset); err != nil {
			return nil, storagef(err, "reading the ledger sweep's cursor page")
		}
		cursors = append(cursors, c)
	}
	if err := rows.Err(); err != nil {
		return nil, storagef(err, "reading the ledger sweep's cursor page")
	}
	if err := rows.Close(); err != nil {
		return nil, storagef(err, "closing the ledger sweep's cursor page")
	}
	return cursors, nil
}

// SweepWriteLedger runs PruneWriteLedger on `interval` until ctx ends. It is
// the resident store's loop; a caller that wants one sweep calls
// PruneWriteLedger directly, which is also what every test does.
//
// IT SWEEPS ONCE AT START. A store that has just come up may be carrying the
// backlog of a long previous run, and waiting a whole interval to look at it
// serves nobody. The hook sweep (SweepHookLines) runs before it on every pass.
func (d *DB) SweepWriteLedger(ctx context.Context, interval time.Duration) {
	if interval <= 0 {
		interval = DefaultLedgerSweepInterval
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		// THE HOOK ROWS THAT DRAW NOTHING ARE DROPPED FIRST (hooksweep.go): all
		// of them on the first pass, then only what was written since. A
		// failure is recorded by the store at the failing statement; only a
		// shutdown ends the sweeps early.
		if _, err := d.SweepHookLines(ctx); err != nil && isContextError(err) {
			return
		}
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
