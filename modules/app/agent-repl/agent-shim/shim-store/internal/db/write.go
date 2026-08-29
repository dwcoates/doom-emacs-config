package db

import (
	"context"
	"database/sql"
	"errors"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// BashRowWritten is one detached-run row this write produced, ready for the
// WatchBashRun fan-out. WriteSeq is the ordinal a watcher is pinned by; it
// never reaches the wire.
type BashRowWritten struct {
	RunID    string
	Row      *storev1.StoreAgentBash
	WriteSeq uint64
}

// LineWritten is one page line this write produced, ready for the fan-out to
// publish. WriteSeq is the store-internal ordinal a watcher is pinned by; it
// never reaches the wire.
type LineWritten struct {
	AgentID  string
	Line     *storev1.StoreLineAt
	WriteSeq uint64
}

// WriteResult reports what one batch did. Absorbed is not a lesser success:
// a replayed batch whose write_ids all landed before is the SAME durable
// answer, and the producer retires it from its retry buffer either way.
type WriteResult struct {
	Written  int
	Absorbed int
	Lines    []LineWritten
	// BashRows is the bash rows this write produced, ready for the WatchBashRun
	// fan-out. A run's rows are published exactly as a book's lines are.
	BashRows []BashRowWritten
}

// WriteBatch commits one producer's batch — records and cursor advance — as ONE
// transaction, and returns the page lines it wrote.
//
// DURABLE OR NOTHING. Every validation refusal happens BEFORE the transaction
// opens and every storage failure rolls it back, so a caller that received a
// failure knows with certainty that no row and no cursor moved. That certainty
// is what lets the producer hold the batch in a bounded in-memory buffer with
// no durable spill behind it.
func (d *DB) WriteBatch(ctx context.Context, producer string, batch *storev1.EntryBatch) (WriteResult, error) {
	var result WriteResult
	base := logging.Fields{Operation: "store.db.write-batch", Table: "entry", Producer: producer}

	if producer == "" {
		return result, d.refuse(base, invalidf("producer is empty — every write is attributed"))
	}
	if batch == nil {
		return result, d.refuse(base, invalidf("batch is unset"))
	}
	entries := batch.GetEntries()
	cursor := batch.GetCursorAdvance()
	if len(entries) == 0 && cursor == nil {
		return result, d.refuse(base, invalidf("batch carries neither entries nor a cursor advance"))
	}

	// Validation first and whole, so a refusal names the offending entry
	// without a transaction ever having been opened.
	routes := make([]routed, 0, len(entries))
	for i, entry := range entries {
		r, err := classify(entry, i)
		if err != nil {
			fields := base
			fields.WriteID = entry.GetWriteId()
			fields.UpsertKey = entry.GetUpsertKey()
			return result, d.refuse(fields, err)
		}
		routes = append(routes, r)
	}
	if cursor != nil {
		if err := validateCursorState(cursor); err != nil {
			return result, d.refuse(base, err)
		}
	}

	started := time.Now()
	defer func() { d.observeQuery(StatementWriteBatch, "entry", base, started, int64(len(entries))) }()

	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "entry", Producer: producer, Transaction: "BEGIN IMMEDIATE",
	}, "starting transaction entries=%d cursor_advance=%t", len(entries), cursor != nil)

	tx, err := d.sql.BeginTx(ctx, nil)
	if err != nil {
		return WriteResult{}, d.refuse(base, storagef(err, "begin write transaction"))
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	nextSeq, err := d.currentWriteSeq(ctx, tx)
	if err != nil {
		return WriteResult{}, d.refuse(base, err)
	}
	now := d.now()

	for i, r := range routes {
		fields := base
		fields.WriteID = r.writeID
		fields.UpsertKey = r.upsertKey
		if r.book.Valid {
			fields.BookAgentID = r.book.String
		}

		absorbed, err := d.absorbedBefore(ctx, tx, r.writeID)
		if err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		if absorbed {
			result.Absorbed++
			d.log.LogVerbose(fields, "write absorbed: this write_id already landed entries_index=%d", i)
			continue
		}

		if r.workflowNotImplemented {
			// DURABLE, NEVER DROPPED, and loud: the row lands whole so nothing
			// is lost, and the warning says why nothing serves it yet.
			warn := fields
			warn.Level = "warn"
			d.log.Log(warn, "workflow ingestion not implemented this wave — the entry is stored as never-served residue and the workflow table is untouched entries_index=%d", i)
		}

		nextSeq++
		position, err := d.upsertEntry(ctx, tx, r, nextSeq, now)
		if err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		if err := d.recordApplied(ctx, tx, r, nextSeq, now); err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		if err := d.applyLifecycle(ctx, tx, r, now); err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		result.Written++
		switch r.kind {
		case kindPageLine:
			result.Lines = append(result.Lines, LineWritten{
				AgentID:  r.book.String,
				Line:     &storev1.StoreLineAt{At: encodePointer(position), Line: r.pageLine},
				WriteSeq: nextSeq,
			})
		case kindBash:
			result.BashRows = append(result.BashRows, BashRowWritten{
				RunID:    r.runID.String,
				Row:      r.bashRow,
				WriteSeq: nextSeq,
			})
		}
		verbose := fields
		verbose.Position = encodePointer(position).GetValue()
		verbose.WriteSeq = nextSeq
		d.log.LogVerbose(verbose, "entry written kind=%s entries_index=%d", r.kind, i)
	}

	if cursor != nil {
		if err := d.upsertCursor(ctx, tx, cursor, now); err != nil {
			return WriteResult{}, d.refuse(base, err)
		}
	}

	if err := tx.Commit(); err != nil {
		return WriteResult{}, d.refuse(base, storagef(err, "commit write transaction"))
	}
	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "entry", Producer: producer, Transaction: "BEGIN IMMEDIATE",
	}, "transaction committed written=%d absorbed=%d lines=%d cursor_advance=%t",
		result.Written, result.Absorbed, len(result.Lines), cursor != nil)
	return result, nil
}

// refuse records the refusal exactly once, here at its owning layer, and hands
// the error back for the server to shape into a typed failure arm.
//
// EVERY error return of this package goes through it, read paths included, so
// the rule "each error is logged exactly once by its owning layer" has one
// implementation rather than a convention.
func (d *DB) refuse(fields logging.Fields, err error) error {
	fields.Level = "error"
	fields.ErrorCause = err.Error()
	if fields.Operation == "" {
		fields.Operation = "store.db"
	}
	d.log.Log(fields, "refused: %v", err)
	return err
}

// currentWriteSeq reads the global write ordinal inside the transaction, so
// every write of this batch is ordered after every write that committed before
// it and before every write that commits after.
func (d *DB) currentWriteSeq(ctx context.Context, tx *sql.Tx) (uint64, error) {
	var seq uint64
	if err := tx.QueryRowContext(ctx, `SELECT COALESCE(MAX(write_seq), 0) FROM entry`).Scan(&seq); err != nil {
		return 0, storagef(err, "reading the current write ordinal")
	}
	return seq, nil
}

// absorbedBefore reports whether this exact write already landed, by asking
// the WRITE LEDGER rather than the `entry` row.
//
// THE LEDGER IS THE WHOLE POINT. `entry.write_id` holds only the LATEST write
// applied to a row, so probing it answered "has this write landed?" with "is
// this write the most recent one?" — and a replay of a SUPERSEDED write (w1
// after w2 settled the same upsert_key) read as never-seen, was re-applied over
// the newer content, and bumped write_seq, re-delivering the regressed line to
// every live watcher. The ledger keeps one row per write ever APPLIED, so
// absorption is a single indexed lookup that no later write can erase.
func (d *DB) absorbedBefore(ctx context.Context, tx *sql.Tx, writeID string) (bool, error) {
	var one int
	switch err := tx.QueryRowContext(ctx, `SELECT 1 FROM write_ledger WHERE write_id = ?`, writeID).Scan(&one); {
	case err == nil:
		return true, nil
	case errors.Is(err, sql.ErrNoRows):
		return false, nil
	default:
		return false, storagef(err, "probing write_id %q", writeID)
	}
}

// recordApplied writes the ledger row for one applied write, IN THE SAME
// TRANSACTION as the row it applied. Split them and a crash between the two
// would either lose the absorption fact (a replay regresses the row) or claim
// one that never happened (a write is silently dropped).
func (d *DB) recordApplied(ctx context.Context, tx *sql.Tx, r routed, writeSeq uint64, now int64) error {
	const insertSQL = `INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms) VALUES (?,?,?,?)`
	if _, err := tx.ExecContext(ctx, insertSQL, r.writeID, r.upsertKey, writeSeq, now); err != nil {
		return storagef(err, "recording write_id %q in the write ledger", r.writeID)
	}
	return nil
}

// upsertEntry writes the row and returns the position it occupies.
//
// `position` IS NEVER IN THE UPDATE CLAUSE. That omission is the whole
// stability guarantee of a StoreItemPointer: an upsert supersedes the row's
// content whole and leaves its place in the book exactly where the first insert
// put it, so a caller paging through a book cannot have a settling unit teleport
// past it.
func (d *DB) upsertEntry(ctx context.Context, tx *sql.Tx, r routed, writeSeq uint64, now int64) (int64, error) {
	const upsertSQL = `INSERT INTO entry (
	    upsert_key, write_id, write_seq, plane, kind, book_agent_id, run_id, top_level, frame,
	    first_inserted_at_ms, last_written_at_ms)
	  VALUES (?,?,?,?,?,?,?,?,?,?,?)
	  ON CONFLICT(upsert_key) DO UPDATE SET
	    write_id = excluded.write_id,
	    write_seq = excluded.write_seq,
	    plane = excluded.plane,
	    kind = excluded.kind,
	    book_agent_id = excluded.book_agent_id,
	    run_id = excluded.run_id,
	    top_level = excluded.top_level,
	    frame = excluded.frame,
	    last_written_at_ms = excluded.last_written_at_ms
	  RETURNING position`
	var position int64
	err := tx.QueryRowContext(ctx, upsertSQL,
		r.upsertKey, r.writeID, writeSeq, r.plane, r.kind, r.book, r.runID, r.topLevel, r.frame, now, now,
	).Scan(&position)
	if err != nil {
		return 0, storagef(err, "writing entry upsert_key=%q write_id=%q", r.upsertKey, r.writeID)
	}
	return position, nil
}

// validateCursorState is the base function for store.v1.CursorState.
func validateCursorState(c *storev1.CursorState) error {
	if c.GetFileId() == "" {
		return invalidf("cursor_advance.file_id is empty — the cursor's identity is what survives the vendor's renames")
	}
	if c.GetPath() == "" {
		return invalidf("cursor_advance.path is empty (file_id=%q)", c.GetFileId())
	}
	if c.GetOffset() < 0 {
		return invalidf("cursor_advance.offset is negative (file_id=%q offset=%d)", c.GetFileId(), c.GetOffset())
	}
	return nil
}

// upsertCursor advances one file's reader position IN THE SAME TRANSACTION as
// the records read at it. That co-commit is the entire exactly-once contract:
// split them and a crash between the two either loses records or duplicates
// them.
func (d *DB) upsertCursor(ctx context.Context, tx *sql.Tx, c *storev1.CursorState, now int64) error {
	const upsertSQL = `INSERT INTO cursor (file_id, path, offset, carry, updated_at_ms)
	  VALUES (?, ?, ?, ?, ?)
	  ON CONFLICT(file_id) DO UPDATE SET
	    path = excluded.path, offset = excluded.offset,
	    carry = excluded.carry, updated_at_ms = excluded.updated_at_ms`
	if _, err := tx.ExecContext(ctx, upsertSQL, c.GetFileId(), c.GetPath(), c.GetOffset(), c.GetCarry(), now); err != nil {
		return storagef(err, "advancing cursor file_id=%q", c.GetFileId())
	}
	offset := c.GetOffset()
	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "cursor", FileID: c.GetFileId(), Path: c.GetPath(), Offset: &offset,
	}, "cursor advanced")
	return nil
}
