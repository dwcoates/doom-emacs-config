package db

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
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

// SkippedEntry is one batch entry the store left UNCHANGED because its
// upsert_key already names a row under a DIFFERENT book.
//
// RE-INGESTING ALREADY-STORED CONTENT IS IDEMPOTENT. A corrected converter
// re-reading the corpus writes the same cross-plane activity key under its
// now-right book, which disagrees with the legacy row an earlier ingest wrote;
// moving the row would teleport every pointer already handed out for it, so the
// stored row is kept and this entry is skipped. The skip is REPORTED rather
// than swallowed so the producer can fold it into its own catch-up summary, and
// it is a per-entry skip rather than a batch-fatal refusal, so the batch's
// genuinely-new entries still commit.
type SkippedEntry struct {
	UpsertKey string
	// FromBook is the book the stored row keeps, rendered for a human: "(none)"
	// for a never-served row, the agent id otherwise.
	FromBook string
	// ToBook is the book this skipped entry would have moved the row to.
	ToBook string
}

// WriteResult reports what one batch did. Absorbed is not a lesser success:
// a replayed batch whose write_ids all landed before is the SAME durable
// answer, and the producer retires it from its retry buffer either way.
type WriteResult struct {
	Written  int
	Absorbed int
	// Settled is how many entries restated a unit's non-concluding state
	// after the stored row had already concluded it, and were not applied
	// (see concludedUnitStands).
	Settled int
	// Skipped is the entries left unchanged as legacy book-conflicts (see
	// SkippedEntry). A skip commits nothing for that entry and keeps the stored
	// row, but the batch still commits every other entry.
	Skipped []SkippedEntry
	Lines   []LineWritten
	// BashRows is the bash rows this write produced, ready for the WatchBashRun
	// fan-out. A run's rows are published exactly as a book's lines are.
	BashRows []BashRowWritten
	// Shapes is how many residue shape observations this batch folded into the
	// catalog. A COUNT and not a list: the store's answer is the same whichever
	// row each landed on, and the producer already knows which hashes it sent.
	Shapes int
}

// WriteBatch commits one producer's batch — records and cursor advance — as ONE
// transaction, and returns the page lines it wrote.
//
// DURABLE OR NOTHING. Every validation refusal happens BEFORE the transaction
// opens and every storage failure rolls it back, so a caller that received a
// failure knows with certainty that no row and no cursor moved. That certainty
// is what lets the producer hold the batch in a bounded in-memory buffer with
// no durable spill behind it.
func (d *DB) WriteBatch(ctx context.Context, producer string, batch *storev1.EntryBatch, shapes []*storev1.ShapeObservation) (WriteResult, error) {
	var result WriteResult
	base := logging.Fields{Operation: "store.db.write-batch", Table: "entry", Producer: producer}

	if producer == "" {
		return result, d.refuse(base, invalidFieldf("producer", "producer is empty — every write is attributed"))
	}
	if batch == nil {
		return result, d.refuse(base, invalidFieldf("batch", "batch is unset"))
	}
	entries := batch.GetEntries()
	cursor := batch.GetCursorAdvance()
	if len(entries) == 0 && cursor == nil && len(shapes) == 0 {
		return result, d.refuse(base, invalidFieldf("batch", "batch carries neither entries nor a cursor advance nor a shape observation"))
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
	// THE SHAPE CATALOG IS VALIDATED WITH EVERYTHING ELSE, before the
	// transaction opens, so a malformed observation refuses the batch whole
	// rather than half-committing the records beside it.
	for i, shape := range shapes {
		if err := validateShapeObservation(shape, i); err != nil {
			return result, d.refuse(base, err)
		}
	}

	// THE CLOCK STARTS BEFORE THE TRANSACTION, AND THE WAIT IS MEASURED APART
	// FROM THE WORK. A batch queues on the process-wide write slot behind
	// whatever else this store is writing, and Timing only the total made that
	// queue look like a slow statement: the owner's store reported a 3822ms
	// `write_batch` for SIX rows whose statements are all single indexed seeks,
	// and the record blamed index maintenance for time no index spent.
	// `lock_wait_ms` is the half an operator can act on — it says to look at
	// what ELSE is writing, not for a missing index.
	//
	// WHAT IT MEASURES IS NOW THE IN-PROCESS QUEUE, which is the same number an
	// operator wanted and a truthful one: before the gate it was time spent
	// inside SQLite's busy handler, which ended either in a write or — nine
	// times on 2026-09-13 — in a SQLITE_BUSY refusal after the whole 5s
	// timeout. A wait here always ends in a turn.
	started := d.mono()
	var lockWait time.Duration
	defer func() {
		base.LockWait = lockWait
		d.observeQuery(StatementWriteBatch, "entry", base, started, int64(len(entries)))
		d.traceStatement(ctx, StatementWriteBatch, "entry", base, int64(len(entries)))
	}()

	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "entry", Producer: producer, Transaction: "BEGIN IMMEDIATE",
	}, "starting transaction entries=%d shapes=%d cursor_advance=%t", len(entries), len(shapes), cursor != nil)

	tx, release, err := d.beginWrite(ctx)
	lockWait = d.mono().Sub(started)
	if err != nil {
		// A CALLER THAT HUNG UP WHILE QUEUED GETS ITS OWN CANCELLATION BACK.
		// The database was never touched and nothing about it failed, so
		// dressing the wait's end as a storage failure would tell the producer
		// to retry a batch its own caller has already abandoned — and would
		// write an error record for a healthy store.
		if isContextError(err) {
			return WriteResult{}, d.refuse(base, err)
		}
		return WriteResult{}, d.refuse(base, storagef(err, "begin write transaction"))
	}
	// LIFO: the rollback runs first, then the slot is released. Releasing
	// before the transaction ended would let the next writer begin against a
	// lock this one still holds, which is the contention the gate removes.
	defer release()
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

		skip, err := d.applyIdentityPolicy(ctx, tx, r)
		if err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		if skip != nil {
			// A LEGACY BOOK-CONFLICT IS A SKIP, NOT A BATCH-FATAL REFUSAL. The
			// stored row is kept, this entry lands nothing (no upsert, no ledger
			// row, so a later replay skips it again — idempotent), and the
			// batch's other entries still commit.
			//
			// THE STORE LOGS THE PER-ENTRY SKIP AT DEBUG. A skip is a benign
			// idempotency outcome the store cannot contextualize; it still
			// RETURNS the skipped entry in result.Skipped so the sidecar — which
			// knows the ingest context — summarizes and decides. Left at warn,
			// re-ingesting the corpus emitted one warn per already-stored entry
			// and flooded a cold re-scan's strict harvest. The skip is still
			// reported to the caller; only the store's own severity drops.
			result.Skipped = append(result.Skipped, *skip)
			d.log.LogVerbose(fields, "entry skipped: upsert_key already names a row under book %q; the stored row is kept and this entry (book %q) is not applied — re-ingesting already-stored content is idempotent entries_index=%d",
				skip.FromBook, skip.ToBook, i)
			continue
		}

		stands, err := d.concludedUnitStands(ctx, tx, r)
		if err != nil {
			return WriteResult{}, d.refuse(fields, err)
		}
		if stands {
			result.Settled++
			d.log.LogVerbose(fields, "entry not applied: the stored row already concludes this unit, and this write restates a state before its conclusion entries_index=%d", i)
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
		if err := d.recordApplied(ctx, tx, r, cursor, nextSeq, now); err != nil {
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

	// THE CATALOG COMMITS WITH THE CURSOR ADVANCE THAT CONSUMED THE LINES IT
	// DESCRIBES. Split them and an advance that survived a lost catalog write
	// takes the shape with it: the bytes are past the cursor, nothing re-reads
	// them, and the shape is gone for good.
	if err := d.applyShapes(ctx, tx, shapes); err != nil {
		return WriteResult{}, d.refuse(base, err)
	}
	result.Shapes = len(shapes)

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
	}, "transaction committed written=%d absorbed=%d settled=%d skipped=%d lines=%d shapes=%d cursor_advance=%t",
		result.Written, result.Absorbed, result.Settled, len(result.Skipped), len(result.Lines), result.Shapes, cursor != nil)
	return result, nil
}

// refuse records one refusal and hands the error back for the server to shape
// into a typed failure arm.
//
// WHO OWNS THE RECORD DEPENDS ON WHOSE FAULT IT IS, and that is the whole rule.
//
//   - A REFUSED REQUEST (ErrInvalid, ErrStalePointer) belongs to the CALL, and
//     only the server knows the call: its rpc, its request id, its producer.
//     This layer's record would name a statement and a table and tie the
//     refusal to nothing, so it is a VERBOSE trace here and the server writes
//     the single normal-level record. Emitting both put two normal-level
//     records on one refusal and made "every error is logged exactly once" false
//     wherever anyone counted.
//   - A STALE POINTER IS NOT AN ERROR AT ALL. It is an ordinary race — the
//     caller walked a book that moved — and its recovery is a repaint. Logging
//     it at `error` meant a healthy store wrote error records during normal
//     operation, which is exactly how an error log stops being read.
//   - A CANCELED CALL IS NOBODY'S FAULT. The caller's context ended before the
//     statement did, so the database never failed and no operator action
//     exists; it is recorded at `info` and the error is still returned.
//   - A STORAGE FAILURE is this layer's own, with statement and table context
//     nothing above can supply, so it stays a normal-level `error` record here
//     and the server answers with a verbose trace instead of a second one.
func (d *DB) refuse(fields logging.Fields, err error) error {
	fields.ErrorCause = err.Error()
	if fields.Operation == "" {
		fields.Operation = "store.db"
	}
	if errors.Is(err, ErrInvalid) || errors.Is(err, ErrStalePointer) || errors.Is(err, ErrUnknownAgent) {
		fields.Level = "debug"
		if site := RefusalSite(err); site != "" {
			fields.RefusalSite = site
		}
		d.log.LogVerbose(fields, "refused: %v", err)
		return err
	}
	// A CALLER THAT HUNG UP IS NOT A STORAGE FAILURE. Every statement here runs
	// under the request's own context, so a client that closed its connection,
	// a watch whose consumer went away, or an rpc whose deadline passed
	// cancels the statement mid-flight and the driver hands back
	// context.Canceled. Nothing about the database went wrong, and nothing an
	// operator can do would have prevented it: the store's own live store
	// logged `store.db.live-work` at ERROR for a GetLiveWork the caller
	// abandoned, which is a healthy store writing an error record during
	// ordinary operation — exactly how an error log stops being read, and the
	// same reasoning that already put a stale pointer below `error`.
	//
	// IT IS STILL RECORDED, at normal verbosity, and the error is still
	// RETURNED unchanged for the server to shape into its failure arm. An
	// abandoned call that left no record at all would be indistinguishable
	// from one that never arrived.
	if isContextError(err) {
		fields.Level = "info"
		d.log.Log(fields, "abandoned: the caller's context ended before the statement finished: %v", err)
		return err
	}
	fields.Level = "error"
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
// THE ROW IS STAMPED WITH THE BATCH'S SOURCE POSITION WHERE THERE IS ONE, and
// that stamp is the whole basis of the ledger's retention (see prune.go). It is
// the batch's `cursor_advance` — the file these rows were read from and the
// offset the producer's NEXT read starts at, which is strictly above every
// offset the batch's own rows came from, so subtracting from it can only ever
// keep a row too long. A FILE-plane entry in a batch that advanced no cursor,
// and every STREAM-plane entry, are stamped NULL and are kept forever: neither
// names a byte the store could measure a re-read against.
func (d *DB) recordApplied(ctx context.Context, tx *sql.Tx, r routed, cursor *storev1.CursorState, writeSeq uint64, now int64) error {
	const insertSQL = `INSERT INTO write_ledger (write_id, upsert_key, write_seq, applied_at_ms, source_file_id, source_offset)
	  VALUES (?,?,?,?,?,?)`
	var sourceFile sql.NullString
	var sourceOffset sql.NullInt64
	if r.plane == planeFile && cursor != nil && cursor.GetFileId() != "" {
		sourceFile = sql.NullString{String: cursor.GetFileId(), Valid: true}
		sourceOffset = sql.NullInt64{Int64: cursor.GetOffset(), Valid: true}
	}
	if _, err := tx.ExecContext(ctx, insertSQL, r.writeID, r.upsertKey, writeSeq, now, sourceFile, sourceOffset); err != nil {
		return storagef(err, "recording write_id %q in the write ledger", r.writeID)
	}
	return nil
}

// applyIdentityPolicy decides what the batch does with an entry whose
// upsert_key already names a stored row. `upsert_key` names one thing, and an
// upsert supersedes that thing's CONTENT, never its identity — so a write that
// would give the row a different identity is not a supersession. There are two
// kinds of identity change, and they are NOT the same fault:
//
//   - A KIND CHANGE is a corruption with no legitimate cause: a served page
//     line becoming an unservable residue row (or the reverse) under a pointer
//     that still exists. Nothing ever re-ingests a row as a different KIND of
//     thing, so this stays a batch-fatal refusal, exactly as before. The
//     returned error is the strict SiteUpsertChangesIdentity refusal, still
//     available for any caller that wants the strict verdict.
//   - A BOOK MOVE is what a corrected re-ingest of ALREADY-STORED content looks
//     like: an earlier ingest booked the row under book A and the converter now
//     books the same key under its corrected book B. Moving the row would
//     teleport every pointer already handed out for it out of the book the
//     caller read it from, so the stored row is KEPT and the entry is SKIPPED —
//     reported to the caller (never swallowed), but not fatal to the batch. A
//     first insert has no identity to change, and a same-book supersede keeps
//     it, so both return (nil, nil) and the caller applies the upsert.
func (d *DB) applyIdentityPolicy(ctx context.Context, tx *sql.Tx, r routed) (*SkippedEntry, error) {
	var book sql.NullString
	var kind string
	switch err := tx.QueryRowContext(ctx,
		`SELECT book_agent_id, kind FROM entry WHERE upsert_key = ?`, r.upsertKey).Scan(&book, &kind); {
	case errors.Is(err, sql.ErrNoRows):
		// A first insert has no identity to change.
		return nil, nil
	case err != nil:
		return nil, storagef(err, "reading the identity of row %q", r.upsertKey)
	}
	if kind != r.kind {
		return nil, invalidSitef(SiteUpsertChangesIdentity,
			entryField(r.index, "agent_update"),
			"entries[%d] (upsert_key=%q) would change the row's kind from %q to %q — an upsert supersedes a row's content, never its identity",
			r.index, r.upsertKey, kind, r.kind)
	}
	if book.Valid != r.book.Valid || book.String != r.book.String {
		return &SkippedEntry{
			UpsertKey: r.upsertKey,
			FromBook:  nullableBook(book),
			ToBook:    nullableBook(r.book),
		}, nil
	}
	return nil, nil
}

// concludedUnitStands reports whether the stored row for this entry's unit has
// already CONCLUDED it while this entry restates a state before the
// conclusion, in which case the entry is not applied.
//
// A UNIT'S ROW NEVER WALKS BACK FROM ITS CONCLUSION. The two planes write one
// unit under ONE key (`activity:<id>`) and neither waits for the other, so the
// sidecar can read a tool call's transcript line — its START — after the
// shim's stream plane has already written the call's success. Applied, that
// start superseded the success, bumped the row's write order and re-delivered
// it to every watcher: the daemon redrew a loaded skill card as running and
// nothing ever settled it again (TestSkillNamedAndArgsParameterized,
// 2026-09-23). A conclusion superseding a conclusion is still applied — the
// later plane may state it more fully — and so is anything on a row that has
// not concluded.
func (d *DB) concludedUnitStands(ctx context.Context, tx *sql.Tx, r routed) (bool, error) {
	incoming := activityOf(r.entry)
	if incoming == nil || activityIsTerminal(incoming) {
		return false, nil
	}
	var blob []byte
	switch err := tx.QueryRowContext(ctx,
		`SELECT frame FROM entry WHERE upsert_key = ?`, r.upsertKey).Scan(&blob); {
	case errors.Is(err, sql.ErrNoRows):
		return false, nil
	case err != nil:
		return false, storagef(err, "reading the stored state of row %q", r.upsertKey)
	}
	var stored storev1.StoreEntry
	if err := proto.Unmarshal(blob, &stored); err != nil {
		return false, storagef(err, "decoding the stored state of row %q", r.upsertKey)
	}
	standing := activityOf(&stored)
	return standing != nil && activityIsTerminal(standing), nil
}

// activityOf answers the activity an entry carries, nil when it carries none.
func activityOf(entry *storev1.StoreEntry) *conversationv1.AgentActivity {
	return entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
}

// nullableBook renders a book column for a human, distinguishing the never-served
// NULL from an agent whose id happens to be empty (which cannot occur).
func nullableBook(book sql.NullString) string {
	if !book.Valid {
		return "(none)"
	}
	return book.String
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
		return invalidFieldf("cursor_advance.file_id", "cursor_advance.file_id is empty — the cursor's identity is what survives the vendor's renames")
	}
	if c.GetPath() == "" {
		return invalidFieldf("cursor_advance.path", "cursor_advance.path is empty (file_id=%q)", c.GetFileId())
	}
	if c.GetOffset() < 0 {
		return invalidFieldf("cursor_advance.offset", "cursor_advance.offset is negative (file_id=%q offset=%d)", c.GetFileId(), c.GetOffset())
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
