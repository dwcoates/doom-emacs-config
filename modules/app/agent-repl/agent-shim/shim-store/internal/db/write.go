package db

import (
	"context"
	"database/sql"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
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
	// Retired says this is not a line but the RETIREMENT of one (retire.go):
	// Line is the line as it was last served, at its own pointer, and a
	// watcher is sent it on the `retired` arm so it removes what it drew.
	Retired bool
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

// UnplacedEntry is one batch entry the store did NOT STORE: a page line its
// producer wrote `owner_unknown` (store.v1 StorePageLineOwnerUnknown) whose
// upsert_key no stored row holds, so it names no book the store could place it
// in. Nothing of it is committed and it is recorded at ERROR; the batch's other
// entries still commit.
type UnplacedEntry struct {
	UpsertKey string
	// Detail is the store's account, for a human and for logs.
	Detail string
}

// WriteResult reports what one batch did. Absorbed is not a lesser success:
// a replayed batch whose write_ids all landed before is the SAME durable
// answer, and the producer retires it from its retry buffer either way.
type WriteResult struct {
	Written  int
	Absorbed int
	// Skipped is the entries left unchanged as legacy book-conflicts (see
	// SkippedEntry). A skip commits nothing for that entry and keeps the stored
	// row, but the batch still commits every other entry.
	Skipped []SkippedEntry
	// Unplaced is the owner-unknown entries the store could not place (see
	// UnplacedEntry). Each committed nothing.
	Unplaced []UnplacedEntry
	Lines    []LineWritten
	// BashRows is the bash rows this write produced, ready for the WatchBashRun
	// fan-out. A run's rows are published exactly as a book's lines are.
	BashRows []BashRowWritten
	// Locators is how many vendor task pairings this batch stated. A COUNT,
	// like Shapes: a re-stated pairing is absorbed and still counts.
	Locators int
	// Claims is how many shell run claims this batch stated. A COUNT, like
	// Locators: a re-stated claim is absorbed and still counts.
	Claims int
	// Shapes is how many residue shape observations this batch folded into the
	// catalog. A COUNT and not a list: the store's answer is the same whichever
	// row each landed on, and the producer already knows which hashes it sent.
	Shapes int
	// Restamped is the entries whose content was already stored exactly as
	// written, apart from the conversion version: the row took the new
	// version and write id and NOTHING ELSE (no write_seq bump, no watcher
	// told), so a re-derivation that still converts a record the same way
	// costs its readers nothing.
	Restamped int
	// Retired is how many rows this batch's retirements retired. Each is also
	// in Lines with Retired set, when it had a book to publish to.
	Retired int
}

// bulkBounds bound ONE bulk transaction. A bulk batch whose entries exceed any
// of them is committed as several transactions, and the writer is yielded
// between them — so an interactive write that arrives while a bulk batch is
// being written waits for at most one of these, never for the whole batch.
//
// ALL THREE ARE NEEDED. Rows bound the index maintenance, bytes bound the WAL a
// transaction appends (one frame can be large), and time bounds what neither
// can predict: a loaded host, a cold page cache, a checkpoint. The time bound
// is checked AFTER each entry, so a transaction always applies at least one
// entry and a batch always makes progress.
//
// THE STORE'S SPLIT IS AUTHORITATIVE. A producer may bound its own batches too,
// and the sidecar does; nothing here depends on it, and nothing a producer sends
// can make a bulk transaction larger than this.
type bulkBounds struct {
	rows  int
	bytes int
	time  time.Duration
}

// The shipped bulk bounds. A healthy row costs ~3.5ms on the owner's largest
// measured database, so 64 rows is ~220ms of work on a quiet host and the
// 100ms time bound is what keeps a loaded one to a short hold.
const (
	DefaultBulkChunkRows  = 64
	DefaultBulkChunkBytes = 1 << 20
	DefaultBulkChunkTime  = 100 * time.Millisecond
)

func resolveBulkBounds(opts Options) bulkBounds {
	b := bulkBounds{rows: opts.BulkChunkRows, bytes: opts.BulkChunkBytes, time: opts.BulkChunkTime}
	if b.rows <= 0 {
		b.rows = DefaultBulkChunkRows
	}
	if b.bytes <= 0 {
		b.bytes = DefaultBulkChunkBytes
	}
	if b.time <= 0 {
		b.time = DefaultBulkChunkTime
	}
	return b
}

// WriteBatch commits one producer's batch — records and cursor advance — and
// returns what it wrote. The CLASS decides how it is committed, and the caller
// states it; the store never infers it from content.
//
// AN INTERACTIVE BATCH IS DURABLE OR NOTHING: one transaction. Every validation
// refusal happens BEFORE it opens and every storage failure rolls it back, so a
// caller that received a failure knows no row and no cursor moved. That
// certainty is what lets the shim hold the batch in a bounded in-memory buffer
// with no durable spill behind it.
//
// A BULK BATCH IS COMMITTED IN BOUNDED TRANSACTIONS (bulkBounds), yielding the
// writer between them so a queued interactive write goes next. The cursor
// advance and the shape observations ride the LAST transaction, so they still
// commit with the final entries they describe. A failure may therefore leave
// LEADING entries committed, never the cursor advance: the producer re-reads
// from its unadvanced cursor and the write ledger absorbs what already landed.
// On failure the result still carries what DID commit, so the caller can
// publish those lines to live watchers — a durable line nobody was told about
// would be absorbed on the retry and never streamed.
func (d *DB) WriteBatch(ctx context.Context, producer string, class WriteClass, batch *storev1.EntryBatch, shapes []*storev1.ShapeObservation) (WriteResult, error) {
	var result WriteResult
	base := logging.Fields{Operation: "store.db.write-batch", Table: "entry", Producer: producer, WriteClass: class.String()}

	if producer == "" {
		return result, d.refuse(base, invalidFieldf("producer", "producer is empty — every write is attributed"))
	}
	if !class.valid() {
		return result, d.refuse(base, invalidSitef(SiteWriteClassUnset, "write_class",
			"write_class is unset — every write states whether it is interactive or bulk, and the store never guesses"))
	}
	if batch == nil {
		return result, d.refuse(base, invalidFieldf("batch", "batch is unset"))
	}
	entries := batch.GetEntries()
	cursor := batch.GetCursorAdvance()
	locators := batch.GetAgentLocators()
	claims := batch.GetShellRunClaims()
	if len(entries) == 0 && cursor == nil && len(shapes) == 0 && len(locators) == 0 && len(claims) == 0 {
		return result, d.refuse(base, invalidFieldf("batch", "batch carries neither entries nor a cursor advance nor a shape observation nor an agent locator nor a shell run claim"))
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
	if err := validateRetirements(batch.GetRetirements(), cursor); err != nil {
		return result, d.refuse(base, err)
	}
	// THE SHAPE CATALOG IS VALIDATED WITH EVERYTHING ELSE, before any
	// transaction opens, so a malformed observation refuses the batch whole
	// rather than half-committing the records beside it.
	for i, shape := range shapes {
		if err := validateShapeObservation(shape, i); err != nil {
			return result, d.refuse(base, err)
		}
	}
	for i, locator := range locators {
		if err := validateAgentLocator(locator, i); err != nil {
			return result, d.refuse(base, err)
		}
	}
	for i, claim := range claims {
		if err := validateShellRunClaim(claim, i); err != nil {
			return result, d.refuse(base, err)
		}
	}

	remaining := routes
	for transaction := 1; ; transaction++ {
		// THE PAIRINGS RIDE THE FIRST TRANSACTION, the one carrying the agent's
		// first rows, so a lookup can answer from the moment any of them is
		// durable. A later transaction's failure leaves them committed, and the
		// re-read re-states them into the absorbing insert.
		var first pairings
		if transaction == 1 {
			first = pairings{locators: locators, claims: claims}
		}
		part, consumed, err := d.writeTransaction(ctx, base, class, transaction, remaining, cursor, shapes, first, batch.GetRetirements())
		if err != nil {
			if transaction > 1 {
				// THE LEADING TRANSACTIONS STAY COMMITTED, and the record says so,
				// because a reader of the refusal alone would conclude nothing
				// moved. The cursor did not advance, so the re-read replays them
				// into the ledger's absorption.
				d.log.LogVerbose(base, "bulk batch failed after %d committed transaction(s) written=%d absorbed=%d; the cursor advance did not commit and the leading entries will be absorbed on the re-read",
					transaction-1, result.Written, result.Absorbed)
			}
			return result, err
		}
		result.merge(part)
		remaining = remaining[consumed:]
		if len(remaining) == 0 {
			return result, nil
		}
	}
}

// merge folds one committed transaction's outcome into the batch's.
func (r *WriteResult) merge(part WriteResult) {
	r.Written += part.Written
	r.Absorbed += part.Absorbed
	r.Skipped = append(r.Skipped, part.Skipped...)
	r.Unplaced = append(r.Unplaced, part.Unplaced...)
	r.Lines = append(r.Lines, part.Lines...)
	r.BashRows = append(r.BashRows, part.BashRows...)
	r.Shapes += part.Shapes
	r.Restamped += part.Restamped
	r.Retired += part.Retired
	r.Locators += part.Locators
	r.Claims += part.Claims
}

// pairings are the batch's joins of a vendor id with the identity every plane
// books under: the subagent locators and the shell run claims. They ride a
// batch's first transaction.
type pairings struct {
	locators []*storev1.AgentLocator
	claims   []*storev1.ShellRunClaim
}

// writeTransaction commits ONE transaction of a batch: every remaining entry
// for an interactive write, or a bounded prefix of them for a bulk one. It
// reports how many entries it consumed, and applies the shapes and the cursor
// advance only when it consumed the last of them. Its outcome is returned only
// once it has COMMITTED; a rolled-back transaction contributes nothing.
func (d *DB) writeTransaction(ctx context.Context, base logging.Fields, class WriteClass, transaction int, routes []routed,
	cursor *storev1.CursorState, shapes []*storev1.ShapeObservation, joins pairings, retirements []*storev1.StoreRetirement) (WriteResult, int, error) {
	var result WriteResult
	consumed := 0

	// THE CLOCK STARTS BEFORE THE TRANSACTION, AND THE WAIT IS MEASURED APART
	// FROM THE WORK. A batch queues on the process-wide write slot behind
	// whatever else this store is writing, and timing only the total made that
	// queue look like a slow statement: the owner's store reported a 3822ms
	// `write_batch` for SIX rows whose statements are all single indexed seeks.
	// `lock_wait_ms` is the queue — the half that says to look at what ELSE is
	// writing — and `exec_ms` is the rest, the half that says to look at this
	// transaction or at the host it ran on.
	started := d.mono()
	var lockWait time.Duration
	defer func() {
		fields := base
		fields.LockWait = lockWait
		d.observeQuery(StatementWriteBatch, "entry", fields, started, int64(consumed))
		d.traceStatement(ctx, StatementWriteBatch, "entry", fields, int64(consumed))
		d.traceWriteTiming(StatementWriteBatch, fields, started, int64(consumed), transaction)
	}()

	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "entry", Producer: base.Producer, WriteClass: class.String(), Transaction: "BEGIN IMMEDIATE",
	}, "starting transaction %d entries_remaining=%d shapes=%d cursor_advance=%t", transaction, len(routes), len(shapes), cursor != nil)

	tx, release, err := d.beginWrite(ctx, class)
	lockWait = d.mono().Sub(started)
	if err != nil {
		// A CALLER THAT HUNG UP WHILE QUEUED GETS ITS OWN CANCELLATION BACK.
		// The database was never touched and nothing about it failed, so
		// dressing the wait's end as a storage failure would tell the producer
		// to retry a batch its own caller has already abandoned — and would
		// write an error record for a healthy store.
		if isContextError(err) {
			return WriteResult{}, 0, d.refuse(base, err)
		}
		return WriteResult{}, 0, d.refuse(base, storagef(err, "begin write transaction"))
	}
	// LIFO: the rollback runs first, then the slot is released. Releasing
	// before the transaction ended would let the next writer begin against a
	// lock this one still holds, which is the contention the gate removes.
	defer release()
	defer d.endTx(tx, base)
	began := d.mono()

	nextSeq, err := d.currentWriteSeq(ctx, tx)
	if err != nil {
		return WriteResult{}, 0, d.refuse(base, err)
	}
	now := d.now()

	frameBytes := 0
	for i, r := range routes {
		if class == WriteBulk && i > 0 && d.bulkFull(i, frameBytes, began) {
			break
		}
		consumed = i + 1
		frameBytes += len(r.frame)
		if err := d.applyEntry(ctx, tx, base, r, cursor, &nextSeq, now, &result); err != nil {
			return WriteResult{}, 0, err
		}
		if class == WriteBulk && d.bulkEntryApplied != nil {
			d.bulkEntryApplied()
		}
	}
	final := consumed == len(routes)

	if err := d.applyAgentLocators(ctx, tx, joins.locators, now); err != nil {
		return WriteResult{}, 0, d.refuse(base, err)
	}
	result.Locators = len(joins.locators)
	if err := d.applyShellRunClaims(ctx, tx, joins.claims, now); err != nil {
		return WriteResult{}, 0, d.refuse(base, err)
	}
	result.Claims = len(joins.claims)

	if final {
		// THE RETIREMENTS RIDE THE CURSOR ADVANCE'S TRANSACTION, AFTER EVERY
		// ENTRY. A record's retirement and the rows it now converts to are one
		// re-derivation, and the cursor advance past that record is what says it
		// happened: split them and a re-read cut short could retire a row whose
		// replacement never committed, or commit the position past a record
		// whose stale row still stands and will never be re-read.
		if err := d.applyRetirements(ctx, tx, base, retirements, &nextSeq, now, &result); err != nil {
			return WriteResult{}, 0, err
		}
		// THE CATALOG COMMITS WITH THE CURSOR ADVANCE THAT CONSUMED THE LINES IT
		// DESCRIBES. Split them and an advance that survived a lost catalog
		// write takes the shape with it: the bytes are past the cursor, nothing
		// re-reads them, and the shape is gone for good. Both ride the LAST
		// transaction of a split bulk batch for the same reason.
		if err := d.applyShapes(ctx, tx, shapes); err != nil {
			return WriteResult{}, 0, d.refuse(base, err)
		}
		result.Shapes = len(shapes)
		if cursor != nil {
			if err := d.upsertCursor(ctx, tx, cursor, now); err != nil {
				return WriteResult{}, 0, d.refuse(base, err)
			}
		}
	}

	if err := tx.Commit(); err != nil {
		return WriteResult{}, 0, d.refuse(base, storagef(err, "commit write transaction"))
	}
	if d.transactionCommitted != nil {
		d.transactionCommitted(class)
	}
	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "entry", Producer: base.Producer, WriteClass: class.String(), Transaction: "BEGIN IMMEDIATE",
	}, "transaction %d committed written=%d absorbed=%d restamped=%d retired=%d skipped=%d lines=%d shapes=%d locators=%d claims=%d cursor_advance=%t final=%t",
		transaction, result.Written, result.Absorbed, result.Restamped, result.Retired, len(result.Skipped), len(result.Lines), result.Shapes, result.Locators, result.Claims, final && cursor != nil, final)
	return result, consumed, nil
}

// bulkFull reports whether a bulk transaction that has applied `applied`
// entries carrying `frameBytes` bytes, and began executing at `began`, must
// commit before taking another.
func (d *DB) bulkFull(applied, frameBytes int, began time.Time) bool {
	return applied >= d.bulk.rows || frameBytes >= d.bulk.bytes || d.mono().Sub(began) >= d.bulk.time
}

// applyEntry applies one validated entry inside an open write transaction:
// absorb it, skip it, or upsert it and record it in the ledger.
func (d *DB) applyEntry(ctx context.Context, tx *sql.Tx, base logging.Fields, r routed, cursor *storev1.CursorState,
	nextSeq *uint64, now int64, result *WriteResult) error {
	fields := base
	fields.WriteID = r.writeID
	fields.UpsertKey = r.upsertKey
	if r.book.Valid {
		fields.BookAgentID = r.book.String
	}

	absorbed, err := d.absorbedBefore(ctx, tx, r.writeID)
	if err != nil {
		return d.refuse(fields, err)
	}
	if absorbed {
		result.Absorbed++
		d.log.LogVerbose(fields, "write absorbed: this write_id already landed entries_index=%d", r.index)
		return nil
	}

	if r.ownerUnknown {
		placed, unplaced, err := d.placeUnownedLine(ctx, tx, r)
		if err != nil {
			return d.refuse(fields, err)
		}
		if unplaced != nil {
			result.Unplaced = append(result.Unplaced, *unplaced)
			failed := fields
			failed.Level = "error"
			failed.ErrorCause = unplaced.Detail
			d.log.Log(failed, "entry not stored: it is owner_unknown and no stored row holds its upsert_key, so it names no book entries_index=%d", r.index)
			return nil
		}
		r = placed
		fields.BookAgentID = r.book.String
		d.log.LogVerbose(fields, "owner_unknown entry placed in the book that holds its upsert_key entries_index=%d", r.index)
	}

	skip, retiredFrom, prior, err := d.applyIdentityPolicy(ctx, tx, fields, &r)
	if err != nil {
		return d.refuse(fields, err)
	}
	if skip != nil {
		// A LEGACY BOOK-CONFLICT IS A SKIP, NOT A BATCH-FATAL REFUSAL. The
		// stored row is kept, this entry lands nothing (no upsert, no ledger
		// row, so a later replay skips it again — idempotent), and the batch's
		// other entries still commit.
		//
		// THE STORE LOGS THE PER-ENTRY SKIP AT DEBUG. A skip is a benign
		// idempotency outcome the store cannot contextualize; it still RETURNS
		// the skipped entry in result.Skipped so the sidecar — which knows the
		// ingest context — summarizes and decides. Left at warn, re-ingesting
		// the corpus emitted one warn per already-stored entry and flooded a
		// cold re-scan's strict harvest. The skip is still reported to the
		// caller; only the store's own severity drops.
		result.Skipped = append(result.Skipped, *skip)
		d.log.LogVerbose(fields, "entry skipped: upsert_key already names a row under book %q; the stored row is kept and this entry (book %q) is not applied — re-ingesting already-stored content is idempotent entries_index=%d",
			skip.FromBook, skip.ToBook, r.index)
		return nil
	}

	held, err := d.fileTerminalHeld(ctx, tx, r)
	if err != nil {
		return d.refuse(fields, err)
	}
	if held {
		// THE TRANSCRIPT'S TERMINAL OUTRANKS THE STREAM'S (owner ruling
		// 2026-09-27). The file plane's terminal carries the spool's exit code
		// and output; the stream plane's says only that the run ended. The
		// stored row is kept, this entry lands nothing (no upsert, no ledger
		// row, no watcher row), and the writer is answered success: this is an
		// expected precedence decision, not a fault.
		result.Absorbed++
		info := fields
		info.Level = "info"
		d.log.Log(info, "stream-plane bash terminal not applied: the row already holds the file plane's terminal, which outranks it entries_index=%d", r.index)
		return nil
	}

	if err := d.refuseLostOverSettled(ctx, tx, r); err != nil {
		return d.refuse(fields, err)
	}

	if prior != nil && retiredFrom == "" {
		unchanged, err := sameContentBarVersion(r, prior)
		if err != nil {
			return d.refuse(fields, err)
		}
		if unchanged {
			return d.restamp(ctx, tx, fields, r, cursor, prior, now, result)
		}
	}

	if r.workflowNotImplemented {
		// DURABLE, NEVER DROPPED, and loud: the row lands whole so nothing is
		// lost, and the warning says why nothing serves it yet.
		warn := fields
		warn.Level = "warn"
		d.log.Log(warn, "workflow ingestion not implemented this wave — the entry is stored as never-served residue and the workflow table is untouched entries_index=%d", r.index)
	}

	*nextSeq++
	seq := *nextSeq
	position, firstInsertedAtMs, err := d.upsertEntry(ctx, tx, r, seq, now)
	if err != nil {
		return d.refuse(fields, err)
	}
	// EVERY BOOKED ROW IS PLACED IN THE SAME TRANSACTION AS ITS WRITE, so the
	// order index never has a hole a page would have to skip or guess across.
	var place servedPlace
	if r.book.Valid {
		place = placeOf(r.entry, firstInsertedAtMs)
		if err := d.placeRow(ctx, tx, position, r.book.String, place); err != nil {
			return d.refuse(fields, err)
		}
	}
	if err := d.recordApplied(ctx, tx, r, cursor, seq, now); err != nil {
		return d.refuse(fields, err)
	}
	if err := d.applyLifecycle(ctx, tx, r, now); err != nil {
		return d.refuse(fields, err)
	}
	if retiredFrom != "" {
		// ONCE PER KEY by construction: the row now holds a real kind, so no
		// later write to it takes this branch again (until it is retired anew).
		info := fields
		info.Level = "info"
		what := "a retired keep-alive row"
		if retiredFrom == kindRetired {
			what = "a page line the file plane's re-derivation retired"
		}
		d.log.Log(info, "%s was superseded by a real record old_kind=%s new_kind=%s entries_index=%d",
			what, retiredFrom, r.kind, r.index)
	}
	result.Written++
	switch r.kind {
	case kindPageLine, kindHookDropped:
		// A HOOK LINE IS PUBLISHED LIKE ANY LINE, and that is the only time it
		// is ever served: the row keeps none of it (kindHookDropped).
		result.Lines = append(result.Lines, LineWritten{
			AgentID:  r.book.String,
			Line:     lineAt(position, r.pageLine, r.entry.GetTurn(), place),
			WriteSeq: seq,
		})
	case kindBash:
		result.BashRows = append(result.BashRows, BashRowWritten{
			RunID:    r.runID.String,
			Row:      r.bashRow,
			WriteSeq: seq,
		})
	}
	verbose := fields
	verbose.Position = encodePointer(position).GetValue()
	verbose.WriteSeq = seq
	d.log.LogVerbose(verbose, "entry written kind=%s entries_index=%d", r.kind, r.index)
	return nil
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
//
// THE ONE KIND CHANGE THAT IS A SUPERSESSION: a row stored under the RETIRED
// `keepalive` kind. The shim that predated "nothing of a keep-alive is stored"
// tagged a backgrounded subagent's frames arriving during a keep-alive turn,
// so such a row can sit under a real record's upsert_key; refusing the real
// record parked its producer's whole file. No keep-alive row is ever served or
// written again, so the retired row names nothing a pointer could hold, and
// the real record takes the key. The caller is told through `retiredFrom`.
//
// A HOOK LINE LANDING ON A ROW STORED AS A PAGE LINE IS NOT AN IDENTITY CHANGE
// EITHER (hookLineDropsStoredLine): it is the same line, written before the
// store stopped keeping hook records, and the write supersedes it in place as
// any same-kind write would.
//
// A ROW THE FILE PLANE'S RE-DERIVATION RETIRED (kindRetired) IS THE SECOND. It
// was a page line and still holds its book and position; a real page line under
// the same key in the SAME book takes it back where it stood, so a pointer
// already served for it names the line again. Into another book it is the
// ordinary book-move skip, and as any other kind the ordinary refusal.
//
// The stored row is handed back (`prior`) whenever it was superseded in place,
// so the caller can see whether the write changes anything at all.
func (d *DB) applyIdentityPolicy(ctx context.Context, tx *sql.Tx, fields logging.Fields, r *routed) (skip *SkippedEntry, retiredFrom string, prior *storedRow, err error) {
	row := &storedRow{}
	switch err := tx.QueryRowContext(ctx,
		`SELECT position, write_seq, plane, book_agent_id, kind, frame FROM entry WHERE upsert_key = ?`, r.upsertKey).
		Scan(&row.position, &row.writeSeq, &row.plane, &row.book, &row.kind, &row.frame); {
	case errors.Is(err, sql.ErrNoRows):
		// A first insert has no identity to change.
		return nil, "", nil, nil
	case err != nil:
		return nil, "", nil, storagef(err, "reading the identity of row %q", r.upsertKey)
	}
	if row.kind == kindKeepaliveRetired && r.kind != kindKeepaliveRetired {
		return nil, row.kind, nil, nil
	}
	if row.kind == kindRetired && r.kind == kindPageLine {
		if row.book.Valid != r.book.Valid || row.book.String != r.book.String {
			return &SkippedEntry{UpsertKey: r.upsertKey, FromBook: nullableBook(row.book), ToBook: nullableBook(r.book)}, "", nil, nil
		}
		return nil, row.kind, nil, d.carryStoredStamps(fields, r, row.frame)
	}
	if row.kind != r.kind && !hookLineDropsStoredLine(row.kind, r.kind) {
		return nil, "", nil, invalidSitef(SiteUpsertChangesIdentity,
			entryField(r.index, "agent_update"),
			"entries[%d] (upsert_key=%q) would change the row's kind from %q to %q — an upsert supersedes a row's content, never its identity",
			r.index, r.upsertKey, row.kind, r.kind)
	}
	if row.book.Valid != r.book.Valid || row.book.String != r.book.String {
		return &SkippedEntry{
			UpsertKey: r.upsertKey,
			FromBook:  nullableBook(row.book),
			ToBook:    nullableBook(r.book),
		}, "", nil, nil
	}
	if err := d.carryStoredStamps(fields, r, row.frame); err != nil {
		return nil, "", nil, err
	}
	return nil, "", row, nil
}

// hookLineDropsStoredLine reports whether a write of kind `incoming` onto a row
// of kind `stored` is a hook line superseding the page line its key held from
// before kindHookDropped existed (a shim that wrote a hook's start as a page
// line, its outcome arriving after the store began dropping hook records).
func hookLineDropsStoredLine(stored, incoming string) bool {
	return stored == kindPageLine && incoming == kindHookDropped
}

// placeUnownedLine places a page line its producer wrote `owner_unknown` in the
// book that already holds its upsert_key (store.v1 StorePageLineOwnerUnknown).
//
// THE PLACED LINE IS THE WRITE A KNOWN BOOK WOULD HAVE MADE. The entry is
// re-built with the stored row's book on the envelope, that book as the
// frame's attribution, and the stored row's top_level, and is classified
// again from the top -- so every check a booked line passes, it passes, and
// nothing after this point can tell the two apart.
//
// A KEY NO STORED ROW HOLDS, or one held only by a never-served row, names no
// book: the entry is answered unplaced, never filed in a guessed one.
func (d *DB) placeUnownedLine(ctx context.Context, tx *sql.Tx, r routed) (routed, *UnplacedEntry, error) {
	var book, topLevel sql.NullString
	switch err := tx.QueryRowContext(ctx,
		`SELECT book_agent_id, top_level FROM entry WHERE upsert_key = ?`, r.upsertKey).Scan(&book, &topLevel); {
	case errors.Is(err, sql.ErrNoRows):
		return routed{}, &UnplacedEntry{UpsertKey: r.upsertKey,
			Detail: "no stored row holds this upsert_key, so an owner_unknown write names no book"}, nil
	case err != nil:
		return routed{}, nil, storagef(err, "reading the book of row %q to place an owner_unknown write", r.upsertKey)
	}
	if !book.Valid {
		return routed{}, &UnplacedEntry{UpsertKey: r.upsertKey,
			Detail: "the stored row under this upsert_key is never served and has no book, so an owner_unknown write names none"}, nil
	}
	entry := proto.Clone(r.entry).(*storev1.StoreEntry)
	update := entry.GetAgentUpdate()
	line := update.GetServeableFrame()
	owner := &conversationv1.AgentId{Value: book.String}
	line.Book = &storev1.StorePageLine_PageAgentId{PageAgentId: owner}
	line.GetAgentItem().GetAgentFrame().AgentId = owner
	if topLevel.Valid {
		update.TopLevel = &conversationv1.AgentId{Value: topLevel.String}
	}
	placed, err := classify(entry, r.index)
	if err != nil {
		return routed{}, nil, err
	}
	return placed, nil, nil
}

// storedRow is the row an upsert found under its key: what the identity policy
// judged and what the unchanged-content test compares against.
type storedRow struct {
	position int64
	writeSeq uint64
	plane    int64
	book     sql.NullString
	kind     string
	frame    []byte
}

// sameContentBarVersion reports whether a FILE-PLANE write would store exactly
// what the row already holds, apart from the conversion version that produced
// it.
//
// IT IS WHAT MAKES A RE-DERIVATION FREE FOR ITS READERS. When the sidecar's
// conversion advances it re-reads whole transcripts, and nearly every record
// converts exactly as before; superseding each such row would bump its
// write_seq and re-deliver it to every standing watch, which for a reader is a
// whole conversation replayed for nothing. Only the file plane's own re-writes
// are judged this way: a stream-plane write is the shim telling live readers
// something, and whether they have heard it before is not the store's call.
//
// THE TURN AND THE PLACE HAVE ALREADY BEEN CARRIED (carryStoredStamps), so a
// write that differs only by omitting a stamp the row already holds is the
// same content.
func sameContentBarVersion(r routed, prior *storedRow) (bool, error) {
	if r.plane != planeFile || prior.plane != planeFile {
		return false, nil
	}
	stored := &storev1.StoreEntry{}
	if err := proto.Unmarshal(prior.frame, stored); err != nil {
		return false, storagef(err, "the stored frame of row %q cannot be decoded to compare its content", r.upsertKey)
	}
	incoming := proto.Clone(r.entry).(*storev1.StoreEntry)
	stored.ConversionVersion, incoming.ConversionVersion = nil, nil
	// The write id is a coordinate, not content: the version is digested into
	// it, so an unchanged record re-read under a new version mints a new one.
	stored.WriteId, incoming.WriteId = "", ""
	return proto.Equal(stored, incoming), nil
}

// restamp records a file-plane write whose content the row already holds: the
// row takes the write's id, frame (which carries the new conversion version)
// and ledger row, and NOTHING that a reader can observe changes — its write_seq
// stays, so no watch replays it, no line is published, and no lifecycle table
// is touched, since those were derived from the same content already.
func (d *DB) restamp(ctx context.Context, tx *sql.Tx, fields logging.Fields, r routed, cursor *storev1.CursorState, prior *storedRow, now int64, result *WriteResult) error {
	if _, err := tx.ExecContext(ctx, `UPDATE entry SET write_id = ?, frame = ? WHERE position = ?`, r.writeID, r.frame, prior.position); err != nil {
		return d.refuse(fields, storagef(err, "restamping row %q with its new conversion version", r.upsertKey))
	}
	if err := d.recordApplied(ctx, tx, r, cursor, prior.writeSeq, now); err != nil {
		return d.refuse(fields, err)
	}
	result.Restamped++
	verbose := fields
	verbose.Position = encodePointer(prior.position).GetValue()
	verbose.WriteSeq = prior.writeSeq
	d.log.LogVerbose(verbose, "entry restamped: the row already holds this content; it takes conversion_version=%d and nothing a reader observes changes entries_index=%d",
		r.entry.GetConversionVersion(), r.index)
	return nil
}

// carryStoredStamps keeps a row's FIRST turn stamp and its FIRST stated
// conversation place across every later write of it, which is what
// StoreEntry.turn and StoreEntry.place promise: a fact never moves between
// turns, and no write moves a row within its book, whichever plane wrote it
// last.
//
// THE PLANES DO NOT KNOW THE SAME THINGS. The shim stamps the turn it had open;
// the sidecar stamps only a turn the vendor's records name, and an agent-repl
// turn's id is in none of them — so the file plane's copy of a unit the stream
// plane already stamped arrives unstamped, and superseding the row whole would
// erase the stamp. An unstamped write therefore inherits the stored turn, and a
// write naming a DIFFERENT turn (the stream plane settling a detached unit
// while a later turn is open) keeps the stored one. A stored row with no stamp
// takes whatever the write carries, which is how a row the file plane wrote
// first is stamped when the stream plane's copy lands.
//
// THE PLACE FOLLOWS THE SAME RULE. The shim states the instant it observed a
// fact and the sidecar the vendor record's own timestamp, so the two planes'
// copies of one unit routinely state different places; the first one stated
// stands. A row stored with no place (a producer that could state none, or a
// row written before places existed) takes the first place a later write
// states — the one way a row moves within its book.
//
// The write's envelope is rewritten in place and re-serialized, because the
// stored blob is the one home of the turn (decodeLineAt serves it from there)
// and the place the order index is written from (placeRow).
func (d *DB) carryStoredStamps(fields logging.Fields, r *routed, stored []byte) error {
	prior := &storev1.StoreEntry{}
	if err := proto.Unmarshal(stored, prior); err != nil {
		return storagef(err, "the stored frame of row %q cannot be decoded to read its turn and place", r.upsertKey)
	}
	rewritten := false
	if kept := prior.GetTurn(); kept != nil {
		incoming := r.entry.GetTurn()
		if incoming.GetValue() != kept.GetValue() {
			if incoming != nil {
				d.log.LogVerbose(fields, "row keeps its first turn: stored_turn=%q write_turn=%q entries_index=%d",
					kept.GetValue(), incoming.GetValue(), r.index)
			}
			r.entry.Turn = kept
			rewritten = true
		}
	}
	if kept := prior.GetPlace(); kept != nil {
		incoming := r.entry.GetPlace()
		if !proto.Equal(incoming, kept) {
			if incoming != nil {
				d.log.LogVerbose(fields, "row keeps its first stated place: stored_place=%d.%d write_place=%d.%d entries_index=%d",
					kept.GetAtMs(), kept.GetOrdinal(), incoming.GetAtMs(), incoming.GetOrdinal(), r.index)
			}
			r.entry.Place = kept
			rewritten = true
		}
	}
	if !rewritten {
		return nil
	}
	frame, err := proto.Marshal(r.entry)
	if err != nil {
		return storagef(err, "re-serializing row %q with its stored turn and place", r.upsertKey)
	}
	r.frame = frame
	return nil
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
//
// IT ALSO RETURNS THE ROW'S FIRST-INSERT INSTANT, which no upsert rewrites: the
// receipt instant a row with no stated place is ordered and served by.
func (d *DB) upsertEntry(ctx context.Context, tx *sql.Tx, r routed, writeSeq uint64, now int64) (int64, int64, error) {
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
	  RETURNING position, first_inserted_at_ms`
	frame := r.frame
	if r.kind == kindHookDropped {
		// THE RECORD IS NOT KEPT: only the envelope's identity and stamps.
		stamps, err := hookStampsFrame(r.entry)
		if err != nil {
			return 0, 0, storagef(err, "serializing the stamps of hook line upsert_key=%q", r.upsertKey)
		}
		frame = stamps
	}
	var position, firstInsertedAtMs int64
	err := tx.QueryRowContext(ctx, upsertSQL,
		r.upsertKey, r.writeID, writeSeq, r.plane, r.kind, r.book, r.runID, r.topLevel, frame, now, now,
	).Scan(&position, &firstInsertedAtMs)
	if err != nil {
		return 0, 0, storagef(err, "writing entry upsert_key=%q write_id=%q", r.upsertKey, r.writeID)
	}
	return position, firstInsertedAtMs, nil
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
	return validateCursorConversion(c)
}

// validateCursorConversion is the base function for store.v1.CursorConversion
// on a cursor advance: REQUIRED, with a real version and a stated state.
//
// A POSITION WITH NO CONVERSION BEHIND IT IS ONE NO RE-DERIVATION CAN REASON
// ABOUT. The sidecar decides whether a file's rows predate its conversion from
// this and nothing else, so an advance that omitted it would read, on the next
// boot, as a file every byte of which a superseded conversion produced.
func validateCursorConversion(c *storev1.CursorState) error {
	conv := c.GetConversion()
	switch {
	case conv == nil:
		return invalidSitef(SiteCursorConversionUnset, "cursor_advance.conversion",
			"cursor_advance.conversion is unset (file_id=%q) — every advance states the conversion the file was read under", c.GetFileId())
	case conv.GetVersion() == 0:
		return invalidSitef(SiteCursorConversionUnset, "cursor_advance.conversion.version",
			"cursor_advance.conversion.version is 0 (file_id=%q), which names no conversion", c.GetFileId())
	}
	switch state := conv.GetState().(type) {
	case *storev1.CursorConversion_Current:
		return nil
	case *storev1.CursorConversion_Healing:
		if state.Healing.GetThrough() <= c.GetOffset() {
			return invalidSitef(SiteCursorConversionUnset, "cursor_advance.conversion.healing.through",
				"cursor_advance.conversion.healing.through=%d is not past the offset %d (file_id=%q) — a re-read that has reached where the older conversion stopped is `current`",
				state.Healing.GetThrough(), c.GetOffset(), c.GetFileId())
		}
		return nil
	default:
		return invalidSitef(SiteCursorConversionUnset, "cursor_advance.conversion.state",
			"cursor_advance.conversion sets no state arm (file_id=%q)", c.GetFileId())
	}
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
	// THE CONVERSION BOOKKEEPING IS PART OF THE ADVANCE, in the same
	// transaction: a position committed without it would be read back as one a
	// superseded conversion produced.
	var through sql.NullInt64
	if healing := c.GetConversion().GetHealing(); healing != nil {
		through = sql.NullInt64{Int64: healing.GetThrough(), Valid: true}
	}
	if _, err := tx.ExecContext(ctx, upsertConversionSQL, c.GetFileId(), c.GetConversion().GetVersion(), through); err != nil {
		return storagef(err, "recording the conversion of cursor file_id=%q", c.GetFileId())
	}
	offset := c.GetOffset()
	d.log.LogVerbose(logging.Fields{
		Operation: "store.db.write-batch", Table: "cursor", FileID: c.GetFileId(), Path: c.GetPath(), Offset: &offset,
	}, "cursor advanced conversion_version=%d healing=%t", c.GetConversion().GetVersion(), through.Valid)
	return nil
}

// upsertConversionSQL binds (file id, conversion version, healing_through or
// NULL). At package scope so the suite EXPLAINs the production text.
const upsertConversionSQL = `INSERT INTO cursor_conversion (file_id, version, healing_through)
  VALUES (?, ?, ?)
  ON CONFLICT(file_id) DO UPDATE SET
    version = excluded.version, healing_through = excluded.healing_through`

// fileTerminalHeld reports whether r is a stream-plane bash TERMINAL arriving
// at a row that already holds a file-plane terminal. That is the one
// supersession the store refuses by plane: for a bash run's terminal the file
// plane outranks the stream plane, never the reverse. Every other write —
// any non-bash row, any non-terminal arm, a file-plane write, a stream write
// over a stream-plane or non-terminal row — supersedes as usual.
func (d *DB) fileTerminalHeld(ctx context.Context, tx *sql.Tx, r routed) (bool, error) {
	if r.kind != kindBash || r.plane != planeStream || !BashRowIsTerminal(r.bashRow) {
		return false, nil
	}
	var plane int64
	var stored []byte
	switch err := tx.QueryRowContext(ctx,
		`SELECT plane, frame FROM entry WHERE upsert_key = ?`, r.upsertKey).Scan(&plane, &stored); {
	case errors.Is(err, sql.ErrNoRows):
		return false, nil
	case err != nil:
		return false, storagef(err, "reading the plane of bash row %q", r.upsertKey)
	}
	if plane != planeFile {
		return false, nil
	}
	entry := &storev1.StoreEntry{}
	if err := proto.Unmarshal(stored, entry); err != nil {
		return false, storagef(err, "the stored frame of bash row %q cannot be decoded", r.upsertKey)
	}
	return BashRowIsTerminal(entry.GetAgentUpdate().GetBash()), nil
}
