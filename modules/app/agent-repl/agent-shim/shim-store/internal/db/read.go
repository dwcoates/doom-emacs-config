package db

import (
	"context"
	"database/sql"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// OpenedPage is the answer to an open: the page itself, and the write ordinal
// the watch that follows it must begin after.
//
// PinSeq IS TAKEN INSIDE THE PAGE'S OWN TRANSACTION. Taking it before would
// re-deliver lines the page already carries; taking it after would drop lines
// written in between. Neither is recoverable by the caller, because the caller
// cannot see the window it lost.
type OpenedPage struct {
	Page   *storev1.AgentSessionPage
	PinSeq uint64
}

// beginRead opens the transaction a PURE READ runs in, ON THE READ POOL.
//
// IT IS A DIFFERENT POOL, AND THAT IS THE STRUCTURAL HALF. `_txlock` is a
// property of the CONNECTION, so the write handle's `immediate` reached every
// transaction opened on it — a page repaint queued for the write lock a
// producer held, and could be refused by it. The read pool's DSN carries no
// `_txlock` and carries `query_only(true)`, so the write lock is not reachable
// from it and a write attempted on it is refused by SQLite outright.
//
// IT IS ALSO DEFERRED, AND THAT IS THE PER-CALL HALF. A read that writes
// nothing has no upgrade to fear and no business holding the write lock at all:
// WAL exists so readers never contend with the writer, and an
// `sql.TxOptions{ReadOnly: true}` makes the driver issue a plain `BEGIN`, which
// takes only a read snapshot. Kept alongside the pool rather than replaced by
// it: it states the intent at the call site, and it costs nothing.
//
// A READ THAT TOOK THE WRITE LOCK COULD BE REFUSED BY A BUSY ONE, AND WAS. The
// owner's store answered two `OpenAgentSession` calls with
// `store.db.open-page` ERROR "begin read transaction: database is locked (5)
// (SQLITE_BUSY)" while a producer held the write lock — a page repaint failed
// outright because somebody else was writing, which is precisely the failure
// mode WAL is chosen to remove.
//
// THE PIN AND THE PAGE STILL COME FROM ONE SNAPSHOT. A deferred transaction in
// WAL takes its read snapshot at its FIRST statement and holds that snapshot
// until it ends, so `OpenPage`'s watch pin is read from the same view of the
// database as the lines it answers with — which is the property that comment
// asks for, and it never needed the write lock to get it.
//
// THE SNAPSHOT ENDS BEFORE THE READ RETURNS, AND THE TRANSACTION'S CONTEXT IS
// WHAT GUARANTEES IT. A transaction begun on a cancellable context is rolled
// back by database/sql's own goroutine the moment that context ends, and the
// caller's deferred Rollback then returns ErrTxDone at once, without waiting
// for that rollback to finish. So a cancelled read returned while its snapshot
// was still held, and nothing bounded how long it stayed held. The
// transaction is therefore begun on context.WithoutCancel(ctx): its end is
// always the caller's own deferred rollback, on the caller's goroutine. The
// statements inside it still run on the caller's ctx, so a cancellation still
// interrupts the read at once. A deferred BEGIN takes no lock and reads
// nothing, so the uncancellable part never waits on anything.
func (d *DB) beginRead(ctx context.Context) (*sql.Tx, error) {
	return d.read.BeginTx(context.WithoutCancel(ctx), &sql.TxOptions{ReadOnly: true})
}

// OpenPage answers one agent's opening page.
//
// AN AGENT THE STORE HAS HEARD OF BUT THAT HAS SAID NOTHING IS A LEGAL, EMPTY
// BOOK — an empty page at the floor. A freshly spawned subagent has an `agent`
// row from its spawn frame before it says a word, so it is openable and
// watchable immediately.
//
// AN AGENT ID THE STORE HOLDS NO `agent` ROW FOR NAMES NO BOOK AND IS REFUSED
// (ErrUnknownAgent, Landing 7). Serving it an empty page told a caller with a
// stale or mistyped target exactly what it told a caller watching a live agent
// that had not spoken yet, so the two were indistinguishable and the mistake
// looked like patience. The `agent` table is the register that separates them:
// every page-line write ensures a row there, so "no row" is "never heard of".
func (d *DB) OpenPage(ctx context.Context, agentID string, pageSize uint32, knownThrough *storev1.StoreItemPointer) (OpenedPage, error) {
	base := logging.Fields{Operation: "store.db.open-page", Table: "entry", BookAgentID: agentID}
	if err := validateBook(agentID, pageSize); err != nil {
		return OpenedPage{}, d.refuse(base, err)
	}
	started := d.mono()

	tx, err := d.beginRead(ctx)
	if err != nil {
		return OpenedPage{}, d.refuse(base, storagef(err, "begin read transaction"))
	}
	defer d.endTx(tx, base)

	// THE REGISTER IS ASKED FIRST, before the pointer. A known_through against
	// a book that does not exist is stale only as a consequence of the book not
	// existing, and answering `stale_pointer` would send the caller off to
	// repaint a book nobody ever kept.
	if err := d.agentIsKnown(ctx, tx, agentID); err != nil {
		return OpenedPage{}, d.refuse(base, err)
	}

	var floorPosition int64
	if knownThrough != nil {
		position, err := decodePointer(knownThrough, "known_through")
		if err != nil {
			return OpenedPage{}, d.refuse(base, err)
		}
		if err := d.pointerInBook(ctx, tx, agentID, position, "known_through", knownThrough.GetValue()); err != nil {
			fields := base
			fields.Position = knownThrough.GetValue()
			return OpenedPage{}, d.refuse(fields, err)
		}
		floorPosition = position
	}

	lines, more, err := d.pageLines(ctx, tx, agentID, pageSize, pageBoundAboveFloor, floorPosition)
	if err != nil {
		return OpenedPage{}, d.refuse(base, err)
	}

	var pinSeq uint64
	if err := tx.QueryRowContext(ctx, `SELECT COALESCE(MAX(write_seq), 0) FROM entry`).Scan(&pinSeq); err != nil {
		return OpenedPage{}, d.refuse(base, storagef(err, "reading the watch pin"))
	}

	page := &storev1.AgentSessionPage{Lines: lines}
	if more {
		page.Boundary = &storev1.AgentSessionPage_More{
			More: &storev1.ReadAgentPageMore{LastItem: lines[len(lines)-1].GetAt()},
		}
	} else {
		page.Boundary = &storev1.AgentSessionPage_Floor{Floor: &storev1.ReadAgentPageFloor{}}
	}
	d.observeQuery(StatementOpenPage, "entry", base, started, int64(len(lines)))
	d.traceStatement(ctx, StatementOpenPage, "entry", base, int64(len(lines)))
	verbose := base
	verbose.WriteSeq = pinSeq
	d.log.LogVerbose(verbose, "page opened lines=%d more=%t known_through=%t", len(lines), more, knownThrough != nil)
	return OpenedPage{Page: page, PinSeq: pinSeq}, nil
}

// ReadPage walks one book OLDER than a served pointer. There is no first-page
// arm: the first page is the open's answer and this verb only ever continues.
func (d *DB) ReadPage(ctx context.Context, agentID string, pageSize uint32, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	base := logging.Fields{Operation: "store.db.read-page", Table: "entry", BookAgentID: agentID}
	if err := validateBook(agentID, pageSize); err != nil {
		return nil, d.refuse(base, err)
	}
	position, err := decodePointer(after, "after")
	if err != nil {
		return nil, d.refuse(base, err)
	}
	started := d.mono()

	tx, err := d.beginRead(ctx)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "begin read transaction"))
	}
	defer d.endTx(tx, base)

	if err := d.pointerInBook(ctx, tx, agentID, position, "after", after.GetValue()); err != nil {
		fields := base
		fields.Position = after.GetValue()
		return nil, d.refuse(fields, err)
	}
	lines, more, err := d.pageLines(ctx, tx, agentID, pageSize, pageBoundBelow, position)
	if err != nil {
		return nil, d.refuse(base, err)
	}

	// EVERY CONTINUATION LINE CARRIES ITS OWN POINTER, exactly as the opening
	// page's do. A reader walking older must be able to echo a real position
	// back — for a later ReadAgentPage, for a known_through re-open — and a
	// page that served bare lines forced it to mint a placeholder mark or to
	// track positions it was never given.
	success := &storev1.ReadAgentPageSuccess{Lines: lines}
	if more {
		success.Boundary = &storev1.ReadAgentPageSuccess_More{
			More: &storev1.ReadAgentPageMore{LastItem: lines[len(lines)-1].GetAt()},
		}
	} else {
		success.Boundary = &storev1.ReadAgentPageSuccess_Floor{Floor: &storev1.ReadAgentPageFloor{}}
	}
	d.observeQuery(StatementReadPage, "entry", base, started, int64(len(lines)))
	d.traceStatement(ctx, StatementReadPage, "entry", base, int64(len(lines)))
	d.log.LogVerbose(base, "page read lines=%d more=%t", len(lines), more)
	return success, nil
}

// LinesSince is the watch replay: every page line of one book written after a
// pin, in WRITE order rather than page order.
//
// TWO ORDERINGS AND THIS ONE IS write_seq, deliberately. A watcher must receive
// an UPSERT of an old row — the row keeps its original position and therefore
// its original pointer, but it is new information, and ordering the replay by
// position would place it back where the caller has already read past.
func (d *DB) LinesSince(ctx context.Context, agentID string, afterSeq uint64) ([]LineWritten, error) {
	base := logging.Fields{Operation: "store.db.lines-since", Table: "entry", BookAgentID: agentID, WriteSeq: afterSeq}
	if agentID == "" {
		return nil, d.refuse(base, invalidFieldf("agent", "agent id value is empty"))
	}
	started := d.mono()

	rows, err := d.read.QueryContext(ctx, linesSinceSQL, agentID, kindPageLine, afterSeq)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "replaying lines of book %q", agentID))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var out []LineWritten
	for rows.Next() {
		var position int64
		var seq uint64
		var frame []byte
		if err := rows.Scan(&position, &seq, &frame); err != nil {
			return nil, d.refuse(base, storagef(err, "scanning a replayed line of book %q", agentID))
		}
		line, err := decodeLineAt(frame, position)
		if err != nil {
			return nil, d.refuse(base, err)
		}
		out = append(out, LineWritten{
			AgentID:  agentID,
			Line:     line,
			WriteSeq: seq,
		})
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "iterating replayed lines of book %q", agentID))
	}
	d.observeQuery(StatementLinesSince, "entry", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementLinesSince, "entry", base, int64(len(out)))
	d.log.LogVerbose(base, "replay read lines=%d", len(out))
	return out, nil
}

// agentIsKnown reports whether the store holds an `agent` row for this id, and
// refuses with ErrUnknownAgent when it does not.
//
// IT ASKS THE `agent` TABLE AND NOT `entry`. The entry spine answers "has this
// agent said anything", which is a different question: a spawned subagent is
// registered by its spawn frame and is legitimately openable with an empty
// book. Asking the spine would refuse every agent for as long as it stayed
// quiet.
func (d *DB) agentIsKnown(ctx context.Context, tx *sql.Tx, agentID string) error {
	var one int
	err := tx.QueryRowContext(ctx, `SELECT 1 FROM agent WHERE agent_id = ?`, agentID).Scan(&one)
	switch {
	case err == nil:
		return nil
	case isNoRows(err):
		return unknownAgentf("agent", "agent %q names no book of this store", agentID)
	default:
		return storagef(err, "looking up agent %q", agentID)
	}
}

// validateBook is the shared refusal for the two paging verbs.
func validateBook(agentID string, pageSize uint32) error {
	if agentID == "" {
		return invalidFieldf("agent", "agent id value is empty")
	}
	if pageSize == 0 {
		return invalidFieldf("page_size", "page_size is zero — a page with no budget is not a page")
	}
	return nil
}

// pointerInBook is the stale-pointer check. THE BOOK IS PART OF IT: a position
// that exists in some OTHER agent's book is stale for this one, and answering
// its page would serve one agent's lines under another's name.
func (d *DB) pointerInBook(ctx context.Context, tx *sql.Tx, agentID string, position int64, field, value string) error {
	var one int
	err := tx.QueryRowContext(ctx, pointerInBookSQL, position, agentID, kindPageLine).Scan(&one)
	switch {
	case err == nil:
		return nil
	case isNoRows(err):
		return stalePointerf(field, "%s %q names no line of book %q", field, value, agentID)
	default:
		return storagef(err, "validating %s against book %q", field, agentID)
	}
}

// THE READ STATEMENTS ARE AT PACKAGE SCOPE so the suite EXPLAINs the
// production text itself (read_test.go) rather than a copy that can drift.
const (
	// linesSinceSQL binds (book, the page-line kind, the write_seq after which
	// to replay).
	linesSinceSQL = `SELECT position, write_seq, frame FROM entry
	  WHERE book_agent_id = ? AND kind = ? AND write_seq > ?
	  ORDER BY write_seq ASC`

	// pointerInBookSQL binds (position, book, the page-line kind).
	pointerInBookSQL = `SELECT 1 FROM entry WHERE position = ? AND book_agent_id = ? AND kind = ?`

	// pageBoundAboveFloor is OpenPage's bound: every line above the floor.
	pageBoundAboveFloor = `position > ?`
	// pageBoundBelow is ReadPage's bound: every line older than the pointer.
	pageBoundBelow = `position < ?`
)

// pageLinesSQL is one page's statement under one of the two bounds above. It
// binds (book, the page-line kind, the bound, the page size plus one).
func pageLinesSQL(boundClause string) string {
	return `SELECT position, frame FROM entry
	  WHERE book_agent_id = ? AND kind = ? AND ` + boundClause + `
	  ORDER BY position DESC LIMIT ?`
}

// pageLines reads one page of a book, newest first, and reports whether older
// lines remain below it.
//
// IT ASKS FOR ONE MORE ROW THAN THE PAGE HOLDS. That extra row is how the
// boundary arm is DECIDED rather than guessed: `more` when the row came back,
// `floor` when it did not. A count query beside the page could disagree with it
// under a concurrent write; one query cannot.
//
// `kind = page_line` is the never-served index doing its work: a keep-alive or
// a residue row carries no book at all, so no page can reach one.
func (d *DB) pageLines(ctx context.Context, tx *sql.Tx, agentID string, pageSize uint32, boundClause string, bound int64) ([]*storev1.StoreLineAt, bool, error) {
	rows, err := tx.QueryContext(ctx, pageLinesSQL(boundClause), agentID, kindPageLine, bound, int64(pageSize)+1)
	if err != nil {
		return nil, false, storagef(err, "reading a page of book %q", agentID)
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var lines []*storev1.StoreLineAt
	more := false
	for rows.Next() {
		if uint32(len(lines)) == pageSize {
			more = true
			break
		}
		var position int64
		var frame []byte
		if err := rows.Scan(&position, &frame); err != nil {
			return nil, false, storagef(err, "scanning a page row of book %q", agentID)
		}
		line, err := decodeLineAt(frame, position)
		if err != nil {
			return nil, false, err
		}
		lines = append(lines, line)
	}
	if err := rows.Err(); err != nil {
		return nil, false, storagef(err, "iterating a page of book %q", agentID)
	}
	return lines, more, nil
}

// decodeLineAt recovers the served line, at its position and with the turn its
// row is stamped with, from a stored frame.
//
// A ROW THAT CANNOT BE DECODED IS A LOUD FAILURE, never a skipped line. The
// blob was written by this very package from a message it had already
// validated, so failing to read one back means the file is damaged — and
// serving the page with a hole in it would report that damage as an agent that
// simply said less than it did.
//
// THE TURN IS THE STORED ENVELOPE'S, which is the row's first stamp: the write
// path carries it forward into every later write of the row
// (carryStoredTurn), so the blob is the one place it lives.
func decodeLineAt(frame []byte, position int64) (*storev1.StoreLineAt, error) {
	entry := &storev1.StoreEntry{}
	if err := proto.Unmarshal(frame, entry); err != nil {
		return nil, storagef(err, "stored frame at position %d cannot be decoded", position)
	}
	line := entry.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		return nil, storagef(errNotAPageLine, "stored frame at position %d is indexed as a page line but carries none", position)
	}
	return &storev1.StoreLineAt{At: encodePointer(position), Line: line, Turn: entry.GetTurn()}, nil
}
