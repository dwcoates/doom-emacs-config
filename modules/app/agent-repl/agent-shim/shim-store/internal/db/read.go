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
	// Newest is the book's newest line BY PLACE as of the open — the line a
	// repaint would serve first — read in the same transaction as the page and
	// the pin, whatever the opening asked for. Nil when the book holds no
	// line. A tail-only caller anchors on it (its teardown head, its lossless
	// re-open mark, whether the book was empty) without reading a page.
	Newest *storev1.StoreItemPointer
}

// PageSize IS THE PAGE: the number of lines every page this store serves holds
// at most — the opening page, a continuation, a bounded `through` read. ONE size
// for the whole stack (owner ruling, docs/protobuf-design/feed-paging-on-demand.md
// change 1), owned by the store and stated nowhere else: no request field
// carries a budget, so no caller can make a page bigger or smaller.
const PageSize = 50

// Opening is what an open's first page carries — the request's `opening` arm.
// Its zero value is the repaint. It is built only through Repaint, CatchUp and
// TailOnly, so "a catch-up and a tail-only at once" cannot be stated.
type Opening struct {
	kind         openingKind
	knownThrough *storev1.StoreItemPointer
}

type openingKind int

const (
	openingRepaint openingKind = iota
	openingCatchUp
	openingTailOnly
)

// Repaint opens on the newest page.
func Repaint() Opening { return Opening{kind: openingRepaint} }

// CatchUp opens on the lines first written after the caller's own mark.
func CatchUp(knownThrough *storev1.StoreItemPointer) Opening {
	return Opening{kind: openingCatchUp, knownThrough: knownThrough}
}

// TailOnly opens on no lines at all: the tail begins after the newest line as
// of the open, and history is read only when a reader asks for it.
func TailOnly() Opening { return Opening{kind: openingTailOnly} }

// String names the opening for logs.
func (o Opening) String() string {
	switch o.kind {
	case openingCatchUp:
		return "known_through"
	case openingTailOnly:
		return "tail_only"
	default:
		return "repaint"
	}
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
//
// EVERY OPEN NAMES THE BOOK'S NEWEST LINE (OpenedPage.Newest), by place, so
// the head a repaint would lead with is stated even when the page is not
// served. By place rather than by write order because that is the head every
// reader means; a catch-up from it is still lossless, since `position > mark`
// is a superset of what was written after the open.
//
// A TAIL-ONLY OPEN SERVES NO LINES AND REPORTS THE FLOOR. The boundary arm
// describes what lies below THIS page, and `more` must point at the page's
// oldest line — an empty page has none, and no pointer can name "the top of
// the book" (`after` reads strictly BEFORE the line it names, so pointing at
// the newest line would lose it). So the empty page says there is nothing more
// to walk FROM IT; it is not a claim that the book is empty. A reader that
// later wants history starts where every reader does — a repaint (the newest
// page) — and walks older with ReadAgentPage from that page's `more`. The watch
// pin is still read in the same transaction, so the tail begins exactly after
// the newest line as of the open.
func (d *DB) OpenPage(ctx context.Context, agentID string, opening Opening) (OpenedPage, error) {
	base := logging.Fields{Operation: "store.db.open-page", Table: "entry", BookAgentID: agentID}
	if err := validateBook(agentID); err != nil {
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

	// REPAINT IS THE NEWEST PLACES; CATCH-UP IS WHAT WAS WRITTEN SINCE. A
	// catch-up is about what the caller has not been told, never about where it
	// sits: a row first written after the caller's mark but placed earlier in
	// the conversation (a transcript read late) is delivered rather than
	// skipped, and the page is still ordered by descending place.
	var lines []*storev1.StoreLineAt
	more := false
	if opening.kind != openingTailOnly {
		statement, args := pageNewestSQL, []any{agentID, kindPageLine}
		if opening.kind == openingCatchUp {
			knownThrough := opening.knownThrough
			position, err := decodePointer(knownThrough, "known_through")
			if err != nil {
				return OpenedPage{}, d.refuse(base, err)
			}
			if err := d.pointerInBook(ctx, tx, agentID, position, "known_through", knownThrough.GetValue()); err != nil {
				fields := base
				fields.Position = knownThrough.GetValue()
				return OpenedPage{}, d.refuse(fields, err)
			}
			statement, args = pageWrittenAfterSQL, []any{agentID, kindPageLine, position}
		}
		var err error
		lines, more, err = d.pageLines(ctx, tx, agentID, statement, args...)
		if err != nil {
			return OpenedPage{}, d.refuse(base, err)
		}
	}

	var pinSeq uint64
	if err := tx.QueryRowContext(ctx, `SELECT COALESCE(MAX(write_seq), 0) FROM entry`).Scan(&pinSeq); err != nil {
		return OpenedPage{}, d.refuse(base, storagef(err, "reading the watch pin"))
	}
	newest, err := newestLine(ctx, tx, agentID)
	if err != nil {
		return OpenedPage{}, d.refuse(base, err)
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
	d.log.LogVerbose(verbose, "page opened lines=%d more=%t opening=%s", len(lines), more, opening)
	return OpenedPage{Page: page, PinSeq: pinSeq, Newest: newest}, nil
}

// newestLine is the pointer of a book's newest page line by place, or nil for
// a book that holds none. It reads the same place index the repaint seeks.
func newestLine(ctx context.Context, tx *sql.Tx, agentID string) (*storev1.StoreItemPointer, error) {
	var position int64
	err := tx.QueryRowContext(ctx, newestLineSQL, agentID, kindPageLine).Scan(&position)
	switch {
	case err == nil:
		return encodePointer(position), nil
	case isNoRows(err):
		return nil, nil
	default:
		return nil, storagef(err, "reading the newest line of book %q", agentID)
	}
}

// ReadPage walks one book to the lines placed strictly BEFORE a served
// pointer's line. There is no first-page arm: the first page is the open's
// answer.
//
// THE BOUND IS THE NAMED LINE'S CURRENT PLACE, read in this transaction. A
// pointer names an item, never a place, so a line that gained its recorded
// place since it was served is walked on from where it sits now.
func (d *DB) ReadPage(ctx context.Context, agentID string, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error) {
	base := logging.Fields{Operation: "store.db.read-page", Table: "entry", BookAgentID: agentID}
	if err := validateBook(agentID); err != nil {
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
	bound, err := placeOfRow(ctx, tx, position)
	if err != nil {
		return nil, d.refuse(base, err)
	}
	lines, more, err := d.pageLines(ctx, tx, agentID, pageBeforeSQL, agentID, kindPageLine, bound.atMs, bound.ordinal, position)
	if err != nil {
		return nil, d.refuse(base, err)
	}
	success := continuationPage(lines, more)
	d.observeQuery(StatementReadPage, "entry", base, started, int64(len(lines)))
	d.traceStatement(ctx, StatementReadPage, "entry", base, int64(len(lines)))
	d.log.LogVerbose(base, "page read lines=%d more=%t", len(lines), more)
	return success, nil
}

// ReadPageThrough reads the newest lines of one book placed AT OR BEFORE an
// instant: the book as it stood then. A fork reads its parent's conversation up
// to the fork point this way, and `more` walks older with ReadPage as usual.
//
// A BOOK THE STORE HAS NEVER HEARD OF IS REFUSED (ErrUnknownAgent), never
// served empty: an empty page would tell a caller with a mistyped book exactly
// what it tells one reading a book that held nothing yet at that instant.
func (d *DB) ReadPageThrough(ctx context.Context, agentID string, throughAtMs int64) (*storev1.ReadAgentPageSuccess, error) {
	base := logging.Fields{Operation: "store.db.read-page", Table: "entry", BookAgentID: agentID}
	if err := validateBook(agentID); err != nil {
		return nil, d.refuse(base, err)
	}
	if throughAtMs <= 0 {
		return nil, d.refuse(base, invalidSitef(SiteThroughNotPositive, "through.at_ms",
			"through.at_ms is %d — a bound on conversation places is a positive instant", throughAtMs))
	}
	started := d.mono()

	tx, err := d.beginRead(ctx)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "begin read transaction"))
	}
	defer d.endTx(tx, base)

	if err := d.agentIsKnown(ctx, tx, agentID); err != nil {
		return nil, d.refuse(base, err)
	}
	lines, more, err := d.pageLines(ctx, tx, agentID, pageThroughSQL, agentID, kindPageLine, throughAtMs)
	if err != nil {
		return nil, d.refuse(base, err)
	}
	success := continuationPage(lines, more)
	d.observeQuery(StatementReadPage, "entry", base, started, int64(len(lines)))
	d.traceStatement(ctx, StatementReadPage, "entry", base, int64(len(lines)))
	d.log.LogVerbose(base, "page read through at_ms=%d lines=%d more=%t", throughAtMs, len(lines), more)
	return success, nil
}

// continuationPage wraps a ReadAgentPage's lines with its boundary.
func continuationPage(lines []*storev1.StoreLineAt, more bool) *storev1.ReadAgentPageSuccess {
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
	return success
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

	rows, err := d.read.QueryContext(ctx, linesSinceSQL, agentID, kindPageLine, kindRetired, afterSeq)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "replaying lines of book %q", agentID))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var out []LineWritten
	for rows.Next() {
		var position int64
		var seq uint64
		var kind string
		var frame []byte
		var atMs, ordinal, recorded sql.NullInt64
		if err := rows.Scan(&position, &seq, &kind, &frame, &atMs, &ordinal, &recorded); err != nil {
			return nil, d.refuse(base, storagef(err, "scanning a replayed line of book %q", agentID))
		}
		place, err := scanPlace(position, atMs, ordinal, recorded)
		if err != nil {
			return nil, d.refuse(base, err)
		}
		line, err := decodeLineAt(frame, position, place)
		if err != nil {
			return nil, d.refuse(base, err)
		}
		// A RETIRED ROW REPLAYS AS ITS RETIREMENT. Its frame is the line as it
		// was last served, so the watcher is handed exactly what to withdraw.
		out = append(out, LineWritten{
			AgentID:  agentID,
			Line:     line,
			WriteSeq: seq,
			Retired:  kind == kindRetired,
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
func validateBook(agentID string) error {
	if agentID == "" {
		return invalidFieldf("agent", "agent id value is empty")
	}
	return nil
}

// pointerInBook is the stale-pointer check. THE BOOK IS PART OF IT: a position
// that exists in some OTHER agent's book is stale for this one, and answering
// its page would serve one agent's lines under another's name.
func (d *DB) pointerInBook(ctx context.Context, tx *sql.Tx, agentID string, position int64, field, value string) error {
	var one int
	err := tx.QueryRowContext(ctx, pointerInBookSQL, position, agentID, kindPageLine, kindRetired, kindHookDropped).Scan(&one)
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
	// linesSinceSQL binds (book, the page-line kind, the retired kind, the
	// write_seq after which to replay). A retired row is replayed too — as its
	// retirement — because a watcher whose page was read before the row was
	// retired must still be told to withdraw it.
	linesSinceSQL = `SELECT e.position, e.write_seq, e.kind, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry e LEFT JOIN entry_place p ON p.position = e.position
	  WHERE e.book_agent_id = ? AND e.kind IN (?, ?) AND e.write_seq > ?
	  ORDER BY e.write_seq ASC`

	// pointerInBookSQL binds (position, book, the page-line kind, the retired
	// kind, the hook kind). A RETIRED ROW'S POSITION IS STILL A PLACE IN ITS
	// BOOK: a reader whose high-water mark was that line walks on from it rather
	// than being sent to repaint a book that only lost a line. A HOOK LINE'S is
	// too: it was published live with that pointer (kindHookDropped).
	pointerInBookSQL = `SELECT 1 FROM entry WHERE position = ? AND book_agent_id = ? AND kind IN (?, ?, ?)`
)

// THE PAGE STATEMENTS. A book is served in DESCENDING CONVERSATION PLACE —
// (at_ms, ordinal) from `entry_place`, with the row's position as the stable
// tiebreak, which carries no meaning — and every statement reads the place
// columns scanPlace turns into the served arm. Each asks for one more row than
// the page holds (the last bind), which is how the boundary is decided.
const (
	// pageNewestSQL binds (book, the page-line kind, limit): the repaint.
	// Driven from the place index, so the newest page is a seek from its top.
	pageNewestSQL = `SELECT e.position, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry_place p CROSS JOIN entry e ON e.position = p.position
	  WHERE p.book_agent_id = ? AND e.kind = ?
	  ORDER BY p.at_ms DESC, p.ordinal DESC, p.position DESC LIMIT ?`

	// newestLineSQL binds (book, the page-line kind): the position a repaint
	// would serve first, from the same place-index seek.
	newestLineSQL = `SELECT p.position
	  FROM entry_place p CROSS JOIN entry e ON e.position = p.position
	  WHERE p.book_agent_id = ? AND e.kind = ?
	  ORDER BY p.at_ms DESC, p.ordinal DESC, p.position DESC LIMIT 1`

	// pageBeforeSQL binds (book, the page-line kind, the named line's at_ms,
	// ordinal and position, limit): every line placed strictly before it.
	pageBeforeSQL = `SELECT e.position, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry_place p CROSS JOIN entry e ON e.position = p.position
	  WHERE p.book_agent_id = ? AND e.kind = ? AND (p.at_ms, p.ordinal, p.position) < (?, ?, ?)
	  ORDER BY p.at_ms DESC, p.ordinal DESC, p.position DESC LIMIT ?`

	// pageThroughSQL binds (book, the page-line kind, the inclusive bound's
	// at_ms, limit): the book as it stood at that instant, whatever the
	// ordinal.
	pageThroughSQL = `SELECT e.position, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry_place p CROSS JOIN entry e ON e.position = p.position
	  WHERE p.book_agent_id = ? AND e.kind = ? AND p.at_ms <= ?
	  ORDER BY p.at_ms DESC, p.ordinal DESC, p.position DESC LIMIT ?`

	// pageWrittenAfterSQL binds (book, the page-line kind, the caller's mark's
	// position, limit): the catch-up. `position` is FIRST-INSERT order, so
	// `position > mark` is exactly "first written after the mark"; driven from
	// the book's position index, it sorts only the rows written since.
	pageWrittenAfterSQL = `SELECT e.position, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry e LEFT JOIN entry_place p ON p.position = e.position
	  WHERE e.book_agent_id = ? AND e.kind = ? AND e.position > ?
	  ORDER BY p.at_ms DESC, p.ordinal DESC, e.position DESC LIMIT ?`
)

// pageLines reads one page — PageSize lines at most — of a book with one of the
// page statements above (its binds less the limit in `args`), and reports
// whether more lines remain below it.
//
// IT ASKS FOR ONE MORE ROW THAN THE PAGE HOLDS. That extra row is how the
// boundary arm is DECIDED rather than guessed: `more` when the row came back,
// `floor` when it did not. A count query beside the page could disagree with it
// under a concurrent write; one query cannot.
//
// `kind = page_line` is the never-served index doing its work: a keep-alive or
// a residue row carries no book at all, so no page can reach one.
func (d *DB) pageLines(ctx context.Context, tx *sql.Tx, agentID string, statement string, args ...any) ([]*storev1.StoreLineAt, bool, error) {
	rows, err := tx.QueryContext(ctx, statement, append(args, PageSize+1)...)
	if err != nil {
		return nil, false, storagef(err, "reading a page of book %q", agentID)
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var lines []*storev1.StoreLineAt
	more := false
	for rows.Next() {
		if len(lines) == PageSize {
			more = true
			break
		}
		var position int64
		var frame []byte
		var atMs, ordinal, recorded sql.NullInt64
		if err := rows.Scan(&position, &frame, &atMs, &ordinal, &recorded); err != nil {
			return nil, false, storagef(err, "scanning a page row of book %q", agentID)
		}
		place, err := scanPlace(position, atMs, ordinal, recorded)
		if err != nil {
			return nil, false, err
		}
		line, err := decodeLineAt(frame, position, place)
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
// (carryStoredStamps), so the blob is the one place it lives. THE PLACE IS THE
// ORDER INDEX'S (entry_place), read beside the frame by every serving path.
func decodeLineAt(frame []byte, position int64, place servedPlace) (*storev1.StoreLineAt, error) {
	entry := &storev1.StoreEntry{}
	if err := proto.Unmarshal(frame, entry); err != nil {
		return nil, storagef(err, "stored frame at position %d cannot be decoded", position)
	}
	line := entry.GetAgentUpdate().GetServeableFrame()
	if line == nil {
		return nil, storagef(errNotAPageLine, "stored frame at position %d is indexed as a page line but carries none", position)
	}
	return lineAt(position, line, entry.GetTurn(), place), nil
}
