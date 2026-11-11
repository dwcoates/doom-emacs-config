package db

import (
	"context"
	"database/sql"
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// openask.go — the ask index: every page line that records an ask (a consent
// ask or a question batch), and whether the ask it records is still OPEN, so
// store.v1.GetLiveWork answers the asks a session's record holds a start for
// and no settle (GetLiveWorkSuccess.open_asks).
//
// WHY IT EXISTS. An ask is an open obligation exactly as live detached work is:
// only the permission gate of the shim that asked can settle it, and a shim
// that dies without a teardown leaves it open in the record forever, so every
// later reader draws a card nobody can answer (2026-11-11). The shim reads the
// open asks at session start and settles each as abandoned; this index is what
// answers them without walking every book.
//
// IT IS AN INDEX OVER PAGE LINES, kept like peer_delivery: one row per page
// line holding an ask, at the row's position, written in the write's own
// transaction and backfilled once from the stored frames. An ask's start and
// its settle share one upsert key, so they are one row at one position, and
// the row's `open` follows the latest frame written to it. A retired line keeps
// its row and is excluded by the reader's kind check, exactly as a page
// excludes it.

const askLineTable = "ask_line"

// errAskIndexDiverged is the cause of an ask_line row naming a row open that
// records no open ask: writeAskLine keeps the two in one transaction, so the
// index's invariant is broken and the store refuses rather than answering.
var errAskIndexDiverged = errors.New("the ask index and the row it indexes disagree")

// askLineDDL is the in-place table and its lineage index (db.go inPlaceTables).
// The book is copied beside the flag because the live-work read is
// book-then-open and an index cannot span two tables.
const askLineDDL = `CREATE TABLE IF NOT EXISTS ask_line (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  open          INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS ask_line_book_open ON ask_line(book_agent_id, open)`

const (
	// askLineUpsertSQL binds (position, book, open).
	askLineUpsertSQL = `INSERT INTO ask_line (position, book_agent_id, open) VALUES (?,?,?)
  ON CONFLICT(position) DO UPDATE SET book_agent_id = excluded.book_agent_id, open = excluded.open`

	// askLineDeleteSQL binds (position): the row is not, or is no longer, an ask.
	askLineDeleteSQL = `DELETE FROM ask_line WHERE position = ?`

	// askLineBackfillSQL binds (the page-line kind): every page line stored
	// before the table existed. Read once, in the create's own transaction.
	askLineBackfillSQL = `SELECT position, book_agent_id, frame FROM entry WHERE kind = ? AND book_agent_id IS NOT NULL`

	// liveAsksSQL binds (session, the page-line kind): every open ask line in
	// the session's lineage, oldest first, with its stored frame. At package
	// scope so the suite EXPLAINs the production text itself.
	liveAsksSQL = sessionLineageCTE + `
	  SELECT k.position, k.book_agent_id, e.frame FROM lineage l
	  CROSS JOIN ask_line k ON k.book_agent_id = l.agent_id
	  CROSS JOIN entry e ON e.position = k.position
	  WHERE k.open = 1 AND e.kind = ?
	  ORDER BY k.position ASC`
)

// askOf is the ask a stored page line records, and whether it is still open:
// (nil, false) when the line records no ask. Exactly one of the returned ask's
// arms is set.
func askOf(line *storev1.StorePageLine) (*conversationv1.SessionOpenAsk, bool) {
	update := line.GetAgentItem().GetAgentFrame().GetUpdate()
	agent := line.GetAgentItem().GetAgentFrame().GetAgentId()
	if q := update.GetQuestion(); q != nil {
		_, open := q.GetResult().(*conversationv1.AgentQuestion_Start)
		return &conversationv1.SessionOpenAsk{Agent: agent, Ask: &conversationv1.SessionOpenAsk_Question{Question: q}}, open
	}
	if p := update.GetPermission(); p != nil {
		_, open := p.GetResult().(*conversationv1.AgentPermission_Start)
		return &conversationv1.SessionOpenAsk{Agent: agent, Ask: &conversationv1.SessionOpenAsk_Permission{Permission: p}}, open
	}
	return nil, false
}

// writeAskLine keeps a booked row's ask_line row, IN THE WRITE'S OWN
// TRANSACTION: present exactly when the row is a page line recording an ask,
// and open exactly while the latest frame written to it is the ask's start. It
// is the one writer of the table besides its backfill.
func (d *DB) writeAskLine(ctx context.Context, tx *sql.Tx, position int64, book string, r routed) error {
	var ask *conversationv1.SessionOpenAsk
	open := false
	if r.kind == kindPageLine {
		ask, open = askOf(r.pageLine)
	}
	if ask == nil {
		if _, err := tx.ExecContext(ctx, askLineDeleteSQL, position); err != nil {
			return storagef(err, "clearing the ask line of the row at position %d", position)
		}
		return nil
	}
	if _, err := tx.ExecContext(ctx, askLineUpsertSQL, position, book, open); err != nil {
		return storagef(err, "recording the row at position %d as an ask (open=%t)", position, open)
	}
	return nil
}

// backfillAskLines indexes every ask stored before ask_line existed, so an ask
// a shim left open before this binary is answered by the next session start. A
// frame that cannot be decoded is a LOUD failure, as on every read path: the
// open fails (never nukes) rather than leaving an ask out of the index.
func backfillAskLines(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, askLineBackfillSQL, kindPageLine)
	if err != nil {
		return storagef(err, "reading the page lines to index their asks")
	}
	type askLine struct {
		position int64
		book     string
		open     bool
	}
	var lines []askLine
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		if err := rows.Scan(&position, &book, &frame); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return storagef(err, "scanning a page line to index its ask")
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		if ask, open := askOf(entry.GetAgentUpdate().GetServeableFrame()); ask != nil {
			lines = append(lines, askLine{position: position, book: book, open: open})
		}
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return storagef(err, "iterating the page lines to index their asks")
	}
	if err := rows.Close(); err != nil {
		return storagef(err, "closing the page-line listing")
	}
	for _, l := range lines {
		if _, err := tx.ExecContext(ctx, askLineUpsertSQL, l.position, l.book, l.open); err != nil {
			return storagef(err, "indexing the ask at position %d", l.position)
		}
	}
	return nil
}

// liveAsks answers every open ask in SESSION's lineage, oldest first, each as
// its stored start under the agent whose book holds it. A stored frame that
// cannot be decoded, or an indexed line that no longer records an open ask, is
// a storage failure: the index and the row it indexes disagree.
func (d *DB) liveAsks(ctx context.Context, session string) ([]*conversationv1.SessionOpenAsk, error) {
	rows, err := d.read.QueryContext(ctx, liveAsksSQL, session, kindPageLine)
	if err != nil {
		return nil, storagef(err, "scanning live asks")
	}
	defer rows.Close() //nolint:errcheck // a read-only listing; every failure is surfaced by Err
	var asks []*conversationv1.SessionOpenAsk
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		if err := rows.Scan(&position, &book, &frame); err != nil {
			return nil, storagef(err, "scanning a live ask")
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			return nil, storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		ask, open := askOf(entry.GetAgentUpdate().GetServeableFrame())
		if ask == nil || !open {
			return nil, storagef(errAskIndexDiverged, "the ask index names position %d in book %s open, and its row records no open ask", position, book)
		}
		// THE BOOK IS THE AGENT THE ASK STANDS IN: a frame is filed in the
		// book of the agent that carried it, so the index's own column says it.
		ask.Agent = &conversationv1.AgentId{Value: book}
		asks = append(asks, ask)
	}
	if err := rows.Err(); err != nil {
		return nil, storagef(err, "iterating live asks")
	}
	return asks, nil
}
