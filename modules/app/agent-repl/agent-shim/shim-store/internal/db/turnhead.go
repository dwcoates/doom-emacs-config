package db

import (
	"context"
	"database/sql"

	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// turnhead.go — THE TURN HEADS OF A PAGE (store.v1 AgentSessionPage.heads and
// ReadAgentPageSuccess.heads; docs/protobuf-design/complete-pages.md).
//
// A PAGE CARRIES EVERYTHING IT TOUCHES THAT IS PERSISTED IN THE STORE (owner
// ruling, 2026-10-12). What a page's lines depend on to draw is their turn's
// prompt, so every page carries, for each turn a line of it is stamped with,
// that turn's PROMPT LINE — the page line holding the turn's AgentPrompt — when
// that line is placed below the page: newest first, each turn at most once. A
// turn whose prompt is on the page, or that the store holds no prompt line for,
// contributes none.
//
// THE TURN AND THE ITEM ARM LIVE ONLY IN THE FRAME BLOB, so a lookup by turn
// needs its own index: `turn_prompt`, one row per prompt line stamped with a
// turn, keyed by position and indexed by (book, turn). writeTurnPrompt keeps it
// in the write's own transaction, and a page reads its heads through it with
// ONE statement, whatever the number of lines or turns.
//
// EACH HEAD SAYS WHETHER STEPS SIT BETWEEN IT AND THE PAGE (StoreTurnHead's
// `between` arm): `steps_between` when some page line stamped with the turn is
// placed strictly between the prompt line and the page's lowest line, by the
// page order (at_ms, ordinal, position); `adjacent` otherwise. That is asked of
// a second index, `turn_line`: one row per page line stamped with a turn, its
// place copied beside it, indexed by (book, turn, place). The same statement
// answers it with one EXISTS per head, which seeks the first line of the turn
// above the prompt and stops there.

const (
	turnPromptTable = "turn_prompt"
	turnLineTable   = "turn_line"
)

// turnLineDDL is the in-place table and its ordered lookup index (db.go
// inPlaceTables). The book AND THE PLACE are copied from entry_place because
// the question is "the next line of this turn above this place", and an index
// cannot span two tables. writeTurnLine writes the place from the same value
// placeRow wrote, in the same transaction, so the copy never disagrees with
// the order index; a retired line keeps its row and is excluded by the
// reader's kind check, exactly as a page excludes it.
const turnLineDDL = `CREATE TABLE IF NOT EXISTS turn_line (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  turn_id       TEXT    NOT NULL,
  at_ms         INTEGER NOT NULL,
  ordinal       INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS turn_line_book_turn_order ON turn_line(book_agent_id, turn_id, at_ms, ordinal, position)`

// turnPromptDDL is the in-place table and its lookup index (db.go
// inPlaceTables). The position is the entry row's; the book is copied, as in
// entry_place, because the lookup is book-then-turn and an index cannot span
// two tables.
const turnPromptDDL = `CREATE TABLE IF NOT EXISTS turn_prompt (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  turn_id       TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS turn_prompt_book_turn ON turn_prompt(book_agent_id, turn_id)`

const (
	// turnPromptUpsertSQL binds (position, book, turn). A row's book and turn
	// never change once stamped (applyIdentityPolicy, carryStoredStamps), so
	// the conflict only re-states what is there — except for a row first
	// written with no turn that gains one, which this records.
	turnPromptUpsertSQL = `INSERT INTO turn_prompt (position, book_agent_id, turn_id) VALUES (?,?,?)
  ON CONFLICT(position) DO UPDATE SET book_agent_id = excluded.book_agent_id, turn_id = excluded.turn_id`

	// turnPromptDeleteSQL binds (position): the row is not, or is no longer, a
	// stamped prompt line.
	turnPromptDeleteSQL = `DELETE FROM turn_prompt WHERE position = ?`

	// turnPromptBackfillSQL binds (the page-line kind): every page line stored
	// before the table existed. Read once, in the create's own transaction.
	turnPromptBackfillSQL = `SELECT position, book_agent_id, frame FROM entry WHERE kind = ? AND book_agent_id IS NOT NULL`

	// turnLineUpsertSQL binds (position, book, turn, at_ms, ordinal). The place
	// moves only when a row that had no stated place gains its first one
	// (placeRow), and this re-states it in the same write.
	turnLineUpsertSQL = `INSERT INTO turn_line (position, book_agent_id, turn_id, at_ms, ordinal) VALUES (?,?,?,?,?)
  ON CONFLICT(position) DO UPDATE SET book_agent_id = excluded.book_agent_id, turn_id = excluded.turn_id,
    at_ms = excluded.at_ms, ordinal = excluded.ordinal`

	// turnLineDeleteSQL binds (position): the row is not, or is no longer, a
	// page line stamped with a turn.
	turnLineDeleteSQL = `DELETE FROM turn_line WHERE position = ?`

	// turnLineBackfillSQL binds (the page-line kind): every page line stored
	// before turn_line existed, with its place. Read once, in the create's own
	// transaction; entry_place precedes it in inPlaceTables, so every booked
	// row has its place by then.
	turnLineBackfillSQL = `SELECT e.position, e.book_agent_id, e.frame, p.at_ms, p.ordinal, p.recorded
	  FROM entry e LEFT JOIN entry_place p ON p.position = e.position
	  WHERE e.kind = ? AND e.book_agent_id IS NOT NULL`

	// turnHeadsSQL binds, in order: the page's lowest line's at_ms, ordinal and
	// position and the page-line kind (the `between` question); then the book,
	// the turn ids (one `?` each in the `%s`), the page-line kind, and the
	// lowest line's at_ms, ordinal and position again. It answers the prompt
	// lines of those turns placed strictly below the page, newest place first,
	// each with `between_steps`: 1 when a page line of its turn is placed
	// strictly between it and the page's lowest line. Driven from the (book,
	// turn) index, so it seeks one short range per turn, and each
	// `between_steps` is one seek of turn_line's (book, turn, place) index that
	// stops at the first page line above the prompt. A retired line is neither
	// a head nor a step.
	turnHeadsSQL = `SELECT t.position, e.frame, p.at_ms, p.ordinal, p.recorded,
	    EXISTS (SELECT 1 FROM turn_line l CROSS JOIN entry s ON s.position = l.position
	      WHERE l.book_agent_id = t.book_agent_id AND l.turn_id = t.turn_id
	        AND (l.at_ms, l.ordinal, l.position) > (p.at_ms, p.ordinal, p.position)
	        AND (l.at_ms, l.ordinal, l.position) < (?, ?, ?)
	        AND s.kind = ?) AS between_steps
	  FROM turn_prompt t
	  CROSS JOIN entry e ON e.position = t.position
	  CROSS JOIN entry_place p ON p.position = t.position
	  WHERE t.book_agent_id = ? AND t.turn_id IN (%s) AND e.kind = ?
	    AND (p.at_ms, p.ordinal, p.position) < (?, ?, ?)
	  ORDER BY p.at_ms DESC, p.ordinal DESC, p.position DESC`
)

// promptTurn is the turn a stored entry is the prompt line of, or "" when it is
// not a prompt line stamped with a turn.
func promptTurn(entry *storev1.StoreEntry) string {
	line := entry.GetAgentUpdate().GetServeableFrame()
	if line.GetAgentItem().GetAgentPrompt() == nil {
		return ""
	}
	return entry.GetTurn().GetValue()
}

// writeTurnPrompt keeps a booked row's turn_prompt row, IN THE WRITE'S OWN
// TRANSACTION: present exactly when the row is a page line holding a prompt and
// stamped with a turn. It is the one writer of the table besides its backfill.
func (d *DB) writeTurnPrompt(ctx context.Context, tx *sql.Tx, position int64, book string, r routed) error {
	turn := ""
	if r.kind == kindPageLine {
		turn = promptTurn(r.entry)
	}
	if turn == "" {
		if _, err := tx.ExecContext(ctx, turnPromptDeleteSQL, position); err != nil {
			return storagef(err, "clearing the turn prompt of the row at position %d", position)
		}
		return nil
	}
	if _, err := tx.ExecContext(ctx, turnPromptUpsertSQL, position, book, turn); err != nil {
		return storagef(err, "recording the row at position %d as the prompt of turn %q", position, turn)
	}
	return nil
}

// backfillTurnPrompts indexes every prompt line stored before turn_prompt
// existed. A frame that cannot be decoded is a LOUD failure, as on every read
// path: the open fails (never nukes) rather than leaving a turn's prompt out of
// the index, which would serve its pages without their head.
func backfillTurnPrompts(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, turnPromptBackfillSQL, kindPageLine)
	if err != nil {
		return storagef(err, "reading the page lines to index their prompts")
	}
	type prompt struct {
		position int64
		book     string
		turn     string
	}
	var prompts []prompt
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		if err := rows.Scan(&position, &book, &frame); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return storagef(err, "scanning a page line to index its prompt")
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		if turn := promptTurn(entry); turn != "" {
			prompts = append(prompts, prompt{position: position, book: book, turn: turn})
		}
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return storagef(err, "iterating the page lines to index their prompts")
	}
	if err := rows.Close(); err != nil {
		return storagef(err, "closing the page-line listing")
	}
	for _, p := range prompts {
		if _, err := tx.ExecContext(ctx, turnPromptUpsertSQL, p.position, p.book, p.turn); err != nil {
			return storagef(err, "indexing the prompt line at position %d of turn %q", p.position, p.turn)
		}
	}
	return nil
}

// lineTurn is the turn a stored page line is stamped with, or "" when it is
// not a page line or carries no turn.
func lineTurn(r routed) string {
	if r.kind != kindPageLine {
		return ""
	}
	return r.entry.GetTurn().GetValue()
}

// writeTurnLine keeps a booked row's turn_line row, IN THE WRITE'S OWN
// TRANSACTION and from the place placeRow just wrote: present exactly when the
// row is a page line stamped with a turn. It is the one writer of the table
// besides its backfill.
func (d *DB) writeTurnLine(ctx context.Context, tx *sql.Tx, position int64, book string, r routed, place servedPlace) error {
	turn := lineTurn(r)
	if turn == "" {
		if _, err := tx.ExecContext(ctx, turnLineDeleteSQL, position); err != nil {
			return storagef(err, "clearing the turn line of the row at position %d", position)
		}
		return nil
	}
	if _, err := tx.ExecContext(ctx, turnLineUpsertSQL, position, book, turn, place.atMs, place.ordinal); err != nil {
		return storagef(err, "recording the row at position %d as a line of turn %q", position, turn)
	}
	return nil
}

// backfillTurnLines indexes every page line stamped with a turn that was
// stored before turn_line existed. A frame that cannot be decoded, or a booked
// row with no place, is a LOUD failure: the open fails (never nukes) rather
// than leaving a step out of the index, which would serve its prompt as
// adjacent when steps sit between.
func backfillTurnLines(ctx context.Context, tx *sql.Tx) error {
	rows, err := tx.QueryContext(ctx, turnLineBackfillSQL, kindPageLine)
	if err != nil {
		return storagef(err, "reading the page lines to index their turns")
	}
	type line struct {
		position int64
		book     string
		turn     string
		place    servedPlace
	}
	var lines []line
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		var atMs, ordinal, recorded sql.NullInt64
		if err := rows.Scan(&position, &book, &frame, &atMs, &ordinal, &recorded); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return storagef(err, "scanning a page line to index its turn")
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		turn := entry.GetTurn().GetValue()
		if turn == "" {
			continue
		}
		place, err := scanPlace(position, atMs, ordinal, recorded)
		if err != nil {
			rows.Close() //nolint:errcheck // the place is already missing
			return err
		}
		lines = append(lines, line{position: position, book: book, turn: turn, place: place})
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return storagef(err, "iterating the page lines to index their turns")
	}
	if err := rows.Close(); err != nil {
		return storagef(err, "closing the page-line listing")
	}
	for _, l := range lines {
		if _, err := tx.ExecContext(ctx, turnLineUpsertSQL, l.position, l.book, l.turn, l.place.atMs, l.place.ordinal); err != nil {
			return storagef(err, "indexing the line at position %d of turn %q", l.position, l.turn)
		}
	}
	return nil
}

// turnHeads reads a page's turn heads: for every turn a line of the page is
// stamped with, that turn's prompt line when it is placed below the page's
// lowest line, newest first and each turn once, each saying whether steps of
// its turn sit between it and the page. `lines` is the page, in its served
// order (descending place), so its last line is its lowest.
//
// ONE STATEMENT PER PAGE, never one per line or per turn, and none at all for
// an empty page or one whose every turn's prompt is on it.
//
// A TURN WITH TWO PROMPT LINES BELOW THE PAGE (which no producer writes) is
// headed by the newest of them, so the answer is still one line per turn.
func (d *DB) turnHeads(ctx context.Context, tx *sql.Tx, agentID string, lines []*storev1.StoreLineAt) ([]*storev1.StoreTurnHead, error) {
	if len(lines) == 0 {
		return nil, nil
	}
	headed := map[string]bool{}
	for _, line := range lines {
		if line.GetLine().GetAgentItem().GetAgentPrompt() != nil && line.GetTurn().GetValue() != "" {
			headed[line.GetTurn().GetValue()] = true
		}
	}
	var turns []any
	for _, line := range lines {
		turn := line.GetTurn().GetValue()
		if turn == "" || headed[turn] {
			continue
		}
		headed[turn] = true
		turns = append(turns, turn)
	}
	if len(turns) == 0 {
		return nil, nil
	}

	lowest := lines[len(lines)-1]
	position, err := decodePointer(lowest.GetAt(), "the page's lowest line")
	if err != nil {
		return nil, storagef(err, "reading the position of the lowest line of a page of book %q", agentID)
	}
	place := lowest.GetRecordedPlace()
	if place == nil {
		place = lowest.GetReceivedPlace()
	}
	args := []any{place.GetAtMs(), place.GetOrdinal(), position, kindPageLine, agentID}
	args = append(args, turns...)
	args = append(args, kindPageLine, place.GetAtMs(), place.GetOrdinal(), position)
	rows, err := tx.QueryContext(ctx, expandInList(turnHeadsSQL, len(turns)), args...)
	if err != nil {
		return nil, storagef(err, "reading the turn heads of a page of book %q", agentID)
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	seen := map[string]bool{}
	var heads []*storev1.StoreTurnHead
	for rows.Next() {
		var headPosition int64
		var frame []byte
		var atMs, ordinal, recorded sql.NullInt64
		var between bool
		if err := rows.Scan(&headPosition, &frame, &atMs, &ordinal, &recorded, &between); err != nil {
			return nil, storagef(err, "scanning a turn head of book %q", agentID)
		}
		served, err := scanPlace(headPosition, atMs, ordinal, recorded)
		if err != nil {
			return nil, err
		}
		head, err := decodeLineAt(frame, headPosition, served)
		if err != nil {
			return nil, err
		}
		turn := head.GetTurn().GetValue()
		if seen[turn] {
			continue
		}
		seen[turn] = true
		heads = append(heads, turnHead(head, between))
	}
	if err := rows.Err(); err != nil {
		return nil, storagef(err, "iterating the turn heads of book %q", agentID)
	}
	return heads, nil
}

// turnHead is THE ONE CONSTRUCTOR of a served head: the prompt line and the
// `between` arm the statement answered.
func turnHead(line *storev1.StoreLineAt, between bool) *storev1.StoreTurnHead {
	head := &storev1.StoreTurnHead{Line: line}
	if between {
		head.Between = &storev1.StoreTurnHead_StepsBetween{StepsBetween: &storev1.StoreTurnHeadStepsBetween{}}
	} else {
		head.Between = &storev1.StoreTurnHead_Adjacent{Adjacent: &storev1.StoreTurnHeadAdjacent{}}
	}
	return head
}
