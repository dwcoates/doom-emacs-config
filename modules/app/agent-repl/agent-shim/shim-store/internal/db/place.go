package db

import (
	"context"
	"database/sql"
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// servedPlace is a row's conversation place as this store serves it: the
// instant, the rank within it, and WHO ESTABLISHED IT — a producer's stated
// place (recorded) or the store's first-insert receipt instant standing in
// (received). Both arms order identically; the arm only says which it was.
//
// ITS ONE HOME IS `entry_place` (db.go), written by placeRow and read back by
// every serving path, so a page, a catch-up, a replay and a live line cannot
// disagree about where a line sits.
type servedPlace struct {
	atMs     int64
	ordinal  uint32
	recorded bool
}

// lineAt is THE ONE CONSTRUCTOR of a served line: its pointer, the line, the
// turn its row is stamped with, and its place. Every path that hands a line to
// a caller builds it here, so no path can serve a line without its place —
// StoreLineAt.place is "always set by this store".
func lineAt(position int64, line *storev1.StorePageLine, turn *conversationv1.TurnId, place servedPlace) *storev1.StoreLineAt {
	at := &storev1.StoreLineAt{At: encodePointer(position), Line: line, Turn: turn}
	value := &conversationv1.ConversationPlace{AtMs: place.atMs, Ordinal: place.ordinal}
	if place.recorded {
		at.Place = &storev1.StoreLineAt_RecordedPlace{RecordedPlace: value}
	} else {
		at.Place = &storev1.StoreLineAt_ReceivedPlace{ReceivedPlace: value}
	}
	return at
}

// placeOf decides a booked row's served place from what its write carries
// (after carryStoredStamps kept the row's first stated place) and the row's
// first-insert receipt instant.
func placeOf(entry *storev1.StoreEntry, firstInsertedAtMs int64) servedPlace {
	if stated := entry.GetPlace(); stated != nil {
		return servedPlace{atMs: stated.GetAtMs(), ordinal: stated.GetOrdinal(), recorded: true}
	}
	return servedPlace{atMs: firstInsertedAtMs}
}

// placeRowSQL binds (position, book, at_ms, ordinal, recorded). A row's book
// never changes (applyIdentityPolicy refuses or skips a move), so the conflict
// only ever re-states the place — which moves only when a row that had no
// stated place gains its first one.
const placeRowSQL = `INSERT INTO entry_place (position, book_agent_id, at_ms, ordinal, recorded)
  VALUES (?,?,?,?,?)
  ON CONFLICT(position) DO UPDATE SET
    book_agent_id = excluded.book_agent_id,
    at_ms = excluded.at_ms,
    ordinal = excluded.ordinal,
    recorded = excluded.recorded`

// placeRow records a booked row's place in the order index, IN THE WRITE'S OWN
// TRANSACTION, and returns it for the line the write publishes. It is the one
// writer of `entry_place`: a row with a book always has exactly one place row,
// so every page statement can drive from the index.
func (d *DB) placeRow(ctx context.Context, tx *sql.Tx, position int64, book string, place servedPlace) error {
	recorded := 0
	if place.recorded {
		recorded = 1
	}
	if _, err := tx.ExecContext(ctx, placeRowSQL, position, book, place.atMs, place.ordinal, recorded); err != nil {
		return storagef(err, "recording the conversation place of the row at position %d", position)
	}
	return nil
}

// errNoPlace is the cause of a booked row found with no place row: the index
// every page is ordered by has a hole, which only a damaged database can have.
var errNoPlace = errors.New("the row has a book but no conversation place")

// scanPlace turns the nullable place columns of a LEFT JOIN into a served
// place, refusing a row that has none. A HOLE IS A LOUD FAILURE, never a line
// served unplaced or skipped: placeRow writes a place for every booked row in
// the row's own transaction, so a missing one means the file is damaged.
func scanPlace(position int64, atMs, ordinal, recorded sql.NullInt64) (servedPlace, error) {
	if !atMs.Valid || !ordinal.Valid || !recorded.Valid {
		return servedPlace{}, storagef(errNoPlace, "the page line at position %d has no row in entry_place", position)
	}
	return servedPlace{atMs: atMs.Int64, ordinal: uint32(ordinal.Int64), recorded: recorded.Int64 == 1}, nil
}

// placeOfRowSQL binds (position). At package scope so the suite EXPLAINs it.
const placeOfRowSQL = `SELECT at_ms, ordinal, recorded FROM entry_place WHERE position = ?`

// placeOfRow reads one booked row's place from the order index. A booked row
// with no place row is the damaged-index failure scanPlace names.
func placeOfRow(ctx context.Context, tx *sql.Tx, position int64) (servedPlace, error) {
	var atMs, ordinal, recorded sql.NullInt64
	err := tx.QueryRowContext(ctx, placeOfRowSQL, position).Scan(&atMs, &ordinal, &recorded)
	if err != nil && !errors.Is(err, sql.ErrNoRows) {
		return servedPlace{}, storagef(err, "reading the conversation place of the row at position %d", position)
	}
	return scanPlace(position, atMs, ordinal, recorded)
}
