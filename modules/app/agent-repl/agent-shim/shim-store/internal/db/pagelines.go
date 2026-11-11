package db

import (
	"context"
	"database/sql"

	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// pagelines.go — every stored page line, decoded: what an in-place index's
// backfill reads (db.go inPlaceTables) when its bookkeeping lives inside the
// stored frames.
//
// THE ONE READ every frame-decoding backfill shares, so none can scan the
// table under a different rule, skip a row another keeps, or decode it
// differently. A frame that cannot be decoded is a LOUD failure, as on every
// read path: the open fails (never nukes) rather than building an index with
// a row missing.

// pageLinesSQL binds (the page-line kind): every booked page line.
const pageLinesSQL = `SELECT position, book_agent_id, frame FROM entry WHERE kind = ? AND book_agent_id IS NOT NULL`

// storedPageLine is one booked page line: its position, its book, and the
// entry its frame decodes to.
type storedPageLine struct {
	position int64
	book     string
	entry    *storev1.StoreEntry
}

// storedPageLines reads and decodes every booked page line, in the create's
// own transaction. INDEXING names what the caller indexes, for the failure's
// account ("their asks").
//
// THE ROWS ARE READ WHOLE BEFORE THE CALLER WRITES: a statement may not run on
// the transaction while its listing is open.
func storedPageLines(ctx context.Context, tx *sql.Tx, indexing string) ([]storedPageLine, error) {
	rows, err := tx.QueryContext(ctx, pageLinesSQL, kindPageLine)
	if err != nil {
		return nil, storagef(err, "reading the page lines to index %s", indexing)
	}
	var lines []storedPageLine
	for rows.Next() {
		var position int64
		var book string
		var frame []byte
		if err := rows.Scan(&position, &book, &frame); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return nil, storagef(err, "scanning a page line to index %s", indexing)
		}
		entry := &storev1.StoreEntry{}
		if err := proto.Unmarshal(frame, entry); err != nil {
			rows.Close() //nolint:errcheck // the decode already failed
			return nil, storagef(err, "stored frame at position %d cannot be decoded", position)
		}
		lines = append(lines, storedPageLine{position: position, book: book, entry: entry})
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return nil, storagef(err, "iterating the page lines to index %s", indexing)
	}
	if err := rows.Close(); err != nil {
		return nil, storagef(err, "closing the page-line listing")
	}
	return lines, nil
}
