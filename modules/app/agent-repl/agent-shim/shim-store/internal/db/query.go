package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// nowMillis is the store's wall clock in unix millis (cursor updated_at).
func nowMillis() int64 { return time.Now().UnixMilli() }

// ReplayStats describes rows successfully handed to a ReplayFrom sink.
type ReplayStats struct {
	Entries  uint64
	FirstSeq uint64
	LastSeq  uint64
	Elapsed  time.Duration
}

// ReplayFrom streams the session's persisted records with seq > fromSeq to
// yield in seq order. from_seq is EXCLUSIVE (Subscribe semantics).
//
// WHAT IT YIELDS IS A DELIVERY, NOT A RECORD. Each row comes back as an
// EntryDelivery on its `stored` arm, carrying the position the store assigned
// and the record's EXTERNAL half only. The internal half stays in the database:
// the store persists both, and hands back one.
//
// Records with no external half are not in `entry` at all, so replay cannot
// yield one — that is a property of which table they were written to rather
// than of a filter this statement remembers to apply.
//
// The callback-only API is load-bearing: the store server writes each row to
// the subscriber before SQLite advances to the next row. A slice-returning API
// would make it possible to buffer an arbitrarily large replay before the
// first socket write, defeating every downstream activity deadline.
func (d *DB) ReplayFrom(ctx context.Context, sessionID string, fromSeq uint64, yield func(*protocolv1.EntryDelivery) error) (stats ReplayStats, replayErr error) {
	if yield == nil {
		panic("shim-store db: ReplayFrom requires a yield callback")
	}
	started := time.Now()
	// DEFERRED so every exit — the sink stopping the stream, a scan failure, a
	// completed replay — reports what the statement cost. A replay's elapsed
	// time deliberately includes the per-row writes to the subscriber: that is
	// the interval the store spent holding this query open.
	defer func() { d.observeQuery(StatementReplay, "entry", sessionID, started, int64(stats.Entries)) }()
	fields := logging.Fields{Operation: "replay", Table: "entry", Session: sessionID}
	d.log.LogVerbose(fields, "streaming replay query from_seq=%d", fromSeq)
	rows, err := d.sql.QueryContext(ctx,
		`SELECT seq, payload FROM entry WHERE session_id = ? AND seq > ? ORDER BY seq ASC`,
		sessionID, fromSeq)
	if err != nil {
		return ReplayStats{}, d.queryError("replay", "entry", sessionID, fmt.Errorf("shim-store query: replay (session=%q from_seq=%d): %w", sessionID, fromSeq, err))
	}
	defer rows.Close()

	for rows.Next() {
		var seq uint64
		var blob []byte
		if err := rows.Scan(&seq, &blob); err != nil {
			return stats, d.queryError("replay-scan", "entry", sessionID, fmt.Errorf("shim-store query: scanning replay row (session=%q from_seq=%d delivered=%d): %w", sessionID, fromSeq, stats.Entries, err))
		}
		delivery, err := storedDelivery(seq, blob)
		if err != nil {
			return stats, d.queryError("replay-unmarshal", "entry", sessionID, fmt.Errorf("shim-store query: decoding replay row (session=%q from_seq=%d delivered=%d): %w", sessionID, fromSeq, stats.Entries, err))
		}
		if err := yield(delivery); err != nil {
			stats.Elapsed = time.Since(started)
			d.log.LogVerbose(fields, "replay sink stopped stream from_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d cause=%q",
				fromSeq, stats.Entries, stats.FirstSeq, stats.LastSeq, stats.Elapsed.Milliseconds(), err)
			return stats, err
		}
		if stats.Entries == 0 {
			stats.FirstSeq = seq
		}
		stats.Entries++
		stats.LastSeq = seq
	}
	if err := rows.Err(); err != nil {
		return stats, d.queryError("replay-iterate", "entry", sessionID, fmt.Errorf("shim-store query: iterating replay rows (session=%q from_seq=%d delivered=%d first_seq=%d last_seq=%d): %w",
			sessionID, fromSeq, stats.Entries, stats.FirstSeq, stats.LastSeq, err))
	}
	stats.Elapsed = time.Since(started)
	d.log.LogVerbose(fields, "replay query completed from_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d",
		fromSeq, stats.Entries, stats.FirstSeq, stats.LastSeq, stats.Elapsed.Milliseconds())
	return stats, nil
}

// externalOf decodes a stored row into the half a reader may see.
//
// A row in `entry` was written BECAUSE it had an external half, so one missing
// here is a corrupt row rather than an unrenderable record, and it is reported
// as an error instead of delivered as an envelope with nothing in it.
func externalOf(blob []byte) (*protocolv1.ExternalEntry, error) {
	entry := &agentshimv1.Entry{}
	if err := proto.Unmarshal(blob, entry); err != nil {
		return nil, fmt.Errorf("unmarshaling stored entry: %w", err)
	}
	external := entry.GetExternal()
	if external == nil {
		return nil, errors.New("stored entry has no external half, but only records with one are written to this table")
	}
	return external, nil
}

// storedDelivery wraps one row in the envelope a subscriber receives.
func storedDelivery(seq uint64, blob []byte) (*protocolv1.EntryDelivery, error) {
	external, err := externalOf(blob)
	if err != nil {
		return nil, err
	}
	return &protocolv1.EntryDelivery{
		Delivery: &protocolv1.EntryDelivery_Stored{Stored: &protocolv1.StoredEntryDelivery{
			Seq:   seq,
			Entry: external,
		}},
	}, nil
}

// MaxSeq returns the highest assigned seq for a session (0 if none).
func (d *DB) MaxSeq(sessionID string) (uint64, error) {
	d.log.LogVerbose(logging.Fields{Operation: "max-seq", Table: "entry", Session: sessionID}, "querying highest sequence")
	started := time.Now()
	var v uint64
	row := d.sql.QueryRow(`SELECT COALESCE(MAX(seq), 0) FROM entry WHERE session_id = ?`, sessionID)
	err := row.Scan(&v)
	d.observeQuery(StatementMaxSeq, "entry", sessionID, started, 1)
	if err != nil {
		return 0, d.queryError("max-seq", "entry", sessionID, fmt.Errorf("shim-store query: max seq (session=%q): %w", sessionID, err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "max-seq", Table: "entry", Session: sessionID}, "highest sequence=%d", v)
	return v, nil
}

// Cursors returns all persisted file cursors for the sidecar's startup
// recovery. The sidecar resumes each file from its stored offset/carry.
func (d *DB) Cursors() ([]*agentshimv1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "list-cursors", Table: "cursor"}, "querying all persisted cursors")
	started := time.Now()
	var out []*agentshimv1.CursorState
	defer func() { d.observeQuery(StatementListCursors, "cursor", "", started, int64(len(out))) }()
	rows, err := d.sql.Query(`SELECT file_id, path, offset, carry FROM cursor`)
	if err != nil {
		return nil, d.queryError("list-cursors", "cursor", "", fmt.Errorf("shim-store query: listing cursors: %w", err))
	}
	defer rows.Close()

	for rows.Next() {
		c := &agentshimv1.CursorState{}
		var carry []byte
		if err := rows.Scan(&c.FileId, &c.Path, &c.Offset, &carry); err != nil {
			return nil, d.queryError("list-cursors-scan", "cursor", "", fmt.Errorf("shim-store query: scanning cursor row: %w", err))
		}
		c.Carry = carry
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, d.queryError("list-cursors-iterate", "cursor", "", fmt.Errorf("shim-store query: iterating cursor rows: %w", err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "list-cursors", Table: "cursor"}, "cursor query returned cursors=%d", len(out))
	return out, nil
}

// Cursor returns one file's persisted cursor, or (nil, nil) if absent.
func (d *DB) Cursor(fileID string) (*agentshimv1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "querying file_id=%q", fileID)
	started := time.Now()
	c := &agentshimv1.CursorState{}
	var carry []byte
	row := d.sql.QueryRow(`SELECT file_id, path, offset, carry FROM cursor WHERE file_id = ?`, fileID)
	scanErr := row.Scan(&c.FileId, &c.Path, &c.Offset, &carry)
	rowsRead := int64(1)
	if errors.Is(scanErr, sql.ErrNoRows) {
		rowsRead = 0
	}
	d.observeQuery(StatementCursor, "cursor", "", started, rowsRead)
	switch err := scanErr; {
	case err == nil:
		c.Carry = carry
		d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "cursor found file_id=%q offset=%d", fileID, c.GetOffset())
		return c, nil
	case errors.Is(err, sql.ErrNoRows):
		d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "cursor absent file_id=%q", fileID)
		return nil, nil
	default:
		return nil, d.queryError("cursor", "cursor", "", fmt.Errorf("shim-store query: reading cursor (file_id=%q): %w", fileID, err))
	}
}

func (d *DB) queryError(operation, table, session string, err error) error {
	d.log.Log(logging.Fields{Operation: operation, Table: table, Session: session, Level: "error"}, "database query failed: %v", err)
	return err
}
