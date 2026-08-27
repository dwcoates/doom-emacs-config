package db

import (
	"database/sql"
	"errors"
	"fmt"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// nowMillis is the store's wall clock in unix millis (cursor updated_at).
func nowMillis() int64 { return time.Now().UnixMilli() }

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
func (d *DB) Cursors() ([]*storev1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "list-cursors", Table: "cursor"}, "querying all persisted cursors")
	started := time.Now()
	var out []*storev1.CursorState
	defer func() { d.observeQuery(StatementListCursors, "cursor", "", started, int64(len(out))) }()
	rows, err := d.sql.Query(`SELECT file_id, path, offset, carry FROM cursor`)
	if err != nil {
		return nil, d.queryError("list-cursors", "cursor", "", fmt.Errorf("shim-store query: listing cursors: %w", err))
	}
	defer rows.Close()

	for rows.Next() {
		c := &storev1.CursorState{}
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
func (d *DB) Cursor(fileID string) (*storev1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "querying file_id=%q", fileID)
	started := time.Now()
	c := &storev1.CursorState{}
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
