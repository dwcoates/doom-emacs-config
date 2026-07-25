// Package db owns the shim-store SQLite database: the §6.2 schema, WAL
// configuration, schema_meta versioning, and the transactional ingest / replay
// operations. It knows nothing about vendors; it extracts only the envelope
// columns (§6.2) needed to index otherwise-opaque payload blobs.
package db

import (
	"database/sql"
	"fmt"
	"net/url"

	_ "modernc.org/sqlite"
)

// SchemaVersion is the current schema_meta version. Bump it and add a
// migration step in migrate() for any schema change.
const SchemaVersion = 1

// Logf is the loud-logging sink (§12). Injected so tests can capture output.
type Logf = func(format string, args ...any)

// DB wraps the SQLite handle plus the store's logger.
type DB struct {
	sql *sql.DB
	log Logf
}

// Open opens (creating if absent) the event database at path with WAL enabled
// and runs migrations to SchemaVersion. path may be an on-disk file; callers
// running tests pass a temp-dir path so WAL is genuinely exercised.
func Open(path string, log Logf) (*DB, error) {
	if log == nil {
		log = func(string, ...any) {}
	}
	// modernc.org/sqlite takes PRAGMAs as _pragma query params. WAL for
	// concurrent readers during live-tail; NORMAL sync is durable under WAL;
	// busy_timeout guards the brief window a checkpoint holds the writer.
	//
	// _txlock=immediate makes Begin() issue BEGIN IMMEDIATE, which is what
	// actually makes busy_timeout apply to Ingest. Ingest reads (SELECT
	// MAX(seq)) before it writes, so under the DEFERRED default the
	// transaction takes a WAL READ snapshot and only later tries to upgrade to
	// a writer — and SQLite refuses to run the busy handler for an upgrade:
	//   - another connection committed since the snapshot -> SQLITE_BUSY_SNAPSHOT
	//     (517), returned immediately because retrying can never succeed;
	//   - a writer holds the lock at upgrade time -> SQLITE_BUSY (5), returned
	//     immediately because waiting mid-upgrade would deadlock.
	// One store process serves every live shim on its own goroutine and pooled
	// connection, so those collisions are routine — and a rejected batch is
	// PERMANENT loss (the shim's store-client drops it: no spill, no retry).
	// Taking the write lock at BEGIN removes the upgrade entirely: contenders
	// queue on busy_timeout instead of erroring.
	dsn := "file:" + path + "?" + url.Values{
		"_pragma": {
			"journal_mode(WAL)",
			"busy_timeout(5000)",
			"synchronous(NORMAL)",
			"foreign_keys(ON)",
		},
		"_txlock": {"immediate"},
	}.Encode()

	sqldb, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("shim-store db: opening %q: %w", path, err)
	}
	if err := sqldb.Ping(); err != nil {
		sqldb.Close()
		return nil, fmt.Errorf("shim-store db: pinging %q: %w", path, err)
	}
	d := &DB{sql: sqldb, log: log}
	if err := d.migrate(); err != nil {
		sqldb.Close()
		return nil, err
	}
	return d, nil
}

// Close closes the underlying handle.
func (d *DB) Close() error { return d.sql.Close() }

const schemaDDL = `
CREATE TABLE IF NOT EXISTS event (
  session_id  TEXT    NOT NULL,
  seq         INTEGER NOT NULL,
  plane       INTEGER NOT NULL,
  class       INTEGER NOT NULL,
  kind        TEXT    NOT NULL,
  task_id     TEXT,
  uuid        TEXT,
  dedup_key   TEXT,
  produced_at INTEGER NOT NULL,
  payload     BLOB    NOT NULL,
  PRIMARY KEY (session_id, seq)
);
CREATE UNIQUE INDEX IF NOT EXISTS event_dedup
  ON event(session_id, dedup_key) WHERE dedup_key IS NOT NULL;
CREATE INDEX IF NOT EXISTS event_task
  ON event(session_id, task_id) WHERE task_id IS NOT NULL;
CREATE TABLE IF NOT EXISTS cursor (
  file_id    TEXT PRIMARY KEY,
  path       TEXT    NOT NULL,
  offset     INTEGER NOT NULL,
  carry      BLOB,
  updated_at INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS schema_meta (version INTEGER NOT NULL);
`

// migrate brings the database to SchemaVersion, loud-logging the transition.
func (d *DB) migrate() error {
	if _, err := d.sql.Exec(schemaDDL); err != nil {
		return fmt.Errorf("shim-store db: applying schema: %w", err)
	}
	var version int
	row := d.sql.QueryRow(`SELECT version FROM schema_meta LIMIT 1`)
	switch err := row.Scan(&version); err {
	case sql.ErrNoRows:
		if _, err := d.sql.Exec(`INSERT INTO schema_meta(version) VALUES (?)`, SchemaVersion); err != nil {
			return fmt.Errorf("shim-store db: seeding schema_meta: %w", err)
		}
		d.log("schema initialized at version %d", SchemaVersion)
	case nil:
		if version > SchemaVersion {
			return fmt.Errorf("shim-store db: on-disk schema version %d is newer than this binary's %d", version, SchemaVersion)
		}
		if version < SchemaVersion {
			// No forward migrations exist yet; a future bump adds ordered
			// steps here. Reaching this with version<current is a loud error,
			// never a silent reset.
			return fmt.Errorf("shim-store db: schema version %d needs migration to %d, but no migration is registered", version, SchemaVersion)
		}
	default:
		return fmt.Errorf("shim-store db: reading schema_meta: %w", err)
	}
	return nil
}
