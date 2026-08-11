// Package db owns the shim-store SQLite database: the §6.2 schema, WAL
// configuration, schema_meta versioning, and the transactional ingest / replay
// operations. It knows nothing about vendors; it extracts only the envelope
// columns (§6.2) needed to index otherwise-opaque payload blobs.
package db

import (
	"database/sql"
	"fmt"
	"net/url"
	"time"

	"agentrepl/shim-store/internal/logging"
	_ "modernc.org/sqlite"
)

// SchemaVersion is the current schema_meta version. Bump it and append a step
// to migrationSteps for any schema change.
const SchemaVersion = 2

// DB wraps the SQLite handle plus the store's logger.
type DB struct {
	sql *sql.DB
	log *logging.Logger
	// slowQuery is the duration past which a completed statement is reported
	// at warn. Non-positive disables the reporting entirely, which only an
	// explicit Options caller can ask for.
	slowQuery time.Duration
}

// Options are the injectable knobs Open resolves from the environment.
type Options struct {
	// SlowQuery is the slow-query threshold; non-positive disables reporting.
	SlowQuery time.Duration
}

// Open opens (creating if absent) the event database at path with WAL enabled
// and runs migrations to SchemaVersion. path may be an on-disk file; callers
// running tests pass a temp-dir path so WAL is genuinely exercised.
//
// The slow-query threshold is resolved from the environment here. A malformed
// value aborts the open rather than running the shipped default underneath an
// operator who believes they changed it.
func Open(path string, log *logging.Logger) (*DB, error) {
	slowQuery, err := SlowQueryFromEnv()
	if err != nil {
		if log != nil {
			log.Log(logging.Fields{Operation: "open", DatabasePath: path, Level: "error"}, "slow-query threshold rejected: %v", err)
		}
		return nil, err
	}
	return OpenWithOptions(path, log, Options{SlowQuery: slowQuery})
}

// OpenWithOptions is Open with the knobs supplied rather than read from the
// environment. Tests use it to exercise both sides of a threshold without
// mutating process state.
func OpenWithOptions(path string, log *logging.Logger, opts Options) (*DB, error) {
	if log == nil {
		panic("shim-store db: nil logger")
	}
	log.LogVerbose(logging.Fields{Operation: "open", DatabasePath: path, Table: "schema_meta"}, "opening SQLite database")
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
		log.Log(logging.Fields{Operation: "open", DatabasePath: path, Level: "error"}, "opening SQLite connection failed: %v", err)
		return nil, fmt.Errorf("shim-store db: opening %q: %w", path, err)
	}
	if err := sqldb.Ping(); err != nil {
		sqldb.Close()
		log.Log(logging.Fields{Operation: "ping", DatabasePath: path, Level: "error"}, "SQLite ping failed: %v", err)
		return nil, fmt.Errorf("shim-store db: pinging %q: %w", path, err)
	}
	d := &DB{sql: sqldb, log: log, slowQuery: opts.SlowQuery}
	if err := d.migrate(); err != nil {
		sqldb.Close()
		log.Log(logging.Fields{Operation: "migrate", DatabasePath: path, Table: "schema_meta", Level: "error"}, "schema migration failed: %v", err)
		return nil, err
	}
	log.Log(logging.Fields{Operation: "open", DatabasePath: path, Table: "event"},
		"SQLite database ready slow_query_threshold_ms=%d", opts.SlowQuery.Milliseconds())
	return d, nil
}

// Close closes the underlying handle.
func (d *DB) Close() error {
	d.log.LogVerbose(logging.Fields{Operation: "close"}, "closing SQLite database")
	if err := d.sql.Close(); err != nil {
		d.log.Log(logging.Fields{Operation: "close", Level: "error"}, "closing SQLite database failed: %v", err)
		return err
	}
	d.log.Log(logging.Fields{Operation: "close"}, "SQLite database closed")
	return nil
}

// baseDDL is the version-1 shape, and stays frozen at it forever. Every later
// change is a numbered step in migrationSteps instead, because a database
// created by an older binary already HAS these objects and would never see an
// edit made here — writing the new shape into the CREATE TABLE would give
// fresh and migrated databases two different schemas.
const baseDDL = `
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

// migrationStep is one ordered forward migration. `to` is the schema_meta
// version the database carries once `ddl` has been applied; steps run in slice
// order and each one commits atomically with its own version stamp, so an
// interrupted upgrade resumes at exactly the step it did not finish.
type migrationStep struct {
	to     int
	name   string
	ddl    string
	reason string
}

// migrationSteps carries every forward migration past baseDDL, in order.
var migrationSteps = []migrationStep{
	{
		to:   2,
		name: "event.write_id",
		ddl: `ALTER TABLE event ADD COLUMN write_id TEXT;
CREATE UNIQUE INDEX IF NOT EXISTS event_write_id
  ON event(session_id, write_id) WHERE write_id IS NOT NULL;`,
		reason: "the producer's stable per-event write identity, which is what makes a replayed batch a no-op instead of a duplicate row",
	},
}

// migrate brings the database to SchemaVersion, loud-logging the transition.
//
// The base schema is applied first (it is IF NOT EXISTS throughout, so it is a
// no-op on an existing database), then every registered step whose target is
// above the recorded version runs in order. A database with no schema_meta row
// is a FRESH one: it is stamped at the base version and then walks the same
// steps as an upgrade, so a fresh database and a migrated one converge on
// byte-identical schemas rather than on two shapes that merely look alike.
func (d *DB) migrate() error {
	d.log.LogVerbose(logging.Fields{Operation: "migrate", Table: "schema_meta"}, "checking schema target_version=%d", SchemaVersion)
	if _, err := d.sql.Exec(baseDDL); err != nil {
		return fmt.Errorf("shim-store db: applying schema: %w", err)
	}
	var version int
	row := d.sql.QueryRow(`SELECT version FROM schema_meta LIMIT 1`)
	switch err := row.Scan(&version); err {
	case sql.ErrNoRows:
		const baseVersion = 1
		if _, err := d.sql.Exec(`INSERT INTO schema_meta(version) VALUES (?)`, baseVersion); err != nil {
			return fmt.Errorf("shim-store db: seeding schema_meta: %w", err)
		}
		version = baseVersion
		d.log.Log(logging.Fields{Operation: "migrate", Table: "schema_meta"}, "schema initialized at version=%d", version)
	case nil:
		if version > SchemaVersion {
			return fmt.Errorf("shim-store db: on-disk schema version %d is newer than this binary's %d", version, SchemaVersion)
		}
	default:
		return fmt.Errorf("shim-store db: reading schema_meta: %w", err)
	}
	if version == SchemaVersion {
		d.log.LogVerbose(logging.Fields{Operation: "migrate", Table: "schema_meta"}, "schema already current version=%d", version)
		return nil
	}
	for _, step := range migrationSteps {
		if step.to <= version {
			continue
		}
		if err := d.applyMigration(version, step); err != nil {
			return err
		}
		version = step.to
	}
	// A gap between the last registered step and SchemaVersion means a bump
	// shipped without its migration. That is a loud error, never a silent
	// stamp: stamping would claim a shape the database does not have.
	if version != SchemaVersion {
		return fmt.Errorf("shim-store db: schema version %d needs migration to %d, but no migration is registered", version, SchemaVersion)
	}
	return nil
}

// applyMigration runs one step and stamps its version in the SAME transaction,
// so the schema change and the claim that it happened can never disagree.
func (d *DB) applyMigration(from int, step migrationStep) error {
	d.log.Log(logging.Fields{Operation: "migrate", Table: "schema_meta", Transaction: "BEGIN IMMEDIATE"},
		"migrating schema %d -> %d (%s): %s", from, step.to, step.name, step.reason)
	tx, err := d.sql.Begin()
	if err != nil {
		return fmt.Errorf("shim-store db: begin migration %d -> %d (%s): %w", from, step.to, step.name, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit
	if _, err := tx.Exec(step.ddl); err != nil {
		return fmt.Errorf("shim-store db: applying migration %d -> %d (%s): %w", from, step.to, step.name, err)
	}
	if _, err := tx.Exec(`UPDATE schema_meta SET version = ?`, step.to); err != nil {
		return fmt.Errorf("shim-store db: stamping migration %d -> %d (%s): %w", from, step.to, step.name, err)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("shim-store db: committing migration %d -> %d (%s): %w", from, step.to, step.name, err)
	}
	d.log.Log(logging.Fields{Operation: "migrate", Table: "schema_meta"}, "schema migrated to version=%d (%s)", step.to, step.name)
	return nil
}
