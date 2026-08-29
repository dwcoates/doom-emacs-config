// Package db owns the shim-store SQLite database: the four-table schema, its
// nuke-never-migrate lifecycle, the transactional WriteBatch routing, and the
// indexed reads the store serves.
//
// WHAT IT PERSISTS IS store.v1.StoreEntry, and it opens that envelope EXACTLY
// far enough to route it — which table the write touches, which book a page
// line belongs to, and which lifecycle row a terminal closes. The frame itself
// stays a serialized blob: the activity vocabulary is conversation CONTENT, and
// unpacking it here would drag every conversation.v1 change into DDL.
//
// TWO ORDERINGS, AND THEY ARE NOT THE SAME ORDERING.
//   - `position` is FIRST-INSERT order. It is the page order and it is what a
//     StoreItemPointer encodes, so a unit that settles mid-walk cannot teleport
//     across a continuation and a pointer stays valid across every upsert of
//     the row it names.
//   - `write_seq` is a GLOBAL monotonic write ordinal, bumped on every insert
//     AND every upsert. It is the watch pin, it never reaches the wire, and it
//     is what makes an upsert of an OLD row stream to a live watcher at its
//     ORIGINAL pointer.
package db

import (
	"context"
	"database/sql"
	"errors"
	"net/url"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/shim-store/internal/logging"
	_ "modernc.org/sqlite"
)

// SchemaVersion is the shape this binary creates. A database stamped at any
// other value is DROPPED and recreated.
//
// IT IS NOT A MIGRATION LINEAGE. There is no ALTER anywhere in this package
// and there never will be: the store is nuked, never migrated, so the version
// answers exactly one question — "did this binary create what is on disk?" —
// and the only remedy for "no" is to recreate it.
const SchemaVersion = 5

// nowMillis is the store's wall clock in unix millis.
func nowMillis() int64 { return time.Now().UnixMilli() }

// DB wraps the SQLite handle plus the store's logger.
type DB struct {
	sql *sql.DB
	log *logging.Logger
	// slowQuery is the duration past which a completed statement is reported
	// at warn. Non-positive disables the reporting entirely, which only an
	// explicit Options caller can ask for.
	slowQuery time.Duration
	// now is the clock every written timestamp is taken from. Injectable so a
	// test can assert an exact instant without sleeping for one.
	now func() int64
}

// Options are the injectable knobs Open resolves from the environment.
type Options struct {
	// SlowQuery is the slow-query threshold; non-positive disables reporting.
	SlowQuery time.Duration
	// Now overrides the wall clock. Zero value means time.Now.
	Now func() int64
}

// Open opens (creating if absent) the store database at path with WAL enabled
// and brings it to SchemaVersion, dropping whatever is there if it does not
// already match.
//
// The slow-query threshold is resolved from the environment here. A malformed
// value aborts the open rather than running the shipped default underneath an
// operator who believes they changed it.
func Open(path string, log *logging.Logger) (*DB, error) {
	slowQuery, err := SlowQueryFromEnv()
	if err != nil {
		if log != nil {
			log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
				"slow-query threshold rejected: %v", err)
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
	log.LogVerbose(logging.Fields{Operation: "store.db.open", DatabasePath: path}, "opening SQLite database")

	// THE LAYER THAT OWNS THE FILE OWNS ITS DIRECTORY. Nothing upstream may
	// create it: doing so would move an unwritable --db path's failure ahead of
	// the profiling surface the boot order opens first, exactly so a wedged or
	// doomed database step stays diagnosable.
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
			"creating the database directory failed: %v", err)
		return nil, storagef(err, "creating the directory for %q", path)
	}

	// modernc.org/sqlite takes PRAGMAs as _pragma query params. WAL for
	// concurrent readers during a live tail; NORMAL sync is durable under WAL;
	// busy_timeout guards the brief window a checkpoint holds the writer.
	//
	// _txlock=immediate makes every Begin() issue BEGIN IMMEDIATE. Both the
	// write path and the read path need it: WriteBatch reads (MAX(write_seq),
	// the write_id probe) before it writes, and OpenPage must take its watch
	// pin in the SAME snapshot as the page it answers with. Under the DEFERRED
	// default such a transaction takes a WAL READ snapshot and only later tries
	// to upgrade to a writer — and SQLite refuses to run the busy handler for
	// an upgrade, so a contending writer is an immediate SQLITE_BUSY rather
	// than a wait. Taking the lock at BEGIN removes the upgrade entirely.
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
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
			"opening SQLite connection failed: %v", err)
		return nil, storagef(err, "opening %q", path)
	}
	if err := sqldb.Ping(); err != nil {
		sqldb.Close() //nolint:errcheck // the open already failed
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
			"SQLite ping failed: %v", err)
		return nil, storagef(err, "pinging %q", path)
	}
	clock := opts.Now
	if clock == nil {
		clock = nowMillis
	}
	d := &DB{sql: sqldb, log: log, slowQuery: opts.SlowQuery, now: clock}
	if err := d.ensureSchema(context.Background(), path); err != nil {
		sqldb.Close() //nolint:errcheck // the open already failed
		return nil, err
	}
	log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path},
		"SQLite database ready schema_version=%d slow_query_threshold_ms=%d", SchemaVersion, opts.SlowQuery.Milliseconds())
	return d, nil
}

// Close closes the underlying handle.
func (d *DB) Close() error {
	d.log.LogVerbose(logging.Fields{Operation: "store.db.close"}, "closing SQLite database")
	if err := d.sql.Close(); err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.close", Level: "error", ErrorCause: err.Error()},
			"closing SQLite database failed: %v", err)
		return storagef(err, "closing the database")
	}
	d.log.Log(logging.Fields{Operation: "store.db.close"}, "SQLite database closed")
	return nil
}

// schemaDDL is the WHOLE schema, and it is the only DDL in this package.
//
// NO `IF NOT EXISTS` ANYWHERE, on purpose: this statement only ever runs
// against a database that was just emptied, and a CREATE that silently
// tolerated an existing object is exactly how two binaries end up believing
// they share a shape they do not.
const schemaDDL = `
CREATE TABLE entry (
  position             INTEGER PRIMARY KEY AUTOINCREMENT,
  upsert_key           TEXT    NOT NULL UNIQUE,
  write_id             TEXT    NOT NULL UNIQUE,
  write_seq            INTEGER NOT NULL,
  plane                INTEGER NOT NULL,
  kind                 TEXT    NOT NULL,
  book_agent_id        TEXT,
  run_id               TEXT,
  top_level            TEXT,
  frame                BLOB    NOT NULL,
  first_inserted_at_ms INTEGER NOT NULL,
  last_written_at_ms   INTEGER NOT NULL
);
CREATE INDEX entry_book_position  ON entry(book_agent_id, position);
CREATE INDEX entry_book_write_seq ON entry(book_agent_id, write_seq);
CREATE INDEX entry_write_seq      ON entry(write_seq);
CREATE INDEX entry_run_position  ON entry(run_id, position);
CREATE INDEX entry_run_write_seq ON entry(run_id, write_seq);

CREATE TABLE agent (
  agent_id              TEXT PRIMARY KEY,
  spawned_by_agent      TEXT,
  spawned_by_workflow   TEXT,
  description           TEXT,
  prompt_text           TEXT,
  subagent_type         TEXT,
  requested_name        TEXT,
  requested_model       TEXT,
  spawn_depth           INTEGER,
  working_dir           TEXT,
  transcript_suppressed INTEGER,
  isolation             TEXT,
  forked_from_caller    INTEGER,
  started_at_ms         INTEGER NOT NULL,
  ended_at_ms           INTEGER,
  terminal              BLOB
);
CREATE INDEX agent_live ON agent(ended_at_ms);

CREATE TABLE workflow (
  run_agent_id  TEXT PRIMARY KEY,
  spawner_agent TEXT,
  origin_unit   TEXT,
  name          TEXT,
  script        BLOB,
  resumed_from  TEXT,
  placement     TEXT,
  started_at_ms INTEGER NOT NULL,
  ended_at_ms   INTEGER,
  terminal      BLOB
);
CREATE INDEX workflow_live ON workflow(ended_at_ms);

-- detached_work holds THE JOIN AND NOTHING ELSE.
--
-- The announcement itself is a PAGE LINE of the announcing agent's book, and
-- that page line is the one copy of what was announced: the spool path, the
-- readability, the detach cause, the timeout. Unpacking any of it here as well
-- would give the same fact two homes that can disagree, and the store would be
-- re-deriving conversation content it is not entitled to interpret. What is
-- left is exactly what the store itself filters and joins on: the handle, the
-- kind, the origin unit a terminal closes the row through, the announcing
-- agent, and the terminal columns.
CREATE TABLE detached_work (
  work_id         TEXT PRIMARY KEY,
  kind            TEXT NOT NULL,
  origin_unit     TEXT,
  owner_agent     TEXT,
  announced_at_ms INTEGER NOT NULL,
  ended_at_ms     INTEGER,
  terminal        BLOB
);
CREATE INDEX detached_work_origin ON detached_work(origin_unit);
CREATE INDEX detached_work_live   ON detached_work(ended_at_ms);

CREATE TABLE cursor (
  file_id       TEXT PRIMARY KEY,
  path          TEXT    NOT NULL,
  offset        INTEGER NOT NULL,
  carry         BLOB,
  updated_at_ms INTEGER NOT NULL
);

CREATE TABLE write_ledger (
  write_id      TEXT    PRIMARY KEY,
  upsert_key    TEXT    NOT NULL,
  write_seq     INTEGER NOT NULL,
  applied_at_ms INTEGER NOT NULL
);
CREATE INDEX write_ledger_upsert_key ON write_ledger(upsert_key);

CREATE TABLE schema_meta (version INTEGER NOT NULL);
`

// schemaTables is the exact table set schemaDDL produces, sorted. It is
// compared against what is on disk so a database carrying the RIGHT version
// stamp on the WRONG shape — a half-applied create, a hand-edited file, a
// binary that crashed between DROP and CREATE — is nuked rather than trusted.
var schemaTables = []string{"agent", "cursor", "detached_work", "entry", "schema_meta", "workflow", "write_ledger"}

// ensureSchema brings the database to SchemaVersion by the only means this
// package has: dropping everything and recreating it.
//
// THERE IS NO MIGRATION AND THERE IS NO BACKFILL. The store holds a cache of
// what the vendor and the shim already know how to produce again, so a shape
// this binary did not create is worth exactly nothing and costs a DROP to be
// rid of. Writing an ALTER here would be the first half of a compatibility
// surface the whole design exists to not have.
func (d *DB) ensureSchema(ctx context.Context, path string) error {
	current, tables, err := d.inspectSchema(ctx)
	if err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"reading the on-disk schema failed: %v", err)
		return err
	}
	if current == SchemaVersion && slicesEqual(tables, schemaTables) {
		d.log.LogVerbose(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
			"schema already current version=%d", current)
		return nil
	}
	// AN EMPTY FILE IS A FIRST CREATE, NOT A NUKE. Every fresh store — every
	// launch on a new machine, every test process — arrives here with no
	// tables at all, and warning about it would bury the one case that
	// genuinely deserves the weight: a shape this binary did not create being
	// DROPPED with whatever was in it.
	if len(tables) == 0 {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
			"no schema on disk; creating it at version=%d tables=%v", SchemaVersion, schemaTables)
	} else {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "warn"},
			"on-disk schema does not match this binary (found version=%d tables=%v, want version=%d tables=%v) — dropping and recreating; the store is nuked, never migrated",
			current, tables, SchemaVersion, schemaTables)
	}
	if err := d.nukeAndCreate(ctx, tables); err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"recreating the schema failed: %v", err)
		return err
	}
	d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
		"schema recreated at version=%d", SchemaVersion)
	return nil
}

// inspectSchema reports the stamped version (0 when there is none) and the
// user tables present, sorted.
func (d *DB) inspectSchema(ctx context.Context) (int, []string, error) {
	rows, err := d.sql.QueryContext(ctx, `SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'`)
	if err != nil {
		return 0, nil, storagef(err, "listing tables")
	}
	var tables []string
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return 0, nil, storagef(err, "scanning a table name")
		}
		tables = append(tables, name)
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return 0, nil, storagef(err, "iterating table names")
	}
	if err := rows.Close(); err != nil {
		return 0, nil, storagef(err, "closing the table listing")
	}
	sort.Strings(tables)

	if !contains(tables, "schema_meta") {
		return 0, tables, nil
	}
	var version int
	switch err := d.sql.QueryRowContext(ctx, `SELECT version FROM schema_meta LIMIT 1`).Scan(&version); {
	case err == nil:
		return version, tables, nil
	case errors.Is(err, sql.ErrNoRows):
		// The table exists and claims nothing. Unstamped is not current.
		return 0, tables, nil
	default:
		// A `schema_meta` whose shape this binary cannot even read is exactly
		// the case the nuke exists for, so it is reported as version 0 rather
		// than failing the open.
		return 0, tables, nil
	}
}

// nukeAndCreate drops every user table and applies schemaDDL, stamping the
// version in the SAME transaction so the schema and the claim about it can
// never disagree.
func (d *DB) nukeAndCreate(ctx context.Context, existing []string) error {
	tx, err := d.sql.BeginTx(ctx, nil)
	if err != nil {
		return storagef(err, "begin schema recreation")
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	for _, table := range existing {
		if _, err := tx.ExecContext(ctx, `DROP TABLE IF EXISTS "`+strings.ReplaceAll(table, `"`, `""`)+`"`); err != nil {
			return storagef(err, "dropping table %q", table)
		}
	}
	if _, err := tx.ExecContext(ctx, schemaDDL); err != nil {
		return storagef(err, "creating the schema")
	}
	if _, err := tx.ExecContext(ctx, `INSERT INTO schema_meta(version) VALUES (?)`, SchemaVersion); err != nil {
		return storagef(err, "stamping schema version %d", SchemaVersion)
	}
	if err := tx.Commit(); err != nil {
		return storagef(err, "committing schema recreation")
	}
	return nil
}

func contains(haystack []string, needle string) bool {
	for _, value := range haystack {
		if value == needle {
			return true
		}
	}
	return false
}

func slicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// queryError records one database failure exactly once, at its owning layer,
// and hands the caller an ErrStorage.
func (d *DB) queryError(operation, table string, fields logging.Fields, err error) error {
	fields.Operation = operation
	fields.Table = table
	fields.Level = "error"
	fields.ErrorCause = err.Error()
	d.log.Log(fields, "database statement failed: %v", err)
	return err
}
