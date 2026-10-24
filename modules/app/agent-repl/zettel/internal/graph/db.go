// Package graph is zettel's database: the knowledge graph's nodes, their
// revisions, and how often each node was seen.
//
// THE GRAPH IS THE USER'S DATA, AND IT IS CARRIED FORWARD, NEVER THROWN AWAY.
// Unlike agent-repl's record store (a cache of transcripts the vendor keeps
// anyway), nothing else holds what this database holds, so a layout change is a
// migration step, and a file stamped newer than this build is refused rather
// than rewritten.
//
// ONE WRITER. The zettel service is the only process that writes the file, and
// inside it every write runs on one connection whose transactions begin
// IMMEDIATE, so a check and the write it guards (a thread starts once, a
// parent exists) are one transaction and can never interleave with another.
package graph

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"time"

	"agentrepl/zettel/internal/zlog"

	_ "modernc.org/sqlite"
)

// DB is an open graph database.
type DB struct {
	write *sql.DB
	read  *sql.DB
	log   *zlog.Logger
	now   func() time.Time
	newID func() string
}

// Options are Open's injectable dependencies; the zero value is production.
type Options struct {
	// Now is the clock stamped on every write. Defaults to time.Now.
	Now func() time.Time
	// NewID mints node ids. Defaults to random 128-bit hex.
	NewID func() string
}

// ErrStorage marks a failure of the database itself (as opposed to a refused
// request), so the server answers it with a storage-failure arm.
var ErrStorage = errors.New("graph storage failure")

func storagef(err error, format string, args ...any) error {
	return fmt.Errorf("%w: %s: %w", ErrStorage, fmt.Sprintf(format, args...), err)
}

// Open opens (creating if absent) the graph database at path and brings its
// layout up to this build's, logging every step through log.
func Open(ctx context.Context, path string, log *zlog.Logger, opts Options) (*DB, error) {
	if log == nil {
		panic("graph: nil logger")
	}
	if opts.Now == nil {
		opts.Now = time.Now
	}
	if opts.NewID == nil {
		opts.NewID = randomID
	}
	log = log.With(zlog.Context{"database": path})
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		log.Error("zettel.db.open", "creating the database directory failed", zlog.Context{"cause": err.Error()})
		return nil, storagef(err, "creating the directory for %q", path)
	}
	writeDSN := "file:" + path + "?" + url.Values{
		"_pragma": {"journal_mode(WAL)", "busy_timeout(5000)", "synchronous(NORMAL)", "foreign_keys(ON)"},
		"_txlock": {"immediate"},
	}.Encode()
	readDSN := "file:" + path + "?" + url.Values{
		"_pragma": {"busy_timeout(5000)", "foreign_keys(ON)", "query_only(true)"},
	}.Encode()
	write, err := sql.Open("sqlite", writeDSN)
	if err != nil {
		log.Error("zettel.db.open", "opening the write handle failed", zlog.Context{"cause": err.Error()})
		return nil, storagef(err, "opening %q for writing", path)
	}
	write.SetMaxOpenConns(1)
	db := &DB{write: write, log: log, now: opts.Now, newID: opts.NewID}
	if err := db.migrate(ctx); err != nil {
		_ = write.Close()
		return nil, err
	}
	read, err := sql.Open("sqlite", readDSN)
	if err != nil {
		_ = write.Close()
		log.Error("zettel.db.open", "opening the read handle failed", zlog.Context{"cause": err.Error()})
		return nil, storagef(err, "opening %q for reading", path)
	}
	db.read = read
	log.Info("zettel.db.open", "the graph database is open", zlog.Context{"layout": len(migrations)})
	return db, nil
}

// Close closes both handles, reporting the first failure.
func (db *DB) Close() error {
	err := errors.Join(db.read.Close(), db.write.Close())
	if err != nil {
		db.log.Error("zettel.db.close", "closing the database failed", zlog.Context{"cause": err.Error()})
	}
	return err
}

// migration is one step forward: the statements that take a file stamped
// with Version-1 to Version. Steps are dense and ordered.
type migration struct {
	Version int
	Name    string
	DDL     string
}

// migrations is every layout step this build knows, in order. A file's
// layout is the number of steps applied to it.
var migrations = []migration{
	{Version: 1, Name: "graph", DDL: layoutOneDDL},
}

const layoutOneDDL = `
CREATE TABLE node (
  id               TEXT PRIMARY KEY,
  thread_id        TEXT NOT NULL,
  -- THE DAEMON TURN THE NODE IS: recording is idempotent by it.
  turn_id          TEXT NOT NULL UNIQUE,
  -- The parent: NULL for the thread's root (its start), else the node whose
  -- answer was asked of, the revision that was read, and the optional quote.
  parent_node_id   TEXT REFERENCES node(id),
  parent_revision  INTEGER,
  quote_text       TEXT,
  quote_context    TEXT,
  question         TEXT NOT NULL,
  created_at_ms    INTEGER NOT NULL,
  sightings        INTEGER NOT NULL DEFAULT 0,
  CHECK ((parent_node_id IS NULL) = (parent_revision IS NULL)),
  CHECK (parent_node_id IS NOT NULL OR quote_text IS NULL),
  CHECK (quote_text IS NOT NULL OR quote_context IS NULL)
);
-- A THREAD STARTS ONCE: at most one root node per thread.
CREATE UNIQUE INDEX node_root_per_thread ON node(thread_id) WHERE parent_node_id IS NULL;
CREATE INDEX node_by_thread ON node(thread_id, created_at_ms);

CREATE TABLE revision (
  node_id        TEXT NOT NULL REFERENCES node(id),
  number         INTEGER NOT NULL CHECK (number >= 1),
  markdown       TEXT NOT NULL,
  written_at_ms  INTEGER NOT NULL,
  PRIMARY KEY (node_id, number)
);
`

// migrate applies every step the file has not had, each in its own
// transaction with the stamp, and refuses a file stamped newer than this build.
func (db *DB) migrate(ctx context.Context) error {
	var stamp int
	if err := db.write.QueryRowContext(ctx, "PRAGMA user_version").Scan(&stamp); err != nil {
		db.log.Error("zettel.db.migrate", "reading the layout stamp failed", zlog.Context{"cause": err.Error()})
		return storagef(err, "reading the layout stamp")
	}
	if stamp > len(migrations) {
		db.log.Error("zettel.db.migrate", "the database was written by a newer build; refusing it rather than rewriting it",
			zlog.Context{"file_layout": stamp, "build_layout": len(migrations)})
		return fmt.Errorf("%w: database layout %d is newer than this build's %d", ErrStorage, stamp, len(migrations))
	}
	for _, m := range migrations[stamp:] {
		if err := db.applyMigration(ctx, m); err != nil {
			return err
		}
	}
	return nil
}

func (db *DB) applyMigration(ctx context.Context, m migration) error {
	logCtx := zlog.Context{"step": m.Version, "name": m.Name}
	tx, err := db.write.BeginTx(ctx, nil)
	if err != nil {
		db.log.Error("zettel.db.migrate", "beginning a layout step failed", merge(logCtx, "cause", err.Error()))
		return storagef(err, "beginning layout step %d", m.Version)
	}
	if _, err := tx.ExecContext(ctx, m.DDL); err != nil {
		_ = tx.Rollback()
		db.log.Error("zettel.db.migrate", "a layout step failed; nothing of it was applied", merge(logCtx, "cause", err.Error()))
		return storagef(err, "applying layout step %d (%s)", m.Version, m.Name)
	}
	if _, err := tx.ExecContext(ctx, fmt.Sprintf("PRAGMA user_version = %d", m.Version)); err != nil {
		_ = tx.Rollback()
		db.log.Error("zettel.db.migrate", "stamping a layout step failed", merge(logCtx, "cause", err.Error()))
		return storagef(err, "stamping layout step %d", m.Version)
	}
	if err := tx.Commit(); err != nil {
		db.log.Error("zettel.db.migrate", "committing a layout step failed", merge(logCtx, "cause", err.Error()))
		return storagef(err, "committing layout step %d", m.Version)
	}
	db.log.Info("zettel.db.migrate", "applied a layout step", logCtx)
	return nil
}

func merge(ctx zlog.Context, key string, value any) zlog.Context {
	out := zlog.Context{key: value}
	for k, v := range ctx {
		out[k] = v
	}
	return out
}
