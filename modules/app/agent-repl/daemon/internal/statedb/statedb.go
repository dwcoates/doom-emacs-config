// Package statedb opens THE daemon's single SQLite state store — the one
// database the session-state manager's log and the session registry's
// identity tables both live in.
//
// One store, one connection, one writer. The session registry used to be a
// JSON file whose crash-safety came from atomic renames and a lock file, and
// whose "cursor and identity move together" property came from write-ordering
// discipline. Sharing the SSM's database turns both into what they should
// always have been: a transaction. That only holds if the two owners share ONE
// *sql.DB rather than opening competing writers, which is what this package
// exists to guarantee.
package statedb

import (
	"database/sql"
	"fmt"
	"net/url"
	"os"
	"path/filepath"

	_ "modernc.org/sqlite"
)

// DefaultPath returns ~/.cache/agent-repl/ssm/state.db, the daemon's state
// store.
func DefaultPath() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("statedb: cannot resolve home dir for default db path: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "ssm", "state.db"), nil
}

// OpenReadOnly opens an existing state database without creating a database,
// schema, directory, WAL file, or any other durable residue.  Operator audits
// use this boundary so their normal mode cannot change the inspected store.
func OpenReadOnly(path string) (*sql.DB, error) {
	if path == "" {
		return nil, fmt.Errorf("statedb: empty db path")
	}
	if path == ":memory:" {
		return nil, fmt.Errorf("statedb: in-memory db path %q is not an auditable durable store", path)
	}
	if _, err := os.Stat(path); err != nil {
		return nil, fmt.Errorf("statedb: inspect read-only db path %q: %w", path, err)
	}
	return openSQLite(path, "read-only", (&url.URL{Scheme: "file", Path: path}).String()+"?mode=ro&_pragma=query_only(1)&_pragma=busy_timeout(5000)")
}

// OpenExisting opens an existing state database for an explicit operator
// migration.  Unlike Open, it refuses to create a path or directory before the
// operator's selected action has been validated.
func OpenExisting(path string) (*sql.DB, error) {
	if path == "" {
		return nil, fmt.Errorf("statedb: empty db path")
	}
	if path == ":memory:" {
		return nil, fmt.Errorf("statedb: in-memory db path %q is not an auditable durable store", path)
	}
	if _, err := os.Stat(path); err != nil {
		return nil, fmt.Errorf("statedb: inspect existing db path %q: %w", path, err)
	}
	return openSQLite(path, "existing", path+"?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate")
}

func openSQLite(path, mode, dsn string) (*sql.DB, error) {
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("statedb: open %s %q: %w", mode, path, err)
	}
	db.SetMaxOpenConns(1)
	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("statedb: open %s %q: %w", mode, path, err)
	}
	return db, nil
}

// Open opens (creating parent dirs as needed) the state database. A path of
// ":memory:" is rejected: this store must be reopen-durable, and WAL is
// meaningless in memory.
//
// The DSN carries the store's locking discipline:
//
//   - journal_mode(WAL) for durable concurrent reads;
//   - busy_timeout so a momentarily locked DB waits rather than erroring
//     (a second daemon draining over the same file is a real case);
//   - _txlock=immediate so every transaction takes its write lock UP FRONT.
//     A deferred transaction that upgrades halfway through cannot be made to
//     wait — SQLite fails it with SQLITE_BUSY_SNAPSHOT rather than blocking —
//     which is exactly how two writers on one file lose an update.
//
// A single open connection keeps append ordering and every SELECT-then-write
// check race-free without a table lock.
func Open(path string) (*sql.DB, error) {
	if path == "" {
		return nil, fmt.Errorf("statedb: empty db path")
	}
	if path == ":memory:" {
		return nil, fmt.Errorf("statedb: in-memory db path %q is not allowed; the state store must be reopen-durable", path)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("statedb: cannot create db dir for %q: %w", path, err)
	}
	// modernc.org/sqlite reads PRAGMAs from _pragma query params.
	dsn := path + "?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)&_txlock=immediate"
	// sql.Open is lazy; openSQLite forces a real handshake so a path that is
	// not a database fails HERE, loudly, instead of on the first unrelated query.
	return openSQLite(path, "writable", dsn)
}
