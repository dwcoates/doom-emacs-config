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
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("statedb: open %q: %w", path, err)
	}
	db.SetMaxOpenConns(1)
	// sql.Open is lazy; force a real handshake so a path that is not a
	// database fails HERE, loudly, instead of on the first unrelated query.
	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("statedb: open %q: %w", path, err)
	}
	return db, nil
}
