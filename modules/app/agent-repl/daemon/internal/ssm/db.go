// Package ssm is the daemon's session-state manager (§9.2 of the
// agent-shim architecture design). It owns the resolved per-workspace
// render-state in its OWN append-only SQLite log, ingesting lifecycle
// events forwarded from shim streams plus daemon-local merge
// transitions, and resolving the current state via SQL whose precedence
// lives in a data table (not a hardcoded cond-ladder). It replaces the
// Emacs-persisted `:agent-state` entirely; state is rebuildable from the
// log, so it survives an SSM reopen.
package ssm

import (
	"database/sql"
	"fmt"
	"os"
	"path/filepath"

	_ "modernc.org/sqlite"
)

// schemaVersion is the current workspace_state schema revision. Bumped
// whenever the on-disk shape changes; migrate() refuses to open a DB
// written by a NEWER schema than this binary understands (loud, no
// silent downgrade).
const schemaVersion = 2

// defaultDBPath is the SSM's own database, distinct from the shim-store
// (§9.2: "own SQLite DB").
func defaultDBPath() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("ssm: cannot resolve home dir for default db path: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "ssm", "state.db"), nil
}

// openDB opens (creating parent dirs as needed) the SSM database in WAL
// mode and runs migrations. A path of ":memory:" is rejected: the SSM
// must be reopen-durable, and WAL is meaningless in memory.
func openDB(path string) (*sql.DB, error) {
	if path == "" {
		return nil, fmt.Errorf("ssm: empty db path")
	}
	if path == ":memory:" {
		return nil, fmt.Errorf("ssm: in-memory db path %q is not allowed; the SSM must be reopen-durable", path)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("ssm: cannot create db dir for %q: %w", path, err)
	}
	// modernc.org/sqlite reads PRAGMAs from _pragma query params. WAL for
	// durable concurrent reads; busy_timeout so a momentarily locked DB
	// waits rather than erroring.
	dsn := path + "?_pragma=busy_timeout(5000)&_pragma=journal_mode(WAL)"
	db, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, fmt.Errorf("ssm: open %q: %w", path, err)
	}
	// A single writer connection keeps append ordering and the SELECT-then-
	// INSERT idempotency check race-free without a table lock.
	db.SetMaxOpenConns(1)
	if err := migrate(db); err != nil {
		db.Close()
		return nil, err
	}
	return db, nil
}

// migrate creates the schema on a fresh DB and validates the version on
// an existing one. The workspace_state table is the §9.2 append-only log:
// current state for a workspace is the resolution over its rows (see
// resolve.go); the `state` column holds a SIGNAL token (a superset of the
// RenderState vocabulary — e.g. task_started/merge_none — that the resolve
// query maps back onto render states).
func migrate(db *sql.DB) error {
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS schema_meta (version INTEGER NOT NULL);
		CREATE TABLE IF NOT EXISTS workspace_state (
			workspace   TEXT    NOT NULL,
			session_id  TEXT,
			state       TEXT    NOT NULL,
			cause_kind  TEXT    NOT NULL,
			cause_seq   INTEGER,
			at          INTEGER NOT NULL,
			PRIMARY KEY (workspace, at)
		);
		CREATE INDEX IF NOT EXISTS workspace_state_ws ON workspace_state(workspace, at);
		CREATE INDEX IF NOT EXISTS workspace_state_seq ON workspace_state(session_id, cause_seq);
	`); err != nil {
		return fmt.Errorf("ssm: create schema: %w", err)
	}

	// v2: task_id on the task_started/task_ended rows, so live_task_count can
	// count DISTINCT tasks instead of summing rows. Without it a second
	// TaskEnded for the same task (a spool's EXIT= marker after a TaskStop
	// tool result, say) decrements the counter twice and can drive it
	// negative. Added out-of-band because ALTER TABLE ADD COLUMN is not
	// idempotent in SQLite; an existing column is the migration's success
	// condition, not an error.
	if err := addTaskIDColumn(db); err != nil {
		return err
	}

	var version sql.NullInt64
	if err := db.QueryRow(`SELECT version FROM schema_meta LIMIT 1`).Scan(&version); err != nil {
		if err != sql.ErrNoRows {
			return fmt.Errorf("ssm: read schema version: %w", err)
		}
	}
	if !version.Valid {
		if _, err := db.Exec(`INSERT INTO schema_meta(version) VALUES (?)`, schemaVersion); err != nil {
			return fmt.Errorf("ssm: initialize schema version: %w", err)
		}
		return nil
	}
	if version.Int64 > schemaVersion {
		return fmt.Errorf("ssm: db schema version %d is newer than this binary understands (%d); refusing to open", version.Int64, schemaVersion)
	}
	if version.Int64 < schemaVersion {
		// Every forward migration to date is idempotent and already ran above;
		// record the new version so the stamp keeps describing the DB. Leaving
		// it stale would make the version meaningless as a guard.
		if _, err := db.Exec(`UPDATE schema_meta SET version = ?`, schemaVersion); err != nil {
			return fmt.Errorf("ssm: record schema version %d: %w", schemaVersion, err)
		}
	}
	return nil
}

// addTaskIDColumn adds workspace_state.task_id when it is absent. SQLite has
// no ADD COLUMN IF NOT EXISTS, so the column list is inspected first: a DB
// already carrying the column is the migration having run, not a failure.
// Any OTHER error still propagates.
func addTaskIDColumn(db *sql.DB) error {
	has, err := hasColumn(db, "workspace_state", "task_id")
	if err != nil {
		return err
	}
	if has {
		return nil
	}
	if _, err := db.Exec(`ALTER TABLE workspace_state ADD COLUMN task_id TEXT`); err != nil {
		return fmt.Errorf("ssm: add workspace_state.task_id: %w", err)
	}
	return nil
}

// hasColumn reports whether table carries column.
func hasColumn(db *sql.DB, table, column string) (bool, error) {
	rows, err := db.Query(`SELECT name FROM pragma_table_info(?)`, table)
	if err != nil {
		return false, fmt.Errorf("ssm: inspect %s columns: %w", table, err)
	}
	defer rows.Close()
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			return false, fmt.Errorf("ssm: scan %s column name: %w", table, err)
		}
		if name == column {
			return true, nil
		}
	}
	if err := rows.Err(); err != nil {
		return false, fmt.Errorf("ssm: iterate %s columns: %w", table, err)
	}
	return false, nil
}

type rowExecer interface {
	Exec(query string, args ...any) (sql.Result, error)
}

// appendRow appends one signal to the log. causeSeq is invalid (NULL) for
// daemon-local transitions with no store seq. `at` MUST be strictly
// increasing per workspace (the caller's monotonic clock guarantees it),
// so the (workspace, at) primary key never collides.
//
// taskID identifies the task a task_started/task_ended row is about, and is
// empty for every other signal. It is what makes the live-task counter
// idempotent per task rather than per row.
func appendRow(db rowExecer, workspace, sessionID, state, causeKind string, causeSeq sql.NullInt64, at int64, taskID string) error {
	var sid any
	if sessionID != "" {
		sid = sessionID
	}
	var tid any
	if taskID != "" {
		tid = taskID
	}
	_, err := db.Exec(
		`INSERT INTO workspace_state(workspace, session_id, state, cause_kind, cause_seq, at, task_id) VALUES (?,?,?,?,?,?,?)`,
		workspace, sid, state, causeKind, causeSeq, at, tid)
	if err != nil {
		return fmt.Errorf("ssm: append %q for workspace %q: %w", state, workspace, err)
	}
	return nil
}

// seqApplied reports whether an event with (sessionID, seq) already
// produced a row. It backs Apply's idempotency: the store assigns gapless
// per-session seqs, so (session_id, cause_seq) uniquely identifies an
// event and a replayed event is a no-op.
func seqApplied(db *sql.DB, sessionID string, seq uint64) (bool, error) {
	var one int
	err := db.QueryRow(
		`SELECT 1 FROM workspace_state WHERE session_id = ? AND cause_seq = ? LIMIT 1`,
		sessionID, int64(seq)).Scan(&one)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: idempotency check for session %q seq %d: %w", sessionID, seq, err)
	}
	return true, nil
}

// turnActive reports whether the workspace's LATEST agent-axis row is a
// running turn. It is the no-regress guard's only input: a readiness
// assertion arriving over a live turn must be dropped rather than appended,
// because the agent axis resolves on its latest row and the readiness row
// would otherwise win.
//
// It reads the same agent-axis membership the resolution query does, so the
// two can never disagree about which rows count as agent states.
func turnActive(db *sql.DB, workspace string) (bool, error) {
	var state string
	err := db.QueryRow(
		`SELECT state FROM workspace_state
		 WHERE workspace = ?
		   AND state IN ('init','thinking','permission','done','ready','idle','dead','vendor_blocked','interrupted')
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: turn-active check for workspace %q: %w", workspace, err)
	}
	return state == "thinking", nil
}

// paintWatermark returns the highest seq a frontend has attested painting
// for the workspace, and whether any attestation currently stands.
//
// The two answers are separate on purpose: seq 0 is a REAL attestation of an
// empty history, so "attested at 0" and "never attested" are different facts
// that a single integer could not distinguish. Without the boolean, the
// never-prompted session this whole mechanism exists to make ready could
// never be told apart from one that had reported nothing.
func paintWatermark(db *sql.DB, workspace string) (uint64, bool, error) {
	var (
		state string
		seq   sql.NullInt64
	)
	err := db.QueryRow(
		`SELECT state, cause_seq FROM workspace_state
		 WHERE workspace = ? AND state IN ('painted','unpainted')
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state, &seq)
	if err == sql.ErrNoRows {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, fmt.Errorf("ssm: paint watermark for workspace %q: %w", workspace, err)
	}
	if state != "painted" {
		// The latest paint-axis row withdrew the attestation, so the
		// watermark is gone with it: a re-attestation after a route break
		// starts from nothing rather than inheriting the pre-break seq.
		return 0, false, nil
	}
	return uint64(seq.Int64), true, nil
}

// distinctWorkspaces lists every workspace that has any logged signal, in
// stable order, for Snapshot.
func distinctWorkspaces(db *sql.DB) ([]string, error) {
	rows, err := db.Query(`SELECT DISTINCT workspace FROM workspace_state ORDER BY workspace`)
	if err != nil {
		return nil, fmt.Errorf("ssm: list workspaces: %w", err)
	}
	defer rows.Close()
	var out []string
	for rows.Next() {
		var ws string
		if err := rows.Scan(&ws); err != nil {
			return nil, fmt.Errorf("ssm: scan workspace: %w", err)
		}
		out = append(out, ws)
	}
	return out, rows.Err()
}
