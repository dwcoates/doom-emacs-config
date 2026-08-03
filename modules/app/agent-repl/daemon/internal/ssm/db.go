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

	"claude-repld/internal/statedb"
)

// schemaVersion is the current workspace_state schema revision. Bumped
// whenever the on-disk shape changes; migrate() refuses to open a DB
// written by a NEWER schema than this binary understands (loud, no
// silent downgrade).
const schemaVersion = 6

// defaultDBPath is the daemon's ONE state store — the SSM's log and the
// session registry's identity tables share it (§9.2: "own SQLite DB",
// distinct from the shim-store).
func defaultDBPath() (string, error) {
	path, err := statedb.DefaultPath()
	if err != nil {
		return "", fmt.Errorf("ssm: %w", err)
	}
	return path, nil
}

// openDB opens the state store (see internal/statedb for the WAL/busy-timeout/
// immediate-transaction discipline every owner of the store shares) and runs
// the SSM's migrations.
func openDB(path string) (*sql.DB, error) {
	db, err := statedb.Open(path)
	if err != nil {
		return nil, fmt.Errorf("ssm: %w", err)
	}
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
		CREATE TABLE IF NOT EXISTS turn_lifecycle_claim (
			claim_id             INTEGER PRIMARY KEY AUTOINCREMENT,
			workspace            TEXT    NOT NULL,
			claimant_session_id  TEXT    NOT NULL,
			turn_id              TEXT    NOT NULL,
			start_seq            INTEGER NOT NULL DEFAULT 0,
			start_event_session_id TEXT  NOT NULL DEFAULT '',
			bridge_seq           INTEGER,
			bridge_event_session_id TEXT NOT NULL DEFAULT '',
			end_seq              INTEGER,
			end_event_session_id TEXT NOT NULL DEFAULT '',
			-- end_cause names the SYNTHESIZED terminal close, for a claim the
			-- daemon ended without a TurnEnded because the live shim stated the
			-- turn does not exist. Empty on every claim closed by a real
			-- boundary, which is what lets recordTurnEnd tell a late-arriving
			-- genuine end for an already-accounted turn from a contradiction.
			end_cause            TEXT NOT NULL DEFAULT ''
		);
		CREATE INDEX IF NOT EXISTS turn_lifecycle_claim_active
			ON turn_lifecycle_claim(workspace, claimant_session_id, end_seq, claim_id);
		CREATE UNIQUE INDEX IF NOT EXISTS turn_lifecycle_claim_identity
			ON turn_lifecycle_claim(workspace, claimant_session_id, turn_id)
			WHERE turn_id <> '';
		CREATE TABLE IF NOT EXISTS session_connectivity (
			workspace                 TEXT    NOT NULL,
			agent_repl_session_id     TEXT    NOT NULL,
			controller_generation_id  TEXT    NOT NULL,
			state                     TEXT    NOT NULL,
			cause_kind                TEXT    NOT NULL,
			at                        INTEGER NOT NULL,
			PRIMARY KEY (workspace, at)
		);
		CREATE INDEX IF NOT EXISTS session_connectivity_ws
			ON session_connectivity(workspace, at);
		CREATE TABLE IF NOT EXISTS session_fault (
			workspace                 TEXT    NOT NULL,
			agent_repl_session_id     TEXT    NOT NULL,
			controller_generation_id  TEXT    NOT NULL,
			component                 TEXT    NOT NULL,
			fault_type                TEXT    NOT NULL,
			impact                    TEXT    NOT NULL,
			open                      INTEGER NOT NULL CHECK (open IN (0, 1)),
			cause_kind                TEXT    NOT NULL,
			at                        INTEGER NOT NULL,
			PRIMARY KEY (
				workspace,
				controller_generation_id,
				component,
				fault_type,
				at
			)
		);
		CREATE INDEX IF NOT EXISTS session_fault_current
			ON session_fault(
				workspace,
				controller_generation_id,
				component,
				fault_type,
				at
			);
		-- THE MERGE LEASE LEDGER: one row per exclusivity window merge.Coordinator
		-- has held over a workspace's shim. It is a LEDGER, not a flag, and that
		-- is what makes conversation provenance reproducible: an item's source is
		-- decided by whether its timestamp falls inside a window, so a resync or a
		-- transcript replay years later reaches the same verdict as the live push
		-- instead of re-deriving it from a lease that has long since been released.
		--
		-- An OPEN window (released_at IS NULL) is a lease currently held. It
		-- survives a daemon bounce on purpose: merge.Coordinator.Drain
		-- reconstructs the merge behind it rather than the lease being silently
		-- dropped and the workspace quietly re-opened to user prompts.
		CREATE TABLE IF NOT EXISTS merge_lease (
			lease_id     INTEGER PRIMARY KEY AUTOINCREMENT,
			workspace    TEXT    NOT NULL,
			acquired_at  INTEGER NOT NULL,
			released_at  INTEGER
		);
		CREATE INDEX IF NOT EXISTS merge_lease_window
			ON merge_lease(workspace, acquired_at, released_at);
		-- AT MOST ONE OPEN WINDOW PER WORKSPACE, enforced by the database rather
		-- than by the Go that writes it. Two coordinators believing they both hold
		-- the same shim is exactly the failure the lease exists to prevent, so it
		-- is made unrepresentable at the substrate instead of merely unlikely.
		CREATE UNIQUE INDEX IF NOT EXISTS merge_lease_open
			ON merge_lease(workspace) WHERE released_at IS NULL;
		-- WHEN EACH WORKSPACE'S MERGE LANDED: one row per workspace that has ever
		-- reached the merged phase, written at that transition and never
		-- rewritten. It is a fact of its own rather than a query over the state
		-- log, because the log answers "which merge row is newest" — something a
		-- later transition can change — while "this workspace merged, at this
		-- instant" is permanent the moment it becomes true. The frontend's
		-- recently-merged ordering reads it, so re-deriving it would make that
		-- order depend on whatever happened to the workspace afterwards.
		--
		-- The PRIMARY KEY is what makes set-once STRUCTURAL: the database decides
		-- that a second merged transition cannot move the instant, rather than Go
		-- remembering to look first.
		CREATE TABLE IF NOT EXISTS workspace_merged (
			workspace  TEXT    PRIMARY KEY,
			merged_at  INTEGER NOT NULL
		);
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
	if err := addTurnClaimColumns(db); err != nil {
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

// addTurnClaimColumns installs the receipt coordinates used to distinguish the
// real TurnStarted from a non-lifecycle TurnClaimBridge in a rotated vendor seq
// space. Each ALTER is independently idempotent so a daemon can safely open a
// DB left between migration statements by a crash.
func addTurnClaimColumns(db *sql.DB) error {
	columns := []struct {
		name string
		ddl  string
	}{
		{"start_event_session_id", `TEXT NOT NULL DEFAULT ''`},
		{"bridge_seq", `INTEGER`},
		{"bridge_event_session_id", `TEXT NOT NULL DEFAULT ''`},
		{"end_event_session_id", `TEXT NOT NULL DEFAULT ''`},
		{"end_cause", `TEXT NOT NULL DEFAULT ''`},
	}
	for _, column := range columns {
		has, err := hasColumn(db, "turn_lifecycle_claim", column.name)
		if err != nil {
			return err
		}
		if has {
			continue
		}
		if _, err := db.Exec(fmt.Sprintf(
			`ALTER TABLE turn_lifecycle_claim ADD COLUMN %s %s`,
			column.name, column.ddl,
		)); err != nil {
			return fmt.Errorf("ssm: add turn_lifecycle_claim.%s: %w", column.name, err)
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

// sessionStatusMembers is the session-status lifecycle's row membership, as one SQL list shared
// by every reader of the axis. It is the same membership the resolution query
// carries in its `prec` table, kept in one place here so a reader and the
// resolver can never disagree about which rows count as agent states.
const sessionStatusMembers = `('submitting','thinking','permission','done','ready','idle','dead','vendor_blocked','interrupted')`

// turnActive reports whether the workspace's LATEST session-status lifecycle row is a
// running turn. It is the no-regress guard's only input: a readiness
// assertion arriving over a live turn must be dropped rather than appended,
// because the session-status lifecycle resolves on its latest row and the readiness row
// would otherwise win.
//
// A workspace whose top row is `permission` reads FALSE, exactly as `done`
// does. That is deliberate and predates the permission producer: the row is a
// settled statement about what the workspace is waiting for, and every caller
// of this (the readiness no-regress guard, the interrupt mark, the rotation
// reconciliation) is asking whether the agent is CURRENTLY working, which it
// is not while a human is being asked a question.
func turnActive(db *sql.DB, workspace string) (bool, error) {
	active, _, err := turnClaim(db, workspace)
	return active, err
}

// turnClaim answers turnActive AND names the session that made the claim.
//
// The claimant matters because a `thinking` row is a promise that some session
// will report the turn's end, and a session that is gone can never keep it. A
// claim whose session no longer drives the workspace is therefore not a
// stronger statement than the readiness arriving over it — it is a dead one,
// and treating it as stronger is what wedged a recreated workspace in
// "readiness suppressed (turn in flight)" across a delete-and-recreate.
//
// claimant is "" for an UNATTRIBUTED claim — a row the SSM itself appended
// with no session id, such as the `thinking` a permission close restores. That
// is deliberately NOT read as "a different session": the row describes the
// workspace's own turn and there is no rival identity in it, so it keeps
// suppressing exactly as it always has.
func turnClaim(db *sql.DB, workspace string) (active bool, claimant string, err error) {
	var (
		state string
		sid   sql.NullString
	)
	scanErr := db.QueryRow(
		`SELECT state, session_id FROM workspace_state
		 WHERE workspace = ?
		   AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`, workspace).Scan(&state, &sid)
	if scanErr == sql.ErrNoRows {
		return false, "", nil
	}
	if scanErr != nil {
		return false, "", fmt.Errorf("ssm: turn-active check for workspace %q: %w", workspace, scanErr)
	}
	// BOTH halves of a turn claim it. `submitting` is a turn in flight whose
	// prompt the shim has not acked yet, so a claim check that only counted
	// `thinking` would report the workspace idle during it and let a second
	// prompt bypass the queue into a turn that is already starting.
	return state == sigThinking || state == sigSubmitting, sid.String, nil
}

// sessionStatusTop returns the workspace's newest session-status lifecycle token, plus the newest
// session-status lifecycle token BENEATH any `permission` row sitting on top of it. Both are
// "" when the axis has no such row.
//
// The second answer is what the permission row's CLOSING edge needs. A pending
// permission is written as an session-status lifecycle row that supersedes the `thinking` it
// covers, so once it is answered the only record of whether a turn is still
// running is the row it buried. Reading it back is how the close restores
// `thinking` for a turn that is genuinely still in flight — and how it knows
// NOT to, for one that is not.
func sessionStatusTop(db *sql.DB, workspace string) (top, beneath string, err error) {
	scan := func(query string) (string, error) {
		var state string
		e := db.QueryRow(query, workspace).Scan(&state)
		if e == sql.ErrNoRows {
			return "", nil
		}
		if e != nil {
			return "", fmt.Errorf("ssm: session-status lifecycle read for workspace %q: %w", workspace, e)
		}
		return state, nil
	}
	top, err = scan(`SELECT state FROM workspace_state
		 WHERE workspace = ? AND state IN ` + sessionStatusMembers + `
		 ORDER BY at DESC LIMIT 1`)
	if err != nil {
		return "", "", err
	}
	beneath, err = scan(`SELECT state FROM workspace_state
		 WHERE workspace = ? AND state IN ` + sessionStatusMembers + ` AND state <> 'permission'
		 ORDER BY at DESC LIMIT 1`)
	if err != nil {
		return "", "", err
	}
	return top, beneath, nil
}

// permissionOpenWorkspaces lists every workspace whose session-status lifecycle currently
// tops out in `permission`. It backs the release at Open: the pendings a
// permission row stands for are in-process rendezvous that do not survive a
// daemon restart, so a row left standing across one has nothing that could
// answer it.
func permissionOpenWorkspaces(db *sql.DB) ([]string, error) {
	names, err := distinctWorkspaces(db)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, ws := range names {
		top, _, err := sessionStatusTop(db, ws)
		if err != nil {
			return nil, err
		}
		if top == sigPermission {
			out = append(out, ws)
		}
	}
	return out, nil
}

// cutOpen reports whether a context-cut axis currently stands open: its LATEST
// row is the opening token rather than the closing one. A workspace with no
// row on the axis at all has never had a cut and is closed.
//
// It is the guard on every edge that moves one of the two axes, so a close
// with nothing open appends no row and an open that is already open does not
// stack a second one.
func cutOpen(db *sql.DB, workspace, openToken, closeToken string) (bool, error) {
	var state string
	err := db.QueryRow(
		`SELECT state FROM workspace_state
		 WHERE workspace = ? AND state IN (?, ?)
		 ORDER BY at DESC LIMIT 1`, workspace, openToken, closeToken).Scan(&state)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: %s-axis check for workspace %q: %w", openToken, workspace, err)
	}
	return state == openToken, nil
}

// openCutWorkspaces lists every workspace whose OPENTOKEN axis stands open. It
// backs the clearing watchdog's re-arm at Open: the timer is in memory, so a
// daemon restart mid-clear would otherwise leave the axis with nothing left
// that could ever expire it.
func openCutWorkspaces(db *sql.DB, openToken, closeToken string) ([]string, error) {
	names, err := distinctWorkspaces(db)
	if err != nil {
		return nil, err
	}
	var out []string
	for _, ws := range names {
		open, err := cutOpen(db, ws, openToken, closeToken)
		if err != nil {
			return nil, err
		}
		if open {
			out = append(out, ws)
		}
	}
	return out, nil
}

// distinctWorkspaces lists every workspace that has status or connectivity
// history, in stable order, for Snapshot.
func distinctWorkspaces(db *sql.DB) ([]string, error) {
	rows, err := db.Query(
		`SELECT workspace FROM (
			SELECT workspace FROM workspace_state
			UNION
			SELECT workspace FROM session_connectivity
		)
		ORDER BY workspace`)
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
