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

	"claude-repld/internal/dlog"
	"claude-repld/internal/statedb"
)

// schemaVersion is the current workspace_state schema revision. Bumped
// whenever the on-disk shape changes; migrate() refuses to open a DB
// written by a NEWER schema than this binary understands (loud, no
// silent downgrade).
//
// v7 splits the two identities that shared workspace_state.session_id. See
// normalizeSessionIdentity.
const schemaVersion = 7

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
func openDB(path string, logf dlog.Logf) (*sql.DB, error) {
	db, err := statedb.Open(path)
	if err != nil {
		return nil, fmt.Errorf("ssm: %w", err)
	}
	if err := migrate(db, logf); err != nil {
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
func migrate(db *sql.DB, logf dlog.Logf) error {
	if logf == nil {
		return fmt.Errorf("ssm: migrate requires a logger; a schema migration that normalizes rows may never run silently")
	}
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
	if err := addEventSessionIDColumn(db); err != nil {
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
		// Column-adding migrations are idempotent and already ran above. The
		// v7 identity normalization is NOT idempotent in the same trivial way
		// — it rewrites row VALUES — so it is gated on the stamp and runs
		// exactly once, on a database written before the write path stopped
		// producing the rows it repairs.
		if version.Int64 < 7 {
			if err := normalizeSessionIdentity(db, logf); err != nil {
				return err
			}
		}
		if _, err := db.Exec(`UPDATE schema_meta SET version = ?`, schemaVersion); err != nil {
			return fmt.Errorf("ssm: record schema version %d: %w", schemaVersion, err)
		}
	}
	return nil
}

// addEventSessionIDColumn installs workspace_state.event_session_id, the STORE
// coordinate half of what the session_id column used to carry alone.
//
// See normalizeSessionIdentity for what the split is for. The short version:
// session_id names the SESSION that owns a row (always the daemon-minted
// s_<hex> id, the one every claimant comparison uses), and event_session_id
// names the identity the STORE filed the causing event under (the vendor
// uuid). The two are different strings for the same conversation, and the
// idempotency check needs the second one — a vendor uuid rotation restarts the
// store's seq space at 1, so deduplicating on the daemon id would read a new
// space's seq 1 as a replay of the retired space's.
func addEventSessionIDColumn(db *sql.DB) error {
	has, err := hasColumn(db, "workspace_state", "event_session_id")
	if err != nil {
		return err
	}
	if has {
		return nil
	}
	if _, err := db.Exec(`ALTER TABLE workspace_state ADD COLUMN event_session_id TEXT`); err != nil {
		return fmt.Errorf("ssm: add workspace_state.event_session_id: %w", err)
	}
	if _, err := db.Exec(
		`CREATE INDEX IF NOT EXISTS workspace_state_event_seq ON workspace_state(event_session_id, cause_seq)`,
	); err != nil {
		return fmt.Errorf("ssm: index workspace_state.event_session_id: %w", err)
	}
	return nil
}

// normalizeSessionIdentity is the v7 migration: it gives the session-status
// axis exactly ONE identity per session.
//
// THE BUG IT REPAIRS. Every event the store streams back is filed under the
// VENDOR session uuid, and Apply used to stamp that uuid into
// workspace_state.session_id — while MarkPromptAccepted, CloseStaleTurn,
// InvalidateTurnClaim and the durable turn ledger all name the session by its
// daemon-minted s_<hex> id. One session therefore held rows under two names,
// and every claimant comparison between them read "a different session". A
// `thinking` written by a TurnStarted was consequently unclosable by its own
// session's teardown ("DECLINED — the standing `thinking` is held by session=…,
// which is not this stop's to spend") and unclaimable by its own session's next
// prompt ("while session … owns the active turn"). The workspace rendered
// thinking forever and could not be driven again — observed on
// merge-proto-json at 2026-08-03T01:26Z, wedged across restarts because the
// rows are durable.
//
// THE WRITE PATH IS THE FIX (Apply now canonicalizes through the resolver
// before it appends), which makes recurrence impossible. This migration exists
// only for databases that already hold the bad rows, and it is a ONE-TIME
// normalization rather than a tolerated equivalence: nothing downstream is
// taught that two ids may mean one session, because after this nothing writes
// the second id.
//
// THE MAPPING IS THE REGISTRY'S, read from the same database (the SSM's log and
// the session registry's identity tables deliberately share one store). A row
// is rewritten only when a session record for that row's OWN workspace claims
// the row's session_id as its vendor uuid; a uuid that matches no record for
// that workspace is left alone rather than guessed at.
func normalizeSessionIdentity(db *sql.DB, logf dlog.Logf) error {
	hasRecords, err := hasTable(db, "session_record")
	if err != nil {
		return err
	}
	if !hasRecords {
		// The registry's tables are created before the SSM opens in the daemon,
		// so this is a store the registry has never touched (a bare test or
		// tooling handle). There is no mapping to normalize against, and
		// guessing is not an option, so the split still happens and the rewrite
		// does not.
		logf("ssm: schema v7 normalization SKIPPED the session_id rewrite — this state store carries no session_record table, so no vendor→daemon mapping exists to rewrite against")
	}

	// The store coordinate first: every existing seq-bearing row's session_id
	// IS the event identity, because that is exactly what the old write path
	// put there. Copying it before the rewrite is what keeps idempotency
	// working across the migration.
	split, err := db.Exec(`
		UPDATE workspace_state
		   SET event_session_id = session_id
		 WHERE event_session_id IS NULL
		   AND cause_seq IS NOT NULL
		   AND session_id IS NOT NULL`)
	if err != nil {
		return fmt.Errorf("ssm: schema v7 split of workspace_state.event_session_id: %w", err)
	}
	splitRows, err := split.RowsAffected()
	if err != nil {
		return fmt.Errorf("ssm: schema v7 split row count: %w", err)
	}

	var rewrittenRows int64
	if hasRecords {
		rewrite, err := db.Exec(`
			UPDATE workspace_state
			   SET session_id = (
				   SELECT r.session_id FROM session_record r
				    WHERE r.claude_session_id = workspace_state.session_id
				      AND r.cwd = workspace_state.workspace
				    ORDER BY r.created_at DESC LIMIT 1)
			 WHERE session_id IS NOT NULL
			   AND EXISTS (
				   SELECT 1 FROM session_record r
				    WHERE r.claude_session_id = workspace_state.session_id
				      AND r.cwd = workspace_state.workspace)`)
		if err != nil {
			return fmt.Errorf("ssm: schema v7 normalization of workspace_state.session_id: %w", err)
		}
		rewrittenRows, err = rewrite.RowsAffected()
		if err != nil {
			return fmt.Errorf("ssm: schema v7 normalization row count: %w", err)
		}
	}
	logf("ssm: schema v7 normalization COMPLETE store_coordinates_split=%d vendor_ids_rewritten_to_daemon_ids=%d — the session-status axis now names every session by its daemon id alone, so a turn claim can be closed and re-claimed by the session that made it",
		splitRows, rewrittenRows)
	return nil
}

// hasTable reports whether the state store carries table.
func hasTable(db *sql.DB, table string) (bool, error) {
	var name string
	err := db.QueryRow(
		`SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?`, table).Scan(&name)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: look up table %q: %w", table, err)
	}
	return true, nil
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
	return appendEventRow(db, workspace, sessionID, "", state, causeKind, causeSeq, at, taskID)
}

// appendEventRow appends a row whose cause is a STORE event, recording both
// identities the event has: sessionID is the daemon-minted id that OWNS the row
// (the one every claimant comparison reads), and eventSessionID is the vendor
// uuid the store filed the event under (the one the idempotency check reads).
//
// Callers with no store event behind them use appendRow, which leaves the
// store coordinate NULL. See normalizeSessionIdentity for why the two are
// separate columns rather than one.
func appendEventRow(db rowExecer, workspace, sessionID, eventSessionID, state, causeKind string, causeSeq sql.NullInt64, at int64, taskID string) error {
	var sid any
	if sessionID != "" {
		sid = sessionID
	}
	var esid any
	if eventSessionID != "" {
		esid = eventSessionID
	}
	var tid any
	if taskID != "" {
		tid = taskID
	}
	_, err := db.Exec(
		`INSERT INTO workspace_state(workspace, session_id, event_session_id, state, cause_kind, cause_seq, at, task_id) VALUES (?,?,?,?,?,?,?,?)`,
		workspace, sid, esid, state, causeKind, causeSeq, at, tid)
	if err != nil {
		return fmt.Errorf("ssm: append %q for workspace %q: %w", state, workspace, err)
	}
	return nil
}

// seqApplied reports whether an event with (eventSessionID, seq) already
// produced a row. It backs Apply's idempotency: the store assigns gapless
// per-conversation seqs, so (event_session_id, cause_seq) uniquely identifies
// an event and a replayed event is a no-op.
//
// IT READS THE STORE COORDINATE, never the owning session id. A vendor uuid
// rotation restarts the seq space at 1 under the SAME daemon session, so a
// check keyed on the owner would read the new space's first event as a replay
// of the retired space's and drop the turn it starts.
func seqApplied(db *sql.DB, eventSessionID string, seq uint64) (bool, error) {
	var one int
	err := db.QueryRow(
		`SELECT 1 FROM workspace_state WHERE event_session_id = ? AND cause_seq = ? LIMIT 1`,
		eventSessionID, int64(seq)).Scan(&one)
	if err == sql.ErrNoRows {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: idempotency check for event session %q seq %d: %w", eventSessionID, seq, err)
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
