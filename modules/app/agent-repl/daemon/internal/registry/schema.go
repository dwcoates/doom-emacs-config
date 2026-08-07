package registry

import (
	"database/sql"
	"fmt"
)

// metaSchemaVersion is the registry_meta key holding the schema stamp.
const metaSchemaVersion = "schema_version"

// migrate creates the registry's tables on a store that lacks them and
// validates the stamp on one that has them. It shares the database with the
// SSM's log, so it touches only its OWN tables.
//
// session_record is the roster of daemon sessions, keyed by the s_<hex> id
// frontends hold. conversation_checkpoint is the compact per-conversation
// state that must outlive a pruned session record — keyed by the full
// conversation identity, because one vendor uuid can legitimately exist under
// two account roots or two workspaces and neither may borrow the other's
// cursors.
func migrate(db *sql.DB) error {
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS registry_meta (
			key   TEXT PRIMARY KEY,
			value TEXT NOT NULL
		);
		CREATE TABLE IF NOT EXISTS session_record (
			session_id                  TEXT PRIMARY KEY,
			cwd                         TEXT    NOT NULL DEFAULT '',
			model                       TEXT    NOT NULL DEFAULT '',
			permission_mode             TEXT    NOT NULL DEFAULT '',
			config_dir                  TEXT    NOT NULL DEFAULT '',
			claude_session_id           TEXT    NOT NULL DEFAULT '',
			created_at                  TEXT    NOT NULL DEFAULT '',
			terminal                    INTEGER NOT NULL DEFAULT 0,
			death_reason                TEXT    NOT NULL DEFAULT '',
			terminal_at                 TEXT    NOT NULL DEFAULT '',
			last_seq                    INTEGER NOT NULL DEFAULT 0,
			newest_clear_or_compact_seq INTEGER NOT NULL DEFAULT 0,
			backfill_state              TEXT    NOT NULL DEFAULT '',
			queued_prompts              TEXT    NOT NULL DEFAULT '',
			last_turn_end_ms            INTEGER NOT NULL DEFAULT 0,
			hibernated                  INTEGER NOT NULL DEFAULT 0,
			hibernation_cause           TEXT    NOT NULL DEFAULT '',
			hibernated_since_ms         INTEGER NOT NULL DEFAULT 0,
			hibernation_cutoff_ms       INTEGER NOT NULL DEFAULT 0,
			hibernation_elapsed_ms      INTEGER NOT NULL DEFAULT 0,
			hibernation_ttl_ms          INTEGER NOT NULL DEFAULT 0,
			rewind_previous_vendor_session_id TEXT NOT NULL DEFAULT '',
			rewind_retained_leaf_uuid         TEXT NOT NULL DEFAULT '',
			rewind_dropped_turn_ids           TEXT NOT NULL DEFAULT '',
			death_resolved_at_ms              INTEGER NOT NULL DEFAULT 0
		);
		CREATE INDEX IF NOT EXISTS session_record_conversation
			ON session_record(config_dir, cwd, claude_session_id);
		CREATE TABLE IF NOT EXISTS conversation_checkpoint (
			config_dir                  TEXT    NOT NULL,
			cwd                         TEXT    NOT NULL,
			claude_session_id           TEXT    NOT NULL,
			last_seq                    INTEGER NOT NULL DEFAULT 0,
			newest_clear_or_compact_seq INTEGER NOT NULL DEFAULT 0,
			backfill_state              TEXT    NOT NULL DEFAULT '',
			PRIMARY KEY (config_dir, cwd, claude_session_id)
		);
	`); err != nil {
		return fmt.Errorf("registry: create schema: %w", err)
	}

	// CREATE TABLE IF NOT EXISTS is a NO-OP against a database that already has
	// session_record, so a column added to the DDL above reaches an existing
	// store only through this pass. Every addition is nullable-with-default and
	// purely additive, which is what lets it run unconditionally on every open
	// without a schema-version bump: an older binary reading a store that has
	// the columns simply never selects them.
	if err := addSessionRecordColumns(db); err != nil {
		return err
	}

	var version sql.NullInt64
	if err := db.QueryRow(
		`SELECT CAST(value AS INTEGER) FROM registry_meta WHERE key = ?`, metaSchemaVersion,
	).Scan(&version); err != nil && err != sql.ErrNoRows {
		return fmt.Errorf("registry: read schema version: %w", err)
	}
	if !version.Valid {
		if _, err := db.Exec(
			`INSERT INTO registry_meta(key, value) VALUES (?, ?)`, metaSchemaVersion, schemaVersion,
		); err != nil {
			return fmt.Errorf("registry: initialize schema version: %w", err)
		}
		return nil
	}
	if version.Int64 > schemaVersion {
		return fmt.Errorf("registry: db schema version %d is newer than this binary understands (%d); refusing to open",
			version.Int64, schemaVersion)
	}
	return nil
}

// sessionRecordAddedColumns are the session_record columns added after the
// table's original shape, with the DDL fragment each needs. Order is fixed so
// two daemons migrating the same store concurrently issue the same statements.
var sessionRecordAddedColumns = []struct{ name, ddl string }{
	{"last_turn_end_ms", "INTEGER NOT NULL DEFAULT 0"},
	{"hibernated", "INTEGER NOT NULL DEFAULT 0"},
	{"hibernation_cause", "TEXT NOT NULL DEFAULT ''"},
	{"hibernated_since_ms", "INTEGER NOT NULL DEFAULT 0"},
	{"hibernation_cutoff_ms", "INTEGER NOT NULL DEFAULT 0"},
	{"hibernation_elapsed_ms", "INTEGER NOT NULL DEFAULT 0"},
	{"hibernation_ttl_ms", "INTEGER NOT NULL DEFAULT 0"},
	{"rewind_previous_vendor_session_id", "TEXT NOT NULL DEFAULT ''"},
	{"rewind_retained_leaf_uuid", "TEXT NOT NULL DEFAULT ''"},
	{"rewind_dropped_turn_ids", "TEXT NOT NULL DEFAULT ''"},
	{"death_resolved_at_ms", "INTEGER NOT NULL DEFAULT 0"},
}

// addSessionRecordColumns brings an existing session_record table up to the
// current column set. A column already present is skipped rather than
// re-added: ALTER TABLE ADD COLUMN is not idempotent in SQLite, so the
// presence check IS the idempotence.
func addSessionRecordColumns(db *sql.DB) error {
	present, err := sessionRecordColumns(db)
	if err != nil {
		return err
	}
	for _, col := range sessionRecordAddedColumns {
		if present[col.name] {
			continue
		}
		if _, err := db.Exec(fmt.Sprintf(`ALTER TABLE session_record ADD COLUMN %s %s`, col.name, col.ddl)); err != nil {
			return fmt.Errorf("registry: add session_record column %s: %w", col.name, err)
		}
	}
	return nil
}

// sessionRecordColumns reports the column names session_record currently has.
func sessionRecordColumns(db *sql.DB) (map[string]bool, error) {
	rows, err := db.Query(`PRAGMA table_info(session_record)`)
	if err != nil {
		return nil, fmt.Errorf("registry: read session_record columns: %w", err)
	}
	defer rows.Close()
	present := map[string]bool{}
	for rows.Next() {
		var (
			cid        int
			name, typ  string
			notNull    int
			defaultVal sql.NullString
			pk         int
		)
		if err := rows.Scan(&cid, &name, &typ, &notNull, &defaultVal, &pk); err != nil {
			return nil, fmt.Errorf("registry: scan session_record column: %w", err)
		}
		present[name] = true
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("registry: iterate session_record columns: %w", err)
	}
	return present, nil
}

// querier is the read half shared by *sql.DB (Open's initial load) and *sql.Tx
// (every mutation's load).
type querier interface {
	Query(query string, args ...any) (*sql.Rows, error)
}

// loadState reads both tables. Any malformed row is an error; no partial
// registry is admitted, and the caller records it as a sticky failure so the
// daemon never serves fabricated empty state.
func loadState(q querier, logf func(string, ...any)) (map[string]Record, map[ConversationIdentity]ConversationCheckpoint, error) {
	records := map[string]Record{}
	checkpoints := map[ConversationIdentity]ConversationCheckpoint{}

	rows, err := q.Query(`SELECT session_id, cwd, model, permission_mode, config_dir, claude_session_id,
		created_at, terminal, death_reason, terminal_at, last_seq, newest_clear_or_compact_seq,
		backfill_state, queued_prompts, last_turn_end_ms, hibernated, hibernation_cause,
		hibernated_since_ms, hibernation_cutoff_ms, hibernation_elapsed_ms,
		hibernation_ttl_ms, rewind_previous_vendor_session_id, rewind_retained_leaf_uuid,
		rewind_dropped_turn_ids, death_resolved_at_ms FROM session_record`)
	if err != nil {
		logf("registry: READ FAILED for session_record — refusing to serve: %v", err)
		return records, checkpoints, fmt.Errorf("registry: read session records: %w", err)
	}
	defer rows.Close()
	for rows.Next() {
		var (
			rec    Record
			queued string
		)
		if err := rows.Scan(&rec.SessionID, &rec.CWD, &rec.Model, &rec.PermissionMode, &rec.ConfigDir,
			&rec.ClaudeSessionID, &rec.CreatedAt, &rec.Terminal, &rec.DeathReason, &rec.TerminalAt,
			&rec.LastSeq, &rec.NewestClearOrCompactSeq, &rec.BackfillState, &queued,
			&rec.LastTurnEndMs, &rec.Hibernated, &rec.Hibernation.Cause, &rec.Hibernation.SinceMs,
			&rec.Hibernation.CutoffMs, &rec.Hibernation.ElapsedMs, &rec.Hibernation.TTLMs,
			&rec.Rewind.PreviousVendorSessionID, &rec.Rewind.RetainedLeafUUID,
			&rec.Rewind.DroppedTurnIDs, &rec.DeathResolvedAtMs); err != nil {
			logf("registry: CORRUPT session_record row — refusing to serve: %v", err)
			return records, checkpoints, fmt.Errorf("registry: scan session record: %w", err)
		}
		if rec.SessionID == "" {
			logf("registry: INVALID record with empty session_id — refusing to serve")
			return records, checkpoints, fmt.Errorf("registry: record with empty session_id")
		}
		// A cause this binary cannot name is refused rather than passed
		// through: the whole point of the typed detail is that the revival gate
		// can explain the sleep, and an unnameable cause would reach the
		// frontend as "hibernated for no stated reason".
		if !ValidHibernationCause(rec.Hibernation.Cause) {
			logf("registry: INVALID hibernation cause %q on session %s — refusing to serve",
				rec.Hibernation.Cause, rec.SessionID)
			return records, checkpoints, fmt.Errorf("registry: session %s has invalid hibernation cause %q",
				rec.SessionID, rec.Hibernation.Cause)
		}
		// A PARTIAL LINEAGE IS REFUSED rather than carried. The shim rejects an
		// empty dropped-turn list, so spawning on one would leave the session
		// with no shim at all — and the three fields are written by one Update,
		// so a partial row is corruption rather than an older shape.
		if rec.Rewind.Partial() {
			logf("registry: INVALID partial rewind lineage on session %s (rewound_from=%q retained_leaf=%q dropped_turns=%q) — refusing to serve",
				rec.SessionID, rec.Rewind.PreviousVendorSessionID, rec.Rewind.RetainedLeafUUID, rec.Rewind.DroppedTurnIDs)
			return records, checkpoints, fmt.Errorf("registry: session %s has a partial rewind lineage", rec.SessionID)
		}
		prompts, err := decodeQueuedPrompts(rec.SessionID, queued)
		if err != nil {
			logf("registry: CORRUPT queued prompts — refusing to serve: %v", err)
			return records, checkpoints, err
		}
		rec.QueuedPrompts = prompts
		records[rec.SessionID] = rec
	}
	if err := rows.Err(); err != nil {
		logf("registry: READ FAILED iterating session_record — refusing to serve: %v", err)
		return records, checkpoints, fmt.Errorf("registry: iterate session records: %w", err)
	}

	cpRows, err := q.Query(`SELECT config_dir, cwd, claude_session_id, last_seq,
		newest_clear_or_compact_seq, backfill_state FROM conversation_checkpoint`)
	if err != nil {
		logf("registry: READ FAILED for conversation_checkpoint — refusing to serve: %v", err)
		return records, checkpoints, fmt.Errorf("registry: read conversation checkpoints: %w", err)
	}
	defer cpRows.Close()
	for cpRows.Next() {
		var cp ConversationCheckpoint
		if err := cpRows.Scan(&cp.ConfigDir, &cp.CWD, &cp.ClaudeSessionID, &cp.LastSeq,
			&cp.NewestClearOrCompactSeq, &cp.BackfillState); err != nil {
			logf("registry: CORRUPT conversation_checkpoint row — refusing to serve: %v", err)
			return records, checkpoints, fmt.Errorf("registry: scan conversation checkpoint: %w", err)
		}
		if err := mergeCheckpoint(checkpoints, cp); err != nil {
			logf("registry: INVALID conversation checkpoint — refusing to serve: %v", err)
			return records, checkpoints, err
		}
	}
	if err := cpRows.Err(); err != nil {
		logf("registry: READ FAILED iterating conversation_checkpoint — refusing to serve: %v", err)
		return records, checkpoints, fmt.Errorf("registry: iterate conversation checkpoints: %w", err)
	}
	return records, checkpoints, nil
}

// mergeCheckpoint validates cp and folds it into dst, keeping the strongest
// evidence when the same identity arrives twice (the legacy import can carry
// one conversation in both the registry document and its sidecar).
func mergeCheckpoint(dst map[ConversationIdentity]ConversationCheckpoint, cp ConversationCheckpoint) error {
	id := cp.ConversationIdentity
	if id.CWD == "" || id.ClaudeSessionID == "" {
		return fmt.Errorf("registry: invalid conversation checkpoint: config_dir=%q cwd=%q claude_session_id=%q",
			id.ConfigDir, id.CWD, id.ClaudeSessionID)
	}
	if !validBackfill(cp.BackfillState) {
		return fmt.Errorf("registry: checkpoint %+v has invalid backfill_state %q", id, cp.BackfillState)
	}
	if prior, ok := dst[id]; ok {
		cp.LastSeq = max(cp.LastSeq, prior.LastSeq)
		cp.NewestClearOrCompactSeq = max(cp.NewestClearOrCompactSeq, prior.NewestClearOrCompactSeq)
		cp.BackfillState = strongerBackfill(prior.BackfillState, cp.BackfillState)
	}
	dst[id] = cp
	return nil
}

// execer is the write half, satisfied by *sql.Tx (every write is in one).
type execer interface {
	Exec(query string, args ...any) (sql.Result, error)
}

// saveState rewrites both tables from state, inside the caller's transaction.
// A whole rewrite rather than a diff: the maintenance pass can hydrate,
// create and prune in one go, and a rewrite is how "the new state or the old
// one, never a blend" stays true by construction.
func saveState(tx execer, state *registryState) error {
	if _, err := tx.Exec(`DELETE FROM session_record`); err != nil {
		return fmt.Errorf("registry: clear session records: %w", err)
	}
	for _, rec := range state.records {
		queued, err := encodeQueuedPrompts(rec.QueuedPrompts)
		if err != nil {
			return err
		}
		if _, err := tx.Exec(`INSERT INTO session_record(session_id, cwd, model, permission_mode,
			config_dir, claude_session_id, created_at, terminal, death_reason, terminal_at,
			last_seq, newest_clear_or_compact_seq, backfill_state, queued_prompts,
			last_turn_end_ms, hibernated, hibernation_cause, hibernated_since_ms,
			hibernation_cutoff_ms, hibernation_elapsed_ms, hibernation_ttl_ms,
			rewind_previous_vendor_session_id, rewind_retained_leaf_uuid, rewind_dropped_turn_ids,
			death_resolved_at_ms)
			VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
			rec.SessionID, rec.CWD, rec.Model, rec.PermissionMode, rec.ConfigDir,
			rec.ClaudeSessionID, rec.CreatedAt, rec.Terminal, rec.DeathReason, rec.TerminalAt,
			int64(rec.LastSeq), int64(rec.NewestClearOrCompactSeq), rec.BackfillState, queued,
			rec.LastTurnEndMs, rec.Hibernated, rec.Hibernation.Cause, rec.Hibernation.SinceMs,
			rec.Hibernation.CutoffMs, rec.Hibernation.ElapsedMs, rec.Hibernation.TTLMs,
			rec.Rewind.PreviousVendorSessionID, rec.Rewind.RetainedLeafUUID, rec.Rewind.DroppedTurnIDs,
			rec.DeathResolvedAtMs,
		); err != nil {
			return fmt.Errorf("registry: write session record %s: %w", rec.SessionID, err)
		}
	}

	if _, err := tx.Exec(`DELETE FROM conversation_checkpoint`); err != nil {
		return fmt.Errorf("registry: clear conversation checkpoints: %w", err)
	}
	for id, cp := range state.checkpoints {
		if _, err := tx.Exec(`INSERT INTO conversation_checkpoint(config_dir, cwd, claude_session_id,
			last_seq, newest_clear_or_compact_seq, backfill_state) VALUES (?,?,?,?,?,?)`,
			id.ConfigDir, id.CWD, id.ClaudeSessionID,
			int64(cp.LastSeq), int64(cp.NewestClearOrCompactSeq), cp.BackfillState,
		); err != nil {
			return fmt.Errorf("registry: write conversation checkpoint %+v: %w", id, err)
		}
	}
	return nil
}
