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
			queued_prompts              TEXT    NOT NULL DEFAULT ''
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
		backfill_state, queued_prompts FROM session_record`)
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
			&rec.LastSeq, &rec.NewestClearOrCompactSeq, &rec.BackfillState, &queued); err != nil {
			logf("registry: CORRUPT session_record row — refusing to serve: %v", err)
			return records, checkpoints, fmt.Errorf("registry: scan session record: %w", err)
		}
		if rec.SessionID == "" {
			logf("registry: INVALID record with empty session_id — refusing to serve")
			return records, checkpoints, fmt.Errorf("registry: record with empty session_id")
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
			last_seq, newest_clear_or_compact_seq, backfill_state, queued_prompts)
			VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)`,
			rec.SessionID, rec.CWD, rec.Model, rec.PermissionMode, rec.ConfigDir,
			rec.ClaudeSessionID, rec.CreatedAt, rec.Terminal, rec.DeathReason, rec.TerminalAt,
			int64(rec.LastSeq), int64(rec.NewestClearOrCompactSeq), rec.BackfillState, queued,
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
