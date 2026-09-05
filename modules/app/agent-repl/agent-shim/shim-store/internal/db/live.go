package db

import (
	"context"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// LiveWork answers the open obligations: everything the record says started and
// holds no terminal for.
//
// IT IS A CLAIM ABOUT THE RECORD, NOT ABOUT THE WORLD — "a start was written and
// no terminal ever was" — which is timeless and cannot go stale. The shim
// resolves each item at session start, re-adopting what survived and writing the
// closing terminal for what did not, so the set shrinks to empty either way.
func (d *DB) LiveWork(ctx context.Context) (*storev1.GetLiveWorkSuccess, error) {
	base := logging.Fields{Operation: "store.db.live-work", Table: "agent"}
	started := time.Now()
	success := &storev1.GetLiveWorkSuccess{}

	// MAIN AGENTS ARE NEVER LISTED. A main agent's liveness is the SESSION's
	// liveness, which the shim knows without asking; listing it would hand the
	// shim an obligation to resolve against itself. The filter is the spawn
	// columns: a main agent is the one row with neither.
	//
	// THE ORDER IS THE STORE'S OWN. `started_at_ms` is written from the store's
	// clock and from nowhere else (internal/db/lifecycle.go), so this compares
	// two instants taken by ONE process at ONE point — its own write
	// transaction. It once also held a producer's instant, which made the
	// ordering a comparison across the store's, the shim's and the vendor's
	// clocks; the agent id breaks a tie so the listing is fixed either way.
	const agentsSQL = `SELECT agent_id FROM agent
	  WHERE ended_at_ms IS NULL
	    AND (spawned_by_agent IS NOT NULL OR spawned_by_workflow IS NOT NULL)
	  ORDER BY started_at_ms ASC, agent_id ASC`
	agents, err := d.scanStrings(ctx, agentsSQL)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "scanning live agents"))
	}
	for _, id := range agents {
		success.LiveAgents = append(success.LiveAgents, &conversationv1.AgentId{Value: id})
	}

	// live_workflows stays EMPTY this wave: nothing is routed into the workflow
	// table, so a non-empty answer could only be invented.

	const detachedSQL = `SELECT work_id FROM detached_work
	  WHERE ended_at_ms IS NULL AND kind != ?
	  ORDER BY announced_at_ms ASC, work_id ASC`
	detached, err := d.scanStrings(ctx, detachedSQL, detachedKindWorkflow)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "scanning live detached work"))
	}
	for _, id := range detached {
		success.LiveDetached = append(success.LiveDetached, &conversationv1.DetachedWorkId{Value: id})
	}

	d.observeQuery(StatementLiveWork, "agent", base, started, int64(len(agents)+len(detached)))
	d.traceStatement(ctx, StatementLiveWork, "agent", base, int64(len(agents)+len(detached)))
	d.log.LogVerbose(base, "live work read agents=%d workflows=%d detached=%d",
		len(success.LiveAgents), len(success.LiveWorkflows), len(success.LiveDetached))
	return success, nil
}

// Cursors answers the sidecar's startup recovery: every persisted cursor, or
// one file's. An EMPTY answer is legitimate — a fresh store has no cursors and
// the sidecar starts every file from zero.
func (d *DB) Cursors(ctx context.Context, fileID *string) ([]*storev1.CursorState, error) {
	base := logging.Fields{Operation: "store.db.cursors", Table: "cursor"}
	if fileID != nil {
		if *fileID == "" {
			return nil, d.refuse(base, invalidFieldf("file_id", "file_id is present with an empty value — asking for every cursor is expressed by absence, never by an empty identifier"))
		}
		base.FileID = *fileID
	}
	started := time.Now()

	querySQL := `SELECT file_id, path, offset, carry FROM cursor`
	var args []any
	if fileID != nil {
		querySQL += ` WHERE file_id = ?`
		args = append(args, *fileID)
	}
	querySQL += ` ORDER BY file_id ASC`

	rows, err := d.sql.QueryContext(ctx, querySQL, args...)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading cursors"))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var out []*storev1.CursorState
	for rows.Next() {
		c := &storev1.CursorState{}
		var carry []byte
		if err := rows.Scan(&c.FileId, &c.Path, &c.Offset, &carry); err != nil {
			return nil, d.refuse(base, storagef(err, "scanning a cursor row"))
		}
		c.Carry = carry
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, d.refuse(base, storagef(err, "iterating cursor rows"))
	}
	d.observeQuery(StatementListCursors, "cursor", base, started, int64(len(out)))
	d.traceStatement(ctx, StatementListCursors, "cursor", base, int64(len(out)))
	d.log.LogVerbose(base, "cursors read cursors=%d one_file=%t", len(out), fileID != nil)
	return out, nil
}

func (d *DB) scanStrings(ctx context.Context, query string, args ...any) ([]string, error) {
	rows, err := d.sql.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var out []string
	for rows.Next() {
		var value string
		if err := rows.Scan(&value); err != nil {
			return nil, err
		}
		out = append(out, value)
	}
	return out, rows.Err()
}
