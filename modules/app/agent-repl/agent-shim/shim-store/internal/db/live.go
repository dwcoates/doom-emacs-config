package db

import (
	"context"
	"database/sql"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

// sessionLineageCTE is THE ONE PLACE the store decides which agents belong to
// a session. Every scoped read binds the session's main agent id as its single
// argument and selects from `lineage`, so no second query can disagree with it
// about ownership.
//
// THE STORE IS SHARED BY EVERY SESSION ON THE HOST, so "open in the record" is
// not "open in this session". The lineage is the main agent itself plus every
// agent reached from it transitively through the declared spawn columns:
//
//   - `agent.spawned_by_agent` — a subagent (at any depth) names its spawner;
//   - `agent.spawned_by_workflow` — an agent a workflow spawned names that
//     workflow, which reaches the lineage through the workflow's own declared
//     owner: the `workflow` row's `spawner_agent`, or the workflow's
//     `detached_work` join row's `owner_agent`.
//
// NOTHING IS INFERRED. A row whose spawn or owner column is NULL is reached by
// no lineage at all; it is not guessed into one (see liveWorkGaps).
//
// UNION, not UNION ALL, so a cycle in corrupt data terminates rather than
// recursing forever.
//
// EVERY JOIN COLUMN HERE IS INDEXED (lineageIndexes in db.go) — an
// optimization: without them each run built four automatic indexes.
const sessionLineageCTE = `WITH RECURSIVE lineage(agent_id) AS (
    SELECT ?
    UNION
    SELECT a.agent_id FROM agent a JOIN lineage l ON a.spawned_by_agent = l.agent_id
    UNION
    SELECT a.agent_id FROM agent a
      JOIN workflow f ON a.spawned_by_workflow = f.run_agent_id
      JOIN lineage l ON f.spawner_agent = l.agent_id
    UNION
    SELECT a.agent_id FROM agent a
      JOIN detached_work w ON a.spawned_by_workflow = w.work_id
      JOIN lineage l ON w.owner_agent = l.agent_id
  )`

// THE LIVE-WORK STATEMENTS ARE AT PACKAGE SCOPE so the suite EXPLAINs the
// production text itself (live_test.go) rather than a copy that can drift.
//
// `lineage l CROSS JOIN <table>` STATES THE JOIN ORDER, AS AN OPTIMIZATION.
// The lineage is a handful of ids; driven from it, each row is one seek on the
// table's own key (agent) or on detached_work_owner_agent. Written the other
// way round the planner put the table outermost and built an AUTOMATIC index
// over the materialized lineage on every run — CROSS JOIN is SQLite's
// documented way to pin the order so a row estimate cannot flip it back.
const (
	// liveAgentsSQL binds (session, session): the lineage root, then the main
	// agent excluded from the listing.
	liveAgentsSQL = sessionLineageCTE + `
	  SELECT a.agent_id FROM lineage l CROSS JOIN agent a ON a.agent_id = l.agent_id
	  WHERE a.ended_at_ms IS NULL AND a.agent_id != ?
	  ORDER BY a.started_at_ms ASC, a.agent_id ASC`

	// liveDetachedSQL binds (session, the workflow kind excluded).
	liveDetachedSQL = sessionLineageCTE + `
	  SELECT w.work_id FROM lineage l CROSS JOIN detached_work w ON w.owner_agent = l.agent_id
	  WHERE w.ended_at_ms IS NULL AND w.kind != ?
	  ORDER BY w.announced_at_ms ASC, w.work_id ASC`

	// liveWorkGapsSQL binds (the workflow kind excluded). See liveWorkGaps.
	liveWorkGapsSQL = `
	  SELECT 'detached_work:' || work_id FROM detached_work
	  WHERE ended_at_ms IS NULL AND kind != ? AND owner_agent IS NULL
	  UNION ALL
	  SELECT 'agent:' || a.agent_id FROM agent a
	  WHERE a.ended_at_ms IS NULL AND (
	    (a.spawned_by_agent IS NOT NULL
	      AND NOT EXISTS (SELECT 1 FROM agent p WHERE p.agent_id = a.spawned_by_agent))
	    OR (a.spawned_by_workflow IS NOT NULL
	      AND NOT EXISTS (SELECT 1 FROM workflow f WHERE f.run_agent_id = a.spawned_by_workflow)
	      AND NOT EXISTS (SELECT 1 FROM detached_work w WHERE w.work_id = a.spawned_by_workflow)))
	  ORDER BY 1`
)

// LiveWork answers ONE SESSION'S open obligations: everything the record says
// started and holds no terminal for, within the lineage of `session` — the
// caller's main agent.
//
// IT IS A CLAIM ABOUT THE RECORD, NOT ABOUT THE WORLD — "a start was written and
// no terminal ever was" — which is timeless and cannot go stale. The shim
// resolves each item at session start, re-adopting what survived and writing the
// closing terminal for what did not, so the set shrinks to empty either way.
//
// IT IS NEVER ANSWERED UNSCOPED. The shim writes a closing terminal for every
// item its own vendor does not hold, so an answer spanning sessions made one
// session's start close another session's running work (2026-09-23: opening a
// workspace reaped five running subagents of another). An empty session is
// refused before any statement runs.
func (d *DB) LiveWork(ctx context.Context, session string) (*storev1.GetLiveWorkSuccess, error) {
	base := logging.Fields{Operation: "store.db.live-work", Table: "agent", AgentID: session}
	if session == "" {
		return nil, d.refuse(base, invalidSitef(SiteSessionEmpty, "session", "session: an unscoped live-work read would hand one session every other session's obligations to close"))
	}
	started := d.mono()
	success := &storev1.GetLiveWorkSuccess{}

	// THE MAIN AGENT IS NEVER LISTED. A main agent's liveness is the SESSION's
	// liveness, which the shim knows without asking; listing it would hand the
	// shim an obligation to resolve against itself. It is the lineage's root and
	// the one member excluded here.
	//
	// THE ORDER IS THE STORE'S OWN. `started_at_ms` is written from the store's
	// clock and from nowhere else (internal/db/lifecycle.go), so this compares
	// two instants taken by ONE process at ONE point — its own write
	// transaction. It once also held a producer's instant, which made the
	// ordering a comparison across the store's, the shim's and the vendor's
	// clocks; the agent id breaks a tie so the listing is fixed either way.
	agents, err := d.scanStrings(ctx, liveAgentsSQL, session, session)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "scanning live agents"))
	}
	for _, id := range agents {
		success.LiveAgents = append(success.LiveAgents, &conversationv1.AgentId{Value: id})
	}

	// live_workflows stays EMPTY this wave: nothing is routed into the workflow
	// table, so a non-empty answer could only be invented.

	if err := d.liveWorkGaps(ctx, base); err != nil {
		return nil, err
	}

	detached, err := d.scanStrings(ctx, liveDetachedSQL, session, detachedKindWorkflow)
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

// liveWorkGaps records every OPEN obligation that NO session's lineage can
// reach — the invariant gap a scoped read cannot answer for.
//
// Such a row is excluded from every session's answer, because putting it in one
// would be a guess about whose it is, and the guess is exactly the defect the
// scoping exists to remove. But an open obligation nobody is asked to resolve
// never gets its terminal, so the gap is not silent: it is written at ERROR
// through the store's canonical logger, naming every row.
//
// Two shapes are unreachable by construction:
//
//   - a live `detached_work` row (not a workflow, which this wave serves
//     nowhere) whose `owner_agent` is NULL;
//   - a live agent whose spawn column names a spawner the record does not
//     hold — a `spawned_by_agent` with no agent row, or a `spawned_by_workflow`
//     naming neither a workflow row nor a detached-work row.
//
// A live agent with NEITHER spawn column is indistinguishable from another
// session's main agent, which is legitimately rootless, so it is not a gap the
// store can name.
func (d *DB) liveWorkGaps(ctx context.Context, base logging.Fields) error {
	gaps, err := d.scanStrings(ctx, liveWorkGapsSQL, detachedKindWorkflow)
	if err != nil {
		return d.refuse(base, storagef(err, "scanning live work no session's lineage reaches"))
	}
	if len(gaps) == 0 {
		d.log.LogVerbose(base, "every open obligation in the record is reachable from some session's lineage")
		return nil
	}
	fields := base
	fields.Operation = "store.db.live-work.unscoped"
	fields.Level = "error"
	d.log.Log(fields, "invariant gap: %d open obligation(s) name no owner any session's lineage reaches, so no session is asked to resolve them: %s",
		len(gaps), strings.Join(gaps, ", "))
	return nil
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
	started := d.mono()

	// THE CONVERSION IS LEFT-JOINED, NOT REQUIRED. A cursor written before
	// conversion versions existed has no cursor_conversion row, and is served
	// with the conversion UNSET — which the reader reads as version 0: every
	// byte below it was converted by a conversion older than any current one.
	querySQL := cursorsSQL
	var args []any
	if fileID != nil {
		querySQL += ` WHERE c.file_id = ?`
		args = append(args, *fileID)
	}
	querySQL += ` ORDER BY c.file_id ASC`

	rows, err := d.read.QueryContext(ctx, querySQL, args...)
	if err != nil {
		return nil, d.refuse(base, storagef(err, "reading cursors"))
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read

	var out []*storev1.CursorState
	for rows.Next() {
		c := &storev1.CursorState{}
		var carry []byte
		var version, through sql.NullInt64
		if err := rows.Scan(&c.FileId, &c.Path, &c.Offset, &carry, &version, &through); err != nil {
			return nil, d.refuse(base, storagef(err, "scanning a cursor row"))
		}
		c.Carry = carry
		c.Conversion = cursorConversion(version, through)
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

// cursorsSQL is the cursor listing with its conversion bookkeeping. The WHERE
// and ORDER BY clauses are appended by Cursors.
const cursorsSQL = `SELECT c.file_id, c.path, c.offset, c.carry, v.version, v.healing_through
  FROM cursor c LEFT JOIN cursor_conversion v ON v.file_id = c.file_id`

// cursorConversion rebuilds a cursor's CursorConversion from its row, or nil
// for a cursor stored before conversion versions existed.
func cursorConversion(version, through sql.NullInt64) *storev1.CursorConversion {
	if !version.Valid {
		return nil
	}
	conv := &storev1.CursorConversion{Version: uint32(version.Int64)}
	if through.Valid {
		conv.State = &storev1.CursorConversion_Healing{Healing: &storev1.CursorConversionHealing{Through: through.Int64}}
	} else {
		conv.State = &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}}
	}
	return conv
}

func (d *DB) scanStrings(ctx context.Context, query string, args ...any) ([]string, error) {
	rows, err := d.read.QueryContext(ctx, query, args...)
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
