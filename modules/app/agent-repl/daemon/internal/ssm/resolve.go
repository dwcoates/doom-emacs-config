package ssm

import (
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Signal tokens stored in workspace_state.state. These are a SUPERSET of
// the RenderState vocabulary: the render tokens plus clearing/counter
// signals (merge_none, degraded_clear, task_started, task_ended) that the
// resolution query interprets but never emits as a resolved state.
const (
	sigInit          = "init"
	sigThinking      = "thinking"
	sigDone          = "done"
	sigPermission    = "permission"
	sigStopFailed    = "stop_failed"
	sigIdle          = "idle"
	sigIdleAsync     = "idle_async" // DERIVED at resolve time; never stored.
	sigDead          = "dead"
	sigMerging       = "merging"
	sigMergeQueued   = "merge_queued"
	sigMergeConflict = "merge_conflict"
	sigMergeFailed   = "merge_failed"
	sigMerged        = "merged"
	sigMergeNone     = "merge_none" // merge axis cleared (no merge in flight).
	sigDegraded      = "degraded"
	sigDegradedClear = "degraded_clear" // degraded axis cleared (recovered).
	sigTaskStarted   = "task_started"
	sigTaskEnded     = "task_ended"
)

// Cause-kind strings recorded per row and surfaced on WorkspaceState.
const (
	causeSessionStarted  = "session_started"
	causeTurnStarted     = "turn_started"
	causeTurnEnded       = "turn_ended"
	causeSessionEnded    = "session_ended"
	causeTaskStarted     = "task_started"
	causeTaskEnded       = "task_ended"
	causeMergeTransition = "merge_transition"
)

// renderStateOf maps a resolved signal token to its RenderState enum.
// idle_async is included because resolve() promotes idle→idle_async before
// calling this.
func renderStateOf(token string) frontendv1.RenderState {
	switch token {
	case sigInit:
		return frontendv1.RenderState_RENDER_STATE_INIT
	case sigIdle:
		return frontendv1.RenderState_RENDER_STATE_IDLE
	case sigIdleAsync:
		return frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC
	case sigThinking:
		return frontendv1.RenderState_RENDER_STATE_THINKING
	case sigPermission:
		return frontendv1.RenderState_RENDER_STATE_PERMISSION
	case sigDone:
		return frontendv1.RenderState_RENDER_STATE_DONE
	case sigStopFailed:
		return frontendv1.RenderState_RENDER_STATE_STOP_FAILED
	case sigMerging:
		return frontendv1.RenderState_RENDER_STATE_MERGING
	case sigMergeQueued:
		return frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED
	case sigMergeConflict:
		return frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT
	case sigMergeFailed:
		return frontendv1.RenderState_RENDER_STATE_MERGE_FAILED
	case sigMerged:
		return frontendv1.RenderState_RENDER_STATE_MERGED
	case sigDead:
		return frontendv1.RenderState_RENDER_STATE_DEAD
	case sigDegraded:
		return frontendv1.RenderState_RENDER_STATE_DEGRADED
	default:
		return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED
	}
}

// resolveQuery is the state resolution over the append-only log. Precedence
// lives ENTIRELY in the `prec` VALUES table below — changing precedence is
// editing this query, never a Go cond-ladder. Ranks are lower-wins and
// mirror the reference semantics of the elisp `agent-repl--ws-render-status`
// (dead > merge states? no: merge_conflict/failed/merged/merging/queued
// dominate dead, which dominates the live agent states, which dominate
// idle_async, which dominates idle):
//
//	1 merge_conflict   most actionable
//	2 merge_failed
//	3 merged           GUARDED: only when the agent axis is absent
//	                   (a merged workspace that resumed work shows its live
//	                    agent state, matching the elisp `(null claude)` guard)
//	4 merging
//	5 merge_queued
//	6 dead
//	7 degraded         (EXTENSION: no elisp analogue; ranked just under dead)
//	8 thinking
//	9 permission
//	10 init
//	11 done
//	12 stop_failed
//	14 idle            (13 idle_async is derived from live_task_count, below)
//
// Each axis contributes its LATEST row (by `at`) as a candidate; the winner
// is the min-rank candidate. The guard drops `merged` when any agent-axis
// row exists.
const resolveQuery = `
WITH
  ws(w) AS (SELECT ?),
  prec(state, axis, rank, req_agent_absent) AS (VALUES
    ('merge_conflict','merge',1,0),
    ('merge_failed','merge',2,0),
    ('merged','merge',3,1),
    ('merging','merge',4,0),
    ('merge_queued','merge',5,0),
    ('dead','agent',6,0),
    ('degraded','degraded',7,0),
    ('thinking','agent',8,0),
    ('permission','agent',9,0),
    ('init','agent',10,0),
    ('done','agent',11,0),
    ('stop_failed','agent',12,0),
    ('idle','agent',14,0)
  ),
  rows AS (
    SELECT workspace, session_id, state, cause_kind, cause_seq, at, task_id
    FROM workspace_state WHERE workspace = (SELECT w FROM ws)
  ),
  latest_agent AS (
    SELECT r.* FROM rows r JOIN prec p ON p.state = r.state AND p.axis = 'agent'
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_merge AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('merging','merge_queued','merge_conflict','merge_failed','merged','merge_none')
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_degraded AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('degraded','degraded_clear')
    ORDER BY r.at DESC LIMIT 1
  ),
  candidates AS (
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_agent
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_merge WHERE state <> 'merge_none'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_degraded WHERE state <> 'degraded_clear'
  ),
  ranked AS (
    SELECT c.state, c.cause_kind, c.cause_seq, c.at, c.session_id, p.rank
    FROM candidates c JOIN prec p ON p.state = c.state
    WHERE p.req_agent_absent = 0 OR NOT EXISTS (SELECT 1 FROM latest_agent)
  )
SELECT
  w.state, w.cause_kind, w.cause_seq, w.at,
  COALESCE(
    (SELECT COUNT(DISTINCT COALESCE(task_id, 'row:' || at)) FROM rows WHERE state = 'task_started')
    - (SELECT COUNT(DISTINCT COALESCE(task_id, 'row:' || at)) FROM rows WHERE state = 'task_ended'),
    0) AS live_task_count,
  (SELECT cause_kind FROM latest_agent) AS agent_cause_kind,
  (SELECT state FROM latest_merge) AS merge_state,
  COALESCE((SELECT session_id FROM latest_agent),
           w.session_id,
           (SELECT session_id FROM latest_merge), '') AS session_id
FROM (SELECT state, cause_kind, cause_seq, at, session_id FROM ranked ORDER BY rank LIMIT 1) w
`

// resolved is the outcome of a resolution: the render state plus the input
// snapshot that produced it (for debuggability, per §5.4 / §9.2).
type resolved struct {
	found         bool
	state         frontendv1.RenderState
	turnActive    bool
	liveTaskCount int64
	mergePhase    string
	causeKind     string
	causeSeq      uint64
	atMs          int64
	sessionID     string
}

// resolve computes the current render-state for a workspace from its log.
// A workspace with no render-bearing candidate (only task counters, or
// nothing at all) resolves found=false — matching the elisp `nil` for an
// unborn/tombstoned workspace, and honoring "idle_async needs an idle base"
// (background tasks alone never synthesize a state).
func resolve(db *sql.DB, workspace string) (resolved, error) {
	var (
		token      sql.NullString
		causeKind  sql.NullString
		causeSeq   sql.NullInt64
		at         sql.NullInt64
		taskCount  int64
		agentCause sql.NullString
		mergeState sql.NullString
		sessionID  sql.NullString
	)
	err := db.QueryRow(resolveQuery, workspace).Scan(
		&token, &causeKind, &causeSeq, &at, &taskCount, &agentCause, &mergeState, &sessionID)
	if err == sql.ErrNoRows || (err == nil && !token.Valid) {
		return resolved{found: false, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}, nil
	}
	if err != nil {
		return resolved{}, fmt.Errorf("ssm: resolve workspace %q: %w", workspace, err)
	}

	winner := token.String
	// idle_async is derived, not stored: an idle winner with live background
	// tasks surfaces the amber idle_async state (elisp `--ws-async-live-p`).
	if winner == sigIdle && taskCount > 0 {
		winner = sigIdleAsync
	}

	mergePhase := ""
	if mergeState.Valid && mergeState.String != sigMergeNone {
		mergePhase = mergeState.String
	}

	return resolved{
		found:         true,
		state:         renderStateOf(winner),
		turnActive:    agentCause.Valid && agentCause.String == causeTurnStarted,
		liveTaskCount: taskCount,
		mergePhase:    mergePhase,
		causeKind:     causeKind.String,
		causeSeq:      uint64(causeSeq.Int64),
		atMs:          at.Int64,
		sessionID:     sessionID.String,
	}, nil
}
