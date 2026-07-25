package ssm

import (
	"database/sql"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"

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
// unmatchedTaskEndsQuery names the tasks whose `task_ended` row has no
// `task_started` anywhere in the workspace's log — the only way the started
// minus ended difference can come out negative. Used ONLY to make the
// impossible-state log say WHICH task caused it.
const unmatchedTaskEndsQuery = `
SELECT DISTINCT COALESCE(e.task_id, 'row:' || e.at)
FROM workspace_state e
WHERE e.workspace = ? AND e.state = 'task_ended'
  AND NOT EXISTS (
    SELECT 1 FROM workspace_state s
    WHERE s.workspace = e.workspace AND s.state = 'task_started'
      AND COALESCE(s.task_id, 'row:' || s.at) = COALESCE(e.task_id, 'row:' || e.at)
  )
ORDER BY 1
`

// unmatchedTaskEnds lists the offending task ids for the negative-count log. A
// failure here degrades the LOG LINE only (the clamp still happens and is still
// logged), so it reports the lookup error in place of the ids rather than
// swallowing it or aborting the resolution.
func unmatchedTaskEnds(db *sql.DB, workspace string) string {
	rows, err := db.Query(unmatchedTaskEndsQuery, workspace)
	if err != nil {
		return fmt.Sprintf("<task-id lookup failed: %v>", err)
	}
	defer rows.Close()
	var ids []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return fmt.Sprintf("<task-id scan failed: %v>", err)
		}
		ids = append(ids, id)
	}
	if err := rows.Err(); err != nil {
		return fmt.Sprintf("<task-id iteration failed: %v>", err)
	}
	if len(ids) == 0 {
		return "<none found>"
	}
	return strings.Join(ids, ",")
}

func resolve(db *sql.DB, workspace string, logf dlog.Logf) (resolved, error) {
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

	// live_task_count is started-minus-ended over DISTINCT task ids, so it can
	// only go negative when a `task_ended` arrives for a task whose
	// `task_started` was never logged — a real observed possibility (a task
	// begun before this daemon was watching, or a lost start row). A negative
	// count is not a value: it is an IMPOSSIBLE STATE, and clamping it silently
	// would hide the missing start forever. So it is clamped to 0 AND reported
	// loudly, naming the workspace and the exact task ids responsible.
	if taskCount < 0 {
		if logf != nil {
			logf("ssm: IMPOSSIBLE live_task_count=%d ws=%s — task_ended with no logged task_started for task_id(s) %s; clamping to 0",
				taskCount, workspace, unmatchedTaskEnds(db, workspace))
		}
		taskCount = 0
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
