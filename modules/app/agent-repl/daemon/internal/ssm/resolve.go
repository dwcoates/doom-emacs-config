package ssm

import (
	"database/sql"
	"fmt"

	"claude-repld/internal/dlog"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// Signal tokens stored in workspace_state.state. These are a SUPERSET of
// the RenderState vocabulary: the render tokens plus clearing/counter
// signals (merge_none, degraded_clear, task_started, task_ended) that the
// resolution query interprets but never emits as a resolved state.
const (
	sigInit       = "init"
	sigThinking   = "thinking"
	sigDone       = "done"
	sigPermission = "permission"
	sigIdle       = "idle"
	sigReady      = "ready"
	sigIdleAsync  = "idle_async" // DERIVED at resolve time; never stored.
	// An AGENT-axis token: it reports HOW the last turn ended, exactly as
	// `done` reports that it ended cleanly. It is not a latch and has no
	// clearing token — the next agent-axis row supersedes it.
	sigVendorBlocked = "vendor_blocked"
	// Another AGENT-axis turn outcome (I1): the last turn ended because a
	// user-commanded stop was DELIVERED (the shim's ack said INTERRUPTED).
	// Modeled exactly as `done` and `vendor_blocked` are — one row naming
	// how the turn ended, no clearing token, superseded by the next
	// agent-axis row — so there is nothing to latch and nothing to release.
	sigInterrupted = "interrupted"
	// THE TWO CONTEXT CUTS, each its own transient axis. They are not agent
	// states: the agent axis keeps saying whatever it was saying (usually
	// `thinking`, since a cut runs inside a turn), and these ride above it to
	// say what the agent is busy WITH. Each has a clearing token, because
	// each is a window that opens and must be closed by something.
	sigClearing   = "clearing"
	sigCleared    = "cleared" // clearing axis closed.
	sigCompacting = "compacting"
	sigCompacted  = "compacted" // compacting axis closed.
	sigUnpainted  = "unpainted" // no frontend has attested painting.
	sigPainted    = "painted"   // paint axis cleared (attested).
	// A transcript the sidecar could not fully read: the history is
	// incomplete, so anything painted from it is a partial account.
	sigBackfillFailed = "backfill_failed"
	sigBackfillOK     = "backfill_ok" // backfill axis cleared (done, or nothing to do).
	sigDead           = "dead"
	sigMerging        = "merging"
	sigMergeQueued    = "merge_queued"
	sigMergeConflict  = "merge_conflict"
	sigMergeFailed    = "merge_failed"
	sigMerged         = "merged"
	sigMergeNone      = "merge_none" // merge axis cleared (no merge in flight).
	sigDegraded       = "degraded"
	sigDegradedClear  = "degraded_clear" // degraded axis cleared (recovered).
	sigTaskStarted    = "task_started"
	sigTaskEnded      = "task_ended"
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
	causePaintAck        = "paint_ack"
	causePaintLost       = "paint_lost"
	causeVendorBlocked   = "vendor_blocked"
	causeInterrupted     = "interrupted"
	// The vendor retired one session uuid mid-stream and minted another, so
	// the turn the old identity was running can never report its own end. The
	// row is daemon-local and carries no store seq, exactly as a merge
	// transition does. See Manager.ApplySessionRotated.
	causeSessionRotated = "session_rotated"
	// The two context cuts. The detail after the colon names the edge that
	// moved the axis, which is what makes a wedged or expired cut readable in
	// the log rather than a bare token change.
	causeClearing   = "clearing"
	causeCompacting = "compacting"
)

// vendorBlockingStopReasons are the TurnEnded stop reasons that conclude a
// turn abnormally — the whole family VENDOR_BLOCKED covers.
//
// Every one of them ends the turn in a way the agent cannot resolve by
// trying again: the vendor refused, the account is out, or a limit the user
// set was reached. `aborted` is deliberately absent — a user-initiated
// interrupt is a normal conclusion the user themselves asked for, so it
// settles green.
var vendorBlockingStopReasons = map[string]bool{
	"error_max_turns":        true,
	"error_max_budget":       true,
	"error_during_execution": true,
	"refusal":                true,
	"authentication_failed":  true,
	"billing_error":          true,
	"invalid_request":        true,
	"server_error":           true,
}

// VendorBlockingTurnEnd reports whether a TurnEnded concludes the turn in a
// way only a human or the vendor can release.
//
// Only turn-CONCLUDING events reach here. A tool returning non-zero, or an
// API call the SDK retries past, is not a conclusion: the turn is still in
// flight, so the workspace stays red and nothing here fires.
//
// An `is_error` end whose stop reason we do not recognize is still an
// abnormal conclusion and still blocks. Mislabeling a blocked session as
// ready is the failure this whole vocabulary exists to prevent, so the
// unrecognized case resolves toward the safe answer rather than the
// convenient one.
func VendorBlockingTurnEnd(stopReason string, isError bool) bool {
	if vendorBlockingStopReasons[stopReason] {
		return true
	}
	return isError && stopReason != "aborted"
}

// allowedRateLimitStatuses are the rate-limit statuses that report
// utilization on a request the API ALLOWED.
//
// They are standing telemetry riding a normal response, not a refusal: an
// `allowed_warning` (rate_limit_type=overage) says "you are deep into your
// allowance", and the request carrying it SUCCEEDED. Opening a blocked
// window on one paints a working session as stopped and puts "rate limited"
// in the footer of a turn that answered.
var allowedRateLimitStatuses = map[string]bool{
	"allowed":         true,
	"allowed_warning": true,
}

// VendorBlockingRateLimit reports whether a rate-limit STATUS blocks. An
// empty status is treated as blocking: a rate-limit signal naming no status
// is the pre-status shape, where the only reason the signal existed was a
// refusal.
func VendorBlockingRateLimit(status string) bool {
	return !allowedRateLimitStatuses[status]
}

// greenTokens are the agent-axis tokens that resolve GREEN: the route is
// proven usable and there is no foreground turn. `permission` is green
// because a pending permission means the agent is READY for the user to
// view the response and answer it, not that anything is wrong.
// `interrupted` is green for the same reason `done` is: the turn is over and
// the route is proven usable. The user asked for the turn to stop and it
// stopped, which is a completed request, not a fault.
var greenTokens = map[string]bool{
	sigIdle:        true,
	sigReady:       true,
	sigDone:        true,
	sigInterrupted: true,
	sigPermission:  true,
}

// isGreenToken reports whether a resolved token is one of the green states,
// which is what makes it eligible for promotion to yellow when detached
// work is live.
func isGreenToken(token string) bool { return greenTokens[token] }

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
	case sigClearing:
		return frontendv1.RenderState_RENDER_STATE_CLEARING
	case sigCompacting:
		return frontendv1.RenderState_RENDER_STATE_COMPACTING
	case sigPermission:
		return frontendv1.RenderState_RENDER_STATE_PERMISSION
	case sigDone:
		return frontendv1.RenderState_RENDER_STATE_DONE
	case sigInterrupted:
		return frontendv1.RenderState_RENDER_STATE_INTERRUPTED
	case sigReady:
		return frontendv1.RenderState_RENDER_STATE_READY
	case sigVendorBlocked:
		return frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED
	case sigBackfillFailed:
		// Blue, like every other compromised route: an incomplete history
		// cannot be the basis of a "ready" claim.
		return frontendv1.RenderState_RENDER_STATE_INIT
	case sigUnpainted:
		// An unattested route is BLUE, and INIT is blue's token. A frontend
		// that never reported painting is indistinguishable from one that
		// cannot paint, so the honest answer is the compromised-route state
		// rather than a color of its own.
		return frontendv1.RenderState_RENDER_STATE_INIT
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
// editing this query, never a Go cond-ladder.
//
// THE FIVE-COLOR PRECEDENCE is blue > purple > red > yellow > green, and the
// ranks below are that order made executable. Each color is a strictly
// stronger claim about what the user CANNOT do than the one beneath it, so
// the strongest true claim wins:
//
//   - BLUE outranks everything, INCLUDING a live turn. A turn running behind
//     a route the user cannot see is not something to advertise as working,
//     it is something to advertise as broken.
//   - PURPLE outranks red for the mirror of that reason, but only ever
//     among rows of the SAME vintage: `vendor_blocked` is an AGENT-axis
//     turn OUTCOME, so it competes with the non-agent axes at rank 20 and
//     is superseded outright by whatever the agent does next. A `thinking`
//     row appended after it means a new turn is genuinely running, and red
//     is then the true answer.
//   - YELLOW sits between red and green because "something is still running"
//     is a weaker claim than "a turn is running" and a stronger one than
//     "nothing is running".
//
// Merge states keep their place ABOVE the color ladder: they are workflow
// actionability, not agent liveness, and they carry badges/glyphs rather
// than colors so they never spend one of the five.
//
//	 1 merge_conflict   most actionable
//	 2 merge_failed
//	 3 merged           (unguarded: a merged workspace leaves the tab-bar)
//	 4 merging
//	 5 merge_queued
//	--- blue ---------------------------------------------------------------
//	10 dead             the shim is gone
//	11 degraded         a store/transport outage
//	12 unpainted        no frontend has attested painting the history
//	13 backfill_failed  the transcript could not be fully read
//	14 init             bring-up
//	--- purple -------------------------------------------------------------
//	20 vendor_blocked   agent axis: the last turn ended AT THE VENDOR
//	--- red ----------------------------------------------------------------
//	28 clearing         the agent is discarding the context
//	29 compacting       the agent is summarizing the context
//	30 thinking
//
// The two CONTEXT CUTS outrank `thinking` because they are the more specific
// true statement: a turn is indeed in flight, and what it is doing is not
// producing the answer. All three are red — the claim about what the user
// cannot do is identical — so this ordering only decides the phase WORD.
//
// CLEARING outranks COMPACTING because a clear is USER-INITIATED and a
// compaction can be the vendor's own automatic one. When both are somehow
// open, reporting the edge the user asked for is the more useful answer, and
// a clear's rotation is about to invalidate the compaction's window anyway.
//
//	--- yellow -------------------------------------------------------------
//	   idle_async       DERIVED from live_task_count below, never stored
//	--- green --------------------------------------------------------------
//	40 permission       green: the agent is ready for you to view the answer
//	41 done
//	42 interrupted      agent axis: the last turn ended BECAUSE YOU STOPPED IT
//	43 ready
//	44 idle
//
// `merged` is no longer guarded on the agent axis. A merged workspace leaves
// the tab-bar entirely the moment the merge lands, so there is no tab for a
// live agent state to compete for, and the guard existed only to keep that
// tab from flashing during the async teardown.
//
// Each axis contributes its LATEST row (by `at`) as a candidate; the winner
// is the min-rank candidate.
const resolveQuery = `
WITH
  ws(w) AS (SELECT ?),
  prec(state, axis, rank) AS (VALUES
    ('merge_conflict','merge',1),
    ('merge_failed','merge',2),
    ('merged','merge',3),
    ('merging','merge',4),
    ('merge_queued','merge',5),
    ('dead','agent',10),
    ('degraded','degraded',11),
    ('unpainted','paint',12),
    ('backfill_failed','backfill',13),
    ('init','agent',14),
    ('vendor_blocked','agent',20),
    ('clearing','clearing',28),
    ('compacting','compacting',29),
    ('thinking','agent',30),
    ('permission','agent',40),
    ('done','agent',41),
    ('interrupted','agent',42),
    ('ready','agent',43),
    ('idle','agent',44)
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
  latest_paint AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('unpainted','painted')
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_backfill AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('backfill_failed','backfill_ok')
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_clearing AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('clearing','cleared')
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_compacting AS (
    SELECT r.* FROM rows r
    WHERE r.state IN ('compacting','compacted')
    ORDER BY r.at DESC LIMIT 1
  ),
  candidates AS (
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_agent
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_merge WHERE state <> 'merge_none'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_degraded WHERE state <> 'degraded_clear'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_paint WHERE state <> 'painted'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_backfill WHERE state <> 'backfill_ok'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_clearing WHERE state <> 'cleared'
    UNION ALL
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_compacting WHERE state <> 'compacted'
  ),
  ranked AS (
    SELECT c.state, c.cause_kind, c.cause_seq, c.at, c.session_id, p.rank
    FROM candidates c JOIN prec p ON p.state = c.state
  )
SELECT
  w.state, w.cause_kind, w.cause_seq, w.at,
  COALESCE(
    (SELECT COUNT(*) FROM (
      SELECT DISTINCT task_id AS task_key
      FROM rows WHERE state = 'task_started' AND task_id IS NOT NULL
      EXCEPT
      SELECT DISTINCT task_id AS task_key
      FROM rows WHERE state = 'task_ended' AND task_id IS NOT NULL
    ))
    + MAX(
        (SELECT COUNT(*) FROM rows WHERE state = 'task_started' AND task_id IS NULL)
        - (SELECT COUNT(*) FROM rows WHERE state = 'task_ended' AND task_id IS NULL),
        0),
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

	winner := token.String
	// idle_async is derived, not stored: a GREEN winner with live background
	// tasks surfaces the yellow idle_async state instead.
	//
	// Every green token qualifies, not just `idle`. Yellow's claim is "no
	// foreground turn, but detached work continues", and that is equally
	// true of a session that just finished a turn (`done`), one that came up
	// and was never prompted (`ready`), and one awaiting a permission answer
	// (`permission`) — each of those can have spawned background tasks that
	// outlive the turn. Deriving it from `idle` alone left a workspace with
	// live background work painting flat green for the whole window between
	// the turn ending and the session going idle.
	if taskCount > 0 && isGreenToken(winner) {
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
