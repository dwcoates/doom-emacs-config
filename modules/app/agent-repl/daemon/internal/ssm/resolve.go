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
	// THE legacy connectivity projection. sessioncontroller produces it off the bring-up gate's own
	// verdict, and it is what makes a workspace's color CONNECTION TRUTH:
	// `wired` clears the axis and lets the session-status lifecycle speak, and neither other
	// token does.
	sigWired    = "wired"    // the bring-up gate CLOSED (ShimReady).
	sigStarting = "starting" // a bring-up is actively in flight.
	// THE AXIS'S CLOSED HALF IS TWO TOKENS, and it used to be one. `dormant`
	// meant both "we deliberately put this session to sleep to reclaim its
	// ~500MB" and "the backend substrate is broken", so the most ordinary event
	// in the system — the idle sweeper reaping a workspace nobody touched for an
	// hour — rendered exactly like a dead shim. A color that fires on both means
	// neither, so the split gives blue back its meaning.
	sigSevered    = "severed"    // not wired, and there is EVIDENCE of breakage.
	sigHibernated = "hibernated" // not wired, and nothing is wrong.
	// LEGACY, AND LOAD-BEARING FOREVER. `workspace_state` is append-only, and
	// rows written before the split literally contain the text `dormant`. It is
	// resolved as an ALIAS for `severed` — it must keep ranking, it must keep
	// closing the axis, and it must never be migrated, rewritten or deleted.
	// Severed is the conservative reading: an old row cannot tell us whether the
	// teardown behind it was benign, and claiming "nothing is wrong" on absent
	// evidence is the one direction that can mislead.
	sigDormantLegacy = "dormant"
	sigThinking      = "thinking"
	// sigSubmitting is the FIRST half of a turn: the daemon has committed to
	// submitting the prompt and the shim has not yet acked it. Same band and
	// same hue as `thinking`, which it exists only to make more precise — see
	// RENDER_STATE_SUBMITTING in frontend.proto for why its closing edge is the
	// ack rather than the durable TurnStarted.
	sigSubmitting = "submitting"
	sigDone       = "done"
	sigPermission = "permission"
	sigIdle       = "idle"
	sigReady      = "ready"
	sigIdleAsync  = "idle_async" // DERIVED at resolve time; never stored.
	// An session-status lifecycle token: it reports HOW the last turn ended, exactly as
	// `done` reports that it ended cleanly. It is not a latch and has no
	// clearing token — the next session-status lifecycle row supersedes it.
	sigVendorBlocked = "vendor_blocked"
	// Another session-status lifecycle turn outcome (I1): the last turn ended because a
	// user-commanded stop was DELIVERED (the shim's ack said INTERRUPTED).
	// Modeled exactly as `done` and `vendor_blocked` are — one row naming
	// how the turn ended, no clearing token, superseded by the next
	// session-status lifecycle row — so there is nothing to latch and nothing to release.
	sigInterrupted = "interrupted"
	// THE TWO CONTEXT CUTS, each its own transient axis. They are not agent
	// states: the session-status lifecycle keeps saying whatever it was saying (usually
	// `thinking`, since a cut runs inside a turn), and these ride above it to
	// say what the agent is busy WITH. Each has a clearing token, because
	// each is a window that opens and must be closed by something.
	sigClearing   = "clearing"
	sigCleared    = "cleared" // clearing axis closed.
	sigCompacting = "compacting"
	sigCompacted  = "compacted" // compacting axis closed.
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
	sigDegradedClear  = "degraded_clear" // legacy impairment projection cleared (recovered).
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
	// The legacy connectivity projection's edges. The detail after the colon names which one moved
	// it — the bring-up that started, the ShimReady that closed the gate, or the
	// exit/hibernation/rotation that took the wiring away.
	causeWired         = "wired"
	causeVendorBlocked = "vendor_blocked"
	causeInterrupted   = "interrupted"
	// Daemon-local ordering edges that make prompt/interrupt UI claims true
	// before their corresponding durable stream events finish traversing the
	// store.  See promptstate.go.
	causePromptAccepted = "prompt_accepted"
	// The shim ACKED the submit, so the turn advances from `submitting` to
	// `thinking`: the agent now genuinely holds the prompt, which is the first
	// moment that word is true. See promptstate.go for why this edge is the ack
	// rather than the durable TurnStarted.
	causePromptDelivered = "prompt_delivered"
	// The retraction of causePromptAccepted. The accepted edge is now written
	// BEFORE the shim's Ack, so a submit the shim refuses (or never receives)
	// leaves a `submitting` row describing a turn that never began. Nothing else
	// can close it — no TurnEnded is coming for a prompt no session ever ran —
	// so the submitter writes the closing row itself. See promptstate.go.
	causePromptRejected           = "prompt_rejected"
	causeInterruptAlreadyComplete = "interrupt_already_complete"
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
	// The permission row's two edges. The detail after the colon names which
	// edge moved it — the first pending request, the answer that emptied the
	// set, or the bounding event (rotation, restart) that released it.
	causePermission = "permission"
	// The daemon STOPPED the shim behind a live turn, or a shim came back
	// reporting no turns at all over a workspace still claiming one. Neither
	// turn can ever report its own end, so the closing row is written by the
	// only party left that knows the turn is over. The detail after the colon
	// names the teardown path or the handshake that produced it. See
	// Manager.CloseStaleTurn.
	causeShimStopped = "shim_stopped"
)

// causeShimHandshake is the reason detail carried by the closing row
// ReconcileTurnHandshake writes: a shim came back reporting no turns at all
// over a workspace whose session-status lifecycle still claimed one.
const causeShimHandshake = "shim_handshake_no_turns"

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

// greenTokens are the session-status lifecycle tokens that resolve GREEN: the route is
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
	case sigStarting:
		return frontendv1.RenderState_RENDER_STATE_INIT
	case sigSevered, sigDormantLegacy:
		// The legacy token resolves HERE and nowhere else. A pre-split row says
		// only "the axis was closed"; it cannot say the teardown was benign, and
		// severed is the reading that does not claim more than the row knows.
		return frontendv1.RenderState_RENDER_STATE_SEVERED
	case sigHibernated:
		return frontendv1.RenderState_RENDER_STATE_HIBERNATED
	case sigIdle:
		return frontendv1.RenderState_RENDER_STATE_IDLE
	case sigIdleAsync:
		return frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC
	case sigSubmitting:
		return frontendv1.RenderState_RENDER_STATE_SUBMITTING
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
// THE SIX-COLOR PRECEDENCE is blue > teal > purple > red > yellow > green, and
// the ranks below are that order made executable. Each color is a strictly
// stronger claim about what the user CANNOT do than the one beneath it, so
// the strongest true claim wins:
//
//   - THE legacy connectivity projection IS WHY BLUE OUTRANKS EVERYTHING. A workspace's color is
//     CONNECTION TRUTH: blue means there is no live backend session for it,
//     and every non-blue color is a guarantee that the substrate is wired.
//     `severed` (12) and `starting` (14) therefore sit above every session-status lifecycle
//     row and above `vendor_blocked`, so the agent's last word is visible only
//     while the axis reads `wired`. Nothing in Go decides this; the ranks do.
//
//   - BLUE outranks everything, INCLUDING a live turn. A turn running behind
//     a route the user cannot see is not something to advertise as working,
//     it is something to advertise as broken.
//
//   - TEAL sits directly beneath blue, and its placement is the load-bearing
//     part of the hibernation split. `hibernated` (15) is not wired either, so
//     it makes the SAME actionability claim blue does — you cannot interact
//     without paying a bring-up — and only its REASON is benign. Ranking it
//     below green, the tempting reading of "benign", would let a stale
//     `thinking` row from the turn a workspace was hibernated after mask a
//     workspace that is genuinely asleep. It ranks BELOW the blue band for the
//     mirror reason: a workspace that is both asleep and degraded has something
//     to act on, and teal says the opposite.
//
//     A consequence worth naming: `hibernated` winning while `turn_active` is
//     true is UNREACHABLE BY CONSTRUCTION, because hibernate() refuses a
//     workspace that is not settled. Where it is detectable it is logged as an
//     INVARIANT VIOLATION, never as expected.
//
//   - PURPLE outranks red for the mirror of that reason, but only ever
//     among rows of the SAME vintage: `vendor_blocked` is an session-status lifecycle
//     turn OUTCOME, so it competes with the non-agent axes at rank 20 and
//     is superseded outright by whatever the agent does next. A `thinking`
//     row appended after it means a new turn is genuinely running, and red
//     is then the true answer.
//
//   - YELLOW sits between red and green because "something is still running"
//     is a weaker claim than "a turn is running" and a stronger one than
//     "nothing is running".
//
// Merge states keep their place ABOVE the color ladder: they are workflow
// actionability, not agent liveness, and they carry badges/glyphs rather
// than colors so they never spend one of the six.
//
//	 1 merge_conflict   most actionable
//	 2 merge_failed
//	 3 merged           (unguarded: a merged workspace leaves the tab-bar)
//	 4 merging
//	 5 merge_queued
//	--- blue ---------------------------------------------------------------
//	10 dead             the shim is gone
//	11 degraded         a store/transport outage
//	12 severed          nothing is wired, and the substrate BROKE
//	12 dormant          the pre-split spelling of severed; a legacy alias
//	13 backfill_failed  the transcript could not be fully read
//	14 starting         a bring-up is in flight
//	--- teal ---------------------------------------------------------------
//	15 hibernated       nothing is wired, and NOTHING IS WRONG
//	--- purple -------------------------------------------------------------
//	20 vendor_blocked   session-status lifecycle: the last turn ended AT THE VENDOR
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
//	42 interrupted      session-status lifecycle: the last turn ended BECAUSE YOU STOPPED IT
//	43 ready
//	44 idle
//
// `merged` is no longer guarded on the session-status lifecycle. A merged workspace leaves
// the tab-bar entirely the moment the merge lands, so there is no tab for a
// live agent state to compete for, and the guard existed only to keep that
// tab from flashing during the async teardown.
//
// Each axis contributes its LATEST row (by `at`) as a candidate; the winner
// is the min-rank candidate.
const resolveQuery = `
WITH
  ws(w, composite_active) AS (SELECT ?, ?),
  prec(state, axis, rank) AS (VALUES
    ('merge_conflict','merge',1),
    ('merge_failed','merge',2),
    ('merged','merge',3),
    ('merging','merge',4),
    ('merge_queued','merge',5),
    ('dead','agent',10),
    ('degraded','degraded',11),
    ('severed','wired',12),
    -- THE LEGACY ALIAS, ranked identically to severed because that is what it
    -- resolves to. workspace_state is append-only and pre-split rows carry the
    -- literal text 'dormant'; dropping this row would silently stop ranking them
    -- and every such workspace would resolve off its stale session-status lifecycle instead.
    ('dormant','wired',12),
    ('backfill_failed','backfill',13),
    ('starting','wired',14),
    -- TEAL, and its rank is the load-bearing part of the hibernation split.
    -- Directly below the blue band and above purple: hibernation makes the SAME
    -- actionability claim blue does — you cannot interact without paying a
    -- bring-up — and only the REASON is benign. Ranked below green instead, a
    -- stale 'thinking' row from the turn a workspace was hibernated after would
    -- mask a workspace that is genuinely asleep.
    ('hibernated','wired',15),
    ('vendor_blocked','agent',20),
    ('clearing','clearing',28),
    ('compacting','compacting',29),
    -- SUBMITTING shares thinking's rank because it shares thinking's band: it
    -- is the same claim about what the user can do, refined by one word. Only
    -- one agent row is ever a candidate, so the tie is never resolved.
    ('submitting','agent',30),
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
      AND (SELECT composite_active FROM ws) = 0
    ORDER BY r.at DESC LIMIT 1
  ),
  latest_wired AS (
    SELECT r.* FROM rows r
    -- 'dormant' STAYS IN THIS LIST FOREVER. It is the pre-split spelling of the
    -- axis's closed half, and dropping it would make a workspace whose newest
    -- wired row is a legacy one look like a workspace with NO wired row at all —
    -- which the synthesized branch below then answers 'hibernated' for, quietly
    -- repainting an old failure benign.
    WHERE r.state IN ('wired','starting','severed','hibernated','dormant')
      AND (SELECT composite_active FROM ws) = 0
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
    SELECT state, cause_kind, cause_seq, at, session_id FROM latest_wired WHERE state <> 'wired'
    UNION ALL
    -- ABSENCE OF A WIRED ROW IS HIBERNATED, not "unknown" and not "severed". A
    -- workspace with agent history and no wired row has no evidence of a live
    -- session behind it, and letting the session-status lifecycle answer unopposed would
    -- advertise an agent nobody is connected to. The synthesized row borrows the
    -- session-status lifecycle's timestamp and session id so the emitted state still names the
    -- conversation it is about.
    --
    -- TEAL, because the absence of a wired row is the absence of EVIDENCE OF
    -- BREAKAGE. Nothing was ever wired to this workspace: no bring-up failed and
    -- no session controller died. It resolved to the old blue dormant before the split, which
    -- meant a workspace whose only sin was never having been opened stood there
    -- accusing the local substrate of being broken.
    SELECT 'hibernated', 'wired:absent', cause_seq, at, session_id FROM latest_agent
      WHERE (SELECT composite_active FROM ws) = 0
        AND NOT EXISTS (SELECT 1 FROM latest_wired)
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
	var compositeActive int
	if err := db.QueryRow(
		`SELECT EXISTS(
			SELECT 1 FROM session_connectivity WHERE workspace = ?
		)`,
		workspace,
	).Scan(&compositeActive); err != nil {
		return resolved{}, fmt.Errorf(
			"ssm: resolve connectivity authority for workspace %q: %w",
			workspace, err)
	}
	err := db.QueryRow(resolveQuery, workspace, compositeActive).Scan(
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

	// The shim-accepted command edge is the first authoritative observation
	// that a turn is beginning; the durable stream TurnStarted later replaces
	// its cause without changing the truth value.
	// EVERY edge that opens a turn counts, and the split into `submitting` and
	// `thinking` added one. Omitting the delivered edge would report no turn
	// active for the whole span between the shim ack and the durable
	// TurnStarted, which is exactly when the agent is working hardest.
	turnActive := agentCause.Valid &&
		(agentCause.String == causeTurnStarted ||
			agentCause.String == causePromptAccepted ||
			agentCause.String == causePromptDelivered)

	// AN INVARIANT VIOLATION, never a benign combination. A hibernation is
	// something WE do on purpose, and sessioncontroller's hibernate() refuses any
	// workspace whose resolved state is not settled — so a workspace reading
	// `hibernated` with a turn still active is a state no code path is supposed to
	// be able to produce.
	//
	// It is worth a line precisely because the rank table makes it INVISIBLE.
	// `hibernated` outranks every agent row deliberately, so that a stale
	// `thinking` cannot mask a workspace that is genuinely asleep — which means
	// this combination paints an ordinary teal tab with nothing anywhere to say
	// the agent is working underneath it. Without this log the only symptom is a
	// user watching a sleeping workspace that will not answer.
	//
	// Logged rather than corrected: the resolution query is the sole authority on
	// precedence, and a Go branch that overrode it here would be a second,
	// invisible precedence rule. The repair belongs at whichever writer produced
	// the impossible pair.
	if winner == sigHibernated && turnActive && logf != nil && compositeActive == 0 {
		logf("ssm: INVARIANT VIOLATION ws=%s resolved hibernated WITH A TURN ACTIVE (cause=%s seq=%d session=%s) — legacy hibernation outranks a live turn without a composite connectivity lifecycle",
			workspace, causeKind.String, causeSeq.Int64, sessionID.String)
	}

	return resolved{
		found:         true,
		state:         renderStateOf(winner),
		turnActive:    turnActive,
		liveTaskCount: taskCount,
		mergePhase:    mergePhase,
		causeKind:     causeKind.String,
		causeSeq:      uint64(causeSeq.Int64),
		atMs:          at.Int64,
		sessionID:     sessionID.String,
	}, nil
}
