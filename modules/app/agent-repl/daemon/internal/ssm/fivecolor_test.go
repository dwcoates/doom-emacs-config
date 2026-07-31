package ssm

import (
	"database/sql"
	"path/filepath"
	"reflect"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// evTurnEndedReason is evTurnEnded with an explicit stop reason, for the
// vendor-block classification.
func evTurnEndedReason(sid string, seq uint64, reason string, isErr bool) *corev1.Event {
	return &corev1.Event{
		SessionId: sid,
		Seq:       seq,
		Plane:     corev1.Plane_PLANE_STREAM,
		Payload: &corev1.Event_TurnEnded{
			TurnEnded: &corev1.TurnEnded{StopReason: reason, IsError: isErr},
		},
	}
}

// ---------------------------------------------------------------------------
// Shim-asserted readiness and the no-regress guard
// ---------------------------------------------------------------------------

// A session that is never prompted must still reach green: that is the whole
// reason readiness moved off the vendor's first-prompt-only system:init.
func TestSessionStartedReachesReadyWithoutAnyPrompt(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act — readiness alone; no prompt is ever submitted.
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}

// THE NO-REGRESS GUARD. A readiness assertion arriving over a live turn must
// not knock it back — the regression observed as a THINKING→IDLE flip.
func TestReadinessDuringActiveTurnDoesNotRegress(t *testing.T) {
	// Arrange — a turn is running.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — a shim relaunch/revive re-asserts readiness underneath it.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert — the running turn is the stronger claim and stands.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING (readiness must not regress a live turn)", renderName(got))
	}
}

func TestSuppressedReadinessIsLoggedNotSilent(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert — a dropped signal must never be invisible.
	if !cl.contains("readiness suppressed") {
		t.Fatal("a dropped readiness signal was not logged")
	}
}

// The guard is scoped to a LIVE turn, not to every prior state: readiness
// arriving after the turn ended must take effect normally.
func TestReadinessAfterTurnEndsApplies(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.Apply(evTurnEnded("s1", 2, false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Act.
	if err := m.Apply(evSessionStarted("s1", 3)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got))
	}
}

// ---------------------------------------------------------------------------
// Vendor/account blocking
// ---------------------------------------------------------------------------

func TestVendorBlockingTurnEnd(t *testing.T) {
	tests := []struct {
		name       string
		stopReason string
		isError    bool
		want       bool
	}{
		{name: "clean success does not block", stopReason: "end_turn", want: false},
		{name: "user interrupt does not block", stopReason: "aborted", want: false},
		{name: "aborted with the error flag still does not block", stopReason: "aborted", isError: true, want: false},
		{name: "max turns blocks", stopReason: "error_max_turns", want: true},
		{name: "max budget blocks", stopReason: "error_max_budget", want: true},
		{name: "execution error blocks", stopReason: "error_during_execution", want: true},
		{name: "model refusal blocks", stopReason: "refusal", want: true},
		{name: "auth failure blocks", stopReason: "authentication_failed", want: true},
		{name: "billing failure blocks", stopReason: "billing_error", want: true},
		{name: "a rejected request blocks", stopReason: "invalid_request", want: true},
		{name: "a server error blocks", stopReason: "server_error", want: true},
		{name: "an unrecognized erroring reason blocks", stopReason: "novel_failure", isError: true, want: true},
		{name: "an unrecognized clean reason does not block", stopReason: "novel_ending", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := VendorBlockingTurnEnd(tc.stopReason, tc.isError)
			// Assert.
			if got != tc.want {
				t.Fatalf("VendorBlockingTurnEnd(%q, %v) = %v, want %v",
					tc.stopReason, tc.isError, got, tc.want)
			}
		})
	}
}

// An allowed_warning rides a request the API ALLOWED, so it must not claim
// the session has stopped.
func TestVendorBlockingRateLimit(t *testing.T) {
	tests := []struct {
		name   string
		status string
		want   bool
	}{
		{name: "allowed does not block", status: "allowed", want: false},
		{name: "an overage warning does not block", status: "allowed_warning", want: false},
		{name: "a rejection blocks", status: "rejected", want: true},
		{name: "credits required blocks", status: "credits_required", want: true},
		{name: "a status-less signal blocks", status: "", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := VendorBlockingRateLimit(tc.status)
			// Assert.
			if got != tc.want {
				t.Fatalf("VendorBlockingRateLimit(%q) = %v, want %v", tc.status, got, tc.want)
			}
		})
	}
}

func TestAbnormalConclusionResolvesVendorBlocked(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 2, "error_max_turns", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED", renderName(got))
	}
}

func TestUserInterruptConcludesGreen(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — the user asked for the turn to stop, and it stopped.
	if err := m.Apply(evTurnEndedReason("s1", 2, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE", renderName(got))
	}
}

// A clean turn simply reports its own outcome. Nothing releases anything:
// the newer session-status lifecycle row is the whole mechanism.
func TestCleanTurnAfterABlockReportsDone(t *testing.T) {
	// Arrange — blocked by an auth failure.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnEndedReason("s1", 1, "authentication_failed", true)); err != nil {
		t.Fatalf("blocked turn: %v", err)
	}
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED before the clean turn", renderName(got))
	}
	// Act — the next turn concludes cleanly.
	if err := m.Apply(evTurnEndedReason("s1", 2, "end_turn", false)); err != nil {
		t.Fatalf("clean turn: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE after a clean conclusion", renderName(got))
	}
}

// ---------------------------------------------------------------------------
// vendor_blocked as a TURN OUTCOME, not a latch
//
// `vendor_blocked` reports HOW the last turn ended, exactly as `done` reports
// that it ended cleanly. It shares the session-status lifecycle with `done`, has no
// clearing token, and is superseded by whatever the agent does next.
//
// Modeling it as an independent latched axis was a defect with no correct
// closed form: a usage limit resets on a clock the daemon cannot observe, so
// a release event that must be witnessed can never arrive. A session that
// died blocked stayed purple across restarts forever.
// ---------------------------------------------------------------------------

// rowsFor returns every (state, cause_kind) a workspace has logged, oldest
// first, so a test can assert on the rows WRITTEN rather than only on what
// they resolve to.
func rowsFor(t *testing.T, db *sql.DB, ws string) [][2]string {
	t.Helper()
	rs, err := db.Query(
		`SELECT state, cause_kind FROM workspace_state WHERE workspace = ? ORDER BY at`, ws)
	if err != nil {
		t.Fatalf("query rows for %q: %v", ws, err)
	}
	defer rs.Close()
	var out [][2]string
	for rs.Next() {
		var state, cause string
		if err := rs.Scan(&state, &cause); err != nil {
			t.Fatalf("scan row: %v", err)
		}
		out = append(out, [2]string{state, cause})
	}
	if err := rs.Err(); err != nil {
		t.Fatalf("iterate rows: %v", err)
	}
	return out
}

// agentRowsOnly drops the WIRED-axis rows a test's arrangement wrote, so a
// row-exactness assertion stays about the axis it is asking after.
func agentRowsOnly(rows [][2]string) [][2]string {
	out := make([][2]string, 0, len(rows))
	for _, r := range rows {
		switch r[0] {
		case sigWired, sigStarting, sigSevered:
			continue
		}
		out = append(out, r)
	}
	return out
}

// An abnormal turn end is ONE fact — the turn ended at the vendor — so it is
// one row. The second, vendor-axis row it used to also write is the latch
// that could never be opened.
func TestAbnormalTurnEndWritesExactlyOneAgentRow(t *testing.T) {
	// Arrange.
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 1, "error_max_turns", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	db, err := openDB(path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer db.Close()
	// The wiring arrangement's own row is dropped: this asserts what the TURN
	// END wrote, and the legacy connectivity projection is a different axis entirely.
	got := agentRowsOnly(rowsFor(t, db, "ws1"))
	want := [][2]string{{sigVendorBlocked, causeVendorBlocked}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
}

// The clean counterpart: one `done` row and no vendor row of any kind.
func TestCleanTurnEndWritesExactlyOneDoneRow(t *testing.T) {
	// Arrange.
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 1, "end_turn", false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	db, err := openDB(path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer db.Close()
	got := agentRowsOnly(rowsFor(t, db, "ws1"))
	want := [][2]string{{sigDone, causeTurnEnded}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
}

// THE HEADLINE BEHAVIOR CHANGE. A turn running NOW is a newer and truer fact
// than how the previous one ended, so red wins. Prompts were never gated on
// the purple, so a retry has to be able to read red.
func TestThinkingAfterAVendorBlockResolvesRed(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 2, 2)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING", renderName(got.state))
	}
}

// A session that comes back up is ready, whatever the previous session's last
// turn did. This is what makes a restart heal the purple.
func TestReadyAfterAVendorBlockResolvesGreen(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, db, "ws", "s2", sigReady, causeSessionStarted, 0, 2)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY", renderName(got.state))
	}
}

// THE DOOM-SHAPED REGRESSION, in the shape the live database had it: a
// workspace whose last turn ended at the vendor, then a fresh session. Before
// the fix the latched vendor row outlived every restart and the workspace read
// blocked indefinitely. Historical rows need no migration — they simply lose.
func TestVendorBlockedThenASessionRestartSelfHeals(t *testing.T) {
	// Arrange — the doom workspace's rows: a turn, its abnormal end, then two
	// later session starts that each asserted readiness.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 1)
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 2, 2)
	seedSignal(t, db, "ws", "s2", sigReady, causeSessionStarted, 0, 3)
	seedSignal(t, db, "ws", "s3", sigReady, causeSessionStarted, 0, 4)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY — the purple must not survive a restart", renderName(got.state))
	}
}

// Blue outranks purple on every axis that carries it independently: a route
// the user cannot see is a stronger claim than how the last turn ended. The
// blue row is seeded OLDER, so only rank can make it win.
func TestBlueOutranksVendorBlocked(t *testing.T) {
	tests := []struct {
		name  string
		state string
		cause string
		want  frontendv1.RenderState
	}{
		{"the transcript could not be read", sigBackfillFailed, "backfill:failed", frontendv1.RenderState_RENDER_STATE_INIT},
		{"the transport went quiet", sigDegraded, "connection_degraded", frontendv1.RenderState_RENDER_STATE_DEGRADED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			seedSignal(t, db, "ws", "s1", tc.state, tc.cause, -1, 1)
			seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 2, 2)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != tc.want {
				t.Fatalf("state = %s, want %s", renderName(got.state), renderName(tc.want))
			}
		})
	}
}

// `dead` is blue, but it shares the session-status lifecycle with `vendor_blocked`, so
// recency settles it rather than rank — the same way `dead` already supersedes
// `done`. A shim that dies after a blocked turn reads dead, which is both the
// newer fact and the stronger claim.
func TestShimDeathAfterAVendorBlockResolvesBlue(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, db, "ws", "s1", sigDead, causeSessionEnded, 2, 2)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_DEAD {
		t.Fatalf("state = %s, want DEAD", renderName(got.state))
	}
}

// Merge states keep their place above the whole color ladder, purple included:
// they are workflow actionability, not agent liveness.
func TestMergeStatesOutrankVendorBlocked(t *testing.T) {
	tests := []struct {
		name  string
		state string
		want  frontendv1.RenderState
	}{
		{"conflict", sigMergeConflict, frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT},
		{"failed", sigMergeFailed, frontendv1.RenderState_RENDER_STATE_MERGE_FAILED},
		{"merged", sigMerged, frontendv1.RenderState_RENDER_STATE_MERGED},
		{"merging", sigMerging, frontendv1.RenderState_RENDER_STATE_MERGING},
		{"queued", sigMergeQueued, frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			seedSignal(t, db, "ws", "s1", tc.state, causeMergeTransition, -1, 1)
			seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 2, 2)
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != tc.want {
				t.Fatalf("state = %s, want %s", renderName(got.state), renderName(tc.want))
			}
		})
	}
}

// Rank 20 still does its job downward: with every non-session-status lifecycle CLEARED,
// nothing competes and the purple stands. Moving it to the session-status lifecycle changed
// which rows supersede it, never where it sits in the ladder.
func TestVendorBlockedStandsWhenEveryOtherAxisIsClear(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, db, "ws", "", sigBackfillOK, "backfill:done", -1, 3)
	seedSignal(t, db, "ws", "", sigDegradedClear, "connection_recovered", -1, 4)
	seedSignal(t, db, "ws", "", sigMergeNone, causeMergeTransition, -1, 5)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED", renderName(got.state))
	}
}

// The yellow promotion is for GREEN winners only. A turn that ended at the
// vendor is not green, so live background work must not repaint it yellow —
// that would hide the outcome the purple exists to report.
func TestBackgroundWorkDoesNotPromoteVendorBlockedToYellow(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "task-1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state = %s, want VENDOR_BLOCKED (idle_async is a green-only promotion)", renderName(got.state))
	}
	if got.liveTaskCount != 1 {
		t.Fatalf("live_task_count = %d, want 1 — the count is still reported", got.liveTaskCount)
	}
}

// Databases written before the remodel carry `vendor_clear` rows. No token
// maps them to a render state and no CTE selects them, so they are inert: the
// database opens, warms, and resolves on the rows that remain.
func TestHistoricalVendorClearRowsAreInert(t *testing.T) {
	// Arrange — a pre-remodel log: a block, its old clearing row, a later turn.
	path := filepath.Join(t.TempDir(), "state.db")
	seed, err := openDB(path)
	if err != nil {
		t.Fatalf("openDB: %v", err)
	}
	seedSignal(t, seed, "ws1", "s1", sigVendorBlocked, causeVendorBlocked, 1, 1)
	seedSignal(t, seed, "ws1", "s1", "vendor_clear", "vendor_cleared", 2, 2)
	seedSignal(t, seed, "ws1", "s1", sigThinking, causeTurnStarted, 3, 3)
	if err := seed.Close(); err != nil {
		t.Fatalf("close seed db: %v", err)
	}

	// Act — reopen through the Manager, which warms every workspace.
	cl := &capLog{}
	m, err := Open(Options{DBPath: path, Logf: cl.logf, Resolver: fakeResolver{}})
	if err != nil {
		t.Fatalf("Open over a pre-remodel database: %v", err)
	}
	t.Cleanup(func() { m.Close() })
	// The legacy row's inertness is observed after a new controller generation
	// establishes itself.
	connectOperational(t, m, "ws1", "s1", "generation-reopen")

	// Assert — the live turn wins; the legacy row contributed nothing.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING", renderName(got))
	}
}

// ---------------------------------------------------------------------------
// Yellow is carved out of every green state, not just idle
// ---------------------------------------------------------------------------

func TestGreenStatesPromoteToIdleAsync(t *testing.T) {
	tests := []struct {
		name  string
		token string
	}{
		{name: "idle with live work", token: sigIdle},
		{name: "ready with live work", token: sigReady},
		{name: "done with live work", token: sigDone},
		{name: "permission with live work", token: sigPermission},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			db := newWiredTestDB(t, "ws")
			seedSignal(t, db, "ws", "s1", tc.token, causeSessionStarted, 1, 1)
			seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "a1")
			// Act.
			got, err := resolve(db, "ws", nil)
			// Assert.
			if err != nil {
				t.Fatalf("resolve: %v", err)
			}
			if got.state != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
				t.Fatalf("state = %s, want IDLE_ASYNC", renderName(got.state))
			}
		})
	}
}

// Red is NOT promoted: a running turn is a stronger claim than background
// work, so thinking stays thinking.
func TestThinkingIsNotPromotedToIdleAsync(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 1)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "a1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %s, want THINKING", renderName(got.state))
	}
}

// ---------------------------------------------------------------------------
// Backfill composes into green
// ---------------------------------------------------------------------------

// THE REOPEN WEDGE, end to end at the SSM: a session whose transcript was
// already ingested reaches GREEN with no prompt ever submitted. If the
// backfill axis could not settle for such a session, this would sit blue
// forever on the most ordinary action there is — reopening a workspace.
func TestReopenedSessionWithIngestedHistoryReachesGreen(t *testing.T) {
	// Arrange — the reopen sequence, with NO transcript line arriving:
	// the daemon settles the backfill from the store high-water and the shim
	// asserts readiness.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyBackfillState("ws1", "done"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act — readiness lands with the backfill already settled.
	// Assert
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY for a reopened, fully-backfilled session", renderName(got))
	}
}

// A FAILED backfill is blue: the history is incomplete, so anything painted
// from it is a partial account of the conversation.
func TestFailedBackfillResolvesBlue(t *testing.T) {
	// Arrange — everything else healthy.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Act.
	if err := m.ApplyBackfillState("ws1", "failed"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INIT {
		t.Fatalf("state = %s, want INIT (blue) for a failed backfill", renderName(got))
	}
}

// "Nothing to backfill" is a real, correct answer — a genuinely fresh
// workspace — not an unknown, so it must not hold the workspace blue.
func TestEmptyWorkspaceBackfillDoesNotHoldBlue(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyBackfillState("ws1", "pending"); err != nil {
		t.Fatalf("backfill: %v", err)
	}
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY for a fresh empty workspace", renderName(got))
	}
}

// A failed backfill that later recovers releases the axis.
func TestBackfillRecoveryReleasesBlue(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	if err := m.ApplyBackfillState("ws1", "failed"); err != nil {
		t.Fatalf("backfill failed: %v", err)
	}
	// Act.
	if err := m.ApplyBackfillState("ws1", "done"); err != nil {
		t.Fatalf("backfill done: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY once the backfill settled", renderName(got))
	}
}
