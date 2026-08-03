package ssm

import (
	"reflect"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// `interrupted` as a TURN OUTCOME (I1)
//
// It is modeled EXACTLY as `done` and `vendor_blocked` are: one session-status lifecycle row
// reporting HOW the last turn ended, no clearing token, superseded by whatever
// the agent does next. The tests below assert each half of that separately.
// ---------------------------------------------------------------------------

// The stopped turn's own end reports the stop, instead of the `done` a bare
// `aborted` conclusion would have written.
func TestMarkedTurnEndResolvesInterrupted(t *testing.T) {
	// Arrange — a turn is running and the user's stop was delivered.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 2, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Fatalf("state = %s, want INTERRUPTED", renderName(got))
	}
}

// An UNMARKED turn end is untouched: only the command path marks, so the
// queue's interject — which sends the same Interrupt as machinery — paints
// nothing.
func TestUnmarkedTurnEndStillResolvesDone(t *testing.T) {
	// Arrange — a turn ends after an interject's stop, which never marks.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 2, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE — an interject's stop is not a user-commanded interrupt", renderName(got))
	}
}

// The mark supersedes the vendor-block classification too: the turn ended
// because the user stopped it, and that is the outcome to report.
func TestMarkedTurnEndSupersedesVendorBlocked(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 1, "error_during_execution", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Fatalf("state = %s, want INTERRUPTED", renderName(got))
	}
}

// ONE row, on the session-status lifecycle — the same shape `done` and `vendor_blocked`
// write. A second row on an axis of its own would be the latch this model
// exists to avoid.
func TestInterruptedTurnEndWritesExactlyOneAgentRow(t *testing.T) {
	// Arrange.
	m, _, path := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 1, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	db, err := openDB(path, t.Logf)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer db.Close()
	got := agentRowsOnly(rowsFor(t, db, "ws1"))
	want := [][2]string{{sigInterrupted, causeInterrupted}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
}

// The mark is spent by the turn it named. A LATER turn ending normally is
// unaffected — nothing latched.
func TestTheNextTurnAfterAnInterruptReportsItsOwnOutcome(t *testing.T) {
	// Arrange — a marked turn ends interrupted.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if err := m.Apply(evTurnEndedReason("s1", 1, "aborted", true)); err != nil {
		t.Fatalf("interrupted turn: %v", err)
	}
	// Act — the next turn ends cleanly.
	if err := m.Apply(evTurnEndedReason("s1", 2, "end_turn", false)); err != nil {
		t.Fatalf("clean turn: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE — `interrupted` is superseded exactly as `done` is", renderName(got))
	}
}

// A new turn STARTING supersedes the outcome the same way it supersedes
// `done`: red is the true answer while a turn runs.
func TestInterruptedIsSupersededByTheNextTurnStart(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigInterrupted, causeInterrupted, 1, 1)
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

// A stop marked BEFORE the stopped turn's own TurnStarted came back through
// the store (the shim's ack outran the event pipeline) still reports that
// turn's end as `interrupted`: the ack guarantees a turn WAS live, so a mark
// with no applied turn means events in flight, never a stop of nothing. This
// is the race the e2e suite caught: without the tolerance the late start
// dropped the mark and the stopped turn resolved `vendor_blocked`.
func TestAStopOutrunningItsTurnsOwnStartStillResolvesInterrupted(t *testing.T) {
	// Arrange — the mark lands first; the turn's start arrives late.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	// Act — the stopped turn's end, aborted by the delivered stop.
	if err := m.Apply(evTurnEndedReason("s1", 2, "aborted", true)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Fatalf("state = %s, want INTERRUPTED — the late start belonged to the marked turn", renderName(got))
	}
}

// A stale mark is DROPPED rather than carried onto a turn that never received
// the stop: when the mark was set against an ALREADY-APPLIED turn, the next
// TurnStarted proves the marked turn's end was lost, and the new turn's end
// must not report a stop it never received.
func TestAnUnspentMarkDoesNotPaintTheFollowingTurn(t *testing.T) {
	// Arrange — a turn is applied and running when the stop is marked; its end
	// is never observed, and a NEW turn starts.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if err := m.Apply(evTurnStarted("s1", 2)); err != nil {
		t.Fatalf("new turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 3, "end_turn", false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE — the stale mark must not paint a turn it never stopped", renderName(got))
	}
}

// The late-start tolerance is spent by exactly ONE start: a second TurnStarted
// with the mark still unspent is a genuinely new turn, and the mark drops.
func TestTheLateStartToleranceSpendsOnExactlyOneStart(t *testing.T) {
	// Arrange — mark before any applied turn, then TWO starts with no end
	// between them (the marked turn's end was lost).
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.MarkTurnInterrupted("ws1"); err != nil {
		t.Fatalf("mark: %v", err)
	}
	if err := m.Apply(evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("late start: %v", err)
	}
	if err := m.Apply(evTurnStarted("s1", 2)); err != nil {
		t.Fatalf("new turn started: %v", err)
	}
	// Act.
	if err := m.Apply(evTurnEndedReason("s1", 3, "end_turn", false)); err != nil {
		t.Fatalf("turn ended: %v", err)
	}
	// Assert.
	if got := mustCurrent(t, m, "ws1").State; got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state = %s, want DONE — one tolerated start, then the mark is stale", renderName(got))
	}
}

// Green, so live background work promotes it to yellow exactly as it does a
// `done` — the interrupted turn is over and detached work is the news.
func TestBackgroundWorkPromotesInterruptedToYellow(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigInterrupted, causeInterrupted, 1, 1)
	seedTaskSignal(t, db, "ws", "s1", sigTaskStarted, causeTaskStarted, 2, 2, "task-1")
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if got.state != frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC {
		t.Fatalf("state = %s, want IDLE_ASYNC (interrupted is green)", renderName(got.state))
	}
}

// The readiness no-regress guard reads the session-status lifecycle, so `interrupted` must
// be a member of it: a readiness assertion after an interrupted turn is a
// real transition, not a suppressed one over a live turn.
func TestTurnActiveReadsAnInterruptedRowAsIdle(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigThinking, causeTurnStarted, 1, 1)
	seedSignal(t, db, "ws", "s1", sigInterrupted, causeInterrupted, 2, 2)
	// Act.
	active, err := turnActive(db, "ws")
	// Assert.
	if err != nil {
		t.Fatalf("turnActive: %v", err)
	}
	if active {
		t.Fatal("turnActive = true, want false — the interrupted row is the newest session-status lifecycle row")
	}
}

// MarkTurnInterrupted refuses an empty workspace rather than filing a mark
// nothing can ever spend.
func TestMarkTurnInterruptedRejectsAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act.
	err := m.MarkTurnInterrupted("")
	// Assert.
	if err == nil {
		t.Fatal("want an error for an empty workspace")
	}
}

// An unranked token contributes no candidate at all, so a workspace holding
// only this row would resolve to nothing. This is the prec-table membership
// check.
func TestInterruptedIsRankedInThePrecedenceTable(t *testing.T) {
	// Arrange.
	db := newWiredTestDB(t, "ws")
	seedSignal(t, db, "ws", "s1", sigInterrupted, causeInterrupted, 1, 1)
	// Act.
	got, err := resolve(db, "ws", nil)
	// Assert.
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if !got.found || got.state != frontendv1.RenderState_RENDER_STATE_INTERRUPTED {
		t.Fatalf("resolve = (found %v, %s), want a found INTERRUPTED — the token is unranked", got.found, renderName(got.state))
	}
}
