package sessioncontroller

import (
	"errors"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
	"claude-repld/internal/statedb"
)

// THE WEDGE THIS FILE PINS. A revived session's backlogged `TurnStarted` bound
// the turn record to a keep-alive ping from a daemon generation that no longer
// existed. Nothing was driving it, nothing would ever end it, and the workspace
// rendered `thinking` for 25 minutes with no vendor query behind it. These tests
// pin that such a bind is detected, resolved, and reported — and that an
// ordinary driven turn is never touched.

// undrivenRig is a wired manager whose clock the test owns, so the deadline is
// evaluated against a remembered bind instant rather than against a timer
// nobody can drive.
func undrivenRig(t *testing.T, opts ...func(*Config)) (*Manager, *fakeApplier) {
	t.Helper()
	m, applier, _ := newHibernationRig(t, opts...)
	return m, applier
}

// bindUndrivenTurn binds the turn record to turnID off the SSM's own turn-
// liveness derivation — the exact edge the incident's backlogged TurnStarted
// arrived on — and then advances the clock past the watchdog's deadline.
func bindUndrivenTurn(t *testing.T, m *Manager, turnID string, advanceMs int64) *sessionController {
	t.Helper()
	d := controllerFor(t, m)
	m.noteTurnLivenessIDs(d, []string{turnID})
	base := m.now()
	m.now = func() int64 { return base + advanceMs }
	return d
}

// overdueMs is a comfortable margin past the watchdog's own deadline.
const overdueMs = undrivenTurnDeadlineMs + 1

// turnRecordOf reports the controller's current turn record.
func turnRecordOf(m *Manager, d *sessionController) turnRecord {
	m.mu.Lock()
	defer m.mu.Unlock()
	return d.turn
}

// ---------------------------------------------------------------------------
// THE WATCHDOG FIRES, AND IT RESOLVES THE TURN.
// ---------------------------------------------------------------------------

// A BOUND TURN WITH NO DRIVER IS DETECTED. This is the whole invariant: nothing
// in the daemon submitted `ka_dead`, no shim announced it, and it stood past the
// deadline.
func TestSweepUndrivenTurnsResolvesATurnNothingIsDriving(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 1 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want the one bound with no driver", resolved)
	}
}

// THE DURABLE CLAIM IS RETIRED, and it is the half that matters outside this
// process: hibernation and every restart guard read the ledger, not the
// manager's memory. It is what pinned the incident's workspace.
func TestSweepUndrivenTurnsClosesTheDurableClaim(t *testing.T) {
	// Arrange.
	m, applier := undrivenRig(t)
	bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	calls := applier.recordedOriginTurnCloses()
	if len(calls) != 1 {
		t.Fatalf("origin turn closes = %d, want exactly the one the watchdog licensed", len(calls))
	}
	got := calls[0]
	if got.workspace != "ws" || len(got.turnIDs) != 1 || got.turnIDs[0] != "ka_dead" {
		t.Fatalf("origin turn close = %+v, want the undriven turn %q on ws", got, "ka_dead")
	}
	if got.cause != ssm.TurnCloseUndriven {
		t.Fatalf("origin turn close cause = %q, want %q; a close that cannot name the lifecycle fact behind it is indistinguishable from a lost one",
			got.cause, ssm.TurnCloseUndriven)
	}
}

// THE IN-MEMORY RECORD IS RELEASED TOO. A ledger the watchdog tidied under a
// record still saying `named` is the same wedge one layer up: the queue would
// still hold every prompt behind a turn that no longer exists.
func TestSweepUndrivenTurnsReleasesTheTurnRecord(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	d := bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	if rec := turnRecordOf(m, d); rec.active() {
		t.Fatalf("turn record = %s after the watchdog resolved it, want idle; the queue still holds every prompt behind a turn nothing is driving", rec)
	}
}

// THE USER IS TOLD. Closing the claim un-pins the workspace; without the card
// the session would simply have stopped working on what was asked of it and
// never said so, which is the silence this whole feature exists to end.
func TestSweepUndrivenTurnsSurfacesACard(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	if !pushedFailureType(m, string(errclass.TypeTurnUndriven)) {
		t.Fatalf("no %s failure reached the frontend; the workspace stopped thinking with no account of why",
			errclass.TypeTurnUndriven)
	}
}

// A LEDGER WRITE THAT FAILED STILL RELEASES THE RECORD. Holding the in-memory
// half on top of a failed durable close keeps the workspace pinned, which is
// strictly worse than holding only the durable half.
func TestSweepUndrivenTurnsReleasesTheRecordWhenTheLedgerCloseFails(t *testing.T) {
	// Arrange.
	m, applier := undrivenRig(t)
	applier.originTurnErr = errors.New("the turn ledger is unwritable")
	d := bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	if rec := turnRecordOf(m, d); rec.active() {
		t.Fatalf("turn record = %s after a failed ledger close, want idle; the record must not outlive the resolution", rec)
	}
}

// ONCE, NOT EVERY SWEEP. The resolution publishes a card and a synthesized
// boundary; a second one for the same bind would be a second card for one fault.
func TestSweepUndrivenTurnsResolvesOneBindOnlyOnce(t *testing.T) {
	// Arrange.
	m, applier := undrivenRig(t)
	bindUndrivenTurn(t, m, "ka_dead", overdueMs)

	// Act.
	m.SweepUndrivenTurns()
	again := m.SweepUndrivenTurns()

	// Assert.
	if again != 0 {
		t.Fatalf("the second sweep resolved %d turns, want 0; one bind is one fault", again)
	}
	if calls := applier.recordedOriginTurnCloses(); len(calls) != 1 {
		t.Fatalf("origin turn closes = %d across two sweeps, want 1", len(calls))
	}
}

// ---------------------------------------------------------------------------
// THE RE-DRIVE. A revival that owes work drives it before closing the dead
// claim — the turn is never merely abandoned when there is something to run.
// ---------------------------------------------------------------------------

// THE OWED TURN IS ACTUALLY DRIVEN TO THE SDK. The close alone would un-pin the
// workspace and drop the user's work on the floor; the whole point of the
// resolution is that the work continues where it can.
func TestSweepUndrivenTurnsReDrivesTheOwedTurn(t *testing.T) {
	// Arrange.
	receipts := newFakeReceiptStore()
	m, _ := undrivenRig(t, func(cfg *Config) { cfg.PromptReceipts = receipts })
	if err := receipts.RecordPendingResumption(statedb.PendingResumption{
		RequestID:       resumptionRequestID("ws", "t-owed", 2_000),
		Workspace:       "ws",
		TurnID:          "t-owed",
		Text:            resumptionInstruction,
		InterruptedAtMs: 2_000,
	}); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	bindUndrivenTurn(t, m, "ka_dead", overdueMs)
	before := len(fakeClientFor(t, m, "ws").promptTexts())

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	if got := len(fakeClientFor(t, m, "ws").promptTexts()); got != before+1 {
		t.Fatalf("prompts submitted = %d, want the owed turn re-driven exactly once (was %d)", got, before)
	}
}

// A REVIVAL THAT OWES NOTHING STILL CLOSES THE TURN HONESTLY. This is the
// incident's own shape: the bounce was hard, so no resumption was ever
// recorded, and there is nothing to re-drive. The turn must not be left bound.
func TestSweepUndrivenTurnsClosesTheTurnWhenNoReDriveIsPossible(t *testing.T) {
	// Arrange.
	receipts := newFakeReceiptStore()
	m, applier := undrivenRig(t, func(cfg *Config) { cfg.PromptReceipts = receipts })
	d := bindUndrivenTurn(t, m, "ka_dead", overdueMs)
	before := len(fakeClientFor(t, m, "ws").promptTexts())

	// Act.
	m.SweepUndrivenTurns()

	// Assert.
	if got := len(fakeClientFor(t, m, "ws").promptTexts()); got != before {
		t.Fatalf("prompts submitted = %d, want none: nothing was owed, so nothing may be invented to run", got)
	}
	if len(applier.recordedOriginTurnCloses()) != 1 {
		t.Fatal("the turn was left bound with nothing to re-drive it; a turn that cannot be driven must be closed, never pinned")
	}
	if rec := turnRecordOf(m, d); rec.active() {
		t.Fatalf("turn record = %s, want idle", rec)
	}
	if !pushedFailureType(m, string(errclass.TypeTurnUndriven)) {
		t.Fatal("an un-redrivable turn was closed with no card; the user is owed the account")
	}
}

// ---------------------------------------------------------------------------
// A DRIVEN TURN IS NEVER TOUCHED. The bound exists to catch a turn nothing is
// waiting on; a sweep that reached a live turn would be killing the very work
// it was built to protect.
// ---------------------------------------------------------------------------

// A TURN THIS DAEMON SUBMITTED IS DRIVEN, however long it runs. A tool call that
// takes ten minutes emits nothing the daemon can see, and closing it would be
// far worse than the wedge.
func TestSweepUndrivenTurnsLeavesATurnThisDaemonSubmittedAlone(t *testing.T) {
	// Arrange.
	m, applier := undrivenRig(t)
	d := controllerFor(t, m)
	m.noteTurnDriven(d, "t-mine")
	bindUndrivenTurn(t, m, "t-mine", overdueMs)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 0 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 0; this daemon submitted the turn and is waiting on it", resolved)
	}
	if calls := applier.recordedOriginTurnCloses(); len(calls) != 0 {
		t.Fatalf("origin turn closes = %d, want none against a turn with a live driver", len(calls))
	}
}

// A TURN THE SHIM ANNOUNCED IN FLIGHT IS DRIVEN, whichever daemon generation
// submitted it. A reattach mid-turn is exactly the case where the process
// behind the work outlived the daemon, and it is real work.
func TestSweepUndrivenTurnsLeavesATurnTheShimAnnouncedAlone(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	d := controllerFor(t, m)
	m.reconcileTurnSnapshot(d, true, &corev1.ShimHello{
		TurnInFlight: true, ActiveTurnIds: []string{"t-adopted"},
	})
	bindUndrivenTurn(t, m, "t-adopted", overdueMs)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 0 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 0; the shim itself reported this turn in flight", resolved)
	}
}

// A TURN STILL INSIDE THE DEADLINE IS LEFT ALONE. The window is head-room for a
// bind observed before its driver is recorded, and a sweep that ignored it
// would race every ordinary submit.
func TestSweepUndrivenTurnsLeavesATurnInsideTheDeadlineAlone(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	bindUndrivenTurn(t, m, "ka_fresh", undrivenTurnDeadlineMs-1)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 0 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 0 inside the deadline", resolved)
	}
}

// A TURN THAT ENDED ON ITS OWN LEAVES NOTHING TO RESOLVE. The record is
// re-read at the sweep rather than trusted from the watch, so a boundary that
// landed in between disarms it.
func TestSweepUndrivenTurnsIgnoresATurnThatEndedFirst(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	d := bindUndrivenTurn(t, m, "ka_dead", overdueMs)
	m.noteTurnLivenessIDs(d, nil)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 0 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 0; the turn ended before the deadline was reached", resolved)
	}
}

// A RE-BIND OF THE SAME TURN DOES NOT RESTAMP THE CLOCK. The derivation is
// re-projected on every boundary the workspace sees, and a watch whose instant
// moved on each of them would never reach its own deadline — the wedge would
// survive the cure.
func TestARebindOfTheSameUndrivenTurnDoesNotPostponeTheDeadline(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	d := bindUndrivenTurn(t, m, "ka_dead", overdueMs)
	m.noteTurnLivenessIDs(d, []string{"ka_dead"})

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 1 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 1; re-observing the same bind must not postpone its deadline", resolved)
	}
}

// AN UNNAMEABLE TURN IS NOT WATCHED. A close keyed by an empty id would match
// every legacy claim in the workspace, including turns some other origin is
// still running.
func TestAnUnnameableTurnIsNotWatched(t *testing.T) {
	// Arrange — a legacy start carries no turn id at all, so the record holds
	// the turn as `adopted` and names nothing.
	m, _ := undrivenRig(t)
	bindUndrivenTurn(t, m, "", overdueMs)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 0 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 0; an identity-less claim cannot be closed by name", resolved)
	}
}

// A SUBMIT THE SHIM REFUSED IS NOT A DRIVER. The drive record is taken before
// the submit so a racing TurnStarted is covered; a submit that failed must give
// it back, or a claim some other path opens would be vouched for by a prompt no
// session ever took.
func TestAFailedSubmitRetractsItsDriveRecord(t *testing.T) {
	// Arrange.
	m, _ := undrivenRig(t)
	d := controllerFor(t, m)
	m.noteTurnDriven(d, "t-refused")
	m.forgetTurnDriven(d, "t-refused")
	bindUndrivenTurn(t, m, "t-refused", overdueMs)

	// Act.
	resolved := m.SweepUndrivenTurns()

	// Assert.
	if resolved != 1 {
		t.Fatalf("SweepUndrivenTurns resolved %d turns, want 1; a refused submit leaves nothing driving the turn", resolved)
	}
}
