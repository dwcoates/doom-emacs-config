package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

func TestOnTurnEndedStampsTheTurnsClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseFailed)
	// Assert
	if got := h.db.closedTurns["running-turn"]; got != wsm.CloseFailed {
		t.Fatalf("close = %s, want failed", closeName(got))
	}
}

// TestOnTurnEndedWithNothingHeldIsRecordedAtInfo covers the turn end that
// delivers nothing: it still leaves an INFO record, so a turn end reaching the
// queue is always visible on disk and "the queue was never told" cannot be
// confused with "the queue was told and had nothing to do".
func TestOnTurnEndedWithNothingHeldIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	beforeRecords := len(h.log.Records())
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	for _, record := range h.log.Records()[beforeRecords:] {
		if record.Level == "info" && record.Operation == "daemon.promptqueue.turn_ended" {
			return
		}
	}
	t.Fatalf("records = %+v, want an info daemon.promptqueue.turn_ended", h.log.Records()[beforeRecords:])
}

func TestOnTurnEndedClearsTheInterruptingStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)
	// Assert
	got := h.footer.interruptions()
	if len(got) == 0 || got[len(got)-1] {
		t.Fatalf("footer = %v, want the interrupting status cleared", got)
	}
}

func TestOnTurnEndedDeliversTheNextHeldPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt", started)
	}
}

func TestOnTurnEndedDeliversTheOldestHoldFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	h.q.deps.Now = func() time.Time { return instant }
	if _, err := h.q.Submit(context.Background(), submission("older", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.deps.Now = func() time.Time { return instant.Add(time.Minute) }
	if _, err := h.q.Submit(context.Background(), submission("newer", "second")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "older" {
		t.Fatalf("started = %v, want the oldest hold first", started)
	}
}

func TestOnTurnEndedLeavesAHoldADaemonConditionStillHolds(t *testing.T) {
	// Arrange: a drain hold is not released by a turn ending.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if len(h.sender.started()) != 0 {
		t.Fatal("a prompt the drain still holds must not be delivered")
	}
}

func TestOnLeaseChangedStampsANewHoldOnEveryStandingPrompt(t *testing.T) {
	// Arrange: a prompt held for the running turn, then a drain lease arrives.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	held := h.db.hold("t1")
	if held.Hold == nil || *held.Hold != wsm.HoldShutdown {
		t.Fatalf("hold = %+v, want a shutdown hold", held.Hold)
	}
}

func TestOnLeaseChangedReleasesAHoldTheLeaseNoLongerProjects(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if h.db.hold("t1").Hold != nil {
		t.Fatal("a hold the lease no longer projects must be released")
	}
}

func TestOnLeaseChangedDeliversAReleasedHoldWhenNothingIsRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the released hold delivered", started)
	}
}

func TestOnLeaseChangedRunsAQueuedActBeforeTheReleasedPrompt(t *testing.T) {
	// Arrange: a /compact queued behind a prompt the lease holds.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "t-compact"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert: the cut runs, and the prompt waits for the cut's end.
	if started := h.sender.started(); len(started) != 1 || started[0] != "t-compact" {
		t.Fatalf("started = %v, want the queued /compact first and alone", started)
	}
}

func TestOnLeaseChangedWithNoHoldsStillRunsAQueuedAct(t *testing.T) {
	// Arrange: an act installed with nothing held behind the lease.
	h := newHarness(t)
	h.lease(wsm.HolderRestart, wsm.PolicyHold)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).acts = []Act{{Kind: ActSetModel, Value: "opus"}}
	h.q.mu.Unlock()
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if models := h.sender.modelsSet(); len(models) != 1 || models[0] != "opus" {
		t.Fatalf("models = %v, want the queued act run at the lease's release", models)
	}
}

func TestOnLeaseChangedKeepsAQueuedActWhileTheLeaseStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderRestart, wsm.PolicyHold)
	h.q.mu.Lock()
	h.q.stateLocked(theWorkspace).acts = []Act{{Kind: ActSetModel, Value: "opus"}}
	h.q.mu.Unlock()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if models := h.sender.modelsSet(); len(models) != 0 {
		t.Fatalf("models = %v, want the act kept behind the standing lease", models)
	}
}

func TestOnLeaseChangedHoldsBackWhileATurnStillRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	h.watcher.running("running-turn")
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if len(h.sender.started()) != 0 {
		t.Fatal("a released hold waits for the running turn to end")
	}
}

func TestRestoreHoldsPublishesEveryStandingHoldPerWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.holds.pushes = nil
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.holds.last()) != 1 {
		t.Fatalf("tray = %v, want the restored hold", h.holds.last())
	}
}

func TestRestoreHoldsIsAllOrNothing(t *testing.T) {
	// Arrange: a corrupt row fails the read, and nothing is loaded.
	h := newHarness(t)
	h.db.allHeldErr = errors.New("a held-prompt row does not decode")
	// Act
	err := h.q.RestoreHolds(context.Background())
	// Assert
	if err == nil {
		t.Fatal("a failed restore must be surfaced")
	}
	if h.holds.pushCount() != 0 {
		t.Fatal("nothing may be published when the restore failed")
	}
}

func TestRestoreHoldsClosesTheOrphanedTurnsOfADeadSession(t *testing.T) {
	// Arrange: a turn was in flight and the workspace has no session left.
	h := newHarness(t)
	if err := h.db.PutTurn(context.Background(), wsm.Turn{
		ID: "running-turn", Workspace: theWorkspace, Text: "work", StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	h.noSession = true
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.db.orphaned) != 1 || h.db.orphaned[0] != theWorkspace {
		t.Fatalf("orphaned = %v, want the dead session's workspace", h.db.orphaned)
	}
}

func TestRestoreHoldsLeavesALiveSessionsTurnsToTheWatcher(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.db.PutTurn(context.Background(), wsm.Turn{
		ID: "running-turn", Workspace: theWorkspace, Text: "work", StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.db.orphaned) != 0 {
		t.Fatal("a live session's in-flight turn is the watcher's, not an orphan")
	}
}

func TestNextDeliverablePrefersTheSemanticHead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	head := ids.TurnID("older")
	if _, err := h.q.Submit(context.Background(), submission("newer", "second")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.q.state(theWorkspace).head = &head
	// Act
	got, ok, _, err := h.q.nextDeliverable(context.Background(), theWorkspace)
	// Assert
	if err != nil || !ok {
		t.Fatalf("nextDeliverable: %v, ok=%v", err, ok)
	}
	if got.Turn != head {
		t.Fatalf("next = %q, want the semantic head", got.Turn)
	}
}

// TestTheTurnEndsDrainExcludesALeaseChangeForTheWholeDelivery covers the
// serialization the handover's in-order intake drain rests on: a turn's end
// and a lease change both deliver from the same standing holds, so a quiesce
// that arrived while the turn end was still delivering would read the holds
// the delivery has not yet retired and let the intake out of order.
func TestTheTurnEndsDrainExcludesALeaseChangeForTheWholeDelivery(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	free := true
	h.sender.startHook = func() { free = h.q.state(theWorkspace).drain.TryLock() }

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if free {
		t.Fatal("the workspace's drain was free while the turn end's own delivery was in flight")
	}
}

// TestOnTurnsEndedUnobservedClosesEachTurnAsOrphaned covers the adoption's
// reconciliation: each turn ended while no daemon was watching, which is the
// orphaned close.
func TestOnTurnsEndedUnobservedClosesEachTurnAsOrphaned(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.q.OnTurnsEndedUnobserved(theWorkspace, []ids.TurnID{"turn-1", "turn-2"})
	// Assert
	for _, turn := range []ids.TurnID{"turn-1", "turn-2"} {
		if got, closed := h.db.closedTurns[turn]; !closed || got != wsm.CloseOrphaned {
			t.Fatalf("%s close = (%s, closed %v), want orphaned", turn, closeName(got), closed)
		}
	}
}

// TestOnTurnsEndedUnobservedIsRecordedAtInfo covers the record: an ordinary
// reconciliation, stated per turn at INFO.
func TestOnTurnsEndedUnobservedIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	beforeRecords := len(h.log.Records())
	// Act
	h.q.OnTurnsEndedUnobserved(theWorkspace, []ids.TurnID{"turn-1"})
	// Assert
	for _, record := range h.log.Records()[beforeRecords:] {
		if record.Level == "info" && record.Operation == "daemon.promptqueue.turn_ended" && record.Context["turn"] == "turn-1" {
			return
		}
	}
	t.Fatalf("records = %+v, want an info daemon.promptqueue.turn_ended naming turn-1", h.log.Records()[beforeRecords:])
}

// TestOnTurnsEndedUnobservedDeliversNothing covers what the close is not: no
// turn of these was the session's turn in flight, so nothing held is popped.
func TestOnTurnsEndedUnobservedDeliversNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	h.watcher.idle()
	// Act
	h.q.OnTurnsEndedUnobserved(theWorkspace, []ids.TurnID{"stale-turn"})
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v after the reconciliation, want nothing delivered", started)
	}
}

// TestOnTurnsEndedUnobservedRecordsAFailedCloseAndClosesTheRest covers the
// error path: a close that fails is ERROR, and the other turns still close.
func TestOnTurnsEndedUnobservedRecordsAFailedCloseAndClosesTheRest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.closeTurnErrs = map[ids.TurnID]error{"turn-1": errors.New("the store is down")}
	// Act
	h.q.OnTurnsEndedUnobserved(theWorkspace, []ids.TurnID{"turn-1", "turn-2"})
	// Assert
	failed := false
	for _, record := range h.log.Records() {
		failed = failed || (record.Level == "error" && record.Operation == "daemon.promptqueue.turn_ended" && record.Context["turn"] == "turn-1")
	}
	if !failed {
		t.Fatalf("records = %+v, want an error daemon.promptqueue.turn_ended for the failed close", h.log.Records())
	}
	if got := h.db.closedTurns["turn-2"]; got != wsm.CloseOrphaned {
		t.Fatalf("turn-2 close = %s, want orphaned despite turn-1's failure", closeName(got))
	}
}

// THE ADOPTED TURN. A turn the vendor started on its own is recorded exactly as
// a delivered turn is, so its end closes through the one door.

// TestOnTurnAdoptedRecordsTheTurnsDurableRow covers the row: written open, with
// the vendor-started origin.
func TestOnTurnAdoptedRecordsTheTurnsDurableRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	got, ok := h.db.turns["vendor-turn"]
	if !ok {
		t.Fatal("no turn row was written for the adopted turn")
	}
	if got.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED.String() || got.Close != nil {
		t.Fatalf("row = %+v, want an open vendor-started row", got)
	}
}

// TestOnTurnAdoptedGivesTheRosterTheTurn covers the roster: the turn fact is the
// daemon's own, so the roster takes it thinking.
func TestOnTurnAdoptedGivesTheRosterTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	turns := h.sidebar.rosterTurns()
	if len(turns) != 1 || turns[0] == nil || turns[0].Act != footer.ActPrompt {
		t.Fatalf("roster turns = %v, want the adopted turn installed", turns)
	}
}

// TestOnTurnAdoptedIsRecordedAtInfo covers the record: the adoption is an
// ordinary event, stated at INFO naming the turn.
func TestOnTurnAdoptedIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	beforeRecords := len(h.log.Records())
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	for _, record := range h.log.Records()[beforeRecords:] {
		if record.Level == "info" && record.Operation == "daemon.promptqueue.turn_adopted" && record.Context["turn"] == "vendor-turn" {
			return
		}
	}
	t.Fatalf("records = %+v, want an info daemon.promptqueue.turn_adopted naming vendor-turn", h.log.Records()[beforeRecords:])
}

// TestOnTurnAdoptedDeliversNothing covers the hold: a prompt held behind the
// running work is never delivered into the adopted turn.
func TestOnTurnAdoptedDeliversNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "vendor-turn", "")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v after the adoption, want nothing delivered", started)
	}
}

// TestOnTurnAdoptedRecordsAFailedRowWriteAtError covers the error path: a row
// that could not be written is loud.
func TestOnTurnAdoptedRecordsAFailedRowWriteAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.putTurnErr = errors.New("the store is down")
	beforeRecords := len(h.log.Records())
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	for _, record := range h.log.Records()[beforeRecords:] {
		if record.Level == "error" && record.Operation == "daemon.promptqueue.turn_adopted" {
			return
		}
	}
	t.Fatalf("records = %+v, want an error daemon.promptqueue.turn_adopted", h.log.Records()[beforeRecords:])
}

// TestOnTurnAdoptedStillGivesTheRosterTheTurnWhenTheRowWriteFails covers what a
// failed write does not undo: the turn is running whether or not its row was
// written.
func TestOnTurnAdoptedStillGivesTheRosterTheTurnWhenTheRowWriteFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.putTurnErr = errors.New("the store is down")
	// Act
	h.q.OnTurnAdopted(theWorkspace, "vendor-turn")
	// Assert
	if turns := h.sidebar.rosterTurns(); len(turns) != 1 || turns[0] == nil {
		t.Fatalf("roster turns = %v, want the adopted turn installed", turns)
	}
}

// ANOTHER TURN ALREADY RUNS. A turn end arriving after a vendor-started turn was
// adopted (or another turn delivered) must not treat the session as free.

// TestOnTurnEndedWhileAnotherTurnRunsDeliversNothing covers the pop: nothing
// held is started into the running turn.
func TestOnTurnEndedWhileAnotherTurnRunsDeliversNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "earlier-turn", "the earlier work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	h.watcher.running("vendor-turn")
	// Act
	h.q.OnTurnEnded(theWorkspace, "earlier-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want the held prompt to wait for the running turn", started)
	}
}

// TestOnTurnEndedWhileAnotherTurnRunsLeavesTheRosterAlone covers the roster:
// its thinking is the running turn's, so the earlier turn's end does not close it.
func TestOnTurnEndedWhileAnotherTurnRunsLeavesTheRosterAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "earlier-turn", "the earlier work")
	h.watcher.running("vendor-turn")
	// Act
	h.q.OnTurnEnded(theWorkspace, "earlier-turn", wsm.CloseCompleted)
	// Assert
	if ends := h.sidebar.rosterEnds(); len(ends) != 0 {
		t.Fatalf("roster ends = %v, want none while another turn runs", ends)
	}
}

// TestOnTurnEndedWhileAnotherTurnRunsStillClosesItsRow covers the door: the
// ended turn's row closes all the same.
func TestOnTurnEndedWhileAnotherTurnRunsStillClosesItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "earlier-turn", "the earlier work")
	h.watcher.running("vendor-turn")
	// Act
	h.q.OnTurnEnded(theWorkspace, "earlier-turn", wsm.CloseCompleted)
	// Assert
	if got, closed := h.db.closedTurns["earlier-turn"]; !closed || got != wsm.CloseCompleted {
		t.Fatalf("close = (%s, closed %v), want completed", closeName(got), closed)
	}
}

// THE TURN'S BANNER: every live turn end raises it, after the door has drawn
// the ending it is composed from.

func TestOnTurnEndedRaisesTheTurnsBanner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseFailed)
	// Assert
	want := bannerEnd{WS: theWorkspace, Turn: "running-turn", How: wsm.CloseFailed}
	if got := h.banners.raised(); len(got) != 1 || got[0] != want {
		t.Fatalf("banners = %+v, want [%+v]", got, want)
	}
}

func TestOnTurnEndedWhileAnotherTurnRunsStillRaisesItsBanner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "earlier-turn", "the earlier work")
	h.watcher.running("vendor-turn")
	// Act
	h.q.OnTurnEnded(theWorkspace, "earlier-turn", wsm.CloseCompleted)
	// Assert
	if got := h.banners.raised(); len(got) != 1 || got[0].Turn != "earlier-turn" {
		t.Fatalf("banners = %+v, want the earlier turn's", got)
	}
}

func TestOnTurnEndedForAnUnresolvableWorkspaceStillRaisesItsBanner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	h.q.OnTurnEnded("nowhere", "lost-turn", wsm.CloseCompleted)
	// Assert
	if got := h.banners.raised(); len(got) != 1 || got[0].Turn != "lost-turn" {
		t.Fatalf("banners = %+v, want the turn's banner", got)
	}
}

func TestOnTurnEndedRaisesTheBannerAfterTheDoorClosedTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	closedFirst := false
	h.banners.onEnded = func() { _, closedFirst = h.db.closedTurns["running-turn"] }
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if !closedFirst {
		t.Fatal("the banner was raised before the door closed the turn")
	}
}
