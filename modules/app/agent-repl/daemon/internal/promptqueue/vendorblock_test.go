package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// newVendorBlockHarness is the harness whose delivered turns stand in flight
// and whose clock advances per stamp, so queue order is the submission order.
func newVendorBlockHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	tick := 0
	h.q.deps.Now = func() time.Time {
		tick++
		return instant.Add(time.Duration(tick) * time.Second)
	}
	return h
}

// submitAll submits each turn in order, failing on any error.
func submitAll(t *testing.T, h *harness, turns ...ids.TurnID) {
	t.Helper()
	for _, turn := range turns {
		if _, err := h.q.Submit(context.Background(), submission(turn, "prompt "+string(turn))); err != nil {
			t.Fatalf("Submit(%s): %v", turn, err)
		}
	}
}

// heldUnderBlock stands a usage limit and holds TURNS under it while a turn
// is in flight, so none of the submissions is a try-now (trynow.go); the turn
// then ends, leaving them held after reconnect with nothing running.
func heldUnderBlock(t *testing.T, h *harness, block string, turns ...ids.TurnID) {
	t.Helper()
	h.footer.standVendorBlock(block)
	h.watcher.running("t-busy")
	submitAll(t, h, turns...)
	h.watcher.idle()
}

// vendorServes tells the queue the vendor serves again and joins the release.
func vendorServes(h *harness) {
	h.footer.standVendorBlock("")
	h.q.OnVendorServes(theWorkspace)
	h.q.releasing.Wait()
	h.q.waitForClassifications()
}

// standing is the workspace's standing hold for a turn.
func standing(t *testing.T, h *harness, turn ids.TurnID) wsm.HeldPrompt {
	t.Helper()
	held, err := h.q.standingHold(context.Background(), theWorkspace, turn)
	if err != nil {
		t.Fatalf("standingHold(%s): %v", turn, err)
	}
	return held
}

// heldAfterReconnectUnclassified reports a hold on the after-reconnect hold
// that nothing has classified.
func heldAfterReconnectUnclassified(h wsm.HeldPrompt) bool {
	return h.Tombstone == nil && h.Hold != nil && *h.Hold == wsm.HoldReconnect && h.Classification == nil
}

func TestAPromptDuringAMidSessionVendorBlockIsHeldAfterReconnectUnclassified(t *testing.T) {
	tests := []struct {
		name    string
		block   string
		running bool
	}{
		{name: "a usage limit", block: "usage_limit"},
		{name: "an authentication refusal", block: "auth"},
		{name: "a billing refusal", block: "billing"},
		{name: "an api retry holding the running turn", block: "api_retrying", running: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newVendorBlockHarness(t)
			if tc.running {
				h.watcher.running("t0")
			}
			h.footer.standVendorBlock(tc.block)

			// Act
			got, err := h.q.Submit(context.Background(), submission("t1", "hello"))

			// Assert
			if err != nil || got.Held == nil || *got.Held != wsm.HoldReconnect || got.Classification != nil {
				t.Fatalf("Submit = (%+v, %v), want held after reconnect with no verdict", got, err)
			}
		})
	}
}

func TestAPromptDuringAVendorBlockIsNotSentToTheVendor(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")

	// Act
	submitAll(t, h, "t1")

	// Assert
	if got := h.sender.startAttempts(); got != 0 {
		t.Fatalf("StartTurn attempts = %d, want none while the vendor refuses the session", got)
	}
}

func TestAPromptDuringAnAPIRetryIsNotClassified(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.watcher.running("t0")
	h.footer.standVendorBlock("api_retrying")

	// Act
	submitAll(t, h, "t1")
	h.q.waitForClassifications()

	// Assert
	if got := h.judge.questions(); len(got) != 0 || len(h.sender.killed()) != 0 {
		t.Fatalf("judge questions = %v, kills = %v, want neither while the vendor retries", got, h.sender.killed())
	}
}

func TestAHoldDecisionUnderAVendorBlockIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")

	// Act
	submitAll(t, h, "t1")

	// Assert
	if !hasRecord(h, "info", opSubmit, "the vendor does not serve the session; the prompt is held after reconnect, unclassified, until it serves again") {
		t.Fatalf("no INFO record of the vendor-block hold decision")
	}
}

func TestWithNoVendorBlockAPromptIsDeliveredAsBefore(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err != nil || !got.Delivered {
		t.Fatalf("Submit = (%+v, %v), want delivered at once", got, err)
	}
}

func TestWhileTheBlockStandsTheHeldPromptsStayUnclassified(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)
	h.q.waitForClassifications()

	// Assert
	for _, turn := range []ids.TurnID{"t1", "t2"} {
		if got := standing(t, h, turn); !heldAfterReconnectUnclassified(got) {
			t.Fatalf("%s = %+v, want still held after reconnect, unclassified", turn, got)
		}
	}
}

func TestWhenTheVendorServesTheFirstHeldPromptIsDelivered(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2", "t3")

	// Act
	vendorServes(h)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want [t1]: the oldest held prompt goes first", got)
	}
}

func TestWhenTheVendorServesTheRestAreClassifiedInOrder(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2", "t3")

	// Act
	vendorServes(h)

	// Assert
	for _, turn := range []ids.TurnID{"t2", "t3"} {
		got := standing(t, h, turn)
		if got.Hold != nil || got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
			t.Fatalf("%s = %+v, want released and classified hold_for_turn_end", turn, got)
		}
	}
}

func TestReleasedPromptsAreJudgedAgainstWhatIsAheadOfEach(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")

	// Act
	vendorServes(h)

	// Assert
	got := h.judge.questions()
	if len(got) != 1 || got[0] != [2]string{"prompt t1", "prompt t2"} {
		t.Fatalf("judge questions = %v, want t2 judged against t1, the turn the release started", got)
	}
}

func TestWhenTheRetriedCallIsAnsweredTheHeldPromptIsClassifiedAgainstTheRunningTurn(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.watcher.running("t0")
	if err := h.q.recordTurn(context.Background(), wsm.Turn{ID: "t0", Workspace: theWorkspace, Text: "running work", StartedAt: instant}, h.log.Global()); err != nil {
		t.Fatalf("recordTurn: %v", err)
	}
	h.footer.standVendorBlock("api_retrying")
	submitAll(t, h, "t1")

	// Act
	vendorServes(h)

	// Assert
	got := standing(t, h, "t1")
	if got.Hold != nil || got.Classification == nil || len(h.sender.started()) != 0 {
		t.Fatalf("t1 = %+v, started = %v, want classified against the running turn and not started", got, h.sender.started())
	}
}

func TestADroppedHeldPromptIsNotDeliveredWhenTheVendorServes(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}

	// Act
	vendorServes(h)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t2" {
		t.Fatalf("started = %v, want [t2]: the dropped t1 never goes", got)
	}
}

func TestAnEditCommittedWhileHeldLeavesThePromptHeldUnclassified(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	if err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp); err != nil {
		t.Fatalf("BeginEdit: %v", err)
	}

	// Act
	err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("edited"))
	h.q.waitForClassifications()

	// Assert
	if got := standing(t, h, "t1"); err != nil || !heldAfterReconnectUnclassified(got) {
		t.Fatalf("CommitEdit = %v, t1 = %+v, want still held after reconnect, unclassified", err, got)
	}
}

func TestAnEditStandingAtTheReleaseWithholdsThePrompt(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	if err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp); err != nil {
		t.Fatalf("BeginEdit: %v", err)
	}

	// Act
	vendorServes(h)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing while the edit stands", got)
	}
}

func TestAnEditedHeldPromptIsDeliveredWithItsNewContentWhenTheVendorServes(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	if err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp); err != nil {
		t.Fatalf("BeginEdit: %v", err)
	}
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("edited")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}

	// Act
	vendorServes(h)

	// Assert
	turn, ok := h.db.startedTurn("t1")
	if !ok || turn.Text != "edited" {
		t.Fatalf("started turn = (%+v, %v), want t1 delivered with its edited text", turn, ok)
	}
}

func TestAForceReleaseOfAVendorBlockHoldIsRefused(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")

	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want ErrReleaseRefused", err)
	}
}

func TestTheVendorServingWhileNoSessionIsUpReleasesNothing(t *testing.T) {
	// Arrange: the block lifted, but the session went down meanwhile (an
	// agent-repl fault) -- the session's coming up releases the holds.
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	h.notStarted = true

	// Act
	vendorServes(h)

	// Assert
	if got := standing(t, h, "t1"); !heldAfterReconnectUnclassified(got) {
		t.Fatalf("t1 = %+v, want still held after reconnect", got)
	}
}

func TestTheSessionComingUpDeliversWhatTheVendorBlockHeld(t *testing.T) {
	// Arrange: the session went down under the block and came back, which
	// lifts the block (footer.OnSessionStarted).
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	h.footer.standVendorBlock("")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want [t1]", got)
	}
}

func TestHeldPromptsSurviveADaemonRestartAndGoWhenTheSessionComesUp(t *testing.T) {
	// Arrange: the holds are durable; a fresh queue over the same store is
	// the restarted daemon, whose adoption brings the session up.
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")
	restarted, err := newQueue(h.q.deps)
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	h.footer.standVendorBlock("")
	if err := restarted.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}

	// Act
	restarted.ReleaseReconnectHolds(theWorkspace)
	restarted.waitForClassifications()

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want [t1] after the restart", got)
	}
}

func TestASubmissionAfterTheBlockLiftsNeverOvertakesTheHeldPrompts(t *testing.T) {
	// Arrange: the block lifted, and its release has not run yet.
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	h.footer.standVendorBlock("")

	// Act
	submitAll(t, h, "t2")
	h.q.waitForClassifications()

	// Assert
	if got := h.sender.started(); len(got) != 1 || got[0] != "t1" {
		t.Fatalf("started = %v, want [t1]: the held prompt goes before the new one", got)
	}
}

func TestATurnEndUnderAVendorBlockHoldsTheWaitingPromptAfterReconnect(t *testing.T) {
	// Arrange: t1 was classified behind t0 before the block stood.
	h := newVendorBlockHarness(t)
	h.watcher.running("t0")
	submitAll(t, h, "t1")
	h.q.waitForClassifications()
	h.footer.standVendorBlock("usage_limit")
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "t0", wsm.CloseCompleted)

	// Assert
	got := standing(t, h, "t1")
	if got.Hold == nil || *got.Hold != wsm.HoldReconnect || len(h.sender.started()) != 0 {
		t.Fatalf("t1 = %+v, started = %v, want held after reconnect and not delivered", got, h.sender.started())
	}
}

func TestALeaseEndingUnderAVendorBlockHoldsItsPromptsAfterReconnect(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	submitAll(t, h, "t1")
	h.footer.standVendorBlock("usage_limit")
	h.clearLease()

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	got := standing(t, h, "t1")
	if got.Hold == nil || *got.Hold != wsm.HoldReconnect || len(h.sender.started()) != 0 {
		t.Fatalf("t1 = %+v, started = %v, want held after reconnect and not delivered", got, h.sender.started())
	}
}

func TestTheVendorServingAfterTheDaemonBeganExitingReleasesNothing(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	h.footer.standVendorBlock("usage_limit")
	submitAll(t, h, "t1")
	if !h.q.Drain(time.Second) {
		t.Fatalf("Drain did not finish")
	}

	// Act
	vendorServes(h)

	// Assert
	if got := h.sender.started(); len(got) != 0 {
		t.Fatalf("started = %v, want nothing released by an exiting daemon", got)
	}
}
