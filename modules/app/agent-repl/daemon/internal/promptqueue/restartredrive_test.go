package promptqueue

import (
	"context"
	"errors"
	"slices"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// sentOrigins answers every origin the fake shim was asked to start a turn with.
func (s *fakeSender) sentOrigins() []conversationv1.PromptOrigin {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]conversationv1.PromptOrigin(nil), s.origins...)
}

// sentText answers the text of the nth started turn.
func (s *fakeSender) sentText(n int) string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return saidText(s.said[n])
}

// owe records a resumption the workspace is owed, as a stand-down would.
func owe(t *testing.T, h *harness) {
	t.Helper()
	if err := h.db.OweResumption(context.Background(), theWorkspace, "cut-turn", instant); err != nil {
		t.Fatalf("OweResumption: %v", err)
	}
}

func TestATurnTheStandDownCutIsOwedAResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 1 {
		t.Fatalf("owed = %d, want the cut turn owed once", got)
	}
}

func TestAUserInterruptOwesNoResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d, want nothing owed for an interrupt with no stand-down", got)
	}
}

func TestATurnThatEndedOnItsOwnDuringAStandDownOwesNoResumption(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d, want nothing owed for a turn that completed", got)
	}
}

func TestAStandDownDeliversNothingHeldBehindTheCutTurn(t *testing.T) {
	// Arrange: a prompt queued behind the running turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("queued", "the next question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.standingDown.Store(true)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	for _, turn := range h.sender.started() {
		if turn == "queued" {
			t.Fatal("the queued prompt was started into a shim the stand-down is taking down")
		}
	}
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 1 || standing[0].Turn != "queued" {
		t.Fatalf("held = %+v, %v; want the queued prompt still held", standing, err)
	}
}

func TestTheServingSessionResumesWhatTheStandDownOwed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	origins := h.sender.sentOrigins()
	if len(origins) != 1 || origins[0] != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("started origins = %v, want one resumption after restart", origins)
	}
	if got := h.sender.sentText(0); got != resumptionInstruction {
		t.Fatalf("resumption text = %q, want the established instruction", got)
	}
}

func TestTheResumptionPaysTheDebt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 0 {
		t.Fatalf("owed = %d after the resumption, want 0", got)
	}
}

func TestASecondSessionUpResumesNothingTwice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)
	h.q.ReleaseReconnectHolds(theWorkspace)
	h.watcher.idle()

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := len(h.sender.started()); got != 1 {
		t.Fatalf("started = %d turns, want the one resumption", got)
	}
}

func TestTheResumptionDrawsNoPromptRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if rows := h.feed.mirrored(); len(rows) != 0 {
		t.Fatalf("mirrored rows = %d, want none: the resumption is under the hood", len(rows))
	}
}

func TestAServingSessionWithATurnRunningLeavesTheDebtOwed(t *testing.T) {
	// Arrange: an adopted shim whose turn survived.
	h := newHarness(t)
	owe(t, h)
	h.watcher.running("adopted-turn")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := h.db.owedCount(theWorkspace); got != 1 || len(h.sender.started()) != 0 {
		t.Fatalf("owed = %d, started = %v; want the debt left for a free session", got, h.sender.started())
	}
}

// resumeReports collects every account the queue gives of an owed resumption.
func resumeReports(h *harness) func() []ResumeReport {
	var mu sync.Mutex
	var got []ResumeReport
	h.q.deps.Resumptions = func(ws ids.WorkspaceID, report ResumeReport) {
		mu.Lock()
		defer mu.Unlock()
		got = append(got, report)
	}
	return func() []ResumeReport {
		mu.Lock()
		defer mu.Unlock()
		return append([]ResumeReport(nil), got...)
	}
}

// resumeKinds names the kinds of REPORTS in order.
func resumeKinds(reports []ResumeReport) []ResumeKind {
	out := make([]ResumeKind, 0, len(reports))
	for _, r := range reports {
		out = append(out, r.Kind)
	}
	return out
}

func TestAResumedTurnIsReportedStartingThenStarted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeStarting, ResumeStarted}) {
		t.Fatalf("reports = %v, want starting then started", got)
	}
}

func TestAReleaseWithNothingOwedIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestADebtLeftForARunningTurnIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.watcher.running("adopted-turn")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestAResumptionThatCannotBeClaimedIsReportedFailed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.db.resumeClaimErr = errors.New("database is locked")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	got := reports()
	if len(got) != 1 || got[0].Kind != ResumeFailed || !strings.Contains(got[0].Text, "database is locked") {
		t.Fatalf("reports = %+v, want one failure naming the cause", got)
	}
}

func TestAReleaseTheVendorStillRefusesIsReportedNotNow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	reports := resumeReports(h)
	owe(t, h)
	h.footer.standVendorBlock("rate_limited")

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}

func TestAReleaseIntoASessionThatDoesNotServeIsReportedNotNow(t *testing.T) {
	// Arrange
	h := revivedIntoTheGate(t)
	reports := resumeReports(h)
	owe(t, h)

	// Act
	h.q.ReleaseReconnectHolds(theWorkspace)

	// Assert
	if got := resumeKinds(reports()); !slices.Equal(got, []ResumeKind{ResumeNotNow}) {
		t.Fatalf("reports = %v, want not_now", got)
	}
}
