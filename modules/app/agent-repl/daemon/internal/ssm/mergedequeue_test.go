package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/merge"
)

// Every case reads the offer off mustCurrent rather than off the map directly:
// the claim being made is that the ONE WorkspaceState construction funnel
// stamps it, so a test that read the retained value would pass with the funnel
// unwired and the card would never appear.

func waitingStanding() merge.Standing {
	return merge.Standing{Repo: "/repos/alpha/.git", RunID: "run-waiting", Position: 3, Depth: 5}
}

func headStanding() merge.Standing {
	return merge.Standing{Repo: "/repos/alpha/.git", RunID: "run-head", Position: 1, Depth: 2, Head: true}
}

// A workspace nobody interrupted carries NO offer. Absence is what tells a
// frontend there is no card to draw, so a zero-valued one standing in for it
// would put an unanswerable question on every workspace at once.
func TestNoDequeueOfferWithoutAnInterrupt(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Act.
	got := mustCurrent(t, m, "ws1")

	// Assert.
	if got.GetMergeDequeueOffer() != nil {
		t.Fatalf("merge_dequeue_offer = %v, want none for a workspace nobody asked about", got.GetMergeDequeueOffer())
	}
}

// A WAITING standing becomes the waiting arm, carrying how many merges are
// ahead of it — the fact the card's message turns on.
func TestARaisedOfferCarriesTheWaitingStanding(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)

	// Act.
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Assert.
	offer := mustDequeueOffer(t, m, "ws1")
	if offer.GetRunId() != "run-waiting" {
		t.Fatalf("run_id = %q, want run-waiting", offer.GetRunId())
	}
	waiting := offer.GetWaiting()
	if waiting == nil {
		t.Fatalf("offer standing = %T, want the waiting arm", offer.GetStanding())
	}
	if waiting.GetAhead() != 2 || waiting.GetPosition() != 3 || waiting.GetDepth() != 5 {
		t.Fatalf("waiting = ahead %d, %d of %d; want ahead 2, 3 of 5",
			waiting.GetAhead(), waiting.GetPosition(), waiting.GetDepth())
	}
	if offer.GetOfferId() == "" {
		t.Fatal("offer_id is empty; an offer nothing can name is a question the user cannot answer")
	}
	if offer.GetRaisedAtMs() == 0 {
		t.Fatal("raised_at_ms is zero, want the instant the question went up")
	}
}

// A HEAD standing becomes the running arm, carrying the run's own MergeStatus
// so the card names the stage from the one vocabulary that defines it.
func TestARaisedOfferCarriesTheRunningStatus(t *testing.T) {
	// Arrange — a run mid-cherry-pick.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.ApplyMergeStatus("ws1", string(merge.PhaseMerging), "cherry-picking 3/7",
		pipelineStatus("run-head", 7, 3, "abc123def456", "fix the parser")); err != nil {
		t.Fatalf("ApplyMergeStatus: %v", err)
	}

	// Act.
	if _, err := m.RaiseMergeDequeueOffer("ws1", headStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Assert.
	offer := mustDequeueOffer(t, m, "ws1")
	running := offer.GetRunning()
	if running == nil {
		t.Fatalf("offer standing = %T, want the running arm", offer.GetStanding())
	}
	picking := running.GetStatus().GetCherryPicking()
	if picking == nil {
		t.Fatalf("running status phase = %T, want the cherry-picking phase the run published", running.GetStatus().GetPhase())
	}
	if picking.GetCommitsLanded() != 3 || picking.GetCommitsTotal() != 7 {
		t.Fatalf("running status = %d/%d commits, want 3/7", picking.GetCommitsLanded(), picking.GetCommitsTotal())
	}
}

// A head observed before its run has published anything carries the running arm
// with NO status. It is not an error and not a placeholder: the merge really is
// running and really has said nothing yet, and the arm alone is enough for the
// card to say so.
func TestARaisedOfferOnASilentHeadCarriesTheArmWithoutAStatus(t *testing.T) {
	// Arrange — the merge axis says merging, but no run has published.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Act.
	if _, err := m.RaiseMergeDequeueOffer("ws1", headStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Assert.
	offer := mustDequeueOffer(t, m, "ws1")
	if offer.GetRunning() == nil {
		t.Fatalf("offer standing = %T, want the running arm", offer.GetStanding())
	}
	if offer.GetRunning().GetStatus() != nil {
		t.Fatalf("running status = %v, want none from a run that has published nothing", offer.GetRunning().GetStatus())
	}
}

// A SECOND raise keeps the offer id and refreshes the standing. Interrupting
// twice is ordinary, and a rival question would leave two cards whose answers
// race; the refresh is also what keeps a card honest as the queue advances.
func TestASecondRaiseRefreshesTheStandingAndKeepsTheOfferID(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer(first): %v", err)
	}
	first := mustDequeueOffer(t, m, "ws1").GetOfferId()

	// Act — the merge has advanced to second in line.
	advanced := waitingStanding()
	advanced.Position, advanced.Depth = 2, 4
	if _, err := m.RaiseMergeDequeueOffer("ws1", advanced); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer(second): %v", err)
	}

	// Assert.
	offer := mustDequeueOffer(t, m, "ws1")
	if offer.GetOfferId() != first {
		t.Fatalf("offer_id = %q after a second raise, want the original %q", offer.GetOfferId(), first)
	}
	if got := offer.GetWaiting().GetAhead(); got != 1 {
		t.Fatalf("ahead = %d after the refresh, want 1", got)
	}
}

// Clearing takes the card down, which is the only way it comes down.
func TestClearingTheOfferTakesTheCardDown(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Act.
	had, err := m.ClearMergeDequeueOffer("ws1", "answered:keep")

	// Assert.
	if err != nil || !had {
		t.Fatalf("ClearMergeDequeueOffer() = (%v, %v), want (true, nil)", had, err)
	}
	if got := mustCurrent(t, m, "ws1").GetMergeDequeueOffer(); got != nil {
		t.Fatalf("merge_dequeue_offer = %v after the clear, want none", got)
	}
}

// Clearing a workspace with no question is a benign no-op, not an error: an
// answer racing a merge that ended on its own is an ordinary sequence.
func TestClearingAnAbsentOfferIsANoOp(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	had, err := m.ClearMergeDequeueOffer("ws1", "answered:keep")

	// Assert.
	if err != nil || had {
		t.Fatalf("ClearMergeDequeueOffer() = (%v, %v), want (false, nil)", had, err)
	}
}

// THE MERGE'S OWN END ANSWERS THE QUESTION, one row per terminal. A card
// offering to dequeue a merge that is already gone is a question about nothing.
func TestAMergeReachingItsOwnEndTakesTheCardDown(t *testing.T) {
	tests := []struct {
		name  string
		token string
	}{
		{"merged", sigMerged},
		{"failed", sigMergeFailed},
		{"axis cleared", sigMergeNone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange — a session row too, so the workspace still resolves
			// once the merge axis is cleared and the frame can be read back.
			m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
			if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
				t.Fatalf("Apply: %v", err)
			}
			applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
			if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
				t.Fatalf("RaiseMergeDequeueOffer: %v", err)
			}

			// Act.
			if err := m.ApplyMergeTransition("ws1", tc.token, "test arrangement"); err != nil {
				t.Fatalf("ApplyMergeTransition(%s): %v", tc.token, err)
			}

			// Assert.
			if got := mustCurrent(t, m, "ws1").GetMergeDequeueOffer(); got != nil {
				t.Fatalf("merge_dequeue_offer = %v after %s, want none", got, tc.token)
			}
			if _, ok := m.MergeDequeueOfferID("ws1"); ok {
				t.Fatalf("an offer is still outstanding after %s, want none", tc.token)
			}
		})
	}
}

// A merge transition that is NOT an ending leaves the question standing: the
// merge is still on the queue, so the offer is still about something.
func TestANonTerminalMergeTransitionLeavesTheCardUp(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Act — the merge reached the head and started picking.
	applyPhases(t, m, "ws1", merge.PhaseMerging)

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetMergeDequeueOffer(); got == nil {
		t.Fatal("merge_dequeue_offer is absent after a non-terminal transition, want the question still standing")
	}
}

// The retained offer rides EVERY later frame, not just the one its raise
// pushed. A card that vanished on the next unrelated push would flicker off a
// question nobody answered.
func TestTheRetainedOfferRidesEveryLaterFrame(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Act — an unrelated push for the same workspace.
	if err := applyTest(m, evSessionStarted("s1", 2)); err != nil {
		t.Fatalf("Apply: %v", err)
	}

	// Assert.
	if got := mustDequeueOffer(t, m, "ws1").GetRunId(); got != "run-waiting" {
		t.Fatalf("run_id = %q on a later frame, want the retained run-waiting", got)
	}
}

// MergeDequeueOfferID is what an answer is checked against, so it must report
// exactly what was published and nothing when nothing is up.
func TestMergeDequeueOfferIDReportsTheOutstandingQuestion(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	applyPhases(t, m, "ws1", merge.PhaseMergeQueued)
	if _, ok := m.MergeDequeueOfferID("ws1"); ok {
		t.Fatal("an offer id is reported before anything was raised")
	}
	if _, err := m.RaiseMergeDequeueOffer("ws1", waitingStanding()); err != nil {
		t.Fatalf("RaiseMergeDequeueOffer: %v", err)
	}

	// Act.
	got, ok := m.MergeDequeueOfferID("ws1")

	// Assert.
	if !ok {
		t.Fatal("MergeDequeueOfferID reported nothing outstanding after a raise")
	}
	if want := mustDequeueOffer(t, m, "ws1").GetOfferId(); got != want {
		t.Fatalf("MergeDequeueOfferID = %q, want the published %q", got, want)
	}
}

// An unnamed workspace is a construction bug at the call site, refused rather
// than filed under the empty string.
func TestRaisingAnOfferForAnEmptyWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	offer, err := m.RaiseMergeDequeueOffer("", waitingStanding())

	// Assert.
	if err == nil || offer != nil {
		t.Fatalf("RaiseMergeDequeueOffer(\"\") = (%v, %v), want (nil, error)", offer, err)
	}
}

// mustDequeueOffer returns the workspace's stamped dequeue offer, failing when
// the frame carries none.
func mustDequeueOffer(t *testing.T, m *Manager, ws string) *frontendv1.MergeDequeueOffer {
	t.Helper()
	offer := mustCurrent(t, m, ws).GetMergeDequeueOffer()
	if offer == nil {
		t.Fatalf("WorkspaceState for %s carries no merge_dequeue_offer", ws)
	}
	return offer
}
