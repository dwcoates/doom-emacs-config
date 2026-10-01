package promptqueue

import (
	"context"
	"errors"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestSubmitHoldsUnderTheMergeLeaseUnclassified covers a prompt submitted
// while a merge runs: held by the merge, never classified, and reported held.
func TestSubmitHoldsUnderTheMergeLeaseUnclassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Held == nil || *got.Held != wsm.HoldMerge || got.Classification != nil {
		t.Fatalf("disposition = %+v, want an unclassified merge hold", got)
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("a submission under a merge reached the shim")
	}
}

// TestSubmitPassesTheMergesOwnBriefsUnderItsLease covers the exemption: the
// merge's own brief is delivered, not held behind the merge that waits on it.
func TestSubmitPassesTheMergesOwnBriefsUnderItsLease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	sub := submission("t1", "repair the conflicts")
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR

	// Act
	got, err := h.q.Submit(context.Background(), sub)

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Held != nil {
		t.Fatalf("disposition = %+v, want the brief delivered", got)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the brief", started)
	}
}

// TestSubmitTakesTheOrdinaryPathWhenTheMergeReleasesAsItHolds covers the
// release winning the race: the store refuses the merge hold, the submission
// is delivered as a workspace with no merge delivers it, and nothing is an
// error.
func TestSubmitTakesTheOrdinaryPathWhenTheMergeReleasesAsItHolds(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	h.db.beforeHoldWrite = func(d *fakeDB) { delete(d.leases, theWorkspace) }

	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "hello"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Held != nil {
		t.Fatalf("disposition = %+v, want the ordinary path", got)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the prompt delivered", started)
	}
	if !logged(h.log.Records(), "info", opSubmit, "the merge released its lease as the prompt was held; the submission takes the ordinary path") {
		t.Fatalf("records = %+v, want the lost race recorded at info", h.log.Records())
	}
	for _, r := range h.log.Records() {
		if r.Level == "error" {
			t.Fatalf("the lost race was recorded as an error: %+v", r)
		}
	}
}

// TestOnLeaseChangedStampsUserHoldsAndNotTheMergesBriefs covers the restamp a
// merge lease makes: the user's standing hold waits for the merge, and the
// merge's own brief does not.
func TestOnLeaseChangedStampsUserHoldsAndNotTheMergesBriefs(t *testing.T) {
	// Arrange: one user prompt and one merge brief held behind a running turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t-user", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	brief := submission("t-brief", "repair the tests")
	brief.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR
	if _, err := h.q.Submit(context.Background(), brief); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.lease(wsm.HolderMerge, wsm.PolicyHold)

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if user := h.db.held["t-user"]; user.Hold == nil || *user.Hold != wsm.HoldMerge {
		t.Fatalf("the user's hold = %v, want merge", user.Hold)
	}
	if b := h.db.held["t-brief"]; b.Hold != nil {
		t.Fatalf("the merge's brief was stamped %v, want it left deliverable", *b.Hold)
	}
}

// TestOnLeaseChangedLeavesAStampTheReleaseOutranToTheRelease covers the
// restamp losing the race: the store refuses, nothing is an error, and the
// hold is left for the release's own evaluation.
func TestOnLeaseChangedLeavesAStampTheReleaseOutranToTheRelease(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	h.db.beforeHoldWrite = func(d *fakeDB) { delete(d.leases, theWorkspace) }

	// Act
	h.q.OnLeaseChanged(theWorkspace)

	// Assert
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if held := h.db.held["t1"]; held.Hold != nil {
		t.Fatalf("hold = %v, want the refused stamp absent", *held.Hold)
	}
	if !logged(h.log.Records(), "info", opLeaseChange, "the merge released its lease before the hold was stamped; the release re-evaluates it") {
		t.Fatalf("records = %+v, want the lost race recorded at info", h.log.Records())
	}
}

// TestReleaseRefusesAMergeHold covers Send now on a prompt the merge holds: the
// merge drives the session, so the release is refused and the prompt waits.
func TestReleaseRefusesAMergeHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderMerge, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")

	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("Release = %v, want ErrReleaseRefused", err)
	}
	if len(h.sender.started()) != 0 {
		t.Fatal("a released merge hold reached the shim")
	}
}

// TestATurnEndUnderAHoldingLeaseDeliversOnlyWhatItExempts covers the instant
// between a lease being taken and its restamp: an unstamped user prompt is not
// delivered into the session the merge drives, and the merge's own brief is.
func TestATurnEndUnderAHoldingLeaseDeliversOnlyWhatItExempts(t *testing.T) {
	tests := []struct {
		name        string
		origin      conversationv1.PromptOrigin
		wantStarted []ids.TurnID
	}{
		{"the user's prompt waits", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil},
		{"the merge's brief is delivered", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, []ids.TurnID{"t-held"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a prompt held behind a running turn, then a merge lease
			// taken with no restamp yet.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			sub := submission("t-held", "next")
			sub.Origin = tt.origin
			if _, err := h.q.Submit(context.Background(), sub); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			h.lease(wsm.HolderMerge, wsm.PolicyHold)

			// Act
			h.watcher.idle()
			h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

			// Assert
			if got := h.sender.started(); !slices.Equal(got, tt.wantStarted) {
				t.Fatalf("started = %v, want %v", got, tt.wantStarted)
			}
		})
	}
}
