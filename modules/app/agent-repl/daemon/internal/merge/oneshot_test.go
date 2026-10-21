package merge

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/semver"
	"agent-repld/internal/wsm"
)

func TestAOneShotRequestMarksTheRequesterAndRepublishesTheRoster(t *testing.T) {
	// Arrange: the announcing turn is still in flight.
	h := newHarness(t)
	h.inFlight = "turn-announcing"

	// Act.
	err := h.requestOneShot(t, ownBranch, false)

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if state, ok := h.entryState(); !ok || state != wsm.MergeRequested {
		t.Fatalf("queue entry = (%v, %v), want a request waiting on the turn", state, ok)
	}
	if !h.oneShot() {
		t.Fatal("the requester was not marked a one-shot")
	}
	if got := h.publishes(); got != 1 {
		t.Fatalf("roster republishes = %d, want 1", got)
	}
	record, ok := h.recordFor("info", "daemon.merge.enqueue")
	if !ok || record.Context["one_shot"] != true {
		t.Fatalf("the request record = %+v (found %v), want one_shot true", record, ok)
	}
}

func TestAOneShotMergeClosesTheRequesterOnceItLands(t *testing.T) {
	// Arrange: a one-shot request put in line, whose merge lands.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	h.git.tagsMerged = []string{"0.0.1"}
	// An agent's merge onto agent-repl's master names its version increment.
	source := ownBranch
	source.Version = &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}
	if err := h.requestOneShot(t, source, false); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.runWait(t)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if !h.db.closed[theWorkspace] {
		t.Fatal("the one-shot requester was not closed once its merge landed")
	}
}

func TestAOneShotRequestThatContradictsItselfIsAnErrorAndRecordsNothing(t *testing.T) {
	tests := []struct {
		name     string
		source   wsm.MergeSource
		detached bool
	}{
		{name: "kept open", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}},
		{name: "detached", source: ownBranch, detached: true},
		{name: "another branch", source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "feature"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			err := h.requestOneShot(t, tt.source, tt.detached)

			// Assert.
			if _, refused := Refused(err); err == nil || refused {
				t.Fatalf("Enqueue = %v, want a defect error", err)
			}
			if record, logged := h.recordFor("error", "daemon.merge.enqueue"); !logged || record.Context["one_shot"] != true {
				t.Fatalf("the defect was not recorded at ERROR with one_shot: %+v", h.logs.Records())
			}
			if _, ok := h.entryState(); ok {
				t.Fatal("a contradicting one-shot request was recorded")
			}
			if h.oneShot() || h.publishes() != 0 {
				t.Fatalf("one-shot = %v, republishes = %d, want neither", h.oneShot(), h.publishes())
			}
		})
	}
}

func TestARefusedOneShotRequestLeavesTheRequesterUnmarked(t *testing.T) {
	// Arrange: the requester's merge is already requested.
	h := newHarness(t)
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	err := h.requestOneShot(t, ownBranch, false)

	// Assert.
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmAlreadyQueued {
		t.Fatalf("Enqueue = %v, want already_queued", err)
	}
	if h.oneShot() || h.publishes() != 0 {
		t.Fatalf("one-shot = %v, republishes = %d, want neither", h.oneShot(), h.publishes())
	}
}

func TestAOneShotRequestWhoseRosterRepublishFailsStandsAndSaysSo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.publishErr = errors.New("registry read failed")

	// Act.
	err := h.requestOneShot(t, ownBranch, false)

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue = %v, want the request to stand", err)
	}
	if !h.oneShot() {
		t.Fatal("the requester was not marked a one-shot")
	}
	record, logged := h.recordFor("error", "daemon.merge.enqueue")
	if !logged || record.Context["error"] != "registry read failed" || record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("the failed republish was not recorded at ERROR with its cause: %+v", h.logs.Records())
	}
}
