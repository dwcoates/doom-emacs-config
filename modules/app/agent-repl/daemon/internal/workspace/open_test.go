package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

func TestOpenStartsTheSession(t *testing.T) {
	// Arrange: mounting a parked workspace IS an implicit revival.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 {
		t.Fatalf("sessions started = %d, want exactly one", len(f.fleet.started))
	}
}

func TestOpenIsIdempotentWhenTheSessionIsAlreadyLive(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Open(context.Background(), "w1"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 0 {
		t.Fatalf("sessions started = %d, want none for an already-live session", len(f.fleet.started))
	}
}

func TestOpenClearsTheClosedFlag(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())
	ws.Closed = true
	f.db.with(ws)

	// Act.
	if err := f.verbs.Open(context.Background(), "w1"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if closed, ok := f.db.closedFlags["w1"]; !ok || closed {
		t.Fatalf("closed flag = (%v, %v), want it cleared", closed, ok)
	}
}

func TestOpenRetiresAStandingCloseRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if blocked, ok := f.footer.closing["w1"]; !ok || blocked != nil {
		t.Fatalf("footer close refusal = %v, want it cleared", blocked)
	}
}

func TestOpenRunsTheBuildStalenessCheck(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Open(context.Background(), "w1"); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Assert.
	if len(f.rollout.relaunches) != 1 || f.rollout.relaunches[0].Reason != rollout.ReasonBuildStale {
		t.Fatalf("relaunches = %+v, want one build-staleness bounce", f.rollout.relaunches)
	}
}

func TestOpenSurvivesAFailedBuildStalenessCheck(t *testing.T) {
	// Arrange: the session is up and usable on the older build, so a bounce
	// that will not run is a warning rather than a failed mount.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.rollout.relaunchErr = errors.New("the shim is busy")

	// Act.
	err := f.verbs.Open(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
}

func TestOpenSurfacesABringUpFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.startErr = errors.New("the shim exited during bring-up")

	// Act.
	err := f.verbs.Open(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the bring-up failure surfaced")
	}
}

func TestOpenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Open(context.Background(), "nope")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestCloseBlockerReportsATurnInFlight(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.Reason != "turn_in_flight" {
		t.Fatalf("blocker = %+v, want turn_in_flight", blocked)
	}
}

func TestCloseBlockerReportsNothingWhenQuiet(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil || blocked != nil {
		t.Fatalf("closeBlocker() = (%+v, %v), want quiet", blocked, err)
	}
}

func TestMergeIsPendingOnlyWhileAMergeStillOwesWork(t *testing.T) {
	tests := []struct {
		state string
		want  bool
	}{
		{state: "none", want: false},
		{state: "enqueuing", want: true},
		{state: "queued", want: true},
		{state: "merging", want: true},
		{state: "conflict", want: true},
		{state: "failed", want: false},
		{state: "merged", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.state, func(t *testing.T) {
			// Arrange in the table. Act.
			got := mergeIsPending(tt.state)
			// Assert.
			if got != tt.want {
				t.Fatalf("mergeIsPending(%q) = %v, want %v", tt.state, got, tt.want)
			}
		})
	}
}

// TestCloseBlockerCountsTheLiveWorkItems is landing 7's evidence half: the
// blocker carries the live-work COUNT, not only the sentence.
func TestCloseBlockerCountsTheLiveWorkItems(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
		Shells: []*conversationv1.DetachedWorkId{{Value: "w-1"}, {Value: "w-2"}},
	}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.LiveWork != 3 {
		t.Fatalf("blocker = %+v, want live_work 3", blocked)
	}
}

// TestCloseBlockerCountsTheHeldPromptsBehindALeadingBlocker is the whole-picture
// half: every blocker is computed, so evidence for one that is NOT the leading
// reason still rides the refusal.
func TestCloseBlockerCountsTheHeldPromptsBehindALeadingBlocker(t *testing.T) {
	// Arrange: a turn in flight leads, with a held prompt behind it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.db.held["w1"] = []wsm.HeldPrompt{{Workspace: "w1", Turn: "t2"}}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || blocked.Reason != "turn_in_flight" || blocked.HeldPrompts != 1 {
		t.Fatalf("blocker = %+v, want turn_in_flight leading with held_prompts 1", blocked)
	}
}

// TestCloseBlockerReportsAQueuedMergeBehindALeadingBlocker is the same for the
// merge blocker.
func TestCloseBlockerReportsAQueuedMergeBehindALeadingBlocker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.merge.facts["w1"] = footer.MergeFacts{State: "queued"}

	// Act.
	blocked, err := f.verbs.(*verbs).closeBlocker(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("closeBlocker: %v", err)
	}
	if blocked == nil || !blocked.MergeQueued {
		t.Fatalf("blocker = %+v, want merge_queued evidence", blocked)
	}
}
