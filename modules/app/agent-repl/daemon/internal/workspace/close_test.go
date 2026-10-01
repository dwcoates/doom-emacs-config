package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

func TestCloseSucceedsOnAQuietWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1"); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if !f.db.closedFlags["w1"] {
		t.Fatal("Close() did not record the workspace as closed")
	}
}

func TestCloseRefusesATurnInFlight(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	asRefusal(t, err, "blocked")
}

func TestCloseRefusesLiveDetachedWork(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.running.LiveWork = sessionwatcher.LiveWorkSet{
		Agents: []*conversationv1.AgentId{{Value: "agent-1"}},
	}

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "live_work" {
		t.Fatalf("footer reason = %q, want live_work", f.footer.closing["w1"].Reason)
	}
}

func TestCloseRefusesHeldPrompts(t *testing.T) {
	// Arrange: undelivered user intent is never silently discarded.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.held["w1"] = []wsm.HeldPrompt{{Workspace: "w1", Turn: "t1"}}

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "held_prompts" {
		t.Fatalf("footer reason = %q, want held_prompts", f.footer.closing["w1"].Reason)
	}
}

func TestCloseIsAllowedWithAStandingColdGate(t *testing.T) {
	// Arrange: a standing cold gate holds no undelivered user intent, so it
	// deliberately does NOT block a close.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.footer.coldGates["w1"] = footer.ColdGate{Standing: true}

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseIsAllowedWithAFinishedMerge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.facts["w1"] = footer.MergeFacts{State: "merged"}

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseIsAllowedWithNoLiveSession(t *testing.T) {
	// Arrange: a parked session runs nothing, so it blocks nothing.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
}

func TestCloseManifestsTheRefusalInTheFooter(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	_ = f.verbs.Close(context.Background(), "w1")

	// Assert.
	blocked := f.footer.closing["w1"]
	if blocked == nil || blocked.Detail == "" {
		t.Fatalf("footer close refusal = %+v, want a drawn reason", blocked)
	}
}

func TestCloseRetiresTheFooterRefusalOnSuccess(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1"); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if f.footer.closing["w1"] != nil {
		t.Fatalf("footer close refusal = %+v, want it cleared", f.footer.closing["w1"])
	}
}

func TestCloseEvictsTheWorkspaceLogSink(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Close(context.Background(), "w1"); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if len(f.log.evicted) != 1 || f.log.evicted[0] != ws.Dir {
		t.Fatalf("evicted sinks = %v, want the workspace's own", f.log.evicted)
	}
}

func TestCloseLeavesTheSessionAlone(t *testing.T) {
	// Arrange: Close is view-level; Kill is what ends a session.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Close(context.Background(), "w1"); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 0 {
		t.Fatalf("stopped sessions = %+v, want none", f.fleet.stopped)
	}
}

func TestCloseSucceedsWhenTheSinkEvictionFails(t *testing.T) {
	// Arrange: a sink that will not release is a LEAK, not a reason to refuse a
	// close the record already says happened.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.log.evictErr = errors.New("the sink is already closed")

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if !f.db.closedFlags["w1"] {
		t.Fatal("Close() did not record the workspace as closed")
	}
}

func TestCloseRefusalIsNotLoggedAsAWarning(t *testing.T) {
	// Arrange: `blocked` is a LANDED CloseWorkspaceError arm, so the refusal is
	// an ordinary answer the client reads rather than a fault.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn

	// Act.
	_ = f.verbs.Close(context.Background(), "w1")

	// Assert.
	for _, record := range f.log.logger.Records() {
		if record.Operation == opClose && (record.Level == "warn" || record.Level == "error") {
			t.Fatalf("record = %+v, want the landed refusal recorded below warning", record)
		}
	}
}

func TestCloseRefusesAMergeWaitingInLine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.facts["w1"] = footer.MergeFacts{State: "queued", Step: footer.StepEnqueued, QueuePlace: 2, QueueWaiting: 2}

	// Act.
	err := f.verbs.Close(context.Background(), "w1")

	// Assert.
	asRefusal(t, err, "blocked")
	if f.footer.closing["w1"].Reason != "merge_queued" {
		t.Fatalf("footer reason = %q, want merge_queued", f.footer.closing["w1"].Reason)
	}
}
