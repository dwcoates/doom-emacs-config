package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// runningTurn arranges an open turn with the given number of live detached
// agents, which is the axis the confirm challenge turns on.
func runningTurn(f *fixture, agents int) {
	turn := wsm.TurnID("t1")
	f.running.Turn = &turn
	f.running.LiveWork = sessionwatcher.LiveWorkSet{}
	for i := 0; i < agents; i++ {
		f.running.LiveWork.Agents = append(f.running.LiveWork.Agents,
			&conversationv1.AgentId{Value: string(rune('a' + i))})
	}
}

func TestInterruptTurnWithNoAgentsKillsUnforced(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !outcome.Turn {
		t.Fatalf("outcome = %+v, want the interrupted-turn arm", outcome)
	}
	if len(f.shim.killedTurns) != 1 || f.shim.killedTurns[0].Force {
		t.Fatalf("killed turns = %+v, want one unforced kill", f.shim.killedTurns)
	}
}

func TestInterruptTurnWithLiveAgentsRaisesTheConfirmChallenge(t *testing.T) {
	// Arrange: killing the turn would take the detached agents with it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 3)

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	var challenge *ConfirmRequired
	if !errors.As(err, &challenge) {
		t.Fatalf("error = %v, want the confirm_required challenge", err)
	}
	if challenge.LiveAgentCount != 3 {
		t.Fatalf("live agent count = %d, want 3", challenge.LiveAgentCount)
	}
}

func TestInterruptTurnWithLiveAgentsKillsNothingUnconfirmed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 2)

	// Act.
	_, _ = f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if len(f.shim.killedTurns) != 0 {
		t.Fatalf("killed turns = %+v, want none before confirmation", f.shim.killedTurns)
	}
}

func TestInterruptTurnConfirmedForcesTheKill(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 2)

	// Act.
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, true); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if len(f.shim.killedTurns) != 1 || !f.shim.killedTurns[0].Force {
		t.Fatalf("killed turns = %+v, want one forced kill", f.shim.killedTurns)
	}
}

func TestInterruptTurnFiresTheFooterStatusImmediately(t *testing.T) {
	// Arrange: the status fires the MOMENT the interrupt registers, not at the
	// turn's real end.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)

	// Act.
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if !f.footer.interrupting["w1"] {
		t.Fatal("the footer's interrupting status was not raised")
	}
}

func TestInterruptTurnRetiresTheFooterStatusWhenTheKillFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = errors.New("no turn open")

	// Act.
	_, _ = f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if f.footer.interrupting["w1"] {
		t.Fatal("the footer's interrupting status was left raised after a failed kill")
	}
}

func TestInterruptTurnRaisesTheMergeDequeueOffer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)

	// Act.
	if _, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if len(f.merge.interrupted) != 1 {
		t.Fatalf("merge interrupts = %v, want exactly one", f.merge.interrupted)
	}
}

func TestInterruptTurnWithNothingRunningAnswersNothingRunning(t *testing.T) {
	// Arrange: "nothing was running" is a SUCCESS answer.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !outcome.NothingRunning {
		t.Fatalf("outcome = %+v, want the nothing-running arm", outcome)
	}
}

func TestInterruptWithNoLiveSessionAnswersNothingRunning(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if err != nil || !outcome.NothingRunning {
		t.Fatalf("Interrupt() = (%+v, %v), want the nothing-running arm", outcome, err)
	}
}

func TestInterruptDetachedSubagentStopsTheAgent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedSubagent, ID: "agent-7"}}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if outcome.DetachedCount != 1 || len(f.shim.stoppedAgents) != 1 || f.shim.stoppedAgents[0] != "agent-7" {
		t.Fatalf("stopped agents = %v (count %d), want agent-7", f.shim.stoppedAgents, outcome.DetachedCount)
	}
}

func TestInterruptDetachedShellStopsTheShell(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedShell, ID: "work-3"}}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if outcome.DetachedCount != 1 || len(f.shim.stoppedShells) != 1 || f.shim.stoppedShells[0] != "work-3" {
		t.Fatalf("stopped shells = %v (count %d), want work-3", f.shim.stoppedShells, outcome.DetachedCount)
	}
}

func TestInterruptDetachedRefusesARowThatIsNotDetachedWork(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "act-1"}}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	asRefusal(t, err, ArmUnservedAnswer)
}

func TestInterruptAllAgentsStopsEveryLiveAgent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 3)

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if outcome.DetachedCount != 3 || len(f.shim.stoppedAgents) != 3 {
		t.Fatalf("stopped agents = %v (count %d), want three", f.shim.stoppedAgents, outcome.DetachedCount)
	}
}

func TestInterruptAllAgentsWithNoneLiveAnswersNothingRunning(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	if err != nil || !outcome.NothingRunning {
		t.Fatalf("Interrupt() = (%+v, %v), want the nothing-running arm", outcome, err)
	}
}

func TestInterruptAllAgentsSurfacesAStopFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 2)
	f.shim.stopAgentErr = errors.New("unknown agent")

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	if err == nil {
		t.Fatal("Interrupt() = nil error, want the stop failure surfaced")
	}
}

func TestInterruptRefusesATargetThatNamesNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{}, false)

	// Assert.
	if err == nil {
		t.Fatal("Interrupt(empty target) = nil error, want a failure")
	}
}

func TestConfirmRequiredRendersTheIntendedArm(t *testing.T) {
	// Arrange. Act.
	got := (&ConfirmRequired{LiveAgentCount: 4}).Error()

	// Assert.
	want := "intended arm: InterruptError.confirm_required: 4 detached agents are live"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}
