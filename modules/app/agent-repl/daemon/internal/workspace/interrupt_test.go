package workspace

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

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

func TestInterruptDetachedAgentPropagatesNotDeliverable(t *testing.T) {
	// Arrange: landing 3's SDK limit is answered HONESTLY — the caller learns
	// there is no route, rather than seeing a generic failure.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.stopAgentErr = &ShimRefusal{
		Verb: "UpdateAgent", Arm: ArmShimNotDeliverable, Detail: "no SDK route to a subagent",
	}
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedSubagent, ID: "agent-7"}}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	refusal := asRefusal(t, err, ArmShimNotDeliverable)
	if refusal.Rpc != "Interrupt" {
		t.Fatalf("refusal rpc = %q, want Interrupt", refusal.Rpc)
	}
}

func TestInterruptDetachedAgentAnswersNothingRunningOnABenignRefusal(t *testing.T) {
	// Arrange: the agent finished on its own between the click and the stop.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.stopAgentErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimNothingRunning}
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedSubagent, ID: "agent-7"}}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !outcome.NothingRunning {
		t.Fatalf("outcome = %+v, want the nothing-running arm", outcome)
	}
}

func TestInterruptDetachedAgentPropagatesAStaleRow(t *testing.T) {
	// Arrange: "the row you clicked is stale" is a different answer from "the
	// SDK has no route", so it keeps its own arm.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.stopAgentErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimUnknownAgent}
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedSubagent, ID: "agent-7"}}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	asRefusal(t, err, ArmShimUnknownAgent)
}

func TestInterruptDetachedShellAnswersNothingRunningWhenAlreadyEnded(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.stopBashErr = &ShimRefusal{Verb: "StopBash", Arm: ArmShimAlreadyEnded}
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedShell, ID: "work-3"}}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !outcome.NothingRunning {
		t.Fatalf("outcome = %+v, want the nothing-running arm", outcome)
	}
}

func TestInterruptDetachedShellPropagatesUnknownWork(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.stopBashErr = &ShimRefusal{Verb: "StopBash", Arm: ArmShimUnknownWork}
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindDetachedShell, ID: "work-3"}}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	asRefusal(t, err, ArmShimUnknownWork)
}

func TestInterruptTurnAnswersNothingRunningWhenTheTurnAlreadyEnded(t *testing.T) {
	// Arrange: the turn ended between the freeness read and the kill.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimNoTurnOpen}

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

func TestInterruptTurnPropagatesAnUninterruptibleTurn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimTurnLive, Detail: "the query will not stop"}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	asRefusal(t, err, ArmShimTurnLive)
}

func TestInterruptTurnRetiresTheFooterStatusOnAShimRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimTurnLive}

	// Act.
	_, _ = f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	if f.footer.interrupting["w1"] {
		t.Fatal("the footer's interrupting status was left raised after a refused kill")
	}
}

func TestInterruptAllAgentsSkipsAnAgentThatAlreadyFinished(t *testing.T) {
	// Arrange: one agent finishing mid-sweep is not a failure of the sweep.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 2)
	f.shim.stopAgentErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimNothingRunning}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if outcome.DetachedCount != 0 {
		t.Fatalf("outcome = %+v, want no agents counted as stopped", outcome)
	}
}

func TestInterruptAllAgentsPropagatesANonBenignRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 2)
	f.shim.stopAgentErr = &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimNoSession}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	asRefusal(t, err, ArmShimNoSession)
}

// TestInterruptTurnOnAnIdleQueuedWorkspaceStillRaisesTheDequeueOffer covers the
// case the offer exists for: a workspace whose merge is QUEUED and whose turn
// has already ended. The interrupt has no turn to kill, and the user is asking
// about the merge.
func TestInterruptTurnOnAnIdleQueuedWorkspaceStillRaisesTheDequeueOffer(t *testing.T) {
	// Arrange: a live session with nothing running.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	out, err := f.verbs.Interrupt(context.Background(), ws.ID, InterruptTarget{Turn: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !out.NothingRunning {
		t.Fatalf("Interrupt = %+v, want nothing_running", out)
	}
	if len(f.merge.interrupted) != 1 || f.merge.interrupted[0] != ws.ID {
		t.Fatalf("merge interrupts = %v, want the dequeue offer raised once", f.merge.interrupted)
	}
}

// TestInterruptTurnWithOnlyALiveShellRaisesNoChallenge pins what
// `live_agent_count` counts: AGENTS. A detached shell dies with the query like
// everything else, and the confirmed interrupt still stops it, but it is not
// an agent and the user is not challenged over one.
func TestInterruptTurnWithOnlyALiveShellRaisesNoChallenge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.running.LiveWork.Shells = []*conversationv1.DetachedWorkId{{Value: "work-1"}}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	var challenge *ConfirmRequired
	if errors.As(err, &challenge) {
		t.Fatalf("error = %v, want no challenge over a detached shell", err)
	}
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if !outcome.Turn {
		t.Fatalf("outcome = %+v, want the interrupted-turn arm", outcome)
	}
}

// TestInterruptTurnAnswersShimRefusedForATransportFailure covers the
// fallthrough: the shim would not perform the kill and the failure carried no
// arm at all, so the caller gets the contract's relay arm rather than a raw
// transport error it cannot act on.
func TestInterruptTurnAnswersShimRefusedForATransportFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = connect.NewError(connect.CodeInternal, errors.New("the vendor refused the kill"))

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	refusal := asRefusal(t, err, ArmShimRefused)
	if refusal.Reason != "the vendor refused the kill" {
		t.Fatalf("shim_refused detail = %q, want the shim's own words", refusal.Reason)
	}
}

// TestInterruptTurnAnswersShimRefusedForAFailureWithNoArm covers the other half
// of the fallthrough: a typed KillTurnFailure whose kind oneof is unset names
// no landed arm, so it relays as shim_refused instead of an unlanded arm.
func TestInterruptTurnAnswersShimRefusedForAFailureWithNoArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimUnspecified, Detail: "no cause was named"}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	asRefusal(t, err, ArmShimRefused)
}
