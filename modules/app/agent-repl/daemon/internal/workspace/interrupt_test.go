package workspace

import (
	"context"
	"errors"
	"fmt"
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
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindShellHead, ID: "work-3"}}

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
	asRefusal(t, err, ArmNotDetachedWork)
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
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindShellHead, ID: "work-3"}}

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
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindShellHead, ID: "work-3"}}

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

func TestInterruptTurnAnswersShimRefusedWhenTheShimNamesAnotherOpenTurn(t *testing.T) {
	// Arrange: the shim's open turn is not the one the daemon believes runs.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn, Detail: "turn t2 is open"}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert: the typed shim_refused arm, never a nothing-running success.
	asRefusal(t, err, ArmShimRefused)
}

func TestInterruptTurnCarriesTheShimsWordsWhenTheShimNamesAnotherOpenTurn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn, Detail: "turn t2 is open"}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Reason != "turn t2 is open" {
		t.Fatalf("Interrupt = %v, want the shim's own detail as the refusal's reason", err)
	}
}

func TestInterruptTurnRecordsAnErrorWhenTheShimNamesAnotherOpenTurn(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	f.shim.killTurnErr = &ShimRefusal{Verb: "KillTurn", Arm: ArmShimNotTheOpenTurn}

	// Act.
	_, _ = f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Turn: true}, false)

	// Assert.
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && r.Operation == opInterrupt {
			return
		}
	}
	t.Fatalf("records = %+v, want the disagreement recorded at error under %s", f.log.logger.Records(), opInterrupt)
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

// runningShells adds n detached shells to the fixture's live-work set, on top
// of whatever runningTurn already put there.
func runningShells(f *fixture, shells int) {
	for i := 0; i < shells; i++ {
		f.running.LiveWork.Shells = append(f.running.LiveWork.Shells,
			&conversationv1.DetachedWorkId{Value: fmt.Sprintf("work-%d", i)})
	}
}

// TestInterruptAllAgentsStopsEveryLiveDetachedItem pins the fan-wide stop's
// REACH: agentrepl.v1.Interrupt's all_agents target is "the fan-wide stop", and
// a sweep that walked past a live detached SHELL would leave the live set
// non-empty — the one thing the caller asked it to empty.
func TestInterruptAllAgentsStopsEveryLiveDetachedItem(t *testing.T) {
	tests := []struct {
		name        string
		agents      int
		shells      int
		wantCount   int
		wantAgents  int
		wantShells  int
		wantNothing bool
	}{
		{name: "agents and a shell", agents: 2, shells: 1, wantCount: 3, wantAgents: 2, wantShells: 1},
		{name: "shells alone", agents: 0, shells: 2, wantCount: 2, wantAgents: 0, wantShells: 2},
		{name: "agents alone", agents: 3, shells: 0, wantCount: 3, wantAgents: 3, wantShells: 0},
		{name: "an empty live set", agents: 0, shells: 0, wantNothing: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			runningTurn(f, tc.agents)
			runningShells(f, tc.shells)

			// Act.
			outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

			// Assert.
			if err != nil {
				t.Fatalf("Interrupt: %v", err)
			}
			if outcome.NothingRunning != tc.wantNothing {
				t.Fatalf("outcome = %+v, want nothing_running=%v", outcome, tc.wantNothing)
			}
			if outcome.DetachedCount != tc.wantCount {
				t.Fatalf("stopped count = %d, want %d", outcome.DetachedCount, tc.wantCount)
			}
			if len(f.shim.stoppedAgents) != tc.wantAgents {
				t.Fatalf("stopped agents = %v, want %d", f.shim.stoppedAgents, tc.wantAgents)
			}
			if len(f.shim.stoppedShells) != tc.wantShells {
				t.Fatalf("stopped shells = %v, want %d", f.shim.stoppedShells, tc.wantShells)
			}
		})
	}
}

// TestInterruptAllAgentsSkipsAShellThatAlreadyFinished is the shell's half of
// the benign-refusal rule the agents already have: a shell that ended on its
// own mid-sweep is the state the caller asked for, so it is skipped rather than
// counted or surfaced as a failure.
func TestInterruptAllAgentsSkipsAShellThatAlreadyFinished(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 1)
	runningShells(f, 1)
	f.shim.stopBashErr = &ShimRefusal{Verb: "StopBash", Arm: ArmShimAlreadyEnded}

	// Act.
	outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if outcome.DetachedCount != 1 {
		t.Fatalf("stopped count = %d, want 1 (the agent alone; the shell had already ended)", outcome.DetachedCount)
	}
}

// TestInterruptAllAgentsPropagatesAShellRefusal pins that a shell refusal which
// does NOT say the shell is gone fails the fan-wide stop by name, exactly as an
// agent's does. It was written against `unknown_work`, which the sweep now
// reads as staleness — a shell the shim no longer knows is a shell that is no
// longer running, and endpoint_interrupt.proto's InterruptError carries no
// `unknown_work` arm to answer it with — so it pins the same rule on the one
// remaining StopBash refusal that is a genuine failure.
func TestInterruptAllAgentsPropagatesAShellRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	runningTurn(f, 0)
	runningShells(f, 1)
	f.shim.stopBashErr = &ShimRefusal{Verb: "StopBash", Arm: ArmShimUnspecified}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

	// Assert.
	asRefusal(t, err, ArmShimUnspecified)
}

// TestInterruptAllAgentsSkipsItemsTheShimHasForgotten pins the SECOND fan-wide
// stop: the freeness read's snapshot still carries the items the first stop
// ended, because the live set is stream-driven and the shim's frames have not
// landed yet. Stopping one the shim has already dropped answers `unknown_agent`
// / `unknown_work`, which endpoint_interrupt.proto's InterruptError does not
// carry at all — and could not, since a fan-wide request names no agent. Those
// items are gone, which is exactly what the caller asked for.
func TestInterruptAllAgentsSkipsItemsTheShimHasForgotten(t *testing.T) {
	tests := []struct {
		name         string
		agents       int
		shells       int
		stopAgentErr error
		stopBashErr  error
		wantNothing  bool
		wantCount    int
	}{
		{
			name: "every agent forgotten", agents: 2,
			stopAgentErr: &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimUnknownAgent},
			wantNothing:  true,
		},
		{
			name: "every shell forgotten", shells: 2,
			stopBashErr: &ShimRefusal{Verb: "StopBash", Arm: ArmShimUnknownWork},
			wantNothing: true,
		},
		{
			name: "the whole live set forgotten", agents: 2, shells: 1,
			stopAgentErr: &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimUnknownAgent},
			stopBashErr:  &ShimRefusal{Verb: "StopBash", Arm: ArmShimUnknownWork},
			wantNothing:  true,
		},
		{
			name: "a forgotten agent beside a live shell", agents: 2, shells: 1,
			stopAgentErr: &ShimRefusal{Verb: "UpdateAgent", Arm: ArmShimUnknownAgent},
			wantCount:    1,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			runningTurn(f, tc.agents)
			runningShells(f, tc.shells)
			f.shim.stopAgentErr = tc.stopAgentErr
			f.shim.stopBashErr = tc.stopBashErr

			// Act.
			outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{AllAgents: true}, false)

			// Assert.
			if err != nil {
				t.Fatalf("Interrupt(all_agents) = error %v, want a success", err)
			}
			if outcome.NothingRunning != tc.wantNothing {
				t.Fatalf("outcome = %+v, want nothing_running=%v", outcome, tc.wantNothing)
			}
			if outcome.DetachedCount != tc.wantCount {
				t.Fatalf("stopped count = %d, want %d", outcome.DetachedCount, tc.wantCount)
			}
		})
	}
}

// TestInterruptAllAgentsStillFailsOnARefusalThatIsNotStaleness guards the
// skip's edge: a refusal that does NOT say the item is gone must still fail the
// sweep, because a user who asked for the work to stop must not be told it is
// gone when it is not.
func TestInterruptAllAgentsStillFailsOnARefusalThatIsNotStaleness(t *testing.T) {
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

// TestInterruptDetachedResolvesTheSubagentBubbleRow pins the row form the feed
// ACTUALLY serves for a subagent bubble. resolve/feed/subagent.go mints it as
// RowKey{Kind: activity, ID: <spawn unit>, Sub: <agent id>} — nothing mints
// `detached_subagent` — and endpoint_interrupt.proto's detached target takes
// "the bubble row's FeedId exactly as the feed served it", so that form must
// resolve to the subagent's stop rather than be refused as no detached work.
func TestInterruptDetachedResolvesTheSubagentBubbleRow(t *testing.T) {
	tests := []struct {
		name string
		row  feedid.RowKey
		want string
	}{
		{
			name: "the bubble row the feed serves",
			row:  feedid.RowKey{Kind: feedid.KindActivity, ID: "spawn-unit-1", Sub: "agent-7"},
			want: "agent-7",
		},
		{
			name: "the detached_subagent kind, whose id IS the agent",
			row:  feedid.RowKey{Kind: feedid.KindDetachedSubagent, ID: "agent-9"},
			want: "agent-9",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			ref := feedid.Ref{WS: "w1", Row: tc.row}

			// Act.
			outcome, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

			// Assert.
			if err != nil {
				t.Fatalf("Interrupt(detached) = error %v, want a success", err)
			}
			if outcome.DetachedCount != 1 {
				t.Fatalf("stopped count = %d, want 1", outcome.DetachedCount)
			}
			if len(f.shim.stoppedAgents) != 1 || f.shim.stoppedAgents[0] != tc.want {
				t.Fatalf("stopped agents = %v, want [%s]", f.shim.stoppedAgents, tc.want)
			}
		})
	}
}

// TestInterruptDetachedRefusesAnActivityRowWithNoSubagent guards the resolution's
// edge: a Sub on an activity row is what names a subagent, so a plain activity
// row — a tool call, not a bubble — still addresses no detached work.
func TestInterruptDetachedRefusesAnActivityRowWithNoSubagent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	ref := feedid.Ref{WS: "w1", Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "act-1"}}

	// Act.
	_, err := f.verbs.Interrupt(context.Background(), "w1", InterruptTarget{Detached: &ref}, false)

	// Assert.
	asRefusal(t, err, ArmNotDetachedWork)
	if len(f.shim.stoppedAgents) != 0 {
		t.Fatalf("stopped agents = %v, want none", f.shim.stoppedAgents)
	}
}
