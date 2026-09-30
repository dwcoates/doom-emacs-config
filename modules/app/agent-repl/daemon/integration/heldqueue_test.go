//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestTheOwnersWorkedExampleOfTheHeldQueue is the owner's worked example of
// the held queue (2026-09-30), end to end:
//
//   - "hey update the doc" runs;
//   - /compact is sent: held, never classified, and VISIBLE on the tray as
//     queued behind the running turn (it used to wait in process memory,
//     invisible);
//   - the turn ends and the /compact runs as the session's turn;
//   - "read the doc file and continue" is sent during the compaction: held
//     behind it, never classified (nothing interrupts a context cut);
//   - the compaction ends and that prompt runs;
//   - "actually also do X" is sent while it runs: it is judged against it,
//     adds to it, and joins it after its current tool call with nothing
//     interrupted (the verdict split, 2026-09-30).
func TestTheOwnersWorkedExampleOfTheHeldQueue(t *testing.T) {
	t.Parallel()

	// Arrange: the first prompt runs.
	f := newOpened(t, harness.Opts{})
	holds := f.d.WatchHolds(f.ws)
	f.submit("hey update the doc", "k-doc", origin)
	f.shim.ExpectStartTurn()

	// Act: /compact while it runs.
	compact := f.submit("/compact", "k-compact", origin).GetSuccess().GetTurn().GetTurn()

	// Assert: the /compact is on the tray, waiting and unclassified.
	awaitView(t, f, holds, "the held /compact on the tray", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, compact)
		return p != nil && p.GetHoldForTurnEnd() != nil && text(p.GetSaid()) == "/compact"
	})

	// Act: the first turn ends; the /compact runs.
	pushConcludedTurn(f.shim, mainAgent, "answer-doc")
	if got := text(f.shim.ExpectStartTurn().GetSaid()); got != "/compact" {
		t.Fatalf("StartTurn after the first turn = %q, want the held /compact", got)
	}

	// Act: a prompt during the compaction.
	readDoc := f.submit("read the doc file and continue", "k-read", origin).GetSuccess().GetTurn().GetTurn()

	// Assert: held behind the cut, never classified.
	awaitView(t, f, holds, "the prompt held behind the running compaction", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, readDoc)
		return p != nil && p.GetUninterruptibleTurn() != nil
	})

	// Act: the compaction ends; the prompt runs.
	pushConcludedTurn(f.shim, mainAgent, "answer-compact")
	if got := text(f.shim.ExpectStartTurn().GetSaid()); got != "read the doc file and continue" {
		t.Fatalf("StartTurn after the compaction = %q, want the prompt held behind it", got)
	}

	// Act: a prompt the classifier rules adds to the running work.
	f.submit("[after-tool-call] actually also do X", "k-also", origin)

	// Assert: judged against the running prompt, and sent to join it.
	if join := f.shim.ExpectStartTurn(); !join.GetJoinRunningTurn() || text(join.GetSaid()) != "[after-tool-call] actually also do X" {
		t.Fatalf("StartTurn = %v, want the prompt sent to join the running turn", join)
	}
	if kills := f.shim.Count(harness.RPCKillTurn); kills != 0 {
		t.Fatalf("KillTurn calls = %d, want nothing interrupted", kills)
	}
}

// joinedTurn opens a running turn and sends a second prompt to join it,
// answering both turn ids.
func joinedTurn(t *testing.T, f *fixture) (running, joining string) {
	t.Helper()
	running = f.submit("port the footer", "k-running", origin).GetSuccess().GetTurn().GetTurn().GetValue()
	f.shim.ExpectStartTurn()
	joining = f.submit("[after-tool-call] also cover the edge case", "k-joining", origin).GetSuccess().GetTurn().GetTurn().GetValue()
	if join := f.shim.ExpectStartTurn(); !join.GetJoinRunningTurn() {
		t.Fatalf("StartTurn = %v, want the second prompt sent to join the running turn", join)
	}
	return running, joining
}

// TestAFoldedJoinRunsNoTurnOfItsOwn covers the vendor folding the joining
// prompt in: its prompt row names the running turn, its own turn closes
// unrun, and the running turn's end leaves the session free.
func TestAFoldedJoinRunsNoTurnOfItsOwn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	running, joining := joinedTurn(t, f)

	// Act: the vendor folds the prompt in, then the running turn ends.
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:         &conversationv1.TurnId{Value: joining},
		Agent:      &conversationv1.AgentId{Value: mainAgent},
		Origin:     origin,
		FoldedInto: &conversationv1.TurnId{Value: running},
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's record of the folded join", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.join" && r.Level == "info" && r.Context["turn"] == joining &&
			r.Message == "the vendor folded the prompt into the running turn; its own turn closed unrun"
	})
	pushConcludedTurn(f.shim, mainAgent, "answer-both")

	// Assert
	awaitFooter(t, f, footer, "idle.done once the running turn ends", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
}

// TestAnUnfoldedJoinRunsAsTheNextTurn covers the running turn ending with no
// tool boundary left: the joining prompt runs as its own turn in its place,
// nothing is popped into it, and the next prompt starts once it ends.
func TestAnUnfoldedJoinRunsAsTheNextTurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	running, joining := joinedTurn(t, f)

	// Act: the running turn ends, and the joining prompt opens its own turn.
	f.shim.PushAgentFrameIn(mainAgent, running, successFrame(mainAgent, nil))
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: joining},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: origin,
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's record of the join running as its own turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.join" && r.Level == "info" && r.Context["joining_turn"] == joining
	})
	f.shim.PushAgentFrameIn(mainAgent, joining, successFrame(mainAgent, nil))

	// Assert: the next prompt starts as an ordinary turn.
	f.submit("an unrelated question", "k-next", origin)
	if next := f.shim.ExpectStartTurn(); next.GetJoinRunningTurn() || text(next.GetSaid()) != "an unrelated question" {
		t.Fatalf("StartTurn = %v, want the next prompt started on its own", next)
	}
}
