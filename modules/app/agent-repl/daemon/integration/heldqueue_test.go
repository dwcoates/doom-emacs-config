//go:build integration

package integration

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
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
	f := newCanonical(t)
	// A FAILURE QUOTES THE QUEUE'S OWN ACCOUNT. This test once timed out
	// waiting for the join's StartTurn (35s, a full-suite run, 2026-10-06; 1
	// in ~60 under load, 0 in 400 alone), and the record of what the queue
	// decided about the prompt -- held, judged against what, routed where --
	// is the only evidence of which step it stopped at.
	t.Cleanup(func() {
		if !t.Failed() {
			return
		}
		for _, r := range f.d.WorkspaceLog(f.repo.Dir, "daemon") {
			if strings.HasPrefix(r.Operation, "daemon.promptqueue") || strings.HasPrefix(r.Operation, "daemon.prompthandler") || strings.HasPrefix(r.Operation, "daemon.classifier") || strings.HasPrefix(r.Operation, "daemon.shimclient.start_turn") {
				t.Logf("queue record: %s %s %s %s %v", r.Timestamp, r.Level, r.Operation, r.Message, r.Context)
			}
		}
	})
	holds := f.d.WatchHolds(f.ws)
	f.submit("hey update the doc", "k-doc", origin)
	f.shim.ExpectStartTurn()

	// Act: /compact while it runs.
	compact := f.submit("/compact", "k-compact", origin).GetSuccess().GetTurn().GetTurn()

	// Assert: the /compact is on the tray, waiting and unclassified.
	awaitView(t, f, holds, "the held /compact on the tray", func(tray *frontendv1.FeedHeld) bool {
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
	awaitView(t, f, holds, "the prompt held behind the running compaction", func(tray *frontendv1.FeedHeld) bool {
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
