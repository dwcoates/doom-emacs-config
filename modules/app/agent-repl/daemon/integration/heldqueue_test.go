//go:build integration

package integration

import (
	"testing"

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
//   - "[interject] actually also do X" is sent while it runs: it is judged
//     against it and interrupts it.
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

	// Act: a prompt the classifier rules interrupting, while it runs.
	f.submit("[interject] actually also do X", "k-also", origin)

	// Assert: judged against the running prompt, and it interrupts it.
	f.shim.ExpectKillTurn()
}
