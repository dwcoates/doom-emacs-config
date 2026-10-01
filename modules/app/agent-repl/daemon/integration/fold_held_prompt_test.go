//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestAFoldedHeldPromptIsDeliveredWithThePromptAheadOfIt is FoldHeldPrompt end
// to end: the tray offers the second held prompt a fold into the first,
// carrying the first's token; the fold echoes it; the tray then holds the
// first alone, carrying both prompts' words a blank line apart; and the
// running turn's end delivers that one entry as one turn.
func TestAFoldedHeldPromptIsDeliveredWithThePromptAheadOfIt(t *testing.T) {
	t.Parallel()

	// Arrange: two prompts held behind a running turn.
	f := newOpened(t, harness.Opts{})
	holds := f.d.WatchHolds(f.ws)
	f.submit("the running work", "k-running", origin)
	f.shim.ExpectStartTurn()
	first := f.submit("first words", "k-first", origin).GetSuccess().GetTurn().GetTurn()
	second := f.submit("second words", "k-second", origin).GetSuccess().GetTurn().GetTurn()
	var offered *conversationv1.TurnId
	awaitView(t, f, holds, "the second held prompt offering a fold into the first", func(tray *frontendv1.DaemonHoldTray) bool {
		p, q := promptHeldEntry(tray, first), promptHeldEntry(tray, second)
		if p == nil || q == nil || p.FoldAbove != nil || q.GetFoldAbove().GetAbove().GetValue() != first.GetValue() {
			return false
		}
		offered = q.GetFoldAbove().GetAbove()
		return true
	})

	// Act: the button's click, echoing the served token.
	resp, err := f.d.Client().FoldHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.FoldHeldPromptRequest{
		Workspace: f.ws, Turn: second, Above: offered,
	}))

	// Assert: the tray holds the first entry alone, merged and coalesced.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("FoldHeldPrompt = (%v, %v), want success", resp, err)
	}
	awaitView(t, f, holds, "the first entry alone, carrying both prompts", func(tray *frontendv1.DaemonHoldTray) bool {
		p := promptHeldEntry(tray, first)
		return p != nil && promptHeldEntry(tray, second) == nil && p.GetCoalesced() != nil &&
			text(p.GetSaid()) == "first words\n\nsecond words"
	})

	// Act: the running turn ends.
	pushConcludedTurn(f.shim, mainAgent, "answer-running")

	// Assert: one turn, the first entry's, carrying both prompts' words.
	start := f.shim.ExpectStartTurn()
	if start.GetTurn().GetValue() != first.GetValue() || text(start.GetSaid()) != "first words\n\nsecond words" {
		t.Fatalf("StartTurn = turn %q saying %q, want the first entry carrying both prompts", start.GetTurn().GetValue(), text(start.GetSaid()))
	}
}

// TestAFoldIntoAnEntryNoLongerAheadIsRefusedAndChangesNothing covers the
// typed refusal: a fold naming an entry that is not directly ahead answers
// above_moved, naming the entry that is, and both entries stay on the tray
// as they were.
func TestAFoldIntoAnEntryNoLongerAheadIsRefusedAndChangesNothing(t *testing.T) {
	t.Parallel()

	// Arrange: three prompts held behind a running turn.
	f := newOpened(t, harness.Opts{})
	holds := f.d.WatchHolds(f.ws)
	f.submit("the running work", "k-running", origin)
	f.shim.ExpectStartTurn()
	first := f.submit("first words", "k-first", origin).GetSuccess().GetTurn().GetTurn()
	second := f.submit("second words", "k-second", origin).GetSuccess().GetTurn().GetTurn()
	third := f.submit("third words", "k-third", origin).GetSuccess().GetTurn().GetTurn()
	awaitView(t, f, holds, "three held prompts", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, first) != nil && promptHeldEntry(tray, second) != nil && promptHeldEntry(tray, third) != nil
	})

	// Act: the third folded into the first, which is not directly ahead of it.
	resp, err := f.d.Client().FoldHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.FoldHeldPromptRequest{
		Workspace: f.ws, Turn: third, Above: first,
	}))

	// Assert
	if err != nil {
		t.Fatalf("FoldHeldPrompt = error %v, want the typed above_moved answer", err)
	}
	if got := resp.Msg.GetError().GetAboveMoved().GetCurrentAbove().GetValue(); got != second.GetValue() {
		t.Fatalf("FoldHeldPrompt = %v, want above_moved naming the second entry", resp.Msg)
	}
	awaitView(t, f, holds, "all three prompts still held with their own words", func(tray *frontendv1.DaemonHoldTray) bool {
		p, q, r := promptHeldEntry(tray, first), promptHeldEntry(tray, second), promptHeldEntry(tray, third)
		return p != nil && q != nil && r != nil && text(p.GetSaid()) == "first words" && text(r.GetSaid()) == "third words" &&
			p.GetCoalesced() == nil
	})
}
