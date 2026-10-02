//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestAHeldPromptMidDeliveryIsRefusedAsBeingDelivered is the being_delivered
// arm end to end: the running turn's end pops the first held prompt into a
// StartTurn the shim does not answer, and while that call is in flight a
// drop, an edit and a fold of it each answer being_delivered. Once the shim
// answers, the prompt is delivered once and the second stays held.
func TestAHeldPromptMidDeliveryIsRefusedAsBeingDelivered(t *testing.T) {
	t.Parallel()

	// Arrange: two prompts held behind a running turn, and a shim that will
	// not answer the next StartTurn.
	f := newOpened(t, harness.Opts{})
	holds := f.d.WatchHolds(f.ws)
	f.submit("the running work", "k-running", origin)
	f.shim.ExpectStartTurn()
	first := f.submit("first words", "k-first", origin).GetSuccess().GetTurn().GetTurn()
	second := f.submit("second words", "k-second", origin).GetSuccess().GetTurn().GetTurn()
	awaitView(t, f, holds, "both prompts held", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, first) != nil && promptHeldEntry(tray, second) != nil
	})
	f.shim.Hang()
	pushConcludedTurn(f.shim, mainAgent, "answer-running")
	if start := f.shim.ExpectStartTurn(); start.GetTurn().GetValue() != first.GetValue() {
		t.Fatalf("StartTurn = %q, want the first held prompt", start.GetTurn().GetValue())
	}

	// Act: each step on the first prompt while its StartTurn is in flight.
	drop, dropErr := f.d.Client().UpdateHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
		Workspace: f.ws, Turn: first,
		Action: &agentreplv1.UpdateHeldPromptRequest_Drop{Drop: &agentreplv1.UpdateHeldPromptDrop{}},
	}))
	edit, editErr := f.d.Client().EditHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.EditHeldPromptRequest{
		Workspace: f.ws, Turn: first,
		Action: &agentreplv1.EditHeldPromptRequest_Begin{Begin: &agentreplv1.EditHeldPromptBegin{}},
	}))
	fold, foldErr := f.d.Client().FoldHeldPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.FoldHeldPromptRequest{
		Workspace: f.ws, Turn: second, Above: first,
	}))
	f.shim.Unhang()

	// Assert
	if dropErr != nil || drop.Msg.GetError().GetBeingDelivered() == nil {
		t.Fatalf("drop = (%v, %v), want being_delivered", drop, dropErr)
	}
	if editErr != nil || edit.Msg.GetError().GetBeingDelivered() == nil {
		t.Fatalf("edit = (%v, %v), want being_delivered", edit, editErr)
	}
	if foldErr != nil || fold.Msg.GetError().GetBeingDelivered() == nil {
		t.Fatalf("fold = (%v, %v), want being_delivered", fold, foldErr)
	}
	awaitView(t, f, holds, "the first prompt delivered and the second still held", func(tray *frontendv1.DaemonHoldTray) bool {
		return promptHeldEntry(tray, first) == nil && promptHeldEntry(tray, second) != nil
	})
}
