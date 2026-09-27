package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

func TestSubmitSessionActRefusesAKindThePathDoesNotCarry(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: "reboot"})
	// Assert
	if err == nil {
		t.Fatal("an act kind the path does not carry must be refused")
	}
}

func TestSubmitSessionActSetsTheModelWhenThePathIsClear(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if len(h.sender.models) != 1 || h.sender.models[0] != "opus" {
		t.Fatalf("models = %v, want the requested model", h.sender.models)
	}
}

func TestSubmitSessionActSetsThePermissionModeWhenThePathIsClear(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetPermissionMode, Value: "auto"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if len(h.sender.modes) != 1 || h.sender.modes[0] != "auto" {
		t.Fatalf("modes = %v, want the requested mode", h.sender.modes)
	}
}

func TestSubmitSessionActQueuesBehindARunningTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if len(h.sender.models) != 0 {
		t.Fatal("an act may never overtake the turn already running")
	}
}

func TestSubmitSessionActQueuesBehindAStandingHold(t *testing.T) {
	// Arrange: a prompt is held under the drain lease, so the act waits too.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if len(h.sender.models) != 0 {
		t.Fatal("an act may never overtake a prompt the user already queued")
	}
}

func TestAQueuedActRunsAtTheTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if len(h.sender.models) != 1 || h.sender.models[0] != "opus" {
		t.Fatalf("models = %v, want the queued act delivered at the turn's end", h.sender.models)
	}
}

func TestAQueuedActRunsBeforeTheNextPromptIsPopped(t *testing.T) {
	// Arrange: the model change must apply to the prompt that follows it.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if len(h.sender.models) != 1 {
		t.Fatalf("models = %v, want the act run", h.sender.models)
	}
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt delivered after the act", started)
	}
}

func TestAContextCutIsDeliveredAsATurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActClear, Turn: "cut-1", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "cut-1" {
		t.Fatalf("started = %v, want the caller's minted turn", started)
	}
	if got := saidText(h.sender.said[0]); got != "/clear" {
		t.Fatalf("text = %q, want the literal the CLI answers", got)
	}
}

func TestAContextCutCarriesItsArgument(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActCompact, Turn: "cut-1", Value: "focus on the tests"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if got := saidText(h.sender.said[0]); got != "/compact focus on the tests" {
		t.Fatalf("text = %q, want the command and its argument", got)
	}
}

func TestAContextCutEarnsNoMirroredUserPromptRow(t *testing.T) {
	// Arrange: a recognized command earns no user message.
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if len(h.feed.mirrored()) != 0 {
		t.Fatal("a recognized command earns no user-prompt row")
	}
}

func TestAContextCutHandsItsTurnToTheWatcher(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Assert
	if h.watcher.handovers() != 1 {
		t.Fatalf("handovers = %d, want the cut's turn handed over", h.watcher.handovers())
	}
}

func TestARefusedContextCutClearsTheUninterruptibleMark(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("the vendor query is dead")
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear, Turn: "cut-1"})
	// Assert
	if err == nil {
		t.Fatal("a refused context cut must be surfaced")
	}
	if cut, ok := h.q.runningCut(theWorkspace); ok {
		t.Fatalf("running cut = %+v, want it retired", cut)
	}
}

// A /clear IS REFLECTED IN THE FEED ON RECEIPT, before the shim is asked: the
// receipt call fires so the divider and the cleared feed appear at once.
func TestAClearReflectsInTheFeedBeforeTheShim(t *testing.T) {
	// Arrange: capture the feed's clear-received turns AT the shim call.
	h := newHarness(t)
	var receivedAtShim []ids.TurnID
	h.sender.startHook = func() { receivedAtShim = h.feed.clearReceivedTurns() }

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActClear, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert: the feed already knew about the clear when StartTurn was entered.
	if len(receivedAtShim) != 1 || receivedAtShim[0] != "cut-1" {
		t.Fatalf("clear-received at StartTurn = %v, want the clear reflected before the shim", receivedAtShim)
	}
}

// A /compact REGISTERS AS A DIRECTIVE ON RECEIPT so it draws no prompt bubble.
func TestACompactRegistersAsADirectiveOnReceipt(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActCompact, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	if got := h.feed.compactReceivedTurns(); len(got) != 1 || got[0] != "cut-1" {
		t.Fatalf("compact-received = %v, want the compact registered", got)
	}
	if got := h.feed.clearReceivedTurns(); len(got) != 0 {
		t.Fatalf("clear-received = %v, want a compact to draw no clear divider", got)
	}
}

// A REFUSED CLEAR RETIRES ITS OPTIMISTIC DIVIDER, so the feed recovers rather
// than keeping a phantom red bar for a clear that never ran.
func TestARefusedClearRetiresItsDivider(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.startErr = errors.New("the vendor query is dead")

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActClear, Turn: "cut-1"}); err == nil {
		t.Fatal("a refused context cut must be surfaced")
	}

	// Assert
	if got := h.feed.cutAbortedTurns(); len(got) != 1 || got[0] != "cut-1" {
		t.Fatalf("cut-aborted = %v, want the optimistic divider retired", got)
	}
}

func TestSubmitSessionActRefusesAWorkspaceWithNoSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.noSession = true
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"})
	// Assert
	if !errors.Is(err, ErrNoSession) {
		t.Fatalf("err = %v, want ErrNoSession", err)
	}
}

func TestSubmitSessionActSurfacesARefusedModelChange(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.setModelErr = errors.New("the model is not served")
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "nope"})
	// Assert
	if err == nil {
		t.Fatal("a refused model change must be surfaced")
	}
}

// TestAContextCutTellsTheFooterWhatItCarries pins the footer's act: nothing on
// the shim's streams states what a turn is FOR, so without this a /clear draws
// as thinking·submitting rather than as clearing.
func TestAContextCutTellsTheFooterWhatItCarries(t *testing.T) {
	tests := []struct {
		name string
		kind string
		want footer.SessionAct
	}{
		{name: "clear", kind: ActClear, want: footer.ActClear},
		{name: "compact", kind: ActCompact, want: footer.ActCompact},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{
				Kind: tc.kind, Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			}); err != nil {
				t.Fatalf("SubmitSessionAct: %v", err)
			}

			// Assert
			turns := h.footer.startedTurns()
			if len(turns) != 1 || turns[0] == nil {
				t.Fatalf("footer turn facts = %v, want exactly one", turns)
			}
			if turns[0].Act != tc.want {
				t.Fatalf("footer act = %v, want %v", turns[0].Act, tc.want)
			}
		})
	}
}
