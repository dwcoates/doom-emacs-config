package promptqueue

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/classifier"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/wsm"
)

// olderQueued holds "older" behind the running turn with a waiting verdict.
func olderQueued(t *testing.T, h *harness) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

func TestAnInterruptVerdictInterruptsThePromptAheadWhenItStartedMeanwhile(t *testing.T) {
	// Arrange: the verdict about "later" is with the model when "older" starts.
	h := newHarness(t)
	olderQueued(t, h)
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.watcher.running("older")

	// Act
	release()
	h.q.waitForClassifications()

	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "older" {
		t.Fatalf("killed = %v, want the prompt ahead, now running, interrupted", killed)
	}
}

func TestAnAfterToolCallVerdictJoinsThePromptAheadWhenItStartedMeanwhile(t *testing.T) {
	// Arrange: the verdict about "later" is with the model when "older" starts.
	h := newHarness(t)
	olderQueued(t, h)
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteAfterToolCall, Reason: "it adds to it"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("later", "and also this")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.watcher.running("older")

	// Act
	release()
	h.q.waitForClassifications()

	// Assert
	if killed, joins := h.sender.killed(), h.sender.joins; len(killed) != 0 || len(joins) != 1 || joins[0] != "later" {
		t.Fatalf("killed = %v, joins = %v; want later sent to join the prompt ahead, now running, and nothing interrupted", killed, joins)
	}
}

func TestAnInterruptVerdictWaitsWhenThePromptAheadWasDroppedMeanwhile(t *testing.T) {
	// Arrange
	h := newHarness(t)
	olderQueued(t, h)
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	if err := h.q.Drop(context.Background(), theWorkspace, "older"); err != nil {
		t.Fatalf("Drop: %v", err)
	}

	// Act
	release()
	h.q.waitForClassifications()

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 1 || standing[0].Turn != "later" || standing[0].Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("standing = (%+v, %v), want later waiting its turn", standing, err)
	}
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted", killed)
	}
}

func TestAPromptBehindAHeldModelChangeIsNeverClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "go on")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if asked := h.judge.questions(); len(asked) != 0 {
		t.Fatalf("asked = %v, want nothing asked about a prompt behind a session act", asked)
	}
	if !logged(h.log.Records(), "info", opClassify, "the prompt is queued behind a session act; it waits for it and is never classified") {
		t.Fatalf("the unjudged prompt was not recorded at info: %v", h.log.Records())
	}
}

func TestAnActWithNothingAheadIsAppliedAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActSetModel, Value: "opus"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if len(h.sender.models) != 1 || err != nil || len(standing) != 0 {
		t.Fatalf("models = %v, standing = (%d, %v); want the act applied and nothing held", h.sender.models, len(standing), err)
	}
}

func TestAnActBehindOnlyAHeldPromptIsHeld(t *testing.T) {
	// Arrange: no turn runs, but a prompt is held (a lease holds it).
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
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if len(h.sender.models) != 0 || err != nil || len(standing) != 2 {
		t.Fatalf("models = %v, standing = (%d, %v); want the act held behind the prompt", h.sender.models, len(standing), err)
	}
}

func TestAPromptWhoseQueueCannotBeReadIsNotClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.db.mu.Lock()
	h.db.heldErr = errors.New("disk gone")
	h.db.mu.Unlock()

	// Act: the read failure also fails the tray push, which Submit reports.
	_, _ = h.q.Submit(context.Background(), submission("t1", "go on"))
	h.q.waitForClassifications()

	// Assert
	if asked := h.judge.questions(); len(asked) != 0 {
		t.Fatalf("asked = %v, want nothing judged against a queue that could not be read", asked)
	}
	if !logged(h.log.Records(), "error", opClassify, "could not read what is queued ahead of the prompt; it waits its turn and is not classified") {
		t.Fatalf("the unread queue was not recorded at error: %v", h.log.Records())
	}
}

func TestMergedSaidKeepsEveryBlockInOrder(t *testing.T) {
	// Arrange
	into, from := userSaid("first"), userSaid("second")

	// Act
	merged := mergedSaid(into, from)

	// Assert
	if got := saidText(merged); got != "first\nsecond" {
		t.Fatalf("merged = %q, want first then second", got)
	}
}

func TestTheQueueReadsSessionActsThroughHoldfold(t *testing.T) {
	// The queue's walk, its context-cut recognition and its text all go through
	// the one reading the hold tray shares, so the two can never disagree about
	// what a held entry is.
	tests := []struct {
		name string
		said string
	}{
		{name: "a context cut", said: "/compact keep the plan"},
		{name: "an ordinary prompt", said: "fix the flaky test"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			sub := Submission{WS: theWorkspace, Turn: "t1", Said: userSaid(tt.said)}

			// Act
			command, literal, cut := contextCutOf(sub)

			// Assert
			wantCommand, wantLiteral, wantCut := holdfold.ContextCut(sub.Target, sub.Said)
			if command != wantCommand || literal != wantLiteral || cut != wantCut {
				t.Fatalf("contextCutOf = (%v, %q, %v), want holdfold's (%v, %q, %v)", command, literal, cut, wantCommand, wantLiteral, wantCut)
			}
			if got := saidText(sub.Said); got != holdfold.SaidText(sub.Said) {
				t.Fatalf("saidText = %q, want holdfold's %q", got, holdfold.SaidText(sub.Said))
			}
			if got := holdfold.SessionAct(wsm.HeldPrompt{Said: sub.Said}); got != cut {
				t.Fatalf("holdfold.SessionAct = %v, want it to agree with the queue's cut reading %v", got, cut)
			}
		})
	}
}
