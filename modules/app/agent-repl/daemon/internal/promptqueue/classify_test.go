package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/wsm"
)

// running installs a recorded running turn and tells the watcher about it, so
// the classifier has both halves of the pair to judge.
func running(t *testing.T, h *harness, turn, text string) {
	t.Helper()
	if err := h.db.PutTurn(context.Background(), wsm.Turn{
		ID: idsTurn(turn), Workspace: theWorkspace, Text: text, StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	h.watcher.running(idsTurn(turn))
}

func TestJudgeStampsHoldForTurnEndOnAHoldingVerdict(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmHoldForTurnEnd {
		t.Fatalf("arm = %s, want hold_for_turn_end", armName(got))
	}
}

func TestJudgeKeepsTheJudgesReasonAsEvidence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "genuinely independent"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.db.hold("t1").Classification.Reason; got != "genuinely independent" {
		t.Fatalf("reason = %q, want the judge's own", got)
	}
}

func TestJudgeComparesTheRunningTurnAgainstTheIncomingPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "the new message")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	asked := h.judge.questions()
	if len(asked) != 1 || asked[0][0] != "the running work" || asked[0][1] != "the new message" {
		t.Fatalf("asked = %v, want the running turn and the incoming prompt", asked)
	}
}

func TestJudgeStampsClassificationErrorWhenTheJudgeFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.err = errors.New("the vendor run failed")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert: an error is never a verdict.
	if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmClassificationError {
		t.Fatalf("arm = %s, want classification_error", armName(got))
	}
}

func TestJudgeStampsClassificationErrorWhenTheRunningTurnHasNoRecord(t *testing.T) {
	// Arrange: the watcher reports a turn the store never recorded, so there is
	// nothing to compare against.
	h := newHarness(t)
	h.watcher.running("phantom-turn")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmClassificationError {
		t.Fatalf("arm = %s, want classification_error", armName(got))
	}
}

func TestJudgeStampsUninterruptibleWhileAContextCutRuns(t *testing.T) {
	// Arrange: /clear is running, so there is nothing for the model to judge.
	h := newHarness(t)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	running(t, h, "running-turn", "/clear")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	held := h.db.hold("t1")
	if held.Classification.Arm != wsm.ArmUninterruptibleTurn {
		t.Fatalf("arm = %s, want uninterruptible_turn", armName(held.Classification.Arm))
	}
	if held.Classification.Command != conversationv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("command = %s, want /clear", held.Classification.Command)
	}
}

func TestJudgeDoesNotAskTheModelWhileAContextCutRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	running(t, h, "running-turn", "/compact")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if len(h.judge.questions()) != 0 {
		t.Fatal("an uninterruptible turn is decided without the model")
	}
}

func TestInterjectFiresTheInterruptingStatusAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.footer.interruptions(); len(got) == 0 || !got[0] {
		t.Fatalf("footer = %v, want the interrupting status fired", got)
	}
}

func TestInterjectSendsTheInterruptToTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "stop doing that")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the running turn", killed)
	}
}

func TestInterjectWaitsForTheTurnsRealEndBeforeDelivering(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if len(h.sender.started()) != 0 {
		t.Fatal("an interjecting prompt is not delivered until the turn really ends")
	}
}

func TestInterjectMovesThePromptToTheSemanticHead(t *testing.T) {
	// Arrange: an older prompt is already waiting, and the interjection jumps it.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	if _, err := h.q.Submit(context.Background(), submission("jumper", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "jumper" {
		t.Fatalf("started = %v, want the interjecting prompt first", started)
	}
}

func TestAFailedInterruptStripsTheJumpAndStampsTheError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	h.sender.killErr = errors.New("the turn is not the open one")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmClassificationError {
		t.Fatalf("arm = %s, want classification_error", armName(got))
	}
	if got := h.footer.interruptions(); got[len(got)-1] {
		t.Fatal("a failed interrupt must clear the interrupting status")
	}
}

func TestAFailedInterruptLeavesNoQueueJumpBehind(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	h.sender.killErr = errors.New("the turn is not the open one")
	if _, err := h.q.Submit(context.Background(), submission("jumper", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "older" {
		t.Fatalf("started = %v, want the older prompt: the jump was stripped", started)
	}
}

// TestAHoldBehindAContextCutIsStampedUninterruptibleOnItsFirstPush pins that
// no `classifying` arm is ever recorded for an entry behind an uninterruptible
// turn: the verdict is known before the entry is stored, so the tray's first
// view of it already carries uninterruptible_turn.
func TestAHoldBehindAContextCutIsStampedUninterruptibleOnItsFirstPush(t *testing.T) {
	// Arrange: a context cut is the running turn.
	h := newHarness(t)
	h.q.state("ws-1").uninterruptible = conversationv1.SessionCommand_SESSION_COMMAND_CLEAR
	h.watcher.running("t-running")

	// Act.
	got, err := h.q.Submit(context.Background(), submission("t1", "a follow-up"))
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert.
	if got.Classification == nil || got.Classification.Arm != wsm.ArmUninterruptibleTurn {
		t.Fatalf("disposition classification = %+v, want the uninterruptible_turn verdict", got.Classification)
	}
	first := h.holds.pushes[0]
	if len(first) != 1 || first[0].Classification.Arm != wsm.ArmUninterruptibleTurn {
		t.Fatalf("first tray push = %+v, want uninterruptible_turn with no classifying ahead of it", first)
	}
}

// TestAHoldBehindAContextCutAsksNoClassifier pins the other half: the model is
// never consulted for an entry nothing could interject.
func TestAHoldBehindAContextCutAsksNoClassifier(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.q.state("ws-1").uninterruptible = conversationv1.SessionCommand_SESSION_COMMAND_COMPACT
	h.watcher.running("t-running")

	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.classifying.Wait()

	// Assert.
	if got := len(h.judge.questions()); got != 0 {
		t.Fatalf("classifier calls = %d, want none behind a context cut", got)
	}
}
