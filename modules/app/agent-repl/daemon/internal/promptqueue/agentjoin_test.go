package promptqueue

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// AN AGENT'S PROMPT IS NEVER CLASSIFIED AND IS NEVER HELD FOR THE RUNNING
// TURN'S END (owner ruling, 2026-11-14): it joins the running turn at once.

// interruptingVerdict is a verdict that would interrupt the running turn,
// were the model ever asked.
var interruptingVerdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}

// agentPromptJoining submits an agent's prompt T1 against the running turn
// under a judge that would interrupt, and waits for everything to settle.
func agentPromptJoining(t *testing.T, h *harness) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = interruptingVerdict
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

func TestAnAgentsPromptBehindARunningTurnIsNeverClassified(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Assert
	if got := askedCount(h); got != 0 {
		t.Fatalf("the judge was asked %d times, want the agent's prompt never classified", got)
	}
}

func TestAnAgentsPromptBehindARunningTurnJoinsItAtOnce(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Assert
	if joins := h.sender.joins; len(joins) != 1 || joins[0] != "t1" {
		t.Fatalf("joins = %v, want t1 sent to join the running turn", joins)
	}
}

func TestAnAgentsPromptBehindARunningTurnInterruptsNothing(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Assert
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted", killed)
	}
}

func TestAnAgentsPromptBehindARunningTurnIsStampedAfterToolCall(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Assert
	if c := verdictOf(t, h, "t1"); c.Arm != wsm.ArmAfterToolCall {
		t.Fatalf("verdict = %+v, want after_tool_call", c)
	}
}

func TestAnAgentsPromptKeptFromTheClassifierIsSaidAtInfo(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Assert
	const message = "the prompt was sent by another agent; it joins the running turn and is never classified"
	if !logged(h.log.Records(), "info", opClassify, message) {
		t.Fatalf("records = %+v, want %q at info", h.log.Records(), message)
	}
}

func TestASecondAgentPromptJoinsBesideTheFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	agentPromptJoining(t, h)

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t2", "and the lint is clean")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if joins := h.sender.joins; len(joins) != 2 || joins[1] != "t2" {
		t.Fatalf("joins = %v, want both agent prompts sent to join", joins)
	}
}

func TestAnAgentsPromptBehindAHeldPromptStillJoinsTheRunningTurn(t *testing.T) {
	// Arrange: a user's prompt waits held, ruled to queue.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	asked := askedCount(h)

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if joins := h.sender.joins; len(joins) != 1 || joins[0] != "t1" || askedCount(h) != asked {
		t.Fatalf("joins = %v, asked %d more times; want t1 joined unjudged", joins, askedCount(h)-asked)
	}
}

func TestAnAgentsPromptBehindAVendorStartedTurnWaitsForItsEnd(t *testing.T) {
	// Arrange: the vendor started the running turn, which is never joined.
	h := newHarness(t)
	h.watcher.refuseJoins = true

	// Act
	agentPromptJoining(t, h)

	// Assert
	c := verdictOf(t, h, "t1")
	if len(h.sender.joins) != 0 || c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("joins = %v, verdict = %+v; want nothing sent and the prompt waiting", h.sender.joins, c)
	}
	if got := askedCount(h); got != 0 {
		t.Fatalf("the judge was asked %d times, want the waiting prompt never classified", got)
	}
}

func TestAnAgentsPromptBehindAContextCutIsStampedUninterruptible(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.beginCut("cut-1", conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)

	// Act
	if _, err := h.q.Submit(context.Background(), agentSubmission("t1", "the build is green")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	c := verdictOf(t, h, "t1")
	if len(h.sender.joins) != 0 || c.Arm != wsm.ArmUninterruptibleTurn {
		t.Fatalf("joins = %v, verdict = %+v; want nothing sent and the prompt uninterruptible", h.sender.joins, c)
	}
}

func TestADeferredAgentPromptWaitsForTheRunningTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	sub := agentSubmission("t1", "after this, run the tests")
	sub.Delivery = wsm.DeliveryDeferred

	// Act
	if _, err := h.q.Submit(context.Background(), sub); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	c := verdictOf(t, h, "t1")
	if len(h.sender.joins) != 0 || c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("joins = %v, verdict = %+v; want the deferred prompt held for the turn's end", h.sender.joins, c)
	}
}

func TestAUsersPromptBehindARunningTurnIsStillClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if got := askedCount(h); got != 1 {
		t.Fatalf("the judge was asked %d times, want the user's prompt classified once", got)
	}
}

func TestAHeldAgentPromptReenteredIntoTheClassifierPathJoinsUnjudged(t *testing.T) {
	// Arrange: an agent's prompt stands held against the running turn.
	h := newHarness(t)
	h.watcher.refuseJoins = true
	agentPromptJoining(t, h)
	h.watcher.refuseJoins = false

	// Act: the held prompt is re-entered, as an edit's commit does.
	h.q.classifyHeld(context.Background(), agentSubmission("t1", "the build is green"), "running-turn", dlog.NewTestLogger())
	h.q.waitForClassifications()

	// Assert
	if joins := h.sender.joins; len(joins) != 1 || joins[0] != "t1" || askedCount(h) != 0 {
		t.Fatalf("joins = %v, asked %d times; want t1 joined unjudged", joins, askedCount(h))
	}
}

func TestTheClassifierCallSiteRoutesAnAgentsPromptToJoinWithoutAskingTheModel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = interruptingVerdict

	// Act
	c, route := h.q.verdictFor(context.Background(), agentSubmission("t1", "the build is green"), "running-turn", dlog.NewTestLogger())

	// Assert
	if route != classifier.RouteAfterToolCall || c.Arm != wsm.ArmAfterToolCall || askedCount(h) != 0 {
		t.Fatalf("verdict = (%+v, %v), asked %d times; want after_tool_call and the model never asked", c, route, askedCount(h))
	}
}
