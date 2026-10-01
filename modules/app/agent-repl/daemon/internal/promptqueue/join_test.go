package promptqueue

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// A PROMPT THAT ADDS TO THE RUNNING WORK JOINS THE RUNNING TURN (owner ruling,
// 2026-09-30): the after_tool_call verdict sends it at once, with nothing
// interrupted, and the vendor folds it in or runs it next.

// afterToolCall is the classifier's after_tool_call verdict.
var afterToolCall = classifier.Verdict{Route: classifier.RouteAfterToolCall, Reason: "it adds to the running work"}

// sentToJoin submits T1 against the running turn under an after_tool_call
// verdict and waits for the verdict to settle.
func sentToJoin(t *testing.T, h *harness) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = afterToolCall
	if _, err := h.q.Submit(context.Background(), submission("t1", "also cover the edge case")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

// verdictOf reads the verdict stamped on TURN's hold.
func verdictOf(t *testing.T, h *harness, turn ids.TurnID) wsm.Classification {
	t.Helper()
	held, ok, err := h.db.HeldPromptByTurn(context.Background(), turn)
	if err != nil || !ok || held.Classification == nil {
		t.Fatalf("hold of %s = (%+v, %v, %v), want one carrying a verdict", turn, held, ok, err)
	}
	return *held.Classification
}

// hasRecord reports whether the queue logged a record at LEVEL under OP
// whose message is MESSAGE.
func hasRecord(h *harness, level, op, message string) bool {
	for _, r := range h.log.Records() {
		if r.Level == level && r.Operation == op && r.Message == message {
			return true
		}
	}
	return false
}

func TestAnAfterToolCallVerdictSendsThePromptToJoinTheRunningTurn(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	if got := h.sender.joins; len(got) != 1 || got[0] != "t1" {
		t.Fatalf("joins = %v, want t1 sent to join the running turn", got)
	}
}

func TestAnAfterToolCallVerdictInterruptsNothing(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted", killed)
	}
}

func TestAJoiningPromptIsRecordedWaitingBehindTheRunningTurn(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	if got := h.watcher.joining; len(got) != 1 || got[0] != "t1" {
		t.Fatalf("watcher joining = %v, want t1", got)
	}
}

func TestAJoiningPromptLeavesTheTrayOnceTheSessionTakesIt(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	held, ok, err := h.db.HeldPromptByTurn(context.Background(), "t1")
	if err != nil || !ok || held.Tombstone == nil || held.Tombstone.Kind != tombstoneDelivered {
		t.Fatalf("hold = (%+v, %v, %v), want it retired as delivered", held, ok, err)
	}
}

func TestAJoiningPromptsTurnIsRecordedWithItsText(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	turn, ok := h.db.startedTurn("t1")
	if !ok || turn.Text != "also cover the edge case" {
		t.Fatalf("turn = (%+v, %v), want t1 recorded with its text", turn, ok)
	}
}

func TestAJoiningPromptRaisesTheAfterToolCallStage(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	sentToJoin(t, h)

	// Assert
	stages := h.footer.submissionStages()
	if len(stages) == 0 || stages[len(stages)-1].Stage != footer.StageAfterToolCall {
		t.Fatalf("stages = %+v, want the after-tool-call stage last", stages)
	}
}

func TestAJoinTheShimRefusesWaitsForTheRunningTurnAndIsLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.sender.joinErr = errors.New("turn_already_open")

	// Act
	sentToJoin(t, h)

	// Assert
	c := verdictOf(t, h, "t1")
	if c.Arm != wsm.ArmHoldForTurnEnd || c.Reason != "the session would not take the prompt into the running turn, so it waits for it to end" {
		t.Fatalf("verdict = %+v, want the prompt waiting with the refusal's reason", c)
	}
	if !hasRecord(h, dlog.LevelError, opJoin, "the shim refused the prompt sent to join the running turn; it waits for it to end") {
		t.Fatalf("records = %+v, want the refusal at error", h.log.Records())
	}
	if got := h.watcher.openFailed; len(got) != 1 || got[0] != "t1" {
		t.Fatalf("open failed = %v, want the refused join retired from the watcher", got)
	}
}

func TestAJoinBehindAVendorStartedTurnWaitsForIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.watcher.refuseJoins = true

	// Act
	sentToJoin(t, h)

	// Assert
	c := verdictOf(t, h, "t1")
	if len(h.sender.joins) != 0 || c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("joins = %v, verdict = %+v; want nothing sent and the prompt waiting", h.sender.joins, c)
	}
}

func TestAJoinWhoseTurnEndedBeforeItWasSentWaitsItsTurn(t *testing.T) {
	// Arrange: the verdict is reached as the running turn ends.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = afterToolCall
	h.judge.onJudge = func() { h.watcher.idle() }

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "also cover the edge case")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	c := verdictOf(t, h, "t1")
	if len(h.sender.joins) != 0 || c.Reason != "the turn it was to join ended before it was sent, so it waits its turn" {
		t.Fatalf("joins = %v, verdict = %+v; want nothing sent and the prompt waiting its turn", h.sender.joins, c)
	}
}

func TestAPromptBehindAJoiningPromptIsNeverClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentToJoin(t, h)
	asked := askedCount(h)

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t2", "and another thing")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	c := verdictOf(t, h, "t2")
	if askedCount(h) != asked || c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("asked %d more times, verdict = %+v; want the prompt waiting unjudged", askedCount(h)-asked, c)
	}
}

func TestASecondJoinWhileOneWaitsWaitsItsTurn(t *testing.T) {
	// Arrange: two prompts judged against the running turn at once.
	h := newHarness(t)
	sentToJoin(t, h)
	sub := submission("t2", "and another thing")
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Workspace: theWorkspace, Turn: "t2", Said: sub.Said, Origin: sub.Origin.String(), QueuedAt: instant,
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}

	// Act
	h.q.settle(context.Background(), sub, "running-turn", h.q.contentEpoch(theWorkspace, "t2"), wsm.Classification{
		Arm: wsm.ArmAfterToolCall, Reason: "it adds", At: instant,
	}, classifier.RouteAfterToolCall, dlog.NewTestLogger())

	// Assert
	if got := h.sender.joins; len(got) != 1 {
		t.Fatalf("joins = %v, want only the first prompt sent", got)
	}
	if c := verdictOf(t, h, "t2"); c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("verdict = %+v, want the second prompt waiting", c)
	}
}

func TestAFoldedJoinClosesItsTurnAsFoldedAndRaisesNoBanner(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentToJoin(t, h)

	// Act
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFolded)

	// Assert
	how, ok := h.db.closedTurn("t1")
	if !ok || how != wsm.CloseFolded {
		t.Fatalf("close = (%s, %v), want folded", how, ok)
	}
	if ended := h.banners.ended; len(ended) != 0 {
		t.Fatalf("banners = %+v, want none for a prompt that ran no turn", ended)
	}
}

func TestAFoldedJoinLeavesTheRunningTurnsInterruptStanding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentToJoin(t, h)
	before := len(h.footer.interruptions())

	// Act
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFolded)

	// Assert
	if got := len(h.footer.interruptions()); got != before {
		t.Fatalf("interrupting status moved %d times, want the running turn's left alone", got-before)
	}
}

func TestAFoldedJoinLetsTheNextPromptBeClassifiedAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentToJoin(t, h)
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFolded)
	asked := askedCount(h)
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t2", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if askedCount(h) != asked+1 {
		t.Fatalf("asked %d more times, want the classifier asked once", askedCount(h)-asked)
	}
}

func TestAFoldedCloseForATurnThatNeverJoinedIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")

	// Act
	h.q.OnTurnEnded(theWorkspace, "stray", wsm.CloseFolded)

	// Assert
	if !hasRecord(h, dlog.LevelError, opJoin, "a turn closed as folded that was not the prompt joining the running turn") {
		t.Fatalf("records = %+v, want the stray folded close at error", h.log.Records())
	}
}

func TestAJoinThatRunsAsItsOwnTurnTakesTheTurnFact(t *testing.T) {
	// Arrange: the running turn ends without folding the prompt in, and the
	// watcher stands it in flight in its place.
	h := newHarness(t)
	sentToJoin(t, h)
	h.watcher.running("t1")

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	turns := h.footer.startedTurns()
	if len(turns) == 0 || turns[len(turns)-1].Prompt != "also cover the edge case" {
		t.Fatalf("footer turns = %+v, want the joining prompt's turn fact", turns)
	}
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing popped into the prompt's own turn", started)
	}
}

func TestAJoinThatRanAsItsOwnTurnNoLongerHoldsPromptsBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentToJoin(t, h)
	h.watcher.running("t1")
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	asked := askedCount(h)
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t2", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if askedCount(h) != asked+1 {
		t.Fatalf("asked %d more times, want the classifier asked about the prompt behind the joined turn", askedCount(h)-asked)
	}
}

func TestAJoinThatDidNotStandWhenItsTurnEndedIsAnError(t *testing.T) {
	// Arrange: the watcher reports nothing running after the joined turn ends.
	h := newHarness(t)
	sentToJoin(t, h)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if !hasRecord(h, dlog.LevelError, opJoin, "the turn a prompt was sent to join ended, but the prompt does not run in its place") {
		t.Fatalf("records = %+v, want the missing join at error", h.log.Records())
	}
}

func TestAnAfterToolCallVerdictAgainstAQueuedPromptCoalescesTheTwo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = afterToolCall

	// Act
	if _, err := h.q.Submit(context.Background(), submission("newer", "and also this")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	older, _, _ := h.db.HeldPromptByTurn(context.Background(), "older")
	if !older.Coalesced || len(h.sender.joins) != 0 {
		t.Fatalf("older = %+v, joins = %v; want the two coalesced and nothing sent", older, h.sender.joins)
	}
}

func TestRouteArmRecordsEachRouteAsItsVerdict(t *testing.T) {
	tests := []struct {
		route classifier.Route
		want  wsm.ClassificationArm
	}{
		{route: classifier.RouteQueue, want: wsm.ArmHoldForTurnEnd},
		{route: classifier.RouteAfterToolCall, want: wsm.ArmAfterToolCall},
		{route: classifier.RouteInterrupt, want: wsm.ArmInterject},
	}
	for _, tt := range tests {
		t.Run(tt.route.String(), func(t *testing.T) {
			// Act
			got := routeArm(tt.route)
			// Assert
			if got != tt.want {
				t.Fatalf("routeArm(%s) = %s, want %s", tt.route, got, tt.want)
			}
		})
	}
}

func TestRouteArmRefusesAnUndeclaredRoute(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("routeArm must panic on a route with no verdict arm")
		}
	}()
	// Act
	routeArm(classifier.Route(9))
}

// --- a folded prompt whose turn failed ---------------------------------------

// foldedThenEnded sends T1 to join the running turn, has the vendor fold it
// in, and ends the running turn as HOW with the session left free.
func foldedThenEnded(t *testing.T, h *harness, how wsm.TurnClose) {
	t.Helper()
	sentToJoin(t, h)
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFolded)
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", how)
}

// resubmitted answers the turns started since the join, none of which is the
// folded prompt's own turn.
func resubmitted(h *harness) []ids.TurnID {
	var out []ids.TurnID
	for _, turn := range h.sender.started() {
		if turn != "t1" && turn != "running-turn" {
			out = append(out, turn)
		}
	}
	return out
}

func TestAPromptFoldedIntoATurnThatFailedRunsAsItsOwnTurn(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseFailed)

	// Assert
	turns := resubmitted(h)
	if len(turns) != 1 {
		t.Fatalf("resubmitted = %v, want the folded prompt run once as its own turn", turns)
	}
	if turn, ok := h.db.startedTurn(turns[0]); !ok || turn.Text != "also cover the edge case" {
		t.Fatalf("turn = (%+v, %v), want the folded prompt's text", turn, ok)
	}
}

func TestAPromptFoldedIntoATurnWhoseAgentDiedRunsAsItsOwnTurn(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseAgentDied)

	// Assert
	if turns := resubmitted(h); len(turns) != 1 {
		t.Fatalf("resubmitted = %v, want the folded prompt run as its own turn", turns)
	}
}

func TestAPromptFoldedIntoATurnThatCompletedIsNotResubmitted(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseCompleted)

	// Assert
	if turns := resubmitted(h); len(turns) != 0 {
		t.Fatalf("resubmitted = %v, want nothing: the turn that took the prompt answered it", turns)
	}
}

func TestAPromptFoldedIntoATurnTheUserStoppedIsNotResubmitted(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseKilled)

	// Assert
	if turns := resubmitted(h); len(turns) != 0 {
		t.Fatalf("resubmitted = %v, want nothing after a stop", turns)
	}
}

func TestAResubmittedFoldedPromptKeepsItsOrigin(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseFailed)

	// Assert
	turns := resubmitted(h)
	if len(turns) != 1 {
		t.Fatalf("resubmitted = %v, want one turn", turns)
	}
	original, _ := h.db.startedTurn("t1")
	if turn, _ := h.db.startedTurn(turns[0]); turn.Origin != original.Origin {
		t.Fatalf("origin = %q, want the folded prompt's own %q", turn.Origin, original.Origin)
	}
}

func TestAFoldedPromptIsResubmittedOnlyOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	foldedThenEnded(t, h, wsm.CloseFailed)
	turns := resubmitted(h)
	if len(turns) != 1 {
		t.Fatalf("resubmitted = %v, want one turn", turns)
	}

	// Act: the resubmitted turn fails too.
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, turns[0], wsm.CloseFailed)

	// Assert
	if again := resubmitted(h); len(again) != 1 {
		t.Fatalf("resubmitted = %v, want no second resubmission", again)
	}
}
