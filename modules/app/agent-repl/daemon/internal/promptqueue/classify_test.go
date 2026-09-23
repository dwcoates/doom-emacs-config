package promptqueue

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

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

func TestJudgeLogsAFailedClassifierAtErrorWithItsCauseAndTheRunningTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.err = errors.New("the vendor run failed")
	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert.
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == opClassify && r.Context["cause"] == "the vendor run failed" &&
			r.Context["running_turn"] == "running-turn" {
			return
		}
	}
	t.Fatalf("records = %+v, want one error carrying the classifier's cause and the running turn", h.log.Records())
}

func TestAPromptHeldByAFailedClassifierDeliversAtTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.err = errors.New("the vendor run failed")
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act.
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert.
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt delivered at the turn's end", started)
	}
}

// closeInStore closes a turn in the store alone, leaving the watcher — the
// queue's own account — still reporting it in flight.
func closeInStore(t *testing.T, h *harness, turn string) {
	t.Helper()
	if err := h.db.CloseTurn(context.Background(), idsTurn(turn), instant, wsm.CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}
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

// Every failure on the way to a verdict resolves to the prompt's TRUE state —
// held for the running turn's end — and never to classification_error, which
// the tray draws as "unclassified": a failure leaking into the UI.
func TestJudgeHoldsForTurnEndOnEveryFailure(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{
			name: "the running turn has no durable record",
			arrange: func(h *harness) {
				h.watcher.running("phantom-turn")
			},
		},
		{
			name: "the running turn was closed in the store while the queue still runs it",
			arrange: func(h *harness) {
				running(t, h, "running-turn", "the running work")
				closeInStore(t, h, "running-turn")
			},
		},
		{
			name: "the classifier call fails",
			arrange: func(h *harness) {
				running(t, h, "running-turn", "the running work")
				h.judge.err = errors.New("the vendor run failed")
			},
		},
		{
			name: "the open turns cannot be read",
			arrange: func(h *harness) {
				running(t, h, "running-turn", "the running work")
				h.db.openTurnsErr = errors.New("the store is unreachable")
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.arrange(h)
			// Act.
			if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert.
			if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmHoldForTurnEnd {
				t.Fatalf("arm = %s, want hold_for_turn_end", armName(got))
			}
		})
	}
}

func TestJudgeLogsARunningTurnTheStoreHasClosedAtErrorWithBothIDs(t *testing.T) {
	// Arrange: the incident of 2026-09-23 — a false turn-end closed the
	// queue's running turn in the store, and a newer turn stands open there.
	h := newHarness(t)
	running(t, h, "store-turn", "the store's open turn")
	running(t, h, "running-turn", "the running work")
	closeInStore(t, h, "running-turn")
	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert.
	for _, r := range h.log.Records() {
		if r.Level == "error" && r.Operation == opClassify && r.Context["running_turn"] == "running-turn" &&
			reflect.DeepEqual(r.Context["store_open_turns"], []string{"store-turn"}) {
			return
		}
	}
	t.Fatalf("records = %+v, want one error naming the running turn and the store's open turns", h.log.Records())
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

func TestARefusedInterruptReturnsThePromptToHeld(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "the shim refuses the kill", arrange: func(h *harness) { h.sender.killErr = errors.New("the turn is not the open one") }},
		{name: "the shim refuses a live turn", arrange: func(h *harness) { h.sender.killErr = liveRefusal{} }},
		{name: "the workspace has no session to send it to", arrange: func(h *harness) { h.noSession = true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
			release := h.judge.hold()
			if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			tt.arrange(h)
			// Act.
			release()
			h.q.waitForClassifications()
			// Assert.
			if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmHoldForTurnEnd {
				t.Fatalf("arm = %s, want hold_for_turn_end", armName(got))
			}
		})
	}
}

func TestARefusedInterruptClearsTheInterruptingStatus(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	h.sender.killErr = errors.New("the turn is not the open one")
	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert.
	if got := h.footer.interruptions(); got[len(got)-1] {
		t.Fatal("a refused interrupt must clear the interrupting status")
	}
}

// liveRefusal is the fake's stand-in for the workspace package's KillTurn
// refusal of a live turn, matched structurally exactly as the real one is.
type liveRefusal struct{}

func (liveRefusal) Error() string         { return "shim KillTurn refused: live" }
func (liveRefusal) KillRefusedLive() bool { return true }

func TestARefusedInterruptIsLoggedAtTheLevelItsNatureEarns(t *testing.T) {
	tests := []struct {
		name  string
		cause error
		level string
	}{
		{name: "a live turn's refusal is an expected outcome", cause: liveRefusal{}, level: "info"},
		{name: "any other refusal is unexpected", cause: errors.New("the turn is not the open one"), level: "warn"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
			h.sender.killErr = tt.cause
			// Act.
			if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert.
			var levels []string
			for _, r := range h.log.Records() {
				if r.Operation == opInterject && r.Context["cause"] == tt.cause.Error() {
					levels = append(levels, r.Level)
				}
			}
			if len(levels) != 1 || levels[0] != tt.level {
				t.Fatalf("refusal records at levels %v, want exactly one at %s", levels, tt.level)
			}
		})
	}
}

func TestAPromptWhoseInterruptWasRefusedDeliversAtTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	h.sender.killErr = liveRefusal{}
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act.
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert.
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt delivered at the turn's end", started)
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

// --- the exit's bounded join ----------------------------------------------
//
// A verdict and a background revival each read and write the state client off
// their own goroutine. Nothing joined them, so a SIGTERM landing inside one
// left `daemon.promptqueue.tray: could not read the standing holds — sql:
// database is closed` in the log of an ORDERLY exit.

func TestDrainReportsTrueWhenNoBackgroundWorkIsInFlight(t *testing.T) {
	// Arrange: a queue that has classified nothing.
	h := newHarness(t)

	// Act.
	left := h.q.Drain(time.Second)

	// Assert.
	if !left {
		t.Fatal("Drain = false with nothing in flight, want true")
	}
}

func TestDrainReportsFalseWhenAVerdictOutlivesTheBound(t *testing.T) {
	// Arrange: a classification held at the judge, so it is genuinely in
	// flight for as long as the test wants it to be.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	release := h.judge.hold()
	t.Cleanup(func() { release(); h.q.waitForClassifications() })
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Act.
	left := h.q.Drain(10 * time.Millisecond)

	// Assert: reported, never waited on forever.
	if left {
		t.Fatal("Drain = true with a verdict still in flight, want false")
	}
}

// Every classification arm renders under its own name in the log, and an arm
// nobody taught this function about renders its numeric value rather than
// silently reading as one of the known arms.
func TestArmNameRendersEveryClassificationArm(t *testing.T) {
	tests := []struct {
		name string
		arm  wsm.ClassificationArm
		want string
	}{
		{name: "classifying", arm: wsm.ArmClassifying, want: "classifying"},
		{name: "interject", arm: wsm.ArmInterject, want: "interject"},
		{name: "hold for turn end", arm: wsm.ArmHoldForTurnEnd, want: "hold_for_turn_end"},
		{name: "uninterruptible turn", arm: wsm.ArmUninterruptibleTurn, want: "uninterruptible_turn"},
		{name: "classification error", arm: wsm.ArmClassificationError, want: "classification_error"},
		{name: "an arm this renderer has never been taught", arm: wsm.ClassificationArm(97), want: "arm(97)"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := armName(tc.arm)

			// Assert.
			if got != tc.want {
				t.Fatalf("armName(%v) = %q, want %q", tc.arm, got, tc.want)
			}
		})
	}
}
