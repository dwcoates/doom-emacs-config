package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/classifier"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/netstate"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
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

func TestJudgeFailureIsRecordedAtTheLevelItsCauseEarns(t *testing.T) {
	tests := []struct {
		name     string
		verdict  netstate.Verdict
		level    string
		message  string
		evidence any
	}{
		{
			name:     "a classifier the network failed is an expected condition",
			verdict:  netstate.Verdict{Network: true, Evidence: netstate.EvidenceFaultOpen, Detail: "a network_unreachable fault stands"},
			level:    "info",
			message:  "the classifier failed because this machine cannot reach the network; the prompt waits for the running turn to end",
			evidence: "network_fault_open",
		},
		{
			name:     "a classifier that failed otherwise is a defect",
			verdict:  netstate.Verdict{Detail: "the API answered"},
			level:    "error",
			message:  "the classifier failed; the prompt waits for the running turn to end",
			evidence: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.err = errors.New("timeout: signal: killed")
			h.network.verdict = tt.verdict

			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()

			// Assert
			var found []dlog.Record
			for _, r := range h.log.Records() {
				if r.Operation == opClassify && r.Message == tt.message {
					found = append(found, r)
				}
			}
			if len(found) != 1 || found[0].Level != tt.level || found[0].Context["cause"] != "timeout: signal: killed" ||
				found[0].Context["running_turn"] != "running-turn" || found[0].Context["network_evidence"] != tt.evidence {
				t.Fatalf("records = %+v, want one %s %q carrying the cause, the running turn and evidence %v", found, tt.level, tt.message, tt.evidence)
			}
			if got := h.network.asked; len(got) != 1 || got[0] != "timeout: signal: killed" {
				t.Fatalf("network asked about %q, want the classifier's own failure", got)
			}
		})
	}
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

// Every verdict and its reason is recorded at info, so why a prompt
// interrupted or waited is on disk.
func TestEveryVerdictIsLoggedAtInfoWithItsReason(t *testing.T) {
	tests := []struct {
		name     string
		verdict  classifier.Verdict
		judgeErr error
		arm      string
		reason   string
	}{
		{name: "a holding verdict", verdict: classifier.Verdict{Reason: "independent"}, arm: "hold_for_turn_end", reason: "independent"},
		{name: "an interjecting verdict", verdict: classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}, arm: "interject", reason: "it countermands the work"},
		{name: "a failed classifier's verdict", judgeErr: errors.New("the vendor run failed"), arm: "hold_for_turn_end", reason: "the classifier could not decide, so the prompt waits for the running turn to end"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict, h.judge.err = tt.verdict, tt.judgeErr
			// Act.
			if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert.
			for _, r := range h.log.Records() {
				if r.Level == "info" && r.Operation == opClassify && r.Context["arm"] == tt.arm && r.Context["reason"] == tt.reason {
					return
				}
			}
			t.Fatalf("records = %+v, want the %s verdict at info with its reason", h.log.Records(), tt.arm)
		})
	}
}

func TestAContextCutVerdictIsLoggedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.q.state(theWorkspace).cut = &runningCut{turn: "t-running", command: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR}
	h.watcher.running("t-running")
	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert.
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opClassify {
			return
		}
	}
	t.Fatalf("records = %+v, want the uninterruptible verdict at info", h.log.Records())
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
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "an unrelated question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if got := h.db.hold("t1").Classification.Arm; got != wsm.ArmHoldForTurnEnd {
		t.Fatalf("arm = %s, want hold_for_turn_end", got.String())
	}
}

func TestJudgeKeepsTheJudgesReasonAsEvidence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "genuinely independent"}
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
				t.Fatalf("arm = %s, want hold_for_turn_end", got.String())
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
		t.Fatalf("arm = %s, want uninterruptible_turn", held.Classification.Arm.String())
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
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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

// TestInterjectStatesTheStopAsAnInterjection covers the HOW the kill carries:
// the stop is the superseding prompt's side effect, so the record must say
// interjection and the feed then draws no interruption bubble for it.
func TestInterjectStatesTheStopAsAnInterjection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "stop doing that")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	commands := h.sender.killedCommands()
	if len(commands) != 1 || commands[0].GetInterjection() == nil {
		t.Fatalf("commanded_by = %v, want exactly one interjection", commands)
	}
}

// TestInterjectKillsTheRunningTurnUnforced covers the interjection's stop:
// it is the graceful kill, so the turn's detached work runs on past it.
func TestInterjectKillsTheRunningTurnUnforced(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "stop doing that")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if forces := h.sender.killedForces(); len(forces) != 1 || forces[0] {
		t.Fatalf("kill forces = %v, want exactly one unforced kill", forces)
	}
}

func TestInterjectWaitsForTheTurnsRealEndBeforeDelivering(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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

func TestAnInterruptVerdictAgainstAQueuedPromptCoalescesTheTwo(t *testing.T) {
	// Arrange: an older prompt is already queued, and the next one is ruled to
	// interrupt it (owner rule, 2026-09-30): nothing has started, so the two
	// are folded into one.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the queued prompt"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert: one entry, the older one, carrying both, marked coalesced.
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 1 || standing[0].Turn != "older" || !standing[0].Coalesced {
		t.Fatalf("standing = (%+v, %v), want the older prompt alone, coalesced", standing, err)
	}
	if got := saidText(standing[0].Said); got != "an earlier question\n\ndo it the other way" {
		t.Fatalf("merged text = %q, want the older prompt's text then the later one's after a blank line", got)
	}
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted: the prompt ahead had not started", killed)
	}
}

func TestACoalescenceNamesTheFoldOnTheTraysPush(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the queued prompt"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if got := h.holds.pushedFolds(); len(got) != 1 || got[0] != (holds.Fold{From: "later", Into: "older"}) {
		t.Fatalf("pushed folds = %+v, want the later prompt folded into the older one alone", got)
	}
}

func TestAFailedCoalescenceLeavesBothPromptsStanding(t *testing.T) {
	// Arrange: the merge and the retirement are one store transaction, so a
	// store that refuses it leaves the queued prompt's words and the new
	// prompt's entry exactly as they were, and the new prompt waits its turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the queued prompt"}
	h.db.coalesceErr = errors.New("disk I/O error")

	// Act
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	standing, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil || len(standing) != 2 || standing[0].Coalesced {
		t.Fatalf("standing = (%+v, %v), want both prompts, neither coalesced", standing, err)
	}
	if got := saidText(standing[0].Said); got != "an earlier question" {
		t.Fatalf("older text = %q, want its own words alone", got)
	}
	if !logged(h.log.Records(), "error", opClassify, "could not fold the prompt into the queued prompt ahead; both entries stand as they were") {
		t.Fatalf("the failed coalescence was not recorded at error: %v", h.log.Records())
	}
	if got := h.holds.pushedFolds(); len(got) != 0 {
		t.Fatalf("pushed folds = %+v, want none for a coalescence the store refused", got)
	}
}

func TestAPromptJudgedAgainstAQueuedPromptIsJudgedAgainstThatPromptsText(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Act
	if _, err := h.q.Submit(context.Background(), submission("later", "and another")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	asked := h.judge.questions()
	if len(asked) != 2 || asked[1][0] != "an earlier question" {
		t.Fatalf("asked = %v, want the later prompt judged against the queued one ahead of it", asked)
	}
}

func TestACoalescedPromptIsDeliveredAsOneTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the queued prompt"}
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "older" {
		t.Fatalf("started = %v, want the coalesced prompt as the one turn", started)
	}
}

func TestACoalescedPromptTellsTheFooter(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the queued prompt"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("later", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if !logged(h.log.Records(), "info", opClassify, "the prompt was ruled to interrupt a prompt that had not started; it is folded into it") {
		t.Fatalf("the coalescing was not recorded at info: %v", h.log.Records())
	}
}
func TestARefusedInterruptReturnsThePromptToHeld(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "the shim refuses the kill", arrange: func(h *harness) { h.sender.killErr = errors.New("the turn is not the open one") }},
		{name: "the shim refuses a live turn", arrange: func(h *harness) { h.sender.killErr = liveRefusal{} }},
		{name: "the shim finds no turn open", arrange: func(h *harness) { h.sender.killErr = noTurnOpenRefusal{} }},
		{name: "the workspace has no session to send it to", arrange: func(h *harness) { h.noSession = true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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
				t.Fatalf("arm = %s, want hold_for_turn_end", got.String())
			}
		})
	}
}

func TestARefusedInterruptClearsTheInterruptingStatus(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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

// noTurnOpenRefusal is the fake's stand-in for the workspace package's KillTurn
// refusal that found no turn open, matched structurally exactly as the real one
// is.
type noTurnOpenRefusal struct{}

func (noTurnOpenRefusal) Error() string             { return "shim KillTurn refused: no_turn_open" }
func (noTurnOpenRefusal) KillFoundNoTurnOpen() bool { return true }

// notTheOpenTurnRefusal is the fake's stand-in for the workspace package's
// KillTurn refusal naming a turn that is not the shim's open one, matched
// structurally exactly as the real one is.
type notTheOpenTurnRefusal struct{}

func (notTheOpenTurnRefusal) Error() string {
	return "shim KillTurn refused: not_the_open_turn"
}
func (notTheOpenTurnRefusal) KillRefusedNotTheOpenTurn() bool { return true }

func TestARefusedInterruptIsLoggedAtTheLevelItsNatureEarns(t *testing.T) {
	tests := []struct {
		name    string
		cause   error
		during  func(h *harness)
		level   string
		message string
	}{
		{
			name:    "a turn that already ended is an expected race",
			cause:   noTurnOpenRefusal{},
			level:   "info",
			message: "the running turn had already ended when the interrupt landed; the jump is stripped and the prompt waits for the turn's end",
		},
		{
			name:    "a live refusal is a contract breach the shim no longer produces",
			cause:   liveRefusal{},
			level:   "warn",
			message: "the shim refused an unforced interrupt as live, which its contract no longer produces; the jump is stripped and the prompt waits for the turn to end",
		},
		{
			name:  "a turn replaced by the next before the kill landed is an expected race",
			cause: notTheOpenTurnRefusal{},
			during: func(h *harness) {
				h.sender.callHook = func(call string) {
					if call == "KillTurn" {
						h.watcher.running("next-turn")
					}
				}
			},
			level:   "info",
			message: "the running turn had already ended and the next one opened when the interrupt landed; the jump is stripped",
		},
		{
			name:    "a kill refused as not the open turn the queue still runs is a disagreement",
			cause:   notTheOpenTurnRefusal{},
			level:   "warn",
			message: "the shim refused the interrupt as not naming its open turn, though the queue still runs that turn; the jump is stripped and the prompt waits for the turn to end",
		},
		{
			name:    "any other refusal is unexpected",
			cause:   errors.New("the turn is not the open one"),
			level:   "warn",
			message: "the interrupt was refused; the jump is stripped and the prompt waits for the turn to end",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
			h.sender.killErr = tt.cause
			if tt.during != nil {
				tt.during(h)
			}
			// Act.
			if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert.
			var got []string
			for _, r := range h.log.Records() {
				if r.Operation == opInterject && r.Context["cause"] == tt.cause.Error() {
					got = append(got, r.Level+": "+r.Message)
				}
			}
			if want := tt.level + ": " + tt.message; len(got) != 1 || got[0] != want {
				t.Fatalf("refusal records = %q, want exactly %q", got, want)
			}
		})
	}
}

func TestAPromptWhoseInterruptWasRefusedDeliversAtTheTurnsEnd(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "an earlier question")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
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
	h.q.state("ws-1").cut = &runningCut{turn: "t-running", command: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR}
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
	h.q.state("ws-1").cut = &runningCut{turn: "t-running", command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT}
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

// --- a running session act cannot be interjected ---------------------------
//
// The refusal is on interject itself, the one path every interjection takes,
// so a verdict that was already in flight when a /clear or /compact began
// cannot interrupt it.

// verdictSettlesUnderACut holds an interjecting verdict in flight against an
// ordinary running turn, begins a context cut underneath it, then lets the
// verdict settle.
func verdictSettlesUnderACut(t *testing.T, h *harness, command conversationv1.SessionCommand) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	h.beginCut("cut-1", command)
	release()
	h.q.waitForClassifications()
}

func TestAnInterjectingVerdictSettlingUnderAContextCutSendsNoInterrupt(t *testing.T) {
	tests := []struct {
		name    string
		command conversationv1.SessionCommand
	}{
		{name: "under /compact", command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT},
		{name: "under /clear", command: conversationv1.SessionCommand_SESSION_COMMAND_CLEAR},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			verdictSettlesUnderACut(t, h, tt.command)
			// Assert
			if killed := h.sender.killed(); len(killed) != 0 {
				t.Fatalf("killed = %v, want no interrupt while a session act runs", killed)
			}
		})
	}
}

func TestAnInterjectingVerdictSettlingUnderAContextCutInstallsNoQueueJump(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	verdictSettlesUnderACut(t, h, conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	// Assert
	h.q.mu.Lock()
	head := h.q.states[theWorkspace].head
	h.q.mu.Unlock()
	if head != nil {
		t.Fatalf("head = %s, want no queue jump past a running session act", *head)
	}
}

func TestAnInterjectingVerdictSettlingUnderAContextCutIsStampedUninterruptible(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	verdictSettlesUnderACut(t, h, conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	// Assert
	held := h.db.hold("t1")
	if held.Classification.Arm != wsm.ArmUninterruptibleTurn ||
		held.Classification.Command != conversationv1.SessionCommand_SESSION_COMMAND_COMPACT {
		t.Fatalf("classification = %+v, want uninterruptible_turn naming /compact", held.Classification)
	}
}

func TestAPromptKeptFromInterjectingByASessionActIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	verdictSettlesUnderACut(t, h, conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)
	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opInterject &&
			r.Context["session_act_turn"] == "cut-1" &&
			r.Context["session_act"] == conversationv1.SessionCommand_SESSION_COMMAND_COMPACT.String() &&
			r.Context["held_turn"] == "t1" {
			return
		}
	}
	t.Fatalf("records = %+v, want one info naming the act's turn, the act and the held prompt's turn", h.log.Records())
}

func TestAnInterjectingVerdictStillInterruptsAnOrdinaryTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the ordinary running turn interrupted", killed)
	}
}

// --- a prompt that IS a session act is queued, never classified ------------

func TestASessionActPromptNeverReachesTheClassifierWhileATurnRuns(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "a bare /compact", text: "/compact"},
		{name: "/compact with instructions", text: "/compact foo bar"},
		{name: "/compact with instructions on the next line", text: "/compact\nfoo bar"},
		{name: "a bare /clear", text: "/clear"},
		{name: "/clear with trailing text", text: "/clear foo"},
		{name: "the /reset alias", text: "/reset"},
		{name: "the /new alias", text: "/new"},
		{name: "the /reset alias with trailing text", text: "/reset foo"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", tt.text)); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert
			if asked := h.judge.questions(); len(asked) != 0 {
				t.Fatalf("classifier asked %v, want a session act never classified", asked)
			}
		})
	}
}

func TestASessionActPromptIsHeldForTheRunningTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	got, err := h.q.Submit(context.Background(), submission("t1", "/compact foo bar"))
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	if got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("classification = %+v, want hold_for_turn_end with no classifier", got.Classification)
	}
}

func TestASessionActPromptNeverInterruptsTheRunningTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "/compact")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Assert
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want no routing verdict and so no interrupt", killed)
	}
}

func TestASessionActPromptKeptFromTheClassifierIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "/compact")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opClassify && r.Context["held_turn"] == "t1" &&
			r.Context["session_act"] == conversationv1.SessionCommand_SESSION_COMMAND_COMPACT.String() &&
			r.Context["running_turn"] == "running-turn" {
			return
		}
	}
	t.Fatalf("records = %+v, want one info naming the held turn, the act and the running turn", h.log.Records())
}

func TestANearMissOfASessionActIsClassifiedAsBefore(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "a longer word that begins with the literal", text: "/compacting"},
		{name: "the literal not at the start", text: "please /compact"},
		{name: "a longer word that begins with /new", text: "/newer"},
		{name: "a longer word that begins with /reset", text: "/resetting"},
		{name: "an alias not at the start", text: "please /new"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", tt.text)); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()
			// Assert
			if asked := h.judge.questions(); len(asked) != 1 || asked[0][1] != tt.text {
				t.Fatalf("classifier asked %v, want the prompt routed as today", asked)
			}
		})
	}
}

// ---- a deferred prompt is never classified -------------------------------
//
// agentrepl.v1 SubmitPromptDelivery.DEFERRED: run as its own turn after the
// current one, never interjected. The classifier's interject is exactly what
// the user excluded, so the model is never asked.

// deferredSubmission composes one deferred submission.
func deferredSubmission(turn ids.TurnID, text string) Submission {
	sub := submission(turn, text)
	sub.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_DEFERRED_PROMPT
	sub.Delivery = wsm.DeliveryDeferred
	return sub
}

// askedCount answers how many times the judge was asked.
func askedCount(h *harness) int {
	h.judge.mu.Lock()
	defer h.judge.mu.Unlock()
	return len(h.judge.asked)
}

func TestADeferredPromptBehindARunningTurnIsHeldForItsEndUnjudged(t *testing.T) {
	// Arrange: a verdict that would interject, were the model ever asked.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}

	// Act
	got, err := h.q.Submit(context.Background(), deferredSubmission("t1", "after this, run the tests"))
	h.q.waitForClassifications()

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("disposition = %+v, want held for the running turn's end", got)
	}
	if n := askedCount(h); n != 0 {
		t.Fatalf("the judge was asked %d times, want never for a deferred prompt", n)
	}
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want nothing interjected", started)
	}
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want the running turn never interrupted for a deferred prompt", killed)
	}
}

func TestADeferredHoldRecordsItsDelivery(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")

	// Act
	if _, err := h.q.Submit(context.Background(), deferredSubmission("t1", "later")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	held, err := h.q.standingHold(context.Background(), theWorkspace, "t1")
	if err != nil || held.Delivery != wsm.DeliveryDeferred {
		t.Fatalf("hold = (%+v, %v), want its deferred delivery stored", held, err)
	}
}

func TestADeferredPromptIsDeliveredAtOnceWhenNothingRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got, err := h.q.Submit(context.Background(), deferredSubmission("t1", "run the tests"))

	// Assert
	if err != nil || !got.Delivered {
		t.Fatalf("Submit = (%+v, %v), want delivered at once: there is no turn to wait for", got, err)
	}
}

// ---- a prompt sent now goes past the queue, never classified -------------
//
// agentrepl.v1 SubmitPromptDelivery.NOW: whatever would hold the prompt, it is
// held already pressed "Send now", so it goes as a released held prompt does.

// nowSubmission composes one submission sent now.
func nowSubmission(turn ids.TurnID, text string) Submission {
	sub := submission(turn, text)
	sub.Delivery = wsm.DeliveryNow
	return sub
}

func TestAPromptSentNowJoinsTheRunningTurnUnjudged(t *testing.T) {
	// Arrange: a verdict that would interject, were the model ever asked.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}

	// Act
	_, err := h.q.Submit(context.Background(), nowSubmission("t1", "do this now"))
	h.q.waitForClassifications()

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got := h.sender.joins; len(got) != 1 || got[0] != "t1" {
		t.Fatalf("joins = %v, want t1 sent to join the running turn at once", got)
	}
	if n := askedCount(h); n != 0 {
		t.Fatalf("the judge was asked %d times, want never for a prompt sent now", n)
	}
	if !logged(h.log.Records(), "info", opClassify, "the prompt was sent now; it goes past every held prompt and is never classified") {
		t.Fatalf("records = %+v, want the unjudged hold on disk", h.log.Records())
	}
}

func TestAPromptSentNowGoesPastTheHeldPromptsAheadOfIt(t *testing.T) {
	// Arrange: t1 is queued behind the running turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})

	// Act
	if _, err := h.q.Submit(context.Background(), nowSubmission("t2", "do this now")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if got := h.sender.joins; len(got) != 1 || got[0] != "t2" {
		t.Fatalf("joins = %v, want only t2, past t1", got)
	}
	if held := h.db.hold("t1"); held.Tombstone != nil {
		t.Fatalf("t1 = %+v, want it still queued", held)
	}
}

func TestAPromptSentNowIsDeliveredAtOnceWhenNothingRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	got, err := h.q.Submit(context.Background(), nowSubmission("t1", "run the tests"))

	// Assert
	if err != nil || !got.Delivered {
		t.Fatalf("Submit = (%+v, %v), want delivered at once", got, err)
	}
}

func TestAPromptSentNowThatCannotGoYetWaitsMarkedSentNow(t *testing.T) {
	// Arrange: a hibernated session holds every prompt until it comes up.
	h := newHarness(t)
	h.lease(wsm.HolderHibernate, wsm.PolicyHold)

	// Act
	_, err := h.q.Submit(context.Background(), nowSubmission("t1", "hello"))

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	held := h.db.hold("t1")
	if held.SentNow == nil || held.Delivery != wsm.DeliveryNow {
		t.Fatalf("hold = %+v, want it waiting marked sent now, its delivery stored", held)
	}
	if len(h.sender.started()) != 0 {
		t.Fatalf("started = %v, want nothing sent before the session is up", h.sender.started())
	}
}

func TestAPromptSentNowBehindAnEditStaysQueuedUnmarked(t *testing.T) {
	// Arrange: t1 is held and open in the editor.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"})
	beginEdit(t, h, "t1")
	asked := askedCount(h)

	// Act
	_, err := h.q.Submit(context.Background(), nowSubmission("t2", "do this now"))
	h.q.waitForClassifications()

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	held := h.db.hold("t2")
	if held.Tombstone != nil || held.SentNow != nil {
		t.Fatalf("t2 = %+v, want it queued behind the edit with no Send now standing", held)
	}
	if len(h.sender.joins) != 0 || askedCount(h) != asked {
		t.Fatalf("joins = %v, asked = %d more, want t2 neither sent nor judged", h.sender.joins, askedCount(h)-asked)
	}
	if !logged(h.log.Records(), "info", opRelease, msgReleaseWithdrawn) {
		t.Fatalf("records = %+v, want the refused Send now on disk", h.log.Records())
	}
}

func TestDeferredPromptsRunAsTheirOwnTurnsOneTurnEndAtATime(t *testing.T) {
	// Arrange: two deferred prompts behind a running turn.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	for _, turn := range []ids.TurnID{"t1", "t2"} {
		if _, err := h.q.Submit(context.Background(), deferredSubmission(turn, "deferred "+string(turn))); err != nil {
			t.Fatalf("Submit(%s): %v", turn, err)
		}
	}

	// Act
	turnEnds(h)

	// Assert: only the first starts; the second waits for ITS end.
	if started := h.sender.started(); !reflect.DeepEqual(started, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want only the first deferred prompt", started)
	}
}

func TestAnEditedDeferredPromptIsNotJudged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if _, err := h.q.Submit(context.Background(), deferredSubmission("t1", "later")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	beginEdit(t, h, "t1")

	// Act
	if err := h.q.CommitEdit(context.Background(), theWorkspace, "t1", userSaid("later, edited")); err != nil {
		t.Fatalf("CommitEdit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if n := askedCount(h); n != 0 {
		t.Fatalf("the judge was asked %d times, want never for an edited deferred prompt", n)
	}
	held, err := h.q.standingHold(context.Background(), theWorkspace, "t1")
	if err != nil || held.Classification == nil || held.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("hold = (%+v, %v), want it still held for the turn's end", held, err)
	}
}

func TestTheJudgeRefusesADeferredPromptAtItsOwnCallSite(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "urgent"}
	log := dlog.NewTestLogger()

	// Act
	verdict, route := h.q.verdictFor(context.Background(), deferredSubmission("t1", "later"), "running-turn", log)

	// Assert
	if route != classifier.RouteQueue || verdict.Arm != wsm.ArmHoldForTurnEnd || askedCount(h) != 0 {
		t.Fatalf("verdictFor = (%+v, %s) after %d asks, want hold_for_turn_end and the model never asked", verdict, route, askedCount(h))
	}
}

func TestNeverJudgedAnswersEveryPromptTheModelIsNeverAsked(t *testing.T) {
	tests := []struct {
		name   string
		sub    Submission
		wantOK bool
	}{
		{name: "a session act", sub: submission("t1", "/compact"), wantOK: true},
		{name: "a deferred prompt", sub: deferredSubmission("t1", "later"), wantOK: true},
		{name: "an ordinary prompt", sub: submission("t1", "and also this"), wantOK: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			verdict, why, ok := h.q.neverJudged(tc.sub)

			// Assert
			if ok != tc.wantOK {
				t.Fatalf("neverJudged ok = %v, want %v", ok, tc.wantOK)
			}
			if ok && (verdict.Arm != wsm.ArmHoldForTurnEnd || why == nil) {
				t.Fatalf("neverJudged = (%+v, why set %v), want hold_for_turn_end with its record", verdict, why != nil)
			}
		})
	}
}

// ---- the footer's submitting stages ------------------------------------------

// stagesOf answers the stages the footer was told, in order.
func stagesOf(subs []footer.Submission) []footer.SubmissionStage {
	out := make([]footer.SubmissionStage, len(subs))
	for i, sub := range subs {
		out[i] = sub.Stage
	}
	return out
}

func TestAJudgedPromptReportsClassifyingThenItsPlaceInTheQueue(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "it can wait"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "and then this")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	subs := h.footer.submissionStages()
	want := []footer.SubmissionStage{footer.StageClassifying, footer.StageHeld}
	if got := stagesOf(subs); !reflect.DeepEqual(got, want) {
		t.Fatalf("stages = %v, want %v", got, want)
	}
	if held := subs[1]; held.Position != 1 || held.Queued != 1 {
		t.Fatalf("held = %+v, want place 1 of 1 for the prompt", held)
	}
}

func TestAnInterjectingPromptReportsClassifyingThenInterjecting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "stop doing that")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	want := []footer.SubmissionStage{footer.StageClassifying, footer.StageInterjecting}
	if got := stagesOf(h.footer.submissionStages()); !reflect.DeepEqual(got, want) {
		t.Fatalf("stages = %v, want %v", got, want)
	}
}

func TestAPromptHeldUnderAContextCutReportsOnlyItsPlace(t *testing.T) {
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
	want := []footer.SubmissionStage{footer.StageHeld}
	if got := stagesOf(h.footer.submissionStages()); !reflect.DeepEqual(got, want) {
		t.Fatalf("stages = %v, want %v: no classifier runs under a context cut", got, want)
	}
}

func TestQueuePlaceCountsTheStandingHoldsInOrder(t *testing.T) {
	holds := []wsm.HeldPrompt{{Turn: "a"}, {Turn: "b"}, {Turn: "c"}}
	tests := []struct {
		name         string
		turn         ids.TurnID
		wantPosition uint32
		wantQueued   uint32
	}{
		{"the first", "a", 1, 3},
		{"the last", "c", 3, 3},
		{"one no longer standing", "gone", 0, 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			position, queued := queuePlace(holds, tt.turn)

			// Assert
			if position != tt.wantPosition || queued != tt.wantQueued {
				t.Fatalf("queuePlace = (%d, %d), want (%d, %d)", position, queued, tt.wantPosition, tt.wantQueued)
			}
		})
	}
}

func TestReportHeldLogsAnUnreadableQueueAndReportsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.heldErr = errors.New("disk gone")
	log, err := h.q.logger(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("logger: %v", err)
	}

	// Act
	h.q.reportHeld(context.Background(), submission("t1", "and then this"), log)

	// Assert
	if got := h.footer.submissionStages(); len(got) != 0 {
		t.Fatalf("stages = %v, want none reported without the queue's place", got)
	}
	if !logged(h.log.Records(), "error", opHold, "could not read the holds to report the prompt's place in the queue") {
		t.Fatalf("records = %+v, want the failed read at error", h.log.Records())
	}
}

// --- a running turn waiting out a failed API call --------------------------

func TestAPromptDuringAnApiRetryAsksNoClassifier(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.footer.standRetry()

	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "are you there?")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert.
	if got := len(h.judge.questions()); got != 0 {
		t.Fatalf("classifier calls = %d, want none while the API is being retried", got)
	}
}

func TestAPromptDuringAnApiRetryInterruptsTheWaitingTurn(t *testing.T) {
	// Arrange: the model would have joined the prompt at the next tool
	// boundary, which a turn waiting on the API never reaches.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.footer.standRetry()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteAfterToolCall, Reason: "it adds to the work"}

	// Act.
	if _, err := h.q.Submit(context.Background(), submission("t1", "are you there?")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Assert.
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the turn waiting on the API interrupted", killed)
	}
}

// THE RUNNING TURN ENDING WHILE THE PROMPT WAITED FOR ITS VERDICT is the
// ordinary race of a short turn, not a store disagreement: no ERROR, and the
// prompt waits for that end to deliver it.
func TestARunningTurnThatEndedBeforeTheVerdictIsNoDisagreement(t *testing.T) {
	tests := []struct {
		name      string
		stillRuns bool
		wantError bool
	}{
		{"the turn ended: recorded at INFO", false, false},
		{"the watcher still runs it: a real disagreement at ERROR", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the store holds no open row for the running turn.
			h := newHarness(t)
			if tt.stillRuns {
				h.watcher.running(idsTurn("running-turn"))
			}
			log := dlog.NewTestLogger()

			// Act
			verdict, route := h.q.verdictFor(context.Background(), submission("t1", "and also this"), "running-turn", log)

			// Assert
			gotError := false
			for _, r := range log.Records() {
				if r.Level == dlog.LevelError {
					gotError = true
				}
			}
			if route != classifier.RouteQueue || verdict.Arm != wsm.ArmHoldForTurnEnd || gotError != tt.wantError || askedCount(h) != 0 {
				t.Fatalf("verdictFor = (%+v, %s), error records = %t, asks = %d; want hold_for_turn_end, error=%t, the model never asked",
					verdict, route, gotError, askedCount(h), tt.wantError)
			}
		})
	}
}

func TestAVerdictForAPromptRetiredWhileJudgedIsNotAnError(t *testing.T) {
	tests := []struct {
		name      string
		refusal   error
		wantLevel string
		wantMsg   string
	}{
		{
			name:      "the prompt was retired while it was judged",
			refusal:   fmt.Errorf("wsm: held prompt t1 retired as \"dropped\": %w", wsm.ErrHoldRetired),
			wantLevel: "info",
			wantMsg:   "the prompt was retired while it was judged; its verdict is not recorded",
		},
		{
			name:      "the store failed",
			refusal:   errors.New("disk gone"),
			wantLevel: "error",
			wantMsg:   "could not record the verdict",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.db.mu.Lock()
			h.db.classificationErr = tt.refusal
			h.db.mu.Unlock()

			// Act
			_, _ = h.q.Submit(context.Background(), submission("t1", "go on"))
			h.q.waitForClassifications()

			// Assert
			if !logged(h.log.Records(), tt.wantLevel, opClassify, tt.wantMsg) {
				t.Fatalf("records = %v, want %s %q", h.log.Records(), tt.wantLevel, tt.wantMsg)
			}
			if tt.wantLevel == "info" && logged(h.log.Records(), "error", opClassify, "could not record the verdict") {
				t.Fatalf("a retired prompt's verdict was recorded as an error: %v", h.log.Records())
			}
		})
	}
}

func TestTheClassifiersLatencyIsRecordedOnEitherOutcome(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		level   string
		message string
	}{
		{name: "a verdict", level: "info", message: "the classifier routed the prompt"},
		{name: "a failure", err: errors.New("timeout: signal: killed"), level: "error", message: "the classifier failed; the prompt waits for the running turn to end"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the judge takes 1.5s of the queue's clock.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
			h.judge.err = tt.err
			h.judge.onJudge = func() { h.elapsed.Add(int64(1500 * time.Millisecond)) }

			// Act.
			if _, err := h.q.Submit(context.Background(), submission("t1", "a follow-up")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()

			// Assert.
			for _, r := range h.log.Records() {
				if r.Level == tt.level && r.Operation == opClassify && r.Message == tt.message {
					if r.Context["judge_ms"] != int64(1500) {
						t.Fatalf("judge_ms = %v (%T), want 1500", r.Context["judge_ms"], r.Context["judge_ms"])
					}
					return
				}
			}
			t.Fatalf("records = %+v, want %q at %s carrying judge_ms", h.log.Records(), tt.message, tt.level)
		})
	}
}

// ---- a chat thread's question is never classified --------------------------
//
// One turn is one question in a zettel thread, so a thread question is never
// interjected, folded or coalesced: it waits for the running turn's end, and
// where it was asked from rides its hold to the turn it is delivered as.

// threadSubmission composes one thread question asked of node n1.
func threadSubmission(turn ids.TurnID, text string) Submission {
	sub := submission(turn, text)
	sub.Ask = &wsm.ThreadAsk{Parent: &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: &zettelv1.NodeParentNode{
		Node: &zettelv1.NodeId{Id: "n1"}, Revision: &zettelv1.RevisionNumber{Number: 1},
	}}}}
	return sub
}

func TestAThreadQuestionIsNeverClassified(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running answer")

	// Act
	got, err := h.q.Submit(context.Background(), threadSubmission("t1", "and what about buffers?"))
	h.q.waitForClassifications()

	// Assert
	if err != nil {
		t.Fatalf("Submit: %v", err)
	}
	if got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("disposition = %+v, want held for the running turn's end", got)
	}
	if n := askedCount(h); n != 0 {
		t.Fatalf("the judge was asked %d times, want never for a thread question", n)
	}
	held, err := h.q.standingHold(context.Background(), theWorkspace, "t1")
	if err != nil || held.Ask.GetParentNodeID() != "n1" {
		t.Fatalf("held = %+v, err = %v, want the ask carried on the hold", held, err)
	}
}

func TestADeliveredThreadQuestionRecordsItsQuestionOnTheTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.q.Submit(context.Background(), threadSubmission("t1", "what is hydronium?")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	turn, err := h.db.Turn(context.Background(), theWorkspace, "t1")
	if err != nil {
		t.Fatal(err)
	}
	if turn.Question == nil || turn.Question.Text != "what is hydronium?" || turn.Question.Parent.GetNode().GetNode().GetId() != "n1" {
		t.Fatalf("turn question = %+v, want the typed words and the ask's parent", turn.Question)
	}
}

func TestAnOrdinaryTurnRecordsNoQuestion(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t1", "run the tests")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	turn, err := h.db.Turn(context.Background(), theWorkspace, "t1")
	if err != nil || turn.Question != nil {
		t.Fatalf("turn question = %+v, err = %v, want none", turn.Question, err)
	}
}

func TestAHoldReconstructsItsThreadAsk(t *testing.T) {
	// Arrange
	held := wsm.HeldPrompt{Workspace: theWorkspace, Turn: "t1", Said: userSaid("q"), Origin: "PROMPT_ORIGIN_USER_SENT",
		Ask: threadSubmission("t1", "q").Ask}

	// Act
	sub := submissionOf(held)

	// Assert
	if sub.Ask.GetParentNodeID() != "n1" {
		t.Fatalf("submission ask = %+v, want the hold's ask", sub.Ask)
	}
}

// trayArmsOf answers, in push order, every verdict arm the tray was pushed
// for TURN, consecutive repeats collapsed.
func trayArmsOf(h *harness, turn ids.TurnID) []wsm.ClassificationArm {
	h.holds.mu.Lock()
	defer h.holds.mu.Unlock()
	var out []wsm.ClassificationArm
	for _, push := range h.holds.pushes {
		for _, held := range push {
			if held.Turn != turn || held.Classification == nil {
				continue
			}
			if arm := held.Classification.Arm; len(out) == 0 || out[len(out)-1] != arm {
				out = append(out, arm)
			}
		}
	}
	return out
}

func TestAJudgedPromptsTrayShowsClassifyingThenItsHold(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "the classifier rules it waits", arrange: func(h *harness) {
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
		}},
		{name: "the classifier fails", arrange: func(h *harness) {
			h.judge.err = errors.New("vendor calls are forbidden: classifier")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			tt.arrange(h)

			// Act
			if _, err := h.q.Submit(context.Background(), submission("t1", "please also check the other file")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			h.q.waitForClassifications()

			// Assert
			want := []wsm.ClassificationArm{wsm.ArmClassifying, wsm.ArmHoldForTurnEnd}
			if got := trayArmsOf(h, "t1"); !reflect.DeepEqual(got, want) {
				t.Fatalf("tray arms for t1 = %v, want %v", got, want)
			}
		})
	}
}

func TestAnInterruptVerdictForATurnThatEndedWhileItWasReachedSendsNoKill(t *testing.T) {
	tests := []struct {
		name string
		// end ends the judged turn while the verdict is reached.
		end func(h *harness)
	}{
		{
			name: "the turn's end delivered the prompt",
			end: func(h *harness) {
				h.watcher.idle()
				h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
			},
		},
		{
			name: "the watcher stands the next turn in flight",
			end:  func(h *harness) { h.watcher.running("next-turn") },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			running(t, h, "running-turn", "the running work")
			h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
			release := h.judge.hold()
			if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
				t.Fatalf("Submit: %v", err)
			}
			<-h.judge.asking()
			tt.end(h)
			// Act.
			release()
			h.q.waitForClassifications()
			// Assert.
			if killed := h.sender.killed(); len(killed) != 0 {
				t.Fatalf("killed = %v, want no kill of a turn that already ended", killed)
			}
			for _, on := range h.footer.interruptions() {
				if on {
					t.Fatal("an interjection was registered against a turn that already ended")
				}
			}
			var got []string
			for _, r := range h.log.Records() {
				if r.Operation == opInterject {
					got = append(got, r.Level+": "+r.Message)
				}
			}
			want := "info: the turn the prompt would interrupt ended while the verdict was reached; the prompt is not interjected, and that turn's end delivers it"
			if len(got) != 1 || got[0] != want {
				t.Fatalf("interject records = %q, want exactly %q", got, want)
			}
		})
	}
}

func TestAnInterjectionIsDrawnInterjectingAndDeliveredAtTheInterruptedTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: classifier.ExplicitInterruptReason, FastPath: true}
	if _, err := h.q.Submit(context.Background(), submission("t1", "stop and rebase instead")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	before := h.sender.started()

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)

	// Assert
	if got := trayArmsOf(h, "t1"); len(got) == 0 || got[len(got)-1] != wsm.ArmInterject {
		t.Fatalf("tray arms for t1 = %v, want it drawn interjecting", got)
	}
	if got := h.footer.interruptions(); len(got) == 0 || !got[0] {
		t.Fatalf("footer interrupting = %v, want it raised at once", got)
	}
	if forces := h.sender.killedForces(); len(forces) != 1 || forces[0] {
		t.Fatalf("kill forces = %v, want one unforced kill that spares detached work", forces)
	}
	if len(before) != 0 {
		t.Fatalf("started before the turn ended = %v, want nothing", before)
	}
	if started := h.sender.started(); !reflect.DeepEqual(started, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want the interjected prompt at the interrupted turn's end", started)
	}
}

func TestAFailedInterjectGoesAheadOfALaterPromptInFifoOrder(t *testing.T) {
	// Arrange: the interject is refused; an ordinary prompt follows it.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	h.sender.killErr = errors.New("the vendor refused the kill")
	if _, err := h.q.Submit(context.Background(), submission("t2", "stop and rebase instead")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteQueue, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("t3", "second follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	first := h.sender.started()
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "t2", wsm.CloseCompleted)

	// Assert
	if got := h.db.hold("t2").Classification.Arm; got != wsm.ArmHoldForTurnEnd {
		t.Fatalf("t2 arm = %s, want hold_for_turn_end", got.String())
	}
	if got := h.footer.interruptions(); len(got) == 0 || got[len(got)-1] {
		t.Fatalf("footer interrupting = %v, want it cleared last", got)
	}
	if !reflect.DeepEqual(first, []ids.TurnID{"t2"}) {
		t.Fatalf("started at the running turn's end = %v, want only the FIFO head t2", first)
	}
	if started := h.sender.started(); !reflect.DeepEqual(started, []ids.TurnID{"t2", "t3"}) {
		t.Fatalf("started = %v, want t2 then t3", started)
	}
}

func TestAnInterruptVerdictForAPromptRetiredWhileItWasReachedSendsNoKill(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Route: classifier.RouteInterrupt, Reason: "it countermands the work"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("t1", "do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Act.
	release()
	h.q.waitForClassifications()
	// Assert.
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want no kill for a prompt no longer held", killed)
	}
	for _, on := range h.footer.interruptions() {
		if on {
			t.Fatal("an interjection was registered for a prompt no longer held")
		}
	}
	for _, r := range h.log.Records() {
		if r.Operation == opInterject {
			t.Fatalf("an interject record was written for a prompt no longer held: %s: %s", r.Level, r.Message)
		}
	}
}
