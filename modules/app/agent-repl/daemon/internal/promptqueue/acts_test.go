package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
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

func TestAContextCutTellsTheFooterItsSpelling(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{
		Kind: ActClear, Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Assert
	turns := h.footer.startedTurns()
	if len(turns) != 1 || turns[0].Prompt != "/clear" {
		t.Fatalf("footer turn facts = %+v, want the act's own spelling as its prompt", turns)
	}
}

// --- nothing overtakes a running /clear or /compact ------------------------

// cutRunsWithAnInterjectingPromptHeld starts a context cut down the one path,
// then submits a prompt the classifier would mark interject while it runs.
func cutRunsWithAnInterjectingPromptHeld(t *testing.T, h *harness, kind string) {
	t.Helper()
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: kind, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	h.watcher.running("cut-1")
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

var contextCutKinds = []struct {
	name string
	kind string
}{
	{name: "/compact", kind: ActCompact},
	{name: "/clear", kind: ActClear},
}

func TestAPromptHeldDuringAContextCutDoesNotInterruptIt(t *testing.T) {
	for _, tt := range contextCutKinds {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			cutRunsWithAnInterjectingPromptHeld(t, h, tt.kind)
			// Assert
			if killed := h.sender.killed(); len(killed) != 0 {
				t.Fatalf("killed = %v, want the session act left to run", killed)
			}
		})
	}
}

func TestAPromptHeldDuringAContextCutIsDrawnAsAHeldRow(t *testing.T) {
	for _, tt := range contextCutKinds {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			// Act
			cutRunsWithAnInterjectingPromptHeld(t, h, tt.kind)
			// Assert
			h.holds.mu.Lock()
			defer h.holds.mu.Unlock()
			last := h.holds.pushes[len(h.holds.pushes)-1]
			if len(last) != 1 || last[0].Turn != "t1" || last[0].Tombstone != nil {
				t.Fatalf("tray = %+v, want the prompt standing as a held row", last)
			}
		})
	}
}

func TestAPromptHeldDuringAContextCutIsDeliveredAfterIt(t *testing.T) {
	for _, tt := range contextCutKinds {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			cutRunsWithAnInterjectingPromptHeld(t, h, tt.kind)
			// Act
			h.watcher.idle()
			h.q.OnTurnEnded(theWorkspace, "cut-1", wsm.CloseCompleted)
			// Assert
			if started := h.sender.started(); len(started) != 2 || started[0] != "cut-1" || started[1] != "t1" {
				t.Fatalf("started = %v, want the act and then the held prompt", started)
			}
		})
	}
}

// TestAUserInterruptOfACompactStillEndsItAndDeliversWhatWaited pins that the
// guard is on INTERJECTION only: the user's own interrupt (C-c C-k, or the
// UI's) ends the act like any turn, and the held prompt then goes.
func TestAUserInterruptOfACompactStillEndsItAndDeliversWhatWaited(t *testing.T) {
	// Arrange
	h := newHarness(t)
	cutRunsWithAnInterjectingPromptHeld(t, h, ActCompact)
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "cut-1", wsm.CloseKilled)
	// Assert
	if started := h.sender.started(); len(started) != 2 || started[1] != "t1" {
		t.Fatalf("started = %v, want the held prompt delivered once the interrupt ended the act", started)
	}
}

// actQueuedBehindARunningTurn queues a context cut behind an ordinary running
// turn, with a prompt already standing behind that turn.
func actQueuedBehindARunningTurn(t *testing.T, h *harness) {
	t.Helper()
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
}

func TestATurnEndThatStartsAQueuedCompactDeliversNoHeldPromptIntoIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	actQueuedBehindARunningTurn(t, h)
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "cut-1" {
		t.Fatalf("started = %v, want the compaction alone", started)
	}
}

func TestAPromptKeptBehindAStartedCompactIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	actQueuedBehindARunningTurn(t, h)
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	for _, r := range h.log.Records() {
		if r.Level == "info" && r.Operation == opTurnEnded && r.Context["session_act_turn"] == "cut-1" &&
			r.Context["session_act"] == conversationv1.SessionCommand_SESSION_COMMAND_COMPACT.String() {
			return
		}
	}
	t.Fatalf("records = %+v, want an info naming the running act and its turn", h.log.Records())
}

func TestAStillClassifyingPromptIsNotDeliveredIntoACompactTheTurnEndStarted(t *testing.T) {
	// Arrange: the prompt's verdict is still with the model when the turn
	// ends and the queued /compact starts.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "cut-1"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "it countermands the work"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("t1", "actually, do it the other way")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.watcher.running("cut-1")
	release()
	h.q.waitForClassifications()
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "cut-1" {
		t.Fatalf("started = %v, want the compaction alone", started)
	}
}

func TestActsQueuedAfterAContextCutWaitForItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	for _, act := range []Act{{Kind: ActCompact, Turn: "cut-1"}, {Kind: ActSetModel, Value: "opus"}} {
		if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, act); err != nil {
			t.Fatalf("SubmitSessionAct: %v", err)
		}
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if len(h.sender.models) != 0 {
		t.Fatalf("models = %v, want the model change held behind the running compaction", h.sender.models)
	}
}

func TestActsQueuedAfterAContextCutRunAtItsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	for _, act := range []Act{{Kind: ActCompact, Turn: "cut-1"}, {Kind: ActSetModel, Value: "opus"}} {
		if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, act); err != nil {
			t.Fatalf("SubmitSessionAct: %v", err)
		}
	}
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.watcher.running("cut-1")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "cut-1", wsm.CloseCompleted)
	// Assert
	if len(h.sender.models) != 1 || h.sender.models[0] != "opus" {
		t.Fatalf("models = %v, want the model change run at the compaction's end", h.sender.models)
	}
}

// TestAContextCutOfTheTurnAlreadyInFlightStartsNothing pins the act path's
// re-drive: a context cut whose turn is already running is answered as the
// delivery the original was, never queued to start that turn again at its
// end, with the repeat recorded at ERROR.
func TestAContextCutOfTheTurnAlreadyInFlightStartsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "cut-1", "/clear")
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActClear, Turn: "cut-1", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT})
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "cut-1", wsm.CloseCompleted)
	// Assert
	if err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	if started := h.sender.started(); len(started) != 0 {
		t.Fatalf("started = %v, want the running cut never started again", started)
	}
	if !logged(h.log.Records(), "error", opSubmit, repeatedStartMessage) {
		t.Fatalf("the repeated start was not recorded at error: %v", h.log.Records())
	}
}

func TestSubmitSessionActAfterTheMoveSealedIsRefusedAsMovedAway(t *testing.T) {
	// Arrange: a dispatch-quiet move has sealed the workspace's queue.
	h := newHarness(t)
	busy(t, h)
	transfer := newGate()
	startQuietMove(t, h, transfer)
	if _, _, err := h.q.SealMove(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SealMove: %v", err)
	}
	// Act
	err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "t-compact"})
	// Assert
	if !errors.Is(err, bounce.ErrMovedAway) {
		t.Fatalf("SubmitSessionAct after the seal = %v, want ErrMovedAway", err)
	}
	transfer.finish(h, nil)
}

// TestAContextCutBehindAnotherRunningTurnStillQueues pins that only the SAME
// turn is answered without delivery: a cut behind a different running turn
// queues and starts at that turn's end.
func TestAContextCutBehindAnotherRunningTurnStillQueues(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace,
		Act{Kind: ActClear, Turn: "cut-1", Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "cut-1" {
		t.Fatalf("started = %v, want the queued cut started once at the turn's end", started)
	}
}

func TestSubmitSessionActRecordsAStandingRefusalAtItsLevelOrDebugForARedrive(t *testing.T) {
	const (
		noSession = "the workspace has no session to act on"
		movedAway = "the workspace's move has sealed what it carries; the act is refused so it is asked of the daemon the workspace moves to"
	)
	tests := []struct {
		name      string
		redrive   bool
		sealed    bool
		message   string
		wantLevel string
	}{
		{name: "a live no-session refusal warns", message: noSession, wantLevel: "warn"},
		{name: "a re-driven no-session refusal is debug", redrive: true, message: noSession, wantLevel: "debug"},
		{name: "a live moved-away refusal is info", sealed: true, message: movedAway, wantLevel: "info"},
		{name: "a re-driven moved-away refusal is debug", sealed: true, redrive: true, message: movedAway, wantLevel: "debug"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			var transfer *gate
			if tc.sealed {
				busy(t, h)
				transfer = newGate()
				startQuietMove(t, h, transfer)
				if _, _, err := h.q.SealMove(context.Background(), theWorkspace); err != nil {
					t.Fatalf("SealMove: %v", err)
				}
			} else {
				h.noSession = true
			}
			ctx := context.Background()
			if tc.redrive {
				ctx = WithRedrive(ctx)
			}

			// Act
			err := h.q.SubmitSessionAct(ctx, theWorkspace, Act{Kind: ActCompact, Turn: "t-compact"})

			// Assert
			if err == nil {
				t.Fatal("SubmitSessionAct = nil error, want the refusal")
			}
			if !logged(h.log.Records(), tc.wantLevel, opAct, tc.message) {
				t.Fatalf("records = %+v, want %q at %s", h.log.Records(), tc.message, tc.wantLevel)
			}
			if transfer != nil {
				transfer.finish(h, nil)
			}
		})
	}
}
