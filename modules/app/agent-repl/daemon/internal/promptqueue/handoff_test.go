package promptqueue

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// sealedBusyMove starts a dispatch-quiet move over a running turn and
// answers its gate, so the caller can seal it and then finish it.
func sealedBusyMove(t *testing.T, h *harness) *gate {
	t.Helper()
	busy(t, h)
	transfer := newGate()
	startQuietMove(t, h, transfer)
	return transfer
}

func TestSealMoveRefusesAWorkspaceWithNoQuietMoveRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, _, err := h.q.SealMove(context.Background(), theWorkspace)

	// Assert
	if !errors.Is(err, ErrNoMoveToSeal) {
		t.Fatalf("SealMove = %v, want ErrNoMoveToSeal", err)
	}
}

func TestSealMoveSealsAMoveTakenAtFreenessToo(t *testing.T) {
	// Arrange: a free workspace's freeness-gated move (the fallback
	// transfer) is running.
	h := newHarness(t)
	transfer := newGate()
	if _, err := h.q.RequestBounce(context.Background(), theWorkspace, transfer.moving("handover_transfer")); err != nil {
		t.Fatalf("RequestBounce: %v", err)
	}
	transfer.awaitStart(t)

	// Act
	_, carried, err := h.q.SealMove(context.Background(), theWorkspace)

	// Assert
	if err != nil || len(carried) != 0 {
		t.Fatalf("SealMove = (%+v, %v), want the move sealed with nothing carried", carried, err)
	}
	transfer.finish(h, nil)
}

func TestSealMoveCarriesTheQueuedActsInOrder(t *testing.T) {
	// Arrange: a /compact and a model change queued behind the running turn.
	h := newHarness(t)
	transfer := sealedBusyMove(t, h)
	for _, act := range []Act{{Kind: ActCompact, Turn: "t-compact"}, {Kind: ActSetModel, Value: "opus"}} {
		if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, act); err != nil {
			t.Fatalf("SubmitSessionAct: %v", err)
		}
	}

	// Act
	handoff, _, err := h.q.SealMove(context.Background(), theWorkspace)

	// Assert
	if err != nil || len(handoff.Acts) != 2 || handoff.Acts[0].Kind != ActCompact || handoff.Acts[0].Turn != "t-compact" || handoff.Acts[1].Value != "opus" {
		t.Fatalf("SealMove = (%+v, %v), want the /compact then the model change", handoff, err)
	}
	transfer.finish(h, nil)
}

func TestSealMoveTakesTheActsOutOfThisDaemonsMemory(t *testing.T) {
	// Arrange
	h := newHarness(t)
	transfer := sealedBusyMove(t, h)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "t-compact"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}

	// Act
	if _, _, err := h.q.SealMove(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SealMove: %v", err)
	}

	// Assert
	if acts := h.q.state(theWorkspace).acts; len(acts) != 0 {
		t.Fatalf("acts left here after the seal = %+v, want none: they are the next daemon's", acts)
	}
	transfer.finish(h, nil)
}

func TestSealMoveCarriesTheRunningCut(t *testing.T) {
	// Arrange: the running turn is a /clear.
	h := newHarness(t)
	h.beginCut("t-clear", conversationv1.SessionCommand_SESSION_COMMAND_CLEAR)
	transfer := newGate()
	startQuietMove(t, h, transfer)

	// Act
	handoff, _, err := h.q.SealMove(context.Background(), theWorkspace)

	// Assert
	if err != nil || handoff.Cut == nil || handoff.Cut.Turn != "t-clear" ||
		conversationv1.SessionCommand(handoff.Cut.Command) != conversationv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("SealMove = (%+v, %v), want the running /clear carried", handoff, err)
	}
	transfer.finish(h, nil)
}

func TestSealMoveCarriesTheSemanticHeadAndTheInterruptingStatus(t *testing.T) {
	// Arrange: an interjection installed its head and sent its interrupt.
	h := newHarness(t)
	transfer := sealedBusyMove(t, h)
	head := ids.TurnID("t-head")
	h.q.mu.Lock()
	h.q.states[theWorkspace].head = &head
	h.q.states[theWorkspace].interrupting = true
	h.q.mu.Unlock()

	// Act
	handoff, _, err := h.q.SealMove(context.Background(), theWorkspace)

	// Assert
	if err != nil || handoff.Head != "t-head" || !handoff.Interrupting {
		t.Fatalf("SealMove = (%+v, %v), want the head and the interrupting status carried", handoff, err)
	}
	transfer.finish(h, nil)
}

func TestSealMoveSupersedesAVerdictStillBeingJudged(t *testing.T) {
	// Arrange: a prompt held behind the running turn with its judge in
	// flight, and then the move.
	h := newHarness(t)
	busy(t, h)
	h.judge.verdict = classifier.Verdict{Interject: true, Reason: "supersedes it"}
	release := h.judge.hold()
	if _, err := h.q.Submit(context.Background(), submission("t-held", "a follow-up")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	<-h.judge.asking()
	transfer := newGate()
	startQuietMove(t, h, transfer)

	// Act
	if _, _, err := h.q.SealMove(context.Background(), theWorkspace); err != nil {
		t.Fatalf("SealMove: %v", err)
	}
	release()
	h.q.waitForClassifications()

	// Assert: the verdict settles as a discard, and nothing is interrupted.
	if kills := h.sender.killed(); len(kills) != 0 {
		t.Fatalf("KillTurn was sent for %v after the seal, want the superseded verdict discarded", kills)
	}
	transfer.finish(h, nil)
}

func TestUnsealMovePutsTheActsBackAheadOfLaterOnes(t *testing.T) {
	// Arrange: a sealed /compact, then the move is taken back.
	h := newHarness(t)
	transfer := sealedBusyMove(t, h)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActCompact, Turn: "t-compact"}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	handoff, _, err := h.q.SealMove(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("SealMove: %v", err)
	}
	h.q.mu.Lock()
	h.q.states[theWorkspace].acts = []Act{{Kind: ActSetModel, Value: "opus"}}
	h.q.mu.Unlock()

	// Act
	if err := h.q.UnsealMove(context.Background(), theWorkspace, handoff); err != nil {
		t.Fatalf("UnsealMove: %v", err)
	}

	// Assert
	acts := h.q.state(theWorkspace).acts
	if len(acts) != 2 || acts[0].Kind != ActCompact || acts[1].Kind != ActSetModel {
		t.Fatalf("acts after the take-back = %+v, want the carried /compact ahead of the later act", acts)
	}
	transfer.finish(h, errors.New("the transfer failed"))
}

func TestUnsealMoveLetsTheRunningMoveCarryRequestsAgain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	transfer := sealedBusyMove(t, h)
	handoff, _, err := h.q.SealMove(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("SealMove: %v", err)
	}
	if err := h.q.UnsealMove(context.Background(), theWorkspace, handoff); err != nil {
		t.Fatalf("UnsealMove: %v", err)
	}
	restart := newGate()

	// Act
	_, err = h.q.RequestBounce(context.Background(), theWorkspace, restart.relaunching("restart_verb"))

	// Assert
	if err != nil {
		t.Fatalf("RequestBounce after the take-back = %v, want it carried rather than refused", err)
	}
	transfer.finish(h, errors.New("the transfer failed"))
}

func TestAdoptHandoffQueuesTheCarriedActsForTheTurnsEnd(t *testing.T) {
	// Arrange: the adopted shim still runs the turn the act waited behind.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	handoff := bounce.Handoff{Acts: []bounce.HandoffAct{{Kind: ActSetModel, Value: "opus"}}}
	if err := h.q.AdoptHandoff(context.Background(), theWorkspace, handoff); err != nil {
		t.Fatalf("AdoptHandoff: %v", err)
	}

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if models := h.sender.modelsSet(); len(models) != 1 || models[0] != "opus" {
		t.Fatalf("models set at the turn's end = %v, want the carried act run", models)
	}
}

func TestAdoptHandoffDropsACutWhoseTurnIsNoLongerOpen(t *testing.T) {
	// Arrange: the cut's turn ended while no daemon watched it.
	h := newHarness(t)
	handoff := bounce.Handoff{Cut: &bounce.HandoffCut{Turn: "t-gone", Command: int32(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)}}

	// Act
	if err := h.q.AdoptHandoff(context.Background(), theWorkspace, handoff); err != nil {
		t.Fatalf("AdoptHandoff: %v", err)
	}

	// Assert
	if _, ok := h.q.runningCut(theWorkspace); ok {
		t.Fatalf("a cut whose turn is no longer open was installed")
	}
}

func TestAdoptHandoffInstallsACutWhoseTurnIsStillOpen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "t-compact", "/compact")
	handoff := bounce.Handoff{Cut: &bounce.HandoffCut{Turn: "t-compact", Command: int32(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)}}

	// Act
	if err := h.q.AdoptHandoff(context.Background(), theWorkspace, handoff); err != nil {
		t.Fatalf("AdoptHandoff: %v", err)
	}

	// Assert
	cut, ok := h.q.runningCut(theWorkspace)
	if !ok || cut.turn != "t-compact" || cut.command != conversationv1.SessionCommand_SESSION_COMMAND_COMPACT {
		t.Fatalf("runningCut = (%+v, %v), want the carried /compact installed", cut, ok)
	}
}

func TestAdoptHandoffDropsAHeadThatNoLongerStands(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.AdoptHandoff(context.Background(), theWorkspace, bounce.Handoff{Head: "t-delivered"}); err != nil {
		t.Fatalf("AdoptHandoff: %v", err)
	}

	// Assert
	if head := h.q.state(theWorkspace).head; head != nil {
		t.Fatalf("head = %v, want a head whose prompt no longer stands dropped", *head)
	}
}

func TestAdoptHandoffRaisesTheInterruptingStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if err := h.q.AdoptHandoff(context.Background(), theWorkspace, bounce.Handoff{Interrupting: true}); err != nil {
		t.Fatalf("AdoptHandoff: %v", err)
	}

	// Assert
	if got := h.footer.interruptions(); len(got) != 1 || !got[0] {
		t.Fatalf("footer interrupting = %v, want it raised once", got)
	}
}

func TestRejudgeHeldJudgesASupersededPromptAgainstTheAdoptedTurn(t *testing.T) {
	// Arrange: a hold the previous daemon's seal left `classifying`.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	putClassifying(t, h, "t-held", "a follow-up")

	// Act
	if err := h.q.RejudgeHeld(context.Background(), theWorkspace); err != nil {
		t.Fatalf("RejudgeHeld: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if asked := h.judge.questions(); len(asked) != 1 || asked[0][0] != "the running work" || asked[0][1] != "a follow-up" {
		t.Fatalf("judge asked %v, want the held prompt judged against the adopted turn", asked)
	}
}

func TestRejudgeHeldAsksNothingWithNoRunningTurn(t *testing.T) {
	// Arrange: the turn ended during the move.
	h := newHarness(t)
	putClassifying(t, h, "t-held", "a follow-up")

	// Act
	if err := h.q.RejudgeHeld(context.Background(), theWorkspace); err != nil {
		t.Fatalf("RejudgeHeld: %v", err)
	}
	h.q.waitForClassifications()

	// Assert
	if asked := h.judge.questions(); len(asked) != 0 {
		t.Fatalf("judge asked %v, want nothing: no turn runs to judge against", asked)
	}
}

// putClassifying records a standing hold whose verdict was still being judged.
func putClassifying(t *testing.T, h *harness, turn ids.TurnID, text string) {
	t.Helper()
	if err := h.db.PutHeldPrompt(context.Background(), wsm.HeldPrompt{
		Workspace: theWorkspace, Turn: turn, Said: userSaid(text),
		Origin:         conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT.String(),
		QueuedAt:       instant,
		Classification: &wsm.Classification{Arm: wsm.ArmClassifying, At: instant},
	}); err != nil {
		t.Fatalf("PutHeldPrompt: %v", err)
	}
}
