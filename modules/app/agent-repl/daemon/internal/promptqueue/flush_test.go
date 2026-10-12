package promptqueue

import (
	"context"
	"errors"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A FLUSH SENDS EVERY HELD PROMPT TO THE SHIM AT ONCE (owner ruling,
// 2026-10-12): each joins the running turn in order, nothing interrupted.

// flush runs FlushHeld on the harness workspace, failing the test on an error.
func flush(t *testing.T, h *harness) {
	t.Helper()
	if err := h.q.FlushHeld(context.Background(), theWorkspace); err != nil {
		t.Fatalf("FlushHeld: %v", err)
	}
}

// putHeld records HELD for the harness workspace, behind what is already held.
func putHeld(t *testing.T, h *harness, held wsm.HeldPrompt) {
	t.Helper()
	held.Workspace = theWorkspace
	if held.Said == nil {
		held.Said = submission(held.Turn, "held").Said
	}
	held.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT.String()
	held.QueuedAt = instant
	if err := h.db.PutHeldPrompt(context.Background(), held); err != nil {
		t.Fatalf("PutHeldPrompt(%s): %v", held.Turn, err)
	}
}

// stillHeld reports whether TURN's hold still stands, unretired.
func stillHeld(t *testing.T, h *harness, turn ids.TurnID) bool {
	t.Helper()
	held, ok, err := h.db.HeldPromptByTurn(context.Background(), turn)
	if err != nil || !ok {
		t.Fatalf("hold of %s = (%v, %v), want one recorded", turn, ok, err)
	}
	return held.Tombstone == nil
}

func TestAFlushSendsEveryHeldPromptToJoinTheRunningTurnInOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2", "t3")

	// Act
	flush(t, h)

	// Assert
	if got := h.sender.joins; !slices.Equal(got, []ids.TurnID{"t1", "t2", "t3"}) {
		t.Fatalf("joins = %v, want t1, t2 and t3 in order", got)
	}
}

func TestAFlushInterruptsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")

	// Act
	flush(t, h)

	// Assert
	if killed := h.sender.killed(); len(killed) != 0 {
		t.Fatalf("killed = %v, want nothing interrupted", killed)
	}
}

func TestAFlushRetiresEveryFlushedHoldAsDelivered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")

	// Act
	flush(t, h)

	// Assert
	if stillHeld(t, h, "t1") || stillHeld(t, h, "t2") {
		t.Fatal("a flushed hold still stands")
	}
}

func TestAFlushWithNoTurnRunningStartsTheFirstAndJoinsTheRestToIt(t *testing.T) {
	// Arrange: nothing runs, and the first delivery stands its turn in flight.
	h := newHarness(t)
	h.watcher.standOnOpening = true
	putHeld(t, h, wsm.HeldPrompt{Turn: "t1"})
	putHeld(t, h, wsm.HeldPrompt{Turn: "t2"})
	putHeld(t, h, wsm.HeldPrompt{Turn: "t3"})

	// Act
	flush(t, h)

	// Assert
	if started, joins := h.sender.started(), h.sender.joins; !slices.Equal(started, []ids.TurnID{"t1"}) || !slices.Equal(joins, []ids.TurnID{"t2", "t3"}) {
		t.Fatalf("started = %v, joins = %v; want t1 started and t2, t3 joining it", started, joins)
	}
}

func TestAFlushStopsAtAPromptThatCannotJoinAndKeepsEverythingBehindItHeld(t *testing.T) {
	// Arrange: t2 is a session act, which runs as a turn of its own.
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	putHeld(t, h, wsm.HeldPrompt{Turn: "t2", Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"}})
	putHeld(t, h, wsm.HeldPrompt{Turn: "t3"})

	// Act
	flush(t, h)

	// Assert
	if got := h.sender.joins; !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("joins = %v, want only t1", got)
	}
	if !stillHeld(t, h, "t2") || !stillHeld(t, h, "t3") {
		t.Fatal("the act and the prompt behind it were not left held")
	}
	if !hasRecord(h, dlog.LevelInfo, opFlush, "a held prompt cannot be flushed; it and every prompt behind it wait as they would have") {
		t.Fatalf("records = %+v, want the stop recorded", h.log.Records())
	}
}

func TestFlushableRefusesEveryPromptThatCannotJoin(t *testing.T) {
	reconnect := wsm.HoldReconnect
	tests := []struct {
		name string
		held wsm.HeldPrompt
		want bool
	}{
		{name: "a plain prompt", held: wsm.HeldPrompt{Turn: "t1"}, want: true},
		{name: "a prompt a daemon hold keeps", held: wsm.HeldPrompt{Turn: "t1", Hold: &reconnect}, want: false},
		{name: "a session act", held: wsm.HeldPrompt{Turn: "t1", Act: &wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"}}, want: false},
		{name: "a prompt to a subagent", held: wsm.HeldPrompt{Turn: "t1", Target: bubbleSubmission("t1").Target}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, ok := h.q.flushable(theWorkspace, tt.held)

			// Assert
			if ok != tt.want {
				t.Fatalf("flushable = %v, want %v", ok, tt.want)
			}
		})
	}
}

func TestAFlushStopsAtAPromptBeingEdited(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	beginEdit(t, h, "t2")

	// Act
	flush(t, h)

	// Assert
	if got := h.sender.joins; !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("joins = %v, want only t1 ahead of the edit", got)
	}
}

func TestAFlushKeptByTheWorkspaceStateSendsNothing(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "a holding lease", arrange: func(h *harness) {
			h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
			h.lease(wsm.HolderDrain, wsm.PolicyHold)
		}},
		{name: "a bounce draining the workspace", arrange: func(h *harness) {
			h.q.state(theWorkspace).bounce = &pendingBounce{draining: true}
		}},
		{name: "a session act running", arrange: func(h *harness) {
			h.q.state(theWorkspace).cut = &runningCut{turn: "running-turn", command: conversationv1.SessionCommand_SESSION_COMMAND_COMPACT}
		}},
		{name: "an interjection under way", arrange: func(h *harness) {
			h.q.state(theWorkspace).interrupting = true
		}},
		{name: "a shim call that opens no turn", arrange: func(h *harness) {
			h.q.state(theWorkspace).call = &shimCall{what: "revival"}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			queuedBehind(t, h, "t1")
			tt.arrange(h)

			// Act
			flush(t, h)

			// Assert
			if got := h.sender.joins; len(got) != 0 {
				t.Fatalf("joins = %v, want nothing flushed", got)
			}
			if !hasRecord(h, dlog.LevelInfo, opFlush, "the held prompts are not flushed; they wait as they would have") {
				t.Fatalf("records = %+v, want why nothing was flushed", h.log.Records())
			}
		})
	}
}

func TestAFlushOfAnEmptyQueueSendsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")

	// Act
	flush(t, h)

	// Assert
	if got, started := h.sender.joins, h.sender.started(); len(got) != 0 || len(started) != 0 {
		t.Fatalf("joins = %v, started = %v; want nothing sent", got, started)
	}
}

func TestAFlushThatCannotReadTheHoldsIsAnErrorRecordedWithItsCause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	h.db.heldErr = errors.New("the store is gone")

	// Act
	err := h.q.FlushHeld(context.Background(), theWorkspace)

	// Assert
	if err == nil || !errors.Is(err, h.db.heldErr) {
		t.Fatalf("FlushHeld = %v, want the read's error", err)
	}
	if len(h.sender.joins) != 0 {
		t.Fatalf("joins = %v, want nothing sent", h.sender.joins)
	}
	found := false
	for _, r := range h.log.Records() {
		found = found || (r.Level == dlog.LevelError && r.Operation == opFlush && r.Context["cause"] == "the store is gone")
	}
	if !found {
		t.Fatalf("records = %+v, want the failed read at error with its cause", h.log.Records())
	}
}

func TestAFlushSupersedesAVerdictStillBeingJudgedAboutAFlushedPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1")
	before := h.q.contentEpoch(theWorkspace, "t1")

	// Act
	flush(t, h)

	// Assert
	if after := h.q.contentEpoch(theWorkspace, "t1"); after == before {
		t.Fatalf("epoch = %d, want it moved past %d so a verdict in flight is discarded", after, before)
	}
}

func TestAFlushedJoinTheShimRefusesLeavesItAndEverythingBehindItHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	h.sender.joinErr = errors.New("query_dead")

	// Act
	flush(t, h)

	// Assert
	if !stillHeld(t, h, "t1") || !stillHeld(t, h, "t2") {
		t.Fatal("a prompt the shim refused, or one behind it, was retired")
	}
	if c := verdictOf(t, h, "t1"); c.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("verdict = %+v, want t1 waiting for the turn's end", c)
	}
}

func TestAFlushJoinsBesideAJudgedJoinAlreadyWaiting(t *testing.T) {
	// Arrange: t1 was judged to join; t2 waits unjudged behind it.
	h := newHarness(t)
	sentToJoin(t, h)
	putHeld(t, h, wsm.HeldPrompt{Turn: "t2"})

	// Act
	flush(t, h)

	// Assert
	if got := h.sender.joins; !slices.Equal(got, []ids.TurnID{"t1", "t2"}) {
		t.Fatalf("joins = %v, want t2 sent beside t1", got)
	}
}

func TestAFlushDuringAStartTurnJoinsTheTurnItOpensOnceItSettles(t *testing.T) {
	// Arrange: the flush runs while the StartTurn opening t0 is in flight.
	h := newHarness(t)
	h.watcher.standOnOpening = true
	h.sender.startHook = func() {
		h.sender.startHook = nil
		putHeld(t, h, wsm.HeldPrompt{Turn: "t1"})
		putHeld(t, h, wsm.HeldPrompt{Turn: "t2"})
		flush(t, h)
	}

	// Act
	if _, err := h.q.Submit(context.Background(), submission("t0", "the first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}

	// Assert
	if got := h.sender.joins; !slices.Equal(got, []ids.TurnID{"t1", "t2"}) {
		t.Fatalf("joins = %v, want t1 and t2 sent once the start settled", got)
	}
}

func TestTheOldestOfSeveralJoinsRunsWhenTheTurnTheyJoinedEndsAndTheRestJoinIt(t *testing.T) {
	// Arrange: the turn ends without folding any of them in.
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2", "t3")
	flush(t, h)
	h.watcher.running("t1")

	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	h.q.mu.Lock()
	joins := slices.Clone(h.q.states[theWorkspace].joins)
	h.q.mu.Unlock()
	want := []joiningPrompt{{turn: "t2", into: "t1"}, {turn: "t3", into: "t1"}}
	if len(joins) != len(want) {
		t.Fatalf("joins = %+v, want t2 and t3 joining t1", joins)
	}
	for i := range want {
		if joins[i].turn != want[i].turn || joins[i].into != want[i].into {
			t.Fatalf("joins = %+v, want t2 and t3 joining t1", joins)
		}
	}
}

func TestAJoinFoldedIntoTheOldestJoinIsResubmittedWhenThatTurnFails(t *testing.T) {
	// Arrange: t2 ran in t1's prompt batch, and t1 then failed.
	h := newHarness(t)
	queuedBehind(t, h, "t1", "t2")
	flush(t, h)
	h.watcher.running("t1")
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	h.q.OnTurnEnded(theWorkspace, "t2", wsm.CloseFolded)
	h.watcher.idle()

	// Act
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFailed)

	// Assert
	if got := resubmitted(h); len(got) != 1 {
		t.Fatalf("resubmitted = %v, want t2 resubmitted as its own turn", got)
	}
}
