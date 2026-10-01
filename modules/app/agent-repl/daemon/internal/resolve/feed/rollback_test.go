package feed

import (
	"context"
	"errors"
	"slices"
	"strings"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// fakeRolledBack is an in-memory RolledBackTurnStore.
type fakeRolledBack struct {
	mu      sync.Mutex
	turns   []ids.TurnID
	putErr  error
	loadErr error
}

func (f *fakeRolledBack) RecordRolledBackTurns(_ context.Context, _ ids.WorkspaceID, turns []ids.TurnID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.putErr != nil {
		return f.putErr
	}
	f.turns = append(f.turns, turns...)
	return nil
}

func (f *fakeRolledBack) RolledBackTurns(context.Context, ids.WorkspaceID) ([]ids.TurnID, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.loadErr != nil {
		return nil, f.loadErr
	}
	return slices.Clone(f.turns), nil
}

// promptRowID is the root feed's prompt row of TURN.
func promptRowID(turn string) *frontendv1.FeedId {
	return testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindPrompt, ID: turn}})
}

// threeTurns draws three answered prompts t1, t2, t3.
func threeTurns(h *harness) {
	h.t.Helper()
	for i, turn := range []string{"t1", "t2", "t3"} {
		h.deliverPromptAt(turn, "question "+turn, int64(1_000+100*i))
		h.liveResponse("u-"+turn, "answer "+turn, turn)
	}
}

func values(rows []*frontendv1.FeedId) []string {
	out := make([]string, 0, len(rows))
	for _, row := range rows {
		out = append(out, row.GetValue())
	}
	return out
}

func TestRollbackPromptsAreThePromptsThatOpenTurnsOldestFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)

	// Act
	got := h.resolver.RollbackPrompts(testWorkspace)

	// Assert
	want := []string{promptRowID("t1").GetValue(), promptRowID("t2").GetValue(), promptRowID("t3").GetValue()}
	if !slices.Equal(values(got), want) {
		t.Fatalf("RollbackPrompts() = %v, want %v", values(got), want)
	}
}

func TestRollbackPromptsStopAtTheNewestContextCut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "before the cut", 1_000)
	h.liveCut("p-cut")
	h.deliverPromptAt("t2", "after the cut", 3_000)

	// Act
	got := h.resolver.RollbackPrompts(testWorkspace)

	// Assert
	if !slices.Equal(values(got), []string{promptRowID("t2").GetValue()}) {
		t.Fatalf("RollbackPrompts() = %v, want only t2", values(got))
	}
}

func TestRollbackPromptsLeaveOutAPromptFoldedIntoARunningTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.deliverFoldedPrompt("t1b", "t1", "folded in")

	// Act
	got := h.resolver.RollbackPrompts(testWorkspace)

	// Assert
	if !slices.Equal(values(got), []string{promptRowID("t1").GetValue()}) {
		t.Fatalf("RollbackPrompts() = %v, want only t1", values(got))
	}
}

func TestRollbackPromptsLeaveOutAPromptTheShimHasNotConfirmed(t *testing.T) {
	// Arrange: the queue's mirror of an accepted prompt, before the shim's
	// prompt frame says what was said.
	h := newHarness(t)
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), &frontendv1.FeedRow{
		Id:   promptRowID("t1"),
		Turn: &conversationv1.TurnId{Value: "t1"},
		Row:  &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}},
	})

	// Act
	got := h.resolver.RollbackPrompts(testWorkspace)

	// Assert
	if len(got) != 0 {
		t.Fatalf("RollbackPrompts() = %v, want none", values(got))
	}
}

func TestRollbackTargetIsThePromptsTurnAndEveryLaterOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)

	// Act
	target, ok := h.resolver.RollbackTarget(testWorkspace, promptRowID("t2"))

	// Assert
	if !ok {
		t.Fatal("RollbackTarget(t2) found no target")
	}
	if !slices.Equal(target.Turns, []ids.TurnID{"t2", "t3"}) {
		t.Fatalf("turns = %v, want [t2 t3]", target.Turns)
	}
	if got := target.Said.GetContent().GetBlocks()[0].GetText().GetText(); got != "question t2" {
		t.Fatalf("said = %q, want the prompt as said", got)
	}
}

func TestRollbackTargetRefusesARowThatIsNoReachablePrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)

	// Act
	_, ok := h.resolver.RollbackTarget(testWorkspace, &frontendv1.FeedId{Value: "row|not-a-prompt"})

	// Assert
	if ok {
		t.Fatal("RollbackTarget answered a target for a row that is no prompt")
	}
}

func TestRollBackTurnsRemovesEveryRowOfTheTurnsAndKeepsTheRest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)

	// Act
	err := h.resolver.RollBackTurns(testWorkspace, []ids.TurnID{"t2", "t3"})

	// Assert
	if err != nil {
		t.Fatalf("RollBackTurns: %v", err)
	}
	for _, row := range h.rows(rootFeed()) {
		if turn := row.GetTurn().GetValue(); turn == "t2" || turn == "t3" {
			t.Fatalf("row %s of rolled-back turn %s is still drawn", row.GetId().GetValue(), turn)
		}
	}
	if got := values(h.resolver.RollbackPrompts(testWorkspace)); !slices.Equal(got, []string{promptRowID("t1").GetValue()}) {
		t.Fatalf("RollbackPrompts() = %v, want only t1 left", got)
	}
	if got := h.resolver.FinalResponses(testWorkspace); len(got) > 1 {
		t.Fatalf("FinalResponses() = %v, want at most t1's answer", values(got))
	}
	if !h.hasRecord("info", opRollbackApply) {
		t.Fatalf("records = %+v, want the rollback recorded at INFO", h.records())
	}
}

func TestARowOfARolledBackTurnArrivingLaterIsNotDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)
	if err := h.resolver.RollBackTurns(testWorkspace, []ids.TurnID{"t3"}); err != nil {
		t.Fatalf("RollBackTurns: %v", err)
	}

	// Act: a frame of the dropped turn was already in flight.
	h.liveResponse("u-late", "late words", "t3")

	// Assert
	for _, row := range h.rows(rootFeed()) {
		if row.GetTurn().GetValue() == "t3" {
			t.Fatalf("row %s of the rolled-back turn was drawn", row.GetId().GetValue())
		}
	}
}

func TestARolledBackTurnStaysRolledBackAfterARestart(t *testing.T) {
	// Arrange
	store := &fakeRolledBack{}
	before := newStoredHarness(t, store)
	threeTurns(before)
	if err := before.resolver.RollBackTurns(testWorkspace, []ids.TurnID{"t3"}); err != nil {
		t.Fatalf("RollBackTurns: %v", err)
	}

	// Act: the successor replays the conversation, the dropped branch included.
	after := newStoredHarness(t, store)
	threeTurns(after)

	// Assert
	for _, row := range after.rows(rootFeed()) {
		if row.GetTurn().GetValue() == "t3" {
			t.Fatalf("row %s of the rolled-back turn was drawn after a restart", row.GetId().GetValue())
		}
	}
	if !after.hasRecord("info", opRollbackRestore) {
		t.Fatalf("records = %+v, want the load recorded at INFO", after.records())
	}
}

func TestAFailedRollbackRecordStillRemovesTheRowsAndIsRaised(t *testing.T) {
	// Arrange
	store := &fakeRolledBack{putErr: errors.New("disk full")}
	h := newStoredHarness(t, store)
	threeTurns(h)

	// Act
	err := h.resolver.RollBackTurns(testWorkspace, []ids.TurnID{"t3"})

	// Assert
	if err == nil {
		t.Fatal("RollBackTurns hid a failed record")
	}
	for _, row := range h.rows(rootFeed()) {
		if row.GetTurn().GetValue() == "t3" {
			t.Fatal("the rolled-back turn is still drawn")
		}
	}
	assertRecordedFault(t, h, opRollbackApply, "disk full")
}

func TestAFailedRollbackLoadIsLoggedAndRaised(t *testing.T) {
	// Arrange
	store := &fakeRolledBack{loadErr: errors.New("corrupt page")}

	// Act
	h := newStoredHarness(t, store)

	// Assert
	assertRecordedFault(t, h, opRollbackRestore, "corrupt page")
}

// assertRecordedFault asserts an ERROR record of op naming cause, and a warning
// raised under op.
func assertRecordedFault(t *testing.T, h *harness, op, cause string) {
	t.Helper()
	found := false
	for _, record := range h.records() {
		if record.Level == "error" && record.Operation == op {
			for _, v := range record.Context {
				if s, ok := v.(string); ok && strings.Contains(s, cause) {
					found = true
				}
			}
		}
	}
	if !found {
		t.Fatalf("records = %+v, want an ERROR %s naming %q", h.records(), op, cause)
	}
	raised := false
	for _, key := range h.warnings.keys() {
		if len(key) >= len(op) && key[:len(op)] == op {
			raised = true
		}
	}
	if !raised {
		t.Fatalf("warnings = %v, want one keyed %s", h.warnings.keys(), op)
	}
}
