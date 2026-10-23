package promptqueue

import (
	"context"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// triedUnderBlock holds t1 under a usage limit, then sends t2 with nothing
// running, which tries t1 now: t1 is the turn in flight, t2 held behind it.
func triedUnderBlock(t *testing.T, h *harness) {
	t.Helper()
	heldUnderBlock(t, h, "usage_limit", "t1")
	submitAll(t, h, "t2")
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want [t1] tried now", got)
	}
}

// endTried ends the tried turn t1 as HOW, the session idle again.
func endTried(h *harness, how wsm.TurnClose) {
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "t1", how)
}

// standingInOrder is the workspace's standing holds in queue order.
func standingInOrder(t *testing.T, h *harness) []wsm.HeldPrompt {
	t.Helper()
	held, err := h.db.HeldPrompts(context.Background(), theWorkspace)
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	slices.SortStableFunc(held, func(a, b wsm.HeldPrompt) int { return a.QueuedAt.Compare(b.QueuedAt) })
	return held
}

func TestATriedPromptTheVendorBlocksIsReHeldInItsPlace(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	held := standingInOrder(t, h)
	if len(held) != 2 || saidText(held[0].Said) != "prompt t1" || !heldOnReconnect(held[0]) || held[1].Turn != "t2" {
		t.Fatalf("standing = %+v, want t1's prompt held after reconnect ahead of t2", held)
	}
}

func TestAReHeldPromptCarriesANewTurn(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if held := standingInOrder(t, h); held[0].Turn == "t1" || held[0].Turn == "" {
		t.Fatalf("re-held turn = %q, want a newly minted turn: t1 was started once", held[0].Turn)
	}
}

func TestATriedPromptTheVendorBlocksIsCutFromTheVendorConversation(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if got := h.sender.rolledBack(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("rolled back = %v, want [t1] cut so it is never said twice", got)
	}
}

func TestATriedPromptTheVendorBlocksLeavesNoFailedExchangeInTheFeed(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if got := h.feed.rolledBackTurns(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("feed rolled back = %v, want [t1] removed from the feed", got)
	}
}

func TestTheBlockedTurnEndDeliversNothing(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want only the one try while the block stands", got)
	}
}

func TestAReHeldPromptIsRedeliveredUnderItsNewTurnWhenTheVendorServes(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)
	endTried(h, wsm.CloseFailed)
	again := standingInOrder(t, h)[0].Turn

	// Act
	vendorServes(h)

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t1", again}) {
		t.Fatalf("started = %v, want [t1 %s]: the re-held prompt goes first, under its new turn", got, again)
	}
}

func TestAPromptTheVendorNeverRecordedIsReHeldWithNothingCut(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)
	h.sender.rollBackErr = fmt.Errorf("%w: refused", ErrPromptNotRecorded)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if held := standingInOrder(t, h); len(held) != 2 || saidText(held[0].Said) != "prompt t1" {
		t.Fatalf("standing = %+v, want t1's prompt re-held ahead of t2", held)
	}
}

func TestATriedTurnThatIsNotReHeldStaysAsItWas(t *testing.T) {
	tests := []struct {
		name        string
		how         wsm.TurnClose
		block       string
		rollBackErr error
		wantCut     bool
	}{
		{name: "the turn completed", how: wsm.CloseCompleted, block: "usage_limit"},
		{name: "the turn was killed", how: wsm.CloseKilled, block: "usage_limit"},
		{name: "the turn failed with no vendor block", how: wsm.CloseFailed, block: ""},
		{name: "the cut was refused: it stays in the vendor conversation", how: wsm.CloseFailed, block: "usage_limit",
			rollBackErr: errors.New("RollBackSession refused: first_prompt"), wantCut: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newVendorBlockHarness(t)
			triedUnderBlock(t, h)
			h.footer.standVendorBlock(tc.block)
			h.sender.rollBackErr = tc.rollBackErr

			// Act
			endTried(h, tc.how)

			// Assert
			cut := len(h.sender.rolledBack()) > 0
			for _, held := range standingInOrder(t, h) {
				if saidText(held.Said) == "prompt t1" {
					t.Fatalf("t1's prompt was re-held as %+v, want it left as the turn it was", held)
				}
			}
			if cut != tc.wantCut || len(h.feed.rolledBackTurns()) != 0 {
				t.Fatalf("cut = %v, feed rolled back = %v, want cut %v and nothing removed from the feed", cut, h.feed.rolledBackTurns(), tc.wantCut)
			}
		})
	}
}

func TestATriedTurnEndingWhileAnotherRunsIsNotReHeld(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)
	h.watcher.running("t9")

	// Act
	h.q.OnTurnEnded(theWorkspace, "t1", wsm.CloseFailed)

	// Assert
	if got := h.sender.rolledBack(); len(got) != 0 {
		t.Fatalf("rolled back = %v, want no cut while another turn runs", got)
	}
}

func TestTheReHoldIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	triedUnderBlock(t, h)

	// Act
	endTried(h, wsm.CloseFailed)

	// Assert
	if !logged(h.log.Records(), "info", opTurnEnded, "the vendor blocked the tried prompt; it is back on the after-reconnect hold in its place") {
		t.Fatalf("no INFO record of the re-hold")
	}
}

func TestHeldAgainKeepsThePromptsWordsAndPlaceUnderANewTurn(t *testing.T) {
	// Arrange
	queued := instant.Add(-time.Minute)
	held := wsm.HeldPrompt{Workspace: theWorkspace, Turn: "t1", Said: textSaid("the words"), QueuedAt: queued, Origin: "PROMPT_ORIGIN_USER_SENT"}
	// Act
	again := heldAgain(held, nil, nil)
	// Assert
	if again.Turn == "" || again.Turn == "t1" || !again.QueuedAt.Equal(queued) || saidText(again.Said) != "the words" || again.Origin != held.Origin {
		t.Fatalf("held again = %+v, want t1's words, origin and place under a new turn", again)
	}
}

func TestHeldAgainStandsUnderTheGivenHoldAndVerdict(t *testing.T) {
	// Arrange
	kind := wsm.HoldReconnect
	verdict := &wsm.Classification{Arm: wsm.ArmHoldForTurnEnd, Reason: "why", At: instant}
	// Act
	again := heldAgain(wsm.HeldPrompt{Turn: "t1"}, &kind, verdict)
	// Assert
	if again.Hold != &kind || again.Classification != verdict {
		t.Fatalf("held again = %+v, want the given hold and verdict", again)
	}
}

func TestHeldAgainDropsTheOldEntrysRetirementAndSchedule(t *testing.T) {
	// Arrange
	old := wsm.HeldPrompt{
		Turn:           "t1",
		ScheduleID:     "schedule-1",
		Tombstone:      &wsm.Tombstone{Kind: tombstoneDelivered, At: instant},
		Classification: &wsm.Classification{Arm: wsm.ArmAfterToolCall},
	}
	// Act
	again := heldAgain(old, nil, nil)
	// Assert
	if again.ScheduleID != "" || again.Tombstone != nil || again.Classification != nil || again.Hold != nil {
		t.Fatalf("held again = %+v, want no schedule, retirement, verdict or hold carried over", again)
	}
}

// THE ONE SHAPE. A held prompt's turn is re-minted (`x.Turn = wsm.NewTurnID()`)
// only inside heldAgain, so no hand-rolled re-hold can keep a retirement or a
// schedule the old entry carried.
func TestEveryReHoldGoesThroughHeldAgain(t *testing.T) {
	// Arrange
	fset := token.NewFileSet()
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var sites []string
	// Act
	for _, path := range files {
		if strings.HasSuffix(path, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", path, err)
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Name.Name == "heldAgain" {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				assign, ok := n.(*ast.AssignStmt)
				if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
					return true
				}
				lhs, ok := assign.Lhs[0].(*ast.SelectorExpr)
				if !ok || lhs.Sel.Name != "Turn" {
					return true
				}
				if call, ok := assign.Rhs[0].(*ast.CallExpr); ok {
					if sel, ok := call.Fun.(*ast.SelectorExpr); ok && sel.Sel.Name == "NewTurnID" {
						sites = append(sites, fset.Position(assign.Pos()).String()+" in "+fn.Name.Name)
					}
				}
				return true
			})
		}
	}
	// Assert
	if len(sites) != 0 {
		t.Fatalf("a held prompt's turn re-minted outside heldAgain: %v", sites)
	}
}
