package feed

import (
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/chessboard"
)

// fakeChessBoards answers each session's bubble from a table the test sets,
// and records which sessions Board was asked for.
type fakeChessBoards struct {
	mu      sync.Mutex
	bubbles map[chessboard.Session]*frontendv1.FeedChessBoard
	asked   []chessboard.Session
}

func newFakeChessBoards() *fakeChessBoards {
	return &fakeChessBoards{bubbles: map[chessboard.Session]*frontendv1.FeedChessBoard{}}
}

// set makes s's bubble say text in the given arm-builder's shape.
func (f *fakeChessBoards) set(s chessboard.Session, bubble *frontendv1.FeedChessBoard) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.bubbles[s] = bubble
}

func (f *fakeChessBoards) Board(s chessboard.Session) *frontendv1.FeedChessBoard {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.asked = append(f.asked, s)
	return f.bubbles[s]
}

func (f *fakeChessBoards) View(s chessboard.Session) *frontendv1.FeedChessBoard {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.bubbles[s]
}

var boardSession = chessboard.Session{ID: "agent-a", GameID: "g-1"}

// preparingBubble is a board still being readied, saying step.
func preparingBubble(step string) *frontendv1.FeedChessBoard {
	return &frontendv1.FeedChessBoard{
		Heading: &frontendv1.FeedChessBoardHeading{Text: "Chess board · CEE session agent-a"},
		State: &frontendv1.FeedChessBoard_Preparing{Preparing: &frontendv1.FeedChessBoardPreparing{
			Step: &frontendv1.FeedChessBoardPreparingStep{Text: step},
		}},
	}
}

// boardCall is the agent's board call in one of its arms.
func boardCall(unit string, result any) *conversationv1.AgentActivity {
	call := &conversationv1.AgentChessBoard{}
	switch arm := result.(type) {
	case *conversationv1.AgentChessBoardStart:
		call.Result = &conversationv1.AgentChessBoard_Start{Start: arm}
	case *conversationv1.AgentChessBoardSuccess:
		call.Result = &conversationv1.AgentChessBoard_Success{Success: arm}
	case *conversationv1.AgentChessBoardFailure:
		call.Result = &conversationv1.AgentChessBoard_Failure{Failure: arm}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_ChessBoard{ChessBoard: call},
	}
}

// namedSession is the call's own spelling of boardSession.
func namedSession() *conversationv1.AgentChessBoardSession {
	return &conversationv1.AgentChessBoardSession{SessionId: boardSession.ID, GameId: boardSession.GameID}
}

// chessBoard finds the root feed's board bubble.
func (h *harness) chessBoard() *frontendv1.FeedChessBoard {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if board := row.GetActivity().GetChessBoard(); board != nil {
			return board
		}
	}
	h.t.Fatal("no chess board on the root feed")
	return nil
}

func TestABoardCallsStartDrawsTheBoardStatesBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.boards.set(boardSession, preparingBubble("Building the chess widget…"))

	// Act.
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: namedSession()}))

	// Assert.
	if got := h.chessBoard().GetPreparing().GetStep().GetText(); got != "Building the chess widget…" {
		t.Fatalf("step = %q, want the board state's step", got)
	}
}

func TestABoardCallsSuccessAsksForItsSession(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.boards.set(boardSession, preparingBubble("Loading the game…"))

	// Act.
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardSuccess{Session: namedSession()}))

	// Assert.
	if len(h.boards.asked) != 1 || h.boards.asked[0] != boardSession {
		t.Fatalf("asked %v, want the call's session once", h.boards.asked)
	}
}

func TestAFailedBoardCallDrawsUnavailableWithTheCallsWords(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardFailure{
		Session: namedSession(),
		Error: &conversationv1.AgentToolFailure{Content: &conversationv1.ToolResultContent{
			Blocks: []*conversationv1.ToolResultContentBlock{{Block: &conversationv1.ToolResultContentBlock_Text{
				Text: &conversationv1.TextBlock{Text: "invalid input"},
			}}},
		}},
	}))

	// Assert.
	if got := h.chessBoard().GetUnavailable().GetReason().GetText(); got != "The agent's board request failed: invalid input" {
		t.Fatalf("reason = %q, want the call's words", got)
	}
	if len(h.boards.asked) != 0 {
		t.Fatalf("a failed call asked the board state for %v", h.boards.asked)
	}
}

func TestABoardFrameNamingNoSessionDrawsNothingAndIsLogged(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: &conversationv1.AgentChessBoardSession{}}))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	h.assertUndrawable(errBoardNamesNoSession)
}

func TestABoardFrameWithNoBoardStateWiredDrawsNothingAndIsLogged(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.deps.ChessBoards = nil

	// Act.
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: namedSession()}))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	h.assertUndrawable(errNoChessBoards)
}

func TestRefreshChessBoardsRepublishesTheBoardWithItsNewState(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.boards.set(boardSession, preparingBubble("Building the chess widget…"))
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: namedSession()}))
	h.boards.set(boardSession, preparingBubble("Loading the game…"))

	// Act.
	h.resolver.RefreshChessBoards([]chessboard.Session{boardSession})

	// Assert.
	if got := h.chessBoard().GetPreparing().GetStep().GetText(); got != "Loading the game…" {
		t.Fatalf("step = %q, want the re-published step", got)
	}
}

func TestRefreshChessBoardsLeavesOtherSessionsBoardsAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.boards.set(boardSession, preparingBubble("Building the chess widget…"))
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: namedSession()}))
	h.boards.set(boardSession, preparingBubble("Loading the game…"))

	// Act.
	h.resolver.RefreshChessBoards([]chessboard.Session{{ID: "agent-b", GameID: "g-2"}})

	// Assert.
	if got := h.chessBoard().GetPreparing().GetStep().GetText(); got != "Building the chess widget…" {
		t.Fatalf("step = %q, want the board unchanged", got)
	}
}

func TestRefreshChessBoardsForgetsABoardWhoseRowLeftItsFeed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.boards.set(boardSession, preparingBubble("Building the chess widget…"))
	h.send(boardCall("unit-1", &conversationv1.AgentChessBoardStart{Session: namedSession()}))
	s := h.resolver.workspaces[testWorkspace]
	board := s.chessBoards["unit-1"]
	delete(h.resolver.feed(s, board.at.feed).rows, board.row.GetValue())

	// Act.
	h.resolver.RefreshChessBoards([]chessboard.Session{boardSession})

	// Assert.
	if _, ok := s.chessBoards["unit-1"]; ok {
		t.Fatal("a board whose row left its feed is still remembered")
	}
}

// assertUndrawable asserts the canonical undrawable-activity record carrying
// cause.
func (h *harness) assertUndrawable(cause error) {
	h.t.Helper()
	if !h.hasRecordWith("error", "daemon.feed.activity_undrawable", "cause", cause.Error()) {
		h.t.Fatalf("no ERROR daemon.feed.activity_undrawable with cause %q in %+v", cause, h.records())
	}
}
