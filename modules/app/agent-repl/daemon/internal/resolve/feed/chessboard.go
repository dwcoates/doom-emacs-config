package feed

import (
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/chessboard"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ---- THE CHESS BOARD ----
//
// A board is drawn where the agent asked for it (conversation.v1.AgentChessBoard),
// and its body — preparing, unavailable, or the mounted widget — is the
// daemon's chess board state (internal/chessboard), which changes long after
// the call's frames have arrived. So every drawn board is remembered by its
// session, and a change to that session's board re-publishes the row in place
// (RefreshChessBoards).

// ChessBoards is the daemon's board state, as the feed needs it
// (chessboard.Boards).
type ChessBoards interface {
	// Board answers a session's bubble as it stands, starting its resolution.
	Board(s chessboard.Session) *frontendv1.FeedChessBoard
	// View answers a drawn session's bubble as it stands, starting nothing.
	View(s chessboard.Session) *frontendv1.FeedChessBoard
}

// chessBoardRow is one drawn board: the session it shows and where its row
// stands.
type chessBoardRow struct {
	session chessboard.Session
	at      placement
	row     *frontendv1.FeedId
}

// errBoardNamesNoSession is a board call's start or success that names no
// session: both producers write neither arm without one.
var errBoardNamesNoSession = errors.New("feed: a chess board frame names no session")

// errNoChessBoards is a board arriving with no board state wired.
var errNoChessBoards = errors.New("feed: a chess board arrived but Deps.ChessBoards is nil")

// drawChessBoard draws the agent's board call as its bubble.
func (r *resolver) drawChessBoard(s *wsState, at placement, act *conversationv1.AgentActivity, call *conversationv1.AgentChessBoard) (*frontendv1.FeedRow, error) {
	unit := act.GetActivityId().GetValue()
	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unit})
	var bubble *frontendv1.FeedChessBoard
	switch result := call.GetResult().(type) {
	case *conversationv1.AgentChessBoard_Failure:
		bubble = chessboard.FailedRequest(result.Failure.GetSession(), failureText(result.Failure.GetError()))
		delete(s.chessBoards, unit)
	default:
		named := call.GetStart().GetSession()
		if call.GetSuccess() != nil {
			named = call.GetSuccess().GetSession()
		}
		session, ok := chessboard.SessionOf(named)
		if !ok {
			return nil, errBoardNamesNoSession
		}
		if r.deps.ChessBoards == nil {
			return nil, errNoChessBoards
		}
		bubble = r.deps.ChessBoards.Board(session)
		s.chessBoards[unit] = chessBoardRow{session: session, at: at, row: id}
	}
	return &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_ChessBoard{ChessBoard: bubble},
		}},
	}, nil
}

// RefreshChessBoards re-publishes every drawn board of the named sessions with
// its bubble as it now stands. A board whose row has left its feed is
// forgotten.
func (r *resolver) RefreshChessBoards(sessions []chessboard.Session) {
	changed := make(map[chessboard.Session]bool, len(sessions))
	for _, session := range sessions {
		changed[session] = true
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	for _, s := range r.workspaces {
		for unit, board := range s.chessBoards {
			if !changed[board.session] {
				continue
			}
			f := r.feed(s, board.at.feed)
			stored, ok := f.rows[board.row.GetValue()]
			if !ok || stored.GetActivity().GetChessBoard() == nil {
				r.logger(s.id).Debug("daemon.feed.chess_board", "a changed board's row is no longer on its feed; it is forgotten", dlog.Context{
					"unit": unit, "cee_session_id": board.session.ID,
				})
				delete(s.chessBoards, unit)
				continue
			}
			bubble := r.deps.ChessBoards.View(board.session)
			r.restateRow(s, board.at, stored, true, unclonable{
				operation: "daemon.feed.row_not_clonable",
				message:   "a chess board row could not be snapshotted to re-publish its board",
				context:   dlog.Context{"feed": f.key, "row": board.row.GetValue(), "unit": unit},
			}, func(restated *frontendv1.FeedRow) {
				restated.GetActivity().Unit = &frontendv1.FeedTurnActivity_ChessBoard{ChessBoard: bubble}
			})
			r.logger(s.id).Debug("daemon.feed.chess_board", "re-published a chess board whose state changed", dlog.Context{
				"unit": unit, "cee_session_id": board.session.ID, "row": board.row.GetValue(),
			})
		}
	}
}
