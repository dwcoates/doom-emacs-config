package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/chessboard"
	"claude-repld/internal/dlog"
)

// ChessSquares answers a feed chess board's square clicks
// (chessboard.Boards.InspectSquare).
type ChessSquares interface {
	// InspectSquare answers cee-webapp's GetSquareEventsResponse for one square
	// of a board's game, whole. A failure wraps chessboard.ErrSessionGone or
	// chessboard.ErrBackend.
	InspectSquare(ctx context.Context, s chessboard.Session, gamePoint int64, square uint32) ([]byte, error)
}

// InspectChessBoardSquare relays a square click on a feed chess board to the
// board's CEE backend, and hands its answer back whole for the widget.
func (s *server) InspectChessBoardSquare(
	ctx context.Context,
	req *connect.Request[agentreplv1.InspectChessBoardSquareRequest],
) (*connect.Response[agentreplv1.InspectChessBoardSquareResponse], error) {
	session, err := chessboard.SessionFromToken(req.Msg.GetBoard())
	if err != nil {
		s.log.Info("daemon.server.inspect_chess_board_square", "refused a square click whose board token this daemon did not mint", dlog.Context{
			"cause": err.Error(),
		})
		return nil, connect.NewError(connect.CodeInvalidArgument, err)
	}
	answer, err := s.deps.ChessBoards.InspectSquare(ctx, session, req.Msg.GetGamePoint(), req.Msg.GetSquare())
	resp := &agentreplv1.InspectChessBoardSquareResponse{}
	switch {
	case errors.Is(err, chessboard.ErrSessionGone):
		resp.Result = &agentreplv1.InspectChessBoardSquareResponse_Error{Error: &agentreplv1.InspectChessBoardSquareError{
			Cause: &agentreplv1.InspectChessBoardSquareError_SessionGone{SessionGone: &agentreplv1.InspectChessBoardSquareSessionGone{
				Detail: err.Error(),
			}},
		}}
	case errors.Is(err, chessboard.ErrBackend):
		resp.Result = &agentreplv1.InspectChessBoardSquareResponse_Error{Error: &agentreplv1.InspectChessBoardSquareError{
			Cause: &agentreplv1.InspectChessBoardSquareError_BackendUnreachable{BackendUnreachable: &agentreplv1.InspectChessBoardSquareBackendUnreachable{
				Detail: err.Error(),
			}},
		}}
	case err != nil:
		// InspectSquare's contract wraps every failure in one of the two above.
		panic(fmt.Sprintf("server: InspectSquare answered an error outside its contract: %v", err))
	default:
		resp.Result = &agentreplv1.InspectChessBoardSquareResponse_Success{Success: &agentreplv1.InspectChessBoardSquareSuccess{
			GetSquareEventsResponse: answer,
		}}
	}
	return connect.NewResponse(resp), nil
}
