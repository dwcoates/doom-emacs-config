package server

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/chessboard"
)

// fakeChessSquares answers square clicks from a fixed answer or error, and
// records what it was asked.
type fakeChessSquares struct {
	answer []byte
	err    error
	asked  []string
}

func (f *fakeChessSquares) InspectSquare(_ context.Context, s chessboard.Session, gamePoint int64, square uint32) ([]byte, error) {
	f.asked = append(f.asked, fmt.Sprintf("%s/%s@%d:%d", s.ID, s.GameID, gamePoint, square))
	return f.answer, f.err
}

var squareSession = chessboard.Session{ID: "agent-a", GameID: "g-1"}

// inspect sends one square click through the server.
func inspect(t *testing.T, h *harness, token *frontendv1.FeedChessBoardSquareToken) (*agentreplv1.InspectChessBoardSquareResponse, error) {
	t.Helper()
	resp, err := h.Server.(*server).InspectChessBoardSquare(context.Background(), connect.NewRequest(&agentreplv1.InspectChessBoardSquareRequest{
		Board: token, GamePoint: 42, Square: 28,
	}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestInspectChessBoardSquareRelaysTheBoardsSessionPositionAndSquare(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ChessBoards.answer = []byte{0x08, 0x1c}

	// Act.
	resp, err := inspect(t, h, squareSession.Token())

	// Assert.
	if err != nil || string(resp.GetSuccess().GetGetSquareEventsResponse()) != string([]byte{0x08, 0x1c}) {
		t.Fatalf("response = %v, %v; want the answer whole", resp, err)
	}
	if len(h.ChessBoards.asked) != 1 || h.ChessBoards.asked[0] != "agent-a/g-1@42:28" {
		t.Fatalf("asked %v, want the board's session at 42, square 28", h.ChessBoards.asked)
	}
}

func TestInspectChessBoardSquareRefusesATokenItDidNotMint(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := inspect(t, h, &frontendv1.FeedChessBoardSquareToken{Value: "forged"})

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.ChessBoards.asked) != 0 {
		t.Fatalf("error = %v (asked %v), want InvalidArgument and no call", err, h.ChessBoards.asked)
	}
}

func TestInspectChessBoardSquareAnswersAGoneSessionWithItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ChessBoards.err = fmt.Errorf("%w: swept", chessboard.ErrSessionGone)

	// Act.
	resp, err := inspect(t, h, squareSession.Token())

	// Assert.
	if err != nil || !strings.Contains(resp.GetError().GetSessionGone().GetDetail(), "swept") {
		t.Fatalf("response = %v, %v; want the session_gone arm", resp, err)
	}
}

func TestInspectChessBoardSquareAnswersAnUnreachableBackendWithItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ChessBoards.err = fmt.Errorf("%w: connection refused", chessboard.ErrBackend)

	// Act.
	resp, err := inspect(t, h, squareSession.Token())

	// Assert.
	if err != nil || !strings.Contains(resp.GetError().GetBackendUnreachable().GetDetail(), "connection refused") {
		t.Fatalf("response = %v, %v; want the backend_unreachable arm", resp, err)
	}
}

func TestInspectChessBoardSquarePanicsOnAnErrorOutsideItsContract(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ChessBoards.err = errors.New("unclassified")
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("an unclassified InspectSquare error did not panic")
		}
	}()

	// Act.
	_, _ = inspect(t, h, squareSession.Token())
}

func TestTheChessWidgetBundleIsMountedOnItsRoute(t *testing.T) {
	// Arrange.
	h := newHarness(t, func(d *Deps) {
		d.ChessWidgetBundle = http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { _, _ = w.Write([]byte("bundle")) })
	})
	rec := httptest.NewRecorder()

	// Act.
	h.Server.ServeHTTP(rec, httptest.NewRequest(http.MethodGet, chessboard.BundleRoute+"stamp/cee-web-widget.js", nil))

	// Assert.
	if rec.Body.String() != "bundle" {
		t.Fatalf("GET the bundle route answered %q, want the bundle handler's answer", rec.Body.String())
	}
}

func TestNewRefusesMissingChessDeps(t *testing.T) {
	tests := []struct {
		name  string
		strip func(*Deps)
		want  string
	}{
		{name: "boards", strip: func(d *Deps) { d.ChessBoards = nil }, want: "the chess boards"},
		{name: "bundle", strip: func(d *Deps) { d.ChessWidgetBundle = nil }, want: "the chess widget bundle"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var built Deps
			newHarness(t, func(d *Deps) { built = *d })
			tt.strip(&built)

			// Act.
			_, err := New(built)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("New() error = %v, want it to name %s", err, tt.want)
			}
		})
	}
}
