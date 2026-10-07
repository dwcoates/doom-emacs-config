// Package chessboard resolves the feed's chess boards: a CEE CLI session's
// game, drawn as the CEE CLI webapp's own widget (@chesscom/cee-web-widget).
//
// agent-repl ONLY RENDERS THE WIDGET. This package reads no chess data. It
// owns the widget's backend end to end — finding the explanation-engine
// checkout, building the widget and cee-webapp from it, starting cee-webapp
// through `gns cee debug webapp`, and asking cee-webapp for the widget's data
// and for a clicked square's answer — and composes each board's
// frontend.v1.FeedChessBoard from where that work stands. The feed resolver
// draws the bubble; the webapp mounts the widget from it verbatim.
//
// Design record: docs/protobuf-design/chess-board-feed.md.
package chessboard

import (
	"encoding/base64"
	"errors"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// Session is a CEE CLI session and the game loaded in it, the pair every
// board names. Comparable, so it keys a board's state.
type Session struct {
	// ID is the CEE CLI daemon's session id.
	ID string
	// GameID is the liveness token of the game the board was asked for.
	GameID string
}

// SessionOf reads the session a board call named. ok is false when either
// value is empty: such a call names no session.
func SessionOf(named *conversationv1.AgentChessBoardSession) (Session, bool) {
	s := Session{ID: named.GetSessionId(), GameID: named.GetGameId()}
	return s, s.ID != "" && s.GameID != ""
}

// tokenVersion leads every square token, so a token this daemon cannot read
// is refused rather than misread.
const tokenVersion = "v1"

// ErrMalformedToken is a square token this daemon did not mint.
var ErrMalformedToken = errors.New("chessboard: the square token is malformed")

// Token mints the board's square token: what a square click hands back.
func (s Session) Token() *frontendv1.FeedChessBoardSquareToken {
	enc := base64.RawURLEncoding
	return &frontendv1.FeedChessBoardSquareToken{
		Value: tokenVersion + "." + enc.EncodeToString([]byte(s.ID)) + "." + enc.EncodeToString([]byte(s.GameID)),
	}
}

// SessionFromToken decodes a square token back into its session.
func SessionFromToken(token *frontendv1.FeedChessBoardSquareToken) (Session, error) {
	parts := strings.Split(token.GetValue(), ".")
	if len(parts) != 3 || parts[0] != tokenVersion {
		return Session{}, fmt.Errorf("%w: %q", ErrMalformedToken, token.GetValue())
	}
	enc := base64.RawURLEncoding
	id, err := enc.DecodeString(parts[1])
	if err != nil {
		return Session{}, fmt.Errorf("%w: session id: %v", ErrMalformedToken, err)
	}
	game, err := enc.DecodeString(parts[2])
	if err != nil {
		return Session{}, fmt.Errorf("%w: game id: %v", ErrMalformedToken, err)
	}
	s := Session{ID: string(id), GameID: string(game)}
	if s.ID == "" || s.GameID == "" {
		return Session{}, fmt.Errorf("%w: it names no session", ErrMalformedToken)
	}
	return s, nil
}
