package chessboard

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestSessionOf(t *testing.T) {
	tests := []struct {
		name   string
		named  *conversationv1.AgentChessBoardSession
		want   Session
		wantOK bool
	}{
		{name: "both values", named: &conversationv1.AgentChessBoardSession{SessionId: "agent-a", GameId: "g-1"}, want: Session{ID: "agent-a", GameID: "g-1"}, wantOK: true},
		{name: "no game id", named: &conversationv1.AgentChessBoardSession{SessionId: "agent-a"}, want: Session{ID: "agent-a"}, wantOK: false},
		{name: "no session id", named: &conversationv1.AgentChessBoardSession{GameId: "g-1"}, want: Session{GameID: "g-1"}, wantOK: false},
		{name: "nothing named", named: nil, want: Session{}, wantOK: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, ok := SessionOf(tt.named)

			// Assert.
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("SessionOf() = %v, %t; want %v, %t", got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestATokenDecodesBackToItsSession(t *testing.T) {
	// Arrange. Separator-bearing values must survive the round trip.
	want := Session{ID: "agent.a/b", GameID: "g.1"}

	// Act.
	got, err := SessionFromToken(want.Token())

	// Assert.
	if err != nil || got != want {
		t.Fatalf("SessionFromToken(Token()) = %v, %v; want %v", got, err, want)
	}
}

func TestSessionFromTokenRefusesMalformedTokens(t *testing.T) {
	tests := []struct {
		name  string
		value string
	}{
		{name: "empty", value: ""},
		{name: "another version", value: "v2.YQ.Zw"},
		{name: "missing part", value: "v1.YQ"},
		{name: "not base64", value: "v1.***.Zw"},
		{name: "empty session", value: "v1..Zw"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := SessionFromToken(&frontendv1.FeedChessBoardSquareToken{Value: tt.value})

			// Assert.
			if !errors.Is(err, ErrMalformedToken) {
				t.Fatalf("SessionFromToken(%q) error = %v, want ErrMalformedToken", tt.value, err)
			}
		})
	}
}
