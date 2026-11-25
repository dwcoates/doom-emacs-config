package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestTheLastKeepaliveRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	setErr := s.SetLastKeepalive(context.Background(), ws.ID, instant)
	got, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if setErr != nil || readErr != nil {
		t.Fatalf("errors = %v, %v", setErr, readErr)
	}
	if got.LastKeepaliveAt == nil || !got.LastKeepaliveAt.Equal(instant) {
		t.Fatalf("LastKeepaliveAt = %v, want %v", got.LastKeepaliveAt, instant)
	}
}

func TestAWorkspaceNeverBeatenHasNoLastKeepalive(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || got.LastKeepaliveAt != nil {
		t.Fatalf("LastKeepaliveAt = %v, %v; want nil", got.LastKeepaliveAt, err)
	}
}

func TestCacheClock(t *testing.T) {
	early := instant
	late := instant.Add(time.Hour)
	tests := []struct {
		name      string
		ended     *time.Time
		keepalive *time.Time
		bound     *time.Time
		want      *time.Time
	}{
		{name: "neither", want: nil},
		{name: "a turn alone", ended: &early, want: &early},
		{name: "a keep-alive alone", keepalive: &early, want: &early},
		{name: "a keep-alive after the turn", ended: &early, keepalive: &late, want: &late},
		{name: "a turn after the keep-alive", ended: &late, keepalive: &early, want: &late},
		{name: "a turn from before the binding", ended: &early, bound: &late, want: nil},
		{name: "a turn from before the binding beside the bound conversation's clock", ended: &early, keepalive: &early, bound: &late, want: &early},
		{name: "a turn after the binding", ended: &late, bound: &early, want: &late},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := Workspace{LastTurnEndedAt: tt.ended, LastKeepaliveAt: tt.keepalive, ConversationBoundAt: tt.bound}

			// Act
			got := w.CacheClock()

			// Assert
			if (got == nil) != (tt.want == nil) || (got != nil && !got.Equal(*tt.want)) {
				t.Fatalf("CacheClock() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestBindNewConversationEmptiesTheCacheClock(t *testing.T) {
	// Arrange: the lost conversation's turn and keep-alive both stand.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.SetLastKeepalive(ctx, ws.ID, instant); err != nil {
		t.Fatalf("SetLastKeepalive: %v", err)
	}
	bound := instant.Add(time.Hour)

	// Act
	bindErr := s.BindNewConversation(ctx, ws.ID, bound)
	got, readErr := s.Workspace(ctx, ws.ID)

	// Assert
	if bindErr != nil || readErr != nil {
		t.Fatalf("errors = %v, %v", bindErr, readErr)
	}
	if got.LastKeepaliveAt != nil {
		t.Fatalf("LastKeepaliveAt = %v, want nil", got.LastKeepaliveAt)
	}
	if got.ConversationBoundAt == nil || !got.ConversationBoundAt.Equal(bound) {
		t.Fatalf("ConversationBoundAt = %v, want %v", got.ConversationBoundAt, bound)
	}
	if clock := got.CacheClock(); clock != nil {
		t.Fatalf("CacheClock() = %v, want nil", clock)
	}
}

func TestBindNewConversationRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.BindNewConversation(context.Background(), "missing", instant)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("BindNewConversation() = %v, want ErrNotFound", err)
	}
}
