package wsm

import (
	"context"
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
		want      *time.Time
	}{
		{name: "neither", want: nil},
		{name: "a turn alone", ended: &early, want: &early},
		{name: "a keep-alive alone", keepalive: &early, want: &early},
		{name: "a keep-alive after the turn", ended: &early, keepalive: &late, want: &late},
		{name: "a turn after the keep-alive", ended: &late, keepalive: &early, want: &late},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := Workspace{LastTurnEndedAt: tt.ended, LastKeepaliveAt: tt.keepalive}

			// Act
			got := w.CacheClock()

			// Assert
			if (got == nil) != (tt.want == nil) || (got != nil && !got.Equal(*tt.want)) {
				t.Fatalf("CacheClock() = %v, want %v", got, tt.want)
			}
		})
	}
}
