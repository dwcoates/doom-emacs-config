package wsm

import (
	"context"
	"testing"
	"time"
)

// swappedOnce answers a store holding one workspace whose queue conversation
// was beaten at INSTANT and then parked by a switch an hour later.
func swappedOnce(t *testing.T) (*store, Workspace, time.Time) {
	t.Helper()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.SetLastKeepalive(ctx, ws.ID, instant); err != nil {
		t.Fatalf("SetLastKeepalive: %v", err)
	}
	switchAt := instant.Add(time.Hour)
	if err := s.SwapParkedConversation(ctx, ws.ID, "vendor-queue", switchAt); err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}
	return s, ws, switchAt
}

func TestTheFirstSwapParksTheConversationWithItsClock(t *testing.T) {
	// Arrange
	s, ws, _ := swappedOnce(t)

	// Act
	got, err := s.QueueBypass(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("QueueBypass: %v", err)
	}
	if got.ParkedVendorSessionID != "vendor-queue" || got.ParkedClockAt == nil || !got.ParkedClockAt.Equal(instant) || got.ParkedCold {
		t.Fatalf("parked = %q at %v cold %v, want the queue conversation at its own clock, warm", got.ParkedVendorSessionID, got.ParkedClockAt, got.ParkedCold)
	}
}

func TestTheFirstSwapBindsAConversationWithNoClockOfItsOwn(t *testing.T) {
	// Arrange
	s, ws, switchAt := swappedOnce(t)

	// Act
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.CacheClock() != nil {
		t.Fatalf("CacheClock() = %v, want none until the bound conversation's first turn", got.CacheClock())
	}
	if got.ConversationBoundAt == nil || !got.ConversationBoundAt.Equal(switchAt) {
		t.Fatalf("ConversationBoundAt = %v, want the switch %v", got.ConversationBoundAt, switchAt)
	}
}

func TestASwapBackBindsTheParkedConversationsOwnClock(t *testing.T) {
	// Arrange
	s, ws, switchAt := swappedOnce(t)
	ctx := context.Background()
	bypassBeat := switchAt.Add(10 * time.Minute)
	if err := s.SetLastKeepalive(ctx, ws.ID, bypassBeat); err != nil {
		t.Fatalf("SetLastKeepalive: %v", err)
	}

	// Act
	err := s.SwapParkedConversation(ctx, ws.ID, "vendor-bypass", switchAt.Add(20*time.Minute))

	// Assert
	if err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}
	record, _ := s.Workspace(ctx, ws.ID)
	if clock := record.CacheClock(); clock == nil || !clock.Equal(instant) {
		t.Fatalf("bound CacheClock() = %v, want the queue conversation's own %v", clock, instant)
	}
	standing, _ := s.QueueBypass(ctx, ws.ID)
	if standing.ParkedVendorSessionID != "vendor-bypass" || standing.ParkedClockAt == nil || !standing.ParkedClockAt.Equal(bypassBeat) {
		t.Fatalf("parked = %q at %v, want the bypass conversation at %v", standing.ParkedVendorSessionID, standing.ParkedClockAt, bypassBeat)
	}
}

func TestASwapBindsAColdParkedConversationCold(t *testing.T) {
	// Arrange
	s, ws, switchAt := swappedOnce(t)
	ctx := context.Background()
	if err := s.MarkParkedCold(ctx, ws.ID); err != nil {
		t.Fatalf("MarkParkedCold: %v", err)
	}

	// Act
	err := s.SwapParkedConversation(ctx, ws.ID, "vendor-bypass", switchAt.Add(time.Minute))

	// Assert
	if err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}
	record, _ := s.Workspace(ctx, ws.ID)
	if record.CacheClock() != nil {
		t.Fatalf("bound CacheClock() = %v, want none for a conversation whose cache lapsed", record.CacheClock())
	}
}

func TestASwapRefusesABlankConversationAndWritesNothing(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	err := s.SwapParkedConversation(ctx, ws.ID, " ", instant)

	// Assert
	if err == nil {
		t.Fatal("SwapParkedConversation accepted a blank conversation")
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspace_parked_conversations`); n != 0 {
		t.Fatalf("workspace_parked_conversations holds %d rows after a refusal", n)
	}
	if !loggedError(logs.Records(), "daemon.wsm.swap_parked_conversation") {
		t.Fatalf("the refusal was not logged at error: %+v", logs.Records())
	}
}

func TestAParkedKeepaliveMovesTheParkedClockAndWarmsIt(t *testing.T) {
	// Arrange
	s, ws, switchAt := swappedOnce(t)
	ctx := context.Background()
	if err := s.MarkParkedCold(ctx, ws.ID); err != nil {
		t.Fatalf("MarkParkedCold: %v", err)
	}
	beat := switchAt.Add(time.Hour)

	// Act
	err := s.SetParkedKeepalive(ctx, ws.ID, beat)

	// Assert
	if err != nil {
		t.Fatalf("SetParkedKeepalive: %v", err)
	}
	standing, _ := s.QueueBypass(ctx, ws.ID)
	if standing.ParkedClockAt == nil || !standing.ParkedClockAt.Equal(beat) || standing.ParkedCold {
		t.Fatalf("parked clock %v cold %v, want %v and warm", standing.ParkedClockAt, standing.ParkedCold, beat)
	}
}

func TestMarkParkedColdMarksTheParkedCache(t *testing.T) {
	// Arrange
	s, ws, _ := swappedOnce(t)
	ctx := context.Background()

	// Act
	err := s.MarkParkedCold(ctx, ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MarkParkedCold: %v", err)
	}
	if standing, _ := s.QueueBypass(ctx, ws.ID); !standing.ParkedCold {
		t.Fatal("the parked cache is not marked cold")
	}
}

func TestTheParkedClockRefusesAWorkspaceThatParksNothing(t *testing.T) {
	cases := []struct {
		name  string
		write func(s *store, ws WorkspaceID) error
	}{
		{name: "a parked keep-alive", write: func(s *store, ws WorkspaceID) error { return s.SetParkedKeepalive(context.Background(), ws, instant) }},
		{name: "a parked cold mark", write: func(s *store, ws WorkspaceID) error { return s.MarkParkedCold(context.Background(), ws) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tc.write(s, ws.ID)

			// Assert
			if err == nil {
				t.Fatal("the write was accepted for a workspace that parks nothing")
			}
		})
	}
}
