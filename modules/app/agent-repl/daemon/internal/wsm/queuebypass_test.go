package wsm

import (
	"context"
	"errors"
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
	if err := s.SwapParkedConversation(ctx, ws.ID, "vendor-queue", switchAt, true); err != nil {
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
	err := s.SwapParkedConversation(ctx, ws.ID, "vendor-bypass", switchAt.Add(20*time.Minute), true)

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
	err := s.SwapParkedConversation(ctx, ws.ID, "vendor-bypass", switchAt.Add(time.Minute), true)

	// Assert
	if err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}
	record, _ := s.Workspace(ctx, ws.ID)
	if record.CacheClock() != nil {
		t.Fatalf("bound CacheClock() = %v, want none for a conversation whose cache lapsed", record.CacheClock())
	}
}

func TestASwapThatReplacesTheParkedConversationBindsNoClock(t *testing.T) {
	// Arrange: the bypass conversation was beaten, then the workspace
	// switched back and parked it with that clock.
	s, ws, switchAt := swappedOnce(t)
	ctx := context.Background()
	if err := s.SetLastKeepalive(ctx, ws.ID, switchAt.Add(10*time.Minute)); err != nil {
		t.Fatalf("SetLastKeepalive: %v", err)
	}
	if err := s.SwapParkedConversation(ctx, ws.ID, "vendor-bypass", switchAt.Add(20*time.Minute), true); err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}

	// Act: a fresh fork replaces the parked bypass conversation.
	err := s.SwapParkedConversation(ctx, ws.ID, "vendor-queue", switchAt.Add(30*time.Minute), false)

	// Assert
	if err != nil {
		t.Fatalf("SwapParkedConversation: %v", err)
	}
	record, _ := s.Workspace(ctx, ws.ID)
	if record.CacheClock() != nil {
		t.Fatalf("bound CacheClock() = %v, want none: the fork reads no cache of the discarded conversation", record.CacheClock())
	}
}

func TestASwapRefusesABlankConversationAndWritesNothing(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	err := s.SwapParkedConversation(ctx, ws.ID, " ", instant, true)

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

func TestAQueueBypassHoldLiftsRemoteControl(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(*testing.T, *store) Workspace
		release    bool
		wantOn     bool
		wantLifted bool
	}{
		{name: "a take over remote control off lifts it", arrange: testLinkedWorktree, wantOn: true, wantLifted: true},
		{name: "a take over remote control on lifts nothing", arrange: testWorkspace, wantOn: true},
		{name: "a release after a lift falls back to off", arrange: testLinkedWorktree, release: true},
		{name: "a release after no lift keeps remote control on", arrange: testWorkspace, release: true, wantOn: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := tt.arrange(t, s)
			ctx := context.Background()

			// Act
			if err := s.TakeQueueBypass(ctx, ws.ID, instant); err != nil {
				t.Fatalf("TakeQueueBypass: %v", err)
			}
			if tt.release {
				if err := s.ReleaseQueueBypass(ctx, ws.ID); err != nil {
					t.Fatalf("ReleaseQueueBypass: %v", err)
				}
			}

			// Assert
			got, err := s.RemoteControl(ctx, ws.ID)
			if err != nil || got.On != tt.wantOn || got.LiftedByQueueBypass != tt.wantLifted || got.Recorded {
				t.Fatalf("RemoteControl = %+v, %v; want on %v, lifted %v, nothing recorded", got, err, tt.wantOn, tt.wantLifted)
			}
		})
	}
}

func TestATakeOverAnExplicitOnLiftsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorktree(t, s)
	ctx := context.Background()
	if _, err := s.SetRemoteControlMode(ctx, ws.ID, RemoteControlOn); err != nil {
		t.Fatalf("SetRemoteControlMode: %v", err)
	}

	// Act
	if err := s.TakeQueueBypass(ctx, ws.ID, instant); err != nil {
		t.Fatalf("TakeQueueBypass: %v", err)
	}
	if err := s.ReleaseQueueBypass(ctx, ws.ID); err != nil {
		t.Fatalf("ReleaseQueueBypass: %v", err)
	}

	// Assert
	got, err := s.RemoteControl(ctx, ws.ID)
	if err != nil || !got.On || got.LiftedByQueueBypass {
		t.Fatalf("RemoteControl = %+v, %v; want the explicit on kept past the release", got, err)
	}
}

func TestARefusedTakeLiftsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	holder := testLinkedWorktree(t, s)
	ctx := context.Background()
	other, _, err := s.RegisterWorkspace(ctx, t.TempDir(), RegisterFacts{
		Name: "other", Branch: "other", ParentBranch: "master", RepoDir: scalar[string](t, s, `SELECT r.dir FROM repositories AS r JOIN workspaces AS w ON w.repo_id = r.id WHERE w.id = ?`, holder.ID),
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	if err := s.TakeQueueBypass(ctx, holder.ID, instant); err != nil {
		t.Fatalf("TakeQueueBypass: %v", err)
	}

	// Act
	err = s.TakeQueueBypass(ctx, other.ID, instant)

	// Assert
	if !errors.Is(err, ErrQueueBypassHeld) {
		t.Fatalf("TakeQueueBypass = %v, want ErrQueueBypassHeld", err)
	}
	if got, err := s.RemoteControl(ctx, other.ID); err != nil || got.On || got.LiftedByQueueBypass {
		t.Fatalf("RemoteControl = %+v, %v; want the refused workspace left off", got, err)
	}
}

func TestALiftGoesWithTheWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorktree(t, s)
	if err := s.TakeQueueBypass(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("TakeQueueBypass: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, ws.ID)

	// Assert
	if rows := scalar[int](t, s, `SELECT COUNT(*) FROM queue_bypass_holders`); rows != 0 {
		t.Fatalf("holder rows = %d, want the hold and its lift gone with the workspace", rows)
	}
}

func TestClosingAHolderKeepsItsLift(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorktree(t, s)
	ctx := context.Background()
	if err := s.TakeQueueBypass(ctx, ws.ID, instant); err != nil {
		t.Fatalf("TakeQueueBypass: %v", err)
	}

	// Act
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	got, err := s.RemoteControl(ctx, ws.ID)
	if err != nil || !got.On || !got.LiftedByQueueBypass {
		t.Fatalf("RemoteControl = %+v, %v; want a closed holder still lifted, for its reopening", got, err)
	}
}

// TestTheMigrationAddsTheLiftColumn pins the layout-137 step: a file written
// before the lift was durable carries the column afterwards.
func TestTheMigrationAddsTheLiftColumn(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('queue_bypass_holders') WHERE name = 'lifted_remote_control'`); got != 1 {
		t.Fatalf("lifted_remote_control exists %d times after the migration, want 1", got)
	}
}
