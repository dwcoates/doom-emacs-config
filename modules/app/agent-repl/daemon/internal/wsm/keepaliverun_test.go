package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

// readRun reads workspace WS's keep-alive run back.
func readRun(t *testing.T, s *store, ws WorkspaceID) int {
	t.Helper()
	got, err := s.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return got.KeepaliveRunBeats
}

func TestTheKeepaliveRun(t *testing.T) {
	tests := []struct {
		name string
		// arrange drives the workspace's keep-alives and turns.
		arrange func(t *testing.T, s *store, ws WorkspaceID)
		want    int
	}{
		{name: "a workspace never beaten has run none", arrange: func(*testing.T, *store, WorkspaceID) {}, want: 0},
		{name: "each keep-alive sent counts into the run", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			for i := 1; i <= 3; i++ {
				if err := s.SetLastKeepalive(context.Background(), ws, instant.Add(time.Duration(i)*time.Hour)); err != nil {
					t.Fatalf("SetLastKeepalive: %v", err)
				}
			}
		}, want: 3},
		{name: "a turn closing ends the run with no write of its own", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			if err := s.SetLastKeepalive(context.Background(), ws, instant.Add(time.Hour)); err != nil {
				t.Fatalf("SetLastKeepalive: %v", err)
			}
			closeTurnAt(t, s, ws, instant.Add(2*time.Hour))
		}, want: 0},
		{name: "a keep-alive after a turn starts the run over at one", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			for _, at := range []time.Duration{time.Hour, 2 * time.Hour} {
				if err := s.SetLastKeepalive(context.Background(), ws, instant.Add(at)); err != nil {
					t.Fatalf("SetLastKeepalive: %v", err)
				}
			}
			closeTurnAt(t, s, ws, instant.Add(3*time.Hour))
			if err := s.SetLastKeepalive(context.Background(), ws, instant.Add(4*time.Hour)); err != nil {
				t.Fatalf("SetLastKeepalive: %v", err)
			}
		}, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			tt.arrange(t, s, ws.ID)

			// Act
			got := readRun(t, s, ws.ID)

			// Assert
			if got != tt.want {
				t.Fatalf("KeepaliveRunBeats = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestAParkedKeepaliveCountsIntoTheRun(t *testing.T) {
	// Arrange
	s, ws, switchAt := swappedOnce(t)
	before := readRun(t, s, ws.ID)

	// Act
	err := s.SetParkedKeepalive(context.Background(), ws.ID, switchAt.Add(time.Hour))

	// Assert
	if err != nil {
		t.Fatalf("SetParkedKeepalive: %v", err)
	}
	if got := readRun(t, s, ws.ID); got != before+1 {
		t.Fatalf("KeepaliveRunBeats = %d, want %d", got, before+1)
	}
}

func TestARefusedParkedKeepaliveCountsNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetParkedKeepalive(context.Background(), ws.ID, instant)

	// Assert
	if err == nil {
		t.Fatal("the parked keep-alive was accepted for a workspace that parks nothing")
	}
	if got := readRun(t, s, ws.ID); got != 0 {
		t.Fatalf("KeepaliveRunBeats = %d after a refused write, want 0", got)
	}
}

func TestAKeepaliveOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetLastKeepalive(context.Background(), WorkspaceID("absent"), instant)

	// Assert
	if err == nil {
		t.Fatal("a keep-alive was recorded for a workspace that does not exist")
	}
}

// closeTurnAt closes one turn of WS at AT.
func closeTurnAt(t *testing.T, s *store, ws WorkspaceID, at time.Time) {
	t.Helper()
	if err := s.CloseTurn(context.Background(), openTurn(t, s, ws), at, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}
}

func TestTheMigrationAddsTheKeepaliveRunColumns(t *testing.T) {
	// Arrange — a file written before a keep-alive run was counted.
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	got := scalar[int](t, s,
		`SELECT count(*) FROM pragma_table_info('workspaces') WHERE name IN ('keepalive_run_turn_at', 'keepalive_run_beats')`)
	if got != 2 {
		t.Fatalf("%d of the keep-alive run's two columns exist after the migration", got)
	}
}

// beatAt sends one keep-alive of WS at AT.
func beatAt(t *testing.T, s *store, ws WorkspaceID, at time.Time) {
	t.Helper()
	if err := s.SetLastKeepalive(context.Background(), ws, at); err != nil {
		t.Fatalf("SetLastKeepalive: %v", err)
	}
}

// spendInto adds SPENT to WS's run, failing the test on an error.
func spendInto(t *testing.T, s *store, ws WorkspaceID, spent KeepaliveRunTokens) bool {
	t.Helper()
	took, err := s.AddKeepaliveRunSpend(context.Background(), ws, spent)
	if err != nil {
		t.Fatalf("AddKeepaliveRunSpend: %v", err)
	}
	return took
}

func TestEndKeepaliveRun(t *testing.T) {
	spent := KeepaliveRunTokens{Uncached: 1, CacheRead: 900, Write5m: 2, Write1h: 3, Output: 4}
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, ws WorkspaceID)
		ending  KeepaliveRunEnding
		want    KeepaliveRun
		ended   bool
	}{
		{name: "a workspace with no run ends none", arrange: func(*testing.T, *store, WorkspaceID) {}, ending: RunEndingTurnClosed},
		{name: "a standing run is not ended by a beat", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
		}, ending: RunEndingBeat},
		{name: "a run a turn closed after ended resumed, with what it spent", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
			beatAt(t, s, ws, instant.Add(2*time.Hour))
			spendInto(t, s, ws, spent)
			closeTurnAt(t, s, ws, instant.Add(3*time.Hour))
		}, ending: RunEndingTurnClosed, want: KeepaliveRun{Beats: 2, Spent: spent, End: KeepaliveRunResumed}, ended: true},
		{name: "a run at its cap ended capped", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
		}, ending: RunEndingCap, want: KeepaliveRun{Beats: 1, End: KeepaliveRunCapped}, ended: true},
		{name: "a run of a closed workspace ended closed", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
			if err := s.MarkClosed(context.Background(), ws, Closure{At: instant.Add(2 * time.Hour), By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
		}, ending: RunEndingClosure, want: KeepaliveRun{Beats: 1, End: KeepaliveRunClosed}, ended: true},
		{name: "a reopen ends no standing run", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
		}, ending: RunEndingClosure},
		{name: "a closed workspace's run a turn ended first ended resumed", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
			closeTurnAt(t, s, ws, instant.Add(2*time.Hour))
			if err := s.MarkClosed(context.Background(), ws, Closure{At: instant.Add(3 * time.Hour), By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
		}, ending: RunEndingClosure, want: KeepaliveRun{Beats: 1, End: KeepaliveRunResumed}, ended: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			tt.arrange(t, s, ws.ID)
			// Act
			got, ended, err := s.EndKeepaliveRun(context.Background(), ws.ID, tt.ending)
			// Assert
			if err != nil {
				t.Fatalf("EndKeepaliveRun: %v", err)
			}
			if ended != tt.ended || got != tt.want {
				t.Fatalf("EndKeepaliveRun = %+v, %v; want %+v, %v", got, ended, tt.want, tt.ended)
			}
		})
	}
}

func TestARunsEndIsRecordedOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	closeTurnAt(t, s, ws.ID, instant)
	beatAt(t, s, ws.ID, instant.Add(time.Hour))
	if _, ended, err := s.EndKeepaliveRun(context.Background(), ws.ID, RunEndingCap); err != nil || !ended {
		t.Fatalf("the cap ended nothing: %v, %v", ended, err)
	}
	closeTurnAt(t, s, ws.ID, instant.Add(2*time.Hour))
	// Act
	_, ended, err := s.EndKeepaliveRun(context.Background(), ws.ID, RunEndingTurnClosed)
	// Assert
	if err != nil || ended {
		t.Fatalf("a capped run's end was recorded again on the turn's close: %v, %v", ended, err)
	}
}

func TestANewRunStartsWithNothingSpentAndNoEnd(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	closeTurnAt(t, s, ws.ID, instant)
	beatAt(t, s, ws.ID, instant.Add(time.Hour))
	spendInto(t, s, ws.ID, KeepaliveRunTokens{CacheRead: 900})
	if _, _, err := s.EndKeepaliveRun(context.Background(), ws.ID, RunEndingCap); err != nil {
		t.Fatalf("EndKeepaliveRun: %v", err)
	}
	closeTurnAt(t, s, ws.ID, instant.Add(2*time.Hour))
	beatAt(t, s, ws.ID, instant.Add(3*time.Hour))
	// Act
	got, ended, err := s.EndKeepaliveRun(context.Background(), ws.ID, RunEndingCap)
	// Assert
	if err != nil || !ended || got != (KeepaliveRun{Beats: 1, End: KeepaliveRunCapped}) {
		t.Fatalf("the new run read %+v, %v, %v; want one beat, nothing spent, capped", got, ended, err)
	}
}

func TestAddKeepaliveRunSpend(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, ws WorkspaceID)
		took    bool
	}{
		{name: "a standing run takes the spend", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
		}, took: true},
		{name: "a run a turn ended takes none", arrange: func(t *testing.T, s *store, ws WorkspaceID) {
			closeTurnAt(t, s, ws, instant)
			beatAt(t, s, ws, instant.Add(time.Hour))
			closeTurnAt(t, s, ws, instant.Add(2*time.Hour))
		}},
		{name: "a workspace never beaten takes none", arrange: func(*testing.T, *store, WorkspaceID) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			tt.arrange(t, s, ws.ID)
			// Act
			took := spendInto(t, s, ws.ID, KeepaliveRunTokens{CacheRead: 900, Output: 2})
			took2 := spendInto(t, s, ws.ID, KeepaliveRunTokens{CacheRead: 100})
			// Assert
			if took != tt.took || took2 != tt.took {
				t.Fatalf("AddKeepaliveRunSpend took %v, %v; want %v", took, took2, tt.took)
			}
			if !tt.took {
				return
			}
			run, _, err := s.EndKeepaliveRun(context.Background(), ws.ID, RunEndingCap)
			if err != nil || run.Spent != (KeepaliveRunTokens{CacheRead: 1000, Output: 2}) {
				t.Fatalf("the run spent %+v (%v), want both spends summed", run.Spent, err)
			}
		})
	}
}

func TestEndKeepaliveRunOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	// Act
	_, _, err := s.EndKeepaliveRun(context.Background(), WorkspaceID("absent"), RunEndingCap)
	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("EndKeepaliveRun of an unknown workspace = %v, want ErrNotFound", err)
	}
}

func TestTheMigrationAddsTheKeepaliveRunSpendColumns(t *testing.T) {
	// Arrange — a file written before a keep-alive run's spend was kept.
	path := layout3Fixture(t)
	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()
	// Assert
	got := scalar[int](t, handle.(*store),
		`SELECT count(*) FROM pragma_table_info('workspaces') WHERE name IN ('keepalive_run_uncached', 'keepalive_run_cache_read', 'keepalive_run_write_5m', 'keepalive_run_write_1h', 'keepalive_run_output', 'keepalive_run_ended')`)
	if got != 6 {
		t.Fatalf("%d of the keep-alive run's six spend columns exist after the migration", got)
	}
}
