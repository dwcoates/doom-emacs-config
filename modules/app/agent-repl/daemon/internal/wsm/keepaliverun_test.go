package wsm

import (
	"context"
	"os"
	"path/filepath"
	"strings"
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

// TestTheKeepaliveRunHasOneWrite pins countKeepalive as the run's one write:
// a hand-rolled count elsewhere would drift from its turn-clock rule.
func TestTheKeepaliveRunHasOneWrite(t *testing.T) {
	// Arrange
	sources, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}

	// Act
	var writers []string
	for _, name := range sources {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		text, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("ReadFile: %v", err)
		}
		if strings.Contains(string(text), "keepalive_run_beats =") {
			writers = append(writers, name)
		}
	}

	// Assert
	if len(writers) != 1 || writers[0] != "keepaliverun.go" {
		t.Fatalf("the run is written in %v, want keepaliverun.go alone", writers)
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
