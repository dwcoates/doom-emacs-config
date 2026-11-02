package wsm

import (
	"context"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// selectionWalkWorld is three workspaces selected in order: from (oldest), mid, and
// start, the one a recency walk starts on.
type selectionWalkWorld struct {
	from, mid, start Workspace
}

func newSelectionWalkWorld(t *testing.T, s *store) selectionWalkWorld {
	t.Helper()
	w := selectionWalkWorld{from: testWorkspace(t, s), mid: testWorkspace(t, s), start: testWorkspace(t, s)}
	for i, ws := range []Workspace{w.from, w.mid, w.start} {
		if err := s.SetCurrent(context.Background(), ws.ID, instant.Add(time.Duration(i)*time.Minute)); err != nil {
			t.Fatalf("SetCurrent: %v", err)
		}
	}
	return w
}

func lastSelected(t *testing.T, s *store, id WorkspaceID) *time.Time {
	t.Helper()
	got, err := s.Workspace(context.Background(), id)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return got.LastSelectedAt
}

func TestTheWalkLandingStampsOnlyWhenARecordedSelectionLeavesIt(t *testing.T) {
	walked := instant.Add(time.Hour)
	tests := []struct {
		name string
		// act runs after the walk landed on mid at walked, and answers the
		// workspace whose last-selected instant the case asserts and its want.
		act func(t *testing.T, s *store, w selectionWalkWorld) (WorkspaceID, time.Time)
	}{
		{
			name: "a recorded selection of another workspace commits the landing's walk instant",
			act: func(t *testing.T, s *store, w selectionWalkWorld) (WorkspaceID, time.Time) {
				if err := s.SetCurrent(context.Background(), w.from.ID, instant.Add(2*time.Hour)); err != nil {
					t.Fatalf("SetCurrent: %v", err)
				}
				return w.mid.ID, walked
			},
		},
		{
			name: "a further walk step leaves the step it leaves unstamped",
			act: func(t *testing.T, s *store, w selectionWalkWorld) (WorkspaceID, time.Time) {
				if err := s.SetCurrentKeepingLastSelected(context.Background(), w.from.ID, instant.Add(2*time.Hour)); err != nil {
					t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
				}
				return w.mid.ID, instant.Add(time.Minute)
			},
		},
		{
			name: "a further step then a recorded selection commits only the last landing",
			act: func(t *testing.T, s *store, w selectionWalkWorld) (WorkspaceID, time.Time) {
				if err := s.SetCurrentKeepingLastSelected(context.Background(), w.from.ID, instant.Add(2*time.Hour)); err != nil {
					t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
				}
				if err := s.SetCurrent(context.Background(), w.start.ID, instant.Add(3*time.Hour)); err != nil {
					t.Fatalf("SetCurrent: %v", err)
				}
				return w.from.ID, instant.Add(2 * time.Hour)
			},
		},
		{
			name: "a recorded selection of the landing itself stamps the selection's instant",
			act: func(t *testing.T, s *store, w selectionWalkWorld) (WorkspaceID, time.Time) {
				if err := s.SetCurrent(context.Background(), w.mid.ID, instant.Add(2*time.Hour)); err != nil {
					t.Fatalf("SetCurrent: %v", err)
				}
				return w.mid.ID, instant.Add(2 * time.Hour)
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionWalkWorld(t, s)
			if err := s.SetCurrentKeepingLastSelected(context.Background(), w.mid.ID, walked); err != nil {
				t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
			}

			// Act
			id, want := tt.act(t, s, w)

			// Assert
			got := lastSelected(t, s, id)
			if got == nil || !got.Equal(want) {
				t.Fatalf("last selected of %s = %v, want %v", id, got, want)
			}
		})
	}
}

func TestARecordedSelectionWithNoWalkLeavesThePreviousInstantStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionWalkWorld(t, s)

	// Act
	if err := s.SetCurrent(context.Background(), w.from.ID, instant.Add(time.Hour)); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	got := lastSelected(t, s, w.start.ID)
	if want := instant.Add(2 * time.Minute); got == nil || !got.Equal(want) {
		t.Fatalf("last selected of the left workspace = %v, want its own %v", got, want)
	}
}

func TestAtMostOneWorkspaceCarriesAWalkLanding(t *testing.T) {
	tests := []struct {
		name string
		act  func(s *store, w selectionWalkWorld) error
		want int
	}{
		{
			name: "a walk step carries one",
			act:  func(*store, selectionWalkWorld) error { return nil },
			want: 1,
		},
		{
			name: "a further step moves it",
			act: func(s *store, w selectionWalkWorld) error {
				return s.SetCurrentKeepingLastSelected(context.Background(), w.from.ID, instant.Add(2*time.Hour))
			},
			want: 1,
		},
		{
			name: "a recorded selection clears it",
			act: func(s *store, w selectionWalkWorld) error {
				return s.SetCurrent(context.Background(), w.start.ID, instant.Add(2*time.Hour))
			},
			want: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newSelectionWalkWorld(t, s)
			if err := s.SetCurrentKeepingLastSelected(context.Background(), w.mid.ID, instant.Add(time.Hour)); err != nil {
				t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
			}

			// Act
			if err := tt.act(s, w); err != nil {
				t.Fatalf("act: %v", err)
			}

			// Assert
			if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE walk_landed_at IS NOT NULL`); n != tt.want {
				t.Fatalf("%d workspaces carry a walk landing, want %d", n, tt.want)
			}
		})
	}
}

func TestCommittingAWalkLandingIsRecordedWithTheLanding(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	w := newSelectionWalkWorld(t, s)
	if err := s.SetCurrentKeepingLastSelected(context.Background(), w.mid.ID, instant.Add(time.Hour)); err != nil {
		t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), w.from.ID, instant.Add(2*time.Hour)); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{
		Level: "info", Operation: "daemon.wsm.set_current",
		Message: "the selection left a recency walk's landing; the landing's selection instant is the walk's",
	})
	if !ok {
		t.Fatalf("the commit was not recorded: %v", log.Records())
	}
	if r.Context["landing"] != string(w.mid.ID) || r.Context["workspace"] != string(w.from.ID) {
		t.Fatalf("record context = %v, want landing %s and workspace %s", r.Context, w.mid.ID, w.from.ID)
	}
}

func TestARefusedSelectionCommitsNoWalkLanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newSelectionWalkWorld(t, s)
	if err := s.SetCurrentKeepingLastSelected(context.Background(), w.mid.ID, instant.Add(time.Hour)); err != nil {
		t.Fatalf("SetCurrentKeepingLastSelected: %v", err)
	}

	// Act
	err := s.SetCurrent(context.Background(), WorkspaceID("absent"), instant.Add(2*time.Hour))

	// Assert — the refusal rolls the commit back with the rest of the write.
	if err == nil {
		t.Fatal("SetCurrent of an absent workspace succeeded")
	}
	if got, want := lastSelected(t, s, w.mid.ID), instant.Add(time.Minute); got == nil || !got.Equal(want) {
		t.Fatalf("last selected of the landing = %v, want its standing %v", got, want)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE walk_landed_at IS NOT NULL`); n != 1 {
		t.Fatalf("%d workspaces carry a walk landing after the refusal, want 1", n)
	}
}

// TestTheMigrationAddsTheWalkLandingColumn pins the layout-98 step: a file
// written before walk landings were durable carries the column afterwards,
// empty on every workspace.
func TestTheMigrationAddsTheWalkLandingColumn(t *testing.T) {
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
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE walk_landed_at IS NOT NULL`); got != 0 {
		t.Fatalf("%d migrated workspaces carry a walk landing, want 0", got)
	}
}
