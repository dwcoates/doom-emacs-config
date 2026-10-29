package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestMarkCacheColdRecordsTheLastTurnEnd(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ended := instant.Add(time.Minute)
	if err := s.CloseTurn(context.Background(), openTurn(t, s, ws.ID), ended, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}

	// Act
	marked, markErr := s.MarkCacheCold(context.Background(), ws.ID)
	got, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if markErr != nil || readErr != nil {
		t.Fatalf("errors = %v, %v", markErr, readErr)
	}
	if marked == nil || !marked.Equal(ended) {
		t.Fatalf("marked = %v, want the last turn end %v", marked, ended)
	}
	if got.CacheColdTurnAt == nil || !got.CacheColdTurnAt.Equal(ended) {
		t.Fatalf("CacheColdTurnAt = %v, want %v", got.CacheColdTurnAt, ended)
	}
}

func TestMarkCacheColdOfAWorkspaceWithNoClosedTurnMarksNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	marked, markErr := s.MarkCacheCold(context.Background(), ws.ID)
	got, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if markErr != nil || readErr != nil {
		t.Fatalf("errors = %v, %v", markErr, readErr)
	}
	if marked != nil || got.CacheColdTurnAt != nil {
		t.Fatalf("marked = %v, CacheColdTurnAt = %v; want neither", marked, got.CacheColdTurnAt)
	}
}

func TestMarkCacheColdOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	marked, err := s.MarkCacheCold(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) || marked != nil {
		t.Fatalf("MarkCacheCold = %v, %v; want ErrNotFound and no mark", marked, err)
	}
	// THE CALLER OWNS AN ABSENT RECORD (cachelapse.Keeper.markCold records it
	// at ERROR), so the store's own record of it is DEBUG.
	if !loggedOperation(log, "daemon.wsm.mark_cache_cold", "debug") {
		t.Fatalf("the absent record was not logged under the mark's operation: %v", log.Records())
	}
}

func TestATurnClosedAfterTheMarkLeavesItBehind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.CloseTurn(context.Background(), openTurn(t, s, ws.ID), instant, CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}
	if _, err := s.MarkCacheCold(context.Background(), ws.ID); err != nil {
		t.Fatalf("MarkCacheCold: %v", err)
	}

	// Act
	if err := s.CloseTurn(context.Background(), openTurn(t, s, ws.ID), instant.Add(time.Hour), CloseCompleted); err != nil {
		t.Fatalf("CloseTurn: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || got.CacheKnownCold() {
		t.Fatalf("CacheKnownCold() = %v, %v; want a turn after the mark to warm it", got.CacheKnownCold(), err)
	}
}

func TestCacheKnownCold(t *testing.T) {
	early := instant
	late := instant.Add(time.Hour)
	tests := []struct {
		name  string
		ended *time.Time
		mark  *time.Time
		want  bool
	}{
		{name: "never marked", ended: &early, want: false},
		{name: "marked at the last turn end", ended: &early, mark: &early, want: true},
		{name: "marked after the last turn end", ended: &early, mark: &late, want: true},
		{name: "a turn closed after the mark", ended: &late, mark: &early, want: false},
		{name: "no turn has closed", mark: &early, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := Workspace{LastTurnEndedAt: tt.ended, CacheColdTurnAt: tt.mark}

			// Act
			got := w.CacheKnownCold()

			// Assert
			if got != tt.want {
				t.Fatalf("CacheKnownCold() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestTheMigrationAddsTheCacheColdColumn pins the layout-90 step: a file
// written before the cold mark was durable carries the column afterwards.
func TestTheMigrationAddsTheCacheColdColumn(t *testing.T) {
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
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = 'cache_cold_turn_at'`); got != 1 {
		t.Fatalf("workspaces.cache_cold_turn_at exists %d times after the migration, want 1", got)
	}
}

// TestTheMigratedRowsAreNotKnownCold pins what the new column means for a row
// that predates it: NULL, never marked, so its beats run as before.
func TestTheMigratedRowsAreNotKnownCold(t *testing.T) {
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
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE cache_cold_turn_at IS NOT NULL`); got != 0 {
		t.Fatalf("%d migrated workspace rows carry a cold mark, want none", got)
	}
}
