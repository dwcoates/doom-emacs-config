package wsm

import (
	"context"
	"testing"
	"time"
)

func TestTheMigrationAddsTheMergeRacingDisabledTable(t *testing.T) {
	// Arrange -- a file written before racing could be disabled.
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got := scalar[int](t, handle.(*store),
		`SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'merge_racing_disabled'`)
	if got != 1 {
		t.Fatalf("merge_racing_disabled exists %d times after the migration, want 1", got)
	}
}

func TestMergeRacingIsEnabledOnAFreshFile(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.MergeRacing(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeRacing: %v", err)
	}
	if got != (RacingSwitch{}) {
		t.Fatalf("racing = %+v, want enabled", got)
	}
}

func TestDisabledRacingSurvivesAReopen(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	at := time.Date(2026, 10, 14, 13, 0, 0, 0, time.UTC)
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := handle.SetMergeRacingDisabled(context.Background(), true, at); err != nil {
		t.Fatalf("SetMergeRacingDisabled: %v", err)
	}
	handle.Close()

	// Act
	handle, err = Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("re-Open: %v", err)
	}
	defer handle.Close()
	got, err := handle.MergeRacing(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeRacing: %v", err)
	}
	if got != (RacingSwitch{Disabled: true, Since: at}) {
		t.Fatalf("racing after a reopen = %+v, want disabled since %v", got, at)
	}
}
