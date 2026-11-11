package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestLidDimLevelIsAbsentUntilKept(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, ok, err := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || ok {
		t.Fatalf("LidDimLevel() = (ok %v, err %v), want no dim in force", ok, err)
	}
}

func TestLidDimLevelReadsALevelAnEarlierBuildKept(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	corrupt(t, s, `INSERT INTO persistent_wifi_lid_dim (id, restore_level) VALUES (1, 0)`)

	// Act
	got, ok, err := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || !ok || got != 0 {
		t.Fatalf("LidDimLevel() = (%v, %v, %v), want 0 in force", got, ok, err)
	}
}

func TestClearLidDimLevelEndsTheDim(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	corrupt(t, s, `INSERT INTO persistent_wifi_lid_dim (id, restore_level) VALUES (1, 0.5)`)

	// Act
	err := s.ClearLidDimLevel(context.Background())
	_, ok, readErr := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || readErr != nil || ok {
		t.Fatalf("after ClearLidDimLevel (%v): ok %v, err %v, want no dim in force", err, ok, readErr)
	}
}

func TestLidDimLevelRefusesACorruptStoredLevel(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	corrupt(t, s, `INSERT INTO persistent_wifi_lid_dim (id, restore_level) VALUES (1, 7)`)

	// Act
	_, _, err := s.LidDimLevel(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Table != "persistent_wifi_lid_dim" {
		t.Fatalf("LidDimLevel() error = %v, want a DecodeError on persistent_wifi_lid_dim", err)
	}
}

// TestTheMigrationAddsTheLidDimTable pins the layout-99 step: a file written
// before the lid dim was durable carries the table afterwards, with no dim in
// force.
func TestTheMigrationAddsTheLidDimTable(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	if got := scalar[int](t, handle.(*store), `SELECT count(*) FROM persistent_wifi_lid_dim`); got != 0 {
		t.Fatalf("%d migrated rows, want none", got)
	}
}
