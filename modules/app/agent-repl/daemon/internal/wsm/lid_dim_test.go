package wsm

import (
	"context"
	"errors"
	"math"
	"testing"

	"agent-repld/internal/dlog"
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

func TestPutLidDimLevelRoundTripsAndReplacesTheOneInForce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutLidDimLevel(context.Background(), 0.81); err != nil {
		t.Fatalf("PutLidDimLevel: %v", err)
	}

	// Act
	if err := s.PutLidDimLevel(context.Background(), 0.4); err != nil {
		t.Fatalf("PutLidDimLevel: %v", err)
	}
	got, ok, err := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || !ok || got != 0.4 {
		t.Fatalf("LidDimLevel() = (%v, %v, %v), want 0.4 in force", got, ok, err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM persistent_wifi_lid_dim`); n != 1 {
		t.Fatalf("%d rows, want the one singleton", n)
	}
}

func TestPutLidDimLevelKeepsZero(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.PutLidDimLevel(context.Background(), 0)
	got, ok, readErr := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || readErr != nil || !ok || got != 0 {
		t.Fatalf("LidDimLevel() = (%v, %v, %v) after keeping 0 (%v), want 0 in force", got, ok, readErr, err)
	}
}

func TestClearLidDimLevelEndsTheDim(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if err := s.PutLidDimLevel(context.Background(), 0.5); err != nil {
		t.Fatalf("PutLidDimLevel: %v", err)
	}

	// Act
	err := s.ClearLidDimLevel(context.Background())
	_, ok, readErr := s.LidDimLevel(context.Background())

	// Assert
	if err != nil || readErr != nil || ok {
		t.Fatalf("after ClearLidDimLevel (%v): ok %v, err %v, want no dim in force", err, ok, readErr)
	}
}

func TestPutLidDimLevelRefusesALevelOutsideZeroToOne(t *testing.T) {
	for _, level := range []float64{-0.1, 1.5, math.NaN()} {
		t.Run("", func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := s.PutLidDimLevel(context.Background(), level)

			// Assert
			if err == nil {
				t.Fatalf("PutLidDimLevel(%v) = nil, want a refusal", level)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM persistent_wifi_lid_dim`); n != 0 {
				t.Fatalf("%d rows after the refusal, want none", n)
			}
			if len(dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.put_lid_dim_level"})) != 1 {
				t.Fatalf("records = %+v, want one ERROR for the refusal", log.Records())
			}
		})
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
