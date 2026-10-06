package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestOpenInMemoryRefusesOutsideATestBinary(t *testing.T) {
	// Arrange
	isTestBinary = func() bool { return false }
	t.Cleanup(func() { isTestBinary = testingTesting })

	// Act
	_, err := OpenInMemory(context.Background())

	// Assert
	if !errors.Is(err, errNotATest) {
		t.Fatalf("OpenInMemory outside a test binary = %v, want errNotATest", err)
	}
}

func TestOpenInMemoryStartsAtThisBuildsLayout(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	version := scalar[int](t, s, `SELECT version FROM layout WHERE id = 1`)

	// Assert
	if version != LayoutVersion {
		t.Fatalf("layout version = %d, want %d", version, LayoutVersion)
	}
}

func TestOpenInMemoryGivesEachHandleItsOwnDatabase(t *testing.T) {
	// Arrange
	first, _ := testStore(t)
	second, _ := testStore(t)
	if _, err := first.CreateTask(context.Background(), "only in the first"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	count := scalar[int](t, second, `SELECT count(*) FROM tasks`)

	// Assert
	if count != 0 {
		t.Fatalf("the second in-memory handle sees %d task rows, want 0", count)
	}
}

func TestOpenInMemoryLeavesTheTemplateUntouched(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.CreateTask(context.Background(), "written after the copy"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	var count int
	err := templateHandle.QueryRowContext(context.Background(), `SELECT count(*) FROM tasks`).Scan(&count)

	// Assert
	if err != nil {
		t.Fatalf("count the template's tasks: %v", err)
	}
	if count != 0 {
		t.Fatalf("the template holds %d task rows, want 0", count)
	}
}

func TestOpenInMemoryEnforcesForeignKeys(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := scalar[int](t, s, `PRAGMA foreign_keys`)

	// Assert
	if got != 1 {
		t.Fatalf("PRAGMA foreign_keys = %d, want 1", got)
	}
}
