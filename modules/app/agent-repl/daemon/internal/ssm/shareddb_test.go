package ssm

import (
	"path/filepath"
	"testing"

	"claude-repld/internal/statedb"
)

// The SSM shares ONE state store with the session registry. Both halves of
// that arrangement are contracts: the Manager migrates a handed-in handle, and
// it does not close a handle it does not own.

func TestOpenMigratesAHandedInStateStore(t *testing.T) {
	// Arrange — a bare store, opened by whoever owns it.
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })

	// Act.
	m, err := Open(Options{DB: db, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open with shared db: %v", err)
	}
	t.Cleanup(func() { m.Close() })

	// Assert — the SSM's own log table exists on the shared handle.
	var name string
	if err := db.QueryRow(
		`SELECT name FROM sqlite_master WHERE type='table' AND name='workspace_state'`).Scan(&name); err != nil {
		t.Fatalf("workspace_state missing from the shared store: %v", err)
	}
}

func TestCloseLeavesASharedStoreOpenForItsOwner(t *testing.T) {
	// Arrange.
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	m, err := Open(Options{DB: db, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open with shared db: %v", err)
	}

	// Act.
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert — the other owner's handle still works.
	if err := db.Ping(); err != nil {
		t.Fatalf("shared store was closed under its owner: %v", err)
	}
}

func TestCloseClosesAStoreTheManagerOpened(t *testing.T) {
	// Arrange — the Manager opened the store itself, so it owns it.
	m, err := Open(Options{DBPath: filepath.Join(t.TempDir(), "state.db"), Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if err := m.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	if err := m.db.Ping(); err == nil {
		t.Fatal("an owned store survived Close")
	}
}
