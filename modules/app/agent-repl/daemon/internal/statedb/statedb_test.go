package statedb

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestOpenRejectsAnEmptyPath(t *testing.T) {
	// Arrange / Act.
	db, err := Open("")
	// Assert.
	if err == nil {
		db.Close()
		t.Fatal("Open(\"\") succeeded; an unnamed store cannot be durable")
	}
}

func TestOpenRejectsAnInMemoryPath(t *testing.T) {
	// Arrange / Act.
	db, err := Open(":memory:")
	// Assert.
	if err == nil {
		db.Close()
		t.Fatal("Open(\":memory:\") succeeded; the state store must be reopen-durable")
	}
}

func TestOpenCreatesTheParentDirectory(t *testing.T) {
	// Arrange — a nested path whose directories do not exist yet.
	path := filepath.Join(t.TempDir(), "a", "b", "state.db")
	// Act.
	db, err := Open(path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	// Assert.
	if _, err := os.Stat(filepath.Dir(path)); err != nil {
		t.Fatalf("parent dir was not created: %v", err)
	}
}

func TestOpenFailsLoudlyOnAFileThatIsNotADatabase(t *testing.T) {
	// Arrange — garbage where the store should be.
	path := filepath.Join(t.TempDir(), "state.db")
	if err := os.WriteFile(path, []byte("this is not a database"), 0o600); err != nil {
		t.Fatal(err)
	}
	// Act.
	db, err := Open(path)
	// Assert — the failure surfaces at Open, not on some later unrelated query.
	if err == nil {
		db.Close()
		t.Fatal("Open succeeded on a non-database file")
	}
	if !strings.Contains(err.Error(), path) {
		t.Fatalf("error %q does not name the offending path", err)
	}
}

func TestOpenHoldsExactlyOneConnection(t *testing.T) {
	// Arrange.
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	// Act.
	got := db.Stats().MaxOpenConnections
	// Assert — one writer, shared by every owner of the store.
	if got != 1 {
		t.Fatalf("MaxOpenConnections = %d, want 1", got)
	}
}

func TestOpenBeginsTransactionsImmediately(t *testing.T) {
	// Arrange — one handle holding an open transaction.
	path := filepath.Join(t.TempDir(), "state.db")
	writer, err := Open(path)
	if err != nil {
		t.Fatalf("Open writer: %v", err)
	}
	t.Cleanup(func() { writer.Close() })
	if _, err := writer.Exec(`CREATE TABLE t (k INTEGER PRIMARY KEY)`); err != nil {
		t.Fatalf("create table: %v", err)
	}
	tx, err := writer.Begin()
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	if _, err := tx.Exec(`INSERT INTO t(k) VALUES (1)`); err != nil {
		t.Fatalf("insert in tx: %v", err)
	}
	// Act — a SECOND handle's transaction must wait for the write lock rather
	// than fail; it cannot proceed until the first commits.
	if err := tx.Commit(); err != nil {
		t.Fatalf("commit: %v", err)
	}
	other, err := Open(path)
	if err != nil {
		t.Fatalf("Open other: %v", err)
	}
	t.Cleanup(func() { other.Close() })
	otherTx, err := other.Begin()
	if err != nil {
		t.Fatalf("begin on second handle: %v", err)
	}
	if _, err := otherTx.Exec(`INSERT INTO t(k) VALUES (2)`); err != nil {
		t.Fatalf("insert on second handle: %v", err)
	}
	if err := otherTx.Commit(); err != nil {
		t.Fatalf("commit on second handle: %v", err)
	}
	// Assert — both writes landed, from two handles over one file.
	var n int
	if err := other.QueryRow(`SELECT COUNT(*) FROM t`).Scan(&n); err != nil {
		t.Fatalf("count: %v", err)
	}
	if n != 2 {
		t.Fatalf("rows = %d, want 2", n)
	}
}
