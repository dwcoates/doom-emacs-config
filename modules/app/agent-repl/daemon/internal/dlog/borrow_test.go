package dlog

import (
	"os"
	"path/filepath"
	"testing"
)

func TestBorrowedCloseIsInert(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "shim.log")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer f.Close()
	b := &borrowed{f: f, log: NewTestLogger(), name: "shim.log"}

	// Act: a borrower's ordinary defer.
	if err := b.Close(); err != nil {
		t.Fatalf("Close = %v, want success", err)
	}

	// Assert: the inode is still writable for every other writer.
	if _, err := f.Write([]byte("still open\n")); err != nil {
		t.Fatalf("the borrower's Close took the sink down: %v", err)
	}
}

func TestBorrowedCloseRecordsTheAttempt(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "shim.log")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer f.Close()
	log := NewTestLogger()
	b := &borrowed{f: f, log: log, name: "shim.log"}

	// Act.
	if err := b.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: tolerated, but never silently.
	records := log.Records()
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0].Level != "warn" || records[0].Operation != "daemon.dlog.borrow_close_ignored" {
		t.Fatalf("record = %+v, want a WARN daemon.dlog.borrow_close_ignored", records[0])
	}
}

func TestBorrowedFileIsTheDescriptor(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "shim.log")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	defer f.Close()
	b := &borrowed{f: f, log: NewTestLogger(), name: "shim.log"}

	// Act, Assert.
	if b.File() != f.Fd() {
		t.Fatalf("File() = %d, want the sink's descriptor %d", b.File(), f.Fd())
	}
}
