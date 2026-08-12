package statedb

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"
)

// openReceipts opens a fresh state store on disk and installs the
// prompt_receipt table on it.
func openReceipts(t *testing.T) (*PromptReceipts, *sql.DB) {
	t.Helper()
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	receipts, err := NewPromptReceipts(db)
	if err != nil {
		t.Fatalf("NewPromptReceipts: %v", err)
	}
	return receipts, db
}

func TestAnUnreadableReceiptTableSurfacesItsCause(t *testing.T) {
	// Arrange — a dropped table is the shape of every read failure: the query
	// must report it rather than answering "nothing is owed".
	receipts, db := openReceipts(t)
	if _, err := db.Exec(`DROP TABLE prompt_receipt`); err != nil {
		t.Fatalf("drop table: %v", err)
	}

	// Act.
	_, err := receipts.PendingResumptions("/ws")

	// Assert.
	if err == nil {
		t.Fatal("reading a missing prompt_receipt table reported no error")
	}
	if !strings.Contains(err.Error(), "resumptions") {
		t.Fatalf("error = %v, want it to name the operation that failed", err)
	}
}

func TestNewPromptReceiptsIsIdempotentAcrossOpens(t *testing.T) {
	// Arrange — every daemon start installs the table; the second start must
	// not fail on a table that is already there.
	_, db := openReceipts(t)

	// Act.
	_, err := NewPromptReceipts(db)

	// Assert.
	if err != nil {
		t.Fatalf("second NewPromptReceipts: %v", err)
	}
}

func TestNewPromptReceiptsRefusesAnAbsentStore(t *testing.T) {
	// Arrange / Act.
	_, err := NewPromptReceipts(nil)

	// Assert.
	if err == nil {
		t.Fatal("NewPromptReceipts(nil) succeeded; there is nowhere to record a resumption")
	}
}
