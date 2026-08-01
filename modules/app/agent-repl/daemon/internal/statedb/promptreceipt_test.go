package statedb

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"
)

// openReceipts opens a fresh state store on disk and installs the prompt
// receipt table on it.
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

func TestARecordedReceiptIsOutstandingForItsWorkspace(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)

	// Act.
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Assert.
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 || got[0] != (PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}) {
		t.Fatalf("outstanding = %+v, want the one recorded receipt", got)
	}
}

func TestARecordedReceiptSurvivesReopeningTheStore(t *testing.T) {
	// Arrange — the whole point is testifying to a prompt ACROSS a daemon's
	// death, so the row has to be on disk rather than in a connection.
	path := filepath.Join(t.TempDir(), "state.db")
	first, err := Open(path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	writer, err := NewPromptReceipts(first)
	if err != nil {
		t.Fatalf("NewPromptReceipts: %v", err)
	}
	if err := writer.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act — a second daemon opens the same store.
	second, err := Open(path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	t.Cleanup(func() { _ = second.Close() })
	reader, err := NewPromptReceipts(second)
	if err != nil {
		t.Fatalf("NewPromptReceipts on reopen: %v", err)
	}

	// Assert.
	got, err := reader.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 || got[0].RequestID != "r-1" {
		t.Fatalf("outstanding after reopen = %+v, want the receipt written by the previous daemon", got)
	}
}

func TestOutstandingOrdersReceiptsOldestFirst(t *testing.T) {
	// Arrange — submit order is replay order.
	receipts, _ := openReceipts(t)
	for _, r := range []PromptReceipt{
		{RequestID: "r-2", Workspace: "/ws", Text: "second", AcceptedAtMs: 2_000},
		{RequestID: "r-1", Workspace: "/ws", Text: "first", AcceptedAtMs: 1_000},
	} {
		if err := receipts.Record(r); err != nil {
			t.Fatalf("Record %s: %v", r.RequestID, err)
		}
	}

	// Act.
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}

	// Assert.
	if len(got) != 2 || got[0].RequestID != "r-1" || got[1].RequestID != "r-2" {
		t.Fatalf("outstanding = %+v, want oldest first", got)
	}
}

func TestOutstandingIsScopedToItsWorkspace(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/other", Text: "elsewhere", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}

	// Assert.
	if len(got) != 0 {
		t.Fatalf("outstanding for /ws = %+v, want none: another workspace's receipt is not this one's", got)
	}
}

func TestRecordingTheSameRequestTwiceOverwritesRatherThanFailing(t *testing.T) {
	// Arrange — the request id IS the submit's identity, so a second write
	// under it is the same submit being re-accepted.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "first", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "second", AcceptedAtMs: 2_000}); err != nil {
		t.Fatalf("re-Record: %v", err)
	}

	// Assert.
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 || got[0].Text != "second" || got[0].AcceptedAtMs != 2_000 {
		t.Fatalf("outstanding = %+v, want exactly one receipt carrying the later acceptance", got)
	}
}

func TestRecordingAReceiptWithNoRequestIDIsRefused(t *testing.T) {
	// Arrange — the request id is the identity the bubble is keyed on, so a
	// receipt without one names a bubble nothing could ever claim.
	receipts, _ := openReceipts(t)

	// Act.
	err := receipts.Record(PromptReceipt{Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000})

	// Assert.
	if err == nil {
		t.Fatal("recording a receipt with no request id succeeded")
	}
}

func TestRecordingAReceiptWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange — replay is keyed by workspace; a receipt without one is
	// unreachable by every reader.
	receipts, _ := openReceipts(t)

	// Act.
	err := receipts.Record(PromptReceipt{RequestID: "r-1", Text: "hello", AcceptedAtMs: 1_000})

	// Assert.
	if err == nil {
		t.Fatal("recording a receipt with no workspace succeeded")
	}
}

func TestRetiringAnOutstandingReceiptReportsItWasThere(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	retired, err := receipts.Retire("r-1")
	if err != nil {
		t.Fatalf("Retire: %v", err)
	}

	// Assert.
	if !retired {
		t.Fatal("retiring an outstanding receipt reported nothing was there")
	}
}

func TestRetiringTheSameReceiptTwiceIsANoOpRatherThanAnError(t *testing.T) {
	// Arrange — the retirement points are several (the durable line arriving
	// live, a replay finding the prompt already in the store, a failed submit)
	// and any of them may legitimately run second.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}
	if _, err := receipts.Retire("r-1"); err != nil {
		t.Fatalf("first Retire: %v", err)
	}

	// Act.
	retired, err := receipts.Retire("r-1")

	// Assert.
	if err != nil {
		t.Fatalf("second Retire: %v, want a no-op", err)
	}
	if retired {
		t.Fatal("the second retirement claimed to have deleted a row")
	}
}

func TestRetiringWithNoRequestIDIsRefused(t *testing.T) {
	// Arrange — an empty id would match no row and say nothing, so asking is
	// a caller bug rather than a no-op.
	receipts, _ := openReceipts(t)

	// Act.
	_, err := receipts.Retire("")

	// Assert.
	if err == nil {
		t.Fatal("retiring with no request id succeeded")
	}
}

func TestRetiringAWorkspaceDiscardsOnlyReceiptsAtOrBeforeTheCut(t *testing.T) {
	// Arrange — a clear discards the history below it, and only that.
	receipts, _ := openReceipts(t)
	for _, r := range []PromptReceipt{
		{RequestID: "r-old", Workspace: "/ws", Text: "before", AcceptedAtMs: 1_000},
		{RequestID: "r-new", Workspace: "/ws", Text: "after", AcceptedAtMs: 3_000},
	} {
		if err := receipts.Record(r); err != nil {
			t.Fatalf("Record %s: %v", r.RequestID, err)
		}
	}

	// Act.
	n, err := receipts.RetireWorkspace("/ws", 2_000)
	if err != nil {
		t.Fatalf("RetireWorkspace: %v", err)
	}

	// Assert.
	if n != 1 {
		t.Fatalf("rows deleted = %d, want 1", n)
	}
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 || got[0].RequestID != "r-new" {
		t.Fatalf("outstanding = %+v, want only the receipt accepted after the cut", got)
	}
}

func TestRetiringAWorkspaceLeavesOtherWorkspacesAlone(t *testing.T) {
	// Arrange — a cut in one workspace says nothing about another's prompts.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/other", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	if _, err := receipts.RetireWorkspace("/ws", 9_000); err != nil {
		t.Fatalf("RetireWorkspace: %v", err)
	}

	// Assert.
	got, err := receipts.Outstanding("/other")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("outstanding for /other = %+v, want the untouched receipt", got)
	}
}

func TestAnUnreadableReceiptTableSurfacesItsCause(t *testing.T) {
	// Arrange — a dropped table is the shape of every read failure: the query
	// must report it rather than answering "no receipts".
	receipts, db := openReceipts(t)
	if _, err := db.Exec(`DROP TABLE prompt_receipt`); err != nil {
		t.Fatalf("drop table: %v", err)
	}

	// Act.
	_, err := receipts.Outstanding("/ws")

	// Assert.
	if err == nil {
		t.Fatal("reading a missing prompt_receipt table reported no error")
	}
	if !strings.Contains(err.Error(), "prompt receipts") {
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
		t.Fatal("NewPromptReceipts(nil) succeeded; there is nowhere to record a receipt")
	}
}
