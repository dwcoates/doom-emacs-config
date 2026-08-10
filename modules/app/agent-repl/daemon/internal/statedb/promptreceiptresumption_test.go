package statedb

import (
	"database/sql"
	"path/filepath"
	"strings"
	"testing"
)

// pending is the resumption every test here starts from.
func pending() PendingResumption {
	return PendingResumption{
		RequestID:       "rd-1",
		Workspace:       "/ws",
		TurnID:          "t-1",
		Text:            "continue the interrupted work",
		InterruptedAtMs: 2_000,
	}
}

func TestARecordedResumptionIsOwedByItsWorkspace(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)

	// Act.
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}

	// Assert.
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 1 || got[0] != pending() {
		t.Fatalf("pending = %+v, want the one recorded resumption", got)
	}
}

func TestAPendingResumptionIsNeverServedAsAReceipt(t *testing.T) {
	// Arrange — this is the invisibility guarantee. Outstanding is the ONLY
	// path by which a row in this table becomes a rendered prompt bubble, so a
	// resumption it cannot return is a resumption no client can render.
	receipts, _ := openReceipts(t)

	// Act.
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}

	// Assert.
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("outstanding = %+v, want no renderable receipt for a resumption row", got)
	}
}

func TestAnOrdinaryReceiptIsStillServedAlongsideAResumption(t *testing.T) {
	// Arrange — the resumption filter must exclude resumptions and nothing
	// else, or hiding the re-drive would also hide the user's own prompt.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}

	// Act.
	got, err := receipts.Outstanding("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 || got[0].RequestID != "r-1" {
		t.Fatalf("outstanding = %+v, want only the user's own receipt", got)
	}
}

func TestAResumptionSurvivesReopeningTheStore(t *testing.T) {
	// Arrange — the whole point is surviving the bounce that interrupted the
	// turn, so the row has to be on disk rather than in a connection.
	path := filepath.Join(t.TempDir(), "state.db")
	first, err := Open(path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	writer, err := NewPromptReceipts(first)
	if err != nil {
		t.Fatalf("NewPromptReceipts: %v", err)
	}
	if err := writer.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("close first: %v", err)
	}

	// Act.
	second, err := Open(path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close()
	reader, err := NewPromptReceipts(second)
	if err != nil {
		t.Fatalf("NewPromptReceipts (reopen): %v", err)
	}
	got, err := reader.PendingResumptions("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 1 || got[0] != pending() {
		t.Fatalf("pending = %+v, want the resumption to outlive the process that recorded it", got)
	}
}

func TestPendingResumptionsOrdersOldestInterruptionFirst(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	late := pending()
	late.RequestID, late.InterruptedAtMs = "rd-late", 9_000
	early := pending()
	early.RequestID, early.InterruptedAtMs = "rd-early", 1_000
	if err := receipts.RecordPendingResumption(late); err != nil {
		t.Fatalf("RecordPendingResumption(late): %v", err)
	}
	if err := receipts.RecordPendingResumption(early); err != nil {
		t.Fatalf("RecordPendingResumption(early): %v", err)
	}

	// Act.
	got, err := receipts.PendingResumptions("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 2 || got[0].RequestID != "rd-early" || got[1].RequestID != "rd-late" {
		t.Fatalf("pending = %+v, want interruption order", got)
	}
}

func TestPendingResumptionsIsScopedToItsWorkspace(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	other := pending()
	other.RequestID, other.Workspace = "rd-other", "/elsewhere"
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	if err := receipts.RecordPendingResumption(other); err != nil {
		t.Fatalf("RecordPendingResumption(other): %v", err)
	}

	// Act.
	got, err := receipts.PendingResumptions("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 1 || got[0].RequestID != "rd-1" {
		t.Fatalf("pending = %+v, want only this workspace's resumption", got)
	}
}

func TestRecordingTheSameResumptionTwiceOverwritesRatherThanDuplicating(t *testing.T) {
	// Arrange — exactly-once is level-triggered off this row, so two writes
	// under one request id must leave ONE thing owed rather than two.
	receipts, _ := openReceipts(t)
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("first RecordPendingResumption: %v", err)
	}

	// Act.
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("second RecordPendingResumption: %v", err)
	}

	// Assert.
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("pending = %+v, want one owed resumption after two identical records", got)
	}
}

func TestRecordingAResumptionWithNoRequestIDIsRefused(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	bad := pending()
	bad.RequestID = ""

	// Act.
	err := receipts.RecordPendingResumption(bad)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no request id") {
		t.Fatalf("err = %v, want a refusal naming the missing request id", err)
	}
}

func TestRecordingAResumptionWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	bad := pending()
	bad.Workspace = ""

	// Act.
	err := receipts.RecordPendingResumption(bad)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no workspace") {
		t.Fatalf("err = %v, want a refusal naming the missing workspace", err)
	}
}

func TestDischargingAnOwedResumptionReportsItWasThere(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}

	// Act.
	discharged, err := receipts.DischargeResumption("rd-1")

	// Assert.
	if err != nil {
		t.Fatalf("DischargeResumption: %v", err)
	}
	if !discharged {
		t.Fatal("discharged = false, want true for a resumption that was owed")
	}
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("pending = %+v, want nothing owed after the discharge", got)
	}
}

func TestDischargingTheSameResumptionTwiceIsANoOpRatherThanAnError(t *testing.T) {
	// Arrange — the re-drive's acceptance and the user's preemption can
	// legitimately race, and the loser is not a failure.
	receipts, _ := openReceipts(t)
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	if _, err := receipts.DischargeResumption("rd-1"); err != nil {
		t.Fatalf("first DischargeResumption: %v", err)
	}

	// Act.
	discharged, err := receipts.DischargeResumption("rd-1")

	// Assert.
	if err != nil {
		t.Fatalf("second DischargeResumption: %v", err)
	}
	if discharged {
		t.Fatal("discharged = true, want false the second time round")
	}
}

func TestDischargingDoesNotTouchAnOrdinaryReceiptSharingTheRequestID(t *testing.T) {
	// Arrange — a discharge is scoped to resumption rows, so it can never
	// delete the user's own durable prompt evidence.
	receipts, _ := openReceipts(t)
	if err := receipts.Record(PromptReceipt{RequestID: "r-1", Workspace: "/ws", Text: "hello", AcceptedAtMs: 1_000}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	discharged, err := receipts.DischargeResumption("r-1")

	// Assert.
	if err != nil {
		t.Fatalf("DischargeResumption: %v", err)
	}
	if discharged {
		t.Fatal("discharged = true, want false: r-1 is a receipt, not a resumption")
	}
	got, err := receipts.Outstanding("/ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("outstanding = %+v, want the user's receipt untouched", got)
	}
}

func TestDischargingWithNoRequestIDIsRefused(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)

	// Act.
	_, err := receipts.DischargeResumption("")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no request id") {
		t.Fatalf("err = %v, want a refusal naming the missing request id", err)
	}
}

func TestReadingResumptionsForAnEmptyWorkspaceIsRefused(t *testing.T) {
	// Arrange — an empty workspace is never a defaulted match; the re-drive
	// must not run against every workspace at once.
	receipts, _ := openReceipts(t)

	// Act.
	_, err := receipts.PendingResumptions("")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("err = %v, want a refusal naming the empty workspace", err)
	}
}

func TestAContextCutRetiresAResumptionBelowIt(t *testing.T) {
	// Arrange — a clear or compaction discards the history the interrupted
	// turn belonged to, so re-driving it would put pre-cut work back above a
	// floor that exists to hide exactly that.
	receipts, _ := openReceipts(t)
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}

	// Act.
	n, err := receipts.RetireWorkspace("/ws", 5_000)

	// Assert.
	if err != nil {
		t.Fatalf("RetireWorkspace: %v", err)
	}
	if n != 1 {
		t.Fatalf("retired = %d, want the resumption below the cut retired", n)
	}
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("pending = %+v, want nothing owed below a context cut", got)
	}
}

func TestAnUnreadableResumptionTableSurfacesItsCause(t *testing.T) {
	// Arrange — an unreadable store must fail loudly rather than read as
	// "nothing is owed", which would silently abandon the interrupted turn.
	receipts, db := openReceipts(t)
	if _, err := db.Exec(`DROP TABLE prompt_receipt`); err != nil {
		t.Fatalf("drop prompt_receipt: %v", err)
	}

	// Act.
	_, err := receipts.PendingResumptions("/ws")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "read pending resumptions") {
		t.Fatalf("err = %v, want the read failure surfaced", err)
	}
	var _ *sql.DB = db
}
