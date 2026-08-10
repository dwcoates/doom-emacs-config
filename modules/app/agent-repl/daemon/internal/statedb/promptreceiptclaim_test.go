package statedb

import (
	"path/filepath"
	"strings"
	"testing"
)

// ONE INTERRUPTED TURN IS RE-DRIVEN ONCE.
//
// The row is the fence. A re-drive claims it before it submits, and only an
// unclaimed row is ever re-driven — so a bring-up that wires the same session
// several times, or a submit whose control request timed out on a shim that ran
// it anyway, cannot put a second copy of the daemon's instruction into the
// conversation. These tests pin the claim's arbitration and what each state is
// visible to.

// claimed records a resumption and takes it for delivery, failing the test if
// either step does.
func claimed(t *testing.T, receipts *PromptReceipts, r PendingResumption, atMs int64) {
	t.Helper()
	if err := receipts.RecordPendingResumption(r); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	took, err := receipts.ClaimResumptionForDelivery(r.RequestID, atMs)
	if err != nil {
		t.Fatalf("ClaimResumptionForDelivery: %v", err)
	}
	if !took {
		t.Fatalf("the first claim of %q must succeed", r.RequestID)
	}
}

func TestAClaimedResumptionIsNoLongerOwed(t *testing.T) {
	// Arrange, Act.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Assert.
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("pending = %+v, want a claimed resumption withheld from the re-drive level", got)
	}
}

func TestASecondClaimOfTheSameResumptionIsRefused(t *testing.T) {
	// Arrange — two wires of one session racing for the same owed turn. The
	// loser must submit nothing.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	took, err := receipts.ClaimResumptionForDelivery("rd-1", 6_000)

	// Assert.
	if err != nil {
		t.Fatalf("ClaimResumptionForDelivery: %v", err)
	}
	if took {
		t.Fatal("the second claim of one resumption must be refused")
	}
}

func TestClaimingAResumptionThatIsNotThereIsRefused(t *testing.T) {
	// Arrange — a discharged resumption cannot be picked back up.
	receipts, _ := openReceipts(t)

	// Act.
	took, err := receipts.ClaimResumptionForDelivery("rd-absent", 5_000)

	// Assert.
	if err != nil {
		t.Fatalf("ClaimResumptionForDelivery: %v", err)
	}
	if took {
		t.Fatal("claiming an absent resumption must report that nothing was taken")
	}
}

func TestClaimingWithNoRequestIDIsRefused(t *testing.T) {
	// Arrange.
	receipts, _ := openReceipts(t)

	// Act.
	_, err := receipts.ClaimResumptionForDelivery("", 5_000)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no request id") {
		t.Fatalf("err = %v, want a refusal naming the missing request id", err)
	}
}

func TestAClaimStampsWhenTheDeliveryStarted(t *testing.T) {
	// Arrange — a `delivering` row standing since long ago is a re-drive
	// nobody ever confirmed, and the stamp is what makes that readable.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	got, err := receipts.UndischargedResumptions("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(got) != 1 || got[0].DeliveryStartedAtMs != 5_000 {
		t.Fatalf("undischarged = %+v, want the claim instant recorded", got)
	}
}

func TestAClaimedResumptionIsStillUndischarged(t *testing.T) {
	// Arrange — the preemption sweep must be able to reach it, or the row
	// nobody will re-drive would stand forever.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	got, err := receipts.UndischargedResumptions("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(got) != 1 || got[0].State != ResumptionDelivering {
		t.Fatalf("undischarged = %+v, want the claimed row reported as delivering", got)
	}
}

func TestAClaimedResumptionIsStillNeverServedAsAReceipt(t *testing.T) {
	// Arrange — the invisibility guarantee holds in the new state too: a
	// claimed re-drive is no more the user's prompt than an owed one.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	got, err := receipts.Outstanding("/ws")

	// Assert.
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("outstanding = %+v, want no renderable receipt for a claimed resumption", got)
	}
}

func TestAClaimedResumptionCanBeDischarged(t *testing.T) {
	// Arrange — the discharge point is the instruction reaching the
	// conversation, which happens after the claim.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	discharged, err := receipts.DischargeResumption("rd-1")

	// Assert.
	if err != nil {
		t.Fatalf("DischargeResumption: %v", err)
	}
	if !discharged {
		t.Fatal("a claimed resumption must be dischargeable on confirmed delivery")
	}
}

func TestDischargingAResumptionLeavesAnOrdinaryReceiptAlone(t *testing.T) {
	// Arrange — the two row kinds share a table, and a discharge that reached
	// a receipt would discard a prompt the user really typed.
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
		t.Fatal("discharging must never reach an ordinary prompt receipt")
	}
}

func TestARetriedTeardownCannotUnclaimAResumption(t *testing.T) {
	// Arrange — a teardown path that runs twice re-records the same row. If
	// that handed a claimed re-drive back as owed, the next wire would re-drive
	// it and the duplicate would be back.
	receipts, _ := openReceipts(t)
	claimed(t, receipts, pending(), 5_000)

	// Act.
	if err := receipts.RecordPendingResumption(pending()); err != nil {
		t.Fatalf("RecordPendingResumption (retried teardown): %v", err)
	}

	// Assert.
	got, err := receipts.PendingResumptions("/ws")
	if err != nil {
		t.Fatalf("PendingResumptions: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("pending = %+v, want a claimed resumption to stay claimed across a re-record", got)
	}
}

func TestAClaimSurvivesReopeningTheStore(t *testing.T) {
	// Arrange — the claim is worthless if a daemon bounce forgets it, which is
	// precisely the case the incident ran into.
	path := filepath.Join(t.TempDir(), "state.db")
	first, err := Open(path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	writer, err := NewPromptReceipts(first)
	if err != nil {
		t.Fatalf("NewPromptReceipts: %v", err)
	}
	claimed(t, writer, pending(), 5_000)
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
	if len(got) != 0 {
		t.Fatalf("pending = %+v, want the claim to outlive the process that took it", got)
	}
}
