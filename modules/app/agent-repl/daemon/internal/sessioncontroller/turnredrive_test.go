package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/frontend"
	"claude-repld/internal/statedb"
)

// THE RE-DRIVE, AND WHY IT CANNOT HAPPEN TWICE.
//
// Exactly-once here is not a flag anybody has to remember to clear: the store
// is asked what is OWED at every wire, and the row is CLAIMED before the submit
// that discharges it. These tests pin that across the cases an edge-triggered
// design breaks on — a second bounce landing mid resumption, a submit the shim
// refuses, and two wire events for one session.
//
// The claim is what a live incident forced. A submit whose control request
// times out may well have been run by the shim anyway, so re-driving on a
// reported failure is choosing to duplicate the daemon's instruction in the
// user's conversation, which is exactly what happened.

// reDriveRequest is a re-drive's own request id, in the shape the teardown
// mints — the prefix is load-bearing, because the marker the submit carries is
// only valid for an id that has it.
var reDriveRequest = resumptionRequestID("ws", "t-1", 2_000)

// owedFor is the resumption the re-drive tests start from.
func owedFor(requestID string) statedb.PendingResumption {
	return statedb.PendingResumption{
		RequestID:       requestID,
		Workspace:       "ws",
		TurnID:          "t-1",
		Text:            resumptionInstruction,
		InterruptedAtMs: 2_000,
	}
}

// holdMergeLease makes the harness's fake ledger refuse submits, which is how
// these tests produce a REFUSED re-drive without breaking the transport.
func holdMergeLease(h *submitHarness, held bool) {
	applier := h.m.cfg.SSM.(*fakeApplier)
	if applier.mergeLeases == nil {
		applier.mergeLeases = map[string]bool{}
	}
	applier.mergeLeases["ws"] = held
}

// seedOwed puts one owed resumption in front of a submit harness.
func seedOwed(t *testing.T, h *submitHarness, requestID string) {
	t.Helper()
	if err := h.receipts.RecordPendingResumption(owedFor(requestID)); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
}

func TestAWiredSessionReDrivesTheTurnItOwes(t *testing.T) {
	// Arrange.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if got := h.lastClient().promptTexts(); len(got) != 1 {
		t.Fatalf("prompts = %v, want the owed turn re-driven once", got)
	}
}

func TestTheReDrivenPromptCarriesItsOwnDurableMarker(t *testing.T) {
	// Arrange — the marker is what survives into the transcript, and it is the
	// only thing that makes the instruction suppressible on a replay.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	got := h.lastClient().promptTexts()
	if len(got) != 1 || frontend.MarkedInternalResumeRequestID(got[0]) != reDriveRequest {
		t.Fatalf("prompts = %v, want the submitted text marked with %q", got, reDriveRequest)
	}
}

func TestTheReDrivenPromptStillCarriesTheInstruction(t *testing.T) {
	// Arrange — the marker must not displace what the model is being told, or
	// the re-drive says nothing.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	got := h.lastClient().promptTexts()
	if len(got) != 1 || !strings.Contains(got[0], resumptionInstruction) {
		t.Fatalf("prompts = %v, want the instruction itself submitted", got)
	}
}

func TestAClaimThatCannotBeRecordedIssuesNoReDrive(t *testing.T) {
	// Arrange — the claim GATES the submit rather than following it. A submit
	// issued past an unrecordable claim could be issued again by the next wire,
	// which is the duplicate the claim exists to prevent.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	h.receipts.fakeReceiptStore.resumptionClaimErr = errors.New("state store is unwritable")

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if got := h.lastClient(); got != nil && len(got.promptTexts()) != 0 {
		t.Fatalf("prompts = %v, want no re-drive when the claim could not be recorded", got.promptTexts())
	}
}

func TestAResumptionAnotherWireClaimedIsNotReDrivenAgain(t *testing.T) {
	// Arrange — two wires of one session racing for the same owed turn.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	took, err := h.receipts.ClaimResumptionForDelivery(reDriveRequest, 3_000)
	if err != nil || !took {
		t.Fatalf("ClaimResumptionForDelivery: took=%v err=%v", took, err)
	}

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if got := h.lastClient(); got != nil && len(got.promptTexts()) != 0 {
		t.Fatalf("prompts = %v, want nothing submitted for a turn another wire is delivering", got.promptTexts())
	}
}

func TestADeliveredInstructionDischargesTheResumption(t *testing.T) {
	// Arrange — the instruction turning up in the vendor conversation is the
	// ONLY confirmation that the re-drive landed, so it is what clears the row.
	receipts := newFakeReceiptStore()
	if err := receipts.RecordPendingResumption(owedFor(reDriveRequest)); err != nil {
		t.Fatalf("RecordPendingResumption: %v", err)
	}
	if _, err := receipts.ClaimResumptionForDelivery(reDriveRequest, 3_000); err != nil {
		t.Fatalf("ClaimResumptionForDelivery: %v", err)
	}
	cons := receiptConsumer(t, receipts)

	// Act.
	cons.dischargeDeliveredResumptions([]string{reDriveRequest})

	// Assert.
	owed, err := receipts.UndischargedResumptions("ws")
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(owed) != 0 {
		t.Fatalf("undischarged = %+v, want the delivered re-drive's record cleared", owed)
	}
}

func TestAnUndeliveredReDriveKeepsItsRecord(t *testing.T) {
	// Arrange — the counterpart: a re-drive nothing has confirmed is not
	// cleared, so the evidence of the owed turn survives the submit.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	owed, err := h.receipts.UndischargedResumptions("ws")
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(owed) != 1 {
		t.Fatalf("undischarged = %+v, want the record to stand until the instruction is seen", owed)
	}
}

func TestDischargingADeliveryTheStoreRefusesIsLoud(t *testing.T) {
	// Arrange — the instruction is already suppressed, so nothing the user sees
	// is wrong; what stands is a stale row, and a stale row nobody was told
	// about is how the next re-drive surprises everybody.
	receipts := newFakeReceiptStore()
	receipts.resumptionDischargeErr = errors.New("state store is unwritable")
	logged := &logCapture{}
	cons := receiptConsumer(t, receipts)
	cons.logf = logged.logf

	// Act.
	cons.dischargeDeliveredResumptions([]string{reDriveRequest})

	// Assert.
	if !logged.contains("turn resumption DISCHARGE FAILED") {
		t.Fatalf("missing the canonical discharge-failure record; log:\n%s", strings.Join(logged.lines, "\n"))
	}
}

func TestTheReDriveLeavesNoDurableReceiptBehind(t *testing.T) {
	// Arrange — a receipt exists to replay the user's own bubble. Recording one
	// for the daemon's instruction would resurrect it from durable storage,
	// where nothing downstream could tell it from a real prompt.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	rows, err := h.receipts.Outstanding("ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(rows) != 0 {
		t.Fatalf("receipts = %+v, want the re-drive to leave none", rows)
	}
}

func TestTheReDrivePushesNoReceiptBubble(t *testing.T) {
	// Arrange — the visible half of the same guarantee: the user must not see a
	// prompt they did not write.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	for _, entry := range h.traced() {
		if strings.HasPrefix(entry, "push:") {
			t.Fatalf("trace = %v, want no pushed bubble for the re-drive", h.traced())
		}
	}
}

func TestASecondWireFindsNothingOwed(t *testing.T) {
	// Arrange — two wire events for one session (a reattach followed by a
	// respawn) must not run the turn twice.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	h.m.driveOwedResumptions("ws", "s1")

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert — one submit, from the first wire only.
	if got := h.lastClient().promptTexts(); len(got) != 1 {
		t.Fatalf("prompts = %v, want exactly one re-drive across two wires", got)
	}
}

func TestAReDriveWhoseSubmitFailedIsNotIssuedAgain(t *testing.T) {
	// Arrange — a submit reported as failed may still have been run by the
	// shim, so re-issuing it is choosing the duplicate. This is the incident's
	// own shape: the control request timed out and the prompt landed anyway.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	holdMergeLease(h, true)
	h.m.driveOwedResumptions("ws", "s1")

	// Act — the next wire, with the obstruction gone.
	holdMergeLease(h, false)
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if got := h.lastClient(); got != nil && len(got.promptTexts()) != 0 {
		t.Fatalf("prompts = %v, want a claimed re-drive never issued a second time", got.promptTexts())
	}
}

func TestAReDriveWhoseSubmitFailedSaysSoLoudly(t *testing.T) {
	// Arrange — the turn is now lost work rather than owed work, which is
	// exactly the outcome this whole path exists to prevent, so it is never
	// silent.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	holdMergeLease(h, true)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if !h.log.contains("turn resumption SUBMIT FAILED") {
		t.Fatalf("missing the canonical submit-failure record; log:\n%s", strings.Join(h.log.lines, "\n"))
	}
}

func TestAReDriveWhoseSubmitFailedKeepsItsRecord(t *testing.T) {
	// Arrange — the row is the durable evidence that this turn was owed, and a
	// failed submit is not a reason to discard it.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	holdMergeLease(h, true)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	owed, err := h.receipts.UndischargedResumptions("ws")
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(owed) != 1 {
		t.Fatalf("undischarged = %+v, want the record to stand after a refused re-drive", owed)
	}
}

func TestABounceDuringResumptionAccumulatesOneRowRatherThanTwo(t *testing.T) {
	// Arrange — the double-bounce case. A second bounce records the SAME
	// interrupted turn while the first re-drive is still being delivered, and
	// the workspace must carry one owed thing rather than two.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	h.m.driveOwedResumptions("ws", "s1")

	// Act — the second bounce re-records the same turn, colliding on the id.
	seedOwed(t, h, reDriveRequest)

	// Assert.
	owed, err := h.receipts.UndischargedResumptions("ws")
	if err != nil {
		t.Fatalf("UndischargedResumptions: %v", err)
	}
	if len(owed) != 1 {
		t.Fatalf("undischarged = %+v, want one row rather than two accumulated", owed)
	}
}

func TestABounceDuringResumptionDoesNotHandTheTurnBackAsOwed(t *testing.T) {
	// Arrange — the sharper half: a re-record that reset the claim would let
	// the next wire re-drive a turn already being delivered, which is the
	// duplicate the whole fence exists to prevent.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)
	h.m.driveOwedResumptions("ws", "s1")
	seedOwed(t, h, reDriveRequest)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if got := h.lastClient().promptTexts(); len(got) != 1 {
		t.Fatalf("prompts = %v, want the re-record to leave the claim standing", got)
	}
}

func TestAnUnreadableStoreIssuesNoReDriveAndSaysSo(t *testing.T) {
	// Arrange — an unreadable store must never read as "nothing is owed",
	// which would abandon the turn silently.
	h := newSubmitHarness(t)
	h.receipts.fakeReceiptStore.resumptionsErr = errors.New("state store is unreadable")

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert — no submit was attempted.
	for _, entry := range h.receipts.callLog() {
		if strings.HasPrefix(entry, "claim-resumption:") {
			t.Fatalf("calls = %v, want no re-drive against an unreadable store", h.receipts.callLog())
		}
	}
}

func TestAUserPromptCancelsAnOwedResumption(t *testing.T) {
	// Arrange — someone submitting after a bounce did not ask for the old turn
	// to resume, and re-driving it behind them would start work they had
	// implicitly abandoned.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "something else", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 0 {
		t.Fatalf("owed = %+v, want the user's own prompt to cancel the resumption", owed)
	}
}

func TestAUserPromptCancelsBeforeTheReDriveCouldSlipIn(t *testing.T) {
	// Arrange — the cancellation runs ahead of the submit, so a wire landing
	// alongside cannot start the turn the user has moved on from.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "something else", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	h.m.driveOwedResumptions("ws", "s1")

	// Assert — the only submit recorded is the user's own.
	rows, err := h.receipts.Outstanding("ws")
	if err != nil {
		t.Fatalf("Outstanding: %v", err)
	}
	if len(rows) != 1 || rows[0].RequestID != "r-1" {
		t.Fatalf("receipts = %+v, want only the user's own prompt", rows)
	}
}

func TestTheCancellationIsRecordedRatherThanSilent(t *testing.T) {
	// Arrange — a silent drop is the failure this whole feature exists to end,
	// so the abandonment is stated against the turn it abandons.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	if err := h.m.SubmitPrompt(context.Background(), "ws", "r-1", "something else", "default", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if !h.log.contains("turn resumption CANCELLED") || !h.log.contains("cause=user_prompt") {
		t.Fatalf("missing the canonical cancellation record; log:\n%s", strings.Join(h.log.lines, "\n"))
	}
}

func TestAnInterruptCancelsAnOwedResumption(t *testing.T) {
	// Arrange — someone pressing stop is telling the session to stop working,
	// which an owed re-drive would immediately contradict.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act — the interrupt's own outcome is irrelevant here; the cancellation
	// runs ahead of the stop precisely so it does not depend on it.
	_ = h.m.Interrupt(context.Background(), "ws", "r-stop")

	// Assert.
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 0 {
		t.Fatalf("owed = %+v, want the user's stop to cancel the resumption", owed)
	}
}

func TestTheDaemonsOwnKeepAliveDoesNotPreempt(t *testing.T) {
	// Arrange — only the USER moving on preempts. The daemon's own producers
	// are not somebody abandoning the work.
	h := newSubmitHarness(t)
	seedOwed(t, h, reDriveRequest)

	// Act.
	_, err := h.m.submitPromptAs(context.Background(), "ws", "ka-1", "respond with only '.'", "",
		"keep-alive", testPromptOrigin, submitterKeepAlive, leavesParkedPermissions)

	// Assert.
	if err != nil {
		t.Fatalf("keep-alive submit: %v", err)
	}
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 1 {
		t.Fatalf("owed = %+v, want the daemon's own ping to leave the resumption alone", owed)
	}
}
