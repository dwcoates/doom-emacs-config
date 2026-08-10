package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/statedb"
)

// THE RE-DRIVE, AND WHY IT CANNOT HAPPEN TWICE.
//
// Exactly-once here is not a flag anybody has to remember to clear: the store
// is asked what is OWED at every wire, and a row is only discharged when the
// re-drive it names is accepted. These tests pin that level-triggering across
// the cases that break an edge-triggered design — a second bounce landing mid
// resumption, a submit the shim refuses, and two wire events for one session.

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
	seedOwed(t, h, "rd-1")

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 0 {
		t.Fatalf("owed = %+v, want the resumption discharged by an accepted re-drive", owed)
	}
}

func TestTheReDriveLeavesNoDurableReceiptBehind(t *testing.T) {
	// Arrange — a receipt exists to replay the user's own bubble. Recording one
	// for the daemon's instruction would resurrect it from durable storage,
	// where nothing downstream could tell it from a real prompt.
	h := newSubmitHarness(t)
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")
	h.m.driveOwedResumptions("ws", "s1")

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert — one submit, from the first wire only.
	submits := 0
	for _, entry := range h.receipts.callLog() {
		if strings.HasPrefix(entry, "discharge-resumption:") {
			submits++
		}
	}
	if submits != 1 {
		t.Fatalf("discharges = %d, want exactly one across two wires", submits)
	}
}

func TestARefusedReDriveLeavesTheRecordStanding(t *testing.T) {
	// Arrange — the discharge is on ACCEPTANCE, not on issue. Clearing the row
	// for a submit that failed would lose the work in exactly the case this
	// path exists for: a successor daemon that is itself unhealthy.
	h := newSubmitHarness(t)
	seedOwed(t, h, "rd-1")
	h.lastClient() // force the client to exist before it is broken
	holdMergeLease(h, true)

	// Act.
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 1 {
		t.Fatalf("owed = %+v, want the record to stand after a refused re-drive", owed)
	}
}

func TestABounceDuringResumptionReDrivesTheSameRowRatherThanASecond(t *testing.T) {
	// Arrange — the double-bounce case. The first wire's re-drive is refused
	// (so its row stands), a second bounce records the SAME interrupted turn,
	// and the next wire must find one owed thing, not two.
	h := newSubmitHarness(t)
	seedOwed(t, h, "rd-1")
	holdMergeLease(h, true)
	h.m.driveOwedResumptions("ws", "s1")
	// The second bounce re-records the same turn, which collides on the id.
	seedOwed(t, h, "rd-1")

	// Act.
	holdMergeLease(h, false)
	h.m.driveOwedResumptions("ws", "s1")

	// Assert.
	if owed := h.receipts.owedResumptions("ws"); len(owed) != 0 {
		t.Fatalf("owed = %+v, want one row discharged rather than two accumulated", owed)
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
		if strings.HasPrefix(entry, "discharge-resumption:") {
			t.Fatalf("calls = %v, want no re-drive against an unreadable store", h.receipts.callLog())
		}
	}
}

func TestAUserPromptCancelsAnOwedResumption(t *testing.T) {
	// Arrange — someone submitting after a bounce did not ask for the old turn
	// to resume, and re-driving it behind them would start work they had
	// implicitly abandoned.
	h := newSubmitHarness(t)
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")

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
	seedOwed(t, h, "rd-1")

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
