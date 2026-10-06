package promptqueue

import (
	"context"
	"errors"
	"slices"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// heldOnReconnect reports a standing entry on the after-reconnect hold.
func heldOnReconnect(h wsm.HeldPrompt) bool {
	return h.Tombstone == nil && h.Hold != nil && *h.Hold == wsm.HoldReconnect
}

func TestASubmissionWhileHeldAndIdleIsItselfHeld(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")

	// Act
	got, err := h.q.Submit(context.Background(), submission("t2", "try now"))

	// Assert
	if err != nil || got.Delivered || got.Held == nil || *got.Held != wsm.HoldReconnect || got.Classification != nil {
		t.Fatalf("Submit = (%+v, %v), want held after reconnect, unclassified", got, err)
	}
}

func TestASubmissionWhileHeldAndIdleDeliversTheOldestHeldPrompt(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")

	// Act
	submitAll(t, h, "t3")

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want [t1]: the oldest held prompt is tried now", got)
	}
}

func TestTheNewPromptStaysHeldBehindAfterTheOldestIsDelivered(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")

	// Act
	submitAll(t, h, "t2")

	// Assert
	if got := standing(t, h, "t2"); !heldOnReconnect(got) {
		t.Fatalf("t2 = %+v, want held after reconnect behind the tried prompt", got)
	}
}

func TestWhenTheTryServesThePromptsBehindAreClassifiedAgainstIt(t *testing.T) {
	// Arrange: the vendor takes the tried prompt, which lifts the block (a new
	// turn opening, as the footer reads it).
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")
	h.sender.startHook = func() { h.footer.standVendorBlock("") }

	// Act
	submitAll(t, h, "t2")
	h.q.waitForClassifications()

	// Assert
	got := standing(t, h, "t2")
	if got.Hold != nil || got.Classification == nil || got.Classification.Arm != wsm.ArmHoldForTurnEnd {
		t.Fatalf("t2 = %+v, want released and classified against the tried turn", got)
	}
}

func TestAVendorRefusalOfTheTryKeepsEverythingHeldInOrder(t *testing.T) {
	tests := []struct {
		name    string
		refusal error
	}{
		{name: "the shim refuses the turn", refusal: errors.New("shim StartTurn refused: rate_limited")},
		{name: "the shim holds no session", refusal: errShimNoSession},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newVendorBlockHarness(t)
			heldUnderBlock(t, h, "usage_limit", "t1")
			h.sender.startErr = tc.refusal

			// Act
			submitAll(t, h, "t2")

			// Assert
			first, second := standing(t, h, "t1"), standing(t, h, "t2")
			if !heldOnReconnect(first) || !heldOnReconnect(second) || !first.QueuedAt.Before(second.QueuedAt) {
				t.Fatalf("t1 = %+v, t2 = %+v, want both held after reconnect, t1 first", first, second)
			}
		})
	}
}

func TestAVendorRefusalOfTheTryTakesItsMirroredRowDown(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")
	h.sender.startErr = errors.New("shim StartTurn refused: rate_limited")

	// Act
	submitAll(t, h, "t2")

	// Assert
	if got := h.feed.retiredPrompts(); !slices.Contains(got, "t1") {
		t.Fatalf("retired prompt rows = %v, want t1's mirrored row taken down", got)
	}
}

func TestASecondTryAfterARefusalTriesTheSameOldestPrompt(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")
	h.sender.startErr = errors.New("shim StartTurn refused: rate_limited")
	submitAll(t, h, "t2")
	h.sender.startErr = nil

	// Act
	submitAll(t, h, "t3")

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t1"}) {
		t.Fatalf("started = %v, want [t1]: the refused prompt kept its place", got)
	}
}

func TestATurnInFlightLeavesTheSubmissionOnItsOrdinaryPath(t *testing.T) {
	// Arrange: a retry holds the running turn and t1 waits behind it.
	h := newVendorBlockHarness(t)
	h.watcher.running("t0")
	h.footer.standVendorBlock("api_retrying")
	submitAll(t, h, "t1")

	// Act
	submitAll(t, h, "t2")

	// Assert
	if got := h.sender.startAttempts(); got != 0 || !heldAfterReconnectUnclassified(standing(t, h, "t2")) {
		t.Fatalf("StartTurn attempts = %d, t2 = %+v, want no try and t2 held unclassified", got, standing(t, h, "t2"))
	}
}

func TestWithNothingHeldTheSubmissionTakesItsOrdinaryPath(t *testing.T) {
	tests := []struct {
		name        string
		block       string
		wantStarted int
	}{
		{name: "the vendor serves: the prompt is delivered", wantStarted: 1},
		{name: "a usage limit stands: the prompt is held, nothing tried", block: "usage_limit", wantStarted: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newVendorBlockHarness(t)
			h.footer.standVendorBlock(tc.block)

			// Act
			submitAll(t, h, "t1")

			// Assert
			if got := h.sender.startAttempts(); got != tc.wantStarted {
				t.Fatalf("StartTurn attempts = %d, want %d", got, tc.wantStarted)
			}
		})
	}
}

func TestAnEditOfTheOldestHeldPromptWithholdsTheTry(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")
	if err := h.q.BeginEdit(context.Background(), theWorkspace, "t1", editorUp); err != nil {
		t.Fatalf("BeginEdit: %v", err)
	}

	// Act
	submitAll(t, h, "t2")

	// Assert
	if got := h.sender.startAttempts(); got != 0 || !heldOnReconnect(standing(t, h, "t1")) {
		t.Fatalf("StartTurn attempts = %d, t1 = %+v, want nothing tried while t1 is edited", got, standing(t, h, "t1"))
	}
}

func TestADroppedHeldPromptIsSkippedByTheTry(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1", "t2")
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}

	// Act
	submitAll(t, h, "t3")

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t2"}) {
		t.Fatalf("started = %v, want [t2]: the dropped t1 never goes", got)
	}
}

func TestAPromptHeldByAnotherConditionIsNotTriedNow(t *testing.T) {
	// Arrange: a drain hold whose lease has ended still waits on its own
	// condition; the submission takes its ordinary path.
	h := newVendorBlockHarness(t)
	heldByDrainLease(t, h, "t1")

	// Act
	submitAll(t, h, "t2")

	// Assert
	if got := h.sender.started(); !slices.Equal(got, []ids.TurnID{"t2"}) {
		t.Fatalf("started = %v, want [t2] delivered on its ordinary path", got)
	}
}

func TestTheTryNowIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newVendorBlockHarness(t)
	heldUnderBlock(t, h, "usage_limit", "t1")

	// Act
	submitAll(t, h, "t2")

	// Assert
	if !hasRecord(h, "info", opSubmit, "prompts are held and nothing runs; the submission is held behind them and the oldest is tried now") {
		t.Fatalf("no INFO record of the try-now decision")
	}
}
