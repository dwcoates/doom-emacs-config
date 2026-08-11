package sessioncontroller

import (
	"context"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// cardDetails renders the details of every published failure card, for the
// failure message of a test that could not find the one it wanted.
func cardDetails(cards []*frontendv1.FailureCardView) []string {
	out := make([]string, 0, len(cards))
	for _, c := range cards {
		out = append(out, c.GetDetail())
	}
	return out
}

// ---------------------------------------------------------------------------
// A FAILED BRING-UP WITH RETRIES REMAINING IS ACTUALLY RETRIED. See
// bringupretry.go: the budget in bringupescape.go had no consumer, so a
// workspace nobody opened sat unwired with given_up=false forever.
// ---------------------------------------------------------------------------

// retryWatch reads the workspace's owed-retry watch, or nil.
func (h *escapeHarness) retryWatch(workspace string) bringUpRetryWatch {
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	w := h.m.bringUpRetries[workspace]
	if w == nil {
		return bringUpRetryWatch{}
	}
	return *w
}

// gaveUpCards counts the terminal budget-exhausted cards published so far, by
// the stable identity bringUpGaveUpUUID mints.
func (h *escapeHarness) gaveUpCards(sessionID string) int {
	n := 0
	for _, item := range h.failureCardItems() {
		if item.GetUuid() == bringUpGaveUpUUID(sessionID) {
			n++
		}
	}
	return n
}

// TestFailedBringUpIsRetriedAfterBackoff: the defect itself. One resolved
// failure with attempts remaining arms a watch, the sweep declines it until the
// backoff has elapsed, and then launches an attempt with nobody having opened
// the workspace.
func TestFailedBringUpIsRetriedAfterBackoff(t *testing.T) {
	// Arrange — one climb of the ladder that resolves in failure.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}

	// Act/Assert — nothing is due yet.
	if got := h.retryWatch("ws").failures; got != 1 {
		t.Fatalf("armed watch failures = %d, want 1", got)
	}
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts before the backoff elapsed, want 0", n)
	}

	// Act — the backoff elapses.
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Assert — the sweep spends the budget on its own.
	if n := h.m.SweepFailedBringUps(); n != 1 {
		t.Fatalf("sweep launched %d attempts once due, want 1 — a workspace with retries remaining must not sit unwired", n)
	}
	if !h.log.contains("bring-up retry ARMED") {
		t.Fatal("the owed retry was never announced")
	}
}

// TestRetrySweepDoesNotStackAttempts: the sweep is idempotent. A second tick
// arriving while an attempt is still climbing must select nothing.
func TestRetrySweepDoesNotStackAttempts(t *testing.T) {
	// Arrange — a due watch whose attempt is in flight.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Act
	first := h.m.SweepFailedBringUps()
	second := h.m.SweepFailedBringUps()

	// Assert
	if first != 1 || second != 0 {
		t.Fatalf("sweeps launched %d then %d attempts, want 1 then 0 — the in-flight latch must stop retries stacking", first, second)
	}
	if got := h.retryWatch("ws").attempts; got != 1 {
		t.Fatalf("watch attempts = %d, want 1", got)
	}
}

// TestSuccessfulRetryClearsTheFailureCount: a retry that wires retires both the
// streak and the owed retry, so an intermittent workspace never accumulates
// toward the give-up bound.
func TestSuccessfulRetryClearsTheFailureCount(t *testing.T) {
	// Arrange — one failed climb, then a client that handshakes.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)

	// Act — the attempt itself, run inline so the assertion needs no timer.
	h.attemptBringUpRetrySynchronously(t, "ws")

	// Assert
	if got := h.m.bringUpFailuresFor("s1"); got != 0 {
		t.Fatalf("consecutive failures after a wired retry = %d, want 0", got)
	}
	if got := h.retryWatch("ws"); got.sessionID != "" {
		t.Fatalf("the owed-retry watch survived a wired retry: %+v", got)
	}
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts against a wired workspace, want 0", n)
	}
}

// attemptBringUpRetrySynchronously runs one automatic attempt on the calling
// goroutine, which is what SweepFailedBringUps launches. Calling it directly
// keeps the assertion above deterministic without waiting on a goroutine.
func (h *escapeHarness) attemptBringUpRetrySynchronously(t *testing.T, workspace string) {
	t.Helper()
	h.mu.Lock()
	h.clients = append(h.clients, &fakeClient{})
	h.mu.Unlock()
	h.m.attemptBringUpRetry(workspace)
}

// TestExhaustingTheBudgetCardsTheTerminalStateOnce: the budget cannot be spent
// silently. Reaching the bound publishes ONE typed terminal card naming the
// park and the way out, and a further refused attempt does not draw a second.
func TestExhaustingTheBudgetCardsTheTerminalStateOnce(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)

	// Act — exhaust the bound, then ask once more (refused by the park).
	failBringUpsToTheBound(t, h)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure past the bound succeeded; it must be refused by the park")
	}

	// Assert
	if got := h.gaveUpCards("s1"); got != 1 {
		t.Fatalf("terminal budget-exhausted cards = %d, want exactly 1", got)
	}
	if !h.warn.contains("bring-up budget EXHAUSTED") {
		t.Fatal("the exhausted budget was not reported loudly")
	}
	if !h.retryWatch("ws").givenUp {
		t.Fatal("the watch does not record the exhausted budget, so the park's own cooldown is not what gates the next attempt")
	}
}

// TestExhaustedBudgetIsStillRetriedWhenTheParkExpires: terminal is LOUD, not
// forever. The park's cooldown gates the next automatic attempt rather than
// cancelling it.
func TestExhaustedBudgetIsStillRetriedWhenTheParkExpires(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)
	failBringUpsToTheBound(t, h)

	// Act/Assert — inside the park, nothing is launched.
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts inside the park, want 0", n)
	}
	h.advance(bringUpParkCooldown + time.Second)
	if n := h.m.SweepFailedBringUps(); n != 1 {
		t.Fatalf("sweep launched %d attempts after the park expired, want 1 — an exhausted budget must not dead-end the workspace", n)
	}
}

// TestRetryWatchIsDroppedForAWorkspaceThatCameUpAnyway: a user who opens the
// workspace answers the owed retry, and the sweep must not re-ensure behind
// them.
func TestRetryWatchIsDroppedForAWorkspaceThatCameUpAnyway(t *testing.T) {
	// Arrange — a due watch, then a bring-up that wires.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}
	h.advance(bringUpRetryBaseDelay + time.Second)
	h.mu.Lock()
	h.clients = append(h.clients, &fakeClient{})
	h.mu.Unlock()
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("the user's own open failed: %v", err)
	}

	// Act/Assert
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts against a live workspace, want 0", n)
	}
}

// TestBringUpRetryDelayBacksOff: the cadence widens with the streak and is
// capped, so a workspace that cannot start does not re-climb in a tight loop.
func TestBringUpRetryDelayBacksOff(t *testing.T) {
	tests := []struct {
		name     string
		failures int
		want     time.Duration
	}{
		{name: "first failure", failures: 1, want: bringUpRetryBaseDelay},
		{name: "second failure doubles", failures: 2, want: 2 * bringUpRetryBaseDelay},
		{name: "a long streak is capped", failures: 20, want: bringUpRetryMaxDelay},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := bringUpRetryDelay(tc.failures); got != tc.want {
				t.Fatalf("bringUpRetryDelay(%d) = %s, want %s", tc.failures, got, tc.want)
			}
		})
	}
}

// TestHibernationDropsTheOwedRetry: a session put to sleep on purpose must not
// be woken by the retry sweep.
func TestHibernationDropsTheOwedRetry(t *testing.T) {
	// Arrange — an owed retry standing over a failed bring-up.
	h := newEscapeHarness(t, deadClients(2)...)
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("ensure succeeded; the harness must fail the bring-up")
	}

	// Act — the deliberate stand-down edge.
	h.m.clearBringUpRetry("ws", "hibernated")

	// Assert
	h.advance(bringUpRetryMaxDelay)
	if n := h.m.SweepFailedBringUps(); n != 0 {
		t.Fatalf("sweep launched %d attempts for a hibernated workspace, want 0", n)
	}
}
