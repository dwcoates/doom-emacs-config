package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// RUNG 4: THE BOUND ON THE LADDER. A session that cannot come up is retried
// bringUpGiveUpAfter times and then parked. See bringupescape.go.
// ---------------------------------------------------------------------------

// deadClients is enough failing clients to exhaust the ladder: each resolved
// failure but the last consumes an initial bring-up AND its same-conversation
// retry, and the last one skips the retry because the bound is reached.
func deadClients(n int) []*fakeClient {
	out := make([]*fakeClient, 0, n)
	for i := 0; i < n; i++ {
		out = append(out, &fakeClient{awaitErr: errBringUpDead})
	}
	return out
}

// failBringUpsToTheBound drives ensure until the session has accumulated
// bringUpGiveUpAfter resolved failures, and returns the last error.
func failBringUpsToTheBound(t *testing.T, h *escapeHarness) error {
	t.Helper()
	var last error
	for i := 0; i < bringUpGiveUpAfter; i++ {
		_, last = h.m.ensure(context.Background(), "ws")
		if last == nil {
			t.Fatalf("ensure #%d succeeded; the harness must fail every bring-up", i+1)
		}
	}
	return last
}

// TestBringUpGivesUpAtTheBound: the respawn loop is bounded. Reaching the bound
// stops the ladder being climbed again rather than merely failing once more.
func TestBringUpGivesUpAtTheBound(t *testing.T) {
	// Arrange — every client dies before readiness.
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)

	// Act — exhaust the bound, then ask once more.
	failBringUpsToTheBound(t, h)
	spawnsAtBound := len(h.spawner.calls)
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert — the extra attempt is a policy refusal that spawned nothing.
	if !errors.Is(err, ErrBringUpGaveUp) {
		t.Fatalf("ensure past the bound = %v, want ErrBringUpGaveUp", err)
	}
	if len(h.spawner.calls) != spawnsAtBound {
		t.Fatalf("EnsureShim calls went %d -> %d past the give-up bound; the session must not be respawned",
			spawnsAtBound, len(h.spawner.calls))
	}
	if !h.log.contains("bring-up REFUSED by the give-up bound") {
		t.Fatal("the parked refusal was not logged")
	}
}

// TestGivingUpPublishesTheAccountOnTheStandingFailureCard: the park is
// SURFACED, through the machinery rung 3 already uses rather than a second one.
func TestGivingUpPublishesTheAccountOnTheStandingFailureCard(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)

	// Act
	failBringUpsToTheBound(t, h)

	// Assert — the last card carries the give-up account, and the ladder's own
	// error path is intact (a start-failed card was published at all).
	cards := h.failureCards()
	if len(cards) == 0 {
		t.Fatal("no failure card was published; the ladder's rung-3 publication was lost")
	}
	last := cards[len(cards)-1]
	if !strings.Contains(last.GetSourceDetail(), "no longer being respawned") {
		t.Fatalf("last failure card detail = %q, want it to name the give-up", last.GetSourceDetail())
	}
	if !h.log.contains("bring-up GIVING UP") {
		t.Fatal("the give-up was not logged")
	}
}

// TestBelowTheBoundTheLadderStillRetries: the bound must not swallow the rungs
// under it.
func TestBelowTheBoundTheLadderStillRetries(t *testing.T) {
	// Arrange — one dead connection, then a live one, which is rung 2.
	h := newEscapeHarness(t, &fakeClient{awaitErr: errBringUpDead}, &fakeClient{})

	// Act
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert
	if err != nil || d == nil {
		t.Fatalf("ensure = (%v, %v), want the same-conversation retry to succeed", d, err)
	}
}

// TestASuccessfulBringUpResetsTheStreak: the count is CONSECUTIVE failures, so
// an intermittent session never accumulates toward the bound.
func TestASuccessfulBringUpResetsTheStreak(t *testing.T) {
	// Arrange — two failures (one resolved), then a clean bring-up.
	h := newEscapeHarness(t, &fakeClient{awaitErr: errBringUpDead}, &fakeClient{awaitErr: errBringUpDead})
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the first ensure succeeded; the harness must fail it")
	}
	if got := h.m.bringUpFailuresFor("s1"); got != 1 {
		t.Fatalf("consecutive failures = %d, want 1", got)
	}

	// Act — the next bring-up wires.
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure after the failure = %v, want it to wire", err)
	}

	// Assert
	if got := h.m.bringUpFailuresFor("s1"); got != 0 {
		t.Fatalf("consecutive failures = %d after a bring-up that wired, want 0", got)
	}
}
