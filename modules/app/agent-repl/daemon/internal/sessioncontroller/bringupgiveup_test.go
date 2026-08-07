package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
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
	// The phrase moved from "no longer being respawned" to "not being respawned
	// YET" when the park became a cooldown: the old wording asserted a
	// permanence the park no longer has, and a card that tells a user their
	// workspace is finished when it will retry on its own is the wrong report.
	last := cards[len(cards)-1]
	if !strings.Contains(last.GetSourceDetail(), "not being respawned yet") {
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

// ---------------------------------------------------------------------------
// THE PARK IS A COOLDOWN. Reaching the bound must not dead-end the workspace:
// the park expires on its own, and the refusal it serves in the meantime says
// so. See bringupescape.go.
// ---------------------------------------------------------------------------

// TestTheParkRefusalNamesWhenAndHowItLifts: a refusal the user cannot act on is
// a dead workspace with extra steps, so the error carries both the wait and the
// deliberate way out.
func TestTheParkRefusalNamesWhenAndHowItLifts(t *testing.T) {
	// Arrange
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter)...)
	failBringUpsToTheBound(t, h)

	// Act
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert
	if err == nil {
		t.Fatal("ensure past the bound succeeded; the park must refuse it")
	}
	for _, want := range []string{"resting for another", "hard restart"} {
		if !strings.Contains(err.Error(), want) {
			t.Fatalf("park refusal %q omits %q; the refusal must say what un-sticks it", err, want)
		}
	}
}

// TestTheParkReleasesWhenItsCooldownExpires: THE DEAD-END. The park's only
// clearing edges used to be a bring-up that wired and a hibernation — and a
// parked session is refused before anything is spawned, so the wiring could
// never happen. Every open of the workspace was nacked for the daemon's life.
func TestTheParkReleasesWhenItsCooldownExpires(t *testing.T) {
	// Arrange — parked, and refusing.
	// Exactly enough dead clients to reach the bound and no more, so the
	// released bring-up below gets the harness's default client and wires.
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter-1)...)
	failBringUpsToTheBound(t, h)
	if _, err := h.m.ensure(context.Background(), "ws"); !errors.Is(err, ErrBringUpGaveUp) {
		t.Fatalf("ensure while parked = %v, want ErrBringUpGaveUp", err)
	}
	spawnsWhileParked := len(h.spawner.calls)

	// Act — the cooldown elapses and the workspace is opened again. The harness
	// has no scripted clients left, so this bring-up wires.
	h.advance(bringUpParkCooldown + time.Second)
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert
	if err != nil || d == nil {
		t.Fatalf("ensure after the cooldown = (%v, %v), want the park released", d, err)
	}
	if len(h.spawner.calls) == spawnsWhileParked {
		t.Fatal("the released bring-up spawned nothing; the park did not lift")
	}
	if !h.log.contains("bring-up park RELEASED") {
		t.Fatal("the release was not logged")
	}
}

// TestAReleasedParkReParksOnTheNextFailure: releasing must not restart the
// streak from zero, or a session that cannot start would buy a fresh run of
// bringUpGiveUpAfter timeouts every cooldown instead of one attempt.
func TestAReleasedParkReParksOnTheNextFailure(t *testing.T) {
	// Arrange — parked, then released by the cooldown.
	h := newEscapeHarness(t, deadClients(2*bringUpGiveUpAfter+1)...)
	failBringUpsToTheBound(t, h)
	h.advance(bringUpParkCooldown + time.Second)

	// Act — the one released attempt fails too.
	if _, err := h.m.ensure(context.Background(), "ws"); err == nil {
		t.Fatal("the released bring-up succeeded; the harness must fail it")
	}

	// Assert — parked again immediately, on a longer cooldown.
	_, err := h.m.ensure(context.Background(), "ws")
	if !errors.Is(err, ErrBringUpGaveUp) {
		t.Fatalf("ensure after the released attempt failed = %v, want it re-parked", err)
	}
	h.m.mu.Lock()
	cooldown := h.m.bringUpFailures["s1"].cooldown
	h.m.mu.Unlock()
	if cooldown != 2*bringUpParkCooldown {
		t.Fatalf("re-park cooldown = %s, want it doubled to %s", cooldown, 2*bringUpParkCooldown)
	}
}

// TestTheParkCooldownIsCapped: the doubling must not run away to a cooldown
// longer than a user would ever wait out.
func TestTheParkCooldownIsCapped(t *testing.T) {
	// Arrange
	tests := []struct {
		name  string
		parks int
		want  time.Duration
	}{
		{name: "first park", parks: 1, want: bringUpParkCooldown},
		{name: "second park doubles", parks: 2, want: 2 * bringUpParkCooldown},
		{name: "many parks stop at the cap", parks: 20, want: bringUpParkCooldownMax},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := &Manager{
				bringUpFailures: map[string]*bringUpStreak{"s1": {failures: bringUpGiveUpAfter - 1}},
				now:             func() int64 { return 0 },
				logf:            func(string, ...any) {},
			}

			// Act — one park per cycle: the failure that reaches the bound, then
			// the release that drops the count back under it.
			for park := 0; park < tc.parks; park++ {
				m.noteBringUpFailure("s1")
				m.bringUpFailures["s1"].failures = bringUpGiveUpAfter - 1
			}

			// Assert
			if got := m.bringUpFailures["s1"].cooldown; got != tc.want {
				t.Fatalf("cooldown after %d parks = %s, want %s", tc.parks, got, tc.want)
			}
		})
	}
}
