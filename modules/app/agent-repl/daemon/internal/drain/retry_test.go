package drain

import (
	"context"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
)

func TestHibernateRetryDelay(t *testing.T) {
	cases := []struct {
		name     string
		every    time.Duration
		failures int
		want     time.Duration
	}{
		{name: "the first failure skips exactly one pass", every: 5 * time.Minute, failures: 1, want: 10 * time.Minute},
		{name: "each further failure doubles the wait", every: 5 * time.Minute, failures: 3, want: 40 * time.Minute},
		{name: "the wait never passes the ceiling", every: 5 * time.Minute, failures: 4, want: HibernateRetryCeiling},
		{name: "a short cadence still grows from its own value", every: 50 * time.Millisecond, failures: 1, want: 100 * time.Millisecond},
		{name: "a long run of failures cannot overflow past the ceiling", every: 50 * time.Millisecond, failures: 200, want: HibernateRetryCeiling},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act
			got := hibernateRetryDelay(tc.every, tc.failures)

			// Assert
			if got != tc.want {
				t.Fatalf("hibernateRetryDelay(%s, %d) = %s, want %s", tc.every, tc.failures, got, tc.want)
			}
		})
	}
}

// refusalAnswer is a typed Hibernate refusal of the given arm.
func refusalAnswer(err *shimv1.HibernateError) *shimv1.HibernateResponse {
	return &shimv1.HibernateResponse{Result: &shimv1.HibernateResponse_Error{Error: err}}
}

// TestTheSweepBacksOffAWorkspaceWhoseHibernationKeepsFailing drives passes at
// fixed offsets from the harness's instant, one SweepEvery (5m) apart and
// beyond, and counts the Hibernate directives the failing workspace received.
//
// A failing workspace is asked at +0, then not again until +10m (one pass
// skipped), then not until +30m (20m later): three directives across the
// seven passes. An ordinary deferral is asked on every pass.
func TestTheSweepBacksOffAWorkspaceWhoseHibernationKeepsFailing(t *testing.T) {
	passes := []time.Duration{0, 5 * time.Minute, 10 * time.Minute, 15 * time.Minute, 25 * time.Minute, 30 * time.Minute, 50 * time.Minute}
	cases := []struct {
		name      string
		arrange   func(h *harness, ws ids.WorkspaceID)
		wantAsked int
	}{
		{
			name:      "a failed directive backs off",
			arrange:   func(h *harness, ws ids.WorkspaceID) { h.stand.hibernateErr[ws] = errFake },
			wantAsked: 3,
		},
		{
			name: "a warned refusal backs off",
			arrange: func(h *harness, ws ids.WorkspaceID) {
				h.stand.answer[ws] = refusalAnswer(&shimv1.HibernateError{
					Kind: &shimv1.HibernateError_NoSession{NoSession: &shimv1.HibernateNoSession{}},
				})
			},
			wantAsked: 3,
		},
		{
			name:      "a failed stand-down backs off",
			arrange:   func(h *harness, ws ids.WorkspaceID) { h.stand.failKill(ws, errFake) },
			wantAsked: 3,
		},
		{
			name: "a turn in flight is an ordinary deferral and is asked every pass",
			arrange: func(h *harness, ws ids.WorkspaceID) {
				h.stand.answer[ws] = refusalAnswer(&shimv1.HibernateError{
					Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}},
				})
			},
			wantAsked: len(passes),
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.workspace(t, instant.Add(-2*time.Hour))
			tc.arrange(h, ws)

			// Act
			for _, offset := range passes {
				if _, err := h.c.Sweep(context.Background(), instant.Add(offset)); err != nil {
					t.Fatalf("Sweep at +%s: %v", offset, err)
				}
			}

			// Assert
			if got := len(h.stand.hibernated); got != tc.wantAsked {
				t.Fatalf("Hibernate directives across %d passes = %d, want %d", len(passes), got, tc.wantAsked)
			}
		})
	}
}

// TestAnOrdinaryDeferralEndsTheBackoff pins that the backoff is for a
// workspace that keeps FAILING: once the shim answers ordinarily, the next
// pass asks again at the sweep's own cadence.
func TestAnOrdinaryDeferralEndsTheBackoff(t *testing.T) {
	// Arrange: a failure at +0, then a turn-in-flight answer at +10m, the
	// first instant the backoff allows.
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = errFake
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep at +0: %v", err)
	}
	delete(h.stand.hibernateErr, ws)
	h.stand.answer[ws] = refusalAnswer(&shimv1.HibernateError{
		Kind: &shimv1.HibernateError_TurnInFlight{TurnInFlight: &shimv1.HibernateTurnInFlight{}},
	})
	if _, err := h.c.Sweep(context.Background(), instant.Add(10*time.Minute)); err != nil {
		t.Fatalf("Sweep at +10m: %v", err)
	}

	// Act: the very next pass.
	if _, err := h.c.Sweep(context.Background(), instant.Add(15*time.Minute)); err != nil {
		t.Fatalf("Sweep at +15m: %v", err)
	}

	// Assert
	if got := len(h.stand.hibernated); got != 3 {
		t.Fatalf("Hibernate directives = %d, want 3: the pass after an ordinary deferral is not backed off", got)
	}
}

// TestABackedOffPassIsRecordedAtDebug pins that the skipped pass is visible in
// the log without being a second ERROR for the one failure.
func TestABackedOffPassIsRecordedAtDebug(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := h.workspace(t, instant.Add(-2*time.Hour))
	h.stand.hibernateErr[ws] = errFake
	if _, err := h.c.Sweep(context.Background(), instant); err != nil {
		t.Fatalf("Sweep at +0: %v", err)
	}

	// Act
	if _, err := h.c.Sweep(context.Background(), instant.Add(5*time.Minute)); err != nil {
		t.Fatalf("Sweep at +5m: %v", err)
	}

	// Assert
	errors, skipped := 0, 0
	for _, r := range records(h.log, opSweep) {
		if r.Level == "error" {
			errors++
		}
		if r.Level == "debug" && r.Message == "the workspace's last hibernation failed; backing off before the next attempt" {
			skipped++
		}
	}
	if errors != 1 || skipped != 1 {
		t.Fatalf("sweep records: %d errors and %d backed-off debug records, want 1 and 1", errors, skipped)
	}
}
