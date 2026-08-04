package server

import (
	"testing"
	"time"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
)

// msAgo renders "d ago" against a fixed now.
func msAgo(now int64, d time.Duration) int64 { return now - int64(d/time.Millisecond) }

// CAUSE PRECEDENCE. One check can cross both thresholds at once, and the order
// is not arbitrary: the idle cutoff SUBSUMES everything past it. A session
// nobody has touched all day is not "a session whose cache went cold" — its
// real reason is the cutoff, and reporting the cache would replace a true
// account with a technically-also-true but less useful one.
func TestKeepAlivePolicyCausePrecedence(t *testing.T) {
	cfg := keepalive.DefaultConfig()
	const now = int64(10_000_000_000)

	tests := []struct {
		name       string
		idle       time.Duration
		wantAction keepalive.Action
		wantCause  string
	}{
		{
			name:       "inside the ping window, neither threshold is crossed",
			idle:       cfg.CacheTTL - cfg.Leeway,
			wantAction: keepalive.ActionPing,
		},
		{
			name:       "past the TTL but under the cutoff is cache_expired",
			idle:       cfg.CacheTTL + time.Minute,
			wantAction: keepalive.ActionHibernate,
			wantCause:  keepalive.CauseCacheExpired,
		},
		{
			name:       "just under the cutoff is still cache_expired",
			idle:       cfg.IdleCutoff - time.Minute,
			wantAction: keepalive.ActionHibernate,
			wantCause:  keepalive.CauseCacheExpired,
		},
		{
			name:       "exactly at the cutoff is idle_cutoff, not cache_expired",
			idle:       cfg.IdleCutoff,
			wantAction: keepalive.ActionHibernate,
			wantCause:  keepalive.CauseIdleCutoff,
		},
		{
			name:       "far past the cutoff is idle_cutoff",
			idle:       cfg.IdleCutoff + 24*time.Hour,
			wantAction: keepalive.ActionHibernate,
			wantCause:  keepalive.CauseIdleCutoff,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := cfg.Evaluate(now, msAgo(now, tc.idle))

			// Assert.
			if got.Action != tc.wantAction || got.Cause != tc.wantCause {
				t.Fatalf("Evaluate(idle=%s) = %s/%q, want %s/%q",
					tc.idle, got.Action, got.Cause, tc.wantAction, tc.wantCause)
			}
		})
	}
}

// A HIBERNATED SESSION IS CLAIMED AND LEFT ALONE. It has no live controller to
// ping and no second sleep to take; letting it fall through to the idle sweep
// would re-hibernate a sleeping session on every tick forever.
func TestKeepAlivePolicyClaimsAHibernatedSessionWithoutActing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rec := registry.Record{
		SessionID: "s1", CWD: "/ws", Hibernated: true,
		Hibernation:   registry.HibernationDetail{Cause: registry.HibernationCauseForced, SinceMs: 5},
		LastTurnEndMs: msAgo(h.srv.now().UnixMilli(), 48*time.Hour),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, h.srv.now().UnixMilli())

	// Assert.
	if !owned {
		t.Fatal("a hibernated session was not claimed by the policy; the idle sweep would re-hibernate it on every tick")
	}
}

// A session with NO recorded turn end is left entirely alone: every unknown
// answers none, the same rule the idle sweeper's own gates follow.
func TestKeepAlivePolicyLeavesAnUndatedSessionToTheIdleSweep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rec := registry.Record{SessionID: "s1", CWD: "/ws"}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, h.srv.now().UnixMilli())

	// Assert.
	if owned {
		t.Fatal("the policy claimed a session it has no durable instant for; an undated session is one it knows nothing about")
	}
}

// A session inside neither threshold is left to the ordinary idle sweep rather
// than claimed, so the two gates compose instead of one masking the other.
func TestKeepAlivePolicyLeavesAFreshSessionToTheIdleSweep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	now := h.srv.now().UnixMilli()
	rec := registry.Record{SessionID: "s1", CWD: "/ws", LastTurnEndMs: msAgo(now, time.Minute)}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if owned {
		t.Fatal("the policy claimed a freshly active session")
	}
}

// THE SWEEP INTERVAL MUST FIT INSIDE THE PING WINDOW. That window is one leeway
// wide, so a sweep slower than it steps straight over the only moment a ping is
// both due and useful — and every session falls through to cache_expired.
func TestSweepIntervalIsTightenedToThePingWindow(t *testing.T) {
	// Arrange: the shipped idle timeout's quarter is 15 minutes, far wider than
	// the 2-minute ping window.
	cfg := keepalive.DefaultConfig()
	idleDerived := time.Hour / 4

	// Act.
	got := cfg.SweepInterval(idleDerived)

	// Assert.
	if got > cfg.Leeway {
		t.Fatalf("sweep interval %s is wider than the %s ping window; a tick could step over it entirely", got, cfg.Leeway)
	}
}
