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
			name:       "past the TTL but under the cutoff lets the cache cool",
			idle:       cfg.CacheTTL + time.Minute,
			wantAction: keepalive.ActionLetCacheCool,
		},
		{
			name:       "just under the cutoff still only lets the cache cool",
			idle:       cfg.IdleCutoff - time.Minute,
			wantAction: keepalive.ActionLetCacheCool,
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
			got := cfg.Evaluate(now, msAgo(now, tc.idle), msAgo(now, tc.idle))

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

// THE RETRY FLOOR CLAIMS THE SESSION WITHOUT SUBMITTING. The floor arm carries
// no submit at all, so the sweeper structurally cannot ping inside it; what the
// sweeper still must do is OWN the tick, or the session would fall through to
// the legacy idle sweep and be torn down for a reason the policy already
// answered.
func TestKeepAlivePolicyOwnsTheRetryFloorWithoutPinging(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.CacheTTL-keepalive.RetryFloor),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if !owned {
		t.Fatal("a session inside the retry floor was not claimed by the policy; it would fall through to the legacy idle sweep")
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

// THE WARM-COMPACTION ARM OWNS ITS TICK. The sweep must claim the session at
// the compaction instant for the same reason it claims the retry floor: a
// session falling through here would be torn down by the legacy idle sweep for
// a reason the policy has already answered.
func TestKeepAlivePolicyOwnsTheWarmCompactionInstant(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.WarmCompactAt()),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if !owned {
		t.Fatal("a session at the warm-compaction instant was not claimed by the policy; it would fall through to the legacy idle sweep")
	}
}

// A SESSION SHORT OF THE COMPACTION INSTANT IS LEFT TO THE ORDINARY IDLE
// SWEEP. The span is bounded below as well as above: a compaction taken early
// is one submitted with more cache life left than the policy claims to be
// trading on, and claiming the tick there would mask the idle sweep for a
// session the policy has said nothing about.
func TestKeepAlivePolicyLeavesASessionShortOfTheCompactionInstantAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.WarmCompactAt()-time.Minute),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if owned {
		t.Fatal("the policy claimed a session a minute short of the warm-compaction instant")
	}
}

// THE COLD-CACHE ARM OWNS ITS TICK, and this is the load-bearing half of the
// fix. If the session fell through here, the generic idle sweep — whose default
// cutoff is an HOUR — would hibernate it immediately, which is the very early
// sleep the arm exists to prevent.
func TestKeepAlivePolicyOwnsAColdCacheWithoutHibernating(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.CacheTTL+time.Minute),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if !owned {
		t.Fatal("a session whose cache went cold was not claimed by the policy; it would fall through to the generic idle sweep and be hibernated hours before the cutoff")
	}
}

// THE COLD-CACHE REPORT IS ONE LINE PER CACHE WINDOW, not one per sweep tick.
// The condition stands for hours, and a line per tick would make it the
// daemon's loudest normal-mode producer.
func TestKeepAlivePolicyReportsAColdCacheOncePerCacheWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.CacheTTL+time.Minute),
	}

	// Act.
	h.srv.applyKeepAlivePolicy(rec, now)
	h.srv.applyKeepAlivePolicy(rec, now)
	h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if got := h.srv.cacheCoolReported[rec.SessionID]; got != rec.LastTurnEndMs {
		t.Fatalf("cold-cache report anchor = %d, want the decision's own last-turn-end %d", got, rec.LastTurnEndMs)
	}
}

// THE GENERIC IDLE SWEEP MAY NOT REAP BELOW THE POLICY'S CUTOFF. Its configured
// `-idle-timeout` and keepalive.Config.IdleCutoff answer the same question, and
// the shorter of the two used to hibernate sessions hours early.
func TestSweepIdleCutoffIsFlooredAtTheKeepAlivePolicyCutoff(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.srv.idleTimeout = time.Hour
	h.srv.keepAlive = keepalive.DefaultConfig()

	// Act.
	got := h.srv.sweepIdleCutoff()

	// Assert.
	if got != keepalive.DefaultIdleCutoff {
		t.Fatalf("sweepIdleCutoff() = %s, want the policy's %s; a shorter generic sweep reaps sessions the policy would still keep",
			got, keepalive.DefaultIdleCutoff)
	}
}

// THE FLOOR RAISES AND NEVER LOWERS. A deployment that deliberately asks for a
// LONGER idle timeout keeps it: delaying a teardown is the safe direction.
func TestSweepIdleCutoffKeepsALongerConfiguredTimeout(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.srv.idleTimeout = keepalive.DefaultIdleCutoff + 24*time.Hour
	h.srv.keepAlive = keepalive.DefaultConfig()

	// Act.
	got := h.srv.sweepIdleCutoff()

	// Assert.
	if got != keepalive.DefaultIdleCutoff+24*time.Hour {
		t.Fatalf("sweepIdleCutoff() = %s, want the configured %s", got, keepalive.DefaultIdleCutoff+24*time.Hour)
	}
}

// A ZERO IDLE TIMEOUT IS NOT FLOORED. Zero is the documented "hibernation is
// off" value, and raising it to six hours would turn a disabled feature into a
// slow one.
func TestSweepIdleCutoffLeavesADisabledTimeoutAlone(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.srv.idleTimeout = 0
	h.srv.keepAlive = keepalive.DefaultConfig()

	// Act.
	got := h.srv.sweepIdleCutoff()

	// Assert.
	if got != 0 {
		t.Fatalf("sweepIdleCutoff() with hibernation disabled = %s, want 0", got)
	}
}

// A PING-ONLY SESSION IS STILL REAPED BY THE SWEEP. The cache clock is reset by
// every keep-alive ping, so before the engagement clock this session read as
// freshly active forever and the sweeper never hibernated it at all.
func TestKeepAlivePolicyHibernatesAPingOnlySessionAtTheCutoff(t *testing.T) {
	// Arrange — engaged eight hours ago; the last ping ended a minute ago.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:        "s1",
		CWD:              "/ws",
		LastTurnEndMs:    msAgo(now, time.Minute),
		LastEngagementMs: msAgo(now, cfg.IdleCutoff+2*time.Hour),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if !owned {
		t.Fatal("the policy did not claim a session idle past the cutoff on its engagement clock; a session kept warm for nobody must still be reaped")
	}
}

// A RECORD WITH NO ENGAGEMENT INSTANT FALLS BACK TO ITS TURN END, which is the
// upgrade path: a session written by an earlier daemon carries only the one
// clock, and it must still be evaluated rather than becoming immortal.
func TestKeepAlivePolicyFallsBackToTheTurnEndForAnUnengagedRecord(t *testing.T) {
	// Arrange — no engagement instant at all, and a turn end past the cutoff.
	h := newHarness(t)
	cfg := keepalive.DefaultConfig()
	now := h.srv.now().UnixMilli()
	rec := registry.Record{
		SessionID:     "s1",
		CWD:           "/ws",
		LastTurnEndMs: msAgo(now, cfg.IdleCutoff+time.Hour),
	}

	// Act.
	owned := h.srv.applyKeepAlivePolicy(rec, now)

	// Assert.
	if !owned {
		t.Fatal("a pre-engagement-clock record past the cutoff was not claimed; the fallback is what keeps it evaluable")
	}
}

// THE GENERIC SWEEP READS THE ENGAGEMENT CLOCK, NOT THE STATE LOG. A keep-alive
// ping's own turn boundaries append workspace_state rows exactly as a real
// turn's do, so `ssm.LastActivityMs` was fooled by pings for the same reason the
// registry's cache clock was.
func TestSweepableMeasuresTheEngagementClockRatherThanTheStateLog(t *testing.T) {
	// Arrange — the state log is fresh (the harness just wrote it), and the
	// record says nobody has engaged with the workspace in days.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(time.Minute)
	rec := sweptRecord(t, h, id, "/w")
	rec.LastEngagementMs = msAgo(h.srv.now().UnixMilli(), 48*time.Hour)

	// Act.
	idleMs, ok := h.srv.sweepable(rec, h.srv.now().UnixMilli())

	// Assert.
	if !ok {
		t.Fatal("sweepable = false for a workspace nobody has engaged with in 48 hours; the fresh state-log row is a keep-alive ping's, not somebody's")
	}
	if idleMs < int64(24*time.Hour/time.Millisecond) {
		t.Fatalf("measured idleness %dms, want the engagement clock's ~48h; the state log's own timestamp would report minutes", idleMs)
	}
}

// AND THE FRESH ENGAGEMENT CLOCK HOLDS IT, which is the same gate stated the
// other way: a workspace somebody used a minute ago is not reaped however old
// its state log happens to be.
func TestSweepableHoldsAFreshlyEngagedWorkspace(t *testing.T) {
	// Arrange.
	h, id, quietFor := sweptWorkspace(t, time.Hour)
	quietFor(48 * time.Hour)
	rec := sweptRecord(t, h, id, "/w")
	rec.LastEngagementMs = msAgo(h.srv.now().UnixMilli(), time.Minute)

	// Act.
	_, ok := h.srv.sweepable(rec, h.srv.now().UnixMilli())

	// Assert.
	if ok {
		t.Fatal("sweepable = true for a workspace engaged a minute ago")
	}
}
