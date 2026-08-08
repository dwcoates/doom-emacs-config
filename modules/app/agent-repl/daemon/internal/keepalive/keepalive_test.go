package keepalive

import (
	"testing"
	"time"
)

// testConfig is a compact, exactly-validating configuration: a 1h TTL, a 2m
// leeway and a 6h cutoff, i.e. the shipped defaults, so the window arithmetic
// under test is the arithmetic production runs.
func testConfig() Config { return DefaultConfig() }

func TestEvaluateAction(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)
	msAgo := func(d time.Duration) int64 { return now - int64(d/time.Millisecond) }

	tests := []struct {
		name          string
		lastTurnEndMs int64
		wantAction    Action
		wantCause     string
	}{
		{
			name:          "no recorded turn end leaves the session alone",
			lastTurnEndMs: 0,
			wantAction:    ActionNone,
		},
		{
			name:          "freshly idle is well before the window",
			lastTurnEndMs: msAgo(5 * time.Minute),
			wantAction:    ActionNone,
		},
		{
			// AMENDED for warm compaction. What this row has always been about —
			// this instant is not a ping — is unchanged; the instant now falls
			// inside the warm-compaction span, which by construction ends
			// exactly where the ping window begins.
			name:          "one millisecond before the ping window opens warm-compacts rather than pinging",
			lastTurnEndMs: msAgo(DefaultCacheTTL - DefaultLeeway - time.Millisecond),
			wantAction:    ActionWarmCompact,
		},
		{
			name:          "the instant the window opens pings",
			lastTurnEndMs: msAgo(DefaultCacheTTL - DefaultLeeway),
			wantAction:    ActionPing,
		},
		{
			name:          "inside the window and above the retry floor pings",
			lastTurnEndMs: msAgo(DefaultCacheTTL - 90*time.Second),
			wantAction:    ActionPing,
		},
		{
			name:          "at the TTL the cache is already cold",
			lastTurnEndMs: msAgo(DefaultCacheTTL),
			wantAction:    ActionHibernate,
			wantCause:     CauseCacheExpired,
		},
		{
			name:          "an overslept session hibernates cache-expired",
			lastTurnEndMs: msAgo(3 * time.Hour),
			wantAction:    ActionHibernate,
			wantCause:     CauseCacheExpired,
		},
		{
			name:          "past the idle cutoff hibernates idle-cutoff, not cache-expired",
			lastTurnEndMs: msAgo(DefaultIdleCutoff + time.Minute),
			wantAction:    ActionHibernate,
			wantCause:     CauseIdleCutoff,
		},
		{
			name:          "a last turn end in the future does nothing",
			lastTurnEndMs: now + int64(time.Hour/time.Millisecond),
			wantAction:    ActionNone,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := cfg.Evaluate(now, tc.lastTurnEndMs)

			if got.Action != tc.wantAction {
				t.Fatalf("Evaluate action = %s, want %s", got.Action, tc.wantAction)
			}
			if got.Cause != tc.wantCause {
				t.Fatalf("Evaluate cause = %q, want %q", got.Cause, tc.wantCause)
			}
		})
	}
}

// TestEvaluateEntersTheRetryFloorAsItsOwnAction covers the floor's edge: the
// decision at exactly TTL-RetryFloor carries no submit. It replaces the
// Retryable flag's test because the flag gated nothing — the sweeper submitted
// on every ActionPing whatever it said.
func TestEvaluateEntersTheRetryFloorAsItsOwnAction(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)

	got := cfg.Evaluate(now, now-int64((DefaultCacheTTL-RetryFloor)/time.Millisecond))

	if got.Action != ActionAwaitExpiry {
		t.Fatalf("Evaluate action at the retry floor = %s, want await_expiry; an ActionPing here is a submit inside the floor", got.Action)
	}
}

// TestEvaluatePingsOneMillisecondBeforeTheRetryFloor covers the other side of
// the same edge: the floor must not swallow the usable part of the window.
func TestEvaluatePingsOneMillisecondBeforeTheRetryFloor(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)

	got := cfg.Evaluate(now, now-int64((DefaultCacheTTL-RetryFloor-time.Millisecond)/time.Millisecond))

	if got.Action != ActionPing {
		t.Fatalf("Evaluate action one millisecond before the retry floor = %s, want ping", got.Action)
	}
}

// TestEvaluateFloorDecisionCarriesTheFloorAccount asserts the arm reports the
// threshold it refused against, which is the log line's whole content.
func TestEvaluateFloorDecisionCarriesTheFloorAccount(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)

	got := cfg.Evaluate(now, now-int64((DefaultCacheTTL-RetryFloor)/time.Millisecond))

	if got.FloorMs != int64(RetryFloor/time.Millisecond) {
		t.Fatalf("Evaluate floor_ms = %d, want %d", got.FloorMs, int64(RetryFloor/time.Millisecond))
	}
}

// TestEvaluatePingReportsRemainingMargin asserts the ping arm carries the
// margin the failure log reports, so no caller re-derives it from a clock that
// has since moved.
func TestEvaluatePingReportsRemainingMargin(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)

	got := cfg.Evaluate(now, now-int64((DefaultCacheTTL-DefaultLeeway)/time.Millisecond))

	if got.RemainingMs != int64(DefaultLeeway/time.Millisecond) {
		t.Fatalf("Evaluate remaining_ms = %d, want %d", got.RemainingMs, int64(DefaultLeeway/time.Millisecond))
	}
}

// TestValidateRefusesALeewayInsideTheRetryFloor covers the configuration whose
// entire ping window lies inside the floor: it can never submit a ping, which
// is a silently inert feature and therefore a startup refusal.
func TestValidateRefusesALeewayInsideTheRetryFloor(t *testing.T) {
	cfg := DefaultConfig()
	cfg.Leeway = RetryFloor

	err := cfg.Validate()

	if err == nil {
		t.Fatal("Validate with leeway == the retry floor = nil, want a refusal; every tick in that window would decline to submit")
	}
}

func TestEvaluateReportsMeasuredElapsed(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)
	const idle = 3 * time.Hour

	got := cfg.Evaluate(now, now-int64(idle/time.Millisecond))

	if want := int64(idle / time.Millisecond); got.ElapsedMs != want {
		t.Fatalf("Evaluate elapsed = %d ms, want %d ms", got.ElapsedMs, want)
	}
}

func TestConfigValidate(t *testing.T) {
	tests := []struct {
		name    string
		mutate  func(*Config)
		wantErr bool
	}{
		{
			name:   "the defaults are coherent",
			mutate: func(*Config) {},
		},
		{
			name:    "a leeway at least as long as the TTL leaves no usable window",
			mutate:  func(c *Config) { c.Leeway = c.CacheTTL },
			wantErr: true,
		},
		{
			name:    "a cutoff below the TTL means no ping could ever fire",
			mutate:  func(c *Config) { c.IdleCutoff = c.CacheTTL - time.Minute },
			wantErr: true,
		},
		{
			name:    "a non-positive TTL is refused",
			mutate:  func(c *Config) { c.CacheTTL = 0 },
			wantErr: true,
		},
		{
			name:    "a non-positive cost threshold is refused",
			mutate:  func(c *Config) { c.UncachedCostAlertTokens = 0 },
			wantErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			cfg := DefaultConfig()
			tc.mutate(&cfg)

			err := cfg.Validate()

			if (err != nil) != tc.wantErr {
				t.Fatalf("Validate() = %v, wantErr %v", err, tc.wantErr)
			}
		})
	}
}

func TestFromEnvRefusesMalformedValue(t *testing.T) {
	t.Setenv(EnvCacheTTLMs, "not-a-number")

	_, err := FromEnv()

	if err == nil {
		t.Fatal("FromEnv with a malformed TTL = nil error, want a loud refusal rather than a silent default")
	}
}

func TestFromEnvRefusesZeroRatherThanDefaulting(t *testing.T) {
	t.Setenv(EnvLeewayMs, "0")

	_, err := FromEnv()

	if err == nil {
		t.Fatal("FromEnv with a zero leeway = nil error, want a refusal; defaulting would run a policy the operator believes they disabled")
	}
}

func TestFromEnvReadsOverrides(t *testing.T) {
	t.Setenv(EnvCacheTTLMs, "600000")
	// Three minutes rather than one: a leeway equal to the retry floor is a
	// window that can never submit a ping, which Validate now refuses.
	t.Setenv(EnvLeewayMs, "180000")
	t.Setenv(EnvIdleCutoffMs, "1800000")
	t.Setenv(EnvUncachedAlertTokens, "1234")

	cfg, err := FromEnv()

	if err != nil {
		t.Fatalf("FromEnv() = %v, want nil", err)
	}
	if cfg.CacheTTL != 10*time.Minute || cfg.Leeway != 3*time.Minute ||
		cfg.IdleCutoff != 30*time.Minute || cfg.UncachedCostAlertTokens != 1234 {
		t.Fatalf("FromEnv() = %+v, want the four overrides applied", cfg)
	}
}

func TestSweepInterval(t *testing.T) {
	cfg := DefaultConfig()

	tests := []struct {
		name     string
		existing time.Duration
		want     time.Duration
	}{
		{
			name:     "with no interval of its own the caller gets a quarter of the leeway",
			existing: 0,
			want:     DefaultLeeway / 4,
		},
		{
			name:     "an already-faster interval is kept",
			existing: time.Second,
			want:     time.Second,
		},
		{
			name:     "a slower interval is tightened to fit inside the ping window",
			existing: time.Hour,
			want:     DefaultLeeway / 4,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := cfg.SweepInterval(tc.existing)

			if got != tc.want {
				t.Fatalf("SweepInterval(%s) = %s, want %s", tc.existing, got, tc.want)
			}
		})
	}
}

// THE WARM-COMPACTION INSTANT IS ANCHORED ON THE LAST SUBMITTABLE ONE. It is
// WarmCompactMargin ahead of CacheTTL-RetryFloor, which is where the policy
// stops submitting anything at all — not ahead of the ping window's opening
// edge, which moves with Leeway and says nothing about remaining cache life.
func TestWarmCompactAtIsTheMarginAheadOfTheLastSubmittableInstant(t *testing.T) {
	// Arrange.
	cfg := DefaultConfig()

	// Act.
	got := cfg.WarmCompactAt()

	// Assert.
	want := cfg.CacheTTL - RetryFloor - WarmCompactMargin
	if got != want {
		t.Fatalf("WarmCompactAt() = %s, want %s (TTL %s less the %s floor and the %s margin)",
			got, want, cfg.CacheTTL, RetryFloor, WarmCompactMargin)
	}
}

// THE WARM COMPACTION IS DUE AT ITS OWN INSTANT AND NOT ONE MILLISECOND
// EARLIER. Both edges are asserted because a decision taken early is a
// compaction submitted with more cache life left than the policy claims, and
// one taken late is the feature silently not existing.
func TestEvaluateWarmCompactsFromItsDueInstant(t *testing.T) {
	cfg := DefaultConfig()
	const now = int64(10_000_000_000)
	msAgo := func(d time.Duration) int64 { return now - int64(d/time.Millisecond) }

	tests := []struct {
		name string
		idle time.Duration
		want Action
	}{
		{
			name: "one millisecond before the warm-compaction instant does nothing",
			idle: cfg.WarmCompactAt() - time.Millisecond,
			want: ActionNone,
		},
		{
			name: "exactly at the warm-compaction instant compacts",
			idle: cfg.WarmCompactAt(),
			want: ActionWarmCompact,
		},
		{
			name: "inside the span and short of the ping window still compacts",
			idle: cfg.CacheTTL - cfg.Leeway - time.Second,
			want: ActionWarmCompact,
		},
		{
			name: "the ping window's opening edge pings rather than compacting",
			idle: cfg.CacheTTL - cfg.Leeway,
			want: ActionPing,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := cfg.Evaluate(now, msAgo(tc.idle))

			// Assert.
			if got.Action != tc.want {
				t.Fatalf("Evaluate(idle=%s) = %s, want %s", tc.idle, got.Action, tc.want)
			}
		})
	}
}

// A CACHE TTL WITH NO ROOM FOR A WARM COMPACTION IS REFUSED AT STARTUP rather
// than silently running a policy whose compaction arm can never be reached.
func TestValidateRefusesACacheTTLWithNoWarmCompactionInstant(t *testing.T) {
	// Arrange: a TTL shorter than the floor plus the margin puts the instant at
	// or before the turn that opened the window.
	cfg := DefaultConfig()
	cfg.CacheTTL = RetryFloor + WarmCompactMargin
	cfg.Leeway = 30 * time.Second

	// Act.
	err := cfg.Validate()

	// Assert.
	if err == nil {
		t.Fatalf("Validate() accepted a %s cache TTL, which leaves no warm-compaction instant at all", cfg.CacheTTL)
	}
}

// A LEEWAY WIDE ENOUGH TO SWALLOW THE WARM COMPACTION IS REFUSED AT STARTUP.
// The ping arm is tested first, so a compaction due at or after the ping window
// opens is one no session could ever reach — a silently inert feature, which is
// exactly what the startup refusal exists to prevent.
func TestValidateRefusesALeewayThatSwallowsTheWarmCompaction(t *testing.T) {
	// Arrange.
	cfg := DefaultConfig()
	cfg.Leeway = RetryFloor + WarmCompactMargin

	// Act.
	err := cfg.Validate()

	// Assert.
	if err == nil {
		t.Fatalf("Validate() accepted a %s leeway against a %s floor and a %s margin; the ping arm would swallow every warm compaction",
			cfg.Leeway, RetryFloor, WarmCompactMargin)
	}
}
