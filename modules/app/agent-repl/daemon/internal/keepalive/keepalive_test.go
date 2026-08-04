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
			name:          "one millisecond before the window opens does not ping",
			lastTurnEndMs: msAgo(DefaultCacheTTL - DefaultLeeway - time.Millisecond),
			wantAction:    ActionNone,
		},
		{
			name:          "the instant the window opens pings",
			lastTurnEndMs: msAgo(DefaultCacheTTL - DefaultLeeway),
			wantAction:    ActionPing,
		},
		{
			name:          "inside the window pings",
			lastTurnEndMs: msAgo(DefaultCacheTTL - time.Minute),
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

func TestEvaluatePingRetryable(t *testing.T) {
	cfg := testConfig()
	const now = int64(10_000_000_000)
	msAgo := func(d time.Duration) int64 { return now - int64(d/time.Millisecond) }

	tests := []struct {
		name          string
		lastTurnEndMs int64
		want          bool
	}{
		{
			name:          "early in the window a failed submit may be retried",
			lastTurnEndMs: msAgo(DefaultCacheTTL - DefaultLeeway),
			want:          true,
		},
		{
			name:          "inside the retry floor no further attempt is licensed",
			lastTurnEndMs: msAgo(DefaultCacheTTL - RetryFloor),
			want:          false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := cfg.Evaluate(now, tc.lastTurnEndMs)

			if got.Action != ActionPing {
				t.Fatalf("Evaluate action = %s, want ping (test setup)", got.Action)
			}
			if got.Retryable != tc.want {
				t.Fatalf("Evaluate retryable = %v, want %v", got.Retryable, tc.want)
			}
		})
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
	t.Setenv(EnvLeewayMs, "60000")
	t.Setenv(EnvIdleCutoffMs, "1800000")
	t.Setenv(EnvUncachedAlertTokens, "1234")

	cfg, err := FromEnv()

	if err != nil {
		t.Fatalf("FromEnv() = %v, want nil", err)
	}
	if cfg.CacheTTL != 10*time.Minute || cfg.Leeway != time.Minute ||
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
