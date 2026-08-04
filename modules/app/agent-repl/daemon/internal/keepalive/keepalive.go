// Package keepalive holds the cache keep-alive policy: the configuration it
// reads from the environment, and the PURE decision it takes from one
// session's durable last-turn-end timestamp.
//
// THERE ARE NO TIMERS HERE, AND THAT IS THE DESIGN. Every decision is a
// time-since comparison against an instant the registry persisted, evaluated
// whenever the idle sweeper next ticks. A timer would be a second, weaker
// representation of the same fact: it dies with the daemon, it does not
// advance across a laptop sleep, and it cannot tell "the ping is due" from
// "the ping is already too late". The comparison can tell those apart, which
// is why the overslept case is a DECISION (Decision.Hibernate with
// CauseCacheExpired) rather than a missed tick nobody notices.
package keepalive

import (
	"fmt"
	"os"
	"strconv"
	"time"
)

// Environment variable names. They are the daemon's only configuration surface
// for the policy; the defaults below are what an unset environment means.
const (
	EnvCacheTTLMs          = "AGENT_REPL_KEEPALIVE_CACHE_TTL_MS"
	EnvLeewayMs            = "AGENT_REPL_KEEPALIVE_LEEWAY_MS"
	EnvIdleCutoffMs        = "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS"
	EnvUncachedAlertTokens = "AGENT_REPL_UNCACHED_COST_ALERT_TOKENS"
)

// Defaults, in the units their environment variables carry.
const (
	// DefaultCacheTTL is the vendor prompt cache's expected lifetime. One hour
	// is the 1h ephemeral cache tier the CLI requests.
	DefaultCacheTTL = time.Hour
	// DefaultLeeway is how far ahead of expiry a ping fires. Two minutes is
	// several sweep intervals' worth of slack, so a ping has room to be
	// retried before the window closes.
	DefaultLeeway = 2 * time.Minute
	// DefaultIdleCutoff is when pinging stops and the session hibernates
	// instead. Six hours is six cache lifetimes: past it, the session is being
	// kept warm for nobody.
	DefaultIdleCutoff = 6 * time.Hour
	// DefaultUncachedCostAlertTokens is the uncached-input figure above which
	// a turn is reported as expensive.
	DefaultUncachedCostAlertTokens int64 = 20000
)

// RetryFloor is how much of the pre-expiry window is reserved as UNUSABLE. A
// ping may be submitted while the remaining margin exceeds this; inside it the
// cache is close enough to cold that an attempt would more likely pay full
// freight than save anything.
//
// IT IS ENFORCED BY THE DECISION, not by a caller remembering to consult it.
// Evaluate returns ActionAwaitExpiry inside the floor — an arm that carries no
// submit — so a sweeper cannot ping there however it is written. It used to be
// a boolean on an ActionPing decision, which gated nothing: every tick in the
// floor still submitted, and the only trace was a log line saying it would not
// be retried while it was being tried.
const RetryFloor = time.Minute

// Config is the resolved keep-alive policy configuration.
type Config struct {
	// CacheTTL is how long the vendor prompt cache is expected to survive
	// after the turn that wrote it ended.
	CacheTTL time.Duration
	// Leeway is how far before CacheTTL a ping becomes due.
	Leeway time.Duration
	// IdleCutoff is the elapsed quiet past which the session hibernates
	// instead of being pinged.
	IdleCutoff time.Duration
	// UncachedCostAlertTokens is the ContextCostAlert threshold.
	UncachedCostAlertTokens int64
}

// DefaultConfig is the configuration an entirely unset environment produces.
func DefaultConfig() Config {
	return Config{
		CacheTTL:                DefaultCacheTTL,
		Leeway:                  DefaultLeeway,
		IdleCutoff:              DefaultIdleCutoff,
		UncachedCostAlertTokens: DefaultUncachedCostAlertTokens,
	}
}

// FromEnv resolves the configuration from the process environment.
//
// A malformed or non-positive value is an ERROR, never a silent fall back to
// the default. Someone who set AGENT_REPL_KEEPALIVE_CACHE_TTL_MS=0 meant
// something by it, and running the default hour while they believe they
// disabled the feature is the failure mode a loud refusal exists to prevent.
func FromEnv() (Config, error) {
	cfg := DefaultConfig()
	var err error
	if cfg.CacheTTL, err = durationEnv(EnvCacheTTLMs, DefaultCacheTTL); err != nil {
		return Config{}, err
	}
	if cfg.Leeway, err = durationEnv(EnvLeewayMs, DefaultLeeway); err != nil {
		return Config{}, err
	}
	if cfg.IdleCutoff, err = durationEnv(EnvIdleCutoffMs, DefaultIdleCutoff); err != nil {
		return Config{}, err
	}
	if cfg.UncachedCostAlertTokens, err = int64Env(EnvUncachedAlertTokens, DefaultUncachedCostAlertTokens); err != nil {
		return Config{}, err
	}
	return cfg, cfg.Validate()
}

// Validate reports a configuration whose knobs cannot describe a coherent
// policy. A leeway at least as large as the TTL leaves no window in which a
// ping is both due and still useful, and a cutoff below the TTL means the
// session hibernates before its first ping could ever fire — each is a
// silently inert feature, so each is refused at startup instead.
func (c Config) Validate() error {
	if c.CacheTTL <= 0 {
		return fmt.Errorf("keepalive: cache TTL must be positive, got %s", c.CacheTTL)
	}
	if c.Leeway <= 0 {
		return fmt.Errorf("keepalive: leeway must be positive, got %s", c.Leeway)
	}
	if c.Leeway >= c.CacheTTL {
		return fmt.Errorf("keepalive: leeway %s must be shorter than the cache TTL %s; otherwise a ping is never both due and useful",
			c.Leeway, c.CacheTTL)
	}
	if c.Leeway <= RetryFloor {
		return fmt.Errorf("keepalive: leeway %s must be longer than the retry floor %s; otherwise the whole ping window lies inside the floor and no ping could ever be submitted",
			c.Leeway, RetryFloor)
	}
	if c.IdleCutoff <= 0 {
		return fmt.Errorf("keepalive: idle cutoff must be positive, got %s", c.IdleCutoff)
	}
	if c.IdleCutoff < c.CacheTTL {
		return fmt.Errorf("keepalive: idle cutoff %s must be at least the cache TTL %s; otherwise the session hibernates before a ping could ever fire",
			c.IdleCutoff, c.CacheTTL)
	}
	if c.UncachedCostAlertTokens <= 0 {
		return fmt.Errorf("keepalive: uncached cost alert threshold must be positive, got %d", c.UncachedCostAlertTokens)
	}
	return nil
}

// SweepInterval is the tick the policy needs to be evaluated at, given
// whatever interval the caller already uses. The ping window is exactly
// Leeway wide, so a sweep slower than that could step straight over it; a
// quarter of the leeway gives four chances to land inside it.
//
// existing is the caller's own interval; a non-positive one is ignored, so a
// caller with no interval of its own gets the derived one.
func (c Config) SweepInterval(existing time.Duration) time.Duration {
	derived := c.Leeway / 4
	if derived <= 0 {
		derived = c.Leeway
	}
	if existing > 0 && existing < derived {
		return existing
	}
	return derived
}

// Action is what the policy decided to do about one session.
type Action int

const (
	// ActionNone leaves the session alone: it is neither due for a ping nor
	// past any threshold.
	ActionNone Action = iota
	// ActionPing submits a cache keep-alive prompt.
	ActionPing
	// ActionHibernate takes the hibernation transition, with Decision.Cause
	// saying why.
	ActionHibernate
	// ActionAwaitExpiry is the RETRY FLOOR as an action: the session is inside
	// the ping window but too close to expiry for a ping to be worth
	// submitting, so nothing is submitted and the cache is left to go cold —
	// which the policy's own cache-expired branch will then report.
	//
	// It is a DISTINCT ARM rather than a flag on ActionPing because the floor
	// has to be unforgettable. A caller switching on Action cannot submit here
	// without writing a case that says it is doing so.
	ActionAwaitExpiry
)

func (a Action) String() string {
	switch a {
	case ActionPing:
		return "ping"
	case ActionHibernate:
		return "hibernate"
	case ActionAwaitExpiry:
		return "await_expiry"
	default:
		return "none"
	}
}

// Cause names the hibernation cause an ActionHibernate carries. The tokens are
// the registry's durable spelling.
const (
	CauseIdleCutoff   = "idle_cutoff"
	CauseCacheExpired = "cache_expired"
)

// Decision is one evaluation's outcome.
type Decision struct {
	Action Action
	// Cause is set exactly when Action is ActionHibernate.
	Cause string
	// ElapsedMs is how long the session had been quiet at evaluation time. It
	// is carried on every decision so the log line and the durable
	// HibernationDetail report the measured figure rather than re-deriving one
	// from a clock that has since moved.
	ElapsedMs int64
	// RemainingMs is how much margin was left before the cache's expected
	// expiry at evaluation time. Meaningful for ActionPing and
	// ActionAwaitExpiry, and it is what the floor's log line reports rather
	// than re-deriving a figure from a clock that has since moved.
	RemainingMs int64
	// FloorMs is the retry floor the decision was taken against, carried on
	// ActionAwaitExpiry so the record of a refusal names the threshold that
	// caused it.
	FloorMs int64
}

// Evaluate takes the policy decision for one session from its durable
// last-turn-end instant.
//
// THE ORDER OF THE TESTS IS THE POLICY. The idle cutoff is checked first
// because it subsumes everything past it: a session quiet for longer than the
// cutoff is not a candidate for a ping under any reading, so asking about the
// cache first would produce a cache_expired hibernation for a session whose
// real reason is that nobody has touched it all day.
//
// A session with no recorded turn end (lastTurnEndMs zero) is left alone.
// EVERY UNKNOWN ANSWERS NONE, the same rule the idle sweeper's own gates
// follow: an undated session is one the policy knows nothing about, and both
// pinging it and reaping it would be acting on absent evidence.
func (c Config) Evaluate(nowMs, lastTurnEndMs int64) Decision {
	if lastTurnEndMs <= 0 {
		return Decision{Action: ActionNone}
	}
	elapsed := time.Duration(nowMs-lastTurnEndMs) * time.Millisecond
	if elapsed < 0 {
		// A last-turn-end in the future is a clock that moved backwards, not a
		// session with negative idle time. Treat it as freshly active: the
		// conservative direction is to do nothing.
		return Decision{Action: ActionNone, ElapsedMs: int64(elapsed / time.Millisecond)}
	}
	elapsedMs := int64(elapsed / time.Millisecond)
	if elapsed >= c.IdleCutoff {
		return Decision{Action: ActionHibernate, Cause: CauseIdleCutoff, ElapsedMs: elapsedMs}
	}
	if elapsed >= c.CacheTTL {
		// THE CACHE IS ALREADY COLD. Pinging now would pay a full context
		// re-ingest to warm a cache for a session nobody is using — the exact
		// cost the keep-alive exists to avoid — so the discovery IS the
		// hibernate transition, reported rather than absorbed.
		return Decision{Action: ActionHibernate, Cause: CauseCacheExpired, ElapsedMs: elapsedMs}
	}
	if elapsed >= c.CacheTTL-c.Leeway {
		remainingMs := int64((c.CacheTTL - elapsed) / time.Millisecond)
		// THE FLOOR IS TESTED INSIDE THE PING WINDOW, not beside it. Asking
		// about the floor first would report await_expiry for a session that
		// has not reached the window at all under a leeway shorter than the
		// floor — a configuration Validate refuses, but the ordering states
		// the containment rather than relying on that refusal.
		if elapsed >= c.CacheTTL-RetryFloor {
			return Decision{
				Action:      ActionAwaitExpiry,
				ElapsedMs:   elapsedMs,
				RemainingMs: remainingMs,
				FloorMs:     int64(RetryFloor / time.Millisecond),
			}
		}
		return Decision{
			Action:      ActionPing,
			ElapsedMs:   elapsedMs,
			RemainingMs: remainingMs,
		}
	}
	return Decision{Action: ActionNone, ElapsedMs: elapsedMs}
}

// durationEnv reads a millisecond-valued environment variable.
func durationEnv(name string, fallback time.Duration) (time.Duration, error) {
	raw := os.Getenv(name)
	if raw == "" {
		return fallback, nil
	}
	ms, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("keepalive: %s=%q is not an integer number of milliseconds: %w", name, raw, err)
	}
	if ms <= 0 {
		return 0, fmt.Errorf("keepalive: %s=%q must be a positive number of milliseconds", name, raw)
	}
	return time.Duration(ms) * time.Millisecond, nil
}

// int64Env reads an integer-valued environment variable.
func int64Env(name string, fallback int64) (int64, error) {
	raw := os.Getenv(name)
	if raw == "" {
		return fallback, nil
	}
	n, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("keepalive: %s=%q is not an integer: %w", name, raw, err)
	}
	if n <= 0 {
		return 0, fmt.Errorf("keepalive: %s=%q must be positive", name, raw)
	}
	return n, nil
}

// PingText is the EXACT prompt the daemon submits as a cache keep-alive. It is
// a constant rather than a template because every consumer that excludes
// keep-alive turns keys on PROMPT_ORIGIN_CACHE_KEEP_ALIVE, and a ping whose
// text varied would still have to be recognizable to a human reading a
// transcript.
const PingText = "respond to this message with only a '.'. make no tool calls or any other changes"
