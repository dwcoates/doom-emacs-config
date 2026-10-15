package proc

import (
	"context"
	"fmt"
	"strconv"
	"strings"
	"time"
)

// The load gate (owner rule via the parent workspace, 2026-10-15): a loaded
// host corrupts a measurement's verdicts, not only its speed. A case slowed
// past twice its healthy latency reads as a hang, and a hang as a kill. So no
// phase starts, and no mutant runs, while the host's one-minute load average
// is above Gate.MaxLoad. The load average has no event to wait on, so the
// gate polls it, and gives up (failing the measurement rather than measuring
// under load) after Gate.QuietWait.

const (
	// DefaultMaxLoad is the parent's limit: twice the host's 16 cores'
	// worth of one-minute load.
	DefaultMaxLoad = 32.0
	// DefaultQuietWait bounds one wait for the load to drop.
	DefaultQuietWait = time.Hour
	// LoadPoll is how often a waiting gate reads the load again.
	LoadPoll = 15 * time.Second
	// LoadProbeTimeout bounds one `sysctl -n vm.loadavg` (milliseconds).
	LoadProbeTimeout = 5 * time.Second
)

// Clock is what the load gate waits by.
type Clock interface {
	Now() time.Time
	After(d time.Duration) <-chan time.Time
}

type wallClock struct{}

func (wallClock) Now() time.Time                         { return time.Now() }
func (wallClock) After(d time.Duration) <-chan time.Time { return time.After(d) }

// Logger is the module's one logging path (run.Log), as the gate uses it.
type Logger interface {
	Infof(format string, args ...any)
}

// Gate waits for a quiet host.
type Gate struct {
	// Run runs the load probe, through the caller's own process counting.
	Run       func(context.Context, Cmd) (Result, error)
	Log       Logger
	MaxLoad   float64
	QuietWait time.Duration
	// Clock is what it waits by; nil is the wall clock.
	Clock Clock
}

func (g Gate) clock() Clock {
	if g.Clock != nil {
		return g.Clock
	}
	return wallClock{}
}

// ParseLoadAvg reads `sysctl -n vm.loadavg`'s "{ 1.23 4.56 7.89 }" and
// answers the one-minute figure.
func ParseLoadAvg(s string) (float64, error) {
	fields := strings.Fields(strings.Trim(strings.TrimSpace(s), "{}"))
	if len(fields) < 1 {
		return 0, fmt.Errorf("proc: vm.loadavg answered %q", s)
	}
	v, err := strconv.ParseFloat(fields[0], 64)
	if err != nil {
		return 0, fmt.Errorf("proc: vm.loadavg answered %q: %w", s, err)
	}
	return v, nil
}

// Load is the host's one-minute load average.
func (g Gate) Load(ctx context.Context) (float64, error) {
	res, err := g.Run(ctx, Cmd{Path: "sysctl", Args: []string{"-n", "vm.loadavg"}, Timeout: LoadProbeTimeout})
	if err != nil {
		return 0, err
	}
	if res.Exit != 0 || res.TimedOut {
		return 0, fmt.Errorf("proc: sysctl -n vm.loadavg failed:\n%s", res.Text())
	}
	return ParseLoadAvg(res.Text())
}

// Check refuses a gate that would let anything run under load.
func (g Gate) Check() error {
	if g.MaxLoad <= 0 || g.QuietWait <= 0 {
		return fmt.Errorf("the load gate needs a positive max load and quiet wait, got %g and %s", g.MaxLoad, g.QuietWait)
	}
	return nil
}

// AwaitQuiet returns once the host's load is at most MaxLoad, logging the
// wait once; it fails past QuietWait.
func (g Gate) AwaitQuiet(ctx context.Context, before string) error {
	deadline := g.clock().Now().Add(g.QuietWait)
	logged := false
	for {
		l, err := g.Load(ctx)
		if err != nil {
			return err
		}
		if l <= g.MaxLoad {
			if logged {
				g.Log.Infof("redundancy: load %.1f, resuming %s", l, before)
			}
			return nil
		}
		if !logged {
			g.Log.Infof("redundancy: load %.1f is over %.0f; waiting before %s", l, g.MaxLoad, before)
			logged = true
		}
		if g.clock().Now().After(deadline) {
			return fmt.Errorf("the host's load stayed over %.0f for %s before %s; nothing was measured under it", g.MaxLoad, g.QuietWait, before)
		}
		select {
		case <-ctx.Done():
			return fmt.Errorf("interrupted waiting for the load to drop: %w", context.Cause(ctx))
		case <-g.clock().After(min(LoadPoll, g.QuietWait)):
		}
	}
}
