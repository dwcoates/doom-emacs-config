package gomeasure

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
// is above Config.MaxLoad. The load average has no event to wait on, so the
// gate polls it, and gives up (failing the measurement rather than measuring
// under load) after Config.QuietWait.

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

// clock is Config.Clock, the wall clock when unset.
func (m *measurer) clock() Clock {
	if m.cfg.Clock != nil {
		return m.cfg.Clock
	}
	return wallClock{}
}

// parseLoadAvg reads `sysctl -n vm.loadavg`'s "{ 1.23 4.56 7.89 }" and
// answers the one-minute figure.
func parseLoadAvg(s string) (float64, error) {
	fields := strings.Fields(strings.Trim(strings.TrimSpace(s), "{}"))
	if len(fields) < 1 {
		return 0, fmt.Errorf("gomeasure: vm.loadavg answered %q", s)
	}
	v, err := strconv.ParseFloat(fields[0], 64)
	if err != nil {
		return 0, fmt.Errorf("gomeasure: vm.loadavg answered %q: %w", s, err)
	}
	return v, nil
}

// load is the host's one-minute load average.
func (m *measurer) load(ctx context.Context) (float64, error) {
	res, err := m.run(ctx, Cmd{Path: "sysctl", Args: []string{"-n", "vm.loadavg"}, Timeout: LoadProbeTimeout})
	if err != nil {
		return 0, err
	}
	if res.Exit != 0 || res.TimedOut {
		return 0, fmt.Errorf("gomeasure: sysctl -n vm.loadavg failed:\n%s", res.Text())
	}
	return parseLoadAvg(res.Text())
}

// awaitQuiet returns once the host's load is at most MaxLoad, logging the
// wait once; it fails past QuietWait.
func (m *measurer) awaitQuiet(ctx context.Context, before string) error {
	deadline := m.clock().Now().Add(m.cfg.QuietWait)
	logged := false
	for {
		l, err := m.load(ctx)
		if err != nil {
			return err
		}
		if l <= m.cfg.MaxLoad {
			if logged {
				m.log.Infof("redundancy: load %.1f, resuming %s", l, before)
			}
			return nil
		}
		if !logged {
			m.log.Infof("redundancy: load %.1f is over %.0f; waiting before %s", l, m.cfg.MaxLoad, before)
			logged = true
		}
		if m.clock().Now().After(deadline) {
			return fmt.Errorf("gomeasure: the host's load stayed over %.0f for %s before %s; nothing was measured under it", m.cfg.MaxLoad, m.cfg.QuietWait, before)
		}
		select {
		case <-ctx.Done():
			return fmt.Errorf("gomeasure: interrupted waiting for the load to drop: %w", ctx.Err())
		case <-m.clock().After(m.pollEvery()):
		}
	}
}

// pollEvery is LoadPoll, shortened to the wait itself when that is shorter.
func (m *measurer) pollEvery() time.Duration {
	return min(LoadPoll, m.cfg.QuietWait)
}
