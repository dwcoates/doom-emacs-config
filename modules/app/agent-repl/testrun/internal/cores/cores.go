// Package cores is the host's test-CPU budget: every test run on the host,
// however many overlap, together holds at most the pool of core seats, and
// the CPU governor (governor.go) holds back what the rest of the host leaves
// no room for, so the host's total CPU use stays under CapPercent (owner
// rulings 2026-10-20, 2026-10-23 and 2026-10-26: overlapping runs drove the
// whole host past 100%, and a fixed share of the cores still let the host
// pass 90% while the rest of it was busy).
//
// THE BUDGET IS A POOL OF CORE SEATS, ONE PER BUDGETED CORE, SHARED BY EVERY
// RUN. A unit takes as many seats as it holds core slots (its width) before it
// starts and gives them back when it exits, so the sum of the widths of every
// running unit on the host never exceeds the pool. That is a property of the
// kernel's locks (package seats), not a hope about how many runs overlap: a
// fourth run arriving while three are going waits for seats rather than
// adding its cores to theirs. A run that dies drops its seats with its locks.
package cores

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"agentrepl/testrun/internal/seats"
)

// DefaultDir is where the host's core seats live; every run on the host
// shares it.
const DefaultDir = "/tmp/agent-repl-test-cores"

// NestedEnv marks a unit's process tree as holding core seats. A test run
// started inside a unit would take seats from the pool its own enclosing unit
// already holds part of, and could wait forever on its own ancestor; Check
// refuses that run instead.
const NestedEnv = "AGENT_REPL_TEST_CORE_SEATS_HELD"

// Budget is the host's pool of core seats: CapPercent of numCPU rounded
// down, all a run may hold on an otherwise idle host. A host too small to
// budget a single core is refused, never adjusted.
func Budget(numCPU int) (int, error) {
	n := numCPU * CapPercent / 100
	if n < 1 {
		return 0, fmt.Errorf("this host has %d cores; tests may hold %d%% of them, which is not one whole core", numCPU, CapPercent)
	}
	return n, nil
}

// Check refuses a run started inside another run's unit (NestedEnv set).
func Check(getenv func(string) string) error {
	if v := getenv(NestedEnv); v != "" {
		return fmt.Errorf("this test run was started inside another run's unit (%s=%s); a nested run would wait on core seats its own ancestor holds", NestedEnv, v)
	}
	return nil
}

// EnvBackground is the marker bin/background.sh exports to every test run:
// nice-<niceness>, the niceness the run was demoted to.
const EnvBackground = "AGENT_REPL_BACKGROUND_PRIORITY"

// Rank is this run's priority among the runs sharing the host's cores: its
// niceness, as bin/background.sh demoted it (the merge queue's head 10, the
// merge behind it 15, every other run 19). A lower rank gathers first
// (seats.Pool.TakeN), so the order nice gives the runs' CPU is the order they
// get cores in. A run without the marker never got here through
// bin/background.sh, and is refused.
func Rank(getenv func(string) string) (int, error) {
	v := getenv(EnvBackground)
	n, err := strconv.Atoi(strings.TrimPrefix(v, "nice-"))
	if !strings.HasPrefix(v, "nice-") || err != nil || n < 0 {
		return 0, fmt.Errorf("%s=%q is not the nice-<niceness> bin/background.sh sets; a test run ranks for the host's cores by it", EnvBackground, v)
	}
	return n, nil
}

// Logger is what Host says on.
type Logger interface {
	Infof(format string, args ...any)
}

// Host hands out the host's core seats.
type Host struct {
	Pool seats.Pool
	// Rank is this run's (Rank).
	Rank int
	Log  Logger
	Now  func() time.Time
	// WaitingForCPU, when set, is told a unit begins to wait while the CPU
	// governor holds every unit back, and CPUWaitEnded when it has its seats.
	WaitingForCPU func(id string, state CPUState)
	CPUWaitEnded  func(id string, waited time.Duration)
}

// NewHost is the host's core pool of budget seats under dir, taken at rank.
func NewHost(dir string, budget, rank int, log Logger) Host {
	return Host{Pool: seats.Pool{Dir: dir, Prefix: "core", N: budget, What: "core"}, Rank: rank, Log: log, Now: time.Now}
}

// Hold takes width core seats for unit id, waiting while the host's budget is
// spent or the CPU governor holds units back, and answers the function that
// gives them back. A wait says which of the two it is.
func (h Host) Hold(id string, width int) (func() error, error) {
	var start time.Time
	waited, forCPU := false, false
	var stateErr error
	held, err := h.Pool.TakeN(width, h.Rank, func() {
		waited, start = true, h.Now()
		state, err := ReadState(h.Pool.Dir)
		if err != nil {
			stateErr = err
			h.Log.Infof("core seats: unit %s waits for %d of the host's %d, and cannot tell whether for the CPU: %v", id, width, h.Pool.N, err)
			return
		}
		if state.WaitingForCPU {
			forCPU = true
			h.Log.Infof("core seats: unit %s is waiting for CPU; the host is %.0f%% busy against the %d%% cap every test and build together is held to", id, state.BusyPercent, state.CapPercent)
			if h.WaitingForCPU != nil {
				h.WaitingForCPU(id, state)
			}
			return
		}
		h.Log.Infof("core seats: unit %s waits for %d of the host's %d, held by test runs or promised to a run that outranks this one (rank %d)", id, width, h.Pool.N, h.Rank)
	})
	if err != nil {
		return nil, errors.Join(fmt.Errorf("hold %d core seat(s) for unit %s: %w", width, id, err), stateErr)
	}
	if stateErr != nil {
		return nil, errors.Join(fmt.Errorf("unit %s waited for core seats but could not tell why: %w", id, stateErr), held.Release())
	}
	if waited {
		took := h.Now().Sub(start)
		h.Log.Infof("core seats: unit %s took %d after %s", id, width, took.Round(time.Millisecond))
		if forCPU && h.CPUWaitEnded != nil {
			h.CPUWaitEnded(id, took)
		}
	}
	return held.Release, nil
}
