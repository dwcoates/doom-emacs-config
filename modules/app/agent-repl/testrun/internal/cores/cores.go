// Package cores is the host's test-CPU budget: every test run on the host,
// however many overlap, together holds at most Share percent of the host's
// cores (owner ruling, 2026-10-20: the merge queue's runs overlapping one
// another and the owners' own runs drove the whole host past 100%).
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
	"fmt"
	"strconv"
	"strings"
	"time"

	"agentrepl/testrun/internal/seats"
)

// Share is the percentage of the host's cores every test run together holds
// (owner ruling 2026-10-23: half, down from the 70% first chosen).
const Share = 50

// DefaultDir is where the host's core seats live; every run on the host
// shares it.
const DefaultDir = "/tmp/agent-repl-test-cores"

// NestedEnv marks a unit's process tree as holding core seats. A test run
// started inside a unit would take seats from the pool its own enclosing unit
// already holds part of, and could wait forever on its own ancestor; Check
// refuses that run instead.
const NestedEnv = "AGENT_REPL_TEST_CORE_SEATS_HELD"

// Budget is the host's core budget, Share percent of numCPU rounded down. A
// host too small to budget a single core is refused, never adjusted.
func Budget(numCPU int) (int, error) {
	n := numCPU * Share / 100
	if n < 1 {
		return 0, fmt.Errorf("this host has %d cores; tests may hold %d%% of them, which is not one whole core", numCPU, Share)
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
}

// NewHost is the host's core pool of budget seats under dir, taken at rank.
func NewHost(dir string, budget, rank int, log Logger) Host {
	return Host{Pool: seats.Pool{Dir: dir, Prefix: "core", N: budget, What: "core"}, Rank: rank, Log: log, Now: time.Now}
}

// Hold takes width core seats for unit id, waiting while the host's budget is
// spent, and answers the function that gives them back.
func (h Host) Hold(id string, width int) (func() error, error) {
	var start time.Time
	waited := false
	held, err := h.Pool.TakeN(width, h.Rank, func() {
		waited, start = true, h.Now()
		h.Log.Infof("core seats: unit %s waits for %d of the host's %d, held by test runs or promised to a run that outranks this one (rank %d)", id, width, h.Pool.N, h.Rank)
	})
	if err != nil {
		return nil, fmt.Errorf("hold %d core seat(s) for unit %s: %w", width, id, err)
	}
	if waited {
		h.Log.Infof("core seats: unit %s took %d after %s", id, width, h.Now().Sub(start).Round(time.Millisecond))
	}
	return held.Release, nil
}
