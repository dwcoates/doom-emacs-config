package grantedclock

import (
	"fmt"
	"sync"
	"time"
)

// A PROCESS TREE'S STALL, for a host with no stall counter of its own.
//
// macOS keeps no CPU pressure for a group of processes the way a Linux cgroup
// does. What it keeps is per process: the kernel's rusage for a process
// carries ri_runnable_time, the total time its threads sat runnable without a
// CPU. A tree's stall is the sum of that over the root and every descendant,
// which is every process a test binary's waits can depend on: the daemon it
// starts, the shims and fake gits that daemon starts, and the binary itself.
// MEASURED, merge gate 2026-10-20: four scenario goldens failed their
// daemon.addr wait while the daemon sat runnable in package init.
//
// THE SUM COUNTS OVERLAP TWICE, which only ever reads as more stall, never
// less: two processes starved at once are both counted, and the clock grants
// a reading whose stall passes its wall time nothing, never less than nothing
// (Span). A real hang -- a tree with nothing runnable -- accrues no stall and
// is reported on time.
//
// WHAT A PROCESS ACCRUED BETWEEN ITS LAST READING AND ITS EXIT IS NOT COUNTED:
// its counter leaves with it. Only a short-lived process (a fake git, a hook)
// loses anything, and at most what it accrued since the reading before, so the
// tree is read on every tick of every open bound's clock to keep that
// tail short.
type processTree struct {
	root int
	// children answers pid's direct children; a pid that has exited answers
	// none.
	children func(pid int) ([]int, error)
	// usage answers pid's identity and its runnable time; gone is true for a
	// pid that has exited.
	usage func(pid int) (u processUsage, gone bool, err error)

	mu   sync.Mutex
	last map[processKey]time.Duration
	// total is every runnable time the tree has accrued since its first
	// reading took the counters it found as their origin.
	total time.Duration
}

// processKey names one process for its whole life: a pid the kernel reuses
// starts at a different instant.
type processKey struct {
	pid   int
	start uint64
}

// processUsage is one reading of one process's rusage.
type processUsage struct {
	start    uint64
	runnable time.Duration
}

// read answers the tree's cumulative stall: every process's runnable time
// accrued since the tree was first read, the root's and its descendants'.
func (t *processTree) read() (time.Duration, error) {
	t.mu.Lock()
	defer t.mu.Unlock()
	now, err := t.walk()
	if err != nil {
		return 0, err
	}
	first := t.last == nil
	for key, runnable := range now {
		prev, seen := t.last[key]
		switch {
		case first:
			// THE FIRST READING IS THE ORIGIN, so nothing a process accrued
			// before it is counted.
		case !seen:
			// A process born since the last reading accrued all of its
			// runnable time inside the interval.
			t.total += runnable
		case runnable < prev:
			return 0, fmt.Errorf("process %d's runnable time went backwards, from %s to %s", key.pid, prev, runnable)
		default:
			t.total += runnable - prev
		}
	}
	t.last = now
	return t.total, nil
}

// walk reads the runnable time of the root and every live descendant.
func (t *processTree) walk() (map[processKey]time.Duration, error) {
	now := map[processKey]time.Duration{}
	queue := []int{t.root}
	for len(queue) > 0 {
		pid := queue[0]
		queue = queue[1:]
		u, gone, err := t.usage(pid)
		if err != nil {
			return nil, fmt.Errorf("read process %d's runnable time: %w", pid, err)
		}
		if gone {
			continue
		}
		now[processKey{pid: pid, start: u.start}] = u.runnable
		kids, err := t.children(pid)
		if err != nil {
			return nil, fmt.Errorf("list process %d's children: %w", pid, err)
		}
		queue = append(queue, kids...)
	}
	if len(now) == 0 {
		return nil, fmt.Errorf("the tree's root, process %d, is gone", t.root)
	}
	return now, nil
}
