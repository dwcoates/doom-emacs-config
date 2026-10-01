// Package sched decides WHAT runs WHEN on a fixed number of core slots.
//
// The model, and every rule the runner obeys, is here:
//
//   - A Unit is one process that occupies exactly ONE core slot for its whole
//     life. Every runner a unit starts is pinned to one core by the suite that
//     builds it (`go test -p 1 -parallel 1`, vitest `--maxWorkers=1`, a
//     single-threaded Emacs), so "one unit, one core" is a property of the
//     command line, not a hope about it.
//   - Units form a DAG through Deps. A unit is READY when every dependency
//     finished successfully; a unit whose dependency failed is CANCELLED, never
//     run, because its input is known to be missing.
//   - Among ready units the one with the highest BOTTOM LEVEL starts first: its
//     own estimate plus the longest estimated chain of units that wait on it.
//     For independent units that is plain longest-processing-time-first; a
//     build that unblocks a long chain jumps ahead of a slightly longer leaf.
//
// The same Queue drives both the real run and Simulate, so the planner's
// predicted makespan is the makespan of exactly the policy the runner uses.
package sched

import (
	"fmt"
	"sort"
)

// Unit is one schedulable process.
type Unit struct {
	// ID is unique across the whole run.
	ID string
	// Suite is the roster suite the unit belongs to; a suite passes when every
	// one of its units does.
	Suite string
	// Deps are unit IDs that must finish successfully before this one starts.
	Deps []string
	// Est is the estimated wall time in seconds.
	Est float64
}

// Queue hands out ready units in bottom-level order and tracks completion.
type Queue struct {
	units     map[string]Unit
	priority  map[string]float64
	waiting   map[string]int      // unfinished dependency count
	dependent map[string][]string // dep -> units that wait on it
	ready     []string
	started   map[string]bool
	finished  map[string]bool
	cancelled map[string]bool
}

// NewQueue validates the DAG and computes every unit's bottom level. An
// unknown dependency, a duplicate ID or a cycle is a planning bug, and is
// returned as an error the caller must fail the run on.
func NewQueue(units []Unit) (*Queue, error) {
	q := &Queue{
		units:     map[string]Unit{},
		priority:  map[string]float64{},
		waiting:   map[string]int{},
		dependent: map[string][]string{},
		started:   map[string]bool{},
		finished:  map[string]bool{},
		cancelled: map[string]bool{},
	}
	for _, u := range units {
		if u.ID == "" {
			return nil, fmt.Errorf("sched: a unit of suite %q has no ID", u.Suite)
		}
		if _, dup := q.units[u.ID]; dup {
			return nil, fmt.Errorf("sched: duplicate unit ID %q", u.ID)
		}
		if u.Est < 0 {
			return nil, fmt.Errorf("sched: unit %q has a negative estimate %v", u.ID, u.Est)
		}
		q.units[u.ID] = u
	}
	for _, u := range units {
		for _, d := range u.Deps {
			if _, ok := q.units[d]; !ok {
				return nil, fmt.Errorf("sched: unit %q depends on unknown unit %q", u.ID, d)
			}
			q.dependent[d] = append(q.dependent[d], u.ID)
		}
		q.waiting[u.ID] = len(u.Deps)
	}
	if err := q.computePriorities(); err != nil {
		return nil, err
	}
	for _, u := range units {
		if q.waiting[u.ID] == 0 {
			q.ready = append(q.ready, u.ID)
		}
	}
	q.sortReady()
	return q, nil
}

// computePriorities sets each unit's bottom level, detecting cycles.
func (q *Queue) computePriorities() error {
	const (
		unvisited = iota
		visiting
		done
	)
	state := map[string]int{}
	var visit func(id string) error
	visit = func(id string) error {
		switch state[id] {
		case done:
			return nil
		case visiting:
			return fmt.Errorf("sched: dependency cycle through unit %q", id)
		}
		state[id] = visiting
		longest := 0.0
		for _, d := range q.dependent[id] {
			if err := visit(d); err != nil {
				return err
			}
			if q.priority[d] > longest {
				longest = q.priority[d]
			}
		}
		q.priority[id] = q.units[id].Est + longest
		state[id] = done
		return nil
	}
	ids := make([]string, 0, len(q.units))
	for id := range q.units {
		ids = append(ids, id)
	}
	sort.Strings(ids)
	for _, id := range ids {
		if err := visit(id); err != nil {
			return err
		}
	}
	return nil
}

// sortReady orders the ready list by bottom level, highest first; ties break
// on ID so a plan is deterministic.
func (q *Queue) sortReady() {
	sort.SliceStable(q.ready, func(i, j int) bool {
		a, b := q.ready[i], q.ready[j]
		if q.priority[a] != q.priority[b] {
			return q.priority[a] > q.priority[b]
		}
		return a < b
	})
}

// Priority is a unit's bottom level.
func (q *Queue) Priority(id string) float64 { return q.priority[id] }

// Next pops the highest-priority ready unit.
func (q *Queue) Next() (Unit, bool) {
	if len(q.ready) == 0 {
		return Unit{}, false
	}
	id := q.ready[0]
	q.ready = q.ready[1:]
	q.started[id] = true
	return q.units[id], true
}

// Done records a started unit's outcome. A success may make dependents ready;
// a failure cancels every unit downstream of it, which Done returns so the
// caller can report each one.
func (q *Queue) Done(id string, ok bool) []Unit {
	if !q.started[id] || q.finished[id] {
		panic(fmt.Sprintf("sched: Done(%q) for a unit that is not running", id))
	}
	q.finished[id] = true
	if ok {
		for _, d := range q.dependent[id] {
			q.waiting[d]--
			if q.waiting[d] == 0 && !q.cancelled[d] {
				q.ready = append(q.ready, d)
			}
		}
		q.sortReady()
		return nil
	}
	var cancelled []Unit
	var cancel func(id string)
	cancel = func(id string) {
		for _, d := range q.dependent[id] {
			if q.cancelled[d] {
				continue
			}
			q.cancelled[d] = true
			cancelled = append(cancelled, q.units[d])
			cancel(d)
		}
	}
	cancel(id)
	sort.Slice(cancelled, func(i, j int) bool { return cancelled[i].ID < cancelled[j].ID })
	return cancelled
}

// Pending is how many units have neither finished nor been cancelled.
func (q *Queue) Pending() int {
	n := 0
	for id := range q.units {
		if !q.finished[id] && !q.cancelled[id] {
			n++
		}
	}
	return n
}
