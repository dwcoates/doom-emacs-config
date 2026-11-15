// Package reusewhy names why a test unit runs rather than being reused, as
// a run's reuse lines tally it ("2 failed before, 1 inputs changed"): the one
// vocabulary the runner prints (internal/reuse) and the daemon reads back
// from a merge gate's log (agent-repld/internal/merge's reuse fact).
package reusewhy

const (
	// FailedBefore: the unit's last outcome was a failure.
	FailedBefore = "failed before"
	// InputsChanged: its inputs differ from every green record's.
	InputsChanged = "inputs changed"
	// NeverRecorded: no run has recorded an outcome of the unit.
	NeverRecorded = "never recorded"
	// NoReuse: the run reuses nothing (--no-reuse, --record, coverage).
	NoReuse = "not reusing"
	// Targeted: a run narrowed below whole units neither reuses nor records.
	Targeted = "targeted"
	// Never: the unit, or its suite, is never reused (roster).
	Never = "never reused"
	// NoSandbox: the host cannot confine a unit, so nothing is reused.
	NoSandbox = "no sandbox"
)

// All is every reason, in the order a tally names them.
var All = []string{FailedBefore, InputsChanged, NeverRecorded, NoReuse, Targeted, Never, NoSandbox}
