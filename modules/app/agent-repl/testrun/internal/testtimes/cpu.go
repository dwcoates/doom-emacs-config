package testtimes

import (
	"fmt"
	"time"
)

// CPUSlice is the CPU a unit's process tree used over one interval
// (cputime.Sampler's reads).
type CPUSlice struct {
	From, To time.Time
	CPU      float64
	// Started is how many processes of the tree the interval's read saw for
	// the first time (cores.MarkGrowth.Started).
	Started int
}

// AttributeCPU gives each of a unit's tests its share of the unit's CPU, and
// of the processes its tree started (Test.Processes), each slice's processes
// split as its CPU is.
//
// UNIT is the CPU the unit's process and its reaped descendants used, as the
// kernel's wait accounting reports it (run.Usage.CPU), and SLICES what the
// sampler read of the unit's whole marked tree while it ran, between START and
// END. What the slices hold short of UNIT (the leader's use after the last
// read, which its unmarked parent reaped) is one more slice to the unit's end.
//
//   - When every test says when it started, each slice is split across the
//     tests running in it, by how long each ran in it: tests running side by
//     side share a slice evenly, and a slice no test ran in (the process's
//     start, a file's collection) is nobody's.
//   - When no test says, the tests ran one after another (a harness's
//     groups), and the unit's CPU is split by each test's seconds.
//
// A test that never ran (a skipped vitest test) says no start and took no
// time, and gets none. A unit whose tests say for some and not for others
// that took time broke its runner's contract, which is an error, and no test
// is given any CPU.
func AttributeCPU(tests []Test, slices []CPUSlice, unit float64, start, end time.Time) error {
	if len(tests) == 0 {
		return nil
	}
	timed, untimed := 0, 0
	for _, t := range tests {
		switch {
		case !t.Start.IsZero():
			timed++
		case t.Seconds > 0:
			untimed++
		}
	}
	if timed != 0 && untimed != 0 {
		return fmt.Errorf("testtimes: %d of the unit's tests say when they started and %d that took time do not; a unit's runner says for every test or for none", timed, untimed)
	}
	slices = withRemainder(slices, unit, start, end)
	if timed == 0 {
		bySeconds(tests, slices)
		return nil
	}
	for i := range tests {
		tests[i].CPU, tests[i].Processes, tests[i].HasCPU = 0, 0, true
	}
	for _, s := range slices {
		overlaps := make([]float64, len(tests))
		sum := 0.0
		for i, t := range tests {
			o := overlap(s.From, s.To, t.Start, t.Start.Add(seconds(t.Seconds)))
			overlaps[i] = o
			sum += o
		}
		if sum == 0 {
			continue
		}
		for i, o := range overlaps {
			tests[i].CPU += s.CPU * o / sum
			tests[i].Processes += float64(s.Started) * o / sum
		}
	}
	return nil
}

// withRemainder is SLICES with what UNIT holds beyond them as one more slice,
// from the last slice's end (START when there is none) to END.
func withRemainder(slices []CPUSlice, unit float64, start, end time.Time) []CPUSlice {
	sampled := 0.0
	from := start
	for _, s := range slices {
		sampled += s.CPU
		if s.To.After(from) {
			from = s.To
		}
	}
	if unit <= sampled {
		return slices
	}
	if !end.After(from) {
		from = end.Add(-time.Millisecond)
	}
	return append(append([]CPUSlice(nil), slices...), CPUSlice{From: from, To: end, CPU: unit - sampled})
}

// bySeconds splits the slices' CPU across TESTS by each one's seconds, evenly
// when none took any.
func bySeconds(tests []Test, slices []CPUSlice) {
	total, started, secs := 0.0, 0.0, 0.0
	for _, s := range slices {
		total += s.CPU
		started += float64(s.Started)
	}
	for _, t := range tests {
		secs += t.Seconds
	}
	for i := range tests {
		share := 1 / float64(len(tests))
		if secs > 0 {
			share = tests[i].Seconds / secs
		}
		tests[i].CPU, tests[i].Processes, tests[i].HasCPU = total*share, started*share, true
	}
}

// overlap is how long, in seconds, [aFrom, aTo] and [bFrom, bTo] share.
func overlap(aFrom, aTo, bFrom, bTo time.Time) float64 {
	from, to := aFrom, aTo
	if bFrom.After(from) {
		from = bFrom
	}
	if bTo.Before(to) {
		to = bTo
	}
	if !to.After(from) {
		return 0
	}
	return to.Sub(from).Seconds()
}

// seconds is S seconds as a duration.
func seconds(s float64) time.Duration { return time.Duration(s * float64(time.Second)) }
