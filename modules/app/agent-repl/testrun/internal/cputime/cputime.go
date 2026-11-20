// Package cputime is each running unit's CPU over time: what every process
// of the unit's tree used between two reads, so each test can be given the
// CPU of the intervals it ran in (testtimes.AttributeCPU).
//
// EVERY PROCESS OF A UNIT CARRIES ITS TAG. The runner starts each unit with
// UnitEnv set to the unit's tag (Tag), which every process the unit starts
// inherits, whatever group or session it moves to, as the CPU governor's mark
// is (cores.NestedEnv). A read scans the processes carrying UnitEnv and counts
// what each tag's tree used since the last read (cores.TestsCPU.ReadByMark),
// a reaped child's use once, through its parent.
package cputime

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"time"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/testtimes"
)

// UnitEnv tags a unit's process tree with the unit's tag.
const UnitEnv = "AGENT_REPL_TESTRUN_UNIT"

// Every is how often the sampler reads. A test shorter than it shares its
// interval's CPU with the tests beside it by how long each ran, so a shorter
// interval tells short tests apart better; each read scans every process of
// the user once (proctable.Marked), so a longer one costs less.
const Every = 500 * time.Millisecond

// Sampler reads every tagged unit's CPU at each tick, keeping each tag's
// intervals until the runner takes them.
type Sampler struct {
	// Read answers the CPU each tag's processes used since the last read
	// (cores.TestsCPU.ReadByMark over proctable.Marked of UnitEnv).
	Read  func() (map[string]float64, error)
	Now   func() time.Time
	Ticks <-chan time.Time
	Log   *run.Log
	// Pid tells this run's tags from every other run's on the host.
	Pid int

	mu     sync.Mutex
	last   time.Time
	primed bool
	slices map[string][]testtimes.CPUSlice
}

// Tag is the tag of unit ID in this run.
func (s *Sampler) Tag(id string) string { return fmt.Sprintf("%d/%s", s.Pid, id) }

// Env is the environment entry that tags unit ID's tree.
func (s *Sampler) Env(id string) string { return UnitEnv + "=" + s.Tag(id) }

// Run reads at every tick until ctx ends. The first read only takes the
// baseline.
func (s *Sampler) Run(ctx context.Context) {
	s.read()
	for {
		select {
		case <-ctx.Done():
			return
		case <-s.Ticks:
			s.read()
		}
	}
}

// read takes one read. A failed read is logged and leaves the last good
// read standing, so the next one counts everything since it.
func (s *Sampler) read() {
	growth, err := s.Read()
	now := s.Now()
	if err != nil {
		s.Log.Errorf("cputime: reading the units' CPU at %s failed, so the interval since the last read at %s is counted by the next read: %v",
			now.Format(time.RFC3339Nano), s.last.Format(time.RFC3339Nano), err)
		return
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.slices == nil {
		s.slices = map[string][]testtimes.CPUSlice{}
	}
	if s.primed {
		own := fmt.Sprintf("%d/", s.Pid)
		for tag, cpu := range growth {
			// Every run's units carry UnitEnv; only this run's are kept.
			if cpu == 0 || !strings.HasPrefix(tag, own) {
				continue
			}
			s.slices[tag] = append(s.slices[tag], testtimes.CPUSlice{From: s.last, To: now, CPU: cpu})
		}
	}
	s.last, s.primed = now, true
}

// Take answers unit ID's intervals and forgets them.
func (s *Sampler) Take(id string) []testtimes.CPUSlice {
	s.mu.Lock()
	defer s.mu.Unlock()
	tag := s.Tag(id)
	out := s.slices[tag]
	delete(s.slices, tag)
	return out
}
