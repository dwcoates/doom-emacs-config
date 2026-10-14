package cpugov

import (
	"fmt"
	"runtime"
	"strconv"
	"sync"
)

// FixedHostLoadEnv names the environment override that makes the governor
// read a FIXED host load instead of the host's own processor ticks. It exists
// for test harnesses: the host's load is the one input of the governor no
// test controls, so a suite whose merges test behind their head pins it
// rather than inherit whatever else the machine is running.
const FixedHostLoadEnv = "AGENT_REPL_FIXED_HOST_LOAD"

// Fixed is a Sampler whose every interval is busy for exactly Utilization of
// its ticks, on CPUs processors.
type Fixed struct {
	Utilization float64
	CPUs        int

	mu    sync.Mutex
	ticks Ticks
}

// Ticks advances the counters by one interval at the fixed load.
func (f *Fixed) Ticks() (Ticks, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ticks.Busy += uint64(f.Utilization * 1000)
	f.ticks.Total += 1000
	f.ticks.CPUs = f.CPUs
	return f.ticks, nil
}

// HostSampler answers the sampler the daemon's governor reads: the host's own
// ticks, or -- FIXED set, the value of FixedHostLoadEnv -- a Fixed load on
// this machine's processor count. A value that is not a fraction in [0, 1] is
// refused: a misspelled override must not quietly read as the real host.
func HostSampler(fixed string) (Sampler, error) {
	if fixed == "" {
		return &Host{}, nil
	}
	load, err := strconv.ParseFloat(fixed, 64)
	if err != nil || load < 0 || load > 1 {
		return nil, fmt.Errorf("cpugov: %s=%q is not a host load in [0, 1]", FixedHostLoadEnv, fixed)
	}
	return &Fixed{Utilization: load, CPUs: runtime.NumCPU()}, nil
}
