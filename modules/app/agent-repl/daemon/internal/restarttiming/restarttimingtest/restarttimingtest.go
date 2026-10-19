// Package restarttimingtest is a restarttiming.Recorder for tests: it keeps
// every observation, so a test asserts what a restart recorded.
package restarttimingtest

import (
	"sync"

	"agent-repld/internal/restarttiming"
)

// Recorder keeps every observation it is given, in order. The zero value is
// ready.
type Recorder struct {
	mu  sync.Mutex
	obs []restarttiming.Observation
}

// ObserveRestart implements restarttiming.Recorder.
func (r *Recorder) ObserveRestart(o restarttiming.Observation) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.obs = append(r.obs, o)
}

// Observations answers a copy of every observation so far.
func (r *Recorder) Observations() []restarttiming.Observation {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]restarttiming.Observation(nil), r.obs...)
}

// Of answers the observations of one restart, in order.
func (r *Recorder) Of(restart restarttiming.Restart) []restarttiming.Observation {
	var out []restarttiming.Observation
	for _, o := range r.Observations() {
		if o.Restart() == restart {
			out = append(out, o)
		}
	}
	return out
}
