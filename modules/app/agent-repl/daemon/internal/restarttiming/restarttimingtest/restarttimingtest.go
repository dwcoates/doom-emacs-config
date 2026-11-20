// Package restarttimingtest is a restarttiming.Recorder for tests: it keeps
// every observation, so a test asserts what a restart recorded.
package restarttimingtest

import (
	"sync"
	"time"

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

// Window is one resource window a Windows was asked to open: what it
// watches, and whether it was closed.
type Window struct {
	Restart restarttiming.Restart
	Phase   string
	Bound   time.Duration
	Watch   []restarttiming.Watched
	Closed  bool
}

// Windows keeps every resource window it is asked to open, in order, and
// marks each closed when its close is called. The zero value is ready.
type Windows struct {
	mu      sync.Mutex
	windows []*Window
}

// OpenWindow implements restarttiming.Windows.
func (w *Windows) OpenWindow(r restarttiming.Restart, phase string, bound time.Duration, watch ...restarttiming.Watched) func() {
	opened := &Window{Restart: r, Phase: phase, Bound: bound, Watch: append([]restarttiming.Watched(nil), watch...)}
	w.mu.Lock()
	w.windows = append(w.windows, opened)
	w.mu.Unlock()
	return func() {
		w.mu.Lock()
		defer w.mu.Unlock()
		opened.Closed = true
	}
}

// Windows answers a copy of every window opened so far, in order.
func (w *Windows) Windows() []Window {
	w.mu.Lock()
	defer w.mu.Unlock()
	out := make([]Window, 0, len(w.windows))
	for _, opened := range w.windows {
		out = append(out, *opened)
	}
	return out
}
