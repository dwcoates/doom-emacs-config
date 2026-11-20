package procmetrics

import (
	"context"
	"errors"
	"sync"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming"

	"github.com/prometheus/client_golang/prometheus"
)

// The series a resource window is recorded in.
const (
	// WindowCPUName is the CPU, in cores, a process group used while a
	// restart's phase ran: its mean over the window, and its peak sample.
	WindowCPUName = "agentrepl_restart_window_cpu_cores"
	// WindowResidentName is a process group's resident memory while a
	// restart's phase ran: its mean over the window, and its peak sample.
	WindowResidentName = "agentrepl_restart_window_resident_bytes"
)

// WindowSampleEvery is how often the watched processes are read while any
// window is open. One read is one proc_pid_rusage per watched process
// (measured 2026-11-20: 1.4µs of CPU each), and every
// windowClassifyEvery-th read also lists and classifies the host's processes
// (1.45ms of CPU over 739 processes), so the reads cost about 0.08% of a core
// while a window is open and nothing while none is.
const WindowSampleEvery = 500 * time.Millisecond

// windowClassifyEvery is how many window reads share one classification of
// the host's processes: a process born inside a window (a session's fresh
// shim) is measured from the next classification on.
const windowClassifyEvery = 4

// The stats a window records.
const (
	statMean = "mean"
	statMax  = "max"
)

// coreBuckets span an idle process to a build using every core.
var coreBuckets = []float64{0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 12, 16}

// residentBuckets span a small helper to a WebKit content process at its
// largest.
var residentBuckets = prometheus.ExponentialBuckets(16<<20, 2, 10)

// cpuRead is one process's CPU at one read.
type cpuRead struct {
	start int64
	cpu   float64
	at    time.Time
}

// groupStat accumulates one process group's samples inside one window.
type groupStat struct {
	coreSamples             int
	coreSum, coreMax        float64
	residentSamples         int
	residentSum, residentMx float64
}

// window is one open resource window.
type window struct {
	restart restarttiming.Restart
	phase   string
	watch   []restarttiming.Watched
	opened  time.Time
	bound   time.Duration
	closed  bool
	stats   map[string]*groupStat
}

func (w *window) watches(g Group) bool {
	for _, watched := range w.watch {
		if watched.Process == g.Process && (watched.Workspace == "" || string(watched.Workspace) == g.Workspace) {
			return true
		}
	}
	return false
}

// Windows measures the processes a restart's phase watches for as long as it
// runs (restarttiming.Windows). It reads them every WindowSampleEvery while
// any window is open, and records each window once when it closes: the mean
// and peak CPU in cores and resident memory of every watched group, so the
// store keeps one record per window however fast the reads ran.
type Windows struct {
	c   *Collector
	clk clock.Clock
	log dlog.Logger

	cores, resident *prometheus.HistogramVec

	mu     sync.Mutex
	open   []*window
	reads  int
	prev   map[int]cpuRead
	wake   chan struct{}
	active bool
}

var _ restarttiming.Windows = (*Windows)(nil)

// NewWindows answers the windows measuring through c's reader and its
// classification, on clk.
func NewWindows(c *Collector, clk clock.Clock) *Windows {
	labels := []string{"restart", "phase", "process", "stat"}
	return &Windows{
		c: c, clk: clk, log: c.deps.Log,
		cores: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: WindowCPUName, Help: "A process group's CPU in cores while a restart's phase ran, its mean and its peak sample.", Buckets: coreBuckets,
		}, labels),
		resident: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: WindowResidentName, Help: "A process group's resident memory while a restart's phase ran, its mean and its peak sample.", Buckets: residentBuckets,
		}, labels),
		prev: map[int]cpuRead{},
		wake: make(chan struct{}, 1),
	}
}

// Collectors answers the series the windows record, for the daemon's
// registry.
func (ws *Windows) Collectors() []prometheus.Collector {
	return []prometheus.Collector{ws.cores, ws.resident}
}

// OpenWindow implements restarttiming.Windows. The close it answers marks the
// window closed; the sampler takes one last read and records it.
func (ws *Windows) OpenWindow(r restarttiming.Restart, phase string, bound time.Duration, watch ...restarttiming.Watched) func() {
	w := &window{restart: r, phase: phase, watch: watch, opened: ws.clk.Now(), bound: bound, stats: map[string]*groupStat{}}
	ws.mu.Lock()
	ws.open = append(ws.open, w)
	ws.mu.Unlock()
	ws.poke()
	return sync.OnceFunc(func() {
		ws.mu.Lock()
		w.closed = true
		ws.mu.Unlock()
		ws.poke()
	})
}

// poke wakes the sampler without blocking.
func (ws *Windows) poke() {
	select {
	case ws.wake <- struct{}{}:
	default:
	}
}

// Run reads the watched processes every WindowSampleEvery while any window
// is open, and at once when one opens or closes, until ctx ends.
func (ws *Windows) Run(ctx context.Context) error {
	for {
		var tick <-chan time.Time
		if ws.Sample() {
			tick = ws.clk.After(WindowSampleEvery)
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ws.wake:
		case <-tick:
		}
	}
}

// Sample takes one read of every watched process, folds it into each open
// window, records and drops each closed window, and drops each window past
// its bound unrecorded. It answers whether any window is still open. With no
// window open it reads nothing, and forgets its last reads, so the next
// window's first read is a baseline rather than a span reaching back to the
// last window.
func (ws *Windows) Sample() bool {
	ws.mu.Lock()
	if len(ws.open) == 0 {
		ws.prev, ws.reads, ws.active = map[int]cpuRead{}, 0, false
		ws.mu.Unlock()
		return false
	}
	classify := ws.reads%windowClassifyEvery == 0 || !ws.active
	ws.reads++
	ws.active = true
	windows := append([]*window(nil), ws.open...)
	ws.mu.Unlock()

	if classify {
		ws.c.sample()
	}
	ws.c.mu.Lock()
	seen := make(map[int]pidState, len(ws.c.seen))
	for pid, s := range ws.c.seen {
		seen[pid] = s
	}
	ws.c.mu.Unlock()

	now := ws.clk.Now()
	type groupRead struct {
		cores    float64
		hasCores bool
		resident float64
	}
	reads := map[Group]*groupRead{}
	var usageErr error
	ws.mu.Lock()
	for pid, s := range seen {
		if !watchedByAny(windows, s.group) {
			continue
		}
		u, err := ws.c.deps.Reader.Usage(pid)
		if err != nil {
			if !errors.Is(err, ErrGone) {
				usageErr = err
			}
			delete(ws.prev, pid)
			continue
		}
		g := reads[s.group]
		if g == nil {
			g = &groupRead{}
			reads[s.group] = g
		}
		g.resident += float64(u.ResidentBytes)
		if prev, ok := ws.prev[pid]; ok && prev.start == s.start && now.After(prev.at) {
			g.cores += nonNegative(u.CPUSeconds-prev.cpu) / now.Sub(prev.at).Seconds()
			g.hasCores = true
		}
		ws.prev[pid] = cpuRead{start: s.start, cpu: u.CPUSeconds, at: now}
	}
	for _, w := range windows {
		byProcess := map[string]*groupRead{}
		for g, r := range reads {
			if !w.watches(g) {
				continue
			}
			sum := byProcess[g.Process]
			if sum == nil {
				sum = &groupRead{}
				byProcess[g.Process] = sum
			}
			sum.resident += r.resident
			sum.cores += r.cores
			sum.hasCores = sum.hasCores || r.hasCores
		}
		for process, r := range byProcess {
			st := w.stats[process]
			if st == nil {
				st = &groupStat{}
				w.stats[process] = st
			}
			st.residentSamples++
			st.residentSum += r.resident
			st.residentMx = max(st.residentMx, r.resident)
			if r.hasCores {
				st.coreSamples++
				st.coreSum += r.cores
				st.coreMax = max(st.coreMax, r.cores)
			}
		}
	}
	var finished, bounded []*window
	kept := ws.open[:0]
	for _, w := range ws.open {
		switch {
		case w.closed:
			finished = append(finished, w)
		case now.Sub(w.opened) > w.bound:
			bounded = append(bounded, w)
		default:
			kept = append(kept, w)
		}
	}
	ws.open = kept
	stillOpen := len(kept) > 0
	ws.mu.Unlock()

	ws.c.note("window_usage", usageErr)
	for _, w := range finished {
		ws.record(w)
	}
	for _, w := range bounded {
		ws.log.Info(Operation, "a resource window stayed open past its bound; it is dropped unrecorded", dlog.Context{
			"restart": string(w.restart), "phase": w.phase, "bound_seconds": w.bound.Seconds(),
			"open_seconds": now.Sub(w.opened).Seconds(),
		})
	}
	return stillOpen
}

// watchedByAny reports whether any of windows watches g.
func watchedByAny(windows []*window, g Group) bool {
	for _, w := range windows {
		if w.watches(g) {
			return true
		}
	}
	return false
}

// record observes a closed window's stats, one per process group it read.
func (ws *Windows) record(w *window) {
	for process, st := range w.stats {
		labels := func(stat string) []string { return []string{string(w.restart), w.phase, process, stat} }
		if st.coreSamples > 0 {
			ws.cores.WithLabelValues(labels(statMean)...).Observe(st.coreSum / float64(st.coreSamples))
			ws.cores.WithLabelValues(labels(statMax)...).Observe(st.coreMax)
		}
		ws.resident.WithLabelValues(labels(statMean)...).Observe(st.residentSum / float64(st.residentSamples))
		ws.resident.WithLabelValues(labels(statMax)...).Observe(st.residentMx)
	}
}
