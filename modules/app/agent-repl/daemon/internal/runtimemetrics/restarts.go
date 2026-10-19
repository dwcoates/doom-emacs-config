package runtimemetrics

import (
	"sort"
	"sync"

	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"

	"github.com/prometheus/client_golang/prometheus"
)

// The restart series (internal/restarttiming).
const (
	// RestartName is the histogram every timed restart and restart phase
	// lands in, labeled by `restart`, `phase`, `cause` and `outcome`.
	RestartName = "agentrepl_restart_seconds"
	// WorkspaceRestartLastName is the last duration of each per-workspace
	// restart phase, labeled by `restart`, `phase` and `workspace`, so the
	// slowest workspace of the latest restart can be named.
	WorkspaceRestartLastName = "agentrepl_workspace_restart_last_seconds"
)

// restartBuckets span a restart phase from a fast step to a slow deploy.
var restartBuckets = []float64{0.1, 0.25, 0.5, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233}

type restartKey struct {
	restart restarttiming.Restart
	phase   string
	cause   string
	outcome restarttiming.Outcome
}

type workspaceRestartKey struct {
	restart   restarttiming.Restart
	phase     string
	workspace ids.WorkspaceID
}

// restarts holds the restart histograms and each workspace's last restart
// durations. Like latencies, it is a prometheus.Collector reading only its
// own state.
type restarts struct {
	mu     sync.Mutex
	series map[restartKey]*histogram
	last   map[workspaceRestartKey]float64

	seriesDesc, lastDesc *prometheus.Desc
}

func newRestarts() *restarts {
	return &restarts{
		series: map[restartKey]*histogram{},
		last:   map[workspaceRestartKey]float64{},
		seriesDesc: prometheus.NewDesc(RestartName,
			"How long a restart or one of its phases took (internal/restarttiming), by restart, phase, cause and outcome.",
			[]string{"restart", "phase", "cause", "outcome"}, nil),
		lastDesc: prometheus.NewDesc(WorkspaceRestartLastName,
			"How long a workspace's latest restart phase took, by restart, phase and workspace.",
			[]string{"restart", "phase", "workspace"}, nil),
	}
}

func (r *restarts) observe(o restarttiming.Observation) {
	r.mu.Lock()
	defer r.mu.Unlock()
	key := restartKey{restart: o.Restart(), phase: o.Phase(), cause: o.Cause(), outcome: o.Outcome()}
	h := r.series[key]
	if h == nil {
		h = newHistogram(restartBuckets)
		r.series[key] = h
	}
	h.observe(o.Seconds())
	if ws := o.Workspace(); ws != "" {
		r.last[workspaceRestartKey{restart: o.Restart(), phase: o.Phase(), workspace: ws}] = o.Seconds()
	}
}

// forget deletes ws's last durations: the workspace left the registry.
func (r *restarts) forget(ws ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for key := range r.last {
		if key.workspace == ws {
			delete(r.last, key)
		}
	}
}

// Describe implements prometheus.Collector.
func (r *restarts) Describe(ch chan<- *prometheus.Desc) {
	ch <- r.seriesDesc
	ch <- r.lastDesc
}

// Collect implements prometheus.Collector.
func (r *restarts) Collect(ch chan<- prometheus.Metric) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for key, h := range r.series {
		ch <- h.metric(r.seriesDesc, string(key.restart), key.phase, key.cause, string(key.outcome))
	}
	keys := make([]workspaceRestartKey, 0, len(r.last))
	for k := range r.last {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].restart != keys[j].restart {
			return keys[i].restart < keys[j].restart
		}
		if keys[i].phase != keys[j].phase {
			return keys[i].phase < keys[j].phase
		}
		return keys[i].workspace < keys[j].workspace
	})
	for _, k := range keys {
		ch <- prometheus.MustNewConstMetric(r.lastDesc, prometheus.GaugeValue, r.last[k], string(k.restart), k.phase, string(k.workspace))
	}
}

// ObserveRestart implements restarttiming.Recorder. The off surface records
// nothing.
func (s *Surface) ObserveRestart(o restarttiming.Observation) {
	if s == nil {
		return
	}
	s.restarts.observe(o)
}
