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

// RestartAffectedName is the histogram of how many workspaces and shim
// sessions each restart affected, labeled by `cause` and `phase`.
const RestartAffectedName = "agentrepl_restart_affected"

// affectedBuckets span one restart's affected count from a few to the 144
// workspaces measured expected back per handover on 2026-11-21.
var affectedBuckets = []float64{1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377}

// reconnectBuckets are workspace_reconnect's: restartBuckets widened above
// 233s, which 374 of the 1,048 cold-boot web samples in the 30 days to
// 2026-11-22 exceeded (so every quantile read 233s). Fibonacci steps continue
// to 1597s so the cold-boot web quantiles resolve; the handover's p50 (about
// 9.5s) and p99 (about 109s) keep the buckets they had.
var reconnectBuckets = append(append([]float64(nil), restartBuckets...), 377, 610, 987, 1597)

// bucketsOf answers the bucket bounds of a restart's histogram.
func bucketsOf(restart restarttiming.Restart) []float64 {
	if restart == restarttiming.WorkspaceReconnect {
		return reconnectBuckets
	}
	return restartBuckets
}

// restartBuckets span a restart phase from a fast step to a slow deploy.
var restartBuckets = []float64{0.1, 0.25, 0.5, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233}

type restartKey struct {
	restart restarttiming.Restart
	phase   string
	cause   string
	outcome restarttiming.Outcome
}

type affectedKey struct {
	kind  restarttiming.AffectedKind
	cause restarttiming.ReconnectCause
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
	// affected is each restart's affected count.
	affected map[affectedKey]*histogram

	seriesDesc, lastDesc, affectedDesc *prometheus.Desc
}

func newRestarts() *restarts {
	return &restarts{
		series:   map[restartKey]*histogram{},
		last:     map[workspaceRestartKey]float64{},
		affected: map[affectedKey]*histogram{},
		affectedDesc: prometheus.NewDesc(RestartAffectedName,
			"How many workspaces (host, web) or shim sessions (shim) one restart affected, by cause and phase.",
			[]string{"cause", "phase"}, nil),
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
		h = newHistogram(bucketsOf(o.Restart()))
		r.series[key] = h
	}
	h.observe(o.Seconds())
	if ws := o.Workspace(); ws != "" {
		r.last[workspaceRestartKey{restart: o.Restart(), phase: o.Phase(), workspace: ws}] = o.Seconds()
	}
}

func (r *restarts) observeAffected(a restarttiming.Affected) {
	r.mu.Lock()
	defer r.mu.Unlock()
	key := affectedKey{kind: a.Kind(), cause: a.Cause()}
	h := r.affected[key]
	if h == nil {
		h = newHistogram(affectedBuckets)
		r.affected[key] = h
	}
	h.observe(float64(a.Count()))
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
	ch <- r.affectedDesc
}

// Collect implements prometheus.Collector.
func (r *restarts) Collect(ch chan<- prometheus.Metric) {
	r.mu.Lock()
	defer r.mu.Unlock()
	for key, h := range r.series {
		ch <- h.metric(r.seriesDesc, string(key.restart), key.phase, key.cause, string(key.outcome))
	}
	for key, h := range r.affected {
		ch <- h.metric(r.affectedDesc, string(key.cause), string(key.kind))
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

// ObserveAffected implements restarttiming.Recorder. The off surface
// records nothing.
func (s *Surface) ObserveAffected(a restarttiming.Affected) {
	if s == nil {
		return
	}
	s.restarts.observeAffected(a)
}
