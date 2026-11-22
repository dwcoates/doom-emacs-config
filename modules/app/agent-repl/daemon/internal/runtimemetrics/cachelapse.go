package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/ids"
	"agent-repld/internal/vendorspend"
	"agent-repld/internal/wsm"
)

// The prompt cache's series (cachelapse.Telemetry).
const (
	// KeepaliveLatenessName is how long after it came due each asked-for
	// keep-alive went out, by how the ask ended. Its count per outcome is how
	// many keep-alives each ended as.
	KeepaliveLatenessName = "agentrepl_keepalive_lateness_seconds"
	// CacheLapsesName counts recorded cache lapses coming due, by what became
	// of them.
	CacheLapsesName = "agentrepl_cache_lapses_total"
	// ColdGateIdleName is how long a cache sat idle before a cold gate was
	// raised over it, by the gate's kind. Its count per outcome is how many
	// gates were raised, and `lapsed_5m` is a cache the vendor sold at the
	// five-minute lifetime.
	ColdGateIdleName = "agentrepl_cold_gate_idle_seconds"
	// KeepaliveRunBeatsName is how many keep-alives each ended keep-alive run
	// sent, by how it ended. Its count per end is how many runs ended so.
	KeepaliveRunBeatsName = "agentrepl_keepalive_run_beats"
	// KeepaliveRunTokensName is the tokens ended keep-alive runs spent, by
	// how each ended and the kind of token.
	KeepaliveRunTokensName = "agentrepl_keepalive_run_tokens_total"
)

// runBeatsBuckets span a run of one keep-alive to one far past the run cap
// (cachelapse.KeepaliveRunCap, 19), which only a run from before the cap
// existed can reach.
var runBeatsBuckets = []float64{1, 2, 3, 5, 8, 12, 16, 19, 25, 35, 50}

// latenessBuckets span a beat sent on time to one sent hours after it was due.
var latenessBuckets = []float64{1, 5, 15, 30, 60, 300, 900, 1800, 3600, 7200, 21600}

// idleBuckets span a cache gone cold within minutes to one idle for days.
var idleBuckets = []float64{60, 300, 900, 1800, 3600, 7200, 14400, 28800, 57600, 86400, 172800}

// cacheTelemetry holds the prompt cache's series; each observation is
// client_golang's own atomic update.
type cacheTelemetry struct {
	lateness, coldIdle, runBeats *prometheus.HistogramVec
	lapses, runTokens            *prometheus.CounterVec
}

func newCacheTelemetry() cacheTelemetry {
	return cacheTelemetry{
		lateness: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: KeepaliveLatenessName, Help: "How long after it came due a keep-alive went out, by how it ended.", Buckets: latenessBuckets,
		}, []string{"outcome", "workspace"}),
		lapses: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: CacheLapsesName, Help: "Recorded cache lapses that came due, by what became of them.",
		}, []string{"outcome", "workspace"}),
		coldIdle: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: ColdGateIdleName, Help: "How long a cache sat idle before a cold gate was raised over it, by the gate's kind.", Buckets: idleBuckets,
		}, []string{"outcome", "workspace"}),
		runBeats: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: KeepaliveRunBeatsName, Help: "How many keep-alives an ended keep-alive run sent, by how it ended.", Buckets: runBeatsBuckets,
		}, []string{"end", "workspace"}),
		runTokens: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: KeepaliveRunTokensName, Help: "Tokens ended keep-alive runs spent, by how each ended and the kind of token.",
		}, []string{"end", "kind", "workspace"}),
	}
}

// ObserveBeat implements cachelapse.Telemetry. The off surface records nothing.
func (s *Surface) ObserveBeat(ws ids.WorkspaceID, outcome string, late time.Duration) {
	if s == nil {
		return
	}
	s.cache.lateness.WithLabelValues(outcome, string(ws)).Observe(late.Seconds())
}

// CountLapse implements cachelapse.Telemetry. The off surface records nothing.
func (s *Surface) CountLapse(ws ids.WorkspaceID, outcome string) {
	if s == nil {
		return
	}
	s.cache.lapses.WithLabelValues(outcome, string(ws)).Inc()
}

// ObserveColdGate implements cachelapse.Telemetry. The off surface records
// nothing.
func (s *Surface) ObserveColdGate(ws ids.WorkspaceID, outcome string, idle time.Duration) {
	if s == nil {
		return
	}
	s.cache.coldIdle.WithLabelValues(outcome, string(ws)).Observe(idle.Seconds())
}

// ObserveKeepaliveRun implements cachelapse.Telemetry. The off surface
// records nothing.
func (s *Surface) ObserveKeepaliveRun(ws ids.WorkspaceID, run wsm.KeepaliveRun) {
	if s == nil {
		return
	}
	end := string(run.End)
	s.cache.runBeats.WithLabelValues(end, string(ws)).Observe(float64(run.Beats))
	for kind, n := range map[string]uint64{
		vendorspend.KindUncachedInput: run.Spent.Uncached,
		vendorspend.KindCacheRead:     run.Spent.CacheRead,
		vendorspend.KindCacheWrite5m:  run.Spent.Write5m,
		vendorspend.KindCacheWrite1h:  run.Spent.Write1h,
		vendorspend.KindOutput:        run.Spent.Output,
	} {
		s.cache.runTokens.WithLabelValues(end, kind, string(ws)).Add(float64(n))
	}
}
