package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"
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
)

// latenessBuckets span a beat sent on time to one sent hours after it was due.
var latenessBuckets = []float64{1, 5, 15, 30, 60, 300, 900, 1800, 3600, 7200, 21600}

// idleBuckets span a cache gone cold within minutes to one idle for days.
var idleBuckets = []float64{60, 300, 900, 1800, 3600, 7200, 14400, 28800, 57600, 86400, 172800}

// cacheTelemetry holds the prompt cache's series; each observation is
// client_golang's own atomic update.
type cacheTelemetry struct {
	lateness, coldIdle *prometheus.HistogramVec
	lapses             *prometheus.CounterVec
}

func newCacheTelemetry() cacheTelemetry {
	return cacheTelemetry{
		lateness: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: KeepaliveLatenessName, Help: "How long after it came due a keep-alive went out, by how it ended.", Buckets: latenessBuckets,
		}, []string{"outcome"}),
		lapses: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: CacheLapsesName, Help: "Recorded cache lapses that came due, by what became of them.",
		}, []string{"outcome"}),
		coldIdle: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: ColdGateIdleName, Help: "How long a cache sat idle before a cold gate was raised over it, by the gate's kind.", Buckets: idleBuckets,
		}, []string{"outcome"}),
	}
}

// ObserveBeat implements cachelapse.Telemetry. The off surface records nothing.
func (s *Surface) ObserveBeat(outcome string, late time.Duration) {
	if s == nil {
		return
	}
	s.cache.lateness.WithLabelValues(outcome).Observe(late.Seconds())
}

// CountLapse implements cachelapse.Telemetry. The off surface records nothing.
func (s *Surface) CountLapse(outcome string) {
	if s == nil {
		return
	}
	s.cache.lapses.WithLabelValues(outcome).Inc()
}

// ObserveColdGate implements cachelapse.Telemetry. The off surface records
// nothing.
func (s *Surface) ObserveColdGate(outcome string, idle time.Duration) {
	if s == nil {
		return
	}
	s.cache.coldIdle.WithLabelValues(outcome).Observe(idle.Seconds())
}
