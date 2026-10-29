package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"

	"agent-repld/internal/ids"
)

// The shim lifecycle's series: why shims start and how long they take to load,
// why they are stood down, and how cold gates are answered, per workspace
// (docs/developer/shim-lifecycle.md).
const (
	// ShimStartName is how long a shim start took, from the fleet taking it to
	// the session up or the start failing, by why it started (shimlife
	// StartCause) and how it ended. Its count per cause is how many starts
	// each cause made.
	ShimStartName = "agentrepl_shim_start_seconds"
	// ShimStandDownsName counts shims stood down on purpose, by why
	// (shimlife StandDownReason).
	ShimStandDownsName = "agentrepl_shim_stand_downs_total"
	// ColdGateAnswersName counts cold gates answered, by the choice and how
	// the answer ended.
	ColdGateAnswersName = "agentrepl_cold_gate_answers_total"
)

// startBuckets span a shim adopted in milliseconds to a cold resume that
// waits minutes on its vendor.
var startBuckets = []float64{0.1, 0.25, 0.5, 1, 2, 3, 5, 8, 13, 21, 34, 60, 120, 300}

// shimLifeTelemetry holds the shim lifecycle's series; each observation is
// client_golang's own atomic update.
type shimLifeTelemetry struct {
	starts      *prometheus.HistogramVec
	standDowns  *prometheus.CounterVec
	gateAnswers *prometheus.CounterVec
}

func newShimLifeTelemetry() shimLifeTelemetry {
	return shimLifeTelemetry{
		starts: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: ShimStartName, Help: "How long a shim start took, by why it started and how it ended.", Buckets: startBuckets,
		}, []string{"cause", "outcome", "workspace"}),
		standDowns: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ShimStandDownsName, Help: "Shims stood down on purpose, by why.",
		}, []string{"reason", "workspace"}),
		gateAnswers: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ColdGateAnswersName, Help: "Cold gates answered, by the choice and how the answer ended.",
		}, []string{"choice", "outcome", "workspace"}),
	}
}

// ObserveShimStart records one shim start of WS. The off surface records
// nothing.
func (s *Surface) ObserveShimStart(ws ids.WorkspaceID, cause, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.shimLife.starts.WithLabelValues(cause, outcome, string(ws)).Observe(took.Seconds())
}

// CountShimStandDown records one shim of WS stood down on purpose. The off
// surface records nothing.
func (s *Surface) CountShimStandDown(ws ids.WorkspaceID, reason string) {
	if s == nil {
		return
	}
	s.shimLife.standDowns.WithLabelValues(reason, string(ws)).Inc()
}

// CountColdGateAnswer records one cold gate of WS answered. The off surface
// records nothing.
func (s *Surface) CountColdGateAnswer(ws ids.WorkspaceID, choice, outcome string) {
	if s == nil {
		return
	}
	s.shimLife.gateAnswers.WithLabelValues(choice, outcome, string(ws)).Inc()
}
