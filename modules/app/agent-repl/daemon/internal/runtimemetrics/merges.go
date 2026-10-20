package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"
)

// The merge queue's series (merge.Timer).
const (
	// MergeStepName is how long a merge spent on each step before it left it.
	MergeStepName = "agentrepl_merge_step_seconds"
	// MergeSpanName is each merge's pick, queued, running and total spans,
	// by how it ended.
	MergeSpanName = "agentrepl_merge_seconds"
)

// mergeBuckets span a merge stage from an instant step to a long test gate.
var mergeBuckets = []float64{0.1, 0.5, 1, 2, 5, 10, 20, 30, 60, 120, 300, 600, 1200, 2400, 3600}

// merges holds the merge queue's histograms; each observation is
// client_golang's own atomic add.
type merges struct {
	steps, spans *prometheus.HistogramVec
}

func newMerges() merges {
	return merges{
		steps: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeStepName, Help: "How long a merge spent on a step before it left it, by step.", Buckets: mergeBuckets,
		}, []string{"step"}),
		spans: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeSpanName, Help: "A merge's pick, queued, running and total spans, by span and outcome.", Buckets: mergeBuckets,
		}, []string{"span", "outcome"}),
	}
}

// ObserveMergeStep implements merge.Timer. The off surface records nothing.
func (s *Surface) ObserveMergeStep(step string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.steps.WithLabelValues(step).Observe(took.Seconds())
}

// ObserveMergeSpan implements merge.Timer. The off surface records nothing.
func (s *Surface) ObserveMergeSpan(span, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.spans.WithLabelValues(span, outcome).Observe(took.Seconds())
}
