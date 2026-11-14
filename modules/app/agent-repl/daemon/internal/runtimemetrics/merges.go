package runtimemetrics

import (
	"time"

	"github.com/prometheus/client_golang/prometheus"
)

// The merge queue's series (merge.Meter).
const (
	// MergeStepName is how long a merge spent on each step before it left it.
	MergeStepName = "agentrepl_merge_step_seconds"
	// MergeSpanName is each merge's pick, queued, running and total spans,
	// by how it ended.
	MergeSpanName = "agentrepl_merge_seconds"
	// MediationSpanName is each deploy mediation's rollout, assessment and
	// total spans, by how it ended (merge's mediation outcomes). Its total
	// span's count per outcome is how many mediations ended so: how often
	// the mediator found nothing, fixed and redeployed, or escalated.
	MediationSpanName = "agentrepl_deploy_mediation_seconds"
	// MediationChainRedeploysName is how many remediations were deployed in
	// a row before the queue advanced, per chain, by how its last mediation
	// ended. Its count per outcome is how many chains ended so.
	MediationChainRedeploysName = "agentrepl_deploy_remediation_chain_redeploys"
	// MediationChainSpanName is how long each chain held the queue, from
	// its first deploy to the queue advancing, by how its last ended.
	MediationChainSpanName = "agentrepl_deploy_remediation_chain_seconds"
	// RemediationsInARowName is the chain owed now: remediations deployed
	// in a row since the queue last advanced, 0 when none is owed.
	RemediationsInARowName = "agentrepl_deploy_remediations_in_a_row"
	// MergeRoundName is how long each round of a merge's tab took (a
	// rebase, a conflict resolution, a test gate, a fixing attempt), by its
	// ledger outcome.
	MergeRoundName = "agentrepl_merge_round_seconds"
	// MergeRoundsName is how many rounds of each repair kind a concluded
	// merge took, by how the merge ended.
	MergeRoundsName = "agentrepl_merge_rounds"
	// QueueChangesName counts every change to a queue's line, by its kind
	// and, for a merge leaving, its cause.
	QueueChangesName = "agentrepl_merge_queue_changes_total"
)

// roundCountBuckets span a clean merge's single round to a merge that
// rebased or fixed past its bound.
var roundCountBuckets = []float64{0, 1, 2, 3, 4, 5, 6, 8, 10, 15, 20}

// mergeBuckets span a merge stage from an instant step to a long test gate.
var mergeBuckets = []float64{0.1, 0.5, 1, 2, 5, 10, 20, 30, 60, 120, 300, 600, 1200, 2400, 3600}

// merges holds the merge queue's histograms; each observation is
// client_golang's own atomic add.
type merges struct {
	steps, spans, mediations, chainRedeploys, chainSpans, rounds, roundCounts *prometheus.HistogramVec
	inARow                                                                    prometheus.Gauge
	changes                                                                   *prometheus.CounterVec
}

func newMerges() merges {
	return merges{
		steps: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeStepName, Help: "How long a merge spent on a step before it left it, by step.", Buckets: mergeBuckets,
		}, []string{"step"}),
		spans: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeSpanName, Help: "A merge's pick, queued, running and total spans, by span and outcome.", Buckets: mergeBuckets,
		}, []string{"span", "outcome"}),
		mediations: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MediationSpanName, Help: "A deploy mediation's rollout, assessment and total spans, by span and how it ended.", Buckets: mergeBuckets,
		}, []string{"span", "outcome"}),
		chainRedeploys: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MediationChainRedeploysName, Help: "Remediations deployed in a row before the queue advanced, per chain, by how its last mediation ended.", Buckets: roundCountBuckets,
		}, []string{"outcome"}),
		chainSpans: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MediationChainSpanName, Help: "How long a chain of mediations held the queue, from its first deploy, by how its last ended.", Buckets: mergeBuckets,
		}, []string{"outcome"}),
		inARow: prometheus.NewGauge(prometheus.GaugeOpts{
			Name: RemediationsInARowName, Help: "Remediations deployed in a row since the queue last advanced; 0 when no mediation is owed.",
		}),
		rounds: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeRoundName, Help: "How long a round of a merge's tab took, by tab kind and ledger outcome.", Buckets: mergeBuckets,
		}, []string{"round", "outcome"}),
		roundCounts: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeRoundsName, Help: "How many rounds of a repair tab kind a concluded merge took, by kind and how the merge ended.", Buckets: roundCountBuckets,
		}, []string{"round", "outcome"}),
		changes: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: QueueChangesName, Help: "Changes to a merge queue's line, by kind and, for a merge leaving, its cause.",
		}, []string{"change", "cause"}),
	}
}

// ObserveMergeStep implements merge.Meter. The off surface records nothing.
func (s *Surface) ObserveMergeStep(step string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.steps.WithLabelValues(step).Observe(took.Seconds())
}

// ObserveMergeSpan implements merge.Meter. The off surface records nothing.
func (s *Surface) ObserveMergeSpan(span, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.spans.WithLabelValues(span, outcome).Observe(took.Seconds())
}

// ObserveMediationSpan implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveMediationSpan(span, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.mediations.WithLabelValues(span, outcome).Observe(took.Seconds())
}

// ObserveMediationChain implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveMediationChain(outcome string, redeploys int, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.chainRedeploys.WithLabelValues(outcome).Observe(float64(redeploys))
	s.merges.chainSpans.WithLabelValues(outcome).Observe(took.Seconds())
}

// SetRemediationsInARow implements merge.Meter. The off surface records
// nothing.
func (s *Surface) SetRemediationsInARow(n int) {
	if s == nil {
		return
	}
	s.merges.inARow.Set(float64(n))
}

// ObserveMergeRound implements merge.Meter. The off surface records nothing.
func (s *Surface) ObserveMergeRound(kind, outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.rounds.WithLabelValues(kind, outcome).Observe(took.Seconds())
}

// ObserveMergeRounds implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveMergeRounds(kind, outcome string, n int) {
	if s == nil {
		return
	}
	s.merges.roundCounts.WithLabelValues(kind, outcome).Observe(float64(n))
}

// CountQueueChange implements merge.Meter. The off surface records nothing.
func (s *Surface) CountQueueChange(change, cause string) {
	if s == nil {
		return
	}
	s.merges.changes.WithLabelValues(change, cause).Inc()
}
