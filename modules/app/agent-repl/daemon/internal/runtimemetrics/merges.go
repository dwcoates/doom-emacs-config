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
	// MediationSpanName is each deploy check's rollout, assessment and total
	// spans, by how it ended (merge's check outcomes). Its total span's
	// count per outcome is how many checks ended so: how often the intake
	// found nothing, dispatched a fix, or escalated.
	MediationSpanName = "agentrepl_deploy_mediation_seconds"
	// RemediationSpanName is each remediation, from its dispatch to the
	// check that ended it, by how it ended: its fix approved (landed), the
	// chain escalated, or dispatched again.
	RemediationSpanName = "agentrepl_deploy_remediation_seconds"
	// RemediationGateRoundsName is how many of a remediation finish's gates
	// failed before it landed or was given up, by which.
	RemediationGateRoundsName = "agentrepl_deploy_remediation_gate_rounds"
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
	// MergeHeadName is each stay of a merge at its queue's head, from taking
	// the head to leaving it, by how it left.
	MergeHeadName = "agentrepl_merge_head_seconds"
	// MediatorTurnTokensName is the tokens each turn of a deploy mediator
	// spent, by the phase of its run the turn ended in and the kind of
	// token. Its count per phase and kind is how many turns ended in it.
	MediatorTurnTokensName = "agentrepl_deploy_mediator_turn_tokens"
	// MediatorCompactionName is how long each compaction of a deploy
	// mediator took, or, after a remediation, owing it to the session's next
	// start (outcome "owed"), by how it ended. Its count per outcome is how
	// many compactions ended so.
	MediatorCompactionName = "agentrepl_deploy_mediator_compaction_seconds"
	// MediatorRunsName counts the deploy mediator's runs, from a deploy's
	// hot reload to the approval or the finish that ended the run, by how
	// it ended.
	MediatorRunsName = "agentrepl_deploy_mediator_runs_total"
	// RaceFindingsName counts the merge queue's race findings, by event: a
	// test that failed in a merge gate and passed on the same inputs found
	// (new), found again (recurred), or found again after its fix deployed
	// (reopened).
	RaceFindingsName = "agentrepl_merge_queue_race_findings_total"
	// ProductionChecksName counts the production checks decided, by how
	// each ended: nothing_to_do, dispatched or unanswered.
	ProductionChecksName = "agentrepl_production_checks_total"
	// ProductionChildrenName counts the production mediator's children by
	// event: dispatched (each child a dispatch made), folded, landed and
	// closed.
	ProductionChildrenName = "agentrepl_production_children_total"
)

// turnTokenBuckets double from a one-line turn to a turn that read a whole
// context window many times over.
var turnTokenBuckets = prometheus.ExponentialBuckets(1000, 2, 15)

// roundCountBuckets span a clean merge's single round to a merge that
// rebased or fixed past its bound.
var roundCountBuckets = []float64{0, 1, 2, 3, 4, 5, 6, 8, 10, 15, 20}

// mergeBuckets span a merge stage from an instant step to a long test gate.
var mergeBuckets = []float64{0.1, 0.5, 1, 2, 5, 10, 20, 30, 60, 120, 300, 600, 1200, 2400, 3600}

// merges holds the merge queue's histograms; each observation is
// client_golang's own atomic add.
type merges struct {
	steps, spans, mediations, chainRedeploys, chainSpans, rounds, roundCounts *prometheus.HistogramVec
	heads                                                                     *prometheus.HistogramVec
	remediations, remediationGates                                            *prometheus.HistogramVec
	inARow                                                                    prometheus.Gauge
	changes                                                                   *prometheus.CounterVec
	noticeSizes                                                               *prometheus.HistogramVec
	noticesSuppressed                                                         *prometheus.CounterVec
	mediatorTurnTokens, mediatorCompactions                                   *prometheus.HistogramVec
	mediatorRuns                                                              *prometheus.CounterVec
	productionChecks, productionChildren                                      *prometheus.CounterVec
	raceFindings                                                              *prometheus.CounterVec
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
			Name: MediationSpanName, Help: "A deploy check's rollout, assessment and total spans, by span and how it ended.", Buckets: mergeBuckets,
		}, []string{"span", "outcome"}),
		remediations: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: RemediationSpanName, Help: "A deploy remediation, from its dispatch to the check that ended it, by how it ended.", Buckets: mergeBuckets,
		}, []string{"outcome"}),
		remediationGates: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: RemediationGateRoundsName, Help: "Failed gates of a deploy remediation's finish before it landed or was given up, by which.", Buckets: roundCountBuckets,
		}, []string{"outcome"}),
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
		heads: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MergeHeadName, Help: "Each stay of a merge at its queue's head, by how it left the head.", Buckets: mergeBuckets,
		}, []string{"exit"}),
		changes: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: QueueChangesName, Help: "Changes to a merge queue's line, by kind and, for a merge leaving, its cause.",
		}, []string{"change", "cause"}),
		noticeSizes: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: NoticeSizeName, Help: "Each notice sent to a workspace, by type: its size in UTF-8 bytes.", Buckets: noticeSizeBuckets,
		}, []string{"notice"}),
		noticesSuppressed: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: NoticesSuppressedName, Help: "Notices of a type the daemon is set not to send, by type.",
		}, []string{"notice"}),
		mediatorTurnTokens: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MediatorTurnTokensName, Help: "Tokens a deploy mediator's turn spent, by the phase of its run the turn ended in and the kind of token.", Buckets: turnTokenBuckets,
		}, []string{"phase", "kind"}),
		mediatorCompactions: prometheus.NewHistogramVec(prometheus.HistogramOpts{
			Name: MediatorCompactionName, Help: "How long a compaction of a deploy mediator, or owing it to its next start, took, by how it ended.", Buckets: mergeBuckets,
		}, []string{"outcome"}),
		mediatorRuns: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: MediatorRunsName, Help: "Deploy mediator runs, from a deploy's hot reload to the approval or finish that ended it, by how it ended.",
		}, []string{"outcome"}),
		productionChecks: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ProductionChecksName, Help: "Production checks decided, by how each ended.",
		}, []string{"outcome"}),
		raceFindings: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: RaceFindingsName, Help: "The merge queue's race findings: a test that failed in a merge gate and passed on the same inputs, by event: new, recurred, reopened.",
		}, []string{"event"}),
		productionChildren: prometheus.NewCounterVec(prometheus.CounterOpts{
			Name: ProductionChildrenName, Help: "The production mediator's children, by event: dispatched, folded, landed, closed.",
		}, []string{"event"}),
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

// ObserveRemediation implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveRemediation(outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.remediations.WithLabelValues(outcome).Observe(took.Seconds())
}

// ObserveRemediationGateRounds implements merge.Meter. The off surface
// records nothing.
func (s *Surface) ObserveRemediationGateRounds(outcome string, n int) {
	if s == nil {
		return
	}
	s.merges.remediationGates.WithLabelValues(outcome).Observe(float64(n))
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

// ObserveMergeHead implements merge.Meter. The off surface records nothing.
func (s *Surface) ObserveMergeHead(exit string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.heads.WithLabelValues(exit).Observe(took.Seconds())
}

// CountQueueChange implements merge.Meter. The off surface records nothing.
func (s *Surface) CountQueueChange(change, cause string) {
	if s == nil {
		return
	}
	s.merges.changes.WithLabelValues(change, cause).Inc()
}

// ObserveMediatorTurnTokens implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveMediatorTurnTokens(phase, kind string, tokens uint64) {
	if s == nil {
		return
	}
	s.merges.mediatorTurnTokens.WithLabelValues(phase, kind).Observe(float64(tokens))
}

// ObserveMediatorCompaction implements merge.Meter. The off surface records
// nothing.
func (s *Surface) ObserveMediatorCompaction(outcome string, took time.Duration) {
	if s == nil {
		return
	}
	s.merges.mediatorCompactions.WithLabelValues(outcome).Observe(took.Seconds())
}

// CountMediatorRun implements merge.Meter. The off surface records nothing.
func (s *Surface) CountMediatorRun(outcome string) {
	if s == nil {
		return
	}
	s.merges.mediatorRuns.WithLabelValues(outcome).Inc()
}

// CountRaceFinding implements merge.Meter: one race finding found, by
// EVENT. The off surface records nothing.
func (s *Surface) CountRaceFinding(event string) {
	if s == nil {
		return
	}
	s.merges.raceFindings.WithLabelValues(event).Inc()
}

// ObserveProductionCheck implements merge.Meter: one production check
// decided, by OUTCOME, and each of the CHILDREN it dispatched. The off
// surface records nothing.
func (s *Surface) ObserveProductionCheck(outcome string, children int) {
	if s == nil {
		return
	}
	s.merges.productionChecks.WithLabelValues(outcome).Inc()
	if children > 0 {
		s.merges.productionChildren.WithLabelValues("dispatched").Add(float64(children))
	}
}

// ObserveProductionChild implements merge.Meter: one production child's
// EVENT. The off surface records nothing.
func (s *Surface) ObserveProductionChild(event string) {
	if s == nil {
		return
	}
	s.merges.productionChildren.WithLabelValues(event).Inc()
}

// The notices the daemon sends workspaces (briefprompt.Meter).
const (
	// NoticeSizeName is each notice's size in UTF-8 bytes, by its type (the
	// brief it was composed from, or agent_sent for a prompt one workspace's
	// agent sent another). Its count per type is how many were sent.
	NoticeSizeName = "agentrepl_workspace_notice_bytes"
	// NoticesSuppressedName counts the notices of a type the daemon is set
	// not to send (AGENT_REPL_SIBLING_NOTICES), by type.
	NoticesSuppressedName = "agentrepl_workspace_notices_suppressed_total"
)

// noticeSizeBuckets double from a one-line notice to a long brief.
var noticeSizeBuckets = prometheus.ExponentialBuckets(64, 2, 11)

// ObserveNotice implements briefprompt.Meter. The off surface records
// nothing.
func (s *Surface) ObserveNotice(notice string, bytes int) {
	if s == nil {
		return
	}
	s.merges.noticeSizes.WithLabelValues(notice).Observe(float64(bytes))
}

// CountSuppressedNotice implements briefprompt.Meter. The off surface
// records nothing.
func (s *Surface) CountSuppressedNotice(notice string) {
	if s == nil {
		return
	}
	s.merges.noticesSuppressed.WithLabelValues(notice).Inc()
}
