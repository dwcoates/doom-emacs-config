package runtimemetrics

import (
	"testing"
	"time"
)

func TestMergeStagesAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveMergeStep("testing", 90*time.Second)
	s.ObserveMergeSpan("total", "landed", 4*time.Minute)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_merge_step_seconds_bucket{step="testing",le="120"} 1`,
		`agentrepl_merge_step_seconds_count{step="testing"} 1`,
		`agentrepl_merge_seconds_bucket{outcome="landed",span="total",le="300"} 1`,
		`agentrepl_merge_seconds_count{outcome="landed",span="total"} 1`,
	)
}

func TestDeployMediationsAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveMediationSpan("assessment", "redeployed", 8*time.Minute)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_deploy_mediation_seconds_bucket{outcome="redeployed",span="assessment",le="600"} 1`,
		`agentrepl_deploy_mediation_seconds_count{outcome="redeployed",span="assessment"} 1`,
	)
}

func TestMergeQueueSeriesAreServed(t *testing.T) {
	tests := []struct {
		name    string
		observe func(s *Surface)
		want    []string
	}{
		{name: "a chain of remediations", observe: func(s *Surface) { s.ObserveMediationChain("approved", 2, 9*time.Minute) },
			want: []string{
				`agentrepl_deploy_remediation_chain_redeploys_bucket{outcome="approved",le="2"} 1`,
				`agentrepl_deploy_remediation_chain_redeploys_bucket{outcome="approved",le="1"} 0`,
				`agentrepl_deploy_remediation_chain_seconds_bucket{outcome="approved",le="600"} 1`,
			}},
		{name: "remediations in a row", observe: func(s *Surface) { s.SetRemediationsInARow(3) },
			want: []string{`agentrepl_deploy_remediations_in_a_row 3`}},
		{name: "a merge's round", observe: func(s *Surface) { s.ObserveMergeRound("fixes", "failed", 4*time.Minute) },
			want: []string{`agentrepl_merge_round_seconds_bucket{outcome="failed",round="fixes",le="300"} 1`}},
		{name: "a merge's round count", observe: func(s *Surface) { s.ObserveMergeRounds("conflicts", "landed", 0) },
			want: []string{`agentrepl_merge_rounds_bucket{outcome="landed",round="conflicts",le="0"} 1`}},
		{name: "a stay at the head", observe: func(s *Surface) { s.ObserveMergeHead("needed_fixes", 90*time.Second) },
			want: []string{`agentrepl_merge_head_seconds_bucket{exit="needed_fixes",le="120"} 1`,
				`agentrepl_merge_head_seconds_bucket{exit="needed_fixes",le="60"} 0`}},
		{name: "a queue change", observe: func(s *Surface) { s.CountQueueChange("dequeued", "evicted") },
			want: []string{`agentrepl_merge_queue_changes_total{cause="evicted",change="dequeued"} 1`}},
		{name: "a notice sent to a workspace", observe: func(s *Surface) { s.ObserveNotice("workspace-sibling-notice", 100) },
			want: []string{`agentrepl_workspace_notice_bytes_bucket{notice="workspace-sibling-notice",le="128"} 1`,
				`agentrepl_workspace_notice_bytes_bucket{notice="workspace-sibling-notice",le="64"} 0`}},
		{name: "a notice suppressed", observe: func(s *Surface) { s.CountSuppressedNotice("workspace-sibling-notice") },
			want: []string{`agentrepl_workspace_notices_suppressed_total{notice="workspace-sibling-notice"} 1`}},
		{name: "a deploy mediator's turn tokens", observe: func(s *Surface) { s.ObserveMediatorTurnTokens("assessing", "output", 3000) },
			want: []string{`agentrepl_deploy_mediator_turn_tokens_bucket{kind="output",phase="assessing",le="4000"} 1`,
				`agentrepl_deploy_mediator_turn_tokens_bucket{kind="output",phase="assessing",le="2000"} 0`}},
		{name: "a deploy mediator's compaction", observe: func(s *Surface) { s.ObserveMediatorCompaction("compacted", 40*time.Second) },
			want: []string{`agentrepl_deploy_mediator_compaction_seconds_bucket{outcome="compacted",le="60"} 1`,
				`agentrepl_deploy_mediator_compaction_seconds_bucket{outcome="compacted",le="30"} 0`}},
		{name: "a deploy mediator's run", observe: func(s *Surface) { s.CountMediatorRun("finished_remediation") },
			want: []string{`agentrepl_deploy_mediator_runs_total{outcome="finished_remediation"} 1`}},
		{name: "a production check that dispatched", observe: func(s *Surface) { s.ObserveProductionCheck("dispatched", 2) },
			want: []string{`agentrepl_production_checks_total{outcome="dispatched"} 1`, `agentrepl_production_children_total{event="dispatched"} 2`}},
		{name: "a production check with nothing to do counts no child", observe: func(s *Surface) { s.ObserveProductionCheck("nothing_to_do", 0) },
			want: []string{`agentrepl_production_checks_total{outcome="nothing_to_do"} 1`}},
		{name: "a race finding found again after its fix", observe: func(s *Surface) { s.CountRaceFinding("reopened") },
			want: []string{`agentrepl_merge_queue_race_findings_total{event="reopened"} 1`}},
		{name: "a production child's event", observe: func(s *Surface) { s.ObserveProductionChild("landed") },
			want: []string{`agentrepl_production_children_total{event="landed"} 1`}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := openSurface(t)

			// Act
			tt.observe(s)
			body := scrape(t, s)

			// Assert
			mustContain(t, body, tt.want...)
		})
	}
}

func TestTheOffSurfaceTimesNoMerge(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the timings and records nothing.
	s.ObserveMergeStep("testing", time.Second)
	s.ObserveMergeSpan("total", "landed", time.Second)
	s.ObserveMediationSpan("total", "approved", time.Second)
	s.ObserveMediationChain("approved", 1, time.Second)
	s.SetRemediationsInARow(1)
	s.ObserveMergeRound("tests", "succeeded", time.Second)
	s.ObserveMergeRounds("tests", "landed", 1)
	s.CountRaceFinding("new")
	s.CountQueueChange("enqueued", "")
	s.ObserveMergeHead("landed", time.Second)
	s.ObserveNotice("agent_sent", 1)
	s.CountSuppressedNotice("workspace-child-notice")
}
