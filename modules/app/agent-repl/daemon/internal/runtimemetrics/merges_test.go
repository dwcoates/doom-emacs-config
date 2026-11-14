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

func TestTheOffSurfaceTimesNoMerge(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the timings and records nothing.
	s.ObserveMergeStep("testing", time.Second)
	s.ObserveMergeSpan("total", "landed", time.Second)
	s.ObserveMediationSpan("total", "approved", time.Second)
}
