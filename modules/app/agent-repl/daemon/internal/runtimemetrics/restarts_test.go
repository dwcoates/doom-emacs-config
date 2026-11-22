package runtimemetrics

import (
	"strings"
	"testing"
	"time"

	"agent-repld/internal/restarttiming"
)

func TestARestartIsServedInItsBuckets(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, 1500*time.Millisecond))
	s.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, 4*time.Second))
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_restart_seconds_bucket{cause="",outcome="ok",phase="build",restart="deploy",le="1"} 0`,
		`agentrepl_restart_seconds_bucket{cause="",outcome="ok",phase="build",restart="deploy",le="2"} 1`,
		`agentrepl_restart_seconds_bucket{cause="",outcome="ok",phase="build",restart="deploy",le="5"} 2`,
		`agentrepl_restart_seconds_count{cause="",outcome="ok",phase="build",restart="deploy"} 2`,
		`agentrepl_restart_seconds_sum{cause="",outcome="ok",phase="build",restart="deploy"} 5.5`,
	)
}

func TestOutcomesAndCausesAreSeparateSeries(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveRestart(restarttiming.OfShimRestart("ws1", restarttiming.ShimRun, "build_stale", restarttiming.OK, time.Second))
	s.ObserveRestart(restarttiming.OfShimRestart("ws1", restarttiming.ShimRun, "restart_verb", restarttiming.Failed, time.Second))
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_restart_seconds_count{cause="build_stale",outcome="ok",phase="run",restart="shim_restart"} 1`,
		`agentrepl_restart_seconds_count{cause="restart_verb",outcome="failed",phase="run",restart="shim_restart"} 1`,
	)
}

func TestAWorkspaceRestartKeepsItsLatestDuration(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveRestart(restarttiming.OfWorkspaceStart("ws1", restarttiming.StartFresh, restarttiming.StartBoot, restarttiming.OK, 9*time.Second))
	s.ObserveRestart(restarttiming.OfWorkspaceStart("ws1", restarttiming.StartFresh, restarttiming.StartBoot, restarttiming.OK, 3*time.Second))
	body := scrape(t, s)

	// Assert
	mustContain(t, body, `agentrepl_workspace_restart_last_seconds{phase="start",restart="workspace_start",workspace="ws1"} 3`)
}

func TestARestartOfNoWorkspaceHasNoLatestDuration(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveRestart(restarttiming.OfBoot(restarttiming.BootServing, restarttiming.BootCold, time.Second))
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, "agentrepl_workspace_restart_last_seconds{") {
		t.Fatalf("a boot published a per-workspace duration:\n%s", body)
	}
}

func TestAForgottenWorkspaceLosesItsLatestDurationsButNotTheHistogram(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	s.ObserveRestart(restarttiming.OfTransfer("ws1", restarttiming.TransferRun, restarttiming.OK, time.Second))
	s.ObserveRestart(restarttiming.OfTransfer("ws2", restarttiming.TransferRun, restarttiming.OK, time.Second))

	// Act
	s.ForgetWorkspace("ws1")
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, `workspace="ws1"`) {
		t.Fatalf("ws1's latest duration outlived it:\n%s", body)
	}
	mustContain(t, body,
		`agentrepl_workspace_restart_last_seconds{phase="run",restart="handover_transfer",workspace="ws2"} 1`,
		`agentrepl_restart_seconds_count{cause="",outcome="ok",phase="run",restart="handover_transfer"} 2`,
	)
}

func TestTheOffSurfaceRecordsNoRestart(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the observation and records nothing.
	s.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployTotal, restarttiming.OK, time.Second))
}

func TestAffectedCountsAreServedPerCauseAndPhase(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveAffected(restarttiming.OfAffected(restarttiming.AffectedHost, restarttiming.ReconnectSuccessor, 125))
	s.ObserveAffected(restarttiming.OfAffected(restarttiming.AffectedHost, restarttiming.ReconnectSuccessor, 15))
	s.ObserveAffected(restarttiming.OfAffected(restarttiming.AffectedWeb, restarttiming.ReconnectCold, 5))
	s.ObserveAffected(restarttiming.OfAffected(restarttiming.AffectedShim, restarttiming.ReconnectCold, 3))
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_restart_affected_bucket{cause="successor",phase="host",le="13"} 0`,
		`agentrepl_restart_affected_bucket{cause="successor",phase="host",le="21"} 1`,
		`agentrepl_restart_affected_bucket{cause="successor",phase="host",le="144"} 2`,
		`agentrepl_restart_affected_count{cause="successor",phase="host"} 2`,
		`agentrepl_restart_affected_sum{cause="successor",phase="host"} 140`,
		`agentrepl_restart_affected_count{cause="cold",phase="web"} 1`,
		`agentrepl_restart_affected_count{cause="cold",phase="shim"} 1`,
	)
}

func TestTheOffSurfaceRecordsNoAffectedCount(t *testing.T) {
	var s *Surface

	// Act and assert: the off surface takes the count and records nothing.
	s.ObserveAffected(restarttiming.OfAffected(restarttiming.AffectedHost, restarttiming.ReconnectCold, 4))
}

func TestWorkspaceReconnectBucketsReachPastTheOldTopBucket(t *testing.T) {
	tests := []struct {
		name    string
		restart restarttiming.Observation
		wantTop string
	}{
		{"a cold web reconnect resolves 500s", restarttiming.OfReconnect("w1", restarttiming.ReconnectWeb, restarttiming.ReconnectCold, 500*time.Second),
			`agentrepl_restart_seconds_bucket{cause="cold",outcome="ok",phase="web",restart="workspace_reconnect",le="610"} 1`},
		{"a cold web reconnect has its 233s bucket empty of 500s", restarttiming.OfReconnect("w1", restarttiming.ReconnectWeb, restarttiming.ReconnectCold, 500*time.Second),
			`agentrepl_restart_seconds_bucket{cause="cold",outcome="ok",phase="web",restart="workspace_reconnect",le="233"} 0`},
		{"a handover host reconnect keeps its fine buckets", restarttiming.OfReconnect("w1", restarttiming.ReconnectHost, restarttiming.ReconnectSuccessor, 9500*time.Millisecond),
			`agentrepl_restart_seconds_bucket{cause="successor",outcome="ok",phase="host",restart="workspace_reconnect",le="13"} 1`},
		{"another restart keeps the old top bucket", restarttiming.OfDeploy(restarttiming.DeployTotal, restarttiming.OK, 500*time.Second),
			`agentrepl_restart_seconds_bucket{cause="",outcome="ok",phase="total",restart="deploy",le="233"} 0`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := openSurface(t)

			// Act
			s.ObserveRestart(tt.restart)
			body := scrape(t, s)

			// Assert
			mustContain(t, body, tt.wantTop)
			if strings.Contains(body, `restart="deploy",le="610"`) {
				t.Fatalf("a deploy histogram carries the reconnect buckets:\n%s", body)
			}
		})
	}
}

func TestReconnectBucketsExtendTheRestartBuckets(t *testing.T) {
	// Assert: the shared prefix is unchanged and the top is widened.
	want := append(append([]float64(nil), restartBuckets...), 377, 610, 987, 1597)
	if got := bucketsOf(restarttiming.WorkspaceReconnect); !equalFloats(got, want) {
		t.Fatalf("reconnect buckets = %v, want %v", got, want)
	}
	if got := bucketsOf(restarttiming.Deploy); !equalFloats(got, restartBuckets) {
		t.Fatalf("deploy buckets = %v, want the restart buckets", got)
	}
}

func equalFloats(a, b []float64) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
