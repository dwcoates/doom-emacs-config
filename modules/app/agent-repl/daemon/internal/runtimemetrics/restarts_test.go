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
