package runtimemetrics

import (
	"testing"
	"time"
)

func TestShimLifeSeriesAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveShimStart("ws-1", "prompt", "ok", 2*time.Second)
	s.CountShimStandDown("ws-1", "keepalive_cycle")
	s.CountColdGateAnswer("ws-1", "pay", "resumed")
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_shim_start_seconds_bucket{cause="prompt",outcome="ok",workspace="ws-1",le="2"} 1`,
		`agentrepl_shim_start_seconds_count{cause="prompt",outcome="ok",workspace="ws-1"} 1`,
		`agentrepl_shim_stand_downs_total{reason="keepalive_cycle",workspace="ws-1"} 1`,
		`agentrepl_cold_gate_answers_total{choice="pay",outcome="resumed",workspace="ws-1"} 1`,
	)
}

func TestTheOffSurfaceRecordsNoShimLifeSeries(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the observations and records nothing.
	s.ObserveShimStart("ws-1", "prompt", "ok", time.Second)
	s.CountShimStandDown("ws-1", "hibernate")
	s.CountColdGateAnswer("ws-1", "clear", "resumed")
}
