package runtimemetrics

import (
	"testing"
	"time"
)

func TestCacheSeriesAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveBeat("ws-1", "sent", 20*time.Second)
	s.ObserveBeat("ws-1", "cache_lapsed", 2*time.Hour)
	s.CountLapse("ws-1", "stood_down")
	s.CountLapse("ws-1", "stood_down")
	s.ObserveColdGate("ws-1", "lapsed_5m", 10*time.Minute)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="sent",workspace="ws-1",le="30"} 1`,
		`agentrepl_keepalive_lateness_seconds_count{outcome="sent",workspace="ws-1"} 1`,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="cache_lapsed",workspace="ws-1",le="3600"} 0`,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="cache_lapsed",workspace="ws-1",le="7200"} 1`,
		`agentrepl_cache_lapses_total{outcome="stood_down",workspace="ws-1"} 2`,
		`agentrepl_cold_gate_idle_seconds_bucket{outcome="lapsed_5m",workspace="ws-1",le="900"} 1`,
		`agentrepl_cold_gate_idle_seconds_count{outcome="lapsed_5m",workspace="ws-1"} 1`,
	)
}

func TestTheOffSurfaceRecordsNoCacheSeries(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the observations and records nothing.
	s.ObserveBeat("ws-1", "sent", time.Second)
	s.CountLapse("ws-1", "stood_down")
	s.ObserveColdGate("ws-1", "model_switch", time.Second)
}
