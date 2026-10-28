package runtimemetrics

import (
	"testing"
	"time"
)

func TestCacheSeriesAreServed(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveBeat("sent", 20*time.Second)
	s.ObserveBeat("cache_lapsed", 2*time.Hour)
	s.CountLapse("stood_down")
	s.CountLapse("stood_down")
	s.ObserveColdGate("lapsed_5m", 10*time.Minute)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="sent",le="30"} 1`,
		`agentrepl_keepalive_lateness_seconds_count{outcome="sent"} 1`,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="cache_lapsed",le="3600"} 0`,
		`agentrepl_keepalive_lateness_seconds_bucket{outcome="cache_lapsed",le="7200"} 1`,
		`agentrepl_cache_lapses_total{outcome="stood_down"} 2`,
		`agentrepl_cold_gate_idle_seconds_bucket{outcome="lapsed_5m",le="900"} 1`,
		`agentrepl_cold_gate_idle_seconds_count{outcome="lapsed_5m"} 1`,
	)
}

func TestTheOffSurfaceRecordsNoCacheSeries(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the observations and records nothing.
	s.ObserveBeat("sent", time.Second)
	s.CountLapse("stood_down")
	s.ObserveColdGate("model_switch", time.Second)
}
