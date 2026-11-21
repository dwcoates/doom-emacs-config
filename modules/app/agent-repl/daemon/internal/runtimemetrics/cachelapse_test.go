package runtimemetrics

import (
	"testing"
	"time"

	"agent-repld/internal/wsm"
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

func TestAnEndedKeepaliveRunIsServedByItsEnd(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	run := wsm.KeepaliveRun{Beats: 7, Spent: wsm.KeepaliveRunTokens{CacheRead: 6300, Output: 14}, End: wsm.KeepaliveRunCapped}
	// Act
	s.ObserveKeepaliveRun("ws1", run)
	// Assert
	mustContain(t, scrape(t, s),
		KeepaliveRunBeatsName+`_bucket{end="run_cap",workspace="ws1",le="8"} 1`,
		KeepaliveRunBeatsName+`_sum{end="run_cap",workspace="ws1"} 7`,
		KeepaliveRunTokensName+`{end="run_cap",kind="cache_read",workspace="ws1"} 6300`,
		KeepaliveRunTokensName+`{end="run_cap",kind="output",workspace="ws1"} 14`)
}

func TestTheOffSurfaceRecordsNoKeepaliveRun(t *testing.T) {
	// Arrange
	var s *Surface
	// Act and Assert: the off surface records nothing and does not panic.
	s.ObserveKeepaliveRun("ws1", wsm.KeepaliveRun{Beats: 1, End: wsm.KeepaliveRunResumed})
}
