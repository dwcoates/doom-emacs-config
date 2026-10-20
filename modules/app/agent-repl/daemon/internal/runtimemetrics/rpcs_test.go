package runtimemetrics

import (
	"testing"
	"time"
)

func TestAnRPCIsServedByMethodAndOutcome(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveRPC("SelectWorkspace", "ok", 3*time.Millisecond)
	s.ObserveRPC("SelectWorkspace", "ok", 40*time.Millisecond)
	s.ObserveRPC("SelectWorkspace", "not_found", time.Millisecond)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_rpc_seconds_bucket{outcome="ok",rpc="SelectWorkspace",le="0.005"} 1`,
		`agentrepl_rpc_seconds_bucket{outcome="ok",rpc="SelectWorkspace",le="0.05"} 2`,
		`agentrepl_rpc_seconds_count{outcome="ok",rpc="SelectWorkspace"} 2`,
		`agentrepl_rpc_seconds_count{outcome="not_found",rpc="SelectWorkspace"} 1`,
	)
}

func TestTheOffSurfaceTimesNoRPC(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the timing and records nothing.
	s.ObserveRPC("SelectWorkspace", "ok", time.Millisecond)
}
