package runtimemetrics

import "testing"

func TestARecordIsCountedByRuntimeLevelAndOperation(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.CountRecord("webapp", "error", "rpc.stream-transport-failure")
	s.CountRecord("webapp", "error", "rpc.stream-transport-failure")
	s.CountRecord("daemon", "error", "daemon.webprocess.stall_beat")
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_log_records_total{level="error",operation="rpc.stream-transport-failure",runtime="webapp"} 2`,
		`agentrepl_log_records_total{level="error",operation="daemon.webprocess.stall_beat",runtime="daemon"} 1`,
	)
}

func TestTheOffSurfaceCountsNoRecord(t *testing.T) {
	// Arrange
	var s *Surface

	// Act and assert: the off surface takes the record and counts nothing.
	s.CountRecord("daemon", "error", "daemon.x")
}
