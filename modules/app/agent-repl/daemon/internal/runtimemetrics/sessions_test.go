package runtimemetrics

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/sessionwhy"
	"agent-repld/internal/shimlife"
)

func TestSessionsUpAreServedByReasonAndWorkspace(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	table := sessionwhy.NewTable()
	table.SetOwner("b", sessionwhy.OwnerRemoteControl)
	table.SetOwner("c", sessionwhy.OwnerCold)
	table.SetHold("c", sessionwhy.HoldDeferredBusy)
	err := s.RegisterSessions(sessionwhy.Census{
		Live: func() []ids.WorkspaceID { return []ids.WorkspaceID{"a", "b", "c"} },
		Work: func(ws ids.WorkspaceID) (sessionwhy.Work, bool) {
			return sessionwhy.Work{Turn: ws == "a"}, true
		},
		Table: table,
		Now:   func() time.Time { return time.Unix(1_700_000_000, 0) },
	})
	if err != nil {
		t.Fatalf("RegisterSessions: %v", err)
	}

	// Act
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_sessions_up{reason="working",workspace="a"} 1`,
		`agentrepl_sessions_up{reason="remote_control",workspace="b"} 1`,
		`agentrepl_sessions_up{reason="stand_down_deferred_busy",workspace="c"} 1`,
	)
}

func TestAParkIsObservedByItsReason(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveIdleBeforePark(shimlife.StandDownIdle, 21*time.Minute)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_session_idle_before_park_seconds_bucket{reason="idle",le="1200"} 0`,
		`agentrepl_session_idle_before_park_seconds_bucket{reason="idle",le="1500"} 1`,
		`agentrepl_session_idle_before_park_seconds_count{reason="idle"} 1`,
	)
}

func TestTheOffSurfaceRecordsNoSessions(t *testing.T) {
	// Arrange
	var s *Surface

	// Act
	err := s.RegisterSessions(sessionwhy.Census{})
	s.ObserveIdleBeforePark(shimlife.StandDownIdle, time.Minute)

	// Assert
	if err != nil {
		t.Fatalf("RegisterSessions on the off surface = %v, want nil", err)
	}
}
