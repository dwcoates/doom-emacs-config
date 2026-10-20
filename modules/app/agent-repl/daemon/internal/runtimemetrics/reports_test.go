package runtimemetrics

import (
	"errors"
	"math"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/restarttiming"
)

func TestValidateEditorStartupAnswersTheMilestonesObservation(t *testing.T) {
	spawned := &agentreplv1.ReportedEditorStartup_Spawned{Spawned: &agentreplv1.EditorStartupDaemonSpawned{}}
	running := &agentreplv1.ReportedEditorStartup_Running{Running: &agentreplv1.EditorStartupDaemonRunning{}}
	tests := []struct {
		name      string
		milestone agentreplv1.EditorStartupMilestone
		daemon    any
		phase     string
		cause     string
	}{
		{"init from a spawned daemon", agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_INIT, spawned, "init", "spawned"},
		{"linked to a running daemon", agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_DAEMON_LINKED, running, "daemon_linked", "running"},
		{"the first tab", agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_FIRST_TAB, running, "first_tab", "running"},
		{"every tab", agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_ALL_TABS, spawned, "all_tabs", "spawned"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			m := &agentreplv1.ReportedEditorStartup{Milestone: tt.milestone, SecondsSinceProcessStart: 4.5}
			switch d := tt.daemon.(type) {
			case *agentreplv1.ReportedEditorStartup_Spawned:
				m.Daemon = d
			case *agentreplv1.ReportedEditorStartup_Running:
				m.Daemon = d
			}

			// Act
			o, err := ValidateEditorStartup(m)

			// Assert
			if err != nil || o.Restart() != restarttiming.EditorStart || o.Phase() != tt.phase || o.Cause() != tt.cause ||
				o.Seconds() != 4.5 || o.Workspace() != "" || o.Outcome() != restarttiming.OK {
				t.Fatalf("ValidateEditorStartup = %+v, %v; want editor_start %s/%s at 4.5s", o, err, tt.phase, tt.cause)
			}
		})
	}
}

func TestValidateEditorStartupRefusesAMalformedMilestone(t *testing.T) {
	spawned := &agentreplv1.ReportedEditorStartup_Spawned{Spawned: &agentreplv1.EditorStartupDaemonSpawned{}}
	init := agentreplv1.EditorStartupMilestone_EDITOR_STARTUP_MILESTONE_INIT
	tests := []struct {
		name string
		m    *agentreplv1.ReportedEditorStartup
	}{
		{"an unspecified milestone", &agentreplv1.ReportedEditorStartup{Daemon: spawned, SecondsSinceProcessStart: 1}},
		{"no daemon arm", &agentreplv1.ReportedEditorStartup{Milestone: init, SecondsSinceProcessStart: 1}},
		{"a negative time", &agentreplv1.ReportedEditorStartup{Milestone: init, Daemon: spawned, SecondsSinceProcessStart: -1}},
		{"a NaN time", &agentreplv1.ReportedEditorStartup{Milestone: init, Daemon: spawned, SecondsSinceProcessStart: math.NaN()}},
		{"an infinite time", &agentreplv1.ReportedEditorStartup{Milestone: init, Daemon: spawned, SecondsSinceProcessStart: math.Inf(1)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if _, err := ValidateEditorStartup(tt.m); !errors.Is(err, ErrInvalidLatency) {
				t.Fatalf("ValidateEditorStartup err = %v, want ErrInvalidLatency", err)
			}
		})
	}
}
