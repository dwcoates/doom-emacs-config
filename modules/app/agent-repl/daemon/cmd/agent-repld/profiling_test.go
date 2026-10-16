package main

import (
	"errors"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/dlog"
	"agentrepl/logging"
	"agentrepl/logging/profiling"
)

func TestRecordProfilingEvent(t *testing.T) {
	tests := []struct {
		name      string
		event     profiling.Event
		level     string
		operation string
	}{
		{name: "a start is info", event: profiling.Event{Kind: profiling.Started, Session: "s1"}, level: "info", operation: "daemon.profiling.started"},
		{name: "a stop is info", event: profiling.Event{Kind: profiling.Stopped, Session: "s1", Reason: "exit"}, level: "info", operation: "daemon.profiling.stopped"},
		{name: "a failure is error", event: profiling.Event{Kind: profiling.Failed, Err: errors.New("boom")}, level: "error", operation: "daemon.profiling.failed"},
		{name: "a level window opened is info", event: profiling.Event{Kind: profiling.LevelOpened, Session: "s1", Level: "debug"}, level: "info", operation: "daemon.profiling.level_opened"},
		{name: "a level window closed is info", event: profiling.Event{Kind: profiling.LevelClosed, Session: "s1", Level: "debug", Reason: "session_closed"}, level: "info", operation: "daemon.profiling.level_closed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()

			// Act
			recordProfilingEvent(log, tt.event)

			// Assert
			records := log.Records()
			if len(records) != 1 || records[0].Level != tt.level || records[0].Operation != tt.operation {
				t.Fatalf("records = %+v, want one %s %s", records, tt.level, tt.operation)
			}
		})
	}
}

func TestAFailedProfilingEventCarriesItsCause(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()

	// Act
	recordProfilingEvent(log, profiling.Event{Kind: profiling.Failed, Err: errors.New("boom")})

	// Assert
	if got := log.Records()[0].Context["cause"]; got != "boom" {
		t.Fatalf("cause = %v, want the error", got)
	}
}

func TestStartRuntimeProfilingRefusesAMissingHome(t *testing.T) {
	// Arrange
	getenv := func(string) string { return "" }
	home := func() (string, error) { return "", errors.New("no home") }

	// Act
	_, err := startRuntimeProfiling(dlog.NewTestLogger(), logging.FixedWindow(logging.LevelInfo), getenv, home)

	// Assert
	if err == nil {
		t.Fatal("startRuntimeProfiling armed with no home and no override")
	}
}

func TestStartRuntimeProfilingStopsCleanlyWithNoSession(t *testing.T) {
	// Arrange
	root := filepath.Join(t.TempDir(), "profiles")
	getenv := func(k string) string {
		if k == profiling.RootEnvironment {
			return root
		}
		return ""
	}
	log := dlog.NewTestLogger()
	stop, err := startRuntimeProfiling(log, logging.FixedWindow(logging.LevelInfo), getenv, os.UserHomeDir)
	if err != nil {
		t.Fatalf("startRuntimeProfiling: %v", err)
	}

	// Act
	stop()

	// Assert
	if records := log.Records(); len(records) != 0 {
		t.Fatalf("records = %+v, want none when no session was ever open", records)
	}
}
