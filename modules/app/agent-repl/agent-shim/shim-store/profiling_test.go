package main

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/logging/profiling"
)

func TestRecordProfilingEvent(t *testing.T) {
	tests := []struct {
		name      string
		event     profiling.Event
		level     string
		operation string
	}{
		{name: "a start is info", event: profiling.Event{Kind: profiling.Started, Session: "s1"}, level: "info", operation: "store.profiling.started"},
		{name: "a stop is info", event: profiling.Event{Kind: profiling.Stopped, Session: "s1", Reason: "exit"}, level: "info", operation: "store.profiling.stopped"},
		{name: "a failure is error", event: profiling.Event{Kind: profiling.Failed, Err: errors.New("boom")}, level: "error", operation: "store.profiling.failed"},
		{name: "a level window opened is info", event: profiling.Event{Kind: profiling.LevelOpened, Session: "s1", Level: "debug"}, level: "info", operation: "store.profiling.level_opened"},
		{name: "a level window closed is info", event: profiling.Event{Kind: profiling.LevelClosed, Session: "s1", Level: "debug", Reason: "session_closed"}, level: "info", operation: "store.profiling.level_closed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, durable := storePprofLogger()

			// Act
			recordProfilingEvent(log, tt.event)

			// Assert
			records := decodeStoreRecords(t, durable)
			if len(records) != 1 || records[0].Level != tt.level || records[0].Operation != tt.operation {
				t.Fatalf("records = %+v, want one %s %s", records, tt.level, tt.operation)
			}
		})
	}
}

func TestAFailedProfilingEventCarriesItsCause(t *testing.T) {
	// Arrange
	log, durable := storePprofLogger()

	// Act
	recordProfilingEvent(log, profiling.Event{Kind: profiling.Failed, Err: errors.New("boom")})

	// Assert
	if records := decodeStoreRecords(t, durable); !strings.Contains(records[0].Message, "cause=boom") {
		t.Fatalf("message = %q, want the cause", records[0].Message)
	}
}

func TestStartRuntimeProfilingRefusesAMissingHome(t *testing.T) {
	// Arrange
	log, _ := storePprofLogger()
	getenv := func(string) string { return "" }
	home := func() (string, error) { return "", errors.New("no home") }

	// Act
	_, err := startRuntimeProfiling(log, getenv, home)

	// Assert
	if err == nil {
		t.Fatal("startRuntimeProfiling armed with no home and no override")
	}
}

func TestStartRuntimeProfilingStopsCleanlyWithNoSession(t *testing.T) {
	// Arrange
	log, durable := storePprofLogger()
	root := filepath.Join(t.TempDir(), "profiles")
	getenv := func(k string) string {
		if k == profiling.RootEnvironment {
			return root
		}
		return ""
	}
	stop, err := startRuntimeProfiling(log, getenv, os.UserHomeDir)
	if err != nil {
		t.Fatalf("startRuntimeProfiling: %v", err)
	}

	// Act
	stop()

	// Assert
	if records := decodeStoreRecords(t, durable); len(records) != 0 {
		t.Fatalf("records = %+v, want none when no session was ever open", records)
	}
}
