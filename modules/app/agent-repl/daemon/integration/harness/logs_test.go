package harness

import (
	"path/filepath"
	"testing"
)

func TestTimestampPatternAcceptsTheContractedShapes(t *testing.T) {
	tests := []struct {
		name  string
		value string
	}{
		{name: "utc seconds", value: "2026-08-29T14:44:00Z"},
		{name: "utc fractional", value: "2026-08-29T14:44:00.123456789Z"},
		{name: "offset", value: "2026-08-29T14:44:00-07:00"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if !TimestampPattern.MatchString(tc.value) {
				t.Fatalf("TimestampPattern rejected %q, want it accepted", tc.value)
			}
		})
	}
}

func TestTimestampPatternRejectsOtherShapes(t *testing.T) {
	tests := []struct {
		name  string
		value string
	}{
		{name: "empty", value: ""},
		{name: "epoch millis", value: "1756480000000"},
		{name: "date only", value: "2026-08-29"},
		{name: "space separated", value: "2026-08-29 14:44:00Z"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if TimestampPattern.MatchString(tc.value) {
				t.Fatalf("TimestampPattern accepted %q, want it rejected", tc.value)
			}
		})
	}
}

func TestWorkspaceLogPathIsTheContractedSymlink(t *testing.T) {
	// Arrange / Act
	got := WorkspaceLogPath("/w/one", "daemon")

	// Assert
	want := filepath.Join("/w/one", ".claude", "emacs", "daemon.log")
	if got != want {
		t.Fatalf("WorkspaceLogPath = %q, want %q", got, want)
	}
}

func TestUnexpectedWarningsWithNothingDeclaredFlagsEveryWarningRecord(t *testing.T) {
	// Arrange: the set StartDaemon arms every daemon with.
	records := []LogRecord{
		{Level: "warn", Operation: "daemon.shimclient.redial"},
		{Level: "error", Operation: "daemon.workspace.open"},
	}

	// Act
	got := unexpectedWarnings(records, map[string]bool{})

	// Assert
	if len(got) != 2 {
		t.Fatalf("unexpectedWarnings with nothing declared = %d records, want 2", len(got))
	}
}

func TestUnexpectedWarningsSkipsADeclaredOperation(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Level: "warn", Operation: "daemon.shimclient.redial"},
		{Level: "error", Operation: "daemon.workspace.open"},
	}

	// Act
	got := unexpectedWarnings(records, map[string]bool{"daemon.shimclient.redial": true})

	// Assert
	if len(got) != 1 || got[0].Operation != "daemon.workspace.open" {
		t.Fatalf("unexpectedWarnings with one declared = %v, want only daemon.workspace.open", got)
	}
}

func TestUnexpectedWarningsIgnoresRecordsBelowAWarningLevel(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Level: "debug", Operation: "daemon.merge.answer_dequeue"},
		{Level: "info", Operation: "daemon.rollout.relaunch"},
	}

	// Act
	got := unexpectedWarnings(records, map[string]bool{})

	// Assert
	if len(got) != 0 {
		t.Fatalf("unexpectedWarnings over debug and info records = %v, want none", got)
	}
}
