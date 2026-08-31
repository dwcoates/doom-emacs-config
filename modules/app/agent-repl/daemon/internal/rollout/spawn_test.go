package rollout

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestReportJoiningAddrRoundTripsTheAddress(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	if err := ReportJoiningAddr(state, "127.0.0.1:7788"); err != nil {
		t.Fatalf("ReportJoiningAddr: %v", err)
	}
	got, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if !reported || got != "127.0.0.1:7788" {
		t.Fatalf("address = %q reported = %v, want the address back", got, reported)
	}
}

func TestReportJoiningAddrRefusesABlankAddress(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	err := ReportJoiningAddr(state, "   ")

	// Assert
	if err == nil {
		t.Fatalf("ReportJoiningAddr accepted a blank address")
	}
}

func TestReadJoiningAddrReportsAbsenceWhileTheSuccessorIsStillBinding(t *testing.T) {
	// Arrange
	state := t.TempDir()

	// Act
	_, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if reported {
		t.Fatalf("reported = true with no report written")
	}
}

func TestReadJoiningAddrTreatsAnEmptyReportAsAbsence(t *testing.T) {
	// Arrange
	state := t.TempDir()
	if err := os.WriteFile(JoiningAddrPath(state), []byte("\n"), 0o644); err != nil {
		t.Fatalf("write the report: %v", err)
	}

	// Act
	_, reported, err := ReadJoiningAddr(state)

	// Assert
	if err != nil {
		t.Fatalf("ReadJoiningAddr: %v", err)
	}
	if reported {
		t.Fatalf("reported = true for an empty report; a blank address would be dialed as a real one")
	}
}

func TestJoiningAddrPathNamesTheOneReportFile(t *testing.T) {
	// Arrange
	state := "/tmp/state"

	// Act
	got := JoiningAddrPath(state)

	// Assert
	if got != filepath.Join(state, JoiningAddrFile) {
		t.Fatalf("path = %q, want %q", got, filepath.Join(state, JoiningAddrFile))
	}
}

func TestSpawnRefusesWithNoDaemonBinary(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner("", t.TempDir())

	// Act
	_, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err == nil {
		t.Fatalf("Spawn accepted an empty binary path")
	}
}

func TestSpawnRefusesWithNoIncumbentAddress(t *testing.T) {
	// Arrange
	spawner := NewProcessSpawner("/bin/true", t.TempDir())

	// Act
	_, err := spawner.Spawn(context.Background(), "")

	// Assert
	if err == nil {
		t.Fatalf("Spawn accepted a blank incumbent address; the successor is told, never left to infer")
	}
}

func TestSpawnAnswersTheAddressTheSuccessorReports(t *testing.T) {
	// Arrange
	state := t.TempDir()
	// A stand-in for the daemon binary that reports an address and exits: the
	// real one is not built in a unit test, and what is under test is the
	// report's round trip, not the daemon.
	script := filepath.Join(state, "successor.sh")
	body := "#!/bin/sh\nprintf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	spawner := NewProcessSpawner(script, state)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second

	// Act
	got, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	if got != "127.0.0.1:7788" {
		t.Fatalf("address = %q, want the successor's report", got)
	}
}

func TestSpawnFailsWhenTheSuccessorNeverReports(t *testing.T) {
	// Arrange
	state := t.TempDir()
	spawner := NewProcessSpawner("/usr/bin/true", state)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 20 * time.Millisecond

	// Act
	_, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")

	// Assert
	if err == nil {
		t.Fatalf("Spawn succeeded with no address reported")
	}
}

func TestSpawnClearsAStaleReportFromAnEarlierHandover(t *testing.T) {
	// Arrange
	state := t.TempDir()
	if err := ReportJoiningAddr(state, "127.0.0.1:9999"); err != nil {
		t.Fatalf("ReportJoiningAddr: %v", err)
	}
	spawner := NewProcessSpawner("/usr/bin/true", state)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 20 * time.Millisecond

	// Act
	_, err := spawner.Spawn(context.Background(), "127.0.0.1:7777")
	_, reported, readErr := ReadJoiningAddr(state)

	// Assert
	if err == nil {
		t.Fatalf("Spawn answered the stale report from an earlier handover")
	}
	if readErr != nil {
		t.Fatalf("ReadJoiningAddr: %v", readErr)
	}
	if reported {
		t.Fatalf("the stale report survived; it would be dialed as this handover's successor")
	}
}
