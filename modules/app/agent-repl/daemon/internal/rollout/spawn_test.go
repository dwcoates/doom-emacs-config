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

func TestSpawnLeavesTheSuccessorRunningWhenTheIncumbentsContextEnds(t *testing.T) {
	// Arrange: a stand-in successor that reports its address and then keeps
	// running, exactly as the real daemon does once its listener is bound.
	// The context is the INCUMBENT'S serving lifetime, which the handover
	// ends -- binding the child to it killed the successor Emacs had already
	// been handed.
	state := t.TempDir()
	alive := filepath.Join(state, "alive")
	script := filepath.Join(state, "successor.sh")
	body := "#!/bin/sh\n" +
		"printf '127.0.0.1:7788\\n' > " + JoiningAddrPath(state) + ".tmp\n" +
		"mv " + JoiningAddrPath(state) + ".tmp " + JoiningAddrPath(state) + "\n" +
		"trap 'exit 0' TERM\n" +
		"i=0\n" +
		"while [ $i -lt 200 ]; do printf 'x' >> " + alive + "; i=$((i+1)); sleep 0.01; done\n"
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the stand-in: %v", err)
	}
	spawner := NewProcessSpawner(script, state)
	spawner.Poll = time.Millisecond
	spawner.Timeout = 10 * time.Second
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the handover completes, then the incumbent's lifetime ends.
	if _, err := spawner.Spawn(ctx, "127.0.0.1:7777"); err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	before := spawnAliveLen(t, alive)
	cancel()

	// Assert: the successor is still writing after the cancellation. Polling
	// for GROWTH is the liveness proof; a killed child's file never moves
	// again, so the wait ends on the first larger read rather than on a sleep.
	deadline := time.Now().Add(2 * time.Second)
	for {
		if spawnAliveLen(t, alive) > before {
			return
		}
		if time.Now().After(deadline) {
			t.Fatalf("the successor stopped writing after the incumbent's context was cancelled; "+
				"it must outlive the daemon that spawned it (size stayed %d)", before)
		}
		time.Sleep(time.Millisecond)
	}
}

// spawnAliveLen reports how much the stand-in successor has written so far.
// An absent file is zero: the child may not have reached its first write.
func spawnAliveLen(t *testing.T, path string) int64 {
	t.Helper()
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return 0
	}
	if err != nil {
		t.Fatalf("stat the liveness file: %v", err)
	}
	return info.Size()
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

// TestTheSuccessorInheritsTheIncumbentsArgv covers what a successor IS: the
// same daemon, re-pointed. A successor assembled from a curated list of flags
// would differ from its incumbent in exactly the ways nobody thought to list.
func TestTheSuccessorInheritsTheIncumbentsArgv(t *testing.T) {
	tests := []struct {
		name      string
		incumbent []string
		want      []string
	}{
		{
			name:      "no flags at all is just the joining flag",
			incumbent: nil,
			want:      []string{JoiningFlag, "127.0.0.1:1"},
		},
		{
			name:      "every other flag is carried through",
			incumbent: []string{"--default-config-dir", "/roots/default", "--prompts-dir", "/prompts"},
			want:      []string{"--default-config-dir", "/roots/default", "--prompts-dir", "/prompts", JoiningFlag, "127.0.0.1:1"},
		},
		{
			name:      "a separated joining flag is dropped with its value",
			incumbent: []string{"--joining", "127.0.0.1:9", "--prompts-dir", "/prompts"},
			want:      []string{"--prompts-dir", "/prompts", JoiningFlag, "127.0.0.1:1"},
		},
		{
			name:      "an attached joining flag is dropped",
			incumbent: []string{"-joining=127.0.0.1:9", "--node", "node"},
			want:      []string{"--node", "node", JoiningFlag, "127.0.0.1:1"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := successorArgv(tc.incumbent, "127.0.0.1:1")

			// Assert.
			if len(got) != len(tc.want) {
				t.Fatalf("argv = %v, want %v", got, tc.want)
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Fatalf("argv = %v, want %v", got, tc.want)
				}
			}
		})
	}
}
