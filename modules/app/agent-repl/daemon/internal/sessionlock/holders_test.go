package sessionlock

import (
	"os"
	"path/filepath"
	"syscall"
	"testing"
)

// TestHoldersAtNamesTheProcessHoldingTheLock: the whole point — a held lock has
// to yield the pid of the process holding it, or nothing can be escalated
// against a squatter.
func TestHoldersAtNamesTheProcessHoldingTheLock(t *testing.T) {
	// Arrange — this test process holds the flock on a real file.
	path := filepath.Join(t.TempDir(), "workspace-deadbeef.lock")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("creating the lock file: %v", err)
	}
	defer f.Close()
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("locking: %v", err)
	}

	// Act
	pids, err := holdersAt(path)

	// Assert
	if err != nil {
		t.Fatalf("holdersAt = %v, want the holder named", err)
	}
	var found bool
	for _, pid := range pids {
		if pid == os.Getpid() {
			found = true
		}
	}
	if !found {
		t.Fatalf("holders = %v, want this process (%d) among them", pids, os.Getpid())
	}
}

// TestHoldersAtOfAnUnlockedFileNamesNobody: an existing but unheld lock file is
// an empty answer, not a pid to kill.
func TestHoldersAtOfAnUnlockedFileNamesNobody(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "workspace-deadbeef.lock")
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("creating the lock file: %v", err)
	}

	// Act
	pids, err := holdersAt(path)

	// Assert
	if err != nil {
		t.Fatalf("holdersAt = %v, want an empty answer", err)
	}
	if len(pids) != 0 {
		t.Fatalf("holders = %v, want none", pids)
	}
}

// TestHoldersAtOfAMissingFileNamesNobody: no lock file means no shim ever
// claimed the workspace.
func TestHoldersAtOfAMissingFileNamesNobody(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "absent.lock")

	// Act
	pids, err := holdersAt(path)

	// Assert
	if err != nil || len(pids) != 0 {
		t.Fatalf("holdersAt = (%v, %v), want (nil, nil)", pids, err)
	}
}

// TestHoldersAtRejectsAnEmptyPath: an unresolvable lock path is "I could not
// tell", never "nobody holds it".
func TestHoldersAtRejectsAnEmptyPath(t *testing.T) {
	// Arrange / Act
	_, err := holdersAt("")

	// Assert
	if err == nil {
		t.Fatal("holdersAt(\"\") = nil error, want a refusal")
	}
}

// TestParsePIDsSkipsUnparsableLines: one odd line from lsof must not throw away
// the pids that did parse.
func TestParsePIDsSkipsUnparsableLines(t *testing.T) {
	// Arrange
	out := []byte("12\nnot-a-pid\n\n34\n-1\n")

	// Act
	pids := parsePIDs(out)

	// Assert
	if len(pids) != 2 || pids[0] != 12 || pids[1] != 34 {
		t.Fatalf("parsePIDs = %v, want [12 34]", pids)
	}
}
