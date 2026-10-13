package harness

import (
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"testing"
)

func TestSuccessorOptsCarriesThePredecessorsMachine(t *testing.T) {
	// Arrange
	d := &Daemon{StateDir: "/s/state", ProfileDir: "/s/profiles", DefaultConfigDir: "/s/config-default"}

	// Act
	got := d.SuccessorOpts()

	// Assert
	if got.StateDir != d.StateDir || got.ProfileDir != d.ProfileDir {
		t.Fatalf("SuccessorOpts() = %+v, want the predecessor's state root and profiles", got)
	}
	if want := []string{"--default-config-dir", d.DefaultConfigDir}; !slices.Equal(got.ExtraArgs, want) {
		t.Fatalf("SuccessorOpts().ExtraArgs = %q, want %q", got.ExtraArgs, want)
	}
}

func TestSuccessorOptsSharesThePredecessorsLocks(t *testing.T) {
	// Arrange
	d := &Daemon{StateDir: "/s/state", LockDir: LockDirFor("/s/state")}

	// Act
	got := LockDirFor(d.SuccessorOpts().StateDir)

	// Assert
	if got != d.LockDir {
		t.Fatalf("the successor's lock directory = %q, want the predecessor's %q", got, d.LockDir)
	}
}

func TestSuccessorOptsAreFreshPerCall(t *testing.T) {
	// Arrange
	d := &Daemon{DefaultConfigDir: "/s/config-default"}
	first := d.SuccessorOpts()

	// Act: one caller extends its own successor's arguments.
	first.ExtraArgs = append(first.ExtraArgs, "--multi-repo-config-dir", "/s/multi")
	second := d.SuccessorOpts()

	// Assert
	if len(second.ExtraArgs) != 2 {
		t.Fatalf("a later SuccessorOpts().ExtraArgs = %q, want it untouched by an earlier caller's extension", second.ExtraArgs)
	}
}

// handRolledSuccessor is a StartDaemon option copying another daemon's
// profile directory: the successor shape SuccessorOpts owns.
var handRolledSuccessor = regexp.MustCompile(`ProfileDir:\s+[\w.]+\.ProfileDir\b`)

// TestEverySuccessorStartsThroughSuccessorOpts keeps the integration suite's
// successors on the one shape, so a hand-rolled one cannot drift from it (a
// successor on a fresh account root resumes nothing).
func TestEverySuccessorStartsThroughSuccessorOpts(t *testing.T) {
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration suite: %v (%d files)", err, len(files))
	}

	// Act, Assert
	for _, file := range files {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if loc := handRolledSuccessor.FindIndex(src); loc != nil {
			t.Errorf("%s hand-rolls a successor (%q); start it with SuccessorOpts", file, src[loc[0]:loc[1]])
		}
	}
}
