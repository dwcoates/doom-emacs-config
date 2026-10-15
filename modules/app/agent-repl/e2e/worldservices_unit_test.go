package e2e

import (
	"errors"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

// exitedProcess is a processExit for a process that has already left.
func exitedProcess(err error) *processExit {
	p := &processExit{done: make(chan struct{}), err: err}
	close(p.done)
	return p
}

func runningProcess() *processExit { return &processExit{done: make(chan struct{})} }

func TestAServiceThatDiedOnItsOwnFailsTheTest(t *testing.T) {
	// Arrange
	r := harness.NewRecordingTB(t, "TestUser")
	store := &Store{exit: runningProcess()}
	sidecar := &Sidecar{exit: exitedProcess(errors.New("signal: killed"))}

	// Act
	requireServicesOutlivedTheTest(r, store, sidecar)

	// Assert
	if !strings.Contains(r.Failures(), "the sidecar exited before test cleanup") {
		t.Fatalf("failures = %q, want the sidecar's death reported", r.Failures())
	}
}

func TestAServiceTheTestStoppedFailsNothing(t *testing.T) {
	// Arrange
	r := harness.NewRecordingTB(t, "TestUser")
	store := &Store{exit: runningProcess()}
	sidecar := &Sidecar{exit: exitedProcess(nil), stopped: true}

	// Act
	requireServicesOutlivedTheTest(r, store, sidecar)

	// Assert
	if r.Failed() {
		t.Fatalf("failures = %q, want none for a stopped sidecar", r.Failures())
	}
}
