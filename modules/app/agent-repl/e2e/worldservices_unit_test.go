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
	requireServicesOutlivedTheTest(r, store, sidecar, nil)

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
	requireServicesOutlivedTheTest(r, store, sidecar, nil)

	// Assert
	if r.Failed() {
		t.Fatalf("failures = %q, want none for a stopped sidecar", r.Failures())
	}
}

func TestAZettelThatDiedOnItsOwnFailsTheTest(t *testing.T) {
	cases := []struct {
		name     string
		zettel   *Zettel
		wantFail bool
	}{
		{name: "no zettel in the world", zettel: nil},
		{name: "a zettel still running", zettel: &Zettel{exit: runningProcess()}},
		{name: "a zettel the test stopped", zettel: &Zettel{exit: exitedProcess(nil), stopped: true}},
		{name: "a zettel that died on its own", zettel: &Zettel{exit: exitedProcess(errors.New("signal: killed"))}, wantFail: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := harness.NewRecordingTB(t, "TestUser")
			store := &Store{exit: runningProcess()}
			sidecar := &Sidecar{exit: runningProcess()}

			// Act
			requireServicesOutlivedTheTest(r, store, sidecar, tc.zettel)

			// Assert
			if got := strings.Contains(r.Failures(), "zettel exited before test cleanup"); got != tc.wantFail {
				t.Fatalf("failures = %q, want zettel's death reported %v", r.Failures(), tc.wantFail)
			}
		})
	}
}
