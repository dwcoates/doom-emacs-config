package stack

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/integration/harness"

	"agentrepl/testrun/testenv"
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

func TestStopProcessMarksTheStopAskedSoTheLatchIgnoresIt(t *testing.T) {
	cases := []struct {
		name string
		// stop stops the process through stopProcess; otherwise it leaves on
		// its own.
		stop      bool
		wantAsked bool
	}{
		{name: "a stop through stopProcess is asked", stop: true, wantAsked: true},
		{name: "a process that left on its own is unasked"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command("/bin/sh", "-c", "read _")
			stdin, err := cmd.StdinPipe()
			if err != nil {
				t.Fatalf("stdin: %v", err)
			}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			exit := watchProcess(cmd)
			t.Cleanup(func() { _ = stdin.Close(); <-exit.done })

			// Act
			if tc.stop {
				stopProcess(t, "store", cmd, exit, "")
			} else {
				_ = stdin.Close()
				<-exit.done
			}

			// Assert
			if got := exit.asked.Load(); got != tc.wantAsked {
				t.Fatalf("asked = %v, want %v", got, tc.wantAsked)
			}
		})
	}
}

// A PROFILING RUN PROFILES EVERY SERVICE A WORLD STARTS: startService hands
// the service the test's whole-life profile directory (testenv.ExitProfile).
// Not parallel: the run's directory is the process's environment.
func TestStartServiceTellsAProfilingRunsServiceItsDirectory(t *testing.T) {
	// Arrange
	root := t.TempDir()
	t.Setenv(testenv.ProfileDir, root)
	out := filepath.Join(t.TempDir(), "told")

	// Act
	cmd := startService(t, "store", "start", "/bin/sh", []string{"-c", `printf %s "$` + testenv.ExitProfileEnv + `" > "$0"`, out}, t.TempDir())
	if err := cmd.Wait(); err != nil {
		t.Fatal(err)
	}

	// Assert
	got, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if want := filepath.Join(root, testenv.ProfileName(t.Name())); string(got) != want {
		t.Fatalf("the service was told %q, want the test's directory %q", got, want)
	}
}
