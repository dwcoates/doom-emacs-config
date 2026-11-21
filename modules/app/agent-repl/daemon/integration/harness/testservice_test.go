package harness

import (
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestTheTestsSocketIsASiblingOfTheStateRoot(t *testing.T) {
	// Act
	got := TestsSocketFor("/tmp/w/state")

	// Assert
	if want := "/tmp/w/tests-state/tests.sock"; got != want {
		t.Fatalf("TestsSocketFor = %q, want %q", got, want)
	}
}

func TestTwoStateRootsUnderOneParentGetTwoTestsSockets(t *testing.T) {
	// Act: two lifecycle steps' roots share a parent, and so a lock dir.
	first, second := TestsSocketFor("/tmp/f/s1"), TestsSocketFor("/tmp/f/s2")

	// Assert
	if first == second {
		t.Fatalf("both roots got the test service socket %s; want one each", first)
	}
	if LockDirFor("/tmp/f/s1") != LockDirFor("/tmp/f/s2") {
		t.Fatal("the two roots' lock dirs differ; want the parent's one")
	}
}

// TestTheHarnessDaemonIsHandedItsTestService pins the harness's launcher to
// the test service of its state root, so no harness daemon's gate reaches
// the owner's test service.
func TestTheHarnessDaemonIsHandedItsTestService(t *testing.T) {
	// Act
	source := readOrFail(t, "daemon.go")

	// Assert
	for _, want := range []string{"ensureTestService(t, d.StateDir, d.LockDir)", `"--tests-socket", d.TestsSocket`} {
		if !strings.Contains(source, want) {
			t.Fatalf("daemon.go states no %s", want)
		}
	}
}

// writeRun lays out one run of a test service's registry: its run.json, and
// end.json when ENDED.
func writeRun(t *testing.T, runs, id, runJSON string, ended bool) {
	t.Helper()
	dir := filepath.Join(runs, id)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if runJSON != "" {
		if err := os.WriteFile(filepath.Join(dir, "run.json"), []byte(runJSON), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if ended {
		if err := os.WriteFile(filepath.Join(dir, "end.json"), []byte(`{"exit_code":0}`), 0o644); err != nil {
			t.Fatal(err)
		}
	}
}

func TestKillRunsLeftGoing(t *testing.T) {
	cases := []struct {
		name     string
		lay      func(t *testing.T, runs string)
		killErr  error
		wantKill []int
		wantErr  string
	}{
		{name: "a run going is killed with its group",
			lay:      func(t *testing.T, runs string) { writeRun(t, runs, "a", `{"pgid":4242}`, false) },
			wantKill: []int{4242}},
		{name: "an ended run is left alone",
			lay: func(t *testing.T, runs string) { writeRun(t, runs, "a", `{"pgid":4242}`, true) }},
		{name: "a run with no record is answered",
			lay:     func(t *testing.T, runs string) { writeRun(t, runs, "a", "", false) },
			wantErr: "read the test service's run a"},
		{name: "a run naming no group is answered",
			lay:     func(t *testing.T, runs string) { writeRun(t, runs, "a", `{"pgid":0}`, false) },
			wantErr: "the test service's run a names no group"},
		{name: "a kill that fails is answered",
			lay:      func(t *testing.T, runs string) { writeRun(t, runs, "a", `{"pgid":4242}`, false) },
			killErr:  errors.New("operation not permitted"),
			wantKill: []int{4242}, wantErr: "kill the test service's run a (group 4242): operation not permitted"},
		{name: "a registry that cannot be read is answered",
			lay:     func(t *testing.T, runs string) { _ = os.RemoveAll(runs) },
			wantErr: "read the test service's runs"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runs := filepath.Join(t.TempDir(), "runs")
			if err := os.MkdirAll(runs, 0o755); err != nil {
				t.Fatal(err)
			}
			tc.lay(t, runs)
			var killed []int

			// Act
			errs := killRunsLeftGoing(runs, func(pgid int) error {
				killed = append(killed, pgid)
				return tc.killErr
			})

			// Assert
			if !slices.Equal(killed, tc.wantKill) {
				t.Fatalf("killed %v, want %v", killed, tc.wantKill)
			}
			if tc.wantErr == "" {
				if len(errs) != 0 {
					t.Fatalf("errors %v, want none", errs)
				}
				return
			}
			if len(errs) != 1 || !strings.Contains(errs[0].Error(), tc.wantErr) {
				t.Fatalf("errors %v, want one naming %q", errs, tc.wantErr)
			}
		})
	}
}
