package e2e

import (
	"os/exec"
	"testing"
	"time"

	"agent-repld/integration/grantedclock"
)

// TestProcessExitAnswersEveryObserver pins the property whose absence turned
// one failing subtest into a 45-minute suite timeout: the exit of a child
// process must be observable by an UNBOUNDED NUMBER of readers, in any order.
// The previous `done chan error` of capacity one was drained by the first
// reader (the non-disturbing Exited() probe in NewWorld's exit-check
// cleanup), leaving the very next reader (Sidecar.Stop) waiting on an empty
// channel forever.
//
// The subject is exercised without a process: exit() is the seam a real
// cmd.Wait feeds, so a table can drive the orderings a live run cannot.
func TestProcessExitAnswersEveryObserver(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		probe func(t *testing.T, p *processExit)
	}{
		{
			name: "repeated exited probes all report the exit",
			probe: func(t *testing.T, p *processExit) {
				for i := range 5 {
					if !p.exited() {
						t.Fatalf("exited() probe %d reported the process still running", i)
					}
				}
			},
		},
		{
			name: "a bounded await after an exited probe still observes the exit",
			probe: func(t *testing.T, p *processExit) {
				if !p.exited() {
					t.Fatalf("exited() reported the process still running")
				}
				if !p.awaitWithin(DefaultTimeout) {
					t.Fatal("awaitWithin timed out after an earlier exited() probe consumed the exit")
				}
			},
		},
		{
			name: "repeated bounded awaits all observe the exit",
			probe: func(t *testing.T, p *processExit) {
				for i := range 5 {
					if !p.awaitWithin(DefaultTimeout) {
						t.Fatalf("awaitWithin %d timed out", i)
					}
				}
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a tracker whose process has already left.
			p := &processExit{done: make(chan struct{})}
			close(p.done)

			// Act + Assert: every observer sees the same answer.
			tc.probe(t, p)
		})
	}
}

// TestProcessExitAwaitWithinBoundsARunningProcess pins the other half: a
// process that has NOT left must make awaitWithin return false within its
// budget rather than block. This is what keeps a wedged child from hanging
// the suite.
func TestProcessExitAwaitWithinBoundsARunningProcess(t *testing.T) {
	t.Parallel()
	// Arrange: an exit that never arrives.
	p := &processExit{done: make(chan struct{})}
	budget := 20 * time.Millisecond

	// THE BUDGET IS GRANTED TIME (grantedclock), as every bound this layer
	// keeps on its own processes is, so it is measured on the granted clock:
	// on a loaded host a 20ms budget rightly took 12.8s of wall time.
	stall, err := grantedclock.Source()
	if err != nil {
		t.Fatalf("read the granted clock's source: %v", err)
	}
	clock, err := grantedclock.Start(time.Now, stall)
	if err != nil {
		t.Fatalf("start the granted clock: %v", err)
	}

	// Act.
	left := p.awaitWithin(budget)
	span, err := clock.Span()
	if err != nil {
		t.Fatalf("read the granted clock: %v", err)
	}

	// Assert.
	if left {
		t.Fatal("awaitWithin claimed a still-running process had left")
	}
	// Granted time accrues reading by reading, so this clock, read once,
	// cannot see the granted time the bound's own clock saw. Granted never
	// exceeds wall, though, so a budget that ran out has taken at least that
	// much wall time.
	if span.Wall < budget {
		t.Fatalf("awaitWithin returned after %s, short of its %s budget", span, budget)
	}
	if span.Granted > DefaultTimeout {
		t.Fatalf("awaitWithin took %s, far beyond its %s budget", span, budget)
	}
}

// TestProcessExitReportsARealChildsExit pins watchProcess against a real
// child. The child is this test binary's own `go` toolchain-free stand-in —
// /bin/sh exiting immediately — so no external service, git, or vendor is
// involved.
func TestProcessExitReportsARealChildsExit(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		args    []string
		wantErr bool
	}{
		{name: "a child that exits 0 is reaped with no error", args: []string{"-c", "exit 0"}, wantErr: false},
		{name: "a child that exits nonzero surfaces its wait error", args: []string{"-c", "exit 3"}, wantErr: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cmd := exec.Command("/bin/sh", tc.args...)
			if err := cmd.Start(); err != nil {
				t.Fatalf("start child: %v", err)
			}

			// Act.
			p := watchProcess(cmd)
			if !p.awaitWithin(DefaultTimeout) {
				t.Fatalf("the child did not exit within %s", DefaultTimeout)
			}

			// Assert.
			if gotErr := p.err != nil; gotErr != tc.wantErr {
				t.Fatalf("wait error = %v, want an error: %v", p.err, tc.wantErr)
			}
			if !p.exited() {
				t.Fatal("exited() reported the reaped child still running")
			}
		})
	}
}
