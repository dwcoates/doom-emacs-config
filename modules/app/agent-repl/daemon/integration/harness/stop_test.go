package harness

import (
	"os/exec"
	"testing"
	"time"
)

// TestAwaitReapWithinBoundsTheWait pins the property that keeps a wedged
// daemon from costing a whole suite its budget: the harness's teardown wait
// is BOUNDED. Before this, Daemon.Stop called cmd.Wait directly, so a child
// that ignored SIGTERM blocked the calling test until the go-test alarm.
//
// Both arms drive a real child process — nothing external, no git, no vendor.
func TestAwaitReapWithinBoundsTheWait(t *testing.T) {
	tests := []struct {
		name     string
		args     []string
		budget   time.Duration
		kill     bool
		wantDone bool
	}{
		{
			name:     "a child that leaves promptly is reaped inside the budget",
			args:     []string{"-c", "exit 0"},
			budget:   DefaultTimeout,
			wantDone: true,
		},
		{
			name:     "a child that outlives the budget is reported, not waited on",
			args:     []string{"-c", "trap '' TERM; while :; do :; done"},
			budget:   50 * time.Millisecond,
			kill:     true,
			wantDone: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a real child under a bare Daemon carrying only the
			// fields the reap path touches.
			cmd := exec.Command("/bin/sh", tc.args...)
			if err := cmd.Start(); err != nil {
				t.Fatalf("start child: %v", err)
			}
			d := &Daemon{t: t, cmd: cmd}
			if tc.kill {
				t.Cleanup(func() {
					if err := cmd.Process.Kill(); err != nil {
						t.Errorf("kill child: %v", err)
					}
					d.awaitReapWithin(reapGrace)
				})
			}

			// Act.
			started := time.Now()
			done := d.awaitReapWithin(tc.budget)
			elapsed := time.Since(started)

			// Assert.
			if done != tc.wantDone {
				t.Fatalf("awaitReapWithin = %v, want %v", done, tc.wantDone)
			}
			if elapsed > DefaultTimeout+reapGrace {
				t.Fatalf("awaitReapWithin took %s, past every bound it is meant to honor", elapsed)
			}
		})
	}
}
