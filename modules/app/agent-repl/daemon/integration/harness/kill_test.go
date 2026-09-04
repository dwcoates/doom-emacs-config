package harness

import (
	"os/exec"
	"syscall"
	"testing"
)

// TestKillNeverSignalsAReapedProcess pins the property that keeps the cleanup
// kill from reaching a stranger: once cmd.Wait has returned, the kernel has
// freed the pid — and with it the process group id that shares it — so
// `kill(-pid)` can only reach whatever the pid was recycled into. That is what
// surfaced as `SIGKILL process group N: operation not permitted` in every test
// that waits for its daemon to exit on its own before the cleanup kill runs.
//
// Both arms drive a real child process — nothing external, no git, no vendor.
func TestKillNeverSignalsAReapedProcess(t *testing.T) {
	tests := []struct {
		name      string
		reaped    bool
		wantAlive bool
	}{
		{
			name:      "a process recorded as reaped is left entirely alone",
			reaped:    true,
			wantAlive: true,
		},
		{
			name:      "a process not yet reaped has its whole group killed",
			reaped:    false,
			wantAlive: false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a real child in a group of its own, under a bare
			// Daemon carrying only the fields the kill path touches. The
			// reaped flag is set directly, because the assertion is about
			// what Kill DOES with it, not about how it comes to be set.
			cmd := exec.Command("/bin/sh", "-c", "while :; do sleep 1; done")
			cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start child: %v", err)
			}
			d := &Daemon{t: t, cmd: cmd}
			if tc.reaped {
				d.mu.Lock()
				d.exited = true
				d.mu.Unlock()
				t.Cleanup(func() {
					_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
					_ = cmd.Wait()
				})
			}

			// Act.
			d.Kill()

			// Assert: signal 0 probes liveness without disturbing the child.
			alive := cmd.Process.Signal(syscall.Signal(0)) == nil
			if alive != tc.wantAlive {
				t.Fatalf("the child is alive = %v after Kill, want %v", alive, tc.wantAlive)
			}
		})
	}
}

// TestReapStraysSparesATestOwnedProcess pins the exemption that keeps a
// world's OWN store and sidecar out of the stray sweep. Both name this run's
// state directory in their argv (their --log paths live under it), which is
// the only key strayPIDs has; without the exemption ReapStrays SIGKILLs them,
// and a SIGKILLed process writes no exit record at all — the failure that
// surfaced as "the sidecar exited before test cleanup" with nothing but
// ordinary records in its log.
//
// It asserts on the SELECTION (StrayPIDs) rather than on the kill, because the
// selection is where the fault is and liveness after a signal is a race with
// the reap. The child is a real process whose argv names a real state
// directory: nothing external, no git, no vendor.
func TestReapStraysSparesATestOwnedProcess(t *testing.T) {
	tests := []struct {
		name      string
		spare     bool
		wantStray bool
	}{
		{
			name:      "a pid declared the test's own is not a stray",
			spare:     true,
			wantStray: false,
		},
		{
			name:      "an undeclared pid naming the state directory is a stray",
			spare:     false,
			wantStray: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a child whose argv carries the state directory, the
			// same way the store's and the sidecar's --log paths do.
			stateDir := t.TempDir()
			cmd := exec.Command("/bin/sh", "-c", "while :; do sleep 1; done # --log "+stateDir+"/logs/x.log")
			if err := cmd.Start(); err != nil {
				t.Fatalf("start child: %v", err)
			}
			t.Cleanup(func() {
				_ = cmd.Process.Kill()
				_ = cmd.Wait()
			})
			if tc.spare {
				SpareFromStrayReaping(t, cmd.Process.Pid)
			}
			d := &Daemon{t: t, StateDir: stateDir}

			// Act.
			strays := d.StrayPIDs()

			// Assert.
			found := false
			for _, pid := range strays {
				if pid == cmd.Process.Pid {
					found = true
				}
			}
			if found != tc.wantStray {
				t.Fatalf("the child (pid %d) is among the strays = %v, want %v (strays: %v)",
					cmd.Process.Pid, found, tc.wantStray, strays)
			}
		})
	}
}
