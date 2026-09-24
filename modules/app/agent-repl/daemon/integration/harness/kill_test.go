package harness

import (
	"bufio"
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
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

// groupUnderKill starts a stand-in for the daemon: a shell leading its own
// process group, with one member of that group running under it the way the
// daemon's gits do. script must start the member in the background and print
// its pid first. It answers the bare Daemon Kill acts on and the member's pid.
func groupUnderKill(t *testing.T, script string) (*Daemon, int) {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", script)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the group: %v", err)
	}
	d := &Daemon{t: t, cmd: cmd}
	t.Cleanup(func() {
		if !d.reaped() {
			_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
			d.Wait()
		}
	})
	line, err := bufio.NewReader(out).ReadString('\n')
	if err != nil {
		t.Fatalf("read the member's pid: %v", err)
	}
	member, err := strconv.Atoi(strings.TrimSpace(line))
	if err != nil {
		t.Fatalf("the member's pid %q: %v", line, err)
	}
	return d, member
}

// TestKillFreezesTheWholeGroupBeforeKillingAnyOfIt pins the ordering the
// daemon's teardown depends on: at the instant before the SIGKILL, the leader
// and its member are BOTH stopped and neither is dead, so no member can die
// while the leader can still run.
func TestKillFreezesTheWholeGroupBeforeKillingAnyOfIt(t *testing.T) {
	// Arrange
	d, member := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait")
	var leaderState, memberState processState
	var leaderErr, memberErr error
	d.afterFreeze = func() {
		leaderState, leaderErr = readProcessState(d.cmd.Process.Pid)
		memberState, memberErr = readProcessState(member)
	}

	// Act
	d.Kill()

	// Assert
	if leaderErr != nil || memberErr != nil {
		t.Fatalf("reading the frozen group: leader %v, member %v", leaderErr, memberErr)
	}
	if leaderState.name != "stopped" {
		t.Fatalf("the leader was %s when the kill was sent, want stopped", leaderState.name)
	}
	if memberState.name != "stopped" {
		t.Fatalf("the member was %s when the kill was sent, want stopped", memberState.name)
	}
}

// TestKillEndsEveryMemberOfTheGroup pins that freezing first still kills the
// whole group, not only its leader.
func TestKillEndsEveryMemberOfTheGroup(t *testing.T) {
	// Arrange
	d, member := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait")
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Act
	d.Kill()

	// Assert
	if !d.reaped() {
		t.Fatal("the leader is unreaped after Kill")
	}
	if err := WaitProcessExit(ctx, member); err != nil {
		t.Fatalf("the member %d outlived the group kill: %v", member, err)
	}
}

// TestKillLeavesTheLeaderNoInstantToObserveAMemberDying is the defect as the
// daemon lived it: a leader that records the death of its member (the daemon
// logging "git was killed by a signal") must never get to, because the member
// only dies once the leader can no longer run.
func TestKillLeavesTheLeaderNoInstantToObserveAMemberDying(t *testing.T) {
	// Arrange: the leader writes the marker the moment its member dies.
	marker := filepath.Join(t.TempDir(), "observed")
	d, _ := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait $!; : > "+marker)

	// Act
	d.Kill()

	// Assert
	if _, err := os.Stat(marker); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the leader recorded its member's death (stat %v), want it frozen before the member died", err)
	}
}

func TestNamesPath(t *testing.T) {
	cases := []struct {
		name string
		line string
		want bool
	}{
		{name: "the directory ending the line", line: "123 /bin/daemon --state-dir /tmp/r/ar12", want: true},
		{name: "the directory followed by an argument", line: "123 /bin/daemon --state-dir /tmp/r/ar12 --fake", want: true},
		{name: "a path beneath the directory", line: "123 /bin/shim --listen /tmp/r/ar12/sock/s", want: true},
		{name: "a sibling the directory is a textual prefix of", line: "123 /bin/shim --listen /tmp/r/ar123/sock/s", want: false},
		{name: "a sibling first, then the directory itself", line: "123 /bin/x /tmp/r/ar123/a /tmp/r/ar12/b", want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := namesPath(tc.line, "/tmp/r/ar12")

			// Assert
			if got != tc.want {
				t.Fatalf("namesPath(%q) = %v, want %v", tc.line, got, tc.want)
			}
		})
	}
}
