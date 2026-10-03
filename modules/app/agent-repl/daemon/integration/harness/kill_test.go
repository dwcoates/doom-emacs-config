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
	"time"
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
			// what Kill DOES with it, not about how it comes to be set. The
			// mark Kill reads is reapBegun, set before cmd.Wait frees the pid.
			cmd := exec.Command("/bin/sh", "-c", "while :; do sleep 1; done")
			cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start child: %v", err)
			}
			d := &Daemon{t: t, cmd: cmd}
			if tc.reaped {
				d.sigMu.Lock()
				d.reapBegun = true
				d.sigMu.Unlock()
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

// TestKillEndsTheLeaderBeforeSignalingAnyMember pins the ordering the
// daemon's teardown depends on: at the instant the group is signaled to die,
// the leader has already exited, so no member can die while the leader can
// still run. It reads the leader alone, because exiting is irrevocable and a
// stop is not: a member inside execve when the group is stopped runs again
// once its exec completes, and the leader's exit orphans the group, which
// continues its stopped members.
func TestKillEndsTheLeaderBeforeSignalingAnyMember(t *testing.T) {
	// Arrange
	d, _ := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait")
	var leaderState processState
	var leaderErr error
	d.afterLeaderExit = func() {
		leaderState, leaderErr = readProcessState(d.cmd.Process.Pid)
	}

	// Act
	d.Kill()

	// Assert
	if leaderErr != nil {
		t.Fatalf("reading the leader before the group kill: %v", leaderErr)
	}
	if !leaderState.exited {
		t.Fatalf("the leader was %s when the group kill was sent, want exited", leaderState.name)
	}
}

// TestKillOwnsTheReapARacingWaitLeftInFlight is Stop giving up on SIGTERM and
// falling through to Kill: the bounded wait it gave up on leaves a reap in
// flight, and that reap must not free the leader's pid, and with it the group
// id, between the leader's SIGKILL and the group's.
func TestKillOwnsTheReapARacingWaitLeftInFlight(t *testing.T) {
	// Arrange: a reap in flight, as Stop's expired wait leaves one.
	d, _ := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait")
	d.awaitReapWithin(0)
	reapedDuringKill := false
	d.afterLeaderExit = func() {
		reapedDuringKill = d.awaitReapWithin(50 * time.Millisecond)
	}

	// Act
	d.Kill()

	// Assert
	if reapedDuringKill {
		t.Fatal("the in-flight reap freed the leader between its SIGKILL and the group's, want it held until the group kill")
	}
	if !d.reaped() {
		t.Fatal("the leader is unreaped after Kill")
	}
}

// TestKillEndsEveryMemberOfTheGroup pins that ending the leader first still
// kills the whole group, not only its leader.
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
// only dies once the leader has exited.
func TestKillLeavesTheLeaderNoInstantToObserveAMemberDying(t *testing.T) {
	// Arrange: the leader writes the marker the moment its member dies.
	marker := filepath.Join(t.TempDir(), "observed")
	d, _ := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait $!; : > "+marker)

	// Act
	d.Kill()

	// Assert
	if _, err := os.Stat(marker); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the leader recorded its member's death (stat %v), want it dead before the member died", err)
	}
}

// TestKillLeavesTheLeaderNoInstantToObserveAMemberTheStopDidNotHold is the
// flake's condition made certain: a stop the kernel discards, as it does for
// a member inside execve, must not give the leader an instant to record its
// member's death. The whole group is continued after the stop, so neither
// leader nor member is held by it.
func TestKillLeavesTheLeaderNoInstantToObserveAMemberTheStopDidNotHold(t *testing.T) {
	// Arrange: the leader writes the marker the moment its member dies.
	marker := filepath.Join(t.TempDir(), "observed")
	d, _ := groupUnderKill(t, "/bin/sleep 100 & echo $!; wait $!; : > "+marker)
	var contErr error
	d.afterGroupStopped = func() {
		contErr = syscall.Kill(-d.cmd.Process.Pid, syscall.SIGCONT)
	}

	// Act
	d.Kill()

	// Assert
	if contErr != nil {
		t.Fatalf("continuing the stopped group: %v", contErr)
	}
	if _, err := os.Stat(marker); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the leader recorded its member's death (stat %v), want it dead before the member died", err)
	}
}

// respawningStray starts a stand-in for a live daemon the harness never
// started (a layout change's replacement): a process naming stateDir that
// spawns a child naming it too, and spawns another the moment that child
// dies, the way a daemon revives a shim that died on its own. It answers the
// respawner, its first child's pid, and a read that blocks until the
// respawner reports its next child and answers that child's pid.
func respawningStray(t *testing.T, stateDir string) (*exec.Cmd, int, func() int) {
	t.Helper()
	script := `while :; do /bin/sh -c 'while :; do /bin/sleep 1; done' "$0/child" & echo $!; wait $!; done`
	cmd := exec.Command("/bin/sh", "-c", script, stateDir)
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("stdout: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the respawner: %v", err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
	})
	pids := bufio.NewReader(out)
	next := func() int {
		t.Helper()
		line, err := pids.ReadString('\n')
		if err != nil {
			t.Fatalf("read the child's pid: %v", err)
		}
		child, err := strconv.Atoi(strings.TrimSpace(line))
		if err != nil {
			t.Fatalf("the child's pid %q: %v", line, err)
		}
		t.Cleanup(func() { _ = syscall.Kill(child, syscall.SIGKILL) })
		return child
	}
	return cmd, next(), next
}

// TestReapStraysFreezesEveryStrayBeforeKillingAny pins the ordering that
// keeps a live stray from replacing what the sweep kills: at the instant a
// stray dies, every other stray is already stopped, so a respawner that would
// bring the dead one back cannot run.
func TestReapStraysFreezesEveryStrayBeforeKillingAny(t *testing.T) {
	// Arrange
	stateDir := t.TempDir()
	respawner, child, _ := respawningStray(t, stateDir)
	d := &Daemon{t: t, StateDir: stateDir}
	var respawnerState processState
	var stateErr, exitErr error
	d.afterStraysFrozen = func() {
		// The child dies first, as it did under a snapshot-then-kill sweep.
		if err := syscall.Kill(child, syscall.SIGKILL); err != nil {
			exitErr = err
			return
		}
		ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
		defer cancel()
		exitErr = WaitProcessExit(ctx, child)
		respawnerState, stateErr = readProcessState(respawner.Process.Pid)
	}

	// Act
	d.ReapStrays()

	// Assert
	if exitErr != nil || stateErr != nil {
		t.Fatalf("killing the child first: exit %v, reading the respawner %v", exitErr, stateErr)
	}
	if respawnerState.name != "stopped" {
		t.Fatalf("the respawner was %s when its child died, want stopped", respawnerState.name)
	}
}

// TestReapStraysLeavesNoReplacementOfAStrayKilledFirst is the leak as the
// layout-restart test lived it: a stray killed ahead of the live process that
// supervises it is not brought back, and nothing naming the state directory
// is left running.
func TestReapStraysLeavesNoReplacementOfAStrayKilledFirst(t *testing.T) {
	// Arrange
	stateDir := t.TempDir()
	_, child, _ := respawningStray(t, stateDir)
	d := &Daemon{t: t, StateDir: stateDir}
	d.afterStraysFrozen = func() { _ = syscall.Kill(child, syscall.SIGKILL) }

	// Act
	d.ReapStrays()

	// Assert
	if err := d.leakedStrays(); err != nil {
		t.Fatalf("leakedStrays after the reap = %v, want none", err)
	}
}

// TestReapStraysLeavesNoReplacementSpawnedThroughADiscardedStop is the hole a
// freeze alone left: a stray whose stop the kernel discards, as it does for a
// process inside execve, runs on through the sweep and spawns a replacement
// no listing so far has named. The respawner is continued once it is stopped
// and its child killed, and the sweep begins its kills only once the
// replacement exists, so the replacement is certain, not raced for.
func TestReapStraysLeavesNoReplacementSpawnedThroughADiscardedStop(t *testing.T) {
	// Arrange
	stateDir := t.TempDir()
	respawner, child, next := respawningStray(t, stateDir)
	d := &Daemon{t: t, StateDir: stateDir}
	escaped := false
	var escapeErr error
	d.afterStraysFrozen = func() {
		if escaped {
			return
		}
		escaped = true
		if err := syscall.Kill(respawner.Process.Pid, syscall.SIGCONT); err != nil {
			escapeErr = err
			return
		}
		if err := syscall.Kill(child, syscall.SIGKILL); err != nil {
			escapeErr = err
			return
		}
		next()
	}

	// Act
	d.ReapStrays()

	// Assert
	if escapeErr != nil {
		t.Fatalf("letting the respawner escape its stop: %v", escapeErr)
	}
	if err := d.leakedStrays(); err != nil {
		t.Fatalf("leakedStrays after the reap = %v, want none", err)
	}
}

func TestLeakedStrays(t *testing.T) {
	cases := []struct {
		name string
		// arrange leaves the state directory's process set in the state
		// under test.
		arrange    func(t *testing.T, stateDir string)
		wantLeaked bool
	}{
		{
			name:       "no process names the state directory",
			arrange:    func(*testing.T, string) {},
			wantLeaked: false,
		},
		{
			name: "a running process naming the state directory is a leak",
			arrange: func(t *testing.T, stateDir string) {
				cmd := exec.Command("/bin/sh", "-c", "while :; do /bin/sleep 1; done", stateDir+"/sock/s.sock")
				if err := cmd.Start(); err != nil {
					t.Fatalf("start: %v", err)
				}
				t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
			},
			wantLeaked: true,
		},
		{
			name: "an exited, unreaped process naming the state directory is not a leak",
			arrange: func(t *testing.T, stateDir string) {
				cmd := exec.Command("/bin/sh", "-c", "while :; do /bin/sleep 1; done", stateDir+"/sock/s.sock")
				if err := cmd.Start(); err != nil {
					t.Fatalf("start: %v", err)
				}
				t.Cleanup(func() { _ = cmd.Wait() })
				if err := cmd.Process.Kill(); err != nil {
					t.Fatalf("SIGKILL: %v", err)
				}
				if err := awaitFrozen(cmd.Process.Pid, DefaultTimeout); err != nil {
					t.Fatalf("await the exit: %v", err)
				}
			},
			wantLeaked: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			stateDir := t.TempDir()
			tc.arrange(t, stateDir)
			d := &Daemon{t: t, StateDir: stateDir}

			// Act
			err := d.leakedStrays()

			// Assert
			if leaked := errors.Is(err, ErrLeakedProcess); leaked != tc.wantLeaked {
				t.Fatalf("leakedStrays() = %v, want a leak reported = %v", err, tc.wantLeaked)
			}
			if !tc.wantLeaked && err != nil {
				t.Fatalf("leakedStrays() = %v, want nil", err)
			}
		})
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

// TestAwaitKilledLeaderReportsAStuckExitAndKeepsWaiting pins the stuck-kill
// report: a SIGKILLed leader still alive past the report bound is reported
// once, while the wait goes on, and nothing else is ever reported.
func TestAwaitKilledLeaderReportsAStuckExitAndKeepsWaiting(t *testing.T) {
	errWait := errors.New("the exit event could not be registered")
	tests := []struct {
		name       string
		exitsAfter bool // the first (bounded) wait reaches its deadline
		failWith   error
		wantStalls int
		wantWaits  int
		wantErr    error
	}{
		{name: "an exit inside the bound reports nothing", wantStalls: 0, wantWaits: 1},
		{name: "an exit past the bound is reported once and still awaited", exitsAfter: true, wantStalls: 1, wantWaits: 2},
		{name: "a failed wait is answered, never reported as a stall", failWith: errWait, wantStalls: 0, wantWaits: 1, wantErr: errWait},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			waits, stalls := 0, 0
			wait := func(ctx context.Context, pid int) error {
				waits++
				if tt.failWith != nil {
					return tt.failWith
				}
				if _, bounded := ctx.Deadline(); bounded && tt.exitsAfter {
					<-ctx.Done()
					return context.DeadlineExceeded
				}
				return nil
			}
			stalled := func(int, time.Duration) { stalls++ }

			// Act
			err := awaitKilledLeader(4242, time.Millisecond, wait, stalled)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Errorf("err = %v, want %v", err, tt.wantErr)
			}
			if stalls != tt.wantStalls || waits != tt.wantWaits {
				t.Errorf("stalls = %d, waits = %d, want %d and %d", stalls, waits, tt.wantStalls, tt.wantWaits)
			}
		})
	}
}
