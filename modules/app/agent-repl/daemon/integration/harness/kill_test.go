package harness

import (
	"agent-repld/internal/procgroup"
	"bufio"
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
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
			d := &Daemon{t: t, daemonProc: &daemonProc{cmd: cmd}}
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
			cmd := standIn(t, idleShell+" # --log "+stateDir+"/logs/x.log")
			if tc.spare {
				SpareFromStrayReaping(t, cmd.Process.Pid)
			}
			d := &Daemon{t: t, StateDir: stateDir, daemonProc: &daemonProc{}}

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

// sleepLoop is a stand-in shell's loop: it always has a `sleep 1` running.
const sleepLoop = "while :; do /bin/sleep 1; done"

// idleShell is a stand-in shell that runs no child at all: the `read` builtin
// blocks on the stdin pipe standIn holds open. A stand-in that must be the
// ONLY process naming its arguments uses it, because sleepLoop's every fork is
// a copy of the shell, argv and all, until its exec: killing the leader
// orphaned one such copy mid-fork, which a stray listing then read as a
// running process naming the state directory (2026-10-14, TestLeakedStrays).
const idleShell = "read line"

// standIn starts the shell script (with args as $0...) in a process group of
// its own, with a stdin pipe held open until it exits, and kills the whole
// group at cleanup, before reaping its leader.
//
// KILLING THE SHELL ALONE ORPHANED ITS LOOP'S `sleep 1`, which outlived the
// test: the test runner's last reap found one still running out of a harness
// run root (2026-10-14). The group dies whole, and it is killed while the
// unreaped leader still holds the group id, so no stranger can be signalled.
func standIn(t *testing.T, script string, args ...string) *exec.Cmd {
	t.Helper()
	return standInTo(t, nil, script, args...)
}

// standInTo is standIn with the shell's stdout on out.
func standInTo(t *testing.T, out *os.File, script string, args ...string) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("/bin/sh", append([]string{"-c", script}, args...)...)
	cmd.Stdout = out
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	// THE WRITE END IS NEVER WRITTEN, and Wait closes it once the shell
	// exits, so idleShell's read blocks for the shell's whole life.
	if _, err := cmd.StdinPipe(); err != nil {
		t.Fatalf("the stand-in's stdin: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the stand-in %q: %v", script, err)
	}
	t.Cleanup(func() {
		_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		_ = cmd.Wait()
	})
	return cmd
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
	d := &Daemon{t: t, daemonProc: &daemonProc{cmd: cmd}}
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
	script := `while :; do /bin/sh -c '` + sleepLoop + `' "$0/child" & echo $!; wait $!; done`
	r, w, err := os.Pipe()
	if err != nil {
		t.Fatalf("stdout: %v", err)
	}
	cmd := standInTo(t, w, script, stateDir)
	w.Close()
	t.Cleanup(func() { r.Close() })
	pids := bufio.NewReader(r)
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
	d := &Daemon{t: t, StateDir: stateDir, daemonProc: &daemonProc{}}
	var respawnerState processState
	var stateErr, exitErr error
	var frozenView string
	d.afterStraysFrozen = func() {
		// The child dies first, as it did under a snapshot-then-kill sweep.
		if err := syscall.Kill(child, syscall.SIGKILL); err != nil {
			exitErr = err
			frozenView = strayView(d, respawner.Process.Pid)
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
		// A child already gone at the freeze ended before the sweep began
		// (merge gate 2026-10-15, once, not reproduced alone or in its
		// package): the view says whether the respawner had started a
		// replacement, i.e. whether the child died while its respawner ran.
		t.Fatalf("killing the child %d first: exit %v, reading the respawner %v; at the freeze %s", child, exitErr, stateErr, frozenView)
	}
	if respawnerState.name != "stopped" {
		t.Fatalf("the respawner was %s when its child died, want stopped", respawnerState.name)
	}
}

// strayView is what a stray sweep saw: every process naming the state
// directory, with its kernel state, and the respawner's.
func strayView(d *Daemon, respawner int) string {
	strays, err := d.listStrays()
	if err != nil {
		return fmt.Sprintf("(listing failed: %v)", err)
	}
	var b strings.Builder
	for _, s := range strays {
		st, err := readProcessState(s.pid)
		fmt.Fprintf(&b, "\n  %d %q state %s (err %v)", s.pid, s.args, st.name, err)
	}
	st, err := readProcessState(respawner)
	fmt.Fprintf(&b, "\n  respawner %d state %s (err %v)", respawner, st.name, err)
	return b.String()
}

// TestReapStraysLeavesNoReplacementOfAStrayKilledFirst is the leak as the
// layout-restart test lived it: a stray killed ahead of the live process that
// supervises it is not brought back, and nothing naming the state directory
// is left running.
func TestReapStraysLeavesNoReplacementOfAStrayKilledFirst(t *testing.T) {
	// Arrange
	stateDir := t.TempDir()
	_, child, _ := respawningStray(t, stateDir)
	d := &Daemon{t: t, StateDir: stateDir, daemonProc: &daemonProc{}}
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
	d := &Daemon{t: t, StateDir: stateDir, daemonProc: &daemonProc{}}
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
				standIn(t, idleShell, stateDir+"/sock/s.sock")
			},
			wantLeaked: true,
		},
		{
			name: "an exited, unreaped process naming the state directory is not a leak",
			arrange: func(t *testing.T, stateDir string) {
				cmd := standIn(t, idleShell, stateDir+"/sock/s.sock")
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
			d := &Daemon{t: t, StateDir: stateDir, daemonProc: &daemonProc{}}

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
			wait := func(ctx context.Context, pid int) (bool, error) {
				waits++
				if tt.failWith != nil {
					return false, tt.failWith
				}
				// The first wait is bounded (on the granted clock, which has no
				// wall deadline); the second waits on context.Background.
				if bounded := ctx.Done() != nil; bounded && tt.exitsAfter {
					<-ctx.Done()
					return false, context.DeadlineExceeded
				}
				return true, nil
			}
			stalled := func(int, time.Duration) { stalls++ }

			// Act
			_, err := awaitKilledLeader(4242, time.Millisecond, wait, stalled)

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

// TestKillStopped pins the signals that end a process the harness may hold
// stopped: its SIGKILL, then a SIGCONT to it alone, since on Darwin an
// accepted SIGKILL never resumes a stopped process's suspended task.
func TestKillStopped(t *testing.T) {
	errRefused := syscall.EPERM
	tests := []struct {
		name      string
		failKill  error
		failCont  error
		wantSent  []string
		wantErrIs error
		wantErr   bool
	}{
		{name: "the SIGKILL is followed by a SIGCONT to the same pid", wantSent: []string{"4242 killed", "4242 continued"}},
		{name: "a refused SIGKILL is answered and nothing is continued", failKill: errRefused, wantSent: []string{"4242 killed"}, wantErrIs: errRefused, wantErr: true},
		{name: "a process gone before its SIGCONT is no error", failCont: syscall.ESRCH, wantSent: []string{"4242 killed", "4242 continued"}},
		{name: "a refused SIGCONT is answered", failCont: errRefused, wantSent: []string{"4242 killed", "4242 continued"}, wantErrIs: errRefused, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var sent []string
			kill := func(pid int, sig syscall.Signal) error {
				sent = append(sent, strconv.Itoa(pid)+" "+sig.String())
				if sig == syscall.SIGKILL {
					return tt.failKill
				}
				return tt.failCont
			}

			// Act
			err := killStopped(4242, kill)

			// Assert
			if strings.Join(sent, ",") != strings.Join(tt.wantSent, ",") {
				t.Errorf("sent %q, want %q", sent, tt.wantSent)
			}
			if (err != nil) != tt.wantErr || (tt.wantErrIs != nil && !errors.Is(err, tt.wantErrIs)) {
				t.Errorf("err = %v, want error %v (is %v)", err, tt.wantErr, tt.wantErrIs)
			}
		})
	}
}

// TestKillStoppedEndsAStoppedProcess is the same against a real stopped
// process: it exits on the kill, with nothing else sent to it.
func TestKillStoppedEndsAStoppedProcess(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sleep", "100")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
	if err := syscall.Kill(cmd.Process.Pid, syscall.SIGSTOP); err != nil {
		t.Fatalf("SIGSTOP: %v", err)
	}
	if err := awaitFrozen(cmd.Process.Pid, freezeBound); err != nil {
		t.Fatalf("the process did not stop: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Act
	err := killStopped(cmd.Process.Pid, syscall.Kill)

	// Assert
	if err != nil {
		t.Fatalf("killStopped: %v", err)
	}
	if err := WaitProcessExit(ctx, cmd.Process.Pid); err != nil {
		t.Fatalf("the stopped process did not exit: %v", err)
	}
}

func TestStandInDiesWithItsWholeGroup(t *testing.T) {
	// Arrange: a stand-in whose child is running when the test ends.
	var child int
	t.Run("stand-in", func(t *testing.T) {
		r, w, err := os.Pipe()
		if err != nil {
			t.Fatal(err)
		}
		defer r.Close()
		standInTo(t, w, "/bin/sleep 100 & echo $!; wait")
		w.Close()
		line, err := bufio.NewReader(r).ReadString('\n')
		if err != nil {
			t.Fatalf("read the child's pid: %v", err)
		}
		if child, err = strconv.Atoi(strings.TrimSpace(line)); err != nil {
			t.Fatal(err)
		}
	})

	// Act: the subtest's cleanup has run.
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Assert.
	if err := WaitProcessExit(ctx, child); err != nil {
		t.Fatalf("the stand-in's child %d outlived its test: %v", child, err)
	}
}

func TestEveryLoopingStandInIsStartedByStandIn(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("kill_test.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act: shells started by hand, outside standInTo and groupUnderKill.
	byHand := strings.Count(string(src), "exec.Command("+`"/bin/sh"`)

	// Assert: standInTo, groupUnderKill and the reaped-flag table, which
	// leaves its group to the Kill under test.
	if byHand != 3 {
		t.Fatalf("kill_test.go starts %d shells by hand, want 3: start a looping stand-in with standIn", byHand)
	}
}

func TestKillGroupKillsALiveGroupAndTakesAGoneOneAsNoFailure(t *testing.T) {
	tests := []struct {
		name string
		reap bool // the group's leader has exited and been reaped before the kill
	}{
		{name: "a running group is killed"},
		{name: "a group already gone is no failure", reap: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a process leading its own group.
			cmd := exec.Command("/bin/sleep", "60")
			cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			if tt.reap {
				if err := cmd.Process.Kill(); err != nil {
					t.Fatalf("kill: %v", err)
				}
				_ = cmd.Wait()
			}

			// Act.
			err := KillGroup(cmd.Process.Pid)

			// Assert.
			if err != nil {
				t.Fatalf("KillGroup = %v, want nil", err)
			}
			if !tt.reap {
				_ = cmd.Wait()
				ws, ok := cmd.ProcessState.Sys().(syscall.WaitStatus)
				if !ok || !ws.Signaled() || ws.Signal() != syscall.SIGKILL {
					t.Fatalf("the group's leader ended with %v, want SIGKILL", cmd.ProcessState)
				}
			}
		})
	}
}

// TestKillReturnsOnceEveryMemberHasReleasedItsFiles pins that Kill awaits the
// whole group, not only its leader: a member still exiting when Kill returned
// held the killed daemon's boot claim, and the next daemon refused to boot.
// It reads what the claim needs, every member's files released, through a
// FIFO whose only writer is the member: a read answers end of file exactly
// when no writer is left. A zombie listing is NOT that property: Darwin posts
// the exit event Kill awaits after the files are closed but before the
// process turns zombie, so a member can be listed as exiting after Kill has
// rightly returned.
func TestKillReturnsOnceEveryMemberHasReleasedItsFiles(t *testing.T) {
	// Arrange
	fifo := filepath.Join(t.TempDir(), "member")
	if err := syscall.Mkfifo(fifo, 0o600); err != nil {
		t.Fatalf("mkfifo: %v", err)
	}
	d, _ := groupUnderKill(t, "/bin/sleep 100 >"+fifo+" & echo $!; wait")
	// A blocking open of a FIFO's read end returns once a writer has opened
	// it: the member holds the file before Kill runs, with no clock.
	r, err := syscall.Open(fifo, syscall.O_RDONLY, 0)
	if err != nil {
		t.Fatalf("open the FIFO's read end: %v", err)
	}
	t.Cleanup(func() { syscall.Close(r) })
	if err := syscall.SetNonblock(r, true); err != nil {
		t.Fatalf("set the read end non-blocking: %v", err)
	}

	// Act
	d.Kill()

	// Assert
	n, err := syscall.Read(r, make([]byte, 1))
	if n != 0 || err != nil {
		t.Fatalf("read the FIFO after Kill = %d, %v; want end of file: a member still held its write end", n, err)
	}
}

// TestAwaitGroupEnd pins the group wait: every listed member is awaited once,
// a member forked while the kill was under way is awaited on the next
// listing, and a failure is answered naming the member.
func TestAwaitGroupEnd(t *testing.T) {
	errList := errors.New("the group could not be listed")
	errWait := errors.New("the exit event could not be registered")
	m := func(pid int) procgroup.Member { return procgroup.Member{Pid: pid, Comm: "c", State: "running"} }
	tests := []struct {
		name     string
		held     []int
		listings [][]procgroup.Member // answered in turn; the last repeats
		listErr  error
		waitErr  error
		// unseenWaits is how many of a pid's waits answer an exit not seen.
		unseenWaits map[int]int
		wantWaits   []int
		wantErr     error
	}{
		{name: "an empty group awaits nothing", listings: [][]procgroup.Member{nil}},
		{name: "each member is awaited once though it stays listed until it is a zombie",
			listings: [][]procgroup.Member{{m(1), m(2)}}, wantWaits: []int{1, 2}},
		{name: "a member forked during the kill is awaited on the next listing",
			listings: [][]procgroup.Member{{m(1)}, {m(1), m(3)}}, wantWaits: []int{1, 3}},
		{name: "a member whose exit was not seen is awaited again until the listing drops it",
			listings: [][]procgroup.Member{{m(1), m(2)}, {m(2)}, {m(2)}, {}}, unseenWaits: map[int]int{2: 2},
			wantWaits: []int{1, 2, 2, 2}},
		{name: "a held member is awaited though no listing shows it",
			held: []int{5}, listings: [][]procgroup.Member{{}}, wantWaits: []int{5}},
		{name: "a held member is not awaited again while it stays listed",
			held: []int{1}, listings: [][]procgroup.Member{{m(1)}}, wantWaits: []int{1}},
		{name: "a held member's wait that answers no exit is answered", held: []int{5},
			listings: [][]procgroup.Member{{}}, unseenWaits: map[int]int{5: 1}, wantWaits: []int{5}, wantErr: errHeldExitUnseen},
		{name: "a listing that fails is answered", listErr: errList, wantErr: errList},
		{name: "a wait that fails is answered", listings: [][]procgroup.Member{{m(1)}}, waitErr: errWait,
			wantWaits: []int{1}, wantErr: errWait},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			calls := 0
			list := func(int) ([]procgroup.Member, error) {
				if tt.listErr != nil {
					return nil, tt.listErr
				}
				l := tt.listings[min(calls, len(tt.listings)-1)]
				calls++
				return l, nil
			}
			var waits []int
			unseen := maps.Clone(tt.unseenWaits)
			wait := func(_ context.Context, pid int) (bool, error) {
				waits = append(waits, pid)
				if unseen[pid] > 0 {
					unseen[pid]--
					return false, nil
				}
				return tt.waitErr == nil, tt.waitErr
			}
			stalled := func(int, time.Duration) { t.Error("a stall was reported") }

			// Act
			err := awaitGroupEnd(7, tt.held, time.Minute, list, wait, stalled)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("err = %v, want %v", err, tt.wantErr)
			}
			if tt.waitErr != nil && !strings.Contains(err.Error(), "member 1") {
				t.Errorf("err = %v, want the member named", err)
			}
			if !slices.Equal(waits, tt.wantWaits) {
				t.Errorf("waits = %v, want %v", waits, tt.wantWaits)
			}
		})
	}
}

// TestHoldGroupExits pins the hold Kill takes while the group is stopped:
// every listed member's event is taken, a member the kernel refuses is left to
// the listing, and a failure is answered naming the member, with the events
// taken so far kept for the caller's close.
func TestHoldGroupExits(t *testing.T) {
	errList := errors.New("the group could not be listed")
	errTake := errors.New("the exit event could not be registered")
	m := func(pid int) procgroup.Member { return procgroup.Member{Pid: pid, Comm: "c", State: "stopped"} }
	tests := []struct {
		name     string
		members  []procgroup.Member
		listErr  error
		refused  map[int]bool
		failing  int
		wantHeld []int
		wantErr  error
	}{
		{name: "every member is held", members: []procgroup.Member{m(3), m(1)}, wantHeld: []int{1, 3}},
		{name: "a member the kernel refuses is not held", members: []procgroup.Member{m(1), m(2)},
			refused: map[int]bool{2: true}, wantHeld: []int{1}},
		{name: "a listing that fails is answered", listErr: errList, wantHeld: []int{}, wantErr: errList},
		{name: "a take that fails is answered with what was held", members: []procgroup.Member{m(1), m(2)},
			failing: 2, wantHeld: []int{1}, wantErr: errTake},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			list := func(int) ([]procgroup.Member, error) { return tt.members, tt.listErr }
			take := func(pid int) (*exitEvent, error) {
				if pid == tt.failing {
					return nil, errTake
				}
				if tt.refused[pid] {
					return nil, nil
				}
				return &exitEvent{}, nil
			}

			// Act
			held, err := holdGroupExits(7, list, take)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("err = %v, want %v", err, tt.wantErr)
			}
			if tt.failing != 0 && !strings.Contains(err.Error(), fmt.Sprintf("member %d", tt.failing)) {
				t.Errorf("err = %v, want the member named", err)
			}
			if got := held.pids(); !slices.Equal(got, tt.wantHeld) {
				t.Errorf("held = %v, want %v", got, tt.wantHeld)
			}
		})
	}
}

// TestHeldExitsWaitSeesAHeldMembersExit pins that a held event sees its
// member's exit, the property awaitGroupEnd counts on for a member the
// listing no longer shows.
func TestHeldExitsWaitSeesAHeldMembersExit(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sleep", "100")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
	pid := cmd.Process.Pid
	held, err := holdGroupExits(0, func(int) ([]procgroup.Member, error) {
		return []procgroup.Member{{Pid: pid}}, nil
	}, takeExitEvent)
	if err != nil {
		t.Fatalf("holdGroupExits: %v", err)
	}
	defer held.close()
	if err := cmd.Process.Kill(); err != nil {
		t.Fatalf("kill: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	// Act
	seen, err := held.wait(ctx, pid)

	// Assert
	if err != nil || !seen {
		t.Fatalf("wait = %v, %v; want the exit seen", seen, err)
	}
}

// TestStallListingShowsTheSelectedProcess pins the stuck-kill reports' one
// listing: the process it selects, with the state columns a report reads.
func TestStallListingShowsTheSelectedProcess(t *testing.T) {
	// Act
	out, err := stallListing("-p", os.Getpid())

	// Assert
	if err != nil {
		t.Fatalf("stallListing: %v: %s", err, out)
	}
	if !strings.Contains(string(out), "STAT") || !strings.Contains(string(out), strconv.Itoa(os.Getpid())) {
		t.Fatalf("the listing shows neither its columns nor this process:\n%s", out)
	}
}

// TestEveryStallReportListsThroughStallListing pins that both stuck-kill
// reports share the one listing, so their columns cannot drift apart.
func TestEveryStallReportListsThroughStallListing(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	direct := strings.Count(string(src), `"pid,ppid,pgid,stat,wchan,flags,time,command"`)
	shared := strings.Count(string(src), "stallListing(")

	// Assert
	if direct != 1 || shared != 3 {
		t.Fatalf("the listing's columns are spelled %d times and stallListing named %d times, want 1 and 3 (its definition and both reports)", direct, shared)
	}
}
