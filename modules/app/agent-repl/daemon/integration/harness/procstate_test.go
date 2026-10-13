package harness

import (
	"context"
	"errors"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"
)

// startIdle starts a process that runs until killed, and reaps it at cleanup.
func startIdle(t *testing.T) *exec.Cmd {
	t.Helper()
	cmd := exec.Command("/bin/sleep", "100")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
	return cmd
}

func TestReadProcessState(t *testing.T) {
	cases := []struct {
		name string
		// prepare puts the process into the state under test.
		prepare    func(t *testing.T, cmd *exec.Cmd)
		wantFrozen bool
		wantExited bool
	}{
		{
			name:       "a running process is not frozen",
			prepare:    func(*testing.T, *exec.Cmd) {},
			wantFrozen: false,
			wantExited: false,
		},
		{
			name: "a stopped process is frozen",
			prepare: func(t *testing.T, cmd *exec.Cmd) {
				if err := cmd.Process.Signal(syscall.SIGSTOP); err != nil {
					t.Fatalf("SIGSTOP: %v", err)
				}
				// The kernel's report, not a guess: WUNTRACED returns once the
				// child is stopped.
				var ws syscall.WaitStatus
				if _, err := syscall.Wait4(cmd.Process.Pid, &ws, syscall.WUNTRACED, nil); err != nil {
					t.Fatalf("wait for the stop: %v", err)
				}
			},
			wantFrozen: true,
			wantExited: false,
		},
		{
			name: "an exited, unreaped process is frozen and exited",
			prepare: func(t *testing.T, cmd *exec.Cmd) {
				if err := cmd.Process.Signal(syscall.SIGKILL); err != nil {
					t.Fatalf("SIGKILL: %v", err)
				}
				// The kernel posts the exit event before the state reads as
				// a zombie, so the zombie state itself is awaited.
				if err := awaitFrozen(cmd.Process.Pid, DefaultTimeout); err != nil {
					t.Fatalf("await the exit: %v", err)
				}
			},
			wantFrozen: true,
			wantExited: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := startIdle(t)
			tc.prepare(t, cmd)

			// Act
			state, err := readProcessState(cmd.Process.Pid)

			// Assert
			if err != nil {
				t.Fatalf("readProcessState = error %v", err)
			}
			if state.frozen != tc.wantFrozen {
				t.Fatalf("readProcessState frozen = %v (%s), want %v", state.frozen, state.name, tc.wantFrozen)
			}
			if state.exited != tc.wantExited {
				t.Fatalf("readProcessState exited = %v (%s), want %v", state.exited, state.name, tc.wantExited)
			}
		})
	}
}

// TestAwaitFrozenAcceptsAnExitedUnreapedProcess covers a daemon that died on
// its own before the freeze: a zombie runs nothing, so the kill may proceed.
// It waits on awaitFrozen itself, because the kernel posts the exit event
// before the process's state reads as a zombie.
func TestAwaitFrozenAcceptsAnExitedUnreapedProcess(t *testing.T) {
	// Arrange: the process exits, and nothing reaps it until cleanup.
	cmd := startIdle(t)
	if err := cmd.Process.Signal(syscall.SIGTERM); err != nil {
		t.Fatalf("SIGTERM: %v", err)
	}

	// Act
	err := awaitFrozen(cmd.Process.Pid, DefaultTimeout)

	// Assert
	if err != nil {
		t.Fatalf("awaitFrozen = %v for an exited, unreaped process, want it accepted", err)
	}
}

func TestAwaitFrozenRefusesAProcessThatNeverStops(t *testing.T) {
	// Arrange: a process nobody stops.
	cmd := startIdle(t)

	// Act
	err := awaitFrozen(cmd.Process.Pid, 20*time.Millisecond)

	// Assert: the refusal names the process and the state it was still in.
	if err == nil {
		t.Fatal("awaitFrozen = nil for a process that was never stopped, want a refusal")
	}
	if !strings.Contains(err.Error(), strconv.Itoa(cmd.Process.Pid)) || !strings.Contains(err.Error(), "still") {
		t.Fatalf("awaitFrozen = %q, want it to name pid %d and the state it was still in", err, cmd.Process.Pid)
	}
}

func TestLiveGroupMembers(t *testing.T) {
	cases := []struct {
		name string
		// exit, when true, SIGKILLs the group's only process and awaits its
		// exit, leaving it unreaped.
		exit bool
		// wantLeader is whether the leader is listed as a live member.
		wantLeader bool
	}{
		{name: "a group with a running process lists it", exit: false, wantLeader: true},
		{name: "a group whose only process is exited and unreaped lists nobody", exit: true, wantLeader: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a process leading a group of its own.
			cmd := exec.Command("/bin/sleep", "100")
			cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
			if tc.exit {
				if err := cmd.Process.Signal(syscall.SIGKILL); err != nil {
					t.Fatalf("SIGKILL: %v", err)
				}
				ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
				defer cancel()
				if err := WaitProcessExit(ctx, cmd.Process.Pid); err != nil {
					t.Fatalf("await the exit: %v", err)
				}
			}

			// Act
			got, err := liveGroupMembers(cmd.Process.Pid)

			// Assert
			if err != nil {
				t.Fatalf("liveGroupMembers(%d) = %v", cmd.Process.Pid, err)
			}
			listed := len(got) == 1 && got[0].pid == cmd.Process.Pid && got[0].comm == "sleep"
			if listed != tc.wantLeader || (!tc.wantLeader && len(got) != 0) {
				t.Fatalf("liveGroupMembers(%d) = %+v, want the leader listed = %v and nothing else", cmd.Process.Pid, got, tc.wantLeader)
			}
		})
	}
}

func TestStopFrozenStopsARunningProcess(t *testing.T) {
	// Arrange
	cmd := startIdle(t)

	// Act
	err := stopFrozen(cmd.Process.Pid, DefaultTimeout, syscall.Kill)

	// Assert
	if err != nil {
		t.Fatalf("stopFrozen = %v, want the process stopped", err)
	}
	state, err := readProcessState(cmd.Process.Pid)
	if err != nil || !state.frozen {
		t.Fatalf("after stopFrozen the process is %+v (%v), want frozen", state, err)
	}
}

// TestStopFrozenResendsAStopTheProcessDiscarded stands in for a stop that
// reached a process inside execve, which Darwin discards: the first send is
// undone at once, and stopFrozen must still leave the process frozen.
func TestStopFrozenResendsAStopTheProcessDiscarded(t *testing.T) {
	// Arrange: the first stop is "discarded" by an immediate continue.
	cmd := startIdle(t)
	sends := 0
	discardFirst := func(pid int, sig syscall.Signal) error {
		sends++
		if err := syscall.Kill(pid, sig); err != nil {
			return err
		}
		if sends == 1 {
			return syscall.Kill(pid, syscall.SIGCONT)
		}
		return nil
	}

	// Act
	err := stopFrozen(cmd.Process.Pid, DefaultTimeout, discardFirst)

	// Assert
	if err != nil {
		t.Fatalf("stopFrozen = %v after a discarded first stop, want the process stopped", err)
	}
	if sends < 2 {
		t.Fatalf("stopFrozen sent %d stop(s), want the discarded one re-sent", sends)
	}
}

func TestStopFrozenAnswersTheSendErrorForAGoneProcess(t *testing.T) {
	// Arrange: a process that has exited and been reaped.
	cmd := exec.Command("/usr/bin/true")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run /usr/bin/true: %v", err)
	}

	// Act
	err := stopFrozen(cmd.Process.Pid, DefaultTimeout, syscall.Kill)

	// Assert
	if !errors.Is(err, syscall.ESRCH) {
		t.Fatalf("stopFrozen = %v for a reaped process, want ESRCH", err)
	}
}

func TestIsZombie(t *testing.T) {
	cases := []struct {
		name    string
		prepare func(t *testing.T, cmd *exec.Cmd)
		want    bool
	}{
		{name: "a running process has not exited", prepare: func(*testing.T, *exec.Cmd) {}, want: false},
		{
			name: "a killed, unreaped process has exited",
			prepare: func(t *testing.T, cmd *exec.Cmd) {
				if err := cmd.Process.Signal(syscall.SIGKILL); err != nil {
					t.Fatalf("SIGKILL: %v", err)
				}
				if err := awaitFrozen(cmd.Process.Pid, DefaultTimeout); err != nil {
					t.Fatalf("await the exit: %v", err)
				}
			},
			want: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := startIdle(t)
			tc.prepare(t, cmd)

			// Act
			got := isZombie(cmd.Process.Pid)

			// Assert
			if got != tc.want {
				t.Fatalf("isZombie = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestHeldStateOfAStoppedProcessIsFrozenOnceTheStopHolds pins the wait's
// read: stopped, with no thread running, once the stop has held.
func TestHeldStateOfAStoppedProcessIsFrozenOnceTheStopHolds(t *testing.T) {
	// Arrange
	cmd := startIdle(t)
	if err := stopFrozen(cmd.Process.Pid, freezeBound, syscall.Kill); err != nil {
		t.Fatalf("stopFrozen: %v", err)
	}

	// Act
	state, err := heldState(cmd.Process.Pid)

	// Assert
	if err != nil || !state.frozen || state.exited {
		t.Fatalf("heldState = %+v, %v; want frozen, not exited", state, err)
	}
}

// TestHeldStateOfARunningProcessIsNotFrozen pins that a process never stopped
// is not read held.
func TestHeldStateOfARunningProcessIsNotFrozen(t *testing.T) {
	// Arrange
	cmd := startIdle(t)

	// Act
	state, err := heldState(cmd.Process.Pid)

	// Assert
	if err != nil || state.frozen {
		t.Fatalf("heldState = %+v, %v; want not frozen", state, err)
	}
}
