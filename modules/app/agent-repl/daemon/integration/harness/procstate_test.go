package harness

import (
	"context"
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

func TestGroupExited(t *testing.T) {
	cases := []struct {
		name string
		// exit, when true, SIGKILLs the group's only process and awaits its
		// exit, leaving it unreaped.
		exit bool
		want bool
	}{
		{name: "a group with a running process has not exited", exit: false, want: false},
		{name: "a group whose only process is exited and unreaped has exited", exit: true, want: true},
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
			got, err := groupExited(cmd.Process.Pid)

			// Assert
			if err != nil {
				t.Fatalf("groupExited(%d) = %v", cmd.Process.Pid, err)
			}
			if got != tc.want {
				t.Fatalf("groupExited(%d) = %v, want %v", cmd.Process.Pid, got, tc.want)
			}
		})
	}
}
