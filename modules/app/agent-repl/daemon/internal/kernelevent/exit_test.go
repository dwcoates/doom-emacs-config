package kernelevent

import (
	"context"
	"errors"
	"os/exec"
	"syscall"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

// waitBound is one wait's failure bound in these tests.
const waitBound = 5 * time.Second

func TestWaitExit(t *testing.T) {
	cases := []struct {
		name string
		// kill ends the process once the wait is armed; false leaves it
		// running.
		kill   bool
		reaped bool
		// canceled ends the wait by canceling a context with no deadline.
		canceled bool
		wantErr  error
	}{
		{name: "a process that exits is reported", kill: true},
		{name: "a process already gone is reported", reaped: true},
		{name: "a wait whose context is canceled is refused", canceled: true, wantErr: context.Canceled},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			cmd := exec.Command("cat")
			stdin, err := cmd.StdinPipe()
			if err != nil {
				t.Fatalf("stdin: %v", err)
			}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
			if tc.reaped {
				stdin.Close()
				_ = cmd.Wait()
			}
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			if tc.canceled {
				ctx, cancel = context.WithCancel(context.Background())
			}
			defer cancel()
			ev, err := TakeExit(cmd.Process.Pid)
			if err != nil {
				t.Fatalf("TakeExit: %v", err)
			}

			// Act
			if tc.canceled {
				cancel()
			}
			if tc.kill {
				if err := cmd.Process.Kill(); err != nil {
					t.Fatalf("kill: %v", err)
				}
			}
			if ev != nil {
				err = ev.Await(ctx)
				ev.Close()
			}

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("Await = %v, want %v", err, tc.wantErr)
			}
		})
	}
}

func TestAwaitExitAnswersWhetherTheExitWasSeen(t *testing.T) {
	// Arrange: a process reaped before the wait, whose event cannot be taken.
	cmd := exec.Command("/bin/sh", "-c", "exit 0")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	// Act
	seen, err := AwaitExit(context.Background(), cmd.Process.Pid)

	// Assert
	if err != nil || seen != UnregisteredExitSeen {
		t.Fatalf("AwaitExit = (%v, %v), want (%v, nil)", seen, err, UnregisteredExitSeen)
	}
}

func TestAwaitEndReadsHowAChildEndedWithoutReapingIt(t *testing.T) {
	cases := []struct {
		name string
		args []string
		kill bool
		want ProcessEnd
	}{
		{name: "an exit code", args: []string{"-c", "exit 3"}, want: ProcessEnd{Code: 3}},
		{name: "a clean exit", args: []string{"-c", "exit 0"}, want: ProcessEnd{}},
		{name: "a kill", args: []string{"-c", "exec cat"}, kill: true, want: ProcessEnd{Signal: syscall.SIGKILL}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the child holds its stdin open until the end is
			// registered, so it cannot exit before the watch is taken.
			cmd := exec.Command("/bin/sh", append(tc.args[:1:1], "read _; "+tc.args[1])...)
			stdin, err := cmd.StdinPipe()
			if err != nil {
				t.Fatalf("stdin: %v", err)
			}
			if err := cmd.Start(); err != nil {
				t.Fatalf("start: %v", err)
			}
			t.Cleanup(func() { _ = cmd.Process.Kill(); _ = cmd.Wait() })
			ctx, cancel := context.WithTimeout(context.Background(), waitBound)
			defer cancel()
			ev, err := TakeEnd(cmd.Process.Pid)
			if err != nil || ev == nil {
				t.Fatalf("TakeEnd = (%v, %v), want an event", ev, err)
			}
			defer ev.Close()

			// Act
			if tc.kill {
				if err := cmd.Process.Kill(); err != nil {
					t.Fatalf("kill: %v", err)
				}
			} else {
				_ = stdin.Close()
			}
			if err := ev.Await(ctx); err != nil {
				t.Fatalf("Await: %v", err)
			}
			end, err := ev.EndOf(cmd.Process.Pid)

			// Assert
			if err != nil || end != tc.want {
				t.Fatalf("end = (%v, %v), want %v", end, err, tc.want)
			}
			if !unreaped(cmd.Process.Pid) {
				t.Fatalf("pid %d was reaped by reading its end", cmd.Process.Pid)
			}
		})
	}
}

// unreaped reports whether a child is still waitable.
func unreaped(pid int) bool {
	var ws unix.WaitStatus
	got, err := unix.Wait4(pid, &ws, unix.WNOHANG|unix.WNOWAIT, nil)
	if err == nil && got == pid {
		return true
	}
	// WNOWAIT is not honored by wait4 everywhere: a live kill(0) on an
	// exited child means it is still a zombie, so unreaped.
	return unix.Kill(pid, 0) == nil
}

func TestAwaitEndRefusesAProcessAlreadyReaped(t *testing.T) {
	// Arrange
	cmd := exec.Command("/bin/sh", "-c", "exit 0")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v", err)
	}

	// Act
	_, err := AwaitEnd(context.Background(), cmd.Process.Pid)

	// Assert
	if !errors.Is(err, ErrEndUnwatchable) {
		t.Fatalf("AwaitEnd of a reaped pid = %v, want ErrEndUnwatchable", err)
	}
}

func TestAwakenOnEnd(t *testing.T) {
	tests := []struct {
		name         string
		releaseFirst bool
		wantWakes    int
	}{
		{name: "an ended context wakes the wait once", wantWakes: 1},
		{name: "a released wait is never woken", releaseFirst: true, wantWakes: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			woken := make(chan struct{}, 2)
			release := AwakenOnEnd(ctx, func() { woken <- struct{}{} })

			// Act
			wakes := 0
			if tt.releaseFirst {
				release()
				cancel()
			} else {
				cancel()
				<-woken
				wakes++
				release()
			}

			// Assert: once released, no wake is still to come.
			wakes += len(woken)
			if wakes != tt.wantWakes {
				t.Fatalf("AwakenOnEnd woke %d times, want %d", wakes, tt.wantWakes)
			}
		})
	}
}

func TestAwakenOnEndReleaseAwaitsARunningWake(t *testing.T) {
	// Arrange: a wake that is running, held until the test lets it finish.
	ctx, cancel := context.WithCancel(context.Background())
	entered, finish := make(chan struct{}), make(chan struct{})
	var finished bool
	release := AwakenOnEnd(ctx, func() {
		close(entered)
		<-finish
		finished = true
	})
	cancel()
	<-entered

	// Act
	released := make(chan struct{})
	go func() { release(); close(released) }()
	close(finish)
	<-released

	// Assert
	if !finished {
		t.Fatalf("release returned while the wake was still running")
	}
}
