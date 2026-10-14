package harness

import (
	"agent-repld/internal/procgroup"
	"context"
	"os/exec"
	"syscall"
	"testing"
)

func TestSignalGroupMembersAcceptsAGroupOfOnlyAnUnreapedLeader(t *testing.T) {
	// Arrange: a real group whose only process exited and is not reaped,
	// which is the group the kernel answers killpg's EPERM for on darwin.
	cmd := exec.Command("/bin/sleep", "100")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	t.Cleanup(func() { _ = cmd.Wait() })
	if err := cmd.Process.Signal(syscall.SIGKILL); err != nil {
		t.Fatalf("SIGKILL: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()
	if err := WaitProcessExit(ctx, cmd.Process.Pid); err != nil {
		t.Fatalf("await the exit: %v", err)
	}

	// Act
	err := procgroup.Signal(cmd.Process.Pid, syscall.SIGKILL)

	// Assert
	if err != nil {
		t.Fatalf("procgroup.Signal on a group of one unreaped zombie = %v, want nil", err)
	}
}

func TestProcgroupGone(t *testing.T) {
	cases := []struct {
		name string
		exit bool
		want bool
	}{
		{name: "a group with a running process is not gone", exit: false, want: false},
		{name: "a group of one exited, unreaped leader is gone", exit: true, want: true},
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
			got, err := procgroup.Gone(cmd.Process.Pid)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("procgroup.Gone(%d) = %v, %v; want %v", cmd.Process.Pid, got, err, tc.want)
			}
		})
	}
}

func TestProcgroupGoneOfNoSuchGroup(t *testing.T) {
	// Arrange: a group whose leader has been reaped.
	cmd := exec.Command("/usr/bin/true")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Run(); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := procgroup.Gone(cmd.Process.Pid)

	// Assert
	if err != nil || !got {
		t.Fatalf("procgroup.Gone(%d) = %v, %v; want gone", cmd.Process.Pid, got, err)
	}
}
