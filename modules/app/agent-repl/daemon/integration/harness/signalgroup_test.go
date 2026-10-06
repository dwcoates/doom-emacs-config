package harness

import (
	"context"
	"errors"
	"os/exec"
	"strings"
	"syscall"
	"testing"
)

// fakeKill records every kill it is asked for and answers from a table keyed
// by the target (a negative target is a group).
type fakeKill struct {
	answers map[int]error
	asked   []int
}

func (f *fakeKill) kill(target int, _ syscall.Signal) error {
	f.asked = append(f.asked, target)
	return f.answers[target]
}

func TestSignalGroupMembers(t *testing.T) {
	const pgid = 500
	members := []groupMember{{pid: 500, comm: "daemon", state: "sleeping"}, {pid: 501, comm: "tool", state: "stopped"}}
	cases := []struct {
		name      string
		answers   map[int]error
		listErr   error
		wantAsked []int
		// wantErr is a substring the error must hold; "" wants nil.
		wantErr string
	}{
		{name: "a group that takes the signal is done", answers: map[int]error{}, wantAsked: []int{-pgid}},
		{name: "a group already gone is done", answers: map[int]error{-pgid: syscall.ESRCH}, wantAsked: []int{-pgid}},
		{name: "an EPERM group has each live member signalled by pid", answers: map[int]error{-pgid: syscall.EPERM},
			wantAsked: []int{-pgid, 500, 501}},
		{name: "an EPERM group whose member has since gone is done", answers: map[int]error{-pgid: syscall.EPERM, 501: syscall.ESRCH},
			wantAsked: []int{-pgid, 500, 501}},
		{name: "an EPERM group whose member refuses its own signal is reported naming it",
			answers: map[int]error{-pgid: syscall.EPERM, 501: syscall.EPERM}, wantAsked: []int{-pgid, 500, 501},
			wantErr: "member 501 (tool, stopped)"},
		{name: "an EPERM group whose members cannot be listed is reported", answers: map[int]error{-pgid: syscall.EPERM},
			listErr: errors.New("sysctl refused"), wantAsked: []int{-pgid}, wantErr: "could not be read: sysctl refused"},
		{name: "any other group error is reported", answers: map[int]error{-pgid: syscall.EINVAL}, wantAsked: []int{-pgid},
			wantErr: "invalid argument"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &fakeKill{answers: tc.answers}
			live := func(int) ([]groupMember, error) { return members, tc.listErr }

			// Act
			err := signalGroupMembers(pgid, syscall.SIGKILL, f.kill, live)

			// Assert
			if tc.wantErr == "" && err != nil {
				t.Fatalf("signalGroupMembers = %v, want nil", err)
			}
			if tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("signalGroupMembers = %v, want an error holding %q", err, tc.wantErr)
			}
			if len(f.asked) != len(tc.wantAsked) {
				t.Fatalf("kills asked = %v, want %v", f.asked, tc.wantAsked)
			}
			for i := range f.asked {
				if f.asked[i] != tc.wantAsked[i] {
					t.Fatalf("kills asked = %v, want %v", f.asked, tc.wantAsked)
				}
			}
		})
	}
}

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
	err := signalGroupMembers(cmd.Process.Pid, syscall.SIGKILL, syscall.Kill, liveGroupMembers)

	// Assert
	if err != nil {
		t.Fatalf("signalGroupMembers on a group of one unreaped zombie = %v, want nil", err)
	}
}
