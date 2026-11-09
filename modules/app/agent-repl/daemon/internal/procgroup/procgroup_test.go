package procgroup

import (
	"context"
	"errors"
	"os/exec"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"
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

func TestSignalMembers(t *testing.T) {
	const pgid = 500
	members := []Member{{Pid: 500, Comm: "daemon", State: "sleeping"}, {Pid: 501, Comm: "tool", State: "stopped"}}
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
			live := func(int) ([]Member, error) { return members, tc.listErr }

			// Act
			err := SignalMembers(pgid, syscall.SIGKILL, f.kill, live)

			// Assert
			if tc.wantErr == "" && err != nil {
				t.Fatalf("SignalMembers = %v, want nil", err)
			}
			if tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("SignalMembers = %v, want an error holding %q", err, tc.wantErr)
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

func TestAwaitGone(t *testing.T) {
	errList := errors.New("sysctl refused")
	errExit := errors.New("exit refused")
	cases := []struct {
		name string
		// rounds is what each round's Gone answers.
		rounds  []bool
		members []Member
		listErr error
		exitErr error
		wantErr error
		// wantWaited is every pid waited on, in order.
		wantWaited []int
	}{
		{name: "a group already gone waits on nothing", rounds: []bool{true}},
		{name: "a group's live members are each waited on, then it is gone",
			rounds: []bool{false, true}, members: []Member{{Pid: 3}, {Pid: 4}}, wantWaited: []int{3, 4}},
		{name: "a member forked meanwhile is waited on in the next round",
			rounds: []bool{false, false, true}, members: []Member{{Pid: 3}}, wantWaited: []int{3, 3}},
		{name: "a listing that fails is answered", rounds: []bool{false}, listErr: errList, wantErr: errList},
		{name: "an exit wait that fails is answered", rounds: []bool{false}, members: []Member{{Pid: 3}},
			exitErr: errExit, wantErr: errExit, wantWaited: []int{3}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			round := 0
			gone := func(int) (bool, error) {
				g := tc.rounds[min(round, len(tc.rounds)-1)]
				round++
				return g, nil
			}
			live := func(int) ([]Member, error) { return tc.members, tc.listErr }
			var waited []int
			exit := func(_ context.Context, pid int) error {
				waited = append(waited, pid)
				return tc.exitErr
			}

			// Act
			err := awaitGone(context.Background(), 7, gone, live, exit)

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("awaitGone = %v, want %v", err, tc.wantErr)
			}
			if !slices.Equal(waited, tc.wantWaited) {
				t.Fatalf("waited on %v, want %v", waited, tc.wantWaited)
			}
		})
	}
}

func TestAwaitGoneSeesARealGroupExit(t *testing.T) {
	// Arrange: a process leading its own group, which the test then kills.
	cmd := exec.Command("/bin/sleep", "60")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	reaped := make(chan struct{})
	go func() { _ = cmd.Wait(); close(reaped) }()
	t.Cleanup(func() { _ = cmd.Process.Kill(); <-reaped })
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	done := make(chan error, 1)
	go func() { done <- AwaitGone(ctx, cmd.Process.Pid) }()

	// Act
	if err := syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL); err != nil {
		t.Fatalf("kill the group: %v", err)
	}
	err := <-done

	// Assert
	if err != nil {
		t.Fatalf("AwaitGone = %v, want the group gone", err)
	}
}
