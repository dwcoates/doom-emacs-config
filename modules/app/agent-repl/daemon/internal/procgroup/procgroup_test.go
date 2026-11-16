package procgroup

import (
	"context"
	"errors"
	"os/exec"
	"slices"
	"syscall"
	"testing"
	"time"
)

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
