package procgroup

import (
	"errors"
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
