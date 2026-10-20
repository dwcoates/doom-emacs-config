package harness

import (
	"errors"
	"testing"
	"time"

	"golang.org/x/sys/unix"
)

// TestProcArgs pins how a process caught between its fork and the end of its
// exec is read: the kernel's answers in that window are scripted, one per
// read, since the window is too short to arrange with a real process.
func TestProcArgs(t *testing.T) {
	halfWritten := procArgs2("/opt/node/bin/node", "main.js", "--listen")
	halfWritten = halfWritten[:len(halfWritten)-len("--listen\x00HOME=/nowhere\x00")+3]
	complete := procArgs2("/opt/node/bin/node", "main.js", "--listen")
	running := processState{name: "running"}
	cases := []struct {
		name      string
		reads     []func() ([]byte, error)
		states    []processState
		wantArgs  string
		wantLive  bool
		wantError bool
	}{
		{
			name:     "a buffer the exec has not finished writing is read again until it decodes",
			reads:    []func() ([]byte, error){func() ([]byte, error) { return halfWritten, nil }, func() ([]byte, error) { return complete, nil }},
			states:   []processState{running},
			wantArgs: "main.js --listen",
			wantLive: true,
		},
		{
			name:     "an argv the kernel will not answer yet is read again until it does",
			reads:    []func() ([]byte, error){func() ([]byte, error) { return nil, unix.EINVAL }, func() ([]byte, error) { return complete, nil }},
			states:   []processState{running},
			wantArgs: "main.js --listen",
			wantLive: true,
		},
		{
			name:   "a process that exits before its argv is readable is no stray",
			reads:  []func() ([]byte, error){func() ([]byte, error) { return halfWritten, nil }},
			states: []processState{{name: "zombie", exited: true}},
		},
		{
			name:   "a process another harness stopped before its argv was readable is no stray",
			reads:  []func() ([]byte, error){func() ([]byte, error) { return nil, unix.EINVAL }},
			states: []processState{{name: "stopped", frozen: true}},
		},
		{
			name:      "any other read failure is an error at once",
			reads:     []func() ([]byte, error){func() ([]byte, error) { return nil, unix.EPERM }},
			wantError: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			nRead, nState := 0, 0
			read := func(int) ([]byte, error) {
				r := tc.reads[min(nRead, len(tc.reads)-1)]
				nRead++
				return r()
			}
			state := func(int) (processState, error) {
				if len(tc.states) == 0 {
					return processState{}, errors.New("no state was scripted")
				}
				s := tc.states[min(nState, len(tc.states)-1)]
				nState++
				return s, nil
			}

			// Act
			args, live, err := procArgs(1, read, state)

			// Assert
			if (err != nil) != tc.wantError {
				t.Fatalf("procArgs error = %v, want error %v", err, tc.wantError)
			}
			if args != tc.wantArgs || live != tc.wantLive {
				t.Fatalf("procArgs = %q, live %v; want %q, live %v", args, live, tc.wantArgs, tc.wantLive)
			}
		})
	}
}

func TestKinfoIdentityNamesTheProcessWithoutItsArgv(t *testing.T) {
	// Arrange
	var p unix.KinfoProc
	copy(p.Proc.P_comm[:], "node")
	p.Eproc.Ppid = 7
	p.Proc.P_starttime = unix.Timeval{Sec: 1760500000, Usec: 250000}
	p.Proc.P_flag = 0x100

	// Act
	got := kinfoIdentity(p)

	// Assert
	want := `comm="node" ppid=7 started=` + time.Unix(1760500000, 250000000).Format(time.RFC3339Nano) + ` flags=0x100`
	if got != want {
		t.Fatalf("kinfoIdentity = %s, want %s", got, want)
	}
}

func TestMayBeThisRunsStray(t *testing.T) {
	// Arrange: the test binary (100) runs a daemon (110) that runs a shim
	// (120); 130 is an orphan; another run's binary (200) runs a child (210).
	parents := map[int]int{100: 50, 50: 1, 110: 100, 120: 110, 130: 1, 140: 4242, 200: 60, 60: 1, 210: 200}
	tests := []struct {
		name string
		pid  int
		want bool
	}{
		{"the test binary itself", 100, true},
		{"its daemon", 110, true},
		{"the daemon's shim", 120, true},
		{"an orphan reparented to launchd", 130, true},
		{"one whose parent has just exited", 140, true},
		{"another run's live child", 210, false},
		{"another run's binary", 200, false},
		{"a process the table does not hold", 999, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := mayBeThisRunsStray(tt.pid, parents, 100); got != tt.want {
				t.Fatalf("mayBeThisRunsStray(%d) = %v, want %v", tt.pid, got, tt.want)
			}
		})
	}
}
