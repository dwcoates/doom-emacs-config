package proctable

import (
	"errors"
	"reflect"
	"strings"
	"syscall"
	"testing"
	"time"
)

// procArgsBuf is a kern.procargs2 buffer: argc, the executable path and its
// padding, then argv and env, each NUL-terminated.
func procArgsBuf(argc int, path string, strs ...string) []byte {
	b := []byte{byte(argc), byte(argc >> 8), 0, 0}
	b = append(b, path...)
	b = append(b, 0, 0, 0, 0)
	for _, s := range strs {
		b = append(append(b, s...), 0)
	}
	return append(b, 0, 0)
}

func TestParseProcArgs(t *testing.T) {
	cases := []struct {
		name     string
		buf      []byte
		wantArgv []string
		wantEnv  []string
		wantErr  string
	}{
		{name: "argv then env", buf: procArgsBuf(2, "/bin/sh", "sh", "-c", "A=1", "B=2"), wantArgv: []string{"sh", "-c"}, wantEnv: []string{"A=1", "B=2"}},
		{name: "no environment", buf: procArgsBuf(1, "/bin/true", "true"), wantArgv: []string{"true"}},
		{name: "an empty argument is kept", buf: procArgsBuf(2, "/bin/echo", "echo", "", "A=1"), wantArgv: []string{"echo", ""}, wantEnv: []string{"A=1"}},
		{name: "too short for argc", buf: []byte{1, 0}, wantErr: "too short for argc"},
		{name: "no executable path", buf: []byte{1, 0, 0, 0, 'x'}, wantErr: "no executable path"},
		{name: "a rewritten title leaves fewer strings than argc: what is there, no env", buf: []byte{3, 0, 0, 0, '/', 0, 'n', 'p', 'm', ' ', 'e', 'x', 'e', 'c', 0}, wantArgv: []string{"npm exec"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			argv, env, err := ParseProcArgs(tc.buf)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ParseProcArgs = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || !reflect.DeepEqual(argv, tc.wantArgv) || len(env) != len(tc.wantEnv) || (len(env) > 0 && !reflect.DeepEqual(env, tc.wantEnv)) {
				t.Fatalf("ParseProcArgs = %q, %q, %v; want %q, %q", argv, env, err, tc.wantArgv, tc.wantEnv)
			}
		})
	}
}

func TestEntryLine(t *testing.T) {
	cases := []struct {
		name string
		e    Entry
		want string
	}{
		{name: "argv then env, as ps -E prints it", e: Entry{Argv: []string{"go", "test"}, Env: []string{"A=1"}}, want: "go test A=1"},
		{name: "unreadable arguments are the short name in parentheses", e: Entry{Comm: "launchd"}, want: "(launchd)"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.e.Line()

			// Assert
			if got != tc.want {
				t.Fatalf("Line() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestStateLetter(t *testing.T) {
	for stat, want := range map[int8]byte{1: 'I', 2: 'R', 3: 'S', 4: 'T', 5: 'Z', 9: '?'} {
		if got := stateLetter(stat); got != want {
			t.Errorf("stateLetter(%d) = %c, want %c", stat, got, want)
		}
	}
}

// fakeKernel answers from maps; a pid missing from records is gone.
type fakeKernel struct {
	pids    []int
	listErr error
	records map[int]procRecord
	recErr  map[int]error
	shorts  map[int]procRecord
	shortEr map[int]error
	args    map[int][]byte
	argsErr map[int]error
}

func (f fakeKernel) kernel() kernel {
	return kernel{
		listPids: func() ([]int, error) { return f.pids, f.listErr },
		record: func(pid int) (procRecord, error) {
			if err := f.recErr[pid]; err != nil {
				return procRecord{}, err
			}
			r, ok := f.records[pid]
			if !ok {
				return procRecord{}, syscall.ESRCH
			}
			return r, nil
		},
		shortRecord: func(pid int) (procRecord, error) {
			if err := f.shortEr[pid]; err != nil {
				return procRecord{}, err
			}
			return f.shorts[pid], nil
		},
		procArgs: func(pid int) ([]byte, error) {
			if b, ok := f.args[pid]; ok {
				return b, nil
			}
			if err := f.argsErr[pid]; err != nil {
				return nil, err
			}
			return nil, syscall.EINVAL
		},
	}
}

func TestListReadsEveryProcess(t *testing.T) {
	// Arrange
	start := time.Date(2026, 10, 23, 16, 32, 19, 5000, time.UTC)
	k := fakeKernel{
		pids: []int{0, 10, 11, 12, 13, 14, 15, 16},
		records: map[int]procRecord{
			10: {Ppid: 1, Pgid: 10, Status: 3, Start: start, Comm: "sh"},
			11: {Ppid: 10, Pgid: 10, Status: 5, Start: start, Comm: "zomb"},
			12: {Ppid: 10, Pgid: 10, Status: 2, Start: start, Comm: "exiting"},
			16: {Ppid: 10, Pgid: 10, Status: 2, Start: start, Comm: "mid-exec"},
		},
		recErr:  map[int]error{14: syscall.EPERM, 15: syscall.EPERM},
		shorts:  map[int]procRecord{14: {Ppid: 1, Pgid: 14, Status: 3, Comm: "rootd"}},
		shortEr: map[int]error{15: syscall.EPERM},
		args:    map[int][]byte{10: procArgsBuf(1, "/bin/sh", "sh", "A=1"), 14: procArgsBuf(1, "/usr/libexec/rootd", "/usr/libexec/rootd")},
		argsErr: map[int]error{12: syscall.EINVAL, 16: syscall.EIO},
	}

	// Act
	got, err := k.kernel().list()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []Entry{
		{Pid: 10, Ppid: 1, Pgid: 10, State: 'S', Start: start, Comm: "sh", Argv: []string{"sh"}, Env: []string{"A=1"}},
		{Pid: 11, Ppid: 10, Pgid: 10, State: 'Z', Start: start, Comm: "zomb"},
		{Pid: 12, Ppid: 10, Pgid: 10, State: 'R', Start: start, Comm: "exiting"},
		{Pid: 14, Ppid: 1, Pgid: 14, State: 'S', Comm: "rootd", Argv: []string{"/usr/libexec/rootd"}, Env: []string{}},
		{Pid: 15, State: '?'},
		{Pid: 16, Ppid: 10, Pgid: 10, State: 'R', Start: start, Comm: "mid-exec"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("list() =\n%+v\nwant\n%+v\n(pid 0 skipped, 13 gone)", got, want)
	}
}

func TestListFailures(t *testing.T) {
	boom := errors.New("boom")
	cases := []struct {
		name string
		k    fakeKernel
		want string
	}{
		{name: "the pid list fails", k: fakeKernel{listErr: boom}, want: "list the processes (proc_info listpids): boom"},
		{name: "a record fails", k: fakeKernel{pids: []int{7}, recErr: map[int]error{7: boom}}, want: "read pid 7's record (proc_info bsdinfo): boom"},
		{name: "a short record fails", k: fakeKernel{pids: []int{7}, recErr: map[int]error{7: syscall.EPERM}, shortEr: map[int]error{7: boom}}, want: "read pid 7's short record (proc_info shortbsdinfo): boom"},
		{name: "a command line fails", k: fakeKernel{pids: []int{7}, records: map[int]procRecord{7: {Status: 3}}, argsErr: map[int]error{7: boom}}, want: "read pid 7's command line (sysctl kern.procargs2): boom"},
		{name: "a command line is malformed", k: fakeKernel{pids: []int{7}, records: map[int]procRecord{7: {Status: 3}}, args: map[int][]byte{7: {1}}}, want: "read pid 7's command line: kern.procargs2: 1 bytes"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := tc.k.kernel().list()

			// Assert
			if err == nil || got != nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("list() = %v, %v; want error %q and no entries", got, err, tc.want)
			}
		})
	}
}

func TestListSkipsAProcessGoneBeforeItsShortRecord(t *testing.T) {
	// Arrange
	k := fakeKernel{pids: []int{7}, recErr: map[int]error{7: syscall.EPERM}, shortEr: map[int]error{7: syscall.ESRCH}}

	// Act
	got, err := k.kernel().list()

	// Assert
	if err != nil || len(got) != 0 {
		t.Fatalf("list() = %v, %v; want nothing", got, err)
	}
}
