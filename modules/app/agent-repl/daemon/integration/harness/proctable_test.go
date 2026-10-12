package harness

import (
	"encoding/binary"
	"os"
	"os/exec"
	"strings"
	"testing"
)

// procArgs2 builds a darwin kern.procargs2 buffer: argc, the executable path,
// NUL padding, the arguments, then an environment entry the decoder ignores.
func procArgs2(exe string, args ...string) []byte {
	buf := binary.LittleEndian.AppendUint32(nil, uint32(len(args)))
	buf = append(buf, exe...)
	buf = append(buf, 0, 0, 0)
	for _, a := range args {
		buf = append(append(buf, a...), 0)
	}
	return append(buf, "HOME=/nowhere\x00"...)
}

func TestParseProcArgs2(t *testing.T) {
	cases := []struct {
		name    string
		buf     []byte
		want    string
		wantErr bool
	}{
		{name: "arguments are joined by spaces", buf: procArgs2("/bin/daemon", "/bin/daemon", "--state-dir", "/tmp/s"), want: "/bin/daemon --state-dir /tmp/s"},
		{name: "the environment after argv is ignored", buf: procArgs2("/bin/x", "/bin/x"), want: "/bin/x"},
		{name: "an empty argv is the empty string", buf: procArgs2("/bin/x"), want: ""},
		{name: "a buffer too short for argc is refused", buf: []byte{1, 0}, wantErr: true},
		{name: "an unterminated executable path is refused", buf: append(binary.LittleEndian.AppendUint32(nil, 1), "/bin/x"...), wantErr: true},
		{name: "an unterminated argument is refused", buf: append(binary.LittleEndian.AppendUint32(nil, 2), "/bin/x\x00/bin/x\x00tail"...), wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := parseProcArgs2(tc.buf)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("parseProcArgs2 = %q, want an error", got)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("parseProcArgs2 = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestParseProcCmdline(t *testing.T) {
	cases := []struct {
		name string
		raw  string
		want string
	}{
		{name: "arguments are joined by spaces", raw: "/bin/daemon\x00--state-dir\x00/tmp/s\x00", want: "/bin/daemon --state-dir /tmp/s"},
		{name: "a single argument stands alone", raw: "/bin/x\x00", want: "/bin/x"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := parseProcCmdline([]byte(tc.raw))

			// Assert.
			if got != tc.want {
				t.Fatalf("parseProcCmdline = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestProcessTableListsALiveChildWithItsArgv asserts the table names a process
// this test started, under its own pid, with the argv it was started with.
func TestProcessTableListsALiveChildWithItsArgv(t *testing.T) {
	// Arrange: a child that waits on its stdin, so it lives until released.
	marker := "proctable-marker-" + t.Name()
	cmd := exec.Command("/bin/sh", "-c", "read _", marker)
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("stdin pipe: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the child: %v", err)
	}
	t.Cleanup(func() { _ = stdin.Close(); _ = cmd.Wait() })

	// Act.
	table, err := processTable()

	// Assert.
	if err != nil {
		t.Fatalf("processTable: %v", err)
	}
	for _, p := range table {
		if p.pid == cmd.Process.Pid {
			if !strings.Contains(p.args, marker) {
				t.Fatalf("pid %d argv = %q, want it to name %q", p.pid, p.args, marker)
			}
			return
		}
	}
	t.Fatalf("pid %d is not in the %d-entry table", cmd.Process.Pid, len(table))
}

// TestProcessTableListsThisProcess asserts the table includes the test binary
// itself, which listStrays then excludes by pid.
func TestProcessTableListsThisProcess(t *testing.T) {
	// Act.
	table, err := processTable()

	// Assert.
	if err != nil {
		t.Fatalf("processTable: %v", err)
	}
	for _, p := range table {
		if p.pid == os.Getpid() {
			return
		}
	}
	t.Fatalf("this process (pid %d) is not in the %d-entry table", os.Getpid(), len(table))
}
