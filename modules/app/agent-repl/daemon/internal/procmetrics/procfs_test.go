package procmetrics

import (
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
)

// A /proc fixture: the reader parses files, so a tree of them is the whole
// kernel it needs.

const fixtureBoot = 1_700_000_000

// fixtureProc is one process's files in a fixture tree.
type fixtureProc struct {
	pid     int
	stat    string
	status  string
	io      string
	cmdline string
	exe     string // "" for none: a kernel thread, or a hidden executable
	fds     int
}

// liveStat is a running process's stat: ppid 1, utime 150 and stime 50
// ticks, started 300 ticks after boot.
const liveStat = "42 (node) S 1 42 42 0 -1 4194304 0 0 0 0 150 50 0 0 20 0 7 0 300 0 0"

const liveStatus = "Name:\tnode\nThreads:\t7\nVmRSS:\t  2048 kB\nRssAnon:\t  1024 kB\nVmSwap:\t   512 kB\n"

const liveIO = "rchar: 9\nwchar: 9\nread_bytes: 4096\nwrite_bytes: 8192\n"

func newFixture(t *testing.T, procs ...fixtureProc) procfs {
	t.Helper()
	root := t.TempDir()
	write := func(path, content string) {
		t.Helper()
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	write(filepath.Join(root, "stat"), "cpu  1 2 3 4\nbtime "+strconv.Itoa(fixtureBoot)+"\nprocesses 9\n")
	write(filepath.Join(root, "meminfo"), "MemTotal:  16384 kB\nMemFree:  1024 kB\nMemAvailable: 8192 kB\nBuffers:  256 kB\nCached:  4096 kB\nActive:  2048 kB\nInactive:  3072 kB\n")
	write(filepath.Join(root, "self"), "") // a non-numeric entry, which is no process
	for _, p := range procs {
		dir := filepath.Join(root, strconv.Itoa(p.pid))
		write(filepath.Join(dir, "stat"), p.stat)
		write(filepath.Join(dir, "status"), p.status)
		write(filepath.Join(dir, "io"), p.io)
		write(filepath.Join(dir, "cmdline"), p.cmdline)
		if err := os.MkdirAll(filepath.Join(dir, "fd"), 0o755); err != nil {
			t.Fatal(err)
		}
		for i := range p.fds {
			write(filepath.Join(dir, "fd", strconv.Itoa(i)), "")
		}
		if p.exe != "" {
			if err := os.Symlink(p.exe, filepath.Join(dir, "exe")); err != nil {
				t.Fatal(err)
			}
		}
	}
	return newProcfs(root)
}

func liveProc() fixtureProc {
	return fixtureProc{pid: 42, stat: liveStat, status: liveStatus, io: liveIO,
		cmdline: "node\x00main.js\x00--listen\x00/s/ws.sock\x00", exe: "/usr/bin/node", fds: 3}
}

func TestProcfsListsEveryNumericEntry(t *testing.T) {
	// Arrange
	p := newFixture(t, liveProc(), fixtureProc{pid: 7, stat: liveStat})

	// Act
	pids, err := p.PIDs()

	// Assert
	if err != nil || len(pids) != 2 || !(pids[0] == 7 && pids[1] == 42 || pids[0] == 42 && pids[1] == 7) {
		t.Fatalf("PIDs = (%v, %v), want 7 and 42", pids, err)
	}
}

func TestProcfsListingAMissingTreeIsAnError(t *testing.T) {
	// Act
	_, err := newProcfs(filepath.Join(t.TempDir(), "absent")).PIDs()

	// Assert
	if err == nil || errors.Is(err, ErrGone) {
		t.Fatalf("PIDs = %v, want an error that is not ErrGone", err)
	}
}

func TestProcfsIdentity(t *testing.T) {
	tests := []struct {
		name     string
		proc     fixtureProc
		wantName string
	}{
		{name: "the executable's base name names the process", proc: liveProc(), wantName: "node"},
		{
			name:     "a long executable name is whole, unlike stat's command",
			proc:     fixtureProc{pid: 42, stat: strings.Replace(liveStat, "(node)", "(shim-claude-sid)", 1), exe: "/home/u/.cache/agent-repl/bin/shim-claude-sidecar"},
			wantName: "shim-claude-sidecar",
		},
		{
			name:     "a replaced executable is named without the deleted mark",
			proc:     fixtureProc{pid: 42, stat: liveStat, exe: "/usr/bin/shim-store (deleted)"},
			wantName: "shim-store",
		},
		{
			name:     "with no executable the command names the process",
			proc:     fixtureProc{pid: 42, stat: strings.Replace(liveStat, "(node)", "(systemd)", 1)},
			wantName: "systemd",
		},
		{
			name:     "a command holding parentheses and spaces is read whole",
			proc:     fixtureProc{pid: 42, stat: strings.Replace(liveStat, "(node)", "(a) b (c)", 1)},
			wantName: "a) b (c",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			p := newFixture(t, tt.proc)

			// Act
			id, err := p.Identity(42)

			// Assert
			want := Identity{PID: 42, PPID: 1, Name: tt.wantName, StartUnixMicros: fixtureBoot*1e6 + 3e6}
			if err != nil || id != want {
				t.Fatalf("Identity = (%+v, %v), want %+v", id, err, want)
			}
		})
	}
}

func TestProcfsUsage(t *testing.T) {
	// Arrange
	p := newFixture(t, liveProc())

	// Act
	u, err := p.Usage(42)

	// Assert
	want := Usage{CPUSeconds: 2, ResidentBytes: 2048 << 10, FootprintBytes: 1536 << 10, DiskReadBytes: 4096, DiskWriteBytes: 8192, Threads: 7, OpenFDs: 3}
	if err != nil || u != want {
		t.Fatalf("Usage = (%+v, %v), want %+v", u, err, want)
	}
}

func TestProcfsUsageOfAZombieIsGone(t *testing.T) {
	// Arrange: a zombie's status has no memory lines left.
	p := newFixture(t, fixtureProc{pid: 42, stat: strings.Replace(liveStat, ") S ", ") Z ", 1), status: "Name:\tnode\nThreads:\t1\n"})

	// Act
	_, err := p.Usage(42)

	// Assert
	if !errors.Is(err, ErrGone) {
		t.Fatalf("Usage = %v, want ErrGone", err)
	}
}

func TestProcfsAnswersGoneForAProcessThatDoesNotExist(t *testing.T) {
	// Arrange
	p := newFixture(t)

	// Act
	_, identityErr := p.Identity(42)
	_, usageErr := p.Usage(42)
	_, argsErr := p.Args(42)

	// Assert
	if !errors.Is(identityErr, ErrGone) || !errors.Is(usageErr, ErrGone) || !errors.Is(argsErr, ErrGone) {
		t.Fatalf("Identity = %v, Usage = %v, Args = %v, want ErrGone from all three", identityErr, usageErr, argsErr)
	}
}

func TestProcfsRefusesMalformedFiles(t *testing.T) {
	tests := []struct {
		name string
		proc fixtureProc
		read func(procfs) error
		want string
	}{
		{
			name: "a stat with no command",
			proc: fixtureProc{pid: 42, stat: "42 node S 1"},
			read: func(p procfs) error { _, err := p.Identity(42); return err },
			want: "no command name",
		},
		{
			name: "a stat cut short",
			proc: fixtureProc{pid: 42, stat: "42 (node) S 1 42"},
			read: func(p procfs) error { _, err := p.Identity(42); return err },
			want: "no field 14",
		},
		{
			name: "a stat field that is no number",
			proc: fixtureProc{pid: 42, stat: strings.Replace(liveStat, " 150 ", " x ", 1)},
			read: func(p procfs) error { _, err := p.Usage(42); return err },
			want: "stat field 14",
		},
		{
			name: "a status with no resident size",
			proc: fixtureProc{pid: 42, stat: liveStat, status: "Threads:\t7\n", io: liveIO},
			read: func(p procfs) error { _, err := p.Usage(42); return err },
			want: "no VmRSS field",
		},
		{
			name: "an io with no bytes written",
			proc: fixtureProc{pid: 42, stat: liveStat, status: liveStatus, io: "read_bytes: 1\n"},
			read: func(p procfs) error { _, err := p.Usage(42); return err },
			want: "io names no write_bytes",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			p := newFixture(t, tt.proc)

			// Act
			err := tt.read(p)

			// Assert
			if err == nil || errors.Is(err, ErrGone) || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("read = %v, want an error naming %q", err, tt.want)
			}
		})
	}
}

func TestProcfsIdentityRefusesAHostStatWithNoBootTime(t *testing.T) {
	// Arrange
	p := newFixture(t, liveProc())
	if err := os.WriteFile(filepath.Join(p.root, "stat"), []byte("cpu 1 2 3 4\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := p.Identity(42)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no btime") {
		t.Fatalf("Identity = %v, want the missing btime named", err)
	}
}

func TestProcfsArgs(t *testing.T) {
	tests := []struct {
		name    string
		cmdline string
		want    []string
	}{
		{name: "argv split on its NULs", cmdline: "node\x00main.js\x00--listen\x00/s/ws.sock\x00", want: []string{"node", "main.js", "--listen", "/s/ws.sock"}},
		{name: "an empty argument kept", cmdline: "a\x00\x00b\x00", want: []string{"a", "", "b"}},
		{name: "a kernel thread has no argv", cmdline: "", want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			p := newFixture(t, fixtureProc{pid: 42, stat: liveStat, cmdline: tt.cmdline})

			// Act
			args, err := p.Args(42)

			// Assert
			if err != nil || !reflect.DeepEqual(args, tt.want) {
				t.Fatalf("Args = (%q, %v), want %q", args, err, tt.want)
			}
		})
	}
}

func TestProcfsMemory(t *testing.T) {
	// Arrange
	p := newFixture(t)

	// Act
	states, total, err := p.Memory()

	// Assert
	want := map[string]uint64{"free": 1024 << 10, "active": 2048 << 10, "inactive": 3072 << 10, "buffers": 256 << 10, "cached": 4096 << 10}
	if err != nil || total != 16384<<10 || !reflect.DeepEqual(states, want) {
		t.Fatalf("Memory = (%v, %d, %v), want %v and %d", states, total, err, want, 16384<<10)
	}
}

func TestProcfsMemoryRefusesAMeminfoMissingAState(t *testing.T) {
	// Arrange
	p := newFixture(t)
	if err := os.WriteFile(filepath.Join(p.root, "meminfo"), []byte("MemTotal: 1 kB\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, _, err := p.Memory()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "meminfo") {
		t.Fatalf("Memory = %v, want a meminfo error", err)
	}
}
