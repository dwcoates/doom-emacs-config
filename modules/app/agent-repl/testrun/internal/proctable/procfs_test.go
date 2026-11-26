package proctable

import (
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/logging/procstat"
)

// procFixture is one process in a /proc fixture tree.
type procFixture struct {
	pid       int
	stat      string
	uid       int
	cmdline   string
	environ   string
	noEnviron bool // the environment is another user's, unreadable
}

const fixtureBoot = 1_700_000_000

// fixtureStat is a stat line: ppid, pgid, state, 30+10 own and 5+5 reaped
// children's ticks, started 100 ticks (1s) after boot.
func fixtureStat(pid, ppid, pgid int, state, comm string) string {
	return strconv.Itoa(pid) + " (" + comm + ") " + state + " " + strconv.Itoa(ppid) + " " + strconv.Itoa(pgid) +
		" 0 0 -1 0 0 0 0 0 30 10 5 5 20 0 1 0 100 0 0"
}

func newProcTree(t *testing.T, procs ...procFixture) procstat.Tree {
	t.Helper()
	root := t.TempDir()
	write := func(path, content string, mode os.FileMode) {
		t.Helper()
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), mode); err != nil {
			t.Fatal(err)
		}
	}
	write(filepath.Join(root, "stat"), "btime "+strconv.Itoa(fixtureBoot)+"\n", 0o644)
	for _, p := range procs {
		dir := filepath.Join(root, strconv.Itoa(p.pid))
		write(filepath.Join(dir, "stat"), p.stat, 0o644)
		u := strconv.Itoa(p.uid)
		write(filepath.Join(dir, "status"), "Name:\tx\nUid:\t"+u+"\t"+u+"\t"+u+"\t"+u+"\n", 0o644)
		write(filepath.Join(dir, "cmdline"), p.cmdline, 0o644)
		mode := os.FileMode(0o644)
		if p.noEnviron {
			mode = 0
		}
		write(filepath.Join(dir, "environ"), p.environ, mode)
	}
	return procstat.New(root)
}

var fixtureStart = time.Unix(fixtureBoot+1, 0)

func TestProcfsListReadsEveryProcess(t *testing.T) {
	// Arrange
	tree := newProcTree(t,
		procFixture{pid: 10, stat: fixtureStat(10, 1, 10, "S", "sh"), uid: 501, cmdline: "sh\x00-c\x00", environ: "A=1\x00"},
		procFixture{pid: 11, stat: fixtureStat(11, 10, 10, "Z", "zomb"), uid: 501},
		procFixture{pid: 12, stat: fixtureStat(12, 1, 12, "S", "rootd"), uid: 0, cmdline: "/usr/sbin/rootd\x00", noEnviron: true},
	)

	// Act
	got, err := procfsKernel(tree).list()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	byPid := map[int]Entry{}
	for _, e := range got {
		byPid[e.Pid] = e
	}
	want := map[int]Entry{
		10: {Pid: 10, Ppid: 1, Pgid: 10, State: 'S', Start: fixtureStart, Comm: "sh", Argv: []string{"sh", "-c"}, Env: []string{"A=1"}},
		11: {Pid: 11, Ppid: 10, Pgid: 10, State: 'Z', Start: fixtureStart, Comm: "zomb"},
		12: {Pid: 12, Ppid: 1, Pgid: 12, State: 'S', Start: fixtureStart, Comm: "rootd", Argv: []string{"/usr/sbin/rootd"}},
	}
	if !reflect.DeepEqual(byPid, want) {
		t.Fatalf("list() =\n%+v\nwant\n%+v", byPid, want)
	}
}

func TestProcfsRecordAnswersTheErrnoTheSharedLogicReads(t *testing.T) {
	// Arrange
	tree := newProcTree(t)

	// Act
	_, err := procfsRecord(tree)(42)

	// Assert
	if !errors.Is(err, syscall.ESRCH) {
		t.Fatalf("record of an absent pid = %v, want ESRCH", err)
	}
}

func TestProcfsRecordRefusesAStatusWithNoEffectiveUser(t *testing.T) {
	// Arrange
	tree := newProcTree(t, procFixture{pid: 10, stat: fixtureStat(10, 1, 10, "S", "sh")})
	if err := os.WriteFile(tree.Path(10, "status"), []byte("Uid:\t501\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := procfsRecord(tree)(10)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "names no effective user") {
		t.Fatalf("record = %v, want the missing effective user named", err)
	}
}

func TestProcfsMarkedScanReadsThisUsersMarkAndCPU(t *testing.T) {
	// Arrange
	tree := newProcTree(t,
		procFixture{pid: 10, stat: fixtureStat(10, 1, 10, "S", "go"), uid: 501, cmdline: "go\x00", environ: "MARK=1\x00"},
		procFixture{pid: 11, stat: fixtureStat(11, 1, 11, "S", "sh"), uid: 501, cmdline: "sh\x00", environ: "OTHER=1\x00"},
		procFixture{pid: 12, stat: fixtureStat(12, 1, 12, "S", "rootd"), uid: 0, cmdline: "rootd\x00", noEnviron: true},
	)
	m := &Marked{Name: "MARK", UID: 501, k: procfsMarkedKernel(tree)}

	// Act
	got, err := m.Scan()

	// Assert
	want := []Usage{{Pid: 10, Ppid: 1, Start: fixtureStart, Mark: "1", Program: "go", Own: 0.4, Children: 0.1}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("Scan() = %+v, %v; want %+v", got, err, want)
	}
}

func TestProcfsErrno(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want error
	}{
		{name: "a gone process is ESRCH", err: procstat.ErrGone, want: syscall.ESRCH},
		{name: "a refused read is EPERM", err: os.ErrPermission, want: syscall.EPERM},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := procfsErrno("/proc stat", tc.err)

			// Assert
			if !errors.Is(got, tc.want) || !strings.HasPrefix(got.Error(), "/proc stat: ") {
				t.Fatalf("procfsErrno = %v, want %v named for its call", got, tc.want)
			}
		})
	}
}

func TestProcfsErrnoKeepsAnyOtherCause(t *testing.T) {
	// Arrange
	boom := errors.New("boom")

	// Act
	got := procfsErrno("/proc stat", boom)

	// Assert
	if !errors.Is(got, boom) || errors.Is(got, syscall.ESRCH) || errors.Is(got, syscall.EPERM) {
		t.Fatalf("procfsErrno = %v, want boom alone", got)
	}
}
