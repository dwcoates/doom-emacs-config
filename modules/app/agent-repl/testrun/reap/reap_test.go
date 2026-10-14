package reap

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"
)

// recLog records every line Reap logs.
type recLog struct{ info, errs []string }

func (l *recLog) Infof(format string, args ...any) {
	l.info = append(l.info, fmt.Sprintf(format, args...))
}
func (l *recLog) Errorf(format string, args ...any) {
	l.errs = append(l.errs, fmt.Sprintf(format, args...))
}

func (l *recLog) infoHas(sub string) bool {
	return slices.ContainsFunc(l.info, func(s string) bool { return strings.Contains(s, sub) })
}

func (l *recLog) errHas(sub string) bool {
	return slices.ContainsFunc(l.errs, func(s string) bool { return strings.Contains(s, sub) })
}

// fakeTable is a process table a test scripts. A SIGSTOP marks a process
// stopped and runs onStop (a fork the stopped process made first); a SIGKILL
// makes it a zombie unless it is unkillable.
type fakeTable struct {
	procs      map[int]*Proc
	signals    []string
	onStop     map[int]func(*fakeTable)
	listErr    []error
	stopErr    map[int]error
	killErr    map[int]error
	unkillable map[int]bool
	awaitErr   error
	awaited    []int
}

func newFakeTable(procs ...Proc) *fakeTable {
	t := &fakeTable{procs: map[int]*Proc{}, onStop: map[int]func(*fakeTable){}, stopErr: map[int]error{}, killErr: map[int]error{}, unkillable: map[int]bool{}}
	for _, p := range procs {
		t.add(p)
	}
	return t
}

func (t *fakeTable) add(p Proc) {
	if p.State == 0 {
		p.State = 'S'
	}
	t.procs[p.Pid] = &p
}

func (t *fakeTable) List() ([]Proc, error) {
	if len(t.listErr) > 0 {
		err := t.listErr[0]
		t.listErr = t.listErr[1:]
		if err != nil {
			return nil, err
		}
	}
	var out []Proc
	for _, p := range t.procs {
		out = append(out, *p)
	}
	slices.SortFunc(out, func(a, b Proc) int { return a.Pid - b.Pid })
	return out, nil
}

func (t *fakeTable) Signal(pid int, sig syscall.Signal) error {
	t.signals = append(t.signals, fmt.Sprintf("%v:%d", sig, pid))
	p, ok := t.procs[pid]
	if !ok {
		return syscall.ESRCH
	}
	switch sig {
	case syscall.SIGSTOP:
		if err := t.stopErr[pid]; err != nil {
			return err
		}
		if fork := t.onStop[pid]; fork != nil {
			fork(t)
		}
		p.State = 'T'
	case syscall.SIGKILL:
		if err := t.killErr[pid]; err != nil {
			return err
		}
		if !t.unkillable[pid] {
			p.State = 'Z'
		}
	}
	return nil
}

func (t *fakeTable) AwaitExit(pids []int, _ time.Duration) ([]int, error) {
	t.awaited = append(t.awaited, pids...)
	if t.awaitErr != nil {
		return nil, t.awaitErr
	}
	var left []int
	for _, pid := range pids {
		if p, ok := t.procs[pid]; ok && p.State != 'Z' {
			left = append(left, pid)
		}
	}
	return left, nil
}

var (
	since  = time.Date(2026, 10, 14, 1, 0, 0, 0, time.UTC)
	before = since.Add(-time.Hour)
	after  = since.Add(time.Minute)
)

// leader is the run's session leader, the reaper itself.
const leader = 500

func runMembers() Members {
	return Members{What: "run", Session: leader, Since: since, Dirs: []string{"/tmp/artr-7"}, Except: []int{leader}}
}

func member(pid int, line string) Proc { return Proc{Pid: pid, Sid: leader, Start: after, Line: line} }

func pidsOf(procs []Proc) []int {
	var pids []int
	for _, p := range procs {
		pids = append(pids, p.Pid)
	}
	return pids
}

func TestReapFreezesUntilAPassFindsNothingNewThenKillsEveryMember(t *testing.T) {
	// Arrange: 501 forks 601 while it is being stopped, and 601 forks 701
	// while it is.
	table := newFakeTable(Proc{Pid: leader, Sid: leader, Start: since, Line: "testrun run"}, member(501, "bash harness.sh"))
	table.onStop[501] = func(t *fakeTable) { t.add(member(601, "git status")) }
	table.onStop[601] = func(t *fakeTable) { t.add(member(701, "git ls-files")) }
	log := &recLog{}

	// Act.
	res, err := Reap(table, runMembers(), log)

	// Assert.
	if err != nil {
		t.Fatalf("Reap() error = %v", err)
	}
	if res.Passes != 4 || !slices.Equal(pidsOf(res.Killed), []int{501, 601, 701}) {
		t.Fatalf("Reap() = %d passes killing %v, want 4 passes killing [501 601 701]", res.Passes, pidsOf(res.Killed))
	}
	for _, want := range []string{"freeze pass 1 stopped 1 process(es): bash[501]", "freeze pass 2 stopped 1 process(es): git[601]", "freeze pass 3 stopped 1 process(es): git[701]", "freeze pass 4 found no member left running; 3 member(s) stopped", "killed 3 member(s), all exited in "} {
		if !log.infoHas(want) {
			t.Errorf("no log line %q in %q", want, log.info)
		}
	}
	if table.procs[leader].State != 'S' {
		t.Error("the reaper stopped or killed itself")
	}
}

// TestReapLogsHowLongEachListingTook pins the record ListBound is set from:
// every freeze pass and the confirmation name their listing's time.
func TestReapLogsHowLongEachListingTook(t *testing.T) {
	// Arrange.
	table := newFakeTable(member(501, "bash harness.sh"))
	log := &recLog{}
	took := regexp.MustCompile(`the (confirming )?listing took [0-9.]+(ns|µs|ms|s)$`)

	// Act.
	_, err := Reap(table, runMembers(), log)

	// Assert.
	if err != nil {
		t.Fatalf("Reap() error = %v", err)
	}
	for _, prefix := range []string{"reap run: freeze pass 1 stopped", "reap run: freeze pass 2 found no member", "reap run: killed 1 member(s)"} {
		i := slices.IndexFunc(log.info, func(s string) bool { return strings.HasPrefix(s, prefix) })
		if i < 0 || !took.MatchString(log.info[i]) {
			t.Errorf("no line %q... naming its listing's time in %q", prefix, log.info)
		}
	}
	if !strings.Contains(log.info[len(log.info)-1], "the confirming listing took") {
		t.Errorf("the kill line %q does not name the confirming listing", log.info[len(log.info)-1])
	}
}

func TestReapStopsEveryMemberBeforeKillingAny(t *testing.T) {
	// Arrange.
	table := newFakeTable(member(501, "a"), member(502, "b"))

	// Act.
	if _, err := Reap(table, runMembers(), &recLog{}); err != nil {
		t.Fatalf("Reap() error = %v", err)
	}

	// Assert.
	want := []string{"suspended (signal):501", "suspended (signal):502", "killed:501", "killed:502"}
	got := slices.Clone(table.signals)
	slices.Sort(got[2:])
	if !slices.Equal(got, want) {
		t.Fatalf("signals = %v, want %v", got, want)
	}
}

func TestReapLeavesNonMembersAlone(t *testing.T) {
	tests := []struct {
		name string
		proc Proc
	}{
		{name: "another session", proc: Proc{Pid: 801, Sid: 900, Start: after, Line: "emacs"}},
		{name: "the run's session but started before its leader", proc: Proc{Pid: 802, Sid: leader, Start: before, Line: "old"}},
		{name: "an excepted process", proc: Proc{Pid: leader, Sid: leader, Start: since, Line: "testrun"}},
		{name: "a zombie of the session", proc: Proc{Pid: 803, Sid: leader, Start: after, State: 'Z', Line: "done"}},
		{name: "a sibling of the net's directory", proc: Proc{Pid: 804, Sid: 900, Start: after, Line: "sh TMPDIR=/tmp/artr-71/x"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			table := newFakeTable(tt.proc)

			// Act.
			res, err := Reap(table, runMembers(), &recLog{})

			// Assert.
			if err != nil || len(res.Killed) != 0 || len(table.signals) != 0 {
				t.Fatalf("Reap() = %v, %v with signals %v, want nothing touched", pidsOf(res.Killed), err, table.signals)
			}
		})
	}
}

func TestReapTakesTheSessionsEscapeThroughTheDirectoryNet(t *testing.T) {
	// Arrange: the rollout successor, in a session of its own, running out of
	// the run's mount.
	table := newFakeTable(Proc{Pid: 901, Sid: 901, Start: after, Line: "agent-repld TMPDIR=/tmp/artr-7/tr-1"})

	// Act.
	res, err := Reap(table, runMembers(), &recLog{})

	// Assert.
	if err != nil || !slices.Equal(pidsOf(res.Killed), []int{901}) {
		t.Fatalf("Reap() = %v, %v, want 901 killed", pidsOf(res.Killed), err)
	}
}

func TestReapDropsASessionIdALiveStrangerNowLeads(t *testing.T) {
	// Arrange: the dead run's session id is a new session's now, led by a
	// live process that started at another time.
	table := newFakeTable(Proc{Pid: leader, Sid: leader, Start: after, Line: "zsh"}, Proc{Pid: 502, Sid: leader, Start: after, Line: "vim"})
	m := runMembers()
	m.Except = nil
	log := &recLog{}

	// Act.
	res, err := Reap(table, m, log)

	// Assert.
	if err != nil || len(res.Killed) != 0 {
		t.Fatalf("Reap() = %v, %v, want nothing killed", pidsOf(res.Killed), err)
	}
	if !log.infoHas("session 500 now names another session") {
		t.Fatalf("no record of the reused session id: %q", log.info)
	}
}

func TestReapTakesADeadLeadersSession(t *testing.T) {
	// Arrange: the runner is gone; its session's members remain.
	table := newFakeTable(member(502, "go test"))
	m := runMembers()
	m.Except = nil

	// Act.
	res, err := Reap(table, m, &recLog{})

	// Assert.
	if err != nil || !slices.Equal(pidsOf(res.Killed), []int{502}) {
		t.Fatalf("Reap() = %v, %v, want 502 killed", pidsOf(res.Killed), err)
	}
}

func TestReapWithNothingToReapSignalsNothing(t *testing.T) {
	// Arrange.
	table := newFakeTable()

	// Act.
	res, err := Reap(table, runMembers(), &recLog{})

	// Assert.
	if err != nil || res.Passes != 1 || len(table.signals) != 0 || len(table.awaited) != 0 {
		t.Fatalf("Reap() = %+v, %v, signals %v, awaited %v; want one pass and nothing else", res, err, table.signals, table.awaited)
	}
}

func TestReapToleratesAMemberGoneBeforeItsSignal(t *testing.T) {
	// Arrange: 502 is listed, then gone before it is stopped.
	table := newFakeTable(member(501, "a"), member(502, "b"))
	table.onStop[501] = func(t *fakeTable) { delete(t.procs, 502) }

	// Act.
	_, err := Reap(table, runMembers(), &recLog{})

	// Assert.
	if err != nil {
		t.Fatalf("Reap() error = %v, want a vanished member tolerated", err)
	}
}

func TestReapFailures(t *testing.T) {
	boom := errors.New("boom")
	tests := []struct {
		name    string
		arrange func(t *fakeTable)
		want    string
		// killed says the members were still killed despite the failure.
		killed bool
	}{
		{name: "the first listing fails", arrange: func(t *fakeTable) { t.listErr = []error{boom} }, want: "list the processes on freeze pass 1: boom"},
		{name: "a stop fails", arrange: func(t *fakeTable) { t.stopErr[501] = syscall.EPERM }, want: "stop pid 501 (a): operation not permitted", killed: true},
		{name: "a kill fails", arrange: func(t *fakeTable) { t.killErr[501] = syscall.EPERM }, want: "kill pid 501 (a): operation not permitted"},
		{name: "waiting for the exits fails", arrange: func(t *fakeTable) { t.awaitErr = boom }, want: "wait for the 1 killed member(s) to exit: boom", killed: true},
		{name: "a member does not exit", arrange: func(t *fakeTable) { t.unkillable[501] = true }, want: "1 killed member(s) had not exited within 1s: pids [501]"},
		{name: "the confirming listing fails", arrange: func(t *fakeTable) { t.listErr = []error{nil, nil, boom} }, want: "list the processes to confirm no member remains: boom", killed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			table := newFakeTable(member(501, "a"))
			tt.arrange(table)
			log := &recLog{}

			// Act.
			_, err := Reap(table, runMembers(), log)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Reap() error = %v, want %q", err, tt.want)
			}
			if !log.errHas("reap run: ") || !log.errHas(tt.want) {
				t.Fatalf("no ERROR record naming %q: %q", tt.want, log.errs)
			}
			if tt.killed && table.procs[501].State != 'Z' {
				t.Fatalf("the member was left %c, want killed despite the failure", table.procs[501].State)
			}
		})
	}
}

func TestReapReportsAMemberStillListedAfterItsKill(t *testing.T) {
	// Arrange: the exit wait answers 501 gone, but the confirming listing
	// still shows it running.
	table := newFakeTable(member(501, "a"))
	table.unkillable[501] = true
	table.awaitErr = nil
	log := &recLog{}
	wrapped := awaitAll{table}

	// Act.
	_, err := Reap(wrapped, runMembers(), log)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "1 member(s) remain after the kill: a[501]") {
		t.Fatalf("Reap() error = %v, want the remaining member named", err)
	}
	if !log.errHas("remain after the kill") {
		t.Fatalf("no ERROR record: %q", log.errs)
	}
}

// awaitAll is a table whose exit wait answers every pid gone.
type awaitAll struct{ *fakeTable }

func (awaitAll) AwaitExit([]int, time.Duration) ([]int, error) { return nil, nil }

func TestParsePS(t *testing.T) {
	// Arrange.
	out := "  101 Ss   Tue Oct 14 01:20:27 2026     /bin/bash -c x TMPDIR=/tmp/artr-7\n" +
		"  102 T    Tue Oct  7 09:05:01 2026     git status\n" +
		"  103 S    Tue Oct 14 01:20:27 2026     gone\n" +
		"  104 S    Tue Oct 14 01:20:27 2026     root-owned\n"
	getsid := func(pid int) (int, error) {
		switch pid {
		case 103:
			return 0, syscall.ESRCH
		case 104:
			return 0, syscall.EPERM
		}
		return 77, nil
	}

	// Act.
	got, err := parsePS(out, getsid)

	// Assert.
	if err != nil {
		t.Fatalf("parsePS() error = %v", err)
	}
	want := []Proc{
		{Pid: 101, Sid: 77, State: 'S', Start: time.Date(2026, 10, 14, 1, 20, 27, 0, time.Local), Line: "/bin/bash -c x TMPDIR=/tmp/artr-7"},
		{Pid: 102, Sid: 77, State: 'T', Start: time.Date(2026, 10, 7, 9, 5, 1, 0, time.Local), Line: "git status"},
		{Pid: 104, Sid: -1, State: 'S', Start: time.Date(2026, 10, 14, 1, 20, 27, 0, time.Local), Line: "root-owned"},
	}
	if len(got) != len(want) {
		t.Fatalf("parsePS() = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i].Pid != want[i].Pid || got[i].Sid != want[i].Sid || got[i].State != want[i].State || !got[i].Start.Equal(want[i].Start) || got[i].Line != want[i].Line {
			t.Errorf("parsePS()[%d] = %+v, want %+v", i, got[i], want[i])
		}
	}
}

func TestParsePSFailures(t *testing.T) {
	tests := []struct {
		name string
		out  string
		sid  error
		want string
	}{
		{name: "a short line", out: "101 S Tue\n", want: "has fewer fields"},
		{name: "no pid", out: "abc S Tue Oct 14 01:20:27 2026 x\n", want: "names no pid"},
		{name: "no start", out: "101 S Tue Oct 99 01:20:27 2026 x\n", want: "names no start time"},
		{name: "getsid fails", out: "101 S Tue Oct 14 01:20:27 2026 x\n", sid: syscall.EINVAL, want: "getsid(101)"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := parsePS(tt.out, func(int) (int, error) { return 1, tt.sid })

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("parsePS() error = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestNames(t *testing.T) {
	tests := []struct {
		name string
		line string
		want bool
	}{
		{name: "a path under dir", line: "/tmp/artr-7/bin/daemon", want: true},
		{name: "dir in the environment", line: "emacs TMPDIR=/tmp/artr-7/tr-1", want: true},
		{name: "dir at the end", line: "go test AGENT_REPL_TEST_SHORT_BASE=/tmp/artr-7", want: true},
		{name: "dir before a space", line: "x=/tmp/artr-7 y", want: true},
		{name: "a sibling sharing the prefix", line: "sh /tmp/artr-71/x", want: false},
		{name: "a sibling then dir", line: "sh /tmp/artr-71/x /tmp/artr-7/y", want: true},
		{name: "nothing", line: "/usr/bin/emacs", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act, Assert.
			if got := names(tt.line, "/tmp/artr-7/"); got != tt.want {
				t.Fatalf("names(%q) = %v, want %v", tt.line, got, tt.want)
			}
		})
	}
}

func TestSpellings(t *testing.T) {
	// Arrange: a parent reached through a symlink.
	resolvedReal, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(resolvedReal, link); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name string
		dir  string
		want []string
	}{
		{name: "a resolved parent is one spelling", dir: filepath.Join(resolvedReal, "artr-7"), want: []string{filepath.Join(resolvedReal, "artr-7")}},
		{name: "a symlinked parent is two", dir: filepath.Join(link, "artr-7"), want: []string{filepath.Join(link, "artr-7"), filepath.Join(resolvedReal, "artr-7")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := Spellings(tc.dir)

			// Assert
			if err != nil || !slices.Equal(got, tc.want) {
				t.Fatalf("Spellings = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestUnderFailsAMissingParentWithoutReaping(t *testing.T) {
	// Arrange.
	table := newFakeTable()

	// Act
	_, err := Under(table, filepath.Join(t.TempDir(), "gone", "artr-7"), &recLog{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "resolve the directory holding") || len(table.signals) != 0 {
		t.Fatalf("Under = %v with signals %v; want the failed resolution and nothing signalled", err, table.signals)
	}
}

func TestUnderReapsWhatRunsOutOfDir(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	table := newFakeTable(Proc{Pid: 501, Sid: 9, Start: after, Line: "sh TMPDIR=" + dir + "/tr-1"}, Proc{Pid: 502, Sid: 9, Start: after, Line: "emacs"})

	// Act
	res, err := Under(table, dir, &recLog{})

	// Assert
	if err != nil || !slices.Equal(pidsOf(res.Killed), []int{501}) {
		t.Fatalf("Under = %v, %v; want 501 reaped", pidsOf(res.Killed), err)
	}
}

func TestReclaimsShareTheOneReap(t *testing.T) {
	// Arrange: every reclaim of a run's processes.
	sites := []string{"../internal/ramdisk/ramdisk.go", "../internal/life/runner.go", "../internal/life/warden.go", "../../daemon/integration/harness/runroot.go"}

	for _, site := range sites {
		// Act
		src, err := os.ReadFile(site)
		if err != nil {
			t.Fatal(err)
		}

		// Assert
		body := string(src)
		if !strings.Contains(body, "reap.") || strings.Contains(body, "pid=,") || strings.Contains(body, "syscall.SIGKILL") {
			t.Errorf("%s kills processes by hand; want the one reap", site)
		}
	}
}

func TestDescribeNamesEachMembersTMPDIR(t *testing.T) {
	// Arrange.
	procs := []Proc{
		{Pid: 9, Line: "/bin/sleep 60 HOME=/Users/x TMPDIR=/tmp/artr-7/tr-1/tu-2/test-suite-slot.x"},
		{Pid: 3, Line: "/usr/bin/git status"},
	}

	// Act.
	got := describe(procs)

	// Assert.
	if want := "git[3], sleep[9] (TMPDIR=/tmp/artr-7/tr-1/tu-2/test-suite-slot.x)"; got != want {
		t.Fatalf("describe() = %q, want %q", got, want)
	}
}
