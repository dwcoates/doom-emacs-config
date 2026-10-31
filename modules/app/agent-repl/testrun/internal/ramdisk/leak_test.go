package ramdisk

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/testrun/internal/proctable"
	"agentrepl/testrun/reap"
)

// liveRecord writes a record and holds its lock for the test's life.
func liveRecord(t *testing.T, m Manager, pid, device, mount string) {
	t.Helper()
	path := deadRecord(t, m, pid, device, mount)
	held, err := os.OpenFile(path, os.O_RDWR, 0)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { held.Close() })
	if err := syscall.Flock(int(held.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatal(err)
	}
}

func attachCalled(calls []string) bool {
	for _, call := range calls {
		if strings.HasPrefix(call, "hdiutil attach") {
			return true
		}
	}
	return false
}

func TestAcquireRefusesWhileADeadRunsRecordedRAMDiskStands(t *testing.T) {
	// Arrange: the reclaim of pid 7 failed, so its record still names its disk.
	h := &fakeHost{info: ramInfo("/dev/disk7")}
	m := newManager(t, h)
	record := deadRecord(t, m, "7", "/dev/disk7", "/tmp/artr-7")

	// Act
	_, err := m.Acquire(16)

	// Assert
	var leaked *LeakError
	if !errors.As(err, &leaked) || len(leaked.Leaks) != 1 {
		t.Fatalf("Acquire = %v, want one leak", err)
	}
	if got := leaked.Leaks[0]; got.Device != "/dev/disk7" || got.Pid != 7 || got.Record != record {
		t.Fatalf("leak = %+v, want /dev/disk7 of pid 7 named by %s", got, record)
	}
}

func TestAcquireRefusesWhileAnUnrecordedRunsRAMDiskStands(t *testing.T) {
	// Arrange: a run's disk mounted at its artr-<pid> mount, with no record.
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7583")}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	var leaked *LeakError
	if !errors.As(err, &leaked) || len(leaked.Leaks) != 1 || leaked.Leaks[0].Pid != 7583 {
		t.Fatalf("Acquire = %v, want the disk of pid 7583 leaked", err)
	}
}

func TestAcquireAttachesNothingWhileALeakStands(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7583")}
	m := newManager(t, h)

	// Act
	m.Acquire(16) //nolint:errcheck // the calls are the subject

	// Assert
	if attachCalled(h.calls) {
		t.Fatalf("calls = %q, want no attach while a leak stands", h.calls)
	}
}

func TestAcquireMakesNoRecordWhileALeakStands(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7583")}
	m := newManager(t, h)

	// Act
	m.Acquire(16) //nolint:errcheck // the registry is the subject

	// Assert
	if _, err := os.Stat(filepath.Join(m.Registry, "4242.lock")); !os.IsNotExist(err) {
		t.Fatalf("a record was made while a leak stands (stat err = %v)", err)
	}
}

func TestAcquireSaysWhetherALeaksOwnerIsAlive(t *testing.T) {
	// Arrange: the owner is stuck exiting in the kernel, still a process.
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7583")}
	m := newManager(t, h)
	m.Alive = func(pid int) bool { return pid == 7583 }

	// Act
	_, err := m.Acquire(16)

	// Assert
	var leaked *LeakError
	if !errors.As(err, &leaked) || !leaked.Leaks[0].PidAlive {
		t.Fatalf("Acquire = %v, want the owner said to be alive", err)
	}
}

func TestAcquireAttachesBesideALiveRunsRAMDisk(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7")}
	m := newManager(t, h)
	liveRecord(t, m, "7", "/dev/disk4", "/private/tmp/artr-7")

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire = %v, want a live run's disk not taken for a leak", err)
	}
	v.Release() //nolint:errcheck // cleanup
}

func TestAcquireLeavesARAMDiskTheRunnerDidNotMakeOutOfTheCheck(t *testing.T) {
	// Arrange: a RAM disk nothing marks as the runner's.
	h := &fakeHost{info: ramInfo("/dev/disk4=/Volumes/Scratch")}
	m := newManager(t, h)

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire = %v, want a foreign RAM disk left out of the check", err)
	}
	v.Release() //nolint:errcheck // cleanup
}

func TestLeakString(t *testing.T) {
	tests := []struct {
		name string
		leak Leak
		want string
	}{
		{
			name: "a recorded leak of a dead run",
			leak: Leak{Device: "/dev/disk7", Pid: 7, Record: "/r/7.lock"},
			want: "/dev/disk7 (owning run pid 7, not alive, not mounted, named by the dead run's record /r/7.lock)",
		},
		{
			name: "an unrecorded leak of a live run",
			leak: Leak{Device: "/dev/disk4", Pid: 7583, PidAlive: true, Mounts: []string{"/private/tmp/artr-7583"}},
			want: "/dev/disk4 (owning run pid 7583, still alive, mounted at /private/tmp/artr-7583, named by no record)",
		},
		{
			name: "an unknown owner",
			leak: Leak{Device: "/dev/disk4"},
			want: "/dev/disk4 (owning pid unknown, not mounted, named by no record)",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.leak.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTroubleString(t *testing.T) {
	tests := []struct {
		name    string
		trouble Trouble
		want    string
	}{
		{
			name:    "a leak with an answering disk manager",
			trouble: Trouble{Leaks: []Leak{{Device: "/dev/disk7"}}, InfoTook: 9 * time.Millisecond, ListTook: 25 * time.Millisecond},
			want:    "1 leaked RAM disk(s): /dev/disk7 (owning pid unknown, not mounted, named by no record); the disk manager answered hdiutil info in 9ms; the disk manager answered diskutil list in 25ms",
		},
		{
			name:    "a stuck registry lock",
			trouble: Trouble{Lock: &LockTimeoutError{Path: "/r/registry.mutex", Bound: time.Second, Holders: "testrun[7583]"}},
			want:    "the RAM disk registry lock /r/registry.mutex was not taken within 1s, so its holder is taken to be stuck (processes with it open: testrun[7583]); the disk manager answered hdiutil info in 0s; the disk manager answered diskutil list in 0s",
		},
		{
			name:    "a disk manager that did not answer",
			trouble: Trouble{ListErr: errors.New("timed out"), ListTook: 100 * time.Millisecond, InfoTook: 9 * time.Millisecond},
			want:    "the disk manager answered hdiutil info in 9ms; the disk manager did not answer diskutil list after 100ms: timed out",
		},
		{
			name:    "the host when the disk manager did not answer",
			trouble: Trouble{InfoErr: errors.New("timed out"), Host: "load average 28.7 21.4 17.2\ndisk process 357 S - /usr/libexec/diskarbitrationd\n"},
			want:    "the disk manager did not answer hdiutil info after 0s: timed out; the disk manager answered diskutil list in 0s; the host then: load average 28.7 21.4 17.2, disk process 357 S - /usr/libexec/diskarbitrationd",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.trouble.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String =\n%q\nwant\n%q", got, tc.want)
			}
		})
	}
}

func TestSetupAcquiresOnAHealthyHost(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || trouble != nil || v == nil {
		t.Fatalf("Setup = %v, %v, %v; want a volume", v, trouble, err)
	}
	v.Release() //nolint:errcheck // cleanup
}

func TestSetupAnswersADiskManagerThatDoesNotListAsTrouble(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}}
	m := newManager(t, h)

	// Act
	_, trouble, err := m.Setup(16)

	// Assert
	if err != nil || trouble == nil || trouble.ListErr == nil {
		t.Fatalf("Setup = %v, %v; want the unanswered diskutil list as trouble", trouble, err)
	}
}

func TestSetupAnswersADiskManagerThatDoesNotListImagesAsTrouble(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}}
	m := newManager(t, h)

	// Act
	_, trouble, err := m.Setup(16)

	// Assert
	if err != nil || trouble == nil || trouble.InfoErr == nil {
		t.Fatalf("Setup = %v, %v; want the unanswered hdiutil info as trouble", trouble, err)
	}
}

func TestSetupAsksAWedgedDiskManagerNothingMore(t *testing.T) {
	// Arrange: a dead run's record that a reclaim would detach.
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}, info: attachedInfo}
	m := newManager(t, h)
	deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))

	// Act
	m.Setup(16) //nolint:errcheck // the calls are the subject

	// Assert
	want := []string{"diskutil list", "hdiutil info", "sysctl -n vm.loadavg"}
	if strings.Join(h.calls, "\n") != strings.Join(want, "\n") {
		t.Fatalf("calls = %q, want only the two probes and the host's state", h.calls)
	}
}

func TestSetupProbesWithinTheMeasuredBounds(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}}
	m := newManager(t, h)

	// Act
	m.Setup(16) //nolint:errcheck // the bounds are the subject

	// Assert
	if h.bounds["diskutil list"] != ListBound || h.bounds["hdiutil info"] != InfoBound {
		t.Fatalf("bounds = %v, want diskutil list %s and hdiutil info %s", h.bounds, ListBound, InfoBound)
	}
}

func TestSetupTimesTheProbes(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if trouble.ListTook != time.Second || trouble.InfoTook != time.Second {
		t.Fatalf("took list %s, info %s; want each probe timed by the clock (1s)", trouble.ListTook, trouble.InfoTook)
	}
}

func TestSetupLogsTheProbesAtInfo(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager answered diskutil list in 1s; the disk manager answered hdiutil info in 1s") {
		t.Fatalf("log = %q, want the probes' timings", log.lines)
	}
}

func TestSetupAnswersALeakAsTrouble(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-7583")}
	m := newManager(t, h)

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || v != nil || trouble == nil || len(trouble.Leaks) != 1 || trouble.Leaks[0].Device != "/dev/disk4" {
		t.Fatalf("Setup = %v, %+v, %v; want the leak as trouble", v, trouble, err)
	}
}

func TestSetupReclaimsADeadRunsDiskBeforeTheLeakCheck(t *testing.T) {
	// Arrange: pid 7 died with its disk recorded; the reclaim takes it down.
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || trouble != nil {
		t.Fatalf("Setup = %+v, %v; want the reclaimed disk not taken for a leak", trouble, err)
	}
	v.Release() //nolint:errcheck // cleanup
}

func TestSetupLogsAFailedReclaimAtError(t *testing.T) {
	// Arrange: the dead run's disk will not detach.
	h := &fakeHost{fail: map[string]bool{"hdiutil detach": true}}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	h.info = ramInfo("/dev/disk7=" + mount)
	deadRecord(t, m, "7", "/dev/disk7", mount)
	log := m.Log.(*fakeLog)

	// Act
	m.Setup(16) //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("ERROR reclaiming a dead run's RAM disk failed: detach the dead run's RAM disk /dev/disk7") {
		t.Fatalf("log = %q, want the failed reclaim at ERROR", log.lines)
	}
}

func TestSetupAnswersAStuckRegistryLockAsTrouble(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || v != nil || trouble == nil || trouble.Lock == nil {
		t.Fatalf("Setup = %v, %+v, %v; want the stuck lock as trouble", v, trouble, err)
	}
}

func TestSetupWaitsOnAStuckRegistryLockOnce(t *testing.T) {
	// Arrange: a dead run's record makes the reclaim wait on the lock first.
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	m.LockBound = 20 * time.Millisecond
	deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))
	holdRegistryLock(t, m)

	// Act
	m.Setup(16) //nolint:errcheck // the calls are the subject

	// Assert
	lsofs := 0
	for _, call := range h.calls {
		if strings.HasPrefix(call, "lsof") {
			lsofs++
		}
	}
	if lsofs != 1 || attachCalled(h.calls) {
		t.Fatalf("calls = %q, want one timed-out wait and no attach", h.calls)
	}
}

func TestHostSnapshot(t *testing.T) {
	procs := []proctable.Entry{
		proctable.Entry{Pid: 357, State: 'S', Comm: "diskarbitrationd", Argv: []string{"/usr/libexec/diskarbitrationd"}},
		{Pid: 901, State: 'S', Comm: "hdiutil", Argv: []string{"/usr/bin/hdiutil", "detach", "/dev/disk4"}, Env: []string{"HOME=/h"}},
		{Pid: 902, State: 'R', Comm: "zsh", Argv: []string{"/bin/zsh"}},
		{Pid: 903, State: 'S', Comm: "diskimagesiod"},
	}
	tests := []struct {
		name string
		host *fakeHost
		want string
	}{
		{
			name: "names the load and every disk process",
			host: &fakeHost{load: "{ 28.72 21.45 17.25 }\n", procs: procs},
			want: "load average 28.72 21.45 17.25\ndisk process 357 S - /usr/libexec/diskarbitrationd\ndisk process 901 S - /usr/bin/hdiutil detach /dev/disk4 HOME=/h\ndisk process 903 S - (diskimagesiod)\n",
		},
		{
			name: "says no disk process ran",
			host: &fakeHost{load: "{ 4.50 13.70 15.31 }", procs: procs[2:3]},
			want: "load average 4.50 13.70 15.31\nno disk process running\n",
		},
		{
			name: "names a failed sysctl in place of the load",
			host: &fakeHost{loadErr: errors.New("sysctl did not answer within 500ms"), procs: procs},
			want: "load average unknown, sysctl failed: sysctl did not answer within 500ms\ndisk process 357",
		},
		{
			name: "names a failed process table in place of the processes",
			host: &fakeHost{load: "{ 1.00 1.00 1.00 }", procsErr: errors.New("proc_info listpids: EINVAL")},
			want: "load average 1.00 1.00 1.00\ndisk processes unknown, the process table failed: proc_info listpids: EINVAL\n",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m := newManager(t, tc.host)

			// Act
			got := m.hostSnapshot()

			// Assert
			if !strings.HasPrefix(got, tc.want) {
				t.Fatalf("snapshot =\n%q\nwant it to start\n%q", got, tc.want)
			}
		})
	}
}

func TestElapsed(t *testing.T) {
	now := time.Date(2026, 10, 23, 17, 0, 0, 0, time.UTC)
	cases := []struct {
		name  string
		start time.Time
		want  string
	}{
		{name: "an unknown start", want: "-"},
		{name: "minutes and seconds", start: now.Add(-(51*time.Minute + 8*time.Second)), want: "51:08"},
		{name: "hours", start: now.Add(-(2*time.Hour + 3*time.Minute + 4*time.Second)), want: "02:03:04"},
		{name: "days", start: now.Add(-(4*24*time.Hour + 23*time.Hour + 43*time.Minute + 20*time.Second)), want: "04-23:43:20"},
		{name: "a start after now reads zero", start: now.Add(time.Second), want: "00:00"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := elapsed(now, tc.start)

			// Assert
			if got != tc.want {
				t.Fatalf("elapsed = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestHostSnapshotReadsWithinTheHostBound(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	m.hostSnapshot()

	// Assert
	if h.bounds["sysctl -n vm.loadavg"] != HostBound {
		t.Fatalf("bounds = %v, want sysctl within HostBound %s", h.bounds, HostBound)
	}
	for call := range h.bounds {
		if strings.HasPrefix(call, "ps ") {
			t.Fatalf("the snapshot ran %q; the process table is never read through ps", call)
		}
	}
}

func TestSetupReadsTheHostWhenAProbeDoesNotAnswer(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}, load: "{ 28.72 21.45 17.25 }", procs: []proctable.Entry{proctable.Entry{Pid: 357, State: 'S', Comm: "diskarbitrationd", Argv: []string{"/usr/libexec/diskarbitrationd"}}}}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if want := "load average 28.72 21.45 17.25\ndisk process 357 S - /usr/libexec/diskarbitrationd\n"; trouble == nil || trouble.Host != want {
		t.Fatalf("trouble = %+v, want the host's state %q", trouble, want)
	}
}

func TestSetupLogsTheHostWhenAProbeDoesNotAnswer(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}, load: "{ 28.72 21.45 17.25 }", procs: []proctable.Entry{proctable.Entry{Pid: 357, State: 'S', Comm: "diskarbitrationd", Argv: []string{"/usr/libexec/diskarbitrationd"}}}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	m.Setup(16) //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager did not answer; the host: load average 28.72 21.45 17.25, disk process 357 S - /usr/libexec/diskarbitrationd") {
		t.Fatalf("log = %q, want the host's state at INFO", log.lines)
	}
}

func TestSetupReadsNoHostStateWhenBothProbesAnswer(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	for _, call := range h.calls {
		if strings.HasPrefix(call, "sysctl") {
			t.Fatalf("calls = %q, want no read of the host's state", h.calls)
		}
	}
	if h.procsRead != 0 {
		t.Fatalf("the process table was read %d times, want none", h.procsRead)
	}
}

func TestSetupStampsTheCheck(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if want := time.Date(2026, 10, 12, 0, 0, 1, 0, time.UTC); !trouble.At.Equal(want) {
		t.Fatalf("At = %s, want the clock's first reading %s", trouble.At, want)
	}
}

func TestHostLine(t *testing.T) {
	tests := []struct {
		name string
		host string
		want string
	}{
		{name: "joins the facts", host: "load average 1 2 3\nno disk process running\n", want: "load average 1 2 3, no disk process running"},
		{name: "keeps one fact as is", host: "load average 1 2 3\n", want: "load average 1 2 3"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := hostLine(tc.host)

			// Assert
			if got != tc.want {
				t.Fatalf("hostLine = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestSetupAsksBothProbesThroughProbe(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("leak.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	setup := string(src)[strings.Index(string(src), "func (m Manager) Setup("):]
	setup = setup[:strings.Index(setup, "\n}\n")]

	// Assert
	if strings.Count(setup, "m.probe(t, ") != 2 || strings.Contains(setup, "m.Watch(") || strings.Contains(setup, ".Sub(") {
		t.Fatalf("Setup asks or times a probe by hand; want both through probe")
	}
}

func TestProbeBoundsCoverTwiceTheSlowestHealthyRead(t *testing.T) {
	// Arrange: the slowest healthy `diskutil info` the runs recorded at their
	// own priority (2026-10-12 and 13).
	const slowestHealthyRead = 471 * time.Millisecond

	// Act
	bounds := map[string]time.Duration{"InfoBound": InfoBound, "ListBound": ListBound}

	// Assert
	for name, bound := range bounds {
		if bound < 2*slowestHealthyRead {
			t.Errorf("%s = %s, want at least twice the slowest healthy read %s", name, bound, slowestHealthyRead)
		}
	}
}

func TestLockBoundCoversTwiceTheLongestHealthyAcquireHold(t *testing.T) {
	// Arrange: an acquire's hold is its listing and its attach, at their
	// slowest healthy times recorded by the runs (2026-10-12 and 13).
	const longestHold = 471*time.Millisecond + 332*time.Millisecond

	// Act
	bound := LockBound

	// Assert
	if bound < 2*longestHold {
		t.Fatalf("LockBound = %s, want at least twice the longest healthy hold %s", bound, longestHold)
	}
}

func TestProbe(t *testing.T) {
	never := errors.New("hdiutil info did not answer within 1m0s")
	tests := []struct {
		name     string
		late     *Late
		fail     bool
		wantOut  string
		wantTook time.Duration
		wantErr  bool
	}{
		{name: "an answer in time", wantTook: time.Second},
		{name: "a failure in time", fail: true, wantOut: "refused", wantTook: time.Second, wantErr: true},
		{name: "an answer past the bound", late: &Late{Out: "framework : 1\n", Took: 1156 * time.Millisecond}, wantOut: "framework : 1\n", wantTook: 1156 * time.Millisecond},
		{name: "no answer at CommandBound", late: &Late{Took: CommandBound, Err: never}, wantTook: CommandBound, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := &fakeHost{fail: map[string]bool{}}
			if tc.fail {
				h.fail["hdiutil info"] = true
			}
			if tc.late != nil {
				late := make(chan Late, 1)
				late <- *tc.late
				h.late = map[string]chan Late{"hdiutil info": late}
			}
			m := newManager(t, h)

			// Act
			out, took, err := m.probe(&Trouble{At: m.Now()}, InfoBound, "hdiutil", "info")

			// Assert
			if out != tc.wantOut || took != tc.wantTook || (err != nil) != tc.wantErr {
				t.Fatalf("probe = %q, %s, %v; want %q, %s, error %t", out, took, err, tc.wantOut, tc.wantTook, tc.wantErr)
			}
		})
	}
}

// lateHost is a host whose probes of each call in took answer past their
// bounds, after that long; a call also in never fails at CommandBound
// instead.
func lateHost(took map[string]time.Duration, never ...string) *fakeHost {
	h := &fakeHost{late: map[string]chan Late{}, load: "{ 14.99 13.71 13.41 }", procs: []proctable.Entry{{Pid: 358, State: 'S', Comm: "diskarbitrationd", Argv: []string{"/usr/libexec/diskarbitrationd"}}}}
	for call, d := range took {
		late := make(chan Late, 1)
		late <- Late{Took: d}
		h.late[call] = late
	}
	for _, call := range never {
		late := make(chan Late, 1)
		late <- Late{Took: CommandBound, Err: errors.New(call + " did not answer within 1m0s")}
		h.late[call] = late
	}
	return h
}

func TestSetupAttachesWhenAProbeAnswersPastItsBound(t *testing.T) {
	// Arrange: the refusal of 2026-10-20, whose diskutil list answered after
	// 1.156s against its 1s bound at load 15.
	h := lateHost(map[string]time.Duration{"diskutil list": 1156 * time.Millisecond})
	m := newManager(t, h)

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || trouble != nil || v == nil {
		t.Fatalf("Setup = %v, %+v, %v; want a volume: a late answer is a slow disk manager, not a wedged one", v, trouble, err)
	}
	v.Release() //nolint:errcheck // cleanup
}

func TestSetupLogsAProbeThatAnswersLate(t *testing.T) {
	// Arrange
	h := lateHost(map[string]time.Duration{"diskutil list": 1156 * time.Millisecond})
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager answered diskutil list late, after 1.156s, so it was slow, not wedged") {
		t.Fatalf("log = %q, want the late answer at INFO", log.snapshot())
	}
}

func TestSetupLogsTheHostWhileAProbeIsLate(t *testing.T) {
	// Arrange
	h := lateHost(map[string]time.Duration{"diskutil list": 1156 * time.Millisecond})
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager had not answered diskutil list within 1s, so it is waited for to its own answer; the host: load average 14.99 13.71 13.41, disk process 358 S - /usr/libexec/diskarbitrationd") {
		t.Fatalf("log = %q, want the host's state while the probe was late", log.snapshot())
	}
}

func TestSetupReadsTheHostOnceWhenBothProbesAreLate(t *testing.T) {
	// Arrange
	h := lateHost(map[string]time.Duration{"diskutil list": 2 * time.Second, "hdiutil info": 3 * time.Second})
	m := newManager(t, h)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	reads := 0
	for _, call := range h.calls {
		if strings.HasPrefix(call, "sysctl") {
			reads++
		}
	}
	if reads != 1 {
		t.Fatalf("calls = %q, want the host read once", h.calls)
	}
}

func TestSetupRefusesAProbeThatNeverAnswers(t *testing.T) {
	// Arrange
	h := lateHost(nil, "hdiutil info")
	m := newManager(t, h)

	// Act
	v, trouble, err := m.Setup(16)

	// Assert
	if err != nil || v != nil || trouble == nil || trouble.InfoErr == nil || trouble.InfoTook != CommandBound {
		t.Fatalf("Setup = %v, %+v, %v; want hdiutil info unanswered at CommandBound as trouble", v, trouble, err)
	}
}

func TestSetupKeepsTheHostOfAProbeThatNeverAnswers(t *testing.T) {
	// Arrange
	h := lateHost(nil, "hdiutil info")
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if want := "load average 14.99 13.71 13.41\ndisk process 358 S - /usr/libexec/diskarbitrationd\n"; trouble == nil || trouble.Host != want {
		t.Fatalf("trouble = %+v, want the host's state read while the probe was late %q", trouble, want)
	}
}

func TestSetupLogsAProbeThatNeverAnswers(t *testing.T) {
	// Arrange
	h := lateHost(nil, "hdiutil info")
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	m.Setup(16) //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager never answered hdiutil info, which failed after 1m0s: hdiutil info did not answer within 1m0s") {
		t.Fatalf("log = %q, want the unanswered probe at INFO; the run's refusal is its one ERROR", log.snapshot())
	}
}

func TestSetupLogsAStrayMountPointItRemoved(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	m.Reap = func(reap.Members) (reap.Result, error) { return killedN(12), nil }
	log := m.Log.(*fakeLog)

	// Act
	v, _, _ := m.Setup(16)

	// Assert
	t.Cleanup(func() { v.Release() })
	if !log.has("INFO removed the mount point " + dir + " left by dead run pid 28386, which no record named (killed 12 leftover processes)") {
		t.Fatalf("log = %q, want the stray's removal at INFO", log.snapshot())
	}
}

func TestAcquireTakesADeviceALiveRecordNamesAsTheLiveRunsBesideADeadRecord(t *testing.T) {
	// Arrange: a dead record a failed reclaim left names /dev/disk4, which a
	// live run attached since; "9.lock" sorts after the live "35768.lock".
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-35768")}
	m := newManager(t, h)
	liveRecord(t, m, "35768", "/dev/disk4", "/tmp/artr-35768")
	deadRecord(t, m, "9", "/dev/disk4", "/tmp/artr-9")

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire = %v, want the live run's disk taken as no leak", err)
	}
	v.Release() //nolint:errcheck // cleanup
}
