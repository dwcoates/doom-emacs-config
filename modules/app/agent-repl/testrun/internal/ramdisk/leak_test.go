package ramdisk

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

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
			trouble: Trouble{InfoErr: errors.New("timed out"), Host: "load average 28.7 21.4 17.2\ndisk process 357 Ss 51:08 /usr/libexec/diskarbitrationd\n"},
			want:    "the disk manager did not answer hdiutil info after 0s: timed out; the disk manager answered diskutil list in 0s; the host then: load average 28.7 21.4 17.2, disk process 357 Ss 51:08 /usr/libexec/diskarbitrationd",
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
	want := []string{"diskutil list", "hdiutil info", "sysctl -n vm.loadavg", "ps -axo pid=,stat=,etime=,args="}
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
	h := &fakeHost{info: attachedInfo, fail: map[string]bool{"hdiutil detach": true}}
	m := newManager(t, h)
	deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))
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
	ps := "  357 Ss   51:08 /usr/libexec/diskarbitrationd\n" +
		"  901 S     0:01 /usr/bin/hdiutil detach /dev/disk4\n" +
		"  902 R     0:00 /bin/zsh\n"
	tests := []struct {
		name string
		host *fakeHost
		want string
	}{
		{
			name: "names the load and every disk process",
			host: &fakeHost{load: "{ 28.72 21.45 17.25 }\n", ps: ps},
			want: "load average 28.72 21.45 17.25\ndisk process 357 Ss 51:08 /usr/libexec/diskarbitrationd\ndisk process 901 S 0:01 /usr/bin/hdiutil detach /dev/disk4\n",
		},
		{
			name: "says no disk process ran",
			host: &fakeHost{load: "{ 4.50 13.70 15.31 }", ps: "  902 R     0:00 /bin/zsh\n"},
			want: "load average 4.50 13.70 15.31\nno disk process running\n",
		},
		{
			name: "names a failed sysctl in place of the load",
			host: &fakeHost{loadErr: errors.New("sysctl did not answer within 500ms"), ps: ps},
			want: "load average unknown, sysctl failed: sysctl did not answer within 500ms\ndisk process 357",
		},
		{
			name: "names a failed ps in place of the processes",
			host: &fakeHost{load: "{ 1.00 1.00 1.00 }", psErr: errors.New("ps did not answer within 500ms")},
			want: "load average 1.00 1.00 1.00\ndisk processes unknown, ps failed: ps did not answer within 500ms\n",
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

func TestHostSnapshotReadsWithinTheHostBound(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	m.hostSnapshot()

	// Assert
	if h.bounds["sysctl -n vm.loadavg"] != HostBound || h.bounds["ps -axo pid=,stat=,etime=,args="] != HostBound {
		t.Fatalf("bounds = %v, want sysctl and ps within HostBound %s", h.bounds, HostBound)
	}
}

func TestSetupReadsTheHostWhenAProbeDoesNotAnswer(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}, load: "{ 28.72 21.45 17.25 }", ps: "  357 Ss   51:08 /usr/libexec/diskarbitrationd\n"}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if want := "load average 28.72 21.45 17.25\ndisk process 357 Ss 51:08 /usr/libexec/diskarbitrationd\n"; trouble == nil || trouble.Host != want {
		t.Fatalf("trouble = %+v, want the host's state %q", trouble, want)
	}
}

func TestSetupLogsTheHostWhenAProbeDoesNotAnswer(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}, load: "{ 28.72 21.45 17.25 }", ps: "  357 Ss   51:08 /usr/libexec/diskarbitrationd\n"}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	m.Setup(16) //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("INFO RAM disk check: the host when the disk manager did not answer: load average 28.72 21.45 17.25, disk process 357 Ss 51:08 /usr/libexec/diskarbitrationd") {
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
		if strings.HasPrefix(call, "sysctl") || strings.HasPrefix(call, "ps ") {
			t.Fatalf("calls = %q, want no read of the host's state", h.calls)
		}
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

func TestTimeProbe(t *testing.T) {
	failing := errors.New("did not answer")
	tests := []struct {
		name string
		out  string
		err  error
	}{
		{name: "an answer", out: "framework : 1\n"},
		{name: "a failure", out: "partial", err: failing},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m := newManager(t, &fakeHost{})
			start := m.Now()

			// Act
			out, took, err := m.timeProbe(start, func() (string, error) { return tc.out, tc.err })

			// Assert
			if out != tc.out || !errors.Is(err, tc.err) || took != time.Second {
				t.Fatalf("timeProbe = %q, %v, %s; want %q, %v, 1s", out, err, took, tc.out, tc.err)
			}
		})
	}
}

func TestSetupTimesBothProbesThroughTimeProbe(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("leak.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	setup := string(src)[strings.Index(string(src), "func (m Manager) Setup("):]
	setup = setup[:strings.Index(setup, "\n}\n")]

	// Assert
	if strings.Count(setup, "m.timeProbe(") != 2 || strings.Contains(setup, ".Sub(start)") {
		t.Fatalf("Setup times its probes by hand; want both through timeProbe")
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

func TestSetupLogsAProbeThatAnswersLate(t *testing.T) {
	// Arrange
	late := make(chan Late, 1)
	h := &fakeHost{late: map[string]chan Late{"diskutil list": late}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)
	m.Setup(16) //nolint:errcheck // the late answer is the subject

	// Act
	late <- Late{Took: 247 * time.Millisecond}

	// Assert
	log.waitFor(t, "INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager answered diskutil list late, after 247ms, so it was slow, not wedged")
}

func TestSetupLogsAProbeThatNeverAnswersAtError(t *testing.T) {
	// Arrange
	late := make(chan Late, 1)
	h := &fakeHost{late: map[string]chan Late{"hdiutil info": late}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)
	m.Setup(16) //nolint:errcheck // the late failure is the subject

	// Act
	late <- Late{Took: CommandBound, Err: errors.New("hdiutil info did not answer within 1m0s")}

	// Assert
	log.waitFor(t, "ERROR RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager never answered hdiutil info, which failed after 1m0s: hdiutil info did not answer within 1m0s")
}

func TestSetupWatchesEveryProbeThatMissedItsBound(t *testing.T) {
	// Arrange
	h := &fakeHost{late: map[string]chan Late{"diskutil list": make(chan Late), "hdiutil info": make(chan Late)}}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if trouble == nil || len(trouble.late) != 2 || trouble.late[0].call != "diskutil list" || trouble.late[1].call != "hdiutil info" {
		t.Fatalf("trouble = %+v, want both probes watched", trouble)
	}
}

func TestSetupWatchesNoProbeThatAnswered(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil list": true}}
	m := newManager(t, h)

	// Act
	_, trouble, _ := m.Setup(16)

	// Assert
	if trouble == nil || len(trouble.late) != 0 {
		t.Fatalf("trouble = %+v, want no probe watched: the failed one answered", trouble)
	}
}

func TestSettleProbesRecordsAProbeStillUnanswered(t *testing.T) {
	// Arrange
	h := &fakeHost{late: map[string]chan Late{"diskutil list": make(chan Late)}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)
	_, trouble, _ := m.Setup(16)

	// Act
	m.SettleProbes(trouble)

	// Assert
	if !log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager had still not answered diskutil list when the run ended, ") {
		t.Fatalf("log = %q, want the unanswered probe recorded", log.snapshot())
	}
}

func TestSettleProbesKeepsAnOutcomeAfterTheRunFromTheLog(t *testing.T) {
	// Arrange
	late := make(chan Late)
	h := &fakeHost{late: map[string]chan Late{"diskutil list": late}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)
	_, trouble, _ := m.Setup(16)
	m.SettleProbes(trouble)

	// Act: the unbuffered send returns once the watch has taken the outcome.
	late <- Late{Took: time.Second}

	// Assert
	if log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager answered diskutil list late") {
		t.Fatalf("log = %q, want an outcome after the run's end left out", log.snapshot())
	}
}

func TestSettleProbesLeavesALoggedOutcomeAlone(t *testing.T) {
	// Arrange
	late := make(chan Late, 1)
	h := &fakeHost{late: map[string]chan Late{"diskutil list": late}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)
	_, trouble, _ := m.Setup(16)
	late <- Late{Took: time.Second}
	log.waitFor(t, "INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager answered diskutil list late")

	// Act
	m.SettleProbes(trouble)

	// Assert
	if log.has("INFO RAM disk check at 2026-10-12T00:00:01.000Z: the disk manager had still not answered") {
		t.Fatalf("log = %q, want the logged outcome to stand alone", log.snapshot())
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
