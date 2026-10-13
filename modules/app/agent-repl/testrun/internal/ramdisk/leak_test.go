package ramdisk

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
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
			name:    "a disk manager that did not answer",
			trouble: Trouble{ListErr: errors.New("timed out"), ListTook: 100 * time.Millisecond, InfoTook: 9 * time.Millisecond},
			want:    "the disk manager answered hdiutil info in 9ms; the disk manager did not answer diskutil list after 100ms: timed out",
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
	if want := []string{"diskutil list", "hdiutil info"}; strings.Join(h.calls, "\n") != strings.Join(want, "\n") {
		t.Fatalf("calls = %q, want only the two probes", h.calls)
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
	if !log.has("INFO RAM disk check: the disk manager answered diskutil list in 1s; the disk manager answered hdiutil info in 1s") {
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
