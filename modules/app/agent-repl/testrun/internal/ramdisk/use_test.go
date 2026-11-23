package ramdisk

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/seats"
)

// useVolume is a volume whose Used answers from answers, one per call, the
// last repeating.
func useVolume(answers ...func() (uint64, error)) (*Volume, *fakeLog) {
	log := &fakeLog{}
	calls := 0
	m := Manager{Log: log, Used: func(string) (uint64, error) {
		a := answers[min(calls, len(answers)-1)]
		calls++
		return a()
	}}
	return &Volume{m: m, Device: "/dev/disk9", Mount: "/tmp/artr-1", SizeMiB: 8192}, log
}

func used(n uint64) func() (uint64, error) { return func() (uint64, error) { return n, nil } }

func TestStopUseLogsThePeak(t *testing.T) {
	// Arrange: WatchUse samples once at once; the hour-long tick never fires.
	v, log := useVolume(used(1536 << 20))
	v.WatchUse(time.Hour)
	v.sampleUse()
	v.m.Used = func(string) (uint64, error) { return 512 << 20, nil }
	v.sampleUse()

	// Act
	v.stopUse()

	// Assert
	if !log.has("INFO RAM disk use: peak 1536 MiB of 8192 MiB on /dev/disk9 (3 samples)") {
		t.Fatalf("log = %q, want the peak of the three samples", log.lines)
	}
}

func TestStopUseNamesFailedSamplesAtError(t *testing.T) {
	// Arrange
	v, log := useVolume(func() (uint64, error) { return 0, errors.New("statfs /tmp/artr-1: no such file") })
	v.WatchUse(time.Hour)
	v.m.Used = func(string) (uint64, error) { return 100 << 20, nil }
	v.sampleUse()

	// Act
	v.stopUse()

	// Assert
	if !log.has("INFO RAM disk use: peak 100 MiB of 8192 MiB on /dev/disk9 (1 samples)") ||
		!log.has("ERROR RAM disk use: 1 of 2 samples of /tmp/artr-1 failed, so the peak may be low; the first: statfs /tmp/artr-1: no such file") {
		t.Fatalf("log = %q, want the peak and the failed sample at ERROR", log.lines)
	}
}

func TestStopUseLogsOnce(t *testing.T) {
	// Arrange
	v, log := useVolume(used(1 << 20))
	v.WatchUse(time.Hour)

	// Act
	v.stopUse()
	v.stopUse()

	// Assert
	n := 0
	for _, l := range log.lines {
		if strings.Contains(l, "RAM disk use:") {
			n++
		}
	}
	if n != 1 {
		t.Fatalf("log = %q, want one use line", log.lines)
	}
}

func TestStopUseWithoutAWatchLogsNothing(t *testing.T) {
	// Arrange
	v, log := useVolume(used(1))

	// Act
	v.stopUse()

	// Assert
	if len(log.lines) != 0 {
		t.Fatalf("log = %q, want nothing from a volume never watched", log.lines)
	}
}

func TestMountUsed(t *testing.T) {
	cases := []struct {
		name    string
		path    func(t *testing.T) string
		wantErr string
	}{
		{name: "a mounted filesystem answers its use", path: func(t *testing.T) string { return t.TempDir() }},
		{name: "a missing path fails, naming it", path: func(t *testing.T) string { return filepath.Join(t.TempDir(), "gone") }, wantErr: "statfs "},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := MountUsed(tc.path(t))

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("MountUsed = %d, %v; want %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got == 0 {
				t.Fatalf("MountUsed = %d, %v; want some bytes in use", got, err)
			}
		})
	}
}

// seatedVolume is useVolume holding a seat of a two-seat pool in a temporary
// directory.
func seatedVolume(t *testing.T, answers ...func() (uint64, error)) (*Volume, *fakeLog, seats.Pool) {
	t.Helper()
	v, log := useVolume(answers...)
	p := seats.Pool{Dir: t.TempDir(), Prefix: "ram", N: 2, What: "RAM disk"}
	s, err := p.Admit(seats.Admission{
		Me:      seats.Holder{Pid: 4242, Workspace: "holder", Size: 4096},
		Check:   func([]seats.Holder) (bool, error) { return true, nil },
		Waiting: func(w seats.Wait) error { t.Fatalf("the holder waited: %+v", w); return nil },
		Recheck: func() <-chan time.Time { return nil },
	})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { s.Release() })
	v.seat = s
	return v, log, p
}

// heldUse is what the volume's seat published it has taken, as the next
// taker's admission check reads it.
func heldUse(t *testing.T, p seats.Pool) int {
	t.Helper()
	used := -1
	s, err := p.Admit(seats.Admission{
		Me: seats.Holder{Pid: 5353, Workspace: "next", Size: 4096},
		Check: func(held []seats.Holder) (bool, error) {
			used = held[0].Used
			return true, nil
		},
		Waiting: func(w seats.Wait) error { t.Fatalf("the next taker waited: %+v", w); return nil },
		Recheck: func() <-chan time.Time { return nil },
	})
	if err != nil {
		t.Fatal(err)
	}
	s.Release()
	return used
}

func TestASampledPeakIsPublishedInTheDisksSeat(t *testing.T) {
	// Arrange
	v, _, p := seatedVolume(t, used(1536<<20))
	v.use = &use{}

	// Act
	v.sampleUse()

	// Assert
	if got := heldUse(t, p); got != 1536 {
		t.Fatalf("published use = %d MiB, want 1536", got)
	}
}

func TestAPeakThatDidNotGrowIsNotPublishedAgain(t *testing.T) {
	// Arrange: the peak was published, and the seat's directory then refuses writes.
	v, log, p := seatedVolume(t, used(1536<<20))
	v.use = &use{}
	v.sampleUse()
	if err := os.Chmod(p.Dir, 0o555); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(p.Dir, 0o755) })
	v.m.Used = func(string) (uint64, error) { return 512 << 20, nil }

	// Act
	v.sampleUse()

	// Assert
	if v.use.publishFailures != 0 || len(log.lines) != 0 {
		t.Fatalf("failures = %d, log = %q, want no publish asked", v.use.publishFailures, log.lines)
	}
}

func TestAPublishThatFailsIsRecordedAtErrorOnceAndLeavesTheLastStanding(t *testing.T) {
	// Arrange
	v, log, p := seatedVolume(t, used(100<<20))
	v.use = &use{}
	v.sampleUse()
	if err := os.Chmod(p.Dir, 0o555); err != nil {
		t.Fatal(err)
	}
	v.m.Used = func(string) (uint64, error) { return 2048 << 20, nil }

	// Act
	v.sampleUse()
	v.m.Used = func(string) (uint64, error) { return 3000 << 20, nil }
	v.sampleUse()

	// Assert
	if err := os.Chmod(p.Dir, 0o755); err != nil {
		t.Fatal(err)
	}
	errors := 0
	for _, l := range log.lines {
		if strings.HasPrefix(l, "ERROR RAM disk use: could not publish the peak 2048 MiB of /dev/disk9 in its memory seat, so the next run's admission counts it at 100 MiB") {
			errors++
		}
	}
	if errors != 1 || v.use.publishFailures != 2 {
		t.Fatalf("log = %q, failures = %d; want the first failure at ERROR once, two counted", log.lines, v.use.publishFailures)
	}
	if got := heldUse(t, p); got != 100 {
		t.Fatalf("published use = %d MiB, want the last publish, 100", got)
	}
}

func TestStopUseCountsTheFailedPublishesAtError(t *testing.T) {
	// Arrange
	v, log := useVolume(used(0))
	v.WatchUse(time.Hour)
	v.use.publishFailures = 3

	// Act
	v.stopUse()

	// Assert
	if !log.has("ERROR RAM disk use: 3 publishes of the peak of /dev/disk9 in its memory seat failed") {
		t.Fatalf("log = %q, want the failed publishes counted at ERROR", log.lines)
	}
}
