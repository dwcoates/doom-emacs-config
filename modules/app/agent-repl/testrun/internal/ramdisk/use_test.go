package ramdisk

import (
	"errors"
	"path/filepath"
	"strings"
	"testing"
	"time"
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
