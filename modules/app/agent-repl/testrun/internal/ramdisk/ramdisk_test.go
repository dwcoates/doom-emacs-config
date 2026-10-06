package ramdisk

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

// fakeHost is a scripted hdiutil/diskutil: it records every call and answers
// from fail (a command prefix that fails) and info (what `hdiutil info` says).
type fakeHost struct {
	calls []string
	fail  map[string]bool
	info  string
	mount string
}

func (h *fakeHost) run(name string, args ...string) (string, error) {
	call := name + " " + strings.Join(args, " ")
	h.calls = append(h.calls, call)
	for prefix := range h.fail {
		if strings.HasPrefix(call, prefix) {
			return "refused", errors.New("refused: " + call)
		}
	}
	switch {
	case strings.HasPrefix(call, "hdiutil attach"):
		return "/dev/disk9          \t\n", nil
	case strings.HasPrefix(call, "diskutil info"):
		return "   Device Identifier:         disk10s1\n   Mount Point: /Volumes/x\n", nil
	case strings.HasPrefix(call, "hdiutil info"):
		return h.info, nil
	case strings.HasPrefix(call, "hdiutil detach"):
		// The real detach unmounts the volume, emptying the mount point.
		if h.mount != "" {
			os.Remove(filepath.Join(h.mount, ".metadata_never_index"))
		}
	}
	return "", nil
}

func newManager(t *testing.T, h *fakeHost) Manager {
	t.Helper()
	parent := t.TempDir()
	h.mount = filepath.Join(parent, "artr-4242")
	return Manager{
		Run: h.run, Registry: filepath.Join(t.TempDir(), "registry"), MountParent: parent, Pid: 4242,
		KillUnder: func(string) (int, error) { return 0, nil },
	}
}

func TestAcquireAttachesFormatsAndMountsUnderTheMountParent(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(func() { v.Release() })
	want := []string{
		"hdiutil attach -nomount ram://32768",
		"diskutil eraseDisk APFS artr-4242 /dev/disk9",
		"diskutil info /Volumes/artr-4242",
		"diskutil unmount disk10s1",
		"diskutil mount -mountPoint " + h.mount + " disk10s1",
	}
	if strings.Join(h.calls, "\n") != strings.Join(want, "\n") {
		t.Fatalf("calls =\n%s\nwant\n%s", strings.Join(h.calls, "\n"), strings.Join(want, "\n"))
	}
	if v.Device != "/dev/disk9" || v.Mount != h.mount {
		t.Fatalf("volume = %s at %s, want /dev/disk9 at %s", v.Device, v.Mount, h.mount)
	}
}

func TestAcquireRecordsTheDeviceAndMount(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(func() { v.Release() })
	raw, err := os.ReadFile(filepath.Join(m.Registry, "4242.lock"))
	if err != nil {
		t.Fatalf("read the record: %v", err)
	}
	if got := string(raw); got != "device=/dev/disk9\nmount="+h.mount+"\n" {
		t.Fatalf("record = %q", got)
	}
}

func TestAcquireThatCannotAttachLeavesNothingBehind(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil attach": true}}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil {
		t.Fatal("Acquire succeeded with a refused attach")
	}
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); !os.IsNotExist(statErr) {
		t.Fatalf("the record survives a failed acquire (stat err = %v)", statErr)
	}
}

func TestAcquireThatCannotFormatDetachesTheDevice(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil eraseDisk": true}}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil {
		t.Fatal("Acquire succeeded with a refused format")
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach /dev/disk9" {
		t.Fatalf("last call = %q, want the device detached", last)
	}
}

func TestReleaseDetachesAndRemovesTheMountPointAndRecord(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}

	// Act
	err = v.Release()

	// Assert
	if err != nil {
		t.Fatalf("Release: %v", err)
	}
	for _, path := range []string{h.mount, filepath.Join(m.Registry, "4242.lock")} {
		if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
			t.Fatalf("%s survives the release (stat err = %v)", path, statErr)
		}
	}
}

func TestReleaseForcesARefusedDetachAndSaysSo(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach /dev": true}

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "only when forced") {
		t.Fatalf("Release = %v, want the forced detach reported", err)
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach -force /dev/disk9" {
		t.Fatalf("last call = %q, want a forced detach", last)
	}
}

const attachedInfo = "framework : 1\n================================================\nimage-path      : ram://2048\nblockcount      : 2048\n/dev/disk7\t\t\n"

// deadRecord writes a record nobody holds the lock of.
func deadRecord(t *testing.T, m Manager, pid, device, mount string) string {
	t.Helper()
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(m.Registry, pid+".lock")
	if err := os.WriteFile(path, []byte("device="+device+"\nmount="+mount+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestReclaimTakesDownADeadRunsRAMDisk(t *testing.T) {
	// Arrange
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	var killedUnder string
	m.KillUnder = func(dir string) (int, error) { killedUnder = dir; return 2, nil }
	mount := filepath.Join(t.TempDir(), "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk7", mount)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil {
		t.Fatalf("Reclaim: %v", err)
	}
	if len(done) != 1 || !done[0].Attached || done[0].Killed != 2 || killedUnder != mount {
		t.Fatalf("reclaimed = %+v (killed under %q), want the attached disk of pid 7 with 2 kills under %s", done, killedUnder, mount)
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach -force /dev/disk7" {
		t.Fatalf("last call = %q, want the dead disk force-detached", last)
	}
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
}

func TestReclaimLeavesALiveRunAlone(t *testing.T) {
	// Arrange
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	record := deadRecord(t, m, "7", "/dev/disk7", "/tmp/artr-7")
	held, err := os.OpenFile(record, os.O_RDWR, 0)
	if err != nil {
		t.Fatal(err)
	}
	defer held.Close()
	if err := syscall.Flock(int(held.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatal(err)
	}

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 0 || len(h.calls) != 0 {
		t.Fatalf("Reclaim = %+v, %v with calls %v; want a live run left alone", done, err, h.calls)
	}
}

func TestReclaimAfterARebootRemovesOnlyTheRecord(t *testing.T) {
	// Arrange: the device is no RAM disk any more.
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	record := deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || done[0].Attached {
		t.Fatalf("Reclaim = %+v, %v; want the record reclaimed with nothing detached", done, err)
	}
	for _, call := range h.calls {
		if strings.HasPrefix(call, "hdiutil detach") {
			t.Fatalf("detached %q, a device that is no longer a RAM disk", call)
		}
	}
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the stale record survives (stat err = %v)", statErr)
	}
}

func TestSizeFromEnv(t *testing.T) {
	tests := []struct {
		name    string
		raw     string
		want    int
		wantErr bool
	}{
		{name: "unset is the default", raw: "", want: DefaultSizeMiB},
		{name: "a positive size is honored", raw: "512", want: 512},
		{name: "zero is refused", raw: "0", wantErr: true},
		{name: "a non-number is refused", raw: "lots", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(string) string { return tc.raw }

			// Act
			got, err := SizeFromEnv(getenv)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("SizeFromEnv = %d, %v; want %d (error %v)", got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestIsAttachedRAMDisk(t *testing.T) {
	tests := []struct {
		name   string
		info   string
		device string
		want   bool
	}{
		{name: "a listed RAM disk", info: attachedInfo, device: "/dev/disk7", want: true},
		{name: "another device", info: attachedInfo, device: "/dev/disk70", want: false},
		{name: "a file-backed image", info: strings.Replace(attachedInfo, "ram://2048", "/Users/x/a.dmg", 1), device: "/dev/disk7", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := isAttachedRAMDisk(tc.info, tc.device)

			// Assert
			if got != tc.want {
				t.Fatalf("isAttachedRAMDisk = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestUnitPrefixRunsUnitsAtTheUtilityTier(t *testing.T) {
	// Act
	got := strings.Join(UnitPrefix(), " ")

	// Assert
	if got != "/usr/sbin/taskpolicy -d utility" {
		t.Fatalf("UnitPrefix = %q", got)
	}
}
