package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

// fakeHost is a scripted hdiutil/diskutil: it records every call and answers
// from fail (a command prefix that fails) and info (what `hdiutil info` says).
type fakeHost struct {
	calls []string
	fail  map[string]bool
	info  string
	mount string
	// lsof is what `lsof` answers; lsofErr is its failure, if any.
	lsof    string
	lsofErr error
	// attached is set once `hdiutil attach` was called; from then on
	// `hdiutil info` answers infoAfterAttach (when set) and infoErrAfterAttach.
	attached           bool
	infoAfterAttach    string
	infoErrAfterAttach error
	// attachOut, when set, is what `hdiutil attach` answers.
	attachOut string
	// bounds are the bounds the probes ran under, by call.
	bounds map[string]time.Duration
}

func (h *fakeHost) run(name string, args ...string) (string, error) {
	call := name + " " + strings.Join(args, " ")
	h.calls = append(h.calls, call)
	for prefix := range h.fail {
		if strings.HasPrefix(call, prefix) {
			if strings.HasPrefix(call, "hdiutil attach") {
				h.attached = true
			}
			return "refused", errors.New("refused: " + call)
		}
	}
	switch {
	case strings.HasPrefix(call, "hdiutil attach"):
		h.attached = true
		if h.attachOut != "" {
			return h.attachOut, nil
		}
		return "/dev/disk9          \t\n", nil
	case strings.HasPrefix(call, "diskutil info"):
		return "   Device Identifier:         disk10s1\n   Mount Point: /Volumes/x\n", nil
	case strings.HasPrefix(call, "hdiutil info") && h.attached && (h.infoAfterAttach != "" || h.infoErrAfterAttach != nil):
		return h.infoAfterAttach, h.infoErrAfterAttach
	case strings.HasPrefix(call, "hdiutil info"):
		return h.info, nil
	case strings.HasPrefix(call, "lsof"):
		return h.lsof, h.lsofErr
	}
	return "", nil
}

func (h *fakeHost) probe(bound time.Duration, name string, args ...string) (string, error) {
	if h.bounds == nil {
		h.bounds = map[string]time.Duration{}
	}
	h.bounds[name+" "+strings.Join(args, " ")] = bound
	return h.run(name, args...)
}

// fakeLog records every line the Manager logs, "INFO " or "ERROR " first.
type fakeLog struct{ lines []string }

func (l *fakeLog) Infof(format string, args ...any) {
	l.lines = append(l.lines, "INFO "+fmt.Sprintf(format, args...))
}

func (l *fakeLog) Errorf(format string, args ...any) {
	l.lines = append(l.lines, "ERROR "+fmt.Sprintf(format, args...))
}

func (l *fakeLog) has(prefix string) bool {
	for _, line := range l.lines {
		if strings.HasPrefix(line, prefix) {
			return true
		}
	}
	return false
}

// tickingClock answers a time one second later on every call.
func tickingClock() func() time.Time {
	now := time.Date(2026, 10, 12, 0, 0, 0, 0, time.UTC)
	return func() time.Time {
		now = now.Add(time.Second)
		return now
	}
}

func newManager(t *testing.T, h *fakeHost) Manager {
	t.Helper()
	parent := t.TempDir()
	h.mount = filepath.Join(parent, "artr-4242")
	return Manager{
		Run: h.run, Registry: filepath.Join(t.TempDir(), "registry"), MountParent: parent, Pid: 4242,
		KillUnder: func(string) (int, error) { return 0, nil },
		Probe:     h.probe,
		Alive:     func(int) bool { return false },
		Log:       &fakeLog{},
		Now:       tickingClock(),
		LockBound: 10 * time.Second,
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
		"hdiutil info",
		"hdiutil attach -nomount ram://32768",
		"diskutil eraseDisk APFS artr-4242 /dev/disk9",
		"diskutil info /Volumes/artr-4242",
		"diskutil unmount disk10s1",
		"diskutil mount nobrowse -mountPoint " + h.mount + " disk10s1",
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

func TestReleaseNamesWhoHeldTheVolumeBeforeForcing(t *testing.T) {
	// Arrange
	h := &fakeHost{lsof: "COMMAND   PID USER   FD   TYPE DEVICE SIZE/OFF NODE NAME\ntail    77001 me    cwd    DIR   1,30      96    2 /tmp/artr-4242/x\ntail    77001 me    3r     REG   1,30       0    5 /tmp/artr-4242/x/f\n"}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach /dev": true}

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "held by: tail[77001])") {
		t.Fatalf("Release = %v, want the holder named once", err)
	}
}

func TestReleaseListsTheHoldersBeforeTheForcedDetach(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach /dev": true}

	// Act
	v.Release() //nolint:errcheck // the order of the calls is the subject

	// Assert
	tail := h.calls[len(h.calls)-2:]
	if !strings.HasPrefix(tail[0], "lsof -n -P +f -- ") || tail[1] != "hdiutil detach -force /dev/disk9" {
		t.Fatalf("last calls = %q, want lsof then the forced detach", tail)
	}
}

func TestReleaseSaysSoWhenLsofSeesNoHolder(t *testing.T) {
	// Arrange: lsof answers exit 1 and no output when nothing is open.
	h := &fakeHost{lsofErr: errors.New("exit status 1")}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach /dev": true}

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "held by: no process lsof could see") {
		t.Fatalf("Release = %v, want lsof's empty answer stated", err)
	}
}

func TestReleaseNamesAnLsofFailureInPlaceOfTheHolders(t *testing.T) {
	// Arrange
	h := &fakeHost{lsof: "lsof: illegal option", lsofErr: errors.New("exit status 2")}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach /dev": true}

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "unknown, lsof failed: exit status 2") {
		t.Fatalf("Release = %v, want lsof's failure named", err)
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

func TestReclaimIgnoresTheRegistryLock(t *testing.T) {
	// Arrange: a live run made the registry lock and holds its record.
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(func() { v.Release() })
	other := m
	other.Pid = 5151

	// Act
	done, err := other.Reclaim()

	// Assert
	if err != nil || len(done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want the registry lock read as no record at all", done, err)
	}
}

func TestReclaimLeavesARecordWhoseRunIsMidCreation(t *testing.T) {
	// Arrange: a run holds the registry lock, its record written and not
	// flocked yet -- the instant between Acquire's open and its flock.
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}
	unlock, err := m.lockRegistry()
	if err != nil {
		t.Fatalf("lockRegistry: %v", err)
	}
	record := filepath.Join(m.Registry, "7.lock")
	creating, err := os.OpenFile(record, os.O_CREATE|os.O_RDWR|os.O_TRUNC, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	defer creating.Close()
	reclaimer := m
	reclaimer.Pid = 5151
	type answer struct {
		done []Reclaimed
		err  error
	}
	answered := make(chan answer, 1)
	go func() {
		done, err := reclaimer.Reclaim()
		answered <- answer{done, err}
	}()

	// Act: the run takes its record's flock, then lets go of the registry.
	if err := syscall.Flock(int(creating.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatal(err)
	}
	if err := unlock(); err != nil {
		t.Fatal(err)
	}
	got := <-answered

	// Assert
	if got.err != nil || len(got.done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want the run mid-creation left alone", got.done, got.err)
	}
	if _, err := os.Stat(record); err != nil {
		t.Fatalf("the live run's record is gone: %v", err)
	}
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

func TestReclaimRemovesWhatADeadRunLeftUnderItsDetachedMountPoint(t *testing.T) {
	// Arrange: the disk is gone, but files the dead run wrote after it went
	// sit on the host's filesystem under the mount point.
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	if err := os.MkdirAll(filepath.Join(mount, "tr-1", "tu-2"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(mount, "tr-1", "tu-2", "store.db"), []byte("x"), 0o644); err != nil {
		t.Fatal(err)
	}
	deadRecord(t, m, "7", "/dev/disk7", mount)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 {
		t.Fatalf("Reclaim = %+v, %v; want the dead run reclaimed", done, err)
	}
	if _, statErr := os.Stat(mount); !os.IsNotExist(statErr) {
		t.Fatalf("the dead run's mount point survives (stat err = %v)", statErr)
	}
}

func TestReclaimRefusesARecordNamingSomeoneElsesDirectory(t *testing.T) {
	// Arrange: a record whose mount is not the one a run of its pid is given.
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	elsewhere := filepath.Join(t.TempDir(), "precious")
	if err := os.MkdirAll(elsewhere, 0o755); err != nil {
		t.Fatal(err)
	}
	deadRecord(t, m, "7", "/dev/disk7", elsewhere)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "no mount point a run of pid 7 is given") {
		t.Fatalf("Reclaim error = %v, want the record refused", err)
	}
	if _, statErr := os.Stat(elsewhere); statErr != nil {
		t.Fatalf("the directory a bad record named was touched (stat err = %v)", statErr)
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

func TestRunBoundedFailsACommandThatOutlivesItsBound(t *testing.T) {
	// Arrange
	start := time.Now()

	// Act
	_, err := runBounded(100*time.Millisecond, "sleep", "10")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer within") {
		t.Fatalf("runBounded(sleep 10) = %v, want the bound's failure", err)
	}
	if took := time.Since(start); took > 5*time.Second {
		t.Fatalf("runBounded took %s for a 100ms bound, want the command killed at the bound", took)
	}
}

func TestRunBoundedAnswersACommandThatFinishesInTime(t *testing.T) {
	// Act
	out, err := runBounded(10*time.Second, "echo", "attached")

	// Assert
	if err != nil || strings.TrimSpace(out) != "attached" {
		t.Fatalf("runBounded(echo attached) = %q, %v; want its output", out, err)
	}
}

func TestRunBoundedReportsAFailingCommandsOutput(t *testing.T) {
	// Act
	_, err := runBounded(10*time.Second, "sh", "-c", "echo refused >&2; exit 3")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "refused") || strings.Contains(err.Error(), "did not answer") {
		t.Fatalf("runBounded(exit 3) = %v, want the command's own failure with its output", err)
	}
}

// ramInfo is `hdiutil info` listing one RAM disk image per device given, each
// "<device>[=<mount>]".
func ramInfo(disks ...string) string {
	var b strings.Builder
	b.WriteString("framework : 1\n")
	for _, d := range disks {
		device, mount, _ := strings.Cut(d, "=")
		b.WriteString("================================================\nimage-path      : ram://32768\n")
		b.WriteString(device + "\tGUID_partition_scheme\t\n")
		if mount != "" {
			b.WriteString(device + "s1\t41504653-0000-11AA-AA11-00306543ECAC\t" + mount + "\n")
		}
	}
	return b.String()
}

func TestAcquireDetachesWhatAKilledAttachLeft(t *testing.T) {
	// Arrange: hdiutil is killed at its bound, but the kernel's attach lands.
	h := &fakeHost{fail: map[string]bool{"hdiutil attach": true}, infoAfterAttach: ramInfo("/dev/disk9")}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "attached /dev/disk9 anyway; it was detached") {
		t.Fatalf("Acquire = %v, want the stray disk named as detached", err)
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach -force /dev/disk9" {
		t.Fatalf("last call = %q, want the stray disk force-detached", last)
	}
}

func TestAcquireLeavesTheDisksListedBeforeAFailedAttachAttached(t *testing.T) {
	// Arrange: a live run's disk is attached before and after the failed attach.
	h := &fakeHost{fail: map[string]bool{"hdiutil attach": true}, info: ramInfo("/dev/disk5"), infoAfterAttach: ramInfo("/dev/disk5")}
	m := newManager(t, h)

	// Act
	m.Acquire(16) //nolint:errcheck // the calls are the subject

	// Assert
	for _, call := range h.calls {
		if strings.HasPrefix(call, "hdiutil detach") {
			t.Fatalf("called %q, want the disk attached before the failed attach left alone", call)
		}
	}
}

func TestAcquireNamesAFailedListingAfterAFailedAttach(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil attach": true}, infoErrAfterAttach: errors.New("hdiutil info did not answer within 50ms")}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "after a failed attach, to detach what it may have left: hdiutil info did not answer") {
		t.Fatalf("Acquire = %v, want the failed listing named", err)
	}
}

func TestAcquireDetachesWhatAnAttachAnsweringNoDeviceLeft(t *testing.T) {
	// Arrange
	h := &fakeHost{attachOut: "hdiutil: attach: interrupted\n", infoAfterAttach: ramInfo("/dev/disk9")}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "answered no device") {
		t.Fatalf("Acquire = %v, want the deviceless answer refused", err)
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach -force /dev/disk9" {
		t.Fatalf("last call = %q, want the stray disk force-detached", last)
	}
}

func TestAcquireAttachesNothingWhenTheImagesCannotBeListed(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}}
	m := newManager(t, h)

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the attached images before attaching") {
		t.Fatalf("Acquire = %v, want the failed listing named", err)
	}
	for _, call := range h.calls {
		if strings.HasPrefix(call, "hdiutil attach") {
			t.Fatalf("attached (%q) without a listing to check for leaks", call)
		}
	}
}

func TestAcquireListsTheImagesWithinTheInfoBound(t *testing.T) {
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
	if got := h.bounds["hdiutil info"]; got != InfoBound {
		t.Fatalf("hdiutil info ran under %s, want InfoBound %s", got, InfoBound)
	}
}

func TestAcquireLogsItsEdgesWithTimings(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	v, err := m.Acquire(16)

	// Assert
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	t.Cleanup(func() { v.Release() })
	want := []string{
		"INFO RAM disk acquire: begin, 16 MiB",
		"INFO RAM disk acquire: /dev/disk9 mounted at " + h.mount + " in 3s (attach 1s, format 1s, mount 1s)",
	}
	if strings.Join(log.lines, "\n") != strings.Join(want, "\n") {
		t.Fatalf("log =\n%s\nwant\n%s", strings.Join(log.lines, "\n"), strings.Join(want, "\n"))
	}
}

func TestAcquireLogsAFailedEdgeWithItsTiming(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"diskutil eraseDisk": true}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	m.Acquire(16) //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("INFO RAM disk acquire: failed after ") {
		t.Fatalf("log = %q, want the failed acquire's edge", log.lines)
	}
}

func TestReleaseKeepsTheRecordOfADiskThatWouldNotDetach(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach": true}

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "is kept, so the next run reclaims /dev/disk9") {
		t.Fatalf("Release = %v, want the kept record said", err)
	}
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); statErr != nil {
		t.Fatalf("the record of a disk still attached is gone: %v", statErr)
	}
}

func TestReleaseLeavesTheMountPointOfADiskThatWouldNotDetach(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach": true}

	// Act
	v.Release() //nolint:errcheck // the mount point is the subject

	// Assert
	if _, statErr := os.Stat(h.mount); statErr != nil {
		t.Fatalf("the mount point of a disk still attached is gone: %v", statErr)
	}
}

func TestReleaseLogsItsEdgesWithTimings(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	log := m.Log.(*fakeLog)
	log.lines = nil

	// Act
	if err := v.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Assert
	want := []string{
		"INFO RAM disk release: begin, /dev/disk9 at " + h.mount,
		"INFO RAM disk release: /dev/disk9 released in 1s",
	}
	if strings.Join(log.lines, "\n") != strings.Join(want, "\n") {
		t.Fatalf("log =\n%s\nwant\n%s", strings.Join(log.lines, "\n"), strings.Join(want, "\n"))
	}
}

func TestReleaseLogsAFailedEdgeWithItsTiming(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil detach": true}
	log := m.Log.(*fakeLog)

	// Act
	v.Release() //nolint:errcheck // the log is the subject

	// Assert
	if !log.has("INFO RAM disk release: failed after 1s") {
		t.Fatalf("log = %q, want the failed release's edge", log.lines)
	}
}

func TestReclaimListsTheImagesWithinTheInfoBound(t *testing.T) {
	// Arrange
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))

	// Act
	if _, err := m.Reclaim(); err != nil {
		t.Fatalf("Reclaim: %v", err)
	}

	// Assert
	if got := h.bounds["hdiutil info"]; got != InfoBound {
		t.Fatalf("hdiutil info ran under %s, want InfoBound %s", got, InfoBound)
	}
}

func TestParseImages(t *testing.T) {
	tests := []struct {
		name string
		info string
		want []image
	}{
		{name: "no image", info: "framework : 1\n", want: nil},
		{name: "an unmounted RAM disk", info: ramInfo("/dev/disk9"), want: []image{{Path: "ram://32768", Devices: []string{"/dev/disk9"}}}},
		{
			name: "a mounted RAM disk",
			info: ramInfo("/dev/disk4=/private/tmp/artr-63245"),
			want: []image{{Path: "ram://32768", Devices: []string{"/dev/disk4", "/dev/disk4s1"}, Mounts: []string{"/private/tmp/artr-63245"}}},
		},
		{name: "a mount point with a space", info: ramInfo("/dev/disk4=/Volumes/my disk"), want: []image{{Path: "ram://32768", Devices: []string{"/dev/disk4", "/dev/disk4s1"}, Mounts: []string{"/Volumes/my disk"}}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := parseImages(tc.info)

			// Assert
			if fmt.Sprint(got) != fmt.Sprint(tc.want) {
				t.Fatalf("parseImages = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestProcessAliveAnswersTrueForThisProcess(t *testing.T) {
	// Act
	got := ProcessAlive(os.Getpid())

	// Assert
	if !got {
		t.Fatal("ProcessAlive(self) = false")
	}
}

// holdRegistryLock takes the registry lock from outside the Manager, as
// another run would, and answers its release.
func holdRegistryLock(t *testing.T, m Manager) func() {
	t.Helper()
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}
	f, err := os.OpenFile(filepath.Join(m.Registry, registryLockName), os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatal(err)
	}
	released := false
	release := func() {
		if !released {
			released = true
			f.Close()
		}
	}
	t.Cleanup(release)
	return release
}

const lockLsof = "COMMAND   PID USER   FD   TYPE DEVICE SIZE/OFF NODE NAME\ntestrun  7583 me    3u    REG   1,30       0    5 /tmp/agent-repl-test-ramdisks/registry.mutex\n"

func TestLockRegistryTakesAFreeLock(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{})
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	unlock, err := m.lockRegistry()

	// Assert
	if err != nil {
		t.Fatalf("lockRegistry = %v, want the free lock taken", err)
	}
	if err := unlock(); err != nil {
		t.Fatalf("unlock: %v", err)
	}
}

func TestLockRegistryTakesALockItsHolderLetsGoOf(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{})
	release := holdRegistryLock(t, m)
	answered := make(chan error, 1)
	go func() {
		unlock, err := m.lockRegistry()
		if err == nil {
			err = unlock()
		}
		answered <- err
	}()

	// Act
	release()

	// Assert
	if err := <-answered; err != nil {
		t.Fatalf("lockRegistry = %v, want the lock taken once its holder let go", err)
	}
}

func TestLockRegistryTimesOutOnAStuckHolder(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{})
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	_, err := m.lockRegistry()

	// Assert
	var stuck *LockTimeoutError
	if !errors.As(err, &stuck) || stuck.Path != filepath.Join(m.Registry, registryLockName) || stuck.Bound != 20*time.Millisecond {
		t.Fatalf("lockRegistry = %v, want a LockTimeoutError naming the lock and its bound", err)
	}
}

func TestLockRegistryNamesTheProcessesWithTheLockOpen(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{lsof: lockLsof})
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	_, err := m.lockRegistry()

	// Assert
	var stuck *LockTimeoutError
	if !errors.As(err, &stuck) || stuck.Holders != "testrun[7583]" {
		t.Fatalf("lockRegistry = %v, want testrun[7583] named", err)
	}
}

func TestLockRegistryLeavesThisProcessOutOfTheHolders(t *testing.T) {
	// Arrange
	self := fmt.Sprintf("COMMAND PID USER FD TYPE DEVICE SIZE/OFF NODE NAME\ntestrun %d me 3u REG 1,30 0 5 /x\n", os.Getpid())
	m := newManager(t, &fakeHost{lsof: self})
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	_, err := m.lockRegistry()

	// Assert
	var stuck *LockTimeoutError
	if !errors.As(err, &stuck) || stuck.Holders != "no other process lsof could see" {
		t.Fatalf("lockRegistry = %v, want this process left out", err)
	}
}

func TestLockRegistrySaysWhenLsofSeesNoOtherProcess(t *testing.T) {
	// Arrange: lsof answers exit 1 and no output when nothing has it open.
	m := newManager(t, &fakeHost{lsofErr: errors.New("lsof -n -P -- x: exit status 1: ")})
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	_, err := m.lockRegistry()

	// Assert
	var stuck *LockTimeoutError
	if !errors.As(err, &stuck) || stuck.Holders != "no other process lsof could see" {
		t.Fatalf("lockRegistry = %v, want lsof's empty answer stated", err)
	}
}

func TestLockRegistryNamesAnLsofFailureInPlaceOfTheHolders(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{lsofErr: errors.New("lsof did not answer within 600ms")})
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	_, err := m.lockRegistry()

	// Assert
	var stuck *LockTimeoutError
	if !errors.As(err, &stuck) || stuck.Holders != "unknown, lsof failed: lsof did not answer within 600ms" {
		t.Fatalf("lockRegistry = %v, want lsof's failure named", err)
	}
}

func TestLockRegistryRunsLsofWithinItsBound(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.LockBound = 20 * time.Millisecond
	holdRegistryLock(t, m)

	// Act
	m.lockRegistry() //nolint:errcheck // the bound is the subject

	// Assert
	if got := h.bounds["lsof -n -P -- "+filepath.Join(m.Registry, registryLockName)]; got != LsofBound {
		t.Fatalf("lsof ran under %s, want LsofBound %s", got, LsofBound)
	}
}

func TestLockRegistryLetsGoOfALockTakenPastItsBound(t *testing.T) {
	// Arrange: a wait that timed out, then its holder let go.
	m := newManager(t, &fakeHost{})
	m.LockBound = 20 * time.Millisecond
	release := holdRegistryLock(t, m)
	if _, err := m.lockRegistry(); err == nil {
		t.Fatal("lockRegistry took a held lock")
	}
	release()
	m.LockBound = 10 * time.Second

	// Act: the abandoned wait takes the lock and must let it go again.
	unlock, err := m.lockRegistry()

	// Assert
	if err != nil {
		t.Fatalf("lockRegistry = %v, want the lock free once the abandoned wait let go", err)
	}
	unlock() //nolint:errcheck // cleanup
}

func TestTimedNotesEachCallsDurationAndResult(t *testing.T) {
	// Arrange
	var call string
	var took time.Duration
	var noted error
	failing := errors.New("refused")
	r := Timed(func(string, ...string) (string, error) { return "out", failing }, func(c string, d time.Duration, err error) {
		call, took, noted = c, d, err
	})

	// Act
	out, err := r("hdiutil", "attach", "-nomount", "ram://16")

	// Assert
	if out != "out" || !errors.Is(err, failing) {
		t.Fatalf("Timed answered %q, %v; want the runner's own answer", out, err)
	}
	if call != "hdiutil attach -nomount ram://16" || !errors.Is(noted, failing) || took < 0 {
		t.Fatalf("noted %q in %s with %v, want the call, its duration and its error", call, took, noted)
	}
}
