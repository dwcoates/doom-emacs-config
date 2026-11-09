package ramdisk

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"

	"agentrepl/testrun/internal/proctable"
	"agentrepl/testrun/reap"
)

// fakeHost is a scripted hdiutil/diskutil: it records every call and answers
// from fail (a command prefix that fails) and info (what `hdiutil info` says).
type fakeHost struct {
	calls []string
	fail  map[string]bool
	info  string
	mount string
	// volumes is the fake /Volumes: `diskutil eraseDisk` makes its volume's
	// root there, as the real one mounts it there.
	volumes string
	// noLogAtMount says whether the volume's .fseventsd/no_log existed when
	// it was mounted under the mount parent.
	noLogAtMount bool
	// lsof is what `lsof` answers; lsofErr is its failure, if any.
	lsof    string
	lsofErr error
	// attached is set once `hdiutil attach` was called; from then on
	// `hdiutil info` answers infoAfterAttach (when set) and infoErrAfterAttach.
	attached bool
	// detached is set once /dev/disk9 was detached: its helper is gone.
	detached bool
	// noHelper leaves the attached disk's helper out of the process table.
	noHelper           bool
	infoAfterAttach    string
	infoErrAfterAttach error
	// attachOut, when set, is what `hdiutil attach` answers.
	attachOut string
	// bounds are the bounds the probes ran under, by call.
	bounds map[string]time.Duration
	// load and procs are what `sysctl -n vm.loadavg` and the process table
	// answer; loadErr and procsErr their failures, if any.
	load     string
	loadErr  error
	procs    []proctable.Entry
	procsErr error
	// procsRead counts the process table's reads.
	procsRead int
	// mountedAt is where `diskutil mount` mounted the attached disk
	// (/dev/disk9), until it is detached; `hdiutil info` lists it there, as
	// the real one does.
	mountedAt string
	// late, by call, is the channel a watched probe answers once it missed
	// its bound; a call not in it answers as run does.
	late map[string]chan Late
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
	case strings.HasPrefix(call, "diskutil eraseDisk"):
		if err := os.MkdirAll(filepath.Join(h.volumes, args[2]), 0o755); err != nil {
			return "", err
		}
	case strings.HasPrefix(call, "diskutil mount nobrowse"):
		_, err := os.Stat(filepath.Join(h.volumes, "artr-4242", ".fseventsd", "no_log"))
		h.noLogAtMount = err == nil
		h.mountedAt = args[3]
	case call == "hdiutil detach /dev/disk9" || call == "hdiutil detach -force /dev/disk9":
		h.mountedAt = ""
		h.detached = true
	case len(detaches([]string{call})) == 1:
		h.info = withoutImage(h.info, args[len(args)-1])
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
	case strings.HasPrefix(call, "hdiutil info") && h.attached && !h.detached:
		return h.info + servedBlock("/dev/disk9", h.mountedAt, attachedHelper), nil
	case strings.HasPrefix(call, "hdiutil info"):
		return h.info, nil
	case strings.HasPrefix(call, "lsof"):
		return h.lsof, h.lsofErr
	case strings.HasPrefix(call, "sysctl"):
		return h.load, h.loadErr
	}
	return "", nil
}

// attachedHelper is the process serving the disk the fake attach makes, and
// helperStart when every fake helper started.
const attachedHelper = 9999

var helperStart = time.Date(2026, 11, 1, 19, 12, 6, 123456000, time.UTC)

// table is the fake process table: procs, with the attached disk's helper
// while that disk is attached.
func (h *fakeHost) table() ([]proctable.Entry, error) {
	h.procsRead++
	procs := slices.Clone(h.procs)
	if h.attached && !h.detached && !h.noHelper {
		procs = append(procs, proctable.Entry{Pid: attachedHelper, State: 'S', Start: helperStart, Comm: "diskimages-helper"})
	}
	return procs, h.procsErr
}

// withoutImage is info with the image whose whole disk is device dropped, as
// `hdiutil info` lists it once that disk is detached.
func withoutImage(info, device string) string {
	blocks := strings.Split(info, imageSeparator)
	kept := blocks[:1]
	for _, block := range blocks[1:] {
		if !strings.Contains(block, "\n"+device+"\t") {
			kept = append(kept, block)
		}
	}
	return strings.Join(kept, imageSeparator)
}

func (h *fakeHost) probe(bound time.Duration, name string, args ...string) (string, error) {
	if h.bounds == nil {
		h.bounds = map[string]time.Duration{}
	}
	h.bounds[name+" "+strings.Join(args, " ")] = bound
	return h.run(name, args...)
}

func (h *fakeHost) watch(bound time.Duration, name string, args ...string) (string, <-chan Late, error) {
	call := name + " " + strings.Join(args, " ")
	if late, ok := h.late[call]; ok {
		if h.bounds == nil {
			h.bounds = map[string]time.Duration{}
		}
		h.bounds[call] = bound
		h.calls = append(h.calls, call)
		return "", late, unansweredError(bound, name, args, context.DeadlineExceeded)
	}
	out, err := h.probe(bound, name, args...)
	return out, nil, err
}

// fakeLog records every line the Manager logs, "INFO " or "ERROR " first.
type fakeLog struct {
	lines []string
}

func (l *fakeLog) Infof(format string, args ...any) {
	l.lines = append(l.lines, "INFO "+fmt.Sprintf(format, args...))
}

func (l *fakeLog) Errorf(format string, args ...any) {
	l.lines = append(l.lines, "ERROR "+fmt.Sprintf(format, args...))
}

func (l *fakeLog) has(prefix string) bool { return l.count(prefix) > 0 }

func (l *fakeLog) count(prefix string) int {
	n := 0
	for _, line := range l.lines {
		if strings.HasPrefix(line, prefix) {
			n++
		}
	}
	return n
}

func (l *fakeLog) snapshot() []string { return slices.Clone(l.lines) }

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
	h.volumes = t.TempDir()
	return Manager{
		Run: h.run, Registry: filepath.Join(t.TempDir(), "registry"), MountParent: parent, VolumesParent: h.volumes, Pid: 4242,
		Reap:      func(reap.Members) (reap.Result, error) { return reap.Result{}, nil },
		Probe:     h.probe,
		Procs:     h.table,
		Watch:     h.watch,
		Alive:     func(int) bool { return false },
		Mounted:   func(string) (bool, error) { return false, nil },
		Log:       &fakeLog{},
		Now:       tickingClock(),
		LockBound: 10 * time.Second,
		Memory:    func() (Memory, error) { return Memory{TotalBytes: 64 << 30, AvailableBytes: 64 << 30}, nil },
		Recheck:   func() <-chan time.Time { return nil },
		Rank:      19,
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
		"hdiutil info",
		"diskutil eraseDisk APFS artr-4242 /dev/disk9",
		"diskutil info " + filepath.Join(h.volumes, "artr-4242"),
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

func TestAcquireKeepsFSEventsLogOffTheVolumeBeforeMountingIt(t *testing.T) {
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
	if !h.noLogAtMount {
		t.Fatal(".fseventsd/no_log did not exist on the volume when it was mounted under the mount parent")
	}
}

func TestAcquireThatCannotKeepFSEventsLogOffTheVolumeDetachesItUnmounted(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	// A file where .fseventsd must be a directory refuses the no_log write.
	m.Run = func(name string, args ...string) (string, error) {
		out, err := h.run(name, args...)
		if name == "diskutil" && len(args) > 0 && args[0] == "eraseDisk" {
			if werr := os.WriteFile(filepath.Join(h.volumes, "artr-4242", ".fseventsd"), nil, 0o600); werr != nil {
				t.Fatalf("arrange: %v", werr)
			}
		}
		return out, err
	}

	// Act
	_, err := m.Acquire(16)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "to keep fseventsd's log off it") {
		t.Fatalf("Acquire err = %v, want the refused no_log write named", err)
	}
	for _, call := range h.calls {
		if strings.HasPrefix(call, "diskutil mount") || strings.HasPrefix(call, "diskutil unmount") {
			t.Fatalf("calls = %q, want no move of the volume after a refused no_log write", h.calls)
		}
	}
	if last := h.calls[len(h.calls)-1]; last != "hdiutil detach /dev/disk9" {
		t.Fatalf("last call = %q, want the device detached", last)
	}
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); !os.IsNotExist(statErr) {
		t.Fatalf("the record survives a failed acquire (stat err = %v)", statErr)
	}
}

func TestAcquireRecordsTheDeviceAndMount(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Own = reap.Members{Session: 4242, Since: time.Unix(1760400000, 0)}

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
	if got, want := string(raw), fmt.Sprintf("device=/dev/disk9\nhelper=9999\nhelper_start=%d\nmount=%s\nsession=4242\nsince=1760400000\n", helperStart.UnixMicro(), h.mount); got != want {
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

func TestReleaseKillsWhatRunsOutOfTheVolumeBeforeDetaching(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	var killedUnder string
	callsAtKill := -1
	m.Reap = func(ms reap.Members) (reap.Result, error) {
		killedUnder, callsAtKill = ms.Dirs[0], len(h.calls)
		return killedN(3), nil
	}
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	before := len(h.calls)

	// Act
	err = v.Release()

	// Assert
	if err != nil {
		t.Fatalf("Release: %v", err)
	}
	if killedUnder != h.mount || callsAtKill != before {
		t.Fatalf("killed under %q after %d of the release's calls, want %q before any detach", killedUnder, callsAtKill-before, h.mount)
	}
}

func TestReleaseKeepsTheDiskWhenWhatRunsOutOfItCannotBeKilled(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Reap = func(reap.Members) (reap.Result, error) { return reap.Result{}, errors.New("ps failed") }
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	before := len(h.calls)

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "ps failed") {
		t.Fatalf("Release = %v, want the failed kill reported", err)
	}
	noDetach(t, h.calls[before:])
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); statErr != nil {
		t.Fatalf("the record is gone (stat err = %v), want it kept for the next run's Reclaim", statErr)
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
	if err == nil || !strings.Contains(err.Error(), "held by: no process of this user lsof could see, and it sees no root process") {
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
	h := &fakeHost{}
	m := newManager(t, h)
	var killedUnder string
	m.Reap = func(ms reap.Members) (reap.Result, error) { killedUnder = ms.Dirs[0]; return killedN(2), nil }
	mount := filepath.Join(t.TempDir(), "artr-7")
	h.info = ramInfo("/dev/disk7=" + mount)
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

func TestReclaimLeavesADeviceAnotherRunNowHoldsAttached(t *testing.T) {
	tests := []struct {
		name string
		// info is what `hdiutil info` answers: /dev/disk7 attached.
		info string
		// otherRecord, when set, is another run's record naming /dev/disk7.
		otherRecord bool
	}{
		{
			name:        "the device is mounted at another run's mount point",
			info:        "framework : 1\n================================================\nimage-path      : ram://2048\n/dev/disk7\t\t\n/dev/disk8s1\t\t/private/tmp/artr-9\n",
			otherRecord: true,
		},
		{
			name:        "another run's record names the device",
			info:        attachedInfo,
			otherRecord: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := &fakeHost{info: tc.info}
			m := newManager(t, h)
			mount := filepath.Join(t.TempDir(), "artr-7")
			record := deadRecord(t, m, "7", "/dev/disk7", mount)
			if tc.otherRecord {
				other := filepath.Join(m.Registry, "9.lock")
				if err := os.WriteFile(other, []byte("device=/dev/disk7\nmount=/tmp/artr-9\n"), 0o644); err != nil {
					t.Fatal(err)
				}
				held, err := os.OpenFile(other, os.O_RDWR, 0)
				if err != nil {
					t.Fatal(err)
				}
				defer held.Close()
				if err := syscall.Flock(int(held.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			done, err := m.Reclaim()

			// Assert
			if err != nil {
				t.Fatalf("Reclaim: %v", err)
			}
			noDetach(t, h.calls)
			if len(done) != 1 || done[0].Pid != 7 || done[0].Attached {
				t.Fatalf("reclaimed = %+v, want pid 7's record reclaimed with no disk of its own attached", done)
			}
			if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
				t.Fatalf("the dead record survives (stat err = %v)", statErr)
			}
		})
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
	if err != nil || len(done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want a live run left alone", done, err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); statErr != nil {
		t.Fatalf("the live run's record is gone: %v", statErr)
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
	noDetach(t, h.calls)
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

func TestDescribeDevice(t *testing.T) {
	tests := []struct {
		name   string
		info   string
		device string
		want   string
	}{
		{name: "an unmounted RAM disk", info: attachedInfo, device: "/dev/disk7", want: "a device of the RAM disk /dev/disk7, mounted nowhere"},
		{name: "a mounted RAM disk's volume", info: ramInfo("/dev/disk4=/private/tmp/artr-35768"), device: "/dev/disk4s1", want: "a device of the RAM disk /dev/disk4, mounted at /private/tmp/artr-35768"},
		{name: "another device", info: attachedInfo, device: "/dev/disk70", want: "no RAM disk's device"},
		{name: "a file-backed image", info: strings.Replace(attachedInfo, "ram://2048", "/Users/x/a.dmg", 1), device: "/dev/disk7", want: "no RAM disk's device"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := describeDevice(tc.info, tc.device)

			// Assert
			if got != tc.want {
				t.Fatalf("describeDevice = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestUnitPrefixRunsUnitsAtTheDefaultTier(t *testing.T) {
	// Act
	got := strings.Join(UnitPrefix(), " ")

	// Assert
	if got != "/usr/sbin/taskpolicy -d default" {
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
		b.WriteString(ramBlock(device, mount))
	}
	return b.String()
}

// ramBlock is one RAM disk's block in `hdiutil info`: its whole disk device,
// and its volume's device mounted at mount when mount is not empty.
func ramBlock(device, mount string) string {
	block := "================================================\nimage-path      : ram://32768\n" + device + "\tGUID_partition_scheme\t\n"
	if mount != "" {
		block += device + "s1\t41504653-0000-11AA-AA11-00306543ECAC\t" + mount + "\n"
	}
	return block
}

// servedBlock is ramBlock with the image's helper: its "process ID".
func servedBlock(device, mount string, helper int) string {
	return strings.Replace(ramBlock(device, mount), "image-path      : ram://32768\n", fmt.Sprintf("image-path      : ram://32768\nprocess ID      : %d\n", helper), 1)
}

// helperEntry is the process-table entry of a helper that started at start.
func helperEntry(pid int, start time.Time) proctable.Entry {
	return proctable.Entry{Pid: pid, State: 'S', Start: start, Comm: "diskimages-helper"}
}

// servedRecord writes a record nobody holds the lock of, naming the disk's
// helper as an attach since the helper was recorded writes it.
func servedRecord(t *testing.T, m Manager, pid, device, mount string, helper int) string {
	t.Helper()
	path := deadRecord(t, m, pid, device, mount)
	body := fmt.Sprintf("device=%s\nhelper=%d\nhelper_start=%d\nmount=%s\n", device, helper, helperStart.UnixMicro(), mount)
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
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
	noDetach(t, h.calls)
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
		"INFO RAM disk acquire: begin at 2026-10-12T00:00:01.000Z, 16 MiB",
		"INFO RAM disk seat: took seat 0 of 1706; holders=[pid 4242]; available=65536MiB wired=16MiB ceiling=27306MiB floor=10240MiB rank=19",
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
		"INFO RAM disk release: begin at 2026-10-12T00:00:05.000Z, /dev/disk9 mounted at " + h.mount,
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
		{name: "a RAM disk with its helper", info: ramInfo() + servedBlock("/dev/disk4", "", 50373), want: []image{{Path: "ram://32768", Devices: []string{"/dev/disk4"}, Helper: 50373}}},
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

func TestUnreapedRunsEachCallInsideTheShield(t *testing.T) {
	// Arrange.
	var shielded, ran []string
	inner := func(name string, args ...string) (string, error) {
		ran = append(ran, name+" "+strings.Join(args, " "))
		return "out", errors.New("refused")
	}
	shield := func(fn func()) { shielded = append(shielded, "enter"); fn(); shielded = append(shielded, "leave") }

	// Act.
	out, err := Unreaped(inner, shield)("diskutil", "eraseDisk", "APFS")

	// Assert.
	if out != "out" || err == nil || err.Error() != "refused" {
		t.Fatalf("Unreaped() = %q, %v; want the inner call's answer", out, err)
	}
	if !slices.Equal(ran, []string{"diskutil eraseDisk APFS"}) || !slices.Equal(shielded, []string{"enter", "leave"}) {
		t.Fatalf("ran %q inside %q; want the one call inside the shield", ran, shielded)
	}
}

func TestCommandOutcome(t *testing.T) {
	expired, cancel := context.WithTimeout(context.Background(), 0)
	defer cancel()
	<-expired.Done()
	tests := []struct {
		name    string
		ctx     context.Context
		out     string
		err     error
		wantOut string
		wantErr string
	}{
		{name: "an answer is the output", ctx: context.Background(), out: "framework : 1\n", wantOut: "framework : 1\n"},
		{name: "a failure names the command and its output", ctx: context.Background(), out: "refused\n", err: errors.New("exit status 1"), wantOut: "refused\n", wantErr: "hdiutil info: exit status 1: refused"},
		{name: "a killed command did not answer within its bound", ctx: expired, out: "", err: errors.New("signal: killed"), wantErr: "hdiutil info did not answer within 1s; the disk manager (diskarbitrationd) may be wedged: context deadline exceeded"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			out, err := commandOutcome(tc.ctx, time.Second, "hdiutil", []string{"info"}, []byte(tc.out), tc.err)

			// Assert
			gotErr := ""
			if err != nil {
				gotErr = err.Error()
			}
			if out != tc.wantOut || gotErr != tc.wantErr {
				t.Fatalf("commandOutcome = %q, %q; want %q, %q", out, gotErr, tc.wantOut, tc.wantErr)
			}
		})
	}
}

func TestCommandOutcomeKeepsTheDeadlineInTheChain(t *testing.T) {
	// Arrange
	expired, cancel := context.WithTimeout(context.Background(), 0)
	defer cancel()
	<-expired.Done()

	// Act
	_, err := commandOutcome(expired, time.Second, "diskutil", []string{"list"}, nil, errors.New("signal: killed"))

	// Assert
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("err = %v, want context.DeadlineExceeded in its chain", err)
	}
}

func TestRunWatchedAnswersACommandThatFinishesInTime(t *testing.T) {
	// Act
	out, late, err := runWatched(10*time.Second, 20*time.Second, "echo", "listed")

	// Assert
	if err != nil || late != nil || strings.TrimSpace(out) != "listed" {
		t.Fatalf("runWatched(echo listed) = %q, %v, %v; want its output and no watch", out, late, err)
	}
}

func TestRunWatchedReportsAFailingCommandInTime(t *testing.T) {
	// Act
	_, late, err := runWatched(10*time.Second, 20*time.Second, "sh", "-c", "echo refused >&2; exit 3")

	// Assert
	if err == nil || late != nil || !strings.Contains(err.Error(), "refused") {
		t.Fatalf("runWatched(exit 3) = %v, %v; want the command's own failure and no watch", late, err)
	}
}

func TestRunWatchedAnswersAtItsBoundAndWatchesTheLateAnswer(t *testing.T) {
	// Act: the command cannot finish within 20ms.
	_, late, err := runWatched(20*time.Millisecond, 20*time.Second, "sh", "-c", "sleep 0.3")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer within 20ms") || late == nil {
		t.Fatalf("runWatched = %v, %v; want the bound's failure and a watch", late, err)
	}
	select {
	case outcome := <-late:
		if outcome.Err != nil || outcome.Took < 300*time.Millisecond {
			t.Fatalf("late = %+v, want the answer after at least 300ms", outcome)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the late answer never came")
	}
}

func TestRunWatchedCarriesTheLateAnswersOutput(t *testing.T) {
	// Act: the command cannot finish within 20ms.
	_, late, _ := runWatched(20*time.Millisecond, 20*time.Second, "sh", "-c", "sleep 0.3; echo listed")

	// Assert
	select {
	case outcome := <-late:
		if outcome.Err != nil || strings.TrimSpace(outcome.Out) != "listed" {
			t.Fatalf("late = %+v, want the command's own output", outcome)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the late answer never came")
	}
}

func TestRunWatchedKillsALateCommandAtItsLimit(t *testing.T) {
	// Act
	_, late, _ := runWatched(20*time.Millisecond, 200*time.Millisecond, "sleep", "10")

	// Assert
	select {
	case outcome := <-late:
		if outcome.Err == nil || !strings.Contains(outcome.Err.Error(), "did not answer within 200ms") {
			t.Fatalf("late = %+v, want the limit's failure", outcome)
		}
	case <-time.After(10 * time.Second):
		t.Fatal("the command outlived its limit")
	}
}

func TestScriptDispatchRunsItsCommandThroughTheSharedBoundedRun(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("investigation.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	rest := string(src)[strings.Index(string(src), "func ScriptDispatch("):]
	body := rest[:strings.Index(rest, "\n}\n")]

	// Assert
	if !strings.Contains(body, "command.Bounded(") || !strings.Contains(body, "command.Outcome(") || strings.Contains(body, "exec.CommandContext") {
		t.Fatal("ScriptDispatch starts or judges its command by hand; want command.Bounded and command.Outcome")
	}
}

func TestBoundedRunsShareOneOutcome(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("ramdisk.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	body := func(name string) string {
		rest := string(src)[strings.Index(string(src), "func "+name+"("):]
		return rest[:strings.Index(rest, "\n}\n")]
	}

	// Assert
	for _, name := range []string{"runBounded", "runWatched"} {
		if b := body(name); !strings.Contains(b, "commandOutcome(") || !strings.Contains(b, "command.Bounded(") || strings.Contains(b, "exec.CommandContext") {
			t.Errorf("%s forms its command or its outcome by hand; want command.Bounded and commandOutcome", name)
		}
	}
}

// strayDir makes a mount point named name under m's mount parent, with a
// file a dead run's process left under it.
func strayDir(t *testing.T, m Manager, name string) string {
	t.Helper()
	dir := filepath.Join(m.MountParent, name)
	if err := os.MkdirAll(filepath.Join(dir, "tr-1", "agent-repl-ert-x"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "tr-1", "agent-repl-ert-x", "f"), []byte("x"), 0o644); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestReclaimRemovesAStrayMountPointNoRecordNames(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	var killedUnder string
	m.Reap = func(ms reap.Members) (reap.Result, error) { killedUnder = ms.Dirs[0]; return killedN(12), nil }

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil {
		t.Fatalf("Reclaim: %v", err)
	}
	if _, statErr := os.Stat(dir); !os.IsNotExist(statErr) {
		t.Fatalf("the stray mount point survives (stat err = %v)", statErr)
	}
	if _, statErr := os.Stat(dir + ".reclaim"); !os.IsNotExist(statErr) {
		t.Fatalf("the stray's removal left %s.reclaim (stat err = %v)", dir, statErr)
	}
	want := []Reclaimed{{Pid: 28386, Mount: dir, Killed: 12, Stray: true}}
	if !reflect.DeepEqual(done, want) || killedUnder != dir {
		t.Fatalf("Reclaim = %+v, killed under %q; want %+v under %q", done, killedUnder, want, dir)
	}
}

func TestReclaimLeavesAStrayWhoseRunIsAlive(t *testing.T) {
	// Arrange: a run in its release, its record gone, its mount point not.
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	m.Alive = func(pid int) bool { return pid == 28386 }
	m.Reap = func(reap.Members) (reap.Result, error) {
		t.Fatal("reaped a live run's mount point")
		return reap.Result{}, nil
	}

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(dir); err != nil || len(done) != 0 || statErr != nil {
		t.Fatalf("Reclaim = %+v, %v (stat err = %v); want the live run's mount point left alone", done, err, statErr)
	}
}

func TestReclaimLeavesAMountPointARecordNames(t *testing.T) {
	// Arrange: a live run's record, its lock held.
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk7", dir)
	f, err := os.OpenFile(record, os.O_RDWR, 0)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { f.Close() })
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		t.Fatal(err)
	}

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(dir); err != nil || len(done) != 0 || statErr != nil {
		t.Fatalf("Reclaim = %+v, %v (stat err = %v); want the recorded mount point left alone", done, err, statErr)
	}
}

func TestReclaimLeavesAStrayWithAVolumeMounted(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	m.Mounted = func(d string) (bool, error) { return d == dir, nil }

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(filepath.Join(dir, "tr-1", "agent-repl-ert-x", "f")); err != nil || len(done) != 0 || statErr != nil {
		t.Fatalf("Reclaim = %+v, %v (stat err = %v); want the mounted volume left to the leak check", done, err, statErr)
	}
}

func TestReclaimKeepsAStrayWhoseProcessesCannotBeKilled(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	m.Reap = func(reap.Members) (reap.Result, error) { return reap.Result{}, errors.New("ps failed") }

	// Act
	done, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "kill what still runs out of the dead run's mount point "+dir) || len(done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want the failed kill", done, err)
	}
	if _, statErr := os.Stat(dir); statErr != nil {
		t.Fatalf("the stray went with its processes still running (stat err = %v)", statErr)
	}
}

func TestReclaimFailsAStrayWhoseMountCannotBeTold(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	m.Mounted = func(string) (bool, error) { return false, errors.New("stat failed") }

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(dir); err == nil || len(done) != 0 || statErr != nil {
		t.Fatalf("Reclaim = %+v, %v (stat err = %v); want the failure and the stray kept", done, err, statErr)
	}
}

func TestReclaimFinishesAStrayAnEarlierRemovalLeft(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386.reclaim")
	m.Reap = func(reap.Members) (reap.Result, error) {
		t.Fatal("reaped a mount point already taken")
		return reap.Result{}, nil
	}

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(dir); err != nil || !os.IsNotExist(statErr) {
		t.Fatalf("Reclaim = %v (stat err = %v); want the half-removed stray gone", err, statErr)
	}
	if want := []Reclaimed{{Pid: 28386, Mount: dir, Stray: true}}; !reflect.DeepEqual(done, want) {
		t.Fatalf("Reclaim = %+v, want %+v", done, want)
	}
}

func TestReclaimLeavesOtherDirectoriesInTheMountParent(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	var kept []string
	for _, name := range []string{"artr-x", "arrun123", "artr-7.old"} {
		kept = append(kept, strayDir(t, m, name))
	}
	file := filepath.Join(m.MountParent, "artr-9")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	kept = append(kept, file)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want nothing reclaimed", done, err)
	}
	for _, path := range kept {
		if _, statErr := os.Stat(path); statErr != nil {
			t.Fatalf("%s went (stat err = %v)", path, statErr)
		}
	}
}

func TestReclaimLeavesThisRunsOwnMountPoint(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-4242")

	// Act
	done, err := m.Reclaim()

	// Assert
	if _, statErr := os.Stat(dir); err != nil || len(done) != 0 || statErr != nil {
		t.Fatalf("Reclaim = %+v, %v (stat err = %v); want this run's own mount point left alone", done, err, statErr)
	}
}

func TestReclaimFailsAMountParentItCannotList(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.MountParent = filepath.Join(t.TempDir(), "gone")

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the RAM disk mount parent") {
		t.Fatalf("Reclaim = %v, want the failed listing", err)
	}
}

func TestIsMountPoint(t *testing.T) {
	tests := []struct {
		name string
		dir  string
		want bool
	}{
		{name: "a volume's mount point", dir: "/dev", want: true},
		{name: "a plain directory", dir: t.TempDir(), want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := IsMountPoint(tc.dir)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("IsMountPoint(%s) = %v, %v; want %v", tc.dir, got, err, tc.want)
			}
		})
	}
}

func TestIsMountPointFailsAMissingDirectory(t *testing.T) {
	// Act
	_, err := IsMountPoint(filepath.Join(t.TempDir(), "gone"))

	// Assert
	if err == nil {
		t.Fatal("IsMountPoint of a missing directory succeeded")
	}
}

func TestReclaimFailsAStrayWhenTheRegistryCannotBeMade(t *testing.T) {
	// Arrange: the registry's place is taken by a file.
	h := &fakeHost{}
	m := newManager(t, h)
	dir := strayDir(t, m, "artr-28386")
	blocker := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(blocker, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	m.Registry = filepath.Join(blocker, "registry")

	// Act
	done, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the RAM disk registry") || len(done) != 0 {
		t.Fatalf("Reclaim = %+v, %v; want the failed registry", done, err)
	}
	if _, statErr := os.Stat(dir); statErr != nil {
		t.Fatalf("the stray went without the registry lock (stat err = %v)", statErr)
	}
}

func TestReleaseRegistry(t *testing.T) {
	earlier := errors.New("attach refused")
	closeFailed := errors.New("bad file descriptor")
	tests := []struct {
		name     string
		err      error
		closeErr error
		want     []error
		wantNil  bool
	}{
		{name: "a clean release keeps no error", wantNil: true},
		{name: "a clean release keeps the holder's error", err: earlier, want: []error{earlier}},
		{name: "a failed release is answered", closeErr: closeFailed, want: []error{closeFailed}},
		{name: "a failed release joins the holder's error", err: earlier, closeErr: closeFailed, want: []error{earlier, closeFailed}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			err := tc.err

			// Act
			releaseRegistry(func() error { return tc.closeErr }, &err)

			// Assert
			if tc.wantNil != (err == nil) {
				t.Fatalf("err = %v, want nil %v", err, tc.wantNil)
			}
			for _, want := range tc.want {
				if !errors.Is(err, want) {
					t.Fatalf("err = %v, want %v in its chain", err, want)
				}
			}
			if tc.closeErr != nil && !strings.Contains(err.Error(), "release the RAM disk registry lock") {
				t.Fatalf("err = %v, want the release named", err)
			}
		})
	}
}

func TestRegistryHoldersShareReleaseRegistry(t *testing.T) {
	// The investigation takes no lock: the daemon refusing a second create of
	// its branch is what keeps two runs from opening two.
	for _, file := range []string{"ramdisk.go"} {
		// Arrange
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}

		// Act
		takes := strings.Count(string(src), "unlock, err := m.lockRegistry()")
		releases := strings.Count(string(src), "\tdefer releaseRegistry(unlock, &err)")

		// Assert
		if takes == 0 || takes != releases || strings.Count(string(src), "closeErr := unlock()") > 1 {
			t.Errorf("%s takes the registry lock %d times and releases it through releaseRegistry %d times; want every release through it", file, takes, releases)
		}
	}
}

// killedN is a reap's result that killed n members.
func killedN(n int) reap.Result { return reap.Result{Killed: make([]reap.Proc, n)} }

func TestParseRecord(t *testing.T) {
	tests := []struct {
		name string
		raw  string
		want record
	}{
		{name: "a record naming the run's session", raw: "device=/dev/disk9\nmount=/tmp/artr-7\nsession=7\nsince=1760400000\n", want: record{Device: "/dev/disk9", Mount: "/tmp/artr-7", Session: 7, Since: time.Unix(1760400000, 0)}},
		{name: "a record written before the session was recorded", raw: "device=/dev/disk9\nmount=/tmp/artr-7\n", want: record{Device: "/dev/disk9", Mount: "/tmp/artr-7"}},
		{name: "a record naming the disk's helper", raw: "device=/dev/disk9\nhelper=50373\nhelper_start=1762042326123456\nmount=/tmp/artr-7\n", want: record{Device: "/dev/disk9", Helper: helper{Pid: 50373, Start: time.UnixMicro(1762042326123456)}, Mount: "/tmp/artr-7"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := parseRecord(tt.raw)

			// Assert.
			if got.Device != tt.want.Device || !got.Helper.equal(tt.want.Helper) || got.Mount != tt.want.Mount || got.Session != tt.want.Session || !got.Since.Equal(tt.want.Since) {
				t.Fatalf("parseRecord() = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestReleaseReapsTheRunsSessionAndWhatRunsOutOfTheVolume(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Own = reap.Members{What: "run 4242", Session: 4242, Since: time.Unix(1760400000, 0), Except: []int{4242, 4243}}
	var got reap.Members
	m.Reap = func(ms reap.Members) (reap.Result, error) { got = ms; return reap.Result{}, nil }
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
	if got.Session != 4242 || !got.Since.Equal(m.Own.Since) || !slices.Equal(got.Except, []int{4242, 4243}) || !slices.Contains(got.Dirs, h.mount) {
		t.Fatalf("reaped %+v, want the run's session and the mount %s", got, h.mount)
	}
}

func TestReclaimReapsTheDeadRunsRecordedSession(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	var got reap.Members
	m.Reap = func(ms reap.Members) (reap.Result, error) { got = ms; return reap.Result{}, nil }
	mount := filepath.Join(t.TempDir(), "artr-7")
	h.info = ramInfo("/dev/disk7=" + mount)
	path := deadRecord(t, m, "7", "/dev/disk7", mount)
	if err := os.WriteFile(path, []byte("device=/dev/disk7\nmount="+mount+"\nsession=7\nsince=1760400000\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := m.Reclaim(); err != nil {
		t.Fatalf("Reclaim: %v", err)
	}

	// Assert
	if got.Session != 7 || !got.Since.Equal(time.Unix(1760400000, 0)) || !slices.Contains(got.Except, 4242) || !slices.Contains(got.Dirs, mount) {
		t.Fatalf("reaped %+v, want session 7 since its record and the mount %s", got, mount)
	}
}

func TestReclaimRunTakesDownTheDeadRunnersDisk(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	h.info = ramInfo("/dev/disk7=" + mount)
	record := deadRecord(t, m, "7", "/dev/disk7", mount)
	log := m.Log.(*fakeLog)

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err != nil || !ok {
		t.Fatalf("ReclaimRun = %v, %v; want the disk reclaimed", ok, err)
	}
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead runner's record survives (stat err = %v)", statErr)
	}
	if !log.has("INFO reclaimed the RAM disk /dev/disk7 left by dead run pid 7: it was detached and " + mount + " went") {
		t.Fatalf("log = %q, want the reclaim at INFO", log.snapshot())
	}
}

func TestReclaimRunAnswersFalseForARunWithNoRecord(t *testing.T) {
	// Arrange
	m := newManager(t, &fakeHost{})
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err != nil || ok {
		t.Fatalf("ReclaimRun = %v, %v; want nothing to reclaim", ok, err)
	}
}

func TestReclaimRunKeepsTheRecordWhenTheReapFails(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	m.Reap = func(reap.Members) (reap.Result, error) { return reap.Result{}, errors.New("a member did not exit") }
	mount := filepath.Join(t.TempDir(), "artr-7")
	h.info = ramInfo("/dev/disk7=" + mount)
	record := deadRecord(t, m, "7", "/dev/disk7", mount)
	log := m.Log.(*fakeLog)

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err == nil || ok || !strings.Contains(err.Error(), "a member did not exit") {
		t.Fatalf("ReclaimRun = %v, %v; want the failed reap", ok, err)
	}
	if !log.has("ERROR reclaim the RAM disk of dead run pid 7") {
		t.Fatalf("log = %q, want the failure at ERROR", log.snapshot())
	}
	if _, statErr := os.Stat(record); statErr != nil {
		t.Fatalf("the record went with its run's processes unreaped (stat err = %v)", statErr)
	}
	for _, call := range h.calls {
		if strings.Contains(call, "detach") {
			t.Fatalf("detached under unreaped members: %q", h.calls)
		}
	}
}

func TestTheInvestigationTakesNoRegistryLock(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("investigation.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act / Assert
	if strings.Contains(string(src), "lockRegistry()") {
		t.Fatal("investigation.go takes the registry lock; the daemon's refusal of a second create is the deduplication")
	}
}

// noDetach fails t when any call detached a disk.
func noDetach(t *testing.T, calls []string) {
	t.Helper()
	if got := detaches(calls); len(got) > 0 {
		t.Fatalf("detached %q; want nothing detached", got)
	}
}

// detaches answers every detach among calls.
func detaches(calls []string) []string {
	var got []string
	for _, call := range calls {
		if strings.HasPrefix(call, "hdiutil detach") {
			got = append(got, call)
		}
	}
	return got
}

func TestReclaimLeavesADeviceALiveRunReusedUntouched(t *testing.T) {
	// Arrange: dead run 7 recorded /dev/disk4, its disk is gone, and macOS
	// gave /dev/disk4 to live run 35768's disk (2026-10-30, artr-31920).
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-35768")}
	m := newManager(t, h)
	liveRecord(t, m, "35768", "/dev/disk4", "/tmp/artr-35768")
	mount := filepath.Join(t.TempDir(), "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk4", mount)
	log := m.Log.(*fakeLog)

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err != nil || !ok {
		t.Fatalf("ReclaimRun = %v, %v; want the dead record retired", ok, err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
	want := "INFO retired the RAM disk record of dead run pid 7: no disk it owns was attached, so nothing was detached and only " + mount + " went (the record named /dev/disk4, which was not touched: it is now a device of the RAM disk /dev/disk4, mounted at /private/tmp/artr-35768)"
	if !slices.Contains(log.snapshot(), want) {
		t.Fatalf("log = %q, want %q", log.snapshot(), want)
	}
}

func TestReclaimRetiresAnUnmountedDeadEntryWithoutDetaching(t *testing.T) {
	// Arrange: the recorded device is an attached RAM disk, mounted nowhere,
	// so nothing says it is still the dead run's.
	h := &fakeHost{info: attachedInfo}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk7", mount)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || done[0].Attached || len(done[0].Devices) != 0 || done[0].Recorded != "/dev/disk7" {
		t.Fatalf("Reclaim = %+v, %v; want the record of /dev/disk7 retired with nothing detached", done, err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
}

func TestReclaimDetachesTheDiskMountedAtTheDeadRunsMountNotItsRecordedDevice(t *testing.T) {
	// Arrange: dead run 7's disk is mounted at its mount point under
	// /dev/disk6, and its record's /dev/disk4 is a live run's.
	mount := filepath.Join(t.TempDir(), "artr-7")
	h := &fakeHost{info: ramInfo("/dev/disk4=/private/tmp/artr-35768", "/dev/disk6="+mount)}
	m := newManager(t, h)
	liveRecord(t, m, "35768", "/dev/disk4", "/tmp/artr-35768")
	deadRecord(t, m, "7", "/dev/disk4", mount)
	log := m.Log.(*fakeLog)

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err != nil || !ok {
		t.Fatalf("ReclaimRun = %v, %v; want the dead run's disk reclaimed", ok, err)
	}
	if got := detaches(h.calls); !slices.Equal(got, []string{"hdiutil detach -force /dev/disk6"}) {
		t.Fatalf("detaches = %q, want only the disk mounted at %s", got, mount)
	}
	want := "INFO reclaimed the RAM disk /dev/disk6 left by dead run pid 7: it was detached and " + mount + " went (the record named /dev/disk4, which was not touched: it is now a device of the RAM disk /dev/disk4, mounted at /private/tmp/artr-35768; killed 0 leftover processes)"
	if !slices.Contains(log.snapshot(), want) {
		t.Fatalf("log = %q, want %q", log.snapshot(), want)
	}
}

func TestReclaimFindsTheMountUnderItsResolvedSpelling(t *testing.T) {
	// Arrange: the record names /tmp/artr-7, and hdiutil lists the volume at
	// /private/tmp/artr-7, as macOS spells it.
	real := t.TempDir()
	link := filepath.Join(t.TempDir(), "tmp")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}
	resolved, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatal(err)
	}
	h := &fakeHost{info: ramInfo("/dev/disk6=" + filepath.Join(resolved, "artr-7"))}
	m := newManager(t, h)
	deadRecord(t, m, "7", "/dev/disk6", filepath.Join(link, "artr-7"))

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || !done[0].Attached {
		t.Fatalf("Reclaim = %+v, %v; want the disk found by its resolved mount", done, err)
	}
	if got := detaches(h.calls); !slices.Equal(got, []string{"hdiutil detach -force /dev/disk6"}) {
		t.Fatalf("detaches = %q, want /dev/disk6", got)
	}
}

func TestReclaimKeepsAnEntryWhoseMountHoldsAVolumeNoRAMDiskLists(t *testing.T) {
	// Arrange
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	if err := os.MkdirAll(filepath.Join(mount, "data"), 0o755); err != nil {
		t.Fatal(err)
	}
	m.Mounted = func(d string) (bool, error) { return d == mount, nil }
	record := deadRecord(t, m, "7", "/dev/disk7", mount)
	log := m.Log.(*fakeLog)

	// Act
	ok, err := m.ReclaimRun(7)

	// Assert
	if err == nil || ok || !strings.Contains(err.Error(), "a volume is mounted at "+mount+" that no RAM disk image lists") {
		t.Fatalf("ReclaimRun = %v, %v; want the foreign volume refused", ok, err)
	}
	noDetach(t, h.calls)
	for _, path := range []string{record, filepath.Join(mount, "data")} {
		if _, statErr := os.Stat(path); statErr != nil {
			t.Fatalf("%s was touched: %v", path, statErr)
		}
	}
	if !log.has("ERROR reclaim the RAM disk of dead run pid 7: a volume is mounted at " + mount) {
		t.Fatalf("log = %q, want the refusal at ERROR", log.snapshot())
	}
}

func TestReclaimKeepsAnEntryWhoseMountTwoRAMDisksList(t *testing.T) {
	// Arrange
	mount := filepath.Join(t.TempDir(), "artr-7")
	h := &fakeHost{info: ramInfo("/dev/disk6="+mount, "/dev/disk8="+mount)}
	m := newManager(t, h)
	record := deadRecord(t, m, "7", "/dev/disk6", mount)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "lists 2 RAM disks (/dev/disk6, /dev/disk8) mounted at "+mount) {
		t.Fatalf("Reclaim error = %v, want the two disks named", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReclaimKeepsAnEntryWhoseMountCannotBeTold(t *testing.T) {
	// Arrange
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	m.Mounted = func(string) (bool, error) { return false, errors.New("stat failed") }
	mount := filepath.Join(t.TempDir(), "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk7", mount)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "tell whether a volume is mounted at "+mount+": stat failed") {
		t.Fatalf("Reclaim error = %v, want the failed mount check", err)
	}
	if _, statErr := os.Stat(record); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReclaimRetiresAnEntryWhoseMountPointIsGone(t *testing.T) {
	// Arrange: the real mount check fails on a missing directory.
	h := &fakeHost{info: "framework : 1\n"}
	m := newManager(t, h)
	m.Mounted = IsMountPoint
	record := deadRecord(t, m, "7", "/dev/disk7", filepath.Join(t.TempDir(), "artr-7"))

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || done[0].Attached {
		t.Fatalf("Reclaim = %+v, %v; want the record retired", done, err)
	}
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
}

func TestReclaimKeepsAnEntryWhenTheImagesCannotBeListed(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}}
	m := newManager(t, h)
	mount := filepath.Join(t.TempDir(), "artr-7")
	record := deadRecord(t, m, "7", "/dev/disk7", mount)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the attached images to find the disks of dead run pid 7") {
		t.Fatalf("Reclaim error = %v, want the failed listing", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReleaseOfADiskAlreadyGoneDetachesNothing(t *testing.T) {
	// Arrange: the run's disk vanished, and its number may be another's.
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.mountedAt = ""
	h.detached = true
	h.info = ramInfo("/dev/disk9=/private/tmp/artr-35768")
	h.calls = nil

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the RAM disk /dev/disk9 was already gone before its release (its helper, process 9999, serves no image), so nothing was detached") {
		t.Fatalf("Release = %v, want the gone disk answered", err)
	}
	noDetach(t, h.calls)
	for _, path := range []string{h.mount, filepath.Join(m.Registry, "4242.lock")} {
		if _, statErr := os.Stat(path); !os.IsNotExist(statErr) {
			t.Fatalf("%s survives the release of a gone disk (stat err = %v)", path, statErr)
		}
	}
}

func TestReleaseRefusesAHelperThatServesAnotherDevice(t *testing.T) {
	// Arrange: the run's helper is listed serving /dev/disk6, where an
	// attached image keeps its device.
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.detached = true
	h.info = ramInfo() + servedBlock("/dev/disk6", h.mount, attachedHelper)
	h.procs = []proctable.Entry{helperEntry(attachedHelper, helperStart)}
	h.calls = nil

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the RAM disk /dev/disk9's helper, process 9999, serves /dev/disk6 now, so nothing is detached") {
		t.Fatalf("Release = %v, want the moved helper refused", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReleaseKeepsTheDiskWhenTheProcessTableCannotBeRead(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.procsErr = errors.New("proc_info listpids: EINVAL")
	h.calls = nil

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the process table to identify the RAM disks' helpers: proc_info listpids: EINVAL") {
		t.Fatalf("Release = %v, want the unread process table", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReleaseKeepsTheDiskWhenTheImagesCannotBeListed(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.fail = map[string]bool{"hdiutil info": true}
	h.calls = nil

	// Act
	err = v.Release()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the attached images to find the RAM disk /dev/disk9 by its helper before detaching it") {
		t.Fatalf("Release = %v, want the failed listing", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); statErr != nil {
		t.Fatalf("the record is gone: %v", statErr)
	}
}

func TestReleaseFindsItsDiskByItsHelper(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	v, err := m.Acquire(16)
	if err != nil {
		t.Fatalf("Acquire: %v", err)
	}
	h.calls = nil

	// Act
	if err := v.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Assert
	if !slices.Equal(h.calls[:1], []string{"hdiutil info"}) || !slices.Equal(detaches(h.calls), []string{"hdiutil detach /dev/disk9"}) {
		t.Fatalf("calls = %q, want the images listed, then /dev/disk9 detached", h.calls)
	}
}

func TestDetaches(t *testing.T) {
	tests := []struct {
		name  string
		calls []string
		want  []string
	}{
		{name: "no call", calls: nil, want: nil},
		{name: "no detach", calls: []string{"hdiutil info", "diskutil list"}, want: nil},
		{name: "plain and forced detaches in order", calls: []string{"hdiutil info", "hdiutil detach /dev/disk9", "lsof", "hdiutil detach -force /dev/disk9"}, want: []string{"hdiutil detach /dev/disk9", "hdiutil detach -force /dev/disk9"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := detaches(tc.calls)

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("detaches = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestNoHandRolledDetachChecksRemain(t *testing.T) {
	// Arrange
	// The needle is split so this test's own source never matches it.
	needle := `strings.HasPrefix(call, "hdiutil ` + `detach")`
	var hand []string
	for _, file := range []string{"ramdisk_test.go", "leak_test.go"} {
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		// Act
		for i, line := range strings.Split(string(raw), "\n") {
			if strings.Contains(line, needle) {
				hand = append(hand, fmt.Sprintf("%s:%d", file, i+1))
			}
		}
	}

	// Assert: detaches is the one reader of the detaches among calls.
	if len(hand) != 1 {
		t.Fatalf("hdiutil detach checks at %v, want only detaches's own", hand)
	}
}

func TestAcquireRecordsTheDisksHelper(t *testing.T) {
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
		t.Fatal(err)
	}
	if got := parseRecord(string(raw)); got.Device != "/dev/disk9" || !got.Helper.equal(helper{Pid: attachedHelper, Start: helperStart}) {
		t.Fatalf("record = %+v, want /dev/disk9 served by helper %d since %s", got, attachedHelper, helperStart)
	}
}

func TestAcquireThatCannotIdentifyItsDiskDetachesIt(t *testing.T) {
	tests := []struct {
		name string
		host *fakeHost
		want string
	}{
		{name: "the helper is not in the process table", host: &fakeHost{noHelper: true}, want: "the helper of the RAM disk /dev/disk9, process 9999, is not in the process table"},
		{name: "the attached disk is not listed", host: &fakeHost{infoAfterAttach: "framework : 1\n"}, want: "hdiutil info lists no RAM disk /dev/disk9, which hdiutil attach just answered"},
		{name: "the process table cannot be read", host: &fakeHost{procsErr: errors.New("proc_info listpids: EINVAL")}, want: "read the process table to identify the RAM disks' helpers"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m := newManager(t, tc.host)
			if tc.host.procsErr != nil {
				// The leak check before the attach reads the table too.
				m.Procs = func() ([]proctable.Entry, error) {
					if tc.host.attached {
						return nil, tc.host.procsErr
					}
					return nil, nil
				}
			}

			// Act
			_, err := m.Acquire(16)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Acquire = %v, want %q", err, tc.want)
			}
			if got := detaches(tc.host.calls); !slices.Equal(got, []string{"hdiutil detach /dev/disk9"}) {
				t.Fatalf("detaches = %q, want the unidentified disk detached by its device", got)
			}
			if _, statErr := os.Stat(filepath.Join(m.Registry, "4242.lock")); !os.IsNotExist(statErr) {
				t.Fatalf("the record survives (stat err = %v)", statErr)
			}
		})
	}
}

func TestReclaimDetachesAnUnmountedDiskItsRecordsHelperServes(t *testing.T) {
	// Arrange: run 35946 was cut mid-erase; its disk is mounted nowhere, and
	// its record names the disk's helper (2026-11-01).
	h := &fakeHost{info: ramInfo() + servedBlock("/dev/disk4", "", 50373), procs: []proctable.Entry{helperEntry(50373, helperStart)}}
	m := newManager(t, h)
	mount := filepath.Join(m.MountParent, "artr-35946")
	record := servedRecord(t, m, "35946", "/dev/disk4", mount, 50373)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || !done[0].Attached || !slices.Equal(done[0].Devices, []string{"/dev/disk4"}) {
		t.Fatalf("Reclaim = %+v, %v; want /dev/disk4 reclaimed", done, err)
	}
	if got := detaches(h.calls); !slices.Equal(got, []string{"hdiutil detach -force /dev/disk4"}) {
		t.Fatalf("detaches = %q, want the helper's disk", got)
	}
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
}

func TestReclaimLeavesADiskWhoseHelperPidCameRoundAgain(t *testing.T) {
	// Arrange: the recorded helper's pid now serves another disk, started
	// later.
	h := &fakeHost{info: ramInfo() + servedBlock("/dev/disk4", "", 50373), procs: []proctable.Entry{helperEntry(50373, helperStart.Add(time.Microsecond))}}
	m := newManager(t, h)
	record := servedRecord(t, m, "35946", "/dev/disk4", filepath.Join(m.MountParent, "artr-35946"), 50373)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || done[0].Attached {
		t.Fatalf("Reclaim = %+v, %v; want the record retired with nothing detached", done, err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(record); !os.IsNotExist(statErr) {
		t.Fatalf("the dead record survives (stat err = %v)", statErr)
	}
}

func TestReclaimDetachesADeadRunsDiskMountedOnlyInVolumes(t *testing.T) {
	// Arrange: the erase mounted the dead run's volume in /Volumes, and its
	// record names no helper (written before the helper was recorded).
	h := &fakeHost{info: ramInfo("/dev/disk4=/Volumes/artr-35946")}
	m := newManager(t, h)
	var dirs []string
	m.Reap = func(ms reap.Members) (reap.Result, error) { dirs = ms.Dirs; return reap.Result{}, nil }
	mount := filepath.Join(m.MountParent, "artr-35946")
	deadRecord(t, m, "35946", "/dev/disk4", mount)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || !done[0].Attached {
		t.Fatalf("Reclaim = %+v, %v; want the disk in /Volumes reclaimed", done, err)
	}
	if got := detaches(h.calls); !slices.Equal(got, []string{"hdiutil detach -force /dev/disk4"}) {
		t.Fatalf("detaches = %q, want /dev/disk4", got)
	}
	if !slices.Contains(dirs, mount) || !slices.Contains(dirs, "/Volumes/artr-35946") {
		t.Fatalf("reaped under %q, want the run's mount point and the volume's mount", dirs)
	}
}

func TestReclaimAdoptsAnOrphanedDiskAndDetachesIt(t *testing.T) {
	// Arrange: run 35946's disk, mounted in /Volumes, which no record names
	// since a reclaim retired its record (2026-11-01).
	h := &fakeHost{info: ramInfo() + servedBlock("/dev/disk4", "/Volumes/artr-35946", 50373), procs: []proctable.Entry{helperEntry(50373, helperStart)}}
	m := newManager(t, h)
	log := m.Log.(*fakeLog)

	// Act
	done, err := m.Reclaim()

	// Assert
	if err != nil || len(done) != 1 || done[0].Pid != 35946 || !slices.Equal(done[0].Devices, []string{"/dev/disk4"}) {
		t.Fatalf("Reclaim = %+v, %v; want the orphan reclaimed as pid 35946's", done, err)
	}
	if got := detaches(h.calls); !slices.Equal(got, []string{"hdiutil detach -force /dev/disk4"}) {
		t.Fatalf("detaches = %q, want the orphan", got)
	}
	if _, statErr := os.Stat(filepath.Join(m.Registry, "35946.lock")); !os.IsNotExist(statErr) {
		t.Fatalf("the adopted record survives (stat err = %v)", statErr)
	}
	if !log.has("INFO adopted the RAM disk /dev/disk4 (mounted at /Volumes/artr-35946), which no record named, as dead run pid 35946's, so it is reclaimed") {
		t.Fatalf("log = %q, want the adoption at INFO", log.snapshot())
	}
}

func TestReclaimKeepsTheAdoptedRecordOfAnOrphanThatWouldNotDetach(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/Volumes/artr-35946"), fail: map[string]bool{"hdiutil detach -force /dev/disk4": true}}
	m := newManager(t, h)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "detach the RAM disk /dev/disk4 of dead run pid 35946") {
		t.Fatalf("Reclaim error = %v, want the refused detach", err)
	}
	raw, readErr := os.ReadFile(filepath.Join(m.Registry, "35946.lock"))
	if readErr != nil || parseRecord(string(raw)).Mount != filepath.Join(m.MountParent, "artr-35946") {
		t.Fatalf("the adopted record = %q (%v), want it kept for the next reclaim", raw, readErr)
	}
}

func TestReclaimAdoptsNoDiskThatHasARecord(t *testing.T) {
	tests := []struct {
		name string
		info string
		// record, when set, writes pid 7's dead record first.
		record bool
	}{
		{name: "this run's own disk", info: ramInfo("/dev/disk4=/private/tmp/artr-4242")},
		{name: "a disk whose pid has a record", info: ramInfo("/dev/disk4=/Volumes/artr-7", "/dev/disk5=/Volumes/artr-7"), record: true},
		{name: "a disk nothing marks", info: ramInfo("/dev/disk4=/Volumes/Scratch")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := &fakeHost{info: tc.info}
			m := newManager(t, h)
			if tc.record {
				liveRecord(t, m, "7", "", filepath.Join(m.MountParent, "artr-7"))
			}
			if err := os.MkdirAll(m.Registry, 0o755); err != nil {
				t.Fatal(err)
			}

			// Act
			orphans, err := m.adoptOrphans()

			// Assert
			if err != nil || len(orphans) != 0 {
				t.Fatalf("adoptOrphans = %+v, %v; want nothing adopted", orphans, err)
			}
		})
	}
}

func TestReclaimAnswersAnAdoptionThatCannotReadTheProcessTable(t *testing.T) {
	// Arrange
	h := &fakeHost{info: ramInfo("/dev/disk4=/Volumes/artr-35946"), procsErr: errors.New("proc_info listpids: EINVAL")}
	m := newManager(t, h)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "read the process table to identify the RAM disks' helpers: proc_info listpids: EINVAL") {
		t.Fatalf("Reclaim error = %v, want the unread process table", err)
	}
	noDetach(t, h.calls)
	if _, statErr := os.Stat(filepath.Join(m.Registry, "35946.lock")); !os.IsNotExist(statErr) {
		t.Fatalf("a record was adopted without the table (stat err = %v)", statErr)
	}
}

func TestReclaimAnswersAnAdoptionThatCannotListTheImages(t *testing.T) {
	// Arrange
	h := &fakeHost{fail: map[string]bool{"hdiutil info": true}}
	m := newManager(t, h)

	// Act
	_, err := m.Reclaim()

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the attached images to find the RAM disks no record names") {
		t.Fatalf("Reclaim error = %v, want the failed listing", err)
	}
	noDetach(t, h.calls)
}

func TestOwns(t *testing.T) {
	served := image{Path: "ram://32768", Devices: []string{"/dev/disk4"}, Helper: 50373}
	starts := map[int]time.Time{50373: helperStart}
	tests := []struct {
		name string
		img  image
		pid  int
		rec  record
		want bool
	}{
		{name: "the record's helper serves it", img: served, pid: 7, rec: record{Helper: helper{Pid: 50373, Start: helperStart}}, want: true},
		{name: "the record's helper pid serves it, started at another time", img: served, pid: 7, rec: record{Helper: helper{Pid: 50373, Start: helperStart.Add(time.Microsecond)}}},
		{name: "another helper serves it", img: served, pid: 7, rec: record{Helper: helper{Pid: 50374, Start: helperStart}}},
		{name: "only the record's device names it", img: served, pid: 7, rec: record{Device: "/dev/disk4"}},
		{name: "it is mounted at the pid's run mount", img: image{Devices: []string{"/dev/disk4"}, Mounts: []string{"/Volumes/artr-7"}}, pid: 7, want: true},
		{name: "it is mounted at another pid's run mount", img: image{Devices: []string{"/dev/disk4"}, Mounts: []string{"/private/tmp/artr-8"}}, pid: 7},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := owns(tc.img, starts, tc.pid, tc.rec)

			// Assert
			if got != tc.want {
				t.Fatalf("owns = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestImageMarked(t *testing.T) {
	tests := []struct {
		name   string
		mounts []string
		want   int
		ok     bool
	}{
		{name: "no mount"},
		{name: "a run's own mount", mounts: []string{"/private/tmp/artr-35946"}, want: 35946, ok: true},
		{name: "the erase's mount in /Volumes", mounts: []string{"/Volumes/artr-35946"}, want: 35946, ok: true},
		{name: "a mount no run is given", mounts: []string{"/Volumes/Scratch"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := image{Mounts: tc.mounts}.marked()

			// Assert
			if got != tc.want || ok != tc.ok {
				t.Fatalf("marked = %d, %v; want %d, %v", got, ok, tc.want, tc.ok)
			}
		})
	}
}

func TestReapMembersNetsEverySpellingOfEveryDirectory(t *testing.T) {
	// Arrange
	h := &fakeHost{}
	m := newManager(t, h)
	var got reap.Members
	m.Reap = func(ms reap.Members) (reap.Result, error) { got = ms; return reap.Result{}, nil }
	first, second := t.TempDir(), t.TempDir()

	// Act
	if _, err := m.reapMembers(reap.Members{Dirs: []string{"/kept"}}, first, second); err != nil {
		t.Fatal(err)
	}

	// Assert
	for _, dir := range []string{first, second} {
		spellings, err := reap.Spellings(dir)
		if err != nil {
			t.Fatal(err)
		}
		for _, sp := range spellings {
			if !slices.Contains(got.Dirs, sp) {
				t.Fatalf("dirs = %q, want %q among them", got.Dirs, sp)
			}
		}
	}
	if got.Dirs[0] != "/kept" || got.What != "the run on "+first+", "+second {
		t.Fatalf("reaped %+v, want the given dirs kept and both directories named", got)
	}
}

func TestRAMDisks(t *testing.T) {
	tests := []struct {
		name string
		info string
		want []string
	}{
		{name: "no image", info: "framework : 1\n"},
		{name: "a RAM disk", info: ramInfo("/dev/disk4"), want: []string{"/dev/disk4"}},
		{name: "a disk image that is no RAM disk", info: "framework : 1\n================================================\nimage-path      : /Users/x/a.dmg\n/dev/disk6\t\t\n"},
		{name: "a RAM disk with no device line", info: "framework : 1\n================================================\nimage-path      : ram://2048\n"},
		{name: "RAM disks among other images", info: ramInfo("/dev/disk4") + "================================================\nimage-path      : /Users/x/a.dmg\n/dev/disk6\t\t\n" + ramBlock("/dev/disk7", "/Volumes/artr-7"), want: []string{"/dev/disk4", "/dev/disk7"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			var got []string
			for _, img := range ramDisks(tc.info) {
				got = append(got, img.Devices[0])
			}

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("ramDisks = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestNoHandRolledRAMDiskFiltersRemain(t *testing.T) {
	// Arrange
	// The needle is split so this test's own source never matches it.
	needle := "img.is" + "RAM()"
	var hand []string
	for _, file := range []string{"ramdisk.go", "leak.go"} {
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		// Act
		for i, line := range strings.Split(string(raw), "\n") {
			if strings.Contains(line, needle) {
				hand = append(hand, fmt.Sprintf("%s:%d", file, i+1))
			}
		}
	}

	// Assert: ramDisks is the one filter of the RAM disk images.
	if len(hand) != 1 {
		t.Fatalf("RAM image checks at %v, want only ramDisks's own", hand)
	}
}

func TestListDisks(t *testing.T) {
	tests := []struct {
		name string
		host *fakeHost
		want string
	}{
		{name: "both read", host: &fakeHost{info: ramInfo() + servedBlock("/dev/disk4", "", 50373), procs: []proctable.Entry{helperEntry(50373, helperStart), {Pid: 1, Comm: "launchd"}}}},
		{name: "the listing fails", host: &fakeHost{fail: map[string]bool{"hdiutil info": true}}, want: "list the attached images to find a disk: refused: hdiutil info"},
		{name: "the process table fails", host: &fakeHost{procsErr: errors.New("proc_info listpids: EINVAL")}, want: "read the process table to identify the RAM disks' helpers: proc_info listpids: EINVAL"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m := newManager(t, tc.host)

			// Act
			info, starts, err := m.listDisks("find a disk")

			// Assert
			if tc.want != "" {
				if err == nil || err.Error() != tc.want {
					t.Fatalf("listDisks error = %v, want %q", err, tc.want)
				}
				return
			}
			if err != nil || info != tc.host.info || len(starts) != 1 || !starts[50373].Equal(helperStart) {
				t.Fatalf("listDisks = %q, %v, %v; want the listing and the one started process", info, starts, err)
			}
		})
	}
}

func TestEveryDiskListingReadsTheHelpersThroughListDisks(t *testing.T) {
	// Arrange
	// The needle is split so this test's own source never matches it.
	needle := ".helper" + "Starts()"
	var sites []string
	for _, file := range []string{"ramdisk.go", "leak.go"} {
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		// Act
		for i, line := range strings.Split(string(raw), "\n") {
			if strings.Contains(line, needle) {
				sites = append(sites, fmt.Sprintf("%s:%d", file, i+1))
			}
		}
	}

	// Assert: listDisks, and the leak check, which is handed its listing.
	if len(sites) != 2 {
		t.Fatalf("process table reads at %v, want only listDisks's and leaksLocked's", sites)
	}
}
