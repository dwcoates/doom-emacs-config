// Package ramdisk gives a scheduled test run a RAM disk to hold its whole temp
// root on macOS (owner ruling, 2026-10-06).
//
// WHY. A full run's integration and e2e suites start real daemons, stores and
// sidecars that share SQLite files, and hundreds of those files were written
// and fsynced on the host's one SSD: the owner's live store once waited 54s for
// a WAL checkpoint behind them. Under a RAM disk, none of that traffic reaches
// the SSD at all.
//
// WHERE. The volume is mounted UNDER /tmp (/tmp/artr-<pid>), never in
// /Volumes: every path beneath it is still a temporary directory to the
// daemon's registration guard (tempdirs.FixedRoots), and still short enough
// for a unix socket path (104 bytes on macOS).
//
// ITS LIFETIME IS HELD BY THE KERNEL. The run holds an exclusive flock on
// <Registry>/<pid>.lock for as long as it lives, and the record in that file
// names the device and the mount. A run that does not end ordinarily (a
// SIGKILL, a crash) leaves its RAM disk attached; the next run's Reclaim finds
// the record, sees the lock free, kills whatever still runs out of the mount,
// and detaches it.
package ramdisk

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
)

// DefaultRegistry is where every run's lock record lives.
const DefaultRegistry = "/tmp/agent-repl-test-ramdisks"

// DefaultMountParent is where every run's volume is mounted.
const DefaultMountParent = "/tmp"

// EnvShortBase names, to every unit of a run on a RAM disk, the volume's mount:
// the short base the daemon's integration harness and unit-test roots make
// their directories in instead of /tmp, so they land on the RAM disk too.
const EnvShortBase = "AGENT_REPL_TEST_SHORT_BASE"

// EnvSizeMiB overrides the volume's size in MiB.
const EnvSizeMiB = "AGENT_REPL_TEST_RAMDISK_MIB"

// DefaultSizeMiB is the volume's size. A RAM disk's memory is taken as blocks
// are first written and is never handed back before the detach, so the size
// is also the most memory a run can hold. It is sized from measurement: the
// largest live footprint of a full run's temp root (see testrun's AGENTS.md
// section), with headroom.
const DefaultSizeMiB = 8192

// Runner runs one external command and answers its combined output.
type Runner func(name string, args ...string) (string, error)

// ExecRunner is the real Runner.
func ExecRunner(name string, args ...string) (string, error) {
	out, err := exec.Command(name, args...).CombinedOutput()
	if err != nil {
		return string(out), fmt.Errorf("%s %s: %w: %s", name, strings.Join(args, " "), err, strings.TrimSpace(string(out)))
	}
	return string(out), nil
}

// Manager creates, releases and reclaims run RAM disks.
type Manager struct {
	// Run runs hdiutil and diskutil.
	Run Runner
	// Registry holds the runs' lock records.
	Registry string
	// MountParent is the directory the volume is mounted in.
	MountParent string
	// Pid is this run's pid: it names the record, the volume and the mount.
	Pid int
	// KillUnder SIGKILLs every process whose command line names a path under
	// dir, answering how many it killed.
	KillUnder func(dir string) (int, error)
}

// Volume is one attached, mounted RAM disk owned by this run.
type Volume struct {
	// Device is the RAM disk's whole-disk device (/dev/diskN).
	Device string
	// Mount is where its volume is mounted.
	Mount string

	m    Manager
	lock *os.File
}

// Default is the Manager a real run uses.
func Default() Manager {
	return Manager{Run: ExecRunner, Registry: DefaultRegistry, MountParent: DefaultMountParent, Pid: os.Getpid(), KillUnder: KillProcessesUnder}
}

// SizeFromEnv answers EnvSizeMiB, or DefaultSizeMiB when it is unset. A value
// that is not a positive integer is an error, never a silent default.
func SizeFromEnv(getenv func(string) string) (int, error) {
	raw := getenv(EnvSizeMiB)
	if raw == "" {
		return DefaultSizeMiB, nil
	}
	n, err := strconv.Atoi(raw)
	if err != nil || n <= 0 {
		return 0, fmt.Errorf("%s=%q is not a positive number of MiB", EnvSizeMiB, raw)
	}
	return n, nil
}

// Acquire attaches a RAM disk of sizeMiB, formats it APFS (whose nanosecond
// timestamps the suites' mtime checks need, which HFS+'s one-second ones would
// not give) and mounts it at <MountParent>/artr-<pid>. On any failure it
// undoes what it did and answers the error; the caller falls back to disk.
func (m Manager) Acquire(sizeMiB int) (_ *Volume, err error) {
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		return nil, fmt.Errorf("make the RAM disk registry %s: %w", m.Registry, err)
	}
	recordPath := m.recordPath(m.Pid)
	lock, err := os.OpenFile(recordPath, os.O_CREATE|os.O_RDWR|os.O_TRUNC, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the RAM disk record %s: %w", recordPath, err)
	}
	if err := syscall.Flock(int(lock.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		lock.Close()
		return nil, fmt.Errorf("take the RAM disk record %s: %w", recordPath, err)
	}
	v := &Volume{m: m, lock: lock, Mount: filepath.Join(m.MountParent, fmt.Sprintf("artr-%d", m.Pid))}
	defer func() {
		if err != nil {
			err = errors.Join(err, v.Release())
		}
	}()

	out, err := m.Run("hdiutil", "attach", "-nomount", fmt.Sprintf("ram://%d", sizeMiB*2048))
	if err != nil {
		return v, fmt.Errorf("attach a %d MiB RAM disk: %w", sizeMiB, err)
	}
	fields := strings.Fields(out)
	if len(fields) == 0 || !strings.HasPrefix(fields[0], "/dev/disk") {
		return v, fmt.Errorf("attach a %d MiB RAM disk: hdiutil answered no device: %q", sizeMiB, out)
	}
	v.Device = fields[0]
	// THE RECORD IS WRITTEN BEFORE ANYTHING ELSE CAN FAIL, so a run killed
	// from here on leaves a device the next run can find.
	if err := writeRecord(lock, v.Device, v.Mount); err != nil {
		return v, err
	}
	name := fmt.Sprintf("artr-%d", m.Pid)
	if _, err := m.Run("diskutil", "eraseDisk", "APFS", name, v.Device); err != nil {
		return v, fmt.Errorf("format the RAM disk %s: %w", v.Device, err)
	}
	// eraseDisk mounted the volume in /Volumes; it moves under /tmp.
	info, err := m.Run("diskutil", "info", "/Volumes/"+name)
	if err != nil {
		return v, fmt.Errorf("find the RAM disk's volume: %w", err)
	}
	volume, err := deviceIdentifier(info)
	if err != nil {
		return v, err
	}
	if _, err := m.Run("diskutil", "unmount", volume); err != nil {
		return v, fmt.Errorf("unmount the RAM disk's volume %s from /Volumes: %w", volume, err)
	}
	if err := os.MkdirAll(v.Mount, 0o755); err != nil {
		return v, fmt.Errorf("make the RAM disk's mount point %s: %w", v.Mount, err)
	}
	if _, err := m.Run("diskutil", "mount", "-mountPoint", v.Mount, volume); err != nil {
		return v, fmt.Errorf("mount the RAM disk's volume %s at %s: %w", volume, v.Mount, err)
	}
	// Spotlight indexing a scratch volume would only add the work it removes.
	if err := os.WriteFile(filepath.Join(v.Mount, ".metadata_never_index"), nil, 0o644); err != nil {
		return v, fmt.Errorf("mark the RAM disk unindexed: %w", err)
	}
	return v, nil
}

// Release detaches the RAM disk and removes its mount point and record. A
// plain detach that is refused (something still holds a file on the volume)
// is forced, and the refusal is answered alongside, never dropped.
func (v *Volume) Release() error {
	var errs []error
	if v.Device != "" {
		if _, err := v.m.Run("hdiutil", "detach", v.Device); err != nil {
			if _, forceErr := v.m.Run("hdiutil", "detach", "-force", v.Device); forceErr != nil {
				errs = append(errs, fmt.Errorf("detach the RAM disk %s: %w; forced: %w", v.Device, err, forceErr))
			} else {
				errs = append(errs, fmt.Errorf("the RAM disk %s detached only when forced, so something still held it: %w", v.Device, err))
			}
		}
	}
	if err := os.Remove(v.Mount); err != nil && !errors.Is(err, os.ErrNotExist) {
		errs = append(errs, fmt.Errorf("remove the RAM disk's mount point %s: %w", v.Mount, err))
	}
	if v.lock != nil {
		if err := os.Remove(v.lock.Name()); err != nil && !errors.Is(err, os.ErrNotExist) {
			errs = append(errs, fmt.Errorf("remove the RAM disk record %s: %w", v.lock.Name(), err))
		}
		if err := v.lock.Close(); err != nil {
			errs = append(errs, fmt.Errorf("close the RAM disk record: %w", err))
		}
		v.lock = nil
	}
	return errors.Join(errs...)
}

// Reclaimed is one dead run's RAM disk that Reclaim took down.
type Reclaimed struct {
	Pid    int
	Device string
	Mount  string
	// Attached is false when the record's device was no longer a RAM disk
	// (the host rebooted since), so only the record and mount point went.
	Attached bool
	Killed   int
}

// Reclaim takes down the RAM disk of every dead run in the registry: one whose
// record's lock nobody holds. A live run's record is left strictly alone.
func (m Manager) Reclaim() ([]Reclaimed, error) {
	records, err := filepath.Glob(filepath.Join(m.Registry, "*.lock"))
	if err != nil {
		return nil, fmt.Errorf("list the RAM disk records: %w", err)
	}
	var done []Reclaimed
	var errs []error
	for _, path := range records {
		r, ok, err := m.reclaimOne(path)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if ok {
			done = append(done, r)
		}
	}
	return done, errors.Join(errs...)
}

func (m Manager) reclaimOne(path string) (Reclaimed, bool, error) {
	pid, err := strconv.Atoi(strings.TrimSuffix(filepath.Base(path), ".lock"))
	if err != nil {
		return Reclaimed{}, false, fmt.Errorf("the RAM disk record %s is not named by a pid", path)
	}
	if pid == m.Pid {
		return Reclaimed{}, false, nil
	}
	f, err := os.OpenFile(path, os.O_RDWR, 0)
	if errors.Is(err, os.ErrNotExist) {
		return Reclaimed{}, false, nil
	}
	if err != nil {
		return Reclaimed{}, false, fmt.Errorf("open the RAM disk record %s: %w", path, err)
	}
	defer f.Close()
	switch err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); {
	case errors.Is(err, syscall.EWOULDBLOCK):
		return Reclaimed{}, false, nil
	case err != nil:
		return Reclaimed{}, false, fmt.Errorf("probe the RAM disk record %s: %w", path, err)
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		return Reclaimed{}, false, fmt.Errorf("read the RAM disk record %s: %w", path, err)
	}
	device, mount := parseRecord(string(raw))
	r := Reclaimed{Pid: pid, Device: device, Mount: mount}
	if device != "" {
		info, err := m.Run("hdiutil", "info")
		if err != nil {
			return r, false, fmt.Errorf("list the attached images to reclaim %s: %w", device, err)
		}
		r.Attached = isAttachedRAMDisk(info, device)
	}
	if r.Attached {
		if mount != "" {
			killed, err := m.KillUnder(mount)
			r.Killed = killed
			if err != nil {
				return r, false, fmt.Errorf("kill what still runs out of %s: %w", mount, err)
			}
		}
		if _, err := m.Run("hdiutil", "detach", "-force", device); err != nil {
			return r, false, fmt.Errorf("detach the dead run's RAM disk %s: %w", device, err)
		}
	}
	if mount != "" {
		if err := os.Remove(mount); err != nil && !errors.Is(err, os.ErrNotExist) {
			return r, false, fmt.Errorf("remove the dead run's mount point %s: %w", mount, err)
		}
	}
	if err := os.Remove(path); err != nil {
		return r, false, fmt.Errorf("remove the dead run's RAM disk record %s: %w", path, err)
	}
	return r, true, nil
}

func (m Manager) recordPath(pid int) string {
	return filepath.Join(m.Registry, fmt.Sprintf("%d.lock", pid))
}

func writeRecord(f *os.File, device, mount string) error {
	if _, err := f.WriteAt([]byte("device="+device+"\nmount="+mount+"\n"), 0); err != nil {
		return fmt.Errorf("write the RAM disk record %s: %w", f.Name(), err)
	}
	if err := f.Sync(); err != nil {
		return fmt.Errorf("sync the RAM disk record %s: %w", f.Name(), err)
	}
	return nil
}

func parseRecord(raw string) (device, mount string) {
	for _, line := range strings.Split(raw, "\n") {
		if v, ok := strings.CutPrefix(line, "device="); ok {
			device = v
		}
		if v, ok := strings.CutPrefix(line, "mount="); ok {
			mount = v
		}
	}
	return device, mount
}

// deviceIdentifier reads "Device Identifier:" out of `diskutil info`.
func deviceIdentifier(info string) (string, error) {
	for _, line := range strings.Split(info, "\n") {
		if v, ok := strings.CutPrefix(strings.TrimSpace(line), "Device Identifier:"); ok {
			if id := strings.TrimSpace(v); id != "" {
				return id, nil
			}
		}
	}
	return "", fmt.Errorf("diskutil info named no device identifier: %q", info)
}

// isAttachedRAMDisk reports whether `hdiutil info` lists device as the whole
// disk of an image whose path is a ram:// URL. Each image's block starts at a
// line of '=' and carries its image-path and its device lines.
func isAttachedRAMDisk(info, device string) bool {
	for _, block := range strings.Split(info, "================================================") {
		ram, has := false, false
		for _, line := range strings.Split(block, "\n") {
			line = strings.TrimSpace(line)
			if v, ok := strings.CutPrefix(line, "image-path"); ok && strings.HasPrefix(strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(v), ":")), "ram://") {
				ram = true
			}
			if f := strings.Fields(line); len(f) > 0 && f[0] == device {
				has = true
			}
		}
		if ram && has {
			return true
		}
	}
	return false
}

// KillProcessesUnder SIGKILLs every process whose command line names a path
// under dir, the way the daemon integration harness reclaims a dead run root.
func KillProcessesUnder(dir string) (int, error) {
	out, err := exec.Command("ps", "-axo", "pid=,args=").Output()
	if err != nil {
		return 0, fmt.Errorf("list processes to reclaim %s: %w", dir, err)
	}
	prefix := strings.TrimSuffix(dir, "/") + "/"
	killed := 0
	var errs []error
	for _, line := range strings.Split(string(out), "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || !strings.Contains(line, prefix) {
			continue
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil || pid == os.Getpid() {
			continue
		}
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
			errs = append(errs, fmt.Errorf("kill leftover pid %d under %s: %w", pid, dir, err))
			continue
		}
		killed++
	}
	return killed, errors.Join(errs...)
}
