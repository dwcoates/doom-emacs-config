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
//
// A LEAK OR A WEDGED DISK MANAGER STOPS THE ATTACH (owner ruling,
// 2026-10-12). Before anything is attached, Setup asks the disk manager for
// `diskutil list` and `hdiutil info` within short bounds, reclaims the dead
// runs' disks, and then, under the registry lock, looks for a RAM disk this
// runner made that no live run's record names. Either finding answers a
// Trouble instead of a volume: the run's root goes on the disk, the run says
// why at ERROR, and EnsureInvestigation opens ONE agent-repl workspace to find
// and fix the cause. A run that went on attaching while one stood leaked an
// 8 GiB disk per run, 13 at once, each of its disk commands running out its
// bound first.
package ramdisk

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"time"
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

// UnitIOTier is the disk I/O tier every unit of a RAM-disk run runs at.
//
// NOT THE THROTTLE TIER bin/background.sh gives the whole run. Throttled I/O
// to a RAM disk stalls for hundreds of milliseconds whenever anything of a
// higher tier touches the same volume (the kernel's throttle window for a
// device it does not know to be solid state), measured at a max 378-622ms
// per fsync against 0.4ms on the SSD; a run under it took 320-347s against
// 158s and missed its timing bounds. The utility tier measured a max 0.7ms on
// the RAM disk and still yields to the live runtime's normal-tier I/O on the
// SSD, where every unit's remaining traffic (build caches, sources) goes.
const UnitIOTier = "utility"

// UnitPrefix is the command line every unit of a RAM-disk run is started
// under.
func UnitPrefix() []string { return []string{"/usr/sbin/taskpolicy", "-d", UnitIOTier} }

// Runner runs one external command and answers its combined output.
type Runner func(name string, args ...string) (string, error)

// Timed wraps r so every call's duration and result go to note: the
// measurements CommandBound is set from (owner rule, 2026-10-12: a bound sits
// just over twice the healthy latency, measured, not guessed).
func Timed(r Runner, note func(call string, took time.Duration, err error)) Runner {
	return func(name string, args ...string) (string, error) {
		start := time.Now()
		out, err := r(name, args...)
		note(name+" "+strings.Join(args, " "), time.Since(start), err)
		return out, err
	}
}

// BoundedRunner runs one external command within bound and answers its
// combined output.
type BoundedRunner func(bound time.Duration, name string, args ...string) (string, error)

// Logger is the run's log: testrun's run.Log.
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// InfoBound is how long `hdiutil info` may run before the disk manager is
// taken not to answer.
//
// MEASURED FROM THE RUNS' OWN LOGS, at the runs' own priority (nice 19, the
// throttle I/O tier) on a host carrying several runs at once. The first bound,
// 50ms, came from 30 quiet calls (max 22ms) and missed the tail: the run logs
// of 2026-10-13 show healthy answers of 7-20ms, one of 42ms, and one still
// running at 56ms when the 50ms bound killed it (load average 28). That one
// was healthy: `diskutil list` answered in the same check, and the next run,
// 13s later, had its answer in 7ms. A refusal on it put the run on the SSD
// and opened an investigation workspace for a disk manager that was fine.
// The bound is just over twice that 56ms. A wedged disk manager never
// answers at all (2026-10-12: every call ran out its 60s bound), so the
// bound costs a healthy run nothing and a wedged one only this long.
const InfoBound = 120 * time.Millisecond

// ListBound is how long the `diskutil list` probe may run before the disk
// manager is taken not to answer: the same run logs show healthy answers of
// 21-49ms and one of 85ms (in the same check as the 56ms `hdiutil info`), and
// this is just over twice that max.
const ListBound = 175 * time.Millisecond

// CommandBound is how long one hdiutil, diskutil or lsof call may run.
//
// A WEDGED DISK MANAGER MUST FAIL THE ACQUIRE, NOT HANG THE RUN. Every one of
// these commands goes through diskarbitrationd, and when it wedged
// (2026-10-12, after scratch RAM disks were attached and detached by hand) a
// run sat 36 minutes in `diskutil unmount` with nothing printed. Bounded, the
// call fails, Acquire undoes what it did, and the run goes on with its temp
// root on disk, saying why. A healthy call answers in a few seconds; the
// bound is generous because a RAM disk's erase competes with a loaded host.
const CommandBound = 60 * time.Second

// ExecRunner is the real Runner: the command, bounded by CommandBound.
func ExecRunner(name string, args ...string) (string, error) {
	return runBounded(CommandBound, name, args...)
}

// RunBounded is the real BoundedRunner.
func RunBounded(bound time.Duration, name string, args ...string) (string, error) {
	return runBounded(bound, name, args...)
}

// runBounded runs the command, killing it once bound has passed.
func runBounded(bound time.Duration, name string, args ...string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	cmd := exec.CommandContext(ctx, name, args...)
	// A killed command's output pipe is not waited on past this.
	cmd.WaitDelay = time.Second
	out, err := cmd.CombinedOutput()
	if errors.Is(ctx.Err(), context.DeadlineExceeded) {
		return string(out), fmt.Errorf("%s %s did not answer within %s; the disk manager (diskarbitrationd) may be wedged: %w",
			name, strings.Join(args, " "), bound, ctx.Err())
	}
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
	// Probe runs the short-bounded reads of the disk manager (`hdiutil info`,
	// `diskutil list`).
	Probe BoundedRunner
	// Alive answers whether a process of pid exists.
	Alive func(pid int) bool
	// Log is the run's log: the acquire and release edges, with timings.
	Log Logger
	// Now is the clock the edges are timed by.
	Now func() time.Time
	// LockBound is how long the registry lock is waited for (LockBound).
	LockBound time.Duration
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
func Default(log Logger) Manager {
	return Manager{
		Run: ExecRunner, Registry: DefaultRegistry, MountParent: DefaultMountParent, Pid: os.Getpid(),
		KillUnder: KillProcessesUnder, Probe: RunBounded, Alive: ProcessAlive, Log: log, Now: time.Now,
		LockBound: LockBound,
	}
}

// ProcessAlive answers whether a process of pid exists, the way kill(pid, 0)
// says so: a process this user may not signal exists too.
func ProcessAlive(pid int) bool {
	err := syscall.Kill(pid, 0)
	return err == nil || errors.Is(err, syscall.EPERM)
}

// listImages answers `hdiutil info`, within InfoBound.
func (m Manager) listImages() (string, error) {
	return m.Probe(InfoBound, "hdiutil", "info")
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
//
// IT ATTACHES NOTHING WHILE A LEAK STANDS: a RAM disk this runner made that no
// live run's record names answers a *LeakError, before any record is made.
func (m Manager) Acquire(sizeMiB int) (_ *Volume, err error) {
	start := m.Now()
	m.Log.Infof("RAM disk acquire: begin, %d MiB", sizeMiB)
	defer func() {
		if err != nil {
			m.Log.Infof("RAM disk acquire: failed after %s", m.Now().Sub(start))
		}
	}()
	if err := os.MkdirAll(m.Registry, 0o755); err != nil {
		return nil, fmt.Errorf("make the RAM disk registry %s: %w", m.Registry, err)
	}
	v, err := m.attach(sizeMiB)
	if v != nil {
		defer func() {
			if err != nil {
				err = errors.Join(err, v.Release())
			}
		}()
	}
	if err != nil {
		return v, err
	}
	attached := m.Now()
	name := fmt.Sprintf("artr-%d", m.Pid)
	if _, err := m.Run("diskutil", "eraseDisk", "APFS", name, v.Device); err != nil {
		return v, fmt.Errorf("format the RAM disk %s: %w", v.Device, err)
	}
	formatted := m.Now()
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
	// NOBROWSE KEEPS SPOTLIGHT OFF THE VOLUME. Mounted plainly under /tmp, mds
	// opened an index store on every run's RAM disk (`open stores complete
	// success:1`, macOS 26.2, 2026-10-06) and took part in its unmount; a
	// `.metadata_never_index` written after the mount did not stop it. Mounted
	// nobrowse, mds declines the volume (`MDSDisk ... whyFail:-22`) and opens
	// no store at all, measured on the same host.
	if _, err := m.Run("diskutil", "mount", "nobrowse", "-mountPoint", v.Mount, volume); err != nil {
		return v, fmt.Errorf("mount the RAM disk's volume %s at %s: %w", volume, v.Mount, err)
	}
	end := m.Now()
	m.Log.Infof("RAM disk acquire: %s mounted at %s in %s (attach %s, format %s, mount %s)",
		v.Device, v.Mount, end.Sub(start), attached.Sub(start), formatted.Sub(attached), end.Sub(formatted))
	return v, nil
}

// attach makes this run's record and attaches its RAM disk, all under the
// registry lock, after looking for leaks under that same lock. It answers the
// volume whenever a record was made, so the caller's Release undoes it.
//
// THE REGISTRY LOCK IS HELD FROM THE LEAK CHECK UNTIL THE RECORD NAMES THE
// DEVICE, so no other run's check can see this run's disk attached and not
// yet recorded, and no other run attaches between this run's listing and its
// attach (which the cleanup of a failed attach below relies on).
func (m Manager) attach(sizeMiB int) (_ *Volume, err error) {
	unlock, err := m.lockRegistry()
	if err != nil {
		return nil, err
	}
	defer func() {
		if closeErr := unlock(); closeErr != nil {
			err = errors.Join(err, fmt.Errorf("release the RAM disk registry lock: %w", closeErr))
		}
	}()
	before, err := m.listImages()
	if err != nil {
		return nil, fmt.Errorf("list the attached images before attaching: %w", err)
	}
	if leaks, err := m.leaksLocked(before); err != nil {
		return nil, err
	} else if len(leaks) > 0 {
		return nil, &LeakError{Leaks: leaks, Info: before}
	}
	lock, err := m.createRecordLocked(m.recordPath(m.Pid))
	if err != nil {
		return nil, err
	}
	v := &Volume{m: m, lock: lock, Mount: filepath.Join(m.MountParent, fmt.Sprintf("artr-%d", m.Pid))}
	out, err := m.Run("hdiutil", "attach", "-nomount", fmt.Sprintf("ram://%d", sizeMiB*2048))
	if err != nil {
		// A KILLED ATTACH MAY STILL HAVE ATTACHED. hdiutil killed at its bound
		// leaves the kernel's attach to finish without it, and nothing would
		// ever name the device: each such run leaked one disk.
		return v, errors.Join(fmt.Errorf("attach a %d MiB RAM disk: %w", sizeMiB, err), m.detachNewRAMDisks(before))
	}
	fields := strings.Fields(out)
	if len(fields) == 0 || !strings.HasPrefix(fields[0], "/dev/disk") {
		return v, errors.Join(fmt.Errorf("attach a %d MiB RAM disk: hdiutil answered no device: %q", sizeMiB, out), m.detachNewRAMDisks(before))
	}
	v.Device = fields[0]
	// THE RECORD IS WRITTEN BEFORE ANYTHING ELSE CAN FAIL, so a run killed
	// from here on leaves a device the next run can find.
	if err := writeRecord(lock, v.Device, v.Mount); err != nil {
		return v, err
	}
	return v, nil
}

// detachNewRAMDisks force-detaches every RAM disk `hdiutil info` lists now
// that the listing before did not: what a failed attach left behind. Under the
// registry lock nothing else attached in between. What it detached is named,
// and so is every failure, never dropped.
func (m Manager) detachNewRAMDisks(before string) error {
	after, err := m.listImages()
	if err != nil {
		return fmt.Errorf("list the attached images after a failed attach, to detach what it may have left: %w", err)
	}
	old := map[string]bool{}
	for _, img := range parseImages(before) {
		if img.isRAM() && len(img.Devices) > 0 {
			old[img.Devices[0]] = true
		}
	}
	var errs []error
	for _, img := range parseImages(after) {
		if !img.isRAM() || len(img.Devices) == 0 || old[img.Devices[0]] {
			continue
		}
		device := img.Devices[0]
		if _, err := m.Run("hdiutil", "detach", "-force", device); err != nil {
			errs = append(errs, fmt.Errorf("detach the RAM disk %s the failed attach left: %w", device, err))
			continue
		}
		errs = append(errs, fmt.Errorf("the failed attach had attached %s anyway; it was detached", device))
	}
	return errors.Join(errs...)
}

// Release detaches the RAM disk and removes its mount point and record. A
// plain detach that is refused (something still holds a file on the volume)
// is forced, and the refusal is answered alongside, never dropped.
//
// A DISK THAT WOULD NOT DETACH KEEPS ITS RECORD. Removed, the record was the
// only thing that named the device, and the disk leaked for good; kept, its
// lock drops with this run and the next run's Reclaim retries the detach.
func (v *Volume) Release() error {
	start := v.m.Now()
	v.m.Log.Infof("RAM disk release: begin, %s at %s", v.Device, v.Mount)
	var errs []error
	detached := true
	if v.Device != "" {
		if _, err := v.m.Run("hdiutil", "detach", v.Device); err != nil {
			// WHO HELD IT IS READ BEFORE THE FORCE, which would close every
			// holder's files and leave nothing to name.
			holders := v.holders()
			if _, forceErr := v.m.Run("hdiutil", "detach", "-force", v.Device); forceErr != nil {
				detached = false
				errs = append(errs, fmt.Errorf("detach the RAM disk %s (held by: %s): %w; forced: %w", v.Device, holders, err, forceErr))
			} else {
				errs = append(errs, fmt.Errorf("the RAM disk %s detached only when forced, so something still held it (held by: %s): %w", v.Device, holders, err))
			}
		}
	}
	if detached {
		if err := os.Remove(v.Mount); err != nil && !errors.Is(err, os.ErrNotExist) {
			errs = append(errs, fmt.Errorf("remove the RAM disk's mount point %s: %w", v.Mount, err))
		}
	}
	if v.lock != nil {
		if !detached {
			errs = append(errs, fmt.Errorf("the RAM disk record %s is kept, so the next run reclaims %s", v.lock.Name(), v.Device))
		} else if err := os.Remove(v.lock.Name()); err != nil && !errors.Is(err, os.ErrNotExist) {
			errs = append(errs, fmt.Errorf("remove the RAM disk record %s: %w", v.lock.Name(), err))
		}
		if err := v.lock.Close(); err != nil {
			errs = append(errs, fmt.Errorf("close the RAM disk record: %w", err))
		}
		v.lock = nil
	}
	if len(errs) > 0 {
		v.m.Log.Infof("RAM disk release: failed after %s", v.m.Now().Sub(start))
	} else {
		v.m.Log.Infof("RAM disk release: %s released in %s", v.Device, v.m.Now().Sub(start))
	}
	return errors.Join(errs...)
}

// holders names the processes with a file open on the volume, as
// `lsof +f -- <mount>` lists them: "command[pid]" per process, in lsof's order.
// lsof answers exit 1 and no output when nothing is open there, which is said
// as such; any other failure of lsof is named in place of the holders, never
// dropped.
func (v *Volume) holders() string {
	out, err := v.m.Run("lsof", "-n", "-P", "+f", "--", v.Mount)
	names := holderNames(out)
	switch {
	case len(names) > 0:
		return strings.Join(names, ", ")
	case err != nil && strings.TrimSpace(out) == "":
		return "no process lsof could see"
	case err != nil:
		return fmt.Sprintf("unknown, lsof failed: %v", err)
	default:
		return "no process lsof could see"
	}
}

// holderNames reads `lsof` output into "command[pid]" names, one per process.
func holderNames(out string) []string {
	var names []string
	seen := map[string]bool{}
	for i, line := range strings.Split(out, "\n") {
		fields := strings.Fields(line)
		if i == 0 && len(fields) > 0 && fields[0] == "COMMAND" {
			continue
		}
		if len(fields) < 2 {
			continue
		}
		if _, err := strconv.Atoi(fields[1]); err != nil {
			continue
		}
		name := fields[0] + "[" + fields[1] + "]"
		if !seen[name] {
			seen[name] = true
			names = append(names, name)
		}
	}
	return names
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
	f, dead, err := m.probeRecord(path)
	if err != nil || !dead {
		return Reclaimed{}, false, err
	}
	defer f.Close()
	raw, err := os.ReadFile(path)
	if err != nil {
		return Reclaimed{}, false, fmt.Errorf("read the RAM disk record %s: %w", path, err)
	}
	device, mount := parseRecord(string(raw))
	r := Reclaimed{Pid: pid, Device: device, Mount: mount}
	if device != "" {
		info, err := m.listImages()
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
		// THE MOUNT POINT GOES WITH EVERYTHING UNDER IT. Once the disk is
		// detached, whatever the dead run left under its mount point sits on
		// the host's own filesystem (a write that landed after the volume went
		// away), and all of it is that dead run's. The record must name the
		// mount point a run of that pid is given, or nothing is removed: a
		// recursive removal never follows a record it cannot vouch for.
		if !filepath.IsAbs(mount) || filepath.Base(mount) != fmt.Sprintf("artr-%d", pid) {
			return r, false, fmt.Errorf("the RAM disk record %s names %q, which is no mount point a run of pid %d is given", path, mount, pid)
		}
		if err := os.RemoveAll(mount); err != nil {
			return r, false, fmt.Errorf("remove the dead run's mount point %s: %w", mount, err)
		}
	}
	if err := os.Remove(path); err != nil {
		return r, false, fmt.Errorf("remove the dead run's RAM disk record %s: %w", path, err)
	}
	return r, true, nil
}

// registryLockName is the registry's own lock: the one a record's creation and
// a reclaim's probe of a record both hold. It does not end in ".lock", so it is
// never read as a run's record.
const registryLockName = "registry.mutex"

// LockBound is how long a run waits for the registry lock before it takes
// the lock's holder to be stuck.
//
// MEASURED ON THE OWNER'S HOST (2026-10-12, load average 15-24). Uncontended,
// the flock is taken in p50 10us, max 64us (200 takes). Contended, a run waits
// out the other holder's hold:
//   - an acquire holds it for `hdiutil info` (at most InfoBound, 120ms) and
//     the 8 GiB attach up to its record's write (95ms and 97ms, read off two
//     live runs' records as their write time less their creation time):
//     about 220ms;
//   - an investigation holds it for `git worktree list` (max 142ms) and the
//     workspace CLI's dispatch (max 559ms): about 700ms, the longest hold.
//
// The bound is just over twice the longest hold, so a run queued behind a
// healthy investigation is never taken for one behind a stuck holder.
const LockBound = 1500 * time.Millisecond

// LsofBound is how long `lsof` may run when it names who has the registry lock
// open: measured 70-100ms warm and 260ms cold on the owner's host
// (2026-10-12, 23 calls at a load average of 24), and just over twice the max.
const LsofBound = 600 * time.Millisecond

// LockTimeoutError is a registry lock that was not taken within its bound.
type LockTimeoutError struct {
	Path  string
	Bound time.Duration
	// Holders names the other processes with the lock file open (its holder,
	// and any run waiting beside this one), as lsof saw them.
	Holders string
}

func (e *LockTimeoutError) Error() string {
	return fmt.Sprintf("the RAM disk registry lock %s was not taken within %s, so its holder is taken to be stuck (processes with it open: %s)", e.Path, e.Bound, e.Holders)
}

// lockRegistry takes the registry's lock, waiting at most m.LockBound, and
// answers its release.
//
// IT CLOSES THE WINDOW BETWEEN A RECORD'S CREATION AND ITS FLOCK. Acquire
// opens its record and only then flocks it; a concurrent run's Reclaim that
// opened the record in between found its flock free, read it empty, and
// removed it from under the live run, whose RAM disk then had no record a
// later Reclaim could find. Under the registry lock a record is never seen
// unlocked between its creation and its owner's flock.
//
// THE WAIT IS BOUNDED. A holder stuck in the kernel (2026-10-12: a run 16
// hours in its exit) would otherwise hang every later run. Past the bound the
// answer is a *LockTimeoutError; the flock still waiting is left to finish on
// its own and is let go the moment it is taken, so nothing is held after.
func (m Manager) lockRegistry() (func() error, error) {
	path := filepath.Join(m.Registry, registryLockName)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the RAM disk registry lock %s: %w", path, err)
	}
	taken := make(chan error, 1)
	go func() { taken <- syscall.Flock(int(f.Fd()), syscall.LOCK_EX) }()
	timer := time.NewTimer(m.LockBound)
	defer timer.Stop()
	select {
	case err := <-taken:
		if err != nil {
			f.Close()
			return nil, fmt.Errorf("take the RAM disk registry lock %s: %w", path, err)
		}
		return f.Close, nil
	case <-timer.C:
		go func() {
			<-taken
			f.Close()
		}()
		return nil, &LockTimeoutError{Path: path, Bound: m.LockBound, Holders: m.lockHolders(path)}
	}
}

// lockHolders names the processes other than this one with the lock file
// open, as lsof lists them within LsofBound; lsof's own failure is named in
// their place, never dropped.
func (m Manager) lockHolders(path string) string {
	out, err := m.Probe(LsofBound, "lsof", "-n", "-P", "--", path)
	var others []string
	for _, name := range holderNames(out) {
		if !strings.HasSuffix(name, fmt.Sprintf("[%d]", os.Getpid())) {
			others = append(others, name)
		}
	}
	switch {
	case len(others) > 0:
		return strings.Join(others, ", ")
	case err != nil && strings.TrimSpace(out) != "":
		return fmt.Sprintf("unknown, lsof failed: %v", err)
	case err != nil && !strings.Contains(err.Error(), "exit status 1"):
		return fmt.Sprintf("unknown, lsof failed: %v", err)
	default:
		return "no other process lsof could see"
	}
}

// createRecordLocked creates this run's record and takes its flock. The
// caller holds the registry lock, so the record is never seen unlocked.
func (m Manager) createRecordLocked(recordPath string) (*os.File, error) {
	lock, err := os.OpenFile(recordPath, os.O_CREATE|os.O_RDWR|os.O_TRUNC, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the RAM disk record %s: %w", recordPath, err)
	}
	if err := syscall.Flock(int(lock.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		lock.Close()
		return nil, fmt.Errorf("take the RAM disk record %s: %w", recordPath, err)
	}
	return lock, nil
}

// probeRecord opens one record and takes its flock without blocking, under the
// registry lock. dead is false when a live run holds it, or when the path no
// longer names the file whose flock was taken (its run released it between the
// open and the flock). The caller closes the file it answers.
func (m Manager) probeRecord(path string) (_ *os.File, dead bool, err error) {
	unlock, err := m.lockRegistry()
	if err != nil {
		return nil, false, err
	}
	defer func() {
		if closeErr := unlock(); closeErr != nil {
			err = errors.Join(err, fmt.Errorf("release the RAM disk registry lock: %w", closeErr))
		}
	}()
	return probeRecordLocked(path)
}

// probeRecordLocked is probeRecord for a caller that holds the registry lock.
func probeRecordLocked(path string) (_ *os.File, dead bool, err error) {
	f, err := os.OpenFile(path, os.O_RDWR, 0)
	if errors.Is(err, os.ErrNotExist) {
		return nil, false, nil
	}
	if err != nil {
		return nil, false, fmt.Errorf("open the RAM disk record %s: %w", path, err)
	}
	switch err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); {
	case errors.Is(err, syscall.EWOULDBLOCK):
		f.Close()
		return nil, false, nil
	case err != nil:
		f.Close()
		return nil, false, fmt.Errorf("probe the RAM disk record %s: %w", path, err)
	}
	// A RECORD ITS LIVE RUN RELEASED between the open and the flock is no
	// longer at the path: the flock was taken on an unlinked file, which says
	// nothing about a dead run.
	held, err := f.Stat()
	if err != nil {
		f.Close()
		return nil, false, fmt.Errorf("stat the RAM disk record %s: %w", path, err)
	}
	named, err := os.Stat(path)
	if errors.Is(err, os.ErrNotExist) || (err == nil && !os.SameFile(held, named)) {
		f.Close()
		return nil, false, nil
	}
	if err != nil {
		f.Close()
		return nil, false, fmt.Errorf("stat the RAM disk record %s: %w", path, err)
	}
	return f, true, nil
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

// image is one attached disk image as `hdiutil info` lists it.
type image struct {
	// Path is its image-path: a ram:// URL for a RAM disk.
	Path string
	// Devices are its device lines' devices, the whole disk first.
	Devices []string
	// Mounts are the mount points its device lines name.
	Mounts []string
}

func (img image) isRAM() bool { return strings.HasPrefix(img.Path, "ram://") }

// imageSeparator starts each image's block in `hdiutil info`.
const imageSeparator = "================================================"

// parseImages reads `hdiutil info` into its images. Each image's block starts
// at a line of '=' and carries its image-path line and its device lines,
// "<device>\t<content hint>\t<mount point>", the mount point only when one is.
func parseImages(info string) []image {
	var images []image
	for i, block := range strings.Split(info, imageSeparator) {
		if i == 0 {
			continue
		}
		var img image
		for _, line := range strings.Split(block, "\n") {
			if v, ok := strings.CutPrefix(strings.TrimSpace(line), "image-path"); ok {
				img.Path = strings.TrimSpace(strings.TrimPrefix(strings.TrimSpace(v), ":"))
				continue
			}
			cols := strings.Split(line, "\t")
			device := strings.TrimSpace(cols[0])
			if !strings.HasPrefix(device, "/dev/disk") {
				continue
			}
			img.Devices = append(img.Devices, device)
			if len(cols) >= 3 {
				if mount := strings.TrimSpace(cols[2]); mount != "" {
					img.Mounts = append(img.Mounts, mount)
				}
			}
		}
		images = append(images, img)
	}
	return images
}

// isAttachedRAMDisk reports whether `hdiutil info` lists device as a device of
// an image whose path is a ram:// URL.
func isAttachedRAMDisk(info, device string) bool {
	for _, img := range parseImages(info) {
		if img.isRAM() && slices.Contains(img.Devices, device) {
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
