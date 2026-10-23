// Package recordlock is the daemon's one kernel lock: a whole-file record
// lock (fcntl), held by the PROCESS that took it. The boot claim and the
// serving lock (daemonaddr) go through it.
//
// A RECORD LOCK, NOT AN flock, because a record lock belongs to a process and
// an flock to an open file description. An flock is held for as long as ANY
// descriptor of its open file description exists, and every child the daemon
// forks gets one: the descriptors are O_CLOEXEC, so they close at the child's
// exec, but between its fork and its exec the child holds the lock, and on
// Darwin past the exec too, through the pre-exec image's shadow proc, which
// closes its copies only when its own exit finishes (bsd/kern/kern_exec.c
// proc_exec_switch_task) and which nothing in user space can see or await. A
// daemon killed while it forked a child went on "holding" its locks through
// that child; a fresh daemon on the same state root read its serving lock as
// an incumbent serving and refused to boot (2026-10-22,
// TestCrashBootWithADeadManifestPidTakesTheOrdinaryDeadShimPath).
//
// A record lock is never inherited by a forked child, and the kernel drops it
// when its owning process exits, whatever descriptors its children still
// hold. So a dead daemon holds nothing, structurally.
//
// THE TWO KINDS EXCLUDE EACH OTHER ON DARWIN, which is what keeps a rolling
// deploy safe across the build that changes the kind: an incumbent holding a
// flock refuses a successor's record lock, and the reverse (measured
// 2026-10-22, both directions EAGAIN). Linux keeps the two independent, and no
// Linux host runs two builds of the daemon on one state root.
//
// A RECORD LOCK HAS ONE HAZARD A FLOCK DOES NOT: closing ANY descriptor of
// the file drops every record lock this process holds on it, and two
// descriptors in one process never conflict. So this process's own holders
// are arbitrated in-process, per file, by gateFor's lock, and the rule every
// function here keeps is: a descriptor of a lock file is closed only by the
// party holding that file's gate exclusively, or by a shared holder while no
// exclusive holder can exist. That is also what keeps the in-process
// exclusivity the callers (and their tests) rely on: a second take in one
// process is refused, as a second flock was. A lock file is therefore opened
// in the daemon ONLY through this package.
package recordlock

import (
	"errors"
	"fmt"
	"os"
	"sync"
	"syscall"
)

// gates is this process's arbitration of its own holders, one per lock file,
// keyed by the file's identity rather than its path, so two spellings of one
// path share a gate.
var (
	gatesMu sync.Mutex
	gates   = map[fileKey]*sync.RWMutex{}
)

// fileKey is a lock file's identity: its device and inode.
type fileKey struct {
	dev uint64
	ino uint64
}

// gateFor answers the in-process gate of the lock file at PATH, creating the
// file when it is absent. A file created here is opened and closed at once:
// no process can hold a lock on an inode that did not exist, so the close
// drops nothing.
func gateFor(path string) (*sync.RWMutex, error) {
	gatesMu.Lock()
	defer gatesMu.Unlock()
	info, err := os.Stat(path)
	if errors.Is(err, os.ErrNotExist) {
		f, cerr := OpenFile(path)
		if cerr != nil {
			return nil, cerr
		}
		info, err = f.Stat()
		f.Close()
	}
	if err != nil {
		return nil, err
	}
	return gateLocked(info, path)
}

// gateOfFile answers the in-process gate of the lock file F is open on,
// naming no path, so it never creates one.
func gateOfFile(f *os.File, path string) (*sync.RWMutex, error) {
	info, err := f.Stat()
	if err != nil {
		return nil, err
	}
	gatesMu.Lock()
	defer gatesMu.Unlock()
	return gateLocked(info, path)
}

// gateLocked answers the gate of the file INFO describes. gatesMu is held.
func gateLocked(info os.FileInfo, path string) (*sync.RWMutex, error) {
	st, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return nil, fmt.Errorf("the lock file %q has no device and inode", path)
	}
	key := fileKey{dev: uint64(st.Dev), ino: uint64(st.Ino)}
	gate, ok := gates[key]
	if !ok {
		gate = &sync.RWMutex{}
		gates[key] = gate
	}
	return gate, nil
}

// OpenFile opens the lock file at PATH for reading and writing, creating it
// when absent but never its directory: THE one open every lock file goes
// through, so every descriptor is one a record lock can be taken on.
func OpenFile(path string) (*os.File, error) {
	return os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
}

// wholeFile is a record lock request of TYPE over the whole file.
func wholeFile(typ int16) *syscall.Flock_t {
	return &syscall.Flock_t{Type: typ, Whence: 0, Start: 0, Len: 0}
}

// ErrWouldBlock reports a lock another holder has: another process, or
// another holder in this one.
var ErrWouldBlock = errors.New("the lock is held")

// recordLocked reports whether ERR is the kernel refusing a lock another
// process holds.
func recordLocked(err error) bool {
	return errors.Is(err, syscall.EAGAIN) || errors.Is(err, syscall.EACCES)
}

// Holder is the process the kernel names as holding a lock.
type Holder struct {
	// PID is the holding process, or zero when the kernel named none: the
	// lock was released at the instant it was asked about, or its holder
	// took it with a flock, which belongs to no one process (a daemon of a
	// build from before the record locks).
	PID int
}

// HolderOn answers the process holding a lock that conflicts with an
// exclusive one, on F, a descriptor of the lock file. It asks the kernel and
// takes nothing, so it never disturbs a wait queued on F.
func HolderOn(f *os.File) (Holder, error) {
	probe := wholeFile(syscall.F_WRLCK)
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_GETLK, probe); err != nil {
		return Holder{}, err
	}
	if probe.Type == syscall.F_UNLCK || probe.Pid <= 0 {
		return Holder{}, nil
	}
	return Holder{PID: int(probe.Pid)}, nil
}

// Record is one exclusive record lock this process holds, with the gate that
// arbitrates it in-process.
type Record struct {
	f    *os.File
	gate *sync.RWMutex
	path string
}

// TryExclusive takes the exclusive lock on the file at PATH at once, creating
// the file but never its directory, or answers ErrWouldBlock with the holder
// the kernel names. A holder in THIS process is named as this process. Any
// other error is undecided.
func TryExclusive(path string) (*Record, Holder, error) {
	gate, err := gateFor(path)
	if err != nil {
		return nil, Holder{}, err
	}
	// THE GATE IS TAKEN BEFORE ANY DESCRIPTOR IS OPENED: a descriptor opened
	// and closed while another holder in this process held the lock would
	// drop that holder's lock.
	if !gate.TryLock() {
		return nil, Holder{PID: os.Getpid()}, ErrWouldBlock
	}
	f, err := OpenFile(path)
	if err != nil {
		gate.Unlock()
		return nil, Holder{}, err
	}
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_WRLCK)); err != nil {
		var holder Holder
		if recordLocked(err) {
			// NAMED AT THE REFUSAL, on the descriptor that was refused, so
			// the holder is the one that refused it, not whoever holds the
			// lock a moment later.
			holder, _ = HolderOn(f)
		}
		f.Close()
		gate.Unlock()
		if recordLocked(err) {
			return nil, holder, ErrWouldBlock
		}
		return nil, Holder{}, err
	}
	return &Record{f: f, gate: gate, path: path}, Holder{}, nil
}

// BlockExclusive waits for the exclusive lock on F, an already open
// descriptor of the lock file at PATH, and takes ownership of F: it waits
// first for every holder in this process, then in the kernel for every other
// process. F is closed on any failure.
func BlockExclusive(f *os.File, path string) (*Record, error) {
	// THE GATE IS F'S OWN FILE'S, read from the descriptor: the path may name
	// another inode by now, or none, and a wait never names it again. A
	// descriptor that cannot be stated is open on no file, so closing it
	// drops no lock.
	gate, err := gateOfFile(f, path)
	if err != nil {
		f.Close()
		return nil, err
	}
	gate.Lock()
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLKW, wholeFile(syscall.F_WRLCK)); err != nil {
		f.Close()
		gate.Unlock()
		return nil, err
	}
	return &Record{f: f, gate: gate, path: path}, nil
}

// Path is the lock file, for the log record.
func (r *Record) Path() string { return r.path }

// Stat answers the held file's own stat, which a caller compares with what
// its path names now (os.SameFile) to tell a lock file that was replaced.
func (r *Record) Stat() (os.FileInfo, error) {
	if r == nil || r.f == nil {
		return nil, fmt.Errorf("the lock %s is released", r.Path())
	}
	return r.f.Stat()
}

// Release releases the lock and closes its descriptor, then lets the next
// holder in this process have the gate. Releasing twice, or releasing a nil
// record, is safe, because a teardown runs on both the ordinary and the
// failing path.
func (r *Record) Release() error {
	if r == nil || r.f == nil {
		return nil
	}
	f := r.f
	r.f = nil
	uerr := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_UNLCK))
	cerr := f.Close()
	r.gate.Unlock()
	if uerr != nil {
		return fmt.Errorf("unlocking %s: %w", r.path, uerr)
	}
	return cerr
}

// ProbeShared answers whether some holder has an EXCLUSIVE lock on the file
// at PATH right now, by asking for a shared one and releasing it at once: two
// probes never refuse each other. A holder in this process counts as held.
// The holder the kernel names rides with a held answer.
func ProbeShared(path string) (bool, Holder, error) {
	gate, err := gateFor(path)
	if err != nil {
		return false, Holder{}, err
	}
	if !gate.TryRLock() {
		return true, Holder{PID: os.Getpid()}, nil
	}
	defer gate.RUnlock()
	f, err := OpenFile(path)
	if err != nil {
		return false, Holder{}, err
	}
	// Closed under the shared gate: no exclusive holder in this process can
	// exist, so the close can drop only another probe's shared lock, which
	// excludes nothing.
	defer f.Close()
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_RDLCK)); err != nil {
		if recordLocked(err) {
			holder, _ := HolderOn(f)
			return true, holder, nil
		}
		return false, Holder{}, err
	}
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_UNLCK)); err != nil {
		return false, Holder{}, err
	}
	return false, Holder{}, nil
}

// HolderOf answers the process holding the lock at PATH right now, without
// taking it. A holder in this process is named as this process.
//
// A LOOKUP CREATES NOTHING. A lock file that does not exist is held by no one,
// and making it here would leave a lock file wherever a path was merely asked
// about.
func HolderOf(path string) (Holder, error) {
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		return Holder{}, nil
	}
	gate, err := gateFor(path)
	if err != nil {
		return Holder{}, err
	}
	if !gate.TryRLock() {
		return Holder{PID: os.Getpid()}, nil
	}
	defer gate.RUnlock()
	f, err := OpenFile(path)
	if err != nil {
		return Holder{}, err
	}
	defer f.Close()
	return HolderOn(f)
}
