package daemonaddr

import (
	"errors"
	"fmt"
	"os"
	"sync"
	"syscall"
)

// THE BOOT CLAIM AND THE SERVING LOCK ARE WHOLE-FILE RECORD LOCKS (fcntl),
// NOT flocks, because a record lock belongs to a PROCESS and a flock to an
// open file description.
//
// A flock is held for as long as ANY descriptor of its open file description
// exists, and every child the daemon forks gets one: the descriptors are
// O_CLOEXEC, so they close at the child's exec, but between its fork and its
// exec the child holds the claim. A child spawned into its own process group
// or session (a shim, a script, a successor) leaves the daemon's group in
// that window, before its exec, so a group kill cannot reach it, and a daemon
// that was killed went on "holding" its boot claim and serving lock through
// a child it had not finished spawning. A fresh daemon on the same state root
// read that as its incumbent serving and refused to boot
// (2026-10-22, TestCrashBootWithADeadManifestPidTakesTheOrdinaryDeadShimPath).
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
// exclusivity the package's callers (and its tests) rely on: a second take in
// one process is refused as ErrClaimed, as a second flock was.

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
		f, cerr := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
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

// wholeFile is a record lock request of TYPE over the whole file.
func wholeFile(typ int16) *syscall.Flock_t {
	return &syscall.Flock_t{Type: typ, Whence: 0, Start: 0, Len: 0}
}

// errWouldBlock reports a lock another holder has: another process, or
// another holder in this one.
var errWouldBlock = errors.New("the lock is held")

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

// holderOn answers the process holding a lock that conflicts with an
// exclusive one, on F, a descriptor of the lock file this process holds the
// file's gate for.
func holderOn(f *os.File) (Holder, error) {
	probe := wholeFile(syscall.F_WRLCK)
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_GETLK, probe); err != nil {
		return Holder{}, err
	}
	if probe.Type == syscall.F_UNLCK || probe.Pid <= 0 {
		return Holder{}, nil
	}
	return Holder{PID: int(probe.Pid)}, nil
}

// heldRecord is one exclusive record lock this process holds, with the gate
// that arbitrates it in-process.
type heldRecord struct {
	f    *os.File
	gate *sync.RWMutex
}

// tryExclusive takes the exclusive lock on the file at PATH at once, or
// answers errWouldBlock with the holder the kernel names. A holder in THIS
// process is named as this process. Any other error is undecided.
func tryExclusive(path string) (*heldRecord, Holder, error) {
	gate, err := gateFor(path)
	if err != nil {
		return nil, Holder{}, err
	}
	// THE GATE IS TAKEN BEFORE ANY DESCRIPTOR IS OPENED: a descriptor opened
	// and closed while another holder in this process held the lock would
	// drop that holder's lock.
	if !gate.TryLock() {
		return nil, Holder{PID: os.Getpid()}, errWouldBlock
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
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
			holder, _ = holderOn(f)
		}
		f.Close()
		gate.Unlock()
		if recordLocked(err) {
			return nil, holder, errWouldBlock
		}
		return nil, Holder{}, err
	}
	return &heldRecord{f: f, gate: gate}, Holder{}, nil
}

// blockExclusive waits for the exclusive lock on F, an already open
// descriptor of the lock file at PATH, and takes ownership of F: it waits
// first for every holder in this process, then in the kernel for every other
// process. F is closed on any failure.
func blockExclusive(f *os.File, path string) (*heldRecord, error) {
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
	return &heldRecord{f: f, gate: gate}, nil
}

// unlock releases the lock and closes its descriptor, then lets the next
// holder in this process have the gate. It is called once.
func (h *heldRecord) unlock() error {
	uerr := syscall.FcntlFlock(h.f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_UNLCK))
	cerr := h.f.Close()
	h.gate.Unlock()
	if uerr != nil {
		return uerr
	}
	return cerr
}

// probeShared answers whether some holder has an EXCLUSIVE lock on the file
// at PATH right now, by asking for a shared one and releasing it at once: two
// probes never refuse each other. A holder in this process counts as held.
// The holder the kernel names rides with a held answer.
func probeShared(path string) (bool, Holder, error) {
	gate, err := gateFor(path)
	if err != nil {
		return false, Holder{}, err
	}
	if !gate.TryRLock() {
		return true, Holder{PID: os.Getpid()}, nil
	}
	defer gate.RUnlock()
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return false, Holder{}, err
	}
	// Closed under the shared gate: no exclusive holder in this process can
	// exist, so the close can drop only another probe's shared lock, which
	// excludes nothing.
	defer f.Close()
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_RDLCK)); err != nil {
		if recordLocked(err) {
			holder, _ := holderOn(f)
			return true, holder, nil
		}
		return false, Holder{}, err
	}
	if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_UNLCK)); err != nil {
		return false, Holder{}, err
	}
	return false, Holder{}, nil
}

// holderOf answers the process holding the lock at PATH right now, without
// taking it. A holder in this process is named as this process.
func holderOf(path string) (Holder, error) {
	gate, err := gateFor(path)
	if err != nil {
		return Holder{}, err
	}
	if !gate.TryRLock() {
		return Holder{PID: os.Getpid()}, nil
	}
	defer gate.RUnlock()
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return Holder{}, err
	}
	defer f.Close()
	return holderOn(f)
}

// LockHolders answers the processes the kernel names as holding the boot
// claim and the serving lock beside addrPath right now, without taking
// either. A zero PID is a lock nobody holds, or one held by a flock (see
// Holder). A holder in this process is named as this process.
func LockHolders(addrPath string) (boot, serving Holder, err error) {
	if boot, err = holderOf(LockPath(addrPath)); err != nil {
		return Holder{}, Holder{}, fmt.Errorf("name the boot claim's holder beside %q: %w", addrPath, err)
	}
	if serving, err = holderOf(ServingPath(addrPath)); err != nil {
		return Holder{}, Holder{}, fmt.Errorf("name the serving lock's holder beside %q: %w", addrPath, err)
	}
	return boot, serving, nil
}
