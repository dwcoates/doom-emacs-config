package daemonaddr

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
	"time"
)

// LockName is the boot-exclusivity lock file beside daemon.addr.
const LockName = "daemon.lock"

// ErrClaimed reports that another daemon already holds the boot claim. An
// unflagged second daemon exits on this without touching the incumbent's
// listener or its daemon.addr; a successor is distinguishable because it was
// SPAWNED with the joining argument, not because it raced and lost.
var ErrClaimed = errors.New("another daemon holds the boot claim")

// ClaimWaitBound is how long a booting daemon WAITS for a held boot claim
// before deciding the incumbent is genuinely still serving.
//
// THE CLAIM, NOT THE ADDRESS FILE, IS WHAT "THE PREVIOUS DAEMON IS GONE"
// MEANS. daemon.addr is now withdrawn at the START of the shutdown sequence,
// so that a client forwarding during shutdown finds no address; the claim is
// released only when the process actually exits. A replacement spawned into
// that window used to check once, find the claim held, and exit -- and the
// restart destroyed the daemon instead of replacing it (2026-09-12: the
// incumbent pid 57345 withdrew at 12:43:45.817 and ended at 12:43:47.830,
// while its replacement pid 17381 checked and exited at 12:43:45.909, 93ms
// into a 2.013s window).
//
// THE BOUND COMES FROM THE OBSERVED SHUTDOWN COST, not from a round number.
// A healthy orderly exit is dominated by one fixed term, the merge terminal
// drain (merge.TerminalDrainBound, 2s), which the run log shows being paid in
// full on the ordinary path: announce-to-exit measured 2.013s on 2026-09-10
// and 2.014s on 2026-09-12. Everything else in the teardown is the residue,
// measured at 14ms, 26ms and 80ms across those same shutdowns. So the bound
// is the fixed drain term plus four times the worst observed residue:
// 2.000s + 4*80ms. A daemon still holding the claim after that is not
// shutting down, it is serving, and ErrClaimed is the right answer for it.
//
// THE EXIT CAN ALSO WAIT OUT A MERGE'S GIT (merge.MergeGitStopBound, 10s):
// the drain never kills a merge's git command halfway, so a shutdown that
// lands while one runs waits for it to end (2026-10-06, a merge always
// resumes where it left off). That wait is paid only when a command is in
// flight -- sub-second on a healthy machine -- but a replacement must outlast
// its bound too, or the restart that met it would destroy the daemon. The
// wait costs nothing when no daemon holds the claim.
//
// It is checked against merge.TerminalDrainBound and merge.MergeGitStopBound
// by a test in the daemon command, which is the one package that may see all
// three.
const ClaimWaitBound = 2*time.Second + 10*time.Second + 320*time.Millisecond

// LockPath is the boot lock's path for a given daemon.addr path. The lock
// lives beside the advertisement because they are the same claim: the file
// says who to talk to, the lock says who may say it.
func LockPath(addrPath string) string {
	return filepath.Join(filepath.Dir(addrPath), LockName)
}

// bootLock is an exclusive, non-blocking kernel lock on the boot lock file.
//
// The lock, not the bind, is the exclusivity: a port-0 bind hands every
// racing daemon a different free port and so arbitrates nothing. flock does
// arbitrate, and the kernel releases it on process death, so a daemon that
// was force-killed leaves the claim free rather than a stale file that has to
// be reasoned about.
type bootLock struct {
	f    *os.File
	path string
}

// openBootLock opens the lock file without attempting to lock it. Failing to
// open it is never read as the claim being free: it is an undecided claim.
func openBootLock(path string) (*os.File, error) {
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("open the boot lock %q: %w", path, err)
	}
	return f, nil
}

// acquireBootLock takes the claim IMMEDIATELY, or returns ErrClaimed if
// another process holds it right now. Any other error means the claim could
// not be decided and is never read as free.
//
// This is the single-shot form. A successor taking over at Publish uses it
// unchanged -- it is not racing anybody, the incumbent has already stood down
// -- and so does ProbeBootClaim, whose whole question is "is it held NOW".
func acquireBootLock(path string) (*bootLock, error) {
	f, err := openBootLock(path)
	if err != nil {
		return nil, err
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, fmt.Errorf("%w: %s", ErrClaimed, path)
		}
		return nil, fmt.Errorf("take the boot lock %q: %w", path, err)
	}
	return &bootLock{f: f, path: path}, nil
}

// acquireBootLockWithin takes the claim, waiting up to WAIT for an incumbent
// that is on its way out to release it. A wait of zero or less is the
// single-shot acquireBootLock.
//
// The wait is a BLOCKING flock on its own goroutine, arbitrated by the
// kernel, raced against a timer -- not a poll and not a sleep. A blocking
// flock that lands after the bound has already been reported is released at
// once by the drain goroutine, so this never leaves a claim held by a caller
// that was told it lost.
//
// onRefused is a TEST SEAM, called once after the first refusal and before
// the blocking wait begins, so a test can release the incumbent at exactly
// the point the wait is under way. Production passes nil.
func acquireBootLockWithin(path string, wait time.Duration, onRefused func()) (*bootLock, error) {
	lock, err := acquireBootLock(path)
	if err == nil || !errors.Is(err, ErrClaimed) || wait <= 0 {
		return lock, err
	}
	if onRefused != nil {
		onRefused()
	}
	type outcome struct {
		lock *bootLock
		err  error
	}
	// THE FILE IS OPENED HERE, BEFORE THE WAIT GOES TO ITS GOROUTINE. The
	// open creates the lock file when it is absent, and a goroutine that
	// opened it late -- after this call had answered and its caller had torn
	// the state root down -- recreated a file in a directory being removed.
	// Everything the goroutine does after this point is on a descriptor it
	// already holds, so nothing it does outlives the answer on disk.
	f, err := openBootLock(path)
	if err != nil {
		return nil, err
	}
	settled := make(chan outcome, 1)
	go func() {
		l, e := blockForBootLock(f, path)
		settled <- outcome{lock: l, err: e}
	}()
	timer := time.NewTimer(wait)
	defer timer.Stop()
	select {
	case got := <-settled:
		return got.lock, got.err
	case <-timer.C:
		// THE LATE WINNER IS RELEASED, NEVER LEAKED. The blocking attempt is
		// still queued in the kernel; if it lands after this answer it hands
		// back a claim nobody asked for any more.
		go func() {
			if got := <-settled; got.lock != nil {
				got.lock.release()
			}
		}()
		return nil, fmt.Errorf("%w: %s (still held after %s)", ErrClaimed, path, wait)
	}
}

// blockForBootLock waits in the kernel for the claim on F, an already open
// descriptor of the lock file at PATH, and takes ownership of F. It returns
// only when the lock is taken or the attempt fails outright.
//
// It takes an open descriptor rather than a path so that a caller running it
// on a goroutine opens -- and so creates -- the lock file synchronously: the
// goroutine itself never touches the filesystem by name, and a wait that is
// abandoned cannot recreate the file after its owner has gone.
func blockForBootLock(f *os.File, path string) (*bootLock, error) {
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		f.Close()
		return nil, fmt.Errorf("wait for the boot lock %q: %w", path, err)
	}
	return &bootLock{f: f, path: path}, nil
}

// ProbeBootClaim answers whether the boot claim beside addrPath is free RIGHT
// NOW: nil when it is, ErrClaimed when a live process holds it, and any other
// error when the claim could not be decided at all.
//
// AN UNDECIDED CLAIM IS NOT A FREE ONE. The third answer exists precisely so
// a caller cannot read a failure to look as permission to proceed.
//
// It takes the claim and releases it again, which is the only way to ask the
// kernel the question. That is safe for a caller that is merely watching a
// daemon depart: holding it for the length of one flock pair cannot make a
// departing daemon's exit any different, and a booting daemon that collides
// with the probe waits ClaimWaitBound rather than exiting.
func ProbeBootClaim(addrPath string) error {
	lock, err := acquireBootLock(LockPath(addrPath))
	if err != nil {
		return err
	}
	return lock.release()
}

// release drops the claim. The lock file itself is left in place: removing it
// would let a racing daemon create a second inode and take a lock nobody else
// can see.
func (l *bootLock) release() error {
	if l.f == nil {
		return nil
	}
	f := l.f
	l.f = nil
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
		f.Close()
		return fmt.Errorf("release the boot lock %q: %w", l.path, err)
	}
	if err := f.Close(); err != nil {
		return fmt.Errorf("close the boot lock %q: %w", l.path, err)
	}
	return nil
}

// verify reports whether the lock file at the lock's path is still the inode
// this lock holds. A lock file that is gone, or was replaced by another file,
// is a claim nobody else can see: a second daemon opening the path creates or
// opens a different inode and takes a lock that does not conflict with this
// one.
func (l *bootLock) verify() error {
	if l.f == nil {
		return fmt.Errorf("the boot lock %q is released", l.path)
	}
	held, err := l.f.Stat()
	if err != nil {
		return fmt.Errorf("stat the held boot lock %q: %w", l.path, err)
	}
	named, err := os.Stat(l.path)
	switch {
	case os.IsNotExist(err):
		return fmt.Errorf("%w: daemon.lock %q is gone", ErrVanished, l.path)
	case err != nil:
		return fmt.Errorf("stat the boot lock %q: %w", l.path, err)
	case !os.SameFile(held, named):
		return fmt.Errorf("%w: daemon.lock %q was replaced by another file", ErrVanished, l.path)
	}
	return nil
}
