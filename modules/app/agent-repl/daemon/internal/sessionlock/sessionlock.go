// Package sessionlock answers "is a shim alive for this session?" with a
// kernel-enforced lock rather than bookkeeping the daemon has to get right.
//
// # Why this exists
//
// While each shim listened on its own session-<id>.sock, uniqueness was free:
// only one process can bind a path, so a second shim for the same session was
// unreachable — bind() returned EADDRINUSE and it died. Once shims dial OUT to
// the daemon instead (design-shim-transport-inversion.md), nothing stops two
// processes claiming one session, and two shims on one conversation means two
// writers on one transcript.
//
// Connection tracking alone cannot close this. On a fresh daemon boot a
// surviving shim may not have dialled in yet, so "do I have a connection for
// this session?" answers NO when the truth is NOT YET, and the daemon would
// spawn a duplicate of a shim that is alive and mid-turn.
//
// So the shim takes an exclusive lock on its session at startup and holds it
// for its lifetime. The lock is:
//
//   - kernel-enforced, so exclusion is not advisory bookkeeping;
//   - released automatically when the holder dies, however it dies, so there
//     is no stale state to reap and no PID-reuse hazard;
//   - testable synchronously, which is what covers the boot window that
//     connection tracking misses.
//
// # Platform
//
// BSD flock(2) semantics. The shim side takes the same lock from Node via
// open(2)'s O_EXLOCK, which is the same underlying lock on macOS/BSD, so the
// two interoperate. Linux has no O_EXLOCK, so the shim cannot take this lock
// there without native code — Held reports that loudly rather than pretending
// a session is free.
package sessionlock

import (
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// Dir returns the directory session locks live in: a sibling of sock/ and
// store/ under the agent-repl cache root. A lock is not a socket, so it does
// not live among them — the same split as /var/run.
func Dir() (string, error) {
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("sessionlock: resolving home dir: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "run"), nil
}

// Path returns the lock file for sessionID.
func Path(sessionID string) (string, error) {
	dir, err := Dir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, "session-"+sessionID+".lock"), nil
}

// EnsureDir creates the lock directory. Called at daemon boot alongside the
// other runtime dirs; the shim needs it to exist before it can take a lock.
func EnsureDir() error {
	dir, err := Dir()
	if err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return fmt.Errorf("sessionlock: creating %s: %w", dir, err)
	}
	return nil
}

// Held reports whether a live process holds sessionID's lock — i.e. whether a
// shim is alive for that session, INCLUDING one that has not dialled in yet.
//
// It answers by trying to take the lock non-blockingly: success means nobody
// held it (the lock is released again immediately), failure with EWOULDBLOCK
// means someone does. A missing lock file means no shim has ever locked this
// session, which is a free session, not an error.
//
// Any other error is returned rather than guessed at: the caller must not read
// "I could not tell" as "free" and spawn a duplicate.
func Held(sessionID string) (bool, error) {
	return heldAt(pathOrEmpty(sessionID))
}

// HeldAt is Held against an explicit path (tests, and callers that already
// resolved it).
func HeldAt(path string) (bool, error) { return heldAt(path) }

func pathOrEmpty(sessionID string) string {
	p, err := Path(sessionID)
	if err != nil {
		return ""
	}
	return p
}

func heldAt(path string) (bool, error) {
	if path == "" {
		return false, fmt.Errorf("sessionlock: empty lock path")
	}
	f, err := os.OpenFile(path, os.O_RDWR, 0o644)
	if err != nil {
		if os.IsNotExist(err) {
			// No lock file: no shim has ever claimed this session.
			return false, nil
		}
		return false, fmt.Errorf("sessionlock: opening %s: %w", path, err)
	}
	defer f.Close()

	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if err == nil {
		// We got it, so nobody held it. Release immediately — this is a probe,
		// not a claim; the daemon never holds a session lock (it must survive
		// the daemon, not track it).
		_ = syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
		return false, nil
	}
	if err == syscall.EWOULDBLOCK {
		return true, nil
	}
	return false, fmt.Errorf("sessionlock: probing %s: %w", path, err)
}
