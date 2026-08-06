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
// So the shim takes TWO exclusive locks at startup — one keyed by session id,
// one keyed by workspace directory — and holds both for its lifetime. The
// session key names one conversation attempt; the workspace key names the thing
// the invariant is about, and only it catches two daemon session ids pointed at
// one workspace over one transcript (WorkspaceLockPath). Each lock is:
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
	"crypto/md5"
	"encoding/hex"
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

// WorkspaceKey is a workspace directory's identity in a file name: the first
// eight hex digits of the MD5 of its cleaned absolute path.
//
// It is the SAME derivation as the workspace_id on every canonical log record
// (dlog.WorkspaceFromDirectory, and the shim's own log.ts), so a lock file is
// greppable against the log lines of the shim holding it. The shim recomputes
// it in Node from the --cwd the daemon hands it — already symlink-resolved and
// absolute — and both sides normalize with their platform's path cleaner, so
// one workspace derives one path on both sides.
func WorkspaceKey(cwd string) string {
	sum := md5.Sum([]byte(filepath.Clean(cwd)))
	return hex.EncodeToString(sum[:])[:8]
}

// WorkspaceLockPath returns the lock file for the workspace rooted at cwd.
//
// It is a SECOND lock, held alongside the session lock rather than instead of
// it, because the two answer different questions. A session id names one
// daemon-side conversation attempt; a workspace names the thing the invariant
// is about — a workspace and each resumed transcript keep exactly one live
// session at a time. Two daemon session ids can point at one workspace over one
// vendor transcript, and each would take its own session lock and exclude
// nothing.
//
// The key is the CWD rather than the vendor transcript uuid because a FRESH
// session has no transcript yet, so a transcript key cannot cover the window in
// which a duplicate gets spawned.
func WorkspaceLockPath(cwd string) (string, error) {
	if cwd == "" {
		return "", fmt.Errorf("sessionlock: a workspace lock path needs a workspace directory, got an empty one")
	}
	dir, err := Dir()
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, "workspace-"+WorkspaceKey(cwd)+".lock"), nil
}

// WorkspaceLockHeld reports whether a live process holds the workspace's lock —
// i.e. whether a shim is alive for that workspace, INCLUDING one that has not
// dialled in yet. It answers exactly as Held does, and its errors mean the same
// thing: "I could not tell", which a caller must never read as free.
func WorkspaceLockHeld(cwd string) (bool, error) {
	path, err := WorkspaceLockPath(cwd)
	if err != nil {
		return false, err
	}
	return heldAt(path)
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
