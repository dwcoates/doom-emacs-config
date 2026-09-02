package main

import (
	"crypto/md5"
	"encoding/hex"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// EnvLockDir redirects the kernel-lock directory so a test never touches the
// real ~/.cache/agent-repl/run.
const EnvLockDir = "AGENT_REPL_LOCK_DIR"

// LockDir resolves the directory holding the shim's kernel locks: the
// AGENT_REPL_LOCK_DIR override when set, else the contracted
// ~/.cache/agent-repl/run.
func LockDir(env func(string) string, home string) string {
	if d := env(EnvLockDir); d != "" {
		return d
	}
	return filepath.Join(home, ".cache", "agent-repl", "run")
}

// WorkspaceLockPath derives the workspace lock's path from the workspace's
// absolute directory, per the contract:
// workspace-<md5hex(filepath.Clean(absDir))[:8]>.lock.
func WorkspaceLockPath(dir, absDir string) string {
	sum := md5.Sum([]byte(filepath.Clean(absDir)))
	return filepath.Join(dir, "workspace-"+hex.EncodeToString(sum[:])[:8]+".lock")
}

// SessionLockPath derives the session lock's path from the vendor session id.
func SessionLockPath(dir, vendorSessionID string) string {
	return filepath.Join(dir, "session-"+vendorSessionID+".lock")
}

// heldLock is an exclusively flocked file kept open for the process's life.
type heldLock struct {
	path string
	file *os.File
}

// takeLock opens the path and takes flock(LOCK_EX), blocking until it is free.
// Any failure is returned, never treated as a successful acquisition.
func takeLock(path string) (*heldLock, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("fakeshim: lock dir %s: %w", filepath.Dir(path), err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("fakeshim: open lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		f.Close()
		return nil, fmt.Errorf("fakeshim: flock %s: %w", path, err)
	}
	return &heldLock{path: path, file: f}, nil
}

// tryLock takes flock(LOCK_EX|LOCK_NB): it never blocks, and a lock another
// process holds answers (nil, nil) so the caller can refuse rather than wait.
// The daemon's own probe uses exactly this shape.
func tryLock(path string) (*heldLock, error) {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return nil, fmt.Errorf("fakeshim: lock dir %s: %w", filepath.Dir(path), err)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		return nil, fmt.Errorf("fakeshim: open lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, nil
		}
		return nil, fmt.Errorf("fakeshim: flock %s: %w", path, err)
	}
	return &heldLock{path: path, file: f}, nil
}

func (l *heldLock) release() {
	if l == nil || l.file == nil {
		return
	}
	syscall.Flock(int(l.file.Fd()), syscall.LOCK_UN)
	l.file.Close()
}
