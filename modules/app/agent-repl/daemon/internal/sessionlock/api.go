// Package sessionlock PROBES the kernel locks the shim holds. The daemon never
// holds one.
//
// Kernel file locks are the cross-process arbitration mechanism — self-
// releasing on death, no stale-pid state — and WSM holds only the lease's
// policy metadata: the lock decides, the row describes. The daemon probes the
// WORKSPACE lock only. See ARCHITECTURE.md "Shim-held kernel locks (probe
// only)".
package sessionlock

import (
	"claude-repld/internal/notimpl"
)

// RunDir is the directory the kernel locks live in.
const RunDir = "~/.cache/agent-repl/run"

// State is what a probe could determine.
type State int

// The probe states. "Could not tell" is never read as free.
const (
	// StateUnknown means the probe failed for a reason other than the lock
	// being held — a missing directory, a permission error. Never read as
	// free.
	StateUnknown State = iota
	// StateFree means the probe took and released the lock: no live shim holds
	// this conversation.
	StateFree
	// StateHeld means the probe got EWOULDBLOCK: a live shim holds it. A fresh
	// daemon boot reads this as "a surviving shim owns this conversation",
	// even before that shim dials in.
	StateHeld
)

// WorkspaceLockPath derives a workspace's lock path:
// <runDir>/workspace-<md5hex(filepath.Clean(absDir))[:8]>.lock.
func WorkspaceLockPath(runDir, workspaceDir string) (string, error) {
	return "", notimpl.Err
}

// SessionLockPath derives a session's lock path,
// <runDir>/session-<vendor session id>.lock. The daemon does not probe it —
// the workspace lock is the load-bearing one — but the spelling is here so
// the cross-system contract has one home.
func SessionLockPath(runDir, vendorSessionID string) (string, error) {
	return "", notimpl.Err
}

// Probe opens the lock file and attempts flock(LOCK_EX|LOCK_NB), unlocking
// again on success. Success is StateFree, EWOULDBLOCK is StateHeld, and every
// other error is StateUnknown WITH the error: an unreadable lock is never
// reported as free.
func Probe(lockPath string) (State, error) {
	return StateUnknown, notimpl.Err
}
