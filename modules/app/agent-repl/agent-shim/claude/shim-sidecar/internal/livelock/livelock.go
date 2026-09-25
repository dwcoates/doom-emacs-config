// Package livelock answers whether a LIVE SHIM holds a workspace's kernel lock,
// without ever taking that lock itself.
//
// WHY THE LOCK IS THE AUTHORITY FOR "ACTIVE". The shim takes
// `<lock dir>/workspace-<key>.lock` (agent-shim/claude/shim/src/locks.ts) inside
// StartSession and holds it for the rest of its life; the kernel drops it when
// the process dies, however it dies. An inert shim with no session holds none.
// So "the lock is held" is exactly "a live shim session owns this workspace's
// conversation" — the same fact the daemon's own probe
// (daemon/internal/sessionlock) reads before it spawns a shim, and one that
// needs no daemon, no database and no stale-pid bookkeeping. `<key>` is the
// workspace's md5(cwd)[:8], which is also the directory the shim writes its
// identity records under (`<state>/shim/<key>/`), so the two join by name.
//
// WHY THIS PROBE NEVER TAKES THE LOCK. The daemon's probe does
// flock(LOCK_EX|LOCK_NB) and unlocks again. That is safe for it, because it
// probes only before spawning. This process asks every poll tick, and a probe
// that held the lock even for microseconds could make a shim's own claim fail
// with `conversation_owned`, or make the daemon read a free workspace as held.
// So the probe is a pure QUERY:
//
//   - darwin: fcntl(F_GETLK). XNU keeps flock(2) and POSIX locks in one list per
//     vnode, so F_GETLK reports a flock another descriptor holds (type
//     F_WRLCK, pid -1) and never takes anything. The package's own suite pins
//     this.
//   - linux: flock and POSIX locks are separate there, so F_GETLK cannot see a
//     flock. /proc/locks can: it is read ONCE per probe and matched by
//     device:inode.
//
// AN ABSENT LOCK FILE IS NOT HELD. Nobody can hold a lock on a file that does
// not exist, so that answer is definite rather than a guess. Every OTHER failure
// is returned as an error for that path. The caller reads "could not tell" as
// held, never as free, which is the daemon probe's rule too.
package livelock

import (
	"path/filepath"
)

// Path is the workspace lock the shim holds for workspace key `key`:
// `<lockDir>/workspace-<key>.lock`. The spelling is the cross-system contract
// the shim (locks.ts workspaceLockPath) and the daemon (sessionlock
// WorkspaceLockPath) share.
func Path(lockDir, key string) string {
	return filepath.Join(lockDir, "workspace-"+key+".lock")
}

// Held answers, for each path, whether some process holds a flock on it.
//
// A path whose answer could not be read appears in errs rather than in held.
// A failure that stops the whole probe (linux: /proc/locks unreadable) puts the
// same error on every path, so no path goes unanswered.
func Held(paths []string) (held map[string]bool, errs map[string]error) {
	return platformHeld(paths)
}
