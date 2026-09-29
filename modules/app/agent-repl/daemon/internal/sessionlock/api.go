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
	"crypto/md5"
	"encoding/hex"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/dlog"
)

// RunDir is the directory the kernel locks live in.
const RunDir = "~/.cache/agent-repl/run"

// RunDirEnv overrides RunDir. It exists for tests, which must never touch the
// real run directory of a live machine.
const RunDirEnv = "AGENT_REPL_LOCK_DIR"

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

// String names the state for a log record.
func (s State) String() string {
	switch s {
	case StateFree:
		return "free"
	case StateHeld:
		return "held"
	default:
		return "unknown"
	}
}

// ResolveRunDir answers the absolute run directory: RunDirEnv when it is set,
// otherwise RunDir with the home directory substituted for the tilde. Both go
// through dirpath.Absolute, so a relative override is refused rather than
// resolved against the daemon's working directory.
func ResolveRunDir() (string, error) {
	if v := os.Getenv(RunDirEnv); v != "" {
		abs, err := dirpath.Absolute(v, "")
		if err != nil {
			return "", fmt.Errorf("sessionlock: resolve %s: %w", RunDirEnv, err)
		}
		return abs, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("sessionlock: resolve run dir: %w", err)
	}
	abs, err := dirpath.Absolute(RunDir, home)
	if err != nil {
		return "", fmt.Errorf("sessionlock: resolve run dir: %w", err)
	}
	return abs, nil
}

// WorkspaceLockPath derives a workspace's lock path:
// <runDir>/workspace-<md5hex(filepath.Clean(absDir))[:8]>.lock.
func WorkspaceLockPath(runDir, workspaceDir string) (string, error) {
	if strings.TrimSpace(workspaceDir) == "" {
		return "", errors.New("sessionlock: workspace dir is empty")
	}
	dir, err := resolveRunDir(runDir)
	if err != nil {
		return "", err
	}
	abs, err := filepath.Abs(workspaceDir)
	if err != nil {
		return "", fmt.Errorf("sessionlock: absolute workspace dir %q: %w", workspaceDir, err)
	}
	sum := md5.Sum([]byte(resolveForKey(abs)))
	return filepath.Join(dir, "workspace-"+hex.EncodeToString(sum[:])[:8]+".lock"), nil
}

// SessionLockPath derives a session's lock path,
// <runDir>/session-<vendor session id>.lock. The daemon does not probe it —
// the workspace lock is the load-bearing one — but the spelling is here so
// the cross-system contract has one home.
func SessionLockPath(runDir, vendorSessionID string) (string, error) {
	if strings.TrimSpace(vendorSessionID) == "" {
		return "", errors.New("sessionlock: vendor session id is empty")
	}
	if strings.ContainsRune(vendorSessionID, filepath.Separator) || strings.Contains(vendorSessionID, "/") {
		return "", fmt.Errorf("sessionlock: vendor session id %q contains a path separator", vendorSessionID)
	}
	dir, err := resolveRunDir(runDir)
	if err != nil {
		return "", err
	}
	return filepath.Join(dir, "session-"+vendorSessionID+".lock"), nil
}

// Probe opens the lock file and attempts flock(LOCK_EX|LOCK_NB), unlocking
// again on success. Success is StateFree, EWOULDBLOCK is StateHeld, and every
// other error is StateUnknown WITH the error: an unreadable lock is never
// reported as free.
//
// THE RUN DIRECTORY IS CREATED HERE, and that is a bootstrap fix rather than a
// convenience. A missing run directory made this probe answer StateUnknown; the
// daemon refuses to spawn a shim until a probe says FREE; and the shim was the
// only thing that ever created the directory — so on a machine that had never
// run a session, nothing could ever start one. Nobody can hold a lock in a
// directory that does not exist, so the honest answer to a missing directory is
// FREE, and this probe is the process that gets there first. Creating the
// directory is no more than the probe already did in creating the lock FILE.
func Probe(lockPath string) (State, error) {
	if strings.TrimSpace(lockPath) == "" {
		return StateUnknown, errors.New("sessionlock: lock path is empty")
	}
	if err := os.MkdirAll(filepath.Dir(lockPath), 0o755); err != nil {
		return StateUnknown, fmt.Errorf("sessionlock: create the run directory for %q: %w", lockPath, err)
	}
	f, err := os.OpenFile(lockPath, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		return StateUnknown, fmt.Errorf("sessionlock: open %q: %w", lockPath, err)
	}
	defer f.Close()

	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return StateHeld, nil
		}
		return StateUnknown, fmt.Errorf("sessionlock: flock %q: %w", lockPath, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_UN); err != nil {
		return StateUnknown, fmt.Errorf("sessionlock: unlock %q: %w", lockPath, err)
	}
	return StateFree, nil
}

// ProbeWithLog is Probe with the canonical record on every branch. Probe
// itself stays a pure primitive so a caller that already logs does not log
// twice.
func ProbeWithLog(log dlog.Logger, lockPath string) (State, error) {
	state, err := Probe(lockPath)
	if log == nil {
		return state, err
	}
	ctx := dlog.Context{"lock_path": lockPath, "state": state.String()}
	if err != nil {
		ctx["error"] = err.Error()
		log.Error("daemon.sessionlock.probe", "workspace lock probe could not tell", ctx)
		return state, err
	}
	log.Debug("daemon.sessionlock.probe", "workspace lock probed", ctx)
	return state, nil
}

// resolveForKey is the workspace key's canonical spelling: the absolute path
// with every symlink resolved.
//
// IT MUST AGREE WITH THE SHIM'S, and the shim's is not a choice. The daemon
// chdirs the shim into the worktree and the shim keys its lock off
// `process.cwd()` — the KERNEL's answer, which is always fully resolved. So a
// daemon that hashed the path as WSM spelled it disagreed with the shim on
// every workspace reached through a symlink, which on macOS is every path
// under /var/folders (a symlink to /private/var/folders): the shim held
// `workspace-<hash of /private/var/...>.lock` and the daemon probed
// `workspace-<hash of /var/...>.lock`, found it free, and spawned a second
// shim onto a conversation a live one was serving.
//
// The LONGEST EXISTING PREFIX is resolved rather than the whole path, so a
// worktree that has since been deleted still hashes to the same key its
// running shim locked under. A path with nothing resolvable keeps its cleaned
// spelling, which is what filepath.Clean already gave.
func resolveForKey(abs string) string {
	rest := ""
	for dir := filepath.Clean(abs); ; {
		if resolved, err := filepath.EvalSymlinks(dir); err == nil {
			return filepath.Join(resolved, rest)
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return filepath.Clean(abs)
		}
		rest = filepath.Join(filepath.Base(dir), rest)
		dir = parent
	}
}

// resolveRunDir answers the caller's run directory, or the resolved default
// when the caller named none.
func resolveRunDir(runDir string) (string, error) {
	if strings.TrimSpace(runDir) != "" {
		return runDir, nil
	}
	return ResolveRunDir()
}
