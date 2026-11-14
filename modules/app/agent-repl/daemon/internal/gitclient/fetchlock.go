package gitclient

import (
	"fmt"
	"os"
	"os/exec"
	"syscall"
)

// THE FETCH LOCK IS WAITED FOR IN A PROCESS THAT HOLDS NO OTHER LOCK.
//
// A daemon fetch holds the repository's fetch lock (fetchLockName), a record
// lock, for the length of its git. The kernel judges a record lock wait
// (F_SETLKW) a deadlock per PROCESS: a daemon waiting for the fetch lock that
// another daemon holds, while that other daemon waits (on another goroutine,
// for its boot claim) for a lock this daemon holds, is refused EDEADLK
// ("resource deadlock avoided"), though the fetch holding the lock ends on
// its own and nothing is deadlocked (2026-11-13 23:59:34, during a handover).
//
// So the daemon never waits for the fetch lock itself: it runs its fetch's git
// through a HELPER, this same executable re-run with fetchLockEnv set, which
// takes the lock, keeps its descriptor open across the exec, and execs git in
// its own place. The lock belongs to that one process from the wait until git
// exits, and the kernel drops it when git exits or is killed. A waiter that
// owns no other lock can be in no cycle, so the refusal cannot arise.

// fetchLockEnv names the lock file the helper takes; set, it makes this
// executable the helper (runFetchLockHelper).
const fetchLockEnv = "AGENT_REPL_GITCLIENT_FETCH_LOCK"

// fetchGitEnv names the git executable the helper execs once it holds the
// lock: the client's gitBinary, which a test build points at its mocked git.
const fetchGitEnv = "AGENT_REPL_GITCLIENT_FETCH_GIT"

// fetchLockHelperFailed is the exit status the helper answers when it could
// not take the lock or exec git; its stderr says which, and run records it.
const fetchLockHelperFailed = 125

func init() {
	if path := os.Getenv(fetchLockEnv); path != "" {
		os.Exit(runFetchLockHelper(path, os.Getenv(fetchGitEnv), os.Args[1:]))
	}
}

// runFetchLockHelper takes the exclusive record lock at PATH, waiting in the
// kernel for it, then execs GIT with ARGS in this process's place, the lock's
// descriptor left open so git holds the lock until it exits. It answers only
// on a failure, writing the cause to stderr for the daemon's record of the
// git that failed.
func runFetchLockHelper(path, git string, args []string) int {
	// THE HELPER'S OWN VARIABLES STOP HERE: a hook git runs, or any program
	// linking this package that it starts, must never become a helper too.
	os.Unsetenv(fetchLockEnv)
	os.Unsetenv(fetchGitEnv)
	if err := holdFetchLockAcrossExec(path); err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl fetch lock: taking %s: %v\n", path, err)
		return fetchLockHelperFailed
	}
	resolved, err := exec.LookPath(git)
	if err != nil {
		fmt.Fprintf(os.Stderr, "agent-repl fetch lock: finding git %q: %v\n", git, err)
		return fetchLockHelperFailed
	}
	err = syscall.Exec(resolved, append([]string{git}, args...), os.Environ())
	fmt.Fprintf(os.Stderr, "agent-repl fetch lock: exec %s: %v\n", resolved, err)
	return fetchLockHelperFailed
}

// holdFetchLockAcrossExec opens PATH without close-on-exec and waits for its
// exclusive record lock. The descriptor is never closed: it is the lock, and
// the exec keeps both.
func holdFetchLockAcrossExec(path string) error {
	fd, err := syscall.Open(path, syscall.O_RDWR|syscall.O_CREAT, 0o644)
	if err != nil {
		return err
	}
	lock := &syscall.Flock_t{Type: syscall.F_WRLCK, Whence: 0, Start: 0, Len: 0}
	if err := syscall.FcntlFlock(uintptr(fd), syscall.F_SETLKW, lock); err != nil {
		syscall.Close(fd)
		return err
	}
	return nil
}
