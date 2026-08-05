package e2e

import (
	"os"
	"os/signal"
	"sync"
	"syscall"
)

// THE CHILD REAPER: teardown that survives the test binary being KILLED.
//
// t.Cleanup is not a teardown guarantee, it is a teardown guarantee FOR A
// BINARY THAT REACHES THE END OF ITS TESTS. Every long-lived child this
// package starts was registered with t.Cleanup alone, so any death that
// bypasses the testing framework's unwinding orphaned every one of them:
//
//   - `go test ./... | head -20`. head exits at its twentieth line, closing
//     the pipe; the next write earns EPIPE and the process dies by SIGPIPE.
//     This is the one that actually happened — it stranded 28 shim-store
//     processes, which sat holding sockets until they were killed by hand.
//   - Ctrl-C at the terminal (SIGINT), or any supervisor's SIGTERM.
//   - A panic that unwinds past the testing framework.
//
// The children are not merely idle when this happens. A stranded shim-store
// holds its socket and its SQLite handle, so the leak is not just process
// count: a later run can meet a socket that answers and a database that is
// already open.
//
// WHAT THIS FIXES AND WHAT IT CANNOT. Registered children are killed on the
// catchable signals and again after m.Run returns, so the only remaining hole
// is SIGKILL, which by definition cannot be handled. That is the honest limit
// of an in-process reaper; closing it would take a supervisor outside this
// binary, which is not worth its own failure modes here.
//
// EACH CHILD LEADS ITS OWN PROCESS GROUP (SysProcAttr.Setpgid at spawn) so a
// child that spawns its own children is killed as a unit. Killing the pid
// alone would leave grandchildren behind, which is the same leak one level
// down.

// reaper tracks the process groups this package has spawned.
var reaper = struct {
	mu     sync.Mutex
	groups map[int]struct{}
}{groups: map[int]struct{}{}}

// registerChild records PID as a process-group leader to be killed on the way
// out. Call it immediately after a successful Start.
func registerChild(pid int) {
	reaper.mu.Lock()
	defer reaper.mu.Unlock()
	reaper.groups[pid] = struct{}{}
}

// unregisterChild drops PID once its own cleanup has reaped it, so the sweep
// never signals a pid the OS may have recycled onto an unrelated process.
func unregisterChild(pid int) {
	reaper.mu.Lock()
	defer reaper.mu.Unlock()
	delete(reaper.groups, pid)
}

// reapChildren kills every still-registered process group.
//
// SIGKILL rather than SIGTERM: this runs when the binary is already going
// away, so there is no one left to observe a graceful shutdown, and a child
// that ignored a TERM would be exactly the child that leaked.
func reapChildren() {
	reaper.mu.Lock()
	pids := make([]int, 0, len(reaper.groups))
	for pid := range reaper.groups {
		pids = append(pids, pid)
	}
	reaper.groups = map[int]struct{}{}
	reaper.mu.Unlock()
	for _, pid := range pids {
		// Negative pid addresses the whole group. The error is deliberately
		// ignored: the child exiting on its own first is the common case and
		// is not a failure of this sweep.
		_ = syscall.Kill(-pid, syscall.SIGKILL)
	}
}

// watchForTerminationSignals reaps the children when this binary is killed by
// a catchable signal, then re-raises so the exit status still reports the
// signal rather than a tidy zero.
//
// SIGPIPE is included and is the whole point. Go only delivers it for writes
// to fds 1 and 2 when it is being caught, which is precisely the
// `| head` case that stranded the processes this reaper exists to prevent.
func watchForTerminationSignals() {
	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP, syscall.SIGPIPE)
	go func() {
		sig := <-ch
		reapChildren()
		// Restore the default disposition and re-raise, so the caller sees a
		// process killed by the signal instead of one that swallowed it.
		signal.Stop(ch)
		signal.Reset(sig.(syscall.Signal))
		_ = syscall.Kill(os.Getpid(), sig.(syscall.Signal))
	}()
}
