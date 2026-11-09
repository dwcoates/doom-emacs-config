package harness

import (
	"context"
	"testing"

	"agent-repld/internal/kernelevent"
)

// The harness waits on process exits through the daemon's own kernel-event
// package, so production and the suite share one implementation.
type (
	// exitEvent is one process's exit event, held from takeExitEvent until
	// close.
	exitEvent = kernelevent.Exit
	// ProcessEnd is how a process ended.
	ProcessEnd = kernelevent.ProcessEnd
)

var (
	// takeExitEvent registers for a process's exit (kernelevent.TakeExit).
	takeExitEvent = kernelevent.TakeExit
	// awaitExitEvent waits for a process's exit, answering whether it saw
	// the exit itself (kernelevent.AwaitExit).
	awaitExitEvent = kernelevent.AwaitExit
	// errEndUnwatchable is AwaitProcessEnd's answer for a process whose exit
	// could not be registered (kernelevent.ErrEndUnwatchable).
	errEndUnwatchable = kernelevent.ErrEndUnwatchable
)

// WaitProcessExit waits for pid's exit on the kernel's exit event, bounded by
// ctx (kernelevent.WaitExit).
func WaitProcessExit(ctx context.Context, pid int) error {
	return kernelevent.WaitExit(ctx, pid)
}

// AwaitProcessExit blocks on the kernel's own report that pid exited, bounded
// by ctx. It waits on the exit event itself, so it works for a process this
// test did not start (a daemon's shim) and returns the moment the process
// ends.
func AwaitProcessExit(t testing.TB, ctx context.Context, pid int) {
	t.Helper()
	if err := WaitProcessExit(ctx, pid); err != nil {
		t.Fatalf("process %d did not exit: %v", pid, err)
	}
}

// AwaitProcessEnd waits, bounded by ctx, for the exit of pid, which must be a
// child of this process, and answers how it ended WITHOUT REAPING IT: the
// process's owner still reaps it, on its own schedule, so every signal it
// sends under its reap's lock (Daemon.sigMu) stays safe.
func AwaitProcessEnd(ctx context.Context, pid int) (ProcessEnd, error) {
	return kernelevent.AwaitEnd(ctx, pid)
}
