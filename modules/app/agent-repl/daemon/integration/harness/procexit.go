package harness

import (
	"context"
	"testing"
)

// AwaitProcessExit blocks on the kernel's own report that pid exited, bounded
// by ctx. Unlike AwaitProcessGone it does not poll: it waits on the exit event
// itself, so it works for a process this test did not start (a daemon's shim)
// and returns the moment the process ends.
func AwaitProcessExit(t *testing.T, ctx context.Context, pid int) {
	t.Helper()
	if err := WaitProcessExit(ctx, pid); err != nil {
		t.Fatalf("process %d did not exit: %v", pid, err)
	}
}
