package harness

import (
	"context"
	"sync"
	"testing"
)

// WaitProcessExit waits for pid's exit on the kernel's exit event, bounded by
// ctx's deadline (awaitExitEvent, per platform). A process the event cannot be
// taken on has exited or is exiting; a caller that needs its files released
// asks awaitExitEvent whether the exit was seen.
func WaitProcessExit(ctx context.Context, pid int) error {
	_, err := awaitExitEvent(ctx, pid)
	return err
}

// awaitExitEvent waits for pid's exit on the kernel's exit event, bounded by
// ctx's deadline, answering whether it saw the exit itself. An event that
// could not be taken answers unregisteredExitSeen, the platform's reading of
// that refusal.
func awaitExitEvent(ctx context.Context, pid int) (seen bool, err error) {
	ev, err := takeExitEvent(pid)
	if err != nil {
		return false, err
	}
	if ev == nil {
		return unregisteredExitSeen, nil
	}
	defer ev.close()
	if err := ev.await(ctx); err != nil {
		return false, err
	}
	return true, nil
}

// AwaitProcessExit blocks on the kernel's own report that pid exited, bounded
// by ctx. Unlike AwaitProcessGone it does not poll: it waits on the exit event
// itself, so it works for a process this test did not start (a daemon's shim)
// and returns the moment the process ends.
func AwaitProcessExit(t testing.TB, ctx context.Context, pid int) {
	t.Helper()
	if err := WaitProcessExit(ctx, pid); err != nil {
		t.Fatalf("process %d did not exit: %v", pid, err)
	}
}

// awakenOnEnd runs WAKE once ctx ends, until the release it answers is called:
// how an exit wait blocked in the kernel learns that its context ended.
//
// NO WAKE OUTLIVES THE WAIT. The release stops the wake, and one already
// running finishes before the release returns, so the caller may close the
// descriptor the wake writes to as soon as the release is back.
func awakenOnEnd(ctx context.Context, wake func()) (release func()) {
	var mu sync.Mutex
	released := false
	stop := context.AfterFunc(ctx, func() {
		mu.Lock()
		defer mu.Unlock()
		if !released {
			wake()
		}
	})
	return func() {
		stop()
		mu.Lock()
		released = true
		mu.Unlock()
	}
}
