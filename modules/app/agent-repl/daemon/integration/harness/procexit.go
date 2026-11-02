package harness

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"syscall"
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

// ProcessEnd is how a process ended: its exit code, or the signal that ended
// it (Signal non-zero, Code then meaningless).
type ProcessEnd struct {
	Code   int
	Signal syscall.Signal
}

// Clean reports an exit of its own accord with code 0.
func (e ProcessEnd) Clean() bool { return e.Signal == 0 && e.Code == 0 }

func (e ProcessEnd) String() string {
	if e.Signal != 0 {
		return fmt.Sprintf("killed by %v", e.Signal)
	}
	return fmt.Sprintf("exit code %d", e.Code)
}

// processEndOf reads a wait status.
func processEndOf(ws syscall.WaitStatus) ProcessEnd {
	if ws.Signaled() {
		return ProcessEnd{Signal: ws.Signal()}
	}
	return ProcessEnd{Code: ws.ExitStatus()}
}

// errEndUnwatchable is AwaitProcessEnd's answer for a process whose exit
// could not be registered: it had already exited (or was exiting), so how it
// ended is no longer the kernel's to tell without a reap.
var errEndUnwatchable = errors.New("the process had already exited when its end was registered")

// AwaitProcessEnd waits, bounded by ctx, for the exit of pid, which must be a
// child of this process, and answers how it ended WITHOUT REAPING IT: the
// process's owner still reaps it, on its own schedule, so every signal it
// sends under its reap's lock (Daemon.sigMu) stays safe.
func AwaitProcessEnd(ctx context.Context, pid int) (ProcessEnd, error) {
	ev, err := takeEndEvent(pid)
	if err != nil {
		return ProcessEnd{}, err
	}
	if ev == nil {
		return ProcessEnd{}, errEndUnwatchable
	}
	defer ev.close()
	if err := ev.await(ctx); err != nil {
		return ProcessEnd{}, err
	}
	return ev.endOf(pid)
}
