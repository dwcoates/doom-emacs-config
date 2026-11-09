package kernelevent

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"syscall"
)

// WaitExit waits for pid's exit on the kernel's exit event, until ctx ends. A
// process the event cannot be taken on has exited or is exiting.
func WaitExit(ctx context.Context, pid int) error {
	_, err := AwaitExit(ctx, pid)
	return err
}

// AwaitExit waits for pid's exit on the kernel's exit event, until ctx ends,
// answering whether it saw the exit itself. An event that could not be taken
// answers UnregisteredExitSeen, the platform's reading of that refusal.
func AwaitExit(ctx context.Context, pid int) (seen bool, err error) {
	ev, err := TakeExit(pid)
	if err != nil {
		return false, err
	}
	if ev == nil {
		return UnregisteredExitSeen, nil
	}
	defer ev.Close()
	if err := ev.Await(ctx); err != nil {
		return false, err
	}
	return true, nil
}

// ErrEndUnwatchable is AwaitEnd's answer for a process whose exit could not be
// registered: it had already exited (or was exiting), so how it ended is no
// longer the kernel's to tell without a reap.
var ErrEndUnwatchable = errors.New("the process had already exited when its end was registered")

// AwaitEnd waits, until ctx ends, for the exit of pid, which must be a child
// of this process, and answers how it ended WITHOUT REAPING IT: the process's
// owner still reaps it, on its own schedule.
func AwaitEnd(ctx context.Context, pid int) (ProcessEnd, error) {
	ev, err := TakeEnd(pid)
	if err != nil {
		return ProcessEnd{}, err
	}
	if ev == nil {
		return ProcessEnd{}, ErrEndUnwatchable
	}
	defer ev.Close()
	if err := ev.Await(ctx); err != nil {
		return ProcessEnd{}, err
	}
	return ev.EndOf(pid)
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

// AwakenOnEnd runs WAKE once ctx ends, until the release it answers is called:
// how a wait blocked in the kernel learns that its context ended.
//
// NO WAKE OUTLIVES THE WAIT. The release stops the wake, and one already
// running finishes before the release returns, so the caller may close the
// descriptor the wake writes to as soon as the release is back.
func AwakenOnEnd(ctx context.Context, wake func()) (release func()) {
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
