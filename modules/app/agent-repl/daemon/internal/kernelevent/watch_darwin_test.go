package kernelevent

import (
	"context"
	"errors"
	"testing"

	"golang.org/x/sys/unix"
)

func TestAwaitAndTakePendingTakesEveryPendingEvent(t *testing.T) {
	// Arrange: two pipes' reads pending on one queue. A read event, unlike
	// the queue's own user event, is never taken for the context's end.
	kq, err := newQueue()
	if err != nil {
		t.Fatalf("newQueue: %v", err)
	}
	defer unix.Close(kq)
	for range 2 {
		var fds [2]int
		if err := unix.Pipe(fds[:]); err != nil {
			t.Fatalf("pipe: %v", err)
		}
		t.Cleanup(func() { unix.Close(fds[0]); unix.Close(fds[1]) })
		add := unix.Kevent_t{Ident: uint64(fds[0]), Filter: unix.EVFILT_READ, Flags: unix.EV_ADD | unix.EV_CLEAR}
		if _, err := unix.Kevent(kq, []unix.Kevent_t{add}, nil, nil); err != nil {
			t.Fatalf("register a read: %v", err)
		}
		if _, err := unix.Write(fds[1], []byte("x")); err != nil {
			t.Fatalf("write: %v", err)
		}
	}
	ctx, cancel := context.WithTimeout(context.Background(), waitBound)
	defer cancel()
	if err := awaitAndTakePending(ctx, kq); err != nil {
		t.Fatalf("first wait = %v, want the pending events", err)
	}
	ended, end := context.WithCancel(context.Background())
	end()

	// Act: nothing is pending, so only the context's end wakes the wait.
	err = awaitAndTakePending(ended, kq)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("second wait = %v, want context.Canceled", err)
	}
}
