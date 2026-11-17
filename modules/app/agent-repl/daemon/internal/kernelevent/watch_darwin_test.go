package kernelevent

import (
	"context"
	"errors"
	"os"
	"path/filepath"
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

func TestOpenErrorNamesWhatTheRefusalMeans(t *testing.T) {
	// Arrange: a regular file and a symlink to it.
	dir := t.TempDir()
	file := filepath.Join(dir, "target.log")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	link := filepath.Join(dir, "shim.log")
	if err := os.Symlink(file, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}
	tests := []struct {
		name string
		path string
		err  error
		want error
	}{
		{name: "a socket the kernel cannot watch is unwatchable", path: file, err: unix.EOPNOTSUPP, want: ErrUnwatchable},
		{name: "an EINVAL on a symlink is the link replaced under the open", path: link, err: unix.EINVAL, want: ErrLinkReplaced},
		{name: "an EINVAL on a regular file stays the kernel's refusal", path: file, err: unix.EINVAL, want: unix.EINVAL},
		{name: "an EINVAL on a path gone since stays the kernel's refusal", path: filepath.Join(dir, "gone.log"), err: unix.EINVAL, want: unix.EINVAL},
		{name: "a missing path is not-exist", path: filepath.Join(dir, "gone.log"), err: unix.ENOENT, want: os.ErrNotExist},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := openError(tt.path, tt.err)

			// Assert
			if !errors.Is(got, tt.want) {
				t.Fatalf("openError(%s, %v) = %v, want %v", tt.path, tt.err, got, tt.want)
			}
			var pe *os.PathError
			if !errors.As(got, &pe) || pe.Path != tt.path {
				t.Fatalf("openError(%s, %v) = %v, want a PathError naming the path", tt.path, tt.err, got)
			}
		})
	}
}
