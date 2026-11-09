package kernelevent

import (
	"context"
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// Watch reports changes under the paths added to it: an entry made, removed
// or renamed in a directory (never a write to a file inside it), and a file
// written, grown, changed, removed or renamed. It is a kqueue holding one EVFILT_VNODE registration per path.
//
// A WATCHED FILE IS AN INODE, NOT A NAME. A file removed and made again under
// the same name is a new inode the old registration never sees; a caller that
// waits for a name to appear watches its directory.
type Watch struct {
	kq  int
	fds []int
}

// vnodeChanges is every vnode change a Watch reports.
const vnodeChanges = unix.NOTE_WRITE | unix.NOTE_EXTEND | unix.NOTE_ATTRIB | unix.NOTE_DELETE | unix.NOTE_RENAME | unix.NOTE_REVOKE | unix.NOTE_LINK

// NewWatch answers a Watch with nothing added to it.
func NewWatch() (*Watch, error) {
	kq, err := newQueue()
	if err != nil {
		return nil, err
	}
	return &Watch{kq: kq}, nil
}

// Add watches PATH. A path that does not exist is refused with an error
// wrapping fs.ErrNotExist, and one the kernel cannot watch (a unix socket:
// open(2) refuses it EOPNOTSUPP) with one wrapping ErrUnwatchable; its
// directory is what reports it made or removed.
func (w *Watch) Add(path string) error {
	fd, err := unix.Open(path, unix.O_EVTONLY|unix.O_CLOEXEC, 0)
	if errors.Is(err, unix.EOPNOTSUPP) {
		return &os.PathError{Op: "watch", Path: path, Err: ErrUnwatchable}
	}
	if err != nil {
		return &os.PathError{Op: "watch", Path: path, Err: err}
	}
	change := unix.Kevent_t{Ident: uint64(fd), Filter: unix.EVFILT_VNODE, Flags: unix.EV_ADD | unix.EV_CLEAR, Fflags: vnodeChanges}
	if _, err := unix.Kevent(w.kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		unix.Close(fd)
		return fmt.Errorf("register a watch on %s: %w", path, err)
	}
	w.fds = append(w.fds, fd)
	return nil
}

// Wait blocks until a change has been reported since the last Wait, or ctx
// ends, then takes every change already pending so the next Wait waits for a
// new one.
func (w *Watch) Wait(ctx context.Context) error {
	if _, err := awaitQueue(ctx, w.kq); err != nil {
		return err
	}
	zero := unix.Timespec{}
	events := make([]unix.Kevent_t, 16)
	for {
		n, err := unix.Kevent(w.kq, nil, events, &zero)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("take the pending changes: %w", err)
		}
		if n < len(events) {
			return nil
		}
	}
}

// Close releases every registration.
func (w *Watch) Close() {
	for _, fd := range w.fds {
		unix.Close(fd)
	}
	unix.Close(w.kq)
}
