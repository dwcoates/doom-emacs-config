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
// wrapping fs.ErrNotExist, one the kernel cannot watch (a unix socket:
// open(2) refuses it EOPNOTSUPP) with one wrapping ErrUnwatchable, and a
// symlink being replaced under the open with one wrapping ErrLinkReplaced;
// its directory is what reports each of them made, removed or replaced.
func (w *Watch) Add(path string) error {
	fd, err := unix.Open(path, unix.O_EVTONLY|unix.O_CLOEXEC, 0)
	if err != nil {
		return openError(path, err)
	}
	change := unix.Kevent_t{Ident: uint64(fd), Filter: unix.EVFILT_VNODE, Flags: unix.EV_ADD | unix.EV_CLEAR, Fflags: vnodeChanges}
	if _, err := unix.Kevent(w.kq, []unix.Kevent_t{change}, nil, nil); err != nil {
		unix.Close(fd)
		return fmt.Errorf("register a watch on %s: %w", path, err)
	}
	w.fds = append(w.fds, fd)
	return nil
}

// openError is Add's error for PATH, which open(2) refused with ERR.
//
// A SYMLINK RENAMED OVER WHILE open(2) RESOLVES IT IS REFUSED EINVAL. Darwin
// answers so whenever the link is replaced mid-resolution, as the daemon
// replaces a workspace's canonical log link (dlog.replaceLink renames a fresh
// link onto it). The link is not missing and not broken: its directory reports
// the rename, and the next watch opens the new link.
func openError(path string, err error) error {
	switch {
	case errors.Is(err, unix.EOPNOTSUPP):
		err = ErrUnwatchable
	case errors.Is(err, unix.EINVAL):
		if info, lerr := os.Lstat(path); lerr == nil && info.Mode()&os.ModeSymlink != 0 {
			err = ErrLinkReplaced
		}
	}
	return &os.PathError{Op: "watch", Path: path, Err: err}
}

// Wait blocks until a change has been reported since the last Wait, or ctx
// ends, then takes every change already pending so the next Wait waits for a
// new one.
func (w *Watch) Wait(ctx context.Context) error {
	return awaitAndTakePending(ctx, w.kq)
}

// awaitAndTakePending blocks on KQ until one of its registered events fires
// or ctx ends, then takes every event already pending, so the next wait
// waits for a new one. Watch.Wait and Spawns.Wait share it.
func awaitAndTakePending(ctx context.Context, kq int) error {
	if _, err := awaitQueue(ctx, kq); err != nil {
		return err
	}
	zero := unix.Timespec{}
	events := make([]unix.Kevent_t, 16)
	for {
		n, err := unix.Kevent(kq, nil, events, &zero)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("take the pending events: %w", err)
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
