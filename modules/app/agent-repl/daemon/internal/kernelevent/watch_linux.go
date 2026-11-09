package kernelevent

import (
	"context"
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// Watch reports changes under the paths added to it: an entry made, removed
// or renamed in a directory, and a file written, changed, removed or renamed.
// It is an inotify instance holding one watch per path.
//
// A WATCHED FILE IS AN INODE, NOT A NAME. A file removed and made again under
// the same name is a new inode the old watch never sees; a caller that waits
// for a name to appear watches its directory.
type Watch struct {
	fd int
}

// A directory's watch reports its ENTRIES only, as a kqueue vnode watch on a
// directory does: inotify would also report every write to a file inside it,
// so a watcher that writes a file of its own there would wake itself.
const (
	dirChanges  = unix.IN_CREATE | unix.IN_DELETE | unix.IN_MOVED_FROM | unix.IN_MOVED_TO | unix.IN_DELETE_SELF | unix.IN_MOVE_SELF
	fileChanges = unix.IN_MODIFY | unix.IN_ATTRIB | unix.IN_CLOSE_WRITE | unix.IN_DELETE_SELF | unix.IN_MOVE_SELF
)

// NewWatch answers a Watch with nothing added to it.
func NewWatch() (*Watch, error) {
	fd, err := unix.InotifyInit1(unix.IN_CLOEXEC | unix.IN_NONBLOCK)
	if err != nil {
		return nil, fmt.Errorf("inotify_init1: %w", err)
	}
	return &Watch{fd: fd}, nil
}

// Add watches PATH. A path that does not exist is refused with an error
// wrapping fs.ErrNotExist.
func (w *Watch) Add(path string) error {
	info, err := os.Stat(path)
	if err != nil {
		return &os.PathError{Op: "watch", Path: path, Err: err}
	}
	mask := uint32(fileChanges)
	if info.IsDir() {
		mask = dirChanges
	}
	if _, err := unix.InotifyAddWatch(w.fd, path, mask); err != nil {
		return &os.PathError{Op: "watch", Path: path, Err: err}
	}
	return nil
}

// Wait blocks until a change has been reported since the last Wait, or ctx
// ends, then takes every change already pending so the next Wait waits for a
// new one.
func (w *Watch) Wait(ctx context.Context) error {
	if err := awaitReadable(ctx, w.fd); err != nil {
		return err
	}
	buf := make([]byte, 64*1024)
	for {
		_, err := unix.Read(w.fd, buf)
		if errors.Is(err, unix.EAGAIN) {
			return nil
		}
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("take the pending changes: %w", err)
		}
	}
}

// Close releases the instance and every watch on it.
func (w *Watch) Close() {
	unix.Close(w.fd)
}
