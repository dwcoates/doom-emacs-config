package harness

import (
	"errors"
	"os"
	"syscall"
)

// takeExclusive takes f's exclusive flock, blocking in the kernel until it is
// free. ON BUSY runs (when non-nil) only when the lock is held elsewhere, just
// before the wait, so a caller can say whom it waits for; the lock taken is
// the same either way.
func takeExclusive(f *os.File, onBusy func()) error {
	err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if !errors.Is(err, syscall.EWOULDBLOCK) {
		return err
	}
	if onBusy != nil {
		onBusy()
	}
	return syscall.Flock(int(f.Fd()), syscall.LOCK_EX)
}
