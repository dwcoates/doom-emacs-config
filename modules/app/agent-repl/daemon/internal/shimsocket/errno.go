package shimsocket

import (
	"errors"
	"os"
	"syscall"
)

// Gone reports whether an error means NO PROCESS SERVES a shim's unix socket:
// ECONNREFUSED (the path outlived its listener) or ENOENT (no path at all) on
// a dial, and ENOTCONN on a connection made and then lost before anything
// could be read off it -- the same evidence arriving one instant later, when a
// dial wins the race with the shim's exit. It is the ONE classification every
// caller that dials a shim reads that evidence through: a probe reads it as a
// stale socket, a kill as a shim already stopped, a delivery as a session to
// wait for. Every wrapper on the error (net.OpError, os.SyscallError, a
// transport's own error) is unwrapped through errors.Is.
func Gone(err error) bool {
	return errors.Is(err, syscall.ECONNREFUSED) || errors.Is(err, syscall.ENOENT) ||
		errors.Is(err, os.ErrNotExist) || errors.Is(err, syscall.ENOTCONN)
}
