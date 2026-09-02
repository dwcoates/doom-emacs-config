package login

import (
	"errors"
	"syscall"
)

// isPtyHangup reports whether err is a pty's spelling of "the child on the
// other side is gone".
//
// A vanished child surfaces on the master side as EIO on Linux (and as EIO or
// a plain EOF on macOS), which is the ordinary end of a login rather than a
// failure worth an error record. It is named here so the read loop tests for
// exactly that errno and nothing broader.
func isPtyHangup(err error) bool {
	return errors.Is(err, syscall.EIO)
}
