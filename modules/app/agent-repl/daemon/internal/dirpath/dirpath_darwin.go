//go:build darwin

package dirpath

import (
	"bytes"
	"fmt"
	"runtime"
	"unsafe"

	"golang.org/x/sys/unix"
)

// onDiskPath answers an existing path as the volume stores it: the kernel's
// F_GETPATH on a descriptor opened for it, which names the file in its stored
// case with every symlink resolved. O_EVTONLY opens without requiring read
// permission, so a directory the daemon may only traverse is still answered.
func onDiskPath(path string) (string, error) {
	fd, err := unix.Open(path, unix.O_EVTONLY|unix.O_CLOEXEC, 0)
	if err != nil {
		return "", fmt.Errorf("open %q: %w", path, err)
	}
	buf := make([]byte, unix.PathMax)
	// THE BUFFER IS PINNED because the kernel writes through its address,
	// which crosses the call as an integer the garbage collector cannot see.
	var pin runtime.Pinner
	pin.Pin(&buf[0])
	_, fcntlErr := unix.FcntlInt(uintptr(fd), unix.F_GETPATH, int(uintptr(unsafe.Pointer(&buf[0]))))
	pin.Unpin()
	closeErr := unix.Close(fd)
	if fcntlErr != nil {
		return "", fmt.Errorf("F_GETPATH %q: %w", path, fcntlErr)
	}
	if closeErr != nil {
		return "", fmt.Errorf("close %q: %w", path, closeErr)
	}
	end := bytes.IndexByte(buf, 0)
	if end < 0 {
		return "", fmt.Errorf("F_GETPATH %q: the answer is not terminated", path)
	}
	return string(buf[:end]), nil
}
