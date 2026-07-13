//go:build linux

package pty

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// open returns a PTY master and the path of its slave.
//
// Linux's handshake differs from Darwin's: there is no grant step, and
// the kernel hands back the slave's NUMBER rather than its name, so the
// path is assembled here.
//
// The ioctl numbers come from the syscall package rather than from
// hand-computed _IOW()/_IOR() arithmetic — see pty_darwin.go for what
// deriving them by hand cost the first time.
func open() (*os.File, string, error) {
	master, err := os.OpenFile("/dev/ptmx", os.O_RDWR, 0)
	if err != nil {
		return nil, "", fmt.Errorf("pty: open /dev/ptmx: %w", err)
	}
	var unlock int32 // 0 = unlocked
	if _, _, errno := syscall.Syscall(
		syscall.SYS_IOCTL,
		master.Fd(),
		syscall.TIOCSPTLCK,
		uintptr(unsafe.Pointer(&unlock)),
	); errno != 0 {
		_ = master.Close()
		return nil, "", fmt.Errorf("pty: ioctl TIOCSPTLCK: %w", errno)
	}
	var n uint32
	if _, _, errno := syscall.Syscall(
		syscall.SYS_IOCTL,
		master.Fd(),
		syscall.TIOCGPTN,
		uintptr(unsafe.Pointer(&n)),
	); errno != 0 {
		_ = master.Close()
		return nil, "", fmt.Errorf("pty: ioctl TIOCGPTN: %w", errno)
	}
	return master, fmt.Sprintf("/dev/pts/%d", n), nil
}
