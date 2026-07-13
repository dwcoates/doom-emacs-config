//go:build darwin

package pty

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// open returns a PTY master and the path of its slave.
//
// Darwin's handshake is the POSIX one: grant the slave, unlock it, then
// ask for its name. There is no ptsname(3) call — TIOCPTYGNAME hands the
// path back through the buffer directly.
//
// The ioctl numbers come from the syscall package rather than from
// hand-computed _IO() arithmetic. Deriving them by hand is how this file
// first shipped TIOCPTYGRANT as _IO('t', 71) instead of _IO('t', 84),
// which is a different ioctl entirely and failed with ENOTTY.
func open() (*os.File, string, error) {
	master, err := os.OpenFile("/dev/ptmx", os.O_RDWR, 0)
	if err != nil {
		return nil, "", fmt.Errorf("pty: open /dev/ptmx: %w", err)
	}
	for _, op := range []struct {
		name string
		num  uintptr
	}{
		{"TIOCPTYGRANT", syscall.TIOCPTYGRANT},
		{"TIOCPTYUNLK", syscall.TIOCPTYUNLK},
	} {
		if _, _, errno := syscall.Syscall(syscall.SYS_IOCTL, master.Fd(), op.num, 0); errno != 0 {
			_ = master.Close()
			return nil, "", fmt.Errorf("pty: ioctl %s: %w", op.name, errno)
		}
	}
	var buf [128]byte
	if _, _, errno := syscall.Syscall(
		syscall.SYS_IOCTL,
		master.Fd(),
		syscall.TIOCPTYGNAME,
		uintptr(unsafe.Pointer(&buf[0])),
	); errno != 0 {
		_ = master.Close()
		return nil, "", fmt.Errorf("pty: ioctl TIOCPTYGNAME: %w", errno)
	}
	end := 0
	for end < len(buf) && buf[end] != 0 {
		end++
	}
	return master, string(buf[:end]), nil
}
