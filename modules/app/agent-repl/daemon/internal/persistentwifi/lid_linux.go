//go:build linux

package persistentwifi

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"unsafe"

	"golang.org/x/sys/unix"
)

// inputDevices is the kernel's list of input devices.
const inputDevices = "/proc/bus/input/devices"

// SystemLid hears the lid switch's input device (lidevdev.go). Reading it
// takes the `input' group, which the installer's requirements name; a lid it
// may not read is an unwatched lid saying so.
func SystemLid() LidWatcher { return evdevLid{} }

type evdevLid struct{}

func (evdevLid) Watch(ctx context.Context, heard func(Lid)) error {
	devices, err := os.ReadFile(inputDevices)
	if err != nil {
		return fmt.Errorf("persistentwifi: read %s to find the lid switch: %w", inputDevices, err)
	}
	handler := lidDeviceIn(string(devices))
	if handler == "" {
		heard(LidNone)
		<-ctx.Done()
		return nil
	}
	path := filepath.Join("/dev/input", handler)
	// SUBSCRIBE, THEN READ: the device is open (its events queue from here)
	// before its switch state is read, so a change between the two is heard
	// as an event.
	f, err := os.Open(path)
	if errors.Is(err, fs.ErrPermission) {
		return fmt.Errorf("persistentwifi: the lid switch %s may not be read; add this user to the input group: %w", path, err)
	}
	if err != nil {
		return fmt.Errorf("persistentwifi: open the lid switch %s: %w", path, err)
	}
	state := make([]byte, 8)
	if err := evioCGSW(f, state); err != nil {
		_ = f.Close()
		return fmt.Errorf("persistentwifi: read the lid switch's state from %s: %w", path, err)
	}
	return watchLidEvents(ctx, f, lidFromSwitches(state), heard)
}

// evioCGSW is the EVIOCGSW ioctl: the device's switch states into buf.
func evioCGSW(f *os.File, buf []byte) error {
	const iocRead = 2
	req := uintptr(iocRead<<30 | len(buf)<<16 | 'E'<<8 | 0x1b)
	raw, err := f.SyscallConn()
	if err != nil {
		return err
	}
	var ioErr error
	if err := raw.Control(func(fd uintptr) {
		if _, _, errno := unix.Syscall(unix.SYS_IOCTL, fd, req, uintptr(unsafe.Pointer(&buf[0]))); errno != 0 {
			ioErr = errno
		}
	}); err != nil {
		return err
	}
	return ioErr
}
