package kernelevent

import (
	"context"
	"errors"
	"fmt"
	"syscall"
	"unsafe"

	"golang.org/x/sys/unix"
)

// UnregisteredExitSeen is what an exit event that could not be taken says of
// the exit (AwaitExit): pidfd_open(2) opens a zombie too, so its ESRCH is a
// process already reaped, whose files are released.
const UnregisteredExitSeen = true

// Exit is one process's exit event on a pidfd, which turns readable at the
// exit, held from TakeExit until Close.
//
// The pidfd needs no close-on-exec mark of its own: pidfd_open(2) always sets
// O_CLOEXEC on the descriptor it answers, so it never leaks into a child.
type Exit struct {
	fd int
}

// TakeExit opens pid's pidfd, answering nil (and no error) for a process
// already reaped.
func TakeExit(pid int) (*Exit, error) {
	fd, err := unix.PidfdOpen(pid, 0)
	if err != nil {
		if errors.Is(err, unix.ESRCH) {
			return nil, nil
		}
		return nil, fmt.Errorf("pidfd_open: %w", err)
	}
	return &Exit{fd: fd}, nil
}

// TakeEnd is TakeExit for a child of this process; on linux every pidfd's
// process can be asked how it ended (EndOf).
func TakeEnd(pid int) (*Exit, error) { return TakeExit(pid) }

// Await waits for the exit, until ctx ends.
func (e *Exit) Await(ctx context.Context) error {
	return awaitReadable(ctx, e.fd)
}

// awaitReadable blocks until FD is readable or ctx ends.
//
// THE WAIT ENDS WITH CTX, NOT AT A DEADLINE READ OFF IT: the poll also watches
// an eventfd that ctx's end writes (AwakenOnEnd).
func awaitReadable(ctx context.Context, fd int) error {
	ended, err := unix.Eventfd(0, unix.EFD_CLOEXEC)
	if err != nil {
		return fmt.Errorf("eventfd for the context's end: %w", err)
	}
	defer unix.Close(ended)
	release := AwakenOnEnd(ctx, func() {
		_, _ = unix.Write(ended, []byte{1, 0, 0, 0, 0, 0, 0, 0})
	})
	defer release()
	fds := []unix.PollFd{{Fd: int32(fd), Events: unix.POLLIN}, {Fd: int32(ended), Events: unix.POLLIN}}
	for {
		_, err := unix.Poll(fds, -1)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return fmt.Errorf("wait on the descriptor: %w", err)
		}
		if fds[0].Revents == 0 {
			return ctx.Err()
		}
		return nil
	}
}

// EndOf answers how the exited process of the event ended, read with waitid's
// WNOWAIT so the process stays unreaped for its owner's own reap.
func (e *Exit) EndOf(pid int) (ProcessEnd, error) {
	var info unix.Siginfo
	if err := unix.Waitid(unix.P_PIDFD, e.fd, &info, unix.WEXITED|unix.WNOWAIT, nil); err != nil {
		return ProcessEnd{}, fmt.Errorf("waitid %d: %w", pid, err)
	}
	// si_status follows si_pid and si_uid in the SIGCHLD arm of the union,
	// 24 bytes into the struct on every 64-bit linux.
	status := int(*(*int32)(unsafe.Add(unsafe.Pointer(&info), 24)))
	switch info.Code {
	case cldExited:
		return ProcessEnd{Code: status}, nil
	case cldKilled, cldDumped:
		return ProcessEnd{Signal: syscall.Signal(status)}, nil
	}
	return ProcessEnd{}, fmt.Errorf("waitid %d answered si_code %d, which is no exit", pid, info.Code)
}

// The SIGCHLD si_code values (asm-generic/siginfo.h) waitid answers for an
// exited child.
const (
	cldExited = 1
	cldKilled = 2
	cldDumped = 3
)

// Close releases the pidfd.
func (e *Exit) Close() {
	unix.Close(e.fd)
}
