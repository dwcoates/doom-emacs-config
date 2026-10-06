package harness

import (
	"testing"

	"golang.org/x/sys/unix"
)

func TestNewExitQueueIsCloseOnExec(t *testing.T) {
	// Act
	kq, err := newExitQueue()
	if err != nil {
		t.Fatalf("newExitQueue: %v", err)
	}
	defer unix.Close(kq)
	flags, err := unix.FcntlInt(uintptr(kq), unix.F_GETFD, 0)

	// Assert
	if err != nil {
		t.Fatalf("F_GETFD: %v", err)
	}
	if flags&unix.FD_CLOEXEC == 0 {
		t.Fatalf("kqueue descriptor flags = %#x, want FD_CLOEXEC set", flags)
	}
}
