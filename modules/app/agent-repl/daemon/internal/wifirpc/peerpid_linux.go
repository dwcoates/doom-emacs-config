package wifirpc

import (
	"fmt"
	"net"

	"golang.org/x/sys/unix"
)

// peerPID answers the pid of the process at the other end of a unix
// connection, as the kernel records it (SO_PEERCRED).
func peerPID(conn net.Conn) (int, error) {
	uc, ok := conn.(*net.UnixConn)
	if !ok {
		return 0, fmt.Errorf("a %T is no unix connection, so it has no kernel-recorded peer", conn)
	}
	raw, err := uc.SyscallConn()
	if err != nil {
		return 0, fmt.Errorf("the connection's descriptor: %w", err)
	}
	var cred *unix.Ucred
	var sockErr error
	if err := raw.Control(func(fd uintptr) {
		cred, sockErr = unix.GetsockoptUcred(int(fd), unix.SOL_SOCKET, unix.SO_PEERCRED)
	}); err != nil {
		return 0, fmt.Errorf("the connection's descriptor: %w", err)
	}
	if sockErr != nil {
		return 0, fmt.Errorf("SO_PEERCRED: %w", sockErr)
	}
	return int(cred.Pid), nil
}
