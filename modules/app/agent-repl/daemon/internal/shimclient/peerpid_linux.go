//go:build linux

package shimclient

import (
	"fmt"
	"net"

	"golang.org/x/sys/unix"
)

// peerPIDOfConn reads SO_PEERCRED, Linux's answer to "who is on the other end
// of this unix socket".
func peerPIDOfConn(conn *net.UnixConn) (int, error) {
	pid := 0
	err := peerCredential(conn, func(fd uintptr) error {
		cred, err := unix.GetsockoptUcred(int(fd), unix.SOL_SOCKET, unix.SO_PEERCRED)
		if err != nil {
			return fmt.Errorf("getsockopt SO_PEERCRED: %w", err)
		}
		pid = int(cred.Pid)
		return nil
	})
	return pid, err
}
