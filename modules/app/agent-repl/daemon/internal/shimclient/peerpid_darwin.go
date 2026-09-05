//go:build darwin

package shimclient

import (
	"fmt"
	"net"

	"golang.org/x/sys/unix"
)

// peerPIDOfConn reads LOCAL_PEERPID. Darwin's LOCAL_PEERCRED (`struct xucred')
// carries the peer's uid and groups but NO pid, so the pid is its own
// getsockopt — which is why this is a platform split rather than one call.
func peerPIDOfConn(conn *net.UnixConn) (int, error) {
	pid := 0
	err := peerCredential(conn, func(fd uintptr) error {
		got, err := unix.GetsockoptInt(int(fd), unix.SOL_LOCAL, unix.LOCAL_PEERPID)
		if err != nil {
			return fmt.Errorf("getsockopt LOCAL_PEERPID: %w", err)
		}
		pid = got
		return nil
	})
	return pid, err
}
