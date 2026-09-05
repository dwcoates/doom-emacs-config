package shimclient

import (
	"fmt"
	"net"
)

// socketPeerPID answers the pid of the process SERVING a shim's unix socket
// right now, as the kernel reports it.
//
// IT IS THE ONLY HONEST ANSWER FOR AN ADOPTED SHIM. A shim this daemon spawned
// is a child, so its pid is `cmd.Process.Pid' and the kernel keeps it reserved
// until the reap. A shim this daemon ADOPTED — a handover's transferred
// process, a crash boot's survivor — is nobody's child here, and the daemon
// never learned a pid for it at all: the shim protocol carries none, and the
// kernel locks it holds are held by `shim-lock' children rather than by the
// shim itself. The peer credential is what remains, and it is better than a
// number the shim could have reported: it names the process on the other end
// of THIS connection, so it cannot be a stale or recycled pid, and it cannot
// be claimed by a process that is not actually serving the socket.
//
// A REFUSED OR ABSENT SOCKET IS NOT AN ERROR TO THE CALLER, it is evidence:
// `isSocketGone' recognizes it, and a caller stopping a shim reads it as the
// shim already being gone. Every other failure is returned.
func socketPeerPID(udsPath string) (int, error) {
	conn, err := net.Dial("unix", udsPath)
	if err != nil {
		return 0, fmt.Errorf("shimclient: dial %q for its peer credential: %w", udsPath, err)
	}
	defer conn.Close()
	unixConn, ok := conn.(*net.UnixConn)
	if !ok {
		return 0, fmt.Errorf("shimclient: %q dialed as %T, want a unix connection", udsPath, conn)
	}
	pid, err := peerPIDOfConn(unixConn)
	if err != nil {
		return 0, fmt.Errorf("shimclient: peer credential of %q: %w", udsPath, err)
	}
	if pid <= 0 {
		return 0, fmt.Errorf("shimclient: peer credential of %q names pid %d", udsPath, pid)
	}
	return pid, nil
}

// peerCredential runs one getsockopt against a connected unix socket's fd,
// which is the shape both platform implementations need.
func peerCredential(conn *net.UnixConn, read func(fd uintptr) error) error {
	raw, err := conn.SyscallConn()
	if err != nil {
		return fmt.Errorf("take the connection's raw handle: %w", err)
	}
	var readErr error
	if err := raw.Control(func(fd uintptr) { readErr = read(fd) }); err != nil {
		return fmt.Errorf("reach the connection's descriptor: %w", err)
	}
	return readErr
}
