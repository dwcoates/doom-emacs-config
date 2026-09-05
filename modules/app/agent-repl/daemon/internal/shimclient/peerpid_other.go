//go:build !linux && !darwin

package shimclient

import (
	"fmt"
	"net"
	"runtime"
)

// peerPIDOfConn REFUSES on a platform whose peer credential this package has
// not been taught to read. It is a loud refusal rather than a zero: a daemon
// that cannot learn an adopted shim's pid cannot stop it, and silently
// answering "no pid" would turn that into a leaked process nobody reports.
func peerPIDOfConn(*net.UnixConn) (int, error) {
	return 0, fmt.Errorf("no peer-credential implementation for GOOS %s", runtime.GOOS)
}
