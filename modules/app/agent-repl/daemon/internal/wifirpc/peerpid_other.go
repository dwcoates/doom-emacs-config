//go:build !darwin && !linux

package wifirpc

import (
	"errors"
	"net"
)

// peerPID is unavailable off darwin and linux, whose kernels record a unix
// connection's peer. The service serves on neither of the others.
func peerPID(net.Conn) (int, error) {
	return 0, errors.New("this platform records no unix connection's peer pid")
}
