//go:build !darwin

package vendortraffic

import (
	"fmt"
	"runtime"
)

// DialStatistics has no kernel to read off Darwin: the network statistics
// control is XNU's. Each process it is asked about is refused at ERROR.
func DialStatistics() (Conn, error) {
	return nil, fmt.Errorf("vendortraffic: no network statistics control on %s", runtime.GOOS)
}

// KernelProcesses is the process table; off Darwin it lists nothing it can
// measure, and says so.
type KernelProcesses struct{}

// Group refuses: off Darwin no process could be measured anyway.
func (KernelProcesses) Group(pgid int) ([]Proc, error) {
	return nil, fmt.Errorf("vendortraffic: process group %d is not listed on %s", pgid, runtime.GOOS)
}
