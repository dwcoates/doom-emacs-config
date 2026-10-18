//go:build !darwin

package procmetrics

import "errors"

// errUnsupported is every read on a platform with no kernel reader here. The
// daemon's metrics surface is off in the e2e sandbox (Linux), the only other
// platform it runs on; a surface turned on there records this loudly.
var errUnsupported = errors.New("procmetrics: no process reader on this platform")

// Kernel is the real Reader; on this platform it refuses every read.
type Kernel struct{}

// PIDs implements Reader.
func (Kernel) PIDs() ([]int, error) { return nil, errUnsupported }

// Identity implements Reader.
func (Kernel) Identity(int) (Identity, error) { return Identity{}, errUnsupported }

// Usage implements Reader.
func (Kernel) Usage(int) (Usage, error) { return Usage{}, errUnsupported }

// Args implements Reader.
func (Kernel) Args(int) ([]string, error) { return nil, errUnsupported }

// Host implements Reader.
func (Kernel) Host(string) (Host, error) { return Host{}, errUnsupported }
