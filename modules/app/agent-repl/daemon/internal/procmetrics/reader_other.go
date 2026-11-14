//go:build !darwin && !linux

package procmetrics

import "errors"

// errUnsupported is every read on a platform with no kernel reader here,
// which agent-repl runs on none of; a surface turned on there records this
// loudly.
var errUnsupported = errors.New("procmetrics: no process reader on this platform")

// Kernel is the real Reader; on this platform it refuses every read.
type Kernel struct{}

// Platform implements Reader: no rule matches, as no read succeeds.
func (Kernel) Platform() Platform {
	return Platform{
		ServiceParent: func(map[int]Identity, int) bool { return false },
		WebKitRole:    func(map[int]Identity, Identity, int) (string, bool) { return "", false },
	}
}

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
