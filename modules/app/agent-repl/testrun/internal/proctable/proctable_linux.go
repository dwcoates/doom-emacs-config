//go:build linux

package proctable

import (
	"fmt"
	"os"

	"agentrepl/logging/procstat"
)

// host is the host's /proc.
var host = procstat.New("/proc")

// List reads every process on the host from /proc (procfs.go).
func List() ([]Entry, error) {
	return procfsKernel(host).list()
}

// NewMarked scans this user's processes for the environment variable name,
// reading /proc (procfs.go).
func NewMarked(name string) (*Marked, error) {
	return &Marked{Name: name, UID: os.Geteuid(), k: procfsMarkedKernel(host)}, nil
}

// ProcessCPU answers the CPU seconds process PID has used, its own and its
// reaped children's, from /proc (procfs.go).
func ProcessCPU(pid int) (float64, error) {
	own, children, err := procfsRusage(host)(pid)
	if err != nil {
		return 0, fmt.Errorf("read pid %d's CPU: %w", pid, err)
	}
	return own + children, nil
}
