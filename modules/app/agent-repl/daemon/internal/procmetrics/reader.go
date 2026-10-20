// Package procmetrics is the daemon's PROCESS COLLECTOR
// (docs/PLAN-runtime-metrics.md): CPU, memory, disk, energy, threads and
// descriptors for every process agent-repl runs, and the host's own CPU,
// memory, disk and power, read from the kernel at scrape time.
//
// It runs only inside the /metrics handler, on the scrape's goroutine, and
// reads nothing but the kernel: no daemon lock, no state client, no
// subprocess. A slow kernel read therefore never stalls a prompt or a
// switch, and the daemon's own request handling never waits on it.
//
// WHAT IS AGENT-REPL'S is decided from the kernel's own records, so a shim
// adopted from a previous daemon or a process a successor inherited is
// counted exactly like one this daemon spawned:
//   - the daemon is this process;
//   - Emacs is the process whose WatchDaemon stream stands (EmacsProcess);
//   - the WebKit processes are the ones macOS holds Emacs responsible for;
//   - the store and the sidecar are the processes named for their binaries;
//   - a shim is a node process whose argv listens on
//     a shim socket, and the socket's name is its workspace;
//   - a shim's descendants are its tools (the vendor CLI, shim-lock, every
//     command the agent runs), the daemon's other descendants are its tools.
package procmetrics

import "errors"

// ErrGone is a process that exited between the listing and the read: an
// ordinary schedule, never an error worth recording.
var ErrGone = errors.New("procmetrics: the process is gone")

// Identity is what classifying a process needs, read for every process on
// the host.
type Identity struct {
	PID         int
	PPID        int
	Name        string
	Responsible int
	// StartUnixMicros tells a reused pid from the process that held it.
	StartUnixMicros int64
}

// Usage is what is measured, read only for agent-repl's processes.
type Usage struct {
	CPUSeconds     float64
	ResidentBytes  uint64
	FootprintBytes uint64
	DiskReadBytes  uint64
	DiskWriteBytes uint64
	EnergyJoules   float64
	Threads        int
	OpenFDs        int
}

// Host is the host's own record.
type Host struct {
	// CPUSeconds is the time every processor spent in each mode since boot.
	CPUSeconds map[string]float64
	// MemoryBytes is physical memory by state, and MemoryTotalBytes all of it.
	MemoryBytes      map[string]uint64
	MemoryTotalBytes uint64
	// MemoryPressure is the kernel's pressure level: 1 normal, 2 warning, 4
	// critical.
	MemoryPressure int
	// DiskFreeBytes and DiskTotalBytes are the volume holding the state root.
	DiskFreeBytes  uint64
	DiskTotalBytes uint64
}

// Reader reads the kernel. The real one is reader_darwin.go's; tests fake it.
type Reader interface {
	// PIDs lists every process on the host.
	PIDs() ([]int, error)
	// Identity reads one process's identity; ErrGone when it already exited.
	Identity(pid int) (Identity, error)
	// Usage reads one process's usage; ErrGone when it already exited.
	Usage(pid int) (Usage, error)
	// Args reads one process's argv; ErrGone when it already exited.
	Args(pid int) ([]string, error)
	// Host reads the host, its disk figures for the volume holding path.
	Host(path string) (Host, error)
}
