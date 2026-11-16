package procmetrics

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"agentrepl/logging/procstat"
)

// procfs reads Linux's process table from a /proc tree (procstat.Tree). It
// is the Linux kernel reader's whole body (reader_linux.go), kept free of a
// build tag so it is proven on every platform against a fixture tree.
type procfs struct {
	tree procstat.Tree
}

// newProcfs reads the /proc tree at root.
func newProcfs(root string) procfs { return procfs{tree: procstat.New(root)} }

// zombie is stat's state of a process that exited and awaits its reap.
const zombie = "Z"

// goneAs turns procstat's gone process into this package's, and names any
// other failure for this package.
func goneAs(err error) error {
	if errors.Is(err, procstat.ErrGone) {
		return ErrGone
	}
	return fmt.Errorf("procmetrics: %w", err)
}

// PIDs implements Reader.
func (p procfs) PIDs() ([]int, error) {
	pids, err := p.tree.PIDs()
	if err != nil {
		return nil, fmt.Errorf("procmetrics: %w", err)
	}
	return pids, nil
}

// Identity reads stat and the executable. The name is the executable's base
// name, as macOS names a process; /proc's own command name is cut at 15
// bytes ("shim-claude-sid"), so it is the name only where the executable
// cannot be read: a kernel thread, or another user's process.
func (p procfs) Identity(pid int) (Identity, error) {
	line, err := p.tree.Stat(pid)
	if err != nil {
		return Identity{}, goneAs(err)
	}
	ppid, err := line.Uint(procstat.FieldPPID)
	if err != nil {
		return Identity{}, goneAs(err)
	}
	start, err := p.tree.Start(line)
	if err != nil {
		return Identity{}, goneAs(err)
	}
	name := line.Comm
	exe, err := os.Readlink(p.tree.Path(pid, "exe"))
	switch {
	case err == nil:
		name = filepath.Base(strings.TrimSuffix(exe, " (deleted)"))
	case errors.Is(err, fs.ErrPermission), errors.Is(err, fs.ErrNotExist):
		// A kernel thread has no executable, another user's process hides it,
		// and a process that just exited is told by the next read.
	default:
		return Identity{}, fmt.Errorf("procmetrics: read the executable of pid %d: %w", pid, err)
	}
	return Identity{PID: pid, PPID: int(ppid), Name: name, StartUnixMicros: start.UnixMicro()}, nil
}

// Usage reads stat, status, io and the descriptor table. The footprint is
// the process's private anonymous memory, resident or swapped, which is what
// macOS's physical footprint counts on Linux's terms. Linux attributes no
// energy to a process, so EnergyJoules stays zero (Platform says so).
func (p procfs) Usage(pid int) (Usage, error) {
	line, err := p.tree.Stat(pid)
	if err != nil {
		return Usage{}, goneAs(err)
	}
	if line.Fields[0] == zombie {
		// An exited process its parent has not reaped yet holds no memory,
		// descriptors or status lines left to read.
		return Usage{}, ErrGone
	}
	ticks := uint64(0)
	for _, n := range []int{procstat.FieldUtime, procstat.FieldStime} {
		v, err := line.Uint(n)
		if err != nil {
			return Usage{}, goneAs(err)
		}
		ticks += v
	}
	status, err := p.tree.Status(pid)
	if err != nil {
		return Usage{}, goneAs(err)
	}
	memory := map[string]uint64{}
	for _, key := range []string{"VmRSS", "RssAnon", "VmSwap"} {
		if memory[key], err = procstat.KiB(status, key); err != nil {
			return Usage{}, goneAs(fmt.Errorf("status of pid %d: %w", pid, err))
		}
	}
	threads, err := strconv.Atoi(status["Threads"])
	if err != nil {
		return Usage{}, fmt.Errorf("procmetrics: pid %d threads %q: %w", pid, status["Threads"], err)
	}
	ioData, err := p.tree.Read(pid, "io")
	if err != nil {
		return Usage{}, goneAs(err)
	}
	io := procstat.ParseKeyed(ioData)
	disk := map[string]uint64{}
	for _, key := range []string{"read_bytes", "write_bytes"} {
		v, ok := io[key]
		if !ok {
			return Usage{}, fmt.Errorf("procmetrics: io of pid %d names no %s", pid, key)
		}
		if disk[key], err = strconv.ParseUint(v, 10, 64); err != nil {
			return Usage{}, fmt.Errorf("procmetrics: io of pid %d %s %q: %w", pid, key, v, err)
		}
	}
	fds, err := os.ReadDir(p.tree.Path(pid, "fd"))
	if err != nil {
		if procstat.Gone(err) {
			return Usage{}, ErrGone
		}
		return Usage{}, fmt.Errorf("procmetrics: list the descriptors of pid %d: %w", pid, err)
	}
	return Usage{
		CPUSeconds:     float64(ticks) / procstat.UserHZ,
		ResidentBytes:  memory["VmRSS"],
		FootprintBytes: memory["RssAnon"] + memory["VmSwap"],
		DiskReadBytes:  disk["read_bytes"],
		DiskWriteBytes: disk["write_bytes"],
		Threads:        threads,
		OpenFDs:        len(fds),
	}, nil
}

// Args reads cmdline.
func (p procfs) Args(pid int) ([]string, error) {
	args, err := p.tree.Cmdline(pid)
	if err != nil {
		return nil, goneAs(err)
	}
	return args, nil
}

// meminfoStates maps /proc/meminfo's fields to the memory states the host's
// record names.
var meminfoStates = map[string]string{
	"MemFree":  "free",
	"Active":   "active",
	"Inactive": "inactive",
	"Buffers":  "buffers",
	"Cached":   "cached",
}

// Memory reads /proc/meminfo: the states and the total.
func (p procfs) Memory() (map[string]uint64, uint64, error) {
	data, err := os.ReadFile(filepath.Join(p.tree.Root(), "meminfo"))
	if err != nil {
		return nil, 0, fmt.Errorf("procmetrics: read meminfo: %w", err)
	}
	fields := procstat.ParseKeyed(data)
	states := make(map[string]uint64, len(meminfoStates))
	for key, state := range meminfoStates {
		if states[state], err = procstat.KiB(fields, key); err != nil {
			return nil, 0, fmt.Errorf("procmetrics: meminfo: %w", err)
		}
	}
	total, err := procstat.KiB(fields, "MemTotal")
	if err != nil {
		return nil, 0, fmt.Errorf("procmetrics: meminfo: %w", err)
	}
	return states, total, nil
}
