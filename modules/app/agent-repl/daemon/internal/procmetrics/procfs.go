package procmetrics

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"syscall"

	"agentrepl/logging/procstat"
)

// procfs reads Linux's process table from a /proc tree rooted at root. It
// is the Linux kernel reader's whole body (reader_linux.go), kept free of a
// build tag so its parsing is proven on every platform against a fixture
// tree.
type procfs struct {
	root string
	// boot is the host's boot time, read once: btime holds for the boot.
	boot func() (int64, error)
}

// newProcfs reads the /proc tree at root.
func newProcfs(root string) procfs {
	p := procfs{root: root}
	p.boot = sync.OnceValues(p.bootUnixSeconds)
	return p
}

// userHZ is the unit of /proc's tick counts: the kernel ABI fixes USER_HZ at
// 100 on every architecture, whatever the kernel's own HZ.
const userHZ = 100

// zombie is stat's state of a process that exited and awaits its reap.
const zombie = "Z"

// pidDir is one process's directory.
func (p procfs) pidDir(pid int) string { return filepath.Join(p.root, strconv.Itoa(pid)) }

// readPID reads one file of a process, ErrGone once the process exited.
func (p procfs) readPID(pid int, name string) ([]byte, error) {
	data, err := os.ReadFile(filepath.Join(p.pidDir(pid), name))
	if err != nil {
		if goneErr(err) {
			return nil, ErrGone
		}
		return nil, fmt.Errorf("procmetrics: read %s of pid %d: %w", name, pid, err)
	}
	return data, nil
}

// goneErr answers whether a read failed because the process exited.
func goneErr(err error) bool {
	return errors.Is(err, fs.ErrNotExist) || errors.Is(err, syscall.ESRCH)
}

// PIDs lists every numeric directory of the tree.
func (p procfs) PIDs() ([]int, error) {
	entries, err := os.ReadDir(p.root)
	if err != nil {
		return nil, fmt.Errorf("procmetrics: list the processes in %s: %w", p.root, err)
	}
	pids := make([]int, 0, len(entries))
	for _, e := range entries {
		if pid, err := strconv.Atoi(e.Name()); err == nil && pid > 0 {
			pids = append(pids, pid)
		}
	}
	return pids, nil
}

// procStat is the fields of /proc/<pid>/stat a reader needs.
type procStat struct {
	comm      string
	state     string
	ppid      int
	cpuTicks  uint64
	startTick uint64
}

// parseStat reads the fields of /proc/<pid>/stat a reader needs, split
// through procstat.
func parseStat(data []byte) (procStat, error) {
	line, err := procstat.ParseStat(string(data))
	if err != nil {
		return procStat{}, fmt.Errorf("procmetrics: %w", err)
	}
	nums := map[int]uint64{}
	for _, n := range []int{statPPID, statUtime, statStime, statStarttime} {
		field, err := line.Field(n)
		if err != nil {
			return procStat{}, fmt.Errorf("procmetrics: %w", err)
		}
		if nums[n], err = strconv.ParseUint(field, 10, 64); err != nil {
			return procStat{}, fmt.Errorf("procmetrics: stat field %d %q: %w", n, field, err)
		}
	}
	return procStat{
		comm:      line.Comm,
		state:     line.Fields[0],
		ppid:      int(nums[statPPID]),
		cpuTicks:  nums[statUtime] + nums[statStime],
		startTick: nums[statStarttime],
	}, nil
}

// The stat fields read, numbered as proc(5) numbers them.
const (
	statPPID      = 4
	statUtime     = 14
	statStime     = 15
	statStarttime = 22
)

// bootUnixSeconds reads the host's boot time, /proc/stat's btime.
func (p procfs) bootUnixSeconds() (int64, error) {
	data, err := os.ReadFile(filepath.Join(p.root, "stat"))
	if err != nil {
		return 0, fmt.Errorf("procmetrics: read the host's stat: %w", err)
	}
	for line := range strings.SplitSeq(string(data), "\n") {
		if v, ok := strings.CutPrefix(line, "btime "); ok {
			n, err := strconv.ParseInt(strings.TrimSpace(v), 10, 64)
			if err != nil {
				return 0, fmt.Errorf("procmetrics: btime %q: %w", v, err)
			}
			return n, nil
		}
	}
	return 0, errors.New("procmetrics: the host's stat names no btime")
}

// Identity reads stat and the executable. The name is the executable's base
// name, as macOS names a process; /proc's own command name is cut at 15
// bytes ("shim-claude-sid"), so it is the name only where the executable
// cannot be read: a kernel thread, or another user's process.
func (p procfs) Identity(pid int) (Identity, error) {
	data, err := p.readPID(pid, "stat")
	if err != nil {
		return Identity{}, err
	}
	st, err := parseStat(data)
	if err != nil {
		return Identity{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	boot, err := p.boot()
	if err != nil {
		return Identity{}, err
	}
	name := st.comm
	exe, err := os.Readlink(filepath.Join(p.pidDir(pid), "exe"))
	switch {
	case err == nil:
		name = filepath.Base(strings.TrimSuffix(exe, " (deleted)"))
	case errors.Is(err, fs.ErrPermission), errors.Is(err, fs.ErrNotExist):
		// A kernel thread has no executable, another user's process hides it,
		// and a process that just exited is told by the next read.
	default:
		return Identity{}, fmt.Errorf("procmetrics: read the executable of pid %d: %w", pid, err)
	}
	return Identity{
		PID:             pid,
		PPID:            st.ppid,
		Name:            name,
		StartUnixMicros: boot*1e6 + int64(st.startTick)*1e6/userHZ,
	}, nil
}

// statusFields parses /proc/<pid>/status's "Key: value" lines.
func statusFields(data []byte) map[string]string {
	out := map[string]string{}
	for line := range strings.SplitSeq(string(data), "\n") {
		if k, v, ok := strings.Cut(line, ":"); ok {
			out[k] = strings.TrimSpace(v)
		}
	}
	return out
}

// kibField reads a status or meminfo field written as "<n> kB", in bytes.
func kibField(fields map[string]string, key string) (uint64, error) {
	v, ok := fields[key]
	if !ok {
		return 0, fmt.Errorf("procmetrics: no %s field", key)
	}
	n, err := strconv.ParseUint(strings.TrimSuffix(v, " kB"), 10, 64)
	if err != nil {
		return 0, fmt.Errorf("procmetrics: %s %q: %w", key, v, err)
	}
	return n * 1024, nil
}

// ioFields parses /proc/<pid>/io's "key: value" lines.
func ioFields(data []byte) (read, written uint64, err error) {
	fields := statusFields(data)
	for key, dst := range map[string]*uint64{"read_bytes": &read, "write_bytes": &written} {
		v, ok := fields[key]
		if !ok {
			return 0, 0, fmt.Errorf("procmetrics: io names no %s", key)
		}
		if *dst, err = strconv.ParseUint(v, 10, 64); err != nil {
			return 0, 0, fmt.Errorf("procmetrics: io %s %q: %w", key, v, err)
		}
	}
	return read, written, nil
}

// Usage reads stat, status, io and the descriptor table. The footprint is
// the process's private anonymous memory, resident or swapped, which is what
// macOS's physical footprint counts on Linux's terms. Linux attributes no
// energy to a process, so EnergyJoules stays zero (Measures says so).
func (p procfs) Usage(pid int) (Usage, error) {
	statData, err := p.readPID(pid, "stat")
	if err != nil {
		return Usage{}, err
	}
	st, err := parseStat(statData)
	if err != nil {
		return Usage{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	if st.state == zombie {
		// An exited process its parent has not reaped yet holds no memory,
		// descriptors or status lines left to read.
		return Usage{}, ErrGone
	}
	statusData, err := p.readPID(pid, "status")
	if err != nil {
		return Usage{}, err
	}
	status := statusFields(statusData)
	resident, err := kibField(status, "VmRSS")
	if err != nil {
		return Usage{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	anon, err := kibField(status, "RssAnon")
	if err != nil {
		return Usage{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	swapped, err := kibField(status, "VmSwap")
	if err != nil {
		return Usage{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	threads, err := strconv.Atoi(status["Threads"])
	if err != nil {
		return Usage{}, fmt.Errorf("procmetrics: pid %d threads %q: %w", pid, status["Threads"], err)
	}
	ioData, err := p.readPID(pid, "io")
	if err != nil {
		return Usage{}, err
	}
	read, written, err := ioFields(ioData)
	if err != nil {
		return Usage{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	fds, err := os.ReadDir(filepath.Join(p.pidDir(pid), "fd"))
	if err != nil {
		if goneErr(err) {
			return Usage{}, ErrGone
		}
		return Usage{}, fmt.Errorf("procmetrics: list the descriptors of pid %d: %w", pid, err)
	}
	return Usage{
		CPUSeconds:     float64(st.cpuTicks) / userHZ,
		ResidentBytes:  resident,
		FootprintBytes: anon + swapped,
		DiskReadBytes:  read,
		DiskWriteBytes: written,
		Threads:        threads,
		OpenFDs:        len(fds),
	}, nil
}

// Args reads cmdline.
func (p procfs) Args(pid int) ([]string, error) {
	data, err := p.readPID(pid, "cmdline")
	if err != nil {
		return nil, err
	}
	return procstat.ParseCmdline(data), nil
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
	data, err := os.ReadFile(filepath.Join(p.root, "meminfo"))
	if err != nil {
		return nil, 0, fmt.Errorf("procmetrics: read meminfo: %w", err)
	}
	fields := statusFields(data)
	states := make(map[string]uint64, len(meminfoStates))
	for key, state := range meminfoStates {
		if states[state], err = kibField(fields, key); err != nil {
			return nil, 0, fmt.Errorf("meminfo: %w", err)
		}
	}
	total, err := kibField(fields, "MemTotal")
	if err != nil {
		return nil, 0, fmt.Errorf("meminfo: %w", err)
	}
	return states, total, nil
}
