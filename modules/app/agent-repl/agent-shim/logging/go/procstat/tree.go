package procstat

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
	"time"
)

// ErrGone is a read of a process that exited since it was listed.
var ErrGone = errors.New("procstat: the process is gone")

// UserHZ is the unit of /proc's tick counts: the kernel ABI fixes USER_HZ at
// 100 on every architecture, whatever the kernel's own HZ.
const UserHZ = 100

// The stat fields the readers use, numbered as proc(5) numbers them.
const (
	FieldPPID      = 4
	FieldPGID      = 5
	FieldUtime     = 14
	FieldStime     = 15
	FieldCutime    = 16
	FieldCstime    = 17
	FieldStarttime = 22
)

// Tree is a /proc tree: the host's at "/proc", a fixture's in tests. Every
// Linux process reader of agent-repl reads through one, so each file is read
// and its gone process told one way.
type Tree struct {
	root string
	// boot is the host's boot time, read once: btime holds for the boot.
	boot func() (int64, error)
}

// New reads the /proc tree at root.
func New(root string) Tree {
	t := Tree{root: root}
	t.boot = sync.OnceValues(t.bootUnixSeconds)
	return t
}

// Root answers the tree's root.
func (t Tree) Root() string { return t.root }

// PIDs lists every numeric directory of the tree.
func (t Tree) PIDs() ([]int, error) {
	entries, err := os.ReadDir(t.root)
	if err != nil {
		return nil, fmt.Errorf("list the processes in %s: %w", t.root, err)
	}
	pids := make([]int, 0, len(entries))
	for _, e := range entries {
		if pid, err := strconv.Atoi(e.Name()); err == nil && pid > 0 {
			pids = append(pids, pid)
		}
	}
	return pids, nil
}

// Path answers the path of one file of a process.
func (t Tree) Path(pid int, name string) string {
	return filepath.Join(t.root, strconv.Itoa(pid), name)
}

// Read reads one file of a process, ErrGone once the process exited. Any
// other failure keeps its cause (a permission refusal is fs.ErrPermission).
func (t Tree) Read(pid int, name string) ([]byte, error) {
	data, err := os.ReadFile(t.Path(pid, name))
	if err != nil {
		if Gone(err) {
			return nil, ErrGone
		}
		return nil, fmt.Errorf("read %s of pid %d: %w", name, pid, err)
	}
	return data, nil
}

// Gone answers whether a read of a process's file failed because the
// process exited.
func Gone(err error) bool {
	return errors.Is(err, ErrGone) || errors.Is(err, fs.ErrNotExist) || errors.Is(err, syscall.ESRCH)
}

// Stat reads and splits a process's stat.
func (t Tree) Stat(pid int) (StatLine, error) {
	data, err := t.Read(pid, "stat")
	if err != nil {
		return StatLine{}, err
	}
	line, err := ParseStat(string(data))
	if err != nil {
		return StatLine{}, fmt.Errorf("pid %d: %w", pid, err)
	}
	return line, nil
}

// Status reads a process's status, its "Key: value" lines.
func (t Tree) Status(pid int) (map[string]string, error) {
	data, err := t.Read(pid, "status")
	if err != nil {
		return nil, err
	}
	return ParseKeyed(data), nil
}

// Cmdline reads a process's argv.
func (t Tree) Cmdline(pid int) ([]string, error) {
	data, err := t.Read(pid, "cmdline")
	if err != nil {
		return nil, err
	}
	return ParseCmdline(data), nil
}

// Environ reads a process's environment, which only its own user may.
func (t Tree) Environ(pid int) ([]string, error) {
	data, err := t.Read(pid, "environ")
	if err != nil {
		return nil, err
	}
	return ParseCmdline(data), nil
}

// Start answers when the process of a stat line started.
func (t Tree) Start(line StatLine) (time.Time, error) {
	ticks, err := line.Uint(FieldStarttime)
	if err != nil {
		return time.Time{}, err
	}
	boot, err := t.boot()
	if err != nil {
		return time.Time{}, err
	}
	return time.Unix(boot, 0).Add(time.Duration(ticks) * time.Second / UserHZ), nil
}

// bootUnixSeconds reads the host's boot time, /proc/stat's btime.
func (t Tree) bootUnixSeconds() (int64, error) {
	data, err := os.ReadFile(filepath.Join(t.root, "stat"))
	if err != nil {
		return 0, fmt.Errorf("read the host's stat: %w", err)
	}
	v, ok := ParseKeyed(data)["btime"]
	if !ok {
		return 0, errors.New("the host's stat names no btime")
	}
	n, err := strconv.ParseInt(v, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("btime %q: %w", v, err)
	}
	return n, nil
}

// ParseKeyed parses a /proc file of "key: value" or "key value" lines
// (status, io, meminfo, the host's stat): each line's first word, without a
// trailing colon, keys the rest of the line, trimmed.
func ParseKeyed(data []byte) map[string]string {
	out := map[string]string{}
	for line := range strings.SplitSeq(string(data), "\n") {
		key, value, ok := strings.Cut(strings.TrimSpace(line), ":")
		if !ok || strings.ContainsAny(key, " \t") {
			key, value, ok = strings.Cut(strings.TrimSpace(line), " ")
		}
		if ok {
			out[key] = strings.TrimSpace(value)
		}
	}
	return out
}

// Uint answers stat field n as a number.
func (l StatLine) Uint(n int) (uint64, error) {
	field, err := l.Field(n)
	if err != nil {
		return 0, err
	}
	v, err := strconv.ParseUint(field, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("stat field %d %q: %w", n, field, err)
	}
	return v, nil
}

// KiB answers a keyed field written as "<n> kB", in bytes.
func KiB(fields map[string]string, key string) (uint64, error) {
	v, ok := fields[key]
	if !ok {
		return 0, fmt.Errorf("no %s field", key)
	}
	n, err := strconv.ParseUint(strings.TrimSuffix(v, " kB"), 10, 64)
	if err != nil {
		return 0, fmt.Errorf("%s %q: %w", key, v, err)
	}
	return n * 1024, nil
}
