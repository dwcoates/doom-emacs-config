package proctable

import (
	"errors"
	"fmt"
	"strings"
	"syscall"
	"time"
)

// MARKED PROCESSES: the processes of this user that started with one
// environment variable set, and the CPU each has used. testrun's CPU governor
// reads them every second to tell the tests' own CPU from the rest of the
// host's (cores.TestsCPU): every process a test run's unit starts inherits the
// run's mark (cores.NestedEnv), whatever group or session it moves to.
//
// IT READS ONLY WHAT NEVER BLOCKS on a wedged process (see the package
// comment): the BSD record, kern.procargs2, and the kernel's resource usage
// record (proc_pid_rusage), which the daemon's process metrics read of every
// process every few seconds. A process's mark is read once, from its
// environment, and remembered for as long as the same process (pid and start)
// is alive, so a scan reads the environment of new processes only.

// Usage is one marked process's CPU, user and system together.
type Usage struct {
	Pid, Ppid int
	// Start tells this process from an earlier one with its pid.
	Start time.Time
	// Mark is the value the process's environment gives the marking
	// variable, which tells one marked tree from another (a test run's unit
	// tag, cores.UnitEnv).
	Mark string
	// Own is the CPU seconds the process used itself.
	Own float64
	// Children is the CPU seconds of every child it reaped, theirs and their
	// own reaped children's: the kernel adds a child's whole use to its
	// parent's when the parent waits for it.
	Children float64
}

// markedKernel is the reads a scan makes, each answering the kernel's errno:
// ESRCH for a process gone, EPERM for one this user may not read.
type markedKernel struct {
	listPids func() ([]int, error)
	record   func(pid int) (procRecord, error)
	args     func(pid int) (argv, env []string, err error)
	// rusage is the process's own and reaped children's CPU seconds.
	rusage func(pid int) (own, children float64, err error)
}

// mark is what a scan remembers of a process it read the environment of.
type mark struct {
	start  time.Time
	marked bool
	value  string
}

// Marked scans for the processes of one user whose environment names Name.
type Marked struct {
	// Name is the environment variable that marks a process, whatever its
	// value.
	Name string
	// UID is the user whose processes are scanned; every other user's are
	// skipped unread.
	UID   int
	k     markedKernel
	marks map[int]mark
}

// Scan answers every marked process alive now, and its CPU.
func (m *Marked) Scan() ([]Usage, error) {
	if m.marks == nil {
		m.marks = map[int]mark{}
	}
	pids, err := m.k.listPids()
	if err != nil {
		return nil, fmt.Errorf("list the processes: %w", err)
	}
	alive := make(map[int]bool, len(pids))
	var out []Usage
	for _, pid := range pids {
		if pid <= 0 {
			continue
		}
		rec, err := m.k.record(pid)
		switch {
		case errors.Is(err, syscall.ESRCH), errors.Is(err, syscall.EPERM):
			// Gone since the list, or another user's.
			continue
		case err != nil:
			return nil, fmt.Errorf("read pid %d's record: %w", pid, err)
		}
		if rec.UID != m.UID {
			continue
		}
		alive[pid] = true
		value, marked, err := m.marked(pid, rec)
		if err != nil {
			return nil, err
		}
		if !marked {
			continue
		}
		own, children, err := m.k.rusage(pid)
		if errors.Is(err, syscall.ESRCH) {
			delete(alive, pid)
			continue
		}
		if err != nil {
			return nil, fmt.Errorf("read pid %d's CPU: %w", pid, err)
		}
		out = append(out, Usage{Pid: pid, Ppid: rec.Ppid, Start: rec.Start, Mark: value, Own: own, Children: children})
	}
	for pid := range m.marks {
		if !alive[pid] {
			delete(m.marks, pid)
		}
	}
	return out, nil
}

// marked answers whether the process carries the mark, and its value,
// reading its environment only the first time this process (pid and start)
// is seen. A
// process whose environment cannot be read right now (exiting, mid-exec, a
// zombie) is unmarked for this scan and read again by the next, unless it
// was already read.
func (m *Marked) marked(pid int, rec procRecord) (string, bool, error) {
	if known, ok := m.marks[pid]; ok && known.start.Equal(rec.Start) {
		return known.value, known.marked, nil
	}
	_, env, err := m.k.args(pid)
	switch {
	case err == nil:
	case errors.Is(err, syscall.EINVAL), errors.Is(err, syscall.EPERM), errors.Is(err, syscall.ESRCH), errors.Is(err, syscall.EIO):
		return "", false, nil
	default:
		return "", false, fmt.Errorf("read pid %d's environment: %w", pid, err)
	}
	found := mark{start: rec.Start}
	for _, kv := range env {
		if value, ok := strings.CutPrefix(kv, m.Name+"="); ok {
			found.marked, found.value = true, value
			break
		}
	}
	m.marks[pid] = found
	return found.value, found.marked, nil
}
