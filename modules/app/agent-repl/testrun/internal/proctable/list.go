package proctable

import (
	"encoding/binary"
	"errors"
	"fmt"
	"syscall"
	"time"
)

// procRecord is what the kernel's record of a process says.
type procRecord struct {
	Ppid, Pgid int
	// UID is the process's effective user; the short record leaves it 0.
	UID int
	// State is ps's letter for the process's state ('Z' a zombie).
	State byte
	Start time.Time
	Comm  string
}

// kernel is the reads List makes, each answering the kernel's errno: ESRCH
// for a process gone, EPERM for one this user may not read. Each platform's
// reads name their own kernel call in their errors (proctable_darwin.go,
// proctable_linux.go).
type kernel struct {
	// listPids answers every pid on the host.
	listPids func() ([]int, error)
	// record is a process's full record.
	record func(pid int) (procRecord, error)
	// shortRecord is the record any user may read of any process: no start
	// time.
	shortRecord func(pid int) (procRecord, error)
	// args is the process's argv and environment.
	args func(pid int) (argv, env []string, err error)
}

// list reads every process through k.
func (k kernel) list() ([]Entry, error) {
	pids, err := k.listPids()
	if err != nil {
		return nil, fmt.Errorf("list the processes: %w", err)
	}
	var entries []Entry
	for _, pid := range pids {
		if pid <= 0 {
			continue
		}
		e, ok, err := k.entry(pid)
		if err != nil {
			return nil, err
		}
		if ok {
			entries = append(entries, e)
		}
	}
	return entries, nil
}

// entry reads one process; ok is false when it exited since the list was read.
// Another user's process is read from its short record, which carries no
// start; its command line, like ps's, is read all the same.
func (k kernel) entry(pid int) (Entry, bool, error) {
	rec, err := k.record(pid)
	if errors.Is(err, syscall.EPERM) {
		rec, err = k.shortRecord(pid)
		if err != nil && !errors.Is(err, syscall.ESRCH) && !errors.Is(err, syscall.EPERM) {
			return Entry{}, false, fmt.Errorf("read pid %d's short record: %w", pid, err)
		}
		if errors.Is(err, syscall.EPERM) {
			return Entry{Pid: pid, State: '?'}, true, nil
		}
	} else if err != nil && !errors.Is(err, syscall.ESRCH) {
		return Entry{}, false, fmt.Errorf("read pid %d's record: %w", pid, err)
	}
	if errors.Is(err, syscall.ESRCH) {
		return Entry{}, false, nil
	}
	e := Entry{Pid: pid, Ppid: rec.Ppid, Pgid: rec.Pgid, State: rec.State, Start: rec.Start, Comm: rec.Comm}
	if e.State == 'Z' {
		return e, true, nil
	}
	argv, env, err := k.args(pid)
	switch {
	case err == nil:
		e.Argv, e.Env = argv, env
	case errors.Is(err, syscall.EINVAL), errors.Is(err, syscall.EPERM), errors.Is(err, syscall.ESRCH), errors.Is(err, syscall.EIO):
		// Exiting, mid-exec (EIO: its argument area cannot be read, measured
		// 2026-10-23 on a process exiting under a test), or not readable:
		// listed by its short name, as ps lists it.
	default:
		return Entry{}, false, fmt.Errorf("read pid %d's command line: %w", pid, err)
	}
	return e, true, nil
}

// procargs2Args reads argv and the environment from a kern.procargs2 read,
// naming the call in its failure.
func procargs2Args(read func(pid int) ([]byte, error)) func(pid int) ([]string, []string, error) {
	return func(pid int) ([]string, []string, error) {
		buf, err := read(pid)
		if err != nil {
			return nil, nil, fmt.Errorf("sysctl kern.procargs2: %w", err)
		}
		return ParseProcArgs(buf)
	}
}

var errNoPath = errors.New("kern.procargs2: no executable path")

// nativeEndian is the byte order of the kernel's buffers on every host this
// runs on (arm64 and amd64 darwin).
var nativeEndian = binary.LittleEndian

func errShort(n int) error {
	return fmt.Errorf("kern.procargs2: %d bytes, too short for argc", n)
}
