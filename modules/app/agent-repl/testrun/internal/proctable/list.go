package proctable

import (
	"encoding/binary"
	"errors"
	"fmt"
	"syscall"
	"time"
)

// procRecord is what the kernel's BSD record of a process says.
type procRecord struct {
	Ppid, Pgid int
	Status     int8
	Start      time.Time
	Comm       string
}

// kernel is the reads List makes, each answering the kernel's errno: ESRCH
// for a process gone, EPERM for one this user may not read.
type kernel struct {
	// listPids answers every pid on the host.
	listPids func() ([]int, error)
	// record is a process's full BSD record (proc_info PROC_PIDTBSDINFO).
	record func(pid int) (procRecord, error)
	// shortRecord is the record any user may read of any process
	// (PROC_PIDT_SHORTBSDINFO): no start time.
	shortRecord func(pid int) (procRecord, error)
	// procArgs is the process's kern.procargs2 buffer.
	procArgs func(pid int) ([]byte, error)
}

// list reads every process through k.
func (k kernel) list() ([]Entry, error) {
	pids, err := k.listPids()
	if err != nil {
		return nil, fmt.Errorf("list the processes (proc_info listpids): %w", err)
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
			return Entry{}, false, fmt.Errorf("read pid %d's short record (proc_info shortbsdinfo): %w", pid, err)
		}
		if errors.Is(err, syscall.EPERM) {
			return Entry{Pid: pid, State: '?'}, true, nil
		}
	} else if err != nil && !errors.Is(err, syscall.ESRCH) {
		return Entry{}, false, fmt.Errorf("read pid %d's record (proc_info bsdinfo): %w", pid, err)
	}
	if errors.Is(err, syscall.ESRCH) {
		return Entry{}, false, nil
	}
	e := Entry{Pid: pid, Ppid: rec.Ppid, Pgid: rec.Pgid, State: stateLetter(rec.Status), Start: rec.Start, Comm: rec.Comm}
	if e.State == 'Z' {
		return e, true, nil
	}
	buf, err := k.procArgs(pid)
	switch {
	case err == nil:
		argv, env, perr := ParseProcArgs(buf)
		if perr != nil {
			return Entry{}, false, fmt.Errorf("read pid %d's command line: %w", pid, perr)
		}
		e.Argv, e.Env = argv, env
	case errors.Is(err, syscall.EINVAL), errors.Is(err, syscall.EPERM), errors.Is(err, syscall.ESRCH), errors.Is(err, syscall.EIO):
		// Exiting, mid-exec (EIO: its argument area cannot be read, measured
		// 2026-10-23 on a process exiting under a test), or not readable:
		// listed by its short name, as ps lists it.
	default:
		return Entry{}, false, fmt.Errorf("read pid %d's command line (sysctl kern.procargs2): %w", pid, err)
	}
	return e, true, nil
}

var errNoPath = errors.New("kern.procargs2: no executable path")

// nativeEndian is the byte order of the kernel's buffers on every host this
// runs on (arm64 and amd64 darwin).
var nativeEndian = binary.LittleEndian

func errShort(n int) error {
	return fmt.Errorf("kern.procargs2: %d bytes, too short for argc", n)
}
