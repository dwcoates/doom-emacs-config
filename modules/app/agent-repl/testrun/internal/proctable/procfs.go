package proctable

import (
	"errors"
	"fmt"
	"io/fs"
	"strconv"
	"strings"
	"syscall"

	"agentrepl/logging/procstat"
)

// LINUX READS /proc (procstat.Tree), in process, never through ps. Unlike
// macOS's proc_info records, a read of a process's cmdline or environ takes
// its memory map's lock, as ps's does, so a process wedged holding that lock
// would block the read: the stat and status records the scan reads of every
// process take no such lock, and argv and the environment are read only of
// this user's processes. This file is free of a build tag so its reads are
// proven on every platform against a fixture tree; proctable_linux.go binds
// them to the host's /proc.

// procfsErrno answers a /proc read's failure as the errno the shared logic
// reads: ESRCH for a process gone, EPERM for one this user may not read.
func procfsErrno(call string, err error) error {
	switch {
	case procstat.Gone(err):
		return fmt.Errorf("%s: %v: %w", call, err, syscall.ESRCH)
	case errors.Is(err, fs.ErrPermission):
		return fmt.Errorf("%s: %v: %w", call, err, syscall.EPERM)
	default:
		return fmt.Errorf("%s: %w", call, err)
	}
}

// procfsKernel is List's reads of tree. Any user may read any process's stat
// and status, so the short record is the full one.
func procfsKernel(tree procstat.Tree) kernel {
	return kernel{
		listPids: func() ([]int, error) {
			pids, err := tree.PIDs()
			if err != nil {
				return nil, fmt.Errorf("/proc: %w", err)
			}
			return pids, nil
		},
		record:      procfsRecord(tree),
		shortRecord: procfsRecord(tree),
		args:        procfsArgs(tree),
	}
}

// procfsMarkedKernel is a scan's reads of tree.
func procfsMarkedKernel(tree procstat.Tree) markedKernel {
	k := procfsKernel(tree)
	return markedKernel{listPids: k.listPids, record: k.record, args: k.args, rusage: procfsRusage(tree)}
}

// procfsRecord reads a process's stat and its effective user from status.
func procfsRecord(tree procstat.Tree) func(pid int) (procRecord, error) {
	return func(pid int) (procRecord, error) {
		line, err := tree.Stat(pid)
		if err != nil {
			return procRecord{}, procfsErrno("/proc stat", err)
		}
		ids := map[int]uint64{}
		for _, n := range []int{procstat.FieldPPID, procstat.FieldPGID} {
			if ids[n], err = line.Uint(n); err != nil {
				return procRecord{}, procfsErrno("/proc stat", err)
			}
		}
		start, err := tree.Start(line)
		if err != nil {
			return procRecord{}, procfsErrno("/proc stat", err)
		}
		status, err := tree.Status(pid)
		if err != nil {
			return procRecord{}, procfsErrno("/proc status", err)
		}
		// Uid: real, effective, saved and filesystem user.
		uids := strings.Fields(status["Uid"])
		if len(uids) < 2 {
			return procRecord{}, fmt.Errorf("/proc status of pid %d: Uid %q names no effective user", pid, status["Uid"])
		}
		uid, err := strconv.Atoi(uids[1])
		if err != nil {
			return procRecord{}, fmt.Errorf("/proc status of pid %d: effective user %q: %w", pid, uids[1], err)
		}
		return procRecord{
			Ppid: int(ids[procstat.FieldPPID]), Pgid: int(ids[procstat.FieldPGID]), UID: uid,
			State: line.Fields[0][0], Start: start, Comm: line.Comm,
		}, nil
	}
}

// procfsArgs reads a process's cmdline and environ. Another user's
// environment is hidden from this one, so its argv is answered alone.
func procfsArgs(tree procstat.Tree) func(pid int) ([]string, []string, error) {
	return func(pid int) ([]string, []string, error) {
		argv, err := tree.Cmdline(pid)
		if err != nil {
			return nil, nil, procfsErrno("/proc cmdline", err)
		}
		env, err := tree.Environ(pid)
		switch {
		case errors.Is(err, fs.ErrPermission):
			return argv, nil, nil
		case err != nil:
			return nil, nil, procfsErrno("/proc environ", err)
		}
		return argv, env, nil
	}
}

// procfsRusage reads a process's own and reaped children's CPU from stat.
func procfsRusage(tree procstat.Tree) func(pid int) (float64, float64, error) {
	return func(pid int) (float64, float64, error) {
		line, err := tree.Stat(pid)
		if err != nil {
			return 0, 0, procfsErrno("/proc stat", err)
		}
		ticks := map[int]uint64{}
		for _, n := range []int{procstat.FieldUtime, procstat.FieldStime, procstat.FieldCutime, procstat.FieldCstime} {
			if ticks[n], err = line.Uint(n); err != nil {
				return 0, 0, procfsErrno("/proc stat", err)
			}
		}
		seconds := func(a, b int) float64 { return float64(ticks[a]+ticks[b]) / procstat.UserHZ }
		return seconds(procstat.FieldUtime, procstat.FieldStime), seconds(procstat.FieldCutime, procstat.FieldCstime), nil
	}
}
