package grantedclock

import (
	"errors"
	"fmt"
	"math/bits"
	"time"
	"unsafe"

	"golang.org/x/sys/unix"
)

// The <sys/proc_info.h> and <sys/resource.h> values the tree is read with.
const (
	sysProcInfo           = 336 // SYS_proc_info
	procInfoCallListPIDs  = 1   // PROC_INFO_CALL_LISTPIDS
	procInfoCallPIDRusage = 9   // PROC_INFO_CALL_PIDRUSAGE
	procPPIDOnly          = 6   // PROC_PPID_ONLY: the pids whose parent is typeinfo
	rusageInfoV4          = 4   // RUSAGE_INFO_V4, the first carrying ri_runnable_time
)

// rusageInfo is <sys/resource.h>'s struct rusage_info_v4, naming only the
// fields the tree reads. Its times are in mach absolute time units.
type rusageInfo struct {
	_                [16]byte  // ri_uuid
	_                [8]uint64 // ri_user_time .. ri_phys_footprint
	ProcStartAbstime uint64
	_                [25]uint64 // ri_proc_exit_abstime .. ri_interval_max_phys_footprint
	RunnableTime     uint64
}

// processTreeStall is the stall of this process's tree: root and every
// descendant, read through proc_info(2).
func processTreeStall(root int) (Stall, error) {
	freq, err := unix.SysctlUint64("hw.tbfrequency")
	if err != nil {
		return nil, fmt.Errorf("read the mach timebase frequency (hw.tbfrequency): %w", err)
	}
	if freq == 0 {
		return nil, errors.New("the mach timebase frequency (hw.tbfrequency) is zero")
	}
	tree := &processTree{
		root:     root,
		children: childPIDs,
		usage: func(pid int) (processUsage, usageState, error) {
			ri, state, err := pidRusage(pid)
			if err != nil || state != usageRead {
				return processUsage{}, state, err
			}
			return processUsage{start: ri.ProcStartAbstime, runnable: machDuration(ri.RunnableTime, freq)}, usageRead, nil
		},
	}
	return tree.read, nil
}

// pidRusage is proc_pid_rusage(3) for RUSAGE_INFO_V4: ESRCH is a pid that has
// exited and been reaped, EPERM a process running as another user (a setuid
// binary).
func pidRusage(pid int) (ri rusageInfo, state usageState, err error) {
	_, _, errno := unix.Syscall6(sysProcInfo, procInfoCallPIDRusage, uintptr(pid), rusageInfoV4, 0, uintptr(unsafe.Pointer(&ri)), 0)
	switch errno {
	case 0:
		return ri, usageRead, nil
	case unix.ESRCH:
		return ri, usageGone, nil
	case unix.EPERM:
		return ri, usageForbidden, nil
	default:
		return ri, usageRead, errno
	}
}

// childPIDs is proc_listchildpids(3): pid's direct children.
//
// A LISTING THAT FILLS ITS BUFFER MAY HAVE BEEN CUT SHORT, so it is taken
// again into one twice the size until it does not.
func childPIDs(pid int) ([]int, error) {
	for size := 64; ; size *= 2 {
		buf := make([]int32, size)
		n, _, errno := unix.Syscall6(sysProcInfo, procInfoCallListPIDs, procPPIDOnly, uintptr(pid), 0, uintptr(unsafe.Pointer(&buf[0])), uintptr(len(buf)*4))
		if errno != 0 {
			return nil, errno
		}
		count := int(n) / 4
		if count >= size {
			continue
		}
		kids := make([]int, 0, count)
		for _, kid := range buf[:count] {
			if kid != 0 {
				kids = append(kids, int(kid))
			}
		}
		return kids, nil
	}
}

// machDuration converts TICKS of a mach timebase running at FREQ Hz to a
// duration. The product is taken in 128 bits: a day of ticks at 24MHz times
// 1e9 overflows 64.
func machDuration(ticks, freq uint64) time.Duration {
	hi, lo := bits.Mul64(ticks, uint64(time.Second))
	ns, _ := bits.Div64(hi, lo, freq)
	return time.Duration(ns)
}
