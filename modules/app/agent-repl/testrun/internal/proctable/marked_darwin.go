package proctable

import (
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// proc_pid_rusage(3) is proc_info's PIDRUSAGE call (libproc.c), whose
// RUSAGE_INFO_V1 record (sys/resource.h) is the first to carry the reaped
// children's CPU.
const (
	callPidRusage   = 9 // PROC_INFO_CALL_PIDRUSAGE
	rusageInfoV1    = 1 // RUSAGE_INFO_V1
	sizeofRusageV1  = 144
	offUserTime     = 16  // ri_user_time
	offSystemTime   = 24  // ri_system_time
	offChildUser    = 96  // ri_child_user_time
	offChildSystem  = 104 // ri_child_system_time
	timebaseSysctl  = "hw.tbfrequency"
	timebaseMissing = "the kernel's timebase frequency (sysctl hw.tbfrequency) is zero"
)

// NewMarked scans for this user's processes whose environment names name.
// It reads the kernel's timebase once: proc_pid_rusage answers CPU in mach
// absolute time units, hw.tbfrequency of them a second (24 MHz on Apple
// silicon, 1 GHz on Intel).
func NewMarked(name string) (*Marked, error) {
	hz, err := unix.SysctlUint64(timebaseSysctl)
	if err != nil {
		return nil, fmt.Errorf("read the kernel's timebase frequency (sysctl %s): %w", timebaseSysctl, err)
	}
	if hz == 0 {
		return nil, fmt.Errorf("%s", timebaseMissing)
	}
	seconds := func(ticks uint64) float64 { return float64(ticks) / float64(hz) }
	rusage := func(pid int) (float64, float64, error) {
		buf := make([]byte, sizeofRusageV1)
		if _, err := procInfo(callPidRusage, pid, rusageInfoV1, buf); err != nil {
			return 0, 0, err
		}
		at := func(off int) uint64 { return nativeEndian.Uint64(buf[off:]) }
		own := seconds(at(offUserTime)) + seconds(at(offSystemTime))
		children := seconds(at(offChildUser)) + seconds(at(offChildSystem))
		return own, children, nil
	}
	return &Marked{
		Name: name,
		UID:  os.Geteuid(),
		k:    markedKernel{listPids: listPids, record: record, procArgs: procArgs, rusage: rusage},
	}, nil
}
