//go:build darwin

package vendortraffic

import (
	"bytes"
	"fmt"
	"os"
	"time"

	"golang.org/x/sys/unix"
)

// statisticsControl is the kernel control nettop reads.
const statisticsControl = "com.apple.network.statistics"

// sysprotoControl is SYSPROTO_CONTROL (sys/sys_domain.h), which x/sys does
// not name.
const sysprotoControl = 2

// receiveBuffer is the control socket's receive buffer. Every socket closing
// on the machine announces itself to every subscription, so a reader
// descheduled for a while must not have the closing update it cares about
// dropped behind a burst of removals it does not.
const receiveBuffer = 1 << 20

// DialStatistics opens one statistics control socket, unprivileged. The
// descriptor is non-blocking and handed to the runtime's poller, so a reader
// blocked in Read parks no thread and a Close wakes it.
func DialStatistics() (Conn, error) {
	fd, err := unix.Socket(unix.AF_SYSTEM, unix.SOCK_DGRAM, sysprotoControl)
	if err != nil {
		return nil, fmt.Errorf("vendortraffic: open a system control socket: %w", err)
	}
	info := &unix.CtlInfo{}
	copy(info.Name[:], statisticsControl)
	if err := unix.IoctlCtlInfo(fd, info); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("vendortraffic: resolve the %s control: %w", statisticsControl, err)
	}
	if err := unix.Connect(fd, &unix.SockaddrCtl{ID: info.Id, Unit: 0}); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("vendortraffic: connect the %s control: %w", statisticsControl, err)
	}
	if err := unix.SetsockoptInt(fd, unix.SOL_SOCKET, unix.SO_RCVBUF, receiveBuffer); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("vendortraffic: size the %s control's receive buffer to %d: %w", statisticsControl, receiveBuffer, err)
	}
	if err := unix.SetNonblock(fd, true); err != nil {
		_ = unix.Close(fd)
		return nil, fmt.Errorf("vendortraffic: make the %s control non-blocking: %w", statisticsControl, err)
	}
	return os.NewFile(uintptr(fd), statisticsControl), nil
}

// KernelProcesses is the kernel's process table.
type KernelProcesses struct{}

// Group lists a process group's members (sysctl kern.proc.pgrp). A group with
// no live member lists nothing.
func (KernelProcesses) Group(pgid int) ([]Proc, error) {
	procs, err := unix.SysctlKinfoProcSlice("kern.proc.pgrp", pgid)
	if err != nil {
		return nil, fmt.Errorf("vendortraffic: list process group %d: %w", pgid, err)
	}
	out := make([]Proc, 0, len(procs))
	for _, p := range procs {
		start := p.Proc.P_starttime
		out = append(out, Proc{
			PID:     int(p.Proc.P_pid),
			PPID:    int(p.Eproc.Ppid),
			Name:    string(bytes.TrimRight(p.Proc.P_comm[:], "\x00")),
			Started: time.Unix(start.Sec, int64(start.Usec)*int64(time.Microsecond)),
		})
	}
	return out, nil
}
