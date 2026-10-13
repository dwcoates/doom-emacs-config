package harness

import (
	"errors"
	"fmt"
	"unsafe"

	"golang.org/x/sys/unix"
)

// The p_stat values of <sys/proc.h>.
const (
	pStatIdle   = 1 // SIDL: being created by fork
	pStatRun    = 2 // SRUN
	pStatSleep  = 3 // SSLEEP
	pStatStop   = 4 // SSTOP
	pStatZombie = 5 // SZOMB
)

// pWExit is <sys/proc.h>'s P_WEXIT, the p_flag bit the kernel reports once a
// process has begun to exit.
const pWExit = 0x00002000

// readProcessState reads pid's p_stat and p_flag from the kernel's process
// table.
func readProcessState(pid int) (processState, error) {
	info, err := unix.SysctlKinfoProc("kern.proc.pid", pid)
	// THE KERNEL ANSWERS A PID IT HAS NO ENTRY FOR WITH AN EMPTY RECORD, which
	// SysctlKinfoProc reports as EIO: the process is gone, and a process that
	// is gone cannot run.
	if errors.Is(err, unix.EIO) {
		return processState{frozen: true, exited: true, name: "gone"}, nil
	}
	if err != nil {
		return processState{}, err
	}
	state, err := stateOf(info.Proc.P_stat, info.Proc.P_flag)
	if err != nil || !state.frozen || state.exited {
		return state, err
	}
	// A STOPPED p_stat IS NOT YET A STOPPED PROCESS. The kernel marks the
	// process SSTOP as it posts SIGSTOP, before its threads are suspended: a
	// thread already running finishes its slice first. Under a loaded host a
	// daemon read "stopped" here went on to read a frame from its shim, log it
	// and close a turn (TestTheTakeoversOrphanRecoveryReconcilesItsOpenTurn).
	// So the process is frozen only once none of its threads runs: a thread
	// woken after that is held by the task's suspension before it can return
	// to user mode, so it can observe nothing.
	running, err := runningThreads(pid)
	if errors.Is(err, unix.ESRCH) {
		return processState{frozen: true, exited: true, name: "gone"}, nil
	}
	if err != nil {
		return processState{}, fmt.Errorf("read the threads of process %d: %w", pid, err)
	}
	if running > 0 {
		return processState{name: fmt.Sprintf("stopping (%d thread(s) still running)", running)}, nil
	}
	return state, nil
}

// The <sys/proc_info.h> values runningThreads reads the kernel with.
const (
	sysProcInfo         = 336 // SYS_proc_info
	procInfoCallPIDInfo = 2   // PROC_INFO_CALL_PIDINFO
	procPIDListThreads  = 6   // PROC_PIDLISTTHREADS: the thread handles
	procPIDThreadInfo   = 5   // PROC_PIDTHREADINFO: one thread's info
	thStateRunning      = 1   // TH_STATE_RUNNING
)

// procThreadInfo is <sys/proc_info.h>'s struct proc_threadinfo.
type procThreadInfo struct {
	UserTime    uint64
	SystemTime  uint64
	CPUUsage    int32
	Policy      int32
	RunState    int32
	Flags       int32
	SleepTime   int32
	CurPri      int32
	Priority    int32
	MaxPriority int32
	Name        [64]byte
}

// runningThreads answers how many of pid's threads the kernel reports
// running (TH_STATE_RUNNING).
func runningThreads(pid int) (int, error) {
	handles := make([]uint64, 512)
	n, err := procInfo(pid, procPIDListThreads, 0, unsafe.Pointer(&handles[0]), len(handles)*8)
	if err != nil {
		return 0, err
	}
	running := 0
	for _, handle := range handles[:n/8] {
		var info procThreadInfo
		got, err := procInfo(pid, procPIDThreadInfo, handle, unsafe.Pointer(&info), int(unsafe.Sizeof(info)))
		if errors.Is(err, unix.ESRCH) && got == 0 {
			// The thread ended between the listing and this read: it runs no
			// longer.
			continue
		}
		if err != nil {
			return 0, err
		}
		if info.RunState == thStateRunning {
			running++
		}
	}
	return running, nil
}

// procInfo is the proc_info(2) PIDINFO call: FLAVOR of pid, with ARG, into
// the BUFSIZE bytes at BUF, answering the bytes the kernel wrote.
func procInfo(pid int, flavor int, arg uint64, buf unsafe.Pointer, bufsize int) (int, error) {
	r, _, errno := unix.Syscall6(sysProcInfo, procInfoCallPIDInfo, uintptr(pid), uintptr(flavor), uintptr(arg), uintptr(buf), uintptr(bufsize))
	if errno != 0 {
		return 0, errno
	}
	return int(r), nil
}

// stateOf classifies a process-table entry.
//
// A PROCESS WHOSE EXIT IS UNDER WAY HAS EXITED, whatever its p_stat says. The
// kernel sets P_WEXIT on the dying thread's own way out, after which it never
// returns to user mode, and it posts the exit event (the one WaitProcessExit
// waits on) before p_stat turns SZOMB: Kill read its SIGKILLed leader as
// running, with P_WEXIT set, in 4 of 2000 reads taken the moment that event
// arrived. Neither a stop nor anything else can bring such a process back.
func stateOf(stat int8, flag int32) (processState, error) {
	if flag&pWExit != 0 {
		return processState{frozen: true, exited: true, name: "exiting"}, nil
	}
	switch stat {
	case pStatStop:
		return processState{frozen: true, name: "stopped"}, nil
	case pStatZombie:
		return processState{frozen: true, exited: true, name: "a zombie"}, nil
	case pStatIdle:
		return processState{name: "being created"}, nil
	case pStatRun:
		return processState{name: "running"}, nil
	case pStatSleep:
		return processState{name: "sleeping"}, nil
	default:
		return processState{}, fmt.Errorf("unknown p_stat %d", stat)
	}
}

// liveGroupMembers answers the members of process group pgid that have not
// exited: everything in the group but its zombies and exiting processes.
func liveGroupMembers(pgid int) ([]groupMember, error) {
	members, err := unix.SysctlKinfoProcSlice("kern.proc.pgrp", pgid)
	if err != nil {
		return nil, fmt.Errorf("list process group %d: %w", pgid, err)
	}
	var live []groupMember
	for _, m := range members {
		state, err := stateOf(m.Proc.P_stat, m.Proc.P_flag)
		if err != nil {
			return nil, fmt.Errorf("process %d of group %d: %w", m.Proc.P_pid, pgid, err)
		}
		if !state.exited {
			live = append(live, groupMember{
				pid:   int(m.Proc.P_pid),
				comm:  unix.ByteSliceToString(m.Proc.P_comm[:]),
				state: state.name,
			})
		}
	}
	return live, nil
}
