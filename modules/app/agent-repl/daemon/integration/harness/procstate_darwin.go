package harness

import (
	"errors"
	"fmt"
	"unsafe"

	"golang.org/x/sys/unix"

	"agent-repld/internal/procgroup"
)

// readProcessState reads pid's state from the kernel's process table
// (procgroup.ReadState).
func readProcessState(pid int) (processState, error) {
	st, err := procgroup.ReadState(pid)
	return fromState(st), err
}

// threadsHalted reports whether none of a stopped pid's threads runs, and
// how many still do. A thread the kernel wakes after this (a child's exit, a
// socket's data) is held by the task's suspension before it can return to
// user mode, so once this holds the process can observe nothing.
//
// EVERY THREAD STILL TO BE HALTED AWAITS ONLY THE CPU. TH_STATE_RUNNING is a
// thread on a CPU or on a run queue, and the task's suspension holds it at
// its next return to user mode; every other state (waiting, uninterruptible,
// halted) is held already. So a not-halted answer is always awaitingCPU.
func threadsHalted(pid int) (halted, awaitingCPU bool, name string, err error) {
	running, err := runningThreads(pid)
	if errors.Is(err, unix.ESRCH) {
		return true, false, "gone", nil
	}
	if err != nil {
		return false, false, "", fmt.Errorf("read the threads of process %d: %w", pid, err)
	}
	if running > 0 {
		return false, true, fmt.Sprintf("stopping (%d thread(s) still running)", running), nil
	}
	return true, false, "", nil
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
