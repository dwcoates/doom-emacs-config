package harness

import (
	"errors"
	"fmt"

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

// readProcessState reads pid's p_stat from the kernel's process table.
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
	switch stat := info.Proc.P_stat; stat {
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
