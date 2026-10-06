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
	return stateOf(info.Proc.P_stat, info.Proc.P_flag)
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
