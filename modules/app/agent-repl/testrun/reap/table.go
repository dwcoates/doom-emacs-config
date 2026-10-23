package reap

import (
	"errors"
	"fmt"
	"syscall"
	"time"

	"agentrepl/testrun/internal/proctable"
)

// OSTable is the host's process table.
type OSTable struct{}

// List implements Table: the kernel's own records of every process
// (proctable), and getsid(2) for each one's session.
//
// NEVER ps (2026-10-23): a listing blocked behind one process wedged in
// execve, unkillably, so a cancelled run's reap never returned and its RAM
// disk was never detached. proctable reads only what answers for a wedged
// process too.
func (OSTable) List() ([]Proc, error) {
	entries, err := proctable.List()
	if err != nil {
		return nil, err
	}
	return fromEntries(entries, Getsid)
}

// fromEntries makes processes of the table's entries, asking getsid for each
// one's session. A process gone before its getsid is left out; one of another
// user's sessions getsid may not read (EPERM) is listed in no session. Start
// is kept to the second: a run's members are matched against the second its
// session leader started (Members.Since, the warden's --since).
func fromEntries(entries []proctable.Entry, getsid func(int) (int, error)) ([]Proc, error) {
	var procs []Proc
	for _, e := range entries {
		sid, err := getsid(e.Pid)
		switch {
		case errors.Is(err, syscall.ESRCH):
			continue
		case errors.Is(err, syscall.EPERM):
			sid = -1
		case err != nil:
			return nil, fmt.Errorf("getsid(%d): %w", e.Pid, err)
		}
		procs = append(procs, Proc{Pid: e.Pid, Sid: sid, State: e.State, Start: e.Start.Truncate(time.Second), Line: e.Line()})
	}
	return procs, nil
}

// Signal implements Table.
func (OSTable) Signal(pid int, sig syscall.Signal) error { return syscall.Kill(pid, sig) }

// OSReap is Reap over the host's process table, logging to log: the reap every
// caller outside a test is given.
func OSReap(log Logger) func(Members) (Result, error) {
	return func(m Members) (Result, error) { return Reap(OSTable{}, m, log) }
}
