package reap

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// ListBound is how long one listing of the process table may take before it
// is taken not to answer, and the reap fails rather than kill blind.
//
// `ps -axE` answers in about 40ms on the owner's host (load 6, 2026-10-14).
// Like the RAM disk's probes, the bound covers a process started at the run's
// nice 19 on a loaded host, whose slowest healthy answer the runs record is
// 471ms (ramdisk.InfoBound): just over twice that.
const ListBound = 1000 * time.Millisecond

// lstartLayout is ps's `lstart` under LC_ALL=C, its fields joined by one space.
const lstartLayout = "Mon Jan 2 15:04:05 2006"

// psArgs lists every process: pid, state, start, then the command line with
// the environment after it.
var psArgs = []string{"-axEo", "pid=,stat=,lstart=,args="}

// OSTable is the host's process table.
type OSTable struct{}

// List implements Table: `ps` for the processes, getsid(2) for each one's
// session.
func (OSTable) List() ([]Proc, error) {
	ctx, cancel := context.WithTimeout(context.Background(), ListBound)
	defer cancel()
	cmd := exec.CommandContext(ctx, "ps", psArgs...)
	cmd.Env = append(os.Environ(), "LC_ALL=C")
	out, err := cmd.Output()
	if errors.Is(ctx.Err(), context.DeadlineExceeded) {
		return nil, fmt.Errorf("ps did not answer within %s", ListBound)
	}
	if err != nil {
		return nil, fmt.Errorf("ps %s: %w", strings.Join(psArgs, " "), err)
	}
	return parsePS(string(out), Getsid)
}

// parsePS reads `ps -o pid=,stat=,lstart=,args=` into processes, asking getsid
// for each one's session. A process gone before its getsid is left out; one
// of another user's sessions getsid may not read (EPERM) is listed in no
// session.
func parsePS(out string, getsid func(int) (int, error)) ([]Proc, error) {
	var procs []Proc
	for line := range strings.SplitSeq(out, "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) < 8 {
			return nil, fmt.Errorf("ps line %q has fewer fields than pid, state, start and command", line)
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil {
			return nil, fmt.Errorf("ps line %q names no pid: %w", line, err)
		}
		start, err := time.ParseInLocation(lstartLayout, strings.Join(fields[2:7], " "), time.Local)
		if err != nil {
			return nil, fmt.Errorf("ps line %q names no start time: %w", line, err)
		}
		sid, err := getsid(pid)
		switch {
		case errors.Is(err, syscall.ESRCH):
			continue
		case errors.Is(err, syscall.EPERM):
			sid = -1
		case err != nil:
			return nil, fmt.Errorf("getsid(%d): %w", pid, err)
		}
		procs = append(procs, Proc{Pid: pid, Sid: sid, State: fields[1][0], Start: start, Line: strings.Join(fields[7:], " ")})
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
