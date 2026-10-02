package run

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"syscall"
	"time"
)

// KillGrace is how long a unit's process group has to exit after SIGTERM
// before it is SIGKILLed.
const KillGrace = 10 * time.Second

// OSExec runs units as real processes, each the leader of its own process
// group so a kill reaches everything it started.
//
// Output goes to an unlinked temp FILE, not a pipe: a pipe would make Wait
// block until every descendant holding it closed it, so one leaked grandchild
// would hang the run. A file lets Wait return when the unit itself exits,
// exactly as the old serial script's `time cmd` did.
type OSExec struct {
	// Log records a kill that failed: Kill has no caller to return it to.
	Log *Log
	// Grace is how long a killed group has between SIGTERM and SIGKILL.
	Grace time.Duration
}

type osProcess struct {
	cmd   *exec.Cmd
	file  *os.File
	out   *bytes.Buffer
	done  chan struct{}
	log   *Log
	grace time.Duration
	id    string
}

// Start implements Executor.
func (e OSExec) Start(spec Spec, out *bytes.Buffer) (Process, error) {
	if e.Log == nil || e.Grace <= 0 {
		return nil, fmt.Errorf("run: OSExec needs a Log and a positive Grace (log set: %v, grace %v)", e.Log != nil, e.Grace)
	}
	if len(spec.Argv) == 0 {
		return nil, fmt.Errorf("run: unit %s has no command", spec.ID)
	}
	f, err := os.CreateTemp("", "agent-repl-unit-*")
	if err != nil {
		return nil, fmt.Errorf("run: create the output file for %s: %w", spec.ID, err)
	}
	if err := os.Remove(f.Name()); err != nil {
		f.Close()
		return nil, fmt.Errorf("run: unlink the output file %s of %s: %w", f.Name(), spec.ID, err)
	}
	cmd := exec.Command(spec.Argv[0], spec.Argv[1:]...)
	cmd.Dir = spec.Dir
	cmd.Env = append(os.Environ(), spec.Env...)
	cmd.Stdout, cmd.Stderr = f, f
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		f.Close()
		return nil, fmt.Errorf("run: start %s (%v in %s): %w", spec.ID, spec.Argv, spec.Dir, err)
	}
	return &osProcess{cmd: cmd, file: f, out: out, done: make(chan struct{}), log: e.Log, grace: e.Grace, id: spec.ID}, nil
}

// Wait implements Process.
func (p *osProcess) Wait() (int, float64, error) {
	defer close(p.done)
	defer p.file.Close()
	waitErr := p.cmd.Wait()
	if _, err := p.file.Seek(0, io.SeekStart); err != nil {
		return -1, 0, fmt.Errorf("run: rewind the output of pid %d: %w", p.cmd.Process.Pid, err)
	}
	if _, err := io.Copy(p.out, p.file); err != nil {
		return -1, 0, fmt.Errorf("run: read the output of pid %d: %w", p.cmd.Process.Pid, err)
	}
	st := p.cmd.ProcessState
	cpu := (st.UserTime() + st.SystemTime()).Seconds()
	var exitErr *exec.ExitError
	switch {
	case waitErr == nil:
		return 0, cpu, nil
	case errors.As(waitErr, &exitErr):
		if ws, ok := exitErr.Sys().(syscall.WaitStatus); ok && ws.Signaled() {
			return 128 + int(ws.Signal()), cpu, nil
		}
		return exitErr.ExitCode(), cpu, nil
	default:
		return -1, cpu, waitErr
	}
}

// Kill implements Process: SIGTERM to the group, SIGKILL after the grace.
func (p *osProcess) Kill() {
	pgid := p.cmd.Process.Pid
	p.signalGroup(pgid, syscall.SIGTERM)
	go func() {
		select {
		case <-p.done:
		case <-time.After(p.grace):
			p.signalGroup(pgid, syscall.SIGKILL)
		}
	}()
}

// signalGroup signals the unit's process group. A group that is already gone
// (ESRCH) has nothing left to stop; any other failure is logged, because the
// unit may then outlive the run.
func (p *osProcess) signalGroup(pgid int, sig syscall.Signal) {
	if err := syscall.Kill(-pgid, sig); err != nil && !errors.Is(err, syscall.ESRCH) {
		p.log.Errorf("unit %s: send %v to its process group %d: %v", p.id, sig, pgid, err)
	}
}

// WallClock is the real clock.
type WallClock struct{}

// Now implements Clock.
func (WallClock) Now() time.Time { return time.Now() }
