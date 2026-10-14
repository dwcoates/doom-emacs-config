package life

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"sync"
	"syscall"
	"time"

	"agentrepl/testrun/reap"
)

// Runner is the runner's side of the run's lifetime: its session, its
// lifeline from the sentinel, and the warden's lifeline it holds.
type Runner struct {
	// Own names the run's processes: the runner's session, every directory
	// the run told the warden about, and never the runner or its warden.
	Own reap.Members

	log    Logger
	reap   func(reap.Members) (reap.Result, error)
	warden *os.File
	mu     sync.Mutex
}

// WardenCommand is how the runner starts its warden: the program and its
// arguments before the runner's own pid and start, which StartRunner appends
// as `--runner PID --since UNIX --sentinel PID`.
type WardenCommand struct {
	Name string
	Args []string
}

// StartRunner takes up the runner's role: it checks it leads its own session
// (Sentinel starts it so), reads when it started from table, and starts the
// warden in a session of its own, holding its lifeline.
//
// A runner that does not lead its session was not started by a sentinel,
// and every reap it would make would take the wrong processes; that is a
// wiring defect, answered as an error before anything runs.
func StartRunner(table reap.Table, reapFn func(reap.Members) (reap.Result, error), warden WardenCommand, log Logger) (*Runner, error) {
	pid := os.Getpid()
	sid, err := reap.Getsid(pid)
	if err != nil {
		return nil, fmt.Errorf("read the runner's session: %w", err)
	}
	if sid != pid {
		return nil, fmt.Errorf("the runner pid %d is in session %d, not one it leads: it was not started by testrun's sentinel", pid, sid)
	}
	since, err := startOf(table, pid)
	if err != nil {
		return nil, err
	}
	// THE SENTINEL IS NEVER A MEMBER, though its arguments or environment may
	// name the run's directories: it is the process the invoker waits on.
	sentinel := os.Getppid()
	r := &Runner{
		Own:  reap.Members{What: fmt.Sprintf("run %d", pid), Session: pid, Since: since, Except: []int{pid, sentinel}},
		log:  log,
		reap: reapFn,
	}
	// The warden's lifeline is CLOEXEC on this side, so no unit inherits its
	// write end and keeps it open past the runner's death.
	lifeR, lifeW, err := os.Pipe()
	if err != nil {
		return nil, fmt.Errorf("make the warden's lifeline: %w", err)
	}
	cmd := exec.Command(warden.Name, append(append([]string(nil), warden.Args...), "--runner", strconv.Itoa(pid), "--since", strconv.FormatInt(since.Unix(), 10), "--sentinel", strconv.Itoa(sentinel))...)
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	cmd.ExtraFiles = []*os.File{lifeR}
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	err = cmd.Start()
	lifeR.Close()
	if err != nil {
		lifeW.Close()
		return nil, fmt.Errorf("start the warden %s: %w", warden.Name, err)
	}
	// The warden is never waited on by name: it outlives this process when
	// this process is killed, and it is reaped by whoever its parent is then.
	go func() { _ = cmd.Wait() }()
	r.warden = lifeW
	r.Own.Except = append(r.Own.Except, cmd.Process.Pid)
	log.Infof("the run's session is %d; its warden is pid %d", pid, cmd.Process.Pid)
	return r, nil
}

// startOf answers when pid started, as table lists it.
func startOf(table reap.Table, pid int) (time.Time, error) {
	procs, err := table.List()
	if err != nil {
		return time.Time{}, fmt.Errorf("read when the runner started: %w", err)
	}
	for _, p := range procs {
		if p.Pid == pid {
			return p.Start, nil
		}
	}
	return time.Time{}, fmt.Errorf("the runner pid %d is not in the process table", pid)
}

// Lifeline answers a channel closed once the sentinel is gone: EOF on the
// lifeline it holds the write end of. The descriptor is made close-on-exec
// first, so no unit holds it.
func Lifeline() (<-chan struct{}, error) {
	syscall.CloseOnExec(lifelineFD)
	f := os.NewFile(lifelineFD, "lifeline")
	if f == nil {
		return nil, errors.New("the runner has no lifeline descriptor")
	}
	gone := make(chan struct{})
	go func() {
		defer close(gone)
		_, _ = io.Copy(io.Discard, f)
	}()
	return gone, nil
}

// Root names a directory of the run's to the warden, which reaps what runs out
// of it and removes it if the runner dies, and adds it to the run's directory
// net.
func (r *Runner) Root(dir string) error {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.Own.Dirs = append(r.Own.Dirs, dir)
	return r.tell("root " + dir)
}

// Members answers the run's members as they stand.
func (r *Runner) Members() reap.Members {
	r.mu.Lock()
	defer r.mu.Unlock()
	m := r.Own
	m.Dirs = append([]string(nil), r.Own.Dirs...)
	return m
}

// ReapSession reaps every member of the run but the runner and its warden.
func (r *Runner) ReapSession() error {
	_, err := r.reap(r.Members())
	return err
}

// CancelOnLifeline calls cancel and reaps the session the moment gone closes:
// the sentinel is gone, so the run was cut.
func (r *Runner) CancelOnLifeline(gone <-chan struct{}, cancel func()) {
	go func() {
		<-gone
		r.log.Infof("the sentinel is gone (the run was cut): the run is cancelled and its session %d reaped", r.Own.Session)
		cancel()
		// The error is logged by the reap; the release reaps again and keeps
		// the disk if anything still remains.
		_ = r.ReapSession()
	}()
}

// Finish ends the run whose suites answered code: it reaps the session,
// calls release (the run's disk), and tells the warden the run is done. It
// answers the run's exit status: non-zero when anything failed, and when the
// run was cut (gone closed), whatever the suites answered.
func (r *Runner) Finish(code int, gone <-chan struct{}, release func() error) int {
	if err := r.ReapSession(); err != nil {
		code = 1
	}
	if err := release(); err != nil {
		r.log.Errorf("%v", err)
		code = 1
	}
	select {
	case <-gone:
		r.log.Errorf("the run was cut (its sentinel is gone); it reaped its session %d and released its disk", r.Own.Session)
		code = 1
	default:
	}
	if err := r.Done(); err != nil {
		r.log.Errorf("%v", err)
		code = 1
	}
	return code
}

// Done tells the warden the run released everything, so it leaves without
// reaping, and lets go of its lifeline.
func (r *Runner) Done() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	err := r.tell("done")
	if closeErr := r.warden.Close(); closeErr != nil {
		err = errors.Join(err, fmt.Errorf("close the warden's lifeline: %w", closeErr))
	}
	return err
}

// tell writes one line on the warden's lifeline. The caller holds r.mu.
func (r *Runner) tell(line string) error {
	if _, err := io.WriteString(r.warden, line+"\n"); err != nil {
		return fmt.Errorf("tell the warden %q: %w", line, err)
	}
	return nil
}

// wardenMessages reads the runner's lines: "root DIR" and "done".
func wardenMessages(in io.Reader) (roots []string, done bool, err error) {
	sc := bufio.NewScanner(in)
	for sc.Scan() {
		line := sc.Text()
		switch {
		case line == "done":
			done = true
		case len(line) > len("root ") && line[:len("root ")] == "root ":
			roots = append(roots, line[len("root "):])
		default:
			return roots, done, fmt.Errorf("the runner wrote %q, which is no warden message", line)
		}
	}
	return roots, done, sc.Err()
}
