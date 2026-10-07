package rollout

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"syscall"
	"time"

	"claude-repld/internal/atomicfile"
)

// JoiningFlag is the successor's binding argv spelling. Go's flag package
// accepts one dash or two; this is the documented form.
const JoiningFlag = "--joining"

// JoiningAddrFile is the file the SUCCESSOR writes to report its own address
// back to the incumbent that spawned it.
//
// THIS IS NOT A DAEMON-TO-DAEMON CHANNEL. It is a one-shot, write-once report
// in the shared state root, in the same family as the intent manifest: the
// successor writes it the moment its listener is bound, the incumbent reads it
// once, and neither ever writes to the other again. A pipe or a socket between
// the two would be a channel and is ruled out.
const JoiningAddrFile = "joining.addr"

// JoiningAddrPath is where a successor reports its address under a state root.
func JoiningAddrPath(stateDir string) string {
	return filepath.Join(stateDir, JoiningAddrFile)
}

// successorArgv is the incumbent's CONFIGURATION argv with the joining flag
// pointed at it.
//
// The successor inherits the incumbent's configuration as well as its
// environment, for the reason the environment is inherited whole: a successor
// assembled from a curated list would differ from its incumbent in exactly the
// ways nobody thought to list. Every flag the operator or a test did pass (the
// account roots, the shim entry, the webapp dist, the prompts directory) is
// the incumbent's configuration, and a successor without it is a different
// daemon.
//
// It is built from the configuration (ProcessSpawner.Argv), NEVER from
// os.Args: os.Args also carries the flags that said how THIS process booted
// (`--joining`, `--replacing`), and a successor that inherited one refused the
// exclusive pair and exited before it bound, so every handover an incumbent
// booted as a replacement attempted failed (2026-09-30). Stripping the boot
// flags back out of os.Args is a list someone must remember to extend; an
// argv that never held them cannot carry one.
func successorArgv(config []string, address string) []string {
	return append(slices.Clone(config), JoiningFlag, address)
}

// ReportJoiningAddr is the SUCCESSOR's half: write this daemon's bound address
// where the incumbent that spawned it is waiting. It writes atomically, because
// a half-written address would be dialed as a real one.
func ReportJoiningAddr(stateDir, address string) error {
	if strings.TrimSpace(address) == "" {
		return fmt.Errorf("rollout: a joining daemon reports a non-blank address")
	}
	path := JoiningAddrPath(stateDir)
	if err := os.MkdirAll(stateDir, 0o755); err != nil {
		return fmt.Errorf("rollout: create the state root %s: %w", stateDir, err)
	}
	if err := atomicfile.Replace(path, []byte(address+"\n"), atomicfile.Options{Pattern: "joining-*.addr"}); err != nil {
		return fmt.Errorf("rollout: write the joining address report: %w", err)
	}
	return nil
}

// ReadJoiningAddr reads a reported address. The bool is false when none has
// been reported yet, which is the ordinary state while the successor is still
// binding.
func ReadJoiningAddr(stateDir string) (string, bool, error) {
	body, err := os.ReadFile(JoiningAddrPath(stateDir))
	if os.IsNotExist(err) {
		return "", false, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("rollout: read the joining address report: %w", err)
	}
	address := strings.TrimSpace(string(body))
	if address == "" {
		return "", false, nil
	}
	return address, true, nil
}

// ProcessSpawner is the production SuccessorSpawner: it starts the daemon
// binary again with --joining and waits for the child to report its address.
type ProcessSpawner struct {
	// Exe is the daemon binary to start.
	Exe string
	// StateDir is the shared state root the report travels through.
	StateDir string
	// Argv is the incumbent's CONFIGURATION argv: the flags every daemon it
	// spawns inherits, and none of the flags that said how it booted. The
	// daemon's own flag parse builds it (claude-repld's parseFlags), so a
	// boot flag is excluded by where it is declared, not by a strip list.
	Argv []string
	// Poll is how often the report file is checked. It is a field so a test
	// drives it, and it is a POLL rather than a watch because the report is a
	// single file written once: a filesystem watch would be more machinery for
	// the same answer.
	Poll time.Duration
	// Timeout bounds the wait. A successor that never reports is a failed
	// handover, and nothing is announced.
	Timeout time.Duration
	// StopGrace is how long a successor being stopped has to exit on SIGTERM
	// before it is SIGKILLed. A joining successor owns nothing yet, so its
	// orderly exit is short.
	StopGrace time.Duration
	// ReapBound bounds the wait for the reap after the SIGKILL. A process the
	// kernel was told to kill that is still not reaped past it is reported as
	// possibly alive, never assumed gone.
	ReapBound time.Duration
	// Probe asks a reported address for a health answer; see Successor.Ready.
	Probe HealthProbe
	// ProbeEvery and ProbeAttempt are the readiness wait's cadence and the
	// bound on one probe.
	ProbeEvery, ProbeAttempt time.Duration
}

// The production spawner's stop windows.
const (
	// DefaultSuccessorStopGrace is a joining successor's SIGTERM grace.
	DefaultSuccessorStopGrace = 5 * time.Second
	// DefaultSuccessorReapBound bounds the reap after the SIGKILL.
	DefaultSuccessorReapBound = 5 * time.Second
)

// NewProcessSpawner builds the production spawner.
func NewProcessSpawner(exe, stateDir string, argv []string) *ProcessSpawner {
	return &ProcessSpawner{
		Exe:       exe,
		StateDir:  stateDir,
		Argv:      argv,
		Poll:      50 * time.Millisecond,
		Timeout:   30 * time.Second,
		StopGrace: DefaultSuccessorStopGrace,
		ReapBound: DefaultSuccessorReapBound,

		Probe:        DaemonHealthProbe,
		ProbeEvery:   readyProbeEvery,
		ProbeAttempt: readyAttemptBound,
	}
}

// Spawn starts the successor and waits for its address.
//
// The child inherits THIS PROCESS'S ENVIRONMENT WHOLE — the state root, the
// store socket, the vendor guard, everything — because a successor assembled
// from a curated allowlist would differ from its incumbent in exactly the ways
// nobody thought to list.
//
// EVERY RETURN PAST THE START CARRIES THE HANDLE, the failures included: a
// successor that never reported is still a running process, and the caller is
// the one that stops it (see SuccessorSpawner).
func (s *ProcessSpawner) Spawn(ctx context.Context, incumbentAddress string) (Successor, error) {
	if strings.TrimSpace(s.Exe) == "" {
		return nil, fmt.Errorf("rollout: no daemon binary to spawn the successor from")
	}
	if strings.TrimSpace(incumbentAddress) == "" {
		return nil, fmt.Errorf("rollout: the successor is told the incumbent's address, never left to infer it")
	}
	// A report left by an earlier handover would be read as this one's answer.
	if err := os.Remove(JoiningAddrPath(s.StateDir)); err != nil && !os.IsNotExist(err) {
		return nil, fmt.Errorf("rollout: clear the stale joining address report: %w", err)
	}

	// NOT exec.CommandContext, AND THAT IS THE WHOLE POINT. CommandContext
	// kills the child when ctx is done, and ctx here is the incumbent's own
	// serving lifetime -- the very thing the handover ends. Bound that way the
	// successor was SIGKILLed the instant the outgoing daemon finished its
	// orderly exit: it served for the two seconds the exit takes, then died
	// without a shutdown record, and Emacs -- which had already adopted every
	// workspace onto it and promoted it to primary -- found the address it had
	// just been handed refusing connections. ctx still bounds the WAIT below,
	// which is this call's own work; it must not bound the process this call
	// exists to leave running.
	cmd := exec.Command(s.Exe, successorArgv(s.Argv, incumbentAddress)...)
	cmd.Env = os.Environ()
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	// ITS OWN SESSION, WHICH IS WHAT "OUTLIVES" ACTUALLY TAKES.
	//
	// Emacs spawns the incumbent through `make-process' with the default
	// connection type, so the daemon runs on a PTY that Emacs owns. A child
	// started plainly inherits that controlling terminal and the incumbent's
	// process group -- so when the incumbent exited and Emacs closed the pty
	// master, the kernel sent SIGHUP to the whole foreground group and took
	// the successor down with it. The successor died with no shutdown record,
	// two seconds after a handover that had already moved every workspace onto
	// it, and Emacs found the address it had just been promoted to refusing
	// connections.
	//
	// Setsid makes the successor a session leader with NO controlling
	// terminal, which is the structural form of the guarantee: no signal aimed
	// at the incumbent's terminal, session or process group can reach it. It
	// is not a matter of the incumbent exiting politely enough.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("rollout: start the successor %s: %w", s.Exe, err)
	}
	child := &processSuccessor{
		process:      cmd.Process,
		exited:       make(chan struct{}),
		stopGrace:    s.StopGrace,
		reapBound:    s.ReapBound,
		probe:        s.Probe,
		probeEvery:   s.ProbeEvery,
		probeAttempt: s.ProbeAttempt,
	}
	// The successor OUTLIVES this process by design, so nothing blocks on its
	// exit; this goroutine only reaps it and marks the reap, which is what a
	// Stop waits on as its proof that the process is gone.
	//
	// Wait's error is the child's EXIT, kept for the one reader that needs
	// it: a successor that ends before it proves it is serving is reported
	// with it (Ready). It is written before exited closes, so every reader
	// that has seen the close sees it.
	go func() {
		child.waitErr = cmd.Wait()
		close(child.exited)
	}()

	deadline := time.NewTimer(s.Timeout)
	defer deadline.Stop()
	poll := time.NewTicker(s.Poll)
	defer poll.Stop()
	// reaped is set once the successor has exited. A REAPED successor has
	// written everything it ever will, so the read that follows is the final
	// answer, not a race: a report written before the exit is still its
	// address (Ready then names the exit), and no report means it died without
	// one. A successor that exits at once -- a flag it refuses, a layout it
	// cannot read -- is named by its exit, never left for the deadline to
	// misreport as slowness.
	reaped := false
	for {
		address, reported, err := ReadJoiningAddr(s.StateDir)
		if err != nil {
			return child, err
		}
		if reported {
			child.address = address
			return child, nil
		}
		if reaped {
			return child, fmt.Errorf("rollout: the successor exited before it reported an address: %w", child.exitError())
		}
		select {
		case <-ctx.Done():
			return child, ctx.Err()
		case <-deadline.C:
			return child, fmt.Errorf("rollout: the successor did not report an address within %s", s.Timeout)
		case <-child.exited:
			reaped = true
		case <-poll.C:
		}
	}
}

// SpawnReplacement implements SuccessorSpawner.
//
// IT IS STARTED THE WAY A SUCCESSOR IS -- the argv and environment inherited,
// its own session so no signal aimed at this process's terminal reaches it --
// and it is REAPED the same way, off the caller, because it outlives nothing
// this process could wait on: it blocks on the boot claim until this process
// is gone.
func (s *ProcessSpawner) SpawnReplacement(_ context.Context) (int, error) {
	if strings.TrimSpace(s.Exe) == "" {
		return 0, fmt.Errorf("rollout: no daemon binary to spawn the replacement from")
	}
	cmd := exec.Command(s.Exe, replacementArgv(s.Argv)...)
	cmd.Env = os.Environ()
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		return 0, fmt.Errorf("rollout: start the replacement %s: %w", s.Exe, err)
	}
	// The wait status is the replacement's own exit, which its own run log
	// records; this process has exited long before it could act on it.
	go func() { _ = cmd.Wait() }()
	return cmd.Process.Pid, nil
}

// processSuccessor is the production Successor: a child process this daemon
// started and reaps.
type processSuccessor struct {
	address string
	process *os.Process
	// exited closes once the child is REAPED.
	exited chan struct{}

	stopGrace time.Duration
	reapBound time.Duration

	probe                    HealthProbe
	probeEvery, probeAttempt time.Duration
	// waitErr is the reap's answer, readable once exited has closed.
	waitErr error
}

// Address implements Successor.
func (p *processSuccessor) Address() string { return p.address }

// PID implements Successor.
func (p *processSuccessor) PID() int { return p.process.Pid }

// Ready implements Successor: a DaemonHealth round trip on the reported
// address, or the process's own end, whichever comes first.
func (p *processSuccessor) Ready(ctx context.Context) error {
	if p.probe == nil {
		return fmt.Errorf("rollout: the successor (pid %d) has no health probe to prove it is serving", p.process.Pid)
	}
	return awaitAnswer(ctx, p.probe, p.address, p.exited, p.exitError, p.probeEvery, p.probeAttempt)
}

// exitError renders the reaped exit. Called only after exited has closed.
func (p *processSuccessor) exitError() error {
	exit := "exit status 0"
	if p.waitErr != nil {
		exit = p.waitErr.Error()
	}
	return &SuccessorExitedError{PID: p.process.Pid, Exit: exit}
}

// Stop implements Successor: SIGTERM, the grace, SIGKILL, and the reap.
//
// THE REAP IS THE ONLY PROOF. A signal delivered is not a process gone, and
// the caller lowers the one-successor latch on this answer, so nil is
// answered only once this daemon's own Wait has collected the child. The
// signals go through os.Process rather than a raw kill(2) because os.Process
// refuses to signal a pid it has already reaped, which a recycled pid would
// otherwise make somebody else's.
func (p *processSuccessor) Stop(ctx context.Context) error {
	select {
	case <-p.exited:
		return nil
	default:
	}
	if err := p.process.Signal(syscall.SIGTERM); err != nil && !errors.Is(err, os.ErrProcessDone) {
		return fmt.Errorf("rollout: signal the successor %d to stop: %w", p.process.Pid, err)
	}
	grace := time.NewTimer(p.stopGrace)
	defer grace.Stop()
	select {
	case <-p.exited:
		return nil
	case <-grace.C:
	case <-ctx.Done():
	}
	if err := p.process.Kill(); err != nil && !errors.Is(err, os.ErrProcessDone) {
		return fmt.Errorf("rollout: kill the successor %d: %w", p.process.Pid, err)
	}
	bound := time.NewTimer(p.reapBound)
	defer bound.Stop()
	select {
	case <-p.exited:
		return nil
	case <-bound.C:
		return fmt.Errorf("rollout: the successor %d was killed but not reaped within %s; it may still be running",
			p.process.Pid, p.reapBound)
	}
}
