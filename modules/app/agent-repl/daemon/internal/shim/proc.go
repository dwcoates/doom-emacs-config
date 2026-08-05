// Package shim spawns and supervises one TS shim subprocess per session,
// speaking Layer 1 (stdio NDJSON) as declared in internal/protocol.
package shim

import (
	"bufio"
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"
	"syscall"
	"time"

	"claude-repld/internal/protocol"
)

// maxEventLine bounds one shim stdout line (large tool results).
const maxEventLine = 32 * 1024 * 1024

// maxStderrTail bounds the stderr evidence retained per shim process.
//
// A shim that dies during bring-up explains itself in its last few lines — a
// node stack trace, a missing module, a rejected --resume — and that is the
// evidence a spawn failure has to carry. Retaining the WHOLE stream instead
// would make a chatty child an unbounded daemon allocation for the lifetime of
// a long-lived session, so the tail is capped and the drop is announced rather
// than hidden.
const maxStderrTail = 8 * 1024

// stderrTruncationMarker prefixes a tail that dropped older bytes, so a reader
// never mistakes a truncated tail for the child's complete stderr.
const stderrTruncationMarker = "[stderr truncated to the last 8192 bytes] "

const (
	stderrCloseOwnerUnclaimed = "unclaimed"
	stderrCloseOwnerProcWait  = "proc_wait"
	stderrCloseOwnerCaller    = "caller"

	stderrOutcomeCleanEOF            = "clean_eof"
	stderrOutcomeExpectedReaderClose = "expected_reader_close"
	stderrOutcomeScannerError        = "scanner_error"
)

// stderrTail is a byte-capped ring of the child's most recent stderr. It is an
// io.Writer so the same buffer serves both stderr paths: the parsed pump and
// the caller-supplied writer (which it tees).
type stderrTail struct {
	mu       sync.Mutex
	buf      []byte
	truncate bool
}

func (t *stderrTail) Write(p []byte) (int, error) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.buf = append(t.buf, p...)
	if len(t.buf) > maxStderrTail {
		t.truncate = true
		// Copied into a fresh slice rather than re-sliced: re-slicing keeps the
		// original array alive and lets its capacity creep upward, which is the
		// unbounded growth this cap exists to prevent.
		t.buf = append([]byte(nil), t.buf[len(t.buf)-maxStderrTail:]...)
	}
	return len(p), nil
}

// String renders the retained tail, marked when older bytes were dropped.
func (t *stderrTail) String() string {
	t.mu.Lock()
	defer t.mu.Unlock()
	if len(t.buf) == 0 {
		return ""
	}
	if t.truncate {
		return stderrTruncationMarker + string(t.buf)
	}
	return string(t.buf)
}

// ExitDescription renders a Wait error as the child's exit status: the code it
// exited with, the signal that killed it, or a clean exit.
//
// It never swallows an unexpected Wait failure — one that carries no exit
// status is reported verbatim, because "the daemon could not reap its own
// child" is itself the diagnosis.
func ExitDescription(waitErr error) string {
	if waitErr == nil {
		return "exit code 0"
	}
	var exitErr *exec.ExitError
	if errors.As(waitErr, &exitErr) {
		if status, ok := exitErr.Sys().(syscall.WaitStatus); ok && status.Signaled() {
			return fmt.Sprintf("killed by %s", status.Signal())
		}
		return fmt.Sprintf("exit code %d", exitErr.ExitCode())
	}
	return fmt.Sprintf("wait failed: %v", waitErr)
}

// ExitCode extracts the child's exit status: 0 for a clean exit, the process's
// own code for a non-zero one, and -1 when it was signalled or Wait failed
// with no status to report.
func ExitCode(waitErr error) int {
	if waitErr == nil {
		return 0
	}
	var exitErr *exec.ExitError
	if errors.As(waitErr, &exitErr) {
		return exitErr.ExitCode()
	}
	return -1
}

// Proc is a running shim subprocess.
//
// Events() yields decoded Layer-1 events until the shim's stdout closes,
// after which the channel is closed; the supervisor keys lifecycle off
// the `closed` event plus Wait()'s exit code.
type Proc struct {
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	events chan *protocol.L1Event
	logger Logger
	// stderr retains a bounded tail of the child's stderr so a process that
	// dies without ever speaking the protocol still has evidence to hand the
	// bring-up that was waiting for it.
	stderr *stderrTail
	// stderrPump tracks parsed stderr scan completion.  Proc.Wait and os/exec
	// own the reader close, while a caller owns the child's stream when Stderr
	// is supplied.
	stderrPump *stderrPump
	// pid and pgid are captured at spawn so a stop record names the same
	// process the spawn record did, even after the process has exited.
	pid  int
	pgid int

	mu      sync.Mutex
	stdinOK bool
}

// stderrLifecycle records the one authoritative account of why a child is
// stopping and who owns the reader close.  The mutex makes the expected-close
// classification structural: the scanner cannot observe the marker until the
// lifecycle owner has committed it.
type stderrLifecycle struct {
	mu sync.Mutex

	pid               int
	pgid              int
	shutdownInitiator string
	shutdownReason    string
	closeOwner        string
	closeExpected     bool
}

type stderrLifecycleSnapshot struct {
	pid               int
	pgid              int
	shutdownInitiator string
	shutdownReason    string
	closeOwner        string
	closeExpected     bool
}

// stderrPump owns daemon-parsed stderr scan completion and publishes it only
// after the scanner has made its final lifecycle classification.
type stderrPump struct {
	reader       io.ReadCloser
	logger       Logger
	tail         *stderrTail
	done         chan struct{}
	logLifecycle bool
	lifecycle    stderrLifecycle
}

func newStderrPump(reader io.ReadCloser, logger Logger, tail *stderrTail, pid, pgid int, logLifecycle bool) *stderrPump {
	return &stderrPump{
		reader:       reader,
		logger:       logger,
		tail:         tail,
		done:         make(chan struct{}),
		logLifecycle: logLifecycle,
		lifecycle: stderrLifecycle{
			pid:               pid,
			pgid:              pgid,
			shutdownInitiator: "child_exit",
			shutdownReason:    "no deliberate stop requested",
			closeOwner:        stderrCloseOwnerUnclaimed,
		},
	}
}

// signal holds lifecycle ownership across the system call so a scanner can
// never pair a failed signal with a deliberately-attributed shutdown.
func (p *stderrPump) signal(by Stop, send func() error) error {
	p.lifecycle.mu.Lock()
	defer p.lifecycle.mu.Unlock()
	if err := send(); err != nil {
		return err
	}
	p.lifecycle.shutdownInitiator = by.Initiator
	p.lifecycle.shutdownReason = by.Reason
	return nil
}

// expectClose names Proc.Wait as the logical close owner before os/exec's
// Wait closes its pipe reader after the child exits.  Closing it here would
// truncate a still-running child, so Wait is deliberately the only closer.
func (p *stderrPump) expectClose(owner string) {
	p.lifecycle.mu.Lock()
	defer p.lifecycle.mu.Unlock()
	if p.lifecycle.closeOwner == owner && p.lifecycle.closeExpected {
		return
	}
	if p.lifecycle.closeOwner != stderrCloseOwnerUnclaimed {
		panic(fmt.Sprintf("shim: stderr close owner already committed as %q", p.lifecycle.closeOwner))
	}
	p.lifecycle.closeOwner = owner
	p.lifecycle.closeExpected = true
}

func (p *stderrPump) snapshot() stderrLifecycleSnapshot {
	p.lifecycle.mu.Lock()
	defer p.lifecycle.mu.Unlock()
	return stderrLifecycleSnapshot{
		pid:               p.lifecycle.pid,
		pgid:              p.lifecycle.pgid,
		shutdownInitiator: p.lifecycle.shutdownInitiator,
		shutdownReason:    p.lifecycle.shutdownReason,
		closeOwner:        p.lifecycle.closeOwner,
		closeExpected:     p.lifecycle.closeExpected,
	}
}

func (p *stderrPump) wait() { <-p.done }

// Pid is the shim process's pid.
func (p *Proc) Pid() int { return p.pid }

// Pgid is the process group the shim was observed in immediately after its
// spawn. It equals Pid when the detachment took effect, and 0 when the group
// could not be read (which Spawn logs loudly).
func (p *Proc) Pgid() int { return p.pgid }

// Stop attributes a DELIBERATE shim stop: who asked and why.
//
// It is a REQUIRED argument rather than a convenience: a shim that dies for a
// reason nobody recorded is indistinguishable, from the log alone, from a shim
// that died because something went wrong. Making the attribution impossible to
// omit is what keeps that distinction real.
type Stop struct {
	// Initiator names the component that commanded the stop, e.g.
	// "session_controller_hibernate" or "daemon_shutdown".
	Initiator string
	// Reason states why, in a form a log reader can act on.
	Reason string
}

// Validate rejects an unattributed stop. Both halves are load-bearing: an
// initiator with no reason and a reason with no initiator each leave the log
// unable to answer the question the record exists for.
func (s Stop) Validate() error {
	switch {
	case s.Initiator == "":
		return fmt.Errorf("stop attribution needs an Initiator")
	case s.Reason == "":
		return fmt.Errorf("stop attribution needs a Reason")
	}
	return nil
}

// StderrTail returns the bounded tail of everything the child has written to
// stderr so far. Safe to call at any time, including after the process exits.
func (p *Proc) StderrTail() string { return p.stderr.String() }

// Options configures a shim spawn.
type Options struct {
	// Argv is the full command line, e.g. ["node", "dist/main.js", "--fake"].
	Argv []string
	// Dir is the subprocess working directory ("" = inherit).
	Dir string
	// ExtraEnv entries are appended to the inherited environment
	// (KEY=VALUE form). The SDK's claude subprocess inherits them from
	// the shim, which is how the AGENT_REPL_OWNED ownership marker
	// reaches the hook scripts.
	ExtraEnv []string
	// Logger receives shim stderr lines and protocol decode complaints. Required.
	// The spawning runtime binds session and process context before injection.
	Logger Logger
	// Stderr bypasses daemon parsing when the shim owns durable persistence via
	// an inherited log fd. The shim itself selects terminal verbosity.
	Stderr io.Writer
	// ExtraFiles are inherited verbatim by the child. UDS launchers use fd 3
	// for the already-open shim log target and never hand the child a pathname.
	ExtraFiles []*os.File
}

// Logger is the per-process diagnostic boundary supplied by the daemon runtime.
// It deliberately contains only the canonical normal and verbose emission paths.
type Logger interface {
	Log(format string, args ...any)
	LogVerbose(format string, args ...any)
}

// stderrMirror receives canonical shim JSON for terminal-only display when the
// shim owns the durable fd. Malformed stderr remains a daemon-owned error.
type stderrMirror interface{ MirrorShimRecord(line string) }

// detachedSysProcAttr is the process-attribute set that decouples a spawned
// shim's LIFETIME from the daemon's.
//
// THE DETACHMENT IS THE POINT, and it is the system's stated design premise:
// bootsweep.go says "a shim survives its daemon by design", and ShutdownCmd's
// default (stop_shims=false) PRESERVES shims across a daemon bounce. Without
// Setpgid that premise was false in practice — the shim was born into the
// DAEMON's process group, so the one signal that stops a daemon (delivered to
// its group by a supervisor or a shell) reached every shim too, and a bounce
// took all live sessions down with it.
//
// Setpgid puts the child in a NEW process group whose id is the child's own
// pid, so a group-directed signal aimed at the daemon can no longer reach it.
// It deliberately does NOT detach the child in any other way:
//
//   - No Setsid. The shim keeps the daemon's session, so it stays reapable by
//     the daemon that spawned it and Wait() keeps working exactly as before.
//   - No Pdeathsig (Linux) or equivalent: a parent-death signal would re-create
//     the very coupling this removes.
//   - The command is built with exec.Command and NOT exec.CommandContext, so no
//     Cancel/WaitDelay binds the child to a daemon-scoped context.
func detachedSysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{Setpgid: true}
}

// reportProcessGroup reads the freshly spawned child's process group and
// reports the detachment invariant Setpgid is supposed to have established:
// the child's group id equals its own pid.
//
// A violated or unreadable invariant is LOUD but not fatal to the spawn. The
// child is already running, so returning an error here would leak it; and the
// daemon losing the ability to name the group is exactly the condition that
// must not pass quietly, because the next daemon bounce is when it is paid for.
// A group that could not be read is reported as 0 — an obviously impossible
// group rather than a plausible wrong one.
//
// getpgid is a parameter so both failure branches are exercised by tests: a
// process whose group cannot be read, and a process that did NOT get its own
// group, are conditions no test can arrange against the real kernel call.
func reportProcessGroup(pid int, logger Logger, getpgid func(int) (int, error)) int {
	pgid, err := getpgid(pid)
	switch {
	case err != nil:
		logger.Log("shim: spawned pid %d but its process group could NOT be read, so its detachment from the daemon's process group is UNVERIFIED and a daemon bounce may take it down: %v", pid, err)
		return 0
	case pgid != pid:
		logger.Log("shim: spawned pid %d landed in process group %d instead of its own; it is STILL COUPLED to the daemon's process group and will die with the daemon", pid, pgid)
		return pgid
	}
	return pgid
}

// Spawn starts the shim subprocess and its stdout/stderr pumps.
func Spawn(opts Options) (*Proc, error) {
	if len(opts.Argv) == 0 {
		return nil, fmt.Errorf("shim: empty argv")
	}
	if opts.Logger == nil {
		return nil, fmt.Errorf("shim: Logger is required")
	}
	logger := opts.Logger
	cmd := exec.Command(opts.Argv[0], opts.Argv[1:]...)
	cmd.Dir = opts.Dir
	cmd.ExtraFiles = opts.ExtraFiles
	cmd.SysProcAttr = detachedSysProcAttr()
	if len(opts.ExtraEnv) > 0 {
		cmd.Env = append(os.Environ(), opts.ExtraEnv...)
	}
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return nil, fmt.Errorf("shim: stdin pipe: %w", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return nil, fmt.Errorf("shim: stdout pipe: %w", err)
	}
	tail := &stderrTail{}
	var stderr io.ReadCloser
	if opts.Stderr != nil {
		// Teed rather than replaced: the caller's writer keeps receiving the
		// unparsed stream exactly as before, and the tail gains the same bytes
		// so this path is no blinder than the parsed one.
		cmd.Stderr = io.MultiWriter(opts.Stderr, tail)
	} else {
		stderr, err = cmd.StderrPipe()
		if err != nil {
			return nil, fmt.Errorf("shim: stderr pipe: %w", err)
		}
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("shim: start %q: %w", opts.Argv[0], err)
	}

	pid := cmd.Process.Pid
	pgid := reportProcessGroup(pid, logger, syscall.Getpgid)

	p := &Proc{
		cmd:     cmd,
		stdin:   stdin,
		events:  make(chan *protocol.L1Event, 64),
		logger:  logger,
		stderr:  tail,
		pid:     pid,
		pgid:    pgid,
		stdinOK: true,
	}

	go p.pumpStdout(stdout, logger)
	if stderr != nil {
		p.stderrPump = newStderrPump(stderr, logger, tail, pid, pgid, true)
		go p.stderrPump.run()
	}
	return p, nil
}

func (p *Proc) pumpStdout(stdout io.Reader, logger Logger) {
	defer close(p.events)
	scanner := bufio.NewScanner(stdout)
	scanner.Buffer(make([]byte, 64*1024), maxEventLine)
	for scanner.Scan() {
		line := scanner.Bytes()
		if len(line) == 0 {
			continue
		}
		evt, err := protocol.DecodeL1Event(line)
		if err != nil {
			// A malformed line is a shim bug; surface it loudly as a
			// synthetic transport error event rather than dropping it.
			logger.Log("shim: undecodable event line: %v", err)
			p.events <- &protocol.L1Event{
				Type:    "error",
				Code:    "transport",
				Message: mustJSONString(fmt.Sprintf("undecodable shim event line: %v", err)),
			}
			continue
		}
		if evt == nil {
			continue // unknown type: ignored for forward compatibility
		}
		p.events <- evt
	}
	if err := scanner.Err(); err != nil {
		logger.Log("shim: stdout scan error: %v", err)
	}
}

func (p *stderrPump) run() {
	defer close(p.done)
	if p.logLifecycle {
		lifecycle := p.snapshot()
		p.logger.Log("shim: stderr scanner started pid=%d pgid=%d shutdown_initiator=%q shutdown_reason=%q close_owner=%q", lifecycle.pid, lifecycle.pgid, lifecycle.shutdownInitiator, lifecycle.shutdownReason, lifecycle.closeOwner)
	}
	scanner := bufio.NewScanner(p.reader)
	scanner.Buffer(make([]byte, 64*1024), maxEventLine)
	for scanner.Scan() {
		line := scanner.Text()
		// Retained BEFORE classification: a line that fails the shim's record
		// shape — a node stack trace, a loader error — is exactly the evidence
		// a spawn failure needs, so the tail must not be limited to the lines
		// the daemon can parse.
		if _, err := p.tail.Write([]byte(line + "\n")); err != nil {
			p.logger.Log("shim: retaining stderr tail: %v", err)
		}
		verbose, valid := shimRecord(line)
		if valid {
			if mirror, ok := p.logger.(stderrMirror); ok {
				mirror.MirrorShimRecord(line)
			} else if verbose {
				p.logger.LogVerbose("shim stderr: %s", line)
			} else {
				p.logger.Log("shim stderr: %s", line)
			}
			continue
		}
		p.logger.Log("shim stderr malformed: %s", line)
	}
	readErr := scanner.Err()
	lifecycle := p.snapshot()
	expectedClose := readErr != nil && lifecycle.closeExpected && lifecycle.closeOwner == stderrCloseOwnerProcWait && errors.Is(readErr, os.ErrClosed)
	outcome := stderrOutcomeCleanEOF
	if expectedClose {
		outcome = stderrOutcomeExpectedReaderClose
	} else if readErr != nil {
		outcome = stderrOutcomeScannerError
	}
	if readErr != nil && !expectedClose {
		p.logger.Log("shim: stderr scan error: %v [pid=%d pgid=%d shutdown_initiator=%q shutdown_reason=%q close_owner=%q close_expected=%t]", readErr, lifecycle.pid, lifecycle.pgid, lifecycle.shutdownInitiator, lifecycle.shutdownReason, lifecycle.closeOwner, lifecycle.closeExpected)
	}
	if p.logLifecycle {
		p.logger.Log("shim: stderr scanner completed pid=%d pgid=%d shutdown_initiator=%q shutdown_reason=%q close_owner=%q close_expected=%t outcome=%s expected_close=%t", lifecycle.pid, lifecycle.pgid, lifecycle.shutdownInitiator, lifecycle.shutdownReason, lifecycle.closeOwner, lifecycle.closeExpected, outcome, expectedClose)
	}
}

// pumpStderr is retained for focused scanner tests.  A caller that invokes the
// pump directly owns no child lifecycle, so every scanner failure is reported
// as unexpected.
func pumpStderr(stderr io.Reader, logger Logger, tail *stderrTail) {
	pump := newStderrPump(io.NopCloser(stderr), logger, tail, 0, 0, false)
	pump.run()
}

// shimVerboseRecord recognizes only the shim runtime's stable, field-shaped
// verbosity marker. All unmarked or malformed records remain normal diagnostics.
func shimRecord(line string) (bool, bool) {
	var record struct {
		Timestamp          string          `json:"timestamp"`
		Runtime            string          `json:"runtime"`
		Level              string          `json:"level"`
		Verbosity          string          `json:"verbosity"`
		Operation          string          `json:"operation"`
		Message            json.RawMessage `json:"message"`
		Context            json.RawMessage `json:"context"`
		PID                int             `json:"pid"`
		WorkspaceDirectory string          `json:"workspace_dir"`
		WorkspaceID        string          `json:"workspace_id"`
		AgentReplSessionID string          `json:"agent_repl_session_id"`
		ClaudeSessionID    string          `json:"claude_session_id"`
		RequestID          string          `json:"request_id"`
	}
	if err := json.Unmarshal([]byte(line), &record); err != nil {
		return false, false
	}
	parsedTimestamp, timestampErr := time.Parse(time.RFC3339Nano, record.Timestamp)
	workspaceSum := md5.Sum([]byte(record.WorkspaceDirectory))
	expectedWorkspaceID := hex.EncodeToString(workspaceSum[:])[:8]
	var message string
	var context map[string]json.RawMessage
	if timestampErr != nil || parsedTimestamp.IsZero() || !strings.Contains(record.Timestamp, ".") || record.Runtime != "shim" || (record.Level != "debug" && record.Level != "info" && record.Level != "warn" && record.Level != "error") || (record.Verbosity != "normal" && record.Verbosity != "verbose") || record.Operation == "" || json.Unmarshal(record.Message, &message) != nil || message == "" || json.Unmarshal(record.Context, &context) != nil || context == nil || record.PID <= 0 || record.WorkspaceDirectory == "" || record.WorkspaceID != expectedWorkspaceID || record.AgentReplSessionID == "" {
		return false, false
	}
	return record.Verbosity == "verbose", true
}

// Events returns the shim's decoded event stream. Closed when the shim's
// stdout closes.
func (p *Proc) Events() <-chan *protocol.L1Event {
	return p.events
}

// SendRaw writes one pre-encoded NDJSON command line to the shim's
// stdin. The line must already be newline-terminated.
func (p *Proc) SendRaw(line []byte) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if !p.stdinOK {
		return fmt.Errorf("shim: stdin already closed")
	}
	if _, err := p.stdin.Write(line); err != nil {
		return fmt.Errorf("shim: write command: %w", err)
	}
	return nil
}

// Send encodes cmd as one NDJSON line and writes it to the shim.
func (p *Proc) Send(cmd any) error {
	line, err := protocol.EncodeNDJSON(cmd)
	if err != nil {
		return err
	}
	return p.SendRaw(line)
}

// CloseStdin closes the shim's stdin, which the shim treats as an
// implicit shutdown. Idempotent.
func (p *Proc) CloseStdin() error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if !p.stdinOK {
		return nil
	}
	p.stdinOK = false
	if err := p.stdin.Close(); err != nil {
		return fmt.Errorf("shim: close stdin: %w", err)
	}
	return nil
}

// Kill forcibly terminates the shim subprocess. by attributes the stop and is
// required; see Stop.
//
// SIGNALLED BY PID, NEVER BY GROUP. os.Process.Kill delivers to this one pid.
// Since the shim now leads its OWN process group, a group-directed kill would
// reach whatever the shim itself spawned; that is a different, wider act and
// this is deliberately not it.
func (p *Proc) Kill(by Stop) error {
	return p.signal(by, "kill", p.cmd.Process.Kill)
}

// Terminate sends SIGTERM so the shim can stop cleanly (flush its transcript,
// close its listener) — the cooperative stop the daemon uses to hibernate a
// UDS shim. The caller reaps it via Wait separately. by attributes the stop and
// is required; see Stop.
//
// SIGNALLED BY PID, NEVER BY GROUP, for the reason given on Kill.
func (p *Proc) Terminate(by Stop) error {
	return p.signal(by, "terminate", func() error { return p.cmd.Process.Signal(syscall.SIGTERM) })
}

// signal commits lifecycle attribution only after its system call succeeds.
// Holding the pump lifecycle mutex through the call makes an early scanner
// completion structurally unable to observe a shutdown that never happened.
func (p *Proc) signal(by Stop, verb string, send func() error) error {
	if err := by.Validate(); err != nil {
		return fmt.Errorf("shim: refusing to %s pid %d: %w", verb, p.pid, err)
	}
	var err error
	if p.stderrPump != nil {
		err = p.stderrPump.signal(by, send)
	} else {
		err = send()
	}
	if err != nil {
		p.logger.Log("shim: shutdown signal failed verb=%s pid=%d pgid=%d initiator=%q reason=%q error=%v", verb, p.pid, p.pgid, by.Initiator, by.Reason, err)
		return fmt.Errorf("shim: %s pid %d (initiator=%s reason=%s): %w", verb, p.pid, by.Initiator, by.Reason, err)
	}
	p.logger.Log("shim: shutdown requested verb=%s pid=%d pgid=%d initiator=%q reason=%q", verb, p.pid, p.pgid, by.Initiator, by.Reason)
	return nil
}

// Wait makes os/exec the sole stderr reader closer, then waits for the scanner
// to classify that close before returning the child's exit status.
func (p *Proc) Wait() error {
	if p.stderrPump != nil {
		p.stderrPump.expectClose(stderrCloseOwnerProcWait)
		lifecycle := p.stderrPump.snapshot()
		p.logger.Log("shim: stderr lifecycle reaping pid=%d pgid=%d shutdown_initiator=%q shutdown_reason=%q close_owner=%q close_expected=%t", lifecycle.pid, lifecycle.pgid, lifecycle.shutdownInitiator, lifecycle.shutdownReason, lifecycle.closeOwner, lifecycle.closeExpected)
	}
	waitErr := p.cmd.Wait()
	closeOwner := stderrCloseOwnerCaller
	if p.stderrPump != nil {
		p.stderrPump.wait()
		lifecycle := p.stderrPump.snapshot()
		closeOwner = lifecycle.closeOwner
		p.logger.Log("shim: child reaped pid=%d pgid=%d exit=%q shutdown_initiator=%q shutdown_reason=%q close_owner=%q", lifecycle.pid, lifecycle.pgid, ExitDescription(waitErr), lifecycle.shutdownInitiator, lifecycle.shutdownReason, closeOwner)
	} else {
		p.logger.Log("shim: child reaped pid=%d pgid=%d exit=%q shutdown_initiator=%q shutdown_reason=%q close_owner=%q", p.pid, p.pgid, ExitDescription(waitErr), "child_exit", "no deliberate stop requested", closeOwner)
	}
	return waitErr
}

func mustJSONString(s string) []byte {
	b, err := protocol.EncodeNDJSON(s)
	if err != nil {
		panic(err) // marshaling a string cannot fail
	}
	return b[:len(b)-1] // drop the trailing newline
}
