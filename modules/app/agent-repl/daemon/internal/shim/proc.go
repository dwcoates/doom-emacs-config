// Package shim spawns and supervises one TS shim subprocess per session,
// speaking Layer 1 (stdio NDJSON) as declared in internal/protocol.
package shim

import (
	"bufio"
	"bytes"
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
	// stderrOutcomeComplete states that the stream reached EOF, so every byte
	// anything ever wrote to it is in the tail.
	stderrOutcomeComplete = "complete"
	// stderrOutcomeReadFailed states that the stream ended on an error rather
	// than EOF, so the tail stops wherever the read stopped.
	stderrOutcomeReadFailed = "read_failed"
	// stderrOutcomeHeldOpen states that the child was reaped and everything IT
	// wrote was drained, but something that inherited its stderr is still
	// holding the stream, so nothing written from here on is retained. It is
	// recorded because a tail that stops early must never read as a child that
	// fell silent.
	stderrOutcomeHeldOpen = "held_open"
)

// deadlineAlreadyPast releases a reader from an in-flight wait immediately. Its
// value only has to be in the past; the epoch is the least surprising one.
var deadlineAlreadyPast = time.Unix(0, 0)

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
	// sink is the writer the child's stderr is drained into: it owns the tail,
	// the line classification and the caller's tee.
	sink *stderrSink
	// stderrPump owns the read end of that stream and the one account of how it
	// ended.
	stderrPump *stderrPump
	// pid and pgid are captured at spawn so a stop record names the same
	// process the spawn record did, even after the process has exited.
	pid  int
	pgid int

	// shutdown carries the one authoritative account of why this child is
	// stopping, so the reap record can say who asked and why.
	shutdown shutdownAttribution

	mu      sync.Mutex
	stdinOK bool
}

// shutdownAttribution records who commanded a deliberate stop and why.
//
// The mutex is held ACROSS the signalling system call, which is what makes the
// attribution honest rather than merely likely: a stop the kernel refused can
// never be observed as one that happened, because a reader cannot see the
// fields until the sender has committed them.
type shutdownAttribution struct {
	mu        sync.Mutex
	initiator string
	reason    string
}

func newShutdownAttribution() shutdownAttribution {
	return shutdownAttribution{initiator: "child_exit", reason: "no deliberate stop requested"}
}

// commit records the attribution only after send succeeds.
func (a *shutdownAttribution) commit(by Stop, send func() error) error {
	a.mu.Lock()
	defer a.mu.Unlock()
	if err := send(); err != nil {
		return err
	}
	a.initiator = by.Initiator
	a.reason = by.Reason
	return nil
}

func (a *shutdownAttribution) snapshot() (initiator, reason string) {
	a.mu.Lock()
	defer a.mu.Unlock()
	return a.initiator, a.reason
}

// stderrSink is the child's stderr AS A WRITER, and the tail's guarantee rests
// on it: everything retention needs to do happens on the writing side, where
// the bytes arrive, rather than in a reader the reap can outrun.
type stderrSink struct {
	logger Logger
	tail   *stderrTail
	// directed is the caller's own stderr writer, which is TEED rather than
	// replaced. A caller that owns the stream (the shim persists its own log
	// through an inherited fd) still gets every byte, and the daemon still
	// retains the tail, so no path is blinder than the other.
	directed io.Writer
	// parse states whether completed lines are classified and logged by the
	// daemon. A caller that owns stderr owns its rendering too, so the daemon
	// only retains evidence on that path.
	parse bool

	mu      sync.Mutex
	pending []byte
	bytes   int
	lines   int
}

// Write retains the bytes and classifies every line they complete.
//
// It NEVER returns an error: a failed write would abandon the copy, and the
// remaining stderr of a child that is failing is exactly what must not be
// dropped. Every failure it absorbs is reported through the logger instead.
func (s *stderrSink) Write(p []byte) (int, error) {
	if s.directed != nil {
		if _, err := s.directed.Write(p); err != nil {
			s.logger.Log("shim: directed stderr write failed, the caller's stream is missing %d bytes: %v", len(p), err)
		}
	}
	if _, err := s.tail.Write(p); err != nil {
		s.logger.Log("shim: retaining stderr tail: %v", err)
	}
	s.mu.Lock()
	s.bytes += len(p)
	s.pending = append(s.pending, p...)
	lines := s.takeCompleteLinesLocked()
	s.mu.Unlock()
	for _, line := range lines {
		s.classify(line)
	}
	return len(p), nil
}

// takeCompleteLinesLocked splits every newline-terminated line out of pending.
//
// A line that outgrows maxEventLine without ever terminating is surrendered as
// its own line rather than buffered forever: an unbounded pending buffer would
// make a chatty child an unbounded daemon allocation, which is the same reason
// the tail is capped. Retention CONTINUES past it — the scanner this replaced
// abandoned the stream on an over-long token and lost everything said after.
func (s *stderrSink) takeCompleteLinesLocked() []string {
	var lines []string
	for {
		index := bytes.IndexByte(s.pending, '\n')
		if index < 0 {
			if len(s.pending) > maxEventLine {
				s.logger.Log("shim: stderr line exceeded %d bytes with no newline; the bytes so far are reported as one malformed record and retention continues", maxEventLine)
				lines = append(lines, string(s.pending))
				s.pending = nil
			}
			return lines
		}
		lines = append(lines, strings.TrimSuffix(string(s.pending[:index]), "\r"))
		s.pending = s.pending[index+1:]
		s.lines++
	}
}

// flush surrenders a trailing line the child never terminated.
//
// Its ONE caller is Wait, after the stream has ended, so there is no writer
// left to append to it: a child that died mid-line is the case this exists for,
// and half a line of a node stack trace is still the diagnosis.
func (s *stderrSink) flush() {
	s.mu.Lock()
	trailing := string(s.pending)
	s.pending = nil
	s.mu.Unlock()
	if trailing != "" {
		s.classify(strings.TrimSuffix(trailing, "\r"))
	}
}

// classify routes one completed stderr line to the channel its shape earns.
func (s *stderrSink) classify(line string) {
	if !s.parse || line == "" {
		return
	}
	verbose, valid := shimRecord(line)
	if !valid {
		s.logger.Log("shim stderr malformed: %s", line)
		return
	}
	if mirror, ok := s.logger.(stderrMirror); ok {
		mirror.MirrorShimRecord(line)
		return
	}
	if verbose {
		s.logger.LogVerbose("shim stderr: %s", line)
		return
	}
	s.logger.Log("shim stderr: %s", line)
}

// census reports what the sink retained, for the reap record.
func (s *stderrSink) census() (bytesWritten, lines int) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.bytes, s.lines
}

// stderrPump drains the child's stderr into the sink and publishes exactly one
// terminal account of how that stream ended.
//
// THE PIPE IS THIS PACKAGE'S OWN, not cmd.StderrPipe's. os/exec's Wait closes
// the pipes IT created the moment the child exits — os/exec documents that
// reading them and calling Wait cannot be concurrent — so the evidence a dying
// child left in the buffer was destroyed by the very reap that went looking for
// it whenever this goroutine had not been scheduled yet. A failed spawn's tail
// then came back EMPTY, which is the one case the tail exists to serve, and it
// happened only under load, which is when it is needed most. Owning the read
// end means nothing but this pump can end the stream.
type stderrPump struct {
	// reader is an io.ReadCloser rather than the *os.File it always is in
	// production so a test can arrange a stream that fails mid-read, which no
	// test can arrange against a real pipe.
	reader io.ReadCloser
	sink   *stderrSink
	logger Logger
	pid    int
	pgid   int
	done   chan struct{}

	mu      sync.Mutex
	copyErr error
}

func newStderrPump(reader io.ReadCloser, sink *stderrSink, logger Logger, pid, pgid int) *stderrPump {
	return &stderrPump{reader: reader, sink: sink, logger: logger, pid: pid, pgid: pgid, done: make(chan struct{})}
}

// run copies the stream into the sink until it ends, or until finish takes the
// stream over.
func (p *stderrPump) run() {
	defer close(p.done)
	_, err := io.Copy(p.sink, p.reader)
	p.mu.Lock()
	p.copyErr = err
	p.mu.Unlock()
}

// finish ends stderr retention for a child that has ALREADY BEEN REAPED, and
// returns the account of how the stream ended.
//
// It waits for nothing and loses nothing, which are usually opposed and are not
// here. Every byte the child itself wrote completed its write before the child
// exited, so by the time Wait has reaped it those bytes are already in the pipe
// and a read returns them without waiting. finish therefore takes the stream
// over from the pump and drains what is THERE — not what may yet arrive — so a
// descendant that inherited stderr and outlived its parent can neither cost the
// reap a stall nor cost the tail the child's last words. Both used to be on
// offer and neither is acceptable: os/exec's close raced the reader and lost
// the words, and draining to EOF instead would wedge the reap behind whatever
// is still holding the stream.
func (p *stderrPump) finish() string {
	file, isFile := p.reader.(*os.File)
	if isFile {
		// A deadline already past unblocks the pump's in-flight read at once,
		// and unblocking it CONSUMES NOTHING: the deadline is checked before
		// the read, so whatever is buffered is still buffered afterwards.
		if err := file.SetReadDeadline(deadlineAlreadyPast); err != nil {
			p.logger.Log("shim: releasing the stderr reader from its wait failed pid=%d pgid=%d: %v", p.pid, p.pgid, err)
		}
	}
	<-p.done
	outcome := p.account(file, isFile)
	if err := p.reader.Close(); err != nil {
		p.logger.Log("shim: closing the retained stderr stream failed pid=%d pgid=%d: %v", p.pid, p.pgid, err)
	}
	return outcome
}

// account classifies the ended stream, draining anything the pump was still
// waiting on when finish took over.
func (p *stderrPump) account(file *os.File, isFile bool) string {
	p.mu.Lock()
	copyErr := p.copyErr
	p.mu.Unlock()
	switch {
	case copyErr == nil:
		return stderrOutcomeComplete
	case !isFile || !errors.Is(copyErr, os.ErrDeadlineExceeded):
		p.logger.Log("shim: stderr read failed pid=%d pgid=%d: %v — the retained tail stops where the read stopped", p.pid, p.pgid, copyErr)
		return stderrOutcomeReadFailed
	}
	eof, err := p.drainBuffered(file)
	switch {
	case err != nil:
		p.logger.Log("shim: draining the reaped child's remaining stderr failed pid=%d pgid=%d: %v — the retained tail stops where the drain stopped", p.pid, p.pgid, err)
		return stderrOutcomeReadFailed
	case eof:
		return stderrOutcomeComplete
	default:
		return stderrOutcomeHeldOpen
	}
}

// drainBuffered reads everything the pipe HOLDS RIGHT NOW into the sink,
// reporting whether the stream reached EOF.
//
// The read is issued against the raw descriptor, which os.Pipe leaves in
// non-blocking mode, so "the buffer is empty" comes back as EAGAIN instead of a
// wait. That is what makes the drain exact rather than timed: it stops at the
// first read that would have to wait for a writer, and the only writer that
// could still be there is one the reaped child left behind.
func (p *stderrPump) drainBuffered(file *os.File) (bool, error) {
	// The deadline that released the pump would refuse these reads too.
	if err := file.SetReadDeadline(time.Time{}); err != nil {
		return false, fmt.Errorf("clearing the stderr read deadline: %w", err)
	}
	raw, err := file.SyscallConn()
	if err != nil {
		return false, fmt.Errorf("taking the stderr descriptor: %w", err)
	}
	buffer := make([]byte, 32*1024)
	for {
		var read int
		var readErr error
		if err := raw.Read(func(fd uintptr) bool {
			read, readErr = syscall.Read(int(fd), buffer)
			return true // never wait for readiness: this drain is what is THERE
		}); err != nil {
			return false, fmt.Errorf("reading the stderr descriptor: %w", err)
		}
		switch {
		case read > 0:
			if _, err := p.sink.Write(buffer[:read]); err != nil {
				return false, fmt.Errorf("retaining drained stderr: %w", err)
			}
		case readErr == nil:
			return true, nil // a zero-length read with no error is EOF
		case errors.Is(readErr, syscall.EINTR):
		case errors.Is(readErr, syscall.EAGAIN):
			return false, nil
		default:
			return false, fmt.Errorf("reading the stderr descriptor: %w", readErr)
		}
	}
}

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
	// LogLifecycle records the pump's OWN bring-up and teardown, which happen
	// on every healthy spawn. It is a third method rather than a reuse of Log
	// because Log is the shim's error channel — the daemon's implementation
	// stamps it level=error — and a scanner that started is not a failure. It
	// is not LogVerbose either: the lifecycle context is what a spawn
	// post-mortem reads first, so it must survive normal verbosity.
	LogLifecycle(format string, args ...any)
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
	// ONE STDERR PATH, whether or not the caller owns the stream. The two used
	// to be plumbed separately — a pipe the daemon scanned, or a MultiWriter
	// os/exec drove — and only one of them retained a tail the reap could not
	// truncate. Both now run through the same owned pipe and the same sink.
	stderrReader, stderrWriter, err := os.Pipe()
	if err != nil {
		return nil, fmt.Errorf("shim: stderr pipe: %w", err)
	}
	sink := &stderrSink{logger: logger, tail: tail, directed: opts.Stderr, parse: opts.Stderr == nil}
	cmd.Stderr = stderrWriter
	if err := cmd.Start(); err != nil {
		closeStderrPipe(logger, stderrReader, stderrWriter)
		return nil, fmt.Errorf("shim: start %q: %w", opts.Argv[0], err)
	}
	// The parent's copy of the WRITE end goes now that the child holds its own:
	// the stream reaches EOF when its last holder closes it, and a copy this
	// process kept would mean the pump never saw the child's exit at all.
	if err := stderrWriter.Close(); err != nil {
		logger.Log("shim: releasing the parent's stderr write end failed pid=%d: %v — the retained stderr stream will not reach EOF on its own", cmd.Process.Pid, err)
	}

	pid := cmd.Process.Pid
	pgid := reportProcessGroup(pid, logger, syscall.Getpgid)

	p := &Proc{
		cmd:        cmd,
		stdin:      stdin,
		events:     make(chan *protocol.L1Event, 64),
		logger:     logger,
		stderr:     tail,
		sink:       sink,
		stderrPump: newStderrPump(stderrReader, sink, logger, pid, pgid),
		pid:        pid,
		pgid:       pgid,
		shutdown:   newShutdownAttribution(),
		stdinOK:    true,
	}
	logger.LogLifecycle("shim: stderr retention started pid=%d pgid=%d parsed=%t directed=%t", pid, pgid, sink.parse, opts.Stderr != nil)

	go p.pumpStdout(stdout, logger)
	go p.stderrPump.run()
	return p, nil
}

// closeStderrPipe releases both ends of a stderr pipe whose child never
// started. Each failure is reported: a leaked descriptor is a daemon that runs
// out of them later, far from the spawn that lost them.
func closeStderrPipe(logger Logger, reader, writer *os.File) {
	if err := reader.Close(); err != nil {
		logger.Log("shim: closing the stderr read end of an unstarted child failed: %v", err)
	}
	if err := writer.Close(); err != nil {
		logger.Log("shim: closing the stderr write end of an unstarted child failed: %v", err)
	}
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

// signal commits shutdown attribution only after its system call succeeds.
// Holding the attribution mutex through the call makes a reap that lands first
// structurally unable to report a shutdown that never happened.
func (p *Proc) signal(by Stop, verb string, send func() error) error {
	if err := by.Validate(); err != nil {
		p.logger.Log("shim: shutdown attribution invalid verb=%s pid=%d pgid=%d initiator=%q reason=%q error=%v", verb, p.pid, p.pgid, by.Initiator, by.Reason, err)
		return fmt.Errorf("shim: refusing to %s pid %d: %w", verb, p.pid, err)
	}
	if err := p.shutdown.commit(by, send); err != nil {
		p.logger.Log("shim: shutdown signal failed verb=%s pid=%d pgid=%d initiator=%q reason=%q error=%v", verb, p.pid, p.pgid, by.Initiator, by.Reason, err)
		return fmt.Errorf("shim: %s pid %d (initiator=%s reason=%s): %w", verb, p.pid, by.Initiator, by.Reason, err)
	}
	p.logger.Log("shim: shutdown requested verb=%s pid=%d pgid=%d initiator=%q reason=%q", verb, p.pid, p.pgid, by.Initiator, by.Reason)
	return nil
}

// Wait reaps the child and returns its exit status, with the guarantee that
// every byte it wrote to stderr is in the tail by the time this returns.
//
// The guarantee is structural rather than probable. The stderr stream is this
// package's own pipe, so nothing else can close it; the child's exit closes the
// last write end it holds; and every byte it wrote completed its write before
// it exited, so those bytes are in the pipe by the time it is reaped. The
// daemon used to race its own reap for that evidence — os/exec closed the pipe
// it had created as soon as the child exited — and lost the race exactly when
// the machine was loaded enough to schedule the reader late, which is when a
// spawn failure most needs its stderr.
func (p *Proc) Wait() error {
	waitErr := p.cmd.Wait()
	outcome := stderrOutcomeComplete
	if p.stderrPump != nil {
		outcome = p.stderrPump.finish()
	}
	initiator, reason := p.shutdown.snapshot()
	if outcome == stderrOutcomeHeldOpen {
		p.logger.Log("shim: stderr stream STILL HELD at the reap pid=%d pgid=%d shutdown_initiator=%q shutdown_reason=%q — everything this child wrote is retained, but something that inherited its stderr outlived it, so nothing written from here on is", p.pid, p.pgid, initiator, reason)
	}
	// No writer is left once the stream has ended, so a line the child died in
	// the middle of is surrendered here rather than discarded with the sink.
	stderrBytes, stderrLines := 0, 0
	if p.sink != nil {
		p.sink.flush()
		stderrBytes, stderrLines = p.sink.census()
	}
	p.logger.Log("shim: child reaped pid=%d pgid=%d exit=%q shutdown_initiator=%q shutdown_reason=%q stderr_outcome=%s stderr_bytes=%d stderr_lines=%d", p.pid, p.pgid, ExitDescription(waitErr), initiator, reason, outcome, stderrBytes, stderrLines)
	return waitErr
}

func mustJSONString(s string) []byte {
	b, err := protocol.EncodeNDJSON(s)
	if err != nil {
		panic(err) // marshaling a string cannot fail
	}
	return b[:len(b)-1] // drop the trailing newline
}
