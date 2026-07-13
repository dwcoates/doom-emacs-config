// Package shim spawns and supervises one TS shim subprocess per session,
// speaking Layer 1 (stdio NDJSON) as specified in shared/protocol.md §1.
package shim

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"sync"

	"claude-repld/internal/protocol"
)

// maxEventLine bounds one shim stdout line (large tool results).
const maxEventLine = 32 * 1024 * 1024

// Proc is a running shim subprocess.
//
// Events() yields decoded Layer-1 events until the shim's stdout closes,
// after which the channel is closed; the supervisor keys lifecycle off
// the `closed` event plus Wait()'s exit code, per protocol.md §1.2.
type Proc struct {
	cmd    *exec.Cmd
	stdin  io.WriteCloser
	events chan *protocol.L1Event

	mu      sync.Mutex
	stdinOK bool
}

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
	// Logf receives shim stderr lines and protocol decode complaints.
	// Defaults to log.Printf.
	Logf func(format string, args ...any)
}

// Spawn starts the shim subprocess and its stdout/stderr pumps.
func Spawn(opts Options) (*Proc, error) {
	if len(opts.Argv) == 0 {
		return nil, fmt.Errorf("shim: empty argv")
	}
	logf := opts.Logf
	if logf == nil {
		logf = log.Printf
	}
	cmd := exec.Command(opts.Argv[0], opts.Argv[1:]...)
	cmd.Dir = opts.Dir
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
	stderr, err := cmd.StderrPipe()
	if err != nil {
		return nil, fmt.Errorf("shim: stderr pipe: %w", err)
	}
	if err := cmd.Start(); err != nil {
		return nil, fmt.Errorf("shim: start %q: %w", opts.Argv[0], err)
	}

	p := &Proc{
		cmd:     cmd,
		stdin:   stdin,
		events:  make(chan *protocol.L1Event, 64),
		stdinOK: true,
	}

	go p.pumpStdout(stdout, logf)
	go pumpStderr(stderr, logf)
	return p, nil
}

func (p *Proc) pumpStdout(stdout io.Reader, logf func(string, ...any)) {
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
			logf("shim: undecodable event line: %v", err)
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
		logf("shim: stdout scan error: %v", err)
	}
}

func pumpStderr(stderr io.Reader, logf func(string, ...any)) {
	scanner := bufio.NewScanner(stderr)
	scanner.Buffer(make([]byte, 64*1024), maxEventLine)
	for scanner.Scan() {
		logf("shim stderr: %s", scanner.Text())
	}
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

// Kill forcibly terminates the shim subprocess.
func (p *Proc) Kill() error {
	if err := p.cmd.Process.Kill(); err != nil {
		return fmt.Errorf("shim: kill: %w", err)
	}
	return nil
}

// Wait reaps the subprocess and returns its exit error, if any.
func (p *Proc) Wait() error {
	return p.cmd.Wait()
}

func mustJSONString(s string) []byte {
	b, err := protocol.EncodeNDJSON(s)
	if err != nil {
		panic(err) // marshaling a string cannot fail
	}
	return b[:len(b)-1] // drop the trailing newline
}
