// Package shim spawns and supervises one TS shim subprocess per session,
// speaking Layer 1 (stdio NDJSON) as declared in internal/protocol.
package shim

import (
	"bufio"
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
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

// Proc is a running shim subprocess.
//
// Events() yields decoded Layer-1 events until the shim's stdout closes,
// after which the channel is closed; the supervisor keys lifecycle off
// the `closed` event plus Wait()'s exit code.
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
	var stderr io.ReadCloser
	if opts.Stderr != nil {
		cmd.Stderr = opts.Stderr
	} else {
		stderr, err = cmd.StderrPipe()
		if err != nil {
			return nil, fmt.Errorf("shim: stderr pipe: %w", err)
		}
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

	go p.pumpStdout(stdout, logger)
	if stderr != nil {
		go pumpStderr(stderr, logger)
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

func pumpStderr(stderr io.Reader, logger Logger) {
	scanner := bufio.NewScanner(stderr)
	scanner.Buffer(make([]byte, 64*1024), maxEventLine)
	for scanner.Scan() {
		line := scanner.Text()
		verbose, valid := shimRecord(line)
		if valid {
			if mirror, ok := logger.(stderrMirror); ok {
				mirror.MirrorShimRecord(line)
			} else if verbose {
				logger.LogVerbose("shim stderr: %s", line)
			} else {
				logger.Log("shim stderr: %s", line)
			}
			continue
		}
		logger.Log("shim stderr malformed: %s", line)
	}
	if err := scanner.Err(); err != nil {
		logger.Log("shim: stderr scan error: %v", err)
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

// Kill forcibly terminates the shim subprocess.
func (p *Proc) Kill() error {
	if err := p.cmd.Process.Kill(); err != nil {
		return fmt.Errorf("shim: kill: %w", err)
	}
	return nil
}

// Terminate sends SIGTERM so the shim can stop cleanly (flush its transcript,
// close its listener) — the cooperative stop the daemon uses to hibernate a
// UDS shim. The caller reaps it via Wait separately.
func (p *Proc) Terminate() error {
	if err := p.cmd.Process.Signal(syscall.SIGTERM); err != nil {
		return fmt.Errorf("shim: terminate: %w", err)
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
