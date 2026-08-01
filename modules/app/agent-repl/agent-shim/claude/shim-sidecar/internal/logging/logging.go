// Package logging owns the shim-claude-sidecar diagnostic contract.
package logging

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sync"
	"time"
)

// TimestampLayout is the log timestamp representation shared by every
// agent-repl runtime: RFC 3339 in the machine's local zone, on a 24-hour
// clock, with fixed-width microseconds and an explicit numeric offset.
// Fixed width keeps records from different runtimes lexically comparable.
const TimestampLayout = "2006-01-02T15:04:05.000000-07:00"

// Context is the structured runtime attribution attached to a log record.
// Empty fields are omitted so callers only provide context they actually own.
type Context struct {
	Component          string
	StoreSocket        string
	Path               string
	Session            string
	AgentReplSessionID string
	RequestID          string
	Task               string
	Producer           string
	Operation          string
	Level              string
	// SinkEmergency records a delivery-channel failure to stderr only. It is
	// deliberately not an alternate logging API: callers still use Log or
	// LogVerbose, but this record must not re-enter the failed durable sink.
	SinkEmergency bool
}

type record struct {
	Timestamp          string         `json:"timestamp"`
	Runtime            string         `json:"runtime"`
	PID                int            `json:"pid"`
	Level              string         `json:"level"`
	Verbosity          string         `json:"verbosity"`
	Operation          string         `json:"operation"`
	Message            string         `json:"message"`
	ClaudeSessionID    string         `json:"claude_session_id,omitempty"`
	AgentReplSessionID string         `json:"agent_repl_session_id,omitempty"`
	RequestID          string         `json:"request_id,omitempty"`
	Context            map[string]any `json:"context"`
}

// Diagnostic is the in-memory canonical form of a session-owned record.  The
// sidecar root owns delivery of these records to the file plane; the logging
// package must never make store calls itself, because logging can occur while a
// store write is already in progress.
type Diagnostic struct {
	Timestamp time.Time
	PID       int
	Level     string
	Verbosity string
	Operation string
	Message   string
	Session   string
	RequestID string
	Path      string
	Context   map[string]any
}

// Logger routes session records to the diagnostic outbox and conceptually
// global records to the persistent sidecar log. Normal records are also
// written to stderr. Verbose records are emitted only when
// AGENT_REPL_LOG_VERBOSE is enabled.
type Logger struct {
	stderr  io.Writer
	file    io.Writer
	mu      sync.Mutex
	now     func() time.Time
	pid     func() int
	verbose func() bool
	// diagnostic receives session-owned records. It is installed by the sidecar
	// orchestration layer and runs synchronously only to enqueue an
	// in-memory record, never to perform I/O.
	diagnostic func(Diagnostic)
	poisoned   error
}

// Bound is the runtime logger passed through sidecar packages.
type Bound struct {
	logger  *Logger
	context Context
}

// New constructs the sidecar's canonical logger. The caller must provide both
// sinks so normal logging cannot silently lose either delivery target.
func New(stderr, file io.Writer) *Logger {
	if stderr == nil || file == nil {
		panic("sidecar logging requires stderr and persistent file sinks")
	}
	return &Logger{
		stderr:  stderr,
		file:    file,
		now:     time.Now,
		pid:     os.Getpid,
		verbose: func() bool { return os.Getenv("AGENT_REPL_LOG_VERBOSE") != "" },
	}
}

// With creates a logger with stable runtime attribution.
func (l *Logger) With(ctx Context) *Bound {
	if l == nil {
		panic("sidecar logging: With called on nil Logger")
	}
	return &Bound{logger: l, context: ctx}
}

// With extends the bound attribution. Non-empty fields replace earlier values.
func (b *Bound) With(ctx Context) *Bound {
	if b == nil {
		panic("sidecar logging: With called on nil Bound logger")
	}
	return &Bound{logger: b.logger, context: mergeContext(b.context, ctx)}
}

// SetDiagnosticSink installs the sidecar-owned in-memory delivery boundary for
// session diagnostics.  It is intentionally available only through Bound so
// callers retain the normal/verbose two-function emission API.
func (b *Bound) SetDiagnosticSink(sink func(Diagnostic)) {
	if b == nil || b.logger == nil {
		panic("sidecar logging: SetDiagnosticSink called on nil Bound logger")
	}
	b.logger.mu.Lock()
	defer b.logger.mu.Unlock()
	if sink == nil {
		panic("sidecar logging: diagnostic sink is required")
	}
	b.logger.diagnostic = sink
}

// Log records a normal diagnostic to the persistent log and stderr.
func (b *Bound) Log(format string, args ...any) {
	if b == nil {
		panic("sidecar logging: Log called on nil Bound logger")
	}
	b.logger.write(false, b.context, format, args...)
}

// LogVerbose records a verbose diagnostic only when AGENT_REPL_LOG_VERBOSE is
// enabled. Disabled verbose records reach neither the durable sink nor stderr.
func (b *Bound) LogVerbose(format string, args ...any) {
	if b == nil {
		panic("sidecar logging: LogVerbose called on nil Bound logger")
	}
	b.logger.write(true, b.context, format, args...)
}

func (l *Logger) write(verbose bool, ctx Context, format string, args ...any) {
	if l == nil {
		panic("sidecar logging: write called on nil Logger")
	}
	if ctx.Operation == "" {
		panic("sidecar logging: operation is required")
	}
	level := ctx.Level
	if level == "" {
		level = "info"
	}
	switch level {
	case "debug", "info", "warn", "error":
	default:
		panic(fmt.Sprintf("sidecar logging: invalid level %q", level))
	}
	if verbose && !l.verbose() {
		return
	}
	verbosity := "normal"
	if verbose {
		verbosity = "verbose"
	}
	context := map[string]any{}
	for key, value := range map[string]string{
		"component":    ctx.Component,
		"store_socket": ctx.StoreSocket,
		"path":         ctx.Path,
		"task":         ctx.Task,
		"producer":     ctx.Producer,
	} {
		if value != "" {
			context[key] = value
		}
	}
	now := l.now().Local()
	diagnostic := Diagnostic{
		Timestamp: now,
		PID:       l.pid(),
		Level:     level,
		Verbosity: verbosity,
		Operation: ctx.Operation,
		Message:   fmt.Sprintf(format, args...),
		Session:   ctx.Session,
		RequestID: ctx.RequestID,
		Path:      ctx.Path,
		Context:   context,
	}
	payload, err := json.Marshal(record{
		Timestamp:          now.Format(TimestampLayout),
		Runtime:            "sidecar",
		PID:                diagnostic.PID,
		Level:              level,
		Verbosity:          verbosity,
		Operation:          ctx.Operation,
		Message:            diagnostic.Message,
		ClaudeSessionID:    ctx.Session,
		AgentReplSessionID: ctx.AgentReplSessionID,
		RequestID:          ctx.RequestID,
		Context:            context,
	})
	if err != nil {
		panic(fmt.Sprintf("sidecar logging: encode record: %v", err))
	}
	line := string(payload) + "\n"
	l.mu.Lock()
	defer l.mu.Unlock()
	if diagnostic.Session != "" && !ctx.SinkEmergency {
		if l.diagnostic == nil {
			panic("sidecar logging: session diagnostic sink is not installed")
		}
		l.diagnostic(diagnostic)
	} else if !ctx.SinkEmergency {
		if l.poisoned != nil {
			panic(fmt.Sprintf("sidecar logging: persistent sink previously failed: %v", l.poisoned))
		}
		if err := writeAll(l.file, []byte(line)); err != nil {
			l.poisoned = err
			// The persistent sink is the canonical record. Its failure can only be
			// reported through the terminal before the caller is stopped.
			emergency, encodeErr := json.Marshal(record{
				Timestamp: now.Format(TimestampLayout),
				Runtime:   "sidecar",
				PID:       diagnostic.PID,
				Level:     "error",
				Verbosity: "normal",
				Operation: "sidecar.logging.sink-failure",
				Message:   "persistent log sink write failed",
				Context:   map[string]any{"error": err.Error(), "target_operation": ctx.Operation},
			})
			if encodeErr != nil {
				panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v; encode emergency record: %v", err, encodeErr))
			}
			if terminalErr := writeAll(l.stderr, append(emergency, '\n')); terminalErr != nil {
				panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v; emergency stderr also failed: %v", err, terminalErr))
			}
			panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v", err))
		}
	}
	if err := writeAll(l.stderr, []byte(line)); err != nil {
		panic(fmt.Sprintf("sidecar logging: stderr sink failed: %v", err))
	}
}

// writeAll completes ordinary partial writes and rejects only zero or invalid
// progress. JSONL is line-oriented, so a truncated record is never accepted.
func writeAll(w io.Writer, data []byte) error {
	for len(data) > 0 {
		n, err := w.Write(data)
		if n < 0 || n > len(data) {
			return fmt.Errorf("invalid write count %d for %d bytes", n, len(data))
		}
		data = data[n:]
		if err != nil {
			return err
		}
		if n == 0 {
			return io.ErrShortWrite
		}
	}
	return nil
}

func mergeContext(base, add Context) Context {
	if add.Component != "" {
		base.Component = add.Component
	}
	if add.StoreSocket != "" {
		base.StoreSocket = add.StoreSocket
	}
	if add.Path != "" {
		base.Path = add.Path
	}
	if add.Session != "" {
		base.Session = add.Session
	}
	if add.AgentReplSessionID != "" {
		base.AgentReplSessionID = add.AgentReplSessionID
	}
	if add.RequestID != "" {
		base.RequestID = add.RequestID
	}
	if add.Task != "" {
		base.Task = add.Task
	}
	if add.Producer != "" {
		base.Producer = add.Producer
	}
	if add.Operation != "" {
		base.Operation = add.Operation
	}
	if add.Level != "" {
		base.Level = add.Level
	}
	if add.SinkEmergency {
		base.SinkEmergency = true
	}
	return base
}
