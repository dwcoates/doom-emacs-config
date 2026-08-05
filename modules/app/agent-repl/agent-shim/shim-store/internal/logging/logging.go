// Package logging owns shim-store's single structured logging API.
package logging

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
)

// Fields is diagnostic context bound to a store logger or supplied per record.
// Empty fields are omitted.  The store's runtime owners bind the stable values
// they know: database and table in db, socket and connection role in server.
type Fields struct {
	Component          string
	DatabasePath       string
	Table              string
	Socket             string
	AgentReplSessionID string
	Session            string
	Producer           string
	Subscriber         string
	Transaction        string
	ReplayFromSeq      uint64
	ReplayFirstSeq     uint64
	ReplayLastSeq      uint64
	Delivered          uint64
	TerminalOwner      string
	TerminalReason     string
	ErrorCause         string
	Operation          string
	Level              string
	RequestID          string
}

type record struct {
	Timestamp          string         `json:"timestamp"`
	Runtime            string         `json:"runtime"`
	PID                int            `json:"pid"`
	Level              string         `json:"level"`
	Verbosity          string         `json:"verbosity"`
	Operation          string         `json:"operation"`
	Message            string         `json:"message"`
	AgentReplSessionID string         `json:"agent_repl_session_id,omitempty"`
	ClaudeSessionID    string         `json:"claude_session_id,omitempty"`
	RequestID          string         `json:"request_id,omitempty"`
	Context            map[string]any `json:"context"`
}

// Logger writes normal records to the persistent log and stderr. Verbose
// records reach both sinks only when verbose mode is enabled.
type Logger struct {
	file           io.Writer
	stderr         io.Writer
	verboseEnabled bool
	fields         Fields
	state          *sinkState
	clock          func() time.Time
	pid            func() int
}

type sinkState struct {
	mu       sync.Mutex
	poisoned error
}

// New creates the shim-store logger. file is the durable sink and stderr is
// the interactive sink. Both are required runtime dependencies.
func New(file, stderr io.Writer, verboseEnabled bool) *Logger {
	if file == nil || stderr == nil {
		panic("shim-store logging: nil output sink")
	}
	return &Logger{
		file:           file,
		stderr:         stderr,
		verboseEnabled: verboseEnabled,
		state:          &sinkState{},
		clock:          time.Now,
		pid:            os.Getpid,
	}
}

// With returns a logger that adds fields to every record. Explicit fields on
// a later With call replace earlier values of the same name.
func (l *Logger) With(fields Fields) *Logger {
	if l == nil {
		panic("shim-store logging: nil logger")
	}
	copy := *l
	copy.fields = merge(l.fields, fields)
	return &copy
}

// Log records normal-priority diagnostic output to shim-store.log and stderr.
func (l *Logger) Log(fields Fields, format string, args ...any) {
	l.write("normal", fields, format, args, true)
}

// LogVerbose records verbose diagnostic output to both sinks only when
// AGENT_REPL_LOG_VERBOSE enabled verbose mode at startup.
func (l *Logger) LogVerbose(fields Fields, format string, args ...any) {
	l.write("verbose", fields, format, args, l.verboseEnabled)
}

func (l *Logger) write(verbosity string, fields Fields, format string, args []any, enabled bool) {
	if l == nil {
		panic("shim-store logging: nil logger")
	}
	merged := merge(l.fields, fields)
	if merged.Operation == "" {
		panic("shim-store logging: operation is required")
	}
	level := merged.Level
	if level == "" {
		level = "info"
	}
	switch level {
	case "debug", "info", "warn", "error":
	default:
		panic(fmt.Sprintf("shim-store logging: invalid level %q", level))
	}
	if !enabled {
		return
	}
	context := map[string]any{}
	for key, value := range map[string]string{
		"component":       merged.Component,
		"db":              merged.DatabasePath,
		"table":           merged.Table,
		"socket":          merged.Socket,
		"producer":        merged.Producer,
		"subscriber":      merged.Subscriber,
		"transaction":     merged.Transaction,
		"terminal_owner":  merged.TerminalOwner,
		"terminal_reason": merged.TerminalReason,
		"error":           merged.ErrorCause,
	} {
		if value != "" {
			context[key] = value
		}
	}
	terminal := merged.TerminalOwner != "" || merged.TerminalReason != ""
	for key, value := range map[string]uint64{
		"replay_from_seq":  merged.ReplayFromSeq,
		"replay_first_seq": merged.ReplayFirstSeq,
		"replay_last_seq":  merged.ReplayLastSeq,
		"delivered":        merged.Delivered,
	} {
		if value != 0 || terminal {
			context[key] = value
		}
	}
	now := sharedlogging.Timestamp(l.clock())
	entry := record{
		Timestamp:          now,
		Runtime:            "store",
		PID:                l.pid(),
		Level:              level,
		Verbosity:          verbosity,
		Operation:          merged.Operation,
		Message:            fmt.Sprintf(format, args...),
		AgentReplSessionID: merged.AgentReplSessionID,
		ClaudeSessionID:    merged.Session,
		RequestID:          merged.RequestID,
		Context:            context,
	}
	payload, err := json.Marshal(entry)
	if err != nil {
		panic(fmt.Sprintf("shim-store logging: encode record: %v", err))
	}
	line := string(payload) + "\n"
	l.state.mu.Lock()
	defer l.state.mu.Unlock()
	if l.state.poisoned != nil {
		panic(fmt.Sprintf("shim-store logging: sink is poisoned: %v", l.state.poisoned))
	}
	if err := writeFull(l.file, line); err != nil {
		l.state.poisoned = fmt.Errorf("persistent sink: %w", err)
		// The durable sink is the canonical record. Its failure can only be
		// reported through the terminal before the caller is stopped.
		emergencyPayload, encodeErr := json.Marshal(record{
			Timestamp: now,
			Runtime:   "store",
			PID:       entry.PID,
			Level:     "error",
			Verbosity: "normal",
			Operation: "store.logging.sink-failure",
			Message:   "persistent log sink write failed",
			Context:   map[string]any{"error": err.Error(), "target_operation": entry.Operation},
		})
		if encodeErr != nil {
			panic(fmt.Sprintf("shim-store logging: persistent sink failed: %v; encode emergency record: %v", err, encodeErr))
		}
		emergency := string(emergencyPayload) + "\n"
		if terminalErr := writeFull(l.stderr, emergency); terminalErr != nil {
			panic(fmt.Sprintf("shim-store logging: persistent sink failed: %v; emergency stderr also failed: %v", err, terminalErr))
		}
		panic(fmt.Sprintf("shim-store logging: persistent sink failed: %v", err))
	}
	if err := writeFull(l.stderr, line); err != nil {
		l.state.poisoned = fmt.Errorf("stderr sink: %w", err)
		panic(fmt.Sprintf("shim-store logging: stderr sink failed: %v", err))
	}
}

// writeFull completes ordinary partial writes. JSONL records are atomic at the
// logger boundary, so zero or invalid progress is a hard failure.
func writeFull(w io.Writer, value string) error {
	for len(value) > 0 {
		n, err := io.WriteString(w, value)
		if n < 0 || n > len(value) {
			return fmt.Errorf("invalid write count %d for %d bytes", n, len(value))
		}
		value = value[n:]
		if err != nil {
			return err
		}
		if n == 0 {
			return io.ErrShortWrite
		}
	}
	return nil
}

func merge(base, extra Fields) Fields {
	if extra.Component != "" {
		base.Component = extra.Component
	}
	if extra.DatabasePath != "" {
		base.DatabasePath = extra.DatabasePath
	}
	if extra.Table != "" {
		base.Table = extra.Table
	}
	if extra.Socket != "" {
		base.Socket = extra.Socket
	}
	if extra.AgentReplSessionID != "" {
		base.AgentReplSessionID = extra.AgentReplSessionID
	}
	if extra.Session != "" {
		base.Session = extra.Session
	}
	if extra.Producer != "" {
		base.Producer = extra.Producer
	}
	if extra.Subscriber != "" {
		base.Subscriber = extra.Subscriber
	}
	if extra.Transaction != "" {
		base.Transaction = extra.Transaction
	}
	if extra.ReplayFromSeq != 0 {
		base.ReplayFromSeq = extra.ReplayFromSeq
	}
	if extra.ReplayFirstSeq != 0 {
		base.ReplayFirstSeq = extra.ReplayFirstSeq
	}
	if extra.ReplayLastSeq != 0 {
		base.ReplayLastSeq = extra.ReplayLastSeq
	}
	if extra.Delivered != 0 {
		base.Delivered = extra.Delivered
	}
	if extra.TerminalOwner != "" {
		base.TerminalOwner = extra.TerminalOwner
	}
	if extra.TerminalReason != "" {
		base.TerminalReason = extra.TerminalReason
	}
	if extra.ErrorCause != "" {
		base.ErrorCause = extra.ErrorCause
	}
	if extra.Operation != "" {
		base.Operation = extra.Operation
	}
	if extra.Level != "" {
		base.Level = extra.Level
	}
	if extra.RequestID != "" {
		base.RequestID = extra.RequestID
	}
	return base
}
