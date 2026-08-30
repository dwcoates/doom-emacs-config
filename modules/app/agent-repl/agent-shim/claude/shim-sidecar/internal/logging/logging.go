// Package logging owns the shim-claude-sidecar diagnostic contract: one JSON
// record per logged branch, on two emission levels (normal and verbose).
//
// THE CORRELATION VOCABULARY IS THE CONTRACT. Every identifier a record names
// lives in a DEDICATED context key, never only in the message text, so the
// integration loop can join sidecar records against the store's and the shim's
// by the same names. The keys that spelled the retired (session_id, seq)
// addressing — `claude_session_id`, `seq`, `from_seq`, `replay_*_seq` — are
// GONE from this package: the spine is agent-keyed now, and a record that still
// named a session ordinal would be correlating against an address that no
// longer exists.
//
// SELF-DIAGNOSTICS ARE LOGS AND NOTHING ELSE (R10). The sidecar's own
// diagnostics used to be enqueued through this package and written to the store
// as records; store.v1 has no home for a fact about the READER rather than the
// read, so the sink is deleted rather than aimed at an approximate arm.
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

// Context is the structured attribution attached to a log record. Every field
// is optional presence: an empty string (or a nil pointer, for the numeric
// keys) means the caller does not own that fact, and the key is omitted from
// the record rather than emitted as a sentinel.
type Context struct {
	// --- record-level attribution -------------------------------------------

	// Component is the sidecar subsystem writing the record.
	Component string
	// Operation names the branch. Required: a record with no operation cannot
	// be correlated and is refused.
	Operation string
	// Level is one of debug/info/warn/error; empty means info.
	Level string
	// RequestID is the caller-supplied X-Agent-Repl-Request-Id, when one exists.
	RequestID string
	// SinkEmergency records a delivery-channel failure to stderr only. It is
	// deliberately not an alternate logging API: callers still use Log or
	// LogVerbose, but this record must not re-enter the failed durable sink.
	SinkEmergency bool

	// --- the correlation vocabulary (BRIEF-COMMON) --------------------------

	// Producer is the WriteBatch producer string.
	Producer string
	// AgentID is our AgentId.value.
	AgentID string
	// VendorSessionID is the vendor's own session uuid — an ATTRIBUTE of an
	// agent, never an address.
	VendorSessionID string
	// BookAgentID is the page book a line names.
	BookAgentID string
	// WriteID is a StoreEntry.write_id.
	WriteID string
	// UpsertKey is a StoreEntry.upsert_key.
	UpsertKey string
	// Position is an opaque store-minted page position.
	Position string
	// WriteSeq is the store-internal write ordinal.
	WriteSeq *uint64
	// WatchTokenHash is a sha256 prefix of a watch token, never the token.
	WatchTokenHash string
	// RPC is the Connect procedure name.
	RPC string
	// FileID is a tailed file's stable "dev:inode" identity.
	FileID string
	// Path is an absolute file path.
	Path string
	// Offset is a byte offset within a tailed file.
	Offset *int64
	// TaskID is the vendor's detached-task id (the spool basename).
	TaskID string
	// ActivityID is an AgentActivityId.value.
	ActivityID string
	// TurnID is a TurnId.value.
	TurnID string
	// StoreSocket is the store's UDS path.
	StoreSocket string
	// Attempt is the ordinal of a recovery attempt against an unreachable
	// dependency, so an outage's progress is filterable rather than buried in
	// the message text.
	Attempt *int
	// BackoffMs is the delay armed before the NEXT attempt, in milliseconds.
	BackoffMs *int64
	// RefusalKind is a store failure's oneof arm (storage_failure /
	// invalid_request). It is what says whether a retry can help, so it rides a
	// dedicated key rather than the message text a reader would have to parse.
	RefusalKind string
	// Field is the offending field an invalid_request refusal names, so the
	// producer defect can be found without reading prose.
	Field string
	// WriteIDs lists the write_ids of a whole refused batch. A batch is refused
	// WHOLE, so naming one of its records would misreport what was rejected.
	WriteIDs []string
}

// Off boxes a byte offset for Context.Offset, so an unset offset is genuinely
// absent rather than a zero that reads as "the start of the file".
func Off(v int64) *int64 { return &v }

// Attempt boxes a recovery attempt ordinal for Context.Attempt.
func Attempt(v int) *int { return &v }

// BackoffMs boxes an armed retry delay for Context.BackoffMs.
func BackoffMs(d time.Duration) *int64 {
	ms := d.Milliseconds()
	return &ms
}

// Seq boxes a store write ordinal for Context.WriteSeq.
func Seq(v uint64) *uint64 { return &v }

type record struct {
	Timestamp string         `json:"timestamp"`
	Runtime   string         `json:"runtime"`
	PID       int            `json:"pid"`
	Level     string         `json:"level"`
	Verbosity string         `json:"verbosity"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	RequestID string         `json:"request_id,omitempty"`
	Context   map[string]any `json:"context"`
}

// Logger writes the sidecar's records to its persistent log and to stderr.
// Verbose records are emitted only when AGENT_REPL_LOG_VERBOSE is set.
type Logger struct {
	stderr   io.Writer
	file     io.Writer
	mu       sync.Mutex
	now      func() time.Time
	pid      func() int
	verbose  func() bool
	poisoned error
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

// With extends the bound attribution. Set fields replace earlier values.
func (b *Bound) With(ctx Context) *Bound {
	if b == nil {
		panic("sidecar logging: With called on nil Bound logger")
	}
	return &Bound{logger: b.logger, context: mergeContext(b.context, ctx)}
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

// contextMap renders the correlation vocabulary, omitting every key the caller
// did not set.
func contextMap(ctx Context) map[string]any {
	out := map[string]any{}
	for key, value := range map[string]string{
		"component":         ctx.Component,
		"store_socket":      ctx.StoreSocket,
		"producer":          ctx.Producer,
		"agent_id":          ctx.AgentID,
		"vendor_session_id": ctx.VendorSessionID,
		"book_agent_id":     ctx.BookAgentID,
		"write_id":          ctx.WriteID,
		"upsert_key":        ctx.UpsertKey,
		"position":          ctx.Position,
		"watch_token_hash":  ctx.WatchTokenHash,
		"rpc":               ctx.RPC,
		"file_id":           ctx.FileID,
		"path":              ctx.Path,
		"task_id":           ctx.TaskID,
		"activity_id":       ctx.ActivityID,
		"turn_id":           ctx.TurnID,
		"refusal_kind":      ctx.RefusalKind,
		"field":             ctx.Field,
	} {
		if value != "" {
			out[key] = value
		}
	}
	if ctx.Offset != nil {
		out["offset"] = *ctx.Offset
	}
	if ctx.WriteSeq != nil {
		out["write_seq"] = *ctx.WriteSeq
	}
	if ctx.Attempt != nil {
		out["attempt"] = *ctx.Attempt
	}
	if ctx.BackoffMs != nil {
		out["backoff_ms"] = *ctx.BackoffMs
	}
	if len(ctx.WriteIDs) > 0 {
		out["write_ids"] = append([]string(nil), ctx.WriteIDs...)
	}
	return out
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
	now := l.now().Local()
	payload, err := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(now),
		Runtime:   "sidecar",
		PID:       l.pid(),
		Level:     level,
		Verbosity: verbosity,
		Operation: ctx.Operation,
		Message:   fmt.Sprintf(format, args...),
		RequestID: ctx.RequestID,
		Context:   contextMap(ctx),
	})
	if err != nil {
		panic(fmt.Sprintf("sidecar logging: encode record: %v", err))
	}
	line := append(payload, '\n')
	l.mu.Lock()
	defer l.mu.Unlock()
	if !ctx.SinkEmergency {
		if l.poisoned != nil {
			panic(fmt.Sprintf("sidecar logging: persistent sink previously failed: %v", l.poisoned))
		}
		if err := writeAll(l.file, line); err != nil {
			l.poisoned = err
			l.reportSinkFailure(now, ctx.Operation, err)
			panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v", err))
		}
	}
	if err := writeAll(l.stderr, line); err != nil {
		panic(fmt.Sprintf("sidecar logging: stderr sink failed: %v", err))
	}
}

// reportSinkFailure narrates the loss of the canonical sink through the only
// channel left, the terminal. Caller holds mu.
func (l *Logger) reportSinkFailure(now time.Time, operation string, cause error) {
	emergency, encodeErr := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(now),
		Runtime:   "sidecar",
		PID:       l.pid(),
		Level:     "error",
		Verbosity: "normal",
		Operation: "sidecar.logging.sink-failure",
		Message:   "persistent log sink write failed",
		Context:   map[string]any{"error": cause.Error(), "target_operation": operation},
	})
	if encodeErr != nil {
		panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v; encode emergency record: %v", cause, encodeErr))
	}
	if terminalErr := writeAll(l.stderr, append(emergency, '\n')); terminalErr != nil {
		panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v; emergency stderr also failed: %v", cause, terminalErr))
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
	for _, field := range []struct{ dst, src *string }{
		{&base.Component, &add.Component},
		{&base.Operation, &add.Operation},
		{&base.Level, &add.Level},
		{&base.RequestID, &add.RequestID},
		{&base.Producer, &add.Producer},
		{&base.AgentID, &add.AgentID},
		{&base.VendorSessionID, &add.VendorSessionID},
		{&base.BookAgentID, &add.BookAgentID},
		{&base.WriteID, &add.WriteID},
		{&base.UpsertKey, &add.UpsertKey},
		{&base.Position, &add.Position},
		{&base.WatchTokenHash, &add.WatchTokenHash},
		{&base.RPC, &add.RPC},
		{&base.FileID, &add.FileID},
		{&base.Path, &add.Path},
		{&base.TaskID, &add.TaskID},
		{&base.ActivityID, &add.ActivityID},
		{&base.TurnID, &add.TurnID},
		{&base.StoreSocket, &add.StoreSocket},
		{&base.RefusalKind, &add.RefusalKind},
		{&base.Field, &add.Field},
	} {
		if *field.src != "" {
			*field.dst = *field.src
		}
	}
	if add.Offset != nil {
		base.Offset = add.Offset
	}
	if add.WriteSeq != nil {
		base.WriteSeq = add.WriteSeq
	}
	if len(add.WriteIDs) > 0 {
		base.WriteIDs = add.WriteIDs
	}
	if add.Attempt != nil {
		base.Attempt = add.Attempt
	}
	if add.BackoffMs != nil {
		base.BackoffMs = add.BackoffMs
	}
	if add.SinkEmergency {
		base.SinkEmergency = true
	}
	return base
}
