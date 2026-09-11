// Package logging owns the shim-claude-sidecar diagnostic contract: one JSON
// record per logged branch, filtered by the process-wide severity threshold.
//
// THE CORRELATION VOCABULARY IS THE CONTRACT. Every identifier a record names
// lives in a DEDICATED context key, never only in the message text, so the
// integration loop can join sidecar records against the store's and the shim's
// by the same names. The keys that spelled the retired sequence addressing —
// `seq`, `from_seq`, `replay_*_seq` — are gone. `claude_session_id` remains as
// promoted vendor attribution, never as a store address.
//
// GENUINELY GLOBAL SELF-DIAGNOSTICS remain in the process's rotating sink.
// File-scoped diagnostics are forwarded to the daemon, which owns each
// workspace's sidecar.log. A forwarding failure is stated once per daemon
// address and outage window in the global sink and does not fail the file-plane
// operation that produced the diagnostic.
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
	// WorkspaceDir and WorkspaceID identify the workspace whose file produced
	// this record. They are promoted top-level fields, never buried in context.
	WorkspaceDir string
	WorkspaceID  string
	// ClaudeSessionID is the transcript file's vendor session identity.
	ClaudeSessionID string

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
	// RefusalSite is WHERE the refusal was received, in the store's own
	// `refusal_site` vocabulary. It answers a different question from
	// RefusalKind and the two ride together on every refusal record: the KIND
	// says whether a retry can help, the SITE says which call was refused, so a
	// reader joining the sidecar's record against the store's own refusal
	// matches on the site and reads the verdict off the kind.
	RefusalSite string
	// Reason is a conclusion's own vocabulary: a LOST arm (file_vanished /
	// went_silent / swept_up) — the same word the wire's DetachedLost arm
	// carries, so a terminal and the sweep that concluded it join on it — or a
	// held transcript's hold reason, which is what says whether a repeated
	// hold record is the SAME condition or a new one.
	Reason string
	// Field is the offending field an invalid_request refusal names, so the
	// producer defect can be found without reading prose.
	Field string
	// WriteIDs lists the write_ids of a whole refused batch. A batch is refused
	// WHOLE, so naming one of its records would misreport what was rejected.
	WriteIDs []string
	// Repeat is how many times the SAME condition has now been observed for
	// the same subject, when a record stands in for more occurrences than the
	// one that produced it. A repeating condition is restated logarithmically
	// rather than per occurrence, so a defect that never stops being true
	// stays visible without being the log's entire content — and the reader
	// can tell "seen once" from "seen ten thousand times" without counting
	// records that were deliberately not written.
	Repeat *int
}

// Repeat boxes an occurrence count for Context.Repeat.
func Repeat(v int) *int { return &v }

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
	Timestamp       string         `json:"timestamp"`
	Runtime         string         `json:"runtime"`
	PID             int            `json:"pid"`
	Level           string         `json:"level"`
	Verbosity       string         `json:"verbosity"`
	Operation       string         `json:"operation"`
	Message         string         `json:"message"`
	WorkspaceDir    string         `json:"workspace_dir,omitempty"`
	WorkspaceID     string         `json:"workspace_id,omitempty"`
	ClaudeSessionID string         `json:"claude_session_id,omitempty"`
	RequestID       string         `json:"request_id,omitempty"`
	Context         map[string]any `json:"context"`
}

// ForwardRecord is one file-scoped diagnostic handed to the daemon boundary.
// Workspace identity stays explicit so the forwarding implementation cannot
// accidentally bury the daemon's address in arbitrary context.
type ForwardRecord struct {
	Timestamp       string
	PID             int
	Level           string
	Verbose         bool
	Operation       string
	Message         string
	WorkspaceDir    string
	WorkspaceID     string
	ClaudeSessionID string
	Context         map[string]any
}

// Forwarder is the daemon integration boundary. It answers the exact daemon
// address used even when the RPC fails, so Logger can rate-limit the global
// failure record per destination without knowing how daemon.addr is resolved.
type Forwarder interface {
	Forward(ForwardRecord) (daemonAddress string, err error)
}

// Logger writes records at or above one process-wide severity threshold.
type Logger struct {
	stderr io.Writer
	file   io.Writer
	// terminalEmergencyOnly withholds the ORDINARY record stream from the
	// terminal sink, leaving it the one thing it is the last channel for: a
	// SinkEmergency record, which must not re-enter the failed durable sink.
	//
	// WHY IT EXISTS. Under launchd the terminal is a plain append-only file
	// the service does not own and therefore cannot cap or roll, so mirroring
	// every record there is an unbounded second copy of a log that is already
	// durable and rotated. On the owner's machine that copy reached 6.2 GB
	// against a 666 MB `--log`. Nothing is lost by withholding it: the
	// durable sink carries the identical bytes, and a failure of THAT sink is
	// exactly the case this flag still lets through.
	terminalEmergencyOnly bool
	mu                    sync.Mutex
	now                   func() time.Time
	pid                   func() int
	minimumLevel          sharedlogging.Level
	poisoned              error
	files                 map[string]Context
	forwarder             Forwarder
	lastForwardFailure    string
	forwardMu             sync.Mutex
	forwardReady          *sync.Cond
	forwardQueue          []ForwardRecord
	forwardClosing        bool
	forwardDone           chan struct{}
}

// Bound is the runtime logger passed through sidecar packages.
type Bound struct {
	logger  *Logger
	context Context
}

// New constructs a debug-enabled logger for focused tests and foreground
// harnesses. Production passes its parsed threshold through NewAtLevel.
func New(stderr, file io.Writer) *Logger {
	return NewAtLevel(stderr, file, sharedlogging.LevelDebug)
}

// NewAtLevel constructs the canonical logger at one explicit threshold.
func NewAtLevel(stderr, file io.Writer, minimumLevel sharedlogging.Level) *Logger {
	if stderr == nil || file == nil {
		panic("sidecar logging requires stderr and persistent file sinks")
	}
	return &Logger{
		stderr:       stderr,
		file:         file,
		now:          time.Now,
		pid:          os.Getpid,
		minimumLevel: minimumLevel,
		files:        map[string]Context{},
	}
}

// NewDurableOnly constructs a logger that writes the record stream to the
// durable sink ALONE, keeping the terminal for the sink-emergency record it is
// the last channel for.
//
// This is what PRODUCTION uses. `New`'s two-sink mirroring is right when both
// sinks are the caller's to manage — a test holding two buffers, a foreground
// run whose terminal is a person — and wrong under launchd, where the terminal
// is an unbounded file nobody rolls.
func NewDurableOnly(terminal, file io.Writer) *Logger {
	return NewDurableOnlyAtLevel(terminal, file, sharedlogging.LevelInfo)
}

// NewDurableOnlyAtLevel constructs the production logger at one explicit
// severity threshold.
func NewDurableOnlyAtLevel(terminal, file io.Writer, minimumLevel sharedlogging.Level) *Logger {
	l := NewAtLevel(terminal, file, minimumLevel)
	l.terminalEmergencyOnly = true
	return l
}

// NewForwardingDurableOnlyAtLevel constructs the production logger: global
// records go to file, while file-scoped records enter an ordered forwarding
// queue for the daemon-owned workspace sink. A nil forwarder is an invariant
// violation, never permission to put a workspace record in the global sink.
func NewForwardingDurableOnlyAtLevel(terminal, file io.Writer, minimumLevel sharedlogging.Level, forwarder Forwarder) *Logger {
	if forwarder == nil {
		panic("sidecar logging: forwarding logger requires a daemon forwarder")
	}
	l := NewDurableOnlyAtLevel(terminal, file, minimumLevel)
	l.forwarder = forwarder
	l.forwardReady = sync.NewCond(&l.forwardMu)
	l.forwardDone = make(chan struct{})
	go l.forwardLoop()
	return l
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

// Close drains the forwarding queue and stops its worker. Ordinary log calls
// never wait for ClientLog; shutdown is the one boundary that waits so a
// process exit cannot strand diagnostics which were already accepted.
func (l *Logger) Close() {
	if l == nil || l.forwarder == nil {
		return
	}
	l.forwardMu.Lock()
	if !l.forwardClosing {
		l.forwardClosing = true
		l.forwardReady.Broadcast()
	}
	done := l.forwardDone
	l.forwardMu.Unlock()
	<-done
}

// Close drains the root logger's forwarding queue.
func (b *Bound) Close() {
	if b == nil {
		panic("sidecar logging: Close called on nil Bound logger")
	}
	b.logger.Close()
}

// RegisterFile binds proven workspace/session attribution to one normalized
// path so later high-level records naming only that path inherit it.
func (b *Bound) RegisterFile(ctx Context) {
	if b == nil {
		panic("sidecar logging: RegisterFile called on nil Bound logger")
	}
	ctx = mergeContext(b.context, ctx)
	if ctx.Path == "" || ctx.WorkspaceDir == "" || ctx.WorkspaceID == "" || ctx.ClaudeSessionID == "" {
		panic("sidecar logging: registered file requires path, workspace_dir, workspace_id, and claude_session_id")
	}
	identity := Context{
		Path: ctx.Path, WorkspaceDir: ctx.WorkspaceDir, WorkspaceID: ctx.WorkspaceID,
		ClaudeSessionID: ctx.ClaudeSessionID,
	}
	b.logger.mu.Lock()
	b.logger.files[ctx.Path] = identity
	b.logger.mu.Unlock()
}

// Log records a normal diagnostic to the persistent log and stderr.
func (b *Bound) Log(format string, args ...any) {
	if b == nil {
		panic("sidecar logging: Log called on nil Bound logger")
	}
	b.logger.write(false, b.context, format, args...)
}

// LogVerbose records a debug-level verbose diagnostic. AGENT_REPL_LOG_LEVEL
// decides whether the record reaches either sink.
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
		"refusal_site":      ctx.RefusalSite,
		"reason":            ctx.Reason,
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
	if ctx.Repeat != nil {
		out["repeat_count"] = *ctx.Repeat
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
	if ctx.Path != "" {
		l.mu.Lock()
		identity := l.files[ctx.Path]
		l.mu.Unlock()
		ctx = mergeContext(identity, ctx)
	}
	if ctx.Operation == "" {
		panic("sidecar logging: operation is required")
	}
	level := ctx.Level
	if level == "" {
		if verbose {
			level = "debug"
		} else {
			level = "info"
		}
	}
	if (ctx.WorkspaceDir == "") != (ctx.WorkspaceID == "") {
		panic("sidecar logging: workspace_dir and workspace_id must be set together")
	}
	if !l.minimumLevel.Allows(level) {
		return
	}
	verbosity := "normal"
	if verbose {
		verbosity = "verbose"
	}
	now := l.now().Local()
	rec := record{
		Timestamp:       sharedlogging.Timestamp(now),
		Runtime:         "sidecar",
		PID:             l.pid(),
		Level:           level,
		Verbosity:       verbosity,
		Operation:       ctx.Operation,
		Message:         fmt.Sprintf(format, args...),
		WorkspaceDir:    ctx.WorkspaceDir,
		WorkspaceID:     ctx.WorkspaceID,
		ClaudeSessionID: ctx.ClaudeSessionID,
		RequestID:       ctx.RequestID,
		Context:         contextMap(ctx),
	}
	if l.forwarder != nil && ctx.WorkspaceDir != "" && !ctx.SinkEmergency {
		forwardContext := cloneMap(rec.Context)
		forwardContext["pid"] = rec.PID
		forwardContext["claude_session_id"] = rec.ClaudeSessionID
		l.enqueueForward(ForwardRecord{
			Timestamp: rec.Timestamp, PID: rec.PID, Level: rec.Level,
			Verbose: verbose, Operation: rec.Operation, Message: rec.Message,
			WorkspaceDir: rec.WorkspaceDir, WorkspaceID: rec.WorkspaceID,
			ClaudeSessionID: rec.ClaudeSessionID, Context: forwardContext,
		})
		return
	}
	payload, err := json.Marshal(rec)
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
	if l.terminalEmergencyOnly && !ctx.SinkEmergency {
		return
	}
	if err := writeAll(l.stderr, line); err != nil {
		panic(fmt.Sprintf("sidecar logging: stderr sink failed: %v", err))
	}
}

func (l *Logger) enqueueForward(rec ForwardRecord) {
	l.forwardMu.Lock()
	defer l.forwardMu.Unlock()
	if l.forwardClosing {
		panic("sidecar logging: file-scoped record written after forwarding closed")
	}
	l.forwardQueue = append(l.forwardQueue, rec)
	l.forwardReady.Signal()
}

func (l *Logger) forwardLoop() {
	defer close(l.forwardDone)
	for {
		l.forwardMu.Lock()
		for len(l.forwardQueue) == 0 && !l.forwardClosing {
			l.forwardReady.Wait()
		}
		if len(l.forwardQueue) == 0 {
			l.forwardMu.Unlock()
			return
		}
		rec := l.forwardQueue[0]
		l.forwardQueue[0] = ForwardRecord{}
		l.forwardQueue = l.forwardQueue[1:]
		l.forwardMu.Unlock()

		address, err := l.forwarder.Forward(rec)
		if err != nil {
			l.reportForwardFailure(l.now().Local(), address, record{
				Operation: rec.Operation, WorkspaceDir: rec.WorkspaceDir,
				WorkspaceID: rec.WorkspaceID, ClaudeSessionID: rec.ClaudeSessionID,
			}, err)
			continue
		}
		l.mu.Lock()
		l.lastForwardFailure = ""
		l.mu.Unlock()
	}
}

// reportForwardFailure writes one global failure per daemon address and outage
// window. A successful forward resets the limiter. The original file operation
// continues: diagnostics persistence must never stop transcript ingestion. An
// empty address means resolution itself failed, so the daemon.addr path
// supplied by the forwarder remains the rate-limit key.
func (l *Logger) reportForwardFailure(now time.Time, address string, target record, cause error) {
	if address == "" {
		address = "unresolved"
	}
	l.mu.Lock()
	if l.lastForwardFailure == address {
		l.mu.Unlock()
		return
	}
	l.lastForwardFailure = address
	l.mu.Unlock()

	payload, err := json.Marshal(record{
		Timestamp: sharedlogging.Timestamp(now), Runtime: "sidecar", PID: l.pid(),
		Level: "error", Verbosity: "normal", Operation: "sidecar.logging.forward-failure",
		Message: "a file-scoped diagnostic could not be forwarded to the daemon",
		Context: map[string]any{
			"daemon_address": address, "error": cause.Error(),
			"target_operation": target.Operation, "target_workspace_dir": target.WorkspaceDir,
			"target_workspace_id": target.WorkspaceID, "target_claude_session_id": target.ClaudeSessionID,
		},
	})
	if err != nil {
		panic(fmt.Sprintf("sidecar logging: encode forwarding failure: %v", err))
	}
	line := append(payload, '\n')
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.poisoned != nil {
		panic(fmt.Sprintf("sidecar logging: persistent sink previously failed: %v", l.poisoned))
	}
	if err := writeAll(l.file, line); err != nil {
		l.poisoned = err
		l.reportSinkFailure(now, target.Operation, err)
		panic(fmt.Sprintf("sidecar logging: persistent sink failed: %v", err))
	}
}

func cloneMap(in map[string]any) map[string]any {
	out := make(map[string]any, len(in)+2)
	for key, value := range in {
		out[key] = value
	}
	return out
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
		{&base.WorkspaceDir, &add.WorkspaceDir},
		{&base.WorkspaceID, &add.WorkspaceID},
		{&base.ClaudeSessionID, &add.ClaudeSessionID},
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
		{&base.RefusalSite, &add.RefusalSite},
		{&base.Reason, &add.Reason},
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
	if add.Repeat != nil {
		base.Repeat = add.Repeat
	}
	if add.BackoffMs != nil {
		base.BackoffMs = add.BackoffMs
	}
	if add.SinkEmergency {
		base.SinkEmergency = true
	}
	return base
}
