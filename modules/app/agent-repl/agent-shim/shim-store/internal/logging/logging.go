// Package logging owns shim-store's single structured logging API.
//
// THE CORRELATION VOCABULARY IS THE ADDRESSING. The retired (session_id, seq)
// addressing died with the schema, and its keys died with it: claude_session_id,
// seq, from_seq and the replay_*_seq trio name nothing this store can produce.
// What replaces them is the store's real addressing — the agent whose book a
// line belongs to, the write that produced it, the row it superseded, the
// position it landed at, and the store-internal write ordinal a watcher is
// pinned to.
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
// Empty fields are omitted. The store's runtime owners bind the stable values
// they know: database and table in db, socket and rpc in server.
type Fields struct {
	// ---- Runtime placement ----
	Component    string
	DatabasePath string
	Table        string
	Socket       string
	Transaction  string
	Operation    string
	Level        string

	// ---- Top-level record identity ----
	AgentReplSessionID string
	RequestID          string

	// ---- The correlation vocabulary ----

	// Producer is the WriteBatch caller's self-declared name.
	Producer string
	// WriteClass is the queue a write took into the one writer —
	// "interactive" or "bulk" — as the caller stated it.
	WriteClass string
	// AgentID is a conversation.v1.AgentId.value.
	AgentID  string
	AgentIDs []string
	// VendorSessionID is the vendor's own mutable session identity, when the
	// record concerns one. It is an ATTRIBUTE, never an address.
	VendorSessionID string
	// BookAgentID is the agent whose book a page line belongs to
	// (StorePageLine.page_agent_id.value). Empty for every never-served row.
	BookAgentID  string
	BookAgentIDs []string
	// WriteID is StoreEntry.write_id — the replay-dedup identity of one write.
	WriteID string
	// UpsertKey is StoreEntry.upsert_key — the identity of the ROW a write
	// supersedes whole.
	UpsertKey string
	// Position is a page position, rendered as the opaque pointer the store
	// mints for it. Never the raw rowid: a log reader echoing a pointer must
	// be echoing the same string the caller holds.
	Position string
	// WriteSeq is the store-internal global write ordinal — the watch pin.
	// Never on the wire. Omitted when zero, which is never a valid ordinal.
	WriteSeq uint64
	// WatchTokenHash is the sha256 PREFIX of a watch token, never the token.
	WatchTokenHash string
	// RPC is the Connect procedure name, spelled exactly as Connect does —
	// with its leading slash, e.g. "/store.v1.ShimStore/WriteBatch".
	RPC string
	// RefusalSite names WHICH refusal the store issued — the vocabulary the
	// proto's failure `kind` arms are derived from — so refusals are counted
	// by site instead of grepped out of a human detail string.
	RefusalSite string
	// RefusalKind names the WIRE ARM the refusal became — `invalid_request`,
	// `stale_pointer`, `storage_failure`, `not_implemented`.
	//
	// IT IS NOT THE SITE. The site says which of the store's many checks said
	// no; the kind says what the caller received, and several sites map to one
	// arm. A reader triaging refusals needs both: the site to find the check,
	// the kind to know whether the caller could ever have retried.
	RefusalKind string
	// FileID is a CursorState.file_id.
	FileID string
	// Path is a filesystem path a record concerns (a cursor's file, a spool).
	Path string
	// Offset is a byte offset. A POINTER because zero is a meaningful offset
	// and must be reported rather than omitted.
	Offset *int64
	// TaskID, ActivityID and TurnID are the conversation.v1 unit identities.
	TaskID     string
	ActivityID string
	TurnID     string

	// ---- Log level window ----

	// LevelWindow is the evidence of a log level window decision: a startup
	// selection or a window that ended (agentrepl/logging Selection.Context
	// and Expiry.Context). Its keys land in the record's context as given.
	LevelWindow map[string]any

	// ---- Fan-out accounting ----
	Subscriber     string
	Delivered      uint64
	TerminalOwner  string
	TerminalReason string
	ErrorCause     string

	// ---- Query timing ----

	// Statement is a SQL statement FAMILY — "write_batch", "open_page" —
	// never rendered SQL and never bound values. The store's payloads are
	// opaque to it by design, and a slow-query record that quoted a statement
	// with its parameters would put session content into the global log.
	//
	// It is also the marker for the query-timing group below: Duration,
	// LockWait, Rows and Threshold are emitted only alongside a statement
	// family, so a zero row count is reported as zero rather than omitted as
	// "unset".
	Statement string
	Duration  time.Duration
	// LockWait is how much of Duration was spent WAITING to begin — for the
	// database's write lock, and for a connection out of the pool — rather
	// than running the statement.
	//
	// IT IS NOT AN OPTIONAL EXTRA. The store serializes its own writes, so one
	// producer's batch queues on the write slot behind every other writer's.
	// A record reporting only the total said "this statement took 3.8 seconds"
	// about a statement that ran in microseconds behind a 3.8 second queue,
	// and an operator reading it went looking for a missing index that was
	// never missing. Split out, the same record says which of the two it was.
	// (Reads have their own pool and queue on nothing, so they report zero.)
	LockWait time.Duration
	// Exec is the part of Duration spent EXECUTING a write once it held the
	// writer — Duration minus LockWait. Emitted (as exec_ms) only on a
	// record that names a WriteClass, since only a write has a queue to
	// subtract.
	Exec      time.Duration
	Rows      int64
	Threshold time.Duration
	// OverBudget and BudgetWindow are how many of this statement family's last
	// BudgetWindow observations exceeded their budget, and they are what
	// separates a DEFECT from a spike: a lost index or a reintroduced scan
	// makes every statement of the family slow, while a loaded host makes one
	// of them slow. A record carries them only when the store measured the
	// window, so an unmeasured record omits both rather than reporting zero of
	// zero.
	OverBudget   int
	BudgetWindow int

	// WAL is the state of a WAL a reader keeps a checkpoint from folding, on
	// the records that open and close a pin (store.db.wal-pin).
	WAL *WALState
}

// WALState is a pinned WAL as the checkpoint job last saw it.
type WALState struct {
	Frames     int64
	Backfilled int64
	// ReadMarks are the WAL-index's reader slots: the frame each slot's
	// snapshot ends at.
	ReadMarks []uint32
	PinnedFor time.Duration

	ReadPoolOpen  int
	ReadPoolInUse int
	ReadPoolIdle  int
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
	RequestID          string         `json:"request_id,omitempty"`
	Context            map[string]any `json:"context"`
}

// Logger writes records at or above its configured AGENT_REPL_LOG_LEVEL to the
// persistent log and, when configured, the interactive sink.
type Logger struct {
	file   io.Writer
	stderr io.Writer
	// terminalEmergencyOnly withholds the ORDINARY record stream from the
	// terminal, leaving it the sink-failure record it is the last channel for.
	// Under launchd the terminal is an append-only file the process neither
	// owns nor can roll, so mirroring every record there is a second,
	// unbounded copy of a log that is already durable and rotated.
	terminalEmergencyOnly bool
	// threshold is the process's live level, shared by every With copy so a
	// level window that ends ends for all of them.
	threshold *sharedlogging.Window
	fields                Fields
	state                 *sinkState
	clock                 func() time.Time
	pid                   func() int
}

type sinkState struct {
	mu       sync.Mutex
	poisoned error
}

// New creates the shim-store logger. debugEnabled exists for focused tests and
// foreground harnesses; production passes the parsed process threshold through
// NewAtLevel. Both sinks are required runtime dependencies.
func New(file, stderr io.Writer, verboseEnabled bool) *Logger {
	level := sharedlogging.LevelInfo
	if verboseEnabled {
		level = sharedlogging.LevelDebug
	}
	return NewAtLevel(file, stderr, level)
}

// NewAtLevel creates the shim-store logger at one explicit severity threshold
// that never ends.
func NewAtLevel(file, stderr io.Writer, minimumLevel sharedlogging.Level) *Logger {
	return newLogger(file, stderr, sharedlogging.FixedWindow(minimumLevel))
}

func newLogger(file, stderr io.Writer, threshold *sharedlogging.Window) *Logger {
	if file == nil || stderr == nil {
		panic("shim-store logging: nil output sink")
	}
	if threshold == nil {
		panic("shim-store logging: nil level threshold")
	}
	return &Logger{
		file:      file,
		stderr:    stderr,
		threshold: threshold,
		state:     &sinkState{},
		clock:     time.Now,
		pid:       os.Getpid,
	}
}

// NewDurableOnly creates a logger that writes the record stream to the durable
// sink ALONE, keeping the terminal for the sink-failure record.
//
// This is what PRODUCTION uses. `New`'s mirroring is right when both sinks are
// the caller's to manage and wrong under launchd, where the terminal is an
// unbounded file nobody rolls.
func NewDurableOnly(file, terminal io.Writer, verboseEnabled bool) *Logger {
	level := sharedlogging.LevelInfo
	if verboseEnabled {
		level = sharedlogging.LevelDebug
	}
	return NewDurableOnlyAtLevel(file, terminal, level)
}

// NewDurableOnlyAtLevel creates the production logger at one explicit
// severity threshold.
func NewDurableOnlyAtLevel(file, terminal io.Writer, minimumLevel sharedlogging.Level) *Logger {
	return NewDurableOnlyWindow(file, terminal, sharedlogging.FixedWindow(minimumLevel))
}

// NewDurableOnlyWindow creates the production logger on the process's live
// level window: a level other than info reverts to info when its window ends,
// and the revert is recorded at info (operation store.logging.level-window).
func NewDurableOnlyWindow(file, terminal io.Writer, threshold *sharedlogging.Window) *Logger {
	l := newLogger(file, terminal, threshold)
	l.terminalEmergencyOnly = true
	return l
}

// levelWindowOperation is the operation of every level window record.
const levelWindowOperation = "store.logging.level-window"

// NoteLevelSelection records the process's startup level decision at info,
// when anything other than info was asked for.
func (l *Logger) NoteLevelSelection(sel sharedlogging.Selection) {
	if message, ok := sel.Note(); ok {
		l.Log(Fields{Operation: levelWindowOperation, Level: "info", LevelWindow: sel.Context()}, "%s", message)
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
	l.write("normal", fields, format, args)
}

// LogVerbose records a debug-level verbose diagnostic. AGENT_REPL_LOG_LEVEL
// decides whether the record reaches either sink.
func (l *Logger) LogVerbose(fields Fields, format string, args ...any) {
	l.write("verbose", fields, format, args)
}

func (l *Logger) write(verbosity string, fields Fields, format string, args []any) {
	if l == nil {
		panic("shim-store logging: nil logger")
	}
	merged := merge(l.fields, fields)
	if merged.Operation == "" {
		panic("shim-store logging: operation is required")
	}
	level := merged.Level
	if level == "" {
		if verbosity == "verbose" {
			level = "debug"
		} else {
			level = "info"
		}
	}
	allowed, ended := l.threshold.Allows(level)
	if ended != nil {
		// The window is info now, so this record cannot end it again.
		l.Log(Fields{Operation: levelWindowOperation, Level: "info", LevelWindow: ended.Context()}, "%s", ended.Message())
	}
	if !allowed {
		return
	}
	context := map[string]any{}
	for key, value := range map[string]string{
		"component":         merged.Component,
		"db":                merged.DatabasePath,
		"table":             merged.Table,
		"socket":            merged.Socket,
		"transaction":       merged.Transaction,
		"producer":          merged.Producer,
		"write_class":       merged.WriteClass,
		"agent_id":          merged.AgentID,
		"vendor_session_id": merged.VendorSessionID,
		"book_agent_id":     merged.BookAgentID,
		"write_id":          merged.WriteID,
		"upsert_key":        merged.UpsertKey,
		"position":          merged.Position,
		"watch_token_hash":  merged.WatchTokenHash,
		"rpc":               merged.RPC,
		"refusal_site":      merged.RefusalSite,
		"refusal_kind":      merged.RefusalKind,
		"file_id":           merged.FileID,
		"path":              merged.Path,
		"task_id":           merged.TaskID,
		"activity_id":       merged.ActivityID,
		"turn_id":           merged.TurnID,
		"subscriber":        merged.Subscriber,
		"terminal_owner":    merged.TerminalOwner,
		"terminal_reason":   merged.TerminalReason,
		"error":             merged.ErrorCause,
	} {
		if value != "" {
			context[key] = value
		}
	}
	for key, value := range merged.LevelWindow {
		context[key] = value
	}
	if len(merged.AgentIDs) != 0 {
		context["agent_ids"] = merged.AgentIDs
	}
	if len(merged.BookAgentIDs) != 0 {
		context["book_agent_ids"] = merged.BookAgentIDs
	}
	if merged.WriteSeq != 0 {
		context["write_seq"] = merged.WriteSeq
	}
	if merged.Offset != nil {
		context["offset"] = *merged.Offset
	}
	if merged.Statement != "" {
		context["statement"] = merged.Statement
		context["duration_ms"] = merged.Duration.Milliseconds()
		context["lock_wait_ms"] = merged.LockWait.Milliseconds()
		context["rows"] = merged.Rows
		context["threshold_ms"] = merged.Threshold.Milliseconds()
		if merged.WriteClass != "" {
			context["exec_ms"] = merged.Exec.Milliseconds()
		}
		if merged.BudgetWindow != 0 {
			context["over_budget_recent"] = merged.OverBudget
			context["over_budget_window"] = merged.BudgetWindow
		}
	}
	if wal := merged.WAL; wal != nil {
		context["wal_frames"] = wal.Frames
		context["wal_backfilled"] = wal.Backfilled
		context["wal_read_marks"] = wal.ReadMarks
		context["wal_pinned_for_ms"] = wal.PinnedFor.Milliseconds()
		context["read_pool_open"] = wal.ReadPoolOpen
		context["read_pool_in_use"] = wal.ReadPoolInUse
		context["read_pool_idle"] = wal.ReadPoolIdle
	}
	terminal := merged.TerminalOwner != "" || merged.TerminalReason != ""
	if merged.Delivered != 0 || terminal {
		context["delivered"] = merged.Delivered
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
	if l.terminalEmergencyOnly {
		return
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
	for _, pair := range []struct{ dst, src *string }{
		{&base.Component, &extra.Component},
		{&base.DatabasePath, &extra.DatabasePath},
		{&base.Table, &extra.Table},
		{&base.Socket, &extra.Socket},
		{&base.Transaction, &extra.Transaction},
		{&base.Operation, &extra.Operation},
		{&base.Level, &extra.Level},
		{&base.AgentReplSessionID, &extra.AgentReplSessionID},
		{&base.RequestID, &extra.RequestID},
		{&base.Producer, &extra.Producer},
		{&base.WriteClass, &extra.WriteClass},
		{&base.AgentID, &extra.AgentID},
		{&base.VendorSessionID, &extra.VendorSessionID},
		{&base.BookAgentID, &extra.BookAgentID},
		{&base.WriteID, &extra.WriteID},
		{&base.UpsertKey, &extra.UpsertKey},
		{&base.Position, &extra.Position},
		{&base.WatchTokenHash, &extra.WatchTokenHash},
		{&base.RPC, &extra.RPC},
		{&base.RefusalSite, &extra.RefusalSite},
		{&base.RefusalKind, &extra.RefusalKind},
		{&base.FileID, &extra.FileID},
		{&base.Path, &extra.Path},
		{&base.TaskID, &extra.TaskID},
		{&base.ActivityID, &extra.ActivityID},
		{&base.TurnID, &extra.TurnID},
		{&base.Subscriber, &extra.Subscriber},
		{&base.TerminalOwner, &extra.TerminalOwner},
		{&base.TerminalReason, &extra.TerminalReason},
		{&base.ErrorCause, &extra.ErrorCause},
		{&base.Statement, &extra.Statement},
	} {
		if *pair.src != "" {
			*pair.dst = *pair.src
		}
	}
	if extra.LevelWindow != nil {
		base.LevelWindow = extra.LevelWindow
	}
	if extra.WriteSeq != 0 {
		base.WriteSeq = extra.WriteSeq
	}
	if len(extra.AgentIDs) != 0 {
		base.AgentIDs = extra.AgentIDs
	}
	if len(extra.BookAgentIDs) != 0 {
		base.BookAgentIDs = extra.BookAgentIDs
	}
	if extra.Offset != nil {
		base.Offset = extra.Offset
	}
	if extra.Delivered != 0 {
		base.Delivered = extra.Delivered
	}
	if extra.Duration != 0 {
		base.Duration = extra.Duration
	}
	if extra.LockWait != 0 {
		base.LockWait = extra.LockWait
	}
	if extra.Exec != 0 {
		base.Exec = extra.Exec
	}
	if extra.Rows != 0 {
		base.Rows = extra.Rows
	}
	if extra.Threshold != 0 {
		base.Threshold = extra.Threshold
	}
	if extra.BudgetWindow != 0 {
		base.OverBudget = extra.OverBudget
		base.BudgetWindow = extra.BudgetWindow
	}
	if extra.WAL != nil {
		base.WAL = extra.WAL
	}
	return base
}
