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
	"errors"
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	sharedlogging "agentrepl/logging"
)

// ErrForwardTargetNotThere marks a forwarding failure whose target daemon is
// PROVABLY GONE — its advertised pid died, or daemon.addr no longer names the
// address this attempt dialed — rather than merely unreachable. A Forwarder
// wraps it onto a connection/dial failure (see daemonclient.Client.Forward);
// forwardLoop uses errors.Is to treat such a failure as a restart transient
// (DEBUG, retried, still persisted undelivered) instead of manufacturing a
// WARN against a daemon that was replaced or exited mid-flight. A forward
// failure against a target whose advertised pid IS alive remains a genuine
// WARN: this sentinel names an absence, not a fault, mirroring the
// daemon.addr pid invariant (see logging-contract.md).
var ErrForwardTargetNotThere = errors.New("sidecar logging: forward target daemon is not there")

// ErrForwardTargetBooting marks a forwarding failure against a daemon ADDRESS
// that has never once been seen accepting a connection — via Forwarder.Ready
// or a prior successful Forward — even though the advertisement this attempt
// dialed is unchanged and its pid is alive. A daemon publishes daemon.addr and
// its pid before its listener answers, so a connection/dial failure against an
// address still in that boot window is a STARTUP TRANSIENT specific to that
// address, not a stuck daemon: a Forwarder wraps it onto a connection/dial
// failure (see daemonclient.Client.classifyForward); forwardLoop uses
// errors.Is to treat such a failure as a boot transient (DEBUG, retried, still
// persisted undelivered) instead of manufacturing a WARN against an address
// that has simply not finished coming up. Only a forward failure against an
// address that WAS previously seen accepting remains a genuine WARN candidate
// — this refines the pid-liveness check of ErrForwardTargetNotThere with
// PER-ADDRESS boot tolerance (see logging-contract.md).
var ErrForwardTargetBooting = errors.New("sidecar logging: forward target daemon has never been seen accepting")

// ErrForwardWorkspaceUnresolvable marks a forwarding failure whose record
// named a workspace that a HEALTHY, FULLY-DELIVERED roster does not contain --
// a macOS temp-root, or any path that is not a real workspace and so will
// never appear in the roster. The roster stream CONNECTED and delivered its
// current snapshot; the dir is simply absent from it. This is neither a
// transport fault nor a booting daemon: retrying cannot make an absent dir
// appear, and blocking the roster stream to its deadline waiting for a
// workspace that will never register is exactly the stall this sentinel
// forbids. A Forwarder returns it AT ONCE when the first delivered snapshot
// lacks the dir (see daemonclient.Client.resolveWorkspace); forwardLoop, using
// errors.Is, does NOT retry it, narrates it at DEBUG, and persists the record
// UNATTRIBUTED in the global durable sink rather than manufacturing a WARN. It
// is distinct from a roster stream that never connected or errored before
// delivering a snapshot, which remains a transport transient handled by the
// pid/boot sentinels above.
var ErrForwardWorkspaceUnresolvable = errors.New("sidecar logging: forward record names a workspace absent from a healthy roster")

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
	// LevelWindow is the evidence of a log level window decision: a startup
	// selection or a window that ended (agentrepl/logging Selection.Context
	// and Expiry.Context). Its keys land in the record's context as given.
	LevelWindow map[string]any
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
	// Ready reports whether the daemon has PUBLISHED a live address and is
	// accepting connections. It is what separates a daemon that is still
	// booting — address absent or its listener not yet answering — from one
	// that was serving and then went unreachable. The address is the probed
	// destination, returned even when it is not yet live so the logger can key
	// its records on it.
	Ready() (daemonAddress string, ready bool)
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
	// threshold is the process's live level: a level other than info is a
	// window that reverts to info by itself (see NewForwardingDurableOnlyWindow).
	threshold *sharedlogging.Window
	poisoned  error
	files                 map[string]Context
	// The startup catch-up window. A restarted sidecar re-derives the owner's
	// whole historical corpus from files, and the per-item records of that walk
	// are a CONDITION OF THE BACKLOG rather than events worth an INFO line each
	// — the same inverted pyramid the rescan-driven holds and the LOST tracker
	// already level, arriving through the operations that read the corpus.
	// While the window is open, an INFO record of a registered operation is
	// stated at DEBUG and tallied; closing the window states one INFO
	// `catchup-summary` per operation carrying the count. Nothing is silenced:
	// the detail is still written, and the totals ride the summary.
	catchupActive      bool
	catchupOps         map[string]struct{}
	catchupTally       map[string]int
	catchupOrder       []string
	forwarder          Forwarder
	lastForwardFailure string
	// lastForwardDeferred rate-limits the DEBUG startup-transient narration the
	// same way lastForwardFailure rate-limits the WARN, per daemon address and
	// boot window, so a slow boot does not narrate a rung per record.
	lastForwardDeferred string
	forwardMu           sync.Mutex
	forwardReady        *sync.Cond
	forwardQueue        []ForwardRecord
	forwardClosing      bool
	forwardDone         chan struct{}
	// forwardStop is closed by Close. It is what lets a retry ladder ABANDON
	// its wait at shutdown: a booting daemon is worth a dozen seconds of
	// backoff while the process runs, and nothing at all while it exits.
	forwardStop chan struct{}
	// The retry ladder. A DAEMON THAT IS BOOTING IS NOT A DAEMON THAT IS GONE:
	// its 10s boot reconciliation answers no rpc, so the first ClientLog of a
	// sidecar that came up beside it is refused for reasons that fix
	// themselves. One attempt dropped the diagnostic on the floor.
	forwardAttempts   int
	forwardBackoffMin time.Duration
	forwardBackoffMax time.Duration
	// forwardWait is the inter-attempt delay, injected in tests. It answers
	// false when the wait was abandoned because the logger is closing.
	forwardWait func(time.Duration) bool
}

// The forwarding retry ladder's defaults. Six attempts over a doubling
// 250ms..5s backoff span ~12.75s, which outlasts the daemon's boot
// reconciliation window without turning a genuinely dead daemon into a stalled
// queue.
const (
	defaultForwardAttempts   = 6
	defaultForwardBackoffMin = 250 * time.Millisecond
	defaultForwardBackoffMax = 5 * time.Second
)

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
	return newLogger(stderr, file, sharedlogging.FixedWindow(minimumLevel))
}

func newLogger(stderr, file io.Writer, threshold *sharedlogging.Window) *Logger {
	if stderr == nil || file == nil {
		panic("sidecar logging requires stderr and persistent file sinks")
	}
	if threshold == nil {
		panic("sidecar logging requires a level threshold")
	}
	return &Logger{
		stderr:    stderr,
		file:      file,
		now:       time.Now,
		pid:       os.Getpid,
		threshold: threshold,
		files:     map[string]Context{},
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
	return newDurableOnly(terminal, file, sharedlogging.FixedWindow(minimumLevel))
}

func newDurableOnly(terminal, file io.Writer, threshold *sharedlogging.Window) *Logger {
	l := newLogger(terminal, file, threshold)
	l.terminalEmergencyOnly = true
	return l
}

// NewForwardingDurableOnlyAtLevel constructs the production logger: global
// records go to file, while file-scoped records enter an ordered forwarding
// queue for the daemon-owned workspace sink. A nil forwarder is an invariant
// violation, never permission to put a workspace record in the global sink.
func NewForwardingDurableOnlyAtLevel(terminal, file io.Writer, minimumLevel sharedlogging.Level, forwarder Forwarder) *Logger {
	return NewForwardingDurableOnlyWindow(terminal, file, sharedlogging.FixedWindow(minimumLevel), forwarder)
}

// levelWindowOperation is the operation of every level window record.
const levelWindowOperation = "sidecar.logging.level-window"

// NewForwardingDurableOnlyWindow is NewForwardingDurableOnlyAtLevel on the
// process's live level window: a level other than info reverts to info when
// its window ends, and the revert is recorded at info.
func NewForwardingDurableOnlyWindow(terminal, file io.Writer, threshold *sharedlogging.Window, forwarder Forwarder) *Logger {
	if forwarder == nil {
		panic("sidecar logging: forwarding logger requires a daemon forwarder")
	}
	l := newDurableOnly(terminal, file, threshold)
	l.forwarder = forwarder
	l.forwardReady = sync.NewCond(&l.forwardMu)
	l.forwardDone = make(chan struct{})
	l.forwardStop = make(chan struct{})
	l.forwardAttempts = defaultForwardAttempts
	l.forwardBackoffMin = defaultForwardBackoffMin
	l.forwardBackoffMax = defaultForwardBackoffMax
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

// NoteLevelSelection records the process's startup level decision at info,
// when anything other than info was asked for.
func (b *Bound) NoteLevelSelection(sel sharedlogging.Selection) {
	if message, ok := sel.Note(); ok {
		b.logger.write(false, mergeContext(b.context, Context{Operation: levelWindowOperation, Level: "info", LevelWindow: sel.Context()}), "%s", message)
	}
}

// With extends the bound attribution. Set fields replace earlier values.
func (b *Bound) With(ctx Context) *Bound {
	if b == nil {
		panic("sidecar logging: With called on nil Bound logger")
	}
	return &Bound{logger: b.logger, context: mergeContext(b.context, ctx)}
}

// DefaultShutdownDrain bounds how long a process exit waits for the forwarding
// queue to drain.
//
// WHY IT IS BOUNDED. A healthy teardown is sub-millisecond — the owner's log
// puts 90µs between the `shutdown` record and the `exit` record, and the
// replacement process starting 34ms later. An UNHEALTHY one is not slow, it is
// STUCK: with the daemon gone, the closing forward loop still probes and dials
// once per queued record, and one boot had 42,044 undelivered records queued.
// That teardown ran past three minutes on the owner's machine while launchd
// waited on a service it had already asked to stop. Five seconds is a large
// multiple of every healthy teardown ever observed here and a small fraction of
// launchd's exit timeout, so a stuck drain costs the operator a bounded pause
// instead of a hung service.
const DefaultShutdownDrain = 5 * time.Second

// Close drains the forwarding queue and stops its worker, waiting as long as it
// takes. Ordinary log calls never wait for ClientLog; shutdown is the one
// boundary that waits so a process exit cannot strand diagnostics which were
// already accepted.
//
// PRODUCTION USES CloseWithin. An unbounded wait is right for a test that owns
// both ends of the forwarder and wrong for a service launchd is waiting on.
func (l *Logger) Close() {
	if l == nil || l.forwarder == nil {
		return
	}
	done := l.beginClose()
	<-done
}

// CloseWithin drains the forwarding queue and stops its worker, ABANDONING the
// wait after d. It answers how many records were still queued when the bound
// fired, and whether the queue drained.
//
// A bound that fires is STATED, never silent: the record names the count still
// queued and the daemon address the loop was forwarding to, at INFO, through
// the durable sink — the queue is closed by then, so nothing about this record
// can re-enter it.
func (l *Logger) CloseWithin(d time.Duration) (pending int, drained bool) {
	if l == nil || l.forwarder == nil {
		return 0, true
	}
	done := l.beginClose()
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-done:
		return 0, true
	case <-timer.C:
	}
	l.forwardMu.Lock()
	pending = len(l.forwardQueue)
	l.forwardMu.Unlock()
	address, _ := l.forwarder.Ready()
	l.write(false, Context{Operation: "shutdown-drain"},
		"the log forwarding queue did not drain within %s; abandoning it with %d record(s) still queued for the daemon at %s and exiting",
		d, pending, addrOrUnresolved(address))
	return pending, false
}

// beginClose latches the closing state exactly once and answers the channel the
// forward loop closes when it stops.
func (l *Logger) beginClose() chan struct{} {
	l.forwardMu.Lock()
	defer l.forwardMu.Unlock()
	if !l.forwardClosing {
		l.forwardClosing = true
		close(l.forwardStop)
		l.forwardReady.Broadcast()
	}
	return l.forwardDone
}

// Close drains the root logger's forwarding queue.
func (b *Bound) Close() {
	if b == nil {
		panic("sidecar logging: Close called on nil Bound logger")
	}
	b.logger.Close()
}

// CloseWithin drains the root logger's forwarding queue under a bound.
func (b *Bound) CloseWithin(d time.Duration) (int, bool) {
	if b == nil {
		panic("sidecar logging: CloseWithin called on nil Bound logger")
	}
	return b.logger.CloseWithin(d)
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

// BeginCatchup opens the startup catch-up window over the named operations.
// Calling it again while the window is open is an invariant violation: the
// boundary is the process's one boot walk, not a per-pass toggle.
func (l *Logger) BeginCatchup(operations ...string) {
	if l == nil {
		panic("sidecar logging: BeginCatchup called on nil Logger")
	}
	if len(operations) == 0 {
		panic("sidecar logging: BeginCatchup requires at least one operation")
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.catchupActive {
		panic("sidecar logging: the catch-up window is already open")
	}
	l.catchupActive = true
	l.catchupOps = map[string]struct{}{}
	l.catchupTally = map[string]int{}
	l.catchupOrder = append([]string(nil), operations...)
	for _, operation := range operations {
		l.catchupOps[operation] = struct{}{}
	}
}

// EndCatchup closes the window and states ONE `catchup-summary` INFO record per
// operation that demoted anything, carrying the operation in `reason` and the
// count in `repeat_count`. An operation that demoted nothing states nothing.
// Closing a window that is not open is a no-op, so a shutdown that races the
// first drained pass costs nothing.
func (l *Logger) EndCatchup() {
	if l == nil {
		panic("sidecar logging: EndCatchup called on nil Logger")
	}
	l.mu.Lock()
	if !l.catchupActive {
		l.mu.Unlock()
		return
	}
	l.catchupActive = false
	tally, order := l.catchupTally, l.catchupOrder
	l.catchupOps, l.catchupTally, l.catchupOrder = nil, nil, nil
	l.mu.Unlock()
	// Stated after the window is closed and the lock is released, so the
	// summaries themselves are ordinary INFO records rather than candidates for
	// the demotion they are reporting.
	for _, operation := range order {
		count := tally[operation]
		if count == 0 {
			continue
		}
		l.write(false, Context{
			Operation: "catchup-summary", Level: "info",
			Reason: operation, Repeat: Repeat(count),
		}, "startup catch-up stated %d %s record(s) at debug; steady state states each one", count, operation)
	}
}

// tallyCatchup reports whether this operation's INFO record belongs to the open
// catch-up window, counting it when it does.
func (l *Logger) tallyCatchup(operation string) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	if !l.catchupActive {
		return false
	}
	if _, registered := l.catchupOps[operation]; !registered {
		return false
	}
	l.catchupTally[operation]++
	return true
}

// BeginCatchup opens the root logger's startup catch-up window.
func (b *Bound) BeginCatchup(operations ...string) {
	if b == nil {
		panic("sidecar logging: BeginCatchup called on nil Bound logger")
	}
	b.logger.BeginCatchup(operations...)
}

// EndCatchup closes the root logger's startup catch-up window.
func (b *Bound) EndCatchup() {
	if b == nil {
		panic("sidecar logging: EndCatchup called on nil Bound logger")
	}
	b.logger.EndCatchup()
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
	for key, value := range ctx.LevelWindow {
		out[key] = value
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
	// THE CATCH-UP WINDOW LEVELS THE CORPUS WALK. Only an INFO record of a
	// registered operation is affected: a WARN or an ERROR raised during
	// catch-up is a real conclusion and keeps its level.
	if level == "info" && l.tallyCatchup(ctx.Operation) {
		level = "debug"
		verbose = true
	}
	allowed, ended := l.threshold.Allows(level)
	if ended != nil {
		// The window is info now, so this record cannot end it again.
		l.write(false, Context{Operation: levelWindowOperation, Level: "info", LevelWindow: ended.Context()}, "%s", ended.Message())
	}
	if !allowed {
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

		address, attempts, seenServing, err := l.forwardWithRetry(rec)
		if err != nil {
			now := l.now().Local()
			target := record{
				Operation: rec.Operation, WorkspaceDir: rec.WorkspaceDir,
				WorkspaceID: rec.WorkspaceID, ClaudeSessionID: rec.ClaudeSessionID,
			}
			switch {
			case errors.Is(err, ErrForwardWorkspaceUnresolvable):
				// The record named a workspace that a healthy, fully-delivered
				// roster does not contain -- a temp-root or an unknown path
				// that is not a real workspace. It could not be ATTRIBUTED, but
				// the daemon is serving fine, so this is neither an outage nor a
				// boot transient. Narrate it at DEBUG and let writeUndelivered
				// persist the record UNATTRIBUTED in the global sink; never a
				// WARN, and never a deadline wait against a dir that will never
				// appear.
				l.reportForwardTransient(now, address, attempts, target, err,
					"a file-scoped diagnostic named a workspace absent from the daemon's roster; it could not be attributed and was written to the global sink unattributed")
			case errors.Is(err, ErrForwardTargetNotThere):
				// The daemon THIS RECORD targeted is provably gone — its
				// advertised pid died, or daemon.addr now names someone else —
				// mirroring the address-advertisement pid invariant: an absent
				// advertiser costs no WARN, even after it was once seen serving.
				l.reportForwardTransient(now, address, attempts, target, err,
					"a file-scoped diagnostic could not be forwarded because the daemon that would have received it is no longer there (it exited or was replaced); it was written to the global sink")
			case errors.Is(err, ErrForwardTargetBooting):
				// The ADDRESS this record targeted has never once been seen
				// accepting — via Ready or a prior successful Forward — even
				// though it is unchanged and its pid is alive: it is still inside
				// its own boot window. This is a PER-ADDRESS invariant, distinct
				// from this ladder's own seenServing latch below: a different
				// daemon address can have been seen serving earlier without that
				// telling us anything about whether THIS address has come up.
				l.reportForwardTransient(now, address, attempts, target, err,
					"a file-scoped diagnostic was not forwarded because the daemon at this address has never been seen accepting connections; it was written to the global sink")
			case !seenServing:
				// A daemon that never began serving during the ladder is a
				// STARTUP TRANSIENT, not an outage: the record forwarded before
				// its producer published a live address. Narrate it at DEBUG so
				// a boot does not manufacture a WARN, and add the distinguishing
				// record rather than demoting the failure path.
				l.reportForwardTransient(now, address, attempts, target, err,
					"a file-scoped diagnostic was not forwarded because the daemon has not begun serving; it was written to the global sink")
			default:
				// A daemon that WAS serving, whose advertised pid is still
				// alive, and that still failed the forward is a genuine outage,
				// stated once at WARN.
				l.reportForwardFailure(now, address, attempts, target, err)
			}
			// THE DIAGNOSTIC ITSELF IS NOT LOST. The workspace sink is the
			// daemon's and is unreachable, so the record lands in the global
			// durable sink instead, marked as undelivered. Dropping it was the
			// old behavior and it silently swallowed file-plane diagnostics for
			// the whole of a daemon outage.
			l.writeUndelivered(now, address, attempts, rec, err)
			continue
		}
		l.mu.Lock()
		l.lastForwardFailure = ""
		l.lastForwardDeferred = ""
		l.mu.Unlock()
	}
}

// forwardWithRetry climbs the retry ladder, answering the daemon address, how
// many attempts were made, whether the daemon was ever SEEN SERVING across the
// ladder, and the LAST error when every attempt failed.
//
// The ladder exists for the booting daemon: during its boot reconciliation it
// is listening but answering nothing, so the first attempt fails with a
// deadline and the second or third succeeds. A resource forwarded before its
// producer has published is a STARTUP TRANSIENT: each rung first probes
// readiness and does not forward at all until the daemon has published a live
// address. seenServing latches true the first rung the daemon answers a probe
// or a forward, so an outage that begins AFTER the daemon was seen serving is
// still reported as a failure, while a daemon that never came up during the
// ladder is reported as a transient instead.
func (l *Logger) forwardWithRetry(rec ForwardRecord) (address string, attempts int, seenServing bool, err error) {
	backoff := l.forwardBackoffMin
	for attempt := 1; attempt <= l.forwardAttempts; attempt++ {
		if !seenServing {
			if probed, ready := l.forwarder.Ready(); ready {
				seenServing = true
			} else {
				// The daemon has not published a live address. Do not forward
				// prematurely; wait out this rung and re-probe.
				address = probed
				err = fmt.Errorf("daemon at %s has not begun serving", addrOrUnresolved(probed))
				if attempt == l.forwardAttempts {
					return address, attempt, false, err
				}
				if !l.waitBeforeRetry(backoff) {
					return address, attempt, false, err
				}
				backoff = growBackoff(backoff, l.forwardBackoffMax)
				continue
			}
		}
		address, err = l.forwarder.Forward(rec)
		if err == nil {
			return address, attempt, true, nil
		}
		if errors.Is(err, ErrForwardWorkspaceUnresolvable) {
			// A HEALTHY roster that does not name this record's workspace is a
			// permanent condition, not a transport transient: retrying cannot
			// make a temp-root or unknown dir appear in the roster. Abandon the
			// ladder at once so the caller forwards the record unattributed at
			// DEBUG rather than climbing six rungs against a dir that will
			// never resolve. seenServing is reported true because the daemon
			// answered the roster lookup -- it is serving; the workspace, not
			// the daemon, is what could not be resolved.
			return address, attempt, true, err
		}
		if attempt == l.forwardAttempts {
			return address, attempt, true, err
		}
		if !l.waitBeforeRetry(backoff) {
			// Shutdown abandoned the ladder. The record is still accounted for
			// by the caller, which writes it to the durable sink.
			return address, attempt, true, err
		}
		backoff = growBackoff(backoff, l.forwardBackoffMax)
	}
	return address, l.forwardAttempts, seenServing, err
}

// growBackoff doubles a backoff rung, capped at the ladder's maximum.
func growBackoff(backoff, max time.Duration) time.Duration {
	if backoff *= 2; backoff > max {
		return max
	}
	return backoff
}

// addrOrUnresolved names an empty address the way the durable records do, so a
// probe that could not even read daemon.addr still has a stable rate-limit key.
func addrOrUnresolved(address string) string {
	if address == "" {
		return "unresolved"
	}
	return address
}

// waitBeforeRetry sleeps between attempts, answering false when the wait was
// abandoned because the logger is closing.
func (l *Logger) waitBeforeRetry(d time.Duration) bool {
	if l.forwardWait != nil {
		return l.forwardWait(d)
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-timer.C:
		return true
	case <-l.forwardStop:
		return false
	}
}

// writeUndelivered puts a file-scoped record the daemon never accepted into the
// global durable sink, so an outage costs the record its DESTINATION and not
// its existence.
func (l *Logger) writeUndelivered(now time.Time, address string, attempts int, rec ForwardRecord, cause error) {
	if address == "" {
		address = "unresolved"
	}
	context := cloneMap(rec.Context)
	context["forward_undelivered"] = true
	context["daemon_address"] = address
	context["attempt"] = attempts
	context["error"] = cause.Error()
	verbosity := "normal"
	if rec.Verbose {
		verbosity = "verbose"
	}
	l.writeGlobal(record{
		Timestamp: rec.Timestamp, Runtime: "sidecar", PID: rec.PID,
		Level: rec.Level, Verbosity: verbosity, Operation: rec.Operation,
		Message: rec.Message, WorkspaceDir: rec.WorkspaceDir, WorkspaceID: rec.WorkspaceID,
		ClaudeSessionID: rec.ClaudeSessionID, Context: context,
	}, now, rec.Operation)
}

// reportForwardFailure writes one global failure per daemon address and outage
// window, at WARN and carrying the ATTEMPT COUNT — the number says whether the
// destination was merely slow to boot or is genuinely not there, which one
// failure record with no count could never distinguish. A successful forward
// resets the limiter. The original file operation continues: diagnostics
// persistence must never stop transcript ingestion. An empty address means
// resolution itself failed, so the daemon.addr path supplied by the forwarder
// remains the rate-limit key.
func (l *Logger) reportForwardFailure(now time.Time, address string, attempts int, target record, cause error) {
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

	l.writeGlobal(record{
		Timestamp: sharedlogging.Timestamp(now), Runtime: "sidecar", PID: l.pid(),
		Level: "warn", Verbosity: "normal", Operation: "sidecar.logging.forward-failure",
		Message: "a file-scoped diagnostic could not be forwarded to the daemon; it was retried and then written to the global sink",
		Context: map[string]any{
			"daemon_address": address, "error": cause.Error(), "attempt": attempts,
			"target_operation": target.Operation, "target_workspace_dir": target.WorkspaceDir,
			"target_workspace_id": target.WorkspaceID, "target_claude_session_id": target.ClaudeSessionID,
		},
	}, now, target.Operation)
}

// reportForwardTransient narrates a forwarding failure that is NOT a fault at
// DEBUG: either the daemon never published a live address across the whole
// ladder (a startup transient — the record forwarded before its producer was
// serving), or the daemon this record targeted is provably gone (an
// ErrForwardTargetNotThere restart transient — its advertised pid died, or
// daemon.addr now names a replacement). Both are the distinguishing record the
// invariant asks for — added beside the WARN path, never replacing it — so
// neither a boot nor a daemon handover manufactures a forward-failure WARN. It
// is rate-limited per address and outage window like the WARN, and it is
// withheld unless the durable threshold admits DEBUG, so production INFO logs
// stay silent through either window while the undelivered record itself is
// still persisted.
func (l *Logger) reportForwardTransient(now time.Time, address string, attempts int, target record, cause error, message string) {
	allowed, ended := l.threshold.Allows("debug")
	if ended != nil {
		l.write(false, Context{Operation: levelWindowOperation, Level: "info", LevelWindow: ended.Context()}, "%s", ended.Message())
	}
	if !allowed {
		return
	}
	address = addrOrUnresolved(address)
	l.mu.Lock()
	if l.lastForwardDeferred == address {
		l.mu.Unlock()
		return
	}
	l.lastForwardDeferred = address
	l.mu.Unlock()

	l.writeGlobal(record{
		Timestamp: sharedlogging.Timestamp(now), Runtime: "sidecar", PID: l.pid(),
		Level: "debug", Verbosity: "normal", Operation: "sidecar.logging.forward-deferred",
		Message: message,
		Context: map[string]any{
			"daemon_address": address, "error": cause.Error(), "attempt": attempts,
			"target_operation": target.Operation, "target_workspace_dir": target.WorkspaceDir,
			"target_workspace_id": target.WorkspaceID, "target_claude_session_id": target.ClaudeSessionID,
		},
	}, now, target.Operation)
}

// writeGlobal encodes one record into the global durable sink, poisoning the
// logger the same way the ordinary write path does when that sink fails.
func (l *Logger) writeGlobal(rec record, now time.Time, targetOperation string) {
	payload, err := json.Marshal(rec)
	if err != nil {
		panic(fmt.Sprintf("sidecar logging: encode global record: %v", err))
	}
	line := append(payload, '\n')
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.poisoned != nil {
		panic(fmt.Sprintf("sidecar logging: persistent sink previously failed: %v", l.poisoned))
	}
	if err := writeAll(l.file, line); err != nil {
		l.poisoned = err
		l.reportSinkFailure(now, targetOperation, err)
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
	if add.LevelWindow != nil {
		base.LevelWindow = add.LevelWindow
	}
	return base
}
