// Package dlog owns the daemon's durable diagnostic boundary.
package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"agentrepl/logging"
)

const daemonRuntime Runtime = "daemon"

// Stamp renders an instant through logging.Timestamp, the representation
// shared by every agent-repl runtime. It carries a time.Time so callers keep
// the usual comparison helpers.
type Stamp struct{ time.Time }

// NewStamp converts an instant into the shared local-zone representation.
func NewStamp(at time.Time) Stamp { return Stamp{at.Local()} }

// MarshalJSON writes the canonical representation rather than Go's
// variable-precision RFC 3339 default.
func (s Stamp) MarshalJSON() ([]byte, error) {
	return json.Marshal(logging.Timestamp(s.Time))
}

// UnmarshalJSON accepts any RFC 3339 timestamp so records forwarded by other
// runtimes are readable, then normalizes them to the local zone.
func (s *Stamp) UnmarshalJSON(raw []byte) error {
	var text string
	if err := json.Unmarshal(raw, &text); err != nil {
		return fmt.Errorf("dlog: decode timestamp: %w", err)
	}
	at, err := time.Parse(time.RFC3339Nano, text)
	if err != nil {
		return fmt.Errorf("dlog: parse timestamp %q: %w", text, err)
	}
	*s = NewStamp(at)
	return nil
}

// Runtime identifies the runtime which owns a persisted record.
type Runtime string

const (
	RuntimeEmacs   Runtime = "emacs"
	RuntimeDaemon  Runtime = "daemon"
	RuntimeStore   Runtime = "store"
	RuntimeWebapp  Runtime = "webapp"
	RuntimeSidecar Runtime = "sidecar"
	RuntimeShim    Runtime = "shim"
)

// Level is the severity of a record.
type Level string

const (
	LevelDebug Level = "debug"
	LevelInfo  Level = "info"
	LevelWarn  Level = "warn"
	LevelError Level = "error"
)

// Verbosity controls terminal visibility only. Every verbosity reaches durable storage.
type Verbosity string

const (
	Normal  Verbosity = "normal"
	Verbose Verbosity = "verbose"
)

// Workspace identifies the workspace that owns a record.
type Workspace struct {
	Directory string
	ID        string
}

// WorkspaceFromDirectory resolves the same canonical directory and eight-digit
// MD5 identity used by Emacs before any workspace-owned record is routed.
func WorkspaceFromDirectory(directory string) (Workspace, error) {
	if directory == "" {
		return Workspace{}, errors.New("dlog: workspace directory is required")
	}
	canonical, err := filepath.EvalSymlinks(directory)
	if err != nil {
		return Workspace{}, fmt.Errorf("dlog: canonicalize workspace directory: %w", err)
	}
	canonical, err = filepath.Abs(canonical)
	if err != nil {
		return Workspace{}, fmt.Errorf("dlog: absolute workspace directory: %w", err)
	}
	canonical = filepath.Clean(canonical)
	sum := md5.Sum([]byte(canonical))
	return Workspace{Directory: canonical, ID: hex.EncodeToString(sum[:])[:8]}, nil
}

func (w Workspace) validate() error {
	if w.Directory == "" {
		return errors.New("dlog: workspace directory is required")
	}
	if !filepath.IsAbs(w.Directory) {
		return fmt.Errorf("dlog: workspace directory must be absolute: %q", w.Directory)
	}
	if w.ID == "" {
		return errors.New("dlog: workspace ID is required")
	}
	return nil
}

// Scope makes global and workspace routing explicit.
type Scope struct {
	workspace *Workspace
}

// GlobalScope is valid only for records with no conceptual workspace association.
func GlobalScope() Scope { return Scope{} }

// WorkspaceScope rejects malformed workspace attribution before a record can be emitted.
func WorkspaceScope(workspace Workspace) (Scope, error) {
	if err := workspace.validate(); err != nil {
		return Scope{}, err
	}
	return Scope{workspace: &workspace}, nil
}

// Record is the JSONL schema shared by daemon-owned sinks.
type Record struct {
	Timestamp          Stamp          `json:"timestamp"`
	Runtime            Runtime        `json:"runtime"`
	Level              Level          `json:"level"`
	Verbosity          Verbosity      `json:"verbosity"`
	Operation          string         `json:"operation"`
	Message            string         `json:"message"`
	Context            map[string]any `json:"context"`
	PID                int            `json:"pid,omitempty"`
	ConnectionID       string         `json:"connection_id,omitempty"`
	WorkspaceDirectory string         `json:"workspace_dir,omitempty"`
	WorkspaceID        string         `json:"workspace_id,omitempty"`
	AgentReplSessionID string         `json:"agent_repl_session_id,omitempty"`
	ClaudeSessionID    string         `json:"claude_session_id,omitempty"`
	RequestID          string         `json:"request_id,omitempty"`
}

// Event is the caller-supplied, typed portion of a record.
type Event struct {
	Runtime            Runtime
	Level              Level
	Operation          string
	Message            string
	Context            map[string]any
	AgentReplSessionID string
	ClaudeSessionID    string
	RequestID          string
	// PID preserves the originating process for forwarded OS-process records.
	// Zero means this daemon process.
	PID          int
	ConnectionID string
}

// ForwardedIdentity is the daemon-authoritative identity attached to a source
// record before it is persisted in a workspace runtime sink.
type ForwardedIdentity struct {
	AgentReplSessionID string
	ClaudeSessionID    string
	RequestID          string
}

func (e Event) record(scope Scope, verbosity Verbosity) (Record, error) {
	switch e.Runtime {
	case RuntimeEmacs, RuntimeDaemon, RuntimeShim, RuntimeWebapp, RuntimeSidecar, RuntimeStore:
	default:
		return Record{}, fmt.Errorf("dlog: unsupported runtime: %q", e.Runtime)
	}
	switch e.Level {
	case LevelDebug, LevelInfo, LevelWarn, LevelError:
	default:
		return Record{}, fmt.Errorf("dlog: unsupported level: %q", e.Level)
	}
	if e.Operation == "" {
		return Record{}, errors.New("dlog: operation is required")
	}
	if e.Message == "" {
		return Record{}, errors.New("dlog: message is required")
	}
	if e.Context == nil {
		return Record{}, errors.New("dlog: context is required")
	}
	record := Record{Timestamp: NewStamp(time.Now()), Runtime: e.Runtime, Level: e.Level, Verbosity: verbosity, Operation: e.Operation, Message: e.Message, Context: e.Context, AgentReplSessionID: e.AgentReplSessionID, ClaudeSessionID: e.ClaudeSessionID, RequestID: e.RequestID}
	if e.Runtime == RuntimeWebapp {
		if e.ConnectionID == "" {
			return Record{}, errors.New("dlog: webapp connection ID is required")
		}
		if e.PID != 0 {
			return Record{}, errors.New("dlog: webapp records must not include PID")
		}
		record.ConnectionID = e.ConnectionID
	} else {
		if e.ConnectionID != "" {
			return Record{}, errors.New("dlog: connection ID is only valid for webapp records")
		}
		record.PID = e.PID
		if record.PID == 0 {
			record.PID = os.Getpid()
		}
		if record.PID < 0 {
			return Record{}, fmt.Errorf("dlog: invalid PID: %d", record.PID)
		}
	}
	if scope.workspace != nil {
		record.WorkspaceDirectory = scope.workspace.Directory
		record.WorkspaceID = scope.workspace.ID
	}
	return record, nil
}

// Logger writes normal records to terminal and durable storage. Verbose records
// always reach durable storage and reach terminal only when enabled.
type Logger struct {
	sink            *durableSink
	terminal        io.Writer
	verboseTerminal bool
	legacyContext   []any
}

type durableSink struct {
	writer io.Writer
	mu     sync.Mutex
	poison error
}

// New preserves the legacy daemon wiring while its call sites migrate to Emit.
// New typed code must use EmitNormal or EmitVerbose with an explicit Scope.
func New(durable, terminal io.Writer, verboseTerminal bool) *Logger {
	if durable == nil {
		panic("dlog: durable writer is required")
	}
	if terminal == nil {
		panic("dlog: terminal writer is required")
	}
	return newLogger(&durableSink{writer: durable}, terminal, verboseTerminal)
}

func newLogger(sink *durableSink, terminal io.Writer, verboseTerminal bool) *Logger {
	return &Logger{sink: sink, terminal: terminal, verboseTerminal: verboseTerminal}
}

// EmitNormal persists an event and writes it to the terminal.
func (l *Logger) EmitNormal(scope Scope, event Event) error { return l.emit(scope, Normal, event) }

// EmitVerbose persists an event and writes it to the terminal only when enabled.
func (l *Logger) EmitVerbose(scope Scope, event Event) error { return l.emit(scope, Verbose, event) }

// EmitWorkspaceNormal cannot route a workspace-owned event to a global sink.
func (l *Logger) EmitWorkspaceNormal(workspace Workspace, event Event) error {
	scope, err := WorkspaceScope(workspace)
	if err != nil {
		return fmt.Errorf("dlog: workspace routing failure: %w", err)
	}
	return l.EmitNormal(scope, event)
}

// EmitWorkspaceVerbose cannot route a workspace-owned event to a global sink.
func (l *Logger) EmitWorkspaceVerbose(workspace Workspace, event Event) error {
	scope, err := WorkspaceScope(workspace)
	if err != nil {
		return fmt.Errorf("dlog: workspace routing failure: %w", err)
	}
	return l.EmitVerbose(scope, event)
}

func (l *Logger) emit(scope Scope, verbosity Verbosity, event Event) error {
	if l == nil {
		return errors.New("dlog: emit called on nil Logger")
	}
	record, err := event.record(scope, verbosity)
	if err != nil {
		return err
	}
	return l.persist(record)
}

// PersistForwarded writes an already timestamped source record after proving it
// is a canonical record for runtime and attaching daemon-authoritative routing
// and identity. It never routes a workspace-owned source record globally.
func (l *Logger) PersistForwarded(workspace Workspace, runtime Runtime, record Record, identity ForwardedIdentity) error {
	if l == nil {
		return errors.New("dlog: PersistForwarded called on nil Logger")
	}
	if err := validateForwardedRecord(runtime, record); err != nil {
		return err
	}
	if err := workspace.validate(); err != nil {
		return fmt.Errorf("dlog: workspace routing failure: %w", err)
	}
	record.WorkspaceDirectory = workspace.Directory
	record.WorkspaceID = workspace.ID
	if identity.AgentReplSessionID != "" {
		record.AgentReplSessionID = identity.AgentReplSessionID
	}
	if identity.ClaudeSessionID != "" {
		record.ClaudeSessionID = identity.ClaudeSessionID
	}
	if identity.RequestID != "" {
		record.RequestID = identity.RequestID
	}
	return l.persist(record)
}

func validateForwardedRecord(runtime Runtime, record Record) error {
	if record.Timestamp.IsZero() {
		return errors.New("dlog: forwarded record timestamp is required")
	}
	if record.Runtime != runtime {
		return fmt.Errorf("dlog: forwarded record runtime %q does not match sink %q", record.Runtime, runtime)
	}
	if record.Verbosity != Normal && record.Verbosity != Verbose {
		return fmt.Errorf("dlog: forwarded record has invalid verbosity %q", record.Verbosity)
	}
	if _, err := (Event{Runtime: record.Runtime, Level: record.Level, Operation: record.Operation, Message: record.Message, Context: record.Context, PID: record.PID, ConnectionID: record.ConnectionID}).record(GlobalScope(), record.Verbosity); err != nil {
		return fmt.Errorf("dlog: invalid forwarded record: %w", err)
	}
	return nil
}

// ParseForwardedRecord accepts exactly one canonical JSON record. Callers use
// it at untrusted transport boundaries before applying daemon attribution.
func ParseForwardedRecord(raw []byte) (Record, error) {
	decoder := json.NewDecoder(strings.NewReader(string(raw)))
	decoder.DisallowUnknownFields()
	var record Record
	if err := decoder.Decode(&record); err != nil {
		return Record{}, fmt.Errorf("dlog: decode forwarded record: %w", err)
	}
	var trailing any
	if err := decoder.Decode(&trailing); !errors.Is(err, io.EOF) {
		if err == nil {
			return Record{}, errors.New("dlog: forwarded record has trailing JSON value")
		}
		return Record{}, fmt.Errorf("dlog: decode forwarded record trailing data: %w", err)
	}
	return record, nil
}

func (l *Logger) persist(record Record) error {
	line, err := json.Marshal(record)
	if err != nil {
		return fmt.Errorf("dlog: encode record: %w", err)
	}
	line = append(line, '\n')
	l.sink.mu.Lock()
	defer l.sink.mu.Unlock()
	if l.sink.poison != nil {
		err := fmt.Errorf("dlog: durable sink poisoned by prior write failure: %w", l.sink.poison)
		emergency, encodeErr := sinkEmergencyLine(record, err)
		if encodeErr != nil {
			return fmt.Errorf("%w; encode emergency record: %v", err, encodeErr)
		}
		if terminalErr := writeFull(l.terminal, emergency); terminalErr != nil {
			return fmt.Errorf("%w; emergency terminal write also failed: %v", err, terminalErr)
		}
		return err
	}
	if err := writeFull(l.sink.writer, line); err != nil {
		l.sink.poison = err
		emergency, encodeErr := sinkEmergencyLine(record, err)
		if encodeErr != nil {
			return fmt.Errorf("dlog: durable sink write failed: %w; encode emergency record: %v", err, encodeErr)
		}
		if terminalErr := writeFull(l.terminal, emergency); terminalErr != nil {
			return fmt.Errorf("dlog: durable sink write failed: %w; emergency terminal write also failed: %v", err, terminalErr)
		}
		return fmt.Errorf("dlog: durable sink write failed: %w", err)
	}
	if record.Verbosity == Normal || l.verboseTerminal {
		if err := writeFull(l.terminal, line); err != nil {
			return fmt.Errorf("dlog: terminal sink write failed: %w", err)
		}
	}
	return nil
}

func sinkEmergencyLine(failed Record, sinkErr error) ([]byte, error) {
	emergency := Record{
		Timestamp:          NewStamp(time.Now()),
		Runtime:            RuntimeDaemon,
		Level:              LevelError,
		Verbosity:          Normal,
		Operation:          "daemon.logging.sink-failure",
		Message:            "durable log sink write failed",
		Context:            map[string]any{"error": sinkErr.Error(), "target_runtime": failed.Runtime, "target_operation": failed.Operation},
		PID:                os.Getpid(),
		WorkspaceDirectory: failed.WorkspaceDirectory,
		WorkspaceID:        failed.WorkspaceID,
		AgentReplSessionID: failed.AgentReplSessionID,
		ClaudeSessionID:    failed.ClaudeSessionID,
		RequestID:          failed.RequestID,
	}
	line, err := json.Marshal(emergency)
	if err != nil {
		return nil, err
	}
	return append(line, '\n'), nil
}

func writeFull(writer io.Writer, line []byte) error {
	for len(line) > 0 {
		n, err := writer.Write(line)
		if n < 0 || n > len(line) {
			return fmt.Errorf("dlog: invalid write count: %d", n)
		}
		if n == 0 && err == nil {
			return errors.New("dlog: writer made no progress")
		}
		line = line[n:]
		if err != nil {
			return err
		}
	}
	return nil
}

// With is a compatibility adapter for the Wave 2 call-site migration.
func (l *Logger) With(kv ...any) *Logger {
	if l == nil {
		panic("dlog: With called on nil Logger")
	}
	bound := append(append([]any{}, l.legacyContext...), kv...)
	return &Logger{sink: l.sink, terminal: l.terminal, verboseTerminal: l.verboseTerminal, legacyContext: bound}
}

// Log is the legacy normal-emission adapter. New code uses EmitNormal.
func (l *Logger) Log(format string, args ...any) { l.legacyEmit(Normal, format, args...) }

// LogVerbose is the legacy verbose-emission adapter. New code uses EmitVerbose.
func (l *Logger) LogVerbose(format string, args ...any) { l.legacyEmit(Verbose, format, args...) }

func (l *Logger) legacyEmit(verbosity Verbosity, format string, args ...any) {
	if l == nil {
		panic("dlog: log called on nil Logger")
	}
	context := pairsContext(l.legacyContext)
	operation, _ := context["operation"].(string)
	if operation == "" {
		operation = "daemon.legacy"
	}
	event := Event{Runtime: daemonRuntime, Level: LevelInfo, Operation: operation, Message: fmt.Sprintf(format, args...), Context: context}
	var err error
	if verbosity == Normal {
		err = l.EmitNormal(GlobalScope(), event)
	} else {
		err = l.EmitVerbose(GlobalScope(), event)
	}
	if err != nil {
		panic(err)
	}
}

func pairsContext(kv []any) map[string]any {
	context := make(map[string]any, (len(kv)+1)/2)
	for i := 0; i < len(kv); i += 2 {
		key := fmt.Sprint(kv[i])
		if i+1 < len(kv) {
			context[key] = kv[i+1]
		} else {
			context[key] = "!MISSING"
		}
	}
	return context
}

// Logf is the temporary callback adapter for unmigrated dependency injection.
type Logf func(format string, args ...any)

func Legacy(l *Logger) Logf {
	if l == nil {
		panic("dlog: Legacy requires Logger")
	}
	return l.Log
}

const OutputClampLen = 2000

func Clamp(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + " …[clamped]"
}

func kvBlock(kv []any) string {
	var b strings.Builder
	for i := 0; i < len(kv); i += 2 {
		if b.Len() > 0 {
			b.WriteByte(' ')
		}
		if i+1 < len(kv) {
			fmt.Fprintf(&b, "%v=%v", kv[i], kv[i+1])
		} else {
			fmt.Fprintf(&b, "%v=!MISSING", kv[i])
		}
	}
	return b.String()
}

func Tag(logf Logf, kv ...any) Logf {
	block := kvBlock(kv)
	if block == "" {
		return logf
	}
	return func(format string, args ...any) { logf("%s {%s}", fmt.Sprintf(format, args...), block) }
}
func TagFunc(logf Logf, tags func() []any) Logf {
	return func(format string, args ...any) {
		block := kvBlock(tags())
		if block == "" {
			logf(format, args...)
			return
		}
		logf("%s {%s}", fmt.Sprintf(format, args...), block)
	}
}
func Call(logf Logf, name string, kv ...any) func(output string, err error) {
	block := kvBlock(kv)
	suffix := ""
	if block != "" {
		suffix = " {" + block + "}"
	}
	logf("%s: call start%s", name, suffix)
	start := time.Now()
	return func(output string, err error) {
		elapsed := time.Since(start).Round(time.Millisecond)
		if err != nil {
			logf("%s: call FAILED after %s%s: %v (output: %s)", name, elapsed, suffix, err, Clamp(output, OutputClampLen))
			return
		}
		logf("%s: call ok in %s%s: %s", name, elapsed, suffix, Clamp(output, OutputClampLen))
	}
}

// TargetManager owns daemon workspace log targets. The canonical workspace path
// is always a symlink to a runtime-created external temporary file.
type activeTarget struct {
	workspace Workspace
	runtime   Runtime
	file      *os.File
	writer    *workspaceTargetWriter
	sink      *durableSink
}

type TargetManager struct {
	mu       sync.Mutex
	targets  map[string]activeTarget
	capBytes int64
}

// WorkspaceRuntimeCapBytes bounds each daemon-owned workspace runtime target.
// Targets are cleared in place, preserving the canonical symlink and inode.
const WorkspaceRuntimeCapBytes int64 = 64 << 20

// WorkspaceRuntimeCapInterval controls the daemon's scan for direct shim
// writes, which bypass daemon-side Logger handles through inherited fd 3.
const WorkspaceRuntimeCapInterval = 30 * time.Second

func NewTargetManager() *TargetManager {
	return &TargetManager{targets: make(map[string]activeTarget), capBytes: WorkspaceRuntimeCapBytes}
}

// NewTargetManagerWithCap constructs a manager with an explicit positive cap.
// Production uses NewTargetManager; tests and specialized embedders may use a
// smaller boundary without writing WorkspaceRuntimeCapBytes.
func NewTargetManagerWithCap(capBytes int64) (*TargetManager, error) {
	if capBytes <= 0 {
		return nil, errors.New("dlog: workspace runtime cap must be positive")
	}
	return &TargetManager{targets: make(map[string]activeTarget), capBytes: capBytes}, nil
}

type targetFile interface {
	io.Writer
	Stat() (os.FileInfo, error)
	Truncate(size int64) error
}

// workspaceTargetWriter serializes cap enforcement with daemon-owned writes.
// The underlying file is opened O_APPEND, so a truncate never leaves any
// writer, including an inherited shim descriptor, with a stale offset.
type workspaceTargetWriter struct {
	file      targetFile
	link      string
	capBytes  int64
	workspace Workspace
	runtime   Runtime
}

func (w *workspaceTargetWriter) Write(p []byte) (int, error) {
	if err := w.enforceCap(int64(len(p))); err != nil {
		return 0, err
	}
	total := 0
	for total < len(p) {
		n, err := w.file.Write(p[total:])
		if n < 0 || n > len(p)-total {
			return total, fmt.Errorf("dlog: target writer reported invalid write count %d", n)
		}
		total += n
		if err != nil {
			return total, err
		}
		if n == 0 {
			return total, errors.New("dlog: target writer made no progress")
		}
	}
	return total, nil
}

func (w *workspaceTargetWriter) enforceCap(incoming int64) error {
	info, err := w.file.Stat()
	if err != nil {
		return fmt.Errorf("dlog: inspect %s target size: %w", w.runtime, err)
	}
	if info.Size() < w.capBytes && incoming <= w.capBytes-info.Size() {
		return nil
	}
	return w.truncateInPlace()
}

func (w *workspaceTargetWriter) truncateInPlace() error {
	linkInfo, err := os.Lstat(w.link)
	if err != nil {
		return fmt.Errorf("dlog: inspect canonical %s target link: %w", w.runtime, err)
	}
	if linkInfo.Mode()&os.ModeSymlink == 0 {
		return fmt.Errorf("dlog: canonical %s target is not a symlink: %s", w.runtime, w.link)
	}
	linked, err := os.Stat(w.link)
	if err != nil {
		return fmt.Errorf("dlog: resolve canonical %s target link: %w", w.runtime, err)
	}
	owned, err := w.file.Stat()
	if err != nil {
		return fmt.Errorf("dlog: inspect active %s target: %w", w.runtime, err)
	}
	if !os.SameFile(linked, owned) {
		return fmt.Errorf("dlog: canonical %s target link does not name active inode: %s", w.runtime, w.link)
	}
	if err := w.file.Truncate(0); err != nil {
		return fmt.Errorf("dlog: truncate active %s target: %w", w.runtime, err)
	}
	return nil
}

// OpenWorkspace creates the daemon target and atomically installs daemon.log.
// Reopening returns the manager-owned open target without changing its contents.
func (m *TargetManager) OpenWorkspace(workspace Workspace) (*os.File, error) {
	return m.OpenWorkspaceRuntime(workspace, RuntimeDaemon)
}

// OpenWorkspaceRuntime creates a target and atomically installs <runtime>.log.
// Runtime-specific keys keep daemon and shim writers distinct for one workspace.
func (m *TargetManager) OpenWorkspaceRuntime(workspace Workspace, runtime Runtime) (*os.File, error) {
	if m == nil {
		return nil, errors.New("dlog: target manager is required")
	}
	if err := workspace.validate(); err != nil {
		return nil, err
	}
	if runtime != RuntimeDaemon && runtime != RuntimeShim && runtime != RuntimeWebapp && runtime != RuntimeSidecar {
		return nil, fmt.Errorf("dlog: unsupported workspace target runtime: %q", runtime)
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	key := targetKey(workspace, runtime)
	if active, ok := m.targets[key]; ok {
		if active.workspace.ID != workspace.ID || active.runtime != runtime {
			return nil, fmt.Errorf("dlog: active target workspace ID mismatch: active=%q requested=%q", active.workspace.ID, workspace.ID)
		}
		// A retained target that cannot be stat-ed is a target somebody CLOSED
		// out from under the manager — the double-use the borrow type exists to
		// prevent. It is refused with a diagnostic that names it rather than
		// reissued, because handing it back would put a dead descriptor into
		// the next child's fd 3.
		if _, err := active.file.Stat(); err != nil {
			return nil, fmt.Errorf("dlog: the active %s target for workspace %q is CLOSED (%s): %w",
				runtime, workspace.Directory, active.file.Name(), err)
		}
		return active.file, nil
	}
	directory, err := daemonLogDirectory(workspace.Directory)
	if err != nil {
		return nil, err
	}
	reservation, err := os.CreateTemp("", "agent-repl-"+string(runtime)+"-*.log")
	if err != nil {
		return nil, fmt.Errorf("dlog: create external target: %w", err)
	}
	targetPath := reservation.Name()
	if err := reservation.Close(); err != nil {
		os.Remove(targetPath)
		return nil, fmt.Errorf("dlog: close external target reservation: %w", err)
	}
	target, err := os.OpenFile(targetPath, os.O_WRONLY|os.O_APPEND, 0600)
	if err != nil {
		os.Remove(targetPath)
		return nil, fmt.Errorf("dlog: reopen external target O_APPEND: %w", err)
	}
	name := string(runtime) + ".log"
	link := filepath.Join(directory, name)
	stage, err := os.CreateTemp(directory, "."+name+"-stage-*")
	if err != nil {
		target.Close()
		os.Remove(target.Name())
		return nil, fmt.Errorf("dlog: reserve replacement symlink name: %w", err)
	}
	temporaryLink := stage.Name()
	if err := stage.Close(); err != nil {
		os.Remove(temporaryLink)
		target.Close()
		os.Remove(target.Name())
		return nil, fmt.Errorf("dlog: close replacement symlink reservation: %w", err)
	}
	if err := os.Remove(temporaryLink); err != nil {
		target.Close()
		os.Remove(target.Name())
		return nil, fmt.Errorf("dlog: clear replacement symlink reservation: %w", err)
	}
	if err := os.Symlink(target.Name(), temporaryLink); err != nil {
		target.Close()
		os.Remove(target.Name())
		return nil, fmt.Errorf("dlog: create replacement symlink: %w", err)
	}
	if err := os.Rename(temporaryLink, link); err != nil {
		os.Remove(temporaryLink)
		target.Close()
		os.Remove(target.Name())
		return nil, fmt.Errorf("dlog: atomically install daemon log symlink: %w", err)
	}
	writer := &workspaceTargetWriter{
		file: target, link: link, capBytes: m.capBytes, workspace: workspace, runtime: runtime,
	}
	m.targets[key] = activeTarget{
		workspace: workspace, runtime: runtime, file: target, writer: writer,
		sink: &durableSink{writer: writer},
	}
	return target, nil
}

// OpenWorkspaceLogger returns a fresh logger handle backed by the one
// manager-owned daemon sink for this workspace. Every handle shares ordering
// and poison state, including handles created for different sessions.
func (m *TargetManager) OpenWorkspaceLogger(workspace Workspace, terminal io.Writer, verboseTerminal bool) (*Logger, error) {
	return m.OpenWorkspaceRuntimeLogger(workspace, RuntimeDaemon, terminal, verboseTerminal)
}

// OpenWorkspaceRuntimeLogger returns a fresh handle over the shared
// poison-aware sink for one workspace runtime.
func (m *TargetManager) OpenWorkspaceRuntimeLogger(workspace Workspace, runtime Runtime, terminal io.Writer, verboseTerminal bool) (*Logger, error) {
	if terminal == nil {
		return nil, errors.New("dlog: terminal writer is required")
	}
	if _, err := m.OpenWorkspaceRuntime(workspace, runtime); err != nil {
		return nil, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	active, ok := m.targets[targetKey(workspace, runtime)]
	if !ok || active.sink == nil {
		return nil, fmt.Errorf("dlog: %s target disappeared for workspace %q", runtime, workspace.Directory)
	}
	return newLogger(active.sink, terminal, verboseTerminal), nil
}

func targetKey(workspace Workspace, runtime Runtime) string {
	return string(runtime) + "\x00" + workspace.Directory
}

// TruncateWorkspace clears one manager-owned target in place for cap management.
func (m *TargetManager) TruncateWorkspace(workspace Workspace) error {
	return m.TruncateWorkspaceRuntime(workspace, RuntimeDaemon)
}

// TruncateWorkspaceRuntime clears exactly one runtime target after proving the
// canonical symlink still names the manager-owned inode.
func (m *TargetManager) TruncateWorkspaceRuntime(workspace Workspace, runtime Runtime) error {
	if m == nil {
		return errors.New("dlog: target manager is required")
	}
	if err := workspace.validate(); err != nil {
		return err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	active, ok := m.targets[targetKey(workspace, runtime)]
	if !ok {
		return fmt.Errorf("dlog: no active target for workspace %q", workspace.Directory)
	}
	if active.workspace.ID != workspace.ID {
		return fmt.Errorf("dlog: active target workspace ID mismatch: active=%q requested=%q", active.workspace.ID, workspace.ID)
	}
	active.sink.mu.Lock()
	defer active.sink.mu.Unlock()
	return active.writer.truncateInPlace()
}

// TargetCapError identifies the workspace runtime whose cap could not be
// enforced. Callers must persist it through that workspace's daemon logger.
type TargetCapError struct {
	Workspace Workspace
	Runtime   Runtime
	Err       error
}

func (e TargetCapError) Error() string {
	return fmt.Sprintf("dlog: maintain %s target cap for workspace %q: %v", e.Runtime, e.Workspace.Directory, e.Err)
}

func (e TargetCapError) Unwrap() error { return e.Err }

// MaintainSizeCaps checks every active target. This catches direct writes made
// by inherited shim descriptors as well as daemon-owned webapp and sidecar
// writers. Each target is truncated in place under its shared sink lock.
func (m *TargetManager) MaintainSizeCaps() []TargetCapError {
	if m == nil {
		return []TargetCapError{{Err: errors.New("dlog: target manager is required")}}
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var failures []TargetCapError
	for _, active := range m.targets {
		active.sink.mu.Lock()
		if active.sink.poison != nil {
			active.sink.mu.Unlock()
			continue
		}
		err := active.writer.enforceCap(0)
		if err != nil {
			active.sink.poison = err
		}
		active.sink.mu.Unlock()
		if err != nil {
			failures = append(failures, TargetCapError{
				Workspace: active.workspace, Runtime: active.runtime, Err: err,
			})
		}
	}
	return failures
}

// ReportTargetCapError records a maintenance failure in the affected
// workspace's daemon target. If that target cannot be opened, the same
// canonical record is emitted as JSON emergency output to terminal only.
func (m *TargetManager) ReportTargetCapError(failure TargetCapError, terminal io.Writer, verboseTerminal bool) error {
	if m == nil {
		return errors.New("dlog: target manager is required")
	}
	if terminal == nil {
		return errors.New("dlog: terminal writer is required")
	}
	if failure.Err == nil {
		return errors.New("dlog: target cap failure error is required")
	}
	event := Event{
		Runtime:   RuntimeDaemon,
		Level:     LevelError,
		Operation: "daemon.logging.workspace-cap-failed",
		Message:   "workspace runtime log size maintenance failed",
		Context: map[string]any{
			"error":          failure.Err.Error(),
			"target_runtime": failure.Runtime,
			"cap_bytes":      m.capBytes,
		},
	}
	logger, err := m.OpenWorkspaceLogger(failure.Workspace, terminal, verboseTerminal)
	if err == nil {
		return logger.EmitWorkspaceNormal(failure.Workspace, event)
	}
	event.Context["workspace_logger_error"] = err.Error()
	record, recordErr := event.record(Scope{workspace: &failure.Workspace}, Normal)
	if recordErr != nil {
		return errors.Join(err, recordErr)
	}
	line, encodeErr := json.Marshal(record)
	if encodeErr != nil {
		return errors.Join(err, encodeErr)
	}
	if terminalErr := writeFull(terminal, append(line, '\n')); terminalErr != nil {
		return errors.Join(err, terminalErr)
	}
	return err
}

// ActiveTargets is how many targets the manager currently holds open. It is the
// GAUGE behind the borrow's lifetime: targets are opened per workspace runtime
// and released when the workspace closes, so a count that only ever climbs is
// the leak this number exists to make visible.
func (m *TargetManager) ActiveTargets() int {
	if m == nil {
		return 0
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return len(m.targets)
}

// EvictWorkspace closes and forgets EVERY runtime target of one workspace.
//
// THE TARGET'S LIFETIME IS THE WORKSPACE'S. Before this, a target lived from
// its first write until the daemon exited: a workspace closed in the morning
// still held its descriptors at midnight, and a long-lived daemon accumulated
// one set per workspace it had ever touched. The workspace-close path calls
// this, which binds the two lifetimes rather than trusting a caller to remember.
//
// It closes the descriptors and forgets the entries; the canonical symlinks and
// the external files they name are LEFT IN PLACE, exactly as Close leaves them,
// because a closed workspace's log is still the record of what happened there.
// A later reopen of the same workspace opens a fresh target through the
// ordinary path.
//
// Close failures are collected and returned together rather than aborting on
// the first: every target of the workspace is released either way, and the
// caller learns about all of them.
func (m *TargetManager) EvictWorkspace(workspace Workspace) (int, error) {
	if m == nil {
		return 0, errors.New("dlog: target manager is required")
	}
	if err := workspace.validate(); err != nil {
		return 0, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var errs []error
	evicted := 0
	for key, active := range m.targets {
		if active.workspace.Directory != workspace.Directory {
			continue
		}
		if err := active.file.Close(); err != nil {
			errs = append(errs, fmt.Errorf("dlog: close target %q: %w", key, err))
		}
		delete(m.targets, key)
		evicted++
	}
	return evicted, errors.Join(errs...)
}

// Close forgets all manager-owned targets without unlinking their canonical
// workspace symlinks or deleting the external targets those links reference.
func (m *TargetManager) Close() error {
	if m == nil {
		return errors.New("dlog: target manager is required")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var errs []error
	for key, active := range m.targets {
		if err := active.file.Close(); err != nil {
			errs = append(errs, fmt.Errorf("dlog: close target %q: %w", key, err))
		}
		delete(m.targets, key)
	}
	return errors.Join(errs...)
}

func daemonLogDirectory(workspaceDirectory string) (string, error) {
	claude := filepath.Join(workspaceDirectory, ".claude")
	if err := ensureDirectory(claude); err != nil {
		return "", fmt.Errorf("dlog: prepare .claude directory: %w", err)
	}
	emacs := filepath.Join(claude, "emacs")
	if err := ensureDirectory(emacs); err != nil {
		return "", fmt.Errorf("dlog: prepare .claude/emacs directory: %w", err)
	}
	return emacs, nil
}

func ensureDirectory(path string) error {
	info, err := os.Lstat(path)
	if errors.Is(err, os.ErrNotExist) {
		if err := os.Mkdir(path, 0700); err != nil {
			return fmt.Errorf("create directory: %w", err)
		}
		info, err = os.Lstat(path)
	}
	if err != nil {
		return fmt.Errorf("inspect directory: %w", err)
	}
	if info.Mode()&os.ModeSymlink != 0 {
		return fmt.Errorf("refusing symlinked directory: %s", path)
	}
	if !info.IsDir() {
		return fmt.Errorf("refusing non-directory: %s", path)
	}
	return nil
}
