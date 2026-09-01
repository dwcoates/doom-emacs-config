package dlog

import (
	"encoding/json"
	"fmt"
	"time"

	"agentrepl/logging"
)

// The runtime names the logging contract allows. The daemon writes its own
// records as RuntimeDaemon and a forwarded client's records under that
// client's own runtime, so a webapp line in webapp.log still says webapp.
const (
	RuntimeDaemon  = "daemon"
	RuntimeShim    = "shim"
	RuntimeWebapp  = "webapp"
	RuntimeSidecar = "sidecar"
)

// The four levels of the logging contract.
const (
	LevelDebug = "debug"
	LevelInfo  = "info"
	LevelWarn  = "warn"
	LevelError = "error"
)

// The two verbosity classes of the logging contract.
const (
	VerbosityNormal  = "normal"
	VerbosityVerbose = "verbose"
)

// The reserved context keys. A caller carries identity by putting these into
// the context it binds with With; they are promoted out of the context object
// into their dedicated top-level fields, because the contract requires
// identifiers to live in their own fields and never only inside context or
// message.
const (
	KeyWorkspaceDir       = "workspace_dir"
	KeyWorkspaceID        = "workspace_id"
	KeyAgentReplSessionID = "agent_repl_session_id"
	KeyClaudeSessionID    = "claude_session_id"
	KeyRequestID          = "request_id"
)

// reservedKeys is the promotion order, which is also the order the fields
// appear in a record.
var reservedKeys = []string{
	KeyWorkspaceDir,
	KeyWorkspaceID,
	KeyAgentReplSessionID,
	KeyClaudeSessionID,
	KeyRequestID,
}

// record is one JSONL line. The field order is the struct order, which is the
// order logging-contract.md lists them; a reader diffing two records from two
// runtimes sees the same shape in the same order.
type record struct {
	Timestamp string  `json:"timestamp"`
	Runtime   string  `json:"runtime"`
	Level     string  `json:"level"`
	Verbosity string  `json:"verbosity"`
	Operation string  `json:"operation"`
	Message   string  `json:"message"`
	Context   Context `json:"context"`
	// PID is required of every OS-process runtime and is omitted only where
	// the contract says it must be: a record FORWARDED from another runtime
	// carries that runtime's own identity (the browser webapp sends
	// connection_id, the sidecar sends its own pid, both inside context), and
	// stamping the daemon's pid on it would attribute it to the wrong process.
	PID int `json:"pid,omitempty"`

	WorkspaceDir       string `json:"workspace_dir,omitempty"`
	WorkspaceID        string `json:"workspace_id,omitempty"`
	AgentReplSessionID string `json:"agent_repl_session_id,omitempty"`
	ClaudeSessionID    string `json:"claude_session_id,omitempty"`
	RequestID          string `json:"request_id,omitempty"`
}

// verbosityFor maps a level onto the contract's verbosity class. The Logger
// interface fixes four level methods and no separate verbose emitters, so the
// mapping is the level itself: the ordinary-path DEBUG record is the verbose
// class, and everything a reader would want without asking is normal. The
// verbose class still persists; the daemon's verbose setting gates only the
// terminal mirror.
func verbosityFor(level string) string {
	if level == LevelDebug {
		return VerbosityVerbose
	}
	return VerbosityNormal
}

// promote splits a context into the reserved identity fields and the context
// object that remains. A reserved key holding a non-string value is rendered
// with fmt.Sprint rather than dropped, so the field stays typed as the
// contract requires and the value is never lost.
func promote(ctx Context) (Context, map[string]string) {
	ids := make(map[string]string, len(reservedKeys))
	var rest Context
	for k, v := range ctx {
		if isReserved(k) {
			if s := stringify(v); s != "" {
				ids[k] = s
			}
			continue
		}
		if rest == nil {
			rest = make(Context, len(ctx))
		}
		rest[k] = v
	}
	return rest, ids
}

func isReserved(key string) bool {
	for _, k := range reservedKeys {
		if k == key {
			return true
		}
	}
	return false
}

func stringify(v any) string {
	if v == nil {
		return ""
	}
	if s, ok := v.(string); ok {
		return s
	}
	return fmt.Sprint(v)
}

// newRecord builds a record from an already-merged context.
func newRecord(at time.Time, runtime, level, operation, message string, ctx Context, pid int) record {
	rest, ids := promote(ctx)
	if rest == nil {
		// context is a required field and must be an object, never null.
		rest = Context{}
	}
	return record{
		Timestamp:          logging.Timestamp(at),
		Runtime:            runtime,
		Level:              level,
		Verbosity:          verbosityFor(level),
		Operation:          operation,
		Message:            message,
		Context:            rest,
		PID:                pid,
		WorkspaceDir:       ids[KeyWorkspaceDir],
		WorkspaceID:        ids[KeyWorkspaceID],
		AgentReplSessionID: ids[KeyAgentReplSessionID],
		ClaudeSessionID:    ids[KeyClaudeSessionID],
		RequestID:          ids[KeyRequestID],
	}
}

// marshal renders the record as one JSONL line, terminator included. A
// context value that cannot be encoded is replaced by its rendered form
// rather than costing the whole record, because a record lost to an
// unencodable context value is evidence destroyed by its own diagnostics.
func (r record) marshal() []byte {
	line, err := json.Marshal(r)
	if err != nil {
		r.Context = renderable(r.Context)
		line, err = json.Marshal(r)
		if err != nil {
			// Nothing but the shape is left to preserve.
			r.Context = Context{"context_encoding_error": err.Error()}
			line, _ = json.Marshal(r)
		}
	}
	return append(line, '\n')
}

// renderable rewrites every value that json cannot encode into its printed
// form.
func renderable(ctx Context) Context {
	out := make(Context, len(ctx))
	for k, v := range ctx {
		if _, err := json.Marshal(v); err != nil {
			out[k] = fmt.Sprint(v)
			continue
		}
		out[k] = v
	}
	return out
}
