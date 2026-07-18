// Package protocol implements the claude-repl wire formats defined in
// shared/protocol.md: Layer 1 (Go ⇄ TS shim, stdio NDJSON) and Layer 2
// (Go daemon ⇄ webapp, WebSocket NDJSON).
package protocol

import (
	"encoding/json"
	"fmt"
)

// PermissionMode is the session-wide permission mode enum shared by both
// layers.
type PermissionMode string

const (
	PermissionModeDefault           PermissionMode = "default"
	PermissionModeAcceptEdits       PermissionMode = "acceptEdits"
	PermissionModeBypassPermissions PermissionMode = "bypassPermissions"
	PermissionModePlan              PermissionMode = "plan"
	// CLI-era modes (claude >= 2.1 renames/additions): the daemon
	// passes modes through to the CLI's own validation, so the enum
	// tracks the superset the CLI accepts rather than gatekeeping a
	// stale subset.
	PermissionModeAuto     PermissionMode = "auto"
	PermissionModeManual   PermissionMode = "manual"
	PermissionModeDontAsk  PermissionMode = "dontAsk"
	PermissionModeDelegate PermissionMode = "delegate"
)

// ValidPermissionMode reports whether s is a member of the PermissionMode
// enum.
func ValidPermissionMode(s string) bool {
	switch PermissionMode(s) {
	case PermissionModeDefault, PermissionModeAcceptEdits,
		PermissionModeBypassPermissions, PermissionModePlan,
		PermissionModeAuto, PermissionModeManual,
		PermissionModeDontAsk, PermissionModeDelegate:
		return true
	}
	return false
}

// Usage mirrors the shared Usage shape.
type Usage struct {
	InputTokens              int  `json:"input_tokens"`
	OutputTokens             int  `json:"output_tokens"`
	CacheCreationInputTokens *int `json:"cache_creation_input_tokens,omitempty"`
	CacheReadInputTokens     *int `json:"cache_read_input_tokens,omitempty"`
}

// ModelUsage is one model's slice of a result's model_usage map. Unlike
// Usage — which the SDK scopes to the top-level agent loop only — this
// aggregation counts subagent requests too, so summing the map's entries
// is the session's whole-tree spend.
type ModelUsage struct {
	InputTokens              int     `json:"input_tokens"`
	OutputTokens             int     `json:"output_tokens"`
	CacheCreationInputTokens int     `json:"cache_creation_input_tokens"`
	CacheReadInputTokens     int     `json:"cache_read_input_tokens"`
	WebSearchRequests        int     `json:"web_search_requests"`
	CostUSD                  float64 `json:"cost_usd"`
	ContextWindow            int     `json:"context_window"`
}

// ModelInfo is one selectable model, from the SDK's supportedModels().
// Shared by both layers: the shim reports it, the daemon caches it, the
// hello republishes it.
type ModelInfo struct {
	Value       string `json:"value"`
	DisplayName string `json:"displayName"`
	Description string `json:"description"`
}

// SlashCommand is one invocable slash command, from the SDK's
// supportedCommands(). Shared by both layers exactly as ModelInfo is: the
// shim reports it, the daemon caches it, and clients read it back.
//
// Name carries no leading slash. ArgumentHint is empty for a command that
// takes no arguments, which is a real answer rather than a missing one.
type SlashCommand struct {
	Name         string `json:"name"`
	Description  string `json:"description"`
	ArgumentHint string `json:"argumentHint"`
}

// PermissionDenial is one entry of ResultEvt.permission_denials.
type PermissionDenial struct {
	ToolUseID string `json:"tool_use_id"`
	ToolName  string `json:"tool_name"`
	Message   string `json:"message,omitempty"`
}

// L1Event is the decoded form of one shim→Go NDJSON event line. It is a
// deliberately flat union: Type discriminates, and only the fields
// meaningful for that type are populated. Payloads the daemon passes
// through opaquely (stream events, system data, tool content) stay as
// json.RawMessage.
type L1Event struct {
	Type      string `json:"type"`
	SessionID string `json:"session_id,omitempty"`
	RequestID string `json:"request_id,omitempty"`
	UUID      string `json:"uuid,omitempty"`

	// ready
	ShimVersion    string `json:"shim_version,omitempty"`
	SDKVersion     string `json:"sdk_version,omitempty"`
	PermissionMode string `json:"permission_mode,omitempty"`

	// models
	Models []ModelInfo `json:"models,omitempty"`

	// commands
	Commands []SlashCommand `json:"commands,omitempty"`

	// stream-event / assistant-message / tool-result
	ParentToolUseID string          `json:"parent_tool_use_id,omitempty"`
	Event           json.RawMessage `json:"event,omitempty"`
	// Structured is the SDK's tool_use_result on tool-result events: the
	// tool's own JSON result, whose shape is per-tool (§1.2). Content is
	// only the flattened text the model saw; every structured fact about a
	// call — a Bash's separate stderr, an Agent's agentId and outputFile, a
	// TaskUpdate's statusChange — lives here and nowhere else. Absent when
	// the SDK omitted it, and from any shim predating the field.
	Structured json.RawMessage `json:"structured,omitempty"`
	// Message is an object on assistant-message events and a plain JSON
	// string on error events; use MessageText for the latter.
	Message json.RawMessage `json:"message,omitempty"`
	// Error is the SDK's structured verdict that an assistant-message IS an
	// API-level failure (a session/usage limit, a billing or auth error);
	// empty on an ordinary assistant message. It rides beside Message, not
	// inside it, mirroring the SDK shape.
	Error string `json:"error,omitempty"`

	// result
	Subtype           string                `json:"subtype,omitempty"`
	DurationMS        int64                 `json:"duration_ms,omitempty"`
	DurationAPIMS     int64                 `json:"duration_api_ms,omitempty"`
	NumTurns          int                   `json:"num_turns,omitempty"`
	TotalCostUSD      float64               `json:"total_cost_usd,omitempty"`
	Usage             *Usage                `json:"usage,omitempty"`
	ModelUsage        map[string]ModelUsage `json:"model_usage,omitempty"`
	Result            *string               `json:"result,omitempty"`
	IsError           bool                  `json:"is_error,omitempty"`
	PermissionDenials []PermissionDenial    `json:"permission_denials,omitempty"`

	// permission-request
	ToolUseID   string          `json:"tool_use_id,omitempty"`
	ToolName    string          `json:"tool_name,omitempty"`
	Input       json.RawMessage `json:"input,omitempty"`
	Suggestions json.RawMessage `json:"suggestions,omitempty"`

	// tool-result
	Content json.RawMessage `json:"content,omitempty"`

	// system
	Data json.RawMessage `json:"data,omitempty"`

	// error
	Code  string `json:"code,omitempty"`
	Stack string `json:"stack,omitempty"`

	// closed
	ExitCode int    `json:"exit_code,omitempty"`
	Reason   string `json:"reason,omitempty"`
}

// MessageText returns the Message payload as a string for event types
// (like error) whose `message` field is a JSON string rather than an
// object. Returns "" when the field is absent or not a string.
func (e *L1Event) MessageText() string {
	var s string
	if err := json.Unmarshal(e.Message, &s); err != nil {
		return ""
	}
	return s
}

// l1EventKnownTypes is the closed Layer-1 event enum at this protocol
// version (including the tool-result extension).
var l1EventKnownTypes = map[string]bool{
	"ready":              true,
	"ack":                true,
	"models":             true,
	"commands":           true,
	"stream-event":       true,
	"assistant-message":  true,
	"result":             true,
	"permission-request": true,
	"tool-result":        true,
	"system":             true,
	"error":              true,
	"closed":             true,
}

// DecodeL1Event decodes one shim stdout line. Unknown event types return
// (nil, nil): they must be ignored for forward compatibility.
func DecodeL1Event(line []byte) (*L1Event, error) {
	var probe struct {
		Type string `json:"type"`
	}
	if err := json.Unmarshal(line, &probe); err != nil {
		return nil, fmt.Errorf("layer1: invalid JSON event line: %w", err)
	}
	if probe.Type == "" {
		return nil, fmt.Errorf("layer1: event line missing type discriminator")
	}
	if !l1EventKnownTypes[probe.Type] {
		return nil, nil
	}
	var evt L1Event
	if err := json.Unmarshal(line, &evt); err != nil {
		return nil, fmt.Errorf("layer1: malformed %s event: %w", probe.Type, err)
	}
	return &evt, nil
}

// ---------------------------------------------------------------------------
// Commands (Go → shim), §1.1. The daemon usually forwards client command
// lines verbatim (Layer 2 reuses the Layer-1 command shapes), so only the
// commands the daemon originates itself get builder types here.
// ---------------------------------------------------------------------------

// ShutdownCmd asks the shim to drain and exit cleanly.
type ShutdownCmd struct {
	Type      string `json:"type"`
	RequestID string `json:"request_id"`
	Reason    string `json:"reason,omitempty"`
}

// NewShutdownCmd builds a shutdown command frame.
func NewShutdownCmd(requestID, reason string) ShutdownCmd {
	return ShutdownCmd{Type: "shutdown", RequestID: requestID, Reason: reason}
}

// L1Command is the decoded form of one client (or daemon) command frame.
// Layer 2's webapp→daemon traffic reuses these shapes verbatim, so the
// same decoder serves both layers.
type L1Command struct {
	Type      string `json:"type"`
	RequestID string `json:"request_id"`

	// user-message
	Content         json.RawMessage `json:"content,omitempty"`
	ParentToolUseID string          `json:"parent_tool_use_id,omitempty"`
	// Origin tags a user-message injected on the user's behalf so its turn
	// renders specially (see UserTurnFrame.Origin); "merge" is the
	// merge-failure remediation directive. Empty for a user's own prompt.
	Origin string `json:"origin,omitempty"`

	// permission-decision
	Decision *PermissionDecision `json:"decision,omitempty"`

	// set-permission-mode
	Mode string `json:"mode,omitempty"`

	// set-model
	Model string `json:"model,omitempty"`

	// shutdown
	Reason string `json:"reason,omitempty"`

	// replay-request (Layer 2 only; carries no request_id)
	FromSeq int64 `json:"from_seq,omitempty"`

	// queue-run-now / queue-cancel (Layer 2 only, daemon-handled: never
	// forwarded to the shim). Names the queued item the override targets.
	QueueID string `json:"queue_id,omitempty"`
}

// PermissionDecision mirrors PermissionDecisionCmd.decision.
type PermissionDecision struct {
	Behavior           string          `json:"behavior"`
	UpdatedInput       json.RawMessage `json:"updated_input,omitempty"`
	UpdatedPermissions json.RawMessage `json:"updated_permissions,omitempty"`
	Message            string          `json:"message,omitempty"`
	Interrupt          bool            `json:"interrupt,omitempty"`
}

var l1CommandKnownTypes = map[string]bool{
	"user-message":        true,
	"permission-decision": true,
	"interrupt":           true,
	"set-permission-mode": true,
	"set-model":           true,
	"refresh-commands":    true,
	"shutdown":            true,
	"replay-request":      true,
	// §2.13 in-flight queue overrides: Layer-2 only, daemon-handled.
	"queue-run-now": true,
	"queue-cancel":  true,
}

// l1CommandActs lists the command types that must reach a LIVE CLI. A
// hibernated session receiving one has to be revived first; the rest
// (replay-request) are served wholly from the daemon's retained ring and
// must never spawn a process, since that is the free-to-view path.
var l1CommandActs = map[string]bool{
	"user-message":        true,
	"permission-decision": true,
	"interrupt":           true,
	"set-permission-mode": true,
	"set-model":           true,
}

// CommandActs reports whether a raw webapp→daemon frame is a command that
// needs a live CLI (and so must revive a hibernated session). A frame
// that does not decode, or decodes to a non-acting type (replay-request,
// an unknown type, or the deliberately-dropped shutdown), reports false —
// merely observing a session must never resurrect its process pair.
func CommandActs(line []byte) bool {
	var probe struct {
		Type string `json:"type"`
	}
	if err := json.Unmarshal(line, &probe); err != nil {
		return false
	}
	return l1CommandActs[probe.Type]
}

// DecodeCommand decodes one command frame (webapp→daemon or Go→shim
// direction). Unknown types return (nil, nil) per the forward
// compatibility rule; structurally invalid frames return an error.
func DecodeCommand(line []byte) (*L1Command, error) {
	var probe struct {
		Type string `json:"type"`
	}
	if err := json.Unmarshal(line, &probe); err != nil {
		return nil, fmt.Errorf("layer1: invalid JSON command line: %w", err)
	}
	if probe.Type == "" {
		return nil, fmt.Errorf("layer1: command missing type discriminator")
	}
	if !l1CommandKnownTypes[probe.Type] {
		return nil, nil
	}
	var cmd L1Command
	if err := json.Unmarshal(line, &cmd); err != nil {
		return nil, fmt.Errorf("layer1: malformed %s command: %w", probe.Type, err)
	}
	if cmd.Type != "replay-request" && cmd.RequestID == "" {
		return nil, fmt.Errorf("layer1: %s command missing request_id", cmd.Type)
	}
	switch cmd.Type {
	case "user-message":
		if len(cmd.Content) == 0 {
			return nil, fmt.Errorf("layer1: user-message missing content")
		}
	case "permission-decision":
		if cmd.Decision == nil {
			return nil, fmt.Errorf("layer1: permission-decision missing decision")
		}
		if cmd.Decision.Behavior != "allow" && cmd.Decision.Behavior != "deny" {
			return nil, fmt.Errorf("layer1: permission-decision behavior must be allow|deny, got %q", cmd.Decision.Behavior)
		}
		if cmd.Decision.Behavior == "deny" && cmd.Decision.Message == "" {
			return nil, fmt.Errorf("layer1: deny decision requires a message")
		}
	case "set-permission-mode":
		if !ValidPermissionMode(cmd.Mode) {
			return nil, fmt.Errorf("layer1: set-permission-mode invalid mode %q", cmd.Mode)
		}
	case "set-model":
		// Empty is a caller who forgot to name a model, NOT a request for
		// the default one: reading it as "default" would switch the
		// session to a model nobody chose. The model id itself is the
		// CLI's to validate, so it passes through unchecked beyond this.
		if cmd.Model == "" {
			return nil, fmt.Errorf("layer1: set-model requires a non-empty model")
		}
	case "queue-run-now", "queue-cancel":
		// A queue override that names no item cannot be dispatched: the
		// queue_id is the sole target selector.
		if cmd.QueueID == "" {
			return nil, fmt.Errorf("layer1: %s command missing queue_id", cmd.Type)
		}
	}
	return &cmd, nil
}

// EncodeNDJSON marshals v followed by a newline.
func EncodeNDJSON(v any) ([]byte, error) {
	b, err := json.Marshal(v)
	if err != nil {
		return nil, fmt.Errorf("layer1: encode: %w", err)
	}
	return append(b, '\n'), nil
}
