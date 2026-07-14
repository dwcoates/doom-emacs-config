package protocol

import "encoding/json"

// Layer2Version is the wire-compatibility version of the Layer-2
// protocol, carried in every hello and in the GET /sessions envelope.
// Clients compare it against the version they were built for and
// surface a mismatch instead of mis-parsing frames. Bump on any
// breaking frame-shape change. Version 2 = boot-id era (1 = before).
const Layer2Version = 2

// Envelope is the common Layer-2 frame header (§2.1). Seq, TS and
// SessionID are stamped by the session hub just before a frame is
// retained and broadcast; translator code leaves them zero. TS is the
// one field a builder may pre-set: a frame rebuilt from a transcript
// carries the original event's timestamp, and the hub preserves it
// rather than stamping the (much later) replay time.
type Envelope struct {
	Type      string `json:"type"`
	Seq       int64  `json:"seq"`
	TS        string `json:"ts"`
	SessionID string `json:"session_id"`
}

// L2Frame is implemented by every Layer-2 daemon→webapp frame.
type L2Frame interface {
	// Env returns the frame's mutable envelope for stamping.
	Env() *Envelope
}

func (e *Envelope) Env() *Envelope { return e }

// --- §2.2 lifecycle -------------------------------------------------------

type HelloFrame struct {
	Envelope
	DaemonVersion string `json:"daemon_version"`
	// BootID identifies THIS daemon process instance: minted once at
	// startup, stable across sessions, different after every restart.
	// Clients detect a bounce by watching it change.
	BootID          string         `json:"boot_id"`
	ProtocolVersion int            `json:"protocol_version"`
	ResumeFromSeq   int64          `json:"resume_from_seq"`
	PermissionMode  PermissionMode `json:"permission_mode"`
	Model           string         `json:"model"`
	// Models is the selectable-model menu (§1.2 `models`), republished on
	// every hello so a reconnecting client never has to ask for it.
	// Absent until the shim reports it.
	Models []ModelInfo `json:"models,omitempty"`
	CWD    string      `json:"cwd"`
	// ClaudeSessionID is the durable CLI-assigned session uuid (usable
	// as a resume target); empty until the SDK's system:init arrives.
	ClaudeSessionID string `json:"claude_session_id,omitempty"`
	// Queue is the in-flight message queue snapshot (§2.13), front-to-back,
	// so a fresh join or a replay-evicted client rebuilds the pending queue
	// without a gap. Absent when the queue is empty.
	Queue []QueuedItem `json:"queue,omitempty"`
	// TurnActive is the daemon's authoritative "a turn is running right now"
	// bit. The transcript-seeded replay a fresh join receives synthesizes a
	// result for answered turns (closeReplayTurns), but a trailing prompt the
	// agent never answered gets none, so its dangling user-turn would
	// otherwise leave a client believing a turn is still in flight — a
	// cold/rehydrated session, days after that prompt, would paint the topbar
	// timer counting from its stale stamp. A client trusts this bit over what
	// the replayed frames imply.
	TurnActive bool `json:"turn_active"`
}

type ResultFrame struct {
	Envelope
	Subtype       string  `json:"subtype"`
	DurationMS    int64   `json:"duration_ms"`
	DurationAPIMS int64   `json:"duration_api_ms"`
	NumTurns      int     `json:"num_turns"`
	TotalCostUSD  float64 `json:"total_cost_usd"`
	Usage         Usage   `json:"usage"`
	IsError       bool    `json:"is_error"`
	ResultText    string  `json:"result_text,omitempty"`
}

type CompactBoundaryFrame struct {
	Envelope
	Trigger    string `json:"trigger"`
	PreTokens  int64  `json:"pre_tokens"`
	PostTokens int64  `json:"post_tokens"`
}

type RetryFrame struct {
	Envelope
	Attempt int    `json:"attempt"`
	DelayMS int64  `json:"delay_ms"`
	Reason  string `json:"reason"`
	Fatal   bool   `json:"fatal"`
}

type ErrorFrame struct {
	Envelope
	Code        string `json:"code"` // shim_died | sdk_error | transport | internal | resume_unavailable
	Message     string `json:"message"`
	Recoverable bool   `json:"recoverable"`
}

// --- §2.3 user turns ------------------------------------------------------

type UserTurnFrame struct {
	Envelope
	RequestID string          `json:"request_id"`
	Content   json.RawMessage `json:"content"` // normalized ContentBlock[]
}

// --- §2.4 assistant text --------------------------------------------------

type TextStartFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	MessageID string `json:"message_id"`
}

type TextDeltaFrame struct {
	Envelope
	BlockID string `json:"block_id"`
	Text    string `json:"text"`
}

type TextEndFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	FinalText string `json:"final_text"`
}

// --- §2.5 thinking --------------------------------------------------------

type ThinkingStartFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	MessageID string `json:"message_id"`
}

type ThinkingDeltaFrame struct {
	Envelope
	BlockID string `json:"block_id"`
	Text    string `json:"text"`
}

type ThinkingEndFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	FinalText string `json:"final_text"`
	Signature string `json:"signature,omitempty"`
}

// --- §2.6 tool-use cards --------------------------------------------------

type ToolUseStartFrame struct {
	Envelope
	ToolUseID       string `json:"tool_use_id"`
	ToolName        string `json:"tool_name"`
	MessageID       string `json:"message_id"`
	ParentToolUseID string `json:"parent_tool_use_id,omitempty"`
}

type ToolUseInputDeltaFrame struct {
	Envelope
	ToolUseID   string `json:"tool_use_id"`
	PartialJSON string `json:"partial_json"`
}

type ToolUseInputEndFrame struct {
	Envelope
	ToolUseID string          `json:"tool_use_id"`
	Input     json.RawMessage `json:"input"`
}

// RenderHint is the optional pre-rendered payload on tool-use-result
// frames. Kind selects which of the remaining fields apply.
type RenderHint struct {
	Kind string `json:"kind"` // bash | diff | grep | task

	// bash
	Stdout   string `json:"stdout,omitempty"`
	Stderr   string `json:"stderr,omitempty"`
	ExitCode *int   `json:"exit_code,omitempty"`

	// diff
	FilePath    string `json:"file_path,omitempty"`
	UnifiedDiff string `json:"unified_diff,omitempty"`

	// grep
	Matches []GrepMatch `json:"matches,omitempty"`

	// task
	Summary string `json:"summary,omitempty"`
}

type GrepMatch struct {
	File string `json:"file"`
	Line int    `json:"line"`
	Text string `json:"text"`
}

type ToolUseResultFrame struct {
	Envelope
	ToolUseID string          `json:"tool_use_id"`
	IsError   bool            `json:"is_error"`
	Content   json.RawMessage `json:"content"`
	Render    *RenderHint     `json:"render,omitempty"`
}

type ToolUseProgressFrame struct {
	Envelope
	ToolUseID string `json:"tool_use_id"`
	Text      string `json:"text"`
}

// --- §2.7 permission prompts ------------------------------------------------

// PermissionPreview is the optional preview payload on
// permission-request frames. Kind selects which fields apply.
type PermissionPreview struct {
	Kind string `json:"kind"` // bash | diff | write | generic

	// bash
	Command string `json:"command,omitempty"`

	// diff
	FilePath    string `json:"file_path,omitempty"`
	UnifiedDiff string `json:"unified_diff,omitempty"`

	// write
	Bytes   int    `json:"bytes,omitempty"`
	Preview string `json:"preview,omitempty"`

	// generic
	Summary string `json:"summary,omitempty"`
}

type PermissionRequestFrame struct {
	Envelope
	RequestID string             `json:"request_id"`
	ToolUseID string             `json:"tool_use_id"`
	ToolName  string             `json:"tool_name"`
	Input     json.RawMessage    `json:"input"`
	Preview   *PermissionPreview `json:"preview,omitempty"`
}

type PermissionResolvedFrame struct {
	Envelope
	RequestID    string          `json:"request_id"`
	Decision     string          `json:"decision"` // allow | deny | cancel
	Message      string          `json:"message,omitempty"`
	UpdatedInput json.RawMessage `json:"updated_input,omitempty"`
}

// --- §2.8 usage / mode ------------------------------------------------------

type UsageFrame struct {
	Envelope
	MessageID string   `json:"message_id"`
	Usage     Usage    `json:"usage"`
	CostUSD   *float64 `json:"cost_usd,omitempty"`
}

type PermissionModeChangedFrame struct {
	Envelope
	Mode   PermissionMode `json:"mode"`
	Origin string         `json:"origin"` // user | shim | daemon
}

// ModelsFrame carries the selectable-model menu to clients already
// attached when the shim reports it (the same list rides on every
// subsequent hello).
type ModelsFrame struct {
	Envelope
	Models []ModelInfo `json:"models"`
}

// ModelChangedFrame is the ONLY frame that moves the session's model
// after the hello, and it fires for every way the model can move.
//
// Origin says which:
//   - "user":      a set-model command the shim acked
//   - "agent":     a main-chain assistant message reported a different
//     model than the mirror, i.e. the CLI moved it without
//     being asked (a /model, a fallback, a downgrade)
//   - "reconcile": the periodic transcript check caught a drifted mirror
//
// The model is the CLI's to decide, not the daemon's, so the mirror
// FOLLOWS observed truth rather than asserting remembered truth.
type ModelChangedFrame struct {
	Envelope
	Model  string `json:"model"`
	Origin string `json:"origin"` // user | agent | reconcile
}

// --- §2.9 system ------------------------------------------------------------

type SystemFrame struct {
	Envelope
	Subtype string          `json:"subtype"` // init | slash_command
	Data    json.RawMessage `json:"data"`
}

// --- §2.13 in-flight message queue ------------------------------------------

// QueueAddedFrame announces that a user-message submitted while a turn was
// in flight was parked on the daemon's per-session FIFO queue rather than
// forwarded to the shim. Status is always the initial "classifying".
type QueueAddedFrame struct {
	Envelope
	QueueID   string          `json:"queue_id"`
	RequestID string          `json:"request_id"`
	Content   json.RawMessage `json:"content"` // normalized ContentBlock[]
	Status    string          `json:"status"`  // always "classifying"
}

// QueueClassifiedFrame carries the verdict for a queued item: whether it
// should preempt the running turn ("interrupt") or wait its turn ("wait").
// Source names who decided: the async classifier, a manual user override,
// or the fail-closed fallback.
type QueueClassifiedFrame struct {
	Envelope
	QueueID string `json:"queue_id"`
	Verdict string `json:"verdict"` // wait | interrupt
	Reason  string `json:"reason"`  // one-line human explanation
	Source  string `json:"source"`  // classifier | user | fallback
}

// QueueRemovedFrame announces a queued item leaving the queue: drained to
// the shim as its own turn, cancelled by the user, or dropped on session
// end. RequestID is present only when the item drained.
type QueueRemovedFrame struct {
	Envelope
	QueueID   string `json:"queue_id"`
	Reason    string `json:"reason"`               // drained | cancelled | session_end
	RequestID string `json:"request_id,omitempty"` // present when reason == "drained"
}

// QueuedItem is one entry of the queue snapshot carried on hello (§2.2)
// and the GET /sessions listing, front-to-back.
type QueuedItem struct {
	QueueID   string          `json:"queue_id"`
	RequestID string          `json:"request_id"`
	Content   json.RawMessage `json:"content"`
	Status    string          `json:"status"` // classifying | waiting | interrupt
	Verdict   string          `json:"verdict,omitempty"`
	Reason    string          `json:"reason,omitempty"`
}
