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
	// ModelUsage is the per-model usage map INCLUDING subagent spend
	// (Usage above excludes it), passed through from the shim's result
	// event. Absent when the shim reported none — a synthetic replay
	// result, or a pre-model_usage shim.
	ModelUsage map[string]ModelUsage `json:"model_usage,omitempty"`
	IsError    bool                  `json:"is_error"`
	ResultText string                `json:"result_text,omitempty"`
}

type CompactBoundaryFrame struct {
	Envelope
	Trigger    string `json:"trigger"`
	PreTokens  int64  `json:"pre_tokens"`
	PostTokens int64  `json:"post_tokens"`
}

// CompactStatusFrame announces that a context compaction is IN PROGRESS. The
// SDK opens the window with a `status: "compacting"` system message and closes
// it with the compact-boundary that ends the compaction; the SDK reports no
// progress percentage, so the frame is a pure start signal (Active is always
// true) and the GUI's indicator is indeterminate. The window is torn down by
// the compact-boundary, or — belt-and-suspenders, since a compaction always
// runs inside a turn — by the turn's terminating result/error.
type CompactStatusFrame struct {
	Envelope
	Active bool `json:"active"`
}

// InterruptFrame announces that the running turn is being interrupted: the
// gesture has been delivered to the shim, but the turn only actually ends when
// its `aborted` result lands. The window between the two is where already-
// dispatched tools and subagents keep streaming, so the GUI uses this frame to
// show an "interrupting…" indicator and to stop opening NEW bubbles for that
// tail while still letting in-flight bubbles finish streaming. Broadcast only
// when a turn is active (an idle interrupt has nothing to interrupt), and
// cleared client-side by the turn's terminating result/error or the next turn.
type InterruptFrame struct {
	Envelope
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
	Code        string `json:"code"` // shim_died | sdk_error | transport | internal
	Message     string `json:"message"`
	Recoverable bool   `json:"recoverable"`
}

// --- §2.3 user turns ------------------------------------------------------

type UserTurnFrame struct {
	Envelope
	RequestID string          `json:"request_id"`
	Content   json.RawMessage `json:"content"` // normalized ContentBlock[]
	// Origin marks a turn whose prompt was injected on the user's behalf and
	// should NOT render as an ordinary user-prompt bubble. "merge" is the
	// merge-failure remediation turn: the GUI renders it as a Merge status
	// card while the injected directive still drives the agent (see the Emacs
	// merge-remediation path and the webapp user-turn renderer). Empty for a
	// user's own prompt.
	Origin string `json:"origin,omitempty"`
}

// UserTurnRetractedFrame withdraws the user turn named by RequestID: its
// bubble leaves the feed as though the prompt had never been sent. Broadcast
// only for a turn the agent never answered, when the interrupting client asked
// to retract it by request id (the Emacs `C-c C-k' undo, which lands the
// prompt back in its input buffer for revision).
//
// The retraction is a FRAME rather than a rewrite of the retained ring, so it
// survives replay the same way every other state change does: a client
// applying [user-turn, interrupt, user-turn-retracted] in seq order lands on
// the same feed live or replayed.
//
// The turn still ends in its own `aborted` result, which the client folds into
// the retraction rather than rendering — the turn did not "abort", it never
// happened. That result is what clears the interrupting indicator, so the
// frame deliberately leaves the indicator standing.
type UserTurnRetractedFrame struct {
	Envelope
	RequestID string `json:"request_id"`
}

// --- §2.4 assistant text --------------------------------------------------

type TextStartFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	MessageID string `json:"message_id"`
	// The subagent tool call this block belongs to; empty (and omitted)
	// on main-chain blocks. Only the start frame carries it — delta/end
	// key by block_id into a block that already knows its parent.
	ParentToolUseID string `json:"parent_tool_use_id,omitempty"`
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

// AssistantErrorFrame marks an assistant message as an API-level error (a
// session/usage limit, a billing or auth failure, ...) rather than model
// output, so a client can render its text bubble as a failure instead of an
// answer. Keyed by message id — not block id — because the SDK's error
// verdict scopes the whole message and arrives only with the complete
// message, AFTER any of its blocks streamed. Emitted for a streamed and a
// synthesized message alike.
type AssistantErrorFrame struct {
	Envelope
	MessageID string `json:"message_id"`
	// Error mirrors the SDK's SDKAssistantMessageError discriminator
	// (rate_limit, billing_error, authentication_failed, ...).
	Error string `json:"error"`
}

// --- §2.5 thinking --------------------------------------------------------

type ThinkingStartFrame struct {
	Envelope
	BlockID   string `json:"block_id"`
	MessageID string `json:"message_id"`
	// As TextStartFrame: the owning subagent call, omitted on main-chain.
	ParentToolUseID string `json:"parent_tool_use_id,omitempty"`
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
	Kind string `json:"kind"` // bash | diff | grep | skill | task-update

	// bash
	Stdout   string `json:"stdout,omitempty"`
	Stderr   string `json:"stderr,omitempty"`
	ExitCode *int   `json:"exit_code,omitempty"`

	// task-update — one transition of a harness task, read off the SDK's
	// structured statusChange rather than scraped from the result prose.
	// A TaskCreate's card folds these in as its stream (§2.6 kind "task").
	TaskID     string   `json:"task_id,omitempty"`
	StatusFrom string   `json:"status_from,omitempty"`
	StatusTo   string   `json:"status_to,omitempty"`
	Fields     []string `json:"fields,omitempty"`

	// diff
	FilePath    string `json:"file_path,omitempty"`
	UnifiedDiff string `json:"unified_diff,omitempty"`

	// grep
	Matches []GrepMatch `json:"matches,omitempty"`

	// skill — the SKILL.md body the launched skill injects, so the card can
	// show the full skill contents alongside its invocation.
	Content string `json:"content,omitempty"`
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

// StreamRef says where an AsyncSource's stream lives and how to read it.
//
// Format selects the CLIENT's renderer, and is the whole reason the fold
// generalizes: a stream that is a conversation renders as nested bubbles,
// one that is a record log renders as rows, and one that is bytes renders
// as a pre. Nothing is forced into a shape its data does not have.
type StreamRef struct {
	// ws   — already on the wire as task-output-delta frames.
	// poll — GET /sessions/{id}/tasks/{taskId}/output, only while open.
	Transport string `json:"transport"`
	// jsonl-transcript — a subagent's full JSONL transcript (bubbles).
	// jsonl-journal    — a workflow run's journal.jsonl records (rows).
	// text             — unstructured bytes (pre).
	Format string `json:"format"`
}

// AsyncSource declares that one tool call OWNS A STREAM: work that
// outlives the call and keeps producing after it settles.
//
// This is the seam the expanded-bubble view is built on. Before it, every
// layer answered "does this call own a stream?" independently, by regex
// over English prose in the result text — so a CLI wording change silently
// disabled tailing, the poll route, and every watcher fold at once, with no
// error. The answer is now derived ONCE, structurally, from the SDK's
// tool_use_result (§1.2 `structured`), and every layer below consumes it.
//
// Adding a newly supported async type is one arm in classifyAsyncSource:
// no new frame, no new store field, no new render path, no new regex.
type AsyncSource struct {
	// The harness's own id for the work: an agentId, a backgroundTaskId, a
	// workflow task id. Keys the poll route and the client's fold.
	SourceID string `json:"source_id"`
	// agent | shell | workflow
	Kind string `json:"kind"`
	// Absent when the source announced no readable stream.
	Stream *StreamRef `json:"stream,omitempty"`
	// One line naming the work, for the fold's collapsed face.
	Label string `json:"label,omitempty"`
	// running | done | error | killed
	Status string `json:"status"`
	// The stream's file, when the structured result named a readable one —
	// the STRUCTURED twin of the prose path the tailer's spawn regexes read,
	// so the poll route's path record survives a prose wording drift (see
	// superviseTailersLocked). Correlation data; clients need not read it.
	OutputFile string `json:"output_file,omitempty"`
}

// AsyncSourceFrame announces that ToolUseID's call spawned detached work.
// One frame per spawn — a descriptor, never a stream — so it costs the
// retention ring a single frame no matter how loud the work is.
type AsyncSourceFrame struct {
	Envelope
	ToolUseID string      `json:"tool_use_id"`
	Source    AsyncSource `json:"source"`
}

type ToolUseProgressFrame struct {
	Envelope
	ToolUseID string `json:"tool_use_id"`
	Text      string `json:"text"`
	// Structured companions to the synthesized Text: the tool's name, the
	// spawning subagent's tool_use_id (empty on main-chain tools), and the
	// SDK's raw elapsed clock, so clients can attribute and render the
	// heartbeat themselves.
	ToolName        string  `json:"tool_name,omitempty"`
	ParentToolUseID string  `json:"parent_tool_use_id,omitempty"`
	ElapsedSeconds  float64 `json:"elapsed_seconds,omitempty"`
}

// TaskOutputDeltaFrame streams a detached task's output file as it
// grows: the daemon tails the file the spawn result announced (§2.6)
// and coalesces appended bytes into these frames. ToolUseID names the
// spawning call so clients append the text onto that card.
type TaskOutputDeltaFrame struct {
	Envelope
	TaskID    string `json:"task_id"`
	ToolUseID string `json:"tool_use_id,omitempty"`
	Text      string `json:"text"`
}

// TaskNotificationFrame is the completion signal of detached background
// work (a backgrounded Bash, a background agent, a workflow), parsed
// from the harness notification that rode in on a user message.
// ToolUseID names the SPAWNING tool call verbatim from the payload, so
// clients can land the completion on the exact card that started it.
// Text keeps the whole raw notification for anything the tags miss.
type TaskNotificationFrame struct {
	Envelope
	ToolUseID  string `json:"tool_use_id,omitempty"`
	TaskID     string `json:"task_id,omitempty"`
	Status     string `json:"status,omitempty"`
	Summary    string `json:"summary,omitempty"`
	OutputFile string `json:"output_file,omitempty"`
	Text       string `json:"text"`
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
	MessageID string `json:"message_id"`
	// The subagent call whose conversation this usage belongs to; empty on
	// main-chain usage. Attribution is what lets the SPA bank a sidechain
	// figure on that agent's bubble without flipping the session count.
	ParentToolUseID string   `json:"parent_tool_use_id,omitempty"`
	Usage           Usage    `json:"usage"`
	CostUSD         *float64 `json:"cost_usd,omitempty"`
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

// --- §2.9a status snapshot --------------------------------------------------

// StatusFrame carries the session's /status snapshot: the SDK's system:init
// payload the GUI's status panel renders. Pushed at session init (the live
// init warms the cache) and again after each refresh-status re-probe, so the
// panel reflects a value changed mid-session — a /fast toggle, a config edit
// — that the frozen init would miss. Snapshot is opaque exactly like a system
// frame's Data.
type StatusFrame struct {
	Envelope
	Snapshot json.RawMessage `json:"snapshot"`
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
// the shim as its own turn, cancelled by the user, dropped by a user
// interrupt, or dropped on session end. RequestID is present only when
// the item drained.
type QueueRemovedFrame struct {
	Envelope
	QueueID   string `json:"queue_id"`
	Reason    string `json:"reason"`               // drained | cancelled | interrupted | session_end
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

// --- §2.14 task summary ---------------------------------------------------

// TaskSummaryFrame carries a one-line "current objective" label for the
// session, produced by a headless Haiku call over a just-completed turn's
// driving prompt and the main-chain assistant text it produced. Emitted
// at most once per completed turn and only when the model returns a
// usable line: the summary is best-effort, so a failed or empty one
// emits no frame at all and the last good label stands. It carries no
// per-turn key — a newer turn's summary simply supersedes it — and the
// GUI renders it centered in the topbar.
type TaskSummaryFrame struct {
	Envelope
	Summary string `json:"summary"`
}
