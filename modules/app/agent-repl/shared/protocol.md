# agent-repl wire protocol

Source-of-truth catalog for the NDJSON wire formats spoken across the
three new subprojects under `modules/app/agent-repl/`:

```
Emacs (xwidget WS client)
        ▲
        │  Layer 2: Go ⇄ webapp (WebSocket, NDJSON frames)
        ▼
   Go daemon (agent-repld)
        ▲
        │  Layer 1: Go ⇄ TS shim (stdio, NDJSON lines)
        ▼
   TS shim (one per session) ── Agent SDK `query()` ──▶ Claude
```

Both layers use **NDJSON** — one JSON object per line, no trailing
commas, UTF-8, `\n`-terminated. Every frame at every layer carries a
discriminator field named `type`. Unknown `type` values **must** be
ignored by the receiver (forward compatibility).

References to the underlying SDK semantics:
- https://code.claude.com/docs/en/agent-sdk/streaming-output
- https://code.claude.com/docs/en/agent-sdk/user-input
- https://code.claude.com/docs/en/agent-sdk/streaming-vs-single-mode

Field shapes are written in TypeScript-style notation. `?` marks
optional fields. `// <comment>` annotates a field. All `id` fields are
opaque strings; the producer chooses the format and the consumer must
not parse them.

---

## Common shared types

```ts
type ISO8601 = string;                // "2026-05-24T12:34:56.789Z"
type SessionId = string;              // shim-assigned, stable per session
type RequestId = string;              // correlates request/response pairs
type ToolUseId = string;              // SDK-assigned tool_use block id
type PermissionMode =
  | "default"
  | "acceptEdits"
  | "bypassPermissions"
  | "plan";

interface Usage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
}

interface ContentBlockText      { type: "text";      text: string; }
interface ContentBlockThinking  { type: "thinking";  thinking: string; signature?: string; }
interface ContentBlockToolUse   { type: "tool_use";  id: ToolUseId; name: string; input: unknown; }
interface ContentBlockToolResult{
  type: "tool_result";
  tool_use_id: ToolUseId;
  content: string | Array<{ type: "text"; text: string }>;
  is_error?: boolean;
}
type ContentBlock =
  | ContentBlockText
  | ContentBlockThinking
  | ContentBlockToolUse
  | ContentBlockToolResult;
```

---

## Layer 1 — Go ⇄ TS shim (stdio NDJSON)

The Go daemon spawns one shim subprocess per session. The shim's stdin
carries Go→shim **commands**; the shim's stdout carries shim→Go
**events**. The shim's stderr is reserved for free-form log lines (not
NDJSON, never consumed for protocol purposes).

Every frame carries a `type` discriminator. Commands additionally carry
a `request_id` so events can correlate. Events that originate from the
SDK rather than a command carry only `type` (and SDK-native ids).

### 1.1 Commands (Go → shim)

#### `user-message`

Submit a new user turn to the SDK's input generator.

```ts
interface UserMessageCmd {
  type: "user-message";
  request_id: RequestId;
  content:
    | string                          // shorthand: wraps as [{type:"text",text}]
    | Array<ContentBlock>;            // full multi-block form per SDK
  parent_tool_use_id?: ToolUseId;     // for nested subagent input
}
```

The shim yields the resulting `SDKUserMessage` into the `query()`
async-iterable as documented in the streaming-input mode page.

#### `permission-decision`

Resolve a pending `permission-request` event previously emitted by the
shim. Echoes the same `request_id` the shim used.

```ts
interface PermissionDecisionCmd {
  type: "permission-decision";
  request_id: RequestId;              // matches PermissionRequestEvt.request_id
  decision:
    | { behavior: "allow"; updated_input?: unknown; updated_permissions?: unknown[] }
    | { behavior: "deny";  message: string; interrupt?: boolean };
}
```

#### `interrupt`

Abort the in-flight assistant turn. Maps to the SDK `query.interrupt()`
method. The aborted turn still terminates with a `result` event, whose
`subtype` is `"aborted"`.

```ts
interface InterruptCmd {
  type: "interrupt";
  request_id: RequestId;
}
```

#### `set-permission-mode`

Switch the session's default permission mode mid-flight. Maps to
`query.setPermissionMode(mode)`.

```ts
interface SetPermissionModeCmd {
  type: "set-permission-mode";
  request_id: RequestId;
  mode: PermissionMode;
}
```

#### `shutdown`

Ask the shim to drain, close the SDK query, and exit cleanly. Go then
reaps the process.

```ts
interface ShutdownCmd {
  type: "shutdown";
  request_id: RequestId;
  reason?: string;
}
```

### 1.2 Events (shim → Go)

#### `ready`

Emitted once after the shim has constructed the `query()` iterator and
is prepared to accept commands. Go must not send any command before
seeing `ready`.

```ts
interface ReadyEvt {
  type: "ready";
  session_id: SessionId;
  shim_version: string;               // semver of the shim build
  sdk_version: string;                // @anthropic-ai/claude-agent-sdk version
  permission_mode: PermissionMode;    // initial mode
}
```

#### `ack`

Success acknowledgement for a command that has no richer response event
of its own (`set-permission-mode`, `shutdown`). Commands whose effect is
observable through a dedicated event stream (`user-message` → stream
events, `permission-decision` → the unblocked turn) do not produce an
`ack`. In particular, the daemon emits the Layer-2
`permission-mode-changed` frame only after receiving the `ack` for the
corresponding `set-permission-mode` command.

```ts
interface AckEvt {
  type: "ack";
  session_id: SessionId;
  request_id: RequestId;              // matches the acknowledged command
}
```

#### `stream-event`

Raw partial-message stream from the SDK (`--include-partial-messages`
equivalent). Carries `RawMessageStreamEvent`s straight from the
underlying Anthropic API. Used by the daemon for the lowest-latency text
deltas before any assistant-message arrives.

```ts
interface StreamEventEvt {
  type: "stream-event";
  session_id: SessionId;
  uuid: string;                       // SDK-assigned dedup key
  parent_tool_use_id?: ToolUseId;
  event: RawMessageStreamEvent;       // pass-through, see SDK docs
}

// Inlined subset of the Anthropic Messages streaming event union
type RawMessageStreamEvent =
  | { type: "message_start";       message: { id: string; role: "assistant"; model: string; usage: Usage } }
  | { type: "content_block_start"; index: number; content_block: ContentBlock }
  | { type: "content_block_delta"; index: number; delta:
        | { type: "text_delta";        text: string }
        | { type: "thinking_delta";    thinking: string }
        | { type: "input_json_delta";  partial_json: string }
        | { type: "signature_delta";   signature: string } }
  | { type: "content_block_stop";  index: number }
  | { type: "message_delta";       delta: { stop_reason?: string }; usage?: Partial<Usage> }
  | { type: "message_stop" }
  | { type: "ping" };
```

#### `assistant-message`

Committed assistant message after the SDK has assembled a full
top-level message from the stream. Emitted once per assistant turn (or
once per tool-use round).

```ts
interface AssistantMessageEvt {
  type: "assistant-message";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  message: {
    id: string;
    role: "assistant";
    model: string;
    stop_reason: string | null;
    content: ContentBlock[];
    usage: Usage;
  };
}
```

#### `result`

Terminal event for one assistant turn. Mirrors the SDK's `SDKResultMessage`.

```ts
interface ResultEvt {
  type: "result";
  session_id: SessionId;
  uuid: string;
  subtype:
    | "success"
    | "error_max_turns"
    | "error_during_execution"
    | "aborted";                      // turn ended by an `interrupt` command
  duration_ms: number;
  duration_api_ms: number;
  num_turns: number;
  total_cost_usd: number;
  usage: Usage;
  result?: string;                    // present iff subtype === "success"
  is_error: boolean;
  permission_denials?: Array<{
    tool_use_id: ToolUseId;
    tool_name: string;
    message?: string;
  }>;
}
```

#### `permission-request`

The SDK invoked `canUseTool`; the shim is blocked waiting for a
`permission-decision` reply with the matching `request_id`.

```ts
interface PermissionRequestEvt {
  type: "permission-request";
  session_id: SessionId;
  request_id: RequestId;              // Go must echo this in the decision
  tool_use_id: ToolUseId;
  tool_name: string;                  // "Bash" | "Edit" | "Write" | ...
  input: unknown;                     // raw tool input the SDK proposed
  suggestions?: unknown[];            // SDK-proposed permission updates
}
```

#### `tool-result`

Tool results surface from the SDK as user-role messages carrying
`tool_result` content blocks; Layer 2's `tool-use-result` frame (§2.6)
needs them, so the shim decomposes each such block into one
`tool-result` event. (Added after the original event set as a
forward-compatible extension — older receivers ignore it per the
unknown-`type` rule.)

```ts
interface ToolResultEvt {
  type: "tool-result";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  tool_use_id: ToolUseId;
  is_error: boolean;
  content: string | Array<{ type: "text"; text: string }>;
}
```

#### `system`

System frames from the SDK (`SDKSystemMessage`) carrying init/compact/
slash-command boundaries. The shim passes the SDK payload through under
a single envelope.

```ts
interface SystemEvt {
  type: "system";
  session_id: SessionId;
  uuid: string;
  subtype:
    | "init"
    | "compact_boundary"              // pre-compaction marker
    | "tool_use_progress"             // tool emitted progress text
    | "slash_command";
  data: unknown;                      // SDK-shaped payload, see streaming-output docs
}
```

#### `error`

Out-of-band error from the shim itself (SDK threw, JSON parse failure,
internal bug). Distinct from a `result` with `is_error: true`.

```ts
interface ErrorEvt {
  type: "error";
  session_id: SessionId;
  request_id?: RequestId;             // present iff caused by a specific command
  code:
    | "shim_internal"
    | "sdk_throw"
    | "bad_command"
    | "transport"
    | "shutdown_in_progress";
  message: string;
  stack?: string;
}
```

#### `closed`

Terminal event on the shim's stdout: the SDK query has ended and the
shim is about to exit. Emitted for clean shutdowns and abnormal query
termination alike. The daemon's process supervisor keys off this event
(plus the eventual process exit code) rather than inferring lifecycle
from `result` events, which are per-turn and absent when the query dies
while idle. No further frames follow a `closed`.

```ts
interface ClosedEvt {
  type: "closed";
  session_id: SessionId;
  request_id?: RequestId;             // present iff caused by a `shutdown` command
  exit_code: number;                  // code the shim process will exit with
  reason: "shutdown" | "sdk_end" | "fatal_error";
}
```

---

## Layer 2 — Go daemon → webapp (WebSocket NDJSON)

The daemon exposes a per-session WebSocket endpoint (e.g.
`/sessions/{id}/stream`). Frames are NDJSON-encoded WebSocket text
messages — one JSON object per frame, never split across frames. The
direction is bi-directional but webapp→daemon traffic is limited to
UI-originated commands (`user-message`, `permission-decision`,
`interrupt`, `set-permission-mode`) which re-use the Layer-1 command
shapes verbatim, plus the `replay-request` frame (§2.10), so this
section otherwise enumerates only **daemon → webapp** frames.

Every frame carries a `seq` (monotonic, per-session) so the SPA store
can detect drops and request a snapshot replay.

### 2.1 Envelope

```ts
interface WsEnvelope {
  type: string;                       // see subtypes below
  seq: number;                        // strictly increasing per session
  ts: ISO8601;
  session_id: SessionId;
}
```

All frames below extend `WsEnvelope`.

### 2.2 Lifecycle frames

#### `hello`

First frame on every connection. Carries the snapshot resume cursor so
the SPA knows whether it joined mid-conversation.

```ts
interface HelloFrame extends WsEnvelope {
  type: "hello";
  daemon_version: string;
  resume_from_seq: number;            // 0 if no prior history
  permission_mode: PermissionMode;
  model: string;
  cwd: string;
}
```

#### `result`

End-of-turn marker. Drives the SPA "spinner off" + usage chip.

```ts
interface ResultFrame extends WsEnvelope {
  type: "result";
  subtype:
    | "success"
    | "error_max_turns"
    | "error_during_execution"
    | "aborted";                      // turn ended by an interrupt
  duration_ms: number;
  duration_api_ms: number;
  num_turns: number;
  total_cost_usd: number;
  usage: Usage;
  is_error: boolean;
  result_text?: string;               // for "success"
}
```

#### `compact-boundary`

Inserts a visible divider in the conversation feed when the SDK
compacts prior context.

```ts
interface CompactBoundaryFrame extends WsEnvelope {
  type: "compact-boundary";
  trigger: "auto" | "manual";
  pre_tokens: number;
  post_tokens: number;
}
```

#### `retry`

Daemon-originated; reports an SDK / network retry attempt so the SPA
can surface a non-modal "retrying…" badge.

```ts
interface RetryFrame extends WsEnvelope {
  type: "retry";
  attempt: number;                    // 1-based
  delay_ms: number;
  reason: string;                     // short human-readable
  fatal: boolean;                     // true → next frame will be `error`
}
```

#### `error`

Daemon-level error surfaced to the SPA. Distinct from a `result` with
`is_error: true`.

```ts
interface ErrorFrame extends WsEnvelope {
  type: "error";
  code: "shim_died" | "sdk_error" | "transport" | "internal";
  message: string;
  recoverable: boolean;
}
```

### 2.3 User turns

#### `user-turn`

Daemon broadcast of an accepted `user-message` command so every
connected tab (including the submitter) renders the turn from the same
authoritative frame, mirroring how `permission-resolved` converges tabs.

```ts
interface UserTurnFrame extends WsEnvelope {
  type: "user-turn";
  request_id: RequestId;              // matches the originating user-message
  content: ContentBlock[];            // normalized: string shorthand expanded
}
```

### 2.4 Assistant text — streaming

The daemon accumulates `stream-event` deltas into logical "text blocks"
and emits the following frames for the SPA's `TextStream` component.

**Block-closure invariant**: every `*-start` frame in §2.4–2.6 is
eventually followed by its matching closing frame (`text-end`,
`thinking-end`, `tool-use-input-end`) before the turn's `result` frame
— including turns that end via interrupt or error. The SPA never has to
garbage-collect dangling open blocks.

#### `text-start`

Opens a new streaming text block. The SPA mounts a `TextStream` node
keyed by `block_id` and appends subsequent `text-delta` frames into it.

```ts
interface TextStartFrame extends WsEnvelope {
  type: "text-start";
  block_id: string;                   // daemon-assigned, stable
  message_id: string;                 // SDK message.id
}
```

#### `text-delta`

```ts
interface TextDeltaFrame extends WsEnvelope {
  type: "text-delta";
  block_id: string;
  text: string;                       // raw text chunk, may contain partial markdown
}
```

#### `text-end`

```ts
interface TextEndFrame extends WsEnvelope {
  type: "text-end";
  block_id: string;
  final_text: string;                 // canonical text for re-render / re-parse
}
```

### 2.5 Thinking blocks

Streamed similarly to text but rendered as a collapsible section by
the `Thinking` component.

#### `thinking-start` / `thinking-delta` / `thinking-end`

```ts
interface ThinkingStartFrame extends WsEnvelope {
  type: "thinking-start";
  block_id: string;
  message_id: string;
}
interface ThinkingDeltaFrame extends WsEnvelope {
  type: "thinking-delta";
  block_id: string;
  text: string;
}
interface ThinkingEndFrame extends WsEnvelope {
  type: "thinking-end";
  block_id: string;
  final_text: string;
  signature?: string;
}
```

### 2.6 Tool-use cards

The daemon emits one `tool-use-start` per tool invocation, optional
`tool-use-input-delta` frames as the SDK streams `input_json_delta`s,
exactly one `tool-use-input-end` once the input is finalized, then a
single `tool-use-result` once the tool returns. The SPA mounts a
`ToolCard/<Name>` component keyed by `tool_use_id`.

Concrete `tool_name` values the SPA must render specially:
`Bash`, `Read`, `Edit`, `Write`, `Grep`, `Task`. Any other name falls
back to a generic `ToolCard/Generic` renderer.

#### `tool-use-start`

```ts
interface ToolUseStartFrame extends WsEnvelope {
  type: "tool-use-start";
  tool_use_id: ToolUseId;
  tool_name: string;                  // "Bash" | "Read" | ...
  message_id: string;
  parent_tool_use_id?: ToolUseId;     // nested subagent (Task) tools
}
```

#### `tool-use-input-delta`

```ts
interface ToolUseInputDeltaFrame extends WsEnvelope {
  type: "tool-use-input-delta";
  tool_use_id: ToolUseId;
  partial_json: string;               // append to running JSON buffer
}
```

#### `tool-use-input-end`

Final, parsed input object. Sent once. SPA replaces any locally-parsed
partial with this canonical form.

```ts
interface ToolUseInputEndFrame extends WsEnvelope {
  type: "tool-use-input-end";
  tool_use_id: ToolUseId;
  input: BashInput | ReadInput | EditInput | WriteInput | GrepInput | TaskInput | Record<string, unknown>;
}

interface BashInput  { command: string; description?: string; timeout?: number; run_in_background?: boolean; }
interface ReadInput  { file_path: string; offset?: number; limit?: number; }
interface EditInput  { file_path: string; old_string: string; new_string: string; replace_all?: boolean; }
interface WriteInput { file_path: string; content: string; }
interface GrepInput  { pattern: string; path?: string; glob?: string; output_mode?: "content" | "files_with_matches" | "count"; }
interface TaskInput  { description: string; prompt: string; subagent_type?: string; }
```

#### `tool-use-result`

```ts
interface ToolUseResultFrame extends WsEnvelope {
  type: "tool-use-result";
  tool_use_id: ToolUseId;
  is_error: boolean;
  content: string | Array<{ type: "text"; text: string }>;
  // Optional pre-rendered hints for richer cards.
  render?:
    | { kind: "bash";  stdout: string; stderr: string; exit_code?: number }
    | { kind: "diff";  file_path: string; unified_diff: string }
    | { kind: "grep";  matches: Array<{ file: string; line: number; text: string }> }
    | { kind: "task";  summary: string };
}
```

#### `tool-use-progress`

Optional intermediate progress (long-running tools, background bash).

```ts
interface ToolUseProgressFrame extends WsEnvelope {
  type: "tool-use-progress";
  tool_use_id: ToolUseId;
  text: string;
}
```

### 2.7 Permission prompts

Emitted whenever the shim raises a `permission-request`. The SPA mounts
a `PermissionPrompt` modal/card keyed by `request_id`.

#### `permission-request`

```ts
interface PermissionRequestFrame extends WsEnvelope {
  type: "permission-request";
  request_id: RequestId;
  tool_use_id: ToolUseId;
  tool_name: string;
  input: unknown;                     // same shape as Layer-1
  preview?:
    | { kind: "bash";  command: string }
    | { kind: "diff";  file_path: string; unified_diff: string }
    | { kind: "write"; file_path: string; bytes: number; preview: string }
    | { kind: "generic"; summary: string };
}
```

#### `permission-resolved`

Echoes the resolution so all connected SPA tabs converge. A `"cancel"`
decision means no user decision was made: the pending request was
invalidated by an interrupt, shim death, or session shutdown, and the
SPA must dismiss the stale prompt.

```ts
interface PermissionResolvedFrame extends WsEnvelope {
  type: "permission-resolved";
  request_id: RequestId;
  decision: "allow" | "deny" | "cancel";
  message?: string;                   // for deny/cancel
  updated_input?: unknown;            // for allow w/ edits
}
```

### 2.8 Usage / model chip

#### `usage`

Periodic incremental usage update separate from `result`, so the SPA
can update the token chip mid-turn.

```ts
interface UsageFrame extends WsEnvelope {
  type: "usage";
  message_id: string;
  usage: Usage;
  cost_usd?: number;
}
```

#### `permission-mode-changed`

Mirrors a `set-permission-mode` command after the daemon has applied it
to the shim.

```ts
interface PermissionModeChangedFrame extends WsEnvelope {
  type: "permission-mode-changed";
  mode: PermissionMode;
  origin: "user" | "shim" | "daemon";
}
```

### 2.9 System / slash-command frames

Layer-1 `system` events with subtype `tool_use_progress` are NOT
forwarded here; the daemon maps them to the dedicated
`tool-use-progress` frame (§2.6) instead.

```ts
interface SystemFrame extends WsEnvelope {
  type: "system";
  subtype: "init" | "slash_command";
  data: unknown;                      // pass-through from Layer-1 SystemEvt.data
}
```

### 2.10 Resume / replay

The one webapp→daemon frame that is not a shared Layer-1 command shape.
When the SPA detects a gap in `seq` it requests a replay:

```ts
interface ReplayRequestFrame {
  type: "replay-request";
  from_seq: number;                   // first seq the SPA is missing
}
```

- The daemon re-sends its retained frames with `seq >= from_seq`, in
  order, preserving their original `seq` and `ts` values.
- If `from_seq` has already been evicted from the retention window, the
  daemon instead sends a fresh `hello` whose `resume_from_seq` names the
  earliest retained frame, and the SPA rebuilds its store from that
  point (discarding local state older than the gap).

---

## Versioning & forward compatibility

- Each layer's frame discriminator (`type`) is a closed enum at any
  given protocol version, but receivers **must** ignore unknown values
  rather than error.
- Each layer carries a version string at handshake (`shim_version` /
  `daemon_version`); breaking changes bump the minor version and are
  gated behind a capability list negotiated at handshake (future work).
- `seq` (Layer 2 only) lets the SPA detect drops and request a replay
  via `replay-request` (§2.10); the daemon retains the last N frames
  per session for this.

## Non-goals

- This document does not specify the HTTP routes the daemon exposes;
  those live next to `daemon/internal/server/`.
- It does not specify the SPA-side store shape or component props;
  those live next to `webapp/src/`.
- It does not specify the on-disk session JSONL format owned by
  `claude` itself; that is upstream-controlled and consumed unchanged.

---

## Implementation notes

> **Status**: the shim (`shim/src/`) was rewritten from scratch against
> this spec and conforms to §1 in full, including the `tool-result`
> extension event. The earlier pre-spec scaffold (and the nine
> divergences this section used to track) is gone.

- The shim maps SDK result subtypes outside the Layer-1 enum
  (`error_max_budget_usd`, …) to `error_during_execution`; `success`
  and `error_max_turns` pass through, and an interrupted turn becomes
  `aborted`.
- SDK message types with no Layer-1 representation (`auth_status`,
  system `status` / `hook_response`, …) are dropped by the shim rather
  than forwarded.
- Loss of the shim's stdin (daemon death) is treated as an implicit
  `shutdown`: the input stream is drained and `closed` is emitted with
  `reason: "shutdown"` and no `request_id`.
