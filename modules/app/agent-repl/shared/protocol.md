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
  | "plan"
  // CLI-era modes (claude >= 2.1). The stack passes modes through to
  // the CLI's own validation rather than gatekeeping a stale subset;
  // "auto"/"manual" etc. require the daemon to drive a system CLI that
  // knows them (claude-repld -claude-bin).
  | "auto"
  | "manual"
  | "dontAsk"
  | "delegate";

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
`subtype` is `"aborted"`. At the daemon layer a client interrupt also
clears the in-flight message queue (§2.13) so the abort is total:
nothing parked auto-runs afterward.

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

#### `set-model`

Switch the model the session answers with, mid-flight. Maps to
`query.setModel(model)`.

`model` is a concrete model id (a `value` from the `models` event), never
empty: the SDK's `setModel(undefined)` "restore the default" form is
deliberately NOT exposed, because the id it resolves to is unknowable
until the next assistant message and a topbar that cannot name the model
it just selected is worse than one that offers only nameable choices.

```ts
interface SetModelCmd {
  type: "set-model";
  request_id: RequestId;
  model: string;                      // non-empty model id
}
```

#### `refresh-commands`

Ask the shim to re-resolve the slash-command menu and re-emit `commands`.

The SDK memoizes `supportedCommands()` against the init handshake it
performed at startup, so a skill added or edited mid-session is invisible
to the live query. Re-resolving therefore means the shim stands up a
throwaway query whose prompt never yields: the CLI completes a fresh
handshake that carries the current list and then idles, costing one
process spawn and zero model tokens. The shim `ack`s once the fresh
`commands` event has been emitted.

```ts
interface RefreshCommandsCmd {
  type: "refresh-commands";
  request_id: RequestId;
}
```

#### `refresh-status`

Ask the shim to re-resolve the `/status` snapshot and emit a `status` event.

The SDK bakes the init-handshake fields a `/status` panel reports
(`apiKeySource`, `output_style`, `fast_mode_state`, the MCP/plugin/skill/agent
rosters, ...) into the live query's one init and never re-emits them, so a
value changed mid-session (a `/fast` toggle, a config edit) is invisible to
the live query. Re-resolving works exactly as `refresh-commands` does: the
shim stands up a throwaway query whose prompt never yields, reads the fresh
`system:init` off its stream, and then lets it idle, costing one process
spawn and zero model tokens. The shim `ack`s once the `status` event has been
emitted.

```ts
interface RefreshStatusCmd {
  type: "refresh-status";
  request_id: RequestId;
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
is prepared to accept commands. `ready` is the authoritative carrier of
the handshake metadata (session id, versions, initial permission mode);
the daemon does not gate command forwarding on it. That is safe because
the shim constructs its query synchronously at startup, before it
consumes any stdin line, so every command written to the shim's stdin
from process start is accepted.

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
of its own (`set-permission-mode`, `set-model`, `shutdown`). Commands
whose effect is observable through a dedicated event stream
(`user-message` → stream events, `permission-decision` → the unblocked
turn) do not produce an `ack`. In particular, the daemon emits the
Layer-2 `permission-mode-changed` and `model-changed` frames only after
receiving the `ack` for the corresponding `set-permission-mode` /
`set-model` command.

```ts
interface AckEvt {
  type: "ack";
  session_id: SessionId;
  request_id: RequestId;              // matches the acknowledged command
}
```

#### `models`

The models this session may switch to, from the SDK's
`query.supportedModels()`. Emitted once, unsolicited, after the SDK's
init handshake resolves — the list is a property of the account and CLI,
not of any command, so no `set-model` is needed to learn it.

The daemon caches the list on the translator and republishes it in every
`hello`, so a client attaching later never has to ask for it.

```ts
interface ModelInfo {
  value: string;                      // model id, the `set-model` argument
  displayName: string;                // human label, e.g. "Opus 4.8"
  description: string;
}

interface ModelsEvt {
  type: "models";
  session_id: SessionId;
  models: ModelInfo[];
}
```

#### `commands`

The slash commands this session may invoke, from the SDK's
`query.supportedCommands()` — built-ins plus user, project, and plugin
skills, resolved by the CLI itself under the session's own `cwd`,
`CLAUDE_CONFIG_DIR`, and setting sources. Emitted once, unsolicited, after
the init handshake resolves (like `models`, the list belongs to the
session, not to any command), and again after every `refresh-commands`.

The daemon caches the list on the translator, deduplicated by name (a
skill installed at both user and project scope is resolved once per scope
and reported twice). The menu is read back by the Emacs input panel over
`GET /sessions/{id}/commands`, which completes against it; it is not
forwarded as a Layer-2 frame, because the only client that would render it
is host-owned and reads it over HTTP.

```ts
interface SlashCommand {
  name: string;                       // command name, no leading slash
  description: string;
  argumentHint: string;               // "" when the command takes no argument
}

interface CommandsEvt {
  type: "commands";
  session_id: SessionId;
  commands: SlashCommand[];
}
```

#### `status`

A freshly re-resolved `/status` snapshot, emitted in answer to a
`refresh-status`. `status` is the SDK's `system:init` message verbatim (the
same payload the live init carries), re-read off a throwaway query's
handshake so it reflects the CURRENT config rather than the value frozen at
session start.

The daemon caches it on the translator and pushes it on as the Layer-2
`status` frame; the init the live session already emits warms that cache for
free, so a client that never refreshes still sees the start-of-session
snapshot. The shape is the SDK's to define and grows per release, so it rides
opaque exactly like a `system` event's `data`.

```ts
interface StatusEvt {
  type: "status";
  session_id: SessionId;
  status: unknown;                      // the SDK's system:init, verbatim
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

The three usage-bearing fields scope differently, per the SDK's own cost
accounting: `usage` counts the TOP-LEVEL agent loop only (a subagent's
spend is never added to it), while `total_cost_usd` and `model_usage`
count subagent requests too. The shim holds one long-lived streaming
query per session, so all three are CUMULATIVE across the session, not
per-turn: each result supersedes the previous one rather than adding to
it. `model_usage` is normalized from the SDK's camelCase `modelUsage`
and omitted when the SDK reports none (or an empty map).

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
  usage: Usage;                       // top-level agent loop ONLY
  model_usage?: Record<string, ModelUsage>;  // per-model, subagents INCLUDED
  result?: string;                    // present iff subtype === "success"
  is_error: boolean;
  permission_denials?: Array<{
    tool_use_id: ToolUseId;
    tool_name: string;
    message?: string;
  }>;
}

interface ModelUsage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens: number;
  cache_read_input_tokens: number;
  web_search_requests: number;
  cost_usd: number;
  context_window: number;
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

A user message marked `isReplay` is the SDK re-emitting a message already
in its history; its `type` is a plain `"user"`, so it is indistinguishable
by type alone. The shim skips it whole — acting on it would emit a second
`tool-result` for a `tool_use_id` already reported.

```ts
interface ToolResultEvt {
  type: "tool-result";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  tool_use_id: ToolUseId;
  is_error: boolean;
  content: string | Array<{ type: "text"; text: string }>;
  /** The SDK's `tool_use_result`, verbatim; see below. Absent when omitted. */
  structured?: unknown;
}
```

`content` is only the flattened text the MODEL sees. `structured` is the
SDK's `tool_use_result` — the tool's own JSON result, which the SDK
documents as being "provided to make it easier for applications to present
the tool result in a formatted way". Its shape is per-tool and grows with
the harness, so the shim forwards it verbatim rather than projecting it;
the daemon is what classifies it (§2.6 `async-source`). It is the only
source of every structured fact about a call:

| Tool | `structured` carries |
|---|---|
| `Bash` | `stdout` and `stderr` **separately**, `interrupted`, `returnCodeInterpretation` |
| `Agent` (async spawn) | `isAsync`, `agentId`, `outputFile`, `canReadOutputFile`, `status` |
| `Agent` (settled) | `agentId`, `agentType`, `usage`, `totalTokens`, `totalDurationMs`, `toolStats` |
| `Edit` | `structuredPatch`, `originalFile`, `userModified` |
| `TaskUpdate` | `taskId`, `statusChange: {from, to}`, `updatedFields` |
| `Skill` | `commandName`, `success`, `allowedTools` |

Without it, every one of those facts has to be scraped back out of English
prose, which is what the pre-`structured` daemon and webapp did.

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
    | "slash_command"
    | "task_notification";            // background-task completion; data is { text }
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
section otherwise enumerates only **daemon → webapp** frames. A
client-sent `shutdown` is out of contract: session teardown is
daemon-owned (`DELETE /sessions/{id}`). The daemon decodes a `shutdown`
frame (it is a known Layer-1 command shape) but deliberately drops it
without forwarding — the net effect matches an unknown type, the
mechanism does not.

Every frame carries a `seq` (monotonic, per-session) so the SPA store
can detect drops and request a snapshot replay. Exception: `hello` sits
OUTSIDE the seq stream — it reuses the current watermark without
consuming a seq and is never retained for replay, because it is
connection-scoped rather than part of session history (it can also
reappear mid-connection as the §2.10 eviction fallback). Clients must
not fold `hello` into their seq-gap accounting.

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

`ts` is the time of the **event**, not of the send: frames the daemon
rebuilds from a resumed session's transcript carry the original entry's
timestamp, so a `user-turn` replayed on resume still reports when its
prompt was actually sent, and a replayed `text-start` still reports when
the agent wrote its response. Live frames are stamped as the session hub
retains them.

The webapp stamps both bubbles from this field, into their top-right
corner: the prompt bubble from its `user-turn`, the response bubble from
the `text-start` that opened the block (the block's OPENING time, so the
stamp holds still while the response streams).

### 2.2 Lifecycle frames

#### `hello`

First frame on every connection. Carries the snapshot resume cursor so
the SPA knows whether it joined mid-conversation.

```ts
interface HelloFrame extends WsEnvelope {
  type: "hello";
  daemon_version: string;
  // boot_id identifies THIS daemon process instance: minted at startup,
  // stable across all sessions of the instance, different after every
  // restart. Clients detect a daemon bounce by watching it change. Also
  // present in the GET /sessions envelope alongside protocol_version.
  boot_id: string;
  // Layer-2 wire-compatibility version (Go: protocol.Layer2Version). A
  // client built for a different version must surface the mismatch
  // instead of mis-parsing frames.
  protocol_version: number;
  resume_from_seq: number;            // 0 if no prior history
  permission_mode: PermissionMode;
  model: string;
  models?: ModelInfo[];               // selectable models; absent until §1.2 `models` arrives
  cwd: string;
  claude_session_id?: string;         // durable CLI session uuid; absent until system:init
  turn_active: boolean;               // daemon's authoritative "a turn is running right now"
}
```

`model` and `cwd` start as the session-creation REQUESTED values and are
overwritten by the authoritative `system:init` payload once the SDK
reports in. `claude_session_id` is the CLI-assigned uuid captured from
`system:init` — the DURABLE id usable as a resume target across daemon
restarts, unlike the ephemeral daemon session id in `session_id`.

`model` is NOT frozen at hello, and a client that treats it as frozen
goes stale: it moves whenever the model moves, and §2.8's `model-changed`
is what carries every move after the hello. `models` is the menu the
`set-model` command draws from; it is republished in each hello so a
reconnecting client never has to ask.

`turn_active` is the daemon's authoritative statement of whether a turn is
running RIGHT NOW, and a client must trust it over what the replayed frames
imply. A transcript-seeded fresh-join replay (§2.11) synthesizes a `result`
for every ANSWERED turn, but a trailing prompt the agent never answered is
an incomplete turn that gets none, so the replay ends on a dangling
`user-turn` with nothing to close it. That leaves the client believing a
turn is still in flight, and a cold/rehydrated session revived days after
that unanswered prompt would then paint the topbar task timer counting up
from its stale stamp. `turn_active` false on replay completion is the signal
to drop that phantom turn; a genuinely mid-turn fresh join (a live session's
second tab) reports it true and keeps its running clock.

#### `result`

End-of-turn marker. Drives the SPA "spinner off" + usage chip.

`usage` and `model_usage` carry the §1 result event's scoping verbatim:
`usage` is the top-level agent loop's cumulative session spend (subagents
excluded), `model_usage` the per-model cumulative spend with subagents
included. Both are session-cumulative snapshots — each result supersedes
the previous one. `model_usage` is absent on a synthetic replay result
(the transcript records none) and from a pre-`model_usage` shim.

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
  usage: Usage;                       // top-level agent loop ONLY
  model_usage?: Record<string, ModelUsage>;  // per-model, subagents INCLUDED
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

> **Status: reserved.** No daemon code path produces this frame yet —
> Layer 1 carries no retry information, so emitting it needs either a
> new Layer-1 event or shim-side detection first. Consumers should keep
> handling it (the webapp already does); producers emit nothing today.

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
  code:
    | "shim_died"
    | "sdk_error"
    | "transport"
    | "internal";
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

**Harness-injected spans.** A turn submitted by the Emacs host may carry
text the user never typed: the periodic directive to read the metaprompt
file, and (on a generated workspace's first send) the autonomous-execution
preamble and the wrap-up gate. The host brackets each such span with the
inert markers `<!--agent-repl:meta-->` … `<!--/agent-repl:meta-->`. The
daemon passes them through verbatim — the agent must see the injected text
— and a client hides the bracketed spans, so the bubble shows only what
the user wrote. A turn whose content is entirely bracketed renders nothing.
Turns submitted from the webapp composer carry no markers.

**Slash commands on replay.** The CLI stores a slash-command turn in its
transcript as a tagged envelope (`<command-name>` / `<command-message>` /
`<command-args>`) rather than as the text the user typed. A `user-turn`
rebuilt from a transcript collapses that envelope back to the typed form
(`/clear`, `/model fable`), so a resumed conversation renders its commands
exactly as the live stream did.

#### `user-turn-retracted`

Withdraws the turn named by `request_id`: the client drops its bubble, so
the feed reads as though the prompt had never been sent. This is the undo
half of Emacs's `C-c C-k` — interrupting before the agent has answered is
semantically an undo, and the host lands the withdrawn prompt back in its
input buffer for revision.

```ts
interface UserTurnRetractedFrame extends WsEnvelope {
  type: "user-turn-retracted";
  request_id: RequestId;             // the turn to withdraw
}
```

**When the daemon broadcasts it.** Only on explicit request, and only for
a turn that is *both* the one currently in flight *and* unanswered — no
`text-start`, `thinking-start`, `tool-use-start`, or `tool-use-result` has
gone out for it. Once the agent has answered there is a response on screen
to keep, so the turn stands. Naming the turn by `request_id` is what keeps
the retraction honest about which prompt it drops: a caller that raced a
queue drain asks for a turn that is no longer active, and is refused rather
than given the wrong bubble. A turn is retracted at most once.

**Requesting it.** `POST /sessions/{id}/interrupt` with a body of
`{"retract_request_id": "<id>"}`; the reply is `{"retracted": <bool>}`,
saying whether the turn was actually withdrawn. A caller only restores a
prompt the feed actually gave up. The body is optional — a bare interrupt
keeps its no-body contract and never retracts. (As with §2.13's queue
routes, the route is named here because clients key off it.)

**It is a frame, not a ring rewrite.** The retained ring (§2.10) stays
append-only, so the withdrawal survives replay the way every other state
change does: a client applying `[user-turn, interrupt, user-turn-retracted]`
in seq order lands on the same feed live or replayed.

**The turn still ends in a `result`.** The interrupt is what stops the
turn, so its `aborted` result arrives as usual and remains the frame that
ends the turn and clears the interrupting indicator. A client folds that
result into the retraction rather than rendering it — a bubble reporting
that the turn aborted would be the one trace of the prompt left on a feed
that just dropped the prompt itself. The retraction therefore deliberately
leaves the interrupting state standing, so the aborting turn's tail keeps
being kept out of new bubbles for the whole window.

**Not durable across transcript seeding.** The retraction lives in the ring
and in the CLI's transcript it does not: a session reseeded from transcript
(§2.11) rebuilds the withdrawn prompt's `user-turn` from the CLI's own
record, and the bubble returns. The prompt likewise stays in the agent's
context — this withdraws the turn from the FEED, not from the conversation.

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
`parent_tool_use_id` names the subagent tool call the block belongs to
and is absent on main-chain blocks — only the start frame carries it,
since delta/end key by `block_id` into a block that already knows.

```ts
interface TextStartFrame extends WsEnvelope {
  type: "text-start";
  block_id: string;                   // daemon-assigned, stable
  message_id: string;                 // SDK message.id
  parent_tool_use_id?: string;        // subagent blocks only
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
  parent_tool_use_id?: string;        // subagent blocks only (as §2.4)
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
    // Launched skill's SKILL.md body, read from disk by the daemon so the
    // Skill card can show the full skill contents alongside its invocation.
    | { kind: "skill"; content: string }
    // One transition of a harness task, off the SDK's structured
    // statusChange (§1.2). The spawning TaskCreate's card folds these in as
    // its stream (§2.6 `async-source`, kind "task").
    | {
        kind: "task-update";
        task_id: string;
        status_from: string;
        status_to: string;
        fields?: string[];
      };
}
```

`bash`'s `stdout` and `stderr` come from §1.2 `structured`, which is the
only place the two are kept apart — `content` is the blob the SDK hands the
model, with both already spliced together. A shim predating `structured`
falls back to that blob as `stdout`. `exit_code` has no producer: the SDK
reports `interrupted` and a nullable `returnCodeInterpretation`, neither of
which is an exit code, and inventing one would be worse than a dash.

**Task and Agent take no hint.** They used to take a `task` hint whose
`summary` was the result truncated to 200 bytes. That saved nothing — the
full result ships in `content` on the same frame either way — and cost the
reader everything past the cap, so the card now renders `content` whole.

#### `tool-use-progress`

Optional intermediate progress (long-running tools, background bash).
`text` is the display line the daemon synthesizes; the structured fields
travel alongside it so clients can attribute the heartbeat to a subagent
(`parent_tool_use_id`, absent on main-chain tools) and render their own
elapsed clock (`elapsed_seconds`, raw from the SDK).

```ts
interface ToolUseProgressFrame extends WsEnvelope {
  type: "tool-use-progress";
  tool_use_id: ToolUseId;
  text: string;
  tool_name?: string;
  parent_tool_use_id?: ToolUseId;
  elapsed_seconds?: number;
}
```

#### `async-source`

Declares that ONE TOOL CALL OWNS A STREAM: work that outlives the call and
keeps producing after it settles. Rides immediately after the
`tool-use-result` that spawned it, so a client applying the batch in order
already holds the card the source attaches to.

This is the seam the expanded-bubble view is built on. Before it, every
layer answered "does this call own a stream?" independently, by regex over
the English prose in the result text — so a CLI wording change silently
disabled tailing, the poll route, and every fold at once, with no error
(no match is indistinguishable from no spawn). The answer is now derived
once, structurally, from §1.2 `structured`.

**One frame per spawn — a descriptor, never a stream.** Bulk content keeps
its existing transport (`task-output-delta` for shells, the poll route for
agent transcripts), so the retention ring pays a single frame no matter how
loud the work is.

```ts
interface AsyncSourceFrame extends WsEnvelope {
  type: "async-source";
  tool_use_id: ToolUseId;             // the spawning call
  source: {
    source_id: string;                // agentId | backgroundTaskId | workflow task id
    kind: "agent" | "shell" | "workflow" | "task";
    label?: string;                   // one line naming the work
    status: "running" | "done" | "error" | "killed";
    stream?: {
      transport: "ws" | "poll" | "frames";
      format: "jsonl-transcript" | "jsonl-journal" | "events" | "text";
    };
  };
}
```

`format` selects the CLIENT's renderer, and is why the fold generalizes
without forcing every type into one mould:

| `format` | Stream is | Renders as |
|---|---|---|
| `jsonl-transcript` | a background agent's full JSONL transcript | **nested bubbles**, identical to an inline subagent |
| `jsonl-journal` | a workflow run's `journal.jsonl` records | structured rows |
| `events` | discrete state transitions already on the wire | structured rows |
| `text` | a backgrounded shell's spool bytes | a `<pre>` |

A shell's spool is unstructured bytes with nothing to recover, so it stays
a `<pre>`; rendering it as bubbles would mean inventing structure the data
does not have. `transport: "frames"` means the stream is already on the
wire as ordinary tool-use frames and the client correlates it itself — no
transport is needed at all.

A `status` outside the closed enum reads as `running`: a fold that wrongly
says "done" hides live output, while one that wrongly says "running" only
spins a beat too long.

Absent whenever the shim predates §1.2 `structured`, in which case the
daemon's prose-regex discovery still tails the work — only the descriptor
is missing.

#### `task-output-delta`

Live growth of a detached task's output file. The daemon tails the
file the spawn result announced — confined to the harness task spool
(`/tmp/claude-<uid>/**/tasks/*.output`), coalesced to at most one frame
per poll tick, and budgeted (a 64KB cap ends the stream with a
truncation notice). `tool_use_id` names the spawning call so clients
append the text onto that card.

```ts
interface TaskOutputDeltaFrame extends WsEnvelope {
  type: "task-output-delta";
  task_id: string;
  tool_use_id?: ToolUseId;            // the spawning call
  text: string;                       // appended bytes, UTF-8 safe
}
```

#### `task-notification`

The completion signal of detached background work (a backgrounded Bash,
a background agent, a workflow), parsed from the harness notification
that rode in on a user message. `tool_use_id` names the SPAWNING tool
call verbatim from the payload, so a client lands the completion on the
exact card that started the work; `text` keeps the whole raw
notification for anything the tags miss.

```ts
interface TaskNotificationFrame extends WsEnvelope {
  type: "task-notification";
  tool_use_id?: ToolUseId;            // the spawning call
  task_id?: string;
  status?: string;                    // e.g. "completed"
  summary?: string;
  output_file?: string;
  text: string;                       // raw notification payload
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

A subagent's request carries the SUBAGENT's context, not the session's,
so its usage rides ATTRIBUTED: `parent_tool_use_id` names the spawning
call, the SPA banks the figure on that agent's bubble topbar, and only
a bare (unattributed) frame moves the session token count.

```ts
interface UsageFrame extends WsEnvelope {
  type: "usage";
  message_id: string;
  parent_tool_use_id?: string;  // owning subagent call; absent on main-chain
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

#### `models`

The selectable-model menu (§1.2 `models`), forwarded the moment the shim
reports it so a client already attached populates its picker without
reconnecting. The same list rides on every subsequent `hello`.

```ts
interface ModelsFrame extends WsEnvelope {
  type: "models";
  models: ModelInfo[];
}
```

#### `model-changed`

The session's model moved. This is the ONLY frame that moves `model`
after the hello, and it fires for every way the model can move —
selecting one in the UI is merely the least interesting of them:

| `origin`    | what moved the model                                                     |
|-------------|--------------------------------------------------------------------------|
| `user`      | a `set-model` command, emitted once the shim `ack`s it                    |
| `agent`     | a main-chain assistant message reported a different `model` than the mirror — the agent switched itself (`/model`, a fallback, a downgrade under load) |
| `reconcile` | the daemon's periodic transcript check (§2.11) caught a drifted mirror    |

`agent` exists because the model is NOT the daemon's to decide: the CLI
owns it and can move it without being asked. The mirror therefore FOLLOWS
observed truth rather than asserting remembered truth.

Only MAIN-CHAIN assistant messages count. A subagent's message reports the
subagent's own model (a Haiku `Explore` under an Opus session), and
letting that through would flip the topbar to Haiku for the length of every
subagent — the same reason the Emacs mode-line filters `isSidechain`.

```ts
interface ModelChangedFrame extends WsEnvelope {
  type: "model-changed";
  model: string;
  origin: "user" | "agent" | "reconcile";
}
```

### 2.9 System / slash-command frames

Layer-1 `system` events with subtype `tool_use_progress` or
`task_notification` are NOT forwarded here; the daemon maps them to the
dedicated `tool-use-progress` and `task-notification` frames (§2.6)
instead.

```ts
interface SystemFrame extends WsEnvelope {
  type: "system";
  subtype: "init" | "slash_command";
  data: unknown;                      // pass-through from Layer-1 SystemEvt.data
}
```

### 2.9a Status snapshot

The `/status` snapshot the GUI's status panel renders: the SDK's
`system:init` payload. The live init warms the daemon's cache for free
(read back over `GET /sessions/{id}/status`), so this frame is pushed ONLY
by a `refresh-status` re-probe — a mid-session change (a `/fast` toggle, a
config edit) the frozen init would miss. An open panel re-renders on it
without re-fetching; `model` and permission mode are NOT folded in, since
the webapp already tracks those live from their own frames and overlays the
fresher ones it holds.

```ts
interface StatusFrame extends WsEnvelope {
  type: "status";
  snapshot: unknown;                  // pass-through from Layer-1 StatusEvt.status
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
- `replay-request` is honored on TERMINAL sessions too (every other
  client command is rejected once the session ends): a client attaching
  after the shim exited must still be able to rebuild the retained
  history instead of staring at an empty feed.

**Transcript seeding (resumed sessions).** A session created with
`resume` pre-populates its retention window before the shim runs: the
daemon reads the resumed session's transcript JSONL
(`$CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<uuid>.jsonl`, default config
dir `~/.claude`) and translates its user/assistant entries into the
ordinary §2.3–§2.6 frames, closing each completed turn with a synthetic
§2.4 `result` (`subtype: "success"`) and the whole seed with one §2.8
`usage` frame carrying the last assistant message's usage. The
per-turn result is synthesized because the transcript records no result
event (the CLI's `result` is stream-only), yet the SPA keys the green
final-response border off a text block immediately followed by a
`result`; without it the border is lost on every resumed session until
the first live turn. A result closes the PRIOR turn, so one is emitted
before each new user prompt and once at end-of-input; a bare prompt the
agent never answered is an incomplete turn and gets none. A user entry
carrying a `<task-notification>` payload (the harness's background-work
completion, `origin.kind: "task-notification"`) is NOT a prompt: it
replays as a §2.6 `task-notification` frame rather than a `user-turn` —
the live shim never surfaces it as a user turn, so a replayed prompt
bubble full of raw notification XML was a replay/live divergence — while
still closing the prior turn and opening a new (promptless) one, exactly
as the live stream's real results bracket a notification-woken turn.
Rationale: the CLI restores context
on `--resume` but re-emits no history through the stream, so without
seeding every binding recreation (daemon restart, Emacs restart,
vterm→gui frontend switch) attaches to a blank conversation. The seed
also stamps `claude_session_id` (the resume target) and `model` into
the hello instead of leaving them empty until the first live turn.
Sidechain (subagent) and meta transcript entries are skipped; a
missing or unreadable transcript degrades to the old blank-history
behavior and is logged, never fatal.

**Resume viability gate.** Before spawning the shim, the create path
stats the resume target's transcript. When it is absent (an id minted
inside the Docker sandbox, under another `CLAUDE_CONFIG_DIR`, or whose
transcript was deleted), the create is HARD-FAILED with HTTP 422 and a
structured JSON body — `{ "code": "resume_transcript_missing",
"resume_id": <target>, "searched_paths": [<stat'd path>], "error":
<message> }` — and NO session is spawned. Silently downgrading to a
fresh conversation (the former behavior) buries a genuinely lost
session, and the CLI would in any case hard-exit `fatal_error` on the
doomed `--resume`. The Emacs client reacts to `resume_transcript_missing`
by opening an investigation workspace for the lost session (searching
both config dirs) and surfacing the loss loudly and non-recoverably.
Fake sessions (per-create `fake` or the daemon-wide `-fake` flag) skip
the gate: the scripted SDK has no transcripts by design.

### 2.11 Restart rehydration

Daemon session ids (`s_<hex>`) survive daemon restarts. The daemon
persists a per-session registry record (`$AGENT_REPL_STATE_DIR/
claude-repld-sessions.json`, crash-safe write-through) carrying the
s_ id, cwd, model, permission mode, and the durable
`claude_session_id`; on boot it re-registers every non-terminal record
whose transcript still exists as a **rehydratable** session under its
ORIGINAL s_ id. No shim is spawned at boot. The first ATTACH
(`GET /sessions/{id}/stream`) MATERIALIZES the record as a hibernated
session (§2.14): the replay ring is seeded from the transcript so the
whole conversation renders, but no shim is spawned — merely viewing a
restored workspace stays free. The shim launches with
`--resume <claude_session_id>` only on the first ACT
(`POST /sessions/{id}/message`, `POST /sessions/{id}/interrupt`, or a
`user-message`/`interrupt`/`permission-decision`/`set-*` frame over the
stream socket), which revives the session per §2.14. A client holding a
pre-restart id therefore reconnects to the same conversation without
ever observing "session gone".

Wire surface (documented here despite the HTTP-route non-goal below,
because clients key off it): entries in the `GET /sessions` listing
carry `rehydratable: true` while a session is cold — id resolvable,
shim not yet spawned. The flag is absent/false on warm sessions.
Records whose transcript has vanished (and records that never learned
a `claude_session_id`) are pruned rather than rehydrated into a doomed
`--resume`; their ids then report unknown, which routes clients to
their own resume-rebind path. Daemon-wide teardown (`SIGTERM`) leaves
records non-terminal on purpose — only session-scoped ends (DELETE,
shim death, SDK end) mark a record terminal and stop it rehydrating.
Fake sessions are never registered, and a `-fake` daemon neither
rehydrates nor prunes.

### 2.12 Model drift reconciliation

The daemon's `model` is a MIRROR of a value the CLI owns, and every
mirror can drift. The event path (`system:init`, `set-model` acks,
main-chain assistant messages) is what normally keeps it true, but each
of those is a push: a dropped, malformed, or never-sent event leaves the
mirror confidently wrong, and a wrong model in the topbar is the kind of
wrong nobody notices until it has cost them a turn.

So the mirror is also PULLED, every 30s, from a source that owes nothing
to the event stream: the CLI's own transcript JSONL — the same file the
Emacs mode-line has always trusted, which is exactly why reconciling
against it means the two frontends cannot disagree.

- Read the last `32 KiB` of `<configDir>/projects/<cwd-slug>/<uuid>.jsonl`
  (a tail read: transcripts grow without bound, the answer is at the end).
- Take the `message.model` of the last `type: "assistant"` line that is
  neither `isSidechain` nor `isMeta` — main-chain only, per §2.8.
- Differs from the mirror? Adopt it and broadcast `model-changed` with
  `origin: "reconcile"`. Agrees? Emit nothing.

Silence is the steady state: the check broadcasts only on genuine drift,
so a healthy session pays one tail read per 30s and puts no frame on the
wire. A session whose `claude_session_id` has not arrived yet (no init) has
no transcript path to read, and is skipped rather than guessed at.

This is a self-healing loop, NOT a fallback that papers over a broken
event path: a `reconcile`-origin frame is evidence the push path missed
something, and it is visible on the wire precisely so it can be noticed.

### 2.13 In-flight message queue

A `user-message` submitted while a turn is already in flight is not
forwarded to the shim immediately. The daemon owns a per-session FIFO
**queue** and enforces one invariant: **at most one turn is ever in
flight at the shim.** This makes the daemon — not the SDK's opaque
streaming-input buffer — the authority on ordering, so a queued message
is inspectable, cancellable, and re-orderable, and the shim's
turn/result accounting stays 1:1 (no Layer-1 / shim change is needed).

**Submission decision** (`user-message` in `HandleClientFrame`):

- **Idle** (`!turnActive`): forwarded immediately exactly as before —
  broadcast `user-turn`, set `turnActive`, `SendRaw` to the shim. No
  queue involvement.
- **Busy** (`turnActive`): NOT forwarded. The daemon assigns a stable
  `queue_id`, stores the raw command, broadcasts `queue-added`
  (`status: "classifying"`), and launches an async classifier
  (see below). The command reaches the shim only later, at drain.

**Classifier.** A cheap headless model call decides whether the queued
message should preempt the running turn. It runs in a goroutine (never
under `s.mu`) and re-acquires the lock to apply its verdict.

- Spawns `claude -p --model <classifier-model> --output-format json
  --json-schema <schema>` with the message text on stdin, `cwd` a
  neutral temp dir (so its `SessionStart`/`Stop` hooks never resolve to
  a registered workspace — mirrors `prompt-summary.el`), and
  `CLAUDE_CONFIG_DIR` set to the session's account dir.
- Schema: `{verdict: "interrupt"|"wait", reason: string}` both required.
- Prompt is injection-hardened: the running-task text and the new
  message are DATA, not instructions; no tools; one-line reason.
- Interrupting is framed as non-destructive: an `interrupt` only
  delivers the message NOW rather than after the turn, and the agent
  re-plans the prior work from it (continuing, adjusting, reordering,
  or dropping it). So the prompt treats conditional stops (`stop if
  X`), ordering/sequencing constraints (`do X before Y`), and added
  scope the running task should respect as `interrupt`, and breaks
  genuine ambiguity toward `interrupt` rather than `wait`.
- **Fails closed to `wait`** on any error, timeout (~20s), non-zero
  exit, or unparseable output — a classifier failure must NEVER
  interrupt live work. A failure still emits a `queue-classified`
  frame (`source: "fallback"`) rather than being swallowed.
- Model default `haiku`; the whole feature is behind a daemon flag
  defaulting on (`--classify-queue` / `AGENT_REPL_CLASSIFY_QUEUE`).

**Verdict application:**

- `wait`: broadcast `queue-classified` (`verdict: "wait"`). The item
  stays queued and drains in FIFO order when a turn completes.
- `interrupt`: broadcast `queue-classified` (`verdict: "interrupt"`),
  move the item to the queue front, and — if a turn is in flight —
  send an `interrupt` to the shim. The aborted turn's `result` then
  triggers the normal drain, which now finds this item at the front.
  This is the ONLY path that preempts a running turn.

**Drain** (on every `result` frame, i.e. any turn end including
aborted): if `!turnActive` and the queue is non-empty, pop the front
item, run it through `OnUserMessageCmd` (which broadcasts its
`user-turn` and sets `turnActive`), broadcast `queue-removed`
(`reason: "drained"`, carrying the `request_id` of the `user-turn` it
became), and `SendRaw` it to the shim. A verdict that arrives after
its item has already drained is discarded (the item is gone).

**User interrupt clears the queue.** A client `interrupt` command
(webapp Esc, Emacs `C-c C-k`) means "stop everything": before the
interrupt is forwarded to the shim, every queued item is dropped with
`queue-removed` (`reason: "interrupted"`), so the aborted turn's
`result` finds the queue empty and nothing auto-starts — the session
stays silent until the user's next prompt. The queue-escalation
interrupt (a classifier `interrupt` verdict or `queue-run-now`) is
daemon-originated and does NOT clear the queue: its whole point is
that the promoted front item drains after the abort.

**User overrides** (daemon-handled Layer-2 commands, NOT forwarded to
the shim, reusing the command-shape convention like `replay-request`):

```ts
interface QueueRunNowCmd { type: "queue-run-now"; request_id: RequestId; queue_id: string; }
interface QueueCancelCmd { type: "queue-cancel"; request_id: RequestId; queue_id: string; }
```

- `queue-run-now` is a manual `interrupt` verdict: same escalation as a
  classifier `interrupt`, with `queue-classified` `source: "user"`.
- `queue-cancel` removes the item without ever sending it and
  broadcasts `queue-removed` (`reason: "cancelled"`). A stale
  `queue_id` (already drained/cancelled) is a no-op ack, not an error.

HTTP equivalents for the Emacs host (which holds no WebSocket):

- `POST /sessions/{id}/queue/{queueId}/run-now`
- `POST /sessions/{id}/queue/{queueId}/cancel`

**Frames (daemon → client):**

```ts
interface QueueAddedFrame extends WsEnvelope {
  type: "queue-added";
  queue_id: string;                   // daemon-assigned, stable for the item's life
  request_id: RequestId;              // the user-message id it will submit under
  content: ContentBlock[];            // normalized, same shape as user-turn
  status: "classifying";              // always the initial status
}
interface QueueClassifiedFrame extends WsEnvelope {
  type: "queue-classified";
  queue_id: string;
  verdict: "wait" | "interrupt";
  reason: string;                     // one-line human explanation
  source: "classifier" | "user" | "fallback";
}
interface QueueRemovedFrame extends WsEnvelope {
  type: "queue-removed";
  queue_id: string;
  reason: "drained" | "cancelled" | "interrupted" | "session_end";
  request_id?: RequestId;             // present when reason === "drained"
}
```

**Snapshot on (re)connect.** `hello` (§2.2) and the `GET /sessions`
listing gain a `queue` array in front-to-back order so a fresh join or
a replay-evicted client rebuilds the pending queue without a gap:

```ts
interface QueuedItem {
  queue_id: string;
  request_id: RequestId;
  content: ContentBlock[];
  status: "classifying" | "waiting" | "interrupt";
  verdict?: "wait" | "interrupt";
  reason?: string;
}
```

Live frames still ride the seq stream and the retention ring like every
other broadcast, so a seq-gap replay reconstructs the queue too. On
session end (terminal / shutdown / shim death) every queued item is
dropped with `queue-removed` (`reason: "session_end"`) and any
in-flight classifier goroutine no-ops when it finds the item gone.

**UI intent.** A queued item renders as a distinct, visually subdued
"queued — waiting" affordance (NOT a live turn bubble), carrying its
classifier verdict/reason once known, plus a Cancel control and a
Run-now control. The whole point of the feature is that the user SEES
a message is parked for later handling and is explicitly NOT
interrupting the current task unless escalated.
### 2.14 Idle hibernation

Each live session pins a `node` shim plus a `claude` CLI (~500MB
resident). A daemon serving many workspaces would hold that for every
session ever opened, since nothing reaps an idle one. Hibernation frees
the process pair for sessions that have gone idle, WITHOUT tearing the
session down: the ring, the translator, the attached clients, and the
non-terminal registry record all survive, so the session stays listed,
still answers `replay-request`, and revives on the next act.

**Idle** is measured from the last REAL activity — a shim event or an
acting client command (`user-message`, `interrupt`, `permission-decision`,
`set-permission-mode`, `set-model`). Attaching, detaching, replaying, and
listing deliberately do NOT count: a workspace the user merely switches to
(which holds its stream socket open in the background indefinitely) must
still be allowed to go idle, or nothing would ever be reclaimed. A
periodic sweeper hibernates every session idle past `-idle-timeout`
(default 10m; 0 disables).

A session is **skipped** by the sweeper (re-checked next pass, never an
error) when it has no `claude_session_id` yet (no `--resume` target, so
suspending would destroy it), a turn is in flight, or a permission request
is pending (that state lives only in memory, never in the transcript).
Hibernation is cooperative: the daemon sends the shim a `shutdown` so the
CLI flushes its transcript first, escalating to a hard kill only if the
shim ignores it past a grace window.

**Revival** is lazy and act-triggered. An ATTACH
(`GET /sessions/{id}/stream`) never revives — it replays the retained ring
and holds the socket, all free. Only an ACT (an HTTP `message`/`interrupt`,
or an acting frame over the stream socket) spawns a fresh shim with
`--resume`. Nothing is re-seeded on revival: history was never dropped, so
attached clients observe only a brief pause.

Wire surface: a hibernated session carries `hibernated: true` in the
`GET /sessions` listing and stays `terminal: false` — frontends MUST keep
treating it as live, or their reconnect/reattach logic would resurrect it
into a fresh session. Unlike a `rehydratable` cross-restart record, a
hibernated session is a real in-memory session that has simply shed its
process pair.

---

## Account selection (`CLAUDE_CONFIG_DIR`)

WHICH Claude account a session's CLI runs as is chosen **per session, by
Emacs**, and travels to the daemon in the `config_dir` field of the
`POST /sessions` body.

This cannot be left to the daemon's own environment. One daemon serves
every workspace, so its environment can encode at most one account, while
account selection is a function of the project directory:
`agent-repl--compute-config-dir` (session.el) returns
`~/.claude-chesscom` for a project under `$MULTI_REPO_ROOT` and
`~/.claude` (the CLI's own default, sent as an absent field) elsewhere.
The vterm frontend applies the same resolver as a shell env prefix on its
start command, so both frontends land on the same account for the same
project.

- **To the CLI**: `config_dir` becomes `CLAUDE_CONFIG_DIR` in the shim's
  spawn environment (`server.ShimEnv`), which the SDK's claude
  subprocess inherits. An empty `config_dir` exports NOTHING rather than
  an empty override, which the CLI would read as a config root literally
  named `""`.
- **To transcript lookup**: every `TranscriptPath` call resolves its root
  through `session.ClaudeConfigDir(<the session's dir>)`, never the
  daemon's own env. A session whose CLI writes into `~/.claude-chesscom`
  has no transcript under `~/.claude`, so resolving against the daemon's
  environment would fail the resume-viability gate and silently downgrade
  a resume into a fresh conversation.
- **Across restarts**: the registry record persists `config_dir`, so a
  rehydration stats the transcript under the same root that minted it.

---

## Agent-state sentinels (daemon → Emacs side channel)

Not part of the NDJSON wire: a file-based side channel through which
the daemon feeds the Emacs tab-bar state machine for gui sessions. It
piggybacks on the sentinel-file contract the Claude Code hook scripts
already use, so the Emacs consumer (file-notify watcher + 1 Hz poll
fallback + filename-prefix dispatch) is unchanged machinery.

- **Directory**: `$AGENT_REPL_STATE_DIR/workspace-notifications`,
  defaulting to `~/.claude-emacs/workspace-notifications`. The daemon
  resolves this from its **inherited environment** (it is spawned by
  Emacs), which is the same resolution the hook scripts perform —
  daemon and hooks therefore always agree on the directory.
- **File format**: byte-identical to the hook scripts' output — three
  `\n`-terminated lines: the session `cwd`, the durable claude session
  id (may be empty before `system:init`), then the **ownership marker**
  (`owned`, or empty).

  The marker exists because the hooks are keyed on `cwd`: a FOREIGN
  claude — a terminal session the user runs by hand inside a
  workspace's directory — fires the same hooks and, without it, stamped
  ITS session id onto the workspace. Since the workspace's durable id is
  the resume target for both frontends, that hijack silently made
  `SPC o c` (and every daemon-bounce reattach) resume the wrong
  conversation.

  Emitters stamp `owned` when the CLI is module-launched: the vterm
  start command exports `AGENT_REPL_OWNED=1`, the daemon's shim spawn
  passes it in the shim's (and therefore the SDK's claude subprocess's)
  environment, sandboxed sessions are module-launched by definition
  (`DOOM_SANDBOX=1`), and the daemon's own sentinel writer marks
  unconditionally. Emacs adopts a session id ONLY from a marked
  sentinel; unmarked sentinels still drive state transitions
  (thinking/done/permission) but never identity. Legacy two-line files
  parse as unowned — the conservative reading.
- **Write strategy**: a single direct `write()` to the final filename.
  A same-directory tmp+rename is forbidden: the Emacs watcher runs on
  kqueue where the rename arrives as an ignored `renamed` action, and
  the tmp file's own creation event would dispatch a partial file.
- **Daemon-written events** — exactly the set no hook ever produces
  for SDK sessions (dedup with hooks is by disjoint event sets):

  | filename | trigger |
  |---|---|
  | `permission_request_<sid>_<reqid>` | `permission-request` (Layer-2 frame at the broadcast tap) |
  | `permission_resolved_<sid>_<reqid>` | `permission-resolved` (webapp decision or turn-end/interrupt/close/death auto-cancel) |
  | `session_dead_<sid>` | `error` with `code: "shim_died"` (both abnormal death paths; never on clean shutdown) |
  | `account_changed_<sid>` | `POST /sessions/{id}/account` relaunched the session under another canonical root |

  Everything else (`prompt_submit_*`, `stop_*`, `subagent_*`,
  `stop_failure_*`, `session_start_*`) keeps coming from the real
  global hooks, which fire for SDK sessions.

  `account_changed_<sid>` is the one entry that is not a broadcast tap:
  it is written by the server's account-switch endpoint rather than the
  session frame stream. It is a POKE, not a payload — Emacs fetches the
  authoritative config dir back via `GET /sessions/{id}/account` and
  stores it as the workspace's account override, so daemon and Emacs can
  never disagree about the new account. (The retired `login_request_`
  channel, which once relayed the login to an Emacs vterm, is drained by
  Emacs without action; the daemon runs the login on its own pty — see
  `daemon/internal/login`.)
- **Consumer gates**: the Emacs handlers are state-gated and
  idempotent (`permission_request` acts only from `:thinking`,
  `permission_resolved` only from `:permission`), which makes
  duplicate delivery and hook/daemon interleavings order-independent.

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
