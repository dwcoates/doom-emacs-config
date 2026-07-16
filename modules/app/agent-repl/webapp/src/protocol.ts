/**
 * Layer 2 wire protocol — Go daemon ⇄ webapp (WebSocket NDJSON).
 *
 * Source of truth: ../../shared/protocol.md (§2). Frames flow
 * daemon→webapp; webapp→daemon traffic reuses the Layer-1 command shapes
 * plus `replay-request` (§2.10).
 */

export type PermissionMode =
  | "default"
  | "acceptEdits"
  | "bypassPermissions"
  | "plan"
  // CLI-era modes (claude >= 2.1); validated by the CLI itself.
  | "auto"
  | "manual"
  | "dontAsk"
  | "delegate";

export interface Usage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
}

/** One selectable model (§1.2 `models`). */
export interface ModelInfo {
  value: string;
  displayName: string;
  description: string;
}

export interface ContentBlockText {
  type: "text";
  text: string;
}
export interface ContentBlockToolResult {
  type: "tool_result";
  tool_use_id: string;
  content: string | Array<{ type: "text"; text: string }>;
  is_error?: boolean;
}
export type ContentBlock =
  | ContentBlockText
  | ContentBlockToolResult
  | { type: string; [key: string]: unknown };

// --- §2.1 envelope ----------------------------------------------------------

export interface WsEnvelope {
  type: string;
  seq: number;
  ts: string;
  session_id: string;
}

// --- §2.2 lifecycle ----------------------------------------------------------

export interface HelloFrame extends WsEnvelope {
  type: "hello";
  daemon_version: string;
  resume_from_seq: number;
  permission_mode: PermissionMode;
  /**
   * The session's model. NOT frozen at hello: it moves whenever the model
   * moves, and §2.8's `model-changed` carries every move after this.
   */
  model: string;
  /** The `set-model` menu; absent until the shim reports it. */
  models?: ModelInfo[];
  cwd: string;
  /** Durable CLI session uuid (resume target); absent until system:init. */
  claude_session_id?: string;
  /**
   * The in-flight message queue snapshot (§2.13), front-to-back order, so
   * a fresh join or a replay-evicted client rebuilds the pending queue
   * without a gap. Absent when no messages are queued.
   */
  queue?: QueuedItem[];
  /**
   * The daemon's authoritative "a turn is running right now" bit. A
   * transcript-seeded fresh-join replay (§2.11) synthesizes a `result` for
   * answered turns, but a trailing prompt the agent never answered gets
   * none, so its dangling `user-turn` would otherwise leave the store
   * believing a turn is in flight — the topbar timer would then count from a
   * days-old prompt on a cold/rehydrated session. The store trusts this over
   * what the replayed frames imply (see `store.ts`). Absent from a
   * pre-`turn_active` daemon, which the store reads as no turn running.
   */
  turn_active?: boolean;
}

/**
 * One parked message in the in-flight queue (§2.13), as carried in the
 * `hello` snapshot and mirrored in the store's `queued` slice. A queued
 * message is NOT a conversation item: it renders as a subdued "queued —
 * waiting" affordance and only becomes a real `user-turn` once drained.
 */
export interface QueuedItem {
  queue_id: string;
  request_id: string;
  content: ContentBlock[];
  status: "classifying" | "waiting" | "interrupt";
  verdict?: "wait" | "interrupt";
  reason?: string;
}

export type ResultSubtype =
  | "success"
  | "error_max_turns"
  | "error_during_execution"
  | "aborted";

export interface ResultFrame extends WsEnvelope {
  type: "result";
  subtype: ResultSubtype;
  duration_ms: number;
  duration_api_ms: number;
  num_turns: number;
  total_cost_usd: number;
  usage: Usage;
  is_error: boolean;
  result_text?: string;
}

export interface CompactBoundaryFrame extends WsEnvelope {
  type: "compact-boundary";
  trigger: "auto" | "manual";
  pre_tokens: number;
  post_tokens: number;
}

/**
 * A context compaction is IN PROGRESS. The daemon opens the window on the
 * SDK's `status: "compacting"` and the compact-boundary closes it; the SDK
 * reports no progress percentage, so `active` is always true and the GUI's
 * indicator is indeterminate.
 */
export interface CompactStatusFrame extends WsEnvelope {
  type: "compact-status";
  active: boolean;
}

/**
 * The running turn is being interrupted (the gesture reached the shim; the
 * turn ends only when its `aborted` result lands). The store enters an
 * "interrupting" state on this frame: it shows the red "interrupting…"
 * indicator and stops opening NEW bubbles for the tail, while still letting
 * in-flight bubbles finish streaming. Broadcast by the daemon only while a
 * turn is active, and cleared by the terminating result/error or the next turn.
 */
export interface InterruptFrame extends WsEnvelope {
  type: "interrupt";
}

export interface RetryFrame extends WsEnvelope {
  type: "retry";
  attempt: number;
  delay_ms: number;
  reason: string;
  fatal: boolean;
}

export interface ErrorFrame extends WsEnvelope {
  type: "error";
  code: "shim_died" | "sdk_error" | "transport" | "internal";
  message: string;
  recoverable: boolean;
}

// --- §2.3 user turns -----------------------------------------------------------

export interface UserTurnFrame extends WsEnvelope {
  type: "user-turn";
  request_id: string;
  content: ContentBlock[];
}

// --- §2.4 / §2.5 streaming blocks ------------------------------------------------

export interface TextStartFrame extends WsEnvelope {
  type: "text-start";
  block_id: string;
  message_id: string;
  /** Owning subagent call; absent on main-chain blocks. Start-frame only. */
  parent_tool_use_id?: string;
}
export interface TextDeltaFrame extends WsEnvelope {
  type: "text-delta";
  block_id: string;
  text: string;
}
export interface TextEndFrame extends WsEnvelope {
  type: "text-end";
  block_id: string;
  final_text: string;
}

export interface ThinkingStartFrame extends WsEnvelope {
  type: "thinking-start";
  block_id: string;
  message_id: string;
  /** As text-start: the owning subagent call, absent on main-chain. */
  parent_tool_use_id?: string;
}
export interface ThinkingDeltaFrame extends WsEnvelope {
  type: "thinking-delta";
  block_id: string;
  text: string;
}
export interface ThinkingEndFrame extends WsEnvelope {
  type: "thinking-end";
  block_id: string;
  final_text: string;
  signature?: string;
}

// --- §2.6 tool-use cards ----------------------------------------------------------

export interface ToolUseStartFrame extends WsEnvelope {
  type: "tool-use-start";
  tool_use_id: string;
  tool_name: string;
  message_id: string;
  parent_tool_use_id?: string;
}
export interface ToolUseInputDeltaFrame extends WsEnvelope {
  type: "tool-use-input-delta";
  tool_use_id: string;
  partial_json: string;
}
export interface ToolUseInputEndFrame extends WsEnvelope {
  type: "tool-use-input-end";
  tool_use_id: string;
  input: Record<string, unknown>;
}

export type RenderHint =
  | { kind: "bash"; stdout: string; stderr: string; exit_code?: number }
  | { kind: "diff"; file_path: string; unified_diff: string }
  | { kind: "grep"; matches: Array<{ file: string; line: number; text: string }> }
  | { kind: "task"; summary: string }
  // The launched skill's full SKILL.md body, sourced by the daemon so the
  // card can show the skill contents alongside its invocation.
  | { kind: "skill"; content: string };

export interface ToolUseResultFrame extends WsEnvelope {
  type: "tool-use-result";
  tool_use_id: string;
  is_error: boolean;
  content: string | Array<{ type: "text"; text: string }>;
  render?: RenderHint;
}

export interface ToolUseProgressFrame extends WsEnvelope {
  type: "tool-use-progress";
  tool_use_id: string;
  text: string;
  /** The tool's name, raw from the SDK heartbeat. */
  tool_name?: string;
  /** Spawning subagent's tool_use_id; absent on main-chain tools. */
  parent_tool_use_id?: string;
  /** The SDK's raw elapsed clock, for client-rendered tickers. */
  elapsed_seconds?: number;
}

/**
 * Live growth of a detached task's output file: the daemon tails the
 * announced spool file and streams appended text, spawning-call-keyed.
 */
export interface TaskOutputDeltaFrame extends WsEnvelope {
  type: "task-output-delta";
  task_id: string;
  tool_use_id?: string;
  text: string;
}

/**
 * Completion signal of detached background work, parsed from the
 * harness notification. `tool_use_id` names the SPAWNING call, which is
 * what lands the completion on the card that started the work.
 */
export interface TaskNotificationFrame extends WsEnvelope {
  type: "task-notification";
  tool_use_id?: string;
  task_id?: string;
  status?: string;
  summary?: string;
  output_file?: string;
  text: string;
}

// --- §2.7 permission prompts ---------------------------------------------------------

export type PermissionPreview =
  | { kind: "bash"; command: string }
  | { kind: "diff"; file_path: string; unified_diff: string }
  | { kind: "write"; file_path: string; bytes: number; preview: string }
  | { kind: "generic"; summary: string };

export interface PermissionRequestFrame extends WsEnvelope {
  type: "permission-request";
  request_id: string;
  tool_use_id: string;
  tool_name: string;
  input: unknown;
  preview?: PermissionPreview;
}

export interface PermissionResolvedFrame extends WsEnvelope {
  type: "permission-resolved";
  request_id: string;
  decision: "allow" | "deny" | "cancel";
  message?: string;
  updated_input?: unknown;
}

// --- §2.8 usage / mode -----------------------------------------------------------------

export interface UsageFrame extends WsEnvelope {
  type: "usage";
  message_id: string;
  /**
   * The subagent call whose conversation this usage belongs to; absent on
   * main-chain usage. A subagent's request carries the SUBAGENT's context,
   * not the session's, so the store banks an attributed frame on that
   * agent's tool item instead of the session's standing figure.
   */
  parent_tool_use_id?: string;
  usage: Usage;
  cost_usd?: number;
}

export interface PermissionModeChangedFrame extends WsEnvelope {
  type: "permission-mode-changed";
  mode: PermissionMode;
  origin: "user" | "shim" | "daemon";
}

/** The selectable-model menu, for a client already attached when it lands. */
export interface ModelsFrame extends WsEnvelope {
  type: "models";
  models: ModelInfo[];
}

/**
 * The session's model moved. The ONLY frame that moves `model` after the
 * hello, and it fires for every way the model can move — picking one in
 * the topbar is merely the least interesting of them:
 *
 * - `init`:      the SDK's system:init named the model it started on
 * - `user`:      a `set-model` the shim acked
 * - `agent`:     a main-chain assistant message reported a different model,
 *                i.e. the CLI moved it without being asked
 * - `reconcile`: the daemon's periodic transcript check caught a drifted mirror
 */
export interface ModelChangedFrame extends WsEnvelope {
  type: "model-changed";
  model: string;
  origin: "init" | "user" | "agent" | "reconcile";
}

// --- §2.9 system ---------------------------------------------------------------------

export interface SystemFrame extends WsEnvelope {
  type: "system";
  subtype: "init" | "slash_command";
  data: unknown;
}

// --- §2.13 in-flight message queue -------------------------------------------

/**
 * A `user-message` submitted while a turn was in flight was parked rather
 * than forwarded: the daemon assigned it a stable `queue_id`, and it will
 * reach the shim only later, at drain. `status` is always the initial
 * `"classifying"` — the verdict rides a later `queue-classified`.
 */
export interface QueueAddedFrame extends WsEnvelope {
  type: "queue-added";
  queue_id: string;
  request_id: string;
  content: ContentBlock[];
  status: "classifying";
}

/**
 * The classifier's (or a user override's) verdict on a queued item. `wait`
 * keeps the item parked to drain in FIFO order; `interrupt` escalates it to
 * preempt the running turn. `source` names who decided.
 */
export interface QueueClassifiedFrame extends WsEnvelope {
  type: "queue-classified";
  queue_id: string;
  verdict: "wait" | "interrupt";
  reason: string;
  source: "classifier" | "user" | "fallback";
}

/**
 * A queued item left the queue: `drained` (it became the `user-turn` named
 * by `request_id` and went to the shim), `cancelled` (the user removed it),
 * `interrupted` (a user interrupt dropped every parked item so nothing
 * auto-runs after the abort), or `session_end` (the session ended before
 * it ran).
 */
export interface QueueRemovedFrame extends WsEnvelope {
  type: "queue-removed";
  queue_id: string;
  reason: "drained" | "cancelled" | "interrupted" | "session_end";
  request_id?: string;
}

export type L2Frame =
  | HelloFrame
  | ResultFrame
  | CompactBoundaryFrame
  | CompactStatusFrame
  | InterruptFrame
  | RetryFrame
  | ErrorFrame
  | UserTurnFrame
  | TextStartFrame
  | TextDeltaFrame
  | TextEndFrame
  | ThinkingStartFrame
  | ThinkingDeltaFrame
  | ThinkingEndFrame
  | ToolUseStartFrame
  | ToolUseInputDeltaFrame
  | ToolUseInputEndFrame
  | ToolUseResultFrame
  | ToolUseProgressFrame
  | TaskOutputDeltaFrame
  | TaskNotificationFrame
  | PermissionRequestFrame
  | PermissionResolvedFrame
  | UsageFrame
  | PermissionModeChangedFrame
  | ModelsFrame
  | ModelChangedFrame
  | SystemFrame
  | QueueAddedFrame
  | QueueClassifiedFrame
  | QueueRemovedFrame;

/** The set of frame types this client version understands. */
export const KNOWN_FRAME_TYPES: ReadonlySet<string> = new Set([
  "hello",
  "result",
  "compact-boundary",
  "compact-status",
  "interrupt",
  "retry",
  "error",
  "user-turn",
  "text-start",
  "text-delta",
  "text-end",
  "thinking-start",
  "thinking-delta",
  "thinking-end",
  "tool-use-start",
  "tool-use-input-delta",
  "tool-use-input-end",
  "tool-use-result",
  "tool-use-progress",
  "task-output-delta",
  "task-notification",
  "permission-request",
  "permission-resolved",
  "usage",
  "permission-mode-changed",
  "models",
  "model-changed",
  "system",
  "queue-added",
  "queue-classified",
  "queue-removed",
]);

/**
 * Parse one WebSocket text message into a frame. Returns null for
 * unknown types (forward compatibility — but note their seq still
 * advances the store cursor via the raw envelope). Throws on
 * structurally invalid frames.
 */
export function parseFrame(data: string): { envelope: WsEnvelope; frame: L2Frame | null } {
  const parsed = JSON.parse(data) as Record<string, unknown>;
  if (typeof parsed !== "object" || parsed === null || typeof parsed.type !== "string") {
    throw new Error("layer2: frame has no string type discriminator");
  }
  if (typeof parsed.seq !== "number") {
    throw new Error(`layer2: ${parsed.type} frame has no numeric seq`);
  }
  const envelope = parsed as unknown as WsEnvelope;
  return {
    envelope,
    frame: KNOWN_FRAME_TYPES.has(envelope.type) ? (parsed as unknown as L2Frame) : null,
  };
}

// --- webapp → daemon commands (Layer-1 shapes per §2 preamble, + §2.10) -------------

export interface UserMessageCmd {
  type: "user-message";
  request_id: string;
  content: string | ContentBlock[];
  parent_tool_use_id?: string;
}

export interface PermissionDecisionCmd {
  type: "permission-decision";
  request_id: string;
  decision:
    | { behavior: "allow"; updated_input?: unknown; updated_permissions?: unknown[] }
    | { behavior: "deny"; message: string; interrupt?: boolean };
}

export interface InterruptCmd {
  type: "interrupt";
  request_id: string;
}

export interface SetPermissionModeCmd {
  type: "set-permission-mode";
  request_id: string;
  mode: PermissionMode;
}

/**
 * Switch the model mid-flight. `model` is always a concrete id from the
 * `models` menu: the SDK's "back to the default" form is not exposed,
 * because the id it resolves to is unknowable until the next assistant
 * message, and a picker that cannot name what it just selected is worse
 * than one offering only nameable choices.
 */
export interface SetModelCmd {
  type: "set-model";
  request_id: string;
  model: string;
}

export interface ReplayRequestCmd {
  type: "replay-request";
  from_seq: number;
}

/**
 * Escalate a queued item to preempt the running turn — the manual
 * counterpart to a classifier `interrupt` verdict (§2.13). Daemon-handled,
 * never forwarded to the shim; a stale `queue_id` is a no-op ack.
 */
export interface QueueRunNowCmd {
  type: "queue-run-now";
  request_id: string;
  queue_id: string;
}

/**
 * Remove a queued item without ever sending it (§2.13). Daemon-handled;
 * a stale `queue_id` is a no-op ack, not an error.
 */
export interface QueueCancelCmd {
  type: "queue-cancel";
  request_id: string;
  queue_id: string;
}

export type ClientCommand =
  | UserMessageCmd
  | PermissionDecisionCmd
  | InterruptCmd
  | SetPermissionModeCmd
  | SetModelCmd
  | ReplayRequestCmd
  | QueueRunNowCmd
  | QueueCancelCmd;
