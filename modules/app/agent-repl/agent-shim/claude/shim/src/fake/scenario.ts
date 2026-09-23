/**
 * fake/scenario.ts — what a mocked-vendor scenario IS, and what it is handed.
 *
 * # A scenario is a script, not a simulation
 *
 * The mock does not model the vendor; it REPLAYS shapes harvested from it. One
 * `Scenario` is one such replay: selected by the turn's prompt text, it emits a
 * fixed sequence of SDK messages and writes the files the real binary would
 * have written, then ends the turn with a `result`. Nothing in it decides
 * anything — determinism is the point, because the tests that drive it assert
 * exact sequences.
 *
 * # Why the metadata fields are part of the interface
 *
 * `prompt`, `emits`, `writes` and `arms` are not documentation-adjacent: the
 * AGENTS.md prompt→scenario table is GENERATED from them, and a test asserts
 * the table and the registry agree in both directions. A scenario that lies
 * about which conversation.v1 arms it exercises is therefore a lie in the
 * published contract, which is exactly the kind of drift the table exists to
 * prevent.
 */
import type { VendorFiles } from "./vendor-files.js";
import type { CanUseToolLike, PermissionModeLike } from "../sdk/types.js";
import type { ShimLogger } from "../log.js";

/** One content block a fake assistant message can carry. */
export type FakeBlock =
  | { readonly type: "text"; readonly text: string }
  | { readonly type: "thinking"; readonly thinking: string; readonly signature: string }
  | { readonly type: "tool_use"; readonly id: string; readonly name: string; readonly input: Record<string, unknown> }
  | { readonly type: "fallback"; readonly from: { model: string }; readonly to: { model: string } };

/** How one assistant message deviates from the ordinary case. */
export interface AssistantOptions {
  /** Share a message id with an earlier call (a continued API response). */
  readonly messageId?: string;
  /** `stop_reason` on the message and its `message_delta`. */
  readonly stopReason?: string | null;
  /**
   * The vendor's own error CLASS on the message (`SDKAssistantMessageError`).
   *
   * THE ONLY STREAM CARRIER OF A CLASS THE STATUS CANNOT NAME. `api_retry`
   * states it while the vendor is still retrying; for a class it gives up on
   * immediately — `billing_error`, `oauth_org_not_allowed`, `max_output_tokens`
   * — the failed assistant message is where the vendor states it, and without
   * it 402, a second 403 and a status-less failure are indistinguishable.
   */
  readonly error?: string;
  /** `stop_details`, for the refusal shapes that carry one. */
  readonly stopDetails?: Record<string, unknown> | null;
  /** Mark the message as interrupt-truncated (`aborted: true`). */
  readonly aborted?: boolean;
  /** Sidechain attribution: the subagent that produced this message. */
  readonly agent?: SubagentAttribution;
  /** Report a model other than the session's current one (a fallback leg). */
  readonly model?: string;
  /** The `effort` field the transcript line carries. */
  readonly effort?: string;
  /** Suppress the transcript lines (a message the vendor streams but never records). */
  readonly skipTranscript?: boolean;
  /**
   * Suppress the THINKING PRELUDE a tool call and a turn's conclusion carry.
   *
   * OBSERVED IN EVERY CAPTURE: the vendor's first API response of a tool turn
   * is `[thinking, tool_use]` on ONE message id, and its closing response is
   * `[thinking, text]` — a reasoning block precedes both. So the mock emits one
   * by default, and a scenario opts out only where it is deliberately modelling
   * a response the vendor produced without one (a refusal leg, a continuation
   * of an already-open message).
   */
  readonly noReasoning?: boolean;
  /**
   * Stamp the message with a specific instant instead of the mock's clock.
   *
   * FOR SCENARIOS THAT DELIBERATELY LIE ABOUT WHEN, and only about when — the
   * cold-context gate reads the LAST ASSISTANT LINE's `timestamp` and `usage`
   * together, so an old session cannot be seeded by back-dating some other
   * record beside a freshly-stamped assistant line.
   */
  readonly timestamp?: string;
  /**
   * Report a specific CONTEXT SIZE on the message's usage, instead of the
   * mock's ordinary one.
   *
   * FOR SCENARIOS THAT SEED A COLD READ. The cold gate sums the last assistant
   * line's `cache_read_input_tokens + cache_creation_input_tokens +
   * input_tokens`, and it has a floor (`engine/cold.ts`) below which it does
   * not ask. A scenario whose whole purpose is to trip the gate has to state a
   * context above that floor; the mock's ordinary usage is far below it.
   */
  readonly contextTokens?: number;
  /**
   * Another agent's emission that lands IN THE MIDDLE of one of this
   * response's blocks, keyed by block index.
   *
   * THE MAIN AGENT AND ITS BACKGROUND SUBAGENTS SHARE ONE SDK ITERATOR, so a
   * subagent's whole response — its `message_start` included — can arrive
   * between two deltas of the main agent's open block. The emission runs after
   * the block's FIRST delta and before the rest; this response's own stream
   * attribution is restored afterwards.
   */
  readonly interleave?: ReadonlyMap<number, () => void>;
}

/** Who a sidechain message belongs to. */
export interface SubagentAttribution {
  readonly agentId: string;
  readonly parentToolUseId: string;
  readonly subagentType: string;
  readonly taskDescription: string;
}

/** What one emitted assistant message hands back. */
export interface AssistantEmission {
  /** The api message id every block of it shares. */
  readonly messageId: string;
  /** One record uuid per block, in block order. */
  readonly uuids: readonly string[];
}

/** A tool call the scenario announced and can still answer. */
export interface ToolCall {
  readonly toolUseId: string;
  readonly name: string;
  readonly input: Record<string, unknown>;
  /** The uuid of the assistant record that carried the `tool_use` block. */
  readonly assistantUuid: string;
  /** The api message id the block belonged to. */
  readonly messageId: string;
}

/** How one tool result deviates from the ordinary case. */
export interface ToolResultOptions {
  /** Mark the result an error (the model sees a failure). */
  readonly isError?: boolean;
  /** Sidechain attribution, for a result inside a subagent's own run. */
  readonly agent?: SubagentAttribution;
  /** `toolDenialKind` — set by the vendor when a rule denied the call. */
  readonly toolDenialKind?: string;
  /** Suppress the transcript line. */
  readonly skipTranscript?: boolean;
  /** Content blocks instead of a plain string (an image result, say). */
  readonly blocks?: readonly Record<string, unknown>[];
}

/** How the turn ends. */
export interface ResultSpec {
  /** `success` or one of the four declared error subtypes. */
  readonly subtype:
    | "success"
    | "error_during_execution"
    | "error_max_turns"
    | "error_max_budget_usd"
    | "error_max_structured_output_retries";
  /** The settled answer — the turn's conclusion, named by the last text block. */
  readonly result?: string;
  /** The vendor's fine-grained stop taxonomy; where the 16 arms actually live. */
  readonly terminalReason?: string;
  /** `stop_reason` on the result. */
  readonly stopReason?: string | null;
  /** `error_status` for an api-error terminal. */
  readonly apiErrorStatus?: number | null;
  /** The `errors: string[]` an error result carries. */
  readonly errors?: readonly string[];
  /** Tool calls the gate refused this turn. */
  readonly permissionDenials?: readonly {
    tool_name: string;
    tool_use_id: string;
    tool_input: Record<string, unknown>;
  }[];
  /** Report a fast-mode state other than the session's. */
  readonly fastModeState?: "on" | "off" | "cooldown";
  /** Why fast mode is unavailable, when it is. */
  readonly fastModeDisabledReason?: string;
  /** `structured_output`, for the structured-output arms. */
  readonly structuredOutput?: unknown;
  /** Extra model rows in `modelUsage` beyond the session model's. */
  readonly extraModelUsage?: Record<string, Record<string, unknown>>;
}

/** One live detached item the mock is tracking. */
export interface LiveTask {
  readonly taskId: string;
  readonly toolUseId: string;
  readonly kind: "local_bash" | "local_agent" | "local_workflow" | "monitor";
  readonly description: string;
  /** Set once the item has been moved to the background by the user (Ctrl-B). */
  backgrounded: boolean;
}

/**
 * Everything a scenario is allowed to do.
 *
 * Deliberately the ONLY surface: a scenario that reached for `process.env` or
 * wrote a file directly would produce shapes no other scenario shares, and the
 * table's "which files it writes" column would stop being true.
 */
export interface ScenarioContext {
  // -- the turn ------------------------------------------------------------
  /** 1-based turn number within this query. */
  readonly turn: number;
  /** The prompt text, whole. */
  readonly prompt: string;
  /** The prompt text with the `!name` prefix and one space removed. */
  readonly args: string;

  // -- identity and clock --------------------------------------------------
  /** The model the session answers with. */
  readonly model: string;
  /** The gate's mode. */
  readonly permissionMode: PermissionModeLike;
  /** Mint a uuid. Injected so goldens are stable. */
  newUuid(): string;
  /** Milliseconds since the epoch. Injected so goldens are stable. */
  readonly nowMs: () => number;
  /** The current instant as the vendor's ISO-8601 timestamp. */
  nowIso(): string;

  // -- the file plane ------------------------------------------------------
  /** The on-disk writer. */
  readonly files: VendorFiles;
  /** The workspace directory. */
  readonly cwd: string;
  /** `CLAUDE_CONFIG_DIR`. */
  readonly configDir: string;
  /** The spool root. */
  readonly spoolRoot: string;

  // -- raw emission --------------------------------------------------------
  /** Emit one SDK message, uuid and session_id stamped. */
  emit(message: Record<string, unknown>): void;
  /** Emit one `stream_event` wrapping a raw Anthropic stream event. */
  emitStream(event: Record<string, unknown>): void;
  /** End the message stream with no result — a query death by EOF. */
  endStream(): void;
  /** Fail the message stream — a query death by iterator failure. */
  failStream(error: Error): void;

  // -- composed emission ---------------------------------------------------
  /**
   * Emit one assistant API response and record it.
   *
   * ONE TRANSCRIPT LINE PER BLOCK, all sharing `message.id`, in block order —
   * the split the real binary performs and the reason `<message.id>:<index>`
   * addresses a block rather than a line. The SDK side splits identically (the
   * declaration says several assistant messages may share a message id), and
   * every split carries the SAME usage, so the converter's "usage rides block
   * 0" rule has something to pick from.
   */
  assistant(blocks: readonly FakeBlock[], options?: AssistantOptions): AssistantEmission;
  /** Announce one tool call as a single-block assistant message. */
  toolUse(name: string, input: Record<string, unknown>, options?: AssistantOptions): ToolCall;
  /** Answer a tool call: the `user` message plus its `toolUseResult` record. */
  toolResult(
    call: ToolCall,
    content: string,
    toolUseResult: unknown,
    options?: ToolResultOptions,
  ): void;
  /** Write one `attachment` transcript record (no SDK message accompanies it). */
  attachment(attachment: Record<string, unknown>): void;
  /**
   * State a compaction's summary on BOTH planes under `summaryUuid` (the
   * boundary's anchor): the synthetic main-stream `user` record the stream
   * carries right after the boundary, and the transcript's `isCompactSummary`
   * line parented on `boundaryUuid`.
   */
  compactSummary(boundaryUuid: string, summaryUuid: string, summary: string): void;
  /**
   * Emit one `system` SDK message AND its transcript record, under ONE uuid,
   * answering that uuid.
   *
   * `file`, when given, replaces `fields` for the transcript line — the vendor
   * spells some records differently on the two planes (`compact_metadata` on
   * the stream, `compactMetadata` in the file) while keeping one uuid.
   */
  systemRecord(
    subtype: string,
    fields: Record<string, unknown>,
    file?: Record<string, unknown>,
  ): string;
  /** Emit one `system` SDK message with no transcript record. */
  systemMessage(subtype: string, fields: Record<string, unknown>): void;
  /** End the turn. Exactly once per turn. */
  result(spec: ResultSpec): void;

  // -- control -------------------------------------------------------------
  /** The permission callback the shim handed the query. */
  readonly canUseTool: CanUseToolLike;
  /** Park until the turn is interrupted. */
  awaitInterrupt(): Promise<void>;
  /**
   * Park detached work until `AGENT_REPL_FAKE_DETACH_GATE` names a path that
   * exists. A no-op when the env is unset, so an ungated run's timing is
   * byte-for-byte what it always was. There is no interrupt exit: the turn
   * that started this detached work has already concluded, so a caller
   * awaiting this is never a live turn a consumer could interrupt.
   */
  awaitDetachGate(): Promise<void>;
  /**
   * Park until `backgroundTasks` moves this call to the background, then let
   * the caller emit the vendor's detachment record before that verb answers.
   *
   * ORDER IS THE WHOLE POINT. A real Ctrl-B is a vendor-side event: by the time
   * the binary reports the detach, the record that PROVES it — the foreground
   * result carrying `backgroundedByUser` — is already on the stream. A mock
   * that answered first and emitted afterwards would let a consumer observe
   * DetachForeground succeeding against a conversation that still shows the
   * work in the foreground, which is a race no production ordering has.
   *
   * The returned function is the acknowledgement: `backgroundTasks` stays
   * parked until the scenario calls it, so 'the detachment is published' is a
   * happens-before rather than a hope about scheduling.
   */
  awaitBackgrounded(toolUseId: string): Promise<() => void>;
  /** Park for one scheduler turn, so incremental writes are observable. */
  tick(): Promise<void>;
  /** Retire the vendor session identity and mint a new one; answers the new id. */
  rotate(): string;

  // -- minting -------------------------------------------------------------
  /** A `toolu_`-prefixed tool use id. */
  mintToolUseId(): string;
  /** A 9-character base36 shell task id, `b`-prefixed (corpus: `b86pl7ir1`). */
  mintShellTaskId(): string;
  /** A 17-character hex agent id, `a`-prefixed (corpus: `a0cbd94e5da2d662d`). */
  mintAgentTaskId(): string;
  /** An `msg_`-prefixed api message id. */
  mintMessageId(): string;

  // -- the live set --------------------------------------------------------
  /** Announce a task started and add it to the live set. */
  startTask(task: Omit<LiveTask, "backgrounded">): LiveTask;
  /** Announce the live set after a change (REPLACE semantics). */
  announceLiveTasks(): void;
  /** Retire one task from the live set. */
  endTask(taskId: string): void;

  // -- the session's answerable state --------------------------------------
  /** Choose which account-usage answer `usage_EXPERIMENTAL…` gives. */
  setAccountUsageArm(arm: AccountUsageArm): void;
  /** Choose which mcp-server healths `mcpServerStatus()` reports. */
  setMcpArm(arm: "all" | "healthy"): void;
  /**
   * Make `getContextUsage()` answer a GROWING occupancy from now on.
   *
   * The shim samples context usage on its own cadence — at session start and at
   * every turn end — so a scenario cannot push a `context_usage` update. What it
   * CAN do is make the next sample differ from the last, which is the only way
   * to tell a consumer that re-renders on change from one that renders once and
   * never again.
   */
  setContextUsageDrift(drifting: boolean): void;
  /**
   * Set the session's fast-mode state, as the vendor's own toggle does.
   *
   * It STICKS: `sdk.d.ts` carries fast mode on `init` and on every `result`, so
   * a state set here is reported by every later turn's result AND by the init a
   * rotation emits. A per-turn `ResultSpec.fastModeState` states one turn's
   * figure and leaves the session's alone.
   */
  setFastMode(state: "on" | "off" | "cooldown", reason?: string): void;
  /**
   * The VENDOR'S OWN model swap, made persistent for the session.
   *
   * Not `setModel`: that verb is the shim asking, and its answer is a
   * CONFIRMATION. This is the vendor deciding by itself — a refusal fallback —
   * so nothing asked and no confirmation exists. Every later message reports the
   * new model, which is the only evidence the swap happened at all.
   */
  fallbackTo(model: string): void;

  // -- logging -------------------------------------------------------------
  /** Log through `src/log.ts`. Every branch of every scenario logs. */
  log: ShimLogger;
}

/** Which shape `usage_EXPERIMENTAL…` answers with. */
export type AccountUsageArm =
  | "available"
  | "opus_absent"
  | "service_unavailable"
  | "window_unavailable"
  | "utilization_unavailable"
  | "sampling_failure";

/** One registered mocked-vendor scenario. */
export interface Scenario {
  /**
   * The `!name` token that selects it, WITHOUT the bang; `""` for the default
   * plain-prose scenario that any unrecognized text falls through to.
   */
  readonly name: string;
  /** The documented prompt, as a reader would type it. */
  readonly prompt: string;
  /** What the vendor emits — the table's second column. */
  readonly emits: string;
  /** Which files it writes — the table's third column. */
  readonly writes: string;
  /** Which conversation.v1 arms it exercises — the table's fourth column. */
  readonly arms: string;
  /** Emit the turn. */
  run(ctx: ScenarioContext): Promise<void> | void;
}
