/**
 * sdk/types.ts — THE shim's boundary types for the Claude Agent SDK.
 *
 * # Why these are aliases of the SDK's own declarations, not hand-written copies
 *
 * The shim used to mirror the vendor's shapes structurally (`SdkMessageLike`
 * was `{ type: string; [key: string]: unknown }`). That compiles against ANY
 * SDK, which sounds robust and is the opposite: a vendor upgrade that renames
 * `task_started` or reshapes `usage` typechecks perfectly and fails at runtime,
 * in production, as a silently unconverted message.
 *
 * So every type the shim can source from `sdk.d.ts` IS the SDK's type, imported
 * with `import type` (erased at build time — this file is NOT a vendor import
 * site and `vendor-guard.ts` remains the only one). A vendor upgrade that
 * removes or reshapes any of them is a TYPE ERROR here, at build time, which is
 * the whole point: this file is the upgrade canary's surface.
 *
 * Where the shim needs LESS than the SDK declares — `QueryLike` is the subset
 * of `Query` the shim actually drives, so tests and the mocked vendor can
 * implement it without impersonating 30 control verbs — the narrower type is
 * declared here and a compile-time assertion proves the real `Query` still
 * satisfies it.
 */
import type {
  AccountInfo,
  AgentInfo,
  BackgroundTaskSummary,
  CanUseTool,
  EffortLevel,
  McpServerStatus,
  ModelInfo,
  PermissionMode,
  PermissionResult,
  PermissionUpdate,
  Query,
  SDKAPIRetryMessage,
  SDKAssistantMessage,
  SDKBackgroundTasksChangedMessage,
  SDKCompactBoundaryMessage,
  SDKControlGetContextUsageResponse,
  SDKControlGetUsageResponse,
  SDKControlInitializeResponse,
  SDKControlInterruptResponse,
  SDKConversationResetMessage,
  SDKHookResponseMessage,
  SDKHookStartedMessage,
  SDKInformationalMessage,
  SDKLocalCommandOutputMessage,
  SDKMessage,
  SDKModelRefusalFallbackMessage,
  SDKModelRefusalNoFallbackMessage,
  SDKNotificationMessage,
  SDKPartialAssistantMessage,
  SDKPermissionDeniedMessage,
  SDKRateLimitEvent,
  SDKResultMessage,
  SDKSessionStateChangedMessage,
  SDKStatusMessage,
  SDKSystemMessage,
  SDKTaskNotificationMessage,
  SDKTaskProgressMessage,
  SDKTaskStartedMessage,
  SDKTaskUpdatedMessage,
  SDKThinkingTokensMessage,
  SDKToolProgressMessage,
  SDKUserMessage,
  RewindFilesResult,
  SlashCommand,
} from "@anthropic-ai/claude-agent-sdk";

// ---------------------------------------------------------------------------
// The message union the fold consumes
// ---------------------------------------------------------------------------

/**
 * Every message the SDK can yield, discriminated by `type` (and by `subtype`
 * within `system`). `convert/fold.ts` switches on it exhaustively; a vendor
 * upgrade that ADDS an arm makes the exhaustiveness check fail, which is how a
 * new vendor fact is noticed rather than silently residued.
 */
export type SdkMessage = SDKMessage;

/** The streaming-input user message the shim pushes into the query. */
export type SdkUserMessage = SDKUserMessage;

/** `type: "assistant"` — one API response's content blocks, whole. */
export type SdkAssistantMessage = SDKAssistantMessage;
/** `type: "result"` — THE only source of a turn terminal. */
export type SdkResultMessage = SDKResultMessage;
/** `type: "stream_event"` — message_start / content_block_* / message_delta. */
export type SdkPartialAssistantMessage = SDKPartialAssistantMessage;
/** `type: "system", subtype: "init"` — the session's opening facts. */
export type SdkSystemMessage = SDKSystemMessage;
/** `type: "system", subtype: "task_started"` — DetachedWorkId meets its call. */
export type SdkTaskStartedMessage = SDKTaskStartedMessage;
/** `type: "system", subtype: "task_updated"`. */
export type SdkTaskUpdatedMessage = SDKTaskUpdatedMessage;
/** `type: "system", subtype: "task_notification"` — a task's terminal fact. */
export type SdkTaskNotificationMessage = SDKTaskNotificationMessage;
/** `type: "system", subtype: "task_progress"`. */
export type SdkTaskProgressMessage = SDKTaskProgressMessage;
/** `type: "system", subtype: "background_tasks_changed"` — the live set. */
export type SdkBackgroundTasksChangedMessage = SDKBackgroundTasksChangedMessage;
/** `type: "system", subtype: "status"`. */
export type SdkStatusMessage = SDKStatusMessage;
/** `type: "system", subtype: "compact_boundary"`. */
export type SdkCompactBoundaryMessage = SDKCompactBoundaryMessage;
/** `type: "system", subtype: "hook_started"`. */
export type SdkHookStartedMessage = SDKHookStartedMessage;
/** `type: "system", subtype: "hook_response"`. */
export type SdkHookResponseMessage = SDKHookResponseMessage;
/** `type: "system", subtype: "notification"`. */
export type SdkNotificationMessage = SDKNotificationMessage;
/** `type: "system", subtype: "permission_denied"` — the gate's own record. */
export type SdkPermissionDeniedMessage = SDKPermissionDeniedMessage;
/** `type: "system", subtype: "api_retry"`. */
export type SdkApiRetryMessage = SDKAPIRetryMessage;
/** `type: "system", subtype: "session_state_changed"`. */
export type SdkSessionStateChangedMessage = SDKSessionStateChangedMessage;
/** `type: "system", subtype: "thinking_tokens"`. */
export type SdkThinkingTokensMessage = SDKThinkingTokensMessage;
/** `type: "system", subtype: "rate_limit_event"`. */
export type SdkRateLimitEvent = SDKRateLimitEvent;
/** `type: "system", subtype: "informational"`. */
export type SdkInformationalMessage = SDKInformationalMessage;
/** `type: "system", subtype: "model_refusal_fallback"`. */
export type SdkModelRefusalFallbackMessage = SDKModelRefusalFallbackMessage;
/** `type: "system", subtype: "model_refusal_no_fallback"`. */
export type SdkModelRefusalNoFallbackMessage = SDKModelRefusalNoFallbackMessage;
/** `type: "system", subtype: "local_command_output"`. */
export type SdkLocalCommandOutputMessage = SDKLocalCommandOutputMessage;
/** `type: "system", subtype: "tool_progress"` — the nine kinds' progress beat. */
export type SdkToolProgressMessage = SDKToolProgressMessage;
/** `type: "system", subtype: "conversation_reset"` — the identity rotation. */
export type SdkConversationResetMessage = SDKConversationResetMessage;

// ---------------------------------------------------------------------------
// The permission boundary
// ---------------------------------------------------------------------------

/** The `canUseTool` callback the shim hands the SDK; the permission gate's entry. */
export type CanUseToolLike = CanUseTool;
/** What the gate answers with: allow (with the possibly-rewritten input) or deny. */
export type PermissionResultLike = PermissionResult;
/** A standing rule the user's decision carries back into the session's settings. */
export type PermissionUpdateLike = PermissionUpdate;
/** The vendor's permission-mode vocabulary; the shim maps AgentPermissionMode onto it. */
export type PermissionModeLike = PermissionMode;
/** The vendor's effort-level vocabulary; the shim maps AgentEffortLevel onto it. */
export type EffortLevelLike = EffortLevel;

// ---------------------------------------------------------------------------
// Control-request answers the shim reads
// ---------------------------------------------------------------------------

/** `query.getContextUsage()` — the source of SessionUpdate.context_usage. */
export type ContextUsageLike = SDKControlGetContextUsageResponse;
/** `query.usage_EXPERIMENTAL_...()` — the source of SessionUpdate.account_usage. */
export type AccountUsageLike = SDKControlGetUsageResponse;
/** `query.initializationResult()` — commands, models, account, binary version. */
export type InitializationResultLike = SDKControlInitializeResponse;
/** `query.accountInfo()`. */
export type AccountInfoLike = AccountInfo;
/**
 * `query.getSettings()` — the CLI's `get_settings` control answer, as far as
 * the shim reads it.
 *
 * UNDECLARED BY THE VENDOR: `sdk.d.ts` declares the `get_settings` request
 * (`SDKControlGetSettingsRequest`) but neither the `Query.getSettings()` method
 * the SDK's runtime provides nor the response's type. The shim reads exactly
 * one field of the answer — `applied.effort`, "the effort level the session
 * will send on its next request — after env overrides, session state, org caps
 * and model-support downgrades" (sdk.d.ts, SDKSystemMessage.effort) — and the
 * sdk-canary suite fails if the runtime method or that field disappears.
 * `null` is the vendor stating it will send no level.
 */
export interface AppliedSettingsLike {
  readonly applied: {
    readonly effort: EffortLevelLike | null;
  };
}
/** `query.supportedModels()` — the SessionStarted model catalog's source. */
export type ModelInfoLike = ModelInfo;
/** `query.supportedCommands()`. */
export type SlashCommandLike = SlashCommand;
/** `query.supportedAgents()`. */
export type AgentInfoLike = AgentInfo;
/** `query.mcpServerStatus()` — the source of SessionUpdate.mcp_server. */
export type McpServerStatusLike = McpServerStatus;
/** One entry of the background-task table `task_started` and friends describe. */
export type BackgroundTaskSummaryLike = BackgroundTaskSummary;

/**
 * `query.interrupt()`'s receipt.
 *
 * `still_queued` names async user messages that SURVIVE the interrupt and will
 * still run. Older CLIs resolve `undefined`; the pinned 0.3.220 always answers
 * with a receipt.
 */
export type InterruptReceipt = SDKControlInterruptResponse;

/** `Query.rewindFiles`' answer: whether the files can go back, and which did. */
export type RewindFilesResultLike = RewindFilesResult;

// ---------------------------------------------------------------------------
// The query surface the shim actually drives
// ---------------------------------------------------------------------------

/**
 * The subset of the SDK's `Query` the shim uses.
 *
 * Narrower than `Query` ON PURPOSE: the mocked vendor (`src/fake/`) and every
 * unit test implement THIS, and a mock forced to impersonate all thirty-odd
 * control verbs would be mostly `throw new Error("unimplemented")` — noise that
 * hides which capabilities the shim genuinely depends on. This list IS that
 * dependency set, and `_QueryStillSatisfiesQueryLike` below proves the real
 * `Query` still provides every member.
 */
export interface QueryLike extends AsyncIterable<SdkMessage> {
  /** Stop the current turn. Resolves the receipt of what survived. */
  interrupt(): Promise<InterruptReceipt | undefined>;
  /** Change the gate's mode for every subsequent permission request. */
  setPermissionMode(mode: PermissionModeLike): Promise<void>;
  /**
   * Merge settings into the session-scoped flag layer. The shim sends ONLY
   * `effortLevel`: the vendor's one mid-session effort control, which holds
   * for the rest of the session and applies from the next request on.
   */
  applyFlagSettings(settings: { effortLevel: EffortLevelLike }): Promise<void>;
  /**
   * The CLI's effective settings, read for the level its next request sends.
   * See {@link AppliedSettingsLike}: the SDK provides this at runtime without
   * declaring it.
   */
  getSettings(): Promise<AppliedSettingsLike>;
  /** Change the model for subsequent responses; `undefined` restores the default. */
  setModel(model?: string): Promise<void>;
  /** The selectable model catalog, as the worker resolves it. */
  supportedModels(): Promise<ModelInfoLike[]>;
  /** The slash commands this session can invoke, settings and skills included. */
  supportedCommands(): Promise<SlashCommandLike[]>;
  /** The subagent definitions this session can spawn. */
  supportedAgents(): Promise<AgentInfoLike[]>;
  /** Every configured MCP server and its health. */
  mcpServerStatus(): Promise<McpServerStatusLike[]>;
  /** The context window's current occupancy. */
  getContextUsage(): Promise<ContextUsageLike>;
  /** The account's rate-limit windows. Experimental vendor-side; read-only here. */
  usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET(): Promise<AccountUsageLike>;
  /** Who this session is authenticated as. */
  accountInfo(): Promise<AccountInfoLike>;
  /** The cached first-connect initialize response. */
  initializationResult(): Promise<InitializationResultLike>;
  /** THE detached-work stop: subagents run inside the CLI, not as our children. */
  stopTask(taskId: string): Promise<void>;
  /** Whether any background task is live (optionally for one tool_use_id). */
  backgroundTasks(toolUseId?: string): Promise<boolean>;
  /**
   * Restore the files the session's edit tools changed to their state when the
   * user message `userMessageId` was sent (file checkpointing, which every
   * session runs with). `dryRun` asks without changing anything.
   */
  rewindFiles(userMessageId: string, options?: { dryRun?: boolean }): Promise<RewindFilesResultLike>;
  /** Push more user messages into a running streaming-input query. */
  streamInput(stream: AsyncIterable<SdkUserMessage>): Promise<void>;
  /** Release the query and the CLI child behind it. */
  close(): void;
}

/**
 * THE UPGRADE CANARY, spelled as a type.
 *
 * `Assert` only accepts `true`, so if a vendor upgrade drops or reshapes any
 * member of {@link QueryLike}, this line stops compiling and `npm run
 * typecheck` fails with the offending method named. A runtime check could not
 * do this: nothing in the shim calls every verb on every path.
 */
type Assert<T extends true> = T;
//
// `getSettings` is the one member the vendor does not declare; it is held to
// the runtime instead (`asQueryLike` in real-query.ts, and the sdk-canary
// suite), and every other member to the declaration here.
type _QueryStillSatisfiesQueryLike = Assert<Query extends Omit<QueryLike, "getSettings"> ? true : false>;

/**
 * Describe an interrupt receipt that reports surviving work, or return null
 * when the receipt is the expected empty one.
 *
 * WHY A NON-EMPTY RECEIPT IS AN ANOMALY FOR US. The daemon holds the prompt
 * queue and interjects by interrupt-then-prompt; it never enqueues async
 * messages into the CLI. So `still_queued` should ALWAYS be empty here. A
 * non-empty one means prompts are sitting in a CLI-side queue the daemon
 * cannot see, will still run, and cannot cancel — the daemon's whole model of
 * "I hold the queue" is wrong at that moment. That is a broken assumption, not
 * a status update, so the entries are reported VERBATIM rather than counted:
 * the uuids are the only handle anyone has on work about to run unbidden.
 */
export function describeInterruptSurvivors(receipt: InterruptReceipt | undefined): string | null {
  const survivors = receipt?.still_queued ?? [];
  if (survivors.length === 0) return null;
  const cancelled = receipt?.cancelled ?? [];
  const cancelledNote = cancelled.length > 0 ? `; cancelled=[${cancelled.join(" ")}]` : "";
  return (
    `interrupt receipt reports ${survivors.length} message(s) STILL QUEUED CLI-side, ` +
    `which the daemon cannot see or cancel: still_queued=[${survivors.join(" ")}]${cancelledNote}`
  );
}
