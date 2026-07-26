/**
 * The claude-shim SDK-message → proto converter (design §8.2, §5.3).
 *
 * Each raw SDK stream message (the JSON the SDK `query()` iterator yields —
 * exactly the shapes in testdata/corpus/stream/) becomes a core `Event`
 * envelope (plane = STREAM, seq = 0 pre-ingest) whose `vendor` Any wraps the
 * total-fidelity `data.v1.ClaudeStreamMessage`. The switch covers every family
 * typed by SDK 0.3.220 plus the observed-only ones, and NOTHING is ever
 * dropped. Two distinct failure channels, which are not interchangeable:
 *
 *   - An unknown `type`/`subtype` DISCRIMINATOR takes the passthrough arm
 *     (`ClaudeStreamMessage.unknown`, a `data.v1.UnknownRecord`) preserving the
 *     record whole, loud-logged once per distinct discriminator. The SDK union
 *     grew 11 -> 38 members across 0.1.77 -> 0.3.220, so this is the expected
 *     steady state, not an error.
 *   - A record of a KNOWN family that fails conversion (a missing expected
 *     field, an unusable nested discriminator) still hard-errors into a core
 *     `UnparsedEvent` (§5.1), never a zero value.
 *
 * LIFECYCLE TWINS: where derivable, vendor-neutral core lifecycle Events are
 * emitted ALONGSIDE the vendor event (as separate Events the store also
 * persists), so consumers get a clean turn/task/session spine without
 * decoding the vendor payload:
 *   - system:init                 → SessionStarted (FIRST init only, gated by
 *                                   {@link SessionStartGate})
 *   - result                      → TurnEnded
 *   - system:task_started         → TaskStarted
 *   - system:task_notification    → TaskEnded  (RawTaskStatus → TerminalStatus)
 *   - system:task_updated         → TaskEnded  (patch.status → TerminalStatus)
 *   - system:background_tasks_changed → vendor-only (no twin)
 * Dedup keys are NEVER set here: the store derives `uuid:`/`tur:` itself.
 *
 * TURN START IS NOT A TWIN. A `user` message is NOT a turn-start signal and
 * this converter no longer derives one from it. Two reasons, both observed
 * live:
 *   1. The SDK echoes a `user` message only on REPLAY (resume / transcript
 *      rehydration), never for a prompt the daemon just submitted — so the
 *      derived TurnStarted never fired at the one moment it mattered and the
 *      SSM never resolved THINKING for a live submit.
 *   2. When an echo DOES arrive it is history, not a new turn, so counting it
 *      as one double-counts turns against the TurnEnded from `result`.
 * Turn start is therefore SHIM-AUTHORITATIVE: the session emits TurnStarted
 * itself the moment it accepts a SubmitPrompt (see UdsSession.emitTurnStarted).
 * The vendor stream has no message for "a turn began"; the shim is the only
 * component that knows.
 *
 * CLASS: PERSISTENT for everything, EXCEPT `stream_event` / `tool_progress`,
 * which are EPHEMERAL (their live-typing/heartbeat relay is {@link
 * import("./delta.js")}'s job). convert() still models them for total-fidelity
 * decode (probes/tests/diagnostics), marking the envelope EPHEMERAL so the
 * wiring never routes them to `StoreWrite`.
 */
import { create, fromJson } from "@bufbuild/protobuf";
import type { JsonObject, JsonValue } from "@bufbuild/protobuf";
import { anyPack, ListValueSchema, type ListValue } from "@bufbuild/protobuf/wkt";
import { shimLog } from "../uds/log.js";
import {
  EventClass,
  EventSchema,
  Plane,
  SessionSource,
  SessionStartedSchema,
  TaskEndedSchema,
  TaskKind,
  TaskStartedSchema,
  TerminalStatus,
  TurnEndedSchema,
  type Event,
} from "../uds/proto.js";
import {
  MissingFieldError,
  Reader,
  logUnknownDiscriminator,
  unparsedEvent,
  type ExtrasOutcome,
} from "./extras.js";
import {
  DiscriminatorField,
  UnknownRecordSchema,
} from "../../../../../proto/gen/ts/agentshim/data/v1/unknown_pb.js";
import {
  ApiKeySource,
  AssistantMessageError,
  AssistantMessageSchema,
  AuthStatusSchema,
  BackgroundTaskRefSchema,
  BackgroundTasksChangedSchema,
  ClaudeStreamMessageSchema,
  CompactBoundarySchema,
  CompactTrigger,
  ContentBlockDeltaEventSchema,
  ContentBlockStartEventSchema,
  ContentBlockStopEventSchema,
  ControlCancelRequestSchema,
  ControlRequestSchema,
  ControlResponseBodySchema,
  ControlResponseSchema,
  DeferredToolUseSchema,
  HookResponseSchema,
  HookStartedSchema,
  InputJsonDeltaSchema,
  KeepAliveSchema,
  McpServerInfoSchema,
  McpServerState,
  McpServerStatusSchema,
  McpToolAnnotationsSchema,
  McpToolRefSchema,
  MessageDeltaEventSchema,
  MessageStartEventSchema,
  MessageStopEventSchema,
  ModelUsageSchema,
  PermissionDenialSchema,
  PluginRefSchema,
  RateLimitEventSchema,
  RateLimitInfoSchema,
  RawMessageStreamEventSchema,
  ResultMessageSchema,
  ResultSubtype,
  SignatureDeltaSchema,
  StatusMessageSchema,
  StreamEventSchema,
  SubagentRetrySchema,
  SystemInitSchema,
  SystemNotificationSchema,
  TaskNotificationMsgSchema,
  TaskPatchSchema,
  TaskStartedMsgSchema,
  TaskUpdatedMsgSchema,
  TaskUsageSchema,
  TextDeltaSchema,
  ThinkingDeltaSchema,
  ThinkingTokensSchema,
  ToolProgressSchema,
  UsageSchema,
  UserMessageSchema,
  // SDK 0.3.220 families:
  ActiveGoalSchema,
  ActiveGoalValueSchema,
  ApiRetrySchema,
  CommandsChangedSchema,
  ControlRequestProgressSchema,
  ConversationResetSchema,
  ElicitationCompleteSchema,
  FailedFileSchema,
  FilesPersistedSchema,
  HookProgressSchema,
  InformationalSchema,
  LocalCommandOutputSchema,
  MemoryRecallSchema,
  MirrorErrorSchema,
  MirrorKeySchema,
  ModelRefusalFallbackSchema,
  ModelRefusalNoFallbackSchema,
  PermissionDeniedSchema,
  PersistedFileSchema,
  PluginInstallSchema,
  PromptSuggestionSchema,
  RecalledMemorySchema,
  SessionStateChangedSchema,
  SlashCommandRefSchema,
  TaskProgressMsgSchema,
  TaskProgressUsageSchema,
  ToolUseSummarySchema,
  WorkerShuttingDownSchema,
  type ClaudeStreamMessage,
  type ModelUsage,
} from "../../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import {
  OriginKind,
  OriginSchema,
  PreservedMessagesSchema,
  PreservedSegmentSchema,
} from "../../../../../proto/gen/ts/agentshim/data/v1/transcript_pb.js";
import {
  AgentAsyncLaunchSchema,
  AgentResultSchema,
  AgentToolStatsSchema,
  ApiAssistantMessageSchema,
  ApiContentBlocksSchema,
  ApiUsageSchema,
  ApiUserMessageSchema,
  AskUserQuestionResultSchema,
  BashResultSchema,
  CallerSchema,
  LocalAgentTaskSchema,
  LocalBashTaskSchema,
  RetrievalStatus,
  TaskOutputResultSchema,
  WebFetchResultSchema,
  CodeExecutionToolResultBlockSchema,
  ContainerUploadBlockSchema,
  ContentBlockSchema,
  McpToolResultBlockSchema,
  McpToolUseBlockSchema,
  RedactedThinkingBlockSchema,
  SearchResultBlockSchema,
  ServerToolUseBlockSchema,
  ToolResultErrorCodeSchema,
  WebFetchToolResultBlockSchema,
  WebSearchToolResultBlockSchema,
  EditResultSchema,
  FallbackBlockSchema,
  FallbackModelRefSchema,
  ImageBlockSchema,
  ImageSourceSchema,
  MessagePinSchema,
  MonitorResultSchema,
  QuestionOptionSchema,
  QuestionSchema,
  ReadResultSchema,
  ScheduleWakeupResultSchema,
  SendMessageResultSchema,
  SkillResultSchema,
  TaskCreateResultSchema,
  TaskListResultSchema,
  TaskStatusChangeSchema,
  TaskStopResultSchema,
  TaskUpdateResultSchema,
  TextBlockSchema,
  ThinkingBlockSchema,
  ArtifactReadSchema,
  ArtifactResultSchema,
  GitBranchAction,
  GitBranchSchema,
  GitCommitKind,
  GitCommitSchema,
  GitOperationSchema,
  GitPullRequestAction,
  GitPullRequestSchema,
  GitPushSchema,
  GlobResultSchema,
  GrepResultSchema,
  StructuredOutputResultSchema,
  TaskGetResultSchema,
  TaskRecordSchema,
  ToolReferenceBlockSchema,
  ToolResultBlockListSchema,
  ToolResultBlockSchema,
  ToolSearchResultSchema,
  ToolUseBlockSchema,
  ToolUseResultSchema,
  WebSearchResultSchema,
  WorkflowLaunchResultSchema,
  WriteResultSchema,
  type ArtifactRead,
  type ContentBlock,
  type GitOperation,
  type MessagePin,
  type Question,
  type TaskStatusChange,
  type ToolUseResult,
} from "../../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";

const COMPONENT = "claude-shim-convert";

/** How far a TurnStarted prompt preview is bounded (first line, then chars). */
const PROMPT_PREVIEW_CAP = 200;

/**
 * Bound a prompt to its TurnStarted preview: first line, then capped.
 *
 * Exported because the TurnStarted emitter now lives in the session (turn
 * start is shim-authoritative, see the file header) while the bounding
 * contract belongs here beside the rest of the proto shaping.
 */
export function promptPreview(text: string): string {
  const firstLine = text.split("\n", 1)[0] ?? "";
  return firstLine.slice(0, PROMPT_PREVIEW_CAP);
}

/**
 * Admits the FIRST `system:init` of a shim lifetime (and any later init that
 * announces a DIFFERENT vendor session id) to emit a SessionStarted twin.
 *
 * The SDK re-inits on every submit, so an ungated converter emitted a
 * SessionStarted twin per prompt. Downstream that is not a harmless repeat:
 * the SSM maps SessionStarted → IDLE, so a re-init landing at submit time
 * knocked the workspace out of THINKING at exactly the moment it entered it.
 * A session starts once; a re-init of the SAME session is not a new session.
 *
 * A CHANGED vendor session id is a genuinely new session (a
 * conversation_reset, a compact-continue), so it re-admits.
 */
export class SessionStartGate {
  private seen: string | null = null;

  /** True when this init's SessionStarted twin should be emitted. */
  admit(vendorSessionId: string): boolean {
    if (this.seen === vendorSessionId) return false;
    this.seen = vendorSessionId;
    return true;
  }
}

/** Wall-clock injection for deterministic tests. */
export interface ConvertOptions {
  nowMs?: number;
  /**
   * The `SessionStarted.source` a `system:init` should carry. The stitch
   * phase (main.ts) passes SESSION_SOURCE_RESUME when the shim was spawned
   * with `--resume`, FRESH otherwise. Absent → FRESH, which is exactly the
   * pre-seam behavior, so every G4 test that omits it is unaffected.
   */
  sessionSource?: SessionSource;
  /**
   * The shim-lifetime gate deciding whether a `system:init` emits its
   * SessionStarted twin. UdsSession always supplies one (it owns the shim
   * lifetime the gate is scoped to). Absent — the single-message decode path
   * used by probes and unit tests — every init emits its twin, because a
   * lone convert() call has no lifetime to be the second init of.
   */
  sessionGate?: SessionStartGate;
}

/** The full result of converting one SDK message. */
export interface ConvertResult {
  /**
   * The vendor Event wrapping the ClaudeStreamMessage (or, on a parse
   * failure, an `UnparsedEvent`). `class` is EPHEMERAL for stream_event /
   * tool_progress, PERSISTENT otherwise.
   */
  vendor: Event;
  /** Derived core lifecycle twins (may be empty). All PERSISTENT. */
  lifecycle: Event[];
  /** Newly loud-logged `<type>.<field>` extras paths (for tests/wiring). */
  loggedExtras: string[];
}

/**
 * Convert one raw SDK stream message. Never throws: a record it cannot parse
 * comes back as an `UnparsedEvent` vendor with no lifecycle twins.
 */
export function convert(message: unknown, opts?: ConvertOptions): ConvertResult {
  if (!isObject(message)) {
    return {
      vendor: unparsedEvent(safeStringify(message), "message is not a JSON object"),
      lifecycle: [],
      loggedExtras: [],
    };
  }
  const type = message["type"];
  if (typeof type !== "string" || type === "") {
    return {
      vendor: unparsedEvent(safeStringify(message), "message has no string `type` discriminator"),
      lifecycle: [],
      loggedExtras: [],
    };
  }

  const envelope = readEnvelope(message, opts);
  try {
    const built = build(type, message, envelope.reader, opts);
    const extras = envelope.reader.finish(built.typeLabel);
    const vendor = vendorEvent(built.csm, envelope, extras, built.ephemeral);
    const lifecycle = built.lifecyclePayloads.map((p) => lifecycleEvent(envelope, p));
    return { vendor, lifecycle, loggedExtras: extras.logged };
  } catch (err) {
    if (err instanceof MissingFieldError) {
      return {
        vendor: unparsedEvent(safeStringify(message), err.message, {
          sessionId: envelope.sessionId,
          requestId: envelope.requestId,
          producedAtMs: envelope.producedAtMs,
        }),
        lifecycle: [],
        loggedExtras: [],
      };
    }
    throw err;
  }
}

// ---------------------------------------------------------------------------
// Envelope (shared across all message types)
// ---------------------------------------------------------------------------

interface Envelope {
  reader: Reader;
  sessionId: string;
  requestId: string;
  producedAtMs: number;
}

/**
 * Read the fields common to the core Event envelope and consume them off the
 * reader: `session_id` (routing), `request_id` (correlation), `timestamp`
 * (producer wall-clock). `uuid` is consumed but not placed on the envelope —
 * the store derives the dedup key from the vendor payload, not the shim.
 */
function readEnvelope(message: Record<string, unknown>, opts?: ConvertOptions): Envelope {
  const reader = new Reader(message);
  const sessionId = reader.str("session_id", "sessionId");
  const requestId = reader.str("request_id", "requestId");
  reader.ignore("uuid"); // dedup key is the store's to derive (§4.2)
  const ts = reader.val("timestamp");
  const producedAtMs = parseTimestamp(ts) ?? opts?.nowMs ?? Date.now();
  return { reader, sessionId, requestId, producedAtMs };
}

function vendorEvent(
  csm: ClaudeStreamMessage,
  env: Envelope,
  extras: ExtrasOutcome,
  ephemeral: boolean,
): Event {
  return create(EventSchema, {
    sessionId: env.sessionId,
    seq: 0n,
    plane: Plane.STREAM,
    class: ephemeral ? EventClass.EPHEMERAL : EventClass.PERSISTENT,
    requestId: env.requestId,
    producedAtMs: BigInt(env.producedAtMs),
    payload: { case: "vendor", value: anyPack(ClaudeStreamMessageSchema, csm) },
    extras: extras.extras,
  });
}

function lifecycleEvent(env: Envelope, payload: Event["payload"]): Event {
  return create(EventSchema, {
    sessionId: env.sessionId,
    seq: 0n,
    plane: Plane.STREAM,
    class: EventClass.PERSISTENT,
    requestId: env.requestId,
    producedAtMs: BigInt(env.producedAtMs),
    payload,
  });
}

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

interface Built {
  csm: ClaudeStreamMessage;
  /** Core lifecycle-twin payloads; convert() wraps each into an Event. */
  lifecyclePayloads: Event["payload"][];
  typeLabel: string; // for extras logging (`<type>.<field>`)
  ephemeral: boolean;
}

/** Build a Built with no lifecycle twin (the common case). */
function vendorOnly(
  msg: ClaudeStreamMessage["msg"],
  typeLabel: string,
  ephemeral = false,
): Built {
  return { csm: csm(msg), lifecyclePayloads: [], typeLabel, ephemeral };
}

function csm(msg: ClaudeStreamMessage["msg"]): ClaudeStreamMessage {
  return create(ClaudeStreamMessageSchema, { msg });
}

function build(type: string, message: Record<string, unknown>, r: Reader, opts?: ConvertOptions): Built {
  r.ignore("type");
  switch (type) {
    case "user":
      return buildUser(message, r);
    case "assistant":
      return buildAssistant(message, r);
    case "result":
      return buildResult(message, r);
    case "stream_event":
      return buildStreamEvent(message, r);
    case "tool_progress":
      return buildToolProgress(message, r);
    case "auth_status":
      return buildAuthStatus(message, r);
    case "rate_limit_event":
      return buildRateLimitEvent(message, r);
    case "compact_boundary":
      return buildCompactBoundary(message, r);
    case "control_request":
      return vendorOnly({ case: "controlRequest", value: create(ControlRequestSchema, { requestId: r.str("request_id", "requestId"), request: r.struct("request") }) }, type);
    case "control_response":
      return buildControlResponse(message, r);
    case "control_cancel_request":
      return vendorOnly({ case: "controlCancelRequest", value: create(ControlCancelRequestSchema, { requestId: r.str("request_id", "requestId") }) }, type);
    case "keep_alive":
      return vendorOnly({ case: "keepAlive", value: create(KeepAliveSchema, {}) }, type);
    case "tool_use_summary":
      return vendorOnly({ case: "toolUseSummary", value: create(ToolUseSummarySchema, { summary: r.str("summary"), precedingToolUseIds: r.strList("preceding_tool_use_ids", "precedingToolUseIds"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, type);
    case "prompt_suggestion":
      return vendorOnly({ case: "promptSuggestion", value: create(PromptSuggestionSchema, { suggestion: r.str("suggestion"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, type);
    case "conversation_reset":
      return vendorOnly({ case: "conversationReset", value: create(ConversationResetSchema, { newConversationId: r.str("new_conversation_id", "newConversationId"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, type);
    case "active_goal":
      return buildActiveGoal(message, r);
    case "system":
      return buildSystem(message, r, opts);
    default:
      // PASSTHROUGH, not an error: an unmodeled `type` means the SDK union
      // grew, which it does constantly (11 -> 38 members across 0.1.77 ->
      // 0.3.220). The record is preserved whole. A message of a type we DO
      // model that fails conversion still becomes an UnparsedEvent, via the
      // MissingFieldError the typed builders throw.
      return buildUnknown(message, r, type, DiscriminatorField.TYPE, "");
  }
}

/**
 * Build the passthrough arm for a record whose discriminator no arm matched.
 *
 * `consumeAll` is what keeps this honest: `UnknownRecord.raw` already holds
 * every field, so the extras channel must NOT also collect them — otherwise
 * each unknown family would spam the unknown-FIELD log with its entire key
 * set and bury the signal that a family we model grew a field.
 */
function buildUnknown(
  message: Record<string, unknown>,
  r: Reader,
  discriminator: string,
  field: DiscriminatorField,
  parentType: string,
): Built {
  r.consumeAll();
  logUnknownDiscriminator(discriminator, parentType);
  const label = parentType === "" ? discriminator : `${parentType}:${discriminator}`;
  return vendorOnly(
    {
      case: "unknown",
      value: create(UnknownRecordSchema, {
        discriminator,
        discriminatorField: field,
        raw: message as JsonObject,
        parentType,
      }),
    },
    label,
  );
}

function buildSystem(message: Record<string, unknown>, r: Reader, opts?: ConvertOptions): Built {
  const subtype = r.str("subtype");
  const label = `system:${subtype}`;
  switch (subtype) {
    case "init":
      return buildSystemInit(message, r, label, opts);
    case "status":
      return vendorOnly({ case: "status", value: create(StatusMessageSchema, {
        status: r.str("status"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
        permissionMode: r.str("permissionMode", "permission_mode"),
        compactResult: r.str("compact_result", "compactResult"),
        compactError: r.str("compact_error", "compactError"),
      }) }, label);
    case "hook_response":
      return buildHookResponse(message, r, label);
    case "hook_started":
      return vendorOnly({ case: "hookStarted", value: create(HookStartedSchema, { hookId: r.str("hook_id", "hookId"), hookName: r.str("hook_name", "hookName"), hookEvent: r.str("hook_event", "hookEvent"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "thinking_tokens":
      return vendorOnly({ case: "thinkingTokens", value: create(ThinkingTokensSchema, { estimatedTokens: r.big("estimated_tokens", "estimatedTokens"), estimatedTokensDelta: r.big("estimated_tokens_delta", "estimatedTokensDelta"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "notification":
      return vendorOnly({ case: "notification", value: create(SystemNotificationSchema, { key: r.str("key"), text: r.str("text"), priority: r.str("priority"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "task_started":
      return buildTaskStarted(message, r, label);
    case "task_updated":
      return buildTaskUpdated(message, r, label);
    case "task_notification":
      return buildTaskNotification(message, r, label);
    case "background_tasks_changed":
      return buildBackgroundTasksChanged(message, r, label);
    case "compact_boundary":
      return buildCompactBoundary(message, r, label);
    // --- SDK 0.3.220 system subtypes -------------------------------------
    case "api_retry":
      return buildApiRetry(message, r, label);
    case "control_request_progress":
      return buildControlRequestProgress(message, r, label);
    case "model_refusal_fallback":
      return vendorOnly({ case: "modelRefusalFallback", value: create(ModelRefusalFallbackSchema, {
        trigger: r.str("trigger"),
        direction: r.str("direction"),
        originalModel: r.str("original_model", "originalModel"),
        fallbackModel: r.str("fallback_model", "fallbackModel"),
        requestId: r.str("request_id", "requestId"),
        apiRefusalCategory: r.str("api_refusal_category", "apiRefusalCategory"),
        apiRefusalExplanation: r.str("api_refusal_explanation", "apiRefusalExplanation"),
        retractedMessageUuids: r.strList("retracted_message_uuids", "retractedMessageUuids"),
        refusedUserMessageUuid: r.str("refused_user_message_uuid", "refusedUserMessageUuid"),
        content: r.str("content"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
      }) }, label);
    case "model_refusal_no_fallback":
      return vendorOnly({ case: "modelRefusalNoFallback", value: create(ModelRefusalNoFallbackSchema, {
        originalModel: r.str("original_model", "originalModel"),
        requestId: r.str("request_id", "requestId"),
        apiRefusalCategory: r.str("api_refusal_category", "apiRefusalCategory"),
        apiRefusalExplanation: r.str("api_refusal_explanation", "apiRefusalExplanation"),
        refusedUserMessageUuid: r.str("refused_user_message_uuid", "refusedUserMessageUuid"),
        content: r.str("content"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
      }) }, label);
    case "local_command_output":
      return vendorOnly({ case: "localCommandOutput", value: create(LocalCommandOutputSchema, { content: r.str("content"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "hook_progress":
      return vendorOnly({ case: "hookProgress", value: create(HookProgressSchema, {
        hookId: r.str("hook_id", "hookId"),
        hookName: r.str("hook_name", "hookName"),
        hookEvent: r.str("hook_event", "hookEvent"),
        stdout: r.str("stdout"),
        stderr: r.str("stderr"),
        output: r.str("output"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
      }) }, label);
    case "plugin_install":
      return vendorOnly({ case: "pluginInstall", value: create(PluginInstallSchema, { status: r.str("status"), name: r.str("name"), error: r.str("error"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "task_progress":
      return buildTaskProgress(message, r, label);
    case "session_state_changed":
      return vendorOnly({ case: "sessionStateChanged", value: create(SessionStateChangedSchema, { state: r.str("state"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "worker_shutting_down":
      return vendorOnly({ case: "workerShuttingDown", value: create(WorkerShuttingDownSchema, { reason: r.str("reason"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "commands_changed":
      return buildCommandsChanged(message, r, label);
    case "files_persisted":
      return buildFilesPersisted(message, r, label);
    case "memory_recall":
      return buildMemoryRecall(message, r, label);
    case "elicitation_complete":
      return vendorOnly({ case: "elicitationComplete", value: create(ElicitationCompleteSchema, { mcpServerName: r.str("mcp_server_name", "mcpServerName"), elicitationId: r.str("elicitation_id", "elicitationId"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
    case "permission_denied":
      return vendorOnly({ case: "permissionDenied", value: create(PermissionDeniedSchema, {
        toolName: r.str("tool_name", "toolName"),
        toolUseId: r.str("tool_use_id", "toolUseId"),
        agentId: r.str("agent_id", "agentId"),
        decisionReasonType: r.str("decision_reason_type", "decisionReasonType"),
        decisionReason: r.str("decision_reason", "decisionReason"),
        message: r.str("message"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
      }) }, label);
    case "mirror_error":
      return buildMirrorError(message, r, label);
    case "informational":
      return vendorOnly({ case: "informational", value: create(InformationalSchema, {
        content: r.str("content"),
        level: r.str("level"),
        toolUseId: r.str("tool_use_id", "toolUseId"),
        preventContinuation: r.bool("prevent_continuation", "preventContinuation"),
        uuid: uuidOf(message),
        sessionId: r.str("session_id", "sessionId"),
      }) }, label);
    default:
      // PASSTHROUGH: an unmodeled system subtype, captured whole under
      // parent_type "system" so a consumer can tell a new VARIANT of a family
      // we model from a whole new top-level family.
      return buildUnknown(message, r, subtype, DiscriminatorField.SUBTYPE, "system");
  }
}

// ---------------------------------------------------------------------------
// user / assistant / result
// ---------------------------------------------------------------------------

function buildUser(message: Record<string, unknown>, r: Reader): Built {
  const rawMessage = r.obj("message");
  if (rawMessage === undefined) {
    throw new MissingFieldError("user message missing `message`");
  }
  const content = convertApiUserMessage(rawMessage);
  const toolUseResult = r.has("tool_use_result", "toolUseResult")
    ? convertToolUseResult(r.val("tool_use_result", "toolUseResult"), r.str("session_id", "sessionId"))
    : undefined;
  const userMsg = create(UserMessageSchema, {
    message: content,
    parentToolUseId: r.str("parent_tool_use_id", "parentToolUseId"),
    isSynthetic: r.bool("is_synthetic", "isSynthetic"),
    toolUseResult,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    isReplay: r.bool("is_replay", "isReplay"),
    // Subagent user messages stamp `subagent_type`/`task_description` context
    // the UserMessage proto now models as typed fields (the subagent linkage
    // itself lives on `parent_tool_use_id`).
    subagentType: r.str("subagent_type", "subagentType"),
    taskDescription: r.str("task_description", "taskDescription"),
    // [sdk 0.3.220]:
    timestamp: r.str("timestamp"),
    priority: r.str("priority"),
    origin: convertOrigin(r.obj("origin")),
    shouldQuery: r.bool("should_query", "shouldQuery"),
    fileAttachments: asListValueOpt(r.val("file_attachments", "fileAttachments")),
  });

  // NO TurnStarted twin: a `user` message is a replayed echo, not a turn
  // start (see the file header). The session emits TurnStarted at
  // SubmitPrompt accept instead.
  return { csm: csm({ case: "user", value: userMsg }), lifecyclePayloads: [], typeLabel: "user", ephemeral: false };
}

function buildAssistant(message: Record<string, unknown>, r: Reader): Built {
  const rawMessage = r.obj("message");
  if (rawMessage === undefined) {
    throw new MissingFieldError("assistant message missing `message`");
  }
  const errorStr = r.str("error");
  const hasError = r.has("error") && errorStr !== "";
  const assistantMsg = create(AssistantMessageSchema, {
    message: convertApiAssistantMessage(rawMessage),
    parentToolUseId: r.str("parent_tool_use_id", "parentToolUseId"),
    error: hasError ? assistantErrorEnum(errorStr) : AssistantMessageError.UNSPECIFIED,
    hasError,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    // [sdk 0.3.220]:
    requestId: r.str("request_id", "requestId"),
    timestamp: r.str("timestamp"),
    resumedFromIncompleteThinking: r.bool("resumed_from_incomplete_thinking", "resumedFromIncompleteThinking"),
    supersedes: r.strList("supersedes"),
    aborted: r.bool("aborted"),
    subagentType: r.str("subagent_type", "subagentType"),
    taskDescription: r.str("task_description", "taskDescription"),
  });
  return { csm: csm({ case: "assistant", value: assistantMsg }), lifecyclePayloads: [], typeLabel: "assistant", ephemeral: false };
}

function buildResult(message: Record<string, unknown>, r: Reader): Built {
  const apiErrorStatusSet = r.has("api_error_status", "apiErrorStatus") && r.val("api_error_status", "apiErrorStatus") !== null;
  const result = create(ResultMessageSchema, {
    subtype: resultSubtypeEnum(r.str("subtype")),
    durationMs: r.big("duration_ms", "durationMs"),
    durationApiMs: r.big("duration_api_ms", "durationApiMs"),
    isError: r.bool("is_error", "isError"),
    numTurns: r.num("num_turns", "numTurns"),
    result: r.str("result"),
    totalCostUsd: r.num("total_cost_usd", "totalCostUsd"),
    usage: convertUsage(r.obj("usage")),
    modelUsage: convertModelUsage(r.obj("model_usage", "modelUsage")),
    permissionDenials: convertPermissionDenials(r.arr("permission_denials", "permissionDenials")),
    structuredOutput: r.struct("structured_output", "structuredOutput"),
    errors: r.strList("errors"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    apiErrorStatus: r.big("api_error_status", "apiErrorStatus"),
    apiErrorStatusSet,
    ttftMs: r.big("ttft_ms", "ttftMs"),
    ttftStreamMs: r.big("ttft_stream_ms", "ttftStreamMs"),
    timeToRequestMs: r.big("time_to_request_ms", "timeToRequestMs"),
    stopReason: r.str("stop_reason", "stopReason"),
    terminalReason: r.str("terminal_reason", "terminalReason"),
    fastModeState: r.str("fast_mode_state", "fastModeState"),
    // [sdk 0.3.220]:
    userMessageUuid: r.str("user_message_uuid", "userMessageUuid"),
    requestSentWallMs: r.big("request_sent_wall_ms", "requestSentWallMs"),
    timeToRequestFromSpawnMs: r.big("time_to_request_from_spawn_ms", "timeToRequestFromSpawnMs"),
    warmSpareClaimed: r.bool("warm_spare_claimed", "warmSpareClaimed"),
    timeOriginMs: r.big("time_origin_ms", "timeOriginMs"),
    deferredToolUse: convertDeferredToolUse(r.obj("deferred_tool_use", "deferredToolUse")),
    fastModeDisabledReason: r.str("fast_mode_disabled_reason", "fastModeDisabledReason"),
    origin: convertOrigin(r.obj("origin")),
  });
  const turnEnded = create(TurnEndedSchema, {
    stopReason: result.stopReason,
    durationMs: result.durationMs,
    isError: result.isError,
  });
  return {
    csm: csm({ case: "result", value: result }),
    lifecyclePayloads: [{ case: "turnEnded", value: turnEnded }],
    typeLabel: "result",
    ephemeral: false,
  };
}

// ---------------------------------------------------------------------------
// stream_event / tool_progress (EPHEMERAL in live routing; modeled here)
// ---------------------------------------------------------------------------

function buildStreamEvent(message: Record<string, unknown>, r: Reader): Built {
  const rawEvent = r.obj("event");
  if (rawEvent === undefined) {
    throw new MissingFieldError("stream_event missing `event`");
  }
  const streamEvent = create(StreamEventSchema, {
    event: convertRawStreamEvent(rawEvent),
    parentToolUseId: r.str("parent_tool_use_id", "parentToolUseId"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    // `ttft_ms` rides some message_start frames as time-to-first-token
    // telemetry the StreamEvent proto now models as a typed field. (The Event
    // itself remains EPHEMERAL; the delta bypass still governs live routing.)
    ttftMs: r.big("ttft_ms", "ttftMs"),
  });
  return { csm: csm({ case: "streamEvent", value: streamEvent }), lifecyclePayloads: [], typeLabel: "stream_event", ephemeral: true };
}

function convertRawStreamEvent(raw: Record<string, unknown>) {
  switch (raw["type"]) {
    case "message_start":
      return create(RawMessageStreamEventSchema, { event: { case: "messageStart", value: create(MessageStartEventSchema, { message: asStruct(raw["message"]) }) } });
    case "content_block_start":
      return create(RawMessageStreamEventSchema, { event: { case: "contentBlockStart", value: create(ContentBlockStartEventSchema, { index: numOf(raw["index"]), contentBlock: asStruct(raw["content_block"]) }) } });
    case "content_block_delta":
      return create(RawMessageStreamEventSchema, { event: { case: "contentBlockDelta", value: convertContentBlockDelta(raw) } });
    case "content_block_stop":
      return create(RawMessageStreamEventSchema, { event: { case: "contentBlockStop", value: create(ContentBlockStopEventSchema, { index: numOf(raw["index"]) }) } });
    case "message_delta":
      return create(RawMessageStreamEventSchema, { event: { case: "messageDelta", value: create(MessageDeltaEventSchema, { delta: asStruct(raw["delta"]), usage: convertUsage(isObject(raw["usage"]) ? raw["usage"] : undefined) }) } });
    case "message_stop":
    case "ping":
      return create(RawMessageStreamEventSchema, { event: { case: "messageStop", value: create(MessageStopEventSchema, {}) } });
    default:
      throw new MissingFieldError(`unknown raw stream event type "${String(raw["type"])}"`);
  }
}

function convertContentBlockDelta(raw: Record<string, unknown>) {
  const index = numOf(raw["index"]);
  const delta = isObject(raw["delta"]) ? raw["delta"] : {};
  switch (delta["type"]) {
    case "text_delta":
      return create(ContentBlockDeltaEventSchema, { index, delta: { case: "textDelta", value: create(TextDeltaSchema, { text: strOf(delta["text"]) }) } });
    case "thinking_delta": {
      const set = delta["estimated_tokens"] !== undefined && delta["estimated_tokens"] !== null;
      return create(ContentBlockDeltaEventSchema, { index, delta: { case: "thinkingDelta", value: create(ThinkingDeltaSchema, { thinking: strOf(delta["thinking"]), estimatedTokens: bigOf(delta["estimated_tokens"]), estimatedTokensSet: set }) } });
    }
    case "input_json_delta":
      return create(ContentBlockDeltaEventSchema, { index, delta: { case: "inputJsonDelta", value: create(InputJsonDeltaSchema, { partialJson: strOf(delta["partial_json"]) }) } });
    case "signature_delta":
      return create(ContentBlockDeltaEventSchema, { index, delta: { case: "signatureDelta", value: create(SignatureDeltaSchema, { signature: strOf(delta["signature"]) }) } });
    default:
      throw new MissingFieldError(`unknown content_block_delta arm "${String(delta["type"])}"`);
  }
}

function buildToolProgress(message: Record<string, unknown>, r: Reader): Built {
  const tp = create(ToolProgressSchema, {
    toolUseId: r.str("tool_use_id", "toolUseId"),
    toolName: r.str("tool_name", "toolName"),
    parentToolUseId: r.str("parent_tool_use_id", "parentToolUseId"),
    elapsedTimeSeconds: r.num("elapsed_time_seconds", "elapsedTimeSeconds"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    // [sdk 0.3.220]:
    taskId: r.str("task_id", "taskId"),
    heartbeat: r.bool("heartbeat"),
    subagentType: r.str("subagent_type", "subagentType"),
    subagentRetry: convertSubagentRetry(r.obj("subagent_retry", "subagentRetry")),
  });
  return { csm: csm({ case: "toolProgress", value: tp }), lifecyclePayloads: [], typeLabel: "tool_progress", ephemeral: true };
}

/**
 * control_response — the STRUCTURAL arm (see stream.proto's ControlResponse).
 *
 * The wire nests everything under `response`; there is NO top-level
 * `request_id` sibling, so reading one always produced "" and correlating a
 * response back to its request was impossible. The id is LIFTED out of the
 * nested body onto field 1, and the body is also captured typed.
 */
function buildControlResponse(message: Record<string, unknown>, r: Reader): Built {
  const raw = r.obj("response");
  const body = raw === undefined ? undefined : create(ControlResponseBodySchema, {
    subtype: strOf(pick(raw, "subtype")),
    requestId: strOf(pick(raw, "request_id", "requestId")),
    response: asStructOpt(pick(raw, "response")),
    error: strOf(pick(raw, "error")),
    pendingPermissionRequests: structList(pick(raw, "pending_permission_requests", "pendingPermissionRequests")),
    pendingUserDialogRequests: structList(pick(raw, "pending_user_dialog_requests", "pendingUserDialogRequests")),
  });
  return vendorOnly({ case: "controlResponse", value: create(ControlResponseSchema, {
    requestId: body?.requestId ?? "",
    response: asStructOpt(raw),
    body,
  }) }, "control_response");
}

/** Every JSON object in a list, as Structs (non-objects are skipped). */
function structList(raw: unknown): JsonObject[] {
  return Array.isArray(raw) ? raw.filter(isObject).map(asStruct) : [];
}

/**
 * A message's provenance. ONE model for both planes (Origin lives in
 * transcript.proto); the `kind` discriminates which of the payload fields are
 * meaningful.
 *
 * An absent origin stays UNSET — never defaulted to HUMAN, which is exactly
 * the attribution a strict trust gate must refuse to invent.
 */
function convertOrigin(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  const pid = pick(raw, "verifiedPeerPid", "verified_peer_pid");
  return create(OriginSchema, {
    kind: originKindEnum(strOf(pick(raw, "kind"))),
    server: strOf(pick(raw, "server")),
    from: strOf(pick(raw, "from")),
    name: strOf(pick(raw, "name")),
    fromSession: strOf(pick(raw, "fromSession", "from_session")),
    senderTaskId: strOf(pick(raw, "senderTaskId", "sender_task_id")),
    body: strOf(pick(raw, "body")),
    verifiedPeerPid: bigOf(pid),
    verifiedPeerPidSet: pid !== undefined && pid !== null,
    subkind: strOf(pick(raw, "subkind")),
  });
}

function originKindEnum(s: string): OriginKind {
  switch (s) {
    case "human": return OriginKind.HUMAN;
    case "task-notification":
    case "task_notification": return OriginKind.TASK_NOTIFICATION;
    case "coordinator": return OriginKind.COORDINATOR;
    case "channel": return OriginKind.CHANNEL;
    case "peer": return OriginKind.PEER;
    case "observer": return OriginKind.OBSERVER;
    case "auto-continuation":
    case "auto_continuation": return OriginKind.AUTO_CONTINUATION;
    case "observer-activity":
    case "observer_activity": return OriginKind.OBSERVER_ACTIVITY;
    default: return OriginKind.UNSPECIFIED;
  }
}

/** The tool a deferred turn ended without running. */
function convertDeferredToolUse(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  return create(DeferredToolUseSchema, {
    id: strOf(pick(raw, "id")),
    name: strOf(pick(raw, "name")),
    input: asStructOpt(pick(raw, "input")),
  });
}

/** A subagent's retry/backoff state, so a stall reads as "retrying". */
function convertSubagentRetry(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  const status = pick(raw, "error_status", "errorStatus");
  return create(SubagentRetrySchema, {
    agentId: strOf(pick(raw, "agent_id", "agentId")),
    attempt: numOf(pick(raw, "attempt")),
    maxRetries: numOf(pick(raw, "max_retries", "maxRetries")),
    retryDelayMs: bigOf(pick(raw, "retry_delay_ms", "retryDelayMs")),
    errorStatus: bigOf(status),
    errorStatusSet: status !== undefined && status !== null,
    errorCategory: strOf(pick(raw, "error_category", "errorCategory")),
  });
}

/** compact_metadata.preserved_segment (shared with the disk twin). */
function convertPreservedSegment(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  return create(PreservedSegmentSchema, {
    headUuid: strOf(pick(raw, "head_uuid", "headUuid")),
    anchorUuid: strOf(pick(raw, "anchor_uuid", "anchorUuid")),
    tailUuid: strOf(pick(raw, "tail_uuid", "tailUuid")),
  });
}

/** compact_metadata.preserved_messages (shared with the disk twin). */
function convertPreservedMessages(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  return create(PreservedMessagesSchema, {
    anchorUuid: strOf(pick(raw, "anchor_uuid", "anchorUuid")),
    uuids: strListOf(pick(raw, "uuids")),
    allUuids: strListOf(pick(raw, "all_uuids", "allUuids")),
  });
}

/**
 * Whether a task-patch status actually ENDS the task. 0.3.220's vocabulary
 * includes "pending", "running" and "paused", none of which is terminal.
 */
function isTerminalTaskStatus(status: string): boolean {
  switch (status) {
    case "completed":
    case "failed":
    case "killed":
    case "stopped":
      return true;
    default:
      return false;
  }
}

function buildAuthStatus(message: Record<string, unknown>, r: Reader): Built {
  const as = create(AuthStatusSchema, {
    isAuthenticating: r.bool("is_authenticating", "isAuthenticating"),
    output: r.strList("output"),
    error: r.str("error"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  return vendorOnly({ case: "authStatus", value: as }, "auth_status");
}

// ---------------------------------------------------------------------------
// system:init → SessionStarted
// ---------------------------------------------------------------------------

function buildSystemInit(message: Record<string, unknown>, r: Reader, label: string, opts?: ConvertOptions): Built {
  const init = create(SystemInitSchema, {
    agents: r.strList("agents"),
    apiKeySource: apiKeySourceEnum(r.str("api_key_source", "apiKeySource")),
    betas: r.strList("betas"),
    claudeCodeVersion: r.str("claude_code_version", "claudeCodeVersion"),
    cwd: r.str("cwd"),
    tools: r.strList("tools"),
    mcpServers: convertMcpServers(r.arr("mcp_servers", "mcpServers")),
    model: r.str("model"),
    permissionMode: r.str("permission_mode", "permissionMode"),
    slashCommands: r.strList("slash_commands", "slashCommands"),
    outputStyle: r.str("output_style", "outputStyle"),
    skills: r.strList("skills"),
    plugins: convertPlugins(r.arr("plugins")),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    fastModeState: r.str("fast_mode_state", "fastModeState"),
    // Corpus-observed init fields the SystemInit proto now models as typed data.
    capabilities: r.strList("capabilities"),
    analyticsDisabled: r.bool("analytics_disabled", "analyticsDisabled"),
    productFeedbackDisabled: r.bool("product_feedback_disabled", "productFeedbackDisabled"),
    memoryPaths: strMap(r.obj("memory_paths", "memoryPaths")),
    fastModeDisabledReason: r.str("fast_mode_disabled_reason", "fastModeDisabledReason"),
  });
  // The SDK re-inits per submit; only the FIRST init of a shim lifetime (or
  // one announcing a different vendor session id) is a session STARTING. See
  // SessionStartGate: an ungated twin knocked the SSM back to IDLE at exactly
  // the moment a submit put it into THINKING.
  const admitted = opts?.sessionGate === undefined || opts.sessionGate.admit(init.sessionId);
  const lifecyclePayloads: Event["payload"][] = [];
  if (admitted) {
    lifecyclePayloads.push({
      case: "sessionStarted",
      value: create(SessionStartedSchema, {
        source: opts?.sessionSource ?? SessionSource.FRESH,
        model: init.model,
        cwd: init.cwd,
        vendorSessionId: init.sessionId,
      }),
    });
  } else {
    shimLog(COMPONENT, { session: init.sessionId }, `system:init re-announced an already-started session; SessionStarted twin suppressed`);
  }
  return {
    csm: csm({ case: "systemInit", value: init }),
    lifecyclePayloads,
    typeLabel: label,
    ephemeral: false,
  };
}

function buildHookResponse(message: Record<string, unknown>, r: Reader, label: string): Built {
  const set = r.has("exit_code", "exitCode") && r.val("exit_code", "exitCode") !== null;
  const hr = create(HookResponseSchema, {
    hookName: r.str("hook_name", "hookName"),
    hookEvent: r.str("hook_event", "hookEvent"),
    stdout: r.str("stdout"),
    stderr: r.str("stderr"),
    exitCode: r.num("exit_code", "exitCode"),
    exitCodeSet: set,
    hookId: r.str("hook_id", "hookId"),
    output: r.str("output"),
    outcome: r.str("outcome"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  return vendorOnly({ case: "hookResponse", value: hr }, label);
}

function buildCompactBoundary(message: Record<string, unknown>, r: Reader, label = "compact_boundary"): Built {
  const cb = create(CompactBoundarySchema, {
    trigger: compactTriggerEnum(r.str("trigger")),
    preTokens: r.big("pre_tokens", "preTokens"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    // [sdk 0.3.220] the stream type caught up to the disk twin, so compaction
    // is measurable here and not just marked.
    postTokens: r.big("post_tokens", "postTokens"),
    durationMs: r.big("duration_ms", "durationMs"),
    preservedSegment: convertPreservedSegment(r.obj("preserved_segment", "preservedSegment")),
    preservedMessages: convertPreservedMessages(r.obj("preserved_messages", "preservedMessages")),
  });
  return vendorOnly({ case: "compactBoundary", value: cb }, label);
}

function buildRateLimitEvent(message: Record<string, unknown>, r: Reader): Built {
  const info = r.obj("rate_limit_info", "rateLimitInfo");
  const rle = create(RateLimitEventSchema, {
    rateLimitInfo: info === undefined ? undefined : create(RateLimitInfoSchema, {
      status: strOf(pick(info, "status")),
      resetsAt: bigOf(pick(info, "resets_at", "resetsAt")),
      rateLimitType: strOf(pick(info, "rate_limit_type", "rateLimitType")),
      utilization: numOf(pick(info, "utilization")),
      isUsingOverage: pick(info, "is_using_overage", "isUsingOverage") === true,
      overageInUse: pick(info, "overage_in_use", "overageInUse") === true,
      surpassedThreshold: numOf(pick(info, "surpassed_threshold", "surpassedThreshold")),
      // [sdk 0.3.220] the overage/credit half — the part that says whether the
      // user is blocked and what they could do about it.
      overageStatus: strOf(pick(info, "overage_status", "overageStatus")),
      overageResetsAt: bigOf(pick(info, "overage_resets_at", "overageResetsAt")),
      overageDisabledReason: strOf(pick(info, "overage_disabled_reason", "overageDisabledReason")),
      errorCode: strOf(pick(info, "error_code", "errorCode")),
      canUserPurchaseCredits: pick(info, "can_user_purchase_credits", "canUserPurchaseCredits") === true,
      hasChargeableSavedPaymentMethod: pick(info, "has_chargeable_saved_payment_method", "hasChargeableSavedPaymentMethod") === true,
    }),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  return vendorOnly({ case: "rateLimitEvent", value: rle }, "rate_limit_event");
}

// ---------------------------------------------------------------------------
// task lifecycle families
// ---------------------------------------------------------------------------

function buildTaskStarted(message: Record<string, unknown>, r: Reader, label: string): Built {
  const taskId = r.str("task_id", "taskId");
  const taskType = r.str("task_type", "taskType");
  const msg = create(TaskStartedMsgSchema, {
    taskId,
    toolUseId: r.str("tool_use_id", "toolUseId"),
    description: r.str("description"),
    taskType,
    subagentType: r.str("subagent_type", "subagentType"),
    prompt: r.str("prompt"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    workflowName: r.str("workflow_name", "workflowName"),
    skipTranscript: r.bool("skip_transcript", "skipTranscript"),
  });
  const taskStarted = create(TaskStartedSchema, {
    taskId,
    kind: taskKindEnum(taskType),
    toolUseId: msg.toolUseId,
    description: msg.description,
    outputPath: "",
  });
  return {
    csm: csm({ case: "taskStarted", value: msg }),
    lifecyclePayloads: [{ case: "taskStarted", value: taskStarted }],
    typeLabel: label,
    ephemeral: false,
  };
}

function buildTaskUpdated(message: Record<string, unknown>, r: Reader, label: string): Built {
  const taskId = r.str("task_id", "taskId");
  const patchObj = r.obj("patch");
  const status = patchObj ? strOf(pick(patchObj, "status")) : "";
  const msg = create(TaskUpdatedMsgSchema, {
    taskId,
    patch: patchObj === undefined ? undefined : create(TaskPatchSchema, {
      status,
      endTime: bigOf(pick(patchObj, "end_time", "endTime")),
      // [sdk 0.3.220]:
      description: strOf(pick(patchObj, "description")),
      totalPausedMs: bigOf(pick(patchObj, "total_paused_ms", "totalPausedMs")),
      error: strOf(pick(patchObj, "error")),
      isBackgrounded: pick(patchObj, "is_backgrounded", "isBackgrounded") === true,
    }),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  // A patch is NOT automatically a task ending: 0.3.220's vocabulary includes
  // "pending", "running" and "paused". Emitting a TaskEnded for those decremented
  // the live-task counter for a task that was still running.
  const lifecyclePayloads: Event["payload"][] = [];
  if (isTerminalTaskStatus(status)) {
    lifecyclePayloads.push({
      case: "taskEnded",
      value: create(TaskEndedSchema, {
        taskId,
        kind: TaskKind.UNSPECIFIED,
        status: terminalStatusEnum(status),
        summary: "",
        outputPath: "",
        inference: "",
      }),
    });
  }
  return {
    csm: csm({ case: "taskUpdated", value: msg }),
    lifecyclePayloads,
    typeLabel: label,
    ephemeral: false,
  };
}

function buildTaskNotification(message: Record<string, unknown>, r: Reader, label: string): Built {
  const taskId = r.str("task_id", "taskId");
  const status = r.str("status");
  const outputFile = r.str("output_file", "outputFile");
  const summary = r.str("summary");
  const usageObj = r.obj("usage");
  const msg = create(TaskNotificationMsgSchema, {
    taskId,
    toolUseId: r.str("tool_use_id", "toolUseId"),
    status,
    outputFile,
    summary,
    usage: usageObj === undefined ? undefined : create(TaskUsageSchema, {
      totalTokens: bigOf(pick(usageObj, "total_tokens", "totalTokens")),
      toolUses: bigOf(pick(usageObj, "tool_uses", "toolUses")),
      durationMs: bigOf(pick(usageObj, "duration_ms", "durationMs")),
    }),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
    skipTranscript: r.bool("skip_transcript", "skipTranscript"),
  });
  const taskEnded = create(TaskEndedSchema, {
    taskId,
    kind: TaskKind.UNSPECIFIED,
    status: terminalStatusEnum(status),
    summary,
    outputPath: outputFile,
    inference: "",
  });
  return {
    csm: csm({ case: "taskNotification", value: msg }),
    lifecyclePayloads: [{ case: "taskEnded", value: taskEnded }],
    typeLabel: label,
    ephemeral: false,
  };
}

function buildBackgroundTasksChanged(message: Record<string, unknown>, r: Reader, label: string): Built {
  const tasks = (r.arr("tasks") ?? []).filter(isObject).map((t) => create(BackgroundTaskRefSchema, {
    taskId: strOf(pick(t, "task_id", "taskId")),
    taskType: strOf(pick(t, "task_type", "taskType")),
    description: strOf(pick(t, "description")),
  }));
  const btc = create(BackgroundTasksChangedSchema, {
    tasks,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  // Vendor-only: background task list changes derive no lifecycle twin.
  return vendorOnly({ case: "backgroundTasksChanged", value: btc }, label);
}

// ---------------------------------------------------------------------------
// SDK 0.3.220 families needing nested conversion or null-discrimination
// ---------------------------------------------------------------------------

/**
 * `error_status` is `number | null`, and the null is MEANINGFUL: it marks a
 * connection error that never got an HTTP response, as opposed to an HTTP
 * failure that did. A bare 0 cannot carry that, hence the *_set companion.
 */
function buildApiRetry(message: Record<string, unknown>, r: Reader, label: string): Built {
  const statusSet = r.has("error_status", "errorStatus") && r.val("error_status", "errorStatus") !== null;
  return vendorOnly({ case: "apiRetry", value: create(ApiRetrySchema, {
    attempt: r.num("attempt"),
    maxRetries: r.num("max_retries", "maxRetries"),
    retryDelayMs: r.big("retry_delay_ms", "retryDelayMs"),
    errorStatus: r.big("error_status", "errorStatus"),
    errorStatusSet: statusSet,
    error: assistantErrorEnum(r.str("error")),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

/**
 * Every numeric field here is optional and present only for the `api_retry`
 * status, so each gets a *_set companion: "not retrying" and "retrying,
 * attempt 0" are different facts and must not collapse onto the same zero.
 */
function buildControlRequestProgress(message: Record<string, unknown>, r: Reader, label: string): Built {
  const set = (...k: string[]): boolean => r.has(...k) && r.val(...k) !== null;
  return vendorOnly({ case: "controlRequestProgress", value: create(ControlRequestProgressSchema, {
    requestId: r.str("request_id", "requestId"),
    status: r.str("status"),
    attempt: r.num("attempt"),
    attemptSet: set("attempt"),
    maxRetries: r.num("max_retries", "maxRetries"),
    maxRetriesSet: set("max_retries", "maxRetries"),
    retryDelayMs: r.big("retry_delay_ms", "retryDelayMs"),
    retryDelayMsSet: set("retry_delay_ms", "retryDelayMs"),
    errorStatus: r.big("error_status", "errorStatus"),
    errorStatusSet: set("error_status", "errorStatus"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

function buildTaskProgress(message: Record<string, unknown>, r: Reader, label: string): Built {
  const usage = r.obj("usage");
  return vendorOnly({ case: "taskProgress", value: create(TaskProgressMsgSchema, {
    taskId: r.str("task_id", "taskId"),
    toolUseId: r.str("tool_use_id", "toolUseId"),
    description: r.str("description"),
    subagentType: r.str("subagent_type", "subagentType"),
    usage: usage === undefined ? undefined : create(TaskProgressUsageSchema, {
      totalTokens: bigOf(pick(usage, "total_tokens", "totalTokens")),
      toolUses: bigOf(pick(usage, "tool_uses", "toolUses")),
      durationMs: bigOf(pick(usage, "duration_ms", "durationMs")),
    }),
    lastToolName: r.str("last_tool_name", "lastToolName"),
    summary: r.str("summary"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

function buildCommandsChanged(message: Record<string, unknown>, r: Reader, label: string): Built {
  const commands = (r.arr("commands") ?? []).filter(isObject).map((c) => create(SlashCommandRefSchema, {
    name: strOf(pick(c, "name")),
    description: strOf(pick(c, "description")),
    argumentHint: strOf(pick(c, "argument_hint", "argumentHint")),
    aliases: strListOf(pick(c, "aliases")),
  }));
  return vendorOnly({ case: "commandsChanged", value: create(CommandsChangedSchema, {
    commands,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

function buildFilesPersisted(message: Record<string, unknown>, r: Reader, label: string): Built {
  const files = (r.arr("files") ?? []).filter(isObject).map((f) => create(PersistedFileSchema, {
    filename: strOf(pick(f, "filename")),
    fileId: strOf(pick(f, "file_id", "fileId")),
  }));
  const failed = (r.arr("failed") ?? []).filter(isObject).map((f) => create(FailedFileSchema, {
    filename: strOf(pick(f, "filename")),
    error: strOf(pick(f, "error")),
  }));
  return vendorOnly({ case: "filesPersisted", value: create(FilesPersistedSchema, {
    files,
    failed,
    processedAt: r.str("processed_at", "processedAt"),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

/**
 * `content` is absent for file-backed `select` entries (the renderer
 * lazy-loads it from `path`) and present for `synthesize`/`organization`.
 * `content_set` keeps "not inlined, go read the path" distinct from
 * "inlined, and it is empty".
 */
function buildMemoryRecall(message: Record<string, unknown>, r: Reader, label: string): Built {
  const memories = (r.arr("memories") ?? []).filter(isObject).map((m) => create(RecalledMemorySchema, {
    path: strOf(pick(m, "path")),
    scope: strOf(pick(m, "scope")),
    content: strOf(pick(m, "content")),
    contentSet: "content" in m && m["content"] !== null,
  }));
  return vendorOnly({ case: "memoryRecall", value: create(MemoryRecallSchema, {
    mode: r.str("mode"),
    memories,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

function buildMirrorError(message: Record<string, unknown>, r: Reader, label: string): Built {
  const key = r.obj("key");
  return vendorOnly({ case: "mirrorError", value: create(MirrorErrorSchema, {
    error: r.str("error"),
    key: key === undefined ? undefined : create(MirrorKeySchema, {
      projectKey: strOf(pick(key, "projectKey", "project_key")),
      sessionId: strOf(pick(key, "sessionId", "session_id")),
      subpath: strOf(pick(key, "subpath")),
    }),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, label);
}

/**
 * `value` is nullable and the NULL is the signal: it means the standing goal
 * was cleared. `value_set` is what keeps that distinct from a goal that
 * happens to have zero iterations.
 */
function buildActiveGoal(message: Record<string, unknown>, r: Reader): Built {
  const value = r.obj("value");
  return vendorOnly({ case: "activeGoal", value: create(ActiveGoalSchema, {
    value: value === undefined ? undefined : create(ActiveGoalValueSchema, {
      condition: strOf(pick(value, "condition")),
      iterations: bigOf(pick(value, "iterations")),
      setAt: bigOf(pick(value, "set_at", "setAt")),
      tokensAtStart: bigOf(pick(value, "tokens_at_start", "tokensAtStart")),
      lastReason: strOf(pick(value, "last_reason", "lastReason")),
    }),
    valueSet: value !== undefined,
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  }) }, "active_goal");
}

// ---------------------------------------------------------------------------
// content blocks + API messages
// ---------------------------------------------------------------------------

function convertApiUserMessage(raw: Record<string, unknown>) {
  const content = raw["content"];
  if (typeof content === "string") {
    return create(ApiUserMessageSchema, { content: { case: "contentString", value: content } });
  }
  if (Array.isArray(content)) {
    return create(ApiUserMessageSchema, { content: { case: "contentBlocks", value: create(ApiContentBlocksSchema, { blocks: convertBlocks(content) }) } });
  }
  return create(ApiUserMessageSchema, { content: { case: undefined } });
}

function convertApiAssistantMessage(raw: Record<string, unknown>) {
  const content = Array.isArray(raw["content"]) ? convertBlocks(raw["content"]) : [];
  return create(ApiAssistantMessageSchema, {
    id: strOf(raw["id"]),
    model: strOf(raw["model"]),
    content,
    stopReason: strOf(raw["stop_reason"]),
    stopSequence: strOf(raw["stop_sequence"]),
    stopDetails: asStructOpt(raw["stop_details"]),
    usage: isObject(raw["usage"]) ? convertApiUsage(raw["usage"]) : undefined,
    diagnostics: asStructOpt(raw["diagnostics"]),
    contextManagement: asStructOpt(raw["context_management"]),
    container: asStructOpt(raw["container"]),
  });
}

function convertBlocks(items: unknown[]): ContentBlock[] {
  const out: ContentBlock[] = [];
  for (const item of items) {
    if (!isObject(item)) continue;
    const block = convertBlock(item);
    if (block) out.push(block);
  }
  return out;
}

function convertBlock(raw: Record<string, unknown>): ContentBlock | null {
  switch (raw["type"]) {
    case "text":
      return create(ContentBlockSchema, { block: { case: "text", value: create(TextBlockSchema, { text: strOf(raw["text"]) }) } });
    case "thinking":
      return create(ContentBlockSchema, { block: { case: "thinking", value: create(ThinkingBlockSchema, { thinking: strOf(raw["thinking"]), signature: strOf(raw["signature"]) }) } });
    case "tool_use":
      return create(ContentBlockSchema, { block: { case: "toolUse", value: create(ToolUseBlockSchema, { id: strOf(raw["id"]), name: strOf(raw["name"]), input: asStructOpt(raw["input"]), caller: isObject(raw["caller"]) ? create(CallerSchema, { type: strOf(raw["caller"]["type"]) }) : undefined }) } });
    case "tool_result":
      return create(ContentBlockSchema, { block: { case: "toolResult", value: convertToolResultBlock(raw) } });
    case "image":
      return create(ContentBlockSchema, { block: { case: "image", value: create(ImageBlockSchema, { source: convertImageSource(raw["source"]) }) } });
    case "tool_reference":
      return create(ContentBlockSchema, { block: { case: "toolReference", value: create(ToolReferenceBlockSchema, {
        reference: asStruct(raw),
        toolName: strOf(pick(raw, "tool_name", "toolName")),
      }) } });
    case "fallback":
      return create(ContentBlockSchema, { block: { case: "fallback", value: create(FallbackBlockSchema, { from: fallbackModel(raw["from"]), to: fallbackModel(raw["to"]) }) } });
    case "redacted_thinking":
      // `data` is an opaque encrypted blob that must survive byte-for-byte to
      // be replayable to the API, so it is decoded to bytes, not kept as text.
      return create(ContentBlockSchema, { block: { case: "redactedThinking", value: create(RedactedThinkingBlockSchema, { data: typeof raw["data"] === "string" ? base64ToBytes(raw["data"]) : new Uint8Array(0) }) } });
    case "server_tool_use":
      return create(ContentBlockSchema, { block: { case: "serverToolUse", value: create(ServerToolUseBlockSchema, { id: strOf(raw["id"]), name: strOf(raw["name"]), input: asStructOpt(raw["input"]) }) } });
    case "mcp_tool_use":
      return create(ContentBlockSchema, { block: { case: "mcpToolUse", value: create(McpToolUseBlockSchema, { id: strOf(raw["id"]), name: strOf(raw["name"]), serverName: strOf(pick(raw, "server_name", "serverName")), input: asStructOpt(raw["input"]) }) } });
    case "mcp_tool_result":
      return create(ContentBlockSchema, { block: { case: "mcpToolResult", value: create(McpToolResultBlockSchema, {
        toolUseId: strOf(pick(raw, "tool_use_id", "toolUseId")),
        isError: raw["is_error"] === true,
        isErrorSet: raw["is_error"] !== undefined,
        content: asListValueOpt(raw["content"]),
      }) } });
    case "web_search_tool_result":
      return create(ContentBlockSchema, { block: { case: "webSearchToolResult", value: create(WebSearchToolResultBlockSchema, { toolUseId: strOf(pick(raw, "tool_use_id", "toolUseId")), content: toolResultUnion(raw["content"]) }) } });
    case "web_fetch_tool_result":
      return create(ContentBlockSchema, { block: { case: "webFetchToolResult", value: create(WebFetchToolResultBlockSchema, { toolUseId: strOf(pick(raw, "tool_use_id", "toolUseId")), content: toolResultUnion(raw["content"]) }) } });
    case "code_execution_tool_result":
      return create(ContentBlockSchema, { block: { case: "codeExecutionToolResult", value: create(CodeExecutionToolResultBlockSchema, { toolUseId: strOf(pick(raw, "tool_use_id", "toolUseId")), content: toolResultUnion(raw["content"]) }) } });
    case "search_result":
      return create(ContentBlockSchema, { block: { case: "searchResult", value: create(SearchResultBlockSchema, {
        source: strOf(raw["source"]),
        title: strOf(raw["title"]),
        content: asListValueOpt(raw["content"]),
        citations: asStructOpt(raw["citations"]),
      }) } });
    case "container_upload":
      return create(ContentBlockSchema, { block: { case: "containerUpload", value: create(ContainerUploadBlockSchema, { fileId: strOf(pick(raw, "file_id", "fileId")) }) } });
    default: {
      // PASSTHROUGH: this used to return null, which DROPPED the block out of
      // the message body — the one place the never-drop contract was actually
      // violated. The block is now preserved whole on ContentBlock.unknown.
      const type = typeof raw["type"] === "string" ? raw["type"] : "";
      logUnknownDiscriminator(type, "content_block");
      return create(ContentBlockSchema, {
        block: {
          case: "unknown",
          value: create(UnknownRecordSchema, {
            discriminator: type,
            discriminatorField: DiscriminatorField.TYPE,
            raw: raw as JsonObject,
            parentType: "content_block",
          }),
        },
      });
    }
  }
}

function convertToolResultBlock(raw: Record<string, unknown>) {
  const content = raw["content"];
  const isErrorSet = raw["is_error"] !== undefined;
  const base = { toolUseId: strOf(raw["tool_use_id"]), isError: raw["is_error"] === true, isErrorSet };
  if (typeof content === "string") {
    return create(ToolResultBlockSchema, { ...base, content: { case: "contentString", value: content } });
  }
  if (Array.isArray(content)) {
    return create(ToolResultBlockSchema, { ...base, content: { case: "contentBlocks", value: create(ToolResultBlockListSchema, { blocks: convertBlocks(content) }) } });
  }
  return create(ToolResultBlockSchema, { ...base, content: { case: undefined } });
}

/**
 * The result-or-error union shared by the three server-tool result blocks.
 *
 * On the wire `content` is EITHER an array of results OR an `{error_code}`
 * object, and the distinction is load-bearing: collapsing them would make a
 * failed search indistinguishable from one that found nothing. An array wins;
 * an object is read as the error; anything else leaves the oneof unset rather
 * than fabricating an empty result list.
 */
function toolResultUnion(
  raw: unknown,
): { case: "results"; value: ListValue } | { case: "error"; value: ReturnType<typeof create<typeof ToolResultErrorCodeSchema>> } | { case: undefined } {
  if (Array.isArray(raw)) {
    return { case: "results", value: fromJson(ListValueSchema, raw as JsonValue[]) };
  }
  if (isObject(raw)) {
    return {
      case: "error",
      value: create(ToolResultErrorCodeSchema, { errorCode: strOf(pick(raw, "error_code", "errorCode")) }),
    };
  }
  return { case: undefined };
}

function convertImageSource(raw: unknown) {
  if (!isObject(raw)) return undefined;
  const data = typeof raw["data"] === "string" ? base64ToBytes(raw["data"]) : new Uint8Array(0);
  return create(ImageSourceSchema, { type: strOf(raw["type"]), mediaType: strOf(raw["media_type"]), data });
}

function fallbackModel(raw: unknown) {
  if (!isObject(raw)) return undefined;
  return create(FallbackModelRefSchema, { model: strOf(raw["model"]) });
}

// ---------------------------------------------------------------------------
// toolUseResult (string-or-object union; §5.1 universal caveat)
// ---------------------------------------------------------------------------

/**
 * Convert a tool's own result payload into the `ToolUseResult` union.
 *
 * - A STRING is the universal error/rejection form ("Error: …", "User rejected
 *   tool use", …) and ALWAYS routes to the `raw_string` arm (never an error).
 * - An OBJECT is classified into a typed arm ONLY on a high-confidence,
 *   low-collision discriminator; anything else is preserved verbatim into the
 *   `unclassified` Struct arm and loud-logged. This is capture, not guessing:
 *   an unknown object NEVER silently becomes a known type (tools.proto §C3).
 *   Exhaustive per-tool typing of the file plane is the sidecar's (G3) job;
 *   the confident subset here demonstrates the typed path on the stream plane.
 */
export function convertToolUseResult(raw: unknown, sessionId = ""): ToolUseResult {
  if (typeof raw === "string") {
    return create(ToolUseResultSchema, { result: { case: "rawString", value: raw } });
  }
  if (!isObject(raw)) {
    return create(ToolUseResultSchema, { result: { case: "rawString", value: safeStringify(raw) } });
  }
  const arm = classifyToolResult(raw);
  if (arm) return create(ToolUseResultSchema, { result: arm });
  shimLog(COMPONENT, { session: sessionId, keys: Object.keys(raw).slice(0, 8).join(",") }, `unclassified toolUseResult object captured verbatim`);
  return create(ToolUseResultSchema, { result: { case: "unclassified", value: raw as JsonObject } });
}

/** True when `o` is an object carrying at least one of `keys`. */
function anyIn(o: unknown, ...keys: string[]): boolean {
  return isObject(o) && keys.some((k) => k in o);
}

function classifyToolResult(o: Record<string, unknown>): ToolUseResult["result"] | null {
  const has = (...k: string[]) => k.every((x) => x in o);
  const any = (...k: string[]) => k.some((x) => x in o);

  if (has("stdout", "stderr") && any("interrupted", "isImage", "is_image")) {
    return { case: "bash", value: create(BashResultSchema, {
      stdout: strOf(o["stdout"]), stderr: strOf(o["stderr"]),
      interrupted: o["interrupted"] === true,
      isImage: pick(o, "is_image", "isImage") === true,
      noOutputExpected: pick(o, "no_output_expected", "noOutputExpected") === true,
      returnCodeInterpretation: strOf(pick(o, "return_code_interpretation", "returnCodeInterpretation")),
      gitOperation: gitOperationOf(pick(o, "git_operation", "gitOperation")),
      persistedOutputPath: strOf(pick(o, "persisted_output_path", "persistedOutputPath")),
      persistedOutputSize: bigOf(pick(o, "persisted_output_size", "persistedOutputSize")),
      timedOutAfterMs: bigOf(pick(o, "timed_out_after_ms", "timedOutAfterMs")),
      staleReadFileStateHint: strOf(pick(o, "stale_read_file_state_hint", "staleReadFileStateHint")),
      backgroundTaskId: strOf(pick(o, "background_task_id", "backgroundTaskId")),
      backgroundCwdHint: strOf(pick(o, "background_cwd_hint", "backgroundCwdHint")),
      dangerouslyDisableSandbox: pick(o, "dangerously_disable_sandbox", "dangerouslyDisableSandbox") === true,
      backgroundedByUser: pick(o, "backgrounded_by_user", "backgroundedByUser") === true,
    }) };
  }
  // Agent/Task BACKGROUND launch. `is_async` is unique to this member of the
  // SDK's AgentOutput union (and to the whole result census); it is paired with
  // `output_file`, required on the same member, so a stray async flag on some
  // other tool's result cannot claim the arm.
  if (any("is_async", "isAsync") && any("output_file", "outputFile")) {
    return { case: "agentAsyncLaunch", value: create(AgentAsyncLaunchSchema, {
      isAsync: pick(o, "is_async", "isAsync") === true,
      status: rawTaskStatusEnum(strOf(pick(o, "status"))),
      agentId: strOf(pick(o, "agent_id", "agentId")),
      description: strOf(pick(o, "description")),
      resolvedModel: strOf(pick(o, "resolved_model", "resolvedModel")),
      prompt: strOf(pick(o, "prompt")),
      outputFile: strOf(pick(o, "output_file", "outputFile")),
      canReadOutputFile: pick(o, "can_read_output_file", "canReadOutputFile") === true,
    }) };
  }
  // Agent/Task SYNCHRONOUS completion. Keyed on the two totals that 0.3.220's
  // AgentOutput marks REQUIRED on its completed member and that appear on no
  // other tool output in the union. `agent_type` looks like the obvious key but
  // is optional there, so keying on it would drop a result that omits it. The
  // async-launch and remote-launch members carry neither total, so neither can
  // land here.
  if (any("total_duration_ms", "totalDurationMs") && any("total_tool_use_count", "totalToolUseCount")) {
    const usage = pick(o, "usage");
    const stats = pick(o, "tool_stats", "toolStats");
    return { case: "agent", value: create(AgentResultSchema, {
      status: rawTaskStatusEnum(strOf(pick(o, "status"))),
      prompt: strOf(pick(o, "prompt")),
      agentId: strOf(pick(o, "agent_id", "agentId")),
      agentType: strOf(pick(o, "agent_type", "agentType")),
      content: Array.isArray(o["content"]) ? convertBlocks(o["content"]) : [],
      resolvedModel: strOf(pick(o, "resolved_model", "resolvedModel")),
      totalDurationMs: bigOf(pick(o, "total_duration_ms", "totalDurationMs")),
      totalTokens: bigOf(pick(o, "total_tokens", "totalTokens")),
      totalToolUseCount: bigOf(pick(o, "total_tool_use_count", "totalToolUseCount")),
      usage: isObject(usage) ? convertApiUsage(usage) : undefined,
      toolStats: isObject(stats) ? create(AgentToolStatsSchema, {
        readCount: bigOf(pick(stats, "read_count", "readCount")),
        searchCount: bigOf(pick(stats, "search_count", "searchCount")),
        bashCount: bigOf(pick(stats, "bash_count", "bashCount")),
        editFileCount: bigOf(pick(stats, "edit_file_count", "editFileCount")),
        linesAdded: bigOf(pick(stats, "lines_added", "linesAdded")),
        linesRemoved: bigOf(pick(stats, "lines_removed", "linesRemoved")),
        otherToolCount: bigOf(pick(stats, "other_tool_count", "otherToolCount")),
      }) : undefined,
      worktreePath: strOf(pick(o, "worktree_path", "worktreePath")),
      worktreeBranch: strOf(pick(o, "worktree_branch", "worktreeBranch")),
    }) };
  }
  if (has("commandName") || (has("command_name") && any("success", "allowedTools", "allowed_tools"))) {
    return { case: "skill", value: create(SkillResultSchema, {
      commandName: strOf(pick(o, "command_name", "commandName")),
      success: o["success"] === true,
      allowedTools: strListOf(pick(o, "allowed_tools", "allowedTools")),
    }) };
  }
  if (has("matches", "query")) {
    return { case: "toolSearch", value: create(ToolSearchResultSchema, {
      matches: strListOf(o["matches"]),
      query: strOf(o["query"]),
      totalDeferredTools: bigOf(pick(o, "total_deferred_tools", "totalDeferredTools")),
    }) };
  }
  if (has("command") && any("task_id", "taskId") && has("message")) {
    return { case: "taskStop", value: create(TaskStopResultSchema, {
      message: strOf(o["message"]),
      taskId: strOf(pick(o, "task_id", "taskId")),
      taskType: strOf(pick(o, "task_type", "taskType")),
      command: strOf(o["command"]),
    }) };
  }
  // TaskOutput MUST precede every `task`-keyed arm below: it also carries a
  // `task` object, so without this it was classified as a TaskCreate result and
  // silently MISTYPED (not merely left unclassified).
  //
  // The wire spelling is snake_case `retrieval_status`, which is unique to this
  // result across the whole census. The arm ALSO requires its nested task to
  // discriminate, because TaskOutputResult's oneof has only local_bash and
  // local_agent: a task of any other type has nowhere to go and would be
  // DROPPED by the typed arm, so an unresolved inner discriminator sends the
  // whole object to `unclassified`, where it survives verbatim and is logged.
  if (any("retrieval_status", "retrievalStatus")) {
    const t = o["task"];
    if (!isObject(t)) return null;
    const retrievalStatus = retrievalStatusEnum(strOf(pick(o, "retrieval_status", "retrievalStatus")));
    switch (strOf(pick(t, "task_type", "taskType"))) {
      case "local_bash":
        return { case: "taskOutput", value: create(TaskOutputResultSchema, {
          retrievalStatus,
          task: { case: "localBash", value: localBashTask(t) },
        }) };
      case "local_agent":
        return { case: "taskOutput", value: create(TaskOutputResultSchema, {
          retrievalStatus,
          task: { case: "localAgent", value: localAgentTask(t) },
        }) };
      default:
        return null;
    }
  }
  // TaskGet before TaskCreate: BOTH key on `task`, and only TaskGet's carries
  // the dependency lists. `task` is also NULLABLE here, and a null task is
  // still a TaskGet result rather than something unidentifiable.
  if (has("task") && o["task"] === null) {
    return { case: "taskGet", value: create(TaskGetResultSchema, { taskSet: false }) };
  }
  if (has("task") && isObject(o["task"]) && anyIn(o["task"], "blocks", "blockedBy", "blocked_by")) {
    const t = o["task"];
    return { case: "taskGet", value: create(TaskGetResultSchema, {
      taskSet: true,
      task: create(TaskRecordSchema, {
        id: strOf(pick(t, "id")),
        subject: strOf(pick(t, "subject")),
        description: strOf(pick(t, "description")),
        status: strOf(pick(t, "status")),
        blocks: strListOf(pick(t, "blocks")),
        blockedBy: strListOf(pick(t, "blocked_by", "blockedBy")),
      }),
    }) };
  }
  if (has("task") && isObject(o["task"])) {
    return { case: "taskCreate", value: create(TaskCreateResultSchema, { task: o["task"] as JsonObject }) };
  }
  if (has("tasks") && Array.isArray(o["tasks"]) && Object.keys(o).length === 1) {
    return { case: "taskList", value: create(TaskListResultSchema, { tasks: asListValueOpt(o["tasks"]) }) };
  }
  if (has("oldString") || has("old_string")) {
    return { case: "edit", value: create(EditResultSchema, {
      filePath: strOf(pick(o, "file_path", "filePath")),
      oldString: strOf(pick(o, "old_string", "oldString")),
      newString: strOf(pick(o, "new_string", "newString")),
      originalFile: strOf(pick(o, "original_file", "originalFile")),
      replaceAll: pick(o, "replace_all", "replaceAll") === true,
      structuredPatch: asListValueOpt(pick(o, "structured_patch", "structuredPatch")),
      userModified: pick(o, "user_modified", "userModified") === true,
      staleRecovered: pick(o, "stale_recovered", "staleRecovered") === true,
      memdirStamped: pick(o, "memdir_stamped", "memdirStamped") === true,
    }) };
  }
  if (any("structuredPatch", "structured_patch") && has("content") && any("filePath", "file_path")) {
    return { case: "write", value: create(WriteResultSchema, {
      type: strOf(o["type"]),
      filePath: strOf(pick(o, "file_path", "filePath")),
      content: strOf(o["content"]),
      originalFile: strOf(pick(o, "original_file", "originalFile")),
      structuredPatch: asListValueOpt(pick(o, "structured_patch", "structuredPatch")),
      userModified: pick(o, "user_modified", "userModified") === true,
      memdirStamped: pick(o, "memdir_stamped", "memdirStamped") === true,
    }) };
  }
  if (any("clampedDelaySeconds", "clamped_delay_seconds", "scheduledFor", "scheduled_for")) {
    return { case: "scheduleWakeup", value: create(ScheduleWakeupResultSchema, {
      clampedDelaySeconds: bigOf(pick(o, "clamped_delay_seconds", "clampedDelaySeconds")),
      scheduledFor: bigOf(pick(o, "scheduled_for", "scheduledFor")),
      wasClamped: pick(o, "was_clamped", "wasClamped") === true,
      stopped: pick(o, "stopped") === true,
      cancelledWakeups: bigOf(pick(o, "cancelled_wakeups", "cancelledWakeups")),
    }) };
  }
  if (has("questions") && has("answers")) {
    return { case: "askUserQuestion", value: create(AskUserQuestionResultSchema, {
      questions: questionsOf(o["questions"]),
      answers: asStructOpt(o["answers"]),
      annotations: asStructOpt(o["annotations"]),
      afkTimeoutMs: bigOf(pick(o, "afk_timeout_ms", "afkTimeoutMs")),
    }) };
  }
  if (any("searchCount", "search_count", "durationSeconds", "duration_seconds")) {
    return { case: "webSearch", value: create(WebSearchResultSchema, {
      durationSeconds: numOf(pick(o, "duration_seconds", "durationSeconds")),
      query: strOf(o["query"]),
      results: asListValueOpt(o["results"]),
      searchCount: bigOf(pick(o, "search_count", "searchCount")),
    }) };
  }
  // WebFetch, right after its WebSearch sibling. Keyed on the HTTP status text
  // plus the byte count, both required by 0.3.220's WebFetchOutput and neither
  // present on any other result in the census. It cannot collide with the two
  // arms it shares a key with: WebSearch keys on `duration_seconds` (a fetch
  // reports `duration_ms`), and Artifact's `!has("code")` guard already excludes
  // anything carrying an HTTP code.
  if (any("code_text", "codeText") && has("bytes")) {
    return { case: "webFetch", value: create(WebFetchResultSchema, {
      bytes: bigOf(pick(o, "bytes")),
      code: bigOf(pick(o, "code")),
      codeText: strOf(pick(o, "code_text", "codeText")),
      durationMs: bigOf(pick(o, "duration_ms", "durationMs")),
      result: strOf(pick(o, "result")),
      url: strOf(pick(o, "url")),
      artifactRead: artifactReadOf(pick(o, "artifact_read", "artifactRead")),
    }) };
  }
  if (any("statusChange", "status_change", "updatedFields", "updated_fields")) {
    return { case: "taskUpdate", value: create(TaskUpdateResultSchema, {
      taskId: strOf(pick(o, "task_id", "taskId")),
      statusChange: statusChangeOf(pick(o, "status_change", "statusChange")),
      success: o["success"] === true,
      updatedFields: strListOf(pick(o, "updated_fields", "updatedFields")),
    }) };
  }
  if (has("pin")) {
    return { case: "sendMessage", value: create(SendMessageResultSchema, {
      message: strOf(o["message"]),
      pin: pinOf(o["pin"]),
      success: o["success"] === true,
      resumedAgentId: strOf(pick(o, "resumed_agent_id", "resumedAgentId")),
    }) };
  }
  if (any("workflow_name", "workflowName") && any("run_id", "runId")) {
    return { case: "workflowLaunch", value: create(WorkflowLaunchResultSchema, {
      status: rawTaskStatusEnum(strOf(pick(o, "status"))),
      taskId: strOf(pick(o, "task_id", "taskId")),
      taskType: strOf(pick(o, "task_type", "taskType")),
      workflowName: strOf(pick(o, "workflow_name", "workflowName")),
      runId: strOf(pick(o, "run_id", "runId")),
      summary: strOf(pick(o, "summary")),
      transcriptDir: strOf(pick(o, "transcript_dir", "transcriptDir")),
      scriptPath: strOf(pick(o, "script_path", "scriptPath")),
    }) };
  }
  if (any("timeout_ms", "timeoutMs") && has("persistent") && any("task_id", "taskId")) {
    return { case: "monitor", value: create(MonitorResultSchema, {
      taskId: strOf(pick(o, "task_id", "taskId")),
      timeoutMs: bigOf(pick(o, "timeout_ms", "timeoutMs")),
      persistent: o["persistent"] === true,
    }) };
  }
  if (o["type"] === "text" && isObject(o["file"])) {
    return { case: "read", value: create(ReadResultSchema, { type: strOf(o["type"]), file: o["file"] as JsonObject }) };
  }
  // Glob: filenames + a duration, and NO grep-only key. Checked before grep
  // because both carry `filenames`.
  if (has("filenames") && any("durationMs", "duration_ms") && !any("mode", "numMatches", "num_matches", "content")) {
    return { case: "glob", value: create(GlobResultSchema, {
      durationMs: bigOf(pick(o, "duration_ms", "durationMs")),
      numFiles: bigOf(pick(o, "num_files", "numFiles")),
      filenames: strListOf(o["filenames"]),
      truncated: o["truncated"] === true,
      totalMatches: bigOf(pick(o, "total_matches", "totalMatches")),
      countIsComplete: pick(o, "count_is_complete", "countIsComplete") === true,
    }) };
  }
  if (has("filenames") && any("mode", "numMatches", "num_matches", "numLines", "num_lines")) {
    return { case: "grep", value: create(GrepResultSchema, {
      mode: strOf(o["mode"]),
      numFiles: bigOf(pick(o, "num_files", "numFiles")),
      filenames: strListOf(o["filenames"]),
      content: strOf(o["content"]),
      numLines: bigOf(pick(o, "num_lines", "numLines")),
      numMatches: bigOf(pick(o, "num_matches", "numMatches")),
      totalFiles: bigOf(pick(o, "total_files", "totalFiles")),
      totalLines: bigOf(pick(o, "total_lines", "totalLines")),
      appliedLimit: bigOf(pick(o, "applied_limit", "appliedLimit")),
      appliedOffset: bigOf(pick(o, "applied_offset", "appliedOffset")),
    }) };
  }
  if (any("url", "artifactUrl", "artifact_url") && any("favicon", "label", "version") && !has("code")) {
    return { case: "artifact", value: create(ArtifactResultSchema, {
      url: strOf(pick(o, "url", "artifact_url", "artifactUrl")),
      title: strOf(o["title"]),
      description: strOf(o["description"]),
      label: strOf(o["label"]),
      version: strOf(o["version"]),
    }) };
  }
  // StructuredOutput: the caller's OWN schema under a `structuredOutput` key.
  // Schemaless by design, so captured whole rather than guessed at — but
  // CLASSIFIED, so it no longer lands in `unclassified` alongside genuinely
  // unidentifiable objects.
  if (any("structuredOutput", "structured_output")) {
    return { case: "structuredOutput", value: create(StructuredOutputResultSchema, {
      data: asStructOpt(pick(o, "structured_output", "structuredOutput")),
    }) };
  }
  return null;
}

/**
 * TaskOutput's `local_bash` task record (corpus:
 * tool-results/task_output.jsonl).
 *
 * `exitCode` is `number|null` on the wire — null while the task is still
 * running — so the null is carried as `exit_code_set: false` rather than
 * collapsing to a 0 that would read as a clean exit.
 */
function localBashTask(t: Record<string, unknown>) {
  const exitCode = pick(t, "exit_code", "exitCode");
  return create(LocalBashTaskSchema, {
    taskId: strOf(pick(t, "task_id", "taskId")),
    taskType: strOf(pick(t, "task_type", "taskType")),
    status: rawTaskStatusEnum(strOf(pick(t, "status"))),
    description: strOf(pick(t, "description")),
    output: strOf(pick(t, "output")),
    exitCode: numOf(exitCode),
    exitCodeSet: exitCode !== undefined && exitCode !== null,
  });
}

/**
 * TaskOutput's `local_agent` task record (corpus:
 * tool-results/task_output-local_agent.jsonl).
 *
 * `isRawTranscript` is ABSENT while the agent is still running, so its
 * presence is tracked separately: an absent flag is "not known yet", which is
 * not the same claim as "the output is not a raw transcript".
 */
function localAgentTask(t: Record<string, unknown>) {
  const isRawTranscript = pick(t, "is_raw_transcript", "isRawTranscript");
  return create(LocalAgentTaskSchema, {
    taskId: strOf(pick(t, "task_id", "taskId")),
    taskType: strOf(pick(t, "task_type", "taskType")),
    status: rawTaskStatusEnum(strOf(pick(t, "status"))),
    description: strOf(pick(t, "description")),
    output: strOf(pick(t, "output")),
    prompt: strOf(pick(t, "prompt")),
    result: strOf(pick(t, "result")),
    isRawTranscript: isRawTranscript === true,
    isRawTranscriptSet: isRawTranscript !== undefined && isRawTranscript !== null,
  });
}

// ---------------------------------------------------------------------------
// usage / model_usage / permission_denials / mcp / plugins
// ---------------------------------------------------------------------------

function convertUsage(raw: Record<string, unknown> | undefined) {
  if (raw === undefined) return undefined;
  return create(UsageSchema, {
    inputTokens: bigOf(pick(raw, "input_tokens", "inputTokens")),
    outputTokens: bigOf(pick(raw, "output_tokens", "outputTokens")),
    cacheReadInputTokens: bigOf(pick(raw, "cache_read_input_tokens", "cacheReadInputTokens")),
    cacheCreationInputTokens: bigOf(pick(raw, "cache_creation_input_tokens", "cacheCreationInputTokens")),
    cacheCreation: asStructOpt(pick(raw, "cache_creation", "cacheCreation")),
    serverToolUse: asStructOpt(pick(raw, "server_tool_use", "serverToolUse")),
    serviceTier: strOf(pick(raw, "service_tier", "serviceTier")),
  });
}

function convertApiUsage(raw: Record<string, unknown>) {
  return create(ApiUsageSchema, {
    inputTokens: bigOf(pick(raw, "input_tokens", "inputTokens")),
    outputTokens: bigOf(pick(raw, "output_tokens", "outputTokens")),
    cacheReadInputTokens: bigOf(pick(raw, "cache_read_input_tokens", "cacheReadInputTokens")),
    cacheCreationInputTokens: bigOf(pick(raw, "cache_creation_input_tokens", "cacheCreationInputTokens")),
    cacheCreation: asStructOpt(pick(raw, "cache_creation", "cacheCreation")),
    serverToolUse: asStructOpt(pick(raw, "server_tool_use", "serverToolUse")),
    serviceTier: strOf(pick(raw, "service_tier", "serviceTier")),
    // ApiUsage models these three (fields 8-10) and newer harness versions send
    // all three on every assistant/agent usage block, but nothing read them, so
    // the per-model iteration breakdown and the speed/geo routing were dropped
    // on the floor at conversion time.
    iterations: asListValueOpt(pick(raw, "iterations")),
    speed: strOf(pick(raw, "speed")),
    inferenceGeo: strOf(pick(raw, "inference_geo", "inferenceGeo")),
  });
}

function convertModelUsage(raw: Record<string, unknown> | undefined): Record<string, ModelUsage> {
  const out: Record<string, ModelUsage> = {};
  if (raw === undefined) return out;
  for (const [model, v] of Object.entries(raw)) {
    if (!isObject(v)) continue;
    out[model] = create(ModelUsageSchema, {
      inputTokens: bigOf(pick(v, "input_tokens", "inputTokens")),
      outputTokens: bigOf(pick(v, "output_tokens", "outputTokens")),
      cacheReadInputTokens: bigOf(pick(v, "cache_read_input_tokens", "cacheReadInputTokens")),
      cacheCreationInputTokens: bigOf(pick(v, "cache_creation_input_tokens", "cacheCreationInputTokens")),
      webSearchRequests: bigOf(pick(v, "web_search_requests", "webSearchRequests")),
      costUsd: numOf(pick(v, "cost_usd", "costUSD", "costUsd")),
      contextWindow: bigOf(pick(v, "context_window", "contextWindow")),
      maxOutputTokens: bigOf(pick(v, "max_output_tokens", "maxOutputTokens")),
      canonicalModel: strOf(pick(v, "canonical_model", "canonicalModel")),
      provider: strOf(pick(v, "provider")),
    });
  }
  return out;
}

function convertPermissionDenials(raw: unknown[] | undefined) {
  if (raw === undefined) return [];
  return raw.filter(isObject).map((d) => create(PermissionDenialSchema, {
    toolName: strOf(pick(d, "tool_name", "toolName")),
    toolUseId: strOf(pick(d, "tool_use_id", "toolUseId")),
    toolInput: asStructOpt(pick(d, "tool_input", "toolInput")),
  }));
}

function convertMcpServers(raw: unknown[] | undefined) {
  if (raw === undefined) return [];
  return raw.filter(isObject).map((s) => {
    const info = pick(s, "server_info", "serverInfo");
    return create(McpServerStatusSchema, {
      name: strOf(pick(s, "name")),
      status: mcpStateEnum(strOf(pick(s, "status"))),
      // The rich fields ride the control channel's mcp_server_status response,
      // not `system:init` (which carries only {name,status}); one message
      // models both, so these are simply unset on the init path.
      serverInfo: isObject(info)
        ? create(McpServerInfoSchema, { name: strOf(pick(info, "name")), version: strOf(pick(info, "version")) })
        : undefined,
      error: strOf(pick(s, "error")),
      config: asStructOpt(pick(s, "config")),
      scope: strOf(pick(s, "scope")),
      tools: convertMcpTools(pick(s, "tools")),
    });
  });
}

function convertMcpTools(raw: unknown) {
  if (!Array.isArray(raw)) return [];
  return raw.filter(isObject).map((t) => {
    const ann = pick(t, "annotations");
    return create(McpToolRefSchema, {
      name: strOf(pick(t, "name")),
      description: strOf(pick(t, "description")),
      annotations: isObject(ann)
        ? create(McpToolAnnotationsSchema, {
            readOnly: pick(ann, "readOnly", "read_only") === true,
            destructive: pick(ann, "destructive") === true,
            openWorld: pick(ann, "openWorld", "open_world") === true,
          })
        : undefined,
    });
  });
}

/** Coerce a JSON object into a proto map<string,string>; non-string values drop. */
function strMap(o: Record<string, unknown> | undefined): { [k: string]: string } {
  const out: { [k: string]: string } = {};
  if (o === undefined) return out;
  for (const [k, v] of Object.entries(o)) {
    if (typeof v === "string") out[k] = v;
  }
  return out;
}

function convertPlugins(raw: unknown[] | undefined) {
  if (raw === undefined) return [];
  return raw.filter(isObject).map((p) => create(PluginRefSchema, {
    name: strOf(pick(p, "name")),
    path: strOf(pick(p, "path")),
    source: strOf(pick(p, "source")),
    version: strOf(pick(p, "version")),
  }));
}

// ---------------------------------------------------------------------------
// enum mappers
// ---------------------------------------------------------------------------

function resultSubtypeEnum(s: string): ResultSubtype {
  switch (s) {
    case "success": return ResultSubtype.SUCCESS;
    case "error_during_execution": return ResultSubtype.ERROR_DURING_EXECUTION;
    case "error_max_turns": return ResultSubtype.ERROR_MAX_TURNS;
    case "error_max_budget_usd": return ResultSubtype.ERROR_MAX_BUDGET_USD;
    case "error_max_structured_output_retries": return ResultSubtype.ERROR_MAX_STRUCTURED_OUTPUT_RETRIES;
    default: return ResultSubtype.UNSPECIFIED;
  }
}

function assistantErrorEnum(s: string): AssistantMessageError {
  switch (s) {
    case "authentication_failed": return AssistantMessageError.AUTHENTICATION_FAILED;
    case "billing_error": return AssistantMessageError.BILLING_ERROR;
    case "rate_limit": return AssistantMessageError.RATE_LIMIT;
    case "invalid_request": return AssistantMessageError.INVALID_REQUEST;
    case "server_error": return AssistantMessageError.SERVER_ERROR;
    // The rest of the 10-member 0.3.220 vocabulary. Without these four, four
    // distinct failures were reported to the user as "unknown".
    case "oauth_org_not_allowed": return AssistantMessageError.OAUTH_ORG_NOT_ALLOWED;
    case "overloaded": return AssistantMessageError.OVERLOADED;
    case "model_not_found": return AssistantMessageError.MODEL_NOT_FOUND;
    case "max_output_tokens": return AssistantMessageError.MAX_OUTPUT_TOKENS;
    default: return AssistantMessageError.UNKNOWN;
  }
}

function apiKeySourceEnum(s: string): ApiKeySource {
  switch (s) {
    case "user": return ApiKeySource.USER;
    case "project": return ApiKeySource.PROJECT;
    case "org": return ApiKeySource.ORG;
    case "temporary": return ApiKeySource.TEMPORARY;
    case "none": return ApiKeySource.NONE;
    default: return ApiKeySource.UNSPECIFIED;
  }
}

function mcpStateEnum(s: string): McpServerState {
  switch (s) {
    case "connected": return McpServerState.CONNECTED;
    case "failed": return McpServerState.FAILED;
    // The wire spelling is HYPHENATED. Matching only "needs_auth" meant a
    // server actually waiting on the user converted to UNSPECIFIED, so the one
    // status a human has to act on was the one that got lost.
    case "needs-auth":
    case "needs_auth": return McpServerState.NEEDS_AUTH;
    case "pending": return McpServerState.PENDING;
    case "disabled": return McpServerState.DISABLED;
    default: return McpServerState.UNSPECIFIED;
  }
}

function compactTriggerEnum(s: string): CompactTrigger {
  switch (s) {
    case "manual": return CompactTrigger.MANUAL;
    case "auto": return CompactTrigger.AUTO;
    default: return CompactTrigger.UNSPECIFIED;
  }
}

function taskKindEnum(taskType: string): TaskKind {
  switch (taskType) {
    case "local_bash": return TaskKind.SHELL;
    case "local_agent": return TaskKind.AGENT;
    case "local_workflow": return TaskKind.WORKFLOW;
    default: return TaskKind.UNSPECIFIED;
  }
}

/** RawTaskStatus vocabulary (§C4) → core TerminalStatus; "stopped" → STOPPED. */
function terminalStatusEnum(status: string): TerminalStatus {
  switch (status) {
    case "completed": return TerminalStatus.DONE;
    case "failed": return TerminalStatus.ERROR;
    case "killed": return TerminalStatus.KILLED;
    case "stopped": return TerminalStatus.STOPPED;
    default: return TerminalStatus.UNSPECIFIED;
  }
}

/** `gitOperation.commit.kind` vocabulary → GitCommitKind. */
function gitCommitKindEnum(s: string): GitCommitKind {
  switch (s) {
    case "committed": return GitCommitKind.COMMITTED;
    case "amended": return GitCommitKind.AMENDED;
    case "cherry-picked": return GitCommitKind.CHERRY_PICKED;
    default: return GitCommitKind.UNSPECIFIED;
  }
}

/** `gitOperation.branch.action` vocabulary → GitBranchAction. */
function gitBranchActionEnum(s: string): GitBranchAction {
  switch (s) {
    case "merged": return GitBranchAction.MERGED;
    case "rebased": return GitBranchAction.REBASED;
    default: return GitBranchAction.UNSPECIFIED;
  }
}

/** `gitOperation.pr.action` vocabulary → GitPullRequestAction. */
function gitPrActionEnum(s: string): GitPullRequestAction {
  switch (s) {
    case "created": return GitPullRequestAction.CREATED;
    case "edited": return GitPullRequestAction.EDITED;
    case "merged": return GitPullRequestAction.MERGED;
    case "commented": return GitPullRequestAction.COMMENTED;
    case "closed": return GitPullRequestAction.CLOSED;
    case "ready": return GitPullRequestAction.READY;
    case "draft": return GitPullRequestAction.DRAFT;
    case "auto-merge-enabled": return GitPullRequestAction.AUTO_MERGE_ENABLED;
    case "auto-merge-disabled": return GitPullRequestAction.AUTO_MERGE_DISABLED;
    default: return GitPullRequestAction.UNSPECIFIED;
  }
}

/** TaskOutput's `retrieval_status` vocabulary → RetrievalStatus. */
function retrievalStatusEnum(s: string): RetrievalStatus {
  switch (s) {
    case "success": return RetrievalStatus.SUCCESS;
    case "timeout": return RetrievalStatus.TIMEOUT;
    case "not_ready": return RetrievalStatus.NOT_READY;
    default: return RetrievalStatus.UNSPECIFIED;
  }
}

function rawTaskStatusEnum(s: string): number {
  // Imported lazily via numeric map to avoid an extra import churn; RawTaskStatus
  // values mirror the proto (async_launched=1 … stopped=6).
  switch (s) {
    case "async_launched": return 1;
    case "running": return 2;
    case "completed": return 3;
    case "failed": return 4;
    case "killed": return 5;
    case "stopped": return 6;
    default: return 0;
  }
}

// ---------------------------------------------------------------------------
// low-level helpers
// ---------------------------------------------------------------------------

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function pick(o: Record<string, unknown>, ...keys: string[]): unknown {
  for (const k of keys) if (k in o) return o[k];
  return undefined;
}

function strOf(v: unknown): string {
  return typeof v === "string" ? v : "";
}

function numOf(v: unknown): number {
  return typeof v === "number" && Number.isFinite(v) ? v : 0;
}

function bigOf(v: unknown): bigint {
  return typeof v === "number" && Number.isFinite(v) ? BigInt(Math.trunc(v)) : 0n;
}

function strListOf(v: unknown): string[] {
  return Array.isArray(v) ? v.filter((x): x is string => typeof x === "string") : [];
}

function asStruct(v: unknown): JsonObject {
  return isObject(v) ? (v as JsonObject) : {};
}

function asStructOpt(v: unknown): JsonObject | undefined {
  return isObject(v) ? (v as JsonObject) : undefined;
}

/**
 * Build a `google.protobuf.ListValue` from a JSON array subtree.
 *
 * Unlike Struct (whose init accepts a plain JsonObject), a ListValue field's
 * init demands the message form — handing it a bare array silently yields an
 * EMPTY list. `fromJson` on the ListValue schema is the only faithful path:
 * ListValue's JSON representation IS the array, so heterogeneous elements
 * (WebSearchResult.results interleaves objects and strings) survive verbatim.
 * A non-array is NOT coerced — the caller decides (there is no correct
 * fallback for a shape the corpus says is always an array).
 */
function asListValueOpt(v: unknown): ListValue | undefined {
  return Array.isArray(v) ? fromJson(ListValueSchema, v as JsonValue[]) : undefined;
}

/**
 * Map a raw `questions` array onto the typed `Question` element the schema
 * shares with AskUserQuestionInput (corpus: tool-results/ask_user_question).
 * A non-array or a non-object element is skipped rather than guessed.
 */
function questionsOf(v: unknown): Question[] {
  if (!Array.isArray(v)) return [];
  const out: Question[] = [];
  for (const q of v) {
    if (!isObject(q)) continue;
    const rawOpts = q["options"];
    out.push(create(QuestionSchema, {
      question: strOf(q["question"]),
      header: strOf(q["header"]),
      options: (Array.isArray(rawOpts) ? rawOpts : []).filter(isObject).map((opt) =>
        create(QuestionOptionSchema, {
          label: strOf(opt["label"]),
          description: strOf(opt["description"]),
          preview: strOf(opt["preview"]),
        })),
      multiSelect: pick(q, "multi_select", "multiSelect") === true,
    }));
  }
  return out;
}

/**
 * Map a raw `statusChange` onto the typed `TaskStatusChange` (corpus:
 * tool-results/task_update). A non-object is not a transition and yields
 * undefined (the field stays absent) rather than a fabricated {from:"",to:""}.
 */
function statusChangeOf(v: unknown): TaskStatusChange | undefined {
  if (!isObject(v)) return undefined;
  return create(TaskStatusChangeSchema, { from: strOf(v["from"]), to: strOf(v["to"]) });
}

/**
 * Map a raw `pin` onto the typed `MessagePin` (corpus:
 * tool-results/send_message). A non-object is not a pin and yields undefined
 * rather than an all-empty pin that would read as a real one downstream.
 */
function pinOf(v: unknown): MessagePin | undefined {
  if (!isObject(v)) return undefined;
  return create(MessagePinSchema, {
    id: strOf(v["id"]),
    name: strOf(v["name"]),
    ref: strOf(v["ref"]),
  });
}

/**
 * Map a Bash result's `gitOperation` onto the typed `GitOperation` (sdk
 * BashOutput). Four INDEPENDENT optional arms, not a union: a single command
 * routinely reports two at once (census: commit+push, branch+push, pr+push).
 *
 * A non-object yields undefined, and so does an object with no recognized arm
 * — an all-empty GitOperation would read downstream as "this command did git
 * work we could not classify", which is a different claim from "no git work".
 */
function gitOperationOf(v: unknown): GitOperation | undefined {
  if (!isObject(v)) return undefined;
  const commit = asStructOpt(v["commit"]);
  const push = asStructOpt(v["push"]);
  const branch = asStructOpt(v["branch"]);
  const pr = asStructOpt(v["pr"]);
  if (!commit && !push && !branch && !pr) return undefined;
  return create(GitOperationSchema, {
    commit: commit && create(GitCommitSchema, {
      sha: strOf(commit["sha"]),
      kind: gitCommitKindEnum(strOf(commit["kind"])),
    }),
    push: push && create(GitPushSchema, { branch: strOf(push["branch"]) }),
    branch: branch && create(GitBranchSchema, {
      ref: strOf(branch["ref"]),
      action: gitBranchActionEnum(strOf(branch["action"])),
    }),
    pr: pr && create(GitPullRequestSchema, {
      number: bigOf(pr["number"]),
      url: strOf(pr["url"]),
      action: gitPrActionEnum(strOf(pr["action"])),
    }),
  });
}

/**
 * Map a WebFetch result's `artifactRead` onto the typed `ArtifactRead` (sdk
 * 0.3.220 WebFetchOutput). An ordinary fetch omits the key entirely, so a
 * non-object yields undefined and the field stays absent — an all-empty
 * ArtifactRead would read downstream as "this fetch WAS an Artifact".
 */
function artifactReadOf(v: unknown): ArtifactRead | undefined {
  if (!isObject(v)) return undefined;
  return create(ArtifactReadSchema, { slug: strOf(v["slug"]), ver: strOf(v["ver"]) });
}

function uuidOf(message: Record<string, unknown>): string {
  return typeof message["uuid"] === "string" ? message["uuid"] : "";
}

function base64ToBytes(s: string): Uint8Array {
  return new Uint8Array(Buffer.from(s, "base64"));
}

function parseTimestamp(v: unknown): number | undefined {
  if (typeof v !== "string") return undefined;
  const ms = Date.parse(v);
  return Number.isNaN(ms) ? undefined : ms;
}

function safeStringify(v: unknown): string {
  try {
    return JSON.stringify(v) ?? String(v);
  } catch {
    return String(v);
  }
}

