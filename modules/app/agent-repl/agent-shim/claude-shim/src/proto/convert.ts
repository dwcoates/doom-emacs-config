/**
 * The claude-shim SDK-message → proto converter (design §8.2, §5.3).
 *
 * Each raw SDK stream message (the JSON the SDK `query()` iterator yields —
 * exactly the shapes in testdata/corpus/stream/) becomes a core `Event`
 * envelope (plane = STREAM, seq = 0 pre-ingest) whose `vendor` Any wraps the
 * total-fidelity `data.v1.ClaudeStreamMessage`. The switch is TOTAL: every
 * typed SDK family plus every observed-only family (task_started/_updated/
 * _notification, background_tasks_changed, rate_limit_event, hook_started,
 * thinking_tokens, notification) is modeled; an unknown `type`/`subtype`
 * discriminator hard-errors into a core `UnparsedEvent` (§5.1), never a zero
 * value.
 *
 * LIFECYCLE TWINS: where derivable, vendor-neutral core lifecycle Events are
 * emitted ALONGSIDE the vendor event (as separate Events the store also
 * persists), so consumers get a clean turn/task/session spine without
 * decoding the vendor payload:
 *   - system:init                 → SessionStarted
 *   - result                      → TurnEnded
 *   - user (NOT a tool-result)    → TurnStarted (bounded prompt preview)
 *   - system:task_started         → TaskStarted
 *   - system:task_notification    → TaskEnded  (RawTaskStatus → TerminalStatus)
 *   - system:task_updated         → TaskEnded  (patch.status → TerminalStatus)
 *   - system:background_tasks_changed → vendor-only (no twin)
 * Dedup keys are NEVER set here: the store derives `uuid:`/`tur:` itself.
 *
 * CLASS: PERSISTENT for everything, EXCEPT `stream_event` / `tool_progress`,
 * which are EPHEMERAL (their live-typing/heartbeat relay is {@link
 * import("./delta.js")}'s job). convert() still models them for total-fidelity
 * decode (probes/tests/diagnostics), marking the envelope EPHEMERAL so the
 * wiring never routes them to `StoreWrite`.
 */
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { anyPack } from "@bufbuild/protobuf/wkt";
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
  TurnStartedSchema,
  type Event,
} from "../uds/proto.js";
import {
  MissingFieldError,
  Reader,
  unparsedEvent,
  type ExtrasOutcome,
} from "./extras.js";
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
  ControlResponseSchema,
  HookResponseSchema,
  HookStartedSchema,
  InputJsonDeltaSchema,
  KeepAliveSchema,
  McpServerState,
  McpServerStatusSchema,
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
  type ClaudeStreamMessage,
  type ModelUsage,
} from "../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import {
  ApiAssistantMessageSchema,
  ApiContentBlocksSchema,
  ApiUsageSchema,
  ApiUserMessageSchema,
  BashResultSchema,
  CallerSchema,
  ContentBlockSchema,
  FallbackBlockSchema,
  FallbackModelRefSchema,
  ImageBlockSchema,
  ImageSourceSchema,
  MonitorResultSchema,
  ReadResultSchema,
  SkillResultSchema,
  TaskCreateResultSchema,
  TaskListResultSchema,
  TaskStopResultSchema,
  TextBlockSchema,
  ThinkingBlockSchema,
  ToolReferenceBlockSchema,
  ToolResultBlockListSchema,
  ToolResultBlockSchema,
  ToolSearchResultSchema,
  ToolUseBlockSchema,
  ToolUseResultSchema,
  WorkflowLaunchResultSchema,
  type ContentBlock,
  type ToolUseResult,
} from "../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";

const COMPONENT = "claude-shim-convert";

/** How far a TurnStarted prompt preview is bounded (first line, then chars). */
const PROMPT_PREVIEW_CAP = 200;

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
      return vendorOnly({ case: "controlResponse", value: create(ControlResponseSchema, { requestId: r.str("request_id", "requestId"), response: r.struct("response") }) }, type);
    case "control_cancel_request":
      return vendorOnly({ case: "controlCancelRequest", value: create(ControlCancelRequestSchema, { requestId: r.str("request_id", "requestId") }) }, type);
    case "keep_alive":
      return vendorOnly({ case: "keepAlive", value: create(KeepAliveSchema, {}) }, type);
    case "system":
      return buildSystem(message, r, opts);
    default:
      throw new MissingFieldError(`unknown SDK message type "${type}"`);
  }
}

function buildSystem(message: Record<string, unknown>, r: Reader, opts?: ConvertOptions): Built {
  const subtype = r.str("subtype");
  const label = `system:${subtype}`;
  switch (subtype) {
    case "init":
      return buildSystemInit(message, r, label, opts);
    case "status":
      return vendorOnly({ case: "status", value: create(StatusMessageSchema, { status: r.str("status"), uuid: uuidOf(message), sessionId: r.str("session_id", "sessionId") }) }, label);
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
    default:
      throw new MissingFieldError(`unknown system subtype "${subtype}"`);
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
  });

  // A user message that is NOT a tool-result carrier is a genuine prompt turn.
  const lifecyclePayloads: Event["payload"][] = [];
  if (!carriesToolResult(rawMessage)) {
    lifecyclePayloads.push({
      case: "turnStarted",
      value: create(TurnStartedSchema, { promptPreview: promptPreview(rawMessage) }),
    });
  }
  return { csm: csm({ case: "user", value: userMsg }), lifecyclePayloads, typeLabel: "user", ephemeral: false };
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
  });
  return { csm: csm({ case: "toolProgress", value: tp }), lifecyclePayloads: [], typeLabel: "tool_progress", ephemeral: true };
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
  });
  const sessionStarted = create(SessionStartedSchema, {
    source: opts?.sessionSource ?? SessionSource.FRESH,
    model: init.model,
    cwd: init.cwd,
    vendorSessionId: init.sessionId,
  });
  return {
    csm: csm({ case: "systemInit", value: init }),
    lifecyclePayloads: [{ case: "sessionStarted", value: sessionStarted }],
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
    }),
    uuid: uuidOf(message),
    sessionId: r.str("session_id", "sessionId"),
  });
  const taskEnded = create(TaskEndedSchema, {
    taskId,
    kind: TaskKind.UNSPECIFIED,
    status: terminalStatusEnum(status),
    summary: "",
    outputPath: "",
    inference: "",
  });
  return {
    csm: csm({ case: "taskUpdated", value: msg }),
    lifecyclePayloads: [{ case: "taskEnded", value: taskEnded }],
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
      return create(ContentBlockSchema, { block: { case: "toolReference", value: create(ToolReferenceBlockSchema, { reference: asStruct(raw) }) } });
    case "fallback":
      return create(ContentBlockSchema, { block: { case: "fallback", value: create(FallbackBlockSchema, { from: fallbackModel(raw["from"]), to: fallbackModel(raw["to"]) }) } });
    default:
      // No catch-all arm exists on ContentBlock; loud-log the skip so an
      // unmodeled block type is visible, never silently absorbed.
      shimLog(COMPONENT, { block_type: String(raw["type"]) }, `unmodeled content block type skipped`);
      return null;
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
      gitOperation: strOf(pick(o, "git_operation", "gitOperation")),
      persistedOutputPath: strOf(pick(o, "persisted_output_path", "persistedOutputPath")),
      persistedOutputSize: bigOf(pick(o, "persisted_output_size", "persistedOutputSize")),
      timedOutAfterMs: bigOf(pick(o, "timed_out_after_ms", "timedOutAfterMs")),
      staleReadFileStateHint: strOf(pick(o, "stale_read_file_state_hint", "staleReadFileStateHint")),
      backgroundTaskId: strOf(pick(o, "background_task_id", "backgroundTaskId")),
      backgroundCwdHint: strOf(pick(o, "background_cwd_hint", "backgroundCwdHint")),
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
  if (has("task") && isObject(o["task"])) {
    return { case: "taskCreate", value: create(TaskCreateResultSchema, { task: o["task"] as JsonObject }) };
  }
  if (has("tasks") && Array.isArray(o["tasks"]) && Object.keys(o).length === 1) {
    return { case: "taskList", value: create(TaskListResultSchema, { tasks: { tasks: o["tasks"] } as JsonObject }) };
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
  return null;
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
  return raw.filter(isObject).map((s) => create(McpServerStatusSchema, {
    name: strOf(pick(s, "name")),
    status: mcpStateEnum(strOf(pick(s, "status"))),
  }));
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
    case "needs_auth": return McpServerState.NEEDS_AUTH;
    case "pending": return McpServerState.PENDING;
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

function carriesToolResult(rawMessage: Record<string, unknown>): boolean {
  const content = rawMessage["content"];
  return Array.isArray(content) && content.some((b) => isObject(b) && b["type"] === "tool_result");
}

function promptPreview(rawMessage: Record<string, unknown>): string {
  const content = rawMessage["content"];
  let text = "";
  if (typeof content === "string") {
    text = content;
  } else if (Array.isArray(content)) {
    const firstText = content.find((b) => isObject(b) && b["type"] === "text");
    if (isObject(firstText)) text = strOf(firstText["text"]);
  }
  const firstLine = text.split("\n", 1)[0] ?? "";
  return firstLine.slice(0, PROMPT_PREVIEW_CAP);
}
