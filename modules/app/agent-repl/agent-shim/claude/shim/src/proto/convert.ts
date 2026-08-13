/**
 * The claude-shim SDK-message → record converter.
 *
 * # Conversion happens AT THE EDGE now
 *
 * The old converter transliterated the SDK stream into `agentshim.data.v1` —
 * 267 messages whose names and arms were the vendor's own API surface — and
 * shipped them across the wire for downstream systems to interpret. Every
 * consumer that read one had to know the vendor to understand it.
 *
 * What leaves this shim is neutral. Each raw SDK message becomes zero or more
 * `agentshim.v1.Entry` records, and an Entry has two halves:
 *
 *   - the EXTERNAL half (`protocol.v1.ExternalEntry`) is what the daemon
 *     receives — by field access, not by a mapping function that could forget a
 *     field or drift from the record it came from;
 *   - the INTERNAL half never crosses the wire, and carries the observation
 *     plane plus, for anything we could not convert, the record whole.
 *
 * A record with NO external half is the unrenderable case: it is stored, and it
 * simply has nothing to hand over. That is what makes eager conversion at the
 * edge safe rather than lossy.
 *
 * # What the STREAM plane may say
 *
 * The shim is authoritative for session and turn LIFECYCLE, and that is what it
 * converts: `system:init` becomes `SessionBegan`, `result` becomes `TurnEnded`,
 * `conversation_reset` becomes `SessionIdentityChanged`. Every one is a fact
 * ABOUT the session — bookkeeping — and none of them is a message.
 *
 * IT PRODUCES NO CONVERSATION CONTENT, and cannot: a `MessageEntry` must state
 * its `parent`, and the SDK stream carries no parent pointer. The file plane
 * owns content because that is what the vendor itself recorded. So an
 * `assistant` message, a `user` echo, a `compact_boundary`, a task lifecycle
 * frame — all of them are UNDERSTOOD and all of them land on
 * `VendorSpecificEntry`, stored whole, never forwarded.
 *
 * The ONE exception is the live typing preview, which is
 * {@link import("./delta.js")}'s job and never this one's.
 *
 * # Two distinct failure channels, which are not interchangeable
 *
 *   - An unknown `type`/`subtype` DISCRIMINATOR takes `UnknownEntry`,
 *     preserving the record whole, loud-logged once per distinct discriminator.
 *     The SDK union grew 11 -> 38 members across 0.1.77 -> 0.3.220, so this is
 *     the expected steady state, not an error.
 *   - A record of a KNOWN family that fails conversion (a missing expected
 *     field, an unusable nested discriminator) becomes an `UnparsedEntry`,
 *     never a zero value.
 *
 * # Turn start is not derived here
 *
 * A `user` message is NOT a turn-start signal. The SDK echoes one only on
 * REPLAY (resume / transcript rehydration), never for a prompt the daemon just
 * submitted — so a derived turn start never fired at the one moment it
 * mattered, and when an echo DID arrive it was history, double-counting turns
 * against the `result`. Turn start is SHIM-AUTHORITATIVE: the session emits
 * `TurnBegan` itself the moment it accepts a SubmitPrompt.
 */
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { normalizeModel } from "../model.js";
import {
  InvalidModeledUsageError,
  normalizeApiUsage,
  type NormalizedApiUsage,
} from "../api-usage.js";
import { bindLog } from "../uds/log.js";
import {
  AuthApiKeySchema,
  AuthSubscriptionSchema,
  BookkeepingEntrySchema,
  EntrySchema,
  ExternalEntrySchema,
  FastModeOffSchema,
  FastModeOnSchema,
  FastModeSchema,
  InternalEntrySchema,
  McpServerConnectedSchema,
  McpServerFailedSchema,
  McpServerHealthSchema,
  PlaneSchema,
  PlaneStreamSchema,
  SessionAuthSchema,
  SessionBeganSchema,
  SessionIdentityChangedSchema,
  SessionMcpServerSchema,
  SessionPluginSchema,
  TurnCompletedSchema,
  TurnEndedSchema,
  TurnInterruptedSchema,
  type Entry,
  type ExternalEntry,
} from "../uds/proto.js";
import {
  MissingFieldError,
  Reader,
  logUnknownDiscriminator,
  unknownEntry,
  unknownFieldsEntry,
  unparsedEntry,
  vendorSpecificEntry,
  type ExtrasOutcome,
} from "./extras.js";

const COMPONENT = "claude-shim-convert";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.convert.vendor-message" });

/** How far a turn-start prompt preview is bounded (first line, then chars). */
const PROMPT_PREVIEW_CAP = 200;

/**
 * Bound a prompt to its turn-start preview: first line, then capped.
 *
 * Exported because the turn-start emitter lives in the session (turn start is
 * shim-authoritative, see the file header) while the bounding contract belongs
 * here beside the rest of the record shaping.
 */
export function promptPreview(text: string): string {
  const firstLine = text.split("\n", 1)[0] ?? "";
  return firstLine.slice(0, PROMPT_PREVIEW_CAP);
}

/**
 * Admits the FIRST `system:init` of a shim lifetime (and any later init that
 * announces a DIFFERENT vendor session id) to emit `SessionBegan`.
 *
 * The SDK re-inits on every submit, so an ungated converter announced a session
 * beginning per prompt. Downstream that is not a harmless repeat: a session
 * beginning maps to IDLE, so a re-init landing at submit time knocked the
 * workspace out of THINKING at exactly the moment it entered it. A session
 * begins once; a re-init of the SAME session is not a new session.
 *
 * A CHANGED vendor session id is a genuinely new session (a
 * conversation_reset, a compact-continue), so it re-admits.
 */
export class SessionStartGate {
  private seen: string | null = null;
  private closed = false;

  /** True when this init's `SessionBegan` should be emitted. */
  admit(vendorSessionId: string): boolean {
    if (this.closed) return false;
    if (this.seen === vendorSessionId) return false;
    this.seen = vendorSessionId;
    return true;
  }

  /**
   * Permanently stop admitting: the session's beginning has already been
   * asserted for this shim lifetime by someone else.
   *
   * UdsSession calls this when it announces readiness directly. The vendor's
   * `system:init` is no longer the thing that announces a session — it arrives
   * only on the FIRST PROMPT, far too late to be the signal that the session is
   * usable — so its record would duplicate an assertion already made, arriving
   * after the fact.
   *
   * Closing rather than pre-seeding a sentinel id is deliberate: the id the
   * first init will carry is unknowable at readiness time on a fresh session,
   * so there is nothing to seed it WITH.
   */
  close(): void {
    this.closed = true;
  }
}

/** Wall-clock and turn-identity injection for the conversion. */
export interface ConvertOptions {
  nowMs?: number;
  /**
   * Identity of the accepted root turn a terminal result closes.
   *
   * `TurnEnded.turn_id` is REQUIRED to be meaningful: an end can close only the
   * turn it names, so a late boundary cannot close whatever happens to be open.
   * The SDK's own result envelope omits any turn correlation, and UdsSession
   * owns the accepted-prompt FIFO, so it supplies the authoritative id here.
   */
  rootTurnId?: string;
  /**
   * Whether the turn this terminal result closes is one the session ACKED as
   * interrupted.
   *
   * The SDK has no word for it: a stopped turn comes back as an ordinary error
   * flavor, indistinguishable from a turn that broke on its own. `TurnEnded`
   * treats `interrupted` as an ACCUSATION the evidence must support, so only a
   * user-commanded stop the producer saw acknowledged may set it — which is
   * exactly what this flag reports.
   *
   * Ignored on every message that is not a `result`, and ignored when the
   * result closes no turn (`rootTurnId` absent).
   */
  interrupted?: boolean;
  /**
   * The shim-lifetime gate deciding whether a `system:init` announces a session
   * beginning. UdsSession always supplies one (it owns the shim lifetime the
   * gate is scoped to). Absent — the single-message decode path used by probes
   * and unit tests — every init announces, because a lone convert() call has no
   * lifetime to be the second init of.
   */
  sessionGate?: SessionStartGate;
}

/** The full result of converting one SDK message. */
export interface ConvertResult {
  /**
   * The records the store must hold, in the order the producer observed them.
   * Empty for a family the live relay owns.
   */
  entries: Entry[];
  /** Newly loud-logged `<type>.<field>` paths (for tests/wiring). */
  loggedExtras: string[];
  /** Canonically validated assistant usage for the structured accounting log. */
  assistantApiUsage?: NormalizedApiUsage;
}

/**
 * Convert one raw SDK stream message. Never throws: a record it cannot parse
 * comes back as a single `UnparsedEntry`.
 */
export function convert(message: unknown, opts?: ConvertOptions): ConvertResult {
  if (!isObject(message)) {
    const reason = "message is not a JSON object";
    logUnparsed("<non-object>", "", reason, { input_kind: message === null ? "null" : typeof message });
    return { entries: [unparsedEntry(safeStringify(message), reason, { log: false })], loggedExtras: [] };
  }
  const type = message["type"];
  if (typeof type !== "string" || type === "") {
    const reason = "message has no string `type` discriminator";
    const sessionId = strOf(pick(message, "session_id", "sessionId"));
    logUnparsed("<missing>", sessionId, reason);
    return {
      entries: [unparsedEntry(safeStringify(message), reason, { sessionId, log: false })],
      loggedExtras: [],
    };
  }

  const envelope = readEnvelope(message, opts);
  LOGGER.logVerbose({
    ...(envelope.sessionId === "" ? {} : { claude_session_id: envelope.sessionId }),
    sdk_type: type,
  }, "converting SDK message");

  try {
    const built = build(type, message, envelope, opts);
    const extras = envelope.reader.finish(built.typeLabel);
    const entries = [...built.entries, ...companionEntries(built.typeLabel, extras)];
    LOGGER.logVerbose({
      ...(envelope.sessionId === "" ? {} : { claude_session_id: envelope.sessionId }),
      sdk_type: type,
      entry_count: entries.length,
      extra_count: extras.logged.length,
    }, "converted SDK message into records");
    return {
      entries,
      loggedExtras: extras.logged,
      ...(built.assistantApiUsage === undefined ? {} : { assistantApiUsage: built.assistantApiUsage }),
    };
  } catch (err) {
    if (err instanceof MissingFieldError || err instanceof InvalidModeledUsageError) {
      logUnparsed(type, envelope.sessionId, err.message,
        err instanceof InvalidModeledUsageError
          ? {
              usage_field: err.field,
              usage_field_path: err.fieldPath,
              raw_value: err.rawValue,
              raw_value_kind: err.rawValue === null ? "null" : typeof err.rawValue,
              raw_usage: err.rawUsage,
            }
          : {});
      return {
        entries: [unparsedEntry(safeStringify(message), err.message, {
          sessionId: envelope.sessionId,
          log: false,
        })],
        loggedExtras: [],
      };
    }
    throw err;
  }
}

/**
 * The companion record carrying top-level fields a family grew that this
 * converter does not read, when there are any.
 *
 * `Event.extras` used to carry them and nothing replaces it, so they ride an
 * internal-only `VendorSpecificEntry` instead — durable, attributable, and
 * unreachable by any consumer that might act on a value nobody modeled.
 */
function companionEntries(typeLabel: string, extras: ExtrasOutcome): Entry[] {
  return extras.extras === undefined ? [] : [unknownFieldsEntry(typeLabel, extras.extras)];
}

/** Record one converter-owned malformed-record cause without copying raw data. */
function logUnparsed(
  sdkType: string,
  sessionId: string,
  reason: string,
  context: Record<string, unknown> = {},
): void {
  LOGGER.log({
    level: "error",
    sdk_type: sdkType,
    reason,
    ...(sessionId === "" ? {} : { claude_session_id: sessionId }),
    ...context,
  }, "SDK message emitted as an UnparsedEntry");
}

// ---------------------------------------------------------------------------
// Envelope (shared across all message types)
// ---------------------------------------------------------------------------

interface Envelope {
  reader: Reader;
  /** The vendor's session identity — the only id every producer agrees on. */
  sessionId: string;
  /** The producer's wall clock at observation, in unix millis. */
  producedAtMs: bigint;
}

/**
 * Read the fields common to every `ExternalEntry` and consume them off the
 * reader: `session_id` (routing) and `timestamp` (producer wall-clock).
 *
 * `uuid` and `request_id` are consumed WITHOUT being carried, and both
 * deliberately. The vendor uuid is the file plane's business — it is how the
 * transcript names a record, and the shim writes no content. `request_id` had a
 * slot on the retired `Event` envelope and has none on `ExternalEntry`; the one
 * correlation that survived is `TurnEnded.turn_id`, which UdsSession supplies
 * from the authority that actually owns it.
 */
function readEnvelope(message: Record<string, unknown>, opts?: ConvertOptions): Envelope {
  const reader = new Reader(message);
  const sessionId = reader.str("session_id", "sessionId");
  reader.ignore("uuid", "request_id", "requestId");
  const producedAtMs = parseTimestamp(reader.val("timestamp")) ?? opts?.nowMs ?? Date.now();
  return { reader, sessionId, producedAtMs: BigInt(producedAtMs) };
}

/**
 * One record the store will hold: the internal half (which plane observed it)
 * and the external half the daemon receives.
 *
 * `write_id` is left empty for the store client to mint ONCE, because a replay
 * must re-present the identity the record was first delivered under.
 */
function bookkeepingEntry(env: Envelope, kind: BookkeepingKind): Entry {
  return create(EntrySchema, {
    internal: create(InternalEntrySchema, {
      plane: create(PlaneSchema, { plane: { case: "stream", value: create(PlaneStreamSchema, {}) } }),
    }),
    external: create(ExternalEntrySchema, {
      sessionId: env.sessionId,
      producedAtMs: env.producedAtMs,
      entry: { case: "bookkeeping", value: create(BookkeepingEntrySchema, { kind }) },
    }),
  });
}

type BookkeepingKind = Extract<ExternalEntry["entry"], { case: "bookkeeping" }>["value"]["kind"];

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

interface Built {
  entries: Entry[];
  /** For unknown-field logging (`<type>.<field>`). */
  typeLabel: string;
  assistantApiUsage?: NormalizedApiUsage;
}

/**
 * A family we UNDERSTAND and deliberately do not carry into a neutral feed.
 *
 * The bulk of the SDK stream lands here, and that is the design rather than an
 * omission: conversation content belongs to the file plane (only it can resolve
 * a parent), and everything else the vendor reports has no neutral spelling.
 * The record is stored whole, so the decision not to model it is reversible
 * from stored data.
 */
function vendorOnly(message: Record<string, unknown>, r: Reader, typeLabel: string): Built {
  r.consumeAll();
  return { entries: [vendorSpecificEntry(typeLabel, message as JsonObject)], typeLabel };
}

function build(
  type: string,
  message: Record<string, unknown>,
  env: Envelope,
  opts?: ConvertOptions,
): Built {
  const r = env.reader;
  r.ignore("type");
  switch (type) {
    // ---- Converted: the lifecycle facts the stream plane is authoritative for.
    case "system":
      return buildSystem(message, env, opts);
    case "result":
      return buildResult(message, env, opts);
    case "conversation_reset":
      return buildConversationReset(message, env);

    // ---- The live relay owns these, and this converter must not double them.
    //
    // They are ROUTED by delta.ts's isEphemeral() and reach the daemon on the
    // `live` arm, which has no field a store position could go in. Returning no
    // durable record here is what makes "a preview is never written" structural
    // rather than a flag the wiring has to keep set correctly, which is what
    // the retired EventClass.EPHEMERAL used to be.
    case "stream_event":
    case "tool_progress":
      r.consumeAll();
      LOGGER.logVerbose({
        ...(env.sessionId === "" ? {} : { claude_session_id: env.sessionId }),
        sdk_type: type,
      }, "live-relay family produces no durable record here");
      return { entries: [], typeLabel: type };

    // ---- Understood, and not carried into a neutral feed.
    //
    // `user` and `assistant` are conversation CONTENT and are the file plane's
    // to write: a MessageEntry must state its parent, and the stream has none.
    case "user":
      return buildAssistantUsageOnly(message, r, "user", undefined);
    case "assistant":
      return buildAssistant(message, r);
    case "auth_status":
    case "rate_limit_event":
    case "compact_boundary":
    case "control_request":
    case "control_response":
    case "control_cancel_request":
    case "keep_alive":
    case "tool_use_summary":
    case "prompt_suggestion":
    case "active_goal":
      return vendorOnly(message, r, type);

    default:
      // PASSTHROUGH, not an error: an unmodeled `type` means the SDK union
      // grew, which it does constantly. The record is preserved whole. A
      // message of a type we DO convert that fails conversion still becomes an
      // UnparsedEntry, via the MissingFieldError the typed builders throw.
      r.consumeAll();
      logUnknownDiscriminator(type, "");
      return { entries: [unknownEntry(type, "type", message as JsonObject)], typeLabel: type };
  }
}

function buildSystem(
  message: Record<string, unknown>,
  env: Envelope,
  opts?: ConvertOptions,
): Built {
  const r = env.reader;
  const subtype = r.str("subtype");
  const label = `system:${subtype}`;
  switch (subtype) {
    case "init":
      return buildSystemInit(message, env, label, opts);

    // Every other system subtype is understood and has no neutral spelling.
    // Several are real losses and are recorded as gaps rather than invented:
    // task lifecycle becomes DetachedWork MESSAGES, which need a parent the
    // stream does not carry; api_retry, rate limiting and hook progress have no
    // arm on any surface at all.
    case "status":
    case "hook_response":
    case "hook_started":
    case "hook_progress":
    case "thinking_tokens":
    case "notification":
    case "task_started":
    case "task_updated":
    case "task_notification":
    case "task_progress":
    case "background_tasks_changed":
    case "compact_boundary":
    case "api_retry":
    case "control_request_progress":
    case "model_refusal_fallback":
    case "model_refusal_no_fallback":
    case "local_command_output":
    case "plugin_install":
    case "session_state_changed":
    case "worker_shutting_down":
    case "commands_changed":
    case "files_persisted":
    case "memory_recall":
    case "elicitation_complete":
    case "permission_denied":
    case "mirror_error":
    case "informational":
      return vendorOnly(message, r, label);

    default:
      // PASSTHROUGH: an unmodeled system subtype, captured whole and named for
      // the field it was read from, so a new VARIANT of a family we know is
      // distinguishable from a producer that looked in the wrong place.
      r.consumeAll();
      logUnknownDiscriminator(subtype, "system");
      return { entries: [unknownEntry(subtype, "subtype", message as JsonObject)], typeLabel: label };
  }
}

// ---------------------------------------------------------------------------
// system:init → SessionBegan
// ---------------------------------------------------------------------------

/**
 * The facts the `/status` panel is resolved from.
 *
 * They used to be read off the vendor's own init record, which the daemon
 * retained whole and pushed to the client as a lenient JSON object — so the
 * client derived every value the user saw. The producer states neutral facts
 * here, the daemon resolves them into rows, the panel prints them.
 *
 * EVERY FIELD IS THE STARTING POINT, not the standing answer: a session can
 * switch model, gain an MCP server or reload skills mid-run.
 */
function buildSystemInit(
  message: Record<string, unknown>,
  env: Envelope,
  label: string,
  opts?: ConvertOptions,
): Built {
  const r = env.reader;
  const rawModel = r.str("model");
  const model = normalizeModel(rawModel);
  if (model !== rawModel) {
    LOGGER.log({
      ...(env.sessionId === "" ? {} : { claude_session_id: env.sessionId }),
      reported_model: rawModel,
      normalized_model: model,
    }, "normalized empty-equivalent system:init model");
  }

  const began = create(SessionBeganSchema, {
    model,
    cwd: r.str("cwd"),
    agentVersion: r.str("claude_code_version", "claudeCodeVersion"),
    auth: sessionAuth(r.str("api_key_source", "apiKeySource")),
    outputStyle: r.str("output_style", "outputStyle"),
    fastMode: fastMode(
      r.str("fast_mode_state", "fastModeState"),
      r.str("fast_mode_disabled_reason", "fastModeDisabledReason"),
    ),
    skills: r.strList("skills"),
    // The vendor calls them `agents`; the neutral model calls them what they
    // are, which is the agents a turn can dispatch INTO.
    subagents: r.strList("agents"),
    mcpServers: mcpServers(r.arr("mcp_servers", "mcpServers")),
    plugins: plugins(r.arr("plugins")),
    // A LIST of paths rather than the vendor's name->path map: nothing renders
    // the names, and a map invites a consumer to look one up by a key this
    // schema does not promise.
    memoryPaths: mapValues(r.obj("memory_paths", "memoryPaths")),
  });

  // Recognized, and with no field on SessionBegan to carry them. They are
  // carried rather than logged as unknown, because they are not a surprise —
  // the neutral model simply does not state them.
  r.carry(
    "tools",
    "slash_commands", "slashCommands",
    "permission_mode", "permissionMode",
    "betas",
    "capabilities",
    "analytics_disabled", "analyticsDisabled",
    "product_feedback_disabled", "productFeedbackDisabled",
  );

  // The SDK re-inits per submit; only the FIRST init of a shim lifetime (or one
  // announcing a different vendor session id) is a session BEGINNING.
  const admitted = opts?.sessionGate === undefined || opts.sessionGate.admit(env.sessionId);
  if (!admitted) {
    LOGGER.log({ claude_session_id: env.sessionId },
      "system:init re-announced an already-begun session; SessionBegan suppressed and the record kept whole");
    // Suppressed as a lifecycle assertion, NOT discarded: the re-init is still
    // something the vendor said, and it is stored where nothing can act on it.
    r.consumeAll();
    return { entries: [vendorSpecificEntry(`${label}.re-announced`, message as JsonObject)], typeLabel: label };
  }

  return {
    entries: [bookkeepingEntry(env, { case: "sessionBegan", value: began })],
    typeLabel: label,
  };
}

/**
 * How the session is authenticated.
 *
 * A oneof rather than the vendor's enum, because the two cases are genuinely
 * different things rather than two values of one thing — and because the
 * vendor's `"none"` means a SUBSCRIPTION LOGIN, which is exactly the value that
 * gets read as "unauthenticated" by the next person to touch it.
 */
function sessionAuth(apiKeySource: string) {
  if (apiKeySource === "" || apiKeySource === "none") {
    return create(SessionAuthSchema, {
      auth: { case: "subscription", value: create(AuthSubscriptionSchema, {}) },
    });
  }
  return create(SessionAuthSchema, {
    auth: { case: "apiKey", value: create(AuthApiKeySchema, { source: apiKeySource }) },
  });
}

/**
 * Whether the vendor's accelerated mode is on, and — when it is off and the
 * producer knows — WHY, so the state is explicable rather than a bare flag.
 */
function fastMode(state: string, disabledReason: string) {
  if (state === "on" || state === "enabled" || state === "active") {
    return create(FastModeSchema, { state: { case: "on", value: create(FastModeOnSchema, {}) } });
  }
  return create(FastModeSchema, {
    state: { case: "off", value: create(FastModeOffSchema, { reason: disabledReason }) },
  });
}

/** The MCP servers configured, and whether each one came up. */
function mcpServers(raw: unknown[] | undefined) {
  if (raw === undefined) return [];
  return raw.filter(isObject).map((s) => create(SessionMcpServerSchema, {
    name: strOf(pick(s, "name")),
    health: mcpHealth(strOf(pick(s, "status")), strOf(pick(s, "error"))),
  }));
}

/**
 * Whether one MCP server is usable.
 *
 * Only `connected` is connected. `needs-auth`, `pending`, `disabled` and every
 * unrecognized status are FAILED-with-a-reason rather than silently connected:
 * the neutral model has two arms, and reporting a server the user has to
 * authenticate as usable is the one mistake that matters here.
 */
function mcpHealth(status: string, error: string) {
  if (status === "connected") {
    return create(McpServerHealthSchema, {
      health: { case: "connected", value: create(McpServerConnectedSchema, {}) },
    });
  }
  return create(McpServerHealthSchema, {
    health: {
      case: "failed",
      value: create(McpServerFailedSchema, { error: error !== "" ? error : status }),
    },
  });
}

/** The plugins loaded, by name and reported version. */
function plugins(raw: unknown[] | undefined) {
  if (raw === undefined) return [];
  return raw.filter(isObject).map((p) => create(SessionPluginSchema, {
    name: strOf(pick(p, "name")),
    version: strOf(pick(p, "version")),
  }));
}

/** The string values of a name->value map, in declaration order. */
function mapValues(o: Record<string, unknown> | undefined): string[] {
  if (o === undefined) return [];
  return Object.values(o).filter((v): v is string => typeof v === "string");
}

// ---------------------------------------------------------------------------
// result → TurnEnded
// ---------------------------------------------------------------------------

/**
 * A turn ended, and how it ended.
 *
 * `TurnEnded.turn_id` names the turn this closes, so a late boundary cannot
 * close whatever happens to be open. The SDK result envelope carries no turn
 * correlation at all, so the id comes from UdsSession's accepted-prompt FIFO.
 *
 * THE OUTCOME IS A CLAIM THE EVIDENCE MUST SUPPORT. `interrupted` is an
 * accusation: only a user-commanded stop the session saw ACKNOWLEDGED sets it,
 * which is what `opts.interrupted` reports. Everything else ran to its own end
 * and says `completed` — including a turn that errored, because the error is a
 * `FailureRaised` MESSAGE the vendor recorded, not a property of the boundary.
 *
 * A result closing NO turn produces no boundary at all rather than one naming
 * the empty string: an unattributable end is evidence of a wiring fault, not a
 * turn ending.
 */
function buildResult(
  message: Record<string, unknown>,
  env: Envelope,
  opts?: ConvertOptions,
): Built {
  const r = env.reader;
  const turnId = opts?.rootTurnId ?? "";
  const interrupted = opts?.interrupted === true && turnId !== "";

  // Read the whole envelope so a genuinely new field still surfaces, then
  // carry what the boundary has no field for. `stop_reason`, `is_error` and
  // every duration below are recognized-and-unmodeled, not unknown.
  r.carry(
    "subtype",
    "is_error", "isError",
    "stop_reason", "stopReason",
    "terminal_reason", "terminalReason",
    "duration_ms", "durationMs",
    "duration_api_ms", "durationApiMs",
    "ttft_ms", "ttftMs",
    "ttft_stream_ms", "ttftStreamMs",
    "time_to_request_ms", "timeToRequestMs",
    "num_turns", "numTurns",
    "result",
    "total_cost_usd", "totalCostUsd",
    "usage",
    "model_usage", "modelUsage",
    "permission_denials", "permissionDenials",
    "structured_output", "structuredOutput",
    "errors",
    "api_error_status", "apiErrorStatus",
    "fast_mode_state", "fastModeState",
    "fast_mode_disabled_reason", "fastModeDisabledReason",
    "user_message_uuid", "userMessageUuid",
    "request_sent_wall_ms", "requestSentWallMs",
    "time_to_request_from_spawn_ms", "timeToRequestFromSpawnMs",
    "time_origin_ms", "timeOriginMs",
    "warm_spare_claimed", "warmSpareClaimed",
    "deferred_tool_use", "deferredToolUse",
    "origin",
  );

  if (turnId === "") {
    LOGGER.log({
      level: "error",
      ...(env.sessionId === "" ? {} : { claude_session_id: env.sessionId }),
      sdk_type: "result",
      failed_operation: "turn_end_conversion",
      outcome: "unattributed_terminal_result",
    }, "terminal result closes no accepted turn; no turn boundary emitted and the record kept whole");
    return { entries: [vendorSpecificEntry("result.unattributed", message as JsonObject)], typeLabel: "result" };
  }

  const ended = create(TurnEndedSchema, {
    turnId,
    outcome: interrupted
      ? { case: "interrupted", value: create(TurnInterruptedSchema, {}) }
      : { case: "completed", value: create(TurnCompletedSchema, {}) },
  });
  return {
    entries: [bookkeepingEntry(env, { case: "turnEnded", value: ended })],
    typeLabel: "result",
  };
}

// ---------------------------------------------------------------------------
// conversation_reset → SessionIdentityChanged
// ---------------------------------------------------------------------------

/**
 * The vendor rotated the session's identity.
 *
 * Kept so a rotated id is reconciled with the conversation it CONTINUES rather
 * than appearing as a new one. The envelope's own `session_id` is the identity
 * being left behind; `new_conversation_id` is what it becomes — which is why
 * this record is filed under the PREVIOUS id, the seq space the daemon is still
 * subscribed to when it arrives.
 */
function buildConversationReset(message: Record<string, unknown>, env: Envelope): Built {
  const r = env.reader;
  const next = r.str("new_conversation_id", "newConversationId");
  if (next === "") {
    throw new MissingFieldError("conversation_reset missing `new_conversation_id`");
  }
  const changed = create(SessionIdentityChangedSchema, {
    previousSessionId: env.sessionId,
    reason: "conversation_reset",
  });
  LOGGER.log({
    claude_session_id: env.sessionId,
    new_claude_session_id: next,
  }, "vendor rotated the session identity");
  return {
    entries: [
      bookkeepingEntry(env, { case: "sessionIdentityChanged", value: changed }),
      // The id it rotated TO has no field on SessionIdentityChanged, which
      // states only what the identity WAS. Kept whole so the lineage is
      // reconstructable from stored data.
      vendorSpecificEntry("conversation_reset", message as JsonObject),
    ],
    typeLabel: "conversation_reset",
  };
}

// ---------------------------------------------------------------------------
// assistant / user — content, which the FILE plane writes
// ---------------------------------------------------------------------------

/**
 * An assistant response, stored whole and never forwarded.
 *
 * The response's CONTENT is the file plane's to write, because a `MessageEntry`
 * must state its parent and the stream carries none. What this shim still takes
 * from the record is the usage the response reported, which feeds the
 * structured accounting log — and which is VALIDATED here, so a malformed
 * usage block becomes an `UnparsedEntry` rather than a silently zeroed cost.
 */
function buildAssistant(message: Record<string, unknown>, r: Reader): Built {
  const rawMessage = r.obj("message");
  if (rawMessage === undefined) {
    throw new MissingFieldError("assistant message missing `message`");
  }
  const assistantApiUsage = normalizeApiUsage(rawMessage["usage"]);
  return buildAssistantUsageOnly(message, r, "assistant", assistantApiUsage);
}

/** Store the record whole, optionally carrying validated usage for the log. */
function buildAssistantUsageOnly(
  message: Record<string, unknown>,
  r: Reader,
  typeLabel: string,
  assistantApiUsage: NormalizedApiUsage | undefined,
): Built {
  const built = vendorOnly(message, r, typeLabel);
  return assistantApiUsage === undefined ? built : { ...built, assistantApiUsage };
}

// ---------------------------------------------------------------------------
// Shape helpers
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
