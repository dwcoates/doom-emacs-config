/**
 * convert/tool-calls.ts — a tool CALL becomes a unit, and its RETURN settles it.
 *
 * # The two halves of one unit
 *
 * A `tool_use` block announces work; a `tool_result` block, arriving later in a
 * user-role record, says how it went. They share the vendor's `tool_use_id`, and
 * that id IS the unit's identity — which is why the join is equality on one
 * value rather than any kind of matching.
 *
 * # The one thing the fold remembers
 *
 * A settled frame is SELF-DESCRIBING: a bash terminal restates the command, a
 * grep terminal restates the query. Those facts live in the call's INPUT, not in
 * the vendor's result, so the call has to be remembered until it settles. That
 * is the {@link CallRegistry}: bounded by the calls actually in flight, dropped
 * the moment one settles, and capped so a vendor that never returns a result
 * cannot grow it without bound.
 *
 * # Three sets of tool names, and why they are different
 *
 *   - MODELLED: a converter owns the name; the unit is typed.
 *   - EXEMPT: the contract deliberately does not carry it. Dropped SILENTLY —
 *     never `AgentUnmodeled`, never a warning, because it is a decision rather
 *     than a gap.
 *   - ENGINE-OWNED: the unit belongs to the engine's permission gate, which
 *     holds the callback. The fold produces nothing, so the two cannot
 *     double-produce one unit.
 *
 * Anything else is `AgentUnmodeled`, and ONLY anything else: a recognizable
 * built-in arriving there is a producer defect, which the suite forbids.
 */
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";
import type { PersistEntry } from "../store/persistence.js";
import { activityEntry, agentActivity, toolProgress, type FrameOrigin } from "./entries.js";
import { mcpServerNames, type FoldContext } from "./fold-context.js";
import { toolCallActivityId } from "./ids.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools" });

// ---------------------------------------------------------------------------
// What a converter is handed
// ---------------------------------------------------------------------------

/** A tool call in flight: everything its terminal frame will have to restate. */
export interface PendingCall {
  /** The vendor's `tool_use_id` — this unit's identity. */
  readonly toolUseId: string;
  /** The tool as the agent named it. */
  readonly toolName: string;
  /** The arguments, as the agent supplied them. */
  readonly input: Record<string, unknown>;
  /** When the call was ANNOUNCED. The shim stamps it once and never restates it. */
  readonly startedAtMs: number;
  /** Whose work it is: a subagent's own id for a subagent's call. */
  readonly agentId: conversationv1.AgentId;
}

/** What the vendor said when a call returned. */
export interface ToolOutcome {
  /** The result blocks, typed. UNSET when the call returned no content at all. */
  readonly content: conversationv1.ToolResultContent | undefined;
  /** Whether the vendor marked the result an error. */
  readonly isError: boolean;
  /**
   * The tool's own full Output object (`tool_use_result`), keyed by tool.
   *
   * THE TYPED SOURCE, and the one converters read: the string content is what
   * the MODEL was shown, and parsing prose out of it is exactly what the typed
   * outputs exist to make unnecessary.
   */
  readonly structured: unknown;
  /** When the shim observed the settle. */
  readonly settledAtMs: number;
}

/**
 * What a converter may need to know about the SESSION rather than the call.
 *
 * Deliberately tiny: the only per-session fact a per-tool mapping has needed so
 * far is the set of MCP server names, and it is needed to RESOLVE a server
 * rather than to parse one out of a qualified tool name.
 */
export interface ToolEnvironment {
  /** Every MCP server name this session knows. */
  readonly mcpServerNames: readonly string[];
}

/**
 * One tool kind's mapping.
 *
 * `settle` may answer `undefined`, which means THIS RESULT IS NOT THE UNIT'S
 * CONCLUSION — the skill unit settles on the document that arrives after the
 * acknowledgement, and a backgrounded shell command settles on its detached-work
 * frame rather than on the launch receipt. Emitting a terminal in either case
 * would say the work ended when it had not.
 */
export interface ToolConverter {
  /** The unit kind's name, used in the write's discriminator and in logs. */
  readonly kind: string;
  /** Whether the vendor emits a per-call heartbeat for this kind. */
  readonly carriesProgress: boolean;
  /**
   * The unit's `start` arm, or `undefined` when THIS CALL HAS NO ANNOUNCEMENT.
   *
   * A `TaskCreate` has no identity until it returns, and a malformed call names
   * nothing the frame could restate. In both cases the entry is SKIPPED rather
   * than written with no arm set: the store refuses an activity that sets no
   * item arm, and a refused batch blocks the queue behind it.
   */
  start(
    call: PendingCall,
    environment?: ToolEnvironment,
  ): conversationv1.AgentActivity["item"] | undefined;
  /** The unit's terminal arm, or `undefined` when this result does not settle it. */
  settle(
    call: PendingCall,
    outcome: ToolOutcome,
    environment?: ToolEnvironment,
  ): conversationv1.AgentActivity["item"] | undefined;
  /** The unit's `progress` arm, for the kinds that declare one. */
  progress?(beat: conversationv1.AgentToolCallProgress): conversationv1.AgentActivity["item"];
}

// ---------------------------------------------------------------------------
// The three name sets
// ---------------------------------------------------------------------------

/**
 * KNOWN VENDOR BUILT-INS THE CONTRACT DELIBERATELY DOES NOT CARRY.
 *
 * Dropped entirely: no unit, no `AgentUnmodeled`, no unmodeled warning. Each is
 * a decision recorded in the contract, not a gap to be filled later.
 */
export const EXEMPT_TOOLS: ReadonlySet<string> = new Set([
  // The task family's read/stop verbs: the work itself is modelled through
  // detached work, and these merely poll or stop it.
  "TaskStop",
  "TaskOutput",
  "TaskGet",
  "TaskList",
  // Deferred-tool discovery: bookkeeping about what the model MAY call.
  "ToolSearch",
  // Not modelled by this contract at all.
  "NotebookEdit",
  "REPL",
  // The MCP-resource family.
  "ListMcpResources",
  "ReadMcpResource",
  "ReadMcpResourceDir",
  "RefreshMcpTools",
  // Vendor-product surfaces with no place in a conversation feed.
  "SendFeedback",
  "ClaudeDesign",
  "Projects",
  "ShowOnboardingRolePicker",
  "ProposeSkills",
  // The background-shell peek: the shell's own unit already carries its output.
  "BashOutput",
  // DEVIATION, recorded in the record-plane report: `TodoWrite` is a
  // recognizable built-in with no arm here — `AgentTaskAct` needs the tracker's
  // identity and TodoWrite's output carries none — so no honest unit can be
  // built from it. Dropped rather than emitted as `AgentUnmodeled`, which would
  // be a producer defect by this contract's own definition.
  "TodoWrite",
  // WORKFLOW IS KICKED this wave: the vocabulary stays in the contract and no
  // producer implements it.
  "Workflow",
  // Declared by the SDK, carried by no conversation.v1 arm.
  "RemoteTrigger",
]);

/**
 * Names whose unit the ENGINE produces.
 *
 * The permission gate holds the `canUseTool` callback for these, so it — and
 * only it — has the ask and the decision.
 */
export const ENGINE_OWNED_TOOLS: ReadonlySet<string> = new Set(["AskUserQuestion"]);

// ---------------------------------------------------------------------------
// The registry of calls in flight
// ---------------------------------------------------------------------------

/**
 * How many unsettled calls are remembered before the oldest is forgotten.
 *
 * A CAP RATHER THAN A LEAK: a vendor that announces a call and never returns a
 * result would otherwise grow this forever, and the shim's statelessness is not
 * a preference. Forgetting is logged, because a settle that arrives afterwards
 * finds no call and produces no terminal.
 */
export const CALL_REGISTRY_CAPACITY = 512;

/** The calls in flight, oldest first. */
export interface CallRegistry {
  /** Remember a call that was just announced. */
  remember(call: PendingCall): void;
  /** Take a call out because it has settled. */
  take(toolUseId: string): PendingCall | undefined;
  /** Look at a call without settling it (a progress beat). */
  peek(toolUseId: string): PendingCall | undefined;
}

export function createCallRegistry(): CallRegistry {
  const calls = new Map<string, PendingCall>();
  return {
    remember(call) {
      if (calls.size >= CALL_REGISTRY_CAPACITY) {
        const oldest = calls.keys().next();
        if (oldest.done !== true) {
          LOGGER.log(
            { level: "warn", tool_use_id: oldest.value, capacity: CALL_REGISTRY_CAPACITY },
            "forgetting the oldest unsettled tool call: the in-flight registry is full",
          );
          calls.delete(oldest.value);
        }
      }
      calls.set(call.toolUseId, call);
    },
    take(toolUseId) {
      const call = calls.get(toolUseId);
      if (call !== undefined) calls.delete(toolUseId);
      return call;
    },
    peek: (toolUseId) => calls.get(toolUseId),
  };
}

// ---------------------------------------------------------------------------
// Disposition
// ---------------------------------------------------------------------------

/** Which of the four dispositions a tool name has. */
type ToolDisposition =
  | { readonly case: "modelled"; readonly converter: ToolConverter }
  | { readonly case: "exempt" }
  | { readonly case: "engine_owned" }
  | { readonly case: "unmodeled" };

/**
 * Classify one tool name.
 *
 * `converters` is passed rather than imported so this file holds no per-kind
 * knowledge and the registry can be substituted in a test.
 */
export function dispositionOf(
  converters: ReadonlyMap<string, ToolConverter>,
  toolName: string,
): ToolDisposition {
  const converter = converters.get(toolName);
  if (converter !== undefined) return { case: "modelled", converter };
  if (EXEMPT_TOOLS.has(toolName)) return { case: "exempt" };
  if (ENGINE_OWNED_TOOLS.has(toolName)) return { case: "engine_owned" };
  return { case: "unmodeled" };
}

// ---------------------------------------------------------------------------
// The call side
// ---------------------------------------------------------------------------

/** The session facts a converter may read, from the context the engine handed in. */
export function environmentOf(context: FoldContext): ToolEnvironment {
  return { mcpServerNames: mcpServerNames(context) };
}

/** The activity envelope every tool frame shares: the unit's identity. */
function toolActivity(
  call: PendingCall,
  item: conversationv1.AgentActivity["item"],
  envelope: Parameters<typeof agentActivity>[2] = {},
): conversationv1.AgentActivity {
  return agentActivity(toolCallActivityId(call.toolUseId), item, envelope);
}

/**
 * A tool call's `start` row.
 *
 * The call is REMEMBERED whatever its disposition, so an exempt tool's result
 * can be recognized and dropped rather than landing as residue.
 */
export function convertToolUse(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  call: PendingCall,
  origin: Omit<FrameOrigin, "discriminator">,
  envelope: Parameters<typeof agentActivity>[2] = {},
): readonly PersistEntry[] {
  registry.remember(call);
  const disposition = dispositionOf(converters, call.toolName);
  switch (disposition.case) {
    case "exempt":
      LOGGER.logVerbose(
        { tool: call.toolName, tool_use_id: call.toolUseId },
        "exempt tool: dropped entirely, by decision rather than by gap",
      );
      return [];
    case "engine_owned":
      LOGGER.logVerbose(
        { tool: call.toolName, tool_use_id: call.toolUseId },
        "the engine's permission gate owns this unit; the fold produces nothing",
      );
      return [];
    case "modelled": {
      LOGGER.logVerbose(
        { tool: call.toolName, kind: disposition.converter.kind, tool_use_id: call.toolUseId },
        "converting a tool call's start",
      );
      const item = disposition.converter.start(call, environmentOf(context));
      if (item === undefined) {
        LOGGER.logVerbose(
          { tool: call.toolName, kind: disposition.converter.kind, tool_use_id: call.toolUseId },
          "this call has no announcement frame; the start entry is skipped",
        );
        return [];
      }
      return [
        activityEntry(
          context,
          { ...origin, discriminator: `activity.${disposition.converter.kind}.start` },
          toolActivity(call, item, envelope),
        ),
      ];
    }
    default: {
      LOGGER.log(
        { level: "warn", tool: call.toolName, tool_use_id: call.toolUseId },
        "no converter owns this tool name; it becomes an unmodeled unit",
      );
      const unmodeled = converters.get(UNMODELED_KEY);
      if (unmodeled === undefined) {
        throw new Error("shim convert: the unmodeled converter is missing from the registry");
      }
      const item = unmodeled.start(call, environmentOf(context));
      if (item === undefined) {
        throw new Error("shim convert: the unmodeled converter produced no start frame");
      }
      return [
        activityEntry(
          context,
          { ...origin, discriminator: "activity.unmodeled.start" },
          toolActivity(call, item, envelope),
        ),
      ];
    }
  }
}

/**
 * The registry key the unmodeled converter is filed under.
 *
 * A key no vendor tool can ever be named, so a genuine tool called "unmodeled"
 * would not silently take the fallback's place.
 */
export const UNMODELED_KEY = " unmodeled";

// ---------------------------------------------------------------------------
// The result side
// ---------------------------------------------------------------------------

/**
 * A tool result's terminal row.
 *
 * Answers NOTHING when the call is unknown (a result for a call this shim never
 * saw announced — a resumed session's tail), when the tool is exempt, or when
 * the converter says this result does not settle the unit.
 */
export function convertToolResult(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  toolUseId: string,
  outcome: ToolOutcome,
  origin: Omit<FrameOrigin, "discriminator" | "agentId">,
): readonly PersistEntry[] {
  const call = registry.take(toolUseId);
  if (call === undefined) {
    LOGGER.log(
      { level: "warn", tool_use_id: toolUseId },
      "a tool result arrived for a call this shim never saw announced; no terminal is produced",
    );
    return [];
  }
  const disposition = dispositionOf(converters, call.toolName);
  if (disposition.case !== "modelled" && disposition.case !== "unmodeled") {
    LOGGER.logVerbose(
      { tool: call.toolName, tool_use_id: toolUseId, disposition: disposition.case },
      "the result of a tool the fold does not own is consumed and dropped",
    );
    return [];
  }
  const converter =
    disposition.case === "modelled" ? disposition.converter : converters.get(UNMODELED_KEY);
  if (converter === undefined) {
    throw new Error("shim convert: the unmodeled converter is missing from the registry");
  }
  const item = converter.settle(call, outcome, environmentOf(context));
  if (item === undefined) {
    LOGGER.logVerbose(
      { tool: call.toolName, kind: converter.kind, tool_use_id: toolUseId },
      "this result does not conclude the unit; the unit stays open",
    );
    // The unit is NOT settled, so the call must stay remembered: a skill's
    // document and a backgrounded shell's detachment both still need it.
    registry.remember(call);
    return [];
  }
  LOGGER.logVerbose(
    { tool: call.toolName, kind: converter.kind, tool_use_id: toolUseId, error: outcome.isError },
    "converting a tool result's terminal",
  );
  return [
    activityEntry(
      context,
      {
        ...origin,
        agentId: call.agentId,
        discriminator: `activity.${converter.kind}.${outcome.isError ? "failure" : "success"}`,
      },
      toolActivity(call, item),
    ),
  ];
}

// ---------------------------------------------------------------------------
// The progress beat
// ---------------------------------------------------------------------------

/**
 * The vendor's per-call heartbeat, relayed on the kinds that declare the arm.
 *
 * `elapsed_time_seconds` is CONSUMED and never forwarded: the start instant plus
 * this beat replace it, and a duration on the wire would be a second authority
 * for a value derivable from one instant.
 */
export function convertProgressBeat(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  toolUseId: string,
  atMs: number,
  origin: Omit<FrameOrigin, "discriminator" | "agentId">,
): readonly PersistEntry[] {
  const call = registry.peek(toolUseId);
  if (call === undefined) {
    LOGGER.logVerbose(
      { tool_use_id: toolUseId },
      "a progress beat arrived for a call this shim never saw announced; nothing to upsert",
    );
    return [];
  }
  const disposition = dispositionOf(converters, call.toolName);
  if (disposition.case !== "modelled") return [];
  const converter = disposition.converter;
  if (!converter.carriesProgress || converter.progress === undefined) {
    LOGGER.logVerbose(
      { tool: call.toolName, kind: converter.kind },
      "this unit kind declares no progress arm; the beat is consumed",
    );
    return [];
  }
  return [
    activityEntry(
      context,
      { ...origin, agentId: call.agentId, discriminator: `activity.${converter.kind}.progress` },
      toolActivity(call, converter.progress(toolProgress(atMs))),
    ),
  ];
}
