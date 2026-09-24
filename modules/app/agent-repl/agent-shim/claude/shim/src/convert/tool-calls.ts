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
  /**
   * The spawning call of the STREAM the call rode — its message's
   * `parent_tool_use_id`, read through `spawningCallOf` — or UNSET for the
   * main agent's own stream. What the registry holds a call under, so an
   * agent's end and a detached handoff can release exactly its calls.
   */
  readonly spawningCall?: string | undefined;
  /**
   * The tool allowances a DECLINED result stated, kept for the frame that does
   * settle the unit.
   *
   * A skill's acknowledgement is the only record that carries the skill's
   * declared allowances, and it settles nothing; the document that settles the
   * unit carries none. UNSET means the declining record stated no allowances at
   * all, which the proto distinguishes from an empty declared set.
   */
  readonly retainedAllowedTools?: readonly string[] | undefined;
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
 * `settle` may answer `undefined`, which means THIS RESULT WRITES NO TERMINAL
 * — the skill unit settles on the document that arrives after the
 * acknowledgement, a backgrounded shell command settles on its detached-work
 * frame rather than on the launch receipt, and a result too thin to restate
 * the call writes nothing rather than a partial frame. Emitting a terminal in
 * the first two cases would say the work ended when it had not.
 *
 * Whether the CALL stays held is a separate answer, {@link retain}'s: the
 * result is the call's return, so without a `retain` the call is released.
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
  /**
   * The unit's terminal arm for a call THE TURN'S STOP CUT SHORT, or `undefined`
   * when this kind can state no such ending.
   *
   * A stopped turn returns no `tool_result` for the call it landed inside — the
   * vendor's own record of it is the bare `[Request interrupted by user]` line
   * — so nothing else will ever settle the unit, and a card left on its running
   * arm goes on telling the reader that work is in flight inside a turn that
   * ended. This is the producer's side of that: the shim is the one thing that
   * knows both that the turn was stopped and which calls were open when it was.
   *
   * NOTHING IS INVENTED HERE. The cut states only what happened — the user
   * stopped it — never an exit status, an output or a duration the call never
   * reported. A kind whose proto has no vocabulary for being cut short omits
   * this, and its unit stays open exactly as it did before.
   */
  cut?(call: PendingCall, atMs: number): conversationv1.AgentActivity["item"] | undefined;
  /**
   * The call to KEEP HELD when {@link settle} declined, for the one shape that
   * needs it: a unit whose conclusion is a LATER RECORD ON THIS STREAM (the
   * skill's document, joined back to the call).
   *
   * The declining result is the last time its own fields are seen, so the
   * answer may carry something only that result stated. A kind that omits
   * this has its call RELEASED at every declined settle: its work was handed
   * off (a detached shell, an async spawn, an armed monitor — each concluded by
   * the task stream or the file plane, never by a record naming this call), or
   * the result simply could not be restated. Holding those is what leaked.
   */
  retain?(call: PendingCall, outcome: ToolOutcome): PendingCall;
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
 * How many unsettled calls may be held at once.
 *
 * AN INVARIANT, NOT A WORKING LIMIT. The registry holds only what is
 * genuinely open on this plane — every call leaves it at its result, at a
 * detached handoff, with its agent's end, at the turn's end or at the query's
 * end — so it drains to empty at every turn terminal, and reaching the bound
 * means a path registered calls and never let them go. That is logged at
 * error, and the oldest call is still forgotten, because the shim's
 * statelessness is not a preference: an unbounded table would be a second
 * record growing beside the store's.
 */
export const CALL_REGISTRY_CAPACITY = 512;

/**
 * The calls in flight, oldest first.
 *
 * # Whose calls it holds: the STREAMS this plane can settle
 *
 * A call rides the stream its message named (`parent_tool_use_id`, resolved
 * by `spawningCallOf` — the same rule as the book and the block state),
 * and it is held only while that stream is: the main agent's always, a
 * subagent's while its spawning call is itself held and has not been handed
 * off. A BACKGROUNDED agent's calls are never held: its `tool_use` blocks are
 * forwarded onto this stream but its `tool_result` records are not (the
 * `subagent-detached` capture carries the one and never the other), so its
 * units are the file plane's to settle, from the sidechain transcript that
 * does carry them. Holding them anyway is what filled the registry.
 *
 * # When a call leaves
 *
 *   - its RESULT settles it ({@link take}) — unless the converter says the
 *     unit awaits a later record on this stream, and re-remembers it;
 *   - its work is HANDED OFF ({@link detach}): the work left the turn, and
 *     its conclusion is the task stream's or the file plane's, not this call's;
 *   - its AGENT ENDS: a spawning call's own settle releases every call its
 *     stream still held, since no record for them can follow;
 *   - the TURN ENDS ({@link drain}): whatever the turn left open is released,
 *     and a stop cuts it first;
 *   - the QUERY ENDS ({@link drain} again): nothing it announced can settle.
 */
export interface CallRegistry {
  /**
   * Hold a call that was just announced, answering whether it is held.
   *
   * A call on a stream this plane does not hold is REFUSED rather than held:
   * nothing on this stream will ever settle it.
   */
  remember(call: PendingCall): boolean;
  /**
   * Take a call out because it has settled, releasing every call its own
   * stream still held (its agent is over).
   */
  take(toolUseId: string): PendingCall | undefined;
  /** Look at a call without settling it (a progress beat, an owner join). */
  peek(toolUseId: string): PendingCall | undefined;
  /** Whether the stream `spawningCall` names is one this plane holds calls for. */
  holds(spawningCall: string | undefined): boolean;
  /**
   * The call's work LEFT THE TURN while the call stays unsettled (a spawn or a
   * shell the vendor backgrounded): its stream's calls are released and no
   * further call on it is held, and the stop that ends the turn does not cut
   * it. The call itself stays held until its own result arrives.
   */
  detach(toolUseId: string): void;
  /** Whether the call's work was handed off ({@link detach}). */
  isDetached(toolUseId: string): boolean;
  /** Every call still held, oldest first, as a snapshot; nothing is settled. */
  open(): readonly PendingCall[];
  /**
   * Release EVERY call, oldest first, and answer them — the turn's end and the
   * query's end, the two moments nothing still held can settle on this stream.
   * Each answered call carries whether its work was handed off.
   */
  drain(): readonly { readonly call: PendingCall; readonly detached: boolean }[];
}

export function createCallRegistry(): CallRegistry {
  const calls = new Map<string, PendingCall>();
  const detached = new Set<string>();

  const holds = (spawningCall: string | undefined): boolean =>
    spawningCall === undefined || (calls.has(spawningCall) && !detached.has(spawningCall));

  /** Drop every call riding the stream `spawningCall` names, and theirs in turn. */
  const releaseStream = (spawningCall: string, why: string): void => {
    const riding = [...calls.values()].filter((call) => call.spawningCall === spawningCall);
    if (riding.length === 0) return;
    for (const call of riding) {
      calls.delete(call.toolUseId);
      detached.delete(call.toolUseId);
      releaseStream(call.toolUseId, why);
    }
    LOGGER.info(
      {
        spawning_call: spawningCall,
        released: riding.length,
        tool_use_ids: riding.map((call) => call.toolUseId),
        why,
      },
      "an agent's stream is no longer this plane's; the calls it still held are released and the file plane settles their units",
    );
  };

  return {
    remember(call) {
      if (!holds(call.spawningCall)) return false;
      if (calls.size >= CALL_REGISTRY_CAPACITY && !calls.has(call.toolUseId)) {
        const [oldest] = calls.values();
        if (oldest !== undefined) {
          LOGGER.error(
            {
              tool_use_id: oldest.toolUseId,
              tool: oldest.toolName,
              agent: oldest.agentId.value,
              spawning_call: oldest.spawningCall ?? "main",
              held: calls.size,
              capacity: CALL_REGISTRY_CAPACITY,
              detail: "the in-flight registry reached its bound; a path registered calls and never released them",
            },
            "invariant violated: the in-flight call registry is full; the oldest call is forgotten and its unit can no longer settle on this plane",
          );
          calls.delete(oldest.toolUseId);
          detached.delete(oldest.toolUseId);
        }
      }
      calls.set(call.toolUseId, call);
      return true;
    },
    take(toolUseId) {
      const call = calls.get(toolUseId);
      if (call === undefined) return undefined;
      calls.delete(toolUseId);
      detached.delete(toolUseId);
      releaseStream(toolUseId, "its spawning call settled");
      return call;
    },
    peek: (toolUseId) => calls.get(toolUseId),
    holds,
    detach(toolUseId) {
      if (!calls.has(toolUseId) || detached.has(toolUseId)) return;
      detached.add(toolUseId);
      releaseStream(toolUseId, "its work was handed off");
    },
    isDetached: (toolUseId) => detached.has(toolUseId),
    open: () => [...calls.values()],
    drain() {
      const drained = [...calls.values()].map((call) => ({ call, detached: detached.has(call.toolUseId) }));
      calls.clear();
      detached.clear();
      return drained;
    },
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
  if (isMcpToolName(toolName)) {
    const mcp = converters.get(MCP_KEY);
    if (mcp === undefined) throw new Error("shim convert: the MCP tool converter is missing from the registry");
    return { case: "modelled", converter: mcp };
  }
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
 * can be recognized and dropped rather than landing as residue — on every
 * stream this plane holds. A call a BACKGROUNDED agent made is announced and
 * NOT remembered: its result never reaches this stream, so the file plane,
 * which reads the agent's own transcript, is what settles its unit.
 */
export function convertToolUse(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  call: PendingCall,
  origin: Omit<FrameOrigin, "discriminator">,
  envelope: Parameters<typeof agentActivity>[2] = {},
): readonly PersistEntry[] {
  if (!registry.remember(call)) {
    LOGGER.logVerbose(
      { tool: call.toolName, tool_use_id: call.toolUseId, spawning_call: call.spawningCall },
      "a call on a stream this plane does not hold (a backgrounded agent's): announced, never held; the file plane settles it",
    );
  }
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
      LOGGER.debug(
        { tool: call.toolName, tool_use_id: call.toolUseId },
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

/** The vendor's MCP tool-name prefix: `mcp__<server>__<tool>`. */
export const MCP_PREFIX = "mcp__";

/** Whether a vendor tool name is an MCP server's tool. */
export function isMcpToolName(toolName: string): boolean {
  return toolName.startsWith(MCP_PREFIX);
}

/**
 * The registry key the MCP tool converter is filed under. Every MCP tool name
 * is matched by its `mcp__` prefix rather than looked up, so the converter sits
 * under a key no vendor tool can ever be named.
 */
export const MCP_KEY = " mcp";

// ---------------------------------------------------------------------------
// The result side
// ---------------------------------------------------------------------------

/**
 * A tool result's terminal row.
 *
 * Answers NOTHING when the call is unknown (a result for a call this shim never
 * saw announced — a resumed session's tail — or one a backgrounded agent made,
 * which this plane never holds), when the tool is exempt, or when the converter
 * produces no terminal from this result.
 *
 * THE RESULT ALWAYS TAKES THE CALL OUT. A result that produced no terminal is
 * still the call's return, and nothing later on this stream can settle it — so
 * the call is re-remembered only when its converter says the unit awaits a
 * later record on this stream ({@link ToolConverter.retain}). Re-remembering
 * every declined settle is what let the registry fill: a subagent's results
 * carry no typed output on this stream, a backgrounded shell's receipt and an
 * async spawn's receipt hand the work off, and a monitor's arming receipt is
 * not its end — none of which any later record here settles.
 *
 * `spawningCall` is the stream the RESULT rode. It is the same identity the
 * call was registered under, and a result on another stream than its call's is
 * an invariant violation, raised at error and still settled by the vendor's id.
 */
export function convertToolResult(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  toolUseId: string,
  outcome: ToolOutcome,
  origin: Omit<FrameOrigin, "discriminator" | "agentId">,
  spawningCall?: string,
): readonly PersistEntry[] {
  const call = registry.take(toolUseId);
  if (call === undefined) {
    if (!registry.holds(spawningCall)) {
      LOGGER.debug(
        { tool_use_id: toolUseId, spawning_call: spawningCall },
        "a tool result on a stream this plane does not hold (a backgrounded agent's); the file plane settles its unit",
      );
      return [];
    }
    // warn: a defect because an unannounced tool result cannot settle a unit.
    LOGGER.warn(
      { tool_use_id: toolUseId },
      "a tool result arrived for a call this shim never saw announced; no terminal is produced",
    );
    return [];
  }
  if (call.spawningCall !== spawningCall) {
    LOGGER.error(
      {
        tool_use_id: toolUseId,
        tool: call.toolName,
        registered_on: call.spawningCall ?? "main",
        settled_on: spawningCall ?? "main",
        detail: "a call's result rode a different stream from the call's announcement",
      },
      "invariant violated: a tool result's stream is not its call's; settling by the vendor's call id",
    );
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
    const retained = converter.retain?.(call, outcome);
    if (retained !== undefined) {
      LOGGER.logVerbose(
        { tool: call.toolName, kind: converter.kind, tool_use_id: toolUseId },
        "this result does not conclude the unit; it awaits a later record on this stream and stays held",
      );
      registry.remember(retained);
      return [];
    }
    LOGGER.logVerbose(
      { tool: call.toolName, kind: converter.kind, tool_use_id: toolUseId },
      "this result produced no terminal and nothing later on this stream settles the unit; the call is released",
    );
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

// ---------------------------------------------------------------------------
// The turn's end
// ---------------------------------------------------------------------------

/**
 * Release every call the turn left held, and cut the ones a STOP left open.
 *
 * NO CALL OUTLIVES ITS TURN ON THIS PLANE. By the terminal, every call this
 * stream could settle has settled, been handed off or been retained for a
 * record that did not come; a backgrounded agent's calls were never held. So
 * the registry is DRAINED here, at every terminal, and is empty afterwards.
 *
 * ON A STOP (`stopped`), a call still genuinely open is one the stop landed
 * inside, and the vendor returns no `tool_result` for it — so its terminal is
 * owed here or nowhere, and a unit left on its running arm draws a live tool
 * inside a turn that has ended. Each kind that states a {@link ToolConverter.cut}
 * gets that frame, under its OWN block ordinal: the deterministic write id is
 * minted from the source coordinates, so several frames derived from one vendor
 * record must differ somewhere or the store absorbs all but the first as
 * duplicates of it. A call whose work was HANDED OFF is not open — it is still
 * running, elsewhere — and is never cut; nor is a kind with no vocabulary for
 * being cut short, whose unit keeps the shape it had.
 *
 * ON ANY OTHER TERMINAL nothing is cut: the turn ended on its own, and a call
 * still held is one whose settle this stream will not carry (a skill whose
 * document rode only the transcript), so the file plane's row is the unit's.
 */
export function endTurnCalls(
  converters: ReadonlyMap<string, ToolConverter>,
  context: FoldContext,
  registry: CallRegistry,
  origin: Omit<FrameOrigin, "discriminator" | "agentId" | "blockIndex">,
  stopped: boolean,
): readonly PersistEntry[] {
  const entries: PersistEntry[] = [];
  const released: string[] = [];
  for (const { call, detached } of registry.drain()) {
    const disposition = dispositionOf(converters, call.toolName);
    const converter = disposition.case === "modelled" ? disposition.converter : undefined;
    const item = stopped && !detached ? converter?.cut?.(call, context.nowMs()) : undefined;
    if (converter === undefined || item === undefined) {
      released.push(call.toolUseId);
      continue;
    }
    LOGGER.debug(
      { tool: call.toolName, kind: converter.kind, tool_use_id: call.toolUseId },
      "the turn was stopped with this call still open; its unit is settled as cut short",
    );
    entries.push(
      activityEntry(
        context,
        {
          ...origin,
          agentId: call.agentId,
          blockIndex: entries.length,
          discriminator: `activity.${converter.kind}.interrupted`,
        },
        toolActivity(call, item),
      ),
    );
  }
  if (released.length > 0) {
    LOGGER.info(
      { stopped, cut: entries.length, released: released.length, tool_use_ids: released },
      "the turn ended with calls held that no record on this stream will settle; they are released",
    );
  }
  return entries;
}

/**
 * Release every call because the QUERY that announced them is over.
 *
 * A query that died, or one the engine replaced, answers nothing more, so no
 * call it announced can settle on this stream; no frame is minted, because a
 * query's end is not a stop anyone made and a cut would say one was.
 */
export function endQueryCalls(registry: CallRegistry, why: string): void {
  const drained = registry.drain();
  if (drained.length === 0) return;
  LOGGER.info(
    { why, released: drained.length, tool_use_ids: drained.map(({ call }) => call.toolUseId) },
    "the query ended with calls held; they are released and nothing is written for them",
  );
}
