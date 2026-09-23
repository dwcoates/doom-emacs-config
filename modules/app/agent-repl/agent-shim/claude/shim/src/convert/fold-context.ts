/**
 * convert/fold-context.ts — the record plane's view of the ONE fold context.
 *
 * # Why this file re-exports rather than declares
 *
 * There is exactly one FoldContext, and it is `engine/fold-context.ts`'s: the
 * engine is what HOLDS the session state, so the declaration belongs beside the
 * thing that fills it. Two declarations of the same shape would be two places
 * for a field to be added and one place for it to be forgotten.
 *
 * What this file adds is the record plane's own accessors over that context —
 * the two lookups the fold needs are OPTIONAL on the engine's declaration
 * (added additively, 2026-08-29), so the fold reads them through helpers that
 * state what an absent one means rather than through `?.` at every call site.
 */
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";
import type { FoldContext, LastChange } from "../engine/fold-context.js";
import { subagentId } from "./ids.js";

const LOGGER = bindLog({ component: "shim-convert-context", operation: "shim.convert.context" });

export type { FoldContext, LastChange };

/** What kind of ask the engine's permission gate is holding open for a call. */
export interface PendingAsk {
  readonly kind: "permission" | "question";
}

/** A live vendor task, and what it is: the `task_started` join. */
export interface LiveTask {
  /** The call that spawned the task — the unit the work detached FROM. */
  readonly toolUseId: string;
  /** The agent the task is running, when the task is an agent rather than a shell. */
  readonly agentId?: conversationv1.AgentId;
}

/**
 * THE ONE PLACE a subagent's identity is minted from its spawning call.
 *
 * The pinned SDK stream carries no agent id anywhere: a subagent's own
 * assistant messages name `parent_tool_use_id` — the CALL — and nothing else.
 * So until a ruling gives the stream plane a real producer, a subagent's book
 * IS the spawning call's `tool_use_id`, and the engine's own mapping wins
 * whenever it has one (it learns real ids from the spawn's result and from
 * `task_started`).
 *
 * ONE FUNCTION ON PURPOSE: a ruling changes this line and nothing else.
 */
export function subagentBook(
  context: FoldContext,
  spawningToolUseId: string,
): conversationv1.AgentId {
  const known = context.subagentFor?.(spawningToolUseId);
  if (known !== undefined) return known;
  LOGGER.logVerbose(
    { parent_tool_use_id: spawningToolUseId },
    "no agent id is known for this spawning call; the call's own id is the subagent's book",
  );
  return subagentId(spawningToolUseId);
}

/**
 * THE ONE RULE for which stream a vendor message rides: the spawning call a
 * subagent's message names, or `undefined` for the MAIN agent's own stream.
 *
 * The vendor spells "no parent" as `null`, and a loosely-read record may spell
 * it as an absent or empty field; all three are the main stream. The book
 * ({@link bookFor}), the stream's block state and every "top-level only" join
 * the fold keeps resolve through this, so no two of them can disagree about
 * which agent a message belongs to.
 */
export function spawningCallOf(parentToolUseId: unknown): string | undefined {
  return typeof parentToolUseId === "string" && parentToolUseId !== "" ? parentToolUseId : undefined;
}

/**
 * The book a vendor message's frames belong to.
 *
 * `parent_tool_use_id` names the CALL that spawned the agent, never the agent,
 * and the pinned SDK stream states no agent id anywhere — so a subagent's book
 * is {@link subagentBook}, the ONE function that mints it.
 */
export function bookFor(context: FoldContext, parentToolUseId: unknown): conversationv1.AgentId {
  const spawningCall = spawningCallOf(parentToolUseId);
  return spawningCall === undefined ? context.mainAgentId : subagentBook(context, spawningCall);
}

/** The MCP server names the session knows, or none when the engine states none. */
export function mcpServerNames(context: FoldContext): readonly string[] {
  return context.mcpServerNames?.() ?? [];
}
