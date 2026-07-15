/**
 * Subagent roster — the session's Task/Agent tool calls, surfaced as a
 * topbar counter beside the task roster (see `tasks.ts`). Both are thin
 * specializations of the shared `counter-menu.ts` facade: this module only
 * projects `StoreState.items` into `CounterEntry[]` and names the counter's
 * DOM classes; every visual and every retention rule lives in the facade.
 *
 * The roster is DERIVED, not tracked: every subagent the session spawned is
 * already in the feed as a tool item, so the chip's count and the overlay's
 * rows are a projection of `StoreState.items` rather than a second copy of
 * the same state that could drift from it. A `/clear` needs no reset of its
 * own — the roster reads the CONTEXT (`itemsSinceClear`), and the clear is
 * what moves where that context begins.
 */
import {
  CounterEntry,
  CounterSpec,
  CounterStatus,
  counterMenuHtml,
} from "./counter-menu.js";
import { ConversationItem, ToolItem } from "./store.js";
import { itemsSinceClear } from "./turn.js";

/**
 * Tool names that spawn a subagent. The CLI renamed the tool from `Task`
 * to `Agent`; a session replayed from an older transcript still carries
 * the old name, so both count.
 */
export const SUBAGENT_TOOLS: ReadonlySet<string> = new Set(["Task", "Agent"]);

/** The subagent counter's DOM/vocabulary knobs. */
export const AGENTS_SPEC: CounterSpec = {
  menu: "agents",
  item: "agent",
  noun: "agent",
  busyNoun: "running",
  title: "session subagents",
  placeholder: "starting…",
};

function subagentStatus(item: ToolItem): CounterStatus {
  if (!item.inputDone) return "starting";
  if (!item.result) return "running";
  return item.result.isError ? "error" : "done";
}

function stringField(item: ToolItem, key: string): string {
  const value = item.input?.[key];
  return typeof value === "string" ? value : "";
}

/**
 * Every subagent the session's current context carries, in spawn order, as
 * counter entries.
 *
 * A subagent spawned before the last `/clear` is gone from that context
 * (and, since the clear cuts the feed too, off screen with it), so the
 * roster starts where the context starts (`itemsSinceClear`).
 *
 * `deactivatedAtTurn` maps straight from the item's stamp: an active
 * subagent (no result) is unstamped, which reads as null (never
 * deactivated); a settled one carries the counted turn its result landed
 * on, which the recency window ages against.
 */
export function sessionSubagents(items: readonly ConversationItem[]): CounterEntry[] {
  const entries: CounterEntry[] = [];
  for (const item of itemsSinceClear(items)) {
    if (item.kind !== "tool" || !SUBAGENT_TOOLS.has(item.toolName)) continue;
    entries.push({
      id: item.toolUseId,
      summary: stringField(item, "description"),
      detail: stringField(item, "subagent_type"),
      status: subagentStatus(item),
      nested: item.parentToolUseId !== undefined,
      deactivatedAtTurn: item.deactivatedAtTurn ?? null,
    });
  }
  return entries;
}

/** The subagent counter's chip and (when open) its overlay. */
export function agentsMenuHtml(
  agents: readonly CounterEntry[],
  open: boolean,
  currentTurn: number,
): string {
  return counterMenuHtml(AGENTS_SPEC, agents, open, currentTurn);
}
