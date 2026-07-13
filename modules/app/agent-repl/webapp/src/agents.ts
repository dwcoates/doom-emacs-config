/**
 * Subagent roster — the Task/Agent tool calls the session's context still
 * carries, surfaced as a topbar datapoint beside parent workspace / model
 * / tokens.
 *
 * The roster is DERIVED, not tracked: every subagent the session spawned
 * is already in the feed as a tool item, so the chip's count and the
 * overlay's rows are a projection of `StoreState.items` rather than a
 * second copy of the same state that could drift from it. A `/clear` thus
 * needs no reset of its own — the roster reads the context
 * (`itemsSinceClear`), and the clear is what moves where that context
 * begins.
 */
import { escapeHtml } from "./highlight.js";
import { ConversationItem, ToolItem } from "./store.js";
import { itemsSinceClear } from "./turn.js";

/**
 * Tool names that spawn a subagent. The CLI renamed the tool from `Task`
 * to `Agent`; a session replayed from an older transcript still carries
 * the old name, so both count.
 */
export const SUBAGENT_TOOLS: ReadonlySet<string> = new Set(["Task", "Agent"]);

/** Where a subagent is in its life: the roster's status column. */
export type SubagentStatus = "starting" | "running" | "done" | "error";

/** One spawned subagent, as the topbar overlay lists it. */
export interface SubagentEntry {
  toolUseId: string;
  /** The call's `description` input, empty while the input still streams. */
  description: string;
  /** The call's `subagent_type` input, empty when it named no type. */
  agentType: string;
  status: SubagentStatus;
  /** Whether another subagent spawned this one (a nested Task/Agent call). */
  nested: boolean;
}

function subagentStatus(item: ToolItem): SubagentStatus {
  if (!item.inputDone) return "starting";
  if (!item.result) return "running";
  return item.result.isError ? "error" : "done";
}

function stringField(item: ToolItem, key: string): string {
  const value = item.input?.[key];
  return typeof value === "string" ? value : "";
}

/**
 * Every subagent the session's current context carries, in spawn order.
 *
 * A subagent spawned before the last `/clear` is gone from that context
 * even though its tool card is still on screen above the divider, so the
 * roster starts where the context starts.
 */
export function sessionSubagents(items: readonly ConversationItem[]): SubagentEntry[] {
  const entries: SubagentEntry[] = [];
  for (const item of itemsSinceClear(items)) {
    if (item.kind !== "tool" || !SUBAGENT_TOOLS.has(item.toolName)) continue;
    entries.push({
      toolUseId: item.toolUseId,
      description: stringField(item, "description"),
      agentType: stringField(item, "subagent_type"),
      status: subagentStatus(item),
      nested: item.parentToolUseId !== undefined,
    });
  }
  return entries;
}

/** How many of the roster's subagents are still working. */
export function runningCount(agents: readonly SubagentEntry[]): number {
  return agents.filter((a) => a.status === "starting" || a.status === "running").length;
}

/** `1 agent` / `3 agents` — the chip's own label, not a `label: value` datapoint. */
export function agentsLabel(count: number): string {
  return `${count} agent${count === 1 ? "" : "s"}`;
}

/**
 * The topbar's subagent control: the count, a caret, and the roster
 * overlay it drops. Renders to nothing when the session has spawned no
 * subagents — a `0 agents` chip is a control over an empty list.
 *
 * OPEN is renderer-owned state (like the feed's question selections), so
 * the overlay survives the per-frame re-render of the whole topbar.
 */
export function agentsMenuHtml(agents: readonly SubagentEntry[], open: boolean): string {
  if (agents.length === 0) return "";
  const running = runningCount(agents);
  const busy = running > 0 ? ` <span class="agents-running">${running} running</span>` : "";
  return `<span class="agents-menu">
      <button type="button" class="info-agents" data-agents-toggle aria-expanded="${open}" aria-haspopup="true" title="session subagents">${escapeHtml(
        agentsLabel(agents.length),
      )}${busy} <span class="agents-caret" aria-hidden="true">${open ? "▴" : "▾"}</span></button>
      ${open ? agentsOverlayHtml(agents) : ""}
    </span>`;
}

/** The dropped roster: one row per subagent, newest last (spawn order). */
export function agentsOverlayHtml(agents: readonly SubagentEntry[]): string {
  const rows = agents
    .map((a) => {
      const type = a.agentType
        ? `<span class="agent-type">${escapeHtml(a.agentType)}</span>`
        : "";
      const label = a.description === "" ? "starting…" : a.description;
      return `<li class="agent-row${a.nested ? " nested" : ""}">
        <span class="agent-dot agent-${a.status}" aria-hidden="true">●</span>
        <span class="agent-desc">${escapeHtml(label)}</span>
        ${type}
        <span class="agent-status">${escapeHtml(a.status)}</span>
      </li>`;
    })
    .join("");
  return `<ul class="agents-overlay" role="menu">${rows}</ul>`;
}
