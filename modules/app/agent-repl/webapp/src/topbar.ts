/**
 * topbar — the datapoint strip the GUI's header renders, extracted so ONE
 * renderer serves two scopes: the session (the `#session-info` strip in
 * the header) and a single agent (the live strip a subagent's bubble
 * carries, scoped to that agent).
 *
 * The contract of this module is that the two surfaces can never drift: a
 * datapoint added to `TopbarDatapoints` + `topbarInfoHtml` and projected
 * in the two `*TopbarDatapoints` builders renders in the header AND in
 * every agent bubble with no edit anywhere else. Copying the strip into
 * the bubble renderer instead would fork exactly the code this module
 * exists to keep single.
 *
 * The strip's datapoints and where each scope reads them from:
 * - parent workspace — session-only chrome (`?parent_ws=`); agents carry
 *   none, and their spawner is already visible as the enclosing feed.
 * - time — the running turn's elapsed clock for the session
 *   (`turnStartedAt` + the `TaskTimer` tick); the agent's own span for a
 *   bubble (its `tool-use-start` ts to its result, or to now while it
 *   runs — kept ticking by the same once-a-second discipline, see
 *   `agent-clock.ts`).
 * - tokens — the conversation's CURRENT context size: the session's
 *   standing figure, or the subagent conversation's own (the attributed
 *   `usage` frames banked on its tool item).
 * - agents / tasks — the counter rosters, session-wide or filtered to the
 *   agent's DIRECT children (see `agents.ts` / `tasks.ts`).
 */
import { agentSubagents, agentsMenuHtml, sessionSubagents, SUBAGENT_TOOLS } from "./agents.js";
import { CounterEntry } from "./counter-menu.js";
import { formatElapsed } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import { ConversationItem, StoreState, ToolItem } from "./store.js";
import { agentTasks, sessionTasks, tasksMenuHtml } from "./tasks.js";
import { TIMER_SLOT } from "./timer.js";
import { countedTurns } from "./turn-clock.js";

/** Token counts as the topbar and the result chip both write them: `300,000`. */
export function formatTokens(n: number): string {
  return n.toLocaleString("en-US");
}

/**
 * One scope's values for every datapoint the strip renders. Both builders
 * below fill ALL of these, so a new datapoint added here is a compile
 * error until each scope has decided what it means there.
 */
export interface TopbarDatapoints {
  /** Session-only chrome; null renders no entry (agents always pass null). */
  parentWs: string | null;
  /** The scope's elapsed clock, pre-formatted (`5m 30s`, or `--` idle). */
  timerLabel: string;
  /** The scope's CURRENT context size; null is unknown and prints a dash. */
  contextTokens: number | null;
  /** The scope's subagent roster (see `agents.ts`). */
  agents: readonly CounterEntry[];
  /** The scope's task roster (see `tasks.ts`). */
  tasks: readonly CounterEntry[];
  /** The counted-turn clock the counter retention windows age against. */
  currentTurn: number;
}

/** Which counter overlay the strip's owner currently has open. */
export interface TopbarDisclosure {
  agentsOpen: boolean;
  tasksOpen: boolean;
}

/**
 * The datapoint strip: `parent workspace: <ws> · time: <elapsed> ·
 * tokens: <n> · <k> agents ▾ · <k> tasks ▾` (the vterm modeline's context
 * mirror). The parent workspace entry is omitted entirely when PARENT-WS
 * is absent or empty, as is each counter chip before the scope gives it
 * something to count; each value gets its own color via the info-*
 * classes (scoped by the shared `.topbar-info` container class).
 *
 * The model is NOT here. It moved into the #model-select picker, which
 * both names the live model and switches it — printing it again as text
 * immediately left of a dropdown showing the same thing is noise.
 *
 * The two counter chips are the shared `counter-menu` facade: the
 * subagent roster and the task roster. The disclosure state is the
 * caller's (only one overlay is ever open at a time), and CURRENT-TURN is
 * the counted-turn clock their recency windows age against.
 *
 * TIMER-LABEL's span is marked (`TIMER_SLOT`) so the caller's
 * once-a-second tick can repaint that one value without rewriting the
 * whole strip — the two paint paths agree because the tick writes exactly
 * what this would have.
 *
 * CONTEXT-TOKENS is the scope's CURRENT context size (what its next
 * request will carry), never a cumulative spend. `null` means the size is
 * genuinely unknown — a `/clear` and a compaction each leave one behind
 * without reporting it, and a subagent has none before its first request
 * — and prints as a dash rather than a lying `0`.
 */
export function topbarInfoHtml(d: TopbarDatapoints, open: TopbarDisclosure): string {
  const parts: string[] = [];
  if (d.parentWs) {
    parts.push(`parent workspace: <span class="info-ws">${escapeHtml(d.parentWs)}</span>`);
  }
  parts.push(
    `time: <span class="info-time" ${TIMER_SLOT}>${escapeHtml(d.timerLabel)}</span>`,
  );
  const tokens = d.contextTokens === null ? "—" : formatTokens(d.contextTokens);
  parts.push(`tokens: <span class="info-tokens">${tokens}</span>`);
  const agentsMenu = agentsMenuHtml(d.agents, open.agentsOpen, d.currentTurn);
  if (agentsMenu !== "") parts.push(agentsMenu);
  const tasksMenu = tasksMenuHtml(d.tasks, open.tasksOpen, d.currentTurn);
  if (tasksMenu !== "") parts.push(tasksMenu);
  return parts.join(" · ");
}

/**
 * The session's datapoints, as the header strip has always shown them.
 * TIMER-LABEL is handed in rather than derived: the header's clock is the
 * `TaskTimer`'s, and rendering exactly what its last tick painted keeps
 * the two paint paths from ever disagreeing.
 */
export function sessionTopbarDatapoints(
  state: StoreState,
  parentWs: string | null,
  timerLabel: string,
): TopbarDatapoints {
  return {
    parentWs,
    timerLabel,
    contextTokens: state.contextTokens,
    agents: sessionSubagents(state.items),
    tasks: sessionTasks(state.items),
    currentTurn: countedTurns(state.items),
  };
}

/**
 * How long the agent's call has run: from its `tool-use-start` stamp to
 * its result once settled, to NOW-MS while it is still going. Unlike the
 * session's clock there is no idle state — an agent bubble exists only
 * once its call started, and a settled one reads its total span rather
 * than a dash.
 */
export function agentElapsedLabel(agent: ToolItem, nowMs: number): string {
  const end = agent.resultTs !== undefined ? Date.parse(agent.resultTs) : nowMs;
  return formatElapsed(end - Date.parse(agent.ts));
}

/**
 * One agent's datapoints: the same strip the session header renders,
 * scoped to AGENT. ITEMS is the full session list — the direct-children
 * filters and the counted-turn clock both read it.
 */
export function agentTopbarDatapoints(
  items: readonly ConversationItem[],
  agent: ToolItem,
  nowMs: number,
): TopbarDatapoints {
  return {
    parentWs: null,
    timerLabel: agentElapsedLabel(agent, nowMs),
    contextTokens: agent.contextTokens ?? null,
    agents: agentSubagents(items, agent.toolUseId),
    tasks: agentTasks(items, agent.toolUseId),
    currentTurn: countedTurns(items),
  };
}

/**
 * Marks a bubble's topbar with the agent it is scoped to. The tick
 * (`agent-clock.ts`) addresses each live agent's timer slot through it,
 * and the feed's click delegation reads it back to key counter
 * disclosure per bubble.
 */
export const TOPBAR_AGENT_ATTR = "data-topbar-agent";

/**
 * The strip a subagent's bubble carries: `topbarInfoHtml` under the same
 * `.topbar-info` styling contract the header strip renders under, wrapped
 * with the bubble-side identity (`TOPBAR_AGENT_ATTR`) the tick and the
 * click delegation key off.
 */
export function agentTopbarHtml(
  items: readonly ConversationItem[],
  agent: ToolItem,
  open: TopbarDisclosure,
  nowMs: number,
): string {
  return `<div class="agent-topbar topbar-info" ${TOPBAR_AGENT_ATTR}="${escapeHtml(
    agent.toolUseId,
  )}">${topbarInfoHtml(agentTopbarDatapoints(items, agent, nowMs), open)}</div>`;
}

// --- counter click delegation ---------------------------------------------

/**
 * The subset of a DOM element the click classifier reads. Structural
 * rather than `Element` so the classification is testable without a DOM
 * in the dep tree — the same discipline the timer host follows.
 */
export interface ClickTarget {
  closest(selector: string): { getAttribute(name: string): string | null } | null;
}

/** One strip click's meaning: flip a counter overlay, or reveal an agent row. */
export type TopbarClick =
  | { kind: "toggle"; menu: "agents" | "tasks" }
  | { kind: "reveal"; agentId: string };

/**
 * Classify a click inside a topbar strip into the verb it asks for, or
 * null for a click on nothing actionable. Both delegations — the
 * header's (main.ts) and the agent bubbles' (FeedRenderer) — route
 * through this, so the strip's click vocabulary can never fork between
 * the two surfaces that render it.
 */
export function topbarClickAction(target: ClickTarget): TopbarClick | null {
  if (target.closest("[data-agents-toggle]")) return { kind: "toggle", menu: "agents" };
  if (target.closest("[data-tasks-toggle]")) return { kind: "toggle", menu: "tasks" };
  const agentId = target.closest(".agent-row")?.getAttribute("data-agent-id");
  if (agentId) return { kind: "reveal", agentId };
  return null;
}

/** The disclosure after a chip click: the open menu closes, any other opens. */
export function nextCounterMenu(
  current: "agents" | "tasks" | null,
  clicked: "agents" | "tasks",
): "agents" | "tasks" | null {
  return current === clicked ? null : clicked;
}

/** One live agent the bubble tick keeps counting for. */
export interface AgentClockEntry {
  id: string;
  startedAt: string;
}

/**
 * Every subagent call still running (no result yet), with the stamp its
 * bubble clock counts from — the set the once-a-second bubble tick
 * repaints (see `agent-clock.ts`). Settled agents drop out here: their
 * frozen span is baked into their bubble's HTML by the ordinary render.
 */
export function runningAgentClocks(items: readonly ConversationItem[]): AgentClockEntry[] {
  const live: AgentClockEntry[] = [];
  for (const item of items) {
    if (item.kind !== "tool" || !SUBAGENT_TOOLS.has(item.toolName)) continue;
    if (item.result) continue;
    live.push({ id: item.toolUseId, startedAt: item.ts });
  }
  return live;
}
