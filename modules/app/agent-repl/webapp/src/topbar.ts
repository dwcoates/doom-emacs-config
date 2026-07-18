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
 * - tokens — the session renders its CURRENT context size behind the
 *   tokens dropdown (`tokens.ts`: the chip is the standing context size,
 *   the overlay the cumulative spend and whole-tree resolution); an agent
 *   bubble renders its conversation's CURRENT context size as a plain
 *   figure (the attributed `usage` frames banked on its tool item).
 * - agents / tasks — the counter rosters, session-wide or filtered to the
 *   agent's DIRECT children (see `agents.ts` / `tasks.ts`).
 */
import { agentSubagents, agentsMenuHtml, sessionSubagents, SUBAGENT_TOOLS } from "./agents.js";
import { CounterEntry } from "./counter-menu.js";
import { formatElapsed } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import { ConversationItem, StoreState, ToolItem, topLevelUsage } from "./store.js";
import { agentTasks, sessionTasks, tasksMenuHtml } from "./tasks.js";
import { TIMER_SLOT } from "./timer.js";
import { TokenMenuData, formatTokens, tokensMenuHtml } from "./tokens.js";
import { countedTurns } from "./turn-clock.js";

/**
 * One scope's values for every datapoint the strip renders. Both builders
 * below fill ALL of these, so a new datapoint added here is a compile
 * error until each scope has decided what it means there.
 */
export interface TopbarDatapoints {
  /** Session-only chrome; null renders no entry (agents always pass null). */
  parentWs: string | null;
  /**
   * The scope's elapsed clock, pre-formatted (`5m 30s`, or `--` idle), or
   * null to render NO time datapoint at all. The session passes null — its
   * turn clock moved to the live feed-tail row beside the progress indicator
   * (see `turnStatsRowHtml`) — while an agent bubble passes its own span.
   */
  timerLabel: string | null;
  /**
   * The scope's plain tokens figure — its conversation's CURRENT context
   * size — rendered only when `tokenMenu` is null; null is unknown and
   * prints a dash.
   */
  contextTokens: number | null;
  /**
   * The session scope's tokens datapoint: its cumulative usage behind the
   * tokens dropdown (`tokens.ts`). An agent scope passes null — a bubble
   * has no session-wide usage map — and renders the plain `contextTokens`
   * figure instead.
   */
  tokenMenu: TokenMenuData | null;
  /** The scope's subagent roster (see `agents.ts`). */
  agents: readonly CounterEntry[];
  /** The scope's task roster (see `tasks.ts`). */
  tasks: readonly CounterEntry[];
  /** The counted-turn clock the counter retention windows age against. */
  currentTurn: number;
}

/** Which of the strip's overlays its owner currently has open. */
export interface TopbarDisclosure {
  agentsOpen: boolean;
  tasksOpen: boolean;
  /** The tokens breakdown; only the session strip renders its chip. */
  tokensOpen: boolean;
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
 * TIMER-LABEL renders the `time:` datapoint only when non-null — an agent
 * bubble's own span, marked (`TIMER_SLOT`) so the bubble tick can repaint
 * that one value without rewriting the whole strip. The session passes null
 * (its turn clock moved to the live feed-tail row, see `turnStatsRowHtml`),
 * so the header strip omits `time:` entirely.
 *
 * The tokens datapoint splits by scope. With TOKEN-MENU set (the session)
 * it is the tokens dropdown chip: the figure is that scope's CUMULATIVE
 * input-side spend and the overlay its full breakdown. Without one (an
 * agent bubble) it is the plain CONTEXT-TOKENS figure — the scope's
 * CURRENT context size, whose `null` means genuinely unknown (a `/clear`,
 * a compaction, a subagent before its first request) and prints as a dash
 * rather than a lying `0`.
 */
export function topbarInfoHtml(d: TopbarDatapoints, open: TopbarDisclosure): string {
  const parts: string[] = [];
  if (d.parentWs) {
    parts.push(`parent workspace: <span class="info-ws">${escapeHtml(d.parentWs)}</span>`);
  }
  if (d.timerLabel !== null) {
    parts.push(
      `time: <span class="info-time" ${TIMER_SLOT}>${escapeHtml(d.timerLabel)}</span>`,
    );
  }
  if (d.tokenMenu !== null) {
    parts.push(tokensMenuHtml(d.tokenMenu, open.tokensOpen));
  } else {
    const tokens = d.contextTokens === null ? "—" : formatTokens(d.contextTokens);
    parts.push(`tokens: <span class="info-tokens">${tokens}</span>`);
  }
  const agentsMenu = agentsMenuHtml(d.agents, open.agentsOpen, d.currentTurn);
  if (agentsMenu !== "") parts.push(agentsMenu);
  const tasksMenu = tasksMenuHtml(d.tasks, open.tasksOpen, d.currentTurn);
  if (tasksMenu !== "") parts.push(tasksMenu);
  return parts.join(" · ");
}

/**
 * The session's datapoints for the header strip. The turn clock is NOT among
 * them: it moved to the live feed-tail row beside the progress indicator (see
 * `turnStatsRowHtml`), so the session passes `timerLabel: null` and the strip
 * omits the `time:` datapoint entirely.
 */
export function sessionTopbarDatapoints(
  state: StoreState,
  parentWs: string | null,
): TopbarDatapoints {
  return {
    parentWs,
    timerLabel: null,
    // The session's tokens datapoint is the dropdown: the chip reads the
    // session's CURRENT context size (`state.contextTokens`, the same
    // figure the response bubble shows) and the overlay carries the
    // cumulative input-side spend plus the whole-tree resolution.
    contextTokens: null,
    tokenMenu: {
      contextSize: state.contextTokens,
      topLevel: topLevelUsage(state),
      models: state.modelUsage,
    },
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
    // A bubble has no session-wide usage map to break down, so its
    // tokens datapoint stays the plain context figure above.
    tokenMenu: null,
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

/**
 * A strip dropdown's stem: which overlay a chip toggle names. The tokens
 * chip renders only in the session strip, but the vocabulary is shared —
 * a bubble delegation simply never sees its toggle.
 */
export type TopbarMenu = "agents" | "tasks" | "tokens";

/** One strip click's meaning: flip a chip's overlay, or reveal a roster row's bubble. */
export type TopbarClick =
  | { kind: "toggle"; menu: TopbarMenu }
  | { kind: "reveal"; agentId: string }
  | { kind: "reveal-task"; taskId: string };

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
  if (target.closest("[data-tokens-toggle]")) return { kind: "toggle", menu: "tokens" };
  const agentId = target.closest(".agent-row")?.getAttribute("data-agent-id");
  if (agentId) return { kind: "reveal", agentId };
  const taskId = target.closest(".task-row")?.getAttribute("data-task-id");
  if (taskId) return { kind: "reveal-task", taskId };
  return null;
}

/** The disclosure after a chip click: the open menu closes, any other opens. */
export function nextCounterMenu(
  current: TopbarMenu | null,
  clicked: TopbarMenu,
): TopbarMenu | null {
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
