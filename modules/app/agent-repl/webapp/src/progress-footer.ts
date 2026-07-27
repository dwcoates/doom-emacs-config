/**
 * progress-footer — THE consolidated in-flight indicator (F1).
 *
 * One raised, flat, segmented dock at the bottom of the feed column, capped to
 * the same width as the widest response bubble, replacing every scattered
 * progress signal the webapp used to derive for itself: the thinking / working
 * / retrying tail rows, the pulse machinery that chose between them, and the
 * live duration/token stats row they orbited.
 *
 * It DERIVES NOTHING. Its whole input is the daemon-resolved `ProgressInput`
 * (`ProgressView`), plus the two rosters relocated here from the topbar and the
 * running-tool liveness the store already banks from `HeartbeatView`. The
 * webapp's job is to render what the daemon resolved.
 *
 * SHAPE (the V4 "segmented dock" from design-progress-footer-samples.html): a
 * grabber notch over a row of bordered cells —
 *
 *   phase │ activity detail (grows) │ turn clock │ token cell │ counters
 *
 * with an error row under it when an error stands, and a detail sheet that
 * opens on a click. Thin by default: one line of cells.
 *
 * TICKING. The turn clock and the running tool's elapsed are the only values
 * that change without a frame arriving, and they are repainted through the
 * `TaskTimer` slot discipline (`paintTurnTimer`) — one span rewritten per
 * second, never a re-render of the footer, and never of the feed.
 */
import { AGENTS_SPEC, agentsMenuHtml } from "./agents.js";
import { CounterEntry, CounterSpec, isActive } from "./counter-menu.js";
import { formatElapsed } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import type { ProgressInput, WebRenderState } from "./state-adapter.js";
import { ConversationItem, ToolItem } from "./store.js";
import { TASKS_SPEC, tasksMenuHtml } from "./tasks.js";
import { IDLE_LABEL, TIMER_SLOT } from "./timer.js";
import { compactTokens } from "./tokens.js";

/** Which of the footer's counter overlays is open, and whether it is expanded. */
export interface FooterDisclosure {
  agentsOpen: boolean;
  tasksOpen: boolean;
  /** Whether the detail sheet is showing. */
  expanded: boolean;
}

/** Everything one footer render needs. */
export interface FooterInput {
  /** The daemon's resolved view, or null before the first one lands. */
  progress: ProgressInput | null;
  /**
   * THE workspace's resolved phase (F5), read off the same `WorkspaceState` the
   * Emacs tab bar and the sidebar dot read. `null` before the first one lands,
   * which collapses the phase cell rather than naming a phase nothing resolved.
   *
   * It arrives here rather than on `progress` because a phase carried in two
   * messages is a phase kept in two copies, and the stale copy is what read
   * "starting" against an already-green tab.
   */
  renderState: WebRenderState | null;
  /** The session's subagent roster, relocated from the topbar. */
  agents: readonly CounterEntry[];
  /** The session's task roster, relocated from the topbar. */
  tasks: readonly CounterEntry[];
  /**
   * The feed's items, for the ONE thing the ProgressView does not carry: which
   * tool is running right now and how long it has been going (the store banks
   * that from the `HeartbeatView` relay).
   */
  items: readonly ConversationItem[];
  /** The turn clock's current reading, baked in so a fresh render shows it. */
  timerLabel: string;
}

/** The phase word and accent class the footer's anchor cell wears. */
export interface PhaseLabel {
  word: string;
  /**
   * Accent class stem. The five state tones map onto the five colors the
   * tab-bar and the sidebar dots speak, so a workspace never reads one way
   * here and another there:
   *   `thinking` red    a turn is in flight
   *   `blocked`  purple the vendor or the account has stopped this session
   *   `async`    yellow no foreground turn, live detached work
   *   `ok`       green  ready
   *   `error`    blue   the route is compromised on our side
   *   `muted`    quiet chrome (merge phases), which spend no color
   */
  tone: "thinking" | "blocked" | "async" | "retry" | "error" | "ok" | "muted";
  /** Whether the phase spins (the agent is actively working). */
  spinning: boolean;
}

/**
 * The workspace's resolved render state as the footer's anchor word, tone, and
 * spin — read off the ONE authority, not off a copy.
 *
 * The vocabulary is CLOSED, so a new render state is a compile error here
 * rather than a footer that silently shows nothing. Merge phases get the quiet
 * muted tone: a merge is chrome the sidebar leads on, and the footer only
 * echoes it.
 */
export function phaseLabel(state: WebRenderState): PhaseLabel {
  switch (state) {
    case "thinking":
      return { word: "thinking", tone: "thinking", spinning: true };
    case "permission":
      return { word: "permission", tone: "retry", spinning: false };
    case "done":
      return { word: "done", tone: "ok", spinning: false };
    // GREEN like done: an interrupted turn is a CONCLUDED turn — the user
    // asked for the stop, got it, and can prompt again immediately. The word
    // is the distinction; the color claim is the same.
    case "interrupted":
      return { word: "interrupted", tone: "ok", spinning: false };
    // GREEN, not muted: idle and ready are the same claim as done — the
    // route works and the agent is available. Greying them said "nothing to
    // see here" about a workspace that was ready to be used.
    case "idle":
      return { word: "ready", tone: "ok", spinning: false };
    case "ready":
      return { word: "ready", tone: "ok", spinning: false };
    // YELLOW: no foreground turn, but detached work continues.
    case "idle_async":
      return { word: "monitoring", tone: "async", spinning: true };
    // BLUE: the route is compromised on our side. `starting` keeps its spin
    // because bring-up really is in progress.
    case "init":
      return { word: "starting", tone: "error", spinning: true };
    // PURPLE: blocked until a human or the vendor acts. It does NOT spin —
    // every other phase that spins is work happening, and animating this one
    // would say the opposite of what it means.
    case "vendor_blocked":
      return { word: "blocked", tone: "blocked", spinning: false };
    case "dead":
      return { word: "dead", tone: "error", spinning: false };
    case "degraded":
      return { word: "degraded", tone: "error", spinning: false };
    case "merging":
      return { word: "merging", tone: "muted", spinning: true };
    case "merge_queued":
      return { word: "merge queued", tone: "muted", spinning: false };
    case "merge_conflict":
      return { word: "merge conflict", tone: "error", spinning: false };
    case "merge_failed":
      return { word: "merge failed", tone: "error", spinning: false };
    case "merged":
      return { word: "merged", tone: "ok", spinning: false };
    default: {
      const never: never = state;
      throw new Error(`progress-footer: unhandled render state ${String(never)}`);
    }
  }
}

/** One live activity, as the detail cell shows it. */
export interface Activity {
  text: string;
  /** Accent class stem, one per window so they read apart at a glance. */
  tone: "tool" | "thinking" | "blocked" | "retry" | "error" | "muted";
}

/**
 * The single activity the detail cell shows, or null when nothing is live.
 *
 * PRECEDENCE, most-blocking first: an auth prompt and a rate limit are the
 * agent stopped dead, a session parked on the user is the agent waiting on
 * YOU, a retry is the agent stalled, a compaction and a hook are the agent
 * busy with something other than the answer, and a running tool is ordinary
 * work. Exactly one speaks, so the cell never stacks two accounts of the same
 * dead air — the whole point of consolidating the old tail rows.
 *
 * The `blocked` window sits above the retry/compaction/tool band because
 * "nothing will happen until you act" outranks any account of the agent being
 * busy. It sits BELOW auth and rate limits, which are the same statement with
 * a specific remedy attached.
 */
export function activityDetail(input: FooterInput, nowMs: number): Activity | null {
  const p = input.progress;
  if (p === null) return null;
  if (p.authenticating !== null) {
    return { text: authText(p.authenticating.detail), tone: "error" };
  }
  if (p.rateLimited !== null) {
    return { text: rateLimitText(p.rateLimited), tone: "error" };
  }
  if (p.blocked !== null) {
    return { text: p.blocked.detail, tone: "retry" };
  }
  if (p.retrying !== null) {
    return { text: `retrying · ${p.retrying.detail}`, tone: "retry" };
  }
  if (p.compacting !== null) {
    return { text: "compacting…", tone: "thinking" };
  }
  if (p.hook !== null) {
    return { text: `hook · ${p.hook.detail}`, tone: "muted" };
  }
  const tool = runningTool(input.items);
  if (tool !== null) {
    return { text: `${tool.toolName} · ${toolElapsed(tool, nowMs)}`, tone: "tool" };
  }
  return null;
}

/** The auth window's line, or a bare label when it carries no detail. */
function authText(detail: string): string {
  return detail === "" ? "authenticating…" : `auth · ${detail}`;
}

/**
 * The rate-limit line: when the cap lifts, since that is the only part a
 * reader can act on. The utilization rides along when the vendor reported one.
 */
function rateLimitText(r: NonNullable<ProgressInput["rateLimited"]>): string {
  const parts = ["rate-limited"];
  if (r.resetsAt > 0) parts.push(`until ${clockTime(r.resetsAt * 1000)}`);
  if (r.utilization > 0) parts.push(`${Math.round(r.utilization * 100)}%`);
  return parts.join(" ");
}

/** An epoch-millis instant as the reader's own local `HH:MM`. */
export function clockTime(ms: number): string {
  const at = new Date(ms);
  return `${String(at.getHours()).padStart(2, "0")}:${String(at.getMinutes()).padStart(2, "0")}`;
}

/**
 * The tool call running at the feed's tail, or null when none is. Scans
 * tail-first and takes the newest unsettled call, which is the one whose
 * heartbeat is still arriving.
 */
export function runningTool(items: readonly ConversationItem[]): ToolItem | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind !== "tool") continue;
    if (item.result) continue;
    if (item.toolName === "") continue; // a result-only shell awaiting its pair
    return item;
  }
  return null;
}

/**
 * How long the running tool has been going: the daemon's own heartbeat clock
 * when one has arrived (authoritative, and survives a reconnect), else this
 * tab's reading of the call's start stamp.
 */
export function toolElapsed(tool: ToolItem, nowMs: number): string {
  if (tool.progressElapsedS !== undefined) {
    return formatElapsed(tool.progressElapsedS * 1000);
  }
  return formatElapsed(nowMs - Date.parse(tool.ts));
}

/**
 * The token cell: the CURRENT TURN's cumulative input tokens, with the
 * thinking ticker beside it while the model is reasoning.
 *
 * There is deliberately no output figure — the running output-token count is
 * explicitly unwanted here, and the session-wide total lives in the topbar's
 * tokens chip.
 *
 * Returns "" when the turn has spent nothing yet and nothing is being thought,
 * so the caller drops the cell rather than showing a lying `0 in`.
 */
export function tokenCellHtml(p: ProgressInput): string {
  const parts: string[] = [];
  if (p.inputTokens > 0) {
    parts.push(`<span class="info-tokens">${escapeHtml(compactTokens(p.inputTokens))} in</span>`);
  }
  if (p.thinkingTokens > 0) {
    parts.push(
      `<span class="pfooter-thinking-tokens">${escapeHtml(compactTokens(p.thinkingTokens))} thought</span>`,
    );
  }
  return parts.join(" ");
}

/**
 * The counters cluster: the two rosters relocated from the topbar, plus the
 * queue-depth and pending-permission badges.
 *
 * The rosters go through the SHARED `counter-menu` facade (`agentsMenuHtml` /
 * `tasksMenuHtml`) rather than a footer-local reimplementation, so the chips
 * look and behave exactly as they did in the topbar and cannot drift from it.
 * Every element hides at zero: a badge over nothing is chrome with no news.
 */
export function countersHtml(input: FooterInput, open: FooterDisclosure): string {
  const parts: string[] = [];
  const agents = agentsMenuHtml(input.agents, open.agentsOpen);
  if (agents !== "") parts.push(agents);
  const tasks = tasksMenuHtml(input.tasks, open.tasksOpen);
  if (tasks !== "") parts.push(tasks);
  const p = input.progress;
  if (p !== null && p.queueDepth > 0) {
    parts.push(
      `<span class="pfooter-badge queued" title="prompts the daemon is holding">${p.queueDepth} queued</span>`,
    );
  }
  if (p !== null && p.pendingPermissions > 0) {
    parts.push(
      `<span class="pfooter-badge perm" title="permission prompts waiting on you">${p.pendingPermissions} perm</span>`,
    );
  }
  return parts.join(" ");
}

/**
 * The expansion sheet: one row per fact the collapsed strip had no cell for.
 *
 * It is the home for the detail the thin strip drops — every OPEN window with
 * its age, and the counts the cluster shows only as bare numbers. The task
 * roster and the per-model token breakdown join it when their deferred work
 * lands.
 */
export function sheetHtml(input: FooterInput, nowMs: number): string {
  const p = input.progress;
  if (p === null) return "";
  const rows: string[] = [];
  const windows: ReadonlyArray<[string, ProgressInput["compacting"]]> = [
    ["compacting", p.compacting],
    ["retrying", p.retrying],
    ["authenticating", p.authenticating],
    ["hook", p.hook],
    ["blocked", p.blocked],
  ];
  for (const [name, w] of windows) {
    if (w === null) continue;
    const age = w.sinceMs > 0 ? ` (${formatElapsed(nowMs - w.sinceMs)})` : "";
    const detail = w.detail === "" ? "" : ` — ${w.detail}`;
    rows.push(`<span>${escapeHtml(`${name}${age}${detail}`)}</span>`);
  }
  if (p.rateLimited !== null) {
    rows.push(`<span>${escapeHtml(rateLimitText(p.rateLimited))}</span>`);
  }
  if (p.liveTaskCount > 0) {
    rows.push(`<span>${p.liveTaskCount} live task${p.liveTaskCount === 1 ? "" : "s"}</span>`);
  }
  if (p.ttftMs > 0) {
    rows.push(`<span>first token in ${escapeHtml(formatElapsed(p.ttftMs))}</span>`);
  }
  if (rows.length === 0) {
    rows.push(`<span class="pfooter-sheet-empty">nothing else in flight</span>`);
  }
  return `<div class="pfooter-sheet">${rows.join("")}</div>`;
}

/**
 * The error row: a persistent line carrying the daemon's classified failure,
 * standing until the next turn starts. When the failure names a feed item the
 * row is a button, because clicking it scrolls the feed to that card — the row
 * is the only way to find a failure that has already scrolled away.
 *
 * Its color is the failure's CLASS, from the same five-color assignment the
 * card and the workspace dot take. It used to be a hardcoded red no other
 * surface consulted, so the footer could contradict the very card it pointed
 * at.
 */
export function errorRowHtml(p: ProgressInput): string {
  const f = p.failure;
  if (f === null) return "";
  const addressable = f.uuid !== "";
  const attrs = addressable
    ? ` role="button" tabindex="0" data-pfooter-error-uuid="${escapeHtml(f.uuid)}" title="show the error in the feed"`
    : "";
  const classes = ["pfooter-error", `failure-${f.errorClass.toLowerCase()}`];
  if (addressable) classes.push("addressable");
  return `<div class="${classes.join(" ")}"${attrs}>${escapeHtml(f.message)}</div>`;
}

/** Marks the footer's whole clickable strip, so the expansion toggle delegates. */
export const FOOTER_STRIP_ATTR = "data-pfooter-strip";

/**
 * The whole dock. Returns "" before the daemon has resolved anything, so the
 * slot collapses rather than showing an empty chrome bar.
 */
export function footerHtml(
  input: FooterInput,
  open: FooterDisclosure,
  nowMs: number = Date.now(),
): string {
  const p = input.progress;
  if (p === null) return "";
  const cells: string[] = [];
  // The phase cell is present only once a state has actually been resolved.
  // Before that there is no phase to name, and naming one anyway is exactly
  // the fabrication the stale mirror used to commit.
  if (input.renderState !== null) {
    const phase = phaseLabel(input.renderState);
    const spin = phase.spinning ? `<span class="pfooter-spin" aria-hidden="true"></span> ` : "";
    cells.push(
      `<div class="pfooter-cell pfooter-phase ${phase.tone}">${spin}${escapeHtml(phase.word)}</div>`,
    );
  }
  const activity = activityDetail(input, nowMs);
  cells.push(
    `<div class="pfooter-cell pfooter-grow ${activity ? activity.tone : "muted"}">` +
      `${activity ? escapeHtml(activity.text) : ""}</div>`,
  );
  // The clock cell always renders while a turn is in flight: it owns the one
  // span the tick repaints, so dropping it would leave the tick nothing to
  // write to. Off-turn it shows the idle label rather than disappearing, so the
  // strip's cell geometry does not jump on every turn boundary.
  const label = p.turnStartedAtMs > 0 ? input.timerLabel : IDLE_LABEL;
  cells.push(
    `<div class="pfooter-cell pfooter-clock">` +
      `<span class="info-time" ${TIMER_SLOT}>${escapeHtml(label)}</span></div>`,
  );
  const tokens = tokenCellHtml(p);
  if (tokens !== "") cells.push(`<div class="pfooter-cell pfooter-tokens">${tokens}</div>`);
  const counters = countersHtml(input, open);
  if (counters !== "") cells.push(`<div class="pfooter-cell pfooter-counters">${counters}</div>`);

  return (
    `<div class="pfooter" role="status" aria-live="polite">` +
    `<div class="pfooter-grab" aria-hidden="true"></div>` +
    `<div class="pfooter-cells" ${FOOTER_STRIP_ATTR} role="button" tabindex="0" ` +
    `aria-expanded="${open.expanded}" title="click for detail">${cells.join("")}</div>` +
    errorRowHtml(p) +
    (open.expanded ? sheetHtml(input, nowMs) : "") +
    `</div>`
  );
}

// --- click delegation --------------------------------------------------------

/** The subset of an element the footer's click classifier reads. */
export interface FooterClickTarget {
  closest(selector: string): { getAttribute(name: string): string | null } | null;
}

/** One footer click's meaning. */
export type FooterClick =
  | { kind: "toggle-menu"; menu: "agents" | "tasks" }
  | { kind: "reveal-agent"; agentId: string }
  | { kind: "reveal-task"; taskId: string }
  | { kind: "reveal-error"; uuid: string }
  | { kind: "toggle-expand" };

/**
 * Classify a click inside the footer, or null for a click on nothing
 * actionable.
 *
 * The roster verbs are checked BEFORE the strip's expansion toggle: a counter
 * chip and its rows live inside the strip, so testing the strip first would
 * swallow every roster click into an expansion. The error row is likewise its
 * own verb, since it sits outside the strip entirely.
 */
export function footerClickAction(target: FooterClickTarget): FooterClick | null {
  if (target.closest("[data-agents-toggle]")) return { kind: "toggle-menu", menu: "agents" };
  if (target.closest("[data-tasks-toggle]")) return { kind: "toggle-menu", menu: "tasks" };
  const agentId = target.closest(".agent-row")?.getAttribute("data-agent-id");
  if (agentId) return { kind: "reveal-agent", agentId };
  const taskId = target.closest(".task-row")?.getAttribute("data-task-id");
  if (taskId) return { kind: "reveal-task", taskId };
  const uuid = target
    .closest("[data-pfooter-error-uuid]")
    ?.getAttribute("data-pfooter-error-uuid");
  if (uuid) return { kind: "reveal-error", uuid };
  if (target.closest(`[${FOOTER_STRIP_ATTR}]`)) return { kind: "toggle-expand" };
  return null;
}

/** Whether either roster has anything to show (the cluster's own gate). */
export function hasLiveCounters(input: FooterInput): boolean {
  return input.agents.some(isActive) || input.tasks.some(isActive);
}

/** The two counter specs the footer hosts, for the caller's reveal notices. */
export const FOOTER_COUNTER_SPECS: Readonly<Record<"agents" | "tasks", CounterSpec>> = {
  agents: AGENTS_SPEC,
  tasks: TASKS_SPEC,
};

// --- the DOM owner -----------------------------------------------------------

/**
 * The footer's slot renderer: owns one element and rewrites it per frame, with
 * the once-a-second clock tick going through `paintTurnTimer` instead.
 */
export class ProgressFooter {
  private open: FooterDisclosure = { agentsOpen: false, tasksOpen: false, expanded: false };

  constructor(
    private readonly el: HTMLElement,
    private readonly now: () => number = () => Date.now(),
  ) {}

  /** Rewrite the dock. Every value it interpolates is escaped in the builders. */
  render(input: FooterInput): void {
    this.el.innerHTML = footerHtml(input, this.open, this.now());
  }

  /**
   * Repaint JUST the turn clock's span.
   *
   * The tick is deliberately NOT a re-render: rewriting the dock once a second
   * would churn the counter overlays and the sheet in service of one changing
   * digit, and re-rendering the FEED for it (which the nuked stats row's
   * ancestor once did) would be far worse. Same discipline as the topbar's old
   * `TaskTimer` slot.
   */
  paintTurnTimer(label: string): void {
    const slot = this.el.querySelector(`[${TIMER_SLOT}]`);
    if (slot) slot.textContent = label;
  }

  /** Which overlay is open, or null. Flipping one closes any other. */
  setMenu(menu: "agents" | "tasks" | null): void {
    this.open = {
      ...this.open,
      agentsOpen: menu === "agents",
      tasksOpen: menu === "tasks",
    };
  }

  /** Flip the detail sheet. */
  toggleExpanded(): void {
    this.open = { ...this.open, expanded: !this.open.expanded };
  }

  /** Close every overlay (a click-away, or the feed taking focus). */
  closeMenus(): void {
    this.open = { ...this.open, agentsOpen: false, tasksOpen: false };
  }

  /** The current disclosure, for the caller's own bookkeeping. */
  disclosure(): FooterDisclosure {
    return this.open;
  }
}
