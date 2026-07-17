/**
 * DOM renderers for conversation items. Component naming mirrors the
 * spec (§2.4–2.7): TextStream, Thinking, ToolCard/<Name>,
 * PermissionPrompt. The feed renderer reuses one element per item key
 * so streaming updates do not rebuild the whole list.
 */
import { SUBAGENT_TOOLS } from "./agents.js";
import { parseJournal } from "./async-stream.js";
import {
  TOPBAR_AGENT_ATTR,
  TopbarMenu,
  agentTopbarHtml,
  nextCounterMenu,
  topbarClickAction,
} from "./topbar.js";
import { taskCreateToolUseId } from "./tasks.js";
import { formatTokens } from "./tokens.js";
import { formatAge, formatDuration, formatDurationCeil, formatElapsed } from "./duration.js";
import {
  CLICK_THROUGH_SELECTOR,
  PANEL_CLASS,
  applyExpanded,
  expandOwnSections,
  expandedKeys,
  sectionsIn,
} from "./expand.js";
import { escapeHtml, highlightCode, languageForPath } from "./highlight.js";
import { partitionFeed, spawnedTaskIds } from "./partition.js";
import {
  effectiveAsyncSource,
  mayNest,
  mergeChildren,
  openSubfeedSourceIds,
  transcriptFeed,
} from "./subfeed.js";
import { parseUnsupportedCommand } from "./unsupported.js";
import {
  chessGameContainerHtml,
  hydrateChessGames,
  releaseChessGames,
  splitChessGameSegments,
} from "./chess-game.js";
import { inline, renderMarkdown } from "./markdown.js";
import { isMetapromptTree, renderTreeHtml } from "./metaprompt-tree.js";
import { AsyncSource, ModelInfo, QueuedItem } from "./protocol.js";
import { navTokensForItem } from "./nav.js";
import { isPinnedToBottom, parkAtTail, revealNode } from "./scroll.js";
import { blocksToText, isClearTurn, itemsFromLastClear, userTurnText } from "./turn.js";
import { gnsFolds } from "./gns.js";
import { asyncByBubble, isWatcher } from "./watchers.js";
import { TaskTail, WatcherPoller } from "./watcher-poll.js";
import {
  CompactBoundaryItem,
  ConversationItem,
  ErrorItem,
  PermissionItem,
  ResultItem,
  RetryItem,
  StoreState,
  SystemItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "./store.js";

export interface Actions {
  decidePermission(requestId: string, behavior: "allow" | "deny"): void;
  /**
   * Answer an AskUserQuestion permission. UPDATED-INPUT is the tool
   * input echoed back with the `answers` record filled in (question
   * text → answer string, multi-select comma-separated) — the exact
   * contract the CLI's permission component expects.
   */
  answerQuestions(requestId: string, updatedInput: unknown): void;
  /** Remove a parked queued message without ever running it (§2.13). */
  cancelQueued(queueId: string): void;
  /** Escalate a parked queued message to preempt the running turn (§2.13). */
  runQueuedNow(queueId: string): void;
  /**
   * Send TEXT as a user prompt — the channel behind the card controls
   * (stop task). Prompt-mediated on purpose: no daemon-native kill
   * exists, so the control ASKS THE AGENT, spends a real turn, and
   * queues behind an in-flight one; the button's label says as much.
   */
  sendPrompt?(text: string): void;
  /**
   * Fetch a bounded chunk of a detached task's output tail from OFFSET
   * (§ watcher-bubble expansion). Present only when a daemon backs the
   * webapp; the watcher fold degrades to store-streamed tails without it.
   */
  fetchTaskTail?(taskId: string, offset: number): Promise<{
    text: string;
    offset: number;
    done: boolean;
    elapsed_ms: number;
  }>;
  /**
   * Ask Emacs (via the daemon) to open a workspace that adds graphical
   * support for a slash command this environment refused. Resolves to the
   * workspace name Emacs was asked for; rejects when the ask never landed.
   * Present only when a daemon backs the webapp.
   */
  addSupport?(command: string): Promise<string>;
  /**
   * Fired after every render, once the feed's DOM is settled.
   *
   * A render rewrites the `innerHTML` of every item whose HTML changed,
   * which silently destroys anything a non-renderer put inside one — the
   * feed search's `<mark>`s (search.ts) being the case in hand. Derived DOM
   * like that cannot be re-applied by the same capture/re-apply the
   * renderer's OWN state uses (`expandedKeys`), because the renderer does
   * not know what it is or when it is wanted, so it announces the render
   * instead and lets the owner re-derive.
   *
   * Every render path calls it, including the ones the renderer drives
   * itself (a reveal, a tab click, a watcher poll) — a hook wired only to
   * the boot path's renders would drop the highlight on exactly the
   * re-renders nobody thought about.
   */
  onRendered?(): void;
}

/**
 * How far a support request for one command has got. Absent means the
 * button is untouched and still offering.
 */
export type SupportPhase =
  | { kind: "asking" }
  | { kind: "asked"; workspace: string }
  | { kind: "failed"; error: string };

/** One AskUserQuestion entry as carried in the tool input. */
export interface AskQuestion {
  question: string;
  header: string;
  multiSelect?: boolean;
  options: Array<{ label: string; description: string }>;
}

/** Selected option labels per question, keyed `${requestId} ${qIdx}`. */
export type QuestionSelections = ReadonlyMap<string, ReadonlySet<string>>;

/** The item's questions when it is an AskUserQuestion prompt, else null. */
export function askQuestions(item: PermissionItem): AskQuestion[] | null {
  if (item.toolName !== "AskUserQuestion") return null;
  const input = item.input as { questions?: AskQuestion[] } | null;
  return input && Array.isArray(input.questions) && input.questions.length > 0
    ? input.questions
    : null;
}

/** Tool names the SPA renders specially (§2.6); others use Generic. */
const SPECIAL_TOOLS = new Set([
  "Bash",
  "Read",
  "Edit",
  "Write",
  "Grep",
  "Skill",
  ...SUBAGENT_TOOLS,
]);

/**
 * Tool names whose cards are suppressed entirely: AskUserQuestion's UI
 * IS the permission picker card and ToolSearch is deferred-tool schema
 * plumbing — both feed noise. TaskUpdate is NOT here: the partition
 * confines an update to its task's TaskCreate card, where it renders as
 * a nested history bubble (see toolInput's TaskUpdate form).
 */
const SUPPRESSED_TOOLS = new Set(["AskUserQuestion", "ToolSearch"]);

/**
 * The CLI's placeholder for an assistant message that carried no text of
 * its own. The `/clear` re-init emits one as its contextless reply — the
 * empty answer that would otherwise sit under the divider as a green
 * `(no content)` bubble — so a body that is nothing but this placeholder
 * draws nothing, exactly as the `system: init` beside it does.
 */
const EMPTY_CONTENT_PLACEHOLDER = "(no content)";

function contentToText(
  content: string | Array<{ type: string; text?: string }>,
): string {
  if (typeof content === "string") return content;
  return content
    .filter((b) => b.type === "text")
    .map((b) => b.text ?? "")
    .join("\n");
}

// --- topbar chrome ------------------------------------------------------------

// The topbar datapoint strip moved to topbar.ts, where ONE renderer
// serves both the session header and the agent-scoped bubble strips.
// The session's tokens datapoint there is the tokens dropdown chip
// (`tokens.ts`); an agent strip keeps its plain context-size figure.

/**
 * The compaction-in-progress banner: shown while the SDK is compacting the
 * conversation (a `compact-status` frame opened the window and the
 * `compact-boundary` closes it). The SDK reports no progress percentage, so
 * the bar is INDETERMINATE — a looping sweep, mirroring the CLI's own
 * "Compacting conversation…" spinner rather than pretending to a fraction it
 * cannot know.
 *
 * Returns "" when no compaction is running, so the caller can blindly write
 * the result into the banner slot and let an empty string collapse it.
 */
export function compactionBannerHtml(compacting: boolean): string {
  if (!compacting) return "";
  return (
    `<div class="compact-progress" role="status" aria-live="polite">` +
    `<span class="compact-progress-label">Compacting conversation…</span>` +
    `<span class="compact-progress-track"><span class="compact-progress-bar"></span></span>` +
    `</div>`
  );
}

/**
 * The "interrupting…" indicator: the red twin of the textless-thinking
 * spinner (`.thinking-pending`), shown at the feed tail while the store is
 * interrupting a turn. It marks the window between the interrupt gesture and
 * the turn's aborted result — during which in-flight bubbles keep streaming
 * but no new ones open. Same spinner animation as the thinking indicator, in
 * the alarm red rather than the working orange.
 *
 * Returns "" when not interrupting, so the caller can drop the tail node.
 */
export function interruptingIndicatorHtml(interrupting: boolean): string {
  if (!interrupting) return "";
  return (
    `<div class="interrupting-pending" role="status" aria-live="polite">` +
    `<span class="interrupting-spinner" aria-hidden="true"></span> interrupting…` +
    `</div>`
  );
}

/**
 * The "working…" indicator: the orange dead-air row shown at the feed tail
 * while the main agent chain is in flight but nothing else is live — no
 * streaming cursor, no thinking spinner, no running-tool arc, and no frontier
 * breath carrying the beat (see `pulseTarget`, whose `working` state gates
 * this). It fills the gaps a still feed would otherwise leave: the window
 * right after a prompt is sent before the first frame, and any gap between a
 * tool result going back and the next content frame. Reuses the textless-
 * thinking spinner (`.thinking-spinner`) in the working orange, so "still
 * working" reads the same wherever that color speaks.
 *
 * Returns "" when not working, so the caller can drop the tail node.
 */
export function workingRowHtml(working: boolean): string {
  if (!working) return "";
  return (
    `<div class="working-pending" role="status" aria-live="polite">` +
    `<span class="thinking-spinner" aria-hidden="true"></span> working…` +
    `</div>`
  );
}

/**
 * The "retrying…" indicator: the purple tail row shown while the SDK is
 * auto-retrying a failed API request (429/529/5xx/connection drop) — the
 * window between a `retry` frame and the next content frame. Same
 * `thinking-spin` arc as the thinking and interrupting rows, in a purple
 * (`--retry`) that reads as "network", distinct from the interrupting alarm
 * red ("stopping") and the working orange ("working"). It takes precedence
 * over the working row, since a retry is the more specific account of the
 * dead air (see `pulseTarget`, whose `retrying` state gates this).
 *
 * Returns "" when not retrying, so the caller can drop the tail node.
 */
export function retryingRowHtml(retrying: boolean): string {
  if (!retrying) return "";
  return (
    `<div class="retrying-pending" role="status" aria-live="polite">` +
    `<span class="retrying-spinner" aria-hidden="true"></span> retrying…` +
    `</div>`
  );
}

/**
 * The "monitoring…" indicator: the AMBER row saying background (async/detached)
 * work continues while the main chain is IDLE. Distinct from the bucket-1 tail
 * rows (thinking/working/retrying/interrupting), which all mean the main chain
 * is ACTIVE — amber async-quiescence means "model available, background work
 * still running". One markup, two homes: the collapsed face of a bubble's
 * in-bubble catalog (AsyncCatalog), and the global feed-tail fallback shown
 * when the owning bubble is scrolled off (see `showsMonitoringRow`). Reuses
 * the textless-thinking arc (`.thinking-spinner`), tinted amber by
 * `.monitoring-pending`, so "still watching" reads the same wherever it shows.
 *
 * Returns "" when not monitoring, so a caller can drop the node.
 */
export function monitoringRowHtml(monitoring: boolean): string {
  if (!monitoring) return "";
  return (
    `<div class="monitoring-pending" role="status" aria-live="polite">` +
    `<span class="thinking-spinner" aria-hidden="true"></span> monitoring…` +
    `</div>`
  );
}

/**
 * Whether the GLOBAL `monitoring…` tail row shows: the session is IDLE (no
 * turn in flight, so none of thinking/working/retrying/interrupting speak for
 * the tail) yet live async continues somewhere in the feed. It is the
 * always-visible amber signal for when the owning bubble is scrolled off or
 * absent — the quiescent twin of the working row, and mutually exclusive with
 * the whole bucket-1 tail, which only runs while a turn is in flight.
 */
export function showsMonitoringRow(opts: {
  turnInFlight: boolean;
  interrupting: boolean;
  anyLiveAsync: boolean;
}): boolean {
  if (opts.turnInFlight || opts.interrupting) return false;
  return opts.anyLiveAsync;
}

/**
 * The #model-select options: the live model SELECTED, every alternative
 * the daemon offers, and nothing invented.
 *
 * Two cases the picker must not lie about:
 * - No model known yet (pre-hello): a disabled placeholder is selected,
 *   rather than letting the browser auto-select the first option and
 *   claim a model the session is not on.
 * - The live model is not in the menu (an id the CLI accepts but does not
 *   advertise): it is prepended as its own option, so the picker still
 *   names what the session is ACTUALLY running.
 */
export function modelOptionsHtml(
  models: readonly ModelInfo[],
  current: string,
): string {
  const opts: string[] = [];
  if (current === "") {
    opts.push(`<option value="" disabled selected>model…</option>`);
  } else if (!models.some((m) => m.value === current)) {
    opts.push(
      `<option value="${escapeHtml(current)}" selected>${escapeHtml(current)}</option>`,
    );
  }
  for (const m of models) {
    const selected = m.value === current ? " selected" : "";
    opts.push(
      `<option value="${escapeHtml(m.value)}"${selected} title="${escapeHtml(m.description)}">${escapeHtml(m.displayName)}</option>`,
    );
  }
  return opts.join("");
}

// --- per-item components ------------------------------------------------------

/**
 * An ISO8601 ts (§2.1) as the local wall-clock time the reader's own
 * machine shows, zero-padded 24-hour `HH:MM`. The wake line names an
 * absolute future moment a scheduled wakeup fires (`wakes ~10:10`), which
 * a relative label could not; the bubble stamp itself is relative (see
 * `formatBubbleTime`).
 */
export function formatTurnTime(ts: string): string {
  const at = new Date(ts);
  const hh = String(at.getHours()).padStart(2, "0");
  const mm = String(at.getMinutes()).padStart(2, "0");
  return `${hh}:${mm}`;
}

/**
 * The bubble stamp as a relative age: how long ago the envelope ts (§2.1)
 * was, with an `ago` postfix — `5s ago`, `5m 30s ago`, `1h 5m ago` (see
 * `formatAge` for the two-level second-resolution shape). NOW-MS defaults
 * to the reader's clock, so a re-render re-reads it and the stamp ages on
 * its own without the markup having to hold a timer.
 *
 * The daemon's absolute ts stays the source of truth on the item, so a
 * stale session reloaded later recomputes the correct age against the new
 * `now` rather than freezing at whatever the age was when it was recorded.
 */
export function formatBubbleTime(ts: string, nowMs: number = Date.now()): string {
  return `${formatAge(nowMs - Date.parse(ts))} ago`;
}

/**
 * The one bubble shape, shared by the user's prompt and the agent's
 * response: a body column, then a top-right corner column (`.turn-meta`).
 * The corner stacks META — a final response's turn duration and context
 * delta, empty for every other bubble — above the relative-age timestamp,
 * which CSS reveals only while the reader hovers the bubble (see
 * `.turn-meta` / `.turn-ts`). The markup only has to hand both the corner.
 *
 * The corner holds its own flex column rather than floating over the body,
 * so a full-width response line never runs beneath it.
 */
function Bubble(cls: string, body: string, ts: string, meta = ""): string {
  return `<div class="${cls}"><div class="bubble-body">${body}</div><span class="turn-meta">${meta}<span class="turn-ts">${escapeHtml(
    formatBubbleTime(ts),
  )}</span></span></div>`;
}

/**
 * The prompt bubble. It never breathes: a just-sent prompt whose turn has
 * produced nothing visible yet is covered by the orange `working…` tail row
 * now (see `pulseTarget`), which retired the prompt breath — so the bubble is
 * a plain `bubble user`, the same way a running tool card ignores the pulse.
 */
function UserTurn(item: UserTurnItem, panels?: PanelContext): string {
  const divider = isClearTurn(item)
    ? `<div class="clear-divider" role="separator" aria-label="context cleared"></div>`
    : "";
  // A tools-only turn hosts its live async on its own prompt bubble (see
  // asyncByBubble), so the prompt goes amber and catalogs that work too — the
  // invariant holds even for a turn that wrote no answer to host it. The
  // projection keys the prompt by the user-turn's request id.
  const stateCls = hasLiveAsync(item.requestId, panels) ? " async-live" : "";
  const catalog = AsyncCatalog(item.requestId, panels);
  const body = `<pre>${escapeHtml(userTurnText(item))}</pre>${catalog}`;
  return `${Bubble(`bubble user${stateCls}`, body, item.ts)}${divider}`;
}

/**
 * §2.13 status/verdict badge for a queued item. `classifying` is the
 * pre-verdict state; `waiting` parks the message to drain in FIFO order;
 * `interrupt` escalates it to preempt the running turn.
 */
function queuedBadge(item: QueuedItem): { label: string; cls: string } {
  switch (item.status) {
    case "waiting":
      return { label: "queued — waiting", cls: "waiting" };
    case "interrupt":
      return { label: "interrupting…", cls: "interrupt" };
    case "classifying":
    default:
      return { label: "queued — classifying…", cls: "classifying" };
  }
}

/**
 * A parked message in the in-flight queue (§2.13). It is deliberately NOT
 * a `bubble user`: a subdued card, so the reader SEES the message is queued
 * for later and is explicitly not interrupting the running turn unless
 * escalated. It carries the prompt text (injected spans stripped, exactly
 * as a user turn's are), a status/verdict badge, the classifier's one-line
 * reason once known, and a Cancel / Run-now pair. Each button carries the
 * `queue_id` in a data attribute for delegated click handling.
 */
export function QueuedCard(item: QueuedItem): string {
  const badge = queuedBadge(item);
  const qid = escapeHtml(item.queue_id);
  const reason = item.reason
    ? `<div class="queued-reason">${escapeHtml(item.reason)}</div>`
    : "";
  return `
    <div class="queued-card">
      <div class="queued-head">
        <span class="queued-badge ${badge.cls}">${escapeHtml(badge.label)}</span>
      </div>
      <pre class="queued-content">${escapeHtml(blocksToText(item.content))}</pre>
      ${reason}
      <div class="queued-actions">
        <button data-queue-cancel="${qid}">Cancel</button>
        <button data-queue-run-now="${qid}">Run now</button>
      </div>
    </div>`;
}

/** Key identifying one queued card's DOM node across renders (§2.13). */
export function queuedCardKey(item: QueuedItem): string {
  return `queued:${item.queue_id}`;
}

/**
 * A turn's FINAL response gets the green border (§2.4): the answer the
 * turn actually landed on, set apart from the running commentary the
 * agent emits between its tool calls.
 *
 * CHIP is the closing result of the turn that bubble answers. Its stats
 * ride in the bubble's top-right corner (`resultMeta` via `Bubble`'s META):
 * the turn's elapsed time and the context delta it moved, rather than a
 * pill floating in the feed beneath. Only a final response carries one, so
 * CHIP is null for the commentary bubbles above it (see `finalResponses`).
 *
 * The working frontier gets the pulse instead (see `pulseTarget`). The
 * two never land on the same bubble: a final response only exists once the
 * turn has ended, and the pulse only runs while it has not.
 *
 * The bubble is stamped with the time the agent OPENED the block (the
 * `text-start` envelope), not the time it closed it: the stamp then dates
 * the response the same way the user bubble's dates the prompt, and it
 * does not jump while the block streams.
 */
function TextStream(
  item: TextItem,
  chip: ResultItem | null,
  isPulsing = false,
  panels?: PanelContext,
): string {
  const cursor = item.done ? "" : `<span class="cursor">▍</span>`;
  // The async-quiescence invariant: a bubble owning LIVE async wears the
  // amber border and lists that work as selectable badges inside it (see
  // AsyncCatalog). Amber outranks the green final-response — the answer is
  // landed but its background work is not — and flips to green once every
  // member settles (amber → green quiescence). A bubble the projection does
  // not host carries no members, so it never goes amber.
  const liveAsync = hasLiveAsync(item.blockId, panels);
  const stateCls = liveAsync ? " async-live" : chip ? " final-response" : "";
  const cls = `bubble assistant md${stateCls}${isPulsing ? " pulsing" : ""}`;
  // The catalog rides EVERY host bubble, not just a final one: an interrupted
  // or tools-only turn hosts its survivors too (asyncByBubble), so a bubble
  // with no chip can still own live async that must be enumerated in it. The
  // gns-sockets fold stays the completed-answer full-segment view.
  const catalog = AsyncCatalog(item.blockId, panels);
  const gns = chip ? GnsPanel(item.blockId, panels) : "";
  // A completed turn's stats ride in the bubble's top-right corner
  // (`resultMeta`): its elapsed time and the context delta it moved. A
  // sub-second turn reports no time worth showing and a `/clear`-ended turn
  // no delta, so either half may be absent and the corner may render empty.
  const meta = chip ? resultMeta(chip) : "";
  // Chess-game markers split the body FIRST: they must work inside a
  // TLDR-tree response too, and the tree renderer below never sees
  // markdown handling. Each text segment then picks its own pipeline.
  const body = splitChessGameSegments(item.text, !item.done)
    .map((seg) => {
      if ("path" in seg) return chessGameContainerHtml(seg.path);
      // A bare metaprompt TLDR tree (no code fence — fenced trees are the
      // markdown fence handler's job) renders as hanging-indent tree lines;
      // the markdown pipeline would shear its wrapped branches to column 0.
      if (!seg.text.includes("```") && isMetapromptTree(seg.text)) {
        return `<div class="mp-tree">${renderTreeHtml(seg.text, inline)}</div>`;
      }
      return renderMarkdown(seg.text);
    })
    .join("");
  return Bubble(cls, `${body}${cursor}${catalog}${gns}`, item.ts, meta);
}

function Thinking(item: ThinkingItem): string {
  // Adaptive-thinking models withhold the thinking text: the block streams
  // a signature and no thinking_delta, so item.text stays empty. A
  // disclosure triangle over an empty <pre> unfolds to nothing, so a
  // textless block gets a spinner while it is open and, once it closes,
  // disappears entirely (`rendersEmpty`).
  if (item.text === "") {
    return `<div class="thinking-pending"><span class="thinking-spinner" aria-hidden="true"></span> thinking</div>`;
  }
  // While the summary still streams, the disclosure carries the same orange
  // arc the textless indicator spins, beside the `(thinking…)` label — so a
  // texted thinking block reads as live rather than as a finished card that
  // merely happens to be open.
  const state = item.done
    ? ""
    : ` (thinking…) <span class="thinking-spinner" aria-hidden="true"></span>`;
  return `
    <details class="thinking"${item.done ? "" : " open"}>
      <summary>Thinking${state}</summary>
      <pre>${escapeHtml(item.text)}</pre>
    </details>`;
}

/**
 * The renderer-owned context the activity panels draw from: the feed
 * partition's child lists, which cards the user has open, and the
 * question selections child permission prompts need.
 */
export interface PanelContext {
  children: ReadonlyMap<string, readonly ConversationItem[]>;
  isOpen(id: string): boolean;
  selections?: QuestionSelections;
  /** Message-composer drafts per agent id (renderer-owned, like selections). */
  drafts?: ReadonlyMap<string, string>;
  /**
   * Watcher tool items a final-response bubble folds in, keyed by the
   * bubble's block id (see watchers.ts). Absent for a bubble whose turn
   * armed no detached work.
   */
  watchers?: ReadonlyMap<string, readonly ToolItem[]>;
  /**
   * gns-sockets bridge upkeep a final-response bubble folds in, keyed by
   * the host bubble's block id (see gns.ts). Absent for a bubble no
   * bridge segment or bridge-woken turn attached to.
   */
  gnsFolds?: ReadonlyMap<string, readonly ConversationItem[]>;
  /**
   * The poller's accumulated tail for a watcher's task id, when a fold is
   * open and the daemon has been polled (see watcher-poll.ts). Fills the
   * tail for a background agent the WS never streamed, and the live elapsed
   * a settled call's frozen heartbeat no longer feeds.
   */
  taskTail?(taskId: string): TaskTail | undefined;
  /**
   * How far a support request has got for each refused slash command
   * (renderer-owned, like selections). A command absent from the map has
   * an untouched button still offering.
   */
  supportPhases?: ReadonlyMap<string, SupportPhase>;
  /**
   * Whether anything can actually be asked for a support workspace, i.e.
   * whether a daemon backs the webapp. False leaves the unsupported card
   * stating the refusal rather than dangling a button that cannot work.
   */
  canAddSupport?: boolean;
  /**
   * The agent-scoped topbar strip a subagent's card carries (see
   * topbar.ts): the SAME renderer the session header uses, scoped to
   * AGENT. Renderer-owned (like selections) because the counter
   * disclosure it bakes in must survive the card's per-frame re-renders.
   * Absent, the card renders no strip.
   */
  agentTopbar?(agent: ToolItem): string;
  /**
   * How many transcript folds deep this context renders (absent = top).
   * Ticked by `descend` each time a fold recurses into a parsed stream,
   * and read against SUBFEED_DEPTH_CAP so recursion is cut, not runaway.
   */
  depth?: number;
  /**
   * The source ids already rendering in the fold chain above this feed —
   * the cycle guard's memory. A nested spawn card announcing an
   * ancestor's own id renders no fold (see `mayNest`).
   */
  seenSources?: ReadonlySet<string>;
}

/** True when the child renders something the panel and ticker count. */
function visibleChild(item: ConversationItem): boolean {
  return !rendersEmpty(item);
}

/** One child item's ticker line, or "" when it offers nothing live. */
function childLine(item: ConversationItem): string {
  switch (item.kind) {
    case "tool": {
      const head = toolHeadline(item);
      return head === "" ? item.toolName : `${item.toolName}: ${head}`;
    }
    case "text":
      return item.text.replace(/\s+/g, " ").trim();
    case "thinking":
      return item.done ? "" : "thinking…";
    case "permission":
      return item.resolution ? "" : `awaiting permission: ${item.toolName}`;
    default:
      return "";
  }
}

/**
 * The input field that best headlines a tool call, "" when none. The
 * key list is ordered most-specific-first and serves every consumer of
 * a one-line identity: tickers, tab labels, and the generic folded
 * input.
 */
function toolHeadline(item: ToolItem): string {
  if (!item.input) return "";
  const keys = [
    "command",
    "file_path",
    "description",
    "pattern",
    "skill",
    "url",
    "query",
    "subject",
    "name",
    "task_id",
    "reason",
    "prompt",
  ];
  for (const key of keys) {
    const v = item.input[key];
    if (typeof v === "string" && v !== "") return v.replace(/\s+/g, " ");
  }
  return "";
}

/**
 * The collapsed face of an activity panel: how many steps the child feed
 * holds and the most recent one with something to say, capped so the
 * line stays a line.
 */
export function activityTicker(children: readonly ConversationItem[]): string {
  const steps = `${children.length} step${children.length === 1 ? "" : "s"}`;
  for (let i = children.length - 1; i >= 0; i--) {
    const line = childLine(children[i]);
    if (line !== "") {
      const capped = line.length > 80 ? `${line.slice(0, 79)}…` : line;
      return `${steps} · ${capped}`;
    }
  }
  return steps;
}

/**
 * The activity fold on a spawning card: the ticker line is the collapsed
 * face, and the panel — the child feed rendered through the same
 * renderItem the top level uses — exists in the HTML only while the
 * card is open, so a hundred buffered children cost nothing while
 * closed. Open state lives in the RENDERER (like question selections),
 * because the fold must survive the card's own re-renders.
 */
/**
 * The shared skeleton of a click-to-open fold: a pill ticker as the
 * collapsed face and a panel body that exists in the HTML only while open,
 * with open state carried on the wrapper's class and a `data-panel-toggle`
 * the FeedRenderer's delegated handler flips. The activity fold on a
 * spawning card (ActivitySection) and the watcher fold on a final-response
 * bubble (WatcherPanel) both render through this, differing only in their
 * classes, ticker face, and body.
 *
 * BODY is a thunk, not a string: it is called only when the fold is open,
 * so a hundred buffered children (or watcher tails) cost nothing to render
 * while the fold stays closed.
 */
function Fold(opts: {
  id: string;
  foldClass: string;
  tickerClass: string;
  ticker: string;
  body: () => string;
  open: boolean;
}): string {
  const panel = opts.open ? `<div class="agent-panel">${opts.body()}</div>` : "";
  return `<div class="${opts.foldClass}${opts.open ? " open" : ""}" data-panel-toggle="${escapeHtml(opts.id)}">
      <div class="${opts.tickerClass}">${opts.ticker} <span class="agent-caret" aria-hidden="true">${
        opts.open ? "▴" : "▾"
      }</span></div>${panel}
    </div>`;
}

/**
 * The shared body of every fold panel: children rendered through the
 * very renderItem the top level uses, each in its .feed-child shell —
 * the single recursion point all nesting folds share.
 */
function feedChildren(children: readonly ConversationItem[], panels?: PanelContext): string {
  return children
    .map((c) => `<div class="feed-child">${renderItem(c, panels?.selections, undefined, false, panels)}</div>`)
    .join("");
}

function ActivitySection(
  id: string,
  children: readonly ConversationItem[],
  panels: PanelContext,
): string {
  return Fold({
    id,
    foldClass: "agent-activity",
    tickerClass: "agent-ticker",
    ticker: escapeHtml(activityTicker(children)),
    body: () => feedChildren(children, panels),
    open: panels.isOpen(id),
  });
}

// IS-PULSING breathes the card while it is the running frontier (see
// `pulseTarget`): a slow wash under the head's fast arc, so a call in flight
// is never a wholly still card during the second the arc stays hidden. The
// two motions live on different channels — the arc in the badge, the breath
// on the card background — so they read as one signal, not two competing ones.
function ToolCard(
  item: ToolItem,
  panels?: PanelContext,
  tabBar = "",
): string {
  const variant = SPECIAL_TOOLS.has(item.toolName) ? item.toolName : "Generic";
  // In-flight is ONE look, whichever phase the call is in: the orange run
  // badge carrying the same arc the thinking indicator spins, held
  // invisible for its first second by CSS (see .tool-spinner) so the
  // sub-second tools — Edit, Read, most Bash — never flash it. Only the
  // badge's label distinguishes the two phases; a settled card drops the
  // badge for done/error. The card body carries no indicator of its own,
  // so the arc is the single place motion lives.
  const status = item.result
    ? item.result.isError
      ? `<span class="badge err">error</span>`
      : `<span class="badge ok">done</span>`
    : `<span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>${
        item.inputDone ? "running…" : "streaming input…"
      }</span>`;
  const progress = item.progress
    ? `<div class="tool-progress">${escapeHtml(item.progress)}</div>`
    : "";
  // The card's confined children: its panel body, its ticker, and — when
  // one of them is a permission prompt still waiting — a badge loud
  // enough that a CLOSED card can never silently block the turn.
  const children = (panels?.children.get(item.toolUseId) ?? []).filter(visibleChild);
  const pendingPerms = children.filter(
    (c) => c.kind === "permission" && !c.resolution,
  ).length;
  const permBadge =
    pendingPerms > 0 ? `<span class="badge perm">needs permission</span>` : "";
  const activity =
    panels && children.length > 0 ? ActivitySection(item.toolUseId, children, panels) : "";
  // A subagent's card carries its own live topbar right under the head:
  // the session strip's renderer scoped to THIS agent (see topbar.ts).
  const agentTopbar =
    SUBAGENT_TOOLS.has(item.toolName) ? panels?.agentTopbar?.(item) ?? "" : "";
  // The source this card's fold hangs on: daemon-classified for live
  // cards, synthesized from the spawn announcement for transcript-parsed
  // ones — which is what lets a nested spawn card fold at any depth.
  const source = effectiveAsyncSource(item);
  // TABBAR, when a consecutive-run group hands one in, is the row of member
  // chips — rendered as the card's FIRST child so the tabs sit INSIDE the
  // bubble at its top, rather than floating above it (see groupHtml).
  return `
    <div class="tool-card tool-${variant.toLowerCase()}">
      ${tabBar}
      <div class="tool-head"><span class="tool-name">${escapeHtml(item.toolName)}</span>${status}${permBadge}</div>
      ${agentTopbar}
      ${toolInput(item)}
      ${progress}
      ${activity}
      ${AsyncFold(item, source, panels)}
      ${toolResult(item)}
      ${liveTaskOutput(item, source)}
      ${taskControls(item, panels)}
      ${agentComposer(item, panels)}
    </div>`;
}

/** The agent ids a card's spawn result announced (background agents). */
const AGENT_SPAWN_RE = /agentId:\s*([A-Za-z0-9_-]+)/g;

function spawnedAgentIds(item: ToolItem): string[] {
  if (!item.result) return [];
  return [...contentToText(item.result.content).matchAll(AGENT_SPAWN_RE)].map((m) => m[1]);
}

/**
 * A freeform composer for each live background agent this card spawned,
 * gone once the completion notification lands. Prompt-mediated like the
 * stop control: the send asks the MAIN agent to relay via SendMessage,
 * and the label owns that. Drafts live in the renderer (PanelContext),
 * so the card's per-frame re-renders cannot wipe half-typed text.
 */
function agentComposer(item: ToolItem, panels?: PanelContext): string {
  if (item.notification) return "";
  const ids = spawnedAgentIds(item);
  if (ids.length === 0) return "";
  return ids
    .map((id) => {
      const draft = panels?.drafts?.get(id) ?? "";
      return `<div class="agent-msg"><input class="agent-msg-input" data-msg-for="${escapeHtml(
        id,
      )}" placeholder="message this agent…" value="${escapeHtml(
        draft,
      )}"><button type="button" class="task-msg" data-msg-send="${escapeHtml(
        id,
      )}" title="sends a prompt to the agent">send · asks the agent</button></div>`;
    })
    .join("");
}

/**
 * The streamed output of the detached task this card spawned (the
 * daemon's file tail), in the same capped scrollable box every tool
 * output uses. Kept after the notification lands too — the tail's last
 * catch-up read IS the task's final output.
 */
function taskOutputPre(text: string): string {
  if (text === "") return "";
  return `<pre class="tool-output task-live-output">${escapeHtml(text)}</pre>`;
}

function liveTaskOutput(item: ToolItem, source: AsyncSource | undefined): string {
  // A card with an async source renders its tail inside its own fold (see
  // AsyncFold), so painting it here too would double it.
  return source ? "" : taskOutputPre(item.taskOutput ?? "");
}

/**
 * The context one nesting level deeper: depth ticked and SOURCEID marked
 * seen, so the fold recursing into that source's parsed stream carries
 * the guards `mayNest` reads. Everything else (open state, selections,
 * the poller) rides through unchanged — one renderer state serves every
 * depth.
 */
function descend(panels: PanelContext | undefined, sourceId: string): PanelContext | undefined {
  if (!panels) return undefined;
  return {
    ...panels,
    depth: (panels.depth ?? 0) + 1,
    seenSources: new Set([...(panels.seenSources ?? []), sourceId]),
  };
}

/** A line naming what the cap left out, so a bounded fold never poses as whole. */
function droppedNotice(dropped: number): string {
  if (dropped === 0) return "";
  return `<div class="stream-dropped">… ${dropped} earlier ${
    dropped === 1 ? "entry" : "entries"
  } not shown</div>`;
}

/**
 * A background agent's transcript, rendered as NESTED BUBBLES through the
 * very renderItem the top-level feed uses.
 *
 * This is the payoff of the whole seam. A detached agent is not merely
 * "like" the inline Agent case — it IS that case, differing only in that
 * its stream arrives as a file rather than as parent_tool_use_id-tagged
 * frames. The bytes already reached the browser before this; they were
 * painted as an opaque <pre>.
 */
function transcriptBubbles(text: string, panels?: PanelContext): string {
  const { top, children, dropped } = transcriptFeed(text);
  if (top.length === 0) return "";
  // The transcript's own partition rides a derived context, so a nested
  // card resolves ITS children from the very stream that carries them —
  // a TaskUpdate folds into its create's panel here exactly as at depth
  // one.
  const nested =
    panels && children.size > 0
      ? { ...panels, children: mergeChildren(panels.children, children) }
      : panels;
  return `${droppedNotice(dropped)}${feedChildren(top, nested)}`;
}

/**
 * A workflow run's journal, rendered as ROWS rather than bubbles: it is a
 * record log of the run's agent() calls, not a conversation, and inventing
 * a speaker per record would be a lie about what the data is.
 */
function journalRows(text: string): string {
  const { rows, dropped } = parseJournal(text);
  if (rows.length === 0) return "";
  const body = rows
    .map(
      (r) =>
        `<div class="stream-row"><span class="agent-dot agent-${r.status}" aria-hidden="true">●</span> <span class="tool-name">${escapeHtml(
          r.label,
        )}</span><span class="stream-detail">${escapeHtml(r.detail)}</span></div>`,
    )
    .join("");
  return `${droppedNotice(dropped)}${body}`;
}

/**
 * One source's stream, rendered by the renderer its FORMAT names (§2.6).
 *
 * The single place the generalization lands: adding a newly supported async
 * type is a `format` the daemon already classifies plus an arm here, not a
 * bespoke card. Nothing is forced into a shape its data lacks — a shell's
 * spool is bytes, so it stays a <pre>.
 */
function asyncStreamHtml(item: ToolItem, source: AsyncSource, panels?: PanelContext): string {
  // The ws-streamed tail (shells) wins when present; otherwise the polled
  // tail, which is a background agent's only source since the WS never
  // carried its transcript.
  const polled = panels?.taskTail?.(source.source_id);
  const text = item.taskOutput !== undefined && item.taskOutput !== "" ? item.taskOutput : polled?.text ?? "";
  switch (source.stream?.format) {
    case "jsonl-transcript":
      // Recursing into a parsed stream is the one place depth grows, so
      // the descended context carries the guards a deeper fold reads.
      return transcriptBubbles(text, descend(panels, source.source_id));
    case "jsonl-journal":
      return journalRows(text);
    default:
      return taskOutputPre(text);
  }
}

/** The collapsed face of an async fold: what the work is and whether it runs. */
function asyncFace(source: AsyncSource): string {
  const label = source.label !== undefined && source.label !== "" ? source.label : source.source_id;
  const capped = label.length > 60 ? `${label.slice(0, 59)}…` : label;
  return `${source.kind} · ${capped} · ${source.status}`;
}

/**
 * The fold a card carrying an async source wears: its detached work's
 * stream, behind a click.
 *
 * Built on the same Fold as the activity panel and the watcher fold, with
 * open state in the renderer (openPanels) so it survives the card's
 * per-frame re-renders while its stream grows. Keyed `async:<toolUseId>` so
 * it never collides with the card's own activity panel — a card can have
 * both (an inline subagent that also spawned detached work). SOURCE is the
 * card's effective source (see effectiveAsyncSource), precomputed by
 * ToolCard so the fold and the raw tail can never both render. The
 * `mayNest` guard cuts the fold at the depth cap and on a cycle — a
 * nested spawn announcing an ancestor's own id — and a polled tail that
 * reports done settles the face a synthetic source cannot settle itself.
 */
function AsyncFold(item: ToolItem, source: AsyncSource | undefined, panels?: PanelContext): string {
  if (!source || !panels) return "";
  if (!mayNest(panels.depth ?? 0, panels.seenSources, source.source_id)) return "";
  const id = `async:${item.toolUseId}`;
  const settled = source.status !== "running" || (panels.taskTail?.(source.source_id)?.done ?? false);
  const face = settled && source.status === "running" ? { ...source, status: "done" as const } : source;
  const arc = settled ? "" : `<span class="tool-spinner" aria-hidden="true"></span>`;
  return Fold({
    id,
    foldClass: "async-fold",
    tickerClass: "async-ticker",
    ticker: `${arc}${escapeHtml(asyncFace(face))}`,
    body: () => asyncStreamHtml(item, source, panels),
    open: panels.isOpen(id),
  });
}

/**
 * Whether a watcher has SETTLED — no longer counted live by a fold. It
 * settles when its completion notification lands OR a successful `TaskStop`
 * for one of its task ids has been folded under it. The prompt-mediated stop
 * button never yields a task-notification (daemon liveness is notification-
 * only, see `tailer.go`), so the settled TaskStop card — already parsed and
 * folded under the spawner by `partitionFeed` — is the sole signal that a
 * stop actually took. An in-flight or errored TaskStop does not settle.
 */
function watcherSettled(item: ToolItem, panels?: PanelContext): boolean {
  if (item.notification) return true;
  const ids = new Set(spawnedTaskIds(item));
  return (panels?.children.get(item.toolUseId) ?? []).some(
    (c) =>
      c.kind === "tool" &&
      c.toolName === "TaskStop" &&
      c.result !== undefined &&
      !c.result.isError &&
      typeof c.input?.task_id === "string" &&
      ids.has(c.input.task_id),
  );
}

/**
 * Stop controls for the detached work a card spawned, gone once the task
 * has settled (its notification landed or its TaskStop took). Prompt-mediated
 * on purpose (see Actions.sendPrompt): the button asks the agent to run
 * TaskStop, and its label owns that instead of posing as an instant kill
 * switch.
 */
function taskControls(item: ToolItem, panels?: PanelContext): string {
  const live = watcherSettled(item, panels) ? [] : spawnedTaskIds(item);
  if (live.length === 0) return "";
  const buttons = live
    .map((id) => {
      const prompt = `Stop the background task ${id} (TaskStop), then confirm it stopped.`;
      return `<button type="button" class="task-stop" data-send-prompt="${escapeHtml(
        prompt,
      )}" title="sends a prompt to the agent">stop ${escapeHtml(id)} · asks the agent</button>`;
    })
    .join("");
  return `<div class="task-controls">${buttons}</div>`;
}

/**
 * One watcher's row inside a bubble's fold: its identity and live/settled
 * badge, then the SAME live surface its own tool card carries — the
 * daemon's file tail, the stop control, and the message composer — reused
 * wholesale rather than re-implemented. A watcher is live until it settles
 * — its completion notification lands or a successful TaskStop for its task
 * takes — which is when the arc gives way to a settled badge.
 */
function WatcherRow(item: ToolItem, panels?: PanelContext): string {
  const ids = spawnedTaskIds(item);
  const label = ids.length > 0 ? `${item.toolName} · ${ids.join(", ")}` : item.toolName;
  const headline = toolHeadline(item);
  // The poller's tail for this watcher, if a fold is open and a daemon has
  // answered — this is the ONLY tail a background agent ever has, since the
  // WS never streamed it into item.taskOutput.
  const polled = ids.map((id) => panels?.taskTail?.(id)).find(Boolean);
  const status = watcherSettled(item, panels)
    ? `<span class="badge ok">done</span>`
    : `<span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>running…</span>`;
  // Live elapsed comes from the poll (which the daemon computes from the
  // spawn time); the store's progressElapsedS freezes once the spawning
  // call settles, so it is only the fallback.
  const elapsedMs = polled?.elapsedMs ?? (item.progressElapsedS !== undefined ? item.progressElapsedS * 1000 : undefined);
  const elapsed = elapsedMs !== undefined ? `<span class="watcher-elapsed">${escapeHtml(formatElapsed(elapsedMs))}</span>` : "";
  const desc = headline !== "" ? `<div class="file-path">${escapeHtml(headline)}</div>` : "";
  const progress = item.progress ? `<div class="tool-progress">${escapeHtml(item.progress)}</div>` : "";
  // The same format-driven renderer the card's own fold uses, rather than a
  // second hand-rolled <pre>: a watcher that is a background agent renders
  // its transcript as nested bubbles here too. A watcher the daemon
  // classified no source for falls back to the raw tail it always had.
  const source = effectiveAsyncSource(item);
  const tail = source
    ? asyncStreamHtml(item, source, panels)
    : taskOutputPre(item.taskOutput ?? polled?.text ?? "");
  return `<div class="watcher-row">
      <div class="watcher-head"><span class="tool-name">${escapeHtml(label)}</span>${status}${elapsed}</div>
      ${desc}${progress}${tail}${taskControls(item, panels)}${agentComposer(item, panels)}
    </div>`;
}

/** A member's badge label: its tool name and first announced id, capped. */
function asyncBadgeLabel(item: ToolItem): string {
  const ids = spawnedTaskIds(item);
  const label = ids.length > 0 ? `${item.toolName} · ${ids[0]}` : item.toolName;
  return label.length > 24 ? `${label.slice(0, 23)}…` : label;
}

/**
 * One async member's badge in a bubble's catalog: a clickable amber pill
 * naming the member, its live/settled dot leading. Keyed
 * `member:<host>:<toolUseId>` — per MEMBER, not per bubble — so each badge
 * toggles only its own detail (an open badge renders its `WatcherRow` below
 * the strip), and open state lives in the RENDERER (openPanels) so a badge
 * the user opened survives the bubble's per-frame re-renders. The key never
 * collides with a spawning card's own `async:<toolUseId>` fold.
 *
 * A `<div>`, not a `<button>`, because `data-panel-toggle` is dispatched by
 * `handlePanelToggle`, whose click-through guard (a, button, summary) would
 * otherwise swallow every badge click — the same shape every other fold
 * toggle (activity, watcher, gns, async) already wears.
 */
function AsyncBadge(hostId: string, item: ToolItem, panels?: PanelContext): string {
  const id = `member:${hostId}:${item.toolUseId}`;
  const settled = watcherSettled(item, panels);
  const open = panels?.isOpen(id) ?? false;
  return `<div class="async-badge${settled ? " settled" : ""}${
    open ? " active" : ""
  }" data-panel-toggle="${escapeHtml(id)}"><span class="agent-dot agent-${
    settled ? "done" : "running"
  }" aria-hidden="true">●</span> ${escapeHtml(asyncBadgeLabel(item))}</div>`;
}

/**
 * The in-bubble async catalog (async-quiescence UX): the bubble's live
 * background work, enumerated as selectable amber badges at its BOTTOM, under
 * a prominent bucket-1 `monitoring…` line while any member is live. This is
 * the OTHER half of the invariant the amber border keys off — border and
 * catalog read the SAME projection (`panels.watchers`), so a bubble is amber
 * exactly when it lists live selectable work.
 *
 * Every host bubble carries it — a final response, an interrupted turn's last
 * text, a tools-only turn's prompt — not just a completed answer. A quiesced
 * catalog (all members settled) keeps its badges as history but drops the
 * monitoring line, exactly as the border flips amber → green. Clicking a
 * badge expands that member's `WatcherRow` — its live tail as nested bubbles
 * (asyncStreamHtml), its stop control, its composer — below the strip.
 */
function AsyncCatalog(hostId: string, panels?: PanelContext): string {
  const members = panels?.watchers?.get(hostId) ?? [];
  if (members.length === 0) return "";
  const live = members.some((m) => !watcherSettled(m, panels));
  const badges = members.map((m) => AsyncBadge(hostId, m, panels)).join("");
  const details = members
    .filter((m) => panels?.isOpen(`member:${hostId}:${m.toolUseId}`) ?? false)
    .map((m) => WatcherRow(m, panels))
    .join("");
  return `<div class="async-catalog">${monitoringRowHtml(live)}<div class="async-badges">${badges}</div>${details}</div>`;
}

/**
 * One update's badge tone: `completed` lands the settled ok wash,
 * `deleted` the error red, and anything still moving (`pending`,
 * `in_progress`) the working orange — without the spinner, since the row
 * records a transition that already happened, not live work.
 */
function taskUpdateBadge(status: string): string {
  const tone = status === "completed" ? "ok" : status === "deleted" ? "err" : "run";
  return `<span class="badge ${tone}">${escapeHtml(status.replace("_", " "))}</span>`;
}

/**
 * The gns-sockets fold a final-response bubble carries (see gns.ts): the
 * bridge upkeep that followed the answer — Stop-hook respawn segments and
 * whole bridge-woken turns — behind a click, folded exactly like the
 * watcher panel above it. The body renders the folded items through the
 * same renderItem the top feed uses (an ActivitySection's child-feed
 * treatment), so a folded spawn card keeps its activity panel and a folded
 * result keeps its chip. Keyed `gns:<blockId>` so it never collides with
 * the bubble's watcher fold.
 */
function GnsPanel(blockId: string, panels?: PanelContext): string {
  const items = panels?.gnsFolds?.get(blockId) ?? [];
  if (items.length === 0) return "";
  return Fold({
    id: `gns:${blockId}`,
    foldClass: "gns-fold",
    tickerClass: "gns-ticker",
    ticker: `📡 ${escapeHtml(`gns-sockets bridge · ${activityTicker(items)}`)}`,
    body: () => feedChildren(items, panels),
    open: panels?.isOpen(`gns:${blockId}`) ?? false,
  });
}

function toolInput(item: ToolItem): string {
  // While the input is still streaming, item.input is unparsed and the
  // only material is the accumulating RAW partial JSON — flashing that
  // before the pretty per-tool form renders reads as a glitch. The body
  // stays empty until tool-use-input-end lands: the head's running badge
  // (see ToolCard) is the sole in-progress indicator, so an in-body pulse
  // would only double up on the beat the arc already marks.
  if (!item.inputDone) return "";
  // A subagent card carries NO input section: its description plus the
  // folded call JSON (prompt, model, agent type) was low-level clutter the
  // user never needed, and the agent's identity already reads from the card
  // head and its live topbar roster (the same description, see agents.ts
  // `summary`). The whole clickable description box is dropped here.
  if (SUBAGENT_TOOLS.has(item.toolName)) return "";
  if (item.toolName === "Bash" && item.input && typeof item.input.command === "string") {
    // .bash-input caps the visible command at 5 lines, scrollable
    // independently of the output's own 5-line cap.
    return `<pre class="cmd bash-input">$ ${escapeHtml(item.input.command)}</pre>`;
  }
  if (
    (item.toolName === "Read" || item.toolName === "Write" || item.toolName === "Edit") &&
    item.input &&
    typeof item.input.file_path === "string"
  ) {
    return `<div class="file-path">${escapeHtml(item.input.file_path)}</div>`;
  }
  if (item.toolName === "Grep" && item.input && typeof item.input.pattern === "string") {
    return `<pre class="cmd">grep: ${escapeHtml(item.input.pattern)}</pre>`;
  }
  // SendMessage renders summary-only: the UI-preview summary (plus the
  // recipient), never the full message body.
  if (item.toolName === "SendMessage" && item.input && typeof item.input.summary === "string") {
    const to = typeof item.input.to === "string" ? `→ ${item.input.to}: ` : "";
    return `<div class="file-path">${escapeHtml(`${to}${item.input.summary}`)}</div>`;
  }
  // A task-list update reads as its transition: the status it moved the
  // task to (badged) and the subject it (re)named. Nested in its task's
  // create-card panel by the partition, the line IS the history bubble. An
  // update carrying neither field falls through to the generic JSON fold.
  if (item.toolName === "TaskUpdate" && item.input) {
    const status = typeof item.input.status === "string" ? item.input.status : "";
    const subject = typeof item.input.subject === "string" ? item.input.subject : "";
    if (status !== "" || subject !== "") {
      const badge = status !== "" ? taskUpdateBadge(status) : "";
      const line = subject !== "" ? ` ${escapeHtml(subject)}` : "";
      return `<div class="file-path">${badge}${line}</div>`;
    }
  }
  // Skill's input section IS its invocation: the skill name and, when the
  // call carried them, the args it was handed — the "/skill args" a user
  // would type. Capped and click-expandable like a Bash command, since the
  // args can run long. The skill's full SKILL.md body is the separate
  // content section the result renders (see toolResult).
  if (item.toolName === "Skill" && item.input && typeof item.input.skill === "string") {
    const args =
      typeof item.input.args === "string" && item.input.args !== "" ? ` ${item.input.args}` : "";
    return `<pre class="cmd skill-input">/${escapeHtml(item.input.skill)}${escapeHtml(args)}</pre>`;
  }
  // Any other tool folds to a headline when its input offers one: the
  // interesting field on a line, the full JSON one click away (the same
  // fold the subagent card wears). No headline keeps the raw capped JSON.
  const headline = toolHeadline(item);
  if (headline !== "") {
    return `<div class="tool-input folded-input"><div class="file-path folded-headline">${escapeHtml(
      headline,
    )}</div><pre class="folded-json">${escapeHtml(item.inputJson)}</pre></div>`;
  }
  return `<pre class="tool-input">${escapeHtml(item.inputJson)}</pre>`;
}

function toolResult(item: ToolItem): string {
  if (!item.result) return "";
  const r = item.result.render;
  if (r) {
    switch (r.kind) {
      case "bash":
        // .bash-output caps the visible output at 5 lines, scrollable
        // independently of the command's own 5-line cap.
        return `<pre class="tool-output bash-output">${escapeHtml(r.stdout)}${
          r.stderr ? `\n<span class="stderr">${escapeHtml(r.stderr)}</span>` : ""
        }</pre>`;
      case "diff":
        // .diff-output caps the visible diff at 10 lines (the Read
        // preview's cap), scrollable for the rest.
        return `<pre class="diff diff-output">${diffHtml(r.unified_diff)}</pre>`;
      case "grep":
        return `<pre class="tool-output">${r.matches
          .map((m) => `${escapeHtml(m.file)}:${m.line}: ${escapeHtml(m.text)}`)
          .join("\n")}</pre>`;
      case "task-update":
        // The transition itself is the TaskCreate card's stream, folded in
        // there; on the TaskUpdate's own card it would be noise, and that
        // card is suppressed anyway (SUPPRESSED_TOOLS).
        return "";
      case "skill":
        // The launched skill's full SKILL.md body, the content section
        // paired with the invocation the input renders. Capped and
        // click-expandable like a Bash output. A skill IS a markdown file,
        // so its body renders formatted like a markdown Read preview
        // (readResultHtml's .tool-read-md) rather than escaped plain text.
        return `<div class="tool-output skill-content skill-content-md">${renderMarkdown(
          r.content,
        )}</div>`;
    }
  }
  if (item.toolName === "Read" && !item.result.isError) {
    return readResultHtml(item, contentToText(item.result.content));
  }
  // SendMessage is summary-only: the successful delivery echo adds
  // nothing over the summary line. Errors still fall through below so
  // failures stay loud.
  if (item.toolName === "SendMessage" && !item.result.isError) {
    return "";
  }
  // A TaskUpdate's success echo ("Task #1 updated successfully") adds
  // nothing over the transition line its input renders. Errors fall
  // through below, so an update that never applied stays loud.
  if (item.toolName === "TaskUpdate" && !item.result.isError) {
    return "";
  }
  // A Skill result with a skill render hint rendered its SKILL.md body as
  // the content section above. Without one — a skill the daemon could not
  // resolve to a SKILL.md (a plugin skill, a missing file) — the raw
  // "Launching skill: <name>" echo adds nothing over the invocation the
  // input already shows, so it is suppressed. Errors fall through below so
  // a skill that failed to launch stays loud.
  if (item.toolName === "Skill" && !item.result.isError) {
    return "";
  }
  // A ScheduleWakeup's result is a countdown anchor: the card names the
  // wall-clock moment the wakeup fires (result time plus the delay)
  // rather than echoing the scheduler's ack. Errors fall through loud.
  if (item.toolName === "ScheduleWakeup" && !item.result.isError) {
    return wakeLine(item);
  }
  return `<pre class="tool-output${item.result.isError ? " stderr" : ""}">${escapeHtml(
    contentToText(item.result.content),
  )}</pre>`;
}

/**
 * The live countdown suffix of a wake anchor: whole minutes down to the
 * last minute and a half, then seconds, then "firing…" once the moment
 * passes — the periodic re-render in main.ts is what makes it tick.
 */
export function wakeRemainingLabel(fireMs: number, nowMs: number): string {
  const remaining = (fireMs - nowMs) / 1000;
  if (remaining <= 0) return "firing…";
  if (remaining < 90) return `in ${Math.round(remaining)}s`;
  return `in ${Math.round(remaining / 60)}m`;
}

/**
 * The wall-clock moment a scheduled wakeup fires (result time plus
 * delaySeconds) with a ticking countdown and its reason, a stop
 * acknowledgment for `stop: true`, and the raw ack echo when the input
 * carries neither anchor.
 */
function wakeLine(item: ToolItem): string {
  if (item.input?.stop === true) {
    return `<div class="wake-at">loop stopped</div>`;
  }
  const delay = typeof item.input?.delaySeconds === "number" ? item.input.delaySeconds : null;
  if (delay === null || item.resultTs === undefined) {
    return `<pre class="tool-output">${escapeHtml(
      contentToText(item.result?.content ?? ""),
    )}</pre>`;
  }
  const fireMs = new Date(item.resultTs).getTime() + delay * 1000;
  const at = new Date(fireMs);
  const reason = typeof item.input?.reason === "string" ? ` · ${item.input.reason}` : "";
  return `<div class="wake-at">wakes ~${escapeHtml(formatTurnTime(at.toISOString()))} (${escapeHtml(
    wakeRemainingLabel(fireMs, Date.now()),
  )})${escapeHtml(reason)}</div>`;
}

/**
 * Read result preview. cat -n numbered output has each line's number
 * prefix lifted into a muted .line-no span so only the code text is
 * syntax-highlighted (language from the file_path extension, plain
 * escaped text when it is unknown); non-numbered content is
 * highlighted whole. Markdown files render as formatted markdown
 * instead of highlighted source — the number gutter is dropped there,
 * since rendered blocks have no line correspondence. In every form
 * .tool-read-output caps the visible height — the rest stays
 * reachable by scrolling.
 */
function readResultHtml(item: ToolItem, text: string): string {
  const path =
    item.input && typeof item.input.file_path === "string" ? item.input.file_path : "";
  const lang = languageForPath(path) ?? "";
  const lines = text.split("\n");
  const parts = lines.map((l) => l.match(/^(\s*\d+\t)(.*)$/));
  // Numbered when every line carries the cat -n prefix (a blank
  // trailing line from a final newline doesn't break the format).
  const numbered =
    parts.some((p) => p !== null) && parts.every((p, i) => p !== null || lines[i] === "");
  if (lang === "markdown") {
    const md = numbered ? parts.map((p, i) => (p ? p[2] : lines[i])).join("\n") : text;
    return `<div class="tool-output tool-read-output tool-read-md">${renderMarkdown(md)}</div>`;
  }
  if (!numbered) {
    return `<pre class="tool-output tool-read-output"><code class="hljs">${highlightCode(text, lang)}</code></pre>`;
  }
  const code = parts.map((p, i) => (p ? p[2] : lines[i])).join("\n");
  // hljs preserves newlines, so the highlighted HTML splits back into
  // the same number of lines; if that invariant ever breaks, keep the
  // prefixes but fall back to unhighlighted code.
  let codeLines = highlightCode(code, lang).split("\n");
  if (codeLines.length !== lines.length) {
    codeLines = parts.map((p, i) => escapeHtml(p ? p[2] : lines[i]));
  }
  const body = codeLines
    .map((h, i) => {
      const p = parts[i];
      return p ? `<span class="line-no">${escapeHtml(p[1])}</span>${h}` : h;
    })
    .join("\n");
  return `<pre class="tool-output tool-read-output"><code class="hljs">${body}</code></pre>`;
}

export function diffHtml(unifiedDiff: string): string {
  return unifiedDiff
    .split("\n")
    .map((line) => {
      const esc = escapeHtml(line);
      if (line.startsWith("+")) return `<span class="add">${esc}</span>`;
      if (line.startsWith("-")) return `<span class="del">${esc}</span>`;
      if (line.startsWith("@@")) return `<span class="hunk">${esc}</span>`;
      return esc;
    })
    .join("\n");
}

function PermissionPrompt(item: PermissionItem, selections?: QuestionSelections): string {
  const questions = askQuestions(item);
  if (questions) return QuestionPrompt(item, questions, selections);
  const preview = permissionPreviewHtml(item);
  if (item.resolution) {
    const label =
      item.resolution.decision === "cancel"
        ? `cancelled${item.resolution.message ? ` — ${escapeHtml(item.resolution.message)}` : ""}`
        : `${item.resolution.decision}ed`;
    return `
      <div class="permission resolved">
        <div class="perm-head">Permission: ${escapeHtml(item.toolName)} <span class="badge ${
          item.resolution.decision === "allow" ? "ok" : "err"
        }">${label}</span></div>
        ${preview}
      </div>`;
  }
  return `
    <div class="permission pending">
      <div class="perm-head">Allow ${escapeHtml(item.toolName)}?</div>
      ${preview}
      <div class="perm-actions">
        <button data-perm-allow="${escapeHtml(item.requestId)}">Allow</button>
        <button data-perm-deny="${escapeHtml(item.requestId)}">Deny</button>
      </div>
    </div>`;
}

/**
 * AskUserQuestion picker: option chips per question (single-select
 * replaces the pick, multiSelect toggles), a submit enabled once every
 * question has a selection, and a decline path. Selection state lives
 * in the RENDERER, not the DOM, so it survives per-delta re-renders.
 *
 * Question text and option labels render inline markdown (via `inline`)
 * so backtick-wrapped code and other inline markup read the same as they
 * do in message bubbles; the raw label still keys selection and answers,
 * so this is display-only. The option description stays plain-escaped
 * because it lives in a `title` tooltip, which cannot carry markup.
 */
function QuestionPrompt(
  item: PermissionItem,
  questions: AskQuestion[],
  selections?: QuestionSelections,
): string {
  if (item.resolution) {
    const label =
      item.resolution.decision === "allow"
        ? "answered"
        : item.resolution.decision === "cancel"
          ? "cancelled"
          : "declined";
    return `
      <div class="permission resolved question">
        <div class="perm-head">Question <span class="badge ${
          item.resolution.decision === "allow" ? "ok" : "err"
        }">${label}</span></div>
        ${questions.map((q) => `<div class="q-text">${inline(escapeHtml(q.question))}</div>`).join("")}
      </div>`;
  }
  const rid = escapeHtml(item.requestId);
  const blocks = questions.map((q, qi) => {
    const picked = selections?.get(`${item.requestId} ${qi}`);
    const opts = q.options
      .map(
        (o, oi) =>
          `<button class="q-opt${picked?.has(o.label) ? " selected" : ""}" data-q-req="${rid}" data-q-idx="${qi}" data-q-opt="${oi}" title="${escapeHtml(
            o.description,
          )}">${inline(escapeHtml(o.label))}</button>`,
      )
      .join("");
    return `
      <div class="q-block">
        <span class="badge q-chip">${escapeHtml(q.header)}</span>
        <div class="q-text">${inline(escapeHtml(q.question))}${
          q.multiSelect ? ` <span class="q-multi">(select all that apply)</span>` : ""
        }</div>
        <div class="q-opts">${opts}</div>
      </div>`;
  });
  const complete = questions.every(
    (_q, qi) => (selections?.get(`${item.requestId} ${qi}`)?.size ?? 0) > 0,
  );
  return `
    <div class="permission pending question">
      ${blocks.join("")}
      <div class="perm-actions">
        <button data-q-submit="${rid}"${complete ? "" : " disabled"}>Answer</button>
        <button data-perm-deny="${rid}">Decline</button>
      </div>
    </div>`;
}

/**
 * The card for a turn that ended in the CLI refusing a slash command this
 * environment cannot run. The refusal itself is dead weight, so the card's
 * point is the button: it asks Emacs to open a workspace that builds the
 * feature properly (see unsupported.ts).
 *
 * With no `addSupport` action wired there is no daemon to ask, so the card
 * states the refusal and offers nothing rather than dangling a dead button.
 */
function UnsupportedCommandCard(
  command: string,
  phase: SupportPhase | undefined,
  offerable: boolean,
): string {
  const cmd = escapeHtml(command);
  const head = `<div class="perm-head">Unsupported <span class="badge err">/${cmd}</span></div>`;
  const why = `<div class="q-text">This environment can't run <code>/${cmd}</code>, so the CLI refused it.</div>`;
  if (phase?.kind === "asked") {
    return `
      <div class="permission resolved unsupported">
        <div class="perm-head">Unsupported <span class="badge ok">workspace requested</span></div>
        <div class="q-text">Emacs was asked to open <code>${escapeHtml(phase.workspace)}</code> to add support for <code>/${cmd}</code>.</div>
      </div>`;
  }
  const failed =
    phase?.kind === "failed"
      ? `<div class="q-text unsupported-err">Asking failed: ${escapeHtml(phase.error)}</div>`
      : "";
  if (!offerable) {
    return `<div class="permission resolved unsupported">${head}${why}</div>`;
  }
  const asking = phase?.kind === "asking";
  return `
    <div class="permission pending unsupported">
      ${head}${why}${failed}
      <div class="perm-actions">
        <button data-add-support="${cmd}"${asking ? " disabled" : ""}>${
          asking ? "Asking Emacs…" : "Create workspace to add support"
        }</button>
      </div>
    </div>`;
}

function permissionPreviewHtml(item: PermissionItem): string {
  const p = item.preview;
  if (!p) return "";
  switch (p.kind) {
    case "bash":
      return `<pre class="cmd">$ ${escapeHtml(p.command)}</pre>`;
    case "diff":
      return `<div class="file-path">${escapeHtml(p.file_path)}</div><pre class="diff">${diffHtml(p.unified_diff)}</pre>`;
    case "write":
      return `<div class="file-path">${escapeHtml(p.file_path)} (${p.bytes} bytes)</div><pre class="tool-output">${escapeHtml(p.preview)}</pre>`;
    case "generic":
      return `<pre class="tool-output">${escapeHtml(p.summary)}</pre>`;
  }
}

/**
 * Whether a result ends its turn normally: an aborted or errored turn was
 * cut off partway, so it never reached the answer it was working toward.
 */
function isTurnComplete(item: ResultItem): boolean {
  return item.subtype === "success";
}

/**
 * The feed's final responses, seen from both ends of the pairing they
 * create between a turn's answer and the chip that closes the turn.
 */
export interface FinalResponses {
  /** Final-response block id → the closing chip that bubble swallows. */
  readonly chips: ReadonlyMap<string, ResultItem>;
  /** The results a bubble swallowed, which the feed no longer prints itself. */
  readonly swallowed: ReadonlySet<ResultItem>;
}

/**
 * The text bubbles that CONCLUDE a completed turn — the agent's final
 * response to a prompt, as against the commentary it emits between tool
 * calls — each paired with the `result` that closes its turn. A turn's
 * final response is its last text block before the `result`, and only a
 * turn that ran to completion has one: an aborted or errored turn's last
 * text is a severed thought, not an answer. A turn still streaming has no
 * final response either, since its next block could always continue it.
 *
 * The pairing is what nests a completed turn's chip inside its answer.
 * A completed turn that wrote no answer to nest it in (one that only ran
 * tools) pairs with nothing, so its chip keeps printing standalone.
 *
 * Main-chain text only: a subagent's parented prose is commentary inside
 * its card's panel, never the turn's answer, so it can neither win the
 * pairing nor break an earlier main-chain candidate's claim.
 */
export function finalResponses(items: readonly ConversationItem[]): FinalResponses {
  const chips = new Map<string, ResultItem>();
  const swallowed = new Set<ResultItem>();
  let lastText: string | null = null;
  for (const item of items) {
    if (item.kind === "user-turn") {
      lastText = null;
    } else if (item.kind === "text" && item.parentToolUseId === undefined) {
      lastText = item.blockId;
    } else if (item.kind === "result") {
      if (lastText !== null && isTurnComplete(item)) {
        chips.set(lastText, item);
        swallowed.add(item);
      }
      lastText = null;
    }
  }
  return { chips, swallowed };
}

/**
 * Whether an item draws NOTHING into the feed.
 *
 * The renderer and the pulse both need this same answer, from opposite ends:
 * `renderItem` returns the empty node, and `pulseTarget` scans straight past
 * it, since a thing the feed never drew cannot be the progress that
 * supersedes a breathing bubble. Keeping the two on one predicate is what
 * stops a suppressed tool from silently stilling a pulse it never replaced.
 *
 * FINALS is needed only for a result, whose chip rides inside the response
 * bubble that swallowed it rather than standing alone in the feed.
 */
export function rendersEmpty(
  item: ConversationItem,
  finals?: FinalResponses,
): boolean {
  switch (item.kind) {
    // A turn that was nothing BUT injected spans has no bubble at all.
    case "user-turn":
      return userTurnText(item) === "";
    // A textless thinking block leaves nothing behind once it closes.
    case "thinking":
      return item.done && item.text === "";
    // The CLI's empty-message placeholder (`EMPTY_CONTENT_PLACEHOLDER`) is
    // no answer at all: a `/clear`'s contextless reply is the common one,
    // and a green `(no content)` bubble under the divider is exactly what
    // the clear should have left no room for.
    //
    // A body that is EMPTY OR WHITESPACE-ONLY is undrawn on the same terms:
    // the `Bubble` shell would otherwise render nothing but its `.turn-ts`
    // stamp, so a text block that opened but has not streamed yet (the
    // `text-start`/first-`text-delta` gap) or one that closed on blank
    // `final_text` would leave a lone timestamp floating with no visible
    // bubble around it.
    case "text": {
      const body = item.text.trim();
      return body === "" || body === EMPTY_CONTENT_PLACEHOLDER;
    }
    // AskUserQuestion's UI IS the permission picker card — the generic tool
    // card would just dump the questions JSON (input) and the "User has
    // answered…" echo (result) alongside it. ToolSearch is deferred-tool
    // schema plumbing: pure feed noise. A TaskUpdate draws nothing until
    // its input closes — before that the partition cannot know which
    // create card owns it, and a card that flashed top-level then jumped
    // into a panel would read as a glitch.
    case "tool":
      if (item.toolName === "TaskUpdate" && !item.inputDone) return true;
      return SUPPRESSED_TOOLS.has(item.toolName);
    case "result":
      // A refused slash command's card is the turn's ONLY content, so it
      // is never swallowed into a final-response bubble. Swallowing it
      // would silently drop the button along with the refusal.
      if (parseUnsupportedCommand(item.resultText) !== null) return false;
      return finals?.swallowed.has(item) ?? false;
    // The SDK's session (re)init announces itself with no user-facing content:
    // the breathing prompt bubble is the "received, working" signal now, so
    // the textual `system: init` note is dropped from the feed entirely.
    case "system":
      return item.subtype === "init";
    default:
      return false;
  }
}

/**
 * What the feed's tail is doing, or null when a live channel already speaks
 * for it. Exactly one:
 * - `text` — the WORKING FRONTIER breathes (the last finished response of a
 *   still-running turn), a locational accent on the bubble the reader is in.
 * - `working` — the main chain is in flight through a dead-air gap, so the
 *   orange `working…` tail row stands in for the silent feed.
 * - `retrying` — the SDK is auto-retrying a failed request, so the purple
 *   `retrying…` tail row supersedes the working one.
 * - `null` — a streaming cursor, a live thinking spinner, a running-tool arc,
 *   or a pending permission is itself the live signal, so nothing extra shows.
 */
export type PulseTarget =
  | { kind: "text"; blockId: string }
  | { kind: "working" }
  | { kind: "retrying" }
  | null;

/**
 * The single source of truth for what the feed's tail is doing, so a running
 * session always shows one live signal somewhere at its tail. Scans tail-first
 * and returns the FIRST thing that speaks for the turn:
 * - the WORKING FRONTIER (the last response a still-running turn has finished)
 *   breathes once the agent has written something, so a reader who has caught
 *   up sees it is still writing rather than that it went quiet (`text`).
 * - a `retry` at the very tail means the SDK is auto-retrying, so the purple
 *   `retrying…` row supersedes the working one (`retrying`).
 * - reaching the prompt itself (the turn has drawn no visible content yet)
 *   means the chain is in a dead-air gap, so the orange `working…` row stands
 *   in — the PROMPT no longer breathes, the row replaces it (`working`).
 *
 * The states that yield null, each because something else already carries the
 * beat (or because there is no beat to carry):
 * - the turn is not in flight, so nothing more is coming at all;
 * - a response is streaming, and its own cursor is the live signal;
 * - a thinking indicator is running, and its spinner is the live signal;
 * - a tool call is running at the tail, and its own run badge (the arc in
 *   `.badge.run`) is the live signal, so the feed shows brief dead air;
 * - a pending permission is at the tail, and the agent is blocked on the USER
 *   rather than working, so its card is the signal and nothing breathes.
 *
 * A settled section (a done tool, a resolved permission, a recoverable error)
 * followed the prompt before the agent wrote a word: it falls through to the
 * `working` state, since the chain is between frames, not blocked. The scan
 * stops at the newest user turn: the previous turn's answer belongs to a
 * question already answered, so a fresh prompt never breathes life back into
 * it. What the feed never drew (`rendersEmpty`) is skipped outright — a pulse
 * is stilled only by progress the user can actually see — and so are a
 * subagent's parented blocks: their prose lives in its card's panel, so it
 * neither carries the main feed's pulse nor silences the main-chain frontier
 * behind it.
 */
export function pulseTarget(
  items: readonly ConversationItem[],
  turnInFlight: boolean,
  finals?: FinalResponses,
): PulseTarget {
  if (!turnInFlight) return null;
  // Whether the cursor is still at the feed's visible tail, i.e. nothing
  // drawn has been passed on the way back to the item under inspection.
  let atTail = true;
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (rendersEmpty(item, finals)) continue;
    // A subagent's parented blocks live in its card's panel: they neither
    // carry the main feed's pulse nor count as visible progress stilling it.
    if (
      (item.kind === "text" || item.kind === "thinking" || item.kind === "tool") &&
      item.parentToolUseId !== undefined
    ) {
      continue;
    }
    // The prompt no longer breathes: a turn that has drawn no visible content
    // is simply WORKING, and the orange tail row speaks for it now.
    if (item.kind === "user-turn") return { kind: "working" };
    if (item.kind === "thinking" && !item.done) return null;
    if (item.kind === "text") {
      return item.done ? { kind: "text", blockId: item.blockId } : null;
    }
    // A running tool card at the tail no longer takes the beat: its own run
    // badge (the arc spinning in `.badge.run`) is the live signal, so the feed
    // shows brief dead air rather than a second beat competing with it. A
    // settled card falls through, letting the finished response above it
    // breathe as the working frontier.
    if (item.kind === "tool" && atTail && !item.result) return null;
    // Two tail-only signals, checked before the settled fall-through: a retry
    // at the very tail IS the auto-retry window, so the purple row supersedes
    // working; a pending permission at the tail blocks the turn on the user,
    // so nothing breathes beneath its card. A recoverable error is neither —
    // it falls through to `working`, since the chain is about to continue.
    if (atTail) {
      if (item.kind === "retry") return { kind: "retrying" };
      if (item.kind === "permission" && !item.resolution) return null;
    }
    atTail = false;
  }
  return null;
}

/** Whether PULSE names this item as the bubble that breathes (only `text` does). */
export function isPulsed(item: ConversationItem, pulse: PulseTarget): boolean {
  if (!pulse || pulse.kind !== "text") return false;
  return item.kind === "text" && item.blockId === pulse.blockId;
}

/**
 * The single force-appended tail status row to show, or null when none. This
 * is the one home of the tail-row precedence — interrupting (red) > compacting
 * > retrying (purple) > working (orange) — shared by the incremental render
 * and the fresh-join restore so the two never drift. The compaction banner is
 * the topbar's own element (see main.ts), so a compaction shows NO feed-tail
 * row; it only suppresses the two pulse-driven rows beneath it. The `text`
 * pulse is a bubble breath, not a tail row, so it never reaches here.
 */
export function tailStatusRow(
  interrupting: boolean,
  compacting: boolean,
  pulse: PulseTarget,
): { key: string; html: string } | null {
  if (interrupting) {
    return { key: "interrupting", html: interruptingIndicatorHtml(true) };
  }
  if (compacting) return null;
  if (pulse?.kind === "retrying") {
    return { key: "retrying", html: retryingRowHtml(true) };
  }
  if (pulse?.kind === "working") {
    return { key: "working", html: workingRowHtml(true) };
  }
  return null;
}

/** A context increase as a signed figure: `+100,000`, `-40,000`, `+0`. */
function formatTokenDelta(n: number): string {
  return `${n < 0 ? "" : "+"}${formatTokens(n)}`;
}

/**
 * A final response's top-right corner content: the turn's elapsed time in
 * the topbar's time color, then the context delta it moved in the topbar's
 * token color, bullet-separated (`5s · +12,000`). The context total itself
 * is NOT here — it stands in the header's tokens chip now, and repeating it
 * on every bubble was the noise this move removed.
 *
 * Either half may be absent: a sub-second turn reports no elapsed worth a
 * figure (the same one-second floor the old closing chip used), and a turn
 * that ended with the context size unknown (a `/clear` or a compaction)
 * carries no delta. An all-absent corner returns "" and the bubble shows
 * only its hover timestamp.
 */
function resultMeta(chip: ResultItem): string {
  const parts: string[] = [];
  if (chip.sincePrevFinalMs >= 1000) {
    parts.push(
      `<span class="turn-dur">${escapeHtml(formatDurationCeil(chip.sincePrevFinalMs))}</span>`,
    );
  }
  if (chip.context) {
    parts.push(
      `<span class="turn-diff">${escapeHtml(formatTokenDelta(chip.context.delta))}</span>`,
    );
  }
  return parts.join(" · ");
}

/**
 * A turn's closing chip: how long the turn took, then where the session's
 * input tokens now stand and how far this turn moved them.
 *
 * DURATION-MS is the span the chip reports, which differs by where the chip
 * is drawn: a final-response chip reads the turn's elapsed time SINCE THE
 * PREVIOUS final response (`sincePrevFinalMs`), while a standalone chip
 * reads the SDK's whole-task figure (`durationMs`). The caller passes
 * whichever it means, so this renderer never has to know which it is.
 *
 * A completed turn is labelled by the chip's own wash rather than by a
 * word, so only a turn that ended some OTHER way (aborted, errored) names
 * its subtype. A turn that ended with the context size unknown (a
 * `/clear` or a `/compact`) reports the duration alone, since the figure
 * it would otherwise print is the one the turn just invalidated.
 *
 * A completed turn's chip is drawn INSIDE the final response it closes
 * (see `TextStream`), so the feed prints the chip standalone only when no
 * bubble swallowed it: an aborted or errored turn, or a completed turn
 * that wrote no answer at all.
 *
 * SECOND-RESOLUTION rounds the duration up to whole seconds (`6s`, not
 * `5s 984ms`): the nested final-response badge reports a settled turn to a
 * reader who never needs the millisecond digit, whereas the standalone chip
 * keeps the SDK's exact figure so a sub-second abort still reads its span.
 */
function ResultChip(
  item: ResultItem,
  durationMs: number,
  secondResolution = false,
): string {
  const done = isTurnComplete(item);
  // A turn cut off by an interrupt is not a failure — the user chose to stop
  // it — so the "aborted" subtype (the shim's word for an interrupt-ended
  // turn) names itself "interrupted" in the yellow `.interrupted` tone rather
  // than reading the SDK's error red the way a genuine error subtype does.
  // The tone wins over `item.isError`, which the SDK still sets on an abort.
  const interrupted = item.subtype === "aborted";
  const tone = interrupted ? "interrupted" : item.isError ? "err" : "ok";
  const label = interrupted ? "interrupted" : escapeHtml(item.subtype);
  const parts = done ? [] : [label];
  parts.push(
    secondResolution ? formatDurationCeil(durationMs) : formatDuration(durationMs),
  );
  if (item.context) {
    parts.push(formatTokenDelta(item.context.delta));
  }
  return `
    <div class="result ${tone}${done ? " done" : ""}">
      ${parts.join(" · ")}
    </div>`;
}

function CompactDivider(item: CompactBoundaryItem): string {
  return `<div class="compact-divider">— context compacted (${escapeHtml(item.trigger)}, ${item.preTokens} tokens before) —</div>`;
}

function ErrorBanner(item: ErrorItem): string {
  return `<div class="error-banner">[${escapeHtml(item.code)}] ${escapeHtml(item.message)}${
    item.recoverable ? "" : " (fatal)"
  }</div>`;
}

function RetryBadge(item: RetryItem): string {
  return `<div class="retry-badge">retrying (attempt ${item.attempt}): ${escapeHtml(item.reason)}</div>`;
}

function SystemNote(item: SystemItem): string {
  return `<div class="system-note">system: ${escapeHtml(item.subtype)}</div>`;
}

/**
 * One item's HTML, or nothing at all for the items the feed draws no node
 * for (`rendersEmpty`). FINALS pairs the feed's answers with the chips that
 * close their turns: a text block it names renders as a final response
 * carrying its chip, and the paired result renders as nothing, since the
 * bubble above it has already drawn it.
 *
 * IS-PULSING marks the one section that breathes (`pulseTarget`), which is
 * the prompt or a finished response. PANELS carries
 * the feed partition's child lists, letting a spawning card fold its
 * confined children into an activity panel (and letting those children
 * recurse through this same function).
 */
export function renderItem(
  item: ConversationItem,
  selections?: QuestionSelections,
  finals?: FinalResponses,
  isPulsing = false,
  panels?: PanelContext,
): string {
  if (rendersEmpty(item, finals)) return "";
  switch (item.kind) {
    case "user-turn":
      return UserTurn(item, panels);
    case "text":
      return TextStream(item, finals?.chips.get(item.blockId) ?? null, isPulsing, panels);
    case "thinking":
      return Thinking(item);
    case "tool":
      return SUPPRESSED_TOOLS.has(item.toolName) ? "" : ToolCard(item, panels);
    case "permission":
      return PermissionPrompt(item, selections);
    case "result": {
      // A refused slash command's turn carries no answer and a meaningless
      // near-zero duration, so its card replaces the chip outright.
      const unsupported = parseUnsupportedCommand(item.resultText);
      if (unsupported !== null) {
        return UnsupportedCommandCard(
          unsupported,
          panels?.supportPhases?.get(unsupported),
          panels?.canAddSupport ?? false,
        );
      }
      return ResultChip(item, item.durationMs);
    }
    case "compact-boundary":
      return CompactDivider(item);
    case "error":
      return ErrorBanner(item);
    case "retry":
      return RetryBadge(item);
    case "system":
      return SystemNote(item);
  }
}

/** Key identifying one item's DOM node across renders. */
export function itemKey(item: ConversationItem, index: number): string {
  switch (item.kind) {
    case "text":
    case "thinking":
      return `${item.kind}:${item.blockId}`;
    case "tool":
      return `tool:${item.toolUseId}`;
    case "permission":
      return `perm:${item.requestId}`;
    default:
      return `${item.kind}:${index}`;
  }
}

/**
 * Request id of the newest user turn in the feed, or null when there is
 * none. A change in this id between renders means a prompt was just
 * sent — from the webapp composer or from the Emacs host's input buffer
 * alike, since both reach the feed as the daemon's `user-turn`
 * broadcast — which is what re-pins a scrolled-up feed to its tail.
 */
export function lastUserTurnId(items: readonly ConversationItem[]): string | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn") return item.requestId;
  }
  return null;
}

// --- consecutive-run tab groups -------------------------------------------------

/** One slot of the grouped feed: a lone item or a consecutive-run group. */
export type FeedEntry =
  | { kind: "item"; item: ConversationItem; index: number }
  | { kind: "group"; members: ToolItem[]; indexes: number[] };

/**
 * Collapse consecutive same-tool cards into tab groups: three Bash calls
 * in a row become one group of three, selectable by tabs, while a
 * singleton stays a plain card. Any VISIBLE item of another shape breaks
 * a run; items that render nothing (`rendersEmpty`) are held aside and
 * re-emitted after the group (their position among empty nodes is moot).
 */
export function groupFeed(top: readonly ConversationItem[]): FeedEntry[] {
  const entries: FeedEntry[] = [];
  let run: { name: string; members: ToolItem[]; indexes: number[] } | null = null;
  let held: Array<{ item: ConversationItem; index: number }> = [];

  const closeRun = (): void => {
    if (run) {
      if (run.members.length >= 2) {
        entries.push({ kind: "group", members: run.members, indexes: run.indexes });
      } else {
        entries.push({ kind: "item", item: run.members[0], index: run.indexes[0] });
      }
      run = null;
    }
    for (const h of held) entries.push({ kind: "item", item: h.item, index: h.index });
    held = [];
  };

  top.forEach((item, index) => {
    if (rendersEmpty(item)) {
      if (run) {
        held.push({ item, index });
      } else {
        entries.push({ kind: "item", item, index });
      }
      return;
    }
    if (item.kind === "tool") {
      if (run && run.name === item.toolName) {
        run.members.push(item);
        run.indexes.push(index);
        return;
      }
      closeRun();
      run = { name: item.toolName, members: [item], indexes: [index] };
      return;
    }
    closeRun();
    entries.push({ kind: "item", item, index });
  });
  closeRun();
  return entries;
}

/**
 * The member a group shows: the user's pin when it still names a member,
 * else the newest still-running member (auto-follow), else the last.
 */
export function activeGroupMember(members: readonly ToolItem[], pinned?: string): string {
  if (pinned !== undefined && members.some((m) => m.toolUseId === pinned)) return pinned;
  for (let i = members.length - 1; i >= 0; i--) {
    if (!members[i].result) return members[i].toolUseId;
  }
  return members[members.length - 1].toolUseId;
}

/**
 * How the renderer surfaces a tool call's card and where the feed then
 * jumps: `planToolReveal` derives it purely; `FeedRenderer.revealToolCard`
 * applies it.
 */
export interface ToolReveal {
  /** DOM key of the top-level feed-item carrying (or nesting) the call. */
  key: string;
  /** When that feed-item is a consecutive-run tab group, its active-tab key. */
  groupKey: string | null;
  /** The top-level member to pin as that group's active tab, else null. */
  tabMember: string | null;
  /** Tool ids whose activity panels open so the call and its output show. */
  panelIds: string[];
}

/**
 * Plan how to reveal tool call TOOLUSEID's card — a roster entry's bubble:
 * a subagent's card, or a task's `TaskCreate` card — from the topbar
 * dropdowns: which top-level feed-item to scroll to, whether to pin a tab
 * to surface it, and which activity panels to open so its card (and, for
 * a nested call, every panel above it) lays its output out.
 *
 * A top-level call reveals as its own bubble with its own panel open.
 * A nested call (one a subagent made) has no top-level bubble, so the
 * plan scrolls to its outermost ancestor and opens the whole panel chain
 * down to it. Null when no live feed item carries the id — unknown, or
 * discarded by a `/clear`.
 */
export function planToolReveal(
  items: readonly ConversationItem[],
  toolUseId: string,
): ToolReveal | null {
  const visible = itemsFromLastClear(items);
  const parentOf = new Map<string, string | undefined>();
  for (const item of visible) {
    if (item.kind === "tool") parentOf.set(item.toolUseId, item.parentToolUseId);
  }
  if (!parentOf.has(toolUseId)) return null;
  // Ancestor chain from the call up to its outermost tool ancestor. It is
  // exactly the set of panels to open: each ancestor's panel holds the next
  // card down, and the call's own panel holds its output. A cycle guard
  // keeps a corrupt parent link from looping.
  const chain: string[] = [];
  const seen = new Set<string>();
  let cur: string | undefined = toolUseId;
  while (cur !== undefined && parentOf.has(cur) && !seen.has(cur)) {
    seen.add(cur);
    chain.push(cur);
    cur = parentOf.get(cur);
  }
  const outermost = chain[chain.length - 1];
  for (const entry of groupFeed(partitionFeed(visible).top)) {
    if (entry.kind === "item") {
      if (entry.item.kind === "tool" && entry.item.toolUseId === outermost) {
        return { key: `tool:${outermost}`, groupKey: null, tabMember: null, panelIds: chain };
      }
    } else if (entry.members.some((m) => m.toolUseId === outermost)) {
      const key = `group:${entry.members[0].toolUseId}`;
      return { key, groupKey: key, tabMember: outermost, panelIds: chain };
    }
  }
  return null;
}

/**
 * The semantic nav tokens one grouped-feed entry's wrapper carries, so
 * the keyboard cycle (nav.ts) can walk it. A tab group is a run of tool
 * calls, so it is a single `tool` stop however many members it holds.
 *
 * Re-derived every render rather than stamped once at creation: a
 * streaming text bubble only BECOMES final when its turn's result lands,
 * and a cycle that could not see that would skip the newest answer.
 */
export function navTokensForEntry(entry: FeedEntry, finals: FinalResponses): string {
  if (entry.kind === "group") return "tool";
  const item = entry.item;
  return navTokensForItem(item, item.kind === "text" && finals.chips.has(item.blockId));
}

/** A tab's short label: the member's headline, else its name and ordinal. */
function tabLabel(item: ToolItem, index: number): string {
  const head = toolHeadline(item);
  const label = head === "" ? `${item.toolName} #${index + 1}` : head;
  return label.length > 24 ? `${label.slice(0, 23)}…` : label;
}

/**
 * A tab group's card: one status-dotted chip per member, a failure count
 * that stays loud whichever tab is selected, and the active member's own
 * card carrying that chip row INSIDE it at the top — the inactive members'
 * cards exist only as their tabs until picked. The tab bar rides inside the
 * bubble (handed to ToolCard) rather than floating above it.
 */
export function groupHtml(
  members: readonly ToolItem[],
  activeId: string,
  panels?: PanelContext,
): string {
  const groupKey = members[0].toolUseId;
  const failed = members.filter((m) => m.result?.isError).length;
  const tabs = members
    .map((m, i) => {
      const status = m.result ? (m.result.isError ? "error" : "done") : "running";
      return `<button type="button" class="tab-chip${
        m.toolUseId === activeId ? " active" : ""
      }" data-tab-group="${escapeHtml(groupKey)}" data-tab-member="${escapeHtml(
        m.toolUseId,
      )}"><span class="agent-dot agent-${status}" aria-hidden="true">●</span> ${escapeHtml(
        tabLabel(m, i),
      )}</button>`;
    })
    .join("");
  const errBadge = failed > 0 ? `<span class="badge err">${failed} failed</span>` : "";
  const active = members.find((m) => m.toolUseId === activeId) ?? members[members.length - 1];
  const tabBar = `<div class="tab-bar">${tabs}${errBadge}</div>`;
  return `<div class="feed-group">${ToolCard(active, panels, tabBar)}</div>`;
}

/**
 * Whether a render must land the feed on its tail.
 *
 * A feed already parked at the bottom keeps following new content, as
 * ever. The addition: a user turn the previous render had not seen means
 * a prompt was JUST sent, and a sender wants to watch the answer — so the
 * feed jumps to the tail even from a scrolled-up position.
 */
export function repinsToTail(opts: {
  prevTurnId: string | null;
  nextTurnId: string | null;
  pinned: boolean;
}): boolean {
  if (opts.nextTurnId !== null && opts.nextTurnId !== opts.prevTurnId) return true;
  return opts.pinned;
}

/**
 * Identity of the feed's clear-cut boundary: the request id of the
 * `/clear` turn a cut feed opens on, or null for an uncut feed. ITEMS is
 * the already-cut list (`itemsFromLastClear`), whose head is a `/clear`
 * turn exactly when a cut happened. A change between renders means a
 * `/clear` just landed, which is the renderer's cue to rebuild the feed
 * rather than reconcile it (see `lastClearKey`).
 */
export function clearBoundary(items: readonly ConversationItem[]): string | null {
  const first = items[0];
  return first !== undefined && first.kind === "user-turn" && isClearTurn(first)
    ? first.requestId
    : null;
}

/** Items filled per backfill step during a restored-session render. */
export const BACKFILL_CHUNK = 40;

/**
 * Tail-first fill order for a restored session: index groups from the
 * last item backwards, so the newest content renders first and older
 * items backfill upwards. Indices within a group stay ascending (fill
 * order inside one synchronous chunk is invisible).
 */
export function backfillChunks(count: number, chunkSize: number): number[][] {
  const chunks: number[][] = [];
  for (let end = count; end > 0; end -= chunkSize) {
    const start = Math.max(0, end - chunkSize);
    const chunk: number[] = [];
    for (let i = start; i < end; i++) chunk.push(i);
    chunks.push(chunk);
  }
  return chunks;
}

/**
 * The fold a feed click should flip, or null to leave the click alone.
 *
 * TOGGLE is the nearest `data-panel-toggle` wrapper above the click and
 * PANEL the nearest `.agent-panel`. A click inside the toggle's OWN panel
 * belongs to the children rendered there, so it never collapses the fold
 * around them. A NESTED fold's ticker resolves to the nested wrapper as
 * its toggle, whose own panel is never between the ticker and the click —
 * the guard is relative to the toggle rather than absolute, which is what
 * lets folds nest recursively.
 */
export function panelToggleTarget<T extends { contains(node: T): boolean }>(
  toggle: T | null,
  panel: T | null,
): T | null {
  if (toggle === null) return null;
  if (panel !== null && toggle.contains(panel)) return null;
  return toggle;
}

/**
 * The ONE per-bubble async projection the amber border and the in-bubble
 * catalog both read: every bubble that owns detached background work → its
 * member tool items. Two sources, one map — the turn-hosted members
 * (`asyncByBubble` over the visible feed) plus the gns-sockets bridge
 * members, which `gnsFolds` re-hosts onto the answer above their upkeep
 * segment. A bridge spawn is an async member too, so its host bubble goes
 * amber and lists it as a badge exactly as any watcher's does — which is
 * what keeps the invariant true for the one async source `asyncByBubble`
 * cannot see (the bridge spawns it filtered out of the visible feed).
 */
export function asyncMembersByBubble(
  visible: readonly ConversationItem[],
  gnsByBubble: ReadonlyMap<string, readonly ConversationItem[]>,
): Map<string, ToolItem[]> {
  const byBubble = asyncByBubble(visible);
  for (const [host, folded] of gnsByBubble) {
    const members = folded.filter(isWatcher);
    if (members.length === 0) continue;
    const list = byBubble.get(host) ?? [];
    list.push(...members);
    byBubble.set(host, list);
  }
  return byBubble;
}

/**
 * Whether HOST-ID's bubble owns at least one live (unsettled) async member,
 * which is exactly when its border goes amber (see `watcherSettled`). A host
 * whose members have all settled has quiesced — its border flips back to the
 * green final-response (or to none), and the global `monitoring…` row drops.
 */
export function hasLiveAsync(
  hostId: string,
  panels?: PanelContext,
): boolean {
  return (panels?.watchers?.get(hostId) ?? []).some((m) => !watcherSettled(m, panels));
}

/**
 * Whether ANY async member anywhere in the feed is still live — the global
 * `monitoring…` row's gate (see `showsMonitoringRow`). Read over the whole
 * item list rather than one host's members, so a live member that outlived
 * its turn still lights the row even when its own bubble is scrolled off or
 * was never hosted (an orphan the projection could not place). ITEMS is the
 * clear-cut feed, so a member discarded by a /clear no longer counts.
 */
export function anyLiveAsync(
  items: readonly ConversationItem[],
  panels?: PanelContext,
): boolean {
  return items.some((i) => isWatcher(i) && !watcherSettled(i, panels));
}

/**
 * Feed renderer: reconciles the item list into `container`, reusing
 * nodes by key and only rewriting nodes whose HTML changed.
 */
export class FeedRenderer {
  private container: HTMLElement;
  private actions: Actions;
  private nodes = new Map<string, { el: HTMLElement; html: string }>();
  /** AskUserQuestion picks (renderer-owned so re-renders keep them). */
  private questionSelections = new Map<string, Set<string>>();
  /** Cards whose activity panel the user has open (renderer-owned too). */
  private openPanels = new Set<string>();
  /** Tab pins per group key; an unpinned group auto-follows the newest runner. */
  private activeTabs = new Map<string, string>();
  /** Half-typed agent messages, keyed by agent id (see agentComposer). */
  private msgDrafts = new Map<string, string>();
  private lastState: StoreState | null = null;
  /** Pending bottom-up fill steps from renderRestored, oldest last. */
  private backfillQueue: Array<() => void> = [];
  /** Newest user turn seen by a render, so the next one spots a fresh send. */
  private lastUserTurn: string | null = null;
  /**
   * Request id of the /clear turn the feed last cut at, or null when it
   * drew the whole item list. A render whose cut boundary MOVED rebuilds
   * the feed from nothing: the cut shifts every index-based key
   * (`user-turn:N`, `result:N`, …), so reconciling against the old node
   * map would reuse stale pre-clear elements out of position.
   */
  private lastClearKey: string | null = null;
  /**
   * Polls the daemon for watcher tails while their folds are open (see
   * watcher-poll.ts). Null when no daemon fetch was wired, in which case
   * the fold falls back to store-streamed tails only.
   */
  private watcherPoller: WatcherPoller | null = null;
  /**
   * Support-request phase per refused slash command. Renderer state so a
   * re-render cannot wipe an in-flight ask back to an unpressed button.
   */
  private supportPhases = new Map<string, SupportPhase>();
  /**
   * Which counter overlay is open on which agent bubble's topbar, keyed
   * by agent id. Renderer state (like openPanels) so the overlay survives
   * the card's per-frame re-renders; at most one entry, mirroring the
   * header's one-overlay-at-a-time rule feed-wide.
   */
  private agentMenus = new Map<string, TopbarMenu>();

  constructor(container: HTMLElement, actions: Actions) {
    this.container = container;
    this.actions = actions;
    if (actions.fetchTaskTail) {
      const fetchTail = actions.fetchTaskTail;
      this.watcherPoller = new WatcherPoller(fetchTail, () => {
        if (this.lastState) this.render(this.lastState);
      });
    }
    container.addEventListener("click", (e) => {
      const target = e.target as HTMLElement;
      const allow = target.getAttribute("data-perm-allow");
      const deny = target.getAttribute("data-perm-deny");
      if (allow) this.actions.decidePermission(allow, "allow");
      if (deny) this.actions.decidePermission(deny, "deny");
      const qReq = target.getAttribute("data-q-req");
      if (qReq !== null) {
        this.toggleQuestionOption(
          qReq,
          Number(target.getAttribute("data-q-idx")),
          Number(target.getAttribute("data-q-opt")),
        );
      }
      const submit = target.getAttribute("data-q-submit");
      if (submit !== null) this.submitQuestionAnswers(submit);
      // §2.13 queued-card controls: cancel drops the parked message,
      // run-now escalates it to preempt the running turn.
      const cancelQ = target.getAttribute("data-queue-cancel");
      if (cancelQ !== null) this.actions.cancelQueued(cancelQ);
      const runNowQ = target.getAttribute("data-queue-run-now");
      if (runNowQ !== null) this.actions.runQueuedNow(runNowQ);
      const prompt = target.closest("[data-send-prompt]")?.getAttribute("data-send-prompt");
      if (prompt) this.actions.sendPrompt?.(prompt);
      const msgTo = target.closest("[data-msg-send]")?.getAttribute("data-msg-send");
      if (msgTo !== null && msgTo !== undefined) this.sendAgentMessage(msgTo);
      const addSupport = target.closest("[data-add-support]")?.getAttribute("data-add-support");
      if (addSupport) this.requestAddSupport(addSupport);
      if (this.handleAgentTopbarClick(target)) return;
      this.handleTabClick(target);
      this.handlePanelToggle(target);
    });
    // Composer drafts are renderer state so re-renders cannot wipe them;
    // every keystroke lands here before the next frame can rebuild the card.
    container.addEventListener("input", (e) => {
      const target = e.target as HTMLElement;
      const forId = target.getAttribute("data-msg-for");
      if (forId !== null) {
        this.msgDrafts.set(forId, (target as HTMLInputElement).value);
      }
    });
  }

  /** Relay the drafted message to a background agent, via a prompt. */
  private sendAgentMessage(id: string): void {
    const text = (this.msgDrafts.get(id) ?? "").trim();
    if (text === "") return;
    this.msgDrafts.delete(id);
    this.actions.sendPrompt?.(`Use SendMessage to relay this to agent ${id}: ${text}`);
    if (this.lastState) this.render(this.lastState);
  }

  /**
   * Reveal a subagent's card from the roster dropdown. A subagent row's id
   * IS its card's tool-use id, so the reveal is direct.
   */
  revealAgent(agentId: string): boolean {
    return this.revealToolCard(agentId);
  }

  /**
   * Reveal the `TaskCreate` card behind roster task TASKID (see
   * `taskCreateToolUseId`): a task's harness id first maps back to the
   * call that created it, whose card is the task's one bubble in the feed.
   * The reveal opens the card's activity panel (the plan's panel chain
   * names the card itself), which is where the task's update history
   * nests, so the history is on show without a second click. Answers
   * whether that bubble was found — false when the id names no create, or
   * when the create's card is off the current feed.
   */
  revealTask(taskId: string): boolean {
    if (!this.lastState) return false;
    const toolUseId = taskCreateToolUseId(this.lastState.items, taskId);
    return toolUseId !== null && this.revealToolCard(toolUseId);
  }

  /**
   * Reveal tool call TOOLUSEID's card: pin its tab when it lives in a
   * consecutive-run group, open the activity panels that surface it and
   * its output, scroll its (or its outermost ancestor's) bubble to the top
   * of the feed, and lay that card's own capped sections out in full.
   * Answers whether the card was found in the current feed.
   */
  private revealToolCard(toolUseId: string): boolean {
    if (!this.lastState) return false;
    const plan = planToolReveal(this.lastState.items, toolUseId);
    if (!plan) return false;
    if (plan.groupKey !== null && plan.tabMember !== null) {
      this.activeTabs.set(plan.groupKey, plan.tabMember);
    }
    for (const id of plan.panelIds) this.openPanels.add(id);
    this.render(this.lastState);
    const node = this.nodes.get(plan.key)?.el;
    if (!node) return false;
    revealNode(node, "start");
    expandOwnSections(node);
    return true;
  }

  /**
   * A click on an agent bubble's topbar: its counter chips toggle that
   * bubble's overlay (closing any other bubble's, as the header keeps one
   * open at a time), and an agent or task row inside an open overlay
   * jumps the feed to that entry's bubble — the same verbs the header
   * topbar delegates. Answers whether the click was the topbar's, so the
   * caller stops before the card-level handlers see it.
   */
  private handleAgentTopbarClick(target: HTMLElement): boolean {
    const host = target.closest(`[${TOPBAR_AGENT_ATTR}]`);
    if (!host) return false;
    const action = topbarClickAction(target);
    if (!action) return false;
    if (action.kind === "toggle") {
      this.toggleAgentMenu(host.getAttribute(TOPBAR_AGENT_ATTR) ?? "", action.menu);
    } else if (action.kind === "reveal") {
      this.agentMenus.clear();
      this.revealAgent(action.agentId);
    } else {
      this.agentMenus.clear();
      this.revealTask(action.taskId);
    }
    return true;
  }

  /** Flip one bubble counter's overlay, closing every other bubble's. */
  private toggleAgentMenu(agentId: string, menu: TopbarMenu): void {
    const next = nextCounterMenu(this.agentMenus.get(agentId) ?? null, menu);
    this.agentMenus.clear();
    if (next !== null) this.agentMenus.set(agentId, next);
    this.rerender();
  }

  /**
   * Close every bubble topbar's counter overlay. The dismissal gestures
   * live at the document level with the header's (click-away, Escape), so
   * main.ts calls this from the same handlers that close the header's.
   */
  closeAgentMenus(): void {
    if (this.agentMenus.size === 0) return;
    this.agentMenus.clear();
    this.rerender();
  }

  /** A click on a tab chip pins that member as its group's active card. */
  private handleTabClick(target: HTMLElement): void {
    const chip = target.closest("[data-tab-member]");
    if (!chip) return;
    const group = chip.getAttribute("data-tab-group") ?? "";
    const member = chip.getAttribute("data-tab-member") ?? "";
    this.activeTabs.set(`group:${group}`, member);
    if (this.lastState) this.render(this.lastState);
  }

  /**
   * A click on an activity fold flips its card's panel. Clicks INSIDE
   * the toggle's own panel belong to the children (their own expands,
   * permission buttons, NESTED folds — which resolve as their own
   * toggles), a click on any control belongs to that control, and a
   * click ending a text highlight is a selection gesture — the same
   * guards expandAction applies to the capped sections.
   */
  private handlePanelToggle(target: HTMLElement): void {
    const toggle = panelToggleTarget(
      target.closest("[data-panel-toggle]"),
      target.closest(`.${PANEL_CLASS}`),
    );
    if (
      !toggle ||
      target.closest(CLICK_THROUGH_SELECTOR) !== null ||
      (window.getSelection()?.toString() ?? "").trim() !== ""
    ) {
      return;
    }
    const id = toggle.getAttribute("data-panel-toggle") ?? "";
    if (this.openPanels.has(id)) {
      this.openPanels.delete(id);
    } else {
      this.openPanels.add(id);
    }
    if (this.lastState) this.render(this.lastState);
  }

  /** The PanelContext this renderer's state backs. */
  private panelContext(
    children: ReadonlyMap<string, readonly ConversationItem[]>,
    watchers: ReadonlyMap<string, readonly ToolItem[]>,
    gnsFoldsByBubble?: ReadonlyMap<string, readonly ConversationItem[]>,
  ): PanelContext {
    return {
      children,
      isOpen: (id) => this.openPanels.has(id),
      selections: this.questionSelections,
      drafts: this.msgDrafts,
      watchers,
      gnsFolds: gnsFoldsByBubble,
      taskTail: (id) => this.watcherPoller?.tail(id),
      supportPhases: this.supportPhases,
      canAddSupport: this.actions.addSupport !== undefined,
      // The FULL item list, not the feed's clear-cut one: the roster
      // retention stamps (`deactivatedAtTurn`) ride the whole-session
      // counted-turn clock, so the strip's own clock must too — a cut
      // list's smaller count would freeze every settled row at "just now".
      agentTopbar: (agent) =>
        agentTopbarHtml(
          this.lastState?.items ?? [],
          agent,
          {
            agentsOpen: this.agentMenus.get(agent.toolUseId) === "agents",
            tasksOpen: this.agentMenus.get(agent.toolUseId) === "tasks",
            // An agent strip renders no tokens chip (its tokens datapoint
            // is the plain context figure), so nothing ever opens here.
            tokensOpen: false,
          },
          Date.now(),
        ),
    };
  }

  /**
   * Ask Emacs for a workspace that adds support for COMMAND, tracking the
   * request's phase so the card reports it.
   *
   * A second click while one is in flight is dropped: the ask is not
   * idempotent downstream (each emitted create opens another workspace),
   * so a double-press must never become two workspaces.
   */
  private requestAddSupport(command: string): void {
    const ask = this.actions.addSupport;
    if (!ask) return;
    if (this.supportPhases.get(command)?.kind === "asking") return;
    this.supportPhases.set(command, { kind: "asking" });
    this.rerender();
    void ask(command).then(
      (workspace) => {
        this.supportPhases.set(command, { kind: "asked", workspace });
        this.rerender();
      },
      (err: unknown) => {
        // A failed ask is surfaced on the card, never swallowed into a
        // button that quietly went back to looking unpressed.
        this.supportPhases.set(command, {
          kind: "failed",
          error: err instanceof Error ? err.message : String(err),
        });
        this.rerender();
      },
    );
  }

  /** Redraw from the last state, after renderer-owned state changed. */
  private rerender(): void {
    if (this.lastState) this.render(this.lastState);
  }

  /**
   * Point the poller at the sources in currently-open folds — at EVERY
   * depth, via the recursive walk openSubfeedSourceIds shares with the
   * renderer's own guards. Called after every render so opening a fold
   * starts its polls and closing one stops them (a nested fold's tail
   * discovered by a poll feeds the next sync); a no-op when no daemon
   * fetch was wired.
   */
  private syncWatcherPolls(watchers: ReadonlyMap<string, readonly ToolItem[]>): void {
    const ids = openSubfeedSourceIds({
      items: this.lastState?.items ?? [],
      watchers,
      isOpen: (id) => this.openPanels.has(id),
      tailText: (id) => this.watcherPoller?.tail(id)?.text,
    });
    this.watcherPoller?.sync(ids);
  }

  /** The DOM key one grouped-feed entry reconciles under. */
  private entryKey(entry: FeedEntry): string {
    return entry.kind === "group"
      ? `group:${entry.members[0].toolUseId}`
      : itemKey(entry.item, entry.index);
  }

  /**
   * Stamp EL's nav tokens for ENTRY. An item rendering nothing (a
   * meta-only user turn, hidden by `.feed-item:empty`) is never a cycle
   * stop: jumping to an invisible bubble reads as a dead keypress.
   */
  private stampNav(el: HTMLElement, entry: FeedEntry, finals: FinalResponses, html: string): void {
    const tokens = html === "" ? "" : navTokensForEntry(entry, finals);
    if (tokens === "") {
      delete el.dataset.nav;
    } else {
      el.dataset.nav = tokens;
    }
  }

  /** One grouped-feed entry's HTML: the item's own, or its group's card. */
  private entryHtml(
    entry: FeedEntry,
    finals: FinalResponses,
    pulse: PulseTarget,
    panels: PanelContext,
  ): string {
    if (entry.kind === "group") {
      const active = activeGroupMember(entry.members, this.activeTabs.get(this.entryKey(entry)));
      return groupHtml(entry.members, active, panels);
    }
    return this.itemHtml(entry.item, finals, pulse, panels);
  }

  private pendingQuestionItem(requestId: string): { item: PermissionItem; questions: AskQuestion[] } | null {
    const item = this.lastState?.items.find(
      (it): it is PermissionItem => it.kind === "permission" && it.requestId === requestId,
    );
    if (!item || item.resolution) return null;
    const questions = askQuestions(item);
    return questions ? { item, questions } : null;
  }

  private toggleQuestionOption(requestId: string, qIdx: number, optIdx: number): void {
    const found = this.pendingQuestionItem(requestId);
    const opt = found?.questions[qIdx]?.options[optIdx];
    if (!found || !opt) return;
    const key = `${requestId} ${qIdx}`;
    const set = this.questionSelections.get(key) ?? new Set<string>();
    if (found.questions[qIdx].multiSelect) {
      if (set.has(opt.label)) {
        set.delete(opt.label);
      } else {
        set.add(opt.label);
      }
    } else {
      set.clear();
      set.add(opt.label);
    }
    this.questionSelections.set(key, set);
    if (this.lastState) this.render(this.lastState);
  }

  /**
   * One item's HTML: green-bordered and chip-bearing when FINALS marks it a
   * turn's answer, breathing when PULSE names it, and carrying its
   * activity panel when PANELS holds children for it.
   */
  private itemHtml(
    item: ConversationItem,
    finals: FinalResponses,
    pulse: PulseTarget,
    panels: PanelContext,
  ): string {
    return renderItem(item, this.questionSelections, finals, isPulsed(item, pulse), panels);
  }

  private submitQuestionAnswers(requestId: string): void {
    const found = this.pendingQuestionItem(requestId);
    if (!found) return;
    const answers: Record<string, string> = {};
    for (let qi = 0; qi < found.questions.length; qi++) {
      const set = this.questionSelections.get(`${requestId} ${qi}`);
      if (!set || set.size === 0) return; // incomplete (button disabled)
      answers[found.questions[qi].question] = [...set].join(", ");
    }
    for (let qi = 0; qi < found.questions.length; qi++) {
      this.questionSelections.delete(`${requestId} ${qi}`);
    }
    this.actions.answerQuestions(requestId, {
      questions: found.questions,
      answers,
    });
  }

  /**
   * Restored-session render (§2.10 fresh-join replay): builds every
   * item's shell, fills the NEWEST chunk synchronously, jumps straight
   * to the bottom, and backfills older items upwards across animation
   * frames — the latest message is visible immediately with no
   * top-down build crawl and no scroll animation. Scroll position is
   * compensated per chunk so the view never moves off the tail.
   */
  renderRestored(state: StoreState): void {
    this.lastState = state;
    // Replayed history's newest prompt is old news: banking it here keeps
    // the next render from reading it as a fresh send and yanking a feed
    // the user has meanwhile scrolled up.
    this.lastUserTurn = lastUserTurnId(state.items);
    this.backfillQueue = [];
    releaseChessGames(this.container);
    this.container.innerHTML = "";
    this.nodes.clear();
    // A /clear clears the screen: the feed opens on the /clear bubble and
    // its boundary rule, and the discarded turns are not drawn at all.
    // The boundary is banked so the next live render reconciles instead
    // of pointlessly rebuilding the feed this method just built.
    const items = itemsFromLastClear(state.items);
    this.lastClearKey = clearBoundary(items);
    // The gns-sockets fold: bridge upkeep leaves the top feed and every
    // turn-shaped projection (finals, watchers, pulse), so the green
    // border lands on the real answer and the folded segment renders
    // inside its host bubble's panel instead. The partition still runs on
    // the FULL list — a folded spawn card keeps its parented children,
    // which its card renders inside the fold.
    const gns = gnsFolds(items);
    const visible = items.filter((i) => !gns.folded.has(i));
    const part = partitionFeed(items);
    const top = part.top.filter((i) => !gns.folded.has(i));
    const watchers = asyncMembersByBubble(visible, gns.byBubble);
    const panels = this.panelContext(part.children, watchers, gns.byBubble);
    this.syncWatcherPolls(watchers);
    const finals = finalResponses(visible);
    const pulse = pulseTarget(visible, state.turnInFlight, finals);
    const shells: Array<{ el: HTMLElement; entry: FeedEntry }> = [];
    for (const entry of groupFeed(top)) {
      const key = this.entryKey(entry);
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = key;
      this.container.appendChild(el);
      this.nodes.set(key, { el, html: "" });
      shells.push({ el, entry });
    }
    // A fresh join landing mid-turn shows the active tail status row
    // immediately, above the parked queue — the same tail slot and precedence
    // (`tailStatusRow`) render() keeps it in.
    const tailRow = tailStatusRow(state.interrupting, state.compacting, pulse);
    if (tailRow) {
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = tailRow.key;
      el.innerHTML = tailRow.html;
      this.container.appendChild(el);
      this.nodes.set(tailRow.key, { el, html: tailRow.html });
    }
    // The global `monitoring…` fallback, on the same idle-with-live-async
    // terms render() shows it (see `showsMonitoringRow`), so a fresh join
    // landing on a quiescent-but-still-watching session sees it immediately.
    if (
      showsMonitoringRow({
        turnInFlight: state.turnInFlight,
        interrupting: state.interrupting,
        anyLiveAsync: anyLiveAsync(items, panels),
      })
    ) {
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = "monitoring";
      el.innerHTML = monitoringRowHtml(true);
      this.container.appendChild(el);
      this.nodes.set("monitoring", { el, html: monitoringRowHtml(true) });
    }
    // The parked queue (§2.13) renders in full at the tail — a fresh join
    // with a pending queue must show it, and the cards are cheap enough to
    // skip the tail-first backfill the history items get.
    state.queued.forEach((q) => {
      const key = queuedCardKey(q);
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = key;
      const html = QueuedCard(q);
      el.innerHTML = html;
      this.container.appendChild(el);
      this.nodes.set(key, { el, html });
    });
    const fillChunk = (indexes: number[]): void => {
      const before = this.container.scrollHeight;
      for (const i of indexes) {
        const { el, entry } = shells[i];
        const html = this.entryHtml(entry, finals, pulse, panels);
        el.innerHTML = html;
        this.stampNav(el, entry, finals, html);
        hydrateChessGames(el);
        const node = this.nodes.get(el.dataset.key ?? "");
        if (node) node.html = html;
      }
      // Content added above the viewport grows scrollHeight; shift
      // scrollTop by the growth so the tail stays in view.
      this.container.scrollTop += this.container.scrollHeight - before;
      // Per chunk, not once at the end: a backfill step is where an older
      // item's text actually reaches the DOM, so anything derived from that
      // text (the search's marks) is stale until the step that lands it.
      this.actions.onRendered?.();
    };
    const chunks = backfillChunks(shells.length, BACKFILL_CHUNK);
    if (chunks.length > 0) fillChunk(chunks[0]);
    else this.actions.onRendered?.();
    parkAtTail(this.container);
    this.backfillQueue = chunks.slice(1).map((c) => () => fillChunk(c));
    this.scheduleBackfill();
  }

  private scheduleBackfill(): void {
    if (this.backfillQueue.length === 0) return;
    requestAnimationFrame(() => {
      const step = this.backfillQueue.shift();
      if (step) step();
      this.scheduleBackfill();
    });
  }

  /**
   * Complete any in-flight backfill synchronously. render() must
   * reconcile against fully-materialized nodes — an empty shell's
   * cached "" html would otherwise re-fill mid-reconcile anyway, just
   * unpredictably interleaved.
   */
  private flushBackfill(): void {
    while (this.backfillQueue.length > 0) {
      const step = this.backfillQueue.shift();
      if (step) step();
    }
  }

  /**
   * Ensure a single-purpose tail node (keyed KEY) exists, holds HTML, and
   * sits at the current bottom of the feed, marking it seen so the reconcile
   * sweep keeps it. Force-appends so a live item node appended above never
   * lands beneath it — the same discipline the queue cards follow.
   */
  private reconcileTailNode(key: string, html: string, seen: Set<string>): void {
    seen.add(key);
    let entry = this.nodes.get(key);
    if (!entry) {
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = key;
      entry = { el, html: "" };
      this.nodes.set(key, entry);
    }
    if (entry.html !== html) {
      entry.el.innerHTML = html;
      entry.html = html;
    }
    this.container.appendChild(entry.el);
  }

  render(state: StoreState): void {
    this.flushBackfill();
    this.lastState = state;
    const turnId = lastUserTurnId(state.items);
    const toTail = repinsToTail({
      prevTurnId: this.lastUserTurn,
      nextTurnId: turnId,
      pinned: isPinnedToBottom(this.container),
    });
    this.lastUserTurn = turnId;
    // A /clear clears the screen: only the /clear bubble and what follows
    // it render. A cut that just MOVED rebuilds the feed from nothing —
    // the cut shifts every index-based key, so reconciling would reuse
    // stale pre-clear elements out of position.
    const items = itemsFromLastClear(state.items);
    const boundary = clearBoundary(items);
    if (boundary !== this.lastClearKey) {
      this.container.innerHTML = "";
      this.nodes.clear();
    }
    this.lastClearKey = boundary;
    // The gns-sockets fold: bridge upkeep leaves the top feed and every
    // turn-shaped projection (finals, watchers, pulse), so the green
    // border lands on the real answer and the folded segment renders
    // inside its host bubble's panel instead. The partition still runs on
    // the FULL list — a folded spawn card keeps its parented children,
    // which its card renders inside the fold.
    const gns = gnsFolds(items);
    const visible = items.filter((i) => !gns.folded.has(i));
    const part = partitionFeed(items);
    const top = part.top.filter((i) => !gns.folded.has(i));
    const watchers = asyncMembersByBubble(visible, gns.byBubble);
    const panels = this.panelContext(part.children, watchers, gns.byBubble);
    this.syncWatcherPolls(watchers);
    const finals = finalResponses(visible);
    const pulse = pulseTarget(visible, state.turnInFlight, finals);
    const seen = new Set<string>();
    for (const feedEntry of groupFeed(top)) {
      const key = this.entryKey(feedEntry);
      seen.add(key);
      const html = this.entryHtml(feedEntry, finals, pulse, panels);
      let entry = this.nodes.get(key);
      if (!entry) {
        const el = document.createElement("div");
        el.className = "feed-item";
        el.dataset.key = key;
        this.container.appendChild(el);
        entry = { el, html: "" };
        this.nodes.set(key, entry);
      }
      this.stampNav(entry.el, feedEntry, finals, html);
      if (entry.html !== html) {
        // A section the user clicked open outlives the re-render of the
        // item that carries it: a running tool card rewrites its whole
        // body when its result lands, which would otherwise re-cap a
        // command the user had just expanded.
        const open = expandedKeys(sectionsIn(entry.el));
        releaseChessGames(entry.el);
        entry.el.innerHTML = html;
        applyExpanded(sectionsIn(entry.el), open);
        hydrateChessGames(entry.el);
        entry.html = html;
      }
    }
    // The tail status row rides at the tail of the live turn's content, above
    // the parked queue. Force-appended (like the queue cards below) so a live
    // item node appended above never lands beneath it; whichever key is not
    // reconciled this frame falls out of `seen` and the sweep removes it. The
    // precedence lives in `tailStatusRow`.
    const tailRow = tailStatusRow(state.interrupting, state.compacting, pulse);
    if (tailRow) this.reconcileTailNode(tailRow.key, tailRow.html, seen);
    // The global `monitoring…` row: the amber fallback for when the owning
    // bubble is scrolled off, shown only while the session is idle and live
    // async continues (mutually exclusive with the bucket-1 tail above, whose
    // rows all mean the main chain is active).
    if (
      showsMonitoringRow({
        turnInFlight: state.turnInFlight,
        interrupting: state.interrupting,
        anyLiveAsync: anyLiveAsync(items, panels),
      })
    ) {
      this.reconcileTailNode("monitoring", monitoringRowHtml(true), seen);
    }
    // The in-flight queue (§2.13) is a subdued section at the tail, after
    // every conversation item. Each card is re-appended so a live item node
    // appended above (the agent's streaming response during the running
    // turn) never lands beneath a parked message.
    state.queued.forEach((q) => {
      const key = queuedCardKey(q);
      seen.add(key);
      const html = QueuedCard(q);
      let entry = this.nodes.get(key);
      if (!entry) {
        const el = document.createElement("div");
        el.className = "feed-item";
        el.dataset.key = key;
        entry = { el, html: "" };
        this.nodes.set(key, entry);
      }
      if (entry.html !== html) {
        entry.el.innerHTML = html;
        entry.html = html;
      }
      this.container.appendChild(entry.el);
    });
    for (const [key, entry] of this.nodes) {
      if (!seen.has(key)) {
        releaseChessGames(entry.el);
        entry.el.remove();
        this.nodes.delete(key);
      }
    }
    if (toTail) {
      parkAtTail(this.container);
    }
    this.actions.onRendered?.();
  }
}
