/**
 * DOM renderers for conversation items. Component naming mirrors the
 * spec (§2.4–2.7): TextStream, Thinking, ToolCard/<Name>,
 * PermissionPrompt. The feed renderer reuses one element per item key
 * so streaming updates do not rebuild the whole list.
 */
import { SUBAGENT_TOOLS } from "./agents.js";
import { parseJournal } from "./async-stream.js";
import { clearLogDedup, log } from "./wslog.js";
import {
  TOPBAR_AGENT_ATTR,
  TopbarMenu,
  agentTopbarHtml,
  nextCounterMenu,
  topbarClickAction,
} from "./topbar.js";
import { taskCreateToolUseId } from "./tasks.js";
import { IDLE_LABEL, TIMER_SLOT } from "./timer.js";
import { compactTokens, formatTokens, turnInputTokens } from "./tokens.js";
import { formatAge, formatDuration, formatDurationCeil, formatElapsed } from "./duration.js";
import {
  CLICK_THROUGH_SELECTOR,
  PANEL_CLASS,
  applyExpanded,
  expandOwnSections,
  expandedKeys,
  sectionsIn,
} from "./expand.js";
import { animatedEllipsis, escapeHtml, highlightCode, languageForPath } from "./highlight.js";
import { partitionFeed } from "./partition.js";
import {
  mayNest,
  mergeChildren,
  openSubfeedSourceIds,
  transcriptFeed,
} from "./subfeed.js";
import { parseUnsupportedCommand } from "./unsupported.js";
import { StatusResponse, statusPanelHtml, statusSnapshotFromInit } from "./status.js";
import {
  BodySpec,
  MemberContext,
  MemberStatus,
  StreamMember,
  livePollSourceIds,
  resolveMember,
} from "./stream-member.js";
import {
  chessGameContainerHtml,
  hydrateChessGames,
  releaseChessGames,
  splitChessGameSegments,
} from "./chess-game.js";
import { inline, renderMarkdown } from "./markdown.js";
import {
  DEFERRED_CLASS,
  HEIGHT_VAR,
  LazyUpgrader,
  canDeferItems,
  estimateHeightPx,
  isHeavyItem,
  itemPlainText,
  placeholderHtml,
} from "./lazy-item.js";
import { renderPromptBody } from "./prompt-body.js";
import { findTreeRegion, looksLikeIntendedTree, renderTreeHtml } from "./metaprompt-tree.js";
import { AsyncSource, ModelInfo, QueuedItem } from "./protocol.js";
import { previewFromInput } from "./permission-preview.js";
import { navTokensForItem } from "./nav.js";
import { freezeOnScroll, freezeOnToggle, isPinnedToBottom, parkAtTail, revealNode } from "./scroll.js";
import { blocksToText, userTurnText } from "./turn.js";
import { clearOrCompactKey, itemsFromClearOrCompact } from "./clear-compact.js";
import { gnsFolds } from "./gns.js";
import { asyncByBubble, isWatcher, watcherRef } from "./watchers.js";
import { TaskTail, WatcherPoller } from "./watcher-poll.js";
import {
  ContextClearedItem,
  ContextCompactedItem,
  ConversationItem,
  PermissionItem,
  ResultItem,
  SessionCommandItem,
  SystemFailureCard,
  StoreState,
  SystemItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
  liveContextDelta,
  userTurnKey,
} from "./store.js";
import type { SessionCommand } from "./frontend-proto.js";

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
  cancelQueued(entryId: string): void;
  /** Escalate a parked queued message to preempt the running turn (§2.13). */
  runQueuedNow(entryId: string): void;
  /** Confirm a held prompt's classification (view state only, E4). */
  acceptQueued(entryId: string): void;
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
   * Assemble the status panel's inputs: the snapshot projected from the
   * session's PUSHED `SystemInit` plus the account read from the sanctioned
   * account endpoint. Present only when a daemon backs the webapp; without it
   * a refused `/status` falls back to the generic unsupported card.
   * Session-bound by main.ts, like `addSupport`.
   */
  getStatus?(): Promise<StatusResponse>;
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

/**
 * The CLI's sentinel for a turn the user aborted. Claude Code injects it as a
 * standalone assistant text block when a request is interrupted, in one of two
 * forms depending on whether a tool call was in flight. The yellow aborted
 * `ResultChip` (`item.subtype === "aborted"`) is already the single, canonical
 * way this feed marks an interrupt, so the sentinel bubble beside it is a
 * duplicate denotation and draws nothing (`rendersEmpty`). Matching is on the
 * TRIMMED body being nothing but the sentinel, so a real partial answer that
 * merely ends near an interrupt is untouched.
 */
const INTERRUPT_SENTINELS = new Set([
  "[Request interrupted by user]",
  "[Request interrupted by user for tool use]",
]);

/** Whether a text block's trimmed body is nothing but an interrupt sentinel. */
function isInterruptSentinel(body: string): boolean {
  return INTERRUPT_SENTINELS.has(body);
}

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
 * conversation — the daemon's `ProgressView.compacting` window, opened while
 * the work runs and closed when it lands. The SDK reports no progress
 * percentage, so
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
 * Whether the GLOBAL `monitoring…` indicator shows: the session is IDLE (no
 * turn in flight, so none of thinking/working/retrying/interrupting speak for
 * the tail) yet live async continues somewhere in the feed. It is the
 * always-visible amber signal for when the owning bubble is scrolled off or
 * absent — the quiescent twin of the working row, and mutually exclusive with
 * the whole bucket-1 tail, which only runs while a turn is in flight. It now
 * breathes as the sidebar's amber dot on the session's own row (see
 * `WorkspaceSidebar.setMonitoring`) rather than as topbar text or a feed-tail
 * row; this predicate is the gate that surface reads.
 *
 * A live `thinking…` indicator ALSO suppresses it, even with no main-chain
 * turn in flight: a background subagent that is mid-thought renders its own
 * `thinking…` spinner (see `Thinking`), and that more-specific live signal
 * wins the shared tail slot so the two never stack. The agent can be thinking
 * (a subagent block open) and monitoring (async still live) at once, but the
 * amber indicator is only the fallback for when nothing more specific speaks,
 * so a visible `thinking…` takes precedence over it (`anyLiveThinking`).
 */
export function showsMonitoringRow(opts: {
  turnInFlight: boolean;
  interrupting: boolean;
  thinking: boolean;
  anyLiveAsync: boolean;
}): boolean {
  if (opts.turnInFlight || opts.interrupting || opts.thinking) return false;
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
 * which CSS reveals only while the reader hovers that corner (see
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
 * now (the progress footer), which retired the prompt breath — so the bubble is
 * a plain `bubble user`, the same way a running tool card ignores the pulse.
 */
function UserTurn(item: UserTurnItem, panels?: PanelContext): string {
  // A tools-only turn hosts its live async on its own prompt bubble (see
  // asyncByBubble), so the prompt goes amber and catalogs that work too — the
  // invariant holds even for a turn that wrote no answer to host it. The
  // projection keys the prompt by the user-turn's request id.
  const stateCls = hasLiveAsync(item.requestId, panels) ? " async-live" : "";
  const catalog = AsyncCatalog(item.requestId, panels);
  // The prompt is shown verbatim, EXCEPT for markdown fenced code blocks,
  // which render as the same highlighted card the agent's own fences get
  // (see renderPromptBody). A fence-free prompt keeps its plain <pre>.
  const body = `${renderPromptBody(userTurnText(item))}${catalog}`;
  return Bubble(`bubble user${stateCls}`, body, item.ts);
}

/** The fixed body of the Merge status card (see `MergeCard`). */
export const MERGE_CARD_BODY =
  "Workspace conflict resolution failed with merge queue agent, attempting resolution here";

/**
 * The Merge status card: a merge-failure remediation turn (`origin: "merge"`)
 * renders here INSTEAD of as a user-prompt bubble, so the injected directive
 * never shows as a prompt the user did not type. It borrows the tool-card
 * shell the `Bash`/`Agent`/`Skill` cards use — a full-width, left-flushed card
 * with a `Merge` label in the `.tool-head` slot — but its body is one fixed
 * line with no tool input, output, or activity fold, since it is a status
 * marker rather than a tool call. The directive it stands in for still drives
 * the agent under the hood, and the agent's resolution streams below as
 * ordinary bubbles.
 */
function MergeCard(): string {
  return `
    <div class="tool-card tool-merge">
      <div class="tool-head"><span class="tool-name">Merge</span></div>
      <div class="merge-body">${escapeHtml(MERGE_CARD_BODY)}</div>
    </div>`;
}

/**
 * Classification badge for a held prompt (E4). `pending` is the pre-verdict
 * state; `hold` will be delivered when the turn ends; `interject` is preempting
 * the running turn right now; `error` means NOTHING decided it — shown as its
 * own state so it can never read as a real verdict.
 */
function queuedBadge(item: QueuedItem): { label: string; cls: string } {
  const cls = item.classification;
  switch (cls) {
    case "hold":
      return { label: "queued — will run after this turn", cls: "hold" };
    case "interject":
      return { label: "interrupting…", cls: "interject" };
    case "error":
      return { label: "queued — unclassified", cls: "error" };
    case "pending":
      return { label: "queued — classifying…", cls: "pending" };
    default: {
      // EXHAUSTIVE by construction: a new QueueClassification arm fails to
      // COMPILE here. The old `case "pending": default:` fallthrough would
      // have rendered any future verdict as "classifying…", telling the user
      // their prompt was still being judged when the daemon had already
      // decided something this badge does not know how to say.
      const unhandled: never = cls;
      throw new Error(`render: unhandled queue classification ${String(unhandled)}`);
    }
  }
}

/**
 * A prompt the daemon is holding (E4). Deliberately NOT a `bubble user`: a
 * subdued card, so the reader SEES that the message is waiting and has not
 * reached the agent. It carries the prompt text, the classification badge, the
 * classifier's reason once known, and the three controls the daemon actually
 * honors. Each button carries the entry id for delegated click handling.
 */
/**
 * The DRAIN-LEASE bubble: a prompt parked not by a running turn but by a
 * scheduled daemon bounce.
 *
 * A DEDICATED element rather than a variant badge on the classifier card,
 * because the two say opposite things about what happens next. A classifier
 * bubble reports a verdict on the prompt and promises delivery at the end of
 * the current turn; this prompt has NO verdict — the classifier never ran on
 * it — and waits on an event in a different part of the system entirely. Its
 * controls differ for the same reason: there is no verdict to Accept, and
 * forcing it does not merely preempt a turn, it pushes the whole bounce back.
 */
export function LeaseHeldCard(item: QueuedItem, scheduleId: string): string {
  const qid = escapeHtml(item.id);
  return `
    <div class="queued-card lease-card" data-schedule-id="${escapeHtml(scheduleId)}">
      <div class="queued-head">
        <span class="queued-badge lease">held — daemon bounce scheduled</span>
      </div>
      <pre class="queued-content">${escapeHtml(item.text)}</pre>
      <div class="lease-reason">${escapeHtml(LEASE_HELD_REASON)}</div>
      <div class="queued-actions">
        <button data-queue-cancel="${qid}">Cancel</button>
        <button data-queue-run-now="${qid}" title="${escapeHtml(LEASE_FORCE_TITLE)}">Deliver now</button>
      </div>
    </div>`;
}

/** Why the prompt is parked, said plainly — assertable without parsing markup. */
export const LEASE_HELD_REASON =
  "A daemon bounce is scheduled, so no new turn may start. This prompt is held " +
  "until the bounce completes and is delivered afterwards — it is not lost, and " +
  "it has not been judged by the classifier.";

/** The Force control's promise, and its cost. */
export const LEASE_FORCE_TITLE = "deliver now, delays the bounce";

/**
 * The KEEP-ALIVE bubble: a prompt parked behind an in-flight cache keep-alive
 * ping.
 *
 * Modeled on the drain-lease bubble above, NOT on the classifier bubble, and
 * for the same reason: the classifier never ran on this entry, so its
 * `classification` carries no verdict and a badge claiming one would be a
 * judgment nothing made.
 *
 * WHERE IT DIFFERS FROM THE LEASE BUBBLE is the one control it does not draw.
 * A lease-held prompt can be forced through at the cost of delaying the bounce;
 * this one CANNOT, because the keep-alive turn has to finish before the daemon
 * can rewind and submit it — there is no ordering in which forcing works. Its
 * only exits are delivery when the ping's turn ends and Cancel, so those are
 * the only things on screen.
 */
export function KeepAliveHeldCard(item: QueuedItem, turnId: string): string {
  const qid = escapeHtml(item.id);
  return `
    <div class="queued-card keep-alive-card" data-keep-alive-turn-id="${escapeHtml(turnId)}">
      <div class="queued-head">
        <span class="queued-badge keep-alive">held — waiting on a keep-alive response</span>
      </div>
      <pre class="queued-content">${escapeHtml(item.text)}</pre>
      <div class="lease-reason">${escapeHtml(KEEP_ALIVE_HELD_REASON)}</div>
      <div class="queued-actions">
        <button data-queue-cancel="${qid}">Cancel</button>
      </div>
    </div>`;
}

/** Why the prompt is parked, said plainly — assertable without parsing markup. */
export const KEEP_ALIVE_HELD_REASON =
  "A cache keep-alive turn is in flight, so this prompt waits for its response. It is " +
  "delivered as soon as that turn ends — it is not lost, it has not been judged by the " +
  "classifier, and it cannot be forced through ahead of the keep-alive.";

export function QueuedCard(item: QueuedItem): string {
  // THE LEASE HOLD OUTRANKS THE CLASSIFICATION, and there is nothing to weigh:
  // the classifier never ran on a lease-held entry, so its classification
  // field carries no verdict to show. Rendering the classifier card here would
  // put a badge on screen claiming a judgment nothing made.
  if (item.shutdownHold !== undefined) {
    return LeaseHeldCard(item, item.shutdownHold.scheduleId);
  }
  // THE KEEP-ALIVE HOLD OUTRANKS THE CLASSIFICATION for exactly the reason the
  // lease hold above does, and the two holds cannot co-occur: a keep-alive turn
  // is a turn, and the drain lease is what stops turns from starting.
  if (item.keepAliveHold !== undefined) {
    return KeepAliveHeldCard(item, item.keepAliveHold.turnId);
  }
  const badge = queuedBadge(item);
  const qid = escapeHtml(item.id);
  const reason = item.rationale
    ? `<div class="queued-reason">${escapeHtml(item.rationale)}</div>`
    : "";
  // Accept is offered only where it means something: confirming a verdict that
  // has actually landed. There is nothing to confirm about a pending entry, and
  // an already-accepted one says so instead of offering the button again.
  const accept =
    item.classification === "pending" || item.accepted
      ? ""
      : `<button data-queue-accept="${qid}">Accept</button>`;
  const acceptedMark = item.accepted ? `<span class="queued-accepted">accepted</span>` : "";
  return `
    <div class="queued-card">
      <div class="queued-head">
        <span class="queued-badge ${badge.cls}">${escapeHtml(badge.label)}</span>
        ${acceptedMark}
      </div>
      <pre class="queued-content">${escapeHtml(item.text)}</pre>
      ${reason}
      <div class="queued-actions">
        <button data-queue-cancel="${qid}">Cancel</button>
        ${accept}
        <button data-queue-run-now="${qid}">Run now</button>
      </div>
    </div>`;
}

/** Key identifying one queued card's DOM node across renders (E4). */
export function queuedCardKey(item: QueuedItem): string {
  return `queued:${item.id}`;
}

// Bubbles already warned about a metaprompt-tree postprocessing misfire. A
// persistent misfire re-renders on every feed pass, so each bubble warns at
// most once.
const warnedTreeMisfires = new Set<string>();

/**
 * Warn (once per bubble) when a completed, header-led text segment produced
 * no tree region: the response reads as an intended metaprompt tree, yet the
 * postprocessing did not fire. A segment carrying a ``` fence is skipped,
 * because the markdown fence handler may still render a fenced tree from it.
 */
function warnTreeMisfire(blockId: string, done: boolean, segText: string): void {
  if (!done) return;
  if (segText.includes("```")) return;
  if (!looksLikeIntendedTree(segText)) return;
  if (warnedTreeMisfires.has(blockId)) return;
  warnedTreeMisfires.add(blockId);
  log("warn", `metaprompt-tree: postprocessing did not fire for a header-led response (block ${blockId})`, { operation: "render.metaprompt-tree-postprocess-missed", context: { block_id: blockId } });
}

/**
 * A turn's FINAL response gets the green border (§2.4): the answer the
 * turn actually landed on, set apart from the running commentary the
 * agent emits between its tool calls.
 *
 * CHIP is the closing result of the turn that bubble answers. Its stats
 * ride in the bubble's top-right corner (`resultMeta` via `Bubble`'s META):
 * the turn's elapsed time and the input tokens it spent, rather than a
 * pill floating in the feed beneath. Only a final response carries one, so
 * CHIP is null for the commentary bubbles above it (see `finalResponses`).
 *
 * The working frontier no longer breathes at all (the progress footer speaks
 * for a running turn now), but
 * this function never renders it: the breath is a live-view accent the
 * reconcile toggles as a class on the mounted bubble (`applyPulse`), NOT
 * part of an item's intrinsic HTML. Baking it into the string would change
 * the HTML every time the pulse moved, forcing an `innerHTML` rewrite that
 * recreates the `.bubble` node and restarts its animation (the reset a
 * reader sees as a jerk). The two never land on the same bubble anyway: a
 * final response only exists once the turn has ended, and the breath only
 * runs while it has not.
 *
 * The bubble is stamped with the time the agent OPENED the block (the
 * `text-start` envelope), not the time it closed it: the stamp then dates
 * the response the same way the user bubble's dates the prompt, and it
 * does not jump while the block streams.
 */
function TextStream(
  item: TextItem,
  chip: ResultItem | null,
  panels?: PanelContext,
): string {
  // The async-quiescence invariant: a bubble owning LIVE async wears the
  // amber border and lists that work as selectable badges inside it (see
  // AsyncCatalog). Amber outranks the green final-response — the answer is
  // landed but its background work is not — and flips to green once every
  // member settles (amber → green quiescence). A bubble the projection does
  // not host carries no members, so it never goes amber.
  const liveAsync = hasLiveAsync(item.blockId, panels);
  const stateCls = liveAsync ? " async-live" : chip ? " final-response" : "";
  const cls = `bubble assistant md${stateCls}`;
  // The catalog rides EVERY host bubble, not just a final one: an interrupted
  // or tools-only turn hosts its survivors too (asyncByBubble), so a bubble
  // with no chip can still own live async that must be enumerated in it. The
  // gns-sockets fold stays the completed-answer full-segment view.
  const catalog = AsyncCatalog(item.blockId, panels);
  const gns = chip ? GnsPanel(item.blockId, panels) : "";
  // A completed turn's stats ride in the bubble's top-right corner
  // (`resultMeta`): its elapsed time and the input tokens it spent — the
  // progress footer's two turn-scoped figures, which clear to `--` at turn
  // end because this is where they land. A sub-second turn reports no time
  // worth showing and a turn that spent no new input reports no token
  // figure, so either half may be absent and the corner may render empty.
  const meta = chip ? resultMeta(chip) : "";
  const usage = responseUsageMeta(item.tokenUtilization);
  // Chess-game markers split the body FIRST: they must work inside a
  // TLDR-tree response too, and the tree renderer below never sees
  // markdown handling. Each text segment then picks its own pipeline.
  const body = splitChessGameSegments(item.text, !item.done)
    .map((seg) => {
      if ("path" in seg) return chessGameContainerHtml(seg.path);
      // A bare metaprompt TLDR tree renders as hanging-indent tree lines (the
      // markdown pipeline would shear its wrapped branches to column 0). The
      // model sometimes wraps the tree in stray prefix/postfix prose or a
      // fenced block despite the format, so carve out the tree's line bounds
      // and render only that region as a tree, keeping any surrounding lines
      // on the markdown path (fenced trees stay the fence handler's job —
      // findTreeRegion skips fenced lines).
      const region = findTreeRegion(seg.text);
      if (region) {
        const before = region.before.trim() === "" ? "" : renderMarkdown(region.before);
        const after = region.after.trim() === "" ? "" : renderMarkdown(region.after);
        return `${before}<div class="mp-tree">${renderTreeHtml(region.tree, inline)}</div>${after}`;
      }
      // A header-led segment that yielded no tree region is a postprocessing
      // misfire — surface it once per bubble so such cases stay visible.
      warnTreeMisfire(item.blockId, item.done, seg.text);
      return renderMarkdown(seg.text);
    })
    .join("");
  return Bubble(cls, `${body}${catalog}${gns}`, item.ts, `${meta}${usage}`);
}

/** Dense response accounting, with unavailable values named instead of hidden. */
function responseUsageMeta(records: import("./frontend-proto.js").TokenUtilization[] | undefined): string {
  if (records === undefined || records.length === 0) return "";
  return records.map((r) => {
    const u = r.usage;
    const rate = (value: number | undefined): string => value === undefined ? "unavailable" : `${(value * 100).toFixed(1)}%`;
    const cache = u.cacheCreation;
    const rates = u.cacheRates;
    const timing = r.responseTiming;
    const tps = timing?.outputGenerationDurationMs && timing.outputGenerationDurationMs > 0 ? `${(1000 * u.outputTokens / timing.outputGenerationDurationMs).toFixed(1)} tok/s` : "unavailable";
    const ttft = timing?.timeToFirstTokenMs === undefined ? "unavailable" : `${timing.timeToFirstTokenMs} ms`;
    const provenance = r.actor === "subagent" ? `subagent ${r.subagent?.agentId || "unavailable"} ${r.subagent?.subagentType || "unavailable"}` : "main agent";
    const summary = `${provenance} · ${r.model || "model unavailable"} · uncached ${u.inputTokens} · output ${u.outputTokens} · cache read ${u.cacheReadInputTokens} · cache write ${u.cacheCreationInputTokens} (5m ${cache?.ephemeral5mInputTokens ?? "unavailable"}, 1h ${cache?.ephemeral1hInputTokens ?? "unavailable"}) · hit ${rate(rates?.cacheHitRate)} · write ${rate(rates?.cacheWriteRate)} · uncached rate ${rate(rates?.uncachedInputRate)} · tier ${u.serviceTier || "unavailable"} · speed ${u.speed || "unavailable"} · geo ${u.inferenceGeo || "unavailable"} · generation ${tps} · TTFT ${ttft}`;
    const complete = {
      agent_repl_session_id: r.agentReplSessionId,
      claude_session_id: r.claudeSessionId,
      root_turn_id: r.rootTurnId,
      api_request_id: r.apiRequestId ?? null,
      api_message_id: r.apiMessageId,
      model: r.model,
      actor: r.actor,
      subagent: r.subagent ?? null,
      input_tokens: u.inputTokens,
      output_tokens: u.outputTokens,
      cache_read_input_tokens: u.cacheReadInputTokens,
      cache_creation_input_tokens: u.cacheCreationInputTokens,
      cache_creation: u.cacheCreation ?? null,
      server_tool_use: u.serverToolUse ?? null,
      service_tier: u.serviceTier || null,
      speed: u.speed || null,
      inference_geo: u.inferenceGeo || null,
      output_details: u.outputDetails ?? null,
      iterations: u.iterations,
      cache_diagnostic: u.cacheDiagnostic ?? null,
      cache_rates: u.cacheRates ?? null,
      fallback_credit: u.fallbackCredit ?? null,
      unmodeled_usage: u.unmodeledUsage ?? null,
      raw_sdk_usage: u.rawUsage,
      response_timing: r.responseTiming ?? null,
    };
    return `<details class="response-usage"><summary>${escapeHtml(summary)}</summary><pre>${escapeHtml(JSON.stringify(complete, null, 2))}</pre></details>`;
  }).join("");
}

function Thinking(item: ThinkingItem): string {
  // Adaptive-thinking models withhold the thinking text: the block streams
  // a signature and no thinking_delta, so item.text stays empty. A
  // disclosure triangle over an empty <pre> unfolds to nothing, so a
  // textless block draws NO inline card at all: while it is open its
  // `thinking…` beat moves to the bottom-pinned tail slot beside the
  // working/retrying rows (now the progress footer), and once it
  // closes it disappears entirely (`rendersEmpty`). It stays non-empty here
  // while live so the tail scan can still name it for that slot.
  if (item.text === "") return "";
  // While the summary still streams, the disclosure carries the same orange
  // arc the textless indicator spins, beside the `(thinking…)` label — so a
  // texted thinking block reads as live rather than as a finished card that
  // merely happens to be open.
  const state = item.done
    ? ""
    : ` (thinking${animatedEllipsis()}) <span class="thinking-spinner" aria-hidden="true"></span>`;
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
   * The rendered `/status` panel, when the GUI has real support for it
   * (a `getStatus` action is wired). A refused `/status` renders THIS in
   * place of the generic unsupported card; absent, that command falls back
   * to the card like any other. Renderer-owned so the async fetch's result
   * survives the card's per-frame re-renders.
   */
  statusCard?: string;
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
      // Deliberately partial: every other kind (user-turn, result, system,
      // failure, context-cleared, context-compacted, session-command)
      // legitimately contributes no ticker line, so the default is the common
      // case, not a drift signal.
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
 * The resolver context this renderer's PanelContext backs: children come
 * back visibility-filtered (exactly what a panel would draw), tails from
 * the poller when one is wired. Panel-less renders still resolve, so a
 * bare card keeps its face and Stop control.
 */
function memberCtx(panels?: PanelContext): MemberContext {
  return {
    children: (id) => (panels?.children.get(id) ?? []).filter(visibleChild),
    taskTail: (id) => panels?.taskTail?.(id),
  };
}

/** The one truncation rule every face label wears. */
function capLabel(label: string, max: number): string {
  return label.length > max ? `${label.slice(0, max - 1)}…` : label;
}

/**
 * The one status→badge vocabulary every streaming face renders. Exported
 * for direct testing of the off-enum default (a status value outside
 * `MemberStatus`, e.g. a newer daemon's addition this build predates).
 */
export function memberBadge(status: MemberStatus, inputDone = true): string {
  switch (status) {
    case "running":
      return `<span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>${
        inputDone ? "running…" : "streaming input…"
      }</span>`;
    case "done":
      return `<span class="badge ok">done</span>`;
    case "error":
      return `<span class="badge err">error</span>`;
    case "killed":
      return `<span class="badge err">stopped</span>`;
    default:
      log("warn", `memberBadge: unexpected status ${String(status)}`, { operation: "render.member-badge-status-invalid", dedupKey: "member-badge-status", context: { status } });
      return "";
  }
}

/**
 * The shared Stop control, every face's top-right verb: bare label, with
 * the prompt-mediated caveat in the tooltip (the send asks the agent to
 * run TaskStop and spends a real turn, since no daemon-native kill
 * exists). One button per member, stopping every id the member spawned;
 * gone once the member settles.
 */
function stopButton(member: StreamMember): string {
  if (member.settled || member.taskIds.length === 0) return "";
  const ids = member.taskIds.join(", ");
  const one = member.taskIds.length === 1;
  const prompt = `Stop the background task${one ? "" : "s"} ${ids} (TaskStop), then confirm ${
    one ? "it" : "they"
  } stopped.`;
  return `<button type="button" class="face-stop" data-send-prompt="${escapeHtml(
    prompt,
  )}" title="asks the agent to stop ${escapeHtml(ids)} via TaskStop">Stop</button>`;
}

/** The face's right-aligned side: live elapsed, then the Stop verb. */
function faceSide(member: StreamMember | null): string {
  if (!member) return "";
  const elapsed =
    member.elapsedMs !== undefined
      ? `<span class="face-elapsed">${escapeHtml(formatElapsed(member.elapsedMs))}</span>`
      : "";
  const stop = stopButton(member);
  if (elapsed === "" && stop === "") return "";
  return `<span class="face-side">${elapsed}${stop}</span>`;
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
    .map((c) => `<div class="feed-child">${renderItem(c, panels?.selections, undefined, panels)}</div>`)
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

// A running tool card carries a slow wash under the head's fast arc (its own
// CSS, not the bubble breath), so a call in flight is never a wholly still
// card during the second the arc stays hidden. The two motions live on
// different channels — the arc in the badge, the wash on the card background
// — so they read as one signal, not two competing ones.
function ToolCard(
  item: ToolItem,
  panels?: PanelContext,
  tabBar = "",
): string {
  const variant = SPECIAL_TOOLS.has(item.toolName) ? item.toolName : "Generic";
  // The head badge is the member's FACE when the call streams: one status
  // vocabulary (memberBadge), notification-aware, so a spawning card reads
  // running until its detached work settles rather than done the moment
  // the spawn call returned. A non-streaming call reads the same
  // vocabulary off its own result phase. In-flight is ONE look either
  // way: the orange run badge carrying the same arc the thinking
  // indicator spins, held invisible for its first second by CSS (see
  // .tool-spinner) so the sub-second tools never flash it.
  const member = resolveMember(item, memberCtx(panels));
  const phase: MemberStatus = member
    ? member.status
    : item.result
      ? item.result.isError
        ? "error"
        : "done"
      : "running";
  const status = memberBadge(phase, item.inputDone);
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
  // TABBAR, when a consecutive-run group hands one in, is the row of member
  // chips — rendered as the card's FIRST child so the tabs sit INSIDE the
  // bubble at its top, rather than floating above it (see groupHtml).
  return `
    <div class="tool-card tool-${variant.toLowerCase()}">
      ${tabBar}
      <div class="tool-head"><span class="tool-name">${escapeHtml(item.toolName)}</span>${status}${permBadge}${faceSide(member)}</div>
      ${cardContent(item, { item, member, progress, panels })}
    </div>`;
}

// ---------------------------------------------------------------------------
// Card content presenters
// ---------------------------------------------------------------------------
//
// EVERY async card wears the SAME chrome and differs only in what it puts
// under it. ToolCard above owns the chrome — the variant class, the tab bar,
// the name, the status badge, the permission badge, the face — and nothing
// below this line may add to it. What varies is CONTENT, and each kind's
// content is one pure function of the card's inputs, looked up by tool name.
//
// The registry is what makes the family a family. Before it, "what does an
// agent card show that a skill card does not" was answerable only by reading
// one 60-line function for the conditionals sprinkled through it, and adding a
// kind meant adding another conditional to the middle of the shared body. Here
// a kind is an entry, its content is its own function, and a kind that adds
// nothing says so by not appearing.

/** Everything a content presenter is given. Pure in, HTML out. */
interface CardContext {
  item: ToolItem;
  /** The stream member backing this card, when the call streams one. */
  member: StreamMember | null;
  /** The pre-rendered progress line (chrome-adjacent, but content-placed). */
  progress: string;
  panels?: PanelContext;
}

type CardPresenter = (c: CardContext) => string;

/**
 * The content every card has: its input, its progress line, its member's
 * stacked folds, its result, and the composer for any agent it spawned.
 *
 * The folds render in Shape A order (child feed above the detached stream),
 * each through the one MemberFold — this is where an agent's panels and a
 * watcher's subfeeds actually come from, for every kind, not just subagents.
 */
function baseCardContent(c: CardContext): string {
  const folds =
    c.member !== null
      ? c.member.bodies.map((spec) => MemberFold(c.member as StreamMember, spec, c.panels)).join("")
      : "";
  return `${toolInput(c.item)}
      ${c.progress}
      ${folds}
      ${toolResult(c.item)}
      ${agentComposer(c.item, c.panels)}`;
}

/**
 * A subagent's card (Task / Agent): the base content, preceded by the agent's
 * own live topbar — the session strip's renderer scoped to THIS agent (see
 * topbar.ts) — sitting right under the head.
 */
function subagentCardContent(c: CardContext): string {
  const topbar = c.panels?.agentTopbar?.(c.item) ?? "";
  return `${topbar}
      ${baseCardContent(c)}`;
}

/**
 * A skill's card: the base content, followed by the skill's own SKILL.md.
 *
 * The body is FOLDED BY DEFAULT and click-expandable, through the one capped
 * -section mechanic every long card section uses (`skill-content` is a
 * CAPPED_CLASSES entry, so expand.ts caps it, the click handler toggles it,
 * and FeedRenderer.render re-applies the open state after every reconcile —
 * which is what keeps a reader's expansion from collapsing under them when
 * the card updates).
 *
 * It renders as MARKDOWN rather than escaped text because a skill IS a
 * markdown document; the same renderer draws assistant prose and markdown Read
 * previews.
 */
function skillCardContent(c: CardContext): string {
  const body = c.item.skillBody;
  if (body === undefined || body === "") return baseCardContent(c);
  return `${baseCardContent(c)}
      <div class="tool-output skill-content skill-content-md">${renderMarkdown(body)}</div>`;
}

/**
 * Content presenter per tool name. A kind absent here has no content of its
 * own and renders the base content — which is the honest statement that it
 * adds nothing, rather than an entry that forwards.
 */
const CARD_PRESENTERS: ReadonlyMap<string, CardPresenter> = new Map<string, CardPresenter>([
  ...[...SUBAGENT_TOOLS].map((name) => [name, subagentCardContent] as const),
  ["Skill", skillCardContent],
]);

/** Dispatch one card's content to its kind's presenter. */
function cardContent(item: ToolItem, c: CardContext): string {
  return (CARD_PRESENTERS.get(item.toolName) ?? baseCardContent)(c);
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
 * One stream body, rendered by the renderer its BodySpec names (§2.6).
 *
 * The single place the generalization lands: adding a newly supported async
 * type is a `format` the daemon already classifies plus an arm here, not a
 * bespoke card. Nothing is forced into a shape its data lacks — a shell's
 * spool is bytes, so it stays a <pre>. The member's tail is already the
 * one-precedence text (stream-member.ts), so no surface re-decides it.
 */
function streamBodyHtml(spec: BodySpec, panels?: PanelContext): string {
  switch (spec.kind) {
    case "child-feed":
      return feedChildren(spec.items, panels);
    case "transcript":
      // Recursing into a parsed stream is the one place depth grows, so
      // the descended context carries the guards a deeper fold reads.
      return transcriptBubbles(spec.text, descend(panels, spec.sourceId));
    case "journal":
      return journalRows(spec.text);
    case "raw":
      return taskOutputPre(spec.text);
  }
}

/** The collapsed face of an async fold: what the work is and whether it runs. */
function asyncFace(source: AsyncSource): string {
  const label = source.label !== undefined && source.label !== "" ? source.label : source.source_id;
  return `${source.kind} · ${capLabel(label, 60)} · ${source.status}`;
}

/**
 * One fold per member body — the unified Panel every expansion goes
 * through (Shape A: a member owning several bodies stacks one fold each).
 *
 * A child-feed body is the activity fold, keyed by the tool-use id as
 * ever. A stream body is the async fold, keyed `async:<toolUseId>` so
 * the two never collide on a card that owns both. Both render through
 * the same Fold skeleton into the same `.agent-panel`; the face reads
 * the MEMBER's one status, so the fold can never disagree with the head
 * badge about liveness. The `mayNest` guard cuts a sourced fold at the
 * depth cap and on a cycle (a nested spawn announcing an ancestor's own
 * id). A source-less raw body (an announcement-less tail) wears the SAME
 * dress with an `output` face, so no stream renders zero-click inline.
 */
function MemberFold(member: StreamMember, spec: BodySpec, panels?: PanelContext): string {
  if (!panels) return "";
  const item = member.item;
  if (spec.kind === "child-feed") {
    return ActivitySection(item.toolUseId, spec.items, panels);
  }
  const source = member.source;
  if (source && !mayNest(panels.depth ?? 0, panels.seenSources, source.source_id)) return "";
  const id = `async:${item.toolUseId}`;
  const arc = member.settled ? "" : `<span class="tool-spinner" aria-hidden="true"></span>`;
  const face = source
    ? asyncFace({ ...source, status: member.status })
    : `output · ${capLabel(toolHeadline(item) !== "" ? toolHeadline(item) : item.toolName, 60)} · ${member.status}`;
  return Fold({
    id,
    foldClass: "async-fold",
    tickerClass: "async-ticker",
    ticker: `${arc}${escapeHtml(face)}`,
    body: () => streamBodyHtml(spec, panels),
    open: panels.isOpen(id),
  });
}

/**
 * The dot hue a member status wears, on every surface that draws one (the
 * catalog badge, the group tab chip): killed reads the error hue, since
 * the dot palette has no third settled color.
 */
function statusDot(status: MemberStatus): "running" | "done" | "error" {
  return status === "running" ? "running" : status === "done" ? "done" : "error";
}

/** A member's badge label: its tool name and its one watcher id, capped. */
function asyncBadgeLabel(item: ToolItem): string {
  const ref = watcherRef(item);
  const label = ref !== null ? `${item.toolName} · ${ref.id}` : item.toolName;
  return capLabel(label, 24);
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
  const member = resolveMember(item, memberCtx(panels));
  const settled = member?.settled ?? false;
  const dot = statusDot(member?.status ?? "done");
  const open = panels?.isOpen(id) ?? false;
  // The spend rides the badge whenever the stream meters one (see
  // transcriptStats): live it ticks with the tail, settled it is the
  // run's total — the collapsed pill's one glanceable progress figure.
  const tokens =
    member?.outputTokens !== undefined
      ? ` <span class="async-badge-tokens">${escapeHtml(compactTokens(member.outputTokens))} tok</span>`
      : "";
  return `<div class="async-badge${settled ? " settled" : ""}${
    open ? " active" : ""
  }" data-panel-toggle="${escapeHtml(id)}"><span class="agent-dot agent-${dot}" aria-hidden="true">●</span> ${escapeHtml(asyncBadgeLabel(item))}${tokens}</div>`;
}

/**
 * The in-bubble async catalog (async-quiescence UX): the bubble's live
 * background work, enumerated as selectable amber badges at its BOTTOM. This
 * is the OTHER half of the invariant the amber border keys off — border and
 * catalog read the SAME projection (`panels.watchers`), so a bubble is amber
 * exactly when it lists live selectable work. The bubble's own liveness reads
 * from the amber border and each badge's live/settled dot; the animated
 * `monitoring…` signal lives ONLY in the topbar strip (see `topbarInfoHtml`),
 * never duplicated inside the bubble.
 *
 * Every host bubble carries it — a final response, an interrupted turn's last
 * text, a tools-only turn's prompt — not just a completed answer. A quiesced
 * catalog (all members settled) keeps its badges as history, its dots going
 * done, exactly as the border flips amber → green. Clicking a badge expands
 * that member's `WatcherRow` — its live tail as nested bubbles
 * (asyncStreamHtml), its stop control, its composer — below the strip.
 */
function AsyncCatalog(hostId: string, panels?: PanelContext): string {
  const members = panels?.watchers?.get(hostId) ?? [];
  if (members.length === 0) return "";
  const badges = members.map((m) => AsyncBadge(hostId, m, panels)).join("");
  // An open badge expands the member's OWN CARD inside the shared panel
  // inset — the very ToolCard the feed renders, so badge context and feed
  // context are one code path (face, folds, Stop, composer, topbar all
  // included) and cannot diverge. The card's folds arrive collapsed,
  // carrying their usual `async:`/tool-use keys, which the feed instance
  // of the same card shares.
  const details = members
    .filter((m) => panels?.isOpen(`member:${hostId}:${m.toolUseId}`) ?? false)
    .map((m) => `<div class="agent-panel"><div class="feed-child">${ToolCard(m, panels)}</div></div>`)
    .join("");
  return `<div class="async-catalog"><div class="async-badges">${badges}</div>${details}</div>`;
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
  // args can run long. The skill's full SKILL.md body is a separate content
  // section the skill presenter draws (see skillCardContent).
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
  // A Skill's raw "Launching skill: <name>" echo adds nothing over the
  // invocation the input already shows, so it is suppressed. The skill's own
  // SKILL.md is a separate section the skill presenter draws from the
  // daemon-correlated body (skillCardContent), not from this echo. Errors
  // fall through below so a skill that failed to launch stays loud.
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
    const { decision, message } = item.resolution;
    // The reason rides EVERY refusal, not just an abandoned one: a denial
    // carries the daemon's `deny_message`, and dropping it would leave the
    // card stating that permission was refused without saying why.
    const verb = decision === "allow" ? "allowed" : decision === "deny" ? "denied" : "cancelled";
    const label = message ? `${verb} — ${escapeHtml(message)}` : verb;
    return `
      <div class="permission resolved">
        <div class="perm-head">Permission: ${escapeHtml(item.toolName)} <span class="badge ${
          decision === "allow" ? "ok" : "err"
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
  // A pushed core.v1.PermissionItem carries no preview, so it is derived from
  // the tool input rather than leaving the card with nothing to show.
  const p = item.preview ?? previewFromInput(item.toolName, item.input);
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
 * `renderItem` returns the empty node, and the nav scan walks straight past
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
      return (
        body === "" ||
        body === EMPTY_CONTENT_PLACEHOLDER ||
        isInterruptSentinel(body)
      );
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
      // Deliberately partial: only the kinds that CAN be empty are cased.
      // Every other kind (permission, result-with-button, failure,
      // context-cleared, context-compacted, session-command) always renders
      // something, so the default is the common answer, not a drift signal
      // worth logging.
      return false;
  }
}

/** A context increase as a signed figure: `+100,000`, `-40,000`, `+0`. */
function formatTokenDelta(n: number): string {
  return `${n < 0 ? "" : "+"}${formatTokens(n)}`;
}

/**
 * A final response's top-right corner content: THE PROGRESS FOOTER'S TWO
 * TURN-SCOPED FIGURES, stamped onto the answer the turn landed on — how long
 * the turn ran in the topbar's time color, then the new input tokens it fed
 * the model in the topbar's token color, bullet-separated (`5s · 63.5k in`).
 *
 * This is where those figures LIVE once a turn concludes. The footer clears
 * both to `--` at turn end (see `closeTurnLocked` and `tokenCellHtml`) because
 * a settled turn's summary belongs beside its answer, not in the chrome that
 * reports the running one — and because a stamp on a persisted conversation
 * item is REPLAYED, so a reloaded session still shows what each turn cost,
 * which the footer's ephemeral view never could.
 *
 * Both halves come off the result item the daemon persists, so neither has to
 * be re-derived from live state:
 *
 * - the duration is `durationMs`, the SDK's own figure for the turn — which
 *   is the span the footer's clock cell measured, rather than the span since
 *   the PREVIOUS turn ended (that would bill this turn for however long the
 *   user spent reading before sending the prompt);
 * - the tokens are `turnInputTokens(usage)` — a result's `usage` is that
 *   turn's own usage, not a session-cumulative snapshot, which is exactly the
 *   scope the footer's ticker has.
 *
 * Either half may be absent: a sub-second turn reports no elapsed worth a
 * figure (the same one-second floor the old closing chip used), and a turn
 * that spent no new input reports no token figure rather than a `0 in`. An
 * all-absent corner returns "" and the bubble shows only its hover timestamp.
 */
function resultMeta(chip: ResultItem): string {
  const parts: string[] = [];
  if (chip.durationMs >= 1000) {
    parts.push(`<span class="turn-dur">${escapeHtml(formatDurationCeil(chip.durationMs))}</span>`);
  }
  const input = turnInputTokens(chip.usage);
  if (input > 0) {
    parts.push(`<span class="turn-in">${escapeHtml(`${compactTokens(input)} in`)}</span>`);
  }
  if (parts.length === 0) return "";
  // Group both stats under one flex item so the `·` separator sits inline
  // between them: the `.turn-meta` column would otherwise stack the duration,
  // the bare bullet text node, and the delta onto three separate lines.
  return `<span class="turn-stats">${parts.join(" · ")}</span>`;
}

/**
 * A turn's closing chip: how long the turn took, then where the session's
 * input tokens now stand and how far this turn moved them.
 *
 * DURATION-MS is the span the chip reports: the SDK's whole-task figure
 * (`durationMs`). It is passed in rather than read off the item so this
 * renderer never has to know which span its caller means.
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

/**
 * A CLEAR, as the feed's loudest rule.
 *
 * The red bar is the entire visual, because the event is the entire fact: a
 * clear carries no summary, no token counts and no trigger. Everything above
 * it has already left the feed (see `itemsFromClearOrCompact`), so the rule
 * opens the
 * conversation rather than dividing it.
 */
function ClearDivider(_item: ContextClearedItem): string {
  return `<div class="clear-divider" role="separator" aria-label="context cleared"></div>`;
}

/**
 * A COMPACTION: the summary that stands in for everything the compaction
 * discarded, and beneath it the boundary marker — the orange rule and the
 * stamp naming it.
 *
 * The summary is a response bubble with a GREEN border on a PURPLE wash —
 * deliberately the loudest bubble in the feed. It is the only surviving
 * account of the conversation above it, so it is load-bearing content rather
 * than a decorative footnote, and a reader scrolled to the top of a compacted
 * session must be able to find it instantly. A compaction that summarized
 * nothing renders the rule and the stamp alone; an empty bubble would claim
 * an account exists.
 *
 * The TRIGGER is deliberately absent from the output. A compaction is a
 * compaction, and whether the system or the user asked for one changes
 * nothing a reader can act on — so `auto` and `manual` render identically.
 *
 * There is NO failure branch. A compaction reaches this end only by way of a
 * `compact_boundary` line in the vendor transcript, and that line is written
 * once the compaction has COMPLETED — a failed one leaves no line, so the file
 * plane that produces this item structurally cannot report a failure here.
 */
function CompactDivider(item: ContextCompactedItem): string {
  // The marker — the orange rule with its stamp under it — sits AFTER the
  // summary bubble, closing the account of the discarded history rather than
  // introducing it: the reader meets what survived first, then the line that
  // says where the old context ended. Within the marker the rule still
  // precedes the label, mirroring the red rule a clear draws (see
  // `ClearDivider`).
  const tokens = `${formatTokens(item.preTokens)} → ${formatTokens(item.postTokens)} tokens`;
  const summary =
    item.summary !== ""
      ? `<div class="bubble assistant md compact-summary"><div class="bubble-body">${renderMarkdown(
          item.summary,
        )}</div></div>`
      : "";
  return (
    summary +
    `<div class="compact-rule" role="separator" aria-label="context compacted"></div>` +
    `<div class="compact-divider">— context compacted (${tokens}) —</div>`
  );
}

/**
 * A daemon-classified failure, as a bordered card in the feed.
 *
 * It is where a user whose workspace changed color finds out WHY. Before it
 * there was nowhere for that account to live: the degraded banner was chrome
 * that scrolled away, a refused command rendered nothing at all, and an API
 * failure rendered a one-line grey badge built from a code no one set.
 *
 * The border color comes from the failure's CLASS, from the same table the
 * workspace takes its color from — so a purple workspace can never be
 * explained by a card of some other color.
 *
 * A RESOLVED card renders as settled: a check, the closing time, and the
 * alarm styling dropped. The window ended, and a card that went on shouting
 * about it would be lying about the present to be accurate about the past.
 */
function SystemFailureBubble(item: SystemFailureCard): string {
  const resolved = item.resolvedAtMs > 0;
  const cls = `failure-card failure-${item.errorClass.toLowerCase()}${resolved ? " resolved" : ""}`;
  const mark = resolved ? "✓" : "✕";
  const detail = item.sourceDetail
    ? `<div class="failure-detail">${escapeHtml(item.sourceDetail)}</div>`
    : "";
  const stamp = resolved
    ? `<div class="failure-resolved">resolved ${escapeHtml(formatClockTime(item.resolvedAtMs))}</div>`
    : "";
  const structured = failureDetailHtml(item.detail);
  return (
    `<div class="${cls}" data-error-type="${escapeHtml(item.errorType)}">` +
    `<div class="failure-head"><span class="failure-mark">${mark}</span>` +
    `<span class="failure-message">${escapeHtml(item.message)}</span></div>` +
    detail +
    structured +
    stamp +
    `</div>`
  );
}

/** Render the structurally complete detail union carried by every failure card. */
function failureDetailHtml(detail: SystemFailureCard["detail"]): string {
  switch (detail.kind) {
    case "none":
      return "";
    case "sessionResume":
      return resumeFailureHtml(detail.value);
    case "queryTermination":
      return queryTerminationFailureHtml(detail.value);
  }
}

/** Render every query identity and the exact typed termination cause. */
function queryTerminationFailureHtml(failure: import("./frontend-proto.js").QueryTerminationFailure): string {
  const reason = failure.reason.case === "unexpectedEof"
    ? "unexpected EOF"
    : failure.reason.case === "iteratorFailure"
      ? `iterator failure: ${failure.reason.value.cause}`
      : failure.reason.case === "startupFailure"
        ? `startup failure: ${failure.reason.value.cause}`
        : "missing termination reason";
  const vendor = failure.vendorIdentity.case === "vendorSessionId"
    ? failure.vendorIdentity.value
    : failure.vendorIdentity.case === "vendorSessionIdentityUnavailable"
      ? "unavailable before SDK initialization"
      : "missing vendor identity evidence";
  return `<div class="failure-detail">query termination: ${escapeHtml(reason)}<br>agent-repl session: ${escapeHtml(failure.agentReplSessionId)}<br>query instance: ${escapeHtml(failure.queryInstanceId)}<br>vendor session: ${escapeHtml(vendor)}<br>observed_at_ms: ${String(failure.observedAtMs)}</div>`;
}

/** Render resume-continuity evidence without reducing it to a generic death. */
function resumeFailureHtml(failure: import("./frontend-proto.js").SessionResumeFailure): string {
  const attempt = failure.attempt === "create" ? "session creation" : "automatic restoration";
  const header = `<div class="failure-detail">resume blocked during ${escapeHtml(attempt)}<br>conversation: ${escapeHtml(failure.claudeSessionId)}</div>`;
  if (failure.cause.case === "queryTermination") {
    return `${header}${queryTerminationFailureHtml(failure.cause.value)}`;
  }
  const cause = failure.cause.case === "transcriptUnavailable"
    ? `transcript unavailable: ${failure.cause.searchedPaths.join(" | ") || "no readable transcript path"}`
    : failure.cause.case === "identityMismatch"
      ? `identity mismatch: ${failure.cause.replacementClaudeSessionId || "recovery would start a fresh conversation"}`
      : `bring-up failure: ${failure.cause.cause}`;
  return `<div class="failure-detail">resume blocked during ${escapeHtml(attempt)}<br>${escapeHtml(cause)}<br>conversation: ${escapeHtml(failure.claudeSessionId)}</div>`;
}

/** Wall-clock HH:MM:SS for a resolution stamp. */
function formatClockTime(ms: number): string {
  const d = new Date(ms);
  const pad = (n: number): string => String(n).padStart(2, "0");
  return `${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}

function SystemNote(item: SystemItem): string {
  return `<div class="system-note">system: ${escapeHtml(item.subtype)}</div>`;
}

/**
 * The slash form a `SessionCommand` is written as.
 *
 * DERIVED, NEVER RECEIVED. The wire item carries only the enum, so this table
 * is where the text a reader sees comes from — which is precisely why the
 * submitted prompt cannot leak onto this surface: `/model opus` and `/model`
 * both arrive as MODEL and both draw `/model`. The argument the user typed is
 * not on the wire and has nowhere to come from.
 */
const SESSION_COMMAND_LABELS: Record<SessionCommand, string> = {
  CLEAR: "/clear",
  COMPACT: "/compact",
  MODEL: "/model",
  COST: "/cost",
  USAGE: "/usage",
  STATUS: "/status",
  CONTEXT: "/context",
  CONFIG: "/config",
  HELP: "/help",
  DOCTOR: "/doctor",
  LOGIN: "/login",
  LOGOUT: "/logout",
  MEMORY: "/memory",
  PERMISSIONS: "/permissions",
  AGENTS: "/agents",
  MCP: "/mcp",
  HOOKS: "/hooks",
  OUTPUT_STYLE: "/output-style",
  RELEASE_NOTES: "/release-notes",
  TODOS: "/todos",
  EXPORT: "/export",
  ADD_DIR: "/add-dir",
  RESUME: "/resume",
  EXIT: "/exit",
  PRIVACY_SETTINGS: "/privacy-settings",
  STATUSLINE: "/statusline",
  TERMINAL_SETUP: "/terminal-setup",
  VIM: "/vim",
  REWIND: "/rewind",
  BUG: "/bug",
};

/** The slash form of one session command, for display. */
export function sessionCommandLabel(command: SessionCommand): string {
  return SESSION_COMMAND_LABELS[command];
}

/**
 * A SESSION COMMAND the user ran, as a centered chip rather than a bubble.
 *
 * It is deliberately NOT bubble-shaped and deliberately NOT purple. A bubble
 * on the user's side of the feed says "this is what I said to the agent", and
 * the agent never saw `/model` — the CLI answered it locally. The chip says
 * what actually happened: the user acted on the session, and the conversation
 * carries on either side of it.
 *
 * The command name is the entire content, because the item's entire content
 * is the command. There is no argument to show and no prompt text to fall
 * back on: the wire message has one field, and it is this one.
 */
function SessionCommandChip(item: SessionCommandItem): string {
  return `<div class="session-command" role="note"><span class="session-command-name">${escapeHtml(
    sessionCommandLabel(item.command),
  )}</span></div>`;
}

/**
 * One item's HTML, or nothing at all for the items the feed draws no node
 * for (`rendersEmpty`). FINALS pairs the feed's answers with the chips that
 * close their turns: a text block it names renders as a final response
 * carrying its chip, and the paired result renders as nothing, since the
 * bubble above it has already drawn it.
 *
 * The breath is gone entirely with the pulse machinery: the progress footer
 * the reconcile toggles as a class on the mounted bubble (`applyPulse`), so
 * a moving pulse never rewrites an item's HTML. PANELS carries the feed
 * partition's child lists, letting a spawning card fold its confined children
 * into an activity panel (and letting those children recurse through this
 * same function).
 */
export function renderItem(
  item: ConversationItem,
  selections?: QuestionSelections,
  finals?: FinalResponses,
  panels?: PanelContext,
): string {
  if (rendersEmpty(item, finals)) return "";
  switch (item.kind) {
    case "user-turn":
      return item.origin === "merge" ? MergeCard() : UserTurn(item, panels);
    case "text":
      return TextStream(item, finals?.chips.get(item.blockId) ?? null, panels);
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
        // `/status` is refused like any other terminal-only command, but the
        // GUI now renders it richly in place of the generic card (the whole
        // point of this workspace). Other refusals still offer the
        // build-support button.
        if (unsupported === "status" && panels?.statusCard !== undefined) {
          return panels.statusCard;
        }
        return UnsupportedCommandCard(
          unsupported,
          panels?.supportPhases?.get(unsupported),
          panels?.canAddSupport ?? false,
        );
      }
      return ResultChip(item, item.durationMs);
    }
    case "context-cleared":
      return ClearDivider(item);
    case "context-compacted":
      return CompactDivider(item);
    case "session-command":
      return SessionCommandChip(item);
    case "failure":
      return SystemFailureBubble(item);
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
    // Keyed by uuid, not index: a resolved failure replaces its own opening
    // card, and an index key would strand the two on separate DOM nodes.
    case "failure":
      return `failure:${item.uuid}`;
    // Keyed by uuid too: the node must survive the rebuild its own
    // arrival triggers, and an index key would move under it.
    case "context-cleared":
    case "context-compacted":
      return `${item.kind}:${item.uuid}`;
    // Keyed by uuid so a resync's re-push of the SAME invocation (the uuid is
    // derived from the submit's request id) reuses its node instead of drawing
    // the command a second time.
    case "session-command":
      return `session-command:${item.uuid}`;
    // Keyed by the SAME identity the store reconciles a prompt on, so two
    // prompts can never share a DOM node. The index fallback covers a turn
    // carrying neither a request id nor a uuid (fixtures).
    case "user-turn":
      return userTurnKey(item) ?? `user-turn:${index}`;
    default:
      return `${item.kind}:${index}`;
  }
}

/**
 * Identity of the newest user turn in the feed, or null when there is
 * none. A change in this id between renders means a prompt was just
 * sent — from the webapp composer or from the Emacs host's input buffer
 * alike, since both reach the feed as the daemon's `user-turn`
 * broadcast — which is what re-pins a scrolled-up feed to its tail.
 *
 * The identity is the DOM key, NOT the bare request id: every prompt the real
 * pipeline delivers has an empty request id, so reading that left this
 * constant across a whole session — no prompt ever counted as fresh, and the
 * feed never re-pinned to the tail on send.
 */
export function lastUserTurnId(items: readonly ConversationItem[]): string | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn") return itemKey(item, i);
  }
  return null;
}

/** The newest user turn ITEM in the feed, or null when there is none. */
export function lastUserTurnItem(items: readonly ConversationItem[]): UserTurnItem | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn") return item;
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
 * discarded by a clear or a compaction.
 */
export function planToolReveal(
  items: readonly ConversationItem[],
  toolUseId: string,
): ToolReveal | null {
  const visible = itemsFromClearOrCompact(items);
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
      // The chip resolves the SAME member record every other streaming
      // surface reads (stream-member.ts), so a background agent's chip
      // tracks its detached work rather than going green at spawn time —
      // the call's own result decides only for members with nothing
      // detached, which is what the resolver's fallback already encodes.
      const member = resolveMember(m, memberCtx(panels));
      const status = member
        ? statusDot(member.status)
        : m.result
          ? m.result.isError
            ? "error"
            : "done"
          : "running";
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
 *
 * A freeze (the user reading a nested view they opened, see `freezeOnToggle`)
 * suppresses tail-following even from a pinned position, so streaming output
 * cannot yank the view off what they are reading. A fresh prompt still wins
 * over the freeze: sending it is an explicit ask to watch the answer.
 */
export function repinsToTail(opts: {
  prevTurnId: string | null;
  nextTurnId: string | null;
  pinned: boolean;
  frozen: boolean;
}): boolean {
  if (opts.nextTurnId !== null && opts.nextTurnId !== opts.prevTurnId) return true;
  if (opts.frozen) return false;
  return opts.pinned;
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
 * The panels that open ALONGSIDE a toggled-open panel. An async badge's
 * detail mounts the member's card with its stream behind the card's own
 * `async:` fold, and a stream the user must open TWICE reads as "doesn't
 * stream" — so opening the badge seeds the stream fold open with it.
 * Closing the badge leaves the seed alone: it is a first-open convenience,
 * not a lock, and the user's own later toggles win. The member key's tool
 * use id is its last segment (`member:<host>:<toolUseId>`), split from the
 * right because a host id's shape is not ours to constrain.
 */
export function panelSeedsOnOpen(id: string): string[] {
  if (!id.startsWith("member:")) return [];
  const toolUseId = id.slice(id.lastIndexOf(":") + 1);
  return toolUseId === "" ? [] : [`async:${toolUseId}`];
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
  return (panels?.watchers?.get(hostId) ?? []).some((m) => memberLive(m, panels));
}

/** Whether ITEM resolves to a still-live stream member. */
function memberLive(item: ToolItem, panels?: PanelContext): boolean {
  const member = resolveMember(item, memberCtx(panels));
  return member !== null && !member.settled;
}

/**
 * Whether ANY async member anywhere in the feed is still live — the global
 * `monitoring…` row's gate (see `showsMonitoringRow`). Read over the whole
 * item list rather than one host's members, so a live member that outlived
 * its turn still lights the row even when its own bubble is scrolled off or
 * was never hosted (an orphan the projection could not place). ITEMS is the
 * TRUNCATED feed, so a member a clear or a compaction discarded no longer
 * counts.
 */
export function anyLiveAsync(
  items: readonly ConversationItem[],
  panels?: PanelContext,
): boolean {
  return items.some((i) => isWatcher(i) && memberLive(i, panels));
}

/**
 * Whether ANY thinking block in the feed is still live (open, not yet done) —
 * the `monitoring…` row's secondary gate (see `showsMonitoringRow`). A live
 * thinking block renders its own `thinking…` spinner (`Thinking`), whether it
 * is a main-chain block or a background subagent's parented one, so the amber
 * monitoring fallback stands down while one is present rather than stacking a
 * second progress row beneath it. Pass the VISIBLE items (not the raw list):
 * a thinking block folded away behind a gns panel draws no on-screen
 * `thinking…`, so it must NOT suppress the very fallback the fold hid the work
 * behind — an invisible `thinking…` is not a `thinking…` the user can see.
 */
export function anyLiveThinking(items: readonly ConversationItem[]): boolean {
  return items.some((i) => i.kind === "thinking" && !i.done);
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
  /**
   * Tail-following frozen because the user opened a nested view to read it
   * (see `freezeOnToggle`). While set, a render never parks the feed at its
   * tail, so streaming output cannot yank the view off the opened content;
   * scrolling back to the tail (or sending a fresh prompt) lifts it.
   */
  private tailFrozen = false;
  /** Tab pins per group key; an unpinned group auto-follows the newest runner. */
  private activeTabs = new Map<string, string>();
  /** Half-typed agent messages, keyed by agent id (see agentComposer). */
  private msgDrafts = new Map<string, string>();
  private lastState: StoreState | null = null;
  /**
   * Whether the session is IDLE yet live async continues somewhere in the
   * feed — the amber monitoring signal, now the sidebar's breathing dot on
   * the session's own row (see `WorkspaceSidebar.setMonitoring`) rather than
   * topbar text or a feed-tail row. Recomputed from `showsMonitoringRow` at
   * the tail of every real render/renderRestored, and read back by the
   * chrome paint through `isMonitoring`. Defaults false so a chrome paint
   * landing before the first feed render claims nothing to monitor.
   */
  private monitoring = false;
  /** Pending bottom-up fill steps from renderRestored, oldest last. */
  private backfillQueue: Array<() => void> = [];
  /** Newest user turn seen by a render, so the next one spots a fresh send. */
  private lastUserTurn: string | null = null;
  /**
   * Identity of the clear or compaction the feed last truncated at, or null
   * when it drew the whole item list. A render whose boundary MOVED rebuilds
   * the feed from nothing: the truncation shifts every index-based key
   * (`user-turn:N`, `result:N`, …), so reconciling against the old node
   * map would reuse stale elements from above the boundary, out of position.
   */
  private lastClearOrCompactKey: string | null = null;
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
   * The `/status` panel's load state. Renderer state (like supportPhases) so
   * a re-render cannot wipe a landed snapshot back to a spinner. `idle` until
   * a refused `/status` first appears, then `loading` until the account read
   * resolves; the session's pushed `SystemInit` overlays the freshest
   * snapshot on top of whatever that load carried.
   */
  private statusState:
    | { kind: "idle" }
    | { kind: "loading" }
    | { kind: "loaded"; data: StatusResponse }
    | { kind: "error"; error: string } = { kind: "idle" };
  /**
   * Which counter overlay is open on which agent bubble's topbar, keyed
   * by agent id. Renderer state (like openPanels) so the overlay survives
   * the card's per-frame re-renders; at most one entry, mirroring the
   * header's one-overlay-at-a-time rule feed-wide.
   */
  private agentMenus = new Map<string, TopbarMenu>();
  /**
   * Entry keys currently drawn as a cheap placeholder rather than their real
   * render (see lazy-item.ts). Populated ONLY by `renderRestored`, for the
   * replayed history above the tail chunk, and drained as the reader scrolls
   * near — so a live append is never in it and renders in full, exactly as it
   * did before deferral existed.
   */
  private deferred = new Set<string>();
  /**
   * Watches the deferred nodes and says when one has come near enough to be
   * worth its real render. Inert where `IntersectionObserver` is absent, in
   * which case nothing is ever deferred in the first place.
   */
  private upgrader: LazyUpgrader;

  constructor(
    container: HTMLElement,
    actions: Actions,
  ) {
    this.container = container;
    this.actions = actions;
    this.upgrader = new LazyUpgrader(container, (keys) => this.upgradeKeys(keys));
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
      const acceptQ = target.getAttribute("data-queue-accept");
      if (acceptQ !== null) this.actions.acceptQueued(acceptQ);
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
    // A frozen feed (a nested view is open, §freezeOnToggle) resumes
    // tail-following the moment the user scrolls it back to the tail. The
    // renders that DON'T freeze park at the tail themselves, firing this too,
    // but clearing an already-clear freeze is a no-op — so the guard only
    // ever fires on a user scroll that reaches the bottom.
    container.addEventListener("scroll", () => {
      this.tailFrozen = freezeOnScroll(this.tailFrozen, isPinnedToBottom(this.container));
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
   * Reveal the failure card carrying UUID — the progress footer's error row
   * clicking through to the failure its summary came from, which is the only
   * way to find one that has already scrolled off the feed.
   *
   * Answers whether the card was found: false when the uuid names nothing in
   * the current feed (a `/clear` discarded it, or the failure predates this
   * view), which the caller reports rather than silently doing nothing.
   */
  revealError(uuid: string): boolean {
    if (!this.lastState) return false;
    const index = this.lastState.items.findIndex(
      (i) => i.kind === "failure" && i.uuid === uuid,
    );
    if (index === -1) return false;
    const node = this.nodes.get(itemKey(this.lastState.items[index], index))?.el;
    if (!node) return false;
    revealNode(node, "start");
    return true;
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
    const opened = !this.openPanels.has(id);
    if (opened) {
      this.openPanels.add(id);
      for (const seed of panelSeedsOnOpen(id)) this.openPanels.add(seed);
    } else {
      this.openPanels.delete(id);
    }
    // Opening a nested view is the user asking to read it, so freeze the feed
    // off its tail until they scroll back down (see `freezeOnToggle`).
    this.tailFrozen = freezeOnToggle(this.tailFrozen, opened);
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
      statusCard: this.statusCardHtml(),
      // The FULL item list, not the feed's TRUNCATED one: the agent-scoped
      // rosters (agentSubagents / agentTasks) resolve the agent's direct
      // children and its task calls, which can sit anywhere in the session,
      // so a truncated list would drop the ones outside the current feed
      // window.
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
   * The rendered `/status` panel, or undefined when the GUI has no status
   * support wired (no `getStatus` action) — in which case a refused
   * `/status` falls back to the generic unsupported card.
   *
   * Merges three sources: the session's PUSHED `SystemInit` (the freshest,
   * arriving on `sessionInit` frames) preferred over the snapshot the load
   * carried, that load's account, and the store's live model / permission
   * mode.
   */
  private statusCardHtml(): string | undefined {
    if (!this.actions.getStatus) return undefined;
    const st = this.statusState;
    const store = this.lastState;
    const loaded = st.kind === "loaded" ? st.data : null;
    const fromFrame = statusSnapshotFromInit(store?.systemInit ?? null);
    return statusPanelHtml({
      snapshot: fromFrame ?? loaded?.snapshot ?? null,
      account: loaded?.account ?? null,
      model: store?.model ?? "",
      permissionMode: store?.permissionMode ?? "default",
      loading: st.kind === "idle" || st.kind === "loading",
      error: st.kind === "error" ? st.error : undefined,
    });
  }

  /**
   * Kick off the `/status` fetch the first time a refused `/status` appears.
   *
   * Loaded once (gated on `idle`): it only fetches the account. The snapshot
   * half needs no load at all — the pushed `SystemInit` already reflects any
   * mid-session change, and overlays whatever this load carried.
   */
  private maybeRequestStatus(state: StoreState): void {
    if (!this.actions.getStatus) return;
    if (this.statusState.kind !== "idle") return;
    const hasStatusRefusal = state.items.some(
      (i) => i.kind === "result" && parseUnsupportedCommand(i.resultText) === "status",
    );
    if (hasStatusRefusal) this.requestStatus();
  }

  /**
   * Load the panel's inputs. There is no re-probe to trigger: the snapshot
   * half comes off the push plane and is already as fresh as the daemon's own
   * view, so only the account half is actually fetched. Its failure IS
   * surfaced, on the panel, as an error state.
   */
  private requestStatus(): void {
    const get = this.actions.getStatus;
    if (!get) return;
    if (this.statusState.kind !== "idle") return;
    this.statusState = { kind: "loading" };
    void get().then(
      (data) => {
        this.statusState = { kind: "loaded", data };
        this.rerender();
      },
      (err: unknown) => {
        this.statusState = {
          kind: "error",
          error: err instanceof Error ? err.message : String(err),
        };
        this.rerender();
      },
    );
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
   * Point the poller at the sources that need polling: those in
   * currently-open folds — at EVERY depth, via the recursive walk
   * openSubfeedSourceIds shares with the renderer's own guards — UNIONED
   * with every still-live member's poll source regardless of fold state
   * (livePollSourceIds), so a collapsed badge keeps receiving the done
   * flag and the tail its settling and token figures read. Called after
   * every render so opening a fold starts its polls and closing one stops
   * them (a nested fold's tail discovered by a poll feeds the next sync);
   * a no-op when no daemon fetch was wired.
   */
  private syncWatcherPolls(
    watchers: ReadonlyMap<string, readonly ToolItem[]>,
    panels: PanelContext,
  ): void {
    const ids = openSubfeedSourceIds({
      items: this.lastState?.items ?? [],
      watchers,
      isOpen: (id) => this.openPanels.has(id),
      tailText: (id) => this.watcherPoller?.tail(id)?.text,
    });
    for (const id of livePollSourceIds(watchers, memberCtx(panels))) ids.add(id);
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

  /**
   * The one item ENTRY stands for: a lone item is itself, and a tab group is
   * whichever member its card is currently showing — the member whose text a
   * placeholder must carry, since that is the text the full render would put
   * in the DOM for the search to walk.
   */
  private entryItem(entry: FeedEntry): ConversationItem {
    if (entry.kind === "item") return entry.item;
    const active = activeGroupMember(entry.members, this.activeTabs.get(this.entryKey(entry)));
    return entry.members.find((m) => m.toolUseId === active) ?? entry.members[0];
  }

  /**
   * One grouped-feed entry's HTML: the item's own, or its group's card —
   * unless the entry is still DEFERRED, in which case its cheap placeholder
   * stands in (see lazy-item.ts) and neither markdown nor highlighting runs.
   */
  private entryHtml(
    entry: FeedEntry,
    finals: FinalResponses,
    panels: PanelContext,
  ): string {
    if (this.deferred.has(this.entryKey(entry))) {
      return placeholderHtml(this.entryItem(entry));
    }
    if (entry.kind === "group") {
      const active = activeGroupMember(entry.members, this.activeTabs.get(this.entryKey(entry)));
      return groupHtml(entry.members, active, panels);
    }
    return this.itemHtml(entry.item, finals, panels);
  }

  /**
   * Promote the named entries out of placeholder form and repaint. Called by
   * the upgrader when they have scrolled near; a key already promoted (a
   * clear rebuilt the feed under the observer, say) is simply not a change,
   * so a stale batch cannot cost a render.
   */
  private upgradeKeys(keys: readonly string[]): void {
    let changed = false;
    for (const key of keys) {
      if (this.deferred.delete(key)) changed = true;
    }
    if (changed) this.rerender();
  }

  /**
   * Render every deferred entry in full, now.
   *
   * The feed search's contract is that it sees the conversation's DOM text
   * (search.ts), and a placeholder is a REDUCTION of the item it stands for —
   * it carries the item's prose but not a tool card's chrome, its tab chips,
   * or the folds `unsearchedRegions` counts. So starting a search drains the
   * deferral first, and the search then walks exactly the DOM it always did.
   * A no-op once nothing is deferred, which is the steady state.
   */
  upgradeAll(): void {
    if (this.deferred.size === 0) return;
    log("info", `feed: upgrading ${this.deferred.size} deferred item(s) in full`, {
      operation: "render.lazy-upgrade-all",
    });
    this.deferred.clear();
    this.upgrader.reset();
    this.rerender();
  }

  /**
   * Draw EL as ENTRY's placeholder: flag it for the stylesheet's
   * `content-visibility` rule and stamp the estimated height that rule sizes
   * the skipped box with, then hand it to the upgrader to watch.
   */
  private deferEntry(el: HTMLElement, entry: FeedEntry): void {
    const key = this.entryKey(entry);
    this.deferred.add(key);
    el.classList.add(DEFERRED_CLASS);
    el.style.setProperty(HEIGHT_VAR, `${estimateHeightPx(itemPlainText(this.entryItem(entry)))}px`);
    this.upgrader.watch(el);
  }

  /**
   * Whether ENTRY may be drawn as a placeholder at all: heavy enough to be
   * worth it, carrying text to stand in for it, and not one of the items the
   * feed draws nothing for — an empty placeholder would mount a sized box
   * where `.feed-item:empty` hides the real render entirely.
   */
  private deferrableEntry(entry: FeedEntry, finals: FinalResponses): boolean {
    const item = this.entryItem(entry);
    return (
      isHeavyItem(item) && !rendersEmpty(item, finals) && itemPlainText(item) !== ""
    );
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
   * turn's answer, and carrying its activity panel when PANELS holds children
   * for it. The breath is NOT part of the HTML — the reconcile toggles it as
   * a class after mounting (see `applyPulse`).
   */
  private itemHtml(
    item: ConversationItem,
    finals: FinalResponses,
    panels: PanelContext,
  ): string {
    return renderItem(item, this.questionSelections, finals, panels);
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
   *
   * Invoked bare from the rAF coalescer (see coalesce.ts), so a
   * deterministic throw mid-build would otherwise re-throw every frame
   * with no evidence beyond the console — a silent feed-freeze. Logged
   * (deduped so a steady throw logs once, not every frame) then
   * rethrown unchanged: the caller's behavior is untouched, but the
   * failure now reaches the daemon log. clearDedup on a clean run
   * re-arms the key so a later recurrence logs again.
   */
  renderRestored(state: StoreState): void {
    try {
      this.renderRestoredImpl(state);
    } catch (err) {
      log("error", String(err), { operation: "render.restored-failed", dedupKey: "feed-render-restored", context: { cause: err } });
      throw err;
    }
    clearLogDedup("feed-render-restored");
  }

  private renderRestoredImpl(state: StoreState): void {
    this.lastState = state;
    // Replayed history's newest prompt is old news: banking it here keeps
    // the next render from reading it as a fresh send and yanking a feed
    // the user has meanwhile scrolled up.
    this.lastUserTurn = lastUserTurnId(state.items);
    this.backfillQueue = [];
    releaseChessGames(this.container);
    this.container.innerHTML = "";
    this.nodes.clear();
    // The nodes the upgrader was watching are being discarded wholesale, so
    // the deferral is re-decided from scratch below rather than carried.
    this.deferred.clear();
    this.upgrader.reset();
    // A clear or a compaction clears the screen: the feed opens on its own
    // rule and the discarded turns are not drawn at all. On a REPLAY the
    // daemon has already floored the history at the newest of the two, so this
    // slice is usually the identity; it runs anyway because a restore can also
    // be asked to redraw a list a live event has since moved. The boundary is
    // banked so the next live render reconciles instead of pointlessly
    // rebuilding the feed this method just built.
    const items = itemsFromClearOrCompact(state.items);
    this.lastClearOrCompactKey = clearOrCompactKey(items);
    // The gns-sockets fold: bridge upkeep leaves the top feed and every
    // turn-shaped projection (finals, watchers), so the green
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
    this.syncWatcherPolls(watchers, panels);
    const finals = finalResponses(visible);
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
    // The global monitoring signal, on the same idle-with-live-async terms
    // render() computes it (see `showsMonitoringRow`), so a fresh join landing
    // on a quiescent-but-still-watching session sees its sidebar dot breathe
    // immediately (read back by the chrome paint through `isMonitoring`). A
    // visible `thinking…` spinner suppresses it here too, so the two never
    // both claim a live slot.
    this.monitoring = showsMonitoringRow({
      turnInFlight: state.turnInFlight,
      interrupting: state.interrupting,
      thinking: anyLiveThinking(visible),
      anyLiveAsync: anyLiveAsync(items, panels),
    });
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
        const html = this.entryHtml(entry, finals, panels);
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
    // Lazy heavy rendering (lazy-item.ts). The NEWEST chunk — the one filled
    // synchronously below, and the region the feed is about to park on —
    // renders in full; everything above it is drawn as a placeholder until the
    // reader scrolls near. Decided BEFORE any chunk is filled, since the fill
    // is what asks `entryHtml` which form each entry takes.
    if (canDeferItems()) {
      const tail = new Set(chunks.length > 0 ? chunks[0] : []);
      let count = 0;
      shells.forEach(({ el, entry }, i) => {
        if (tail.has(i) || !this.deferrableEntry(entry, finals)) return;
        this.deferEntry(el, entry);
        count++;
      });
      log("info", `feed: replay deferred ${count}/${shells.length} item(s) to first view`, {
        operation: "render.lazy-defer",
      });
    }
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
   * Whether the session is monitoring live async while idle — the amber
   * `monitoring…` gate the last real render computed (see `showsMonitoringRow`).
   * The chrome paint reads it back to render the topbar strip's left-most
   * datapoint (see `sessionTopbarDatapoints`), keeping the signal a projection
   * of the feed the renderer already partitioned rather than a re-derivation.
   */
  isMonitoring(): boolean {
    return this.monitoring;
  }

  /**
   * Invoked bare from the rAF coalescer (see coalesce.ts), so a
   * deterministic throw mid-reconcile would otherwise re-throw every
   * frame with no evidence beyond the console — a silent feed-freeze
   * leaving the feed half-painted forever. Logged (deduped so a steady
   * throw logs once, not every frame) then rethrown unchanged: the
   * caller's behavior is untouched, but the failure now reaches the
   * daemon log. clearDedup on a clean run re-arms the key so a later
   * recurrence logs again.
   */
  render(state: StoreState): void {
    try {
      this.renderImpl(state);
    } catch (err) {
      log("error", String(err), { operation: "render.feed-failed", dedupKey: "feed-render", context: { cause: err } });
      throw err;
    }
    clearLogDedup("feed-render");
  }

  private renderImpl(state: StoreState): void {
    this.flushBackfill();
    this.lastState = state;
    // A refused `/status` turns this feed into the status panel's host: kick
    // off its one-time fetch before building HTML so this very frame shows
    // the loading state rather than the bare card.
    this.maybeRequestStatus(state);
    const turnId = lastUserTurnId(state.items);
    // A fresh prompt re-follows the tail: it lifts any nested-view freeze so
    // the sender watches the answer (repinsToTail lets the fresh turn win too).
    if (turnId !== null && turnId !== this.lastUserTurn) {
      this.tailFrozen = false;
      // The prompt round-trip's LAST receipt: this render is drawing a user
      // turn the previous one had not seen. `last` reports whether it ranks
      // at the feed tail — the position a just-sent prompt must land at, and
      // the newest user turn IS the tail exactly when the tail item is one.
      // `request_id` stays on the line even though it is empty for every
      // transcript-borne prompt: KEY is the identity that actually
      // distinguishes them, and the pair is what pins the attribution gap.
      const tail = state.items[state.items.length - 1];
      const turn = lastUserTurnItem(state.items);
      log(
        "info",
        `feed: user turn rendering request_id=${turn?.requestId ?? ""} key=${turnId} last=${
          tail !== undefined && tail.kind === "user-turn"
        }`,
        { operation: "webapp.render.user-turn" },
      );
    }
    const toTail = repinsToTail({
      prevTurnId: this.lastUserTurn,
      nextTurnId: turnId,
      pinned: isPinnedToBottom(this.container),
      frozen: this.tailFrozen,
    });
    this.lastUserTurn = turnId;
    // A clear or a compaction clears the screen: only that event and what
    // follows it render. This is the LIVE case the truncation exists for — one
    // landing over an already-drawn feed. A boundary that just MOVED rebuilds
    // the feed from nothing: the truncation shifts every index-based key, so
    // reconciling would reuse stale elements from above it, out of position.
    const items = itemsFromClearOrCompact(state.items);
    const boundary = clearOrCompactKey(items);
    if (boundary !== this.lastClearOrCompactKey) {
      this.container.innerHTML = "";
      this.nodes.clear();
      // Every watched node just went with the feed. What the rebuild mounts
      // is drawn in full: a boundary move is rare, and this render is the
      // live path, where nothing is ever deferred (see `deferred`).
      this.deferred.clear();
      this.upgrader.reset();
    }
    this.lastClearOrCompactKey = boundary;
    // The gns-sockets fold: bridge upkeep leaves the top feed and every
    // turn-shaped projection (finals, watchers), so the green
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
    this.syncWatcherPolls(watchers, panels);
    const finals = finalResponses(visible);
    const seen = new Set<string>();
    // Walks the container as the desired order is emitted, so each node can be
    // slotted at its rank. `null` means "next goes at the very front".
    let prevNode: ChildNode | null = null;
    for (const feedEntry of groupFeed(top)) {
      const key = this.entryKey(feedEntry);
      seen.add(key);
      const html = this.entryHtml(feedEntry, finals, panels);
      let entry = this.nodes.get(key);
      if (!entry) {
        const el = document.createElement("div");
        el.className = "feed-item";
        el.dataset.key = key;
        entry = { el, html: "" };
        this.nodes.set(key, entry);
      }
      // Slot the node at its `groupFeed(top)` rank rather than only appending
      // new ones at the tail. render() reuses mounted nodes in place, and a
      // bare append is correct ONLY while every new entry is the newest item —
      // the live-stream invariant. A gap-fill revisit (§2.10) reconciles a
      // whole backlog burst at once, where a batched entry can belong ABOVE an
      // already-mounted node; without this move it strands at the tail, which
      // is the reordering `renderRestored` gets for free by rebuilding from
      // scratch. The `!==` guard makes the steady-state live path a no-op, so
      // an in-order breathing bubble is never moved (nor its animation reset).
      const desiredNext: ChildNode | null = prevNode
        ? prevNode.nextSibling
        : this.container.firstChild;
      if (entry.el !== desiredNext) this.container.insertBefore(entry.el, desiredNext);
      prevNode = entry.el;
      // An upgraded entry drops the placeholder marking with the placeholder
      // itself, so the stylesheet stops skipping its box and stops sizing it
      // from an estimate the real render has just superseded.
      if (!this.deferred.has(key) && entry.el.classList.contains(DEFERRED_CLASS)) {
        entry.el.classList.remove(DEFERRED_CLASS);
        entry.el.style.removeProperty(HEIGHT_VAR);
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
    // The global `monitoring…` signal: the amber fallback for when the owning
    // bubble is scrolled off, shown only while the session is idle and live
    // async continues (mutually exclusive with the bucket-1 tail above, whose
    // rows all mean the main chain is active). A visible `thinking…` spinner —
    // a subagent mid-thought while the main chain idles — also suppresses it,
    // so the more-specific live signal owns the live slot alone. It now paints
    // as the sidebar's breathing amber dot (read back by the chrome paint
    // through `isMonitoring`) rather than a feed-tail row.
    this.monitoring = showsMonitoringRow({
      turnInFlight: state.turnInFlight,
      interrupting: state.interrupting,
      thinking: anyLiveThinking(visible),
      anyLiveAsync: anyLiveAsync(items, panels),
    });
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
