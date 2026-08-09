/**
 * async-render — drawing one `AsyncBubble` as WHAT IT IS.
 *
 * async-bubble.proto's whole point is that detached work is not a notice that
 * something is happening elsewhere, so this module draws each kind as its own
 * thing and never as a generic "background task" row:
 *
 * - an AGENT bubble is a conversation, rendered with THE SAME renderers the
 *   top-level feed uses. That identity is the contract, not a convenience: the
 *   emissions in the bubble are `AgentEmission`s, the same message the feed
 *   carries, so they go through the same decomposition and the same
 *   `renderItem`. There is no second, weaker transcript renderer here — the
 *   feed's is injected (see {@link AsyncRenderContext.renderEmissions}), which
 *   is what makes "the same code" checkable rather than aspirational;
 * - a WORKFLOW JOURNAL is a row table with a status arm per row, because a
 *   journal is a record log rather than a conversation;
 * - a SHELL and an UNCLASSIFIED bubble are byte SPOOLS with a header naming the
 *   command line, or the tool the daemon could not classify. Their bytes stay
 *   in a `<pre>` — the spool is deliberately unparsed, and pretending otherwise
 *   is how a renderer starts guessing at ANSI, line framing and JSON.
 *
 * UNCLASSIFIED IS DRAWN, ALWAYS. It is real work the user started; dropping it
 * would make a running process invisible. What this module may not do is guess:
 * an unrecognized tool is never quietly drawn as a shell, an agent or a
 * workflow, and the tool's own name is what labels it.
 *
 * NESTING IS BY LOOKUP, NOT BY RECURSION INTO PAYLOADS. Bubbles form a tree
 * through `parent_bubble_id` POINTERS, so a child is found with one registry
 * lookup. The only recursion here walks that pointer graph, guarded by a
 * visited set, so a cyclic pointer set (which only a daemon bug could produce)
 * fails loudly instead of hanging the renderer.
 */

import type {
  AsyncBubble,
  AsyncFold,
  AsyncLiveness,
  AsyncOutputSpool,
  AsyncSettled,
  AsyncWorkflowJournalRow,
} from "./async-bubble.js";
import type { AsyncBubbleRegistry } from "./async-routing.js";
import type { UnwrappedEmission } from "./agent-emission.js";
import { Fold, capLabel } from "./fold.js";
import { escapeHtml } from "./highlight.js";
import { log } from "./wslog.js";

/** What the renderer needs from the surfaces around it. */
export interface AsyncRenderContext {
  /** The open bubbles — the ONLY place a child or a parent is looked up. */
  registry: AsyncBubbleRegistry;
  /**
   * THE FEED'S OWN RENDERER for a detached agent's emissions.
   *
   * Injected rather than imported so that this module cannot grow a private
   * transcript renderer: whatever the top-level feed draws an `AgentEmission`
   * as, a bubble draws it as too, because it is the same function. It is also
   * what keeps the dependency one-way — `render.ts` draws bubbles, so a static
   * import back into it would be a cycle.
   *
   * BUBBLEID scopes the synthesized item identities to their bubble, so two
   * bubbles' emissions can never collide on a DOM key.
   */
  renderEmissions: (emissions: readonly UnwrappedEmission[], bubbleId: string) => string;
  /** Whether a fold with this id is open; folds default closed. */
  isOpen?: (id: string) => boolean;
}

/** The dot hue a bubble's liveness wears, in the feed's existing vocabulary. */
function livenessDot(liveness: AsyncLiveness): "running" | "done" | "error" {
  if (liveness.case === "live") return "running";
  // A killed bubble reads the error hue: the dot palette has no third settled
  // colour, and "stopped from outside" is closer to error than to done.
  return liveness.value.outcome.case === "done" ? "done" : "error";
}

/**
 * The one-line verdict a settled bubble carries.
 *
 * The exit status is drawn BESIDE the outcome rather than derived into it: the
 * daemon resolved `outcome` from `shell_exit` and put both on the wire so a
 * shell's card can say "exited 137" instead of showing an unexplained red dot.
 * Re-deriving the verdict here would be this end making a mapping the contract
 * assigns to the daemon.
 */
function settledFace(settled: AsyncSettled): string {
  const exit = settled.shellExit === undefined ? "" : ` · exited ${settled.shellExit.code}`;
  switch (settled.outcome.case) {
    case "done":
      return `done${exit}`;
    case "error":
      // Empty when the source reported failure without a reason — never filled
      // with a manufactured one.
      return settled.outcome.message === "" ? `failed${exit}` : `failed${exit} · ${settled.outcome.message}`;
    default:
      return settled.outcome.reason === "" ? `stopped${exit}` : `stopped${exit} · ${settled.outcome.reason}`;
  }
}

/** Live-or-settled, as the collapsed face says it. */
function livenessFace(liveness: AsyncLiveness): string {
  return liveness.case === "live" ? "running" : settledFace(liveness.value);
}

/** What a bubble calls itself: its label, or its id when the daemon had none. */
export function bubbleLabel(bubble: AsyncBubble): string {
  return bubble.label !== "" ? bubble.label : bubble.id;
}

/**
 * The EARLIER-ENTRIES notice a capped fold draws.
 *
 * `droppedBefore === 0` means the fold is complete and nothing is drawn — which
 * is exactly why the count is on the wire: a fold that silently drops its
 * oldest entries is indistinguishable from a complete one, and the daemon
 * resolving the cap is what keeps two frontends from disagreeing about what the
 * user is being shown.
 */
export function earlierEntriesNotice(fold: AsyncFold, bubbleId: string): string {
  if (fold.droppedBefore === 0) return "";
  log("warn", `async-render: the daemon's ${fold.tailCap}-entry tail cap left ${fold.droppedBefore} earlier entry/entries out of bubble ${bubbleId}`, {
    operation: "async-render.fold-capped",
    dedupKey: `async-fold-capped:${bubbleId}`,
    context: { bubble_id: bubbleId, dropped_before: fold.droppedBefore, tail_cap: fold.tailCap },
  });
  return `<div class="stream-dropped">… ${fold.droppedBefore} earlier ${
    fold.droppedBefore === 1 ? "entry" : "entries"
  } not shown (daemon tail cap ${fold.tailCap})</div>`;
}

/** One journal row: a dot, a name and a detail line. */
function journalRow(row: AsyncWorkflowJournalRow): string {
  // The status ARM maps to the dot vocabulary; a status added to the proto
  // later arrives as an arm a reader must handle rather than an integer this
  // end renders as something else.
  const dot = row.status === "running" ? "running" : row.status === "done" ? "done" : "error";
  return `<div class="stream-row"><span class="agent-dot agent-${dot}" aria-hidden="true">●</span> <span class="tool-name">${escapeHtml(
    row.label,
  )}</span><span class="stream-detail">${escapeHtml(row.detail)}</span></div>`;
}

/** A spool's bytes, verbatim, with the header that says what produced them. */
function spoolBody(header: string, spool: AsyncOutputSpool): string {
  const head = header === "" ? "" : `<div class="stream-row"><span class="tool-name">${escapeHtml(header)}</span></div>`;
  // An empty spool still renders its header: "this ran and has said nothing
  // yet" is a fact, and a blank panel would read as a broken fold.
  const body = spool.text === "" ? "" : `<pre class="tool-output task-live-output">${escapeHtml(spool.text)}</pre>`;
  return `${head}${body}`;
}

/** The kind-specific body of one bubble. */
function bubbleBody(bubble: AsyncBubble, ctx: AsyncRenderContext): string {
  switch (bubble.kind.case) {
    case "agent": {
      const { emissions, fold } = bubble.kind.value;
      // The feed's renderer, not a second one. See renderEmissions.
      return `${earlierEntriesNotice(fold, bubble.id)}${ctx.renderEmissions(emissions, bubble.id)}`;
    }
    case "journal": {
      const { rows, fold } = bubble.kind.value;
      return `${earlierEntriesNotice(fold, bubble.id)}${rows.map(journalRow).join("")}`;
    }
    case "shell":
      // The command line is the header, verbatim. Empty only when the daemon
      // launched the process without a reconstructible one.
      return spoolBody(bubble.kind.value.command, bubble.kind.value.output);
    default:
      // The TOOL NAME is what makes the unclassified arm useful rather than
      // merely honest: it tells a maintainer which tool needs a
      // classification, and labels the bubble with something truer than
      // "unknown".
      return spoolBody(`unclassified tool: ${bubble.kind.value.toolName}`, bubble.kind.value.output);
  }
}

/** The fold id one bubble's panel toggles on. */
export function bubbleFoldId(bubbleId: string): string {
  return `bubble:${bubbleId}`;
}

/**
 * Draw one bubble, and beneath it the bubbles it spawned.
 *
 * VISITED is the cycle guard on the parent-pointer walk. A bubble reached twice
 * means the pointers form a loop, which no legal tree does; it is reported and
 * the branch is cut rather than recursed into forever.
 */
export function AsyncBubbleCard(
  bubble: AsyncBubble,
  ctx: AsyncRenderContext,
  visited: ReadonlySet<string> = new Set(),
): string {
  if (visited.has(bubble.id)) {
    log("error", `async-render: bubble ${bubble.id} is its own ancestor — the parent_bubble_id pointers form a cycle, so the branch is cut rather than recursed into`, {
      operation: "async-render.parent-cycle",
      dedupKey: `async-parent-cycle:${bubble.id}`,
      context: { bubble_id: bubble.id, parent_bubble_id: bubble.parentBubbleId },
    });
    return `<div class="stream-dropped">… ${escapeHtml(bubble.id)} already drawn above (parent pointers form a cycle)</div>`;
  }
  const seen = new Set(visited).add(bubble.id);
  const id = bubbleFoldId(bubble.id);
  const open = ctx.isOpen?.(id) ?? false;
  const dot = livenessDot(bubble.liveness);
  const arc = bubble.liveness.case === "live" ? `<span class="tool-spinner" aria-hidden="true"></span>` : "";
  const face = `${bubble.kind.case} · ${capLabel(bubbleLabel(bubble), 60)} · ${livenessFace(bubble.liveness)}`;
  // Children are resolved by POINTER through the registry — one lookup, no
  // walk into any payload.
  const children = ctx.registry
    .children(bubble.id)
    .map((child) => `<div class="feed-child">${AsyncBubbleCard(child, ctx, seen)}</div>`)
    .join("");
  return Fold({
    id,
    foldClass: "async-fold",
    tickerClass: "async-ticker",
    ticker: `${arc}<span class="agent-dot agent-${dot}" aria-hidden="true">●</span> ${escapeHtml(face)}`,
    body: () => `${bubbleBody(bubble, ctx)}${children}`,
    open,
  });
}

/**
 * The bubble a tool card owns, or "" when it owns none.
 *
 * SPAWNEDBUBBLEID is the daemon's CLASSIFICATION VERDICT, matched against the
 * registry and never derived from anything else. Absent or empty means "this
 * call detached nothing" and ONLY that, so it draws nothing rather than
 * searching for a plausible bubble.
 */
export function AsyncBubbleForCall(spawnedBubbleId: string | undefined, ctx: AsyncRenderContext): string {
  if (spawnedBubbleId === undefined || spawnedBubbleId === "") return "";
  const bubble = ctx.registry.bubbleForSpawn(spawnedBubbleId);
  if (bubble === null) {
    // The verdict named a bubble the registry does not hold — the open has not
    // arrived, or a resync dropped it. Said plainly rather than papered over
    // with an invented placeholder bubble.
    log("warn", `async-render: tool card names bubble ${spawnedBubbleId}, which is not open — nothing is drawn for it`, {
      operation: "async-render.unmatched-verdict",
      dedupKey: `async-unmatched-verdict:${spawnedBubbleId}`,
      context: { spawned_bubble_id: spawnedBubbleId },
    });
    return "";
  }
  return AsyncBubbleCard(bubble, ctx);
}

/**
 * Every bubble that is not attached to a card: the tree's roots, plus the
 * ORPHANS whose parent pointer resolves to nothing.
 *
 * An orphan is listed rather than silently promoted to a root. Its work is
 * real and the user should see it, but drawing it as top-level would assert a
 * tree position the daemon never claimed.
 */
export function AsyncBubbleForest(ctx: AsyncRenderContext): string {
  const roots = ctx.registry.roots().map((b) => AsyncBubbleCard(b, ctx));
  const orphans = ctx.registry.orphans().map(({ bubble, missingParentId }) => {
    log("warn", `async-render: bubble ${bubble.id} points at parent ${missingParentId}, which is not open — it is drawn as a detached branch rather than promoted to a root`, {
      operation: "async-render.orphan-bubble",
      dedupKey: `async-orphan:${bubble.id}`,
      context: { bubble_id: bubble.id, missing_parent_id: missingParentId },
    });
    return `<div class="async-orphan"><div class="stream-dropped">parent ${escapeHtml(
      missingParentId,
    )} not delivered</div>${AsyncBubbleCard(bubble, ctx)}</div>`;
  });
  return [...roots, ...orphans].join("");
}
