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
 * - a MERGE bubble is the same conversation shape, through the same injected
 *   feed renderer, because that identity is what AsyncMergeBubble states in the
 *   contract. What it does NOT share is its chrome: the daemon opened it by
 *   classifying a merge, so its face says `merge` and its fold carries the
 *   merge kind class, which the stylesheet dresses in the SAME amber the Merge
 *   status card and the bounce-lease card wear;
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
 *
 * A CHILD IS DRAWN ONCE, ATTACHED TO THE CARD THAT SPAWNED IT WHERE THERE IS
 * ONE. A conversational bubble's emissions carry the spawning cards, and each
 * of those draws its own bubble through the ordinary card path — so the pointer
 * walk covers only what no card in this bubble draws (see
 * `bubblesDrawnByOwnCards`), rather than putting one piece of live work on
 * screen twice.
 */

import type {
  AsyncBubble,
  AsyncBubbleKindCase,
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
import { SkillBodySection } from "./skill-body.js";
import { asyncAgentItems } from "./state-adapter.js";
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
 * The WORD each kind wears on its collapsed face.
 *
 * A total `Record` over the arm cases rather than the arm string interpolated
 * raw: a kind added to the contract stops compiling here until someone decides
 * what it calls itself, which is the difference between a face that names the
 * work and one that happens to spell whatever the proto field was called. The
 * words are the arm names today because those ARE what this work is called —
 * `merge` included, since the daemon's own classification is what opened it.
 */
export const BUBBLE_KIND_WORD: Readonly<Record<AsyncBubbleKindCase, string>> = {
  agent: "agent",
  journal: "journal",
  shell: "shell",
  unclassified: "unclassified",
  merge: "merge",
  skill: "skill",
};

/**
 * The kind marker class a bubble's fold carries, so the stylesheet can dress
 * one kind without the renderer branching on colour.
 *
 * It is what lets a merge bubble wear the MERGE AMBER (`--merge-border`) the
 * Merge status card and the bounce-lease card already wear, instead of the
 * async teal every other bubble's chrome takes — one merge vocabulary across
 * the surfaces, resolved in CSS rather than by a second face string here.
 */
export function bubbleKindClass(bubble: AsyncBubble): string {
  return `async-kind-${bubble.kind.case}`;
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

/**
 * A CONVERSATIONAL bubble's folded conversation: its earlier-entries notice,
 * then its emissions through the FEED's own renderer.
 *
 * The three conversational kinds — a detached agent, a merge run, a skill
 * window — carry the identical emission shape, which the contract states
 * deliberately, so they draw it here once rather than three times. What differs
 * between them is what LEADS the conversation (a skill's own document) and the
 * chrome around it, not the conversation itself.
 */
function conversationBody(
  bubble: AsyncBubble,
  fold: AsyncFold,
  emissions: readonly UnwrappedEmission[],
  ctx: AsyncRenderContext,
): string {
  return `${earlierEntriesNotice(fold, bubble.id)}${ctx.renderEmissions(emissions, bubble.id)}`;
}

/** The kind-specific body of one bubble. */
function bubbleBody(bubble: AsyncBubble, ctx: AsyncRenderContext): string {
  switch (bubble.kind.case) {
    case "agent":
    case "merge": {
      const { emissions, fold } = bubble.kind.value;
      return conversationBody(bubble, fold, emissions, ctx);
    }
    case "skill": {
      // A skill window is that same conversation, opened by the SKILL's own
      // document: AsyncSkillBubble.body is the skill's contents rather than
      // something the conversation said, so it leads the bubble and the
      // emissions follow. It is drawn through the one skill-body renderer the
      // card path uses, so a reader sees the same section either way.
      const { body, emissions, fold } = bubble.kind.value;
      return `${SkillBodySection(body)}${conversationBody(bubble, fold, emissions, ctx)}`;
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
 *
 * FIXED is set when this bubble is drawn INSIDE a teal card (ASYNC_TEAL_TOOLS
 * in `render.ts`): the whole nested section of a teal card is always open and
 * has no fold affordance, so the bubble renders as a fixed panel and passes
 * the same verdict down to the bubbles it spawned — a nested section with one
 * foldable rung left is exactly the unobvious affordance this removes. A
 * bubble drawn at the top of the feed (`AsyncFeed`) is not in a teal card and
 * folds as it always has.
 */
export function AsyncBubbleCard(
  bubble: AsyncBubble,
  ctx: AsyncRenderContext,
  visited: ReadonlySet<string> = new Set(),
  fixed = false,
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
  const face = `${BUBBLE_KIND_WORD[bubble.kind.case]} · ${capLabel(bubbleLabel(bubble), 60)} · ${livenessFace(bubble.liveness)}`;
  // Children are resolved by POINTER through the registry — one lookup, no
  // walk into any payload — MINUS the ones this bubble's own conversation
  // already draws attached to the card that spawned them. The bubble with no
  // children at all never decomposes its emissions to answer a question about
  // an empty list.
  const pointed = ctx.registry.children(bubble.id);
  const attached = pointed.length === 0 ? EMPTY_ID_SET : bubblesDrawnByOwnCards(bubble, ctx);
  const children = pointed
    .filter((child) => !attached.has(child.id))
    .map((child) => `<div class="feed-child">${AsyncBubbleCard(child, ctx, seen, fixed)}</div>`)
    .join("");
  return Fold({
    id,
    foldClass: `async-fold ${bubbleKindClass(bubble)}`,
    tickerClass: "async-ticker",
    ticker: `${arc}<span class="agent-dot agent-${dot}" aria-hidden="true">●</span> ${escapeHtml(face)}`,
    body: () => `${bubbleBody(bubble, ctx)}${children}`,
    open,
    fixed,
  });
}

/**
 * The bubble(s) a tool card owns, or "" when it owns none.
 *
 * BOTH ENDS OF ONE DAEMON FACT are matched, by exact string equality and
 * nothing else: `AsyncBubble.origin_tool_use_id` against the card's
 * TOOLUSEID, and the card's own `spawned_bubble_id` verdict against the
 * bubble's id. Neither is derived, neither is preferred, and a disagreement
 * between them is reported by `bubbleForCall` rather than resolved — see
 * `async-routing.ts` for why that is not a two-rung identity ladder.
 *
 * Empty on both ends means "this call detached nothing" and ONLY that, so the
 * card draws nothing rather than searching for a plausible bubble.
 *
 * All matching bubbles are drawn. The wire permits several to name one call,
 * and silently drawing the first would hide live work the user started.
 */
export function AsyncBubbleForCall(
  toolUseId: string,
  spawnedBubbleId: string | undefined,
  ctx: AsyncRenderContext,
  fixed = false,
): string {
  return bubblesDrawnForCall(toolUseId, spawnedBubbleId, ctx.registry)
    .map((bubble) => AsyncBubbleCard(bubble, ctx, new Set(), fixed))
    .join("");
}

/**
 * WHICH bubbles a tool card draws — the resolution behind
 * {@link AsyncBubbleForCall}, separated from the drawing because a second
 * caller needs the answer without the HTML: a bubble must not draw a child by
 * pointer that one of its OWN cards is already drawing (see
 * {@link bubblesDrawnByOwnCards}). One function decides it, so the two can
 * never disagree about what is on screen.
 */
export function bubblesDrawnForCall(
  toolUseId: string,
  spawnedBubbleId: string | undefined,
  registry: AsyncBubbleRegistry,
): AsyncBubble[] {
  const byOrigin = registry.bubblesForToolUse(toolUseId);
  if (byOrigin.length > 0) {
    // `bubbleForCall` is what rules on agreement; a contradiction returns null
    // there and nothing is drawn here.
    return registry.bubbleForCall(toolUseId, spawnedBubbleId) === null ? [] : byOrigin;
  }
  if (spawnedBubbleId === undefined || spawnedBubbleId === "") return [];
  const named = registry.bubbleForSpawn(spawnedBubbleId);
  if (named === null) {
    // The verdict named a bubble the registry does not hold — the open has not
    // arrived, or a resync dropped it. Said plainly rather than papered over
    // with an invented placeholder bubble.
    log("warn", `async-render: tool card names bubble ${spawnedBubbleId}, which is not open — nothing is drawn for it`, {
      operation: "async-render.unmatched-verdict",
      dedupKey: `async-unmatched-verdict:${spawnedBubbleId}`,
      context: { tool_use_id: toolUseId, spawned_bubble_id: spawnedBubbleId },
    });
    return [];
  }
  return [named];
}

/** The empty answer, shared so the no-children case allocates nothing. */
const EMPTY_ID_SET: ReadonlySet<string> = new Set();

/**
 * The children this bubble's OWN conversation already draws, attached to the
 * cards that spawned them.
 *
 * A conversational bubble renders its emissions with the feed's own renderer,
 * and a spawning card in there draws its bubble exactly as it would on the
 * top-level feed — which is what "a detached agent's dispatch renders inside
 * the agent that dispatched it" MEANS. Drawing the same child a second time
 * from the parent pointer would put one piece of live work on screen twice, in
 * two places, with two folds that open independently.
 *
 * So the pointer walk is the FALLBACK, not the duplicate: a child whose
 * spawning card is not in this bubble (dropped by the tail cap, or spawned by
 * something the fold never carried) still gets drawn, because losing it would
 * hide running work.
 *
 * WHICH CARDS THE BUBBLE HAS is answered by the ADAPTER'S OWN DECOMPOSITION
 * (`asyncAgentItems`), never by a second walk of the emission payloads here.
 * A tool call reaches a bubble in more than one shape — its own `toolUse` arm,
 * and a `tool_use` block inside an assistant message — and a private walk that
 * knew one shape and not the other would filter exactly the children it fails
 * to recognize. Reading the same items the renderer draws makes the two agree
 * by construction.
 */
function bubblesDrawnByOwnCards(bubble: AsyncBubble, ctx: AsyncRenderContext): ReadonlySet<string> {
  const drawn = new Set<string>();
  const kind = bubble.kind;
  if (kind.case !== "agent" && kind.case !== "merge" && kind.case !== "skill") return drawn;
  for (const item of asyncAgentItems(kind.value.emissions, bubble.id, bubble.startedAtMs).items) {
    if (item.kind !== "tool") continue;
    for (const child of bubblesDrawnForCall(item.toolUseId, item.spawnedBubbleId, ctx.registry)) {
      drawn.add(child.id);
    }
  }
  return drawn;
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
