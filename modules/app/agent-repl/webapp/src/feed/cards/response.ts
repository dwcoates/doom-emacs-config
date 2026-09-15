/**
 * response — the agent's prose bubble, left-rail.
 *
 * THE ARM IS THE STATE and the row upserts through it: `update` while the prose
 * is still arriving, `success` once it settled, `error` when the turn died
 * mid-arrival. A turn holds SEVERAL of these — multiple responses per turn is
 * the normal case — so nothing here assumes it is the only one, and the
 * final-answer treatment is not this module's to apply: `turn-ended.ts` names
 * the answering row and marks it with its own `FINAL_RESPONSE_CLASS`.
 *
 * THE DRAW IS RECORDED, AND THE RECORD IS `feed.draw-response`. The agent's
 * answer appearing in the feed is an action a person watches for, so it cannot
 * be the one drawn thing with nothing but per-fragment DEBUG behind it: before
 * this record the only evidence a response had been drawn at all was
 * `feed.final-answer-marked` finding a `.bubble.assistant` to style, which says
 * a row exists and never says what it holds. The level follows the module's own
 * rule — INFO for the draws a reader would ask about, DEBUG for the loop body:
 *
 *   - the FIRST draw of a row (no `previous` body) is INFO: the bubble appearing;
 *   - a SETTLED draw (`success` or `error`, the row's terminal shape) is INFO:
 *     the answer standing whole, which is the edge a turn's readers wait on;
 *   - every intermediate re-push of an arriving row is DEBUG, because the daemon
 *     re-pushes the WHOLE row on every fragment and those are the fragments.
 *
 * Both carry the same fields, so one reader reads either: `characters`, the
 * prose the row drew, and `blocks`, how many prose blocks it drew — ONE, always,
 * because the daemon folds a response's fragments into a single bubble row (its
 * "THE PROSE FOLD"). The field is stated rather than assumed so a reader can
 * tell a fold that grew a second block from a schema they misread.
 *
 * THE ERROR ARM CARRIES NO REASON, deliberately: it says only that this bubble
 * is the one the death cut short. WHY the turn died is `turn_ended.errored`'s to
 * draw, so a reason sentence here would be the same fact worded twice, in two
 * places that could disagree.
 *
 * THE NOTICE REGISTER RIDES EVERY STATE TOO, and it is a FIELD, not an arm:
 * `notice` says the prose is a vendor-synthesized remark (an interruption note,
 * a system-inserted line) rather than the agent's own words, which is orthogonal
 * to whether it is still arriving. So it is read from presence in all three
 * states and draws the daemon's heading above the prose in the quiet register
 * the stylesheet already uses for system notes. Absent = the ordinary prose
 * bubble, unchanged. The client holds NO notice vocabulary: the heading is
 * composed by the daemon and drawn verbatim.
 *
 * THE USAGE STAMP RIDES EVERY STATE. It is meaningful while arriving (the vendor
 * states usage when a response opens and restates it as it grows), final once
 * settled, and the last observed figure on a broken bubble — so the corner is
 * drawn from presence alone, in every arm. Absent means NO STAMP, never a zero:
 * "we were not told what it cost" and "it cost nothing" are different claims.
 *
 * THE TYPE-OUT IS THE CLIENT'S PACING, and it RESUMES ACROSS PUSHES. The daemon
 * accumulates the fragments and re-pushes the whole prose; the feed core redraws
 * the row whole on each push. If the reveal restarted there, a steadily growing
 * response would re-type itself from the top several times a second. So the
 * shown length is carried on the element the previous draw returned
 * (`data-revealed`) and the new draw resumes from it — the one channel a
 * renderer has for state that must outlive a redraw.
 */
import type {
  FeedResponse,
  FeedResponseError,
  FeedResponseNotice,
  FeedResponseProse,
  FeedResponseSuccess,
  FeedResponseUpdate,
  FeedResponseUsageStamp,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../../log.js";
import { bubbleScroll } from "../bubble-scroll.js";
import { formatAge } from "../../duration.js";
import { renderMarkdown, inline } from "../../markdown.js";
import {
  DEFAULT_TREE_COLS,
  findTreeRegion,
  renderTreeHtml,
  type TreeIssue,
} from "../../metaprompt-tree.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { SmoothReveal } from "../../smooth.js";
import { onDiscard, tick } from "../ticking.js";
import type { RowContext } from "./context.js";

/** The attribute the shown length is carried on across a redraw. */
export const REVEALED_ATTRIBUTE = "data-revealed";

/**
 * How many prose blocks one response row draws.
 *
 * The daemon folds every fragment of a response into ONE bubble row and
 * re-pushes the whole row (daemon/internal/resolve/feed/response.go, "THE PROSE
 * FOLD"), and `FeedResponseProse` is a single markdown string — so a drawn
 * response is exactly one block. It is a named constant rather than a literal
 * because the record states it, and a reader of the record is entitled to know
 * where the number comes from.
 */
const RESPONSE_PROSE_BLOCKS = 1;

/** The prose bubble. */
export function drawFeedResponse(u: FeedResponse, rc: RowContext): HTMLElement {
  const path = "FeedResponse";
  const result = requireCase(u.result, `${path}.result`);

  const bubble = document.createElement("div");
  bubble.className = "bubble assistant md";
  bubble.setAttribute("data-state", result.case);

  // THE METADATA STRIP COMES FIRST, and the scroll box under it (owner ruling,
  // 2026-09-14). The corner stamp is a full-width strip above the prose rather
  // than a column beside it, so the body's scrollbar starts BENEATH the token
  // figure instead of running the bubble's whole height next to it.
  const corner = document.createElement("span");
  corner.className = "turn-meta";
  if (u.usage !== undefined) {
    corner.appendChild(drawFeedResponseUsageStamp(u.usage, rc, `${path}.usage`));
  }
  bubble.appendChild(corner);

  // BEFORE the body, and outside it: the body is rewritten whole by the prose
  // painters (and by every frame of the type-out), so a heading placed inside it
  // would be wiped by the first repaint of an arriving response.
  if (u.notice !== undefined) {
    bubble.classList.add("response-notice");
    bubble.setAttribute("data-notice", "");
    bubble.appendChild(drawFeedResponseNotice(u.notice, `${path}.notice`));
  }

  // The body is the CONTENT WRAPPER; the element appended to the bubble is the
  // scroll box that holds it (see bubble-scroll.ts).
  const body = document.createElement("div");
  body.className = "bubble-body";
  bubble.appendChild(bubbleScroll(body));

  let characters: number;
  switch (result.case) {
    case "update":
      characters = drawFeedResponseUpdate(result.value, rc, `${path}.update`, bubble, body);
      break;
    case "success": {
      const markdown = drawFeedResponseSuccess(result.value, `${path}.success`);
      paintWhole(body, markdown);
      markRevealed(bubble, markdown.length);
      characters = markdown.length;
      break;
    }
    case "error": {
      const markdown = drawFeedResponseError(result.value, `${path}.error`);
      bubble.classList.add("response-cut-short");
      paintWhole(body, markdown);
      markRevealed(bubble, markdown.length);
      bubble.appendChild(cutShortMarker());
      characters = markdown.length;
      break;
    }
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = result;
      return unreachableArm(`${path}.result`, other.case);
    }
  }
  recordDraw(u, rc, result.case, characters);
  return bubble;
}

/**
 * State the draw, at the level the draw deserves.
 *
 * It runs AFTER the body is drawn, never before it: a row whose arm this build
 * cannot read refuses above and must not have claimed a draw on its way out.
 *
 * FIRST IS "NO PREVIOUS BODY", which is the same fact the feed core already
 * uses to decide whether this row has been drawn before — a module-level set of
 * drawn `FeedId`s would be a second answer to that question, and one that
 * outlives the rows it describes (see `RowContext`'s own note on `previous`).
 */
function recordDraw(
  u: FeedResponse,
  rc: RowContext,
  state: "update" | "success" | "error",
  characters: number,
): void {
  const first = rc.previous === undefined;
  const settled = state !== "update";
  const context = {
    row: rc.row.id?.value ?? "unset",
    state,
    characters,
    blocks: RESPONSE_PROSE_BLOCKS,
    first_draw: first,
    settled,
    usage: u.usage !== undefined,
    notice: u.notice !== undefined,
  };
  if (first || settled) {
    log.info(
      settled ? "drew a response bubble settled" : "drew a response bubble for the first time",
      { operation: "feed.draw-response", context },
    );
    return;
  }
  log.debug("redrew an arriving response bubble", {
    operation: "feed.draw-response",
    context,
  });
}

/**
 * The arriving state: the markdown so far, paced.
 *
 * The reveal resumes from the previous draw's shown length and stops the moment
 * it reaches the frontier; a redraw whose prose did not grow therefore does no
 * animation at all.
 *
 * It answers the prose's length — what ARRIVED, not what is shown yet — so the
 * draw record states the same figure in every arm.
 */
export function drawFeedResponseUpdate(
  u: FeedResponseUpdate,
  rc: RowContext,
  path: string,
  bubble: HTMLElement,
  body: HTMLElement,
): number {
  const markdown = drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
  const resumed = revealedSoFar(rc.previous, markdown.length);
  log.debug("drawing an arriving response", {
    operation: "feed.cards.response.update",
    context: { path, length: markdown.length, resumed },
  });
  body.appendChild(arrivingIndicator());
  animate(bubble, body, markdown, resumed, rc);
  return markdown.length;
}

/** The settled state: the whole markdown. */
export function drawFeedResponseSuccess(u: FeedResponseSuccess, path: string): string {
  log.debug("drawing a settled response", {
    operation: "feed.cards.response.success",
    context: { path },
  });
  return drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
}

/** The broken state: the partial markdown, kept on screen. */
export function drawFeedResponseError(u: FeedResponseError, path: string): string {
  log.debug("drawing a response cut short", {
    operation: "feed.cards.response.error",
    context: { path },
  });
  return drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
}

/** The bubble's markdown element: the source, for the prose renderer. */
export function drawFeedResponseProse(u: FeedResponseProse, path: string): string {
  log.debug("reading a response's prose", {
    operation: "feed.cards.response.prose",
    context: { path, length: u.markdown.length },
  });
  return u.markdown;
}

/**
 * The notice register's heading, drawn verbatim above the prose.
 *
 * The daemon composes it because only the daemon knows what kind of remark the
 * prose is; this end states it and adds nothing — no glyph of its own invention,
 * no re-wording, no per-notice branch.
 */
export function drawFeedResponseNotice(u: FeedResponseNotice, path: string): HTMLElement {
  log.debug("drawing a response notice heading", {
    operation: "feed.cards.response.notice",
    context: { path },
  });
  const heading = document.createElement("div");
  heading.className = "response-notice-heading";
  heading.textContent = u.heading;
  return heading;
}

/** The class the corner wears while its timestamp is revealed. */
export const USAGE_REVEALED_CLASS = "usage-corner--revealed";

/**
 * The cost corner: the token figure, drawn verbatim, and — once the response
 * has SETTLED — the relative timestamp it reveals when hovered or focused.
 *
 * THE TWO SIT IN A RIGHT-ANCHORED ROW so the figure stays where it always sat
 * and the timestamp grows in from the RIGHT edge, sliding the figure LEFT to
 * make room (owner ruling, 2026-09-14). The slide is one continuous ~0.5s CSS
 * transition on the timestamp's width and offset, so mouse-leave runs the same
 * transition backwards for free rather than snapping — the stylesheet owns it,
 * and `prefers-reduced-motion` drops it. A state class is toggled here too, so
 * a keyboard focus reveals the same timestamp a hover does.
 *
 * THE TIMESTAMP IS A LIVE CLOCK: it reads `formatAge(now - at_ms)` and repaints
 * once per shared tick, so "5m 30s ago" stays current while it is on screen.
 * The subscription is taken through `tick`, which marks the element, so the
 * feed's teardown of the bubble — a re-push replacing the row, or the turn-end
 * backstop that stops every clock in a settled turn — unsubscribes it with no
 * disposer to remember here.
 *
 * NO TIMESTAMP WHILE ARRIVING: `at_ms` is zero until the response settles, and
 * a corner with no settled instant is the plain figure alone.
 */
export function drawFeedResponseUsageStamp(
  u: FeedResponseUsageStamp,
  rc: RowContext,
  path: string,
): HTMLElement {
  const atMs = u.atMs === 0n ? 0 : msOf(u.atMs, `${path}.at_ms`);
  log.debug("drawing a response usage stamp", {
    operation: "feed.cards.response.usage-stamp",
    context: { path, settled: atMs > 0 },
  });

  const corner = document.createElement("span");
  corner.className = "usage-corner";

  const stamp = document.createElement("span");
  stamp.className = "usage-stamp";
  stamp.textContent = u.text;
  corner.appendChild(stamp);

  if (atMs > 0) {
    const ago = document.createElement("span");
    ago.className = "usage-ago";
    tick(ago, rc.ctx.ticker, (nowMs) => {
      ago.textContent = `${formatAge(nowMs - atMs)} ago`;
    });
    corner.appendChild(ago);

    // A focusable hover target: focus reveals the same timestamp a hover does.
    corner.tabIndex = 0;
    const reveal = (): void => corner.classList.add(USAGE_REVEALED_CLASS);
    const hide = (): void => corner.classList.remove(USAGE_REVEALED_CLASS);
    corner.addEventListener("mouseenter", reveal);
    corner.addEventListener("mouseleave", hide);
    corner.addEventListener("focusin", reveal);
    corner.addEventListener("focusout", hide);
  }

  return corner;
}

/** The webapp surfaces a wrap issue through the client logger. */
const logTreeIssue: TreeIssue = (message, context) => {
  log.debug(message, { operation: "feed.cards.response.tree", context });
};

/**
 * Markdown → HTML, with the metaprompt TLDR tree WRAPPED to COLS columns.
 *
 * The tree arrives unwrapped, one physical line per branch; the wrapper
 * (metaprompt-tree.ts, a port of the daemon's treefmt) breaks every branch too
 * wide for COLS onto continuation lines under its own text column, so the tree
 * fills the bubble and re-flows when the bubble's width changes. Stray prose
 * around the tree keeps the markdown path — which itself wraps a fenced tree to
 * the same COLS — so a model that wrapped the tree in a sentence still reads
 * correctly.
 */
export function proseHtml(markdown: string, cols: number): string {
  const region = findTreeRegion(markdown);
  if (region === null) return renderMarkdown(markdown, cols);
  const before = region.before.trim() === "" ? "" : renderMarkdown(region.before, cols);
  const after = region.after.trim() === "" ? "" : renderMarkdown(region.after, cols);
  return `${before}<div class="mp-tree">${renderTreeHtml(region.tree, inline, cols, logTreeIssue)}</div>${after}`;
}

/**
 * The column limit the tree wraps to: the MAXIMUM content width the bubble may
 * occupy divided by one monospace column, measured in the body's own font so the
 * count is true to the bubble's widest allowed line. A host that cannot lay out
 * (a detached body, a test with no layout engine) yields no measurement and
 * falls back to the default width.
 *
 * REGRESSION WATCH (breadcrumb, per AGENTS.md): this once measured
 * `body.clientWidth` — the body's CURRENT rendered content-box width. But the
 * assistant bubble is `width: fit-content` under a max-width cap
 * (`.bubble.assistant`, `--agent-bubble-cap * 0.85` in styles.css), so
 * `fit-content` shrinks the bubble to its content and `clientWidth` measured the
 * ALREADY-shrunk width — a chicken-and-egg where the tree wrapped to fit the
 * narrow bubble, which kept the bubble narrow, so later trees wrapped
 * prematurely (narrower than an earlier bubble whose lines would fit). The port
 * that moved tree wrapping from the daemon (fixed max width) to the webapp lost
 * the "wrap to the MAX bubble width, never prematurely" invariant. The fix
 * measures against the resolved max-width cap, so `fit-content` sizes the bubble
 * to the true longest line and the tree wraps only when a line exceeds the cap.
 * This is a watch flag, not a lock.
 */
export function measureTreeCols(body: HTMLElement): number {
  const doc = body.ownerDocument;
  const view = doc?.defaultView;
  if (view === null || view === undefined || typeof view.getComputedStyle !== "function") {
    return DEFAULT_TREE_COLS;
  }
  const probe = doc.createElement("div");
  probe.className = "mp-tree";
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre;left:-9999px;top:0;";
  probe.textContent = "0".repeat(100);
  body.appendChild(probe);
  const charPx = probe.getBoundingClientRect().width / 100;
  probe.remove();
  const style = view.getComputedStyle(body);
  const pad = (Number.parseFloat(style.paddingLeft) || 0) + (Number.parseFloat(style.paddingRight) || 0);
  const contentPx = maxBodyContentPx(body, view) ?? body.clientWidth - pad;
  if (!(charPx > 0) || !(contentPx > 0)) return DEFAULT_TREE_COLS;
  return Math.max(1, Math.floor(contentPx / charPx));
}

/**
 * The body's content-box width WHEN THE BUBBLE IS AT ITS CAP — the width the
 * tree must wrap to, not the fit-content width it currently renders at.
 *
 * The cap is the bubble's resolved `max-width` (CSS stays the single source of
 * truth for the cap fraction; nothing here hardcodes it). `box-sizing:
 * border-box` is global (styles.css), so the cap constrains the bubble's BORDER
 * box, which is exactly `bubble.getBoundingClientRect().width`. The horizontal
 * insets from that border box down to the body's content box (the bubble's
 * border and padding, the scroll box's, and the body's padding) do NOT change
 * with content width, so they can be read from the CURRENT geometry and
 * subtracted from the cap: `capPx - (bubbleBorderBox - bodyContent)`.
 *
 * Returns `null` when there is no bubble ancestor, no resolvable cap, or no
 * layout — the caller then falls back to the legacy clientWidth measure (which
 * itself yields the DEFAULT_TREE_COLS fallback under a layout-less host).
 */
function maxBodyContentPx(body: HTMLElement, view: Window): number | null {
  const bubble = body.closest<HTMLElement>(".bubble");
  if (bubble === null) return null;
  const capPx = resolveMaxWidthPx(bubble, view);
  if (capPx === null) return null;
  const style = view.getComputedStyle(body);
  const pad = (Number.parseFloat(style.paddingLeft) || 0) + (Number.parseFloat(style.paddingRight) || 0);
  const bodyContent = body.clientWidth - pad;
  const bubbleBorderBox = bubble.getBoundingClientRect().width;
  if (!(bubbleBorderBox > 0)) return null;
  const insets = bubbleBorderBox - bodyContent;
  const contentPx = capPx - insets;
  return contentPx > 0 ? contentPx : null;
}

/**
 * The bubble's `max-width`, resolved to px. Per CSSOM the resolved value of
 * `max-width` is the COMPUTED value (a percentage stays a percentage, a calc of
 * one percentage serializes as `calc(N%)`), not the used px — so a percentage is
 * resolved here against the bubble's containing block (its parent's content-box
 * width). A browser that hands back px instead is honored directly. Anything
 * else — `none`, an empty value, or a calc mixing units we cannot resolve with a
 * single containing-block multiply — returns `null` so the caller falls back.
 */
function resolveMaxWidthPx(bubble: HTMLElement, view: Window): number | null {
  const mw = view.getComputedStyle(bubble).maxWidth;
  if (mw === "" || mw === "none") return null;
  const px = /^(-?[\d.]+)px$/.exec(mw);
  if (px !== null) return Number.parseFloat(px[1]);
  const pct = /^(?:calc\()?\s*(-?[\d.]+)%\s*\)?$/.exec(mw);
  if (pct !== null) {
    const containing = bubble.parentElement?.clientWidth ?? 0;
    if (containing > 0) return (Number.parseFloat(pct[1]) / 100) * containing;
  }
  return null;
}

/**
 * Re-run REPAINT whenever the body's width changes the column count. A
 * `ResizeObserver` reports the box's new size before paint; the recompute is
 * deferred to an animation frame so a burst of resizes coalesces, and it
 * repaints only when the integer column count actually moved, so a height-only
 * change (the prose growing) does no work. The observer is torn down with the
 * bubble through `stopTicking` (see ticking.ts), so it never outlives the body
 * it watches. A host with no `ResizeObserver` (a test) simply never re-wraps.
 */
function reflowOnResize(body: HTMLElement, repaint: (cols: number) => void, initialCols: number): void {
  const view = body.ownerDocument?.defaultView;
  if (view === null || view === undefined || typeof view.ResizeObserver !== "function") return;
  let lastCols = initialCols;
  let scheduled = false;
  const recompute = (): void => {
    scheduled = false;
    const cols = measureTreeCols(body);
    if (cols === lastCols) return;
    lastCols = cols;
    repaint(cols);
  };
  const observer = new view.ResizeObserver(() => {
    if (scheduled) return;
    scheduled = true;
    const raf = view.requestAnimationFrame?.bind(view);
    if (raf !== undefined) raf(recompute);
    else recompute();
  });
  observer.observe(body);
  onDiscard(body, () => observer.disconnect());
}

/**
 * Write the whole settled prose into the body and keep it wrapped to the live
 * width: paint once at the measured column count, then — ONLY when the prose
 * actually drew a tree — re-wrap on every width change through the SAME path.
 * Plain prose reflows on its own (CSS), so it takes no observer.
 */
function paintWhole(body: HTMLElement, markdown: string): void {
  const cols = measureTreeCols(body);
  body.innerHTML = proseHtml(markdown, cols);
  if (body.querySelector(".mp-tree") !== null) {
    reflowOnResize(
      body,
      (next) => {
        body.innerHTML = proseHtml(markdown, next);
      },
      cols,
    );
  }
}

/** Record the shown length, so the next draw of this row resumes from it. */
function markRevealed(bubble: HTMLElement, length: number): void {
  bubble.setAttribute(REVEALED_ATTRIBUTE, String(length));
}

/**
 * The shown length the previous draw of this row reached, clamped to the prose
 * that has actually arrived.
 *
 * A missing, unparseable or over-long value is treated as "nothing shown yet"
 * respectively "everything shown": both are bounds, not defaults for a value
 * the wire was supposed to carry — the attribute is this renderer's own
 * bookkeeping, never contract data.
 */
export function revealedSoFar(previous: HTMLElement | undefined, length: number): number {
  if (previous === undefined) return 0;
  const raw = previous.getAttribute(REVEALED_ATTRIBUTE);
  if (raw === null) return 0;
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) return 0;
  return Math.min(parsed, length);
}

/** The "still arriving" indicator: the animated ellipsis every live face wears. */
function arrivingIndicator(): HTMLElement {
  const dots = document.createElement("span");
  dots.className = "animated-ellipsis response-arriving";
  dots.setAttribute("aria-hidden", "true");
  return dots;
}

/** The broken bubble's marker. The WHY is the turn's terminal row. */
function cutShortMarker(): HTMLElement {
  const marker = document.createElement("span");
  marker.className = "response-cut-short-marker";
  marker.textContent = "cut short";
  return marker;
}

/**
 * Pace the visible growth from RESUMED up to the whole arrived prose.
 *
 * THE FRAME IS AN ANIMATION FRAME, not the app ticker: the shared ticker steps
 * once a second, which is the right cadence for a clock and useless for a
 * type-out. The loop is self-limiting — it stops as soon as the shown prefix
 * reaches the frontier, and on the first frame that finds the element detached
 * (a re-push replaced it) — so no response leaves a running animation behind.
 * A host with no `requestAnimationFrame` (a test, an unusual embedder) draws
 * the prose whole rather than not at all.
 */
function animate(
  bubble: HTMLElement,
  body: HTMLElement,
  markdown: string,
  resumed: number,
  rc: RowContext,
): void {
  // The arriving prose wraps through the SAME path as the settled whole, at the
  // same live width, so nothing shrinks or re-wraps when the final lands.
  let cols = measureTreeCols(body);
  let lastShown = resumed;
  // Wire the resize re-wrap once a tree has actually been drawn (a resize
  // mid-stream re-wraps the slice already shown), and never for plain prose,
  // which reflows on its own.
  let reflowWired = false;
  const wireReflow = (): void => {
    if (reflowWired || body.querySelector(".mp-tree") === null) return;
    reflowWired = true;
    reflowOnResize(
      body,
      (next) => {
        cols = next;
        paint(lastShown);
      },
      cols,
    );
  };
  const paint = (shown: number): void => {
    lastShown = shown;
    const prose = document.createElement("div");
    prose.innerHTML = proseHtml(markdown.slice(0, shown), cols);
    body.replaceChildren(...prose.childNodes, arrivingIndicator());
    markRevealed(bubble, shown);
    wireReflow();
  };
  const frame = globalThis.requestAnimationFrame?.bind(globalThis);
  if (frame === undefined) {
    log.debug("no animation frame host; drawing the arriving prose whole", {
      operation: "feed.cards.response.no-frames",
      context: { length: markdown.length },
    });
    paint(markdown.length);
    return;
  }

  const reveal = new SmoothReveal({ now: () => rc.ctx.ticker.now() });
  const blockId = "response";
  // Seeding through the module's own "already shown" entry point rather than
  // reaching into its cursor map: this is exactly a restored render, which is
  // what `markShown` exists for.
  reveal.markShown({ items: [{ kind: "text", blockId, text: markdown.slice(0, resumed), done: false }] });
  paint(resumed);

  // A bubble that WAS in the document and no longer is has been replaced by a
  // re-push, and its animation is over. One that was never mounted (a test, a
  // draw into a detached fragment) keeps animating: absence of a document is
  // not the same fact as removal from one.
  let mounted = bubble.isConnected;
  const step = (): void => {
    if (bubble.isConnected) mounted = true;
    else if (mounted) return;
    const shown = reveal.reveal({
      items: [{ kind: "text", blockId, text: markdown, done: false }],
    });
    const item = shown.state.items[0] as { text: string };
    paint(item.text.length);
    if (shown.pending) frame(step);
  };
  frame(step);
}
