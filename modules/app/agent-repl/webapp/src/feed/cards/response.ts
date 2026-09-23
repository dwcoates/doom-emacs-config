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
 * accumulates the fragments and re-pushes the whole prose, and each push is
 * drawn IN PLACE over the bubble the previous draw returned (see
 * `drawFeedResponse`). If the reveal restarted there, a steadily growing
 * response would re-type itself from the top several times a second. So the
 * shown length is carried on that element (`data-revealed`) and the new draw
 * resumes from it.
 */
import type {
  FeedResponse,
  FeedResponseError,
  FeedResponseNotice,
  FeedResponseProse,
  FeedResponseSuccess,
  FeedResponseUsageStamp,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../../log.js";
import { BUBBLE_SCROLL_CLASS, bubbleScroll } from "../bubble-scroll.js";
import { formatAge } from "../../duration.js";
import { hasFencedTree, inline, renderMarkdown, type TreeCols } from "../../markdown.js";
import { findTreeRegion, renderTreeHtml, type TreeIssue } from "../../metaprompt-tree.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { SmoothReveal } from "../../smooth.js";
import { onDiscard, stopTicking, tick } from "../ticking.js";
import type { RowContext } from "./context.js";
import { FINAL_RESPONSE_CLASS } from "../rows/turn-ended.js";

/** The attribute the shown length is carried on across a redraw. */
export const REVEALED_ATTRIBUTE = "data-revealed";

/**
 * The class a THINKING bubble wears: the agent's intermediate reasoning, drawn
 * purple and NON-BORDERED, one bubble per reasoning block. It reuses the whole
 * response bubble above (same fragment-fed arms, same markdown reveal) and only
 * this class changes how it looks. It is ALSO the guard that keeps the green
 * final-answer treatment off it: the stylesheet's `.final-response` rule and
 * `turn-ended.ts`'s bubble lookup both exclude `.thinking-bubble`, so a thinking
 * bubble can never take the green border even were it named the answer (which,
 * daemon-side, it never is). Set from `FeedResponse.thinking`.
 */
export const THINKING_BUBBLE_CLASS = "thinking-bubble";

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

/** The parts of a response bubble a redraw updates IN PLACE. */
interface ResponseParts {
  bubble: HTMLElement;
  scroll: HTMLElement;
  body: ResponseBody;
}

/**
 * The prose bubble.
 *
 * A RE-PUSH UPDATES THE BUBBLE IN PLACE (owner rule, 2026-09-23: the user owns
 * the scroll). The daemon re-pushes the whole row on every fragment of an
 * arriving response and again when it settles; building a fresh bubble for
 * each push replaced the scroll box, so a reader scrolled inside an expanded
 * response was thrown back to its top on every fragment. When the previous
 * draw of this row is a response bubble, it is updated and returned instead:
 * the scroll box keeps its identity (and so its position), the prose is
 * reconciled node by node (`reconcileChildren`), and the corner and the
 * notice are replaced only when what they say changed.
 *
 * Everything the wire can refuse is read BEFORE the bubble is touched, so a
 * malformed push leaves the bubble on screen exactly as it was.
 */
export function drawFeedResponse(u: FeedResponse, rc: RowContext): HTMLElement {
  const path = "FeedResponse";
  const result = requireCase(u.result, `${path}.result`);
  let markdown: string;
  switch (result.case) {
    case "update":
      markdown = drawFeedResponseProse(
        requireMessage(result.value.prose, `${path}.update.prose`),
        `${path}.update.prose`,
      );
      break;
    case "success":
      markdown = drawFeedResponseSuccess(result.value, `${path}.success`);
      break;
    case "error":
      markdown = drawFeedResponseError(result.value, `${path}.error`);
      break;
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = result;
      return unreachableArm(`${path}.result`, other.case);
    }
  }
  const notice = u.notice === undefined ? null : drawFeedResponseNotice(u.notice, `${path}.notice`);
  const corner =
    u.usage === undefined ? null : drawFeedResponseUsageStamp(u.usage, rc, `${path}.usage`);

  const reused = reusableParts(rc.previous);
  const { bubble, scroll, body } = reused ?? freshParts();
  bubble.setAttribute("data-state", result.case);

  // THE THINKING MARKER RIDES EVERY STATE. It is a FIELD, not an arm: whether
  // the prose is intermediate reasoning is orthogonal to whether it is still
  // arriving. The class draws the bubble purple and non-bordered and keeps the
  // green final-answer treatment structurally off it (see THINKING_BUBBLE_CLASS).
  bubble.classList.toggle(THINKING_BUBBLE_CLASS, u.thinking);
  bubble.toggleAttribute("data-thinking", u.thinking);

  // THE GREEN FINAL-ANSWER BORDER IS DATA-DRIVEN, APPLIED ON EVERY DRAW. When
  // the daemon has stamped this response as the turn's concluded answer, the
  // flag rides the row data -- so the green is (re)applied on every push,
  // redraw, tool-group re-arrange and history replay. A THINKING BUBBLE IS
  // EXCLUDED: it is never the answer, so it never greens. The BLUE
  // selected-response class is the controller's, and an in-place update leaves
  // it alone.
  bubble.classList.toggle(FINAL_RESPONSE_CLASS, u.finalAnswer && !u.thinking);

  // BEFORE the scroll box, and outside it: the body is rewritten by the prose
  // painters, so a heading placed inside it would be wiped by a repaint.
  bubble.classList.toggle("response-notice", notice !== null);
  bubble.toggleAttribute("data-notice", notice !== null);
  placeNotice(bubble, scroll, notice);

  // FIRST-LINE-ONLY RESERVATION VIA A ONE-LINE FLOAT (owner ruling, 2026-09-15).
  // The cost corner is INSIDE the scroll box, BEFORE the body, and floats
  // top-right, so the prose's FIRST line flows to its left and every later line
  // runs the full width. Its reserved width is fixed (styles.css), so neither
  // the hover reveal, the live clock nor a growing figure reflows that line.
  placeCorner(scroll, body, corner);

  const broken = result.case === "error";
  bubble.classList.toggle("response-cut-short", broken);
  placeCutShortMarker(bubble, broken);

  if (result.case === "update") {
    const resumed = revealedSoFar(rc.previous, markdown.length);
    log.debug("drawing an arriving response", {
      operation: "feed.cards.response.update",
      context: { path: `${path}.update`, length: markdown.length, resumed, in_place: reused !== null },
    });
    animate(bubble, body, markdown, resumed, rc);
  } else {
    paintWhole(body, markdown);
    markRevealed(bubble, markdown.length);
  }
  recordDraw(u, rc, result.case, markdown.length);
  return bubble;
}

/** A new bubble: the chrome, its scroll box and the body inside it. */
function freshParts(): ResponseParts {
  const bubble = document.createElement("div");
  bubble.className = "bubble assistant md";
  // The body is the CONTENT WRAPPER; the element appended to the bubble is the
  // scroll box that holds it (see bubble-scroll.ts).
  const body = createResponseBody();
  const scroll = bubbleScroll(body);
  bubble.appendChild(scroll);
  return { bubble, scroll, body };
}

/**
 * The parts of PREVIOUS when it is a response bubble this renderer drew, to be
 * updated in place; null for anything else (a first draw, a row whose arm
 * changed), which then gets a fresh bubble.
 */
function reusableParts(previous: HTMLElement | undefined): ResponseParts | null {
  if (previous === undefined) return null;
  if (!previous.classList.contains("bubble") || !previous.classList.contains("assistant")) return null;
  const scroll = previous.querySelector<HTMLElement>(`:scope > .${BUBBLE_SCROLL_CLASS}`);
  const body = scroll?.querySelector<ResponseBody>(`:scope > ${RESPONSE_BODY_TAG}`) ?? null;
  if (scroll === null || body === null) return null;
  return { bubble: previous, scroll, body };
}

/** The attribute a notice heading and a usage corner carry what they say on. */
const SAYS_ATTRIBUTE = "data-says";

/**
 * Put the notice heading NEXT above the scroll box, keeping the one already
 * there when it says the same thing.
 */
function placeNotice(bubble: HTMLElement, scroll: HTMLElement, next: HTMLElement | null): void {
  const current = bubble.querySelector<HTMLElement>(":scope > .response-notice-heading");
  if (next !== null) next.setAttribute(SAYS_ATTRIBUTE, next.textContent ?? "");
  if (current !== null && next !== null && current.getAttribute(SAYS_ATTRIBUTE) === next.getAttribute(SAYS_ATTRIBUTE)) {
    return;
  }
  current?.remove();
  if (next !== null) bubble.insertBefore(next, scroll);
}

/**
 * Put the usage corner first in the scroll box, keeping the one already there
 * when it says the same thing -- a replaced corner would restart its clock and
 * drop a hover reveal for nothing. A discarded corner's clock is stopped.
 */
function placeCorner(scroll: HTMLElement, body: HTMLElement, next: HTMLElement | null): void {
  const current = scroll.querySelector<HTMLElement>(":scope > .usage-corner");
  if (current !== null && next !== null && current.getAttribute(SAYS_ATTRIBUTE) === next.getAttribute(SAYS_ATTRIBUTE)) {
    stopTicking(next);
    return;
  }
  if (current !== null) {
    stopTicking(current);
    current.remove();
  }
  if (next !== null) scroll.insertBefore(next, body);
}

/** Add or drop the cut-short marker after the scroll box. */
function placeCutShortMarker(bubble: HTMLElement, broken: boolean): void {
  const current = bubble.querySelector(":scope > .response-cut-short-marker");
  if (broken && current === null) bubble.appendChild(cutShortMarker());
  if (!broken) current?.remove();
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
  // THE THINKING SIDE OF THE EMIT-VS-DRAW CORRELATION. The daemon logs
  // `daemon.feed.thinking_emitted` when it hands a thinking row to the feed
  // (thinking.go); this is the webapp's matching record that the row actually
  // drew as a thinking bubble, so the two can be correlated to tell whether a
  // thinking response landed on screen. It mirrors `feed.draw-response`'s own
  // context (row id, state, settled) and always logs at DEBUG, regardless of
  // first-draw or settled, since a thinking bubble draws no less often than a
  // prose one and this is purely a correlation record, not a reader-facing one.
  if (u.thinking) {
    log.debug("drew a thinking bubble", { operation: "feed.draw-thinking", context });
  }
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
 * THE MARKUP IS BUILT SO THE REVEAL'S SLIDE IS THE DURATION'S WIDTH BY
 * CONSTRUCTION (the stylesheet's `.usage-corner` comment has the mechanism):
 *
 *   span.usage-corner[data-tokens=<token text>]   ::before is a hidden copy of
 *     span.usage-slider                           the token, reserving its width
 *       span.usage-stamp  <token text>            out of flow, at the slider's left
 *       span.usage-ago    "5m 30s ago"            the slider's only in-flow content
 *
 * The slider's own width is the duration (plus its gap), and it is translated
 * by a percentage of that width, so the collapsed token sits exactly at the
 * right edge and the revealed token slides left exactly as far as the duration
 * needs. The corner's floated width is spacer + slider in both states, so the
 * first prose line that wraps around it never reflows on the reveal. The
 * stylesheet owns the 0.5s transition; `prefers-reduced-motion` drops it. A
 * state class is toggled here too, so a keyboard focus reveals the same
 * timestamp a hover does.
 *
 * THE TIMESTAMP IS A LIVE CLOCK: it reads `formatAge(now - at_ms)` and repaints
 * once per shared tick, so "5m 30s ago" stays current while it is on screen.
 * As it grows the slider grows with it, and the collapsed token stays put.
 * The subscription is taken through `tick`, which marks the element, so the
 * feed's teardown of the bubble — a re-push replacing the row, or the turn-end
 * backstop that stops every clock in a settled turn — unsubscribes it with no
 * disposer to remember here.
 *
 * NO TIMESTAMP WHILE ARRIVING: `at_ms` is zero until the response settles, and
 * a corner with no settled instant has an empty slider, so the token sits at
 * the right edge and nothing slides.
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
  corner.dataset.tokens = u.text;
  corner.setAttribute("data-says", `${u.text}|${String(atMs)}`);

  const slider = document.createElement("span");
  slider.className = "usage-slider";
  corner.appendChild(slider);

  const stamp = document.createElement("span");
  stamp.className = "usage-stamp";
  stamp.textContent = u.text;
  slider.appendChild(stamp);

  if (atMs > 0) {
    const ago = document.createElement("span");
    ago.className = "usage-ago";
    tick(ago, rc.ctx.ticker, (nowMs) => {
      ago.textContent = `${formatAge(nowMs - atMs)} ago`;
    });
    slider.appendChild(ago);

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
 * Markdown → HTML, with the metaprompt TLDR tree WRAPPED to the measured COLS.
 *
 * The tree arrives unwrapped, one physical line per branch; the wrapper
 * (metaprompt-tree.ts, a port of the daemon's treefmt) breaks every branch too
 * wide for COLS onto continuation lines under its own text column, so the tree
 * fills the bubble and re-flows when the bubble's width changes. Stray prose
 * around the tree keeps the markdown path — which itself wraps a fenced tree to
 * the same COLS — so a model that wrapped the tree in a sentence still reads
 * correctly. COLS is asked for only when a tree is actually drawn.
 */
export function proseHtml(markdown: string, cols: TreeCols): string {
  const region = findTreeRegion(markdown);
  if (region === null) return renderMarkdown(markdown, cols);
  const before = region.before.trim() === "" ? "" : renderMarkdown(region.before, cols);
  const after = region.after.trim() === "" ? "" : renderMarkdown(region.after, cols);
  return `${before}<div class="mp-tree">${renderTreeHtml(region.tree, inline, cols(), logTreeIssue)}</div>${after}`;
}

/**
 * Whether drawing MARKDOWN needs a measured column budget: it holds a tree,
 * bare or fenced. Prose that holds none draws the same at every width, so it
 * can be drawn before the body has a containing block.
 */
export function proseNeedsWidth(markdown: string): boolean {
  return findTreeRegion(markdown) !== null || hasFencedTree(markdown);
}

/** The operation every unmeasurable-width violation is recorded under. */
export const TREE_WIDTH_UNMEASURABLE = "feed.cards.response.tree-width-unmeasurable";

/**
 * A tree's column budget could not be measured: record it once, with what was
 * read, and fail. There is no default width to fall back to — a guessed width is
 * how trees came to wrap at 105 columns inside bubbles with room for 140.
 */
function unmeasurable(reason: string, context: Record<string, unknown>): never {
  log.error(`a metaprompt tree's column budget could not be measured: ${reason}`, {
    operation: TREE_WIDTH_UNMEASURABLE,
    context: { reason, ...context },
  });
  throw new Error(`metaprompt tree width unmeasurable: ${reason}`);
}

/** A computed length, which a laid-out element always reports in px. */
function computedPx(el: Element, style: CSSStyleDeclaration, property: ComputedLength): number {
  const value = style[property];
  const px = /^(-?\d+(?:\.\d+)?)px$/.exec(value);
  if (px === null) {
    unmeasurable("a computed length is not in px", { element: el.className, property, value });
  }
  return Number.parseFloat(px[1]);
}

/** The computed lengths the budget reads. */
type ComputedLength =
  | "paddingLeft"
  | "paddingRight"
  | "borderLeftWidth"
  | "borderRightWidth"
  | "marginLeft"
  | "marginRight";

/** One element's horizontal padding (plus border, when BORDER), from computed styles. */
function insetXPx(el: Element, view: Window, parts: { border: boolean; margin: boolean }): number {
  const s = view.getComputedStyle(el);
  let px = computedPx(el, s, "paddingLeft") + computedPx(el, s, "paddingRight");
  if (parts.border) px += computedPx(el, s, "borderLeftWidth") + computedPx(el, s, "borderRightWidth");
  if (parts.margin) px += computedPx(el, s, "marginLeft") + computedPx(el, s, "marginRight");
  return px;
}

/**
 * The column budget a tree in BODY wraps to: the body's content width WHEN THE
 * BUBBLE IS AT ITS CAP, in columns of the tree's monospace font.
 *
 * We never scroll horizontally; lines wider than the max bubble width wrap instead.
 *
 * The budget derives from the CAP ALONE — the bubble's `max-width` resolved
 * against its containing block, less the bubble's fixed chrome summed from
 * computed styles — and never from the bubble's current fit-content width, so
 * a tree below the cap never wraps and a shrink cannot feed back into a re-wrap.
 *
 * It measures only a body that is IN THE DOCUMENT, and anything it cannot read
 * is an invariant violation (`unmeasurable`), never a default.
 *
 * REGRESSION WATCH (breadcrumb, per AGENTS.md): this used to run while the
 * bubble was still DETACHED (drawFeedResponse paints before feed-view attaches
 * the row) and fall back to 105 columns when it read nothing. A real engine
 * neither lays out nor styles a detached node — the character probe measured
 * 0px and `getComputedStyle` answered empty strings — so EVERY first paint
 * wrapped at the fallback, whatever the bubble's room. jsdom styles detached
 * nodes and the unit suite stubbed geometry regardless of attachment, which is
 * why the tests never saw it. The first paint of a tree now waits for the body
 * to join the document (`createTreeWrap`). This is a watch flag, not a lock.
 */
export function measureTreeCols(body: HTMLElement): number {
  if (!body.isConnected) unmeasurable("the bubble body is not in the document", {});
  const view = body.ownerDocument.defaultView;
  if (view === null) unmeasurable("the bubble body's document has no window", {});
  const bubble = requireBubble(body);
  const containing = containingBlockOf(bubble);

  const probe = body.ownerDocument.createElement("div");
  probe.className = "mp-tree";
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre;left:-9999px;top:0;";
  probe.textContent = "0".repeat(100);
  body.appendChild(probe);
  const charPx = probe.getBoundingClientRect().width / 100;
  probe.remove();
  if (!(charPx > 0)) unmeasurable("the tree font's column measured no width", { char_px: charPx });

  const containingPx =
    containing.clientWidth - insetXPx(containing, view, { border: false, margin: false });
  if (!(containingPx > 0)) {
    unmeasurable("the bubble's containing block has no width", { containing_px: containingPx });
  }
  const maxWidth = view.getComputedStyle(bubble).maxWidth;
  const capPx = resolveCapPx(maxWidth, containingPx);
  if (capPx === null) unmeasurable("the bubble's max-width does not resolve", { max_width: maxWidth });

  // The fixed chrome from the bubble's border box in to the body's content box:
  // every border and padding on the way, plus the margins of the boxes inside
  // the bubble. None of it depends on content.
  let chromePx = insetXPx(bubble, view, { border: true, margin: false });
  for (let el: HTMLElement | null = body; el !== null && el !== bubble; el = el.parentElement) {
    chromePx += insetXPx(el, view, { border: true, margin: true });
  }

  const contentPx = capPx - chromePx;
  const cols = Math.floor(contentPx / charPx);
  const context = { cols, char_px: charPx, cap_px: capPx, chrome_px: chromePx, containing_px: containingPx };
  if (cols < 1) unmeasurable("the bubble's content width at its cap holds no column", context);
  log.debug("measured a metaprompt tree's column budget", {
    operation: "feed.cards.response.tree-cols",
    context,
  });
  return cols;
}

/** The block the bubble's percentage cap resolves against: its parent. */
function containingBlockOf(bubble: HTMLElement): HTMLElement {
  const containing = bubble.parentElement;
  if (containing === null) unmeasurable("the bubble has no containing block", {});
  return containing;
}

/**
 * The bubble's `max-width`, resolved to px against CONTAININGPX. Per CSSOM the
 * resolved value of `max-width` is the COMPUTED value (a percentage stays a
 * percentage, a calc of one percentage serializes as `calc(N%)`), so a
 * percentage is resolved here; a browser that hands back px is honored as is.
 * Anything else answers `null`.
 */
function resolveCapPx(maxWidth: string, containingPx: number): number | null {
  const px = /^(-?[\d.]+)px$/.exec(maxWidth);
  if (px !== null) return Number.parseFloat(px[1]);
  const pct = /^(?:calc\()?\s*(-?[\d.]+)%\s*\)?$/.exec(maxWidth);
  if (pct !== null) return (Number.parseFloat(pct[1]) / 100) * containingPx;
  return null;
}

/** The tag of a response's body: a custom element, so it knows when it joins the document. */
export const RESPONSE_BODY_TAG = "response-body";

/** A response's body, which runs its hooks every time it joins the document. */
export interface ResponseBody extends HTMLElement {
  onConnect(hook: () => void): void;
}

/**
 * The response body as a custom element. `connectedCallback` runs synchronously
 * inside the DOM operation that attaches the row — before any frame is painted,
 * and whether or not the webview is visible — which is what lets a tree's first
 * paint wait for a containing block without ever showing a wrong-width frame.
 */
function responseBodyClass(): CustomElementConstructor {
  return class extends HTMLElement implements ResponseBody {
    private readonly hooks: Array<() => void> = [];

    onConnect(hook: () => void): void {
      this.hooks.push(hook);
    }

    connectedCallback(): void {
      for (const hook of this.hooks) hook();
    }
  };
}

/** A fresh response body, the custom element defined on first use. */
export function createResponseBody(): ResponseBody {
  if (customElements.get(RESPONSE_BODY_TAG) === undefined) {
    customElements.define(RESPONSE_BODY_TAG, responseBodyClass());
  }
  const body = document.createElement(RESPONSE_BODY_TAG) as ResponseBody;
  body.className = "bubble-body";
  return body;
}

/** What a body's painters draw through, so a tree is only ever drawn at a measured width. */
interface TreeWrap {
  /** The body's measured column budget, measured on first ask after each attach. */
  readonly cols: TreeCols;
  /**
   * Draw TEXT through DRAW: at once, unless TEXT holds a tree and the body is
   * not in the document yet, in which case the draw runs the moment it joins.
   * The latest draw is also what a width change re-runs.
   */
  paint(text: string, draw: () => void): void;
}

/**
 * Keep BODY's tree wrapped to the cap for its whole life.
 *
 * The FIRST paint of a tree waits for the body to join the document, since only
 * then is there a containing block to measure against. Every later attach (a
 * tool-group re-arrange moves the row) re-measures, and a `ResizeObserver` on
 * the CONTAINING BLOCK — the box the cap is a percentage of, not the
 * fit-content body, which keeps its width when the column grows around a bubble
 * below its cap — re-wraps when the column's width moves the budget. The
 * observer is torn down with the body through `stopTicking` (see ticking.ts).
 */
function createTreeWrap(body: ResponseBody): TreeWrap {
  let cols: number | null = null;
  let latest: (() => void) | null = null;
  let deferred = false;
  let observer: ResizeObserver | null = null;

  const drawsTree = (): boolean => body.querySelector(".mp-tree") !== null;
  const rewrap = (): void => {
    const next = measureTreeCols(body);
    if (next === cols) return;
    cols = next;
    latest?.();
  };
  const watch = (): void => {
    if (observer === null) onDiscard(body, () => observer?.disconnect());
    else observer.disconnect();
    observer = observeContainingBlock(body, rewrap);
  };

  body.onConnect(() => {
    if (deferred) {
      deferred = false;
      latest?.();
    } else if (drawsTree()) {
      rewrap();
    }
    if (drawsTree()) watch();
  });

  return {
    cols: () => {
      cols ??= measureTreeCols(body);
      return cols;
    },
    paint(text, draw) {
      latest = draw;
      if (!body.isConnected && proseNeedsWidth(text)) {
        deferred = true;
        return;
      }
      draw();
      if (body.isConnected && observer === null && drawsTree()) watch();
    },
  };
}

/**
 * Run REWRAP whenever BODY's containing block changes size. The recompute is
 * deferred to an animation frame so a burst of resizes coalesces and the
 * repaint's own height change is not re-observed inside the same delivery; it
 * repaints only when the integer column count moved.
 */
function observeContainingBlock(body: HTMLElement, rewrap: () => void): ResizeObserver {
  const view = body.ownerDocument.defaultView;
  if (view === null || typeof view.ResizeObserver !== "function") {
    unmeasurable("the page has no ResizeObserver to follow the column's width", {});
  }
  const containing = containingBlockOf(requireBubble(body));
  let scheduled = false;
  const recompute = (): void => {
    scheduled = false;
    rewrap();
  };
  const observer = new view.ResizeObserver(() => {
    if (scheduled) return;
    scheduled = true;
    const raf = view.requestAnimationFrame?.bind(view);
    if (raf !== undefined) raf(recompute);
    else recompute();
  });
  observer.observe(containing);
  return observer;
}

/** The bubble around BODY, which a drawn response body always has. */
function requireBubble(body: HTMLElement): HTMLElement {
  const bubble = body.closest<HTMLElement>(".bubble");
  if (bubble === null) unmeasurable("the body has no bubble around it", {});
  return bubble;
}

/** Each body's ONE tree wrap: a body updated in place keeps the wrap it has. */
const wraps = new WeakMap<ResponseBody, TreeWrap>();

/** The tree wrap BODY paints through, created on its first paint. */
function wrapFor(body: ResponseBody): TreeWrap {
  let wrap = wraps.get(body);
  if (wrap === undefined) {
    wrap = createTreeWrap(body);
    wraps.set(body, wrap);
  }
  return wrap;
}

/**
 * Each body's paint GENERATION. Every paint of a body takes the next one, and a
 * type-out loop stops the moment its generation is no longer the body's: an
 * in-place update never leaves the previous push's loop painting over it.
 */
const generations = new WeakMap<ResponseBody, number>();

/** Claim the next paint generation of BODY. */
function nextGeneration(body: ResponseBody): number {
  const next = (generations.get(body) ?? 0) + 1;
  generations.set(body, next);
  return next;
}

/**
 * Bring BODY's prose to MARKDOWN, wrapped to COLS, by RECONCILING it node by
 * node rather than rewriting it: a node that did not change keeps its identity
 * and never re-lays out, so a settle, a re-wrap or an in-place update moves
 * nothing the reader did not see change (owner rule, 2026-09-23: the user owns
 * the scroll). The result is byte-identical to `proseHtml(markdown, cols)`.
 */
function reconcileProse(body: ResponseBody, markdown: string, cols: TreeCols): void {
  const target = document.createElement("div");
  target.innerHTML = proseHtml(markdown, cols);
  reconcileChildren(body, target, null);
}

/**
 * Bring the whole settled prose into the body, wrapped to the cap (see
 * `createTreeWrap`). Plain prose reflows on its own (CSS), so it takes no
 * observer and no measurement. A type-out still running on this body stops.
 */
function paintWhole(body: ResponseBody, markdown: string): void {
  const wrap = wrapFor(body);
  nextGeneration(body);
  wrap.paint(markdown, () => reconcileProse(body, markdown, wrap.cols));
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

/** The broken bubble's marker. The WHY is the turn's terminal row. */
function cutShortMarker(): HTMLElement {
  const marker = document.createElement("span");
  marker.className = "response-cut-short-marker";
  marker.textContent = "cut short";
  return marker;
}

/**
 * Reconcile PARENT's children in place so they match TARGET's, preserving the
 * identity of every node that did not change.
 *
 * This is the whole anti-flicker mechanism. The reveal renders the authoritative
 * `proseHtml(slice, cols)` into a DETACHED target each frame; rather than swap
 * the live subtree for it (which destroys and recreates every settled node under
 * the reader), this walks the two child lists in order and, position by
 * position:
 *
 *   - a node that `isEqualNode` the target's is left exactly as it is — a stable
 *     prose paragraph or a final tree line keeps its identity and never repaints;
 *   - a node that only changed inside (same tag and attributes, or a text node)
 *     is PATCHED in place recursively — a growing paragraph's text node gets its
 *     data updated, a `.mp-tree` is descended into and line-diffed, the one tail
 *     line whose wrap can still change is patched — so the node stays put and
 *     only its altered interior moves;
 *   - anything else is replaced, extra target nodes are appended, and trailing
 *     nodes the target no longer has are removed.
 *
 * Because every position ends structurally equal to the target, the reconciled
 * result is byte-identical to a fresh `proseHtml(slice, cols)` — the same string
 * `paintWhole` writes in one shot.
 *
 * TAIL, when given, is a node kept as PARENT's last child: it is never matched
 * against the target and never removed, so the prose reconciles ahead of it and
 * it stays put. The streaming reveal passes `null` (no trailing node), and the
 * in-place tree line-diff (`patchNode`) does too.
 */
export function reconcileChildren(parent: Node, target: Node, tail: Node | null): void {
  const goal = Array.from(target.childNodes);
  let i = 0;
  for (; i < goal.length; i++) {
    const want = goal[i];
    // `.item` returns null past the end at run time even where the DOM lib types
    // it non-null, so the range check below is real, not dead.
    let existing: ChildNode | null = parent.childNodes.item(i);
    // Reaching the preserved tail means the prose ran out: the rest is new and
    // is inserted ahead of the tail rather than matched against it.
    if (existing === tail) existing = null;
    if (existing === null) {
      parent.insertBefore(want, tail);
      continue;
    }
    if (existing.isEqualNode(want)) continue;
    if (patchable(existing, want)) {
      patchNode(existing, want);
      continue;
    }
    parent.replaceChild(want, existing);
  }
  // Drop any prose nodes the target no longer carries, without touching the tail.
  let extra: ChildNode | null = parent.childNodes.item(i);
  while (extra !== null && extra !== tail) {
    parent.removeChild(extra);
    extra = parent.childNodes.item(i);
  }
}

/**
 * Whether EXISTING can be patched into WANT in place (keeping EXISTING's node
 * identity) rather than replaced wholesale: two text nodes, or two elements of
 * the same tag carrying the same attributes. A tag or attribute difference means
 * the node genuinely changed shape and is replaced instead.
 */
function patchable(existing: Node, want: Node): boolean {
  if (existing.nodeType !== want.nodeType) return false;
  if (existing.nodeType === Node.TEXT_NODE) return true;
  if (existing.nodeType !== Node.ELEMENT_NODE) return false;
  const a = existing as Element;
  const b = want as Element;
  return a.tagName === b.tagName && sameAttributes(a, b);
}

/** Whether two elements carry the same attribute set, name for name and value. */
function sameAttributes(a: Element, b: Element): boolean {
  if (a.attributes.length !== b.attributes.length) return false;
  for (const attr of Array.from(a.attributes)) {
    if (b.getAttribute(attr.name) !== attr.value) return false;
  }
  return true;
}

/** Patch EXISTING to match WANT in place: text data for a text node, otherwise
 * recurse so the element's own children reconcile the same way. */
function patchNode(existing: Node, want: Node): void {
  if (existing.nodeType === Node.TEXT_NODE) {
    const from = existing as Text;
    const to = want as Text;
    if (from.data !== to.data) from.data = to.data;
    return;
  }
  reconcileChildren(existing, want, null);
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
  body: ResponseBody,
  markdown: string,
  resumed: number,
  rc: RowContext,
): void {
  // The arriving prose wraps through the SAME path as the settled whole, at the
  // same measured width, so nothing shrinks or re-wraps when the final lands. A
  // width change re-runs the latest paint, re-wrapping the slice already shown.
  const wrap = wrapFor(body);
  const generation = nextGeneration(body);
  const paint = (shown: number): void => {
    const slice = markdown.slice(0, shown);
    // REGRESSION WATCH (per-frame reveal flicker, 2026-09-15): this once did
    // `body.replaceChildren(...proseHtml(slice).childNodes, freshIndicator())`
    // on EVERY animation frame — a full teardown and rebuild of the whole prose
    // subtree ~60 times a second, so every settled line was destroyed and
    // recreated under the reader and the bubble flickered. It now renders the
    // authoritative `proseHtml(slice, cols)` into a DETACHED target and
    // RECONCILES the live body against it (reconcileChildren): unchanged leading
    // nodes — stable prose paragraphs, final tree lines — keep their identity and
    // are never touched, and only the growing tail (and, inside a tree, the one
    // last line whose wrap can still change) is patched or appended. The
    // reconciled result is byte-identical to a fresh `proseHtml(slice, cols)`,
    // the same whole render `paintWhole` writes at settle, so the reveal never
    // diverges from the oracle. Do NOT reintroduce a whole-subtree rebuild per
    // frame. This is a watch flag, not a lock.
    wrap.paint(slice, () => reconcileProse(body, slice, wrap.cols));
    markRevealed(bubble, shown);
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
    // A later paint of this same body (an in-place update) owns it now.
    if (generations.get(body) !== generation) return;
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
