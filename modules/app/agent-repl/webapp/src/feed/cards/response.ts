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

/** The prose bubble. */
export function drawFeedResponse(u: FeedResponse, rc: RowContext): HTMLElement {
  const path = "FeedResponse";
  const result = requireCase(u.result, `${path}.result`);

  const bubble = document.createElement("div");
  bubble.className = "bubble assistant md";
  bubble.setAttribute("data-state", result.case);

  // THE THINKING MARKER RIDES EVERY STATE. It is a FIELD, not an arm: whether
  // the prose is intermediate reasoning is orthogonal to whether it is still
  // arriving. The class draws the bubble purple and non-bordered and keeps the
  // green final-answer treatment structurally off it (see THINKING_BUBBLE_CLASS).
  if (u.thinking) {
    bubble.classList.add(THINKING_BUBBLE_CLASS);
    bubble.setAttribute("data-thinking", "");
  }

  // THE GREEN FINAL-ANSWER BORDER IS DATA-DRIVEN, APPLIED ON EVERY DRAW. When
  // the daemon has stamped this response as the turn's concluded answer, the
  // flag rides the row data — so the green is (re)applied here on every push,
  // redraw, tool-group re-arrange, and history replay, and no rebuild of this
  // bubble can lose it. This replaces the former one-shot mark the turn-ended
  // row applied in reaction to a live event, which the daemon no longer needs
  // to deliver for the border to appear (turn-ended.ts). A THINKING BUBBLE IS
  // EXCLUDED: it is never the answer, so it never greens — the guard here
  // matches the stylesheet's own `.final-response:not(.thinking-bubble)` rule.
  // The BLUE selected-response border still wins over the green: the controller
  // toggles `.response-selected` on this same bubble and the stylesheet's
  // `.final-response.response-selected` rule paints blue over the green.
  if (u.finalAnswer && !u.thinking) {
    bubble.classList.add(FINAL_RESPONSE_CLASS);
  }

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
  const scroll = bubbleScroll(body);

  // FIRST-LINE-ONLY RESERVATION VIA A ONE-LINE FLOAT (owner ruling, 2026-09-15).
  // The cost corner is inserted INTO the scroll box, BEFORE the body, and floats
  // top-right (`float: right`, styles.css). The body is a plain block sibling in
  // the same block-formatting context (the scroll box, which clips its overflow),
  // so the prose's FIRST line flows to the corner's LEFT and wraps around it;
  // once the text drops past the corner's height — one line, since the corner is
  // a single row of token + duration — every SUBSEQUENT line runs the full width.
  // The corner keeps a CONSTANT reserved width across the hover reveal (the
  // duration slot is reserved even while collapsed — see `.usage-ago`), so
  // exposing the duration animates opacity/translate only and never reflows the
  // first line. It lives here rather than above the box so the wrap can reach it:
  // a strip above the body could not shorten a line inside the body.
  if (u.usage !== undefined) {
    scroll.insertBefore(drawFeedResponseUsageStamp(u.usage, rc, `${path}.usage`), body);
  }
  bubble.appendChild(scroll);

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
 *
 * REGRESSION WATCH (breadcrumb, per AGENTS.md): the cap MUST resolve at
 * synchronous first paint even while the bubble is DETACHED. `drawFeedResponse`
 * paints the body synchronously and feed-view attaches the row only AFTER it
 * returns, so at first paint the bubble has no attached parent — its percentage
 * `max-width` (`70.125%`) had no containing block to resolve against, the cap
 * came back null, and the tree wrapped to `DEFAULT_TREE_COLS` (105) and
 * OVERFLOWED the bubble. The on-attach `reflowOnResize` was supposed to correct
 * it, but its rAF is SUSPENDED while the webview is hidden/unfocused (Emacs
 * xwidget reports `visibilityState === "hidden"` when Emacs is not frontmost),
 * so the correction never ran and the overflow stuck. The fix resolves the cap
 * against an ALREADY-ATTACHED reference — the root `#feed` column the row will be
 * placed into — and reads the width-invariant chrome insets from computed styles
 * when there is no layout, so the FIRST synchronous frame already wraps at the
 * true cap with no dependency on a suspendable re-wrap. Do NOT reintroduce a
 * dependence on the bubble's own attached parent at first paint. This is a watch
 * flag, not a lock.
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
  const insets = bubbleInsetsPx(bubble, body, view);
  if (insets === null) return null;
  const contentPx = capPx - insets;
  return contentPx > 0 ? contentPx : null;
}

/**
 * The width-invariant chrome between the bubble's border box and the body's
 * content box — the bubble's and the scroll box's border+padding, plus the
 * body's padding. It does not change with content, so when the bubble is LAID
 * OUT it is read cheaply from the current geometry (`bubbleBorderBox -
 * bodyContent`, exactly as it always was). At detached FIRST PAINT there is no
 * geometry (every rect is 0), so the same fixed chrome is summed from the
 * chain's computed border+padding instead — available without layout — so the
 * cap-based measure still holds on the first synchronous frame. Returns `null`
 * only when neither path yields a positive width.
 */
function bubbleInsetsPx(bubble: HTMLElement, body: HTMLElement, view: Window): number | null {
  const style = view.getComputedStyle(body);
  const pad = (Number.parseFloat(style.paddingLeft) || 0) + (Number.parseFloat(style.paddingRight) || 0);
  const bodyContent = body.clientWidth - pad;
  const bubbleBorderBox = bubble.getBoundingClientRect().width;
  if (bubbleBorderBox > 0 && bodyContent > 0) return bubbleBorderBox - bodyContent;
  const fromStyles = chromeInsetsFromStyles(bubble, body, view);
  return fromStyles > 0 ? fromStyles : null;
}

/**
 * The chrome insets summed from computed styles: every horizontal border and
 * padding from the body's content box out to (and including) the bubble's border
 * box. Used only when there is no layout to read them from (detached first
 * paint); the values are content-independent, so this equals the geometry
 * measure a laid-out bubble would give.
 */
function chromeInsetsFromStyles(bubble: HTMLElement, body: HTMLElement, view: Window): number {
  let insets = 0;
  let el: HTMLElement | null = body;
  while (el !== null) {
    insets += borderPaddingXPx(el, view);
    if (el === bubble) break;
    el = el.parentElement;
  }
  return insets;
}

/** One element's horizontal border + padding, from computed styles. */
function borderPaddingXPx(el: HTMLElement, view: Window): number {
  const s = view.getComputedStyle(el);
  return (
    (Number.parseFloat(s.paddingLeft) || 0) +
    (Number.parseFloat(s.paddingRight) || 0) +
    (Number.parseFloat(s.borderLeftWidth) || 0) +
    (Number.parseFloat(s.borderRightWidth) || 0)
  );
}

/**
 * The bubble's `max-width`, resolved to px. Per CSSOM the resolved value of
 * `max-width` is the COMPUTED value (a percentage stays a percentage, a calc of
 * one percentage serializes as `calc(N%)`), not the used px — so a percentage is
 * resolved here against the bubble's containing block (its parent's content-box
 * width). A browser that hands back px instead is honored directly. Anything
 * else — `none`, an empty value, or a calc mixing units we cannot resolve with a
 * single containing-block multiply — returns `null` so the caller falls back.
 *
 * The containing block is normally the bubble's own parent's content-box width.
 * At synchronous FIRST PAINT the bubble is still DETACHED (see the REGRESSION
 * WATCH on `measureTreeCols`), so its parent has no layout; the percentage is
 * then resolved against an ALREADY-ATTACHED reference — the root feed column the
 * row will be placed into — whose content width equals that containing block.
 */
function resolveMaxWidthPx(bubble: HTMLElement, view: Window): number | null {
  const mw = view.getComputedStyle(bubble).maxWidth;
  if (mw === "" || mw === "none") return null;
  const px = /^(-?[\d.]+)px$/.exec(mw);
  if (px !== null) return Number.parseFloat(px[1]);
  const pct = /^(?:calc\()?\s*(-?[\d.]+)%\s*\)?$/.exec(mw);
  if (pct !== null) {
    const own = bubble.parentElement?.clientWidth ?? 0;
    const containing = own > 0 ? own : attachedContainingWidthPx(bubble.ownerDocument, view);
    if (containing > 0) return (Number.parseFloat(pct[1]) / 100) * containing;
  }
  return null;
}

/**
 * The content-box width of the root feed column, read from the ALREADY-ATTACHED
 * `#feed` element. A row is placed into a `.feed-item` wrapper that fills this
 * column, so this width IS the containing block the bubble's percentage cap
 * resolves against once attached — which lets the cap resolve at synchronous
 * first paint, before the row is in the document. Returns 0 when no feed is
 * attached (a test with nothing staged, a truly layout-less host), so the caller
 * falls back to `DEFAULT_TREE_COLS`.
 */
function attachedContainingWidthPx(doc: Document, view: Window): number {
  const feed = doc.getElementById("feed");
  if (feed === null) return 0;
  const style = view.getComputedStyle(feed);
  const pad = (Number.parseFloat(style.paddingLeft) || 0) + (Number.parseFloat(style.paddingRight) || 0);
  const content = feed.clientWidth - pad;
  return content > 0 ? content : 0;
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
    const target = document.createElement("div");
    target.innerHTML = proseHtml(markdown.slice(0, shown), cols);
    reconcileChildren(body, target, null);
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
