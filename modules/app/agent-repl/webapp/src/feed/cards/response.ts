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
import {
  createResponseBody,
  createTreeWrap,
  paintWhole,
  proseHtml,
  reconcileChildren,
  type ResponseBody,
} from "../../bubble/body.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { SmoothReveal } from "../../smooth.js";
import { tick } from "../ticking.js";
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
  const body = createResponseBody();
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
  body: ResponseBody,
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
  const wrap = createTreeWrap(body);
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
    wrap.paint(slice, () => {
      const target = document.createElement("div");
      target.innerHTML = proseHtml(slice, wrap.cols);
      reconcileChildren(body, target, null);
    });
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
