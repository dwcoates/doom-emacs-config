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
import { renderMarkdown, inline } from "../../markdown.js";
import { findTreeRegion, renderTreeHtml } from "../../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { SmoothReveal } from "../../smooth.js";
import type { RowContext } from "./context.js";

/** The attribute the shown length is carried on across a redraw. */
export const REVEALED_ATTRIBUTE = "data-revealed";

/** The prose bubble. */
export function drawFeedResponse(u: FeedResponse, rc: RowContext): HTMLElement {
  const path = "FeedResponse";
  const result = requireCase(u.result, `${path}.result`);
  log.debug("drawing a response bubble", {
    operation: "feed.cards.response",
    context: {
      state: result.case,
      usage: u.usage !== undefined,
      notice: u.notice !== undefined,
    },
  });

  const bubble = document.createElement("div");
  bubble.className = "bubble assistant md";
  bubble.setAttribute("data-state", result.case);

  // BEFORE the body, and outside it: the body is rewritten whole by the prose
  // painters (and by every frame of the type-out), so a heading placed inside it
  // would be wiped by the first repaint of an arriving response.
  if (u.notice !== undefined) {
    bubble.classList.add("response-notice");
    bubble.setAttribute("data-notice", "");
    bubble.appendChild(drawFeedResponseNotice(u.notice, `${path}.notice`));
  }

  const body = document.createElement("div");
  body.className = "bubble-body";
  bubble.appendChild(body);

  const corner = document.createElement("span");
  corner.className = "turn-meta";
  if (u.usage !== undefined) {
    corner.appendChild(drawFeedResponseUsageStamp(u.usage, `${path}.usage`));
  }
  bubble.appendChild(corner);

  switch (result.case) {
    case "update":
      drawFeedResponseUpdate(result.value, rc, `${path}.update`, bubble, body);
      break;
    case "success": {
      const markdown = drawFeedResponseSuccess(result.value, `${path}.success`);
      paintWhole(body, markdown);
      markRevealed(bubble, markdown.length);
      break;
    }
    case "error": {
      const markdown = drawFeedResponseError(result.value, `${path}.error`);
      bubble.classList.add("response-cut-short");
      paintWhole(body, markdown);
      markRevealed(bubble, markdown.length);
      bubble.appendChild(cutShortMarker());
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
  return bubble;
}

/**
 * The arriving state: the markdown so far, paced.
 *
 * The reveal resumes from the previous draw's shown length and stops the moment
 * it reaches the frontier; a redraw whose prose did not grow therefore does no
 * animation at all.
 */
export function drawFeedResponseUpdate(
  u: FeedResponseUpdate,
  rc: RowContext,
  path: string,
  bubble: HTMLElement,
  body: HTMLElement,
): void {
  const markdown = drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
  const resumed = revealedSoFar(rc.previous, markdown.length);
  log.debug("drawing an arriving response", {
    operation: "feed.cards.response.update",
    context: { path, length: markdown.length, resumed },
  });
  body.appendChild(arrivingIndicator());
  animate(bubble, body, markdown, resumed, rc);
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

/** The cost corner's figure, drawn verbatim. */
export function drawFeedResponseUsageStamp(
  u: FeedResponseUsageStamp,
  path: string,
): HTMLElement {
  log.debug("drawing a response usage stamp", {
    operation: "feed.cards.response.usage-stamp",
    context: { path },
  });
  const stamp = document.createElement("span");
  stamp.className = "usage-stamp";
  stamp.textContent = u.text;
  return stamp;
}

/**
 * Markdown → HTML, with the metaprompt TLDR tree carved out.
 *
 * A bare tree renders as hanging-indent tree lines rather than through the
 * markdown pipeline, which would shear its wrapped branches back to column 0.
 * Stray prose around the tree keeps the markdown path, so a model that wrapped
 * the tree in a sentence still reads correctly.
 */
export function proseHtml(markdown: string): string {
  const region = findTreeRegion(markdown);
  if (region === null) return renderMarkdown(markdown);
  const before = region.before.trim() === "" ? "" : renderMarkdown(region.before);
  const after = region.after.trim() === "" ? "" : renderMarkdown(region.after);
  return `${before}<div class="mp-tree">${renderTreeHtml(region.tree, inline)}</div>${after}`;
}

/** Write the whole prose into the body, replacing whatever was there. */
function paintWhole(body: HTMLElement, markdown: string): void {
  body.innerHTML = proseHtml(markdown);
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
  const paint = (shown: number): void => {
    const prose = document.createElement("div");
    prose.innerHTML = proseHtml(markdown.slice(0, shown));
    body.replaceChildren(...prose.childNodes, arrivingIndicator());
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
