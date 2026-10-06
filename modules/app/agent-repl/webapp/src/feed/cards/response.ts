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
  FeedResponseRevealWindow,
  FeedResponseSuccess,
  FeedResponseUsageStamp,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../../log.js";
import {
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_UNCAPPED,
  ELLIPSIS_CAP_LINES,
  SAYS_ATTRIBUTE,
  drawBubble,
  type BubbleCapLines,
  type BubbleCapSpec,
} from "../../bubble/draw.js";
import { formatAge } from "../../duration.js";
import { markdownSlot, paintGeneration, repaintSlot, type BubbleBody } from "../../bubble/body.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { SmoothReveal, revealSlice, windowedReveal } from "../../smooth.js";
import { MalformedView } from "../../rpc/malformed.js";
import { tickWhileShown } from "../ticking.js";
import { tokenHeatColor } from "../../token-heat.js";
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
 * A landed thinking bubble's collapsed line limit: ONE line, stated here, never
 * through another kind's constant. It is the ellipsis's one-line cap too
 * (`ELLIPSIS_CAP_LINES`), which the landed thinking cap's more signal needs
 * (`responseCap`), so the type holds it there.
 */
export const THINKING_CAP_LINES = 1 satisfies typeof ELLIPSIS_CAP_LINES;

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

/** The class of the one markdown slot a response's prose is painted into. */
export const RESPONSE_PROSE_CLASS = "response-prose";

/** The prose bubble: its spec, drawn through the one bubble (src/bubble/draw.ts). */
export function drawFeedResponse(u: FeedResponse, rc: RowContext): HTMLElement {
  const path = "FeedResponse";
  const result = requireCase(u.result, `${path}.result`);

  // THE THINKING MARKER RIDES EVERY STATE. It is a FIELD, not an arm: whether
  // the prose is intermediate reasoning is orthogonal to whether it is still
  // arriving. The variant draws the bubble unbordered and keeps the green
  // final-answer treatment structurally off it (see THINKING_BUBBLE_CLASS).
  const hooks = ["assistant"];
  if (u.thinking) hooks.push(THINKING_BUBBLE_CLASS);

  // THE GREEN FINAL-ANSWER BORDER IS DATA-DRIVEN, APPLIED ON EVERY DRAW. When
  // the daemon has stamped this response as the turn's concluded answer, the
  // flag rides the row data — so the green is (re)applied here on every push,
  // redraw, tool-group re-arrange, and history replay, and no rebuild of this
  // bubble can lose it. A THINKING BUBBLE IS EXCLUDED: it is never the answer,
  // so it never greens — the guard here matches the stylesheet's own
  // `.final-response:not(.thinking-bubble)` rule. The BLUE selection
  // border still wins over the green: the controller toggles the one
  // selected-entry class (`.entry-selected`, selected-entry.ts) on this bubble.
  if (u.finalAnswer && !u.thinking) hooks.push(FINAL_RESPONSE_CLASS);

  // The heading is the header strip, outside the body: the body is rewritten
  // by the prose painters (and by every frame of the type-out), so a heading
  // placed inside it would be wiped by the first repaint of an arriving response.
  const strip: HTMLElement[] = [];
  if (u.notice !== undefined) {
    hooks.push("response-notice");
    strip.push(drawFeedResponseNotice(u.notice, `${path}.notice`));
  }
  if (result.case === "error") hooks.push("response-cut-short");

  // FIRST-LINE-ONLY RESERVATION VIA A ONE-LINE FLOAT (owner ruling, 2026-09-15).
  // The cost corner rides INSIDE the scroll box, before the body, floated
  // top-right, so the prose's FIRST line wraps around it and every later line
  // runs the full width (see `.usage-corner`).
  const corner =
    u.usage === undefined ? undefined : drawFeedResponseUsageStamp(u.usage, rc, `${path}.usage`);

  // The arm is read BEFORE anything is drawn, so an arm a newer daemon set
  // reaches the refusal that names it rather than half a bubble.
  switch (result.case) {
    case "update":
    case "success":
    case "error":
      break;
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = result;
      return unreachableArm(`${path}.result`, other.case);
    }
  }

  // The prose is ONE markdown slot, painted by the one body pipeline: the whole
  // prose once settled, and — while arriving — only what the previous draw of
  // this row had already shown, from which the type-out resumes.
  //
  // THE SETTLED WHOLE TYPES OUT TOO when the daemon paced it: the last
  // fragment's text is spread across the reveal window like any other push's,
  // rather than appearing at once. Unpaced, it is drawn whole as before.
  let markdown: string;
  let shown: number;
  let windowMs: number | undefined;
  switch (result.case) {
    case "update":
      markdown = drawFeedResponseProse(
        requireMessage(result.value.prose, `${path}.update.prose`),
        `${path}.update.prose`,
      );
      shown = revealedSoFar(rc.previous, markdown.length);
      windowMs = revealWindowMs(result.value.revealWindow, `${path}.update.reveal_window`);
      break;
    case "success":
      markdown = drawFeedResponseSuccess(result.value, `${path}.success`);
      windowMs = revealWindowMs(result.value.revealWindow, `${path}.success.reveal_window`);
      shown = windowMs === undefined ? markdown.length : revealedSoFar(rc.previous, markdown.length);
      break;
    case "error":
      markdown = drawFeedResponseError(result.value, `${path}.error`);
      shown = markdown.length;
      break;
  }
  const prose = markdownSlot(RESPONSE_PROSE_CLASS, markdown.slice(0, shown));

  // A RE-PUSH UPDATES THE BUBBLE IN PLACE (owner rule, 2026-09-23: the user
  // owns the scroll). The daemon re-pushes the whole row on every fragment of an
  // arriving response and again when it settles; the one bubble updates the
  // previous draw's bubble instead of building a fresh one, so the scroll box
  // keeps its identity (and a reader scrolled inside it keeps their place) and
  // the prose is reconciled node by node. Everything the wire can refuse was
  // read above, so a malformed push leaves the bubble exactly as it was.
  const { bubble, body, content } = drawBubble(
    {
      role: "response",
      variant: u.thinking ? "thinking" : "response",
      state: result.case,
      hooks,
      strip,
      ...(corner === undefined ? {} : { corner }),
      content: [prose],
      footer: result.case === "error" ? [cutShortMarker()] : [],
      ...responseCap(u),
    },
    rc.previous,
  );
  // THE SIZING GHOST'S TEXT (styles.css "THE SIZING GHOST"): the widest
  // duration labels the corner reserves, handed to the ghost that widens the
  // bubble by the corner's footprint. Set only while the bubble has a corner.
  if (corner === undefined) bubble.style.removeProperty(USAGE_RESERVE_LABELS_PROPERTY);
  else bubble.style.setProperty(USAGE_RESERVE_LABELS_PROPERTY, usageReserveLabelsCss());
  bubble.toggleAttribute("data-thinking", u.thinking);
  bubble.toggleAttribute("data-notice", u.notice !== undefined);
  markRevealed(bubble, shown);
  if (result.case === "update") {
    log.debug("drawing an arriving response", {
      operation: "feed.cards.response.update",
      context: {
        path: `${path}.update`,
        length: markdown.length,
        resumed: shown,
        in_place: bubble === rc.previous,
        reveal_window_ms: windowMs ?? null,
      },
    });
  }
  if (shown < markdown.length) {
    animate(bubble, body, content[0] as HTMLElement, markdown, shown, windowMs, rc);
  }
  const characters = markdown.length;
  recordDraw(u, rc, result.case, characters);
  return bubble;
}

/**
 * The bubble's collapsed line limit, drawn verbatim from the row's own state.
 *
 * A RESPONSE IS NEVER ABBREVIATED (owner request, 2026-09-27): every
 * non-thinking response — arriving, interim (pear) or the turn's answer
 * (green) — is `BUBBLE_UNCAPPED`, shown at its full height with no fade, no
 * scroll and no fold. It is uncapped from its first fragment, so settling
 * changes nothing about its box and reflows nothing.
 *
 * A THINKING BUBBLE IS SHOWN IN FULL WHILE ITS OWN TEXT IS STILL ARRIVING, and
 * collapses to the one-line thinking cap the moment it LANDS: when the daemon
 * re-pushes the row in a terminal arm (`thinkingLanded`), which is exactly when
 * its final text is whole and painted in full. Nothing here waits on, or looks
 * at, any other row. Only the DEFAULT limit changes: a bubble the reader
 * expanded wears `.expanded` on its scroll box, which the in-place redraw keeps
 * (src/bubble/draw.ts), so it stays open.
 */
export function responseCapLines(u: FeedResponse): BubbleCapLines {
  return responseCap(u).capLines;
}

/**
 * The bubble's cap: its collapsed line limit (`responseCapLines`) and its more
 * signal. A LANDED THINKING BUBBLE SAYS "MORE" WITH THE ELLIPSIS, never the
 * fade (owner ruling, 2026-09-27): its one collapsed line ends in `…` exactly
 * when anything follows it. A thinking bubble still arriving is under the
 * shared feed cap, which the ellipsis cannot state (no whole line count), so it
 * keeps the default fade until it lands.
 */
export function responseCap(u: FeedResponse): BubbleCapSpec {
  if (!u.thinking) return { capLines: BUBBLE_UNCAPPED };
  return thinkingLanded(u) ? { capLines: THINKING_CAP_LINES, more: BUBBLE_MORE_ELLIPSIS } : { capLines: "feed" };
}

/**
 * Whether U is a thinking row whose own final text has arrived: its arm is a
 * terminal one (`success`, or `error` for reasoning the turn's death cut short,
 * whose prose is equally final). An arriving `update` has not landed. An unset
 * arm has not landed either, and is not decided here: the draw refuses that row
 * as malformed (`drawFeedResponse`'s `requireCase`) before it paints anything.
 */
export function thinkingLanded(u: FeedResponse): boolean {
  if (!u.thinking) return false;
  switch (u.result.case) {
    case "success":
    case "error":
      return true;
    case "update":
    case undefined:
      return false;
  }
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
  // What it says, so an in-place redraw keeps a heading that did not change.
  heading.setAttribute(SAYS_ATTRIBUTE, u.heading);
  return heading;
}

/** The class the corner wears while its timestamp is revealed. */
export const USAGE_REVEALED_CLASS = "usage-corner--revealed";

/**
 * One instant per shape `formatAge` writes, each at the widest figures that
 * shape reaches: two-level and one-level days (to 999), hours and minutes,
 * and seconds alone.
 */
const USAGE_AGE_WIDEST_MS: readonly number[] = [
  (999 * 86_400 + 23 * 3_600) * 1000,
  999 * 86_400 * 1000,
  (23 * 3_600 + 59 * 60) * 1000,
  23 * 3_600 * 1000,
  (59 * 60 + 59) * 1000,
  59 * 60 * 1000,
  59 * 1000,
];

/** A label with every figure zeroed: in tabular figures, the same width. */
function zeroFigures(label: string): string {
  return label.replace(/\d/g, "0");
}

/** The duration's label for an age, as the corner draws it. */
function usageAgeLabel(ageMs: number): string {
  return `${formatAge(ageMs)} ago`;
}

/**
 * THE DURATION'S WIDTH RESERVE: the widest label of every shape the age
 * formatter writes, figures zeroed. The corner stacks them in one grid cell,
 * so the reserve is exactly as wide as the widest label the clock can tick to.
 * Derived from `formatAge` itself, so a formatter that changes its shapes
 * changes the reserve with it.
 */
export const USAGE_AGE_RESERVE_LABELS: readonly string[] = USAGE_AGE_WIDEST_MS.map((ms) =>
  zeroFigures(usageAgeLabel(ms)),
);

/** The custom property the sizing ghost (styles.css) reads its reserved labels from. */
export const USAGE_RESERVE_LABELS_PROPERTY = "--usage-reserve-labels";

/**
 * The reserve labels as one CSS string, one label per line (`\A`), for the
 * sizing ghost's `content`: drawn `white-space: pre`, its box is as wide as
 * the widest label, exactly as the corner's one-cell reserve grid is.
 */
export function usageReserveLabelsCss(): string {
  return `"${USAGE_AGE_RESERVE_LABELS.join("\\A ")}"`;
}

/**
 * Whether LABEL fits the reserve: some reserved label has its shape (the same
 * units in the same order) and at least as many figures. In tabular figures
 * that is "no wider". An age of a thousand days or more does not fit.
 */
export function usageAgeWithinReserve(label: string): boolean {
  const zeroed = zeroFigures(label);
  const shape = zeroed.replace(/0+/g, "0");
  return USAGE_AGE_RESERVE_LABELS.some(
    (reserved) => reserved.replace(/0+/g, "0") === shape && zeroed.length <= reserved.length,
  );
}

/**
 * The cost corner: the token figure, drawn verbatim, and — once the response
 * has SETTLED — the relative timestamp it reveals when hovered or focused.
 *
 * THE MARKUP (the stylesheet's `.usage-corner` comment has the mechanism):
 *
 *   span.usage-corner[data-tokens=<token text>]   ::before reserves the token's slot
 *     span.usage-reserve[aria-hidden]             reserves the gap + widest duration
 *       span.usage-reserve-label × N              one per age shape, stacked
 *     span.usage-slider                           out of flow; phase B moves it
 *       span.usage-stamp  <token text>            out of flow, at the slider's left
 *       span.usage-ago    "5m 30s ago"            the slider's only in-flow content;
 *                                                 phase A moves it
 *
 * The corner's in-flow content is the two reserves, so its float is the whole
 * HOVERED footprint from the first draw, arriving or settled, and the first
 * prose line that wraps around it never reflows on the reveal, the tick or the
 * settle. The stylesheet owns the two-phase slide; `prefers-reduced-motion`
 * drops it. A state class is toggled here too, so a keyboard focus reveals the
 * same timestamp a hover does.
 *
 * THE TIMESTAMP IS A LIVE CLOCK: it reads `formatAge(now - at_ms)` and repaints
 * once per shared tick, so "5m 30s ago" stays current while it is on screen.
 * The subscription is taken through `tickWhileShown`, a PRESENT clock: the
 * finished turn's backstop leaves it counting (an age stays true after the
 * turn ends), and the feed's teardown of the bubble unsubscribes it with no
 * disposer to remember here. A label the reserve cannot hold (an age of a thousand days or more)
 * would widen past it, so it is logged at ERROR, once per corner.
 *
 * NO TIMESTAMP WHILE ARRIVING: `at_ms` is zero until the response settles, and
 * a corner with no settled instant has an empty slider, marked
 * `data-arriving`, so the token sits at the right edge and nothing slides.
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
  corner.setAttribute(SAYS_ATTRIBUTE, `${u.text}|${String(atMs)}`);

  const reserve = document.createElement("span");
  reserve.className = "usage-reserve";
  reserve.setAttribute("aria-hidden", "true");
  for (const text of USAGE_AGE_RESERVE_LABELS) {
    const label = document.createElement("span");
    label.className = "usage-reserve-label";
    label.textContent = text;
    reserve.appendChild(label);
  }
  corner.appendChild(reserve);

  const slider = document.createElement("span");
  slider.className = "usage-slider";
  corner.appendChild(slider);

  const stamp = document.createElement("span");
  stamp.className = "usage-stamp";
  stamp.textContent = u.text;
  // THE FIGURE WEARS ITS HEAT, by the one rule the footer's tokens cell uses.
  const heat = requireMessage(u.heat, `${path}.heat`);
  stamp.style.color = tokenHeatColor(heat.position, `${path}.heat.position`);
  slider.appendChild(stamp);

  // The reserve is drawn either way; an arriving corner marks itself so its
  // empty slider never slides out.
  corner.toggleAttribute("data-arriving", atMs === 0);
  if (atMs > 0) {
    const ago = document.createElement("span");
    ago.className = "usage-ago";
    let overflowReported = false;
    tickWhileShown(ago, rc.ctx.ticker, (nowMs) => {
      const label = usageAgeLabel(nowMs - atMs);
      ago.textContent = label;
      if (overflowReported || usageAgeWithinReserve(label)) return;
      overflowReported = true;
      log.error("the usage corner's duration outgrew its reserved width", {
        operation: "feed.cards.response.usage-reserve-exceeded",
        context: { path, label, reserve: USAGE_AGE_RESERVE_LABELS },
      });
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
/**
 * The daemon's reveal window in milliseconds, or undefined when it sent none
 * (no full record of the model's fragment cadence yet, a subagent, a replay),
 * which leaves the pacing to `SmoothReveal`. A window of zero breaks the
 * contract (`expected_gap_ms` is always > 0) and is refused as malformed.
 */
export function revealWindowMs(window: FeedResponseRevealWindow | undefined, path: string): number | undefined {
  if (window === undefined) return undefined;
  if (window.expectedGapMs === 0) {
    throw new MalformedView(`${path}.expected_gap_ms`, "a reveal window is always longer than zero");
  }
  return window.expectedGapMs;
}

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
 * TWO PACINGS, chosen by whether the daemon sent a reveal window. With one,
 * everything not yet shown is spread evenly across WINDOWMS from this draw
 * (`windowedReveal`), so the type-out runs at the stream's own cadence and
 * finishes as the next push is expected. Without one, `SmoothReveal` chases
 * the frontier at a rate proportional to the backlog, as every bubble did
 * before the daemon measured anything.
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
  body: BubbleBody,
  prose: HTMLElement,
  markdown: string,
  resumed: number,
  windowMs: number | undefined,
  rc: RowContext,
): void {
  // The arriving prose wraps through the SAME body pipeline as the settled
  // whole, at the same measured width, so nothing shrinks or re-wraps when the
  // final lands. A width change repaints the slice already shown.
  const paint = (shown: number): void => {
    // REGRESSION WATCH (per-frame reveal flicker, 2026-09-15): this once did
    // `body.replaceChildren(...proseHtml(slice).childNodes, freshIndicator())`
    // on EVERY animation frame — a full teardown and rebuild of the whole prose
    // subtree ~60 times a second, so every settled line was destroyed and
    // recreated under the reader and the bubble flickered. `repaintSlot`
    // RECONCILES the live slot against the authoritative render
    // (reconcileChildren): unchanged leading nodes — stable prose paragraphs,
    // final tree lines — keep their identity and are never touched, and only
    // the growing tail is patched or appended. The reconciled result is
    // byte-identical to a fresh settled paint of the same text, so the reveal
    // never diverges from the oracle. Do NOT reintroduce a whole-subtree
    // rebuild per frame. This is a watch flag, not a lock.
    repaintSlot(body, prose, markdown.slice(0, shown));
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

  const next =
    windowMs === undefined ? smoothPace(markdown, resumed, rc) : windowedPace(markdown, resumed, windowMs, rc);

  // A bubble that WAS in the document and no longer is has been replaced by a
  // re-push, and its animation is over. One that was never mounted (a test, a
  // draw into a detached fragment) keeps animating: absence of a document is
  // not the same fact as removal from one.
  let mounted = bubble.isConnected;
  // A later paint of this same body (an in-place update) owns it from then on,
  // so this loop never paints the previous push's prose over it.
  const generation = paintGeneration(body);
  const step = (): void => {
    if (paintGeneration(body) !== generation) return;
    if (bubble.isConnected) mounted = true;
    else if (mounted) return;
    const shown = next();
    paint(shown);
    if (shown < markdown.length) frame(step);
  };
  frame(step);
}

/** A pacing: each call answers the length to show on this frame. */
type Pace = () => number;

/** The daemon-paced reveal: RESUMED to the whole, evenly across WINDOWMS. */
function windowedPace(markdown: string, resumed: number, windowMs: number, rc: RowContext): Pace {
  const start = rc.ctx.ticker.now();
  return () => {
    const at = windowedReveal(resumed, markdown.length, rc.ctx.ticker.now() - start, windowMs);
    return revealSlice(markdown, at).length;
  };
}

/** The unpaced reveal: `SmoothReveal` chasing the arrived frontier. */
function smoothPace(markdown: string, resumed: number, rc: RowContext): Pace {
  const reveal = new SmoothReveal({ now: () => rc.ctx.ticker.now() });
  const blockId = "response";
  // Seeding through the module's own "already shown" entry point rather than
  // reaching into its cursor map: this is exactly a restored render, which is
  // what `markShown` exists for. The resumed slice itself was painted with the
  // bubble.
  reveal.markShown({ items: [{ kind: "text", blockId, text: markdown.slice(0, resumed), done: false }] });
  return () => {
    const shown = reveal.reveal({ items: [{ kind: "text", blockId, text: markdown, done: false }] });
    return (shown.state.items[0] as { text: string }).text.length;
  };
}
