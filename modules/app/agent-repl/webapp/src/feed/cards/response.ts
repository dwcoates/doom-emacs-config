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
 * THE TYPE-OUT FOLLOWS THE ARRIVALS WITHIN THE DAEMON'S PACE, and it RESUMES
 * ACROSS PUSHES. Every live draw carries the daemon's reveal pace (the
 * learned minimum and maximum chars/sec and a bank limit, src/reveal-pace.ts);
 * a draw without one is a replay and is drawn whole. The daemon accumulates the
 * fragments and re-pushes the whole prose, and each push is drawn IN PLACE
 * over the bubble the previous draw returned (see `drawFeedResponse`). If the
 * reveal restarted there, a steadily growing response would re-type itself
 * from the top several times a second. So the shown length is carried on that
 * element (`data-revealed`) and the new draw resumes from it; the text arrived
 * past it is the bank the pace types out.
 *
 * THE TYPE-OUT CUTS THE RENDERED TEXT, NOT THE MARKDOWN. Each push renders its
 * whole prose once, and the reveal hides the rendered characters it has not
 * reached (`revealSlot`, src/bubble/text-reveal.ts), so no frame re-parses
 * markdown and a half-typed construct (`**`, an opening fence) is never drawn
 * as its raw syntax. Both attributes count rendered characters.
 */
import type { ParsedProse } from "../../../../proto/gen/ts/frontend/v1/prose_pb";
import { cssString, fitsReserve, reserveLabels, reserveLabelsCss } from "../../width-reserve.js";
import type {
  FeedResponse,
  FeedResponseError,
  FeedResponseNotice,
  FeedResponseProse,
  FeedResponseSuccess,
  FeedResponseUsageStamp,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawSignInAction } from "../../login/request.js";
import { pageResizeObserver } from "../../dom.js";
import { log } from "../../log.js";
import {
  BUBBLE_CLASS,
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_UNCAPPED,
  ELLIPSIS_CAP_LINES,
  SAYS_ATTRIBUTE,
  drawBubble,
  type BubbleCapLines,
  type BubbleCapSpec,
} from "../../bubble/draw.js";
import { formatAge } from "../../duration.js";
import {
  markdownSlot,
  proseCharacters,
  proseSource,
  paintGeneration,
  revealSlot,
  slotText,
  whenSlotPainted,
  type BubbleBody,
} from "../../bubble/body.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { revealPaceOf, type RevealPace } from "../../reveal-pace.js";
import { typeOut } from "../../type-out.js";
import { onDiscard, tickWhileShown } from "../ticking.js";
import { shouldShowMore } from "../bubble-more.js";
import { heatColor } from "../../heat.js";
import type { RowContext } from "./context.js";
import { FINAL_RESPONSE_CLASS } from "../rows/turn-ended.js";
import { easeIntoLook, readBubbleLook } from "../../bubble/collapse-motion.js";
import { drawOutcomeCard } from "../marker.js";

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
 * A thinking bubble's line limit: ONE line under the ellipsis, in EVERY arm
 * (owner request, 2026-10-08, replacing the fixed feed cap of 2026-10-07).
 * Arriving or landed, it is the same height, so its own text landing changes
 * nothing about its box and reflows nothing.
 */
export const THINKING_CAP_LINES = ELLIPSIS_CAP_LINES satisfies BubbleCapLines;

/**
 * An INTERIM response's line limit: ONE line under the ellipsis (owner
 * request, 2026-10-08). See `isInterimResponse` for which rows are interim.
 */
export const INTERIM_CAP_LINES = ELLIPSIS_CAP_LINES satisfies BubbleCapLines;

/**
 * The hook class an INTERIM response bubble wears, which the stylesheet keys
 * its page-colored fill on (owner request, 2026-10-08), so the fill and the
 * one-line cap are decided by the one predicate, `isInterimResponse`.
 */
export const INTERIM_RESPONSE_CLASS = "interim-response";

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
  // A VENDOR NOTICE IS NOT THE AGENT'S PROSE, so it is never a bubble: it is
  // the notice's outcome card, the drawing a fault ending's notices take.
  if (u.notice !== undefined) return drawFeedResponseNotice(u.notice, result.value.prose?.prose === undefined ? "" : proseSource(result.value.prose.prose), rc, `${path}.notice`);

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
  // `.final-response:not(.thinking-bubble)` rule. Selection leaves the green
  // border alone and greys the fill: the controller toggles the one
  // selected-entry class (`.entry-selected`, selected-entry.ts) on this bubble.
  if (u.finalAnswer && !u.thinking) hooks.push(FINAL_RESPONSE_CLASS);
  if (isInterimResponse(u)) hooks.push(INTERIM_RESPONSE_CLASS);

  if (result.case === "error") hooks.push("response-cut-short");

  // FIRST-LINE-ONLY RESERVATION VIA A ONE-LINE FLOAT (owner ruling, 2026-09-15).
  // The cost corner rides INSIDE the scroll box, before the body, floated
  // top-right, so the prose's FIRST line wraps around it and every later line
  // runs the full width (see `.usage-corner`).
  const interim = isInterimResponse(u);
  const corner =
    u.usage === undefined ? undefined : drawFeedResponseUsageStamp(u.usage, rc, `${path}.usage`, interim);

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

  // The prose is ONE markdown slot, painted whole by the one body pipeline.
  // While arriving, only what the previous draw of this row had already shown
  // is revealed, and the type-out resumes from there.
  //
  // THE SETTLED WHOLE TYPES OUT TOO: its unshown text is typed out at the
  // same pace as the rest of the block rather than appearing at once. A draw
  // with no pace (a replay) and the broken arm are drawn whole.
  let served: ParsedProse;
  let pace: RevealPace | undefined;
  switch (result.case) {
    case "update":
      served = drawFeedResponseProse(
        requireMessage(result.value.prose, `${path}.update.prose`),
        `${path}.update.prose`,
      );
      pace = revealPaceOf(result.value.revealPace, `${path}.update.reveal_pace`);
      break;
    case "success":
      served = drawFeedResponseSuccess(result.value, `${path}.success`);
      pace = revealPaceOf(result.value.revealPace, `${path}.success.reveal_pace`);
      break;
    case "error":
      served = drawFeedResponseError(result.value, `${path}.error`);
      pace = undefined;
      break;
  }
  // Read off the previous draw BEFORE the in-place redraw below reuses it.
  const resumed = revealedSoFar(rc.previous);
  // A WHOLE RESPONSE TURNING INTERIM EASES INTO IT (collapse-motion.ts): the
  // whole bubble's look is read here, while it still stands, and the interim
  // bubble that replaces it animates from it.
  const turningInterim =
    rc.previous !== undefined && isInterimResponse(u) && !rc.previous.classList.contains(INTERIM_RESPONSE_CLASS)
      ? readBubbleLook(rc.previous)
      : undefined;
  const prose = markdownSlot(RESPONSE_PROSE_CLASS, served);

  // A RE-PUSH UPDATES THE BUBBLE IN PLACE (owner rule, 2026-09-23: the user
  // owns the scroll). The daemon re-pushes the whole row on every fragment of an
  // arriving response and again when it settles; the one bubble updates the
  // previous draw's bubble instead of building a fresh one, so the scroll box
  // keeps its identity (and a reader scrolled inside it keeps their place) and
  // the prose is reconciled node by node. Everything the wire can refuse was
  // read above, so a malformed push leaves the bubble exactly as it was.
  const { bubble, box, body, content } = drawBubble(
    {
      role: "response",
      variant: u.thinking ? "thinking" : "response",
      state: result.case,
      hooks,
      ...(corner === undefined ? {} : { corner }),
      content: [prose],
      footer: result.case === "error" ? cutShortFooter(result.value) : [],
      ...responseCap(u),
    },
    rc.previous,
  );
  // THE SIZING GHOST'S TEXT (styles.css "THE SIZING GHOST"): the widest
  // duration labels the corner reserves, handed to the ghost that widens the
  // bubble by the corner's footprint. Set only while the bubble has a corner.
  if (corner === undefined) bubble.style.removeProperty(USAGE_RESERVE_LABELS_PROPERTY);
  else bubble.style.setProperty(USAGE_RESERVE_LABELS_PROPERTY, usageReserveLabelsCss());
  // AN OPEN INTERIM'S GHOST IS ITS RESTING CORNER, the token alone (styles.css
  // "AN INTERIM RESPONSE'S CORNER GROWS ITS BUBBLE"), so it reads the figure.
  if (corner === undefined || !interim) bubble.style.removeProperty(USAGE_TOKEN_TEXT_PROPERTY);
  else bubble.style.setProperty(USAGE_TOKEN_TEXT_PROPERTY, cssString(requireMessage(u.usage, `${path}.usage`).text));
  if (turningInterim !== undefined && turningInterim.height > 0 && bubble !== rc.previous) {
    easeIntoLook(bubble, turningInterim);
  }
  bubble.toggleAttribute("data-thinking", u.thinking);
  if (result.case === "update") {
    log.debug("drawing an arriving response", {
      operation: "feed.cards.response.update",
      context: {
        path: `${path}.update`,
        length: proseCharacters(served),
        resumed,
        in_place: bubble === rc.previous,
        reveal_pace: pace === undefined ? null : `${pace.minCps}-${pace.maxCps}/${pace.bankLimitSeconds}s`,
      },
    });
  }
  const slot = content[0] as HTMLElement;
  if (pace !== undefined) {
    revealSlot(body, slot, resumed);
    markRevealed(bubble, resumed);
    animate(bubble, box, body, slot, resumed, pace, rc);
  } else {
    showWhole(bubble, body, slot);
  }
  const characters = proseCharacters(served);
  recordDraw(u, rc, result.case, characters);
  return bubble;
}

/**
 * The bubble's collapsed line limit, drawn verbatim from the row's own state.
 *
 * THE TURN'S ANSWER IS NEVER ABBREVIATED (owner request, 2026-09-27): the
 * green final answer and a response the turn's death cut short are
 * `BUBBLE_UNCAPPED`, shown at their full height with no fade, no scroll and no
 * fold.
 *
 * AN INTERIM RESPONSE IS ONE LINE (owner request, 2026-10-08,
 * `INTERIM_CAP_LINES`): it collapses to its first line, ending in the ellipsis
 * when more follows, and expands to all of it. It is one line once the daemon
 * proves it interim (`isInterimResponse`); until then it is drawn in full,
 * like the answer it may still become. A box never switches mode in place, so
 * the draw that first carries the proof is a fresh, capped bubble
 * (src/bubble/draw.ts).
 *
 * A THINKING BUBBLE IS A FIXED HEIGHT FROM ITS FIRST FRAGMENT TO LONG AFTER
 * IT LANDS (`THINKING_CAP_LINES`, one line): its arm never changes its cap, so
 * it never collapses when its own text lands. Only the DEFAULT limit is fixed:
 * a bubble the reader expanded wears `.expanded` on its scroll box, which the
 * in-place redraw keeps (src/bubble/draw.ts), so it stays open.
 */
export function responseCapLines(u: FeedResponse): BubbleCapLines {
  return responseCap(u).capLines;
}

/**
 * The bubble's cap: its collapsed line limit (`responseCapLines`) and its more
 * signal. A thinking or interim bubble is one line, which says "more" with the
 * one bubble's ellipsis (`BUBBLE_MORE_ELLIPSIS`), the held prompt's own
 * mechanism; every other response is uncapped.
 */
export function responseCap(u: FeedResponse): BubbleCapSpec {
  if (u.thinking) return { capLines: THINKING_CAP_LINES, more: BUBBLE_MORE_ELLIPSIS };
  if (isInterimResponse(u)) return { capLines: INTERIM_CAP_LINES, more: BUBBLE_MORE_ELLIPSIS };
  return { capLines: BUBBLE_UNCAPPED };
}

/**
 * Whether U is an INTERIM response (owner request, 2026-10-08): prose the
 * daemon has PROVEN interim (`interim`: a later row of its turn landed after
 * it), that is not the turn's answer (`final_answer`), and that the turn's
 * death did not cut short. A response that is still its turn's latest row is
 * NOT interim, so a final answer is drawn in full from its first fragment and
 * never collapses, not even while it streams. Every fact here is the row's
 * own; which response is interim is the daemon's to say.
 */
export function isInterimResponse(u: FeedResponse): boolean {
  return !u.thinking && u.interim && !u.finalAnswer && u.result.case !== "error";
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
export function drawFeedResponseSuccess(u: FeedResponseSuccess, path: string): ParsedProse {
  log.debug("drawing a settled response", {
    operation: "feed.cards.response.success",
    context: { path },
  });
  return drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
}

/** The broken state: the partial markdown, kept on screen. */
export function drawFeedResponseError(u: FeedResponseError, path: string): ParsedProse {
  log.debug("drawing a response cut short", {
    operation: "feed.cards.response.error",
    context: { path },
  });
  return drawFeedResponseProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`);
}

/** The bubble's markdown element: the source, for the prose renderer. */
export function drawFeedResponseProse(u: FeedResponseProse, path: string): ParsedProse {
  const prose = requireMessage(u.prose, `${path}.prose`);
  log.debug("reading a response's prose", {
    operation: "feed.cards.response.prose",
    context: { path, length: proseCharacters(prose) },
  });
  return prose;
}

/**
 * A vendor notice drawn on its own: its outcome card, headed by the notice's
 * neutral marker, with the vendor's words verbatim as its face. The daemon
 * composes the head; this end states it and adds nothing.
 */
export function drawFeedResponseNotice(u: FeedResponseNotice, words: string, rc: RowContext, path: string): HTMLElement {
  log.debug("drawing a vendor notice as its card", {
    operation: "feed.cards.response.notice",
    context: { path, length: words.length },
  });
  return drawOutcomeCard(requireMessage(u.marker, `${path}.marker`), { ctx: rc.ctx }, `${path}.marker`, [words]);
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
export const USAGE_AGE_RESERVE_LABELS: readonly string[] = reserveLabels(USAGE_AGE_WIDEST_MS, usageAgeLabel);

/** The custom property the sizing ghost (styles.css) reads its reserved labels from. */
export const USAGE_RESERVE_LABELS_PROPERTY = "--usage-reserve-labels";

/** The custom property an open interim's ghost (styles.css) reads the token's text from. */
export const USAGE_TOKEN_TEXT_PROPERTY = "--usage-token-text";

/** The custom property an interim bubble's hover growth (styles.css) reads the duration's drawn width from. */
export const USAGE_AGO_WIDTH_PROPERTY = "--usage-ago-w";

/**
 * The reserve labels as one CSS string, one label per line (`\A`), for the
 * sizing ghost's `content`: drawn `white-space: pre`, its box is as wide as
 * the widest label, exactly as the corner's one-cell reserve grid is.
 */
export function usageReserveLabelsCss(): string {
  return reserveLabelsCss(USAGE_AGE_RESERVE_LABELS);
}

/**
 * Whether LABEL fits the reserve: some reserved label has its shape (the same
 * units in the same order) and at least as many figures. In tabular figures
 * that is "no wider". An age of a thousand days or more does not fit.
 */
export function usageAgeWithinReserve(label: string): boolean {
  return fitsReserve(label, USAGE_AGE_RESERVE_LABELS);
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
 *
 * AN INTERIM'S CORNER MEASURES ITS DURATION (INTERIM): its bubble opens to the
 * right by the duration's drawn width on hover (`measureUsageAgo`).
 */
export function drawFeedResponseUsageStamp(
  u: FeedResponseUsageStamp,
  rc: RowContext,
  path: string,
  interim: boolean,
): HTMLElement {
  const atMs = u.atMs === 0n ? 0 : msOf(u.atMs, `${path}.at_ms`);
  log.debug("drawing a response usage stamp", {
    operation: "feed.cards.response.usage-stamp",
    context: { path, settled: atMs > 0 },
  });

  const corner = document.createElement("span");
  corner.className = "usage-corner";
  corner.dataset.tokens = u.text;
  // What it says includes whether it measures (INTERIM), so a redraw never
  // keeps a corner that does not measure in a bubble that needs it.
  corner.setAttribute(SAYS_ATTRIBUTE, `${u.text}|${String(atMs)}|${interim ? "interim" : "whole"}`);

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
  stamp.style.color = heatColor(heat.position, `${path}.heat.position`);
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
    if (interim) measureUsageAgo(ago, path);

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

/**
 * Keep AGO's drawn width on its bubble, as `--usage-ago-w`, for an interim
 * bubble's hover growth (styles.css "AN INTERIM RESPONSE'S CORNER GROWS ITS
 * BUBBLE"): the bubble opens by exactly the duration, and a tick that changes
 * the label's width changes the opening with it. A `ResizeObserver` watches the
 * duration and is torn down with it through `onDiscard`. A corner only ever
 * hangs in the bubble it was drawn into (a redraw keeps it only inside the
 * bubble it already hangs in, and discards it otherwise), so the bubble is read
 * when a size lands, and a duration that hangs in none is a defect.
 */
function measureUsageAgo(ago: HTMLElement, path: string): void {
  const Observer = pageResizeObserver(ago);
  if (Observer === null) {
    log.error("an interim's usage corner cannot measure its duration: the page has no ResizeObserver", {
      operation: "feed.cards.response.usage-ago-unmeasured",
      context: { path },
    });
    return;
  }
  const observer = new Observer(() => {
    const bubble = ago.closest<HTMLElement>(`.${BUBBLE_CLASS}`);
    if (bubble === null) {
      log.error("a measured usage duration hangs in no bubble", {
        operation: "feed.cards.response.usage-ago-orphaned",
        context: { path, label: ago.textContent },
      });
      throw new Error(`${path}: a measured usage duration hangs in no bubble`);
    }
    const width = ago.getBoundingClientRect().width;
    bubble.style.setProperty(USAGE_AGO_WIDTH_PROPERTY, `${String(width)}px`);
  });
  observer.observe(ago);
  onDiscard(ago, () => observer.disconnect());
}

/** Record the shown length, so the next draw of this row resumes from it. */
function markRevealed(bubble: HTMLElement, length: number): void {
  bubble.setAttribute(REVEALED_ATTRIBUTE, String(length));
}

/**
 * The shown length, in rendered characters, the previous draw of this row
 * reached. The reveal itself clamps it to the text that has arrived.
 *
 * A missing or unparseable value is treated as "nothing shown yet": the
 * attribute is this renderer's own bookkeeping, never contract data.
 */
export function revealedSoFar(previous: HTMLElement | undefined): number {
  return Math.floor(carriedNumber(previous, REVEALED_ATTRIBUTE) ?? 0);
}

/**
 * A non-negative number the previous draw of this row carried on ATTRIBUTE, or
 * undefined when there is no previous draw, no attribute, or a value that is
 * not a non-negative number. Every reveal fact carried across a redraw is read
 * through here, so they share one parse.
 */
export function carriedNumber(previous: HTMLElement | undefined, attribute: string): number | undefined {
  const raw = previous?.getAttribute(attribute);
  if (raw === undefined || raw === null) return undefined;
  const parsed = Number.parseFloat(raw);
  return Number.isFinite(parsed) && parsed >= 0 ? parsed : undefined;
}

/**
 * Draw the prose whole at once: a replayed or broken bubble, or a collapsed
 * one whose visible lines are full. The shown length is recorded once the slot
 * has painted, since only the render knows how many characters it holds.
 */
function showWhole(bubble: HTMLElement, body: BubbleBody, slot: HTMLElement): void {
  revealSlot(body, slot, undefined);
  whenSlotPainted(slot, () => markRevealed(bubble, renderedText(slot).length));
}

/** SLOT's whole rendered text; only asked once it has painted. */
function renderedText(slot: HTMLElement): string {
  const text = slotText(slot);
  if (text === undefined) {
    throw new Error("response prose: a slot's text was read before it painted");
  }
  return text;
}

/** The broken bubble's marker. The WHY is the turn's terminal row. */
/**
 * The broken bubble's footer: the "cut short" marker, then — when the daemon
 * set it (`FeedResponseError.sign_in`: the turn ended on an account cause a
 * sign-in cures) — the sign-in action, inside the red-bordered bubble itself.
 */
function cutShortFooter(u: FeedResponseError): HTMLElement[] {
  const footer = [cutShortMarker()];
  if (u.signIn !== undefined) {
    const actions = document.createElement("div");
    actions.className = RESPONSE_CUT_SHORT_ACTIONS_CLASS;
    actions.append(drawSignInAction("response-sign-in", "feed.response-sign-in"));
    footer.push(actions);
  }
  return footer;
}

/** The class of the broken bubble's action row. */
export const RESPONSE_CUT_SHORT_ACTIONS_CLASS = "response-cut-short-actions";

function cutShortMarker(): HTMLElement {
  const marker = document.createElement("span");
  marker.className = "response-cut-short-marker";
  marker.textContent = "cut short";
  return marker;
}

/**
 * Pace the visible growth from RESUMED up to the whole rendered prose, through
 * the one type-out loop (src/type-out.ts), which owns the pacing, the frames,
 * the collapsed-full stop and the unseen-page stop.
 *
 * A push only grows the bank; nothing about the reveal's speed is carried
 * across pushes but the shown length, so the type-out reads the same however
 * the fragments were clumped. A later paint of this same body (an in-place
 * update) owns it from then on, so this loop never reveals the previous push's
 * prose over it. A COLLAPSED BUBBLE STOPS TYPING ONCE IT IS FULL (owner
 * request, 2026-10-10): its box's cap is measured by `shouldShowMore`, the
 * measure the "more" signal draws from, and every later push of the row
 * resumes from the whole it then drew and stops on its own first frame, so
 * the bubble lands as the text arrives. An expanded bubble (open folds never
 * show more) or an uncapped one (the final answer) types out to the end.
 */
function animate(
  bubble: HTMLElement,
  box: HTMLElement,
  body: BubbleBody,
  slot: HTMLElement,
  resumed: number,
  pace: RevealPace,
  rc: RowContext,
): void {
  const generation = paintGeneration(body);
  typeOut({
    ctx: rc.ctx,
    pace,
    resumed,
    operation: "feed.cards.response",
    logContext: { row: rc.row.id?.value ?? "unset" },
    element: bubble,
    reowned: () => paintGeneration(body) !== generation,
    total: () => slotText(slot)?.length,
    whenRendered: (fn) => whenSlotPainted(slot, fn),
    reveal: (shown) => {
      revealSlot(body, slot, shown);
      markRevealed(bubble, shown);
    },
    showWhole: () => showWhole(bubble, body, slot),
    full: () => shouldShowMore(box),
  });
}
