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
 * THE USAGE STAMP RIDES EVERY STATE, AND IS READ IN THE FOOTER. It is
 * meaningful while arriving (the vendor states usage when a response opens and
 * restates it as it grows), final once settled, and the last observed figure
 * on a broken bubble. The bubble draws none of it (owner request, 2026-10-25):
 * every draw records the stamp for the bubble (src/feed/response-usage.ts),
 * and hovering the bubble shows it in the footer's activity cell. Absent means
 * NO STAMP, never a zero: "we were not told what it cost" and "it cost
 * nothing" are different claims.
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
import type {
  FeedResponse,
  FeedResponseError,
  FeedResponseNotice,
  FeedResponseProse,
  FeedResponseRevision,
  FeedResponseSuccess,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawSignInAction } from "../../login/request.js";
import { log } from "../../log.js";
import {
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_UNCAPPED,
  ELLIPSIS_CAP_LINES,
  drawBubble,
  type BubbleCapLines,
  type BubbleCapSpec,
} from "../../bubble/draw.js";
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
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { revealPaceOf, type RevealPace } from "../../reveal-pace.js";
import { typeOut } from "../../type-out.js";
import { shouldShowMore } from "../bubble-more.js";
import type { RowContext } from "./context.js";
import { FINAL_RESPONSE_CLASS } from "../rows/turn-ended.js";
import { easeIntoLook, readBubbleLook } from "../../bubble/collapse-motion.js";
import { drawOutcomeCard } from "../marker.js";
import { linkAnswers } from "../entries.js";
import { bubbleCopyControl, reserveCopyRoom } from "../../bubble/copy-room.js";
import { readResponseUsage, setResponseUsage } from "../response-usage.js";
import { markComposerEntry } from "../composer-entry.js";
import { markRevealedNode } from "../revealed-node.js";
import { revisedWindow, type RevealWindow } from "../../bubble/text-reveal.js";
import { clearSelectionIn } from "../../selection.js";
import { responseHighlightTaken } from "../response-highlight.js";

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

  // WHERE A REVISION OF THIS RESPONSE STANDS is read before anything is
  // drawn too (FeedResponse.revision): pending makes it shimmer, failed says
  // why under the prose.
  const revision = readResponseRevision(u.revision, `${path}.revision`);
  if (revision.pending) hooks.push(RESPONSE_REVISING_CLASS);

  // The usage stamp is read before anything is drawn too, so a stamp the
  // wire got wrong refuses the row here rather than at the hover.
  const usage = u.usage === undefined ? undefined : readResponseUsage(u.usage, `${path}.usage`);

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
  // A SETTLED BUBBLE REDRAWN SETTLED (a revision, or the same block restated)
  // types out only what changed: the text it showed is read now, before the
  // redraw replaces it.
  const settledBefore = result.case === "success" ? settledTextOf(rc.previous) : undefined;
  // A WHOLE RESPONSE TURNING INTERIM EASES INTO IT (collapse-motion.ts): the
  // whole bubble's look is read here, while it still stands, and the interim
  // bubble that replaces it animates from it.
  const turningInterim =
    rc.previous !== undefined && isInterimResponse(u) && !rc.previous.classList.contains(INTERIM_RESPONSE_CLASS)
      ? readBubbleLook(rc.previous)
      : undefined;
  // A LABEL A LATER PROMPT ANSWERS IS A LINK to that prompt, restated on every
  // paint of the prose (entries.ts).
  const answers = u.answers;
  // A FINAL RESPONSE CARRIES A COPY CONTROL for its whole text (owner request,
  // 2026-10-24): the response's markdown as the daemon served it, never what
  // it renders as, at the bubble's top right on its first line of text
  // (src/bubble/copy-room.ts).
  const copy =
    u.finalAnswer && !u.thinking && result.case === "success"
      ? bubbleCopyControl("response", "Copy response", proseSource(served))
      : undefined;
  // THE SUBTREE THE COMPOSER IS ANSWERING is boxed on every paint too
  // (composer-entry.ts).
  const rowId = rc.row.id?.value ?? "";
  const prose = markdownSlot(RESPONSE_PROSE_CLASS, served, (rendered) => {
    if (answers.length > 0) linkAnswers(rendered, answers);
    markComposerEntry(rendered, rowId);
    markRevealedNode(rendered, rowId);
  });
  if (copy !== undefined) reserveCopyRoom([prose]);

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
      corner: copy === undefined ? [] : [copy],
      content: [prose],
      footer:
        result.case === "error"
          ? cutShortFooter(result.value)
          : revision.failed === undefined
            ? []
            : [revisionFailedMarker(revision.failed)],
      ...responseCap(u),
    },
    rc.previous,
  );
  // THE STAMP IS READ IN THE FOOTER while the bubble is hovered
  // (response-usage.ts), restated on every draw so a hovered bubble's footer
  // line follows its figure.
  setResponseUsage(bubble, usage);
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
  // A REVISION ON ITS WAY ENDS THE HIGHLIGHT that asked for it: the daemon
  // took the words with the prompt, and the purple would otherwise stay up
  // over words nothing now acts on. Both go: the reader's selection, and the
  // held words' paint (response-highlight.ts).
  if (revision.pending) {
    if (clearSelectionIn(bubble)) {
      log.debug("a revision is on its way; the highlight that asked for it was cleared", {
        operation: "feed.cards.response.revising",
        context: { row: rc.row.id?.value ?? "unset" },
      });
    }
    responseHighlightTaken(bubble.ownerDocument, rowId);
  }
  if (pace !== undefined && settledBefore !== undefined) {
    revealSlot(body, slot, resumed);
    markRevealed(bubble, resumed);
    whenSlotPainted(slot, () => {
      const window = revisedWindow(settledBefore, renderedText(slot), resumed);
      if (window.shown >= window.tailFrom) {
        showWhole(bubble, body, slot);
        return;
      }
      log.debug("a settled bubble was redrawn with different text; only the change types out", {
        operation: "feed.cards.response.revised",
        context: { row: rc.row.id?.value ?? "unset", shown: window.shown, tail_from: window.tailFrom },
      });
      revealSlot(body, slot, window);
      markRevealed(bubble, window.shown);
      animate(bubble, box, body, slot, window, pace, rc);
    });
  } else if (pace !== undefined) {
    revealSlot(body, slot, resumed);
    markRevealed(bubble, resumed);
    animate(bubble, box, body, slot, { shown: resumed, tailFrom: Number.POSITIVE_INFINITY }, pace, rc);
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
 * one bubble's ellipsis (`BUBBLE_MORE_ELLIPSIS`); every other response is
 * uncapped.
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

/**
 * The rendered text a SETTLED previous draw showed, whole, or undefined when
 * there is no previous draw, it was not settled, or its prose never painted.
 */
function settledTextOf(previous: HTMLElement | undefined): string | undefined {
  if (previous?.getAttribute("data-state") !== "success") return undefined;
  const slot = previous.querySelector<HTMLElement>(`.${RESPONSE_PROSE_CLASS}`);
  return slot === null ? undefined : slotText(slot);
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

/** The class a bubble wears while a revision of it is on its way. */
export const RESPONSE_REVISING_CLASS = "response-revising";

/** The class of the line saying why a revision failed. */
export const RESPONSE_REVISION_FAILED_CLASS = "response-revision-failed";

/** Where a revision of a response stands, as the bubble draws it. */
interface DrawnRevision {
  readonly pending: boolean;
  /** Why the last revision failed, in the daemon's words. */
  readonly failed?: string;
}

/** Read FeedResponse.revision; an arm a newer daemon set is refused. */
export function readResponseRevision(u: FeedResponseRevision | undefined, path: string): DrawnRevision {
  if (u === undefined) return { pending: false };
  const state = requireCase(u.state, `${path}.state`);
  switch (state.case) {
    case "pending":
      return { pending: true };
    case "failed":
      return { pending: false, failed: state.value.text };
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${path}.state`, other.case);
    }
  }
}

/** The line saying why a revision failed, under the prose that stands. */
function revisionFailedMarker(text: string): HTMLElement {
  return footerNote(RESPONSE_REVISION_FAILED_CLASS, text);
}

/** The class of the broken bubble's action row. */
export const RESPONSE_CUT_SHORT_ACTIONS_CLASS = "response-cut-short-actions";

function cutShortMarker(): HTMLElement {
  return footerNote("response-cut-short-marker", "cut short");
}

/**
 * A short italic note under a response's prose, wearing CLASSNAME: the cut-short
 * marker and the failed-revision line are both one, and the stylesheet draws
 * both from one rule.
 */
export function footerNote(className: string, text: string): HTMLElement {
  const note = document.createElement("span");
  note.className = className;
  note.textContent = text;
  return note;
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
  from: RevealWindow,
  pace: RevealPace,
  rc: RowContext,
): void {
  const generation = paintGeneration(body);
  typeOut({
    ctx: rc.ctx,
    pace,
    resumed: from.shown,
    operation: "feed.cards.response",
    logContext: { row: rc.row.id?.value ?? "unset" },
    element: bubble,
    reowned: () => paintGeneration(body) !== generation,
    // A KEPT TAIL IS ALREADY SHOWN, so the type-out ends where it begins.
    total: () => {
      const length = slotText(slot)?.length;
      return length === undefined ? undefined : Math.min(length, from.tailFrom);
    },
    whenRendered: (fn) => whenSlotPainted(slot, fn),
    reveal: (shown) => {
      revealSlot(body, slot, { shown, tailFrom: from.tailFrom });
      markRevealed(bubble, shown);
    },
    showWhole: () => showWhole(bubble, body, slot),
    full: () => shouldShowMore(box),
  });
}
