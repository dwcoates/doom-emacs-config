/**
 * turn-ended — how a turn ended, as a row.
 *
 * THE ROW'S EXISTENCE IS THE LIVENESS ANCHOR: no terminal row for the current
 * turn is what "the turn is live" means, so every turn gets one and a quiet
 * conclusion is a row that draws almost nothing. Drawing is the arm's business.
 *
 * THE THREE OUTCOMES DRAW DIFFERENTLY ON PURPOSE:
 *  - `concluded` says nothing in prose. Its whole visible effect is the
 *    FINAL-ANSWER TREATMENT on the response row it NAMES — the green border,
 *    applied to that row and to no other. Absence of a name draws no border
 *    anywhere, because a turn can conclude with no answering prose.
 *  - `errored` draws its OUTCOME MARKER (owner ruling, 2026-10-06) and nothing
 *    else: the inline pill `marker.ts` draws, never a bubble. The cause, its
 *    family's color and everything the marker expands to are the daemon's.
 *  - `interrupted` draws the NEUTRAL "interrupted" marker — the user's act,
 *    never a failure. An INTERJECTION's stop draws nothing at all: the prompt
 *    that superseded the turn, drawn as the active prompt, is its whole
 *    account. Its row still exists.
 */
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedId,
  FeedTurnEnded,
  FeedTurnEndedConcluded,
  FeedTurnEndedErrored,
  FeedTurnEndedInterrupted,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawOutcomeMarker } from "../marker.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";

const PATH = "FeedTurnEnded";

/**
 * The class the existing stylesheet already gives the answering response
 * bubble its green border through (`.bubble.assistant.final-response`). Reusing
 * it is what keeps the final-answer look one rule rather than two.
 */
export const FINAL_RESPONSE_CLASS = "final-response";

/**
 * THE MARK THE ANSWERING ROW WEARS, on its chrome.
 *
 * The feed's record, on the answering row's chrome, that its turn concluded on
 * it. It no longer drives the prompt's wave: whether a turn is working is the
 * prompt row's own daemon-stated `working` flag, which the client draws
 * verbatim.
 */
const FINAL_ANSWER_ATTRIBUTE = "data-final-answer";

/** The terminal row. */
export function drawFeedTurnEnded(msg: FeedTurnEnded, rc: RowContext): HTMLElement {
  const outcome = requireCase(msg.outcome, `${PATH}.outcome`);
  log.info(`drawing a turn_ended row as ${outcome.case}`, {
    operation: "feed.draw-turn-ended",
    context: { outcome: outcome.case },
  });
  // REQUIRED on every row, drawn or not: where the footer clock stopped.
  msOf(msg.endedAtMs, `${PATH}.ended_at_ms`);
  const el = ((): HTMLElement => {
    switch (outcome.case) {
      case "concluded":
        return drawFeedTurnEndedConcluded(outcome.value, rc);
      case "errored":
        return drawFeedTurnEndedErrored(outcome.value, rc);
      case "interrupted":
        return drawFeedTurnEndedInterrupted(outcome.value, rc);
      default:
        return unreachableArm(`${PATH}.outcome`, armName(outcome));
    }
  })();
  // The row's state is HOW THE TURN ENDED. The errored arm keeps its own
  // `data-turn-error` for which failure it was; this is the outcome above it.
  el.setAttribute("data-state", outcome.case);
  return el;
}

/**
 * The quiet conclusion: an empty marker row, plus the green border on the row
 * the producer named as the answer.
 *
 * The border is applied to the NAMED row and stripped from every other row of
 * this feed, so a re-push that moves the answer (a turn that concluded on a
 * later response) cannot leave two rows wearing it.
 */
export function drawFeedTurnEndedConcluded(
  concluded: FeedTurnEndedConcluded,
  rc: RowContext,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended turn-ended-concluded";
  el.setAttribute("data-arm", "concluded");
  if (concluded.answer === undefined) {
    log.debug("the turn concluded with no answering response to mark", {
      operation: "feed.turn-concluded-unanswered",
      context: {},
    });
    return el;
  }
  markFinalAnswer(concluded.answer, rc);
  return el;
}

/**
 * Record on the answering row's chrome that its turn's FINAL ANSWER has landed.
 *
 * THE GREEN BORDER IS NO LONGER APPLIED HERE. It is now a DATA property the
 * daemon stamps on the answering response row (`FeedResponse.final_answer`) and
 * the client draws from that flag on every draw (cards/response.ts), so no
 * live turn-ended event, redraw, re-arrange, or history replay can lose it —
 * which the one-shot green marking this function used to do repeatedly did lose.
 *
 * WHAT SURVIVES HERE is the `data-final-answer` marker on the answering row's
 * chrome. It carries neither the green look nor the prompt's wave (the prompt
 * row's own `working` flag does); it stays as the feed's record of which row
 * the conclusion named, and the absent-row report below.
 *
 * The lookup is THIS FEED's, because a `FeedId` on a turn_ended row names a row
 * of the same feed. A row that is not there is ORDINARY, not a fault: the
 * daemon names an answer only when it resolved it to a response row it drew
 * (resolve/feed turnended.go raises a footer fault otherwise), so an absent
 * row is one this page has not loaded -- older history above the page a fresh
 * webview opened on, whose turn_ended row came first. MEASURED 2026-09-29: a
 * webview precreated after an Emacs restart recorded this at WARN for a turn
 * whose answer sat on the page above. Nothing is marked, since guessing which
 * row is the answer would be worse than none.
 */
function markFinalAnswer(answer: FeedId, rc: RowContext): void {
  const row = rc.findRowElement?.(answer) ?? null;
  if (row === null) {
    log.debug("the concluded turn names an answering row this page has not loaded; nothing is marked", {
      operation: "feed.final-answer-row-absent",
      context: { answer: answer.value },
    });
    return;
  }
  // The marker rides the row CHROME, which the controller reuses across redraws,
  // so a response redraw (drawFeedResponse mints a fresh `.bubble.assistant` on
  // every push) cannot drop it. It is a record only: the green look is drawn
  // from the row's own `final_answer` data, and the prompt's wave from the
  // prompt row's own `working` flag.
  row.setAttribute(FINAL_ANSWER_ATTRIBUTE, "true");
  log.info("recorded the final-answer marker on the answering row", {
    operation: "feed.final-answer-marked",
    context: { answer: answer.value },
  });
}

/**
 * The died-mid-turn row: its OUTCOME MARKER, drawn verbatim. The arm and the
 * cause stay on the row as attributes, for the record and the hook contract.
 */
export function drawFeedTurnEndedErrored(
  errored: FeedTurnEndedErrored,
  rc: RowContext,
): HTMLElement {
  const error = requireCase(errored.error, `${PATH}.errored.error`);
  const el = document.createElement("div");
  el.className = "turn-ended turn-ended-errored";
  el.setAttribute("data-arm", error.case);
  el.setAttribute("data-turn-error", error.case);
  el.append(
    drawOutcomeMarker(
      requireMessage(errored.marker, `${PATH}.errored.marker`),
      { ctx: rc.ctx, previous: rc.previous },
      `${PATH}.errored.marker`,
    ),
  );
  log.info(`drew the turn error arm ${error.case} as its outcome marker`, {
    operation: "feed.draw-turn-error",
    context: { arm: error.case },
  });
  return el;
}

/**
 * The user's stop, drawn as its neutral marker — unless the stop was an
 * INTERJECTION, which draws nothing: an empty row, exactly as a quiet
 * conclusion is one. A direct stop and an UNSET command (a record that did not
 * say how) both draw the marker.
 */
export function drawFeedTurnEndedInterrupted(
  interrupted: FeedTurnEndedInterrupted,
  rc: RowContext,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended turn-ended-interrupted";
  el.setAttribute("data-arm", "interrupted");
  const command = interrupted.command;
  switch (command.case) {
    case "interjection":
      log.debug("an interjection's stop draws nothing; the superseding prompt is its account", {
        operation: "feed.draw-turn-ended-interjection",
        context: { command: command.case },
      });
      return el;
    case "direct":
    case undefined:
      el.append(
        drawOutcomeMarker(
          requireMessage(interrupted.marker, `${PATH}.interrupted.marker`),
          { ctx: rc.ctx, previous: rc.previous },
          `${PATH}.interrupted.marker`,
        ),
      );
      return el;
    default:
      return unreachableArm(
        `${PATH}.interrupted.command`,
        armName(command),
      );
  }
}
