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
 *  - `errored` draws THE DAEMON'S HEADLINE, verbatim. The cause is named by the
 *    arm and worded by the producer, so there is no sentence table here to keep
 *    in step with the schema and no arm this end can re-word as another. The
 *    vendor's own sentence rides below it when the record carried one.
 *  - `interrupted` draws the stop as the stop it was — the user's act, never a
 *    failure.
 *
 * THE COUNTDOWN IS THE CLIENT'S. `retry_after_ms` is the vendor's WAIT, so the
 * deadline is this turn's own end plus that wait, and the figure ticks from the
 * shared clock. An UNSET wait is not zero: the vendor said nothing about when
 * to retry, and the wording says exactly that rather than inventing "now".
 */
import { formatDurationCeil } from "../../duration.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedId,
  FeedTurnEnded,
  FeedTurnEndedConcluded,
  FeedTurnEndedErrored,
  FeedTurnEndedInterrupted,
  FeedTurnErrorHeadline,
  FeedTurnErrorMessage,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { tick } from "../ticking.js";

const PATH = "FeedTurnEnded";

/**
 * The class the existing stylesheet already gives the answering response
 * bubble its green border through (`.bubble.assistant.final-response`). Reusing
 * it is what keeps the final-answer look one rule rather than two.
 */
export const FINAL_RESPONSE_CLASS = "final-response";

/**
 * The error arms that CARRY A WAIT, which is the only per-arm knowledge left in
 * this module now that the wording is the daemon's.
 *
 * Held as data so the suite can hold it against the schema: an arm that grows a
 * `retry_after_ms` without being listed here would silently stop counting down,
 * which is the one failure a verbatim headline cannot make loud on its own.
 */
export const TURN_ERROR_WAIT_ARMS: readonly string[] = ["rateLimited", "overloaded"];

/** The terminal row. */
export function drawFeedTurnEnded(msg: FeedTurnEnded, rc: RowContext): HTMLElement {
  const outcome = requireCase(msg.outcome, `${PATH}.outcome`);
  log("debug", `drawing a turn_ended row as ${outcome.case}`, {
    operation: "feed.draw-turn-ended",
    context: { outcome: outcome.case },
  });
  const endedAtMs = msOf(msg.endedAtMs, `${PATH}.ended_at_ms`);
  const el = ((): HTMLElement => {
    switch (outcome.case) {
      case "concluded":
        return drawFeedTurnEndedConcluded(outcome.value, rc);
      case "errored":
        return drawFeedTurnEndedErrored(outcome.value, endedAtMs, rc);
      case "interrupted":
        return drawFeedTurnEndedInterrupted(outcome.value);
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
    log("debug", "the turn concluded with no answering response to mark", {
      operation: "feed.turn-concluded-unanswered",
      context: {},
    });
    return el;
  }
  markFinalAnswer(concluded.answer, rc);
  return el;
}

/**
 * Put the final-answer treatment on the answering row.
 *
 * The lookup is THIS FEED's, because a `FeedId` on a turn_ended row names a row
 * of the same feed; a row that is not there (a page that has not been walked
 * back to, a producer naming a row it never sent) is reported and nothing is
 * marked, since guessing which row to green-border would be worse than none.
 */
function markFinalAnswer(answer: FeedId, rc: RowContext): void {
  const row = rc.findRowElement?.(answer) ?? null;
  if (row === null) {
    log("warn", "the concluded turn names an answering row this feed has not drawn", {
      operation: "feed.final-answer-row-absent",
      context: { answer: answer.value },
    });
    return;
  }
  row.setAttribute("data-final-answer", "true");
  // The existing green-border rule keys on the response bubble itself, so the
  // class goes where that rule can see it. A card drawn some other way still
  // carries the row-level marker above.
  const bubble = row.querySelector(".bubble.assistant");
  if (bubble !== null) bubble.classList.add(FINAL_RESPONSE_CLASS);
  log("debug", "marked the answering row with the final-answer treatment", {
    operation: "feed.final-answer-marked",
    context: { answer: answer.value, styled_bubble: bubble !== null },
  });
}

/** The died-mid-turn row: the vendor's sentence, the cause, and any wait. */
export function drawFeedTurnEndedErrored(
  errored: FeedTurnEndedErrored,
  endedAtMs: number,
  rc: RowContext,
): HTMLElement {
  const error = requireCase(errored.error, `${PATH}.errored.error`);
  const el = document.createElement("div");
  el.className = "turn-ended turn-ended-errored";
  el.setAttribute("data-arm", error.case);
  el.setAttribute("data-turn-error", error.case);

  el.append(
    drawFeedTurnErrorHeadline(requireMessage(errored.headline, `${PATH}.errored.headline`)),
  );

  if (errored.message !== undefined) {
    el.append(drawFeedTurnErrorMessage(errored.message));
  }
  const wait = retryWait(error);
  if (wait !== null) el.append(drawRetryCountdown(endedAtMs, wait, rc));

  log("debug", `drew the turn error arm ${error.case}`, {
    operation: "feed.draw-turn-error",
    context: {
      arm: error.case,
      has_vendor_message: errored.message !== undefined,
      has_wait: wait !== null && wait !== undefined,
    },
  });
  return el;
}

/**
 * THE DAEMON'S HEADLINE, drawn verbatim.
 *
 * The producer composes it from the arm — it is the one place the cause is
 * turned into words, including the unmodeled arm's vendor type — so this
 * function states the sentence and never inspects, appends to, or re-words it.
 * The field is REQUIRED: an errored row with no headline is a malformed view,
 * not a row to draw a stand-in sentence on.
 */
export function drawFeedTurnErrorHeadline(headline: FeedTurnErrorHeadline): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended-cause";
  el.textContent = headline.text;
  return el;
}

/**
 * The arm's wait, when the arm HAS one.
 *
 * `null` means this cause carries no wait at all; `undefined` means the arm
 * carries one and the vendor left it unset — which is a different fact, and the
 * countdown words it differently.
 */
function retryWait(error: { case: string; value: unknown }): bigint | undefined | null {
  if (!TURN_ERROR_WAIT_ARMS.includes(error.case)) return null;
  return (error.value as { retryAfterMs?: bigint }).retryAfterMs;
}

/** The vendor's own wording, when the record carried one. */
export function drawFeedTurnErrorMessage(message: FeedTurnErrorMessage): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended-vendor";
  el.textContent = message.text;
  return el;
}

/**
 * The ticking retry countdown.
 *
 * The deadline is this turn's own end plus the vendor's wait: the wire ships a
 * DURATION, and a duration counts down from the instant it was stated at. An
 * unset wait ticks nothing — there is no deadline to tick toward, and a figure
 * counting down from a number nobody gave would be invented.
 */
function drawRetryCountdown(
  endedAtMs: number,
  wait: bigint | undefined,
  rc: RowContext,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended-retry";
  if (wait === undefined) {
    el.setAttribute("data-retry", "unstated");
  el.setAttribute("data-retry-countdown", "unstated");
    el.textContent = "retry when ready";
    return el;
  }
  const deadlineMs = endedAtMs + msOf(wait, `${PATH}.errored.retry_after_ms`);
  el.setAttribute("data-retry", "countdown");
  el.setAttribute("data-retry-countdown", "ticking");
  tick(el, rc.ctx.ticker, (nowMs) => {
    const remaining = deadlineMs - nowMs;
    el.textContent =
      remaining > 0 ? `retry in ${formatDurationCeil(remaining)}` : "ready to retry";
  });
  return el;
}

/** The user's stop, stated as the user's act. */
export function drawFeedTurnEndedInterrupted(
  _interrupted: FeedTurnEndedInterrupted,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended turn-ended-interrupted";
  el.setAttribute("data-arm", "interrupted");
  el.textContent = "interrupted";
  return el;
}
