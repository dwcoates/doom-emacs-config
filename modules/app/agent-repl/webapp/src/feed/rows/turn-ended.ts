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
  FeedTurnErrorQueryDied,
  FeedTurnErrorVendorUnmodeled,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking, tick } from "../ticking.js";

/** What each query-died cause says. */
export const QUERY_CAUSE_WORDS = {
  unexpectedEof: "the agent's stream ended without a close",
  iteratorFailure: "the agent sdk's iterator failed",
} as const satisfies Record<string, string>;

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
 * Spelled once and read back by the feed: the prompt bubble's wave ends when
 * its turn's FINAL ANSWER lands (owner ruling, 2026-09-14), and this attribute
 * is the feed's own record that it has — so `markWorkingPrompts` asks for it
 * by this name rather than by a second string that could drift from the one
 * written here.
 */
export const FINAL_ANSWER_ATTRIBUTE = "data-final-answer";

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
  log.info(`drawing a turn_ended row as ${outcome.case}`, {
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
    log.warn("the concluded turn names an answering row this feed has not drawn", {
      operation: "feed.final-answer-row-absent",
      context: { answer: answer.value },
    });
    return;
  }
  // THE ROW-LEVEL MARKER IS THE DURABLE STATE. The class below lives on the
  // bubble element, but a response redraw REBUILDS that element from scratch
  // (drawFeedResponse mints a fresh `.bubble.assistant` on every push, and the
  // daemon delivers a settled response TWICE — once per store plane paying out
  // the same success), so a class added here as a one-shot is dropped by the
  // trailing settled re-draw. The `data-final-answer` attribute rides the row
  // CHROME, which the controller reuses across redraws, so it survives; the
  // controller re-asserts the class from it on every (re)draw (feed-view's
  // `drawBody`), keeping the green durable rather than one-shot.
  row.setAttribute(FINAL_ANSWER_ATTRIBUTE, "true");
  const styled = applyFinalResponseClass(row);
  log.info("marked the answering row with the final-answer treatment", {
    operation: "feed.final-answer-marked",
    context: { answer: answer.value, styled_bubble: styled },
  });
}

/**
 * Put the green final-answer class on ROW's answering bubble, if it has one.
 *
 * The green-border rule keys on the response bubble itself, so the class goes
 * where that rule can see it. A card drawn some other way still carries only the
 * row-level marker its caller set. Returns whether a bubble was found to style.
 *
 * This is shared by the one-shot mark (`markFinalAnswer`) and the controller's
 * per-redraw re-assertion, so both apply the SAME class to the SAME bubble by
 * the SAME rule — there is one place that knows what "green the answer" means.
 *
 * A THINKING BUBBLE IS EXCLUDED FROM THE LOOKUP: it reuses `.bubble.assistant`
 * but is intermediate reasoning, never the answer, so it must never take the
 * green final-answer class. The daemon never files a thinking row as an answer,
 * so this branch is not normally reached for one; the `:not` here is the second
 * guard, matching the stylesheet's own `.final-response` exclusion.
 */
export function applyFinalResponseClass(row: HTMLElement): boolean {
  const bubble = row.querySelector(".bubble.assistant:not(.thinking-bubble)");
  if (bubble === null) return false;
  bubble.classList.add(FINAL_RESPONSE_CLASS);
  return true;
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

  if (error.case === "vendorUnmodeled") {
    el.append(drawFeedTurnErrorVendorUnmodeled(error.value));
  }
  if (error.case === "queryDied" && error.value.cause.case !== undefined) {
    el.append(drawFeedTurnErrorQueryCause(error.value.cause));
  }
  if (errored.message !== undefined) {
    el.append(drawFeedTurnErrorMessage(errored.message));
  }
  const wait = retryWait(error);
  if (wait !== null) el.append(drawRetryCountdown(endedAtMs, wait, rc));

  log.info(`drew the turn error arm ${error.case}`, {
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

/**
 * THE VENDOR'S OWN TYPE NAME, drawn verbatim.
 *
 * The arm exists because the vendor named a cause this contract does not model,
 * and its one field is "the vendor's type name, drawn verbatim"
 * (feed.proto, FeedTurnErrorVendorUnmodeled). So it is STATED, not folded into
 * a sentence: it is the only handle the reader has on what actually happened,
 * and the daemon's headline can only say that the cause was unmodeled.
 */
export function drawFeedTurnErrorVendorUnmodeled(
  unmodeled: FeedTurnErrorVendorUnmodeled,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended-vendor-type";
  el.setAttribute("data-vendor-type", unmodeled.type);
  el.textContent = unmodeled.type;
  return el;
}

/**
 * WHICH WAY THE QUERY DIED, when the producer named it.
 *
 * The headline says the query died; the cause says whether the agent binary's
 * stream ended without a close or the SDK's iterator threw — two different
 * faults with two different owners, which is the whole reason the arm was
 * given a cause. An UNSET cause appends nothing: the line stays exactly what
 * it was before the cause existed, rather than gaining a stand-in.
 */
export function drawFeedTurnErrorQueryCause(
  cause: FeedTurnErrorQueryDied["cause"],
): HTMLElement {
  const el = document.createElement("div");
  el.className = "turn-ended-query-cause";
  switch (cause.case) {
    case "unexpectedEof":
    case "iteratorFailure":
      el.setAttribute("data-query-cause", cause.case);
      el.textContent = QUERY_CAUSE_WORDS[cause.case];
      break;
    default:
      return unreachableArm(
        `${PATH}.errored.query_died.cause`,
        armName(cause as unknown as { case: string }),
      );
  }
  log.debug(`the query died: ${cause.case}`, {
    operation: "feed.turn-error-query-cause",
    context: { cause: cause.case },
  });
  return el;
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
    if (remaining > 0) {
      el.textContent = `retry in ${formatDurationCeil(remaining)}`;
      return;
    }
    // A TIMER STOPS THE MOMENT IT EXPIRES. "ready to retry" is terminal — the
    // wait is over and nothing after it can change the line — so the
    // subscription goes rather than rewriting the same sentence every second
    // for as long as the page is open.
    // `data-retry-countdown` keeps its contract value: the hook names which
    // FORM the line took (a countdown rather than the unstated one), and that
    // does not change when the countdown reaches its end.
    el.textContent = "ready to retry";
    stopTicking(el);
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
