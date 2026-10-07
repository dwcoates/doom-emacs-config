/**
 * held-offer — a question the daemon parked for the user, drawn as a card.
 *
 * THE ARM IS THE QUESTION. A consumer routes on `HeldOffer.offer` rather than
 * on anything inside it, because each arm names both the card drawn and the
 * answers available — and the answers are the ANSWER REQUEST's arms, never
 * fields on the view. That is why nothing here reads a list of buttons off the
 * wire: the two buttons below exist because `AnswerHeldOfferMergeDequeue` has
 * exactly two arms.
 *
 * RED, NOT THE PICKER'S YELLOW. Every other card that puts a question to the
 * user chooses between answers that can be given again; releasing a merge's
 * queue slot aborts a run that may have been going for minutes. The legacy
 * dequeue card wore the alarm red for exactly that reason and keeps it here,
 * with the destructive answer as the filled button so it cannot be mistaken
 * for the safe default.
 *
 * THE HEADLINE IS THE DAEMON'S SENTENCE. It is drawn verbatim; this end never
 * assembles a sentence from the offer's kind, and never re-states where the
 * merge stands — the footer's status and the feed's merge bubble already carry
 * that, and a second account of it would be a second thing to keep in step.
 */
import { labelledControl, CONTROL_SELECTOR, type Control } from "../control.js";
import type {
  HeldOffer,
  HeldOfferHeadline,
  HeldOfferMergeDequeue,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import {
  AnswerHeldOfferResponseSchema,
  type AnswerHeldOfferError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_held_offer_pb";
import { log } from "../log.js";
import { callUnary } from "../rpc/unary.js";
import { isMalformedView } from "../rpc/malformed.js";
import { guardMalformed } from "../rpc/guard.js";
import { crossCuttingSentence } from "../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TrayContext } from "./context.js";

/** The two answers the merge-dequeue question takes. */
export type MergeDequeueDecision = "keep" | "release";

/** One parked question. */
export function drawHeldOffer(u: HeldOffer, tc: TrayContext): HTMLElement {
  const path = "HeldOffer";
  const offer = requireCase(u.offer, `${path}.offer`);
  log.debug("drawing a held offer", {
    operation: "tray.held-offer",
    context: { offer: offer.case },
  });

  const card = document.createElement("div");
  card.className = "merge-dequeue held-offer";
  card.setAttribute("data-offer", offer.case);

  switch (offer.case) {
    case "mergeDequeue":
      card.appendChild(drawHeldOfferMergeDequeue(offer.value, tc, `${path}.merge_dequeue`));
      return card;
    default: {
      const other: { case: string } = offer;
      return unreachableArm(`${path}.offer`, other.case);
    }
  }
}

/** The merge-dequeue question: the daemon's sentence, and the two answers. */
export function drawHeldOfferMergeDequeue(
  u: HeldOfferMergeDequeue,
  tc: TrayContext,
  path: string,
): HTMLElement {
  log.debug("drawing a merge-dequeue offer", {
    operation: "tray.held-offer.merge-dequeue",
    context: { path },
  });
  const body = document.createElement("div");
  body.className = "held-offer-body";
  body.appendChild(
    drawHeldOfferHeadline(requireMessage(u.headline, `${path}.headline`), `${path}.headline`),
  );

  const actions = document.createElement("div");
  actions.className = "merge-dequeue-actions";
  // KEEP FIRST, and outlined: it is the answer that costs nothing. The release
  // sits beside it as the filled alarm-red button, which is the one thing on
  // this card that destroys work.
  actions.appendChild(decisionButton("keep", "Keep it queued", "merge-dequeue-keep", tc));
  actions.appendChild(decisionButton("release", "Release the slot", "merge-dequeue-confirm", tc));
  body.appendChild(actions);
  return body;
}

/** The composed sentence, verbatim. */
export function drawHeldOfferHeadline(u: HeldOfferHeadline, path: string): HTMLElement {
  log.debug("drawing a held offer headline", {
    operation: "tray.held-offer.headline",
    context: { path },
  });
  const headline = document.createElement("div");
  headline.className = "merge-dequeue-head";
  headline.textContent = u.text;
  return headline;
}

function decisionButton(
  decision: MergeDequeueDecision,
  label: string,
  extraClass: string,
  tc: TrayContext,
): Control {
  return labelledControl({
    className: extraClass,
    hook: ["data-offer-decision", decision],
    label,
    onClick: (button) => {
      void guardMalformed(tc.ctx, "tray.held-offer.answer", answer(decision, tc, button));
    },
  });
}

/**
 * Answer the question. A refusal is said AT THE CARD.
 *
 * The card comes down only when the daemon clears the offer, so a refused
 * answer leaves it standing — which reads as the button doing nothing unless
 * the refusal is drawn where the user is already looking.
 */
async function answer(
  decision: MergeDequeueDecision,
  tc: TrayContext,
  button: Control,
): Promise<void> {
  const actions = button.parentElement;
  clearRefusal(actions);
  setDisabled(actions, true);
  try {
    const response = await callUnary(
      tc.ctx,
      "AnswerHeldOffer",
      (client) =>
        client.answerHeldOffer({
          workspace: tc.ctx.workspace,
          answer: {
            case: "mergeDequeue",
            value: { decision: mergeDequeueDecision(decision) },
          },
        }),
      AnswerHeldOfferResponseSchema,
    );
    const result = requireCase(response.result, "AnswerHeldOfferResponse.result");
    if (result.case === "success") return;
    const cause = requireCase(
      (result.value).cause,
      "AnswerHeldOfferError.cause",
    );
    const say = crossCuttingSentence("AnswerHeldOffer", cause) ?? answerHeldOfferRefusal(cause);
    drawRefusal(actions, cause.case, say);
    log.warn(`AnswerHeldOffer refused a ${decision}`, {
      operation: "tray.held-offer.refused",
      context: { decision, arm: cause.case, sentence: say },
    });
  } catch (err) {
    // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE: the daemon answered, and
    // this renderer could not read the answer. It travels up loudly rather
    // than being drawn as "could not be reached", which would be a lie.
    if (isMalformedView(err)) throw err;
    drawRefusal(actions, "error", "the daemon could not be reached");
    log.error(`AnswerHeldOffer failed for a ${decision}: ${String(err)}`, {
      operation: "tray.held-offer.failed",
      context: { decision, cause: err },
    });
  } finally {
    if (actions !== null && actions.parentElement?.querySelector(".offer-refusal") != null) {
      setDisabled(actions, false);
    }
  }
}

/** The decision arm, built by name so a third answer is a compile error. */
function mergeDequeueDecision(
  decision: MergeDequeueDecision,
): { case: "keep"; value: Record<string, never> } | { case: "release"; value: Record<string, never> } {
  switch (decision) {
    case "keep":
      return { case: "keep", value: {} };
    case "release":
      return { case: "release", value: {} };
  }
}

function setDisabled(actions: Element | null, disabled: boolean): void {
  if (actions === null) return;
  for (const control of actions.querySelectorAll<Control>(CONTROL_SELECTOR)) control.disabled = disabled;
}

/** `AnswerHeldOfferError`'s cause union, narrowed to a SET arm. */
type AnswerHeldOfferCause = NonNullable<AnswerHeldOfferError["cause"]> & { case: string };

/**
 * This endpoint's OWN two arms, both of which mean the question the card is
 * asking no longer exists — so each says which way it stopped existing rather
 * than leaving a card standing over a button that appears to do nothing.
 */
export function answerHeldOfferRefusal(cause: AnswerHeldOfferCause): string {
  switch (cause.case) {
    case "noOfferStanding":
      return "there is no offer standing to answer";
    case "offerSuperseded":
      return "this offer has been superseded by a newer one";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("AnswerHeldOfferError.cause", other.case);
    }
  }
}

function drawRefusal(actions: Element | null, arm: string, message: string): void {
  if (actions === null) return;
  const refusal = document.createElement("div");
  refusal.className = "refusal offer-refusal";
  refusal.setAttribute("data-arm", arm);
  refusal.textContent = message;
  actions.after(refusal);
}

function clearRefusal(actions: Element | null): void {
  actions?.parentElement?.querySelectorAll(".offer-refusal").forEach((node) => node.remove());
}
