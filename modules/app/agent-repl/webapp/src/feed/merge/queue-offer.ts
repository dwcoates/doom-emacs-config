/**
 * queue-offer — THE MERGE-QUEUE QUESTION, drawn as an overlay in the merge
 * bubble in place of its tabs.
 *
 * An interrupt of a workspace whose sync merge is queued, with no turn
 * running, changes nothing on the queue: the daemon raises this question on
 * the merge's pinned head (`FeedMerge.queue_offer`) and the bubble draws it
 * over its tabs, focused, until it is answered. THE ANSWER ALONE ACTS, and the
 * three answers are `AnswerMergeQueueOfferRequest`'s arms, never fields on the
 * view: Evict takes the merge off the queue, Prioritize moves it to the front
 * of the merges waiting, Cancel leaves the queue as it is.
 *
 * ANY ANSWER CLOSES THE OVERLAY AT ONCE. The daemon's next push of the head
 * carries no question, which is what keeps it closed; a refused or failed
 * answer brings the overlay back with the refusal said beneath its buttons,
 * since the question still stands.
 *
 * THE HEADLINE IS THE DAEMON'S SENTENCE, drawn verbatim.
 */
import { labelledControl, whileControlsDisabled, type Control } from "../../control.js";
import type { FeedMergeQueueOffer, FeedRow } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  AnswerMergeQueueOfferResponseSchema,
  type AnswerMergeQueueOfferError,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_merge_queue_offer_pb";
import { log } from "../../log.js";
import type { AppContext } from "../../rpc/context.js";
import { callUnary } from "../../rpc/unary.js";
import { isMalformedView } from "../../rpc/malformed.js";
import { guardMalformed } from "../../rpc/guard.js";
import { clearRefusalsAfter, crossCuttingSentence, drawRefusalAfter } from "../../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { BubbleOverlay, OverlayControls } from "../bubble.js";

const PATH = "FeedMerge.queue_offer";

/** The class this overlay's refusal notes carry. */
const REFUSAL_KIND = "offer-refusal";

/** The three answers the merge-queue question takes. */
export type QueueOfferDecision = "evict" | "prioritize" | "cancel";

/** The overlay attribute a test or a stylesheet finds the question by. */
export const QUEUE_OFFER_ATTRIBUTE = "data-merge-queue-offer";

/**
 * The merge bubble's overlay: the question while its head carries one, keyed
 * by its sentence so a re-push of the same question keeps the drawn overlay
 * (and its focus) rather than drawing it again.
 */
export function mergeQueueOfferOverlay(
  ctx: AppContext,
  mergeOf: (row: FeedRow) => { queueOffer?: FeedMergeQueueOffer },
): BubbleOverlay {
  return {
    keyOf: (row) => {
      const offer = mergeOf(row).queueOffer;
      if (offer === undefined) return undefined;
      return requireMessage(offer.headline, `${PATH}.headline`).text;
    },
    draw: (row, controls) =>
      drawMergeQueueOffer(requireMessage(mergeOf(row).queueOffer, PATH), ctx, controls),
  };
}

/** The question: the daemon's sentence, and the three answers. */
export function drawMergeQueueOffer(
  u: FeedMergeQueueOffer,
  ctx: AppContext,
  controls: OverlayControls,
): HTMLElement {
  log.debug("drawing the merge-queue question", { operation: "merge.queue-offer.draw" });
  const el = document.createElement("div");
  el.className = "merge-queue-offer";
  el.setAttribute(QUEUE_OFFER_ATTRIBUTE, "");

  const headline = document.createElement("div");
  headline.className = "merge-queue-offer-head";
  headline.textContent = requireMessage(u.headline, `${PATH}.headline`).text;

  const actions = document.createElement("div");
  actions.className = "merge-queue-offer-actions";
  // CANCEL IS THE FOCUSED ANSWER: the stop key brought the question up, and a
  // stray Enter must cost nothing. Evict is the filled alarm-red button, the
  // one answer on this overlay that throws work away.
  const cancel = decisionButton("cancel", "Cancel", "merge-queue-offer-cancel", ctx, controls);
  cancel.setAttribute("data-autofocus", "");
  actions.append(
    decisionButton("evict", "Evict", "merge-queue-offer-evict", ctx, controls),
    decisionButton("prioritize", "Prioritize", "merge-queue-offer-prioritize", ctx, controls),
    cancel,
  );
  el.append(headline, actions);
  return el;
}

function decisionButton(
  decision: QueueOfferDecision,
  label: string,
  extraClass: string,
  ctx: AppContext,
  controls: OverlayControls,
): Control {
  return labelledControl({
    className: extraClass,
    hook: ["data-offer-decision", decision],
    label,
    onClick: (button) => {
      void guardMalformed(ctx, "merge.queue-offer.answer", answer(decision, ctx, controls, button));
    },
  });
}

/**
 * Answer the question. The overlay closes at once; a refusal or a failure
 * brings it back, said at its buttons.
 */
async function answer(
  decision: QueueOfferDecision,
  ctx: AppContext,
  controls: OverlayControls,
  button: Control,
): Promise<void> {
  const actions = button.parentElement;
  clearRefusalsAfter(actions, REFUSAL_KIND);
  controls.dismiss();
  log.info(`answering the merge-queue question: ${decision}`, {
    operation: "merge.queue-offer.answer",
    context: { decision },
  });
  await whileControlsDisabled(actions, async () => {
    try {
      const response = await callUnary(
        ctx,
        "AnswerMergeQueueOffer",
        (client) =>
          client.answerMergeQueueOffer({
            workspace: ctx.workspace,
            decision: decisionArm(decision),
          }),
        AnswerMergeQueueOfferResponseSchema,
      );
      const result = requireCase(response.result, "AnswerMergeQueueOfferResponse.result");
      if (result.case === "success") return;
      const cause = requireCase(result.value.cause, "AnswerMergeQueueOfferError.cause");
      const say = crossCuttingSentence("AnswerMergeQueueOffer", cause) ?? answerMergeQueueOfferRefusal(cause);
      drawRefusalAfter(actions, REFUSAL_KIND, cause.case, say);
      controls.restore();
      log.warn(`AnswerMergeQueueOffer refused a ${decision}`, {
        operation: "merge.queue-offer.refused",
        context: { decision, arm: cause.case, sentence: say },
      });
    } catch (err) {
      // A MALFORMED VIEW IS NOT A TRANSPORT FAILURE: the daemon answered, and
      // this renderer could not read the answer. It travels up loudly rather
      // than being drawn as "could not be reached", which would be a lie.
      if (isMalformedView(err)) throw err;
      drawRefusalAfter(actions, REFUSAL_KIND, "error", "the daemon could not be reached");
      controls.restore();
      log.error(`AnswerMergeQueueOffer failed for a ${decision}: ${String(err)}`, {
        operation: "merge.queue-offer.failed",
        context: { decision, cause: err },
      });
    }
  });
}

/** The decision arm, built by name so a fourth answer is a compile error. */
function decisionArm(
  decision: QueueOfferDecision,
):
  | { case: "evict"; value: Record<string, never> }
  | { case: "prioritize"; value: Record<string, never> }
  | { case: "cancel"; value: Record<string, never> } {
  switch (decision) {
    case "evict":
      return { case: "evict", value: {} };
    case "prioritize":
      return { case: "prioritize", value: {} };
    case "cancel":
      return { case: "cancel", value: {} };
  }
}

/** `AnswerMergeQueueOfferError`'s cause union, narrowed to a SET arm. */
type AnswerMergeQueueOfferCause = NonNullable<AnswerMergeQueueOfferError["cause"]> & { case: string };

/**
 * This endpoint's OWN arm: the question the overlay asks no longer exists.
 */
export function answerMergeQueueOfferRefusal(cause: AnswerMergeQueueOfferCause): string {
  switch (cause.case) {
    case "noOfferStanding":
      return "there is no merge-queue question standing to answer";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("AnswerMergeQueueOfferError.cause", other.case);
    }
  }
}
