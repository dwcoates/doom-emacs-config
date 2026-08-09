/**
 * merge-dequeue — the DEQUEUE CARD, and the two sentences it can say.
 *
 * An interrupt over a workspace whose merge is on the queue no longer takes
 * that merge off. It raises a question (`WorkspaceState.merge_dequeue_offer`)
 * and this is what draws it: a picker-shaped card ringed in the alarm red,
 * distinct from the yellow every other picker wears, because the button on it
 * destroys minutes of work rather than choosing between two harmless answers.
 *
 * THE CARD IS A PURE FUNCTION OF THE PUSHED OFFER, exactly as the revival gate
 * is of the pushed hibernation. There is no local dismissed-flag: the daemon
 * clears the offer to take the card down, so a decision the webapp remembered
 * on its own would be a second owner of whether the question stands, and the
 * two would disagree the first time a merge ended by itself.
 *
 * THE TWO STANDINGS SAY DIFFERENT THINGS, which is the whole reason the offer
 * carries them as arms. A merge WAITING has not started, so the only fact worth
 * a sentence is how many are in front of it and dequeuing costs nothing. A
 * merge RUNNING is mid-something, so the sentence names the stage — and warns
 * that whatever it already landed stays landed, because that is the part a user
 * cannot undo by clicking again.
 */

import type { MergeDequeueOffer, MergeStatus } from "./frontend-proto.js";
import { escapeHtml } from "./highlight.js";

/** The attribute a dequeue card's buttons carry their answer in. */
export const DEQUEUE_ATTR = "data-merge-dequeue";

/** The card's heading. It names the verb, not the state. */
export const DEQUEUE_HEADING = "Take this workspace's merge off the queue?";

/**
 * The answers the card offers. They are the wire's two arms and nothing else,
 * so a button that could send neither is unrepresentable here too.
 */
export type DequeueAnswer = "dequeue" | "keep";

/**
 * Read an answer off a clicked button's attribute.
 *
 * It THROWS on anything else rather than resolving to a default. "Not the keep
 * button" is not the same statement as "dequeue" — one of these answers is
 * destructive — so a value this build does not recognize must not be guessed
 * at.
 */
export function dequeueAnswerFromAttr(raw: string | null): DequeueAnswer {
  if (raw === "dequeue" || raw === "keep") return raw;
  throw new Error(`merge-dequeue: unrecognized answer attribute ${JSON.stringify(raw)}`);
}

/**
 * The stage word for a running merge's status, or "" when the run has
 * published nothing yet.
 *
 * EVERY PHASE IS NAMED, including the ones a dequeue offer should never be
 * raised over (a merged or failed run is off the queue). They are named anyway
 * because the alternative is a default arm, and a default arm is what turns a
 * phase this build has not learned into a card that says nothing.
 */
export function dequeueStageText(status: MergeStatus | undefined): string {
  if (status === undefined) return "";
  const phase = status.phase;
  switch (phase.case) {
    case "enqueued":
      return `waiting at position ${phase.value.position} of ${phase.value.depth}`;
    case "beforeAction":
      return "running its pre-merge action";
    case "cherryPicking":
      return `cherry-picking commit ${phase.value.commitsLanded + 1} of ${phase.value.commitsTotal}`;
    case "testing":
      return `testing ${phase.value.commitsLanded} of ${phase.value.commitsTotal} landed commits`;
    case "conflict":
      return "parked on a conflict";
    case "afterAction":
      return "running its post-merge action";
    case "merged":
      return "already merged";
    case "failed":
      return "already failed";
  }
}

/**
 * The card's one explanatory sentence, chosen by the standing.
 *
 * Exported on its own because it is the part with the content: the markup
 * around it is a frame, and a test that asserted on the HTML would be
 * asserting on the frame.
 */
export function dequeueOfferText(offer: MergeDequeueOffer): string {
  const standing = offer.standing;
  if (standing.case === "waiting") {
    const { ahead, position, depth } = standing.value;
    const queue = `It is ${position} of ${depth} on the queue`;
    // The count is the fact a waiting user actually wants: "3 of 5" says where
    // it sits, "2 ahead of it" says how long the wait is, and only the second
    // answers the question that made them press the key.
    const wait =
      ahead === 1 ? "with 1 merge ahead of it" : `with ${ahead} merges ahead of it`;
    return `${queue}, ${wait}. Nothing of it has run yet, so taking it off costs nothing.`;
  }
  const stage = dequeueStageText(standing.value.status);
  const doing = stage === "" ? "This merge is running" : `This merge is ${stage}`;
  // THE WARNING IS THE POINT OF THE RED. Every commit is replayed in a
  // temporary worktree and only the finished result reaches the target, so an
  // abort usually costs the target nothing — but "usually" is not a promise a
  // card may make, and the one case where it is wrong is unrecoverable.
  return `${doing}. Dequeuing aborts the run; anything it has already landed on the target stays there.`;
}

/**
 * The one line shown at the click when an answer is REFUSED.
 *
 * The card comes down only when the daemon clears the offer, so a refused
 * answer leaves it standing — which reads as the button doing nothing unless
 * the refusal is said somewhere the user is already looking.
 */
export function dequeueRefusedNotice(answer: DequeueAnswer, cause: unknown): string {
  const verb = answer === "dequeue" ? "dequeue this merge" : "keep this merge queued";
  return `could not ${verb}: ${cause instanceof Error ? cause.message : String(cause)}`;
}

/**
 * The card, or "" when there is no question outstanding.
 *
 * The empty string IS the absent card: the caller writes this into its slot
 * every chrome frame, so a cleared offer takes the card down by the same path
 * that put it up.
 */
export function mergeDequeueCardHtml(offer: MergeDequeueOffer | null): string {
  if (offer === null) return "";
  const running = offer.standing.case === "running";
  return `
    <div class="merge-dequeue${running ? " running" : " waiting"}">
      <div class="merge-dequeue-head">${escapeHtml(DEQUEUE_HEADING)}</div>
      <div class="merge-dequeue-text">${escapeHtml(dequeueOfferText(offer))}</div>
      <div class="merge-dequeue-actions">
        <button ${DEQUEUE_ATTR}="dequeue" class="merge-dequeue-confirm">Dequeue</button>
        <button ${DEQUEUE_ATTR}="keep" class="merge-dequeue-keep">Keep it queued</button>
      </div>
    </div>`;
}
