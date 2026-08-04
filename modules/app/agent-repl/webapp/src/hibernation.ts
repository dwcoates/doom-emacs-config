/**
 * hibernation — the REVIVAL GATE, and the composer block that makes it the
 * first-order interaction on a sleeping session.
 *
 * A hibernated session has no shim: the daemon SIGTERMed it to reclaim its
 * ~500MB and left the registry record rehydratable. Waking it costs a bring-up,
 * and — the part the user actually has to decide — the woken conversation
 * carries its whole accumulated context, which every subsequent turn then pays
 * for. So revival is LAZY and GATED: the daemon nacks `SubmitPromptCmd` on a
 * hibernated session, the webapp renders this gate from
 * `SessionView.hibernation`, and exactly one `ReviveSessionCmd` answers it.
 *
 * WHY A BLOCKING GATE RATHER THAN A NOTICE. The two revival modes are not a
 * preference — `compactFirst` pays the full-context cost ONCE and `direct` pays
 * it on every turn afterwards — and a notice beside a live composer would let
 * the expensive one happen by default, chosen by nobody. The composer is
 * therefore disabled while the gate stands, on the same principle as the merge
 * gate (`merge-gate.ts`): the daemon would refuse the prompt anyway, and
 * refusing it here is what turns a vanished draft and a delayed nack into an
 * immediate explanation and a decision.
 *
 * WHY THE CAUSE IS SAID OUT LOUD. `HibernationDetail.cause` is a oneof, and its
 * three arms are three genuinely different pieces of news. "Asleep after an
 * hour idle" is routine. "You put this to sleep" is a receipt. "The cache
 * expired before a keep-alive ping could fire" is the one the user is meant to
 * ACT on — it means the warm-cache assumption behind `direct` no longer holds,
 * so the mode that is usually the cheap one is not. Collapsing them into
 * "session hibernated" would hide the only fact that changes the answer.
 *
 * NOTHING HERE IS DERIVED. The daemon resolved the cause, the cutoff, the
 * elapsed time and the TTL; this module renders them and clears the gate when a
 * pushed `SessionView` drops the field.
 */

import { formatAge } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import type { HibernationDetail } from "./frontend-proto.js";

/** The document-wide marker the chrome paints against while the gate stands. */
export const HIBERNATED_BODY_CLASS = "hibernated";

/** The gate's two actions, as the click vocabulary the delegation reads. */
export const REVIVE_COMPACT_ATTR = "data-revive-compact";
export const REVIVE_DIRECT_ATTR = "data-revive-direct";

/**
 * Whether a prompt may be submitted right now.
 *
 * The presence of the detail IS the block. There is no second condition and no
 * local timer: the gate stands exactly as long as the daemon says the session
 * is asleep.
 */
export function hibernationBlocked(hibernation: HibernationDetail | null): boolean {
  return hibernation !== null;
}

/**
 * The one-line reason the gate leads with, per cause arm.
 *
 * EXHAUSTIVE by construction: a fourth cause arm fails to COMPILE here rather
 * than falling through to a generic sentence, which is the failure mode this
 * whole surface exists to prevent — a gate that says "hibernated" while the
 * daemon knows something the user needed to hear.
 */
export function hibernationCauseText(hibernation: HibernationDetail): string {
  const cause = hibernation.cause;
  switch (cause.case) {
    case "idleCutoff":
      return (
        `This session was left alone for ${formatAge(cause.value.cutoffMs)}, so the daemon ` +
        `stopped keeping its cache warm and put it to sleep to reclaim its memory.`
      );
    case "forced":
      return "You put this session to sleep.";
    case "cacheExpired":
      return (
        `The prompt cache expired before a keep-alive ping could fire: the session had ` +
        `been idle for ${formatAge(cause.value.elapsedMs)}, past the ` +
        `${formatAge(cause.value.ttlMs)} cache lifetime. Pinging a cold cache would have ` +
        `paid the full re-ingest cost for nothing, so the daemon slept it instead.`
      );
    default: {
      const unhandled: never = cause;
      throw new Error(`hibernation: unhandled cause ${JSON.stringify(unhandled)}`);
    }
  }
}

/**
 * What "resume as-is" costs, worded for the cause in hand.
 *
 * On `cacheExpired` the warning is STRONGER and says so: the cache is already
 * known to be gone, so the first turn after a direct resume re-ingests the
 * whole conversation for certain, not merely probably.
 */
export function reviveDirectWarning(hibernation: HibernationDetail): string {
  if (hibernation.cause.case === "cacheExpired") {
    return (
      "Resume as-is: the whole accumulated context, and the cache behind it is already " +
      "gone — the next turn re-ingests all of it at full price, and every turn after " +
      "that carries the same context."
    );
  }
  return (
    "Resume as-is: the whole accumulated context, carried by every turn from here on. " +
    "The deliberate choice when you know the conversation is worth its size."
  );
}

/** What "compact first" buys, said the same way on every cause. */
export const REVIVE_COMPACT_EXPLANATION =
  "Compact first: summarize the conversation before anything else runs. This pays the " +
  "full-context cost once, instead of on every turn afterwards.";

/** The heading, so the gate is recognizable without parsing the prose below it. */
export const REVIVAL_GATE_HEADING = "This session is asleep";

/**
 * The composer's notice while the gate stands — short, because the gate card
 * above it carries the explanation and the decision.
 */
export const HIBERNATION_COMPOSER_NOTICE =
  "this session is asleep — choose how to wake it above before sending a prompt";

/** The disabled send button's tooltip, or "" when the session is awake. */
export function hibernationSendTitle(hibernation: HibernationDetail | null): string {
  return hibernationBlocked(hibernation) ? HIBERNATION_COMPOSER_NOTICE : "";
}

/** The composer notice's inner HTML, "" when awake (which collapses the slot). */
export function hibernationNoticeHtml(hibernation: HibernationDetail | null): string {
  if (!hibernationBlocked(hibernation)) return "";
  return `<span class="hibernation-gate-text">${escapeHtml(HIBERNATION_COMPOSER_NOTICE)}</span>`;
}

/**
 * The record written when a user tries to submit anyway.
 *
 * Returned rather than logged, so the caller writes it through the webapp's one
 * canonical logging API with its own operation and bound context, and so the
 * wording is assertable without a logger double (same discipline as
 * `mergeGateBlockedLog`).
 */
export function hibernationBlockedLog(
  promptLength: number,
  hibernation: HibernationDetail,
): string {
  return (
    `prompt submission blocked: the session is hibernated and the daemon nacks prompts ` +
    `until it is revived — cause=${hibernation.cause.case} ` +
    `since_ms=${String(hibernation.sinceMs)} prompt_length=${String(promptLength)} ` +
    `(draft retained)`
  );
}

/** Whether a revive has been sent and no cleared `SessionView` has landed yet. */
export type RevivePending = "compactFirst" | "direct" | null;

/** The pending line, so a sent decision never reads as nothing having happened. */
export function revivePendingText(pending: Exclude<RevivePending, null>): string {
  return pending === "compactFirst"
    ? "Waking the session and compacting first…"
    : "Waking the session with its full context…";
}

/**
 * The revival gate card, or "" when the session is awake.
 *
 * TWO actions and no third. There is no "dismiss": the gate is a pure function
 * of the daemon's live state, so a dismissed gate would reappear on the next
 * frame while having taught the user that the block is optional. And there is
 * no "cancel" — a hibernated session has nothing to cancel back to.
 *
 * While a decision is in flight the buttons are replaced by the pending line
 * rather than merely disabled: the daemon's answer is a pushed `SessionView`
 * that drops the field, so the honest report is "waiting", and leaving two
 * greyed buttons on screen would invite a second click on the other one.
 */
export function revivalGateHtml(
  hibernation: HibernationDetail | null,
  pending: RevivePending = null,
  now: number = Date.now(),
): string {
  if (hibernation === null) return "";
  const since =
    hibernation.sinceMs > 0
      ? `<span class="hibernation-since">asleep for ${escapeHtml(
          formatAge(now - hibernation.sinceMs),
        )}</span>`
      : "";
  const actions =
    pending === null
      ? `
      <div class="hibernation-actions">
        <button ${REVIVE_COMPACT_ATTR}="1" class="hibernation-compact">Compact first</button>
        <button ${REVIVE_DIRECT_ATTR}="1" class="hibernation-direct">Resume as-is</button>
      </div>`
      : `<div class="hibernation-pending">${escapeHtml(revivePendingText(pending))}</div>`;
  return `
    <div class="hibernation-gate cause-${escapeHtml(hibernation.cause.case)}">
      <div class="hibernation-head">
        <span class="hibernation-heading">${escapeHtml(REVIVAL_GATE_HEADING)}</span>
        ${since}
      </div>
      <div class="hibernation-cause">${escapeHtml(hibernationCauseText(hibernation))}</div>
      <div class="hibernation-choice">
        <div class="hibernation-option">${escapeHtml(REVIVE_COMPACT_EXPLANATION)}</div>
        <div class="hibernation-option warn">${escapeHtml(reviveDirectWarning(hibernation))}</div>
      </div>
      ${actions}
    </div>`;
}
