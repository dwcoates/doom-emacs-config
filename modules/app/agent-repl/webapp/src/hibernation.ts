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
import type { AdapterEffect, SessionViewInput } from "./state-adapter.js";

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

/**
 * The record written when the daemon refuses a revival decision.
 *
 * Returned rather than logged, for the same reason as
 * {@link hibernationBlockedLog}: the caller writes it through the webapp's one
 * canonical logging API, and the wording stays assertable without a double.
 *
 * The refusal leaves the session exactly as asleep as it was, so the line says
 * that — the gate is coming back up, and the user has a decision to make again.
 */
export function reviveRefusedLog(mode: Exclude<RevivePending, null>, cause: unknown): string {
  return (
    `revival refused: the daemon rejected the ${mode} decision, so the session is still ` +
    `asleep and the gate stands — cause=${causeText(cause)}`
  );
}

/**
 * The topbar status line for a refused hibernate.
 *
 * The sleep verb has no gate of its own to fall back to: the button simply
 * stops being offered when the session sleeps, so a refusal that rendered
 * nowhere near the click would read as the button doing nothing. The classified
 * card in the feed carries the daemon's full account; this is the one line at
 * the place the user is looking.
 */
export function hibernateRefusedNotice(cause: unknown): string {
  return `could not put this session to sleep: ${causeText(cause)}`;
}

/** A rejection's own words, whatever shape the rejection arrived in. */
function causeText(cause: unknown): string {
  return cause instanceof Error ? cause.message : String(cause);
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
 * What the next pushed `SessionView` said about an ACCEPTED revival.
 *
 * - `waiting` — no view for this workspace has landed yet, so there is nothing
 *   to conclude. Not a timeout: nothing here counts time.
 * - `revived` — the view dropped the hibernation field. The session is up.
 * - `failed` — the view STILL carries hibernation. The daemon accepted the
 *   decision and the session is nonetheless asleep, which is the bring-up
 *   having failed behind an ack that only ever meant "decision received".
 */
export type ReviveWatchVerdict =
  | { kind: "waiting" }
  | { kind: "revived"; mode: Exclude<RevivePending, null> }
  | {
      kind: "failed";
      mode: Exclude<RevivePending, null>;
      /** The detail the view STILL carried — the failed case's own evidence. */
      hibernation: HibernationDetail;
    };

/**
 * The ONE-SHOT expectation an accepted revival ack arms.
 *
 * WHY IT EXISTS. `reviveSession`'s ack means the daemon took the decision, not
 * that the session is up — the bring-up follows. So the gate deliberately keeps
 * its "waking…" line after the ack and waits for a pushed `SessionView` to drop
 * the hibernation field. That left exactly one state with no exit: an ack the
 * daemon ACCEPTED whose bring-up then failed. The session stayed asleep, the
 * gate stayed on "Waking the session…" with both buttons gone, and nothing —
 * no view, no ack, no error — could ever take it down again.
 *
 * WHAT BOUNDS IT. The next pushed `SessionView` for the workspace, and nothing
 * else. That is a WIRE FACT, not a deadline: the daemon pushes a view on every
 * session-state change and carries every session in the 15-second snapshot, so
 * the expectation resolves on the daemon's own next word about this session.
 * No timer is involved, and no local clock decides anything.
 *
 * ONE-SHOT because the question is answered once: the view either cleared the
 * field or it did not, and a second view is ordinary state, not a re-judgement
 * of a decision already resolved.
 */
export class ReviveWatch {
  private armed: { workspace: string; mode: Exclude<RevivePending, null> } | null = null;

  /** Arm on an ACCEPTED ack: the next view for `workspace` is the verdict. */
  arm(workspace: string, mode: Exclude<RevivePending, null>): void {
    this.armed = { workspace, mode };
  }

  /**
   * Drop the expectation without a verdict — for a REJECTED ack, where the
   * decision never reached the daemon's revival path at all and the caller has
   * already taken the gate's pending line down.
   */
  disarm(): void {
    this.armed = null;
  }

  /** The decision awaiting a verdict, or null when nothing is armed. */
  get pending(): RevivePending {
    return this.armed?.mode ?? null;
  }

  /**
   * Rule on one ingest batch.
   *
   * The LAST session view in the batch wins, exactly as it does in the store —
   * ruling on an earlier one would judge the revival by a state the store has
   * already superseded. A view carrying no workspace is not filtered out: the
   * session socket carries one session, and a pre-init view is silence about
   * identity, not evidence of a different workspace.
   */
  observe(effects: readonly AdapterEffect[]): ReviveWatchVerdict {
    const armed = this.armed;
    if (armed === null) return { kind: "waiting" };
    let latest: SessionViewInput | null = null;
    for (const effect of effects) {
      if (effect.kind !== "session-view") continue;
      const sv = effect.value;
      if (armed.workspace !== "" && sv.workspace !== "" && sv.workspace !== armed.workspace) {
        continue;
      }
      latest = sv;
    }
    if (latest === null) return { kind: "waiting" };
    this.armed = null;
    // The failed arm CARRIES its evidence rather than leaving the caller to
    // re-read the store for it: a verdict whose report depends on a second
    // lookup is a verdict that can be reported about the wrong state.
    return latest.hibernation === null
      ? { kind: "revived", mode: armed.mode }
      : { kind: "failed", mode: armed.mode, hibernation: latest.hibernation };
  }
}

/**
 * The gate's line when an accepted decision left the session asleep.
 *
 * It says the decision WAS taken, because it was — the user is being told the
 * wake-up failed, not that their click was ignored — and it hands the choice
 * back, which is the only thing left to offer.
 */
export const REVIVE_FAILED_TEXT =
  "That decision was accepted, but the session is still asleep: the daemon could not bring it " +
  "up. Choose again, or try the other mode.";

/**
 * The record written when an accepted revival left the session hibernated.
 *
 * Returned rather than logged, on the same discipline as
 * {@link hibernationBlockedLog}. The cause on the NEW detail is carried: a
 * bring-up that failed into `cacheExpired` when the gate was answered on
 * `idleCutoff` is the daemon saying something different happened.
 */
export function reviveFailedLog(
  mode: Exclude<RevivePending, null>,
  hibernation: HibernationDetail,
): string {
  return (
    `revival did not take: the daemon accepted the ${mode} decision and the next pushed ` +
    `SessionView still reports the session hibernated — cause=${hibernation.cause.case} ` +
    `since_ms=${String(hibernation.sinceMs)} (the gate is restored)`
  );
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
  failure = "",
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
  // The failed-revival line sits ABOVE the cause, because it is the newer news:
  // the cause explains how the session got here, and this explains why the
  // decision the user already made did not get it out.
  const failed =
    failure === "" ? "" : `<div class="hibernation-failed">${escapeHtml(failure)}</div>`;
  return `
    <div class="hibernation-gate cause-${escapeHtml(hibernation.cause.case)}">
      <div class="hibernation-head">
        <span class="hibernation-heading">${escapeHtml(REVIVAL_GATE_HEADING)}</span>
        ${since}
      </div>
      ${failed}
      <div class="hibernation-cause">${escapeHtml(hibernationCauseText(hibernation))}</div>
      <div class="hibernation-choice">
        <div class="hibernation-option">${escapeHtml(REVIVE_COMPACT_EXPLANATION)}</div>
        <div class="hibernation-option warn">${escapeHtml(reviveDirectWarning(hibernation))}</div>
      </div>
      ${actions}
    </div>`;
}
