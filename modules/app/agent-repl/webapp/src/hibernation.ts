/**
 * hibernation — the REVIVAL GATE, and the composer block that makes it the
 * first-order interaction on a sleeping session.
 *
 * A hibernated session has no shim: the daemon SIGTERMed it to reclaim its
 * ~500MB and left the registry record rehydratable. Waking it costs a bring-up,
 * and — the part the user actually has to decide — the woken conversation
 * carries its whole accumulated context, which every subsequent turn then pays
 * for. So revival is LAZY and GATED: the daemon nacks `SubmitPromptCmd` on a
 * hibernated session, the webapp renders this gate from the fenced
 * `WorkspaceGateView`, and exactly one `ReviveSessionCmd` answers it.
 *
 * THE GATE IS READ FROM THE WORKSPACE, NOT FROM A SESSION CATALOG. It used to
 * come off `SessionView.hibernation`, which answers "what is true of session X"
 * and left this end to work out which X was current — while a connect snapshot
 * carries several entries per workspace, in no authority order, including
 * retired ones. `WorkspaceGateView` answers "what is true of this workspace
 * now", which is the only question a gate has, and it is fenced, so a view from
 * a session that has since rotated is discarded before it can raise a gate over
 * a workspace that is already awake.
 *
 * WHY A BLOCKING GATE RATHER THAN A NOTICE. The revival decisions are not a
 * preference — a compaction pays the full-context cost ONCE and `direct` pays
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
 * WHY THE SIZE IS SAID OUT LOUD TOO. The decision is how much context to carry,
 * and a card that never names the amount asks it in the abstract: "the whole
 * accumulated context" is the same sentence for a 12k session and a 400k one,
 * which are opposite answers. So the gate states the session's standing input
 * tokens (see `revivalContextSizeText`) above the options.
 *
 * NOTHING HERE IS DERIVED. The daemon resolved the cause, the cutoff, the
 * elapsed time, the TTL and the token total; this module renders them and
 * clears the gate when a pushed `SessionView` drops the field.
 */

import { formatAge } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import { formatTokens } from "./tokens.js";
import type { ReviveDecision } from "./frontend-command.js";
import type { HibernationDetail, WorkspaceGateView } from "./frontend-proto.js";
import type { AdapterEffect } from "./state-adapter.js";

/** The document-wide marker the chrome paints against while the gate stands. */
export const HIBERNATED_BODY_CLASS = "hibernated";

/**
 * The gate's actions, as the click vocabulary the delegation reads.
 *
 * ONE ATTRIBUTE CARRYING THE DECISION, not one attribute per button. With five
 * options a per-button attribute set would make "the button nobody wired up"
 * indistinguishable from "the button that means resume as-is": the old
 * two-button reader resolved anything-that-is-not-compact to `direct`, which
 * with a third option present would silently resume a conversation at full
 * context when the user asked to compact it. Reading the decision OUT of the
 * attribute, and refusing a value that is not one, removes that fallback.
 */
export const REVIVE_ATTR = "data-revive";

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

/**
 * The line printed when the daemon has reported no token total for the
 * session.
 *
 * IT SAYS UNKNOWN RATHER THAN ZERO. A gate that printed `0 input tokens` for a
 * session whose totals have not landed would recommend `direct` on the exact
 * evidence it does not have — the one reading of the figure that is never
 * recoverable by looking harder, because a free conversation and an
 * unmeasured one would print the same sentence.
 */
export const REVIVAL_CONTEXT_UNKNOWN_TEXT =
  "The daemon has reported no token total for this session yet, so how much context a " +
  "resume would carry is unknown.";

/**
 * WHAT THE DECISION IS ABOUT, IN TOKENS — the session's standing context size,
 * stated before the options are read.
 *
 * WHY IT IS ON THE CARD AT ALL. Every option below is a trade between what the
 * conversation keeps and what the next turn pays, and until this sentence
 * existed the card described the trade without ever naming its size: "the whole
 * accumulated context" reads identically at 12k and at 400k, and those two
 * sessions have opposite right answers. The figure is the one fact that decides
 * whether foregoing a compaction is thrift or an expensive mistake.
 *
 * IT IS THE INPUT SIDE, CACHED AND UNCACHED TOGETHER, because that is what a
 * resumed turn re-presents to the model: the cache hit and both misses are the
 * same standing prefix, and which bucket it lands in is a fact about the cache
 * at that moment, not about the size of the conversation. `SessionView.total_tokens`
 * — the daemon's fold of the last result's usage, and the same figure the
 * topbar chip prints — is that measure.
 *
 * NOTHING IS DERIVED HERE. The daemon resolved the total; this states it.
 */
export function revivalContextSizeText(contextTokens: number | null): string {
  if (contextTokens === null) return REVIVAL_CONTEXT_UNKNOWN_TEXT;
  return (
    `This session carries ${formatTokens(contextTokens)} input tokens of context, cached and ` +
    `uncached together. Every option below that does not compact or clear carries all of ` +
    `them into the next turn, which re-ingests them.`
  );
}

/** What compacting EVERYTHING buys, said the same way on every cause. */
export const REVIVE_COMPACT_EXPLANATION =
  "Compact everything: summarize the whole conversation before anything else runs. This " +
  "pays the full-context cost once, instead of on every turn afterwards, and keeps the " +
  "least.";

/**
 * The three SCOPED compactions.
 *
 * WHY THEY EXIST. "Compact" was one button for a decision that is really two:
 * how much to spend, and what to lose. A long agent conversation is not evenly
 * made of anything — the replies are usually its bulk, the prompts are usually
 * what the user most wants back word for word, and the tool calls and their
 * results are what the agent needs intact to carry on the work. Compacting all
 * of it to reclaim context spent from one of those had no way to be asked for.
 *
 * EACH LINE SAYS WHAT SURVIVES, not only what goes. What is kept is the part
 * the user is deciding about; what is summarized is the consequence.
 */
export const REVIVE_COMPACT_RESPONSES_EXPLANATION =
  "Compact responses only: summarize what the agent said back, and keep your prompts, the " +
  "tool calls and their results word for word. The replies are usually the bulk of a long " +
  "conversation.";

export const REVIVE_COMPACT_PROMPTS_EXPLANATION =
  "Compact prompts only: summarize what you asked for, and keep everything the agent " +
  "produced word for word. For a conversation whose value is in the work rather than in " +
  "how it was requested.";

export const REVIVE_COMPACT_PROMPTS_AND_RESPONSES_EXPLANATION =
  "Compact prompts and responses: summarize the conversation on both sides, and keep the " +
  "tool calls and their results word for word. The work survives; the talking about it " +
  "does not.";

/**
 * What "clear" does, said the same way on every cause.
 *
 * IT SAYS "DISCARDED", NOT "COMPACTED". Every other option on this card keeps
 * something — that is what the scopes are for — and this one keeps nothing.
 * They are one click apart, so the sentence has to make the difference
 * impossible to miss rather than describing it as one more way of making the
 * conversation smaller.
 */
export const REVIVE_CLEAR_EXPLANATION =
  "Clear: discard the conversation entirely — a plain /clear on the workspace. Nothing is " +
  "summarized and nothing is carried forward, so the woken session starts empty and costs " +
  "nothing to resume.";

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
export type RevivePending = ReviveDecision | null;

/**
 * One offered answer to the gate: the decision it sends, the button's words,
 * and the sentence explaining what it costs and what it keeps.
 */
export interface ReviveOption {
  decision: ReviveDecision;
  label: string;
  explanation: string;
  /**
   * Rendered as the warning variant — an option whose consequence a scoped
   * compaction does not carry (`direct` keeps the whole bill on every later
   * turn, `clear` keeps nothing at all).
   */
  warn: boolean;
  /**
   * The button's class.
   *
   * IT IS A FIELD RATHER THAN A CONDITION AT THE RENDER SITE. It used to be an
   * inline `decision === "direct" ? … : "hibernation-compact"`, which silently
   * gave every future non-compaction the filled cheap-path style — so `clear`
   * would have rendered as the recommended option. The table decides.
   */
  className: string;
}

/**
 * The gate's offered answers, in the order they are shown.
 *
 * ORDERED CHEAPEST-KEPT TO MOST-KEPT, so the list reads as one axis: compacting
 * everything keeps the least, resuming as-is keeps all of it, and the scoped
 * three sit between. A user scanning it is choosing a point on that axis rather
 * than picking between five unrelated buttons.
 *
 * DERIVED FROM THE DETAIL, because the direct option's sentence is stronger on
 * `cacheExpired` — the cache is already known to be gone there — and a table of
 * constants could not say so.
 */
export function reviveOptions(hibernation: HibernationDetail): ReviveOption[] {
  return [
    {
      decision: "compactAll",
      label: "Compact everything",
      explanation: REVIVE_COMPACT_EXPLANATION,
      warn: false,
      className: "hibernation-compact",
    },
    {
      decision: "compactPromptsAndResponses",
      label: "Compact prompts + responses",
      explanation: REVIVE_COMPACT_PROMPTS_AND_RESPONSES_EXPLANATION,
      warn: false,
      className: "hibernation-compact",
    },
    {
      decision: "compactResponses",
      label: "Compact responses only",
      explanation: REVIVE_COMPACT_RESPONSES_EXPLANATION,
      warn: false,
      className: "hibernation-compact",
    },
    {
      decision: "compactPrompts",
      label: "Compact prompts only",
      explanation: REVIVE_COMPACT_PROMPTS_EXPLANATION,
      warn: false,
      className: "hibernation-compact",
    },
    {
      decision: "direct",
      label: "Resume as-is",
      explanation: reviveDirectWarning(hibernation),
      warn: true,
      className: "hibernation-direct",
    },
    // CLEAR SITS AT THE END, off the axis rather than at its cheap end. The
    // four compactions and the direct resume are one question — how much of
    // this conversation is worth carrying — and clear answers a different one:
    // none of it. Putting it beside "compact everything", where the axis would
    // otherwise place it, would invite it as the cheapest compaction.
    {
      decision: "clear",
      label: "Clear",
      explanation: REVIVE_CLEAR_EXPLANATION,
      warn: true,
      className: "hibernation-clear",
    },
  ];
}

/**
 * The decision a clicked button carries, or a THROWN error when the attribute
 * holds something that is not one.
 *
 * IT THROWS RATHER THAN DEFAULTING. Every value it can legitimately see is put
 * there by {@link reviveOptions} a few lines above it, so anything else is this
 * module having broken — and the two ways of coping with that quietly are both
 * worse than a crash: falling back to `direct` resumes at full context a user
 * who asked to compact, and falling back to `compactAll` summarizes away a
 * conversation nobody consented to lose.
 */
export function reviveDecisionFromAttr(value: string | null): ReviveDecision {
  if (value !== null && (REVIVE_DECISIONS as readonly string[]).includes(value)) {
    return value as ReviveDecision;
  }
  throw new Error(
    `hibernation: the revival gate produced an unknown decision ${JSON.stringify(value)}; ` +
      `expected one of ${REVIVE_DECISIONS.join(", ")}`,
  );
}

/** Every decision the gate can send, as the recognized attribute vocabulary. */
const REVIVE_DECISIONS = [
  "compactAll",
  "compactPromptsAndResponses",
  "compactResponses",
  "compactPrompts",
  "direct",
  "clear",
] as const satisfies readonly ReviveDecision[];

/**
 * The pending line, so a sent decision never reads as nothing having happened.
 *
 * EXHAUSTIVE by construction, on the same discipline as
 * {@link hibernationCauseText}: a sixth decision fails to compile here rather
 * than falling through to a line that describes a different one.
 */
export function revivePendingText(pending: Exclude<RevivePending, null>): string {
  switch (pending) {
    case "compactAll":
      return "Waking the session and compacting the whole conversation…";
    case "compactPromptsAndResponses":
      return "Waking the session and compacting its prompts and responses…";
    case "compactResponses":
      return "Waking the session and compacting its responses…";
    case "compactPrompts":
      return "Waking the session and compacting its prompts…";
    case "direct":
      return "Waking the session with its full context…";
    case "clear":
      return "Waking the session and clearing the conversation…";
    default: {
      const unhandled: never = pending;
      throw new Error(`hibernation: unhandled revival decision ${JSON.stringify(unhandled)}`);
    }
  }
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
    let latest: WorkspaceGateView | null = null;
    for (const effect of effects) {
      // THE GATE, not a session view. The catalog entry this used to read is a
      // per-session fact that a snapshot carries several of per workspace, in
      // no authority order; the gate is a per-workspace fact and is fenced, so
      // a stale one never reaches this batch at all.
      if (effect.kind !== "fenced-view" || effect.value.case !== "workspaceGate") continue;
      const gate = effect.value.value;
      if (armed.workspace !== "" && gate.workspace !== "" && gate.workspace !== armed.workspace) {
        continue;
      }
      latest = gate;
    }
    if (latest === null) return { kind: "waiting" };
    this.armed = null;
    // The failed arm CARRIES its evidence rather than leaving the caller to
    // re-read the store for it: a verdict whose report depends on a second
    // lookup is a verdict that can be reported about the wrong state.
    return latest.gate.case === "open"
      ? { kind: "revived", mode: armed.mode }
      : { kind: "failed", mode: armed.mode, hibernation: latest.gate.detail };
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
 * Everything the gate is drawn from.
 *
 * IT IS A RECORD RATHER THAN POSITIONALS, and `contextTokens` is why. The
 * three optional arguments this used to trail had accumulated to the point
 * where a fourth would have been a required value sitting behind three
 * defaulted ones — impossible to express, so the size of the conversation
 * would have had to become optional too, and an omitted one would have
 * silently printed {@link REVIVAL_CONTEXT_UNKNOWN_TEXT} at a caller that knew
 * the number perfectly well. A required FIELD makes the omission a compile
 * error instead.
 */
export interface RevivalGateInput {
  /** The daemon's hibernation detail, or null for an awake session. */
  hibernation: HibernationDetail | null;
  /**
   * The session's standing context size (`SessionView.total_tokens`), or null
   * when the daemon has reported none. NULL IS ABSENCE, never a zero: see
   * {@link REVIVAL_CONTEXT_UNKNOWN_TEXT}.
   */
  contextTokens: number | null;
  /** A decision already sent and not yet ruled on, which replaces the buttons. */
  pending?: RevivePending;
  /** The clock the "asleep for" age is measured against. */
  now?: number;
  /** The failed-revival line, or "" when no accepted decision has failed. */
  failure?: string;
}

/**
 * The revival gate card, or "" when the session is awake.
 *
 * THE ACTIONS ARE THE REVIVAL DECISIONS AND NOTHING ELSE. There is no "dismiss": the gate is a pure function
 * of the daemon's live state, so a dismissed gate would reappear on the next
 * frame while having taught the user that the block is optional. And there is
 * no "cancel" — a hibernated session has nothing to cancel back to.
 *
 * While a decision is in flight the buttons are replaced by the pending line
 * rather than merely disabled: the daemon's answer is a pushed `SessionView`
 * that drops the field, so the honest report is "waiting", and leaving two
 * greyed buttons on screen would invite a second click on the other one.
 */
export function revivalGateHtml(input: RevivalGateInput): string {
  const { hibernation, contextTokens, pending = null, now = Date.now(), failure = "" } = input;
  if (hibernation === null) return "";
  const since =
    hibernation.sinceMs > 0
      ? `<span class="hibernation-since">asleep for ${escapeHtml(
          formatAge(now - hibernation.sinceMs),
        )}</span>`
      : "";
  // THE OPTIONS ARE ONE LIST, each button beside the sentence that explains it.
  // Splitting the labels from the explanations — which is what the two-option
  // card did — stops working the moment the explanations outnumber the fingers
  // of one hand: the reader has to pair them by position, and a mispaired
  // reading here chooses what a conversation loses.
  const options = reviveOptions(hibernation);
  const actions =
    pending === null
      ? `
      <div class="hibernation-actions">${options
        .map(
          (option) => `
        <div class="hibernation-option${option.warn ? " warn" : ""}">
          <button ${REVIVE_ATTR}="${escapeHtml(option.decision)}" class="${escapeHtml(
            option.className,
          )}">${escapeHtml(option.label)}</button>
          <span class="hibernation-option-text">${escapeHtml(option.explanation)}</span>
        </div>`,
        )
        .join("")}
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
      <div class="hibernation-context">${escapeHtml(revivalContextSizeText(contextTokens))}</div>
      ${actions}
    </div>`;
}
