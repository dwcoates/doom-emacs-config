/**
 * merge-gate — the composer's gate while a MERGE owns this workspace's session.
 *
 * WHY A CLIENT-SIDE GATE EXISTS HERE AT ALL (contrast `ungated.ts`, which
 * pointedly refuses to be one): this gate is not a security boundary and does
 * not pretend to be. The daemon is the authority — while
 * `WorkspaceState.merge_lease_held` is true it REFUSES `SubmitPromptCmd` with
 * an explanatory error, and it would refuse one sent from here too. What the
 * gate buys is that the user learns WHY before spending a prompt on it: without
 * it the composer looks live, the draft disappears, and the explanation arrives
 * as a failure card some seconds later.
 *
 * THE LEASE IS THE FACT, NOT THE PHASE. `merging` and `merge_queued` are what
 * the merge is DOING; the lease is whether it currently owns the shim. They
 * move together most of the time and apart at the edges (a queued merge holds
 * no lease; a conflict resolution holds one while the phase reads
 * `merge_conflict`), so gating on the phase would both block prompts that are
 * fine and admit prompts that are not. The daemon resolves the lease; this
 * reads it.
 *
 * A blocked attempt is never a silent no-op. `submitBlocked` is the one place
 * that decides, and every caller that has a way to attempt a submit routes
 * through it, so a blocked Enter and a blocked click produce the same
 * explanation and the same logged record.
 */

import { escapeHtml } from "./highlight.js";
import { mergeFacts } from "./merge-status.js";
import type { MergeStatus } from "./state-adapter.js";

/**
 * The explanation shown in the composer and surfaced on a blocked attempt.
 *
 * ONE string, exported, because the notice and the attempt must not drift into
 * two different accounts of the same block.
 */
export const MERGE_GATE_NOTICE =
  "this workspace is being merged — the merge is driving the session, " +
  "so prompting is blocked until it finishes";

/**
 * What the merge is doing right now, appended to the notice when the daemon
 * has stamped a structured status.
 *
 * The block's WORST property was never its length: it was that the user had no
 * idea how far along the thing holding their composer was, so a merge waiting
 * behind two other workspaces and a merge on its last commit read identically.
 * The status carries that, so the notice says it.
 *
 * "" when nothing is known, which leaves the fixed notice exactly as it was.
 */
export function mergeGateProgress(status: MergeStatus | null): string {
  const facts = mergeFacts(status);
  if (facts === null) return "";
  return [facts.word, facts.count, facts.activity].filter((part) => part !== "").join(" · ");
}

/** Whether a prompt may be submitted right now. */
export function submitBlocked(mergeLeaseHeld: boolean): boolean {
  return mergeLeaseHeld;
}

/**
 * The notice element's inner HTML: the explanation while the lease is held, ""
 * otherwise (which collapses the slot via `:empty`).
 *
 * There is no dismiss control, for the same reason the ungated banner has none:
 * the notice is a function of live state, re-rendered from it on every chrome
 * frame, so it cannot be closed while the block is in force. The text is a
 * fixed constant, so nothing caller-controlled is interpolated.
 */
export function mergeGateNoticeHtml(
  mergeLeaseHeld: boolean,
  mergeStatus: MergeStatus | null = null,
): string {
  if (!submitBlocked(mergeLeaseHeld)) return "";
  // No glyph: the notice explains a deliberate, temporary, benign block, and
  // the alarm marks in this stylesheet are spent on things that are wrong.
  //
  // The progress half is the DAEMON's text (a commit subject, a prompt), so it
  // is escaped; the notice itself is a fixed constant with nothing to escape.
  const progress = mergeGateProgress(mergeStatus);
  const detail =
    progress === ""
      ? ""
      : `<span class="merge-gate-progress">${escapeHtml(progress)}</span>`;
  return `<span class="merge-gate-text">${MERGE_GATE_NOTICE}</span>${detail}`;
}

/** The disabled send button's tooltip, or "" when the composer is live. */
export function mergeGateSendTitle(
  mergeLeaseHeld: boolean,
  mergeStatus: MergeStatus | null = null,
): string {
  if (!submitBlocked(mergeLeaseHeld)) return "";
  const progress = mergeGateProgress(mergeStatus);
  return progress === "" ? MERGE_GATE_NOTICE : `${MERGE_GATE_NOTICE} (${progress})`;
}

/**
 * The record written when a user tries to submit anyway.
 *
 * Returned rather than logged here so the caller writes it through the webapp's
 * one canonical logging API with its own operation and bound context, and so
 * the wording is assertable without a logger double.
 */
export function mergeGateBlockedLog(
  promptLength: number,
  mergeStatus: MergeStatus | null = null,
): string {
  const progress = mergeGateProgress(mergeStatus);
  return (
    `prompt submission blocked: the merge coordinator holds this workspace's ` +
    `session lease — ${MERGE_GATE_NOTICE} (prompt_length=${String(promptLength)}, ` +
    `merge=${progress === "" ? "unreported" : progress}, draft retained)`
  );
}
