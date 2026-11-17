/**
 * merge-queue-refusal — the words for UpdateMergeQueue's own refusals, shared
 * by every control that acts on a merge queue: the feed's queue-tab moves and
 * the sidebar's resume and drag. One table, so no two controls word one
 * refusal two ways.
 */
import { refusalOf, type Cause, type SentenceTable } from "./refuse.js";

/** UpdateMergeQueue's own arms, in plain words. */
export const UPDATE_MERGE_QUEUE_SENTENCES = {
  noSuchQueuedMerge: () => "this merge is no longer waiting on the queue",
  notPaused: () => "the queue is no longer paused",
  unknownRepository: () => "the daemon no longer has that repository",
  noEscalationHeld: () => "no escalation holds this queue now",
} satisfies SentenceTable;

/** UpdateMergeQueue's refusal, worded from UPDATE_MERGE_QUEUE_SENTENCES. */
export function updateMergeQueueRefusal(cause: Cause): string {
  return refusalOf(cause, UPDATE_MERGE_QUEUE_SENTENCES, "UpdateMergeQueueError.cause").text;
}

/** RequeueMerge's own arms, in plain words. */
export const REQUEUE_MERGE_SENTENCES = {
  unknownMerge: () => "this merge no longer holds a request",
  stillQueued: () => "this merge is back in line already",
  branchGone: (v: { branch: string }) => `its branch ${v.branch} is gone; discard it`,
  alreadyLanded: () => "this merge has landed",
  versionIncrementRequired: () => "this merge needs a version increment; requeue it from the command line with -version-part",
  versionIncrementUnexpected: () => "this merge carries a version increment it does not need",
  versionAlreadyRecorded: () => "this merge keeps the version increment it was asked for",
  placementBehindNotInLine: () => "the merge it was placed behind is not in line",
  workspaceClosed: () => "this merge's workspace is closed",
  notInCatalog: () => "that model is not in this login's catalog",
  coldGated: () => "this merge's workspace is parked at its cold gate; answer the gate first",
} satisfies SentenceTable;

/** RequeueMerge's refusal, worded from REQUEUE_MERGE_SENTENCES. */
export function requeueMergeRefusal(cause: Cause): string {
  return refusalOf(cause, REQUEUE_MERGE_SENTENCES, "RequeueMergeError.cause").text;
}

/** DiscardMerge's own arms, in plain words. */
export const DISCARD_MERGE_SENTENCES = {
  unknownMerge: () => "this merge no longer holds a request",
  stillQueued: () => "this merge is back in line; take it off the queue first",
} satisfies SentenceTable;

/** DiscardMerge's refusal, worded from DISCARD_MERGE_SENTENCES. */
export function discardMergeRefusal(cause: Cause): string {
  return refusalOf(cause, DISCARD_MERGE_SENTENCES, "DiscardMergeError.cause").text;
}
