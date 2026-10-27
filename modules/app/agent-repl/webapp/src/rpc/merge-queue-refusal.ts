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
} satisfies SentenceTable;

/** UpdateMergeQueue's refusal, worded from UPDATE_MERGE_QUEUE_SENTENCES. */
export function updateMergeQueueRefusal(cause: Cause): string {
  return refusalOf(cause, UPDATE_MERGE_QUEUE_SENTENCES, "UpdateMergeQueueError.cause").text;
}
