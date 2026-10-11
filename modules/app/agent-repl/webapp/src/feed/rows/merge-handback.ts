/**
 * merge-handback — A DETACHED MERGE REPORTED BACK: the small inline badge the
 * daemon's report of a detached merge's end leaves in the requesting
 * workspace's feed (conversation.v1.PromptOrigin.PROMPT_ORIGIN_MERGE_HANDBACK).
 * The merge itself is read in its bubble, so the badge carries the daemon's
 * one-line label and nothing else.
 */
import { requireMessage } from "../../rpc/strict.js";
import type { FeedMergeHandbackBadge } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawHandbackBadge } from "./handback-badge.js";

/** The class the merge hand-back badge's hooks and its tone know it by. */
export const MERGE_HANDBACK_BADGE_CLASS = "merge-handback-badge";

/** The merge hand-back badge: the daemon's label ("merge of x landed"), verbatim. */
export function drawFeedMergeHandback(badge: FeedMergeHandbackBadge): HTMLElement {
  return drawHandbackBadge(requireMessage(badge.label, "FeedMergeHandbackBadge.label").text, {
    className: MERGE_HANDBACK_BADGE_CLASS,
    operation: "feed.draw-merge-handback",
    describe: "a merge hand-back",
  });
}
