/**
 * response-usage-stamp — the figures an assistant bubble's corner renders,
 * drawn from the RESOLVED `AgentResponse.usage_stamp`.
 *
 * EVERY FIGURE IS THE DAEMON'S. `expensiveInputTokens` is the canonical
 * `TokenUsage.input_misses` total — both misses together, because both missed
 * the cache — resolved by `internal/tokenusage`, which is the one owner of that
 * judgment. This end adds nothing to it: no sum of the breakdown rows, no
 * "total tokens" figure derived from the three, no cache-hit rate.
 *
 * ABSENT STAMP RENDERS NO FIGURES. A response that carried no usage record has
 * no stamp, and the corner is then empty. It is never drawn as zeros: a bubble
 * reading "0 in / 0 out" is a claim that the response was free, which is a
 * different and false statement from "we were not told what it cost".
 *
 * AN EMPTY MODEL IS THE SAME KIND OF ABSENCE — synthetic records carry none,
 * and the corner omits the model rather than printing a guess.
 */

import { escapeHtml } from "./highlight.js";
import type { ResponseUsageStamp } from "./frontend-proto.js";

/** The corner's collapsed line: the headline figure and nothing else. */
export function usageStampHeadline(stamp: ResponseUsageStamp): string {
  return `${stamp.expensiveInputTokens} in`;
}

/**
 * The corner, or "" when the response carried no stamp.
 *
 * The expanded breakdown row carries the cache-served input and the output as
 * billed, both verbatim. `undefined` and `null` are both the absence; the
 * parameter accepts them so a call site never has to normalize one into the
 * other and accidentally normalize it into a zero instead.
 */
export function responseUsageStampHtml(stamp: ResponseUsageStamp | null | undefined): string {
  if (stamp === undefined || stamp === null) return "";
  const model =
    stamp.model === ""
      ? ""
      : `<span class="usage-model">${escapeHtml(stamp.model)}</span>`;
  return (
    `<div class="usage-stamp">` +
    `<span class="usage-expensive">${escapeHtml(usageStampHeadline(stamp))}</span>` +
    `<span class="usage-cache-read">${escapeHtml(String(stamp.cacheReadTokens))} cached</span>` +
    `<span class="usage-output">${escapeHtml(String(stamp.outputTokens))} out</span>` +
    model +
    `</div>`
  );
}
