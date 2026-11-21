/**
 * fix-gate — A DEPLOY MEDIATOR'S FIX AT ITS GATE (owner ruling, 2026-11-19).
 *
 * THE MERGE BUBBLE'S FRAME WITH NO TAB STRIP (`drawMergeFrame`): the head
 * reads as a merge head does (glyph, label, badge, clock), and the body is the
 * tests tab's run output and nothing else, drawn by the tests tab's own drawing
 * (`drawFeedMergeTestsRunOutput`), so the suites, the seat wait and the log
 * link read exactly as a queue merge's do.
 *
 * ONE CARD PER GATE ATTEMPT, drawn in the mediator's own feed: an attempt that
 * failed and sent the fix back stays above the next one's.
 *
 * THE CLOCK IS THE CLIENT'S: a live attempt counts up from `live.started_at_ms`
 * through the shared ticker; a settled one shows the span that ran and stops.
 */
import { requireMessage } from "../../rpc/strict.js";
import type { RowContext } from "../renderers.js";
import { drawFeedMergeTestsRunOutput } from "../merge/tests-tab.js";
import { drawMergeFrame } from "./merge-frame.js";
import type { FeedFixGate, FeedFixGateHead } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedFixGate";

/** The badge words, by the attempt's state. */
export const FIX_GATE_WORDS = {
  live: "testing",
  succeeded: "passed",
  failed: "failed",
} as const;

/** A fix's gate attempt: the merge-styled head over the run's output. */
export function drawFeedFixGate(msg: FeedFixGate, rc: RowContext): HTMLElement {
  const head = requireMessage(msg.head, `${PATH}.head`);
  const card = drawMergeFrame(
    {
      kind: "fix-gate",
      label: requireMessage(head.label, `${PATH}.head.label`).text,
      props: [drawAttempt(head)],
      words: FIX_GATE_WORDS,
    },
    msg.state,
    rc,
    `${PATH}.state`,
  );
  const output = requireMessage(msg.output, `${PATH}.output`);
  const body = document.createElement("div");
  body.className = "merge-run merge-run-output fix-gate-run";
  body.append(...drawFeedMergeTestsRunOutput(output, rc.ctx, `${PATH}.output`));
  card.append(body);
  return card;
}

/** The head's attempt ("attempt 2 of 3"). */
function drawAttempt(head: FeedFixGateHead): HTMLElement {
  const attempt = document.createElement("span");
  attempt.className = "fix-gate-attempt";
  attempt.textContent = attemptText(head.attempt, head.maxAttempts);
  return attempt;
}

/** "attempt 2 of 3". */
export function attemptText(attempt: number, maxAttempts: number): string {
  return `attempt ${attempt} of ${maxAttempts}`;
}
