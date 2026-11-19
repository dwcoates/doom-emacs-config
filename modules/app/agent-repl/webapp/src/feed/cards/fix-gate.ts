/**
 * fix-gate — A DEPLOY MEDIATOR'S FIX AT ITS GATE (owner ruling, 2026-11-19).
 *
 * THE MERGE BUBBLE'S FRAME WITH NO TAB STRIP: the head reads as a merge head
 * does (glyph, label, badge, clock), and the body is the tests tab's run output
 * and nothing else, drawn by the tests tab's own drawing
 * (`drawFeedMergeTestsRunOutput`), so the suites, the seat wait and the log
 * link read exactly as a queue merge's do.
 *
 * ONE CARD PER GATE ATTEMPT, drawn in the mediator's own feed: an attempt that
 * failed and sent the fix back stays above the next one's.
 *
 * THE CLOCK IS THE CLIENT'S: a live attempt counts up from `live.started_at_ms`
 * through the shared ticker; a settled one shows the span that ran and stops.
 */
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { MERGE_GLYPH } from "../../merge-glyph.js";
import { liveMergeClock, mergeBadge, settledMergeClock } from "../merge/merge.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import { drawFeedMergeTestsRunOutput } from "../merge/tests-tab.js";
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
  const state = requireCase(msg.state, `${PATH}.state`);
  const output = requireMessage(msg.output, `${PATH}.output`);

  const card = document.createElement("div");
  card.className = "tool-card fix-gate";
  const line = document.createElement("div");
  line.className = "merge-head fix-gate-head";
  line.append(drawHeadProps(head));
  const end = document.createElement("span");
  end.className = "merge-head-end";

  switch (state.case) {
    case "live": {
      card.setAttribute("data-state", "live");
      line.append(mergeBadge(FIX_GATE_WORDS.live, "is-live"));
      end.append(liveMergeClock(msOf(state.value.startedAtMs, `${PATH}.live.started_at_ms`), rc));
      break;
    }
    case "settled": {
      const outcome = requireCase(state.value.outcome, `${PATH}.settled.outcome`);
      const startedMs = msOf(state.value.startedAtMs, `${PATH}.settled.started_at_ms`);
      const endedMs = msOf(state.value.endedAtMs, `${PATH}.settled.ended_at_ms`);
      switch (outcome.case) {
        case "succeeded":
          card.setAttribute("data-state", "succeeded");
          line.append(mergeBadge(FIX_GATE_WORDS.succeeded, "is-succeeded"));
          break;
        case "failed":
          card.setAttribute("data-state", "failed");
          line.append(mergeBadge(FIX_GATE_WORDS.failed, "is-failed"));
          if (outcome.value.summary !== "") line.append(failureSummary(outcome.value.summary));
          break;
        default:
          return unreachableArm(`${PATH}.settled.outcome`, armName(outcome));
      }
      end.append(settledMergeClock(endedMs - startedMs));
      stopTicking(card);
      break;
    }
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
  line.append(end);

  const body = document.createElement("div");
  body.className = "merge-run merge-run-output fix-gate-run";
  body.append(...drawFeedMergeTestsRunOutput(output, rc.ctx, `${PATH}.output`));
  card.append(line, body);
  return card;
}

/** The head's glyph, label and attempt ("attempt 2 of 3"). */
function drawHeadProps(head: FeedFixGateHead): HTMLElement {
  const props = document.createElement("span");
  props.className = "merge-head-props";
  const glyph = document.createElement("span");
  glyph.className = "merge-glyph";
  glyph.textContent = MERGE_GLYPH;
  const label = document.createElement("span");
  label.className = "merge-label";
  label.textContent = requireMessage(head.label, `${PATH}.head.label`).text;
  const attempt = document.createElement("span");
  attempt.className = "fix-gate-attempt";
  attempt.textContent = attemptText(head.attempt, head.maxAttempts);
  props.append(glyph, label, attempt);
  return props;
}

/** "attempt 2 of 3". */
export function attemptText(attempt: number, maxAttempts: number): string {
  return `attempt ${attempt} of ${maxAttempts}`;
}

/** A failed attempt's one-line account, after its badge. */
function failureSummary(text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "fix-gate-summary";
  el.textContent = text;
  return el;
}
