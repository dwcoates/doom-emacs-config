/**
 * merge-step — the salient line a merge's current step stands on, under the
 * `merging` status.
 *
 * THE ARM IS THE STEP (footer.proto, `FooterStatusActivityMergeStep`), so a
 * line can only stand under the step it narrates, and each arm's words are
 * the daemon's: a prompt's text, a rebase command, a commit subject, a suite's
 * name. This module adds only what footer.proto says each arm is DRAWN as —
 * the suite edge's glyph, the "N files" of a conflict, the "fast-forwarding
 * to" of an update — and never decides anything about the merge.
 *
 * FOOTER TEXT IS FOR A HUMAN USER: every word added here is plain, and no
 * identifier spelling reaches the line.
 *
 * COLOUR ON TYPED DATUMS, as the rest of the activity cell: a count takes the
 * figure colour and a commit the identity colour (`tones.ts`). A suite that
 * passed is drawn in green and one that failed in red, which is how
 * footer.proto draws those two edges.
 */
import type {
  FooterMergeStepConflict,
  FooterMergeStepEnqueued,
  FooterMergeStepFixing,
  FooterMergeStepPrompt,
  FooterMergeStepRebasing,
  FooterMergeStepSuite,
  FooterMergeStepUpdatingMain,
  FooterStatusActivityMergeStep,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { log } from "../log.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { toneClass } from "../vocab.js";
import { textLine } from "./parts.js";
import { activityDatumClass } from "./tones.js";

/** The glyph each suite edge is drawn with. */
export const SUITE_EDGE_GLYPHS = {
  started: "▶",
  passed: "✓",
  failed: "✗",
} as const satisfies Record<string, string>;

/** The merge step's line, by the step it narrates. */
export function drawFooterStatusActivityMergeStep(
  u: FooterStatusActivityMergeStep,
  path: string,
): HTMLElement {
  const step = requireCase(u.step, `${path}.step`);
  log.debug(`drawing the merge step line: ${step.case}`, {
    operation: "footer.strip.merge-step",
    context: { step: step.case },
  });
  const line = drawStep(step, `${path}.step`);
  line.classList.add("footer-activity-merge-step");
  line.setAttribute("data-step", step.case);
  return line;
}

/** One arm's line. */
function drawStep(
  step: FooterStatusActivityMergeStep["step"] & { case: string },
  path: string,
): HTMLElement {
  switch (step.case) {
    case "enqueued":
      return drawMergeStepEnqueued(step.value);
    case "preprocessing":
    case "postprocessing":
      return drawMergeStepPrompt(step.value);
    case "rebasing":
      return drawMergeStepRebasing(step.value, `${path}.rebasing`);
    case "conflictResolution":
      return drawMergeStepConflict(step.value);
    case "testing":
      return drawMergeStepSuite(step.value, `${path}.testing`);
    case "fixing":
      return drawMergeStepFixing(step.value);
    case "committing":
      return textLine("footer-merge-step-committing", step.value.subject);
    case "updatingMain":
      return drawMergeStepUpdatingMain(step.value, `${path}.updating_main`);
    default: {
      const other: { case: string } = step;
      return unreachableArm(path, other.case);
    }
  }
}

/** The merge ahead in the queue and its step: "fix-reconnect: testing". */
export function drawMergeStepEnqueued(u: FooterMergeStepEnqueued): HTMLElement {
  return textLine("footer-merge-step-enqueued", `${u.workspaceName}: ${u.step}`);
}

/** A configured prompt's text, verbatim. */
export function drawMergeStepPrompt(u: FooterMergeStepPrompt): HTMLElement {
  return textLine("footer-merge-step-prompt", u.text);
}

/**
 * The rebase's line for the current commit: the command running, or the
 * failure it hit, verbatim. The arm is stamped as `data-line`.
 */
export function drawMergeStepRebasing(u: FooterMergeStepRebasing, path: string): HTMLElement {
  const line = requireCase(u.line, `${path}.line`);
  switch (line.case) {
    case "running":
    case "failed": {
      const el = textLine("footer-merge-step-rebasing", line.value.text);
      el.setAttribute("data-line", line.case);
      return el;
    }
    default: {
      const other: { case: string } = line;
      return unreachableArm(`${path}.line`, other.case);
    }
  }
}

/** The conflicting commit and how many files: "fix the reconnect loop: 3 files". */
export function drawMergeStepConflict(u: FooterMergeStepConflict): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-merge-step-conflict";
  line.appendChild(document.createTextNode(`${u.commitSubject}: `));
  const count = document.createElement("span");
  count.className = activityDatumClass("count");
  count.setAttribute("data-datum", "count");
  count.textContent = String(u.files);
  line.appendChild(count);
  line.appendChild(document.createTextNode(u.files === 1 ? " file" : " files"));
  return line;
}

/**
 * One suite's edge: "▶ name" as it starts, "✓ name" in green as it passes,
 * "✗ name" in red as it fails. The edge is stamped as `data-edge`.
 */
export function drawMergeStepSuite(u: FooterMergeStepSuite, path: string): HTMLElement {
  const edge = requireCase(u.edge, `${path}.edge`);
  const line = document.createElement("span");
  line.className = "footer-merge-step-suite";
  line.setAttribute("data-edge", edge.case);
  switch (edge.case) {
    case "started":
      line.textContent = `${SUITE_EDGE_GLYPHS.started} ${u.name}`;
      return line;
    case "passed":
      line.classList.add(toneClass("green"));
      line.textContent = `${SUITE_EDGE_GLYPHS.passed} ${u.name}`;
      return line;
    case "failed":
      line.classList.add(toneClass("red"));
      line.textContent = `${SUITE_EDGE_GLYPHS.failed} ${u.name}`;
      return line;
    default: {
      const other: { case: string } = edge;
      return unreachableArm(`${path}.edge`, other.case);
    }
  }
}

/** The suites a fixing attempt is repairing, in the gate's order. */
export function drawMergeStepFixing(u: FooterMergeStepFixing): HTMLElement {
  return textLine("footer-merge-step-fixing", u.suites.join(", "));
}

/** Updating main: "fetching", then "fast-forwarding to 4f2a1c". */
export function drawMergeStepUpdatingMain(u: FooterMergeStepUpdatingMain, path: string): HTMLElement {
  const step = requireCase(u.step, `${path}.step`);
  const line = document.createElement("span");
  line.className = "footer-merge-step-updating-main";
  line.setAttribute("data-update-step", step.case);
  switch (step.case) {
    case "fetching":
      line.textContent = "fetching";
      return line;
    case "fastForwarding": {
      line.appendChild(document.createTextNode("fast-forwarding to "));
      const commit = document.createElement("span");
      commit.className = activityDatumClass("sha");
      commit.setAttribute("data-datum", "sha");
      commit.textContent = step.value.commit;
      line.appendChild(commit);
      return line;
    }
    default: {
      const other: { case: string } = step;
      return unreachableArm(`${path}.step`, other.case);
    }
  }
}
