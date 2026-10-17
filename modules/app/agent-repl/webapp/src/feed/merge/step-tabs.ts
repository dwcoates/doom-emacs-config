/**
 * step-tabs — the merge bubble's rebase-first tabs: what the rebasing tab
 * draws above its commits, what a commit's subtab carries, what the committing
 * and updating-main tabs carry, and the attempt a fixing sub-subtab names.
 *
 * RESOLVED, REPLACED WHOLE PER PUSH. Each of these tabs carries its content in
 * its own row, so a push redraws it from the message and nothing accumulates
 * across pushes. Every word below is the daemon's (a narration line, a commit
 * subject, a short hash) except the few footer.proto and feed.proto say the tab
 * is DRAWN as: "3/7", "attempt 2/3", "fetching", "fast-forwarding to".
 *
 * NO COUNTING (R5 still holds): "3/7" is the daemon's two figures side by
 * side, never a count of the narration lines under it.
 */
import { log } from "../../log.js";
import { drawMergeCommitText } from "./merge.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedMergeCommitSubject,
  FeedMergeConflictFile,
  FeedMergeFixAttempt,
  FeedMergeRebaseLine,
  FeedMergeRebaseProgress,
  FeedMergeTabCommitting,
  FeedMergeTabRebaseCommit,
  FeedMergeTabRebasing,
  FeedMergeTabTestsRunFixing,
  FeedMergeTabUpdatingMain,
  FeedMergeUpdatingMainStep,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/**
 * The rebasing tab's own heading, above its commits' strip: its progress, then
 * the rebase-wide narration, oldest first.
 */
export function drawFeedMergeTabRebasing(u: FeedMergeTabRebasing, path: string): HTMLElement {
  const progress = requireMessage(u.progress, `${path}.progress`);
  log.debug("drawing a merge rebasing tab", {
    operation: "merge.draw-rebasing",
    context: { replayed: progress.replayed, total: progress.total, lines: u.lines.length },
  });
  const el = document.createElement("div");
  el.className = "merge-rebasing";
  el.append(drawFeedMergeRebaseProgress(progress), drawFeedMergeRebaseLines(u.lines));
  return el;
}

/** Narration lines, oldest first, as one list. */
export function drawFeedMergeRebaseLines(lines: readonly FeedMergeRebaseLine[]): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-lines list-rows";
  for (const line of lines) el.append(drawFeedMergeRebaseLine(line));
  return el;
}

/**
 * A commit subtab's own content: its narration, oldest first. The conflict
 * section beneath it, when the commit has one, is `drawFeedMergeConflictFiles`
 * over the resolution's rows.
 */
export function drawFeedMergeTabRebaseCommit(u: FeedMergeTabRebaseCommit): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-rebase-commit";
  el.append(drawFeedMergeRebaseLines(u.lines));
  return el;
}

/** The files a commit's replay conflicted in, as its conflict section's heading. */
export function drawFeedMergeConflictFiles(files: readonly FeedMergeConflictFile[]): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-conflict-files list-rows";
  for (const file of files) {
    const line = document.createElement("div");
    line.className = "merge-conflict-file";
    line.textContent = file.path;
    el.append(line);
  }
  return el;
}

/** The replay's progress, drawn "3/7". */
export function drawFeedMergeRebaseProgress(u: FeedMergeRebaseProgress): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-rebase-progress";
  el.setAttribute("data-merge-progress", "");
  el.textContent = `${u.replayed}/${u.total}`;
  return el;
}

/** One daemon-composed narration line, verbatim. */
export function drawFeedMergeRebaseLine(u: FeedMergeRebaseLine): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-line";
  el.textContent = u.text;
  return el;
}

/** The committing tab: the merge commit's subject line. */
export function drawFeedMergeTabCommitting(u: FeedMergeTabCommitting, path: string): HTMLElement {
  return drawFeedMergeCommitSubject(requireMessage(u.subject, `${path}.subject`));
}

/** The merge commit's first line, verbatim. */
export function drawFeedMergeCommitSubject(u: FeedMergeCommitSubject): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-commit-subject";
  el.textContent = u.text;
  return el;
}

/** The updating-main tab: the step it is on. */
export function drawFeedMergeTabUpdatingMain(u: FeedMergeTabUpdatingMain, path: string): HTMLElement {
  return drawFeedMergeUpdatingMainStep(requireMessage(u.step, `${path}.step`), `${path}.step`);
}

/** "fetching", then "fast-forwarding to 4f2a1c". The arm is stamped as `data-update-step`. */
export function drawFeedMergeUpdatingMainStep(u: FeedMergeUpdatingMainStep, path: string): HTMLElement {
  const step = requireCase(u.step, `${path}.step`);
  const el = document.createElement("div");
  el.className = "merge-updating-main";
  el.setAttribute("data-update-step", step.case);
  switch (step.case) {
    case "fetching":
      el.textContent = "fetching";
      return el;
    case "fastForwarding": {
      el.append(document.createTextNode("fast-forwarding to "), drawMergeCommitText(step.value.commit));
      return el;
    }
    default: {
      const other: { case: string } = step;
      return unreachableArm(`${path}.step`, other.case);
    }
  }
}

/** The attempt a fixing sub-subtab is, read off its tab message. */
export function drawFeedMergeTabFixingAttempt(u: FeedMergeTabTestsRunFixing, path: string): HTMLElement {
  return drawFeedMergeFixAttempt(requireMessage(u.attempt, `${path}.attempt`));
}

/** A fixing attempt's place among the attempts allowed: "attempt 2/3". */
export function drawFeedMergeFixAttempt(u: FeedMergeFixAttempt): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-fix-attempt";
  el.setAttribute("data-merge-attempt", "");
  el.textContent = `attempt ${u.attempt}/${u.maxAttempts}`;
  return el;
}
