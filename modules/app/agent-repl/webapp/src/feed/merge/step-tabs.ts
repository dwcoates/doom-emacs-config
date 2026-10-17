/**
 * step-tabs — the merge bubble's rebase-first tabs: what the rebasing tab
 * draws above its commits, what a commit's subtab carries, what the committing
 * and updating-main tabs carry, and the attempt a fixing sub-subtab names.
 *
 * RESOLVED, REPLACED WHOLE PER PUSH. Each of these tabs carries its content in
 * its own row, so a push redraws it from the message and nothing accumulates
 * across pushes. Every word below is the daemon's (a narration line, a commit
 * subject, a short hash) except the few footer.proto and feed.proto say the tab
 * is DRAWN as: "3/7", "attempt 2/3", "fetching", "fast-forwarding to", and the
 * replay header's joining words ("replaying 5 commits of … onto … at …").
 *
 * THE REPLAY HEADER IS DRAWN FROM ITS PARTS: the branch and the target in the
 * merge purple, the tip a link to the target branch's log with the tip
 * selected, and the target a link that opens and switches to the target's
 * workspace when its checkout is one.
 *
 * NO COUNTING (R5 still holds): "3/7" is the daemon's two figures side by
 * side, never a count of the narration lines under it.
 */
import { armButtonRole } from "../../control.js";
import { renderMergeCommitLink } from "../../link.js";
import { log } from "../../log.js";
import { guardMalformed } from "../../rpc/guard.js";
import type { RowContext } from "../renderers.js";
import { openAndJumpToWorkspace, type JumpSite } from "./jump.js";
import { drawMergeCommitText } from "./merge.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedMergeCommitSubject,
  FeedMergeConflictFile,
  FeedMergeFixAttempt,
  FeedMergeRebaseLine,
  FeedMergeRebaseProgress,
  FeedMergeRebaseReplaying,
  FeedMergeTabCommitting,
  FeedMergeTabRebaseCommit,
  FeedMergeTabRebasing,
  FeedMergeTabTestsRunFixing,
  FeedMergeTabUpdatingMain,
  FeedMergeUpdatingMainStep,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";

/**
 * The rebasing tab's own heading, above its commits' strip: its progress, then
 * the rebase-wide narration, oldest first.
 */
export function drawFeedMergeTabRebasing(u: FeedMergeTabRebasing, path: string, rc: RowContext): HTMLElement {
  const progress = requireMessage(u.progress, `${path}.progress`);
  log.debug("drawing a merge rebasing tab", {
    operation: "merge.draw-rebasing",
    context: { replayed: progress.replayed, total: progress.total, lines: u.lines.length },
  });
  const el = document.createElement("div");
  el.className = "merge-rebasing";
  el.append(
    drawFeedMergeRebaseProgress(progress),
    drawFeedMergeRebaseLines(u.lines, `${path}.lines`, (line, at) => drawFeedMergeRebaseLine(line, at, rc)),
  );
  return el;
}

/** Narration lines, oldest first, as one list, each drawn by DRAW. */
export function drawFeedMergeRebaseLines(
  lines: readonly FeedMergeRebaseLine[],
  path: string,
  draw: (line: FeedMergeRebaseLine, path: string) => HTMLElement,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-lines list-rows";
  lines.forEach((line, i) => el.append(draw(line, `${path}[${i}]`)));
  return el;
}

/**
 * A commit subtab's own content: its narration, oldest first. The conflict
 * section beneath it, when the commit has one, is `drawFeedMergeConflictFiles`
 * over the resolution's rows.
 */
export function drawFeedMergeTabRebaseCommit(u: FeedMergeTabRebaseCommit, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-rebase-commit";
  el.append(drawFeedMergeRebaseLines(u.lines, `${path}.lines`, drawFeedMergeRebaseTextLine));
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

/** One line of the rebasing tab's own narration: a sentence, or a replay header. */
export function drawFeedMergeRebaseLine(u: FeedMergeRebaseLine, path: string, rc: RowContext): HTMLElement {
  const line = requireCase(u.line, `${path}.line`);
  switch (line.case) {
    case "text":
      return drawTextLine(line.value);
    case "replaying":
      return drawFeedMergeRebaseReplaying(line.value, `${path}.line.replaying`, rc);
    default: {
      const other: { case: string } = line;
      return unreachableArm(`${path}.line`, other.case);
    }
  }
}

/** One line of a commit's narration, which is only ever a sentence. */
export function drawFeedMergeRebaseTextLine(u: FeedMergeRebaseLine, path: string): HTMLElement {
  const line = requireCase(u.line, `${path}.line`);
  if (line.case !== "text") return unreachableArm(`${path}.line`, line.case);
  return drawTextLine(line.value);
}

/** A daemon-composed narration sentence, verbatim. */
function drawTextLine(text: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-line";
  el.textContent = text;
  return el;
}

/** Where a replay header's target switch is recorded and its refusal drawn. */
const TARGET_JUMP: JumpSite = {
  describe: "a replay header's target",
  operation: "merge.replay-target",
  refusalClass: "merge-replay-target-refusal",
};

/** "replaying 5 commits of ABC/fix onto master at 4f2a1c", from its parts. */
export function drawFeedMergeRebaseReplaying(u: FeedMergeRebaseReplaying, path: string, rc: RowContext): HTMLElement {
  const target = requireMessage(u.target, `${path}.target`);
  const el = document.createElement("div");
  el.className = "merge-line merge-replaying";
  el.setAttribute("data-merge-line", "replaying");
  const branch = document.createElement("span");
  branch.className = "merge-ref merge-replaying-branch";
  branch.textContent = u.branch;
  el.append(
    `replaying ${u.commits} commits of `,
    branch,
    " onto ",
    drawReplayTarget(target.branch, target.workspace, el, rc),
    " at ",
    renderMergeCommitLink(rc.ctx, u.tip, target, u.tipShortSha),
  );
  return el;
}

/**
 * The replay's target branch: a link that opens and switches to the target's
 * workspace when the daemon named one, its name alone otherwise. HOST is the
 * line a refusal is drawn under.
 */
function drawReplayTarget(
  branch: string,
  workspace: WorkspaceRef | undefined,
  host: HTMLElement,
  rc: RowContext,
): HTMLElement {
  if (workspace === undefined) {
    const el = document.createElement("span");
    el.className = "merge-ref merge-replaying-target";
    el.textContent = branch;
    return el;
  }
  const el = document.createElement("a");
  el.className = "merge-ref merge-replaying-target merge-replaying-target-link";
  el.setAttribute("data-merge-target", "");
  el.textContent = branch;
  armButtonRole(el);
  el.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    void guardMalformed(rc.ctx, "feed.merge.replay-target", openAndJumpToWorkspace(host, rc, workspace, TARGET_JUMP));
  });
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
