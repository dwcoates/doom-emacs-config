/**
 * hook-tabs — what a deploy's SCRIPT HOOK tab draws: the script, the tail of
 * its output, how it ended, and where its whole log is kept.
 *
 * RESOLVED, REPLACED WHOLE PER PUSH. The tab carries everything in its own row
 * (frontend.v1.FeedMergeTabHookScript), so a push redraws it from the message
 * and nothing accumulates across pushes. The words "exited", "timed out after"
 * and "log" are the only ones this module adds; the script, its lines and the
 * log's path are the daemon's.
 *
 * A PROMPT HOOK draws nothing of its own: it is an agentic tab whose content is
 * the turn's rows (tab-panels.ts), exactly as any agent stream.
 *
 * THE LOG IS DRAWN AS ITS PATH, NOT A LINK: no agentrepl.v1.OpenInEditorRequest
 * target can open a file in the daemon's state root other than a test log by
 * its token. docs/protobuf-design/deploy-queue.md records the gap.
 */
import { requireCase, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedMergeHookScriptExit,
  FeedMergeTabHookScript,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** A script hook's tab body. */
export function drawFeedMergeTabHookScript(u: FeedMergeTabHookScript, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-hook-script";
  const script = document.createElement("div");
  script.className = "merge-hook-script-path";
  script.textContent = u.path?.text ?? "";
  el.append(script);
  if (u.tail.length > 0) {
    const tail = document.createElement("pre");
    tail.className = "merge-hook-script-tail";
    tail.textContent = u.tail.map((line) => line.text).join("\n");
    el.append(tail);
  }
  if (u.exit !== undefined) el.append(drawFeedMergeHookScriptExit(u.exit, `${path}.exit`));
  if (u.log !== undefined && u.log.path !== "") {
    const logLine = document.createElement("div");
    logLine.className = "merge-hook-script-log";
    logLine.textContent = `log ${u.log.path}`;
    el.append(logLine);
  }
  return el;
}

/** "exited 3", or "timed out after 60s". The arm is stamped as `data-hook-exit`. */
export function drawFeedMergeHookScriptExit(u: FeedMergeHookScriptExit, path: string): HTMLElement {
  const how = requireCase(u.how, `${path}.how`);
  const el = document.createElement("div");
  el.className = "merge-hook-script-exit";
  el.setAttribute("data-hook-exit", how.case);
  switch (how.case) {
    case "exited":
      el.textContent = `exited ${how.value.status}`;
      return el;
    case "timedOut":
      el.textContent = `timed out after ${how.value.timeoutSeconds}s`;
      return el;
    default: {
      const other: { case: string } = how;
      return unreachableArm(`${path}.how`, other.case);
    }
  }
}
