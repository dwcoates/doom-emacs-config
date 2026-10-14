/**
 * version — THE AGENT-REPL VERSION A LANDED MERGE PRODUCED, on the merge head.
 *
 * The daemon splits the version around the one number the merge incremented
 * (frontend.v1.FeedMergeVersion), so this draws three runs verbatim and
 * colors only the incremented one green; it never parses or re-derives the
 * version. Concatenated, the runs are exactly the version's tag.
 */
import { requireMessage } from "../../rpc/strict.js";
import type { FeedMergeVersion } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedMerge.head.version";

/** The version: leading text, the incremented number, any trailing text. */
export function drawFeedMergeVersion(version: FeedMergeVersion): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-version";
  el.append(run("merge-version-leading", requireMessage(version.leading, `${PATH}.leading`).text));
  el.append(
    run("merge-version-incremented", requireMessage(version.incremented, `${PATH}.incremented`).text),
  );
  if (version.trailing !== undefined) {
    el.append(run("merge-version-trailing", version.trailing.text));
  }
  return el;
}

/** One verbatim run of the version's text. */
function run(className: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = className;
  el.textContent = text;
  return el;
}
