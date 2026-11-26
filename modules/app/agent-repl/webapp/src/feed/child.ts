/**
 * child — a child workspace's bubble in the feed of the workspace that made
 * it (`frontend.v1.FeedChild`): the door to the child. ONE ROW: the head line
 * (⇄, the child's name in the canonical purple, where it lands, how it
 * stands, its clock), beneath it the child's summary, and a thin border in
 * the child's canonical status color while it is open.
 *
 * THE FOLD IS THE READER'S (webview-local): the bubble is drawn folded,
 * showing the short summary, and a click on its body opens it to the long
 * one. Both summaries ride every push, so a fold needs no round-trip; a push
 * redraws the bubble keeping the reader's fold.
 *
 * A click on the name selects the child (`FeedChildNameSelect`), the switch
 * its sidebar row makes, which reopens a closed child. A removed child's name
 * has no select, and draws plain and takes no click.
 */

import type {
  FeedChild,
  FeedChildHead,
  FeedChildStanding,
  FeedChildVersion,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderMarkdown } from "../markdown.js";
import { guardMalformed } from "../rpc/guard.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { toneClass, wireColor } from "../vocab.js";
import { jumpToWorkspace, type JumpSite } from "./merge/jump.js";
import { liveMergeClock, mergeBadge, settledMergeClock } from "./merge/merge.js";
import { armName } from "./renderers.js";
import type { RowContext } from "./renderers.js";

const PATH = "FeedChild";

/** The bubble's class. */
export const CHILD_CLASS = "feed-child";

/** The class of the bubble's name, drawn in the canonical purple. */
export const CHILD_NAME_CLASS = "feed-child-name";

/** The attribute an OPEN (unfolded) bubble carries. */
export const CHILD_OPEN_ATTR = "data-open";

/** Where a child bubble's jump is recorded and its refusal drawn. */
const CHILD_JUMP: JumpSite = {
  describe: "a child's bubble",
  operation: "feed.child.select",
  refusalClass: "feed-child-refusal",
};

/** The children this page has unfolded, by the child's row id: the reader's fold. */
const unfolded = new Set<string>();

/** Draw a child's bubble, its row id ROWID keying the reader's fold. */
export function drawFeedChild(u: FeedChild, rowId: string, rc: RowContext): HTMLElement {
  const head = requireMessage(u.head, `${PATH}.head`);
  const el = document.createElement("div");
  el.className = CHILD_CLASS;
  el.setAttribute("data-child-row", rowId);
  if (u.border !== undefined) {
    el.classList.add("has-border", toneClass(wireColor(u.border.tone, `${PATH}.border.tone`)));
  }
  el.append(drawFeedChildHead(head, rc));
  if (u.summary !== undefined) {
    const summary = u.summary;
    const short = requireMessage(summary.shortSummary, `${PATH}.summary.short_summary`).text;
    const body = document.createElement("div");
    body.className = "feed-child-summary";
    const paint = (): void => {
      const open = unfolded.has(rowId);
      el.toggleAttribute(CHILD_OPEN_ATTR, open);
      if (open && summary.longSummary !== undefined) {
        body.innerHTML = renderMarkdown(summary.longSummary.markdown);
      } else {
        body.textContent = short;
      }
    };
    paint();
    body.addEventListener("click", () => {
      if (unfolded.has(rowId)) unfolded.delete(rowId);
      else unfolded.add(rowId);
      paint();
    });
    el.append(body);
  }
  return el;
}

/** The head line: ⇄ · name · → target · standing · clock. */
export function drawFeedChildHead(u: FeedChildHead, rc: RowContext): HTMLElement {
  const line = document.createElement("div");
  line.className = "merge-head feed-child-head";
  const glyph = document.createElement("span");
  glyph.className = "merge-glyph";
  glyph.textContent = requireMessage(u.glyph, `${PATH}.head.glyph`).icon;
  line.append(glyph);
  const name = requireMessage(u.name, `${PATH}.head.name`);
  const label = document.createElement("span");
  label.className = CHILD_NAME_CLASS;
  label.textContent = name.text;
  if (name.select !== undefined) {
    const target = requireMessage(name.select.target, `${PATH}.head.name.select.target`);
    label.classList.add("is-door");
    label.addEventListener("click", (event) => {
      event.stopPropagation();
      void guardMalformed(rc.ctx, "feed.child.select", jumpToWorkspace(line, rc.ctx, target, CHILD_JUMP));
    });
  }
  line.append(label);
  if (u.target !== undefined) {
    const target = document.createElement("span");
    target.className = "merge-label feed-child-target";
    target.textContent = `→ ${u.target.text}`;
    line.append(target);
  }
  const standing = drawFeedChildStanding(requireMessage(u.standing, `${PATH}.head.standing`));
  if (standing !== null) line.append(standing);
  const end = document.createElement("span");
  end.className = "merge-head-end";
  const runtime = requireMessage(u.runtime, `${PATH}.head.runtime`);
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.head.runtime.started_at_ms`);
  end.append(
    runtime.endedAtMs === undefined
      ? liveMergeClock(startedMs, rc)
      : settledMergeClock(msOf(runtime.endedAtMs, `${PATH}.head.runtime.ended_at_ms`) - startedMs),
  );
  line.append(end);
  return line;
}

/** The standing's badge: none while open, else its word in its tone. */
export function drawFeedChildStanding(u: FeedChildStanding): HTMLElement | null {
  const standing = requireCase(u.standing, `${PATH}.head.standing.standing`);
  switch (standing.case) {
    case "open":
      return null;
    case "merging": {
      const stage = requireCase(standing.value.stage, `${PATH}.head.standing.merging.stage`);
      return mergeBadge(stage.case === "waiting" ? "waiting" : "merging", "is-live");
    }
    case "merged": {
      const badge = mergeBadge("merged", "is-succeeded");
      if (standing.value.version !== undefined) badge.append(" ", drawFeedChildVersion(standing.value.version));
      return badge;
    }
    case "closed":
      return mergeBadge("closed", "is-abandoned");
    case "removed":
      return mergeBadge("removed", "is-abandoned");
    default:
      return unreachableArm(`${PATH}.head.standing.standing`, armName(standing));
  }
}

/** The version a landing produced, its incremented number drawn apart. */
export function drawFeedChildVersion(u: FeedChildVersion): HTMLElement {
  const el = document.createElement("span");
  el.className = "feed-child-version";
  el.append(requireMessage(u.leading, `${PATH}.version.leading`).text);
  const incremented = document.createElement("span");
  incremented.className = "feed-child-version-incremented";
  incremented.textContent = requireMessage(u.incremented, `${PATH}.version.incremented`).text;
  el.append(incremented);
  if (u.trailing !== undefined) el.append(u.trailing.text);
  return el;
}
