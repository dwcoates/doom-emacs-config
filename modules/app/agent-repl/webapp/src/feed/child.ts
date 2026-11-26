/**
 * child — a child workspace's bubble in the feed of the workspace that made
 * it (`frontend.v1.FeedChild`): the door to the child. A BUBBLE ROW, drawn
 * by the one bubble chrome every bubble wears (bubble.ts): this module draws
 * only its HEAD, and the bubble's SUB-FEED is the child's own root feed,
 * opened on the row's FeedId as a subagent bubble's is.
 *
 * THE HEAD is the merge head's line (⇄, the child's name in the canonical
 * purple, where it lands, how it stands, its clock) over the child's summary:
 * the short one while the bubble is folded, the long one while it is open.
 * Both ride every push, and the stylesheet shows the one the fold calls for
 * (the bubble head's `aria-expanded`), so a fold needs no redraw.
 *
 * THE FRAME'S TONE is the child's canonical status color while it is open,
 * stated on the head (`FRAME_TONE_ATTRIBUTE`) for the chrome to wear.
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
import { wireColor } from "../vocab.js";
import { FRAME_TONE_ATTRIBUTE } from "./bubble-frame.js";
import { jumpToWorkspace, type JumpSite } from "./merge/jump.js";
import { drawBranchTarget, drawMergeGlyphIcon, liveMergeClock, mergeBadge, settledMergeClock } from "./merge/merge.js";
import { armName } from "./renderers.js";
import type { RowContext } from "./renderers.js";

const PATH = "FeedChild";

/** The head's class. */
export const CHILD_CLASS = "feed-child";

/** The class of the head's name, drawn in the canonical purple. */
export const CHILD_NAME_CLASS = "feed-child-name";

/** The class of the summary a folded bubble shows. */
export const CHILD_SHORT_SUMMARY_CLASS = "feed-child-summary-short";

/** The class of the summary an open bubble shows. */
export const CHILD_LONG_SUMMARY_CLASS = "feed-child-summary-long";

/** Where a child bubble's jump is recorded and its refusal drawn. */
const CHILD_JUMP: JumpSite = {
  describe: "a child's bubble",
  operation: "feed.child.select",
  refusalClass: "feed-child-refusal",
};

/** Draw a child bubble's head: its line over its summaries. */
export function drawFeedChild(u: FeedChild, rc: RowContext): HTMLElement {
  const head = requireMessage(u.head, `${PATH}.head`);
  const el = document.createElement("div");
  el.className = CHILD_CLASS;
  el.setAttribute("data-state", requireCase(requireMessage(head.standing, `${PATH}.head.standing`).standing, `${PATH}.head.standing.standing`).case);
  if (u.border !== undefined) {
    el.setAttribute(FRAME_TONE_ATTRIBUTE, wireColor(u.border.tone, `${PATH}.border.tone`));
  }
  el.append(drawFeedChildHead(head, rc));
  if (u.summary !== undefined) {
    const short = document.createElement("div");
    short.className = `feed-child-summary ${CHILD_SHORT_SUMMARY_CLASS}`;
    short.textContent = requireMessage(u.summary.shortSummary, `${PATH}.summary.short_summary`).text;
    el.append(short);
    if (u.summary.longSummary !== undefined) {
      el.setAttribute("data-has-long", "");
      const long = document.createElement("div");
      long.className = `feed-child-summary ${CHILD_LONG_SUMMARY_CLASS}`;
      long.innerHTML = renderMarkdown(u.summary.longSummary.markdown);
      el.append(long);
    }
  }
  return el;
}

/** The head line: ⇄ · name · → target · standing · clock. */
export function drawFeedChildHead(u: FeedChildHead, rc: RowContext): HTMLElement {
  const line = document.createElement("div");
  line.className = "merge-head feed-child-head";
  line.append(drawMergeGlyphIcon(requireMessage(u.glyph, `${PATH}.head.glyph`).icon));
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
    target.append("→ ", drawBranchTarget(u.target.text, u.target.pullRequest?.url, rc));
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
