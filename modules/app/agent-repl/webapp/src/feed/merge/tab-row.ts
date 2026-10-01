/**
 * tab-row — a merge tab drawn as a ROW of its own.
 *
 * INSIDE a merge bubble a tab is never a row: the strip consumes every
 * `merge_tab` out of the sub-feed and lays them out as tabs, which is what
 * makes the merge body the one legitimate difference from a subagent bubble.
 *
 * ANYWHERE ELSE the tab still arrived, and a feed that silently swallowed it
 * would hide a phase of a real run from the reader with no evidence left
 * behind. So it draws through EXACTLY the pieces the strip draws with — the
 * same badge (`drawFeedMergeTab`) and the same content (`drawTabBody`) — and
 * gains no second rendering of its own to drift from the strip's.
 *
 * The badge is drawn ACTIVE because a lone tab is the only tab there is; there
 * is nothing to select between.
 */
import { log } from "../../log.js";
import type { FeedMergeTab, FeedRow } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { RowContext, SubfeedView } from "../renderers.js";
import { drawFeedMergeTab, readMergeTab } from "./tab-strip.js";
import { drawSummary, drawTabBody } from "./merge-body.js";

/** One merge tab, as a feed row: its badge, its failure summary, its content. */
export function drawFeedMergeTabRow(
  row: FeedRow,
  u: FeedMergeTab,
  view: SubfeedView,
  rc: RowContext,
): HTMLElement {
  const tab = readMergeTab(row, u);
  log.debug("drawing a merge tab as a row of its own", {
    operation: "feed.merge.tab-row",
    context: { kind: tab.kind, state: tab.state, row: tab.id },
  });

  const el = document.createElement("div");
  el.className = "merge-tab-row";
  el.setAttribute("data-state", tab.state);

  const strip = document.createElement("div");
  strip.className = "merge-strip";
  strip.append(drawFeedMergeTab(tab, { active: true, ticker: rc.ctx.ticker }));

  const summary = document.createElement("div");
  summary.className = "merge-tab-summary";
  drawSummary(summary, tab);

  const panel = document.createElement("div");
  panel.className = "merge-tab-panel";
  drawTabBody(panel, tab, view, rc);

  el.append(strip, summary, panel);
  return el;
}
