/**
 * merge-body — the merge bubble's BODY: a tab strip over its own sub-feed.
 *
 * THE PARITY INVARIANT IS THE POINT. A merge bubble and a subagent bubble share
 * every piece of plumbing beneath them — the same expand → OpenFeed → WatchFeed,
 * the same row store, the same upserts, the same reveal. This module is the ONE
 * legitimate difference: a body renderer that arranges the rows it is handed as
 * tabs instead of as a list. It fetches nothing, opens nothing, and keeps no
 * copy of the rows; everything it draws comes out of `view`.
 *
 * THE STRIPS NEST BY THE ROW'S PARENT. The bubble's own strip is the tab rows
 * with no parent (queue, the prompts, rebasing, tests, committing, updating
 * main); the rebasing tab's panel is a strip of its commits, the tests tab's a
 * strip of its runs, and each run's a strip of its output and the fixing that
 * followed it (tab-panels.ts). Every strip is the one `createTabSet`, so each
 * keeps its own selection and draws in place the same way.
 *
 * NOTHING PARKS. A merge that gives up fails and hands the workspace back, so
 * no tab hosts a composer. A commit's conflict resolution or a fixing attempt
 * whose agent has an ask open says so on its badge, and on every tab it is
 * drawn in (❓, tab-strip.ts); the ask itself is answered where every ask is,
 * never in the bubble.
 */
import { log } from "../../log.js";
import type { Handle } from "../../failure/local.js";
import { drawBreadcrumbTrail, type BubbleBodyRenderer, type SubfeedView } from "../renderers.js";
import { childTabsOf, mergeTabIdsOf } from "./tab-strip.js";
import { createTabSet } from "./tab-set.js";
import { mergeTabPanel } from "./tab-panels.js";
import { replaceTicking, stopTicking } from "../ticking.js";

/** The merge bubble's body renderer. */
export const mergeBubbleBody: BubbleBodyRenderer = (mount, view, rc): Handle => {
  const breadcrumbs = document.createElement("div");
  breadcrumbs.className = "feed-breadcrumbs";
  breadcrumbs.setAttribute("data-breadcrumbs", "");

  const tabs = document.createElement("div");
  tabs.className = "merge-tabs";

  // ROWS THE STRIPS CANNOT PLACE. A merge sub-feed is a feed like any other,
  // and a row it carries that names no tab (or names one this bubble has not
  // drawn) is still work that happened. It is drawn here, beneath the strip,
  // and REPORTED — dropping it would hide real conversation, and pinning it
  // under an arbitrary tab would state a grouping the daemon did not.
  const loose = document.createElement("div");
  loose.className = "merge-loose-rows";

  mount.append(tabs, loose);
  const set = createTabSet(tabs, rc.ctx.ticker, "top", (tab) => mergeTabPanel(tab, view, rc));

  const draw = (): void => {
    drawBreadcrumbTrail(breadcrumbs, view.breadcrumbs(), rc, mount);
    const top = childTabsOf(view.rows(), undefined);
    log.debug("drawing a merge bubble body", {
      operation: "merge.draw-body",
      context: { tabs: top.length },
    });
    set.draw(top);
    drawLooseRows(loose, view);
  };

  draw();
  const unsubscribe = view.onChange(draw);
  return {
    dispose(): void {
      unsubscribe();
      set.dispose();
      for (const part of [breadcrumbs, tabs, loose]) {
        stopTicking(part);
        part.remove();
      }
    },
  };
};

/**
 * The sub-feed's rows that belong to no drawn tab, in feed order: an ordinary
 * row naming no tab row as its container, or a tab row naming a container no
 * tab row of this sub-feed is.
 *
 * They go through `view.drawRow` — the ordinary row path, chrome included — so
 * a row nobody could place still reads exactly as it would anywhere else.
 */
export function drawLooseRows(host: HTMLElement, view: SubfeedView): void {
  const placed = mergeTabIdsOf(view.rows());
  const loose: HTMLElement[] = [];
  for (const row of view.rows()) {
    const parent = row.parent?.row?.value;
    if (row.row.case === "mergeTab" && parent === undefined) continue;
    if (parent !== undefined && placed.has(parent)) continue;
    log.warn("a merge sub-feed row names no drawn tab; drawing it beneath the strip", {
      operation: "merge.unplaced-row",
      context: { row: row.id?.value ?? "unset", parent: parent ?? "none" },
    });
    loose.push(view.drawRow(row));
  }
  // Through the ONE replace helper: a row that stopped being loose (it found
  // its tab, or left the feed) is unsubscribed as it is dropped, and one that
  // is still here is merely moved and keeps its clocks.
  replaceTicking(host, loose);
}
