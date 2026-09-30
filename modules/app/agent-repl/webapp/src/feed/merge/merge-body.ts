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
 * TWO TAB SHAPES, ONE STRIP. RESOLVED tabs (queue, rebasing, tests,
 * committing, updating main) carry their content in the tab row itself,
 * replaced whole per push. AGENTIC tabs (pre-prompt, conflicts, fixes,
 * post-prompt) are CONTAINERS: their content is the sub-feed rows parented to
 * the tab row, drawn through `view.drawRow` — the ordinary row path, chrome
 * included — which is what keeps a merge agent's conversation identical to
 * any other agent's. A fixes tab also names its attempt above its rows.
 *
 * SELECTION IS LOCAL AND THE READER'S. The auto rule (the last live tab, else
 * the last tab) decides until the reader clicks, and their choice
 * then stands — a re-push must never yank the tab out from under someone
 * reading it. It is released only when a NEW tab appears, because a new tab is
 * the run moving on and the reader asked to follow the run, not to be pinned to
 * a phase that is over.
 *
 * NOTHING PARKS. A merge that gives up fails and hands the workspace back, so
 * no tab waits on the user and no tab hosts a composer.
 */
import { log } from "../../log.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { Handle } from "../../failure/local.js";
import type { FeedRow } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  NEST_ATTRIBUTE,
  drawBreadcrumbTrail,
  type BubbleBodyRenderer,
  type RowContext,
  type SubfeedView,
} from "../renderers.js";
import {
  AGENTIC_KINDS,
  autoSelectedTab,
  drawFeedMergeTab,
  mergeTabsOf,
  type MergeTab,
} from "./tab-strip.js";
import { replaceTicking, stopTicking } from "../ticking.js";
import { drawFeedMergeQueue } from "./queue.js";
import { drawFeedMergeTabTests } from "./tests-tab.js";
import {
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixesAttempt,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
} from "./step-tabs.js";

/** The merge bubble's body renderer. */
export const mergeBubbleBody: BubbleBodyRenderer = (mount, view, rc): Handle => {
  /** The tab the reader picked, by row id; null while the auto rule decides. */
  let picked: string | null = null;
  /** Every tab id drawn so far, so a NEW tab can release the reader's pick. */
  let known = new Set<string>();

  const breadcrumbs = document.createElement("div");
  breadcrumbs.className = "feed-breadcrumbs";
  breadcrumbs.setAttribute("data-breadcrumbs", "");

  const strip = document.createElement("div");
  strip.className = "merge-strip";

  const summary = document.createElement("div");
  summary.className = "merge-tab-summary";

  const panel = document.createElement("div");
  panel.className = "merge-tab-panel";

  // ROWS THE STRIP CANNOT PLACE. A merge sub-feed is a feed like any other, and
  // a row it carries that names no tab (or names one this bubble has not drawn)
  // is still work that happened. It is drawn here, beneath the strip, and
  // REPORTED — dropping it would hide real conversation, and pinning it under
  // an arbitrary tab would state a grouping the daemon did not.
  const loose = document.createElement("div");
  loose.className = "merge-loose-rows";

  mount.append(strip, summary, panel, loose);

  const draw = (): void => {
    drawBreadcrumbTrail(breadcrumbs, view.breadcrumbs(), rc, mount);
    const tabs = mergeTabsOf(view.rows());
    releasePickOnNewTab(tabs);
    const active = tabs.find((t) => t.id === picked) ?? autoSelectedTab(tabs);
    log.debug("drawing a merge bubble body", {
      operation: "merge.draw-body",
      context: { tabs: tabs.length, active: active?.kind ?? "none", picked: picked ?? "auto" },
    });
    replaceTicking(
      strip,
      tabs.map((tab) => {
        const el = drawFeedMergeTab(tab, { active: tab.id === active?.id });
        el.addEventListener("click", () => {
          picked = tab.id;
          draw();
        });
        return el;
      }),
    );
    drawSummary(summary, active);
    drawTabBody(panel, active, view, rc);
    drawLooseRows(loose, tabs, view);
  };

  /**
   * Let go of the reader's pick when a tab they have never seen arrives.
   *
   * Tabs are APPEND-ONLY, so a tab id the strip has not drawn before means the
   * run moved into a new phase — the one event that should move the view.
   */
  function releasePickOnNewTab(tabs: readonly MergeTab[]): void {
    const ids = new Set(tabs.map((t) => t.id));
    for (const id of ids) {
      if (known.has(id)) continue;
      if (known.size > 0) picked = null;
      break;
    }
    known = ids;
  }

  draw();
  const unsubscribe = view.onChange(draw);
  return {
    dispose(): void {
      unsubscribe();
      for (const part of [breadcrumbs, strip, summary, panel, loose]) {
        stopTicking(part);
        part.remove();
      }
    },
  };
};

/**
 * The sub-feed's rows that belong to no drawn tab, in feed order.
 *
 * They go through `view.drawRow` — the ordinary row path, chrome included — so
 * a row nobody could place still reads exactly as it would anywhere else.
 */
export function drawLooseRows(
  host: HTMLElement,
  tabs: readonly MergeTab[],
  view: SubfeedView,
): void {
  const placed = new Set(tabs.map((tab) => tab.id));
  const loose: HTMLElement[] = [];
  for (const row of view.rows()) {
    if (row.row.case === "mergeTab") continue;
    const parent = row.parent?.row?.value;
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

/**
 * The line under the strip: a settled-failed tab's own account.
 *
 * The daemon composed it; the tab's content carries the detail, so this states
 * the outcome once and does not restate what is already below it.
 */
export function drawSummary(host: HTMLElement, tab: MergeTab | undefined): void {
  replaceTicking(host);
  host.hidden = true;
  if (tab === undefined || tab.state !== "settled" || tab.outcome !== "failed") return;
  const settled = tab.value["state"] as { value: { outcome: { value: { summary: string } } } };
  const el = document.createElement("div");
  el.className = "merge-tab-failed";
  el.textContent = settled.value.outcome.value.summary;
  host.append(el);
  host.hidden = false;
}

/** The selected tab's content: its own payload, or the rows parented to it. */
export function drawTabBody(
  host: HTMLElement,
  tab: MergeTab | undefined,
  view: SubfeedView,
  rc: RowContext,
): void {
  // The tab body is redrawn whole on every change; whatever the previous tab
  // drew is DISCARDED here, so it is unsubscribed here too.
  replaceTicking(host);
  if (tab === undefined) return;
  host.setAttribute("data-merge-tab-body", tab.kind);

  const path = "FeedMergeTab.kind";
  const kind = requireCase(tab.tab.kind, path);
  switch (kind.case) {
    case "queue":
      host.append(drawFeedMergeQueue(requireMessage(kind.value.queue, "FeedMergeTabQueue.queue"), rc));
      return;
    case "rebasing":
      host.append(drawFeedMergeTabRebasing(kind.value, `${path}.rebasing`));
      return;
    case "tests":
      host.append(...drawFeedMergeTabTests(kind.value, rc.ctx, `${path}.tests`));
      return;
    case "committing":
      host.append(drawFeedMergeTabCommitting(kind.value, `${path}.committing`));
      return;
    case "updatingMain":
      host.append(drawFeedMergeTabUpdatingMain(kind.value, `${path}.updating_main`));
      return;
    case "fixes":
      // AGENTIC, and it names which attempt it is above the rows it holds.
      host.append(drawFeedMergeTabFixesAttempt(kind.value, `${path}.fixes`), drawAgenticRows(tab, view));
      return;
    case "prePrompt":
    case "conflicts":
    case "postPrompt":
      host.append(drawAgenticRows(tab, view));
      return;
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * The rows parented to an agentic tab, in feed order, through the ordinary path.
 *
 * ONLY the rows naming THIS tab as their container: a merge bubble's sub-feed
 * carries every tab's conversation at once, and the tab is the routing address
 * the daemon stamped on each row.
 */
export function drawAgenticRows(tab: MergeTab, view: SubfeedView): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-tab-rows";
  el.setAttribute(NEST_ATTRIBUTE, "");
  for (const row of view.rows()) {
    if (row.row.case === "mergeTab") continue;
    if (parentOf(row) !== tab.id) continue;
    el.append(view.drawRow(row));
  }
  return el;
}

/** The container a row names, or undefined for a top-level row. */
function parentOf(row: FeedRow): string | undefined {
  return row.parent?.row?.value;
}

/** The agentic kinds, re-exported so a suite can hold this file to the schema. */
export { AGENTIC_KINDS };
