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
 * replaced whole per push. AGENTIC tabs (pick, pre-prompt, conflicts, fixes,
 * post-prompt) are CONTAINERS: their content is the sub-feed rows parented to
 * the tab row, drawn through `view.drawRow` — the ordinary row path, chrome
 * included — which is what keeps a merge agent's conversation identical to
 * any other agent's, whether it runs in this workspace (a sync merge) or in a
 * hidden merge workspace whose turns the daemon mirrors here (an async merge).
 * A fixes tab also names its attempt above its rows, and a LIVE agentic tab
 * offers "stop" above them (stop-control.ts), aimed at the tab itself.
 *
 * SELECTION IS LOCAL AND THE READER'S. The auto rule (the last live tab, else
 * the last tab) decides until the reader clicks, and their choice
 * then stands — a re-push must never yank the tab out from under someone
 * reading it. It is released only when a NEW tab appears, because a new tab is
 * the run moving on and the reader asked to follow the run, not to be pinned to
 * a phase that is over.
 *
 * NOTHING PARKS. A merge that gives up fails and hands the workspace back, so
 * no tab hosts a composer. A conflict-resolution or fixing tab whose agent has
 * an ask open says so on its badge (❓, tab-strip.ts); the ask itself is
 * answered where every ask is, never in the bubble.
 */
import { equals } from "@bufbuild/protobuf";
import { log } from "../../log.js";
import { placeChildren } from "../../dom.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { Handle } from "../../failure/local.js";
import { FeedRowSchema, type FeedRow } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
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
  updateFeedMergeTab,
  mergeTabsOf,
  type MergeTab,
} from "./tab-strip.js";
import { replaceTicking, stopTicking } from "../ticking.js";
import { drawFeedMergeQueue } from "./queue.js";
import { drawFeedMergeTabTests, updateFeedMergeTabTests } from "./tests-tab.js";
import {
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixesAttempt,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
} from "./step-tabs.js";
import { buildInterruptMergeTabRequest } from "../requests.js";
import { drawStopControl, type StopSite } from "../stop-control.js";

/** The stop a live agentic tab offers: the tab's own row, stopped as a merge tab. */
const MERGE_TAB_STOP: StopSite = {
  prefix: "merge-tab-stop",
  operation: "feed.merge-tab-stop",
  refusedOperation: "feed.merge-tab-stop-refused",
  describe: "stopping a merge tab's agent",
  request: buildInterruptMergeTabRequest,
};

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

  /** Each drawn tab's element, and the tab and activeness it was drawn from, by tab id. */
  const drawnTabs = new Map<string, { el: HTMLElement; tab: MergeTab; active: boolean }>();
  /** The tab the panel last drew, kept while that same tab stays the one shown. */
  let shown: MergeTab | null = null;

  const draw = (): void => {
    drawBreadcrumbTrail(breadcrumbs, view.breadcrumbs(), rc, mount);
    const tabs = mergeTabsOf(view.rows());
    releasePickOnNewTab(tabs);
    const active = tabs.find((t) => t.id === picked) ?? autoSelectedTab(tabs);
    log.debug("drawing a merge bubble body", {
      operation: "merge.draw-body",
      context: { tabs: tabs.length, active: active?.kind ?? "none", picked: picked ?? "auto" },
    });
    drawStrip(tabs, active);
    drawSummary(summary, active);
    drawPanel(active);
    drawLooseRows(loose, tabs, view);
  };

  /**
   * THE STRIP IS DRAWN IN PLACE (owner request, 2026-10-10). The daemon
   * re-pushes a tab row on every beat of its work — a tests tab on every unit
   * verdict, many times a second — and a strip rebuilt on each push restarted
   * every live tab's breathing dot, which read as the dots blinking. Each tab
   * keeps its element for its whole life, repainted only where its tab or its
   * activeness changed (`updateFeedMergeTab`).
   */
  function drawStrip(tabs: readonly MergeTab[], active: MergeTab | undefined): void {
    const els = tabs.map((tab) => {
      const isActive = tab.id === active?.id;
      const drawn = drawnTabs.get(tab.id);
      if (drawn === undefined) {
        const el = drawFeedMergeTab(tab, { active: isActive, ticker: rc.ctx.ticker });
        el.addEventListener("click", () => {
          picked = tab.id;
          draw();
        });
        drawnTabs.set(tab.id, { el, tab, active: isActive });
        return el;
      }
      if (drawn.active !== isActive || !equals(FeedRowSchema, drawn.tab.row, tab.row)) {
        updateFeedMergeTab(drawn.el, tab, drawn.tab, { active: isActive, ticker: rc.ctx.ticker });
        drawn.tab = tab;
        drawn.active = isActive;
      }
      return drawn.el;
    });
    const served = new Set(tabs.map((tab) => tab.id));
    for (const [id, drawn] of drawnTabs) {
      if (served.has(id)) continue;
      stopTicking(drawn.el);
      drawnTabs.delete(id);
    }
    placeChildren(strip, els);
  }

  /**
   * THE PANEL IS DRAWN IN PLACE while the same tab stays the one shown
   * (`updateTabBody`), for the reason the strip is; a different tab is drawn
   * afresh (`drawTabBody`).
   */
  function drawPanel(active: MergeTab | undefined): void {
    if (active !== undefined && shown !== null && shown.id === active.id && shown.kind === active.kind) {
      if (updateTabBody(panel, active, shown, view, rc)) {
        shown = active;
        return;
      }
    }
    drawTabBody(panel, active, view, rc);
    shown = active ?? null;
  }

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
  // drew is DISCARDED, so it is unsubscribed as it is dropped.
  //
  // EVERYTHING IS DRAWN BEFORE THE OLD CONTENT COMES DOWN, and the swap is one
  // DOM operation (owner ruling, 2026-10-08: a merge bubble's update never
  // moves a scroll). The expanded bubble's sub-feed is a scroll box, and a row
  // drawn while the panel stood empty could read layout there, clamping that
  // box's scroll to the emptied height and throwing the reader back up.
  if (tab === undefined) {
    replaceTicking(host);
    return;
  }
  const content = drawTabContent(tab, view, rc);
  host.setAttribute("data-merge-tab-body", tab.kind);
  replaceTicking(host, content);
}

/**
 * Bring HOST, showing TAB's content as it was drawn from PREVIOUS (the same
 * tab), to TAB IN PLACE, answering whether it could. A tests tab updates its
 * suites where they stand (`updateFeedMergeTabTests`); an agentic tab places
 * its rows in the container it already has, so a row that stays is never
 * moved; any other tab is left as it stands when its row did not change, and
 * answers false (draw afresh) when it did.
 */
export function updateTabBody(
  host: HTMLElement,
  tab: MergeTab,
  previous: MergeTab,
  view: SubfeedView,
  rc: RowContext,
): boolean {
  const path = "FeedMergeTab.kind";
  const kind = requireCase(tab.tab.kind, path);
  const before = requireCase(previous.tab.kind, path);
  switch (kind.case) {
    case "tests":
      if (before.case !== "tests") return false;
      updateFeedMergeTabTests(host, kind.value, before.value, rc.ctx, `${path}.tests`);
      return true;
    case "fixes": {
      if (before.case !== "fixes" || stoppable(previous) !== stoppable(tab)) return false;
      const rows = host.querySelector<HTMLElement>(":scope > .merge-tab-rows");
      const attempt = host.firstElementChild;
      if (rows === null || attempt === null || attempt === rows) return false;
      if (!equals(FeedRowSchema, previous.row, tab.row)) {
        attempt.replaceWith(drawFeedMergeTabFixesAttempt(kind.value, `${path}.fixes`));
      }
      placeChildren(rows, agenticRowsOf(tab, view));
      return true;
    }
    case "pick":
    case "prePrompt":
    case "conflicts":
    case "postPrompt": {
      if (stoppable(previous) !== stoppable(tab)) return false;
      const rows = host.querySelector<HTMLElement>(":scope > .merge-tab-rows");
      if (rows === null) return false;
      placeChildren(rows, agenticRowsOf(tab, view));
      return true;
    }
    default:
      return equals(FeedRowSchema, previous.row, tab.row);
  }
}

/** The selected tab's content, drawn off the document. */
function drawTabContent(tab: MergeTab, view: SubfeedView, rc: RowContext): HTMLElement[] {
  const path = "FeedMergeTab.kind";
  const kind = requireCase(tab.tab.kind, path);
  switch (kind.case) {
    case "queue":
      return [drawFeedMergeQueue(requireMessage(kind.value.queue, "FeedMergeTabQueue.queue"), rc)];
    case "rebasing":
      return [drawFeedMergeTabRebasing(kind.value, `${path}.rebasing`)];
    case "tests":
      return drawFeedMergeTabTests(kind.value, rc.ctx, `${path}.tests`);
    case "committing":
      return [drawFeedMergeTabCommitting(kind.value, `${path}.committing`)];
    case "updatingMain":
      return [drawFeedMergeTabUpdatingMain(kind.value, `${path}.updating_main`)];
    case "fixes":
      // AGENTIC, and it names which attempt it is above the rows it holds.
      return [drawFeedMergeTabFixesAttempt(kind.value, `${path}.fixes`), ...agenticContent(tab, view, rc)];
    case "pick":
    case "prePrompt":
    case "conflicts":
    case "postPrompt":
      return agenticContent(tab, view, rc);
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * An agentic tab's content: the stop control while its agent runs, then its
 * rows. The rows are this workspace's own turns for a sync merge, or the hidden
 * merge workspace's turns the daemon mirrors here for an async one; either way
 * they are rows of this sub-feed parented to the tab.
 */
function agenticContent(tab: MergeTab, view: SubfeedView, rc: RowContext): HTMLElement[] {
  const rows = drawAgenticRows(tab, view);
  if (!stoppable(tab)) return [rows];
  const id = requireMessage(tab.row.id, "FeedRow.id");
  return [drawStopControl(rc, MERGE_TAB_STOP, id), rows];
}

/** Whether a tab's agent is running, which is when the tab offers "stop". */
export function stoppable(tab: MergeTab): boolean {
  return AGENTIC_KINDS.includes(tab.kind) && (tab.state === "live" || tab.state === "waitingOnUser");
}

/**
 * The rows parented to an agentic tab, in feed order, through the ordinary path.
 *
 * ONLY the rows naming THIS tab as their container: a merge bubble's sub-feed
 * carries every tab's conversation at once, and the tab is the routing address
 * the daemon stamped on each row.
 */
export function drawAgenticRows(tab: MergeTab, view: SubfeedView): HTMLElement {
  // Every row is drawn first and only then moved into the new container, so
  // no row leaves the document while another is still being drawn.
  const rows = agenticRowsOf(tab, view);
  const el = document.createElement("div");
  el.className = "merge-tab-rows";
  el.setAttribute(NEST_ATTRIBUTE, "");
  el.append(...rows);
  return el;
}

/** The rows parented to TAB, in feed order, each drawn through the ordinary path. */
function agenticRowsOf(tab: MergeTab, view: SubfeedView): HTMLElement[] {
  const rows: HTMLElement[] = [];
  for (const row of view.rows()) {
    if (row.row.case === "mergeTab") continue;
    if (parentOf(row) !== tab.id) continue;
    rows.push(view.drawRow(row));
  }
  return rows;
}

/** The container a row names, or undefined for a top-level row. */
function parentOf(row: FeedRow): string | undefined {
  return row.parent?.row?.value;
}

/** The agentic kinds, re-exported so a suite can hold this file to the schema. */
export { AGENTIC_KINDS };
