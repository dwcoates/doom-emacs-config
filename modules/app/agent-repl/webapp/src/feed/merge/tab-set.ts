/**
 * tab-set — ONE strip of merge tabs, the line under it, and the selected tab's
 * panel: the shape the bubble's own strip and every nested strip (a rebasing
 * tab's commits, a tests tab's runs) share.
 *
 * SELECTION IS LOCAL AND THE READER'S. The auto rule (the most relevant tab,
 * `autoSelectedTab`) decides until the reader clicks, and their choice then
 * stands — a re-push must never yank the tab out from under someone reading
 * it. It is released when a tab in THIS strip CHANGES STATE (a tab appears,
 * starts, waits on the user, or settles), because that is the run moving on
 * and the reader asked to follow the run, not to be pinned to a phase that is
 * over; a push that only redrew content or moved a clock keeps it. Each strip
 * keeps its own choice.
 *
 * THE STRIP IS DRAWN IN PLACE (owner request, 2026-10-10). The daemon re-pushes
 * a tab row on every beat of its work — a run on every unit verdict, many times
 * a second — and a strip rebuilt on each push restarted every live tab's
 * breathing dot, which read as the dots blinking. Each tab keeps its element
 * for its whole life, repainted only where its tab or its activeness changed.
 *
 * THE PANEL IS DRAWN IN PLACE while the same tab stays the one shown: its panel
 * object updates itself (a nested strip draws its own tabs again), and only a
 * different tab, or a panel that cannot follow the change, is drawn afresh.
 * Only a different tab OPENS its panel (`TabPanel.opened`), which is when a
 * panel of agent or test output shows its newest page.
 * EVERYTHING IS DRAWN BEFORE THE OLD CONTENT COMES DOWN, and the swap is one
 * DOM operation (owner ruling, 2026-10-08: a merge bubble's update never moves
 * a scroll).
 */
import { equals } from "@bufbuild/protobuf";
import { log } from "../../log.js";
import { placeChildren } from "../../dom.js";
import type { Ticker } from "../../clock.js";
import { FeedRowSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { replaceTicking, stopTicking } from "../ticking.js";
import { requireMessage } from "../../rpc/strict.js";
import {
  autoSelectedTab,
  drawFeedMergeTab,
  updateFeedMergeTab,
  type MergeTab,
} from "./tab-strip.js";

/** One selected tab's content, which follows its tab's later pushes itself. */
export interface TabPanel {
  /** The content, drawn off the document. */
  readonly nodes: readonly HTMLElement[];
  /**
   * Bring the content to TAB (the same tab, pushed again) IN PLACE, answering
   * whether it could; false has the panel drawn afresh.
   */
  update(tab: MergeTab): boolean;
  /**
   * The reader clicked BADGE, the badge of the tab this panel already shows.
   * A panel with views of its own (a run's tests and remediation) offers them
   * here; a panel without leaves it unset, and the click only pins the tab.
   */
  reselect?(badge: HTMLElement): void;
  /**
   * The strip just OPENED this panel: its tab became the one shown (the
   * reader's click, the auto rule moving on, or the strip's first draw when
   * its bubble or its parent tab opened), never a redraw of the tab already
   * shown. A panel holding agent or test output brings its newest page into
   * view here (`openAtNewest`, scroll.ts); a panel without leaves it unset.
   */
  opened?(): void;
}

/** Builds the panel of one tab. */
export type TabPanelFactory = (tab: MergeTab) => TabPanel;

/** A drawn strip: draw it at the tabs served now, or take it down. */
export interface TabSet {
  draw(tabs: readonly MergeTab[]): void;
  dispose(): void;
}

/** Where a strip sits: the bubble's own strip, or nested in a tab's panel. */
export type TabSetDepth = "top" | "nested";

/**
 * Mount a strip, its summary line and its panel into HOST, drawing each
 * selected tab's content through PANEL.
 */
export function createTabSet(
  host: HTMLElement,
  ticker: Ticker,
  depth: TabSetDepth,
  panelOf: TabPanelFactory,
): TabSet {
  /** The tab the reader picked, by id; null while the auto rule decides. */
  let picked: string | null = null;
  /** Every tab's state as last drawn, so a state change can release the reader's pick. */
  let states = "";
  /** Each drawn tab's element, and the tab and activeness it was drawn from, by tab id. */
  const drawnTabs = new Map<string, { el: HTMLElement; tab: MergeTab; active: boolean }>();
  /** The tab the panel last drew, and its panel, kept while that tab stays shown. */
  let shown: { tab: MergeTab; panel: TabPanel } | null = null;
  /** The tabs as last served, so a click redraws at them. */
  let served: readonly MergeTab[] = [];

  const strip = document.createElement("div");
  strip.className = depth === "top" ? "merge-strip" : "merge-strip is-nested";
  const summary = document.createElement("div");
  summary.className = "merge-tab-summary";
  const panel = document.createElement("div");
  panel.className = "merge-tab-panel";
  host.append(strip, summary, panel);

  const draw = (tabs: readonly MergeTab[]): void => {
    served = tabs;
    releasePickOnStateChange(tabs);
    const active = tabs.find((t) => t.id === picked) ?? autoSelectedTab(tabs);
    log.debug("drawing a merge tab strip", {
      operation: "merge.draw-strip",
      context: { depth, tabs: tabs.length, active: active?.kind ?? "none", picked: picked ?? "auto" },
    });
    drawPanel(active);
    drawStrip(tabs, active);
    drawSummary(summary, active);
  };

  function drawStrip(tabs: readonly MergeTab[], active: MergeTab | undefined): void {
    const els = tabs.map((tab) => {
      const isActive = tab.id === active?.id;
      const drawn = drawnTabs.get(tab.id);
      if (drawn === undefined) {
        const el = drawFeedMergeTab(tab, { active: isActive, ticker });
        el.addEventListener("click", () => {
          picked = tab.id;
          if (shown !== null && shown.tab.id === tab.id) {
            shown.panel.reselect?.(el);
            return;
          }
          draw(served);
        });
        drawnTabs.set(tab.id, { el, tab, active: isActive });
        return el;
      }
      if (drawn.active !== isActive || drawn.tab.label !== tab.label || !equals(FeedRowSchema, drawn.tab.row, tab.row)) {
        updateFeedMergeTab(drawn.el, tab, drawn.tab, { active: isActive, ticker });
        drawn.tab = tab;
        drawn.active = isActive;
      }
      return drawn.el;
    });
    const ids = new Set(tabs.map((tab) => tab.id));
    for (const [id, drawn] of drawnTabs) {
      if (ids.has(id)) continue;
      stopTicking(drawn.el);
      drawnTabs.delete(id);
    }
    placeChildren(strip, els);
  }

  function drawPanel(active: MergeTab | undefined): void {
    if (active !== undefined && shown !== null && shown.tab.id === active.id && shown.tab.kind === active.kind) {
      if (shown.panel.update(active)) {
        shown.tab = active;
        return;
      }
    }
    if (active === undefined) {
      replaceTicking(panel);
      shown = null;
      return;
    }
    const opening = shown === null || shown.tab.id !== active.id;
    const next = panelOf(active);
    panel.setAttribute("data-merge-tab-body", active.kind);
    replaceTicking(panel, [...next.nodes]);
    shown = { tab: active, panel: next };
    if (opening) next.opened?.();
  }

  /**
   * Let go of the reader's pick when any tab's state moved since the last draw.
   *
   * Tabs are APPEND-ONLY, so a new tab is a state change too: the run moved
   * into a new phase, or a phase began, waited or ended — the events that
   * should move the view.
   */
  function releasePickOnStateChange(tabs: readonly MergeTab[]): void {
    const next = tabStatesOf(tabs);
    if (next !== states) picked = null;
    states = next;
  }

  return {
    draw,
    dispose(): void {
      for (const part of [strip, summary, panel]) {
        stopTicking(part);
        part.remove();
      }
    },
  };
}

/**
 * The states of TABS as one comparable key: each tab's id, state and outcome,
 * in strip order. Content and clocks are left out, since neither is a change
 * of state.
 */
export function tabStatesOf(tabs: readonly MergeTab[]): string {
  return JSON.stringify(tabs.map((tab) => [tab.id, tab.state, tab.outcome ?? null]));
}

/**
 * The line under a strip: a settled-failed tab's own account, or the merge a
 * test run waits behind for its repository's test turn.
 *
 * The daemon composed both; the tab's content carries the detail, so this
 * states the outcome or the wait once and does not restate what is below it.
 */
export function drawSummary(host: HTMLElement, tab: MergeTab | undefined): void {
  replaceTicking(host);
  host.hidden = true;
  if (tab !== undefined && tab.state === "waitingForTurn") {
    const waiting = tab.value["state"] as { value: { holder?: { workspaceName: string } } };
    const holder = requireMessage(waiting.value.holder, "FeedMergeTabTestsRunWaitingForTurn.holder");
    const el = document.createElement("div");
    el.className = "merge-tab-waiting-for-turn";
    el.textContent = `waiting for the test turn: ${holder.workspaceName}'s tests run first`;
    host.append(el);
    host.hidden = false;
    return;
  }
  if (tab === undefined || tab.state !== "settled" || tab.outcome !== "failed") return;
  const settled = tab.value["state"] as { value: { outcome: { value: { summary: string } } } };
  const el = document.createElement("div");
  el.className = "merge-tab-failed";
  el.textContent = settled.value.outcome.value.summary;
  host.append(el);
  host.hidden = false;
}
