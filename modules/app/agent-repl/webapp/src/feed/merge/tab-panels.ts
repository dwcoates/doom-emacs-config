/**
 * tab-panels — what a selected merge tab shows beneath its strip, one panel per
 * kind, at every level of the bubble's nested strips.
 *
 * FIVE SHAPES, ONE FACTORY. RESOLVED tabs (committing, a script hook, and
 * updating main in a bubble recorded before deploys) draw their own row. The
 * QUEUE tab draws its snapshot in place, held while a waiting merge is
 * dragged (`queue.ts`). AGENTIC tabs (the resolving turn, a prompt hook, a
 * conflict resolution, a fixing attempt) are containers: their content is the
 * sub-feed rows parented to them, drawn through `view.drawRow` — the ordinary
 * row path, chrome included — which is what keeps a merge agent's
 * conversation identical to any other agent's. PARENT tabs (rebasing,
 * resolving conflicts, tests, fixing tests) draw a nested strip of the tab
 * rows parented to them, through the same `createTabSet` the bubble's own
 * strip is. NARRATED subtabs draw their own row and nothing else: a commit its
 * replay's narration, a run its suites.
 *
 * AGENTS DRAW ONLY IN THEIR OWN TABS (owner request, 2026-11-09): a conflict
 * resolution is a subtab of the resolving-conflicts tab, beside the rebasing
 * tab, and a fixing attempt a subtab of the fixing-tests tab, beside the tests
 * tab. A commit's subtab never holds its resolution, and a run's never its
 * remediation.
 *
 * AN AGENT STREAM IS THE SAME FOR EVERY MERGE: this workspace's own turns for a
 * sync merge, or the hidden merge workspace's turns the daemon mirrors here for
 * an async one; either way rows of this sub-feed parented to the tab. While the
 * stream's agent runs it offers "stop" above it (stop-control.ts), aimed at the
 * row the stream is parented to.
 */
import { equals } from "@bufbuild/protobuf";
import { placeChildren } from "../../dom.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import {
  FeedRowSchema,
  type FeedMergeTestsRunOutput,
  type FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { NEST_ATTRIBUTE, type RowContext, type SubfeedView } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import { AGENTIC_KINDS, childTabsOf, type MergeTab } from "./tab-strip.js";
import { createTabSet, type TabPanel } from "./tab-set.js";
import { createFeedMergeQueueTable } from "./queue.js";
import {
  drawFeedMergeTestsRunOutput,
  updateFeedMergeTestsRunOutput,
} from "./tests-tab.js";
import {
  drawFeedMergeConflictFiles,
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixingAttempt,
  drawFeedMergeTabRebaseCommit,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
} from "./step-tabs.js";
import { drawFeedMergeTabHookScript } from "./hook-tabs.js";
import { buildInterruptMergeTabRequest } from "../requests.js";
import { drawStopControl, type StopSite } from "../stop-control.js";
import { openAtNewest } from "../../scroll.js";

/** The stop a running agent stream offers: its row, stopped as a merge tab. */
const MERGE_TAB_STOP: StopSite = {
  prefix: "merge-tab-stop",
  operation: "feed.merge-tab-stop",
  refusedOperation: "feed.merge-tab-stop-refused",
  describe: "stopping a merge tab's agent",
  malformedOperation: "feed.merge-tab-stop-malformed",
  request: buildInterruptMergeTabRequest,
};

const PATH = "FeedMergeTab.kind";

/** The panel of TAB, over VIEW. */
export function mergeTabPanel(tab: MergeTab, view: SubfeedView, rc: RowContext): TabPanel {
  const kind = requireCase(tab.tab.kind, PATH).case;
  switch (kind) {
    case "queue":
      return queuePanel(tab, rc);
    case "committing":
      return rowPanel(tab, () => drawFeedMergeTabCommitting(armOf(tab, "committing"), `${PATH}.committing`));
    case "updatingMain":
      return rowPanel(tab, () => drawFeedMergeTabUpdatingMain(armOf(tab, "updatingMain"), `${PATH}.updating_main`));
    case "hookScript":
      return rowPanel(tab, () => drawFeedMergeTabHookScript(armOf(tab, "hookScript"), `${PATH}.hook_script`));
    case "resolving":
    case "hookPrompt":
      return agenticPanel(tab, view, rc, () => null);
    case "conflictResolution":
      return agenticPanel(tab, view, rc, (t) => drawFeedMergeConflictFiles(armOf(t, "conflictResolution").files));
    case "testsRunFixing":
      return agenticPanel(tab, view, rc, (t) =>
        drawFeedMergeTabFixingAttempt(armOf(t, "testsRunFixing"), `${PATH}.tests_run_fixing`),
      );
    case "rebaseCommit":
      return rowPanel(tab, () => drawFeedMergeTabRebaseCommit(armOf(tab, "rebaseCommit"), `${PATH}.rebase_commit`));
    case "rebasing":
      return parentPanel(tab, view, rc, (t) => drawFeedMergeTabRebasing(armOf(t, "rebasing"), `${PATH}.rebasing`, rc));
    case "resolvingConflicts":
    case "tests":
    case "fixingTests":
      return parentPanel(tab, view, rc, () => null);
    case "testsRun":
      return runPanel(tab, rc);
    default:
      return unreachableArm(PATH, kind);
  }
}


/** The kind arm's own message of TAB, as the arm CASE. */
function armOf<K extends string>(tab: MergeTab, kase: K): Extract<NonNullable<MergeTab["tab"]["kind"]>, { case: K }>["value"] {
  const kind = requireCase(tab.tab.kind, PATH);
  if (kind.case !== kase) throw new Error(`merge: a ${kind.case} tab was drawn as ${kase}`);
  return kind.value as never;
}

/**
 * The QUEUE tab's panel: its snapshot, redrawn IN PLACE from each changed
 * push, so a drag of one of its waiting merges outlives every push
 * (`createFeedMergeQueueTable`).
 */
function queuePanel(tab: MergeTab, rc: RowContext): TabPanel {
  const queueOf = (t: MergeTab) => requireMessage(armOf(t, "queue").queue, "FeedMergeTabQueue.queue");
  const table = createFeedMergeQueueTable(rc);
  table.offer(queueOf(tab));
  let shown = tab.row;
  return {
    nodes: [table.element],
    update(next: MergeTab): boolean {
      if (equals(FeedRowSchema, shown, next.row)) return true;
      shown = next.row;
      table.offer(queueOf(next));
      return true;
    },
  };
}

/**
 * A RESOLVED tab's panel: its row drawn whole, left standing while the row is
 * unchanged and drawn afresh when it changed.
 */
function rowPanel(tab: MergeTab, draw: () => HTMLElement): TabPanel {
  let shown = tab.row;
  return {
    nodes: [draw()],
    update(next: MergeTab): boolean {
      if (!equals(FeedRowSchema, shown, next.row)) return false;
      shown = next.row;
      return true;
    },
  };
}

/**
 * An AGENTIC tab's panel: an optional heading (a fixing attempt's "attempt
 * 2/3", a conflict resolution's conflicted files), the stop control while its
 * agent runs, then its rows.
 */
function agenticPanel(
  tab: MergeTab,
  view: SubfeedView,
  rc: RowContext,
  heading: (tab: MergeTab) => HTMLElement | null,
): TabPanel {
  let shown = tab;
  let head = heading(tab);
  const stream = drawAgentStream(tab, stoppable(tab), view, rc);
  return {
    nodes: head === null ? stream.nodes : [head, ...stream.nodes],
    update(next: MergeTab): boolean {
      // THE STOP COMES AND GOES WITH THE AGENT: a change of it draws afresh.
      if (stoppable(next) !== stoppable(shown)) return false;
      if (!equals(FeedRowSchema, shown.row, next.row)) head = swapHeading(head, heading(next));
      placeChildren(stream.rows, agenticRowsOf(next, view));
      shown = next;
      return true;
    },
    opened(): void {
      openAtNewest(stream.rows);
    },
  };
}

/**
 * Put NEXT in HEAD's place and answer the heading that now stands. A panel
 * with no heading has none to swap, and keeps none.
 */
export function swapHeading(head: HTMLElement | null, next: HTMLElement | null): HTMLElement | null {
  if (head === null || next === null) return head;
  // A heading that holds a clock must not keep ticking once it is gone.
  stopTicking(head);
  head.replaceWith(next);
  return next;
}

/** Whether an agentic tab's agent is running, which is when it offers "stop". */
export function stoppable(tab: MergeTab): boolean {
  return AGENTIC_KINDS.includes(tab.kind) && (tab.state === "live" || tab.state === "waitingOnUser");
}

/**
 * An agent's stream: the stop control while its agent runs, aimed at TAB's row,
 * then the rows parented to TAB.
 */
function drawAgentStream(
  tab: MergeTab,
  running: boolean,
  view: SubfeedView,
  rc: RowContext,
): { nodes: HTMLElement[]; rows: HTMLElement } {
  const rows = drawAgenticRows(tab, view);
  if (!running) return { nodes: [rows], rows };
  const id = requireMessage(tab.row.id, "FeedRow.id");
  return { nodes: [drawStopControl(rc, MERGE_TAB_STOP, id), rows], rows };
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

/** The ordinary rows parented to TAB, in feed order, each drawn through the ordinary path. */
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

/**
 * A PARENT tab's panel: an optional heading over a nested strip of the tab
 * rows parented to it, drawn through the same strip the bubble's own is.
 */
function parentPanel(
  tab: MergeTab,
  view: SubfeedView,
  rc: RowContext,
  heading: (tab: MergeTab) => HTMLElement | null,
): TabPanel {
  const children = (t: MergeTab): MergeTab[] => childTabsOf(view.rows(), t.id);
  let shown = tab;
  let head = heading(tab);
  const host = document.createElement("div");
  host.className = "merge-substrip";
  const set = createTabSet(host, rc.ctx.ticker, "nested", (child) => mergeTabPanel(child, view, rc));
  set.draw(children(tab));
  return {
    nodes: head === null ? [host] : [head, host],
    update(next: MergeTab): boolean {
      if (!equals(FeedRowSchema, shown.row, next.row)) head = swapHeading(head, heading(next));
      set.draw(children(next));
      shown = next;
      return true;
    },
  };
}

/**
 * A run's panel: its suites and its log, updated where they stand, so a push
 * on every unit's verdict neither redraws a suite nor moves a scroll. It opens
 * at its newest page.
 */
function runPanel(tab: MergeTab, rc: RowContext): TabPanel {
  const path = `${PATH}.tests_run.output`;
  let shownOutput = outputOf(tab);
  const host = document.createElement("div");
  host.className = "merge-run merge-run-output";
  host.append(...drawFeedMergeTestsRunOutput(shownOutput, rc.ctx, path));
  return {
    nodes: [host],
    update(next: MergeTab): boolean {
      const output = outputOf(next);
      updateFeedMergeTestsRunOutput(host, output, shownOutput, rc.ctx, path);
      shownOutput = output;
      return true;
    },
    opened(): void {
      openAtNewest(host);
    },
  };
}

/** A run's output, off its row. */
function outputOf(tab: MergeTab): FeedMergeTestsRunOutput {
  return requireMessage(armOf(tab, "testsRun").output, `${PATH}.tests_run.output`);
}
