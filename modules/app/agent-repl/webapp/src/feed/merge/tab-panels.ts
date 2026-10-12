/**
 * tab-panels — what a selected merge tab shows beneath its strip, one panel per
 * kind, at every level of the bubble's nested strips.
 *
 * FOUR SHAPES, ONE FACTORY. RESOLVED tabs (queue, committing, updating main)
 * draw their own row. AGENTIC tabs (the configured prompts, a fixing attempt)
 * are containers: their content is the sub-feed rows parented to them, drawn
 * through `view.drawRow` — the ordinary row path, chrome included — which is
 * what keeps a merge agent's conversation identical to any other agent's.
 * PARENT tabs (rebasing, tests, a run) draw a nested strip of the tab rows
 * parented to them, through the same `createTabSet` the bubble's own strip is.
 * A COMMIT draws its narration and, once its conflict was handed to the
 * session, a conflict section holding the files and that resolution's rows.
 *
 * A RUN'S STRIP OPENS WITH ITS OWN OUTPUT. The output is the run row's own
 * content rather than a row of its own, so the strip draws it as an entry
 * built from the run row, badged with the run's own state and labelled
 * "output"; the fixing attempt that followed the run is the run's child row.
 *
 * AN AGENT STREAM IS THE SAME FOR EVERY MERGE: this workspace's own turns for a
 * sync merge, or the hidden merge workspace's turns the daemon mirrors here for
 * an async one; either way rows of this sub-feed parented to the tab. While the
 * stream's agent runs it offers "stop" above it (stop-control.ts), aimed at the
 * row the stream is parented to.
 */
import { create, equals } from "@bufbuild/protobuf";
import { placeChildren } from "../../dom.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import {
  FeedMergeTabLabelSchema,
  FeedRowSchema,
  type FeedMergeConflictFile,
  type FeedMergeTestsRunOutput,
  type FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { NEST_ATTRIBUTE, type RowContext, type SubfeedView } from "../renderers.js";
import { AGENTIC_KINDS, childTabsOf, type MergeTab } from "./tab-strip.js";
import { createTabSet, type TabPanel } from "./tab-set.js";
import { drawFeedMergeQueue } from "./queue.js";
import { drawFeedMergeTestsRunOutput, updateFeedMergeTestsRunOutput } from "./tests-tab.js";
import {
  drawFeedMergeConflictFiles,
  drawFeedMergeTabCommitting,
  drawFeedMergeTabFixingAttempt,
  drawFeedMergeTabRebaseCommit,
  drawFeedMergeTabRebasing,
  drawFeedMergeTabUpdatingMain,
} from "./step-tabs.js";
import { buildInterruptMergeTabRequest } from "../requests.js";
import { drawStopControl, type StopSite } from "../stop-control.js";

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

/** The kind a run's own output entry is drawn as in the run's strip. */
export const RUN_OUTPUT_KIND = "testsRunOutput";

/** What a run's output entry's id adds to the run row's, so the two never meet. */
const RUN_OUTPUT_SUFFIX = "#output";

/** The panel of TAB, over VIEW. */
export function mergeTabPanel(tab: MergeTab, view: SubfeedView, rc: RowContext): TabPanel {
  const kind = tab.kind === RUN_OUTPUT_KIND ? RUN_OUTPUT_KIND : requireCase(tab.tab.kind, PATH).case;
  switch (kind) {
    case "queue":
      return rowPanel(tab, () => drawFeedMergeQueue(requireMessage(armOf(tab, "queue").queue, "FeedMergeTabQueue.queue"), rc));
    case "committing":
      return rowPanel(tab, () => drawFeedMergeTabCommitting(armOf(tab, "committing"), `${PATH}.committing`));
    case "updatingMain":
      return rowPanel(tab, () => drawFeedMergeTabUpdatingMain(armOf(tab, "updatingMain"), `${PATH}.updating_main`));
    case "pick":
    case "prePrompt":
    case "postPrompt":
      return agenticPanel(tab, view, rc, () => null);
    case "testsRunFixing":
      return agenticPanel(tab, view, rc, (t) =>
        drawFeedMergeTabFixingAttempt(armOf(t, "testsRunFixing"), `${PATH}.tests_run_fixing`),
      );
    case "rebaseCommit":
      return commitPanel(tab, view, rc);
    case "rebasing":
      return parentPanel(tab, view, rc, (t) => drawFeedMergeTabRebasing(armOf(t, "rebasing"), `${PATH}.rebasing`), (t) =>
        childTabsOf(view.rows(), t.id),
      );
    case "tests":
      return parentPanel(tab, view, rc, () => null, (t) => childTabsOf(view.rows(), t.id));
    case "testsRun":
      return parentPanel(tab, view, rc, () => null, (t) => [runOutputEntry(t), ...childTabsOf(view.rows(), t.id)]);
    case RUN_OUTPUT_KIND:
      return runOutputPanel(tab, rc);
    default:
      return unreachableArm(PATH, kind);
  }
}

/**
 * A run's output as an entry of the run's own strip: the run row itself,
 * labelled "output" and badged with the run's own state.
 */
export function runOutputEntry(run: MergeTab): MergeTab {
  return {
    ...run,
    id: `${run.id}${RUN_OUTPUT_SUFFIX}`,
    kind: RUN_OUTPUT_KIND,
    label: create(FeedMergeTabLabelSchema, { text: "output", round: 1 }),
  };
}

/** The kind arm's own message of TAB, as the arm CASE. */
function armOf<K extends string>(tab: MergeTab, kase: K): Extract<NonNullable<MergeTab["tab"]["kind"]>, { case: K }>["value"] {
  const kind = requireCase(tab.tab.kind, PATH);
  if (kind.case !== kase) throw new Error(`merge: a ${kind.case} tab was drawn as ${kase}`);
  return kind.value as never;
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
 * 2/3"), the stop control while its agent runs, then its rows.
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
      if (!equals(FeedRowSchema, shown.row, next.row) && head !== null) {
        const nextHead = heading(next);
        if (nextHead !== null) {
          head.replaceWith(nextHead);
          head = nextHead;
        }
      }
      placeChildren(stream.rows, agenticRowsOf(next, view));
      shown = next;
      return true;
    },
  };
}

/** Whether an agentic tab's agent is running, which is when it offers "stop". */
export function stoppable(tab: MergeTab): boolean {
  return AGENTIC_KINDS.includes(tab.kind) && (tab.state === "live" || tab.state === "waitingOnUser");
}

/** Whether a commit's conflict resolution is running, which is when its section offers "stop". */
export function conflictStoppable(tab: MergeTab): boolean {
  const state = armOf(tab, "rebaseCommit").conflict?.state.case;
  return state === "live" || state === "waitingOnUser";
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
 * A commit's panel: its narration, and, once its conflict was handed to the
 * session, a conflict section of the files over that resolution's rows.
 */
function commitPanel(tab: MergeTab, view: SubfeedView, rc: RowContext): TabPanel {
  let shown = tab;
  let lines = drawFeedMergeTabRebaseCommit(armOf(tab, "rebaseCommit"));
  const conflict = armOf(tab, "rebaseCommit").conflict;
  let files: HTMLElement | null = null;
  let stream: HTMLElement | null = null;
  const nodes: HTMLElement[] = [lines];
  if (conflict !== undefined) {
    const section = document.createElement("div");
    section.className = "merge-conflict";
    files = drawFeedMergeConflictFiles(conflict.files);
    const drawn = drawAgentStream(tab, conflictStoppable(tab), view, rc);
    stream = drawn.rows;
    section.append(files, ...drawn.nodes);
    nodes.push(section);
  }
  return {
    nodes,
    update(next: MergeTab): boolean {
      const before = armOf(shown, "rebaseCommit");
      const after = armOf(next, "rebaseCommit");
      const changed = !equals(FeedRowSchema, shown.row, next.row);
      // A conflict section that appeared, or a stop that came or went, is a
      // new shape: drawn afresh.
      if ((before.conflict === undefined) !== (after.conflict === undefined)) return false;
      if (before.conflict !== undefined && conflictStoppable(shown) !== conflictStoppable(next)) return false;
      if (changed) {
        const nextLines = drawFeedMergeTabRebaseCommit(after);
        lines.replaceWith(nextLines);
        lines = nextLines;
        if (files !== null && after.conflict !== undefined && !sameFiles(before.conflict?.files ?? [], after.conflict.files)) {
          const nextFiles = drawFeedMergeConflictFiles(after.conflict.files);
          files.replaceWith(nextFiles);
          files = nextFiles;
        }
      }
      if (stream !== null) placeChildren(stream, agenticRowsOf(next, view));
      shown = next;
      return true;
    },
  };
}

/** Whether two conflicted file lists name the same paths in the same order. */
function sameFiles(a: readonly FeedMergeConflictFile[], b: readonly FeedMergeConflictFile[]): boolean {
  return a.length === b.length && a.every((file, i) => file.path === b[i]?.path);
}

/**
 * A PARENT tab's panel: an optional heading over a nested strip of the tabs
 * CHILDREN answers, drawn through the same strip the bubble's own is.
 */
function parentPanel(
  tab: MergeTab,
  view: SubfeedView,
  rc: RowContext,
  heading: (tab: MergeTab) => HTMLElement | null,
  children: (tab: MergeTab) => MergeTab[],
): TabPanel {
  let shown = tab;
  let head = heading(tab);
  const host = document.createElement("div");
  host.className = "merge-substrip";
  const set = createTabSet(host, rc.ctx.ticker, "nested", (child) => mergeTabPanel(child, view, rc));
  set.draw(children(tab));
  return {
    nodes: head === null ? [host] : [head, host],
    update(next: MergeTab): boolean {
      if (head !== null && !equals(FeedRowSchema, shown.row, next.row)) {
        const nextHead = heading(next);
        if (nextHead !== null) {
          head.replaceWith(nextHead);
          head = nextHead;
        }
      }
      set.draw(children(next));
      shown = next;
      return true;
    },
  };
}

/** A run's output panel: its suites and its log, updated where they stand. */
function runOutputPanel(tab: MergeTab, rc: RowContext): TabPanel {
  const path = `${PATH}.tests_run.output`;
  let shown = outputOf(tab);
  const host = document.createElement("div");
  host.className = "merge-run-output";
  host.append(...drawFeedMergeTestsRunOutput(shown, rc.ctx, path));
  return {
    nodes: [host],
    update(next: MergeTab): boolean {
      const output = outputOf(next);
      updateFeedMergeTestsRunOutput(host, output, shown, rc.ctx, path);
      shown = output;
      return true;
    },
  };
}

/** A run's output, off its row. */
function outputOf(tab: MergeTab): FeedMergeTestsRunOutput {
  return requireMessage(armOf(tab, "testsRun").output, `${PATH}.tests_run.output`);
}
