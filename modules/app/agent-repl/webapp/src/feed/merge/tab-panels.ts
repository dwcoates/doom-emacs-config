/**
 * tab-panels — what a selected merge tab shows beneath its strip, one panel per
 * kind, at every level of the bubble's nested strips.
 *
 * FIVE SHAPES, ONE FACTORY. RESOLVED tabs (queue, committing, a script hook,
 * and updating main in a bubble recorded before deploys) draw their own row.
 * AGENTIC tabs (a prompt hook, a conflict resolution, a fixing attempt)
 * are containers: their content is the sub-feed rows parented to them, drawn
 * through `view.drawRow` — the ordinary row path, chrome included — which is
 * what keeps a merge agent's conversation identical to any other agent's.
 * PARENT tabs (rebasing, tests) draw a nested strip of the tab rows parented
 * to them, through the same `createTabSet` the bubble's own strip is.
 * A COMMIT draws its narration and, once its conflict was handed to the
 * session, a conflict section holding the files and that resolution's rows.
 *
 * A RUN IS ONE PANEL OF TWO VIEWS, NEVER A STRIP (owner request, 2026-10-13):
 * "tests", the run row's own output, and "remediation", the fixing attempt
 * that followed a failed run (the run's one child row). The run shows its
 * tests until a fixing attempt exists, and that attempt from then on; clicking
 * the run's already selected badge opens a dropdown that switches between the
 * two, and the reader's choice then stands (`runPanel`). The run's tally rides
 * in the run's own badge (tab-strip.ts), whichever view is shown.
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
  type FeedMergeConflictFile,
  type FeedMergeTestsRunOutput,
  type FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { NEST_ATTRIBUTE, type RowContext, type SubfeedView } from "../renderers.js";
import { createControl } from "../../control.js";
import { log } from "../../log.js";
import type { Ticker } from "../../clock.js";
import { stopTicking } from "../ticking.js";
import { AGENTIC_KINDS, childTabsOf, drawTabDuration, drawTabStateGlyph, type MergeTab } from "./tab-strip.js";
import { createTabSet, drawSummary, type TabPanel } from "./tab-set.js";
import { drawFeedMergeQueue } from "./queue.js";
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
      return rowPanel(tab, () => drawFeedMergeQueue(requireMessage(armOf(tab, "queue").queue, "FeedMergeTabQueue.queue"), rc));
    case "committing":
      return rowPanel(tab, () => drawFeedMergeTabCommitting(armOf(tab, "committing"), `${PATH}.committing`));
    case "updatingMain":
      return rowPanel(tab, () => drawFeedMergeTabUpdatingMain(armOf(tab, "updatingMain"), `${PATH}.updating_main`));
    case "hookScript":
      return rowPanel(tab, () => drawFeedMergeTabHookScript(armOf(tab, "hookScript"), `${PATH}.hook_script`));
    case "resolving":
    case "hookPrompt":
      return agenticPanel(tab, view, rc, () => null);
    case "testsRunFixing":
      return agenticPanel(tab, view, rc, (t) =>
        drawFeedMergeTabFixingAttempt(armOf(t, "testsRunFixing"), `${PATH}.tests_run_fixing`),
      );
    case "rebaseCommit":
      return commitPanel(tab, view, rc);
    case "rebasing":
      return parentPanel(tab, view, rc, (t) => drawFeedMergeTabRebasing(armOf(t, "rebasing"), `${PATH}.rebasing`, rc), (t) =>
        childTabsOf(view.rows(), t.id),
      );
    case "tests":
      return parentPanel(tab, view, rc, () => null, (t) => childTabsOf(view.rows(), t.id));
    case "testsRun":
      return runPanel(tab, view, rc);
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
  // The heading may hold a clock (a run's remediation heading does), which
  // must not keep ticking once the heading is gone.
  stopTicking(head);
  head.replaceWith(next);
  return next;
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
  let lines = drawFeedMergeTabRebaseCommit(armOf(tab, "rebaseCommit"), `${PATH}.rebase_commit`);
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
        const nextLines = drawFeedMergeTabRebaseCommit(after, `${PATH}.rebase_commit`);
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
    opened(): void {
      // A commit with no conflict is a few lines of narration, with no agent
      // output to open at.
      if (stream !== null) openAtNewest(stream);
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
      if (!equals(FeedRowSchema, shown.row, next.row)) head = swapHeading(head, heading(next));
      set.draw(children(next));
      shown = next;
      return true;
    },
  };
}

/** The two views of a run's panel. */
export type RunView = "tests" | "remediation";

/** Every view of a run, in the order its dropdown lists them. */
export const RUN_VIEWS: readonly RunView[] = ["tests", "remediation"];

/**
 * The view a run shows while the reader has not chosen one: its remediation
 * once a fixing attempt followed it, its tests until then.
 */
export function autoRunView(fixing: MergeTab | undefined): RunView {
  return fixing === undefined ? "tests" : "remediation";
}

/** The fixing attempt that followed RUN, off VIEW's rows; a run has at most one. */
export function fixingOf(run: MergeTab, view: SubfeedView): MergeTab | undefined {
  const fixing = childTabsOf(view.rows(), run.id).filter((tab) => tab.kind === "testsRunFixing");
  if (fixing.length > 1) throw new Error(`merge: run ${run.id} has ${fixing.length} fixing attempts, and a run has at most one`);
  return fixing[0];
}

/**
 * A run's panel: its TESTS view
 * (the suites and the log, updated where they stand) and, once a fixing attempt followed the run, its REMEDIATION view (the
 * attempt's agent stream under its heading). BOTH VIEWS STAY DRAWN and only
 * the one shown is unhidden, so switching views neither redraws a row nor
 * restarts a clock. The view shown when the panel opens, and every view shown
 * after it, opens at its newest page (`openAtNewest`). `reselect` (the reader clicked the run's badge again)
 * opens the dropdown that switches them; a run with no remediation has only
 * the one view, and offers none.
 */
function runPanel(tab: MergeTab, view: SubfeedView, rc: RowContext): TabPanel {
  const path = `${PATH}.tests_run.output`;
  let shownOutput = outputOf(tab);
  /** The view the reader picked; null while `autoRunView` decides. */
  let chosen: RunView | null = null;

  const host = document.createElement("div");
  host.className = "merge-run";
  const tests = document.createElement("div");
  tests.className = "merge-run-output merge-run-view";
  tests.setAttribute(RUN_VIEW_ATTRIBUTE, "tests");
  tests.append(...drawFeedMergeTestsRunOutput(shownOutput, rc.ctx, path));
  const remediation = document.createElement("div");
  remediation.className = "merge-run-remediation merge-run-view";
  remediation.setAttribute(RUN_VIEW_ATTRIBUTE, "remediation");
  host.append(tests, remediation);

  let fixing: { tab: MergeTab; panel: TabPanel } | null = null;
  const menu = createRunViewMenu(host, (picked) => {
    chosen = picked;
    show();
  });

  /** Bring the remediation view to FIX: in place where its panel can follow, afresh otherwise. */
  const placeFixing = (fix: MergeTab | undefined): void => {
    if (fix === undefined) {
      if (fixing !== null) throw new Error(`merge: run ${tab.id}'s fixing attempt ${fixing.tab.id} disappeared`);
      return;
    }
    if (fixing !== null && fixing.tab.id === fix.id && fixing.panel.update(fix)) {
      fixing.tab = fix;
      return;
    }
    const panel = agenticPanel(fix, view, rc, (t) => drawRemediationHeading(t, rc.ctx.ticker));
    for (const child of remediation.children) stopTicking(child);
    remediation.replaceChildren(...panel.nodes);
    fixing = { tab: fix, panel };
  };

  /** Whether the strip opened this panel yet, and the view it showed last. */
  let open = false;
  let showing: RunView | null = null;
  const show = (): void => {
    const current = chosen ?? autoRunView(fixing?.tab);
    host.setAttribute(RUN_VIEW_ATTRIBUTE, current);
    tests.hidden = current !== "tests";
    remediation.hidden = current !== "remediation";
    // A VIEW NEWLY SHOWN OPENS AT ITS NEWEST PAGE, as the panel itself did: the
    // reader's pick, or the remediation that followed a failed run.
    if (open && current !== showing) openAtNewest(current === "tests" ? tests : remediation);
    showing = current;
  };

  placeFixing(fixingOf(tab, view));
  show();
  return {
    nodes: [host],
    update(next: MergeTab): boolean {
      const output = outputOf(next);
      updateFeedMergeTestsRunOutput(tests, output, shownOutput, rc.ctx, path);
      shownOutput = output;
      placeFixing(fixingOf(next, view));
      show();
      return true;
    },
    reselect(badge: HTMLElement): void {
      if (fixing === null) {
        log.debug("a run with no remediation has only its tests to show", {
          operation: "merge.run-views-single",
          context: { run: tab.id },
        });
        return;
      }
      menu.toggle(badge, chosen ?? autoRunView(fixing.tab));
    },
    opened(): void {
      open = true;
      openAtNewest(showing === "remediation" ? remediation : tests);
    },
  };
}

/** The attribute a run's panel and each of its views name a view under. */
export const RUN_VIEW_ATTRIBUTE = "data-run-view";

/**
 * The remediation view's heading, standing in for the badge the fixing attempt
 * no longer has: "attempt 2/3", how long it has been at it, its state glyph,
 * and, once it gave up, its own account of why.
 */
export function drawRemediationHeading(fixing: MergeTab, ticker: Ticker): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-remediation-head";
  const line = document.createElement("div");
  line.className = "merge-remediation-line";
  line.append(
    drawFeedMergeTabFixingAttempt(armOf(fixing, "testsRunFixing"), `${PATH}.tests_run_fixing`),
    drawTabDuration(fixing, ticker),
    drawTabStateGlyph(fixing),
  );
  const summary = document.createElement("div");
  summary.className = "merge-tab-summary";
  drawSummary(summary, fixing);
  el.append(line, summary);
  return el;
}

/** A run's view dropdown, as its panel drives it. */
interface RunViewMenu {
  /** Open the dropdown under BADGE with CURRENT marked, or close it when open. */
  toggle(badge: HTMLElement, current: RunView): void;
}

/**
 * The dropdown a run's panel switches its views through, drawn at the top of
 * HOST under the run's badge. Picking a view hands it to PICK and closes the
 * dropdown; a click anywhere outside it, the badge's own excepted (that click
 * is the toggle's), closes it too.
 */
function createRunViewMenu(host: HTMLElement, pick: (view: RunView) => void): RunViewMenu {
  const el = document.createElement("div");
  el.className = "merge-run-views list-rows";
  el.setAttribute("role", "menu");
  el.hidden = true;
  host.prepend(el);
  let opener: HTMLElement | null = null;

  const close = (): void => {
    el.hidden = true;
    opener = null;
    document.removeEventListener("click", dismiss, true);
  };
  const dismiss = (event: MouseEvent): void => {
    const target = event.target;
    if (target instanceof Node && (el.contains(target) || opener?.contains(target) === true)) return;
    close();
  };

  return {
    toggle(badge: HTMLElement, current: RunView): void {
      if (!el.hidden) {
        close();
        return;
      }
      log.debug("opening a run's view dropdown", { operation: "merge.run-views-open", context: { current } });
      el.replaceChildren(
        ...RUN_VIEWS.map((view) => {
          const item = createControl();
          item.className = "merge-run-view-item";
          item.setAttribute("role", "menuitemradio");
          item.setAttribute(RUN_VIEW_ATTRIBUTE, view);
          item.setAttribute("aria-checked", view === current ? "true" : "false");
          item.classList.toggle("is-active", view === current);
          item.textContent = view;
          item.addEventListener("click", () => {
            close();
            pick(view);
          });
          return item;
        }),
      );
      el.style.left = `${badge.getBoundingClientRect().left - host.getBoundingClientRect().left}px`;
      el.hidden = false;
      opener = badge;
      document.addEventListener("click", dismiss, true);
    },
  };
}

/** A run's output, off its row. */
function outputOf(tab: MergeTab): FeedMergeTestsRunOutput {
  return requireMessage(armOf(tab, "testsRun").output, `${PATH}.tests_run.output`);
}
