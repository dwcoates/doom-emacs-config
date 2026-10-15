/**
 * tests-tab — one run's output sub-subtab: its test suites, and their COLORED
 * output.
 *
 * THE DAEMON PARSED THE TERMINAL, NOT THE CLIENT. Each span arrives with a
 * paint-class name out of the shared inventory and the client turns it into a
 * class; there is no ANSI parser here, no language guess, no token table. An
 * empty class is the one spelling of plain text, and a class this build has
 * never heard of draws the text unstyled rather than throwing — the text is the
 * substance, the color is decoration (`vocab.paintClass` logs the drift).
 *
 * THE OUTPUT IS ALREADY CAPPED by the daemon; the client scrolls what it was
 * given inside the block rather than growing the bubble without bound.
 *
 * FAILED SUITES LEAD THE LIST (owner request, 2026-10-14): the failed first,
 * then the still running, then the passed, then the reused, each group in
 * served order (`suitesInListOrder`).
 *
 * A REUSED SUITE DID NOT RUN IN THIS GATE: the runner found an earlier run
 * that passed its exact inputs. It draws the settled green ✓ and names that
 * run after the suite's name, as the daemon resolved it, so it never reads as
 * a pass of this gate. The run's TALLY, every suite's counts summed, rides
 * at the right end of the run strip (`drawFeedMergeTestsRunTally`).
 *
 * THE RUN'S FULL LOG IS A LINK (owner, 2026-09-30), drawn under the suites
 * once the daemon has written it: the shared editor-link verb with the log's
 * token, in the response bubble's link blue (`renderMergeTestLogLink`).
 */
import { log } from "../../log.js";
import { placeChildren, placeOptional, repaintMark } from "../../dom.js";
import { requireCase, requireMessage } from "../../rpc/strict.js";
import { paintSpanClass } from "../cards/paint.js";
import { armName } from "../renderers.js";
import { unreachableArm } from "../../rpc/strict.js";
import { create, equals } from "@bufbuild/protobuf";
import {
  FeedMergeTestCountsSchema,
  FeedMergeTestLogSchema,
  type FeedMergeTestsRunOutput,
  type FeedMergeTestCounts,
  type FeedMergeTestLog,
  type FeedMergeTestSpan,
  type FeedMergeTestSuite,
  type FeedMergeTestSuiteReused,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderMergeTestLogLink } from "../../link.js";
import type { AppContext } from "../../rpc/context.js";

const PATH = "FeedMergeTestSuite";

/** The glyph a suite reports its own state with. */
const SUITE_GLYPHS = {
  running: "●",
  passed: "✓",
  failed: "✗",
  reused: "✓",
} as const satisfies Record<string, string>;

/** A run's output: its suites, then its log link once the daemon has written it. */
export function drawFeedMergeTestsRunOutput(
  u: FeedMergeTestsRunOutput,
  ctx: AppContext,
  path: string,
): HTMLElement[] {
  const parts = [drawTestSuites(u.suites)];
  if (u.log !== undefined) parts.push(drawFeedMergeTestLog(u.log, ctx, `${path}.log`));
  return parts;
}

/**
 * Bring HOST, a run's output as `drawFeedMergeTestsRunOutput` drew it from
 * PREVIOUS, to U IN PLACE: the suites updated where they stand
 * (`updateTestSuites`), and the log line redrawn only when it changed.
 */
export function updateFeedMergeTestsRunOutput(
  host: HTMLElement,
  u: FeedMergeTestsRunOutput,
  previous: FeedMergeTestsRunOutput,
  ctx: AppContext,
  path: string,
): void {
  const suites = host.querySelector<HTMLElement>(":scope > .merge-suites");
  if (suites === null) throw new Error("merge: a drawn run's output lost its suites");
  updateTestSuites(suites, u.suites);
  const logLine = host.querySelector(":scope > .merge-test-log");
  if (u.log === undefined) {
    logLine?.remove();
    return;
  }
  if (logLine !== null && previous.log !== undefined && equals(FeedMergeTestLogSchema, previous.log, u.log)) return;
  const next = drawFeedMergeTestLog(u.log, ctx, `${path}.log`);
  if (logLine === null) host.append(next);
  else logLine.replaceWith(next);
}

/** The log line: "log:" and the link, whose text is the daemon's label. */
export function drawFeedMergeTestLog(u: FeedMergeTestLog, ctx: AppContext, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-test-log";
  el.append(document.createTextNode("log: "), renderMergeTestLogLink(ctx, u, path));
  return el;
}

/** Where each suite state sorts in a run's list: the failed lead, the reused trail. */
const SUITE_LIST_RANK = {
  failed: 0,
  running: 1,
  passed: 2,
  reused: 3,
} as const satisfies Record<keyof typeof SUITE_GLYPHS, number>;

/**
 * SUITES as a run lists them: the failed, then the still running, then the
 * passed, each group in served order.
 */
export function suitesInListOrder(suites: readonly FeedMergeTestSuite[]): FeedMergeTestSuite[] {
  const rank = (suite: FeedMergeTestSuite): number => {
    const state = requireCase(suite.state, `${PATH}.state`);
    switch (state.case) {
      case "failed":
      case "running":
      case "passed":
      case "reused":
        return SUITE_LIST_RANK[state.case];
      default:
        return unreachableArm(`${PATH}.state`, armName(state));
    }
  };
  return suites
    .map((suite, served) => ({ suite, served, rank: rank(suite) }))
    .sort((a, b) => a.rank - b.rank || a.served - b.served)
    .map((entry) => entry.suite);
}

/** Every suite of a run, in list order (`suitesInListOrder`). */
export function drawTestSuites(suites: readonly FeedMergeTestSuite[]): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-suites list-rows";
  for (const suite of suitesInListOrder(suites)) el.append(drawFeedMergeTestSuite(suite));
  return el;
}

/**
 * Bring EL, a run's suites as `drawTestSuites` drew them, to SUITES IN
 * PLACE: each suite keeps its element, keyed by its name, and is repainted
 * where it changed (`updateFeedMergeTestSuite`); a new suite is drawn into its
 * place, one no longer served is dropped, and a suite whose state moved moves
 * to its group (`suitesInListOrder`). A push that only moved a running
 * suite's counts therefore moves only those figures, and its live dot breathes
 * on unbroken.
 */
export function updateTestSuites(el: HTMLElement, suites: readonly FeedMergeTestSuite[]): void {
  const held = new Map<string, HTMLElement>();
  for (const child of el.querySelectorAll<HTMLElement>(":scope > .merge-suite")) {
    held.set(child.getAttribute(SUITE_NAME_ATTRIBUTE) ?? "", child);
  }
  const next = suitesInListOrder(suites).map((suite) => {
    const existing = held.get(suite.name);
    if (existing === undefined) return drawFeedMergeTestSuite(suite);
    updateFeedMergeTestSuite(existing, suite);
    return existing;
  });
  placeChildren(el, next);
}

/** The attribute a suite's element carries its name under, to be found again. */
const SUITE_NAME_ATTRIBUTE = "data-suite";

/** One suite: its name, its state glyph, its painted output. */
export function drawFeedMergeTestSuite(suite: FeedMergeTestSuite): HTMLElement {
  const state = requireCase(suite.state, `${PATH}.state`);
  log.debug(`drawing a merge test suite as ${state.case}`, {
    operation: "merge.draw-suite",
    context: { suite: suite.name, arm: state.case },
  });

  const el = document.createElement("div");
  el.className = "merge-suite";
  el.setAttribute(SUITE_NAME_ATTRIBUTE, suite.name);

  const head = document.createElement("div");
  head.className = "merge-suite-head";
  el.append(head);

  const glyph = document.createElement("span");
  glyph.className = "merge-suite-glyph";
  glyph.setAttribute("aria-hidden", "true");
  head.append(glyph);

  const name = document.createElement("span");
  name.className = "merge-suite-name";
  name.textContent = suite.name;
  head.append(name);

  paintSuite(el, suite);
  return el;
}

/**
 * Bring EL, a suite's element, to SUITE in place: its state, its glyph, its
 * counts and its output, each touched only where it changed (`paintSuite`).
 */
export function updateFeedMergeTestSuite(el: HTMLElement, suite: FeedMergeTestSuite): void {
  log.debug("updating a merge test suite in place", {
    operation: "merge.update-suite",
    context: { suite: suite.name, arm: suite.state.case ?? "unset" },
  });
  paintSuite(el, suite);
}

/**
 * Paint EL, a suite's element, as SUITE. THE GLYPH AND THE OUTPUT KEEP THEIR
 * ELEMENTS: a glyph is repainted only where its state moved, so a running
 * suite's breathing dot is never restarted, and the output box keeps the
 * element the reader may have scrolled, its spans replaced only when they
 * changed.
 */
function paintSuite(el: HTMLElement, suite: FeedMergeTestSuite): void {
  const state = requireCase(suite.state, `${PATH}.state`);
  setAttribute(el, "data-suite-state", state.case);
  const glyph = el.querySelector<HTMLElement>(":scope > .merge-suite-head > .merge-suite-glyph");
  if (glyph === null) throw new Error("merge: a drawn suite lost its glyph");
  switch (state.case) {
    case "running": {
      // THE DOT SAYS WHAT THE TESTS HAVE SAID SO FAR (owner ruling,
      // 2026-10-08), never purple and never green (owner request, 2026-10-11):
      // yellow before any verdict and while every verdict is a pass, red once
      // one failed. The daemon resolved the arm.
      const soFar = requireCase(state.value.soFar, `${PATH}.state.running.so_far`);
      repaintMark(glyph, `merge-suite-glyph is-live is-${soFar.case}`, SUITE_GLYPHS.running);
      setAttribute(el, "data-so-far", soFar.case);
      break;
    }
    case "passed":
      repaintMark(glyph, "merge-suite-glyph is-succeeded", SUITE_GLYPHS.passed);
      el.removeAttribute("data-so-far");
      break;
    case "failed":
      repaintMark(glyph, "merge-suite-glyph is-failed", SUITE_GLYPHS.failed);
      el.removeAttribute("data-so-far");
      break;
    case "reused":
      repaintMark(glyph, "merge-suite-glyph is-succeeded", SUITE_GLYPHS.reused);
      el.removeAttribute("data-so-far");
      break;
    default:
      unreachableArm(`${PATH}.state`, armName(state));
  }
  placeReusedSource(glyph.parentElement as HTMLElement, state.case === "reused" ? state.value : undefined);

  placeFeedMergeTestCounts(glyph.parentElement as HTMLElement, suite.counts);

  let pre = el.querySelector<HTMLElement>(":scope > .merge-suite-output");
  if (suite.output.length === 0) {
    pre?.remove();
    return;
  }
  if (pre === null) {
    pre = document.createElement("pre");
    pre.className = "merge-suite-output";
    el.append(pre);
  }
  const spans = suite.output.map(drawFeedMergeTestSpan);
  const same =
    spans.length === pre.children.length && spans.every((span, i) => span.isEqualNode(pre.children[i] ?? null));
  if (!same) pre.replaceChildren(...spans);
}

/**
 * Bring HEAD, a suite's head line, to the earlier run a reused suite names:
 * drawn after the suite's name while the suite is reused, its text the
 * daemon's, and dropped otherwise.
 */
function placeReusedSource(head: HTMLElement, reused: FeedMergeTestSuiteReused | undefined): void {
  placeOptional(head, ":scope > .merge-suite-reused", reused === undefined ? null : drawReusedSource(reused), (el) => {
    const name = head.querySelector(":scope > .merge-suite-name");
    if (name === null) throw new Error("merge: a drawn suite lost its name");
    name.after(el);
  });
}

/** The earlier run a reused suite names, as the daemon resolved it. */
function drawReusedSource(reused: FeedMergeTestSuiteReused): HTMLElement {
  const source = requireMessage(reused.source, `${PATH}.state.reused.source`);
  const el = document.createElement("span");
  el.className = "merge-suite-reused";
  el.textContent = `reused: ${source.text}`;
  return el;
}

/** Set EL's NAME to VALUE only when it differs. */
function setAttribute(el: HTMLElement, name: string, value: string): void {
  if (el.getAttribute(name) !== value) el.setAttribute(name, value);
}

/**
 * The suite's counts on the right of its head line, "3/1/12": passed in green,
 * failed in red, the total in the row's own color. The red failed figure is
 * drawn only once a test has failed, so a suite with none reads "3/12". Drawn
 * only when the daemon knows them; the figures are the daemon's, never counted
 * here.
 */
export function drawFeedMergeTestCounts(counts: FeedMergeTestCounts): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-suite-counts";
  const passed = document.createElement("span");
  passed.className = "is-passed";
  passed.textContent = String(counts.passed);
  el.append(passed, "/");
  if (counts.failed > 0) {
    const failed = document.createElement("span");
    failed.className = "is-failed";
    failed.textContent = String(counts.failed);
    el.append(failed, "/");
  }
  el.append(String(counts.total));
  return el;
}

/**
 * Bring HOST's counts (`drawFeedMergeTestCounts`, its last child) to COUNTS:
 * drawn once known, redrawn only when a figure moved, and dropped while
 * nothing is known. The one placement of counts, for a suite's head line and
 * a run's tally alike.
 */
export function placeFeedMergeTestCounts(host: HTMLElement, counts: FeedMergeTestCounts | undefined): void {
  placeOptional(host, ":scope > .merge-suite-counts", counts === undefined ? null : drawFeedMergeTestCounts(counts), (el) =>
    host.append(el),
  );
}

/**
 * Every counted suite's counts summed, figure by figure; undefined while no
 * suite carries any. The sum is of the daemon's figures, never a count of
 * the suites themselves.
 */
export function sumFeedMergeTestCounts(suites: readonly FeedMergeTestSuite[]): FeedMergeTestCounts | undefined {
  const counted = suites.flatMap((suite) => (suite.counts === undefined ? [] : [suite.counts]));
  if (counted.length === 0) return undefined;
  return create(FeedMergeTestCountsSchema, {
    passed: counted.reduce((sum, c) => sum + c.passed, 0),
    failed: counted.reduce((sum, c) => sum + c.failed, 0),
    total: counted.reduce((sum, c) => sum + c.total, 0),
  });
}

/**
 * A run's tally: every suite's counts summed (`sumFeedMergeTestCounts`), drawn
 * as a suite's own counts are, so its figures read in the rows' colors. The
 * run panel hands it to the run strip, which draws it at the strip's right
 * end; it is empty while no suite carries counts.
 */
export function drawFeedMergeTestsRunTally(suites: readonly FeedMergeTestSuite[]): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-run-tally";
  updateFeedMergeTestsRunTally(el, suites);
  return el;
}

/** Bring EL, a tally as `drawFeedMergeTestsRunTally` drew it, to SUITES in place. */
export function updateFeedMergeTestsRunTally(el: HTMLElement, suites: readonly FeedMergeTestSuite[]): void {
  placeFeedMergeTestCounts(el, sumFeedMergeTestCounts(suites));
}

/** One painted span of a suite's output. */
export function drawFeedMergeTestSpan(span: FeedMergeTestSpan): HTMLElement {
  const el = document.createElement("span");
  el.className = paintSpanClass(span.paintClass);
  el.textContent = span.text;
  return el;
}
