/**
 * tests-tab — the merge run's test suites, and their COLORED output.
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
 * THE ROUND'S FULL LOG IS A LINK (owner, 2026-09-30), drawn under the suites
 * once the daemon has written it: the shared editor-link verb with the log's
 * token, in the response bubble's link blue (`renderMergeTestLogLink`).
 */
import { log } from "../../log.js";
import { requireCase } from "../../rpc/strict.js";
import { paintSpanClass } from "../cards/paint.js";
import { armName } from "../renderers.js";
import { unreachableArm } from "../../rpc/strict.js";
import type {
  FeedMergeTabTests,
  FeedMergeTestCounts,
  FeedMergeTestLog,
  FeedMergeTestSpan,
  FeedMergeTestSuite,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderMergeTestLogLink } from "../../link.js";
import type { AppContext } from "../../rpc/context.js";

const PATH = "FeedMergeTestSuite";

/** The glyph a suite reports its own state with. */
const SUITE_GLYPHS = {
  running: "●",
  passed: "✓",
  failed: "✗",
} as const satisfies Record<string, string>;

/** A tests tab: its suites, then its log link once the daemon has written it. */
export function drawFeedMergeTabTests(
  u: FeedMergeTabTests,
  ctx: AppContext,
  path: string,
): HTMLElement[] {
  const parts = [drawTestSuites(u.suites)];
  if (u.log !== undefined) parts.push(drawFeedMergeTestLog(u.log, ctx, `${path}.log`));
  return parts;
}

/** The log line: "log:" and the link, whose text is the daemon's label. */
export function drawFeedMergeTestLog(u: FeedMergeTestLog, ctx: AppContext, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-test-log";
  el.append(document.createTextNode("log: "), renderMergeTestLogLink(ctx, u, path));
  return el;
}

/** Every suite of a tests tab, in served order. */
export function drawTestSuites(suites: readonly FeedMergeTestSuite[]): HTMLElement {
  const el = document.createElement("div");
  el.className = "merge-suites list-rows";
  for (const suite of suites) el.append(drawFeedMergeTestSuite(suite));
  return el;
}

/** One suite: its name, its state glyph, its painted output. */
export function drawFeedMergeTestSuite(suite: FeedMergeTestSuite): HTMLElement {
  const state = requireCase(suite.state, `${PATH}.state`);
  log.debug(`drawing a merge test suite as ${state.case}`, {
    operation: "merge.draw-suite",
    context: { suite: suite.name, arm: state.case },
  });

  const el = document.createElement("div");
  el.className = "merge-suite";
  el.setAttribute("data-suite-state", state.case);

  const head = document.createElement("div");
  head.className = "merge-suite-head";
  el.append(head);

  const glyph = document.createElement("span");
  glyph.className = "merge-suite-glyph";
  glyph.setAttribute("aria-hidden", "true");
  switch (state.case) {
    case "running": {
      // THE DOT SAYS WHAT THE TESTS HAVE SAID SO FAR (owner ruling,
      // 2026-10-08), never purple: grey before any verdict, green while every
      // verdict is a pass, red once one failed. The daemon resolved the arm.
      const soFar = requireCase(state.value.soFar, `${PATH}.state.running.so_far`);
      glyph.classList.add("is-live", `is-${soFar.case}`);
      glyph.textContent = SUITE_GLYPHS.running;
      el.setAttribute("data-so-far", soFar.case);
      break;
    }
    case "passed":
      glyph.classList.add("is-succeeded");
      glyph.textContent = SUITE_GLYPHS.passed;
      break;
    case "failed":
      glyph.classList.add("is-failed");
      glyph.textContent = SUITE_GLYPHS.failed;
      break;
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
  head.append(glyph);

  const name = document.createElement("span");
  name.className = "merge-suite-name";
  name.textContent = suite.name;
  head.append(name);
  if (suite.counts !== undefined) head.append(drawFeedMergeTestCounts(suite.counts));

  if (suite.output.length > 0) {
    const pre = document.createElement("pre");
    pre.className = "merge-suite-output";
    for (const span of suite.output) pre.append(drawFeedMergeTestSpan(span));
    el.append(pre);
  }
  return el;
}

/**
 * The suite's counts on the right of its head line, "3/1/12": passed in green,
 * failed in red, the total in the row's own color. Drawn only when the daemon
 * knows them; the figures are the daemon's, never counted here.
 */
export function drawFeedMergeTestCounts(counts: FeedMergeTestCounts): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-suite-counts";
  const passed = document.createElement("span");
  passed.className = "is-passed";
  passed.textContent = String(counts.passed);
  const failed = document.createElement("span");
  failed.className = "is-failed";
  failed.textContent = String(counts.failed);
  el.append(passed, "/", failed, `/${String(counts.total)}`);
  return el;
}

/** One painted span of a suite's output. */
export function drawFeedMergeTestSpan(span: FeedMergeTestSpan): HTMLElement {
  const el = document.createElement("span");
  el.className = paintSpanClass(span.paintClass);
  el.textContent = span.text;
  return el;
}
