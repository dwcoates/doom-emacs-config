// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { childTabsOf, readMergeTab, type MergeTab } from "../../../src/feed/merge/tab-strip.js";
import {
  autoRunView,
  drawRemediationHeading,
  fixingOf,
  mergeTabPanel,
  swapHeading,
} from "../../../src/feed/merge/tab-panels.js";
import { createTicker } from "../../../src/clock.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { FakeSubfeed, childRow, tabRow } from "./fixtures.js";
import TAB_PANELS from "../../../src/feed/merge/tab-panels.ts?raw";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** The one tab ROW reads as. */
function tabOf(row: ReturnType<typeof tabRow>): MergeTab {
  if (row.row.case !== "mergeTab") throw new Error("fixture is not a tab");
  return readMergeTab(row, row.row.value);
}

/** The row context every panel here draws in. */
const rc = () => rowContext(harness().ctx, mergeRow("m1"));

/** A run's suites: a failed one, and a running one that has passed PASSED of 4. */
const RUN_SUITES = (passed: number) => [
  { name: "unit", state: { case: "failed", value: {} }, counts: { passed: 2, failed: 1, total: 3 } },
  {
    name: "e2e",
    state: { case: "running", value: { soFar: { case: "passing", value: {} } } },
    counts: { passed, failed: 0, total: 4 },
  },
];

const SWITCH = {
  workspaceSwitch: {
    label: { text: "in the merge workspace" },
    availability: { case: "available", value: { target: { id: "ws-merge", dir: "/w/merge" } } },
  },
};

describe("autoRunView", () => {
  it("shows a run's tests while no fixing attempt follows it", () => {
    expect(autoRunView(undefined)).toBe("tests");
  });

  it("shows a run's remediation once a fixing attempt follows it", () => {
    expect(autoRunView(tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" })))).toBe("remediation");
  });
});

describe("fixingOf", () => {
  const run = () => tabOf(tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" }));

  it("finds the fixing attempt parented to the run", () => {
    const view = new FakeSubfeed([tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" })]);
    expect(fixingOf(run(), view)?.id).toBe("t-fix");
  });

  it("finds none for a run nothing followed", () => {
    expect(fixingOf(run(), new FakeSubfeed([tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-other" })]))).toBeUndefined();
  });

  it("refuses a run followed by two fixing attempts", () => {
    const view = new FakeSubfeed([
      tabRow("t-fix1", { kind: "testsRunFixing", state: "settled", outcome: "failed", parent: "t-run" }),
      tabRow("t-fix2", { kind: "testsRunFixing", state: "live", parent: "t-run" }),
    ]);
    expect(() => fixingOf(run(), view)).toThrow("a run has at most one");
  });
});

describe("drawRemediationHeading", () => {
  it("draws the attempt, its clock and its state glyph on one line", () => {
    const fix = tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "live", payload: { attempt: { attempt: 2, maxAttempts: 3 } } }));
    const line = drawRemediationHeading(fix, createTicker(1000)).querySelector(".merge-remediation-line");
    expect([...(line?.children ?? [])].map((e) => e.className.split(" ")[0])).toEqual([
      "merge-fix-attempt",
      "merge-tab-duration",
      "merge-tab-glyph",
    ]);
  });

  it("states a failed attempt's own account under the line", () => {
    const fix = tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "settled", outcome: "failed" }));
    const summary = drawRemediationHeading(fix, createTicker(1000)).querySelector<HTMLElement>(".merge-tab-summary");
    expect([summary?.hidden, summary?.querySelector(".merge-tab-failed") !== null]).toEqual([false, true]);
  });

  it("states nothing under the line of an attempt still at work", () => {
    const fix = tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "live" }));
    expect(drawRemediationHeading(fix, createTicker(1000)).querySelector<HTMLElement>(".merge-tab-summary")?.hidden).toBe(true);
  });
});

describe("mergeTabPanel", () => {
  it("leaves a resolved tab standing while its row is unchanged", () => {
    const row = tabRow("t1", { kind: "committing", state: "live" });
    const panel = mergeTabPanel(tabOf(row), new FakeSubfeed([row]), rc());
    expect(panel.update(tabOf(row))).toBe(true);
  });

  it("asks for a resolved tab to be drawn afresh when its row changed", () => {
    const view = new FakeSubfeed();
    const panel = mergeTabPanel(tabOf(tabRow("t1", { kind: "committing", state: "live" })), view, rc());
    expect(panel.update(tabOf(tabRow("t1", { kind: "committing", state: "settled", outcome: "succeeded" })))).toBe(false);
  });

  it("asks for an agentic tab carrying a switch to be drawn afresh when its row changed", () => {
    const view = new FakeSubfeed();
    const panel = mergeTabPanel(tabOf(tabRow("t1", { kind: "prePrompt", state: "live", payload: SWITCH })), view, rc());
    expect(panel.update(tabOf(tabRow("t1", { kind: "prePrompt", state: "settled", outcome: "succeeded", payload: SWITCH })))).toBe(false);
  });

  it("places an agentic tab's new rows in the container it already has", () => {
    const tab = tabRow("t1", { kind: "prePrompt", state: "live" });
    const view = new FakeSubfeed([tab, childRow("r1", "t1")]);
    const panel = mergeTabPanel(tabOf(tab), view, rc());
    const container = panel.nodes.find((node) => node.classList.contains("merge-tab-rows"));
    view.push([tab, childRow("r1", "t1"), childRow("r2", "t1")]);
    panel.update(tabOf(tab));
    expect([...(container?.children ?? [])].map((e) => e.getAttribute("data-feed-row"))).toEqual(["r1", "r2"]);
  });

  it("asks for a commit to be drawn afresh when its conflict section appears", () => {
    const view = new FakeSubfeed();
    const panel = mergeTabPanel(tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live" })), view, rc());
    const handed = tabRow("c1", {
      kind: "rebaseCommit",
      state: "live",
      payload: { conflict: { state: { case: "live", value: { startedAtMs: 1n } }, files: [] } },
    });
    expect(panel.update(tabOf(handed))).toBe(false);
  });

  it("redraws a commit's lines in place when they grew", () => {
    const view = new FakeSubfeed();
    const panel = mergeTabPanel(
      tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ line: { case: "text", value: "a" } }] } })),
      view,
      rc(),
    );
    const host = document.createElement("div");
    host.append(...panel.nodes);
    panel.update(tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ line: { case: "text", value: "a" } }, { line: { case: "text", value: "b" } }] } })));
    expect([...host.querySelectorAll(".merge-line")].map((e) => e.textContent)).toEqual(["a", "b"]);
  });

  it("draws a parent tab's children as a nested strip", () => {
    const rows = [
      tabRow("t-tests", { kind: "tests", state: "live" }),
      tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests" }),
    ];
    const panel = mergeTabPanel(childTabsOf(rows, undefined)[0], new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect(host.querySelector('.merge-strip.is-nested [data-merge-tab="testsRun"]')).not.toBeNull();
  });

  it("draws each run's tally inside its own badge in the tests tab's run strip", () => {
    const rows = [
      tabRow("t-tests", { kind: "tests", state: "live" }),
      tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(1) } } }),
    ];
    const panel = mergeTabPanel(childTabsOf(rows, undefined)[0], new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    const strip = host.querySelector(".merge-strip.is-nested");
    expect([
      strip?.querySelector('[data-merge-tab="testsRun"] > .merge-run-tally')?.textContent,
      strip?.querySelector(":scope > .merge-run-tally"),
    ]).toEqual(["3/1/7", null]);
  });

  it("draws a run as its views, with no strip of its own", () => {
    const rows = [
      tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" }),
      tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" }),
    ];
    const panel = mergeTabPanel(tabOf(rows[0]), new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect([[...host.querySelectorAll(".merge-run-view")].map((e) => e.getAttribute("data-run-view")), host.querySelector(".merge-strip")]).toEqual([
      ["tests", "remediation"],
      null,
    ]);
  });

  it("offers a run's views on reselect only once it has a remediation", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" });
    const view = new FakeSubfeed([run]);
    const panel = mergeTabPanel(tabOf(run), view, rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    const badge = document.createElement("div");
    panel.reselect?.(badge);
    const closed = host.querySelector<HTMLElement>(".merge-run-views")?.hidden;
    view.push([run, tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" })]);
    panel.update(tabOf(run));
    panel.reselect?.(badge);
    expect([closed, host.querySelector<HTMLElement>(".merge-run-views")?.hidden]).toEqual([true, false]);
  });

  it("refuses a run whose fixing attempt disappeared", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" });
    const view = new FakeSubfeed([run, tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" })]);
    const panel = mergeTabPanel(tabOf(run), view, rc());
    view.push([run]);
    expect(() => panel.update(tabOf(run))).toThrow("disappeared");
  });

  it("draws a child tab a later push adds to a parent's strip", () => {
    const parent = tabRow("t-tests", { kind: "tests", state: "live" });
    const view = new FakeSubfeed([parent]);
    const panel = mergeTabPanel(tabOf(parent), view, rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    view.push([parent, tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests" })]);
    panel.update(tabOf(parent));
    expect(host.querySelector('[data-merge-tab="testsRun"]')).not.toBeNull();
  });
});

describe("swapHeading", () => {
  it("puts the next heading in the old one's place", () => {
    const host = document.createElement("div");
    const head = document.createElement("p");
    const next = document.createElement("p");
    host.append(head);
    expect([swapHeading(head, next), host.firstElementChild]).toEqual([next, next]);
  });

  it("keeps no heading on a panel that has none", () => {
    expect(swapHeading(null, document.createElement("p"))).toBeNull();
  });

  it("stops the old heading's clock", () => {
    const inner = createTicker(1000);
    let live = 0;
    const ticker = {
      now: () => inner.now(),
      subscribe(fn: (nowMs: number) => void): () => void {
        live += 1;
        const off = inner.subscribe(fn);
        return () => {
          live -= 1;
          off();
        };
      },
    };
    const head = drawRemediationHeading(tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "live" })), ticker);
    const subscribed = live;
    document.createElement("div").append(head);
    swapHeading(head, document.createElement("p"));
    expect([subscribed, live]).toEqual([1, 0]);
  });

  it("keeps the old heading when no next one is drawn", () => {
    const head = document.createElement("p");
    expect(swapHeading(head, null)).toBe(head);
  });

  it("is the one way a panel's heading is swapped", () => {
    expect([TAB_PANELS.split("swapHeading(").length - 1, TAB_PANELS.split(".replaceWith(next)").length - 1]).toEqual([3, 1]);
  });
});
