// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { childTabsOf, readMergeTab, type MergeTab } from "../../../src/feed/merge/tab-strip.js";
import { RUN_OUTPUT_KIND, mergeTabPanel, runOutputEntry } from "../../../src/feed/merge/tab-panels.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { FakeSubfeed, childRow, tabRow } from "./fixtures.js";

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

const SWITCH = {
  workspaceSwitch: {
    label: { text: "in the merge workspace" },
    availability: { case: "available", value: { target: { id: "ws-merge", dir: "/w/merge" } } },
  },
};

describe("runOutputEntry", () => {
  const run = () => tabOf(tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" }));

  it("is labelled 'output' and drawn as the run's output kind", () => {
    const entry = runOutputEntry(run());
    expect([entry.label.text, entry.label.round, entry.kind]).toEqual(["output", 1, RUN_OUTPUT_KIND]);
  });

  it("wears the run's own state", () => {
    const entry = runOutputEntry(run());
    expect([entry.state, entry.outcome]).toEqual(["settled", "failed"]);
  });

  it("never shares the run row's id", () => {
    const tab = run();
    expect(runOutputEntry(tab).id).not.toBe(tab.id);
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
      tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ text: "a" }] } })),
      view,
      rc(),
    );
    const host = document.createElement("div");
    host.append(...panel.nodes);
    panel.update(tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ text: "a" }, { text: "b" }] } })));
    expect([...host.querySelectorAll(".merge-line")].map((e) => e.textContent)).toEqual(["a", "b"]);
  });

  it("draws a parent tab's children as a nested strip", () => {
    const rows = [
      tabRow("t-tests", { kind: "tests", state: "live" }),
      tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests" }),
    ];
    const panel = mergeTabPanel(childTabsOf(rows, undefined)[0] as MergeTab, new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect(host.querySelector('.merge-strip.is-nested [data-merge-tab="testsRun"]')).not.toBeNull();
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
