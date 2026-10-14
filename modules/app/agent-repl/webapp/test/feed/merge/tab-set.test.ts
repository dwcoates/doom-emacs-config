// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../../src/clock.js";
import { childTabsOf, type MergeTab } from "../../../src/feed/merge/tab-strip.js";
import { createTabSet, drawSummary, tabStatesOf, type TabPanel } from "../../../src/feed/merge/tab-set.js";
import { tabRow } from "./fixtures.js";
import MERGE_BODY from "../../../src/feed/merge/merge-body.ts?raw";
import TAB_PANELS from "../../../src/feed/merge/tab-panels.ts?raw";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** A panel recording every update it was asked for, answering ACCEPT. */
function recordingPanel(tab: MergeTab, log: string[], accept = true): TabPanel {
  const el = document.createElement("div");
  el.setAttribute("data-panel", tab.id);
  log.push(`draw ${tab.id}`);
  return {
    nodes: [el],
    update(next: MergeTab): boolean {
      log.push(`update ${next.id}`);
      return accept;
    },
  };
}

/** A panel recording every reselect it was asked for, by the badge's tab. */
function reselectingPanel(tab: MergeTab, log: string[]): TabPanel {
  return {
    ...recordingPanel(tab, log),
    reselect(badge: HTMLElement): void {
      log.push(`reselect ${tab.id} ${badge.getAttribute("data-merge-tab") ?? ""}`);
    },
  };
}

/** A tab set mounted on a fresh host, with its panel log. */
function mounted(depth: "top" | "nested" = "top", accept = true) {
  const host = document.createElement("div");
  const log: string[] = [];
  const set = createTabSet(host, createTicker(1000), depth, (tab) => recordingPanel(tab, log, accept));
  return { host, log, set };
}

const tabs = (...rows: Parameters<typeof tabRow>[]) => childTabsOf(rows.map((r) => tabRow(...r)), undefined);

describe("createTabSet", () => {
  it("draws the shown panel's accessory at its strip's right end", () => {
    // Arrange
    const host = document.createElement("div");
    const accessory = document.createElement("span");
    const set = createTabSet(host, createTicker(1000), "nested", (tab) => ({ ...recordingPanel(tab, []), accessory }));
    // Act
    set.draw(tabs(["a", { kind: "committing", state: "live" }]));
    // Assert
    expect(host.querySelector(".merge-strip")?.lastElementChild).toBe(accessory);
  });

  it("drops the accessory when a panel without one is shown", () => {
    // Arrange
    const host = document.createElement("div");
    const accessory = document.createElement("span");
    accessory.className = "accessory";
    const set = createTabSet(host, createTicker(1000), "nested", (tab) =>
      tab.id === "b" ? { ...recordingPanel(tab, []), accessory } : recordingPanel(tab, []),
    );
    set.draw(tabs(["a", { kind: "queue", state: "settled", outcome: "succeeded" }], ["b", { kind: "committing", state: "live" }]));
    // Act
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    // Assert
    expect(host.querySelector(".merge-strip .accessory")).toBeNull();
  });

  it("hands a click on the shown tab's badge to its panel", () => {
    const host = document.createElement("div");
    const log: string[] = [];
    const set = createTabSet(host, createTicker(1000), "nested", (tab) => reselectingPanel(tab, log));
    set.draw(tabs(["a", { kind: "committing", state: "live" }]));
    host.querySelector<HTMLElement>(".merge-tab")?.click();
    expect(log).toEqual(["draw a", "reselect a committing"]);
  });

  it("selects a tab not shown on a click, without a reselect", () => {
    const host = document.createElement("div");
    const log: string[] = [];
    const set = createTabSet(host, createTicker(1000), "nested", (tab) => reselectingPanel(tab, log));
    set.draw(tabs(["a", { kind: "queue", state: "settled", outcome: "succeeded" }], ["b", { kind: "committing", state: "live" }]));
    host.querySelector<HTMLElement>('.merge-tab[data-merge-tab="queue"]')?.click();
    expect(log).toEqual(["draw b", "draw a"]);
  });

  it("mounts a strip, a summary line and a panel, in that order", () => {
    const { host } = mounted();
    expect([...host.children].map((c) => c.className)).toEqual(["merge-strip", "merge-tab-summary", "merge-tab-panel"]);
  });

  it("marks a nested strip so it does not stick over the bubble's own", () => {
    const { host } = mounted("nested");
    expect(host.querySelector(".merge-strip")?.classList.contains("is-nested")).toBe(true);
  });

  it("draws the auto-selected tab's panel", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "queue", state: "settled", outcome: "succeeded" }], ["b", { kind: "committing", state: "live" }]));
    expect(host.querySelector("[data-panel]")?.getAttribute("data-panel")).toBe("b");
  });

  it("updates the shown panel in place while the same tab stays shown", () => {
    const { log, set } = mounted();
    const served = tabs(["a", { kind: "committing", state: "live" }]);
    set.draw(served);
    set.draw(served);
    expect(log).toEqual(["draw a", "update a"]);
  });

  it("draws the panel afresh when it cannot follow the change", () => {
    const { log, set } = mounted("top", false);
    const served = tabs(["a", { kind: "committing", state: "live" }]);
    set.draw(served);
    set.draw(served);
    expect(log).toEqual(["draw a", "update a", "draw a"]);
  });

  it("gives a click the tab clicked, drawn at the tabs last served", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "queue", state: "settled", outcome: "succeeded" }], ["b", { kind: "committing", state: "live" }]));
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    expect(host.querySelector("[data-panel]")?.getAttribute("data-panel")).toBe("a");
  });

  it("releases the reader's pick when a new tab arrives", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "queue", state: "live" }], ["b", { kind: "committing", state: "live" }]));
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    set.draw(
      tabs(
        ["a", { kind: "queue", state: "settled", outcome: "succeeded" }],
        ["b", { kind: "committing", state: "settled", outcome: "succeeded" }],
        ["c", { kind: "postPrompt", state: "live" }],
      ),
    );
    expect(host.querySelector("[data-panel]")?.getAttribute("data-panel")).toBe("c");
  });

  it("releases the reader's pick when a tab settles", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "queue", state: "live", startedAtMs: 1_000n }], ["b", { kind: "rebasing", state: "live", startedAtMs: 2_000n }]));
    host.querySelector<HTMLElement>('[data-merge-tab="rebasing"]')?.click();
    set.draw(
      tabs(
        ["a", { kind: "queue", state: "live", startedAtMs: 1_000n }],
        ["b", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 2_000n, endedAtMs: 3_000n }],
      ),
    );
    expect(host.querySelector("[data-panel]")?.getAttribute("data-panel")).toBe("a");
  });

  it("keeps the reader's pick across a push that changes no tab's state", () => {
    const { host, set } = mounted();
    const served = tabs(["a", { kind: "queue", state: "live", startedAtMs: 1_000n }], ["b", { kind: "rebasing", state: "live", startedAtMs: 2_000n }]);
    set.draw(served);
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    set.draw(tabs(["a", { kind: "queue", state: "live", startedAtMs: 1_000n }], ["b", { kind: "rebasing", state: "live", startedAtMs: 2_000n, payload: { progress: { replayed: 1, total: 2 } } }]));
    expect(host.querySelector("[data-panel]")?.getAttribute("data-panel")).toBe("a");
  });

  it("empties the panel when no tab is served", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "committing", state: "live" }]));
    set.draw([]);
    expect(host.querySelector(".merge-tab-panel")?.children.length).toBe(0);
  });

  it("takes its parts off the host when disposed", () => {
    const { host, set } = mounted();
    set.draw(tabs(["a", { kind: "committing", state: "live" }]));
    set.dispose();
    expect(host.children.length).toBe(0);
  });
});

describe("tabStatesOf: a strip's tab states as one key", () => {
  it("is unchanged by a change of content", () => {
    const before = tabs(["a", { kind: "rebasing", state: "live", payload: { progress: { replayed: 0, total: 2 } } }]);
    const after = tabs(["a", { kind: "rebasing", state: "live", payload: { progress: { replayed: 1, total: 2 } } }]);
    expect(tabStatesOf(after)).toBe(tabStatesOf(before));
  });

  it("is unchanged by a change of instants", () => {
    const before = tabs(["a", { kind: "committing", state: "live", startedAtMs: 1_000n }]);
    const after = tabs(["a", { kind: "committing", state: "live", startedAtMs: 2_000n }]);
    expect(tabStatesOf(after)).toBe(tabStatesOf(before));
  });

  it("changes when a tab changes state", () => {
    const before = tabs(["a", { kind: "rebasing", state: "live" }]);
    const after = tabs(["a", { kind: "rebasing", state: "waitingOnUser" }]);
    expect(tabStatesOf(after)).not.toBe(tabStatesOf(before));
  });

  it("changes when a settled tab's outcome differs", () => {
    const before = tabs(["a", { kind: "committing", state: "settled", outcome: "succeeded" }]);
    const after = tabs(["a", { kind: "committing", state: "settled", outcome: "failed" }]);
    expect(tabStatesOf(after)).not.toBe(tabStatesOf(before));
  });

  it("changes when a tab appears", () => {
    const before = tabs(["a", { kind: "queue", state: "live" }]);
    const after = tabs(["a", { kind: "queue", state: "live" }], ["b", { kind: "rebasing", state: "live" }]);
    expect(tabStatesOf(after)).not.toBe(tabStatesOf(before));
  });
});

describe("drawSummary", () => {
  it("states a settled-failed tab's account", () => {
    const host = document.createElement("div");
    const [tab] = tabs(["a", { kind: "committing", state: "settled", outcome: "failed", summary: "the target moved" }]);
    drawSummary(host, tab);
    expect([host.hidden, host.textContent]).toEqual([false, "the target moved"]);
  });

  it("hides itself for a tab that did not fail", () => {
    const host = document.createElement("div");
    const [tab] = tabs(["a", { kind: "committing", state: "live" }]);
    drawSummary(host, tab);
    expect([host.hidden, host.textContent]).toEqual([true, ""]);
  });

  it("names the merge a run waits behind for its test turn", () => {
    const host = document.createElement("div");
    const [tab] = tabs(["r1", { kind: "testsRun", state: "waitingForTurn", holder: "beta" }]);
    drawSummary(host, tab);
    expect([host.hidden, host.textContent]).toEqual([false, "waiting for the test turn: beta's tests run first"]);
  });
});

// EVERY STRIP IS THE ONE TAB SET: the bubble's own and every nested one. A
// module that built a strip by hand would drift from the selection and the
// in-place drawing every other strip has.
describe("every merge strip is built by createTabSet", () => {
  it.each([
    ["merge-body.ts", MERGE_BODY],
    ["tab-panels.ts", TAB_PANELS],
  ])("%s builds its strip through createTabSet", (_name, source) => {
    expect([source.includes("createTabSet("), source.includes('"merge-strip')]).toEqual([true, false]);
  });
});
