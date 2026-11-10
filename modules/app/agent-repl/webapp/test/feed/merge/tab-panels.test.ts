// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { childTabsOf, drawTabDuration, readMergeTab, type MergeTab } from "../../../src/feed/merge/tab-strip.js";
import { mergeTabPanel, swapHeading } from "../../../src/feed/merge/tab-panels.js";
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

describe("mergeTabPanel", () => {
  it("draws a script hook's tab as its own row: the script, its tail and its exit", () => {
    const row = tabRow("t1", {
      kind: "hookScript",
      state: "settled",
      outcome: "failed",
      payload: {
        path: { text: "bin/deploy-landing.sh" },
        tail: [{ text: "installing" }],
        exit: { how: { case: "exited", value: { status: 2 } } },
      },
    });
    const panel = mergeTabPanel(tabOf(row), new FakeSubfeed([row]), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect(host.querySelector(".merge-hook-script-path")?.textContent).toBe("bin/deploy-landing.sh");
    expect(host.querySelector(".merge-hook-script-tail")?.textContent).toBe("installing");
    expect(host.querySelector(".merge-hook-script-exit")?.textContent).toBe("exited 2");
  });

  it("asks for a script hook's tab to be drawn afresh when its row changed", () => {
    const panel = mergeTabPanel(tabOf(tabRow("t1", { kind: "hookScript", state: "live" })), new FakeSubfeed(), rc());
    expect(panel.update(tabOf(tabRow("t1", { kind: "hookScript", state: "live", payload: { tail: [{ text: "more" }] } })))).toBe(false);
  });

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
    const panel = mergeTabPanel(tabOf(tabRow("t1", { kind: "hookPrompt", state: "live", payload: SWITCH })), view, rc());
    expect(panel.update(tabOf(tabRow("t1", { kind: "hookPrompt", state: "settled", outcome: "succeeded", payload: SWITCH })))).toBe(false);
  });

  it("places an agentic tab's new rows in the container it already has", () => {
    const tab = tabRow("t1", { kind: "hookPrompt", state: "live" });
    const view = new FakeSubfeed([tab, childRow("r1", "t1")]);
    const panel = mergeTabPanel(tabOf(tab), view, rc());
    const container = panel.nodes.find((node) => node.classList.contains("merge-tab-rows"));
    view.push([tab, childRow("r1", "t1"), childRow("r2", "t1")]);
    panel.update(tabOf(tab));
    expect([...(container?.children ?? [])].map((e) => e.getAttribute("data-feed-row"))).toEqual(["r1", "r2"]);
  });

  it("draws a commit as its narration alone, never an agent row parented to it", () => {
    const commit = tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ line: { case: "text", value: "a" } }] } });
    const panel = mergeTabPanel(tabOf(commit), new FakeSubfeed([commit, childRow("r1", "c1")]), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect([[...host.querySelectorAll(".merge-line")].map((e) => e.textContent), host.querySelector("[data-feed-row]")]).toEqual([["a"], null]);
  });

  it("asks for a commit to be drawn afresh when its lines grew", () => {
    const view = new FakeSubfeed();
    const panel = mergeTabPanel(
      tabOf(tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ line: { case: "text", value: "a" } }] } })),
      view,
      rc(),
    );
    const grown = tabRow("c1", { kind: "rebaseCommit", state: "live", payload: { lines: [{ line: { case: "text", value: "a" } }, { line: { case: "text", value: "b" } }] } });
    expect(panel.update(tabOf(grown))).toBe(false);
  });

  it("draws a conflict resolution as its conflicted files over its agent rows", () => {
    const resolution = tabRow("cr1", {
      kind: "conflictResolution",
      state: "live",
      parent: "t-conflicts",
      payload: { files: [{ path: "b.go" }, { path: "a.go" }] },
    });
    const panel = mergeTabPanel(tabOf(resolution), new FakeSubfeed([resolution, childRow("r1", "cr1")]), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect([
      [...host.querySelectorAll(".merge-conflict-files > .merge-conflict-file")].map((e) => e.textContent),
      [...host.querySelectorAll(".merge-tab-rows > [data-feed-row]")].map((e) => e.getAttribute("data-feed-row")),
      host.querySelector(".merge-tab-stop") !== null,
    ]).toEqual([["b.go", "a.go"], ["r1"], true]);
  });

  it("draws a fixing attempt as its attempt over its agent rows", () => {
    const fixing = tabRow("f1", {
      kind: "testsRunFixing",
      state: "settled",
      outcome: "succeeded",
      parent: "t-fixing",
      payload: { attempt: { attempt: 2, maxAttempts: 4 } },
    });
    const panel = mergeTabPanel(tabOf(fixing), new FakeSubfeed([fixing, childRow("r1", "f1")]), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect([
      host.querySelector(".merge-fix-attempt")?.textContent,
      [...host.querySelectorAll(".merge-tab-rows > [data-feed-row]")].map((e) => e.getAttribute("data-feed-row")),
    ]).toEqual(["attempt 2/4", ["r1"]]);
  });

  it.each([
    ["resolvingConflicts", "conflictResolution"],
    ["fixingTests", "testsRunFixing"],
  ] as const)("draws the %s tab's %s subtabs as a nested strip", (parent, child) => {
    const rows = [
      tabRow("t-parent", { kind: parent, state: "live" }),
      tabRow("t-child", { kind: child, state: "live", parent: "t-parent" }),
    ];
    const panel = mergeTabPanel(childTabsOf(rows, undefined)[0], new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect(host.querySelector(`.merge-strip.is-nested [data-merge-tab="${child}"]`)).not.toBeNull();
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

  it("draws a run as its suites alone: no strip, and no fixing attempt a stored row parents to it", () => {
    const rows = [
      tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests", payload: { output: { suites: RUN_SUITES(1) } } }),
      tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" }),
      childRow("r1", "t-fix"),
    ];
    const panel = mergeTabPanel(tabOf(rows[0]), new FakeSubfeed(rows), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    expect([
      host.querySelectorAll(".merge-run .merge-suite").length,
      host.querySelector(".merge-strip"),
      host.querySelector(".merge-fix-attempt"),
      host.querySelector("[data-feed-row]"),
    ]).toEqual([2, null, null, null]);
  });

  it("updates a run's suites in place", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(1) } } });
    const panel = mergeTabPanel(tabOf(run), new FakeSubfeed([run]), rc());
    const host = document.createElement("div");
    host.append(...panel.nodes);
    const drawn = host.querySelector(".merge-run");
    const next = tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(2) } } });
    expect([panel.update(tabOf(next)), host.querySelector(".merge-run") === drawn]).toEqual([true, true]);
  });

  it("offers nothing on a run's reselect: a run has one view", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests" });
    expect(mergeTabPanel(tabOf(run), new FakeSubfeed([run]), rc()).reselect).toBeUndefined();
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

describe("the queue tab's panel", () => {
  /** A queue tab whose snapshot's one entry is named NAME. */
  const queueTab = (name: string) =>
    tabOf(
      tabRow("q1", {
        kind: "queue",
        state: "live",
        payload: {
          queue: {
            ahead: [],
            current: {
              workspace: { ref: { id: name, dir: `/w/${name}` } },
              label: { text: name },
              target: { text: "doom", dir: "/src/doom" },
              status: { case: "waiting", value: { stageEnteredAtMs: 1_000n } },
            },
            behind: [],
          },
        },
      }),
    );

  /** The names PANEL's table draws now. */
  const namesIn = (panel: ReturnType<typeof mergeTabPanel>) =>
    [...panel.nodes[0].querySelectorAll(".merge-queue-label")].map((el) => el.textContent);

  it("draws the snapshot's table", () => {
    const panel = mergeTabPanel(queueTab("mine"), new FakeSubfeed(), rc());
    expect(namesIn(panel)).toEqual(["mine"]);
  });

  it("redraws a changed snapshot in place rather than asking to be drawn afresh", () => {
    const panel = mergeTabPanel(queueTab("mine"), new FakeSubfeed(), rc());
    const updated = panel.update(queueTab("theirs"));
    expect([updated, namesIn(panel)]).toEqual([true, ["theirs"]]);
  });

  it("keeps its table standing while the snapshot is unchanged", () => {
    const panel = mergeTabPanel(queueTab("mine"), new FakeSubfeed(), rc());
    const table = panel.nodes[0].querySelector(".merge-queue");
    panel.update(queueTab("mine"));
    expect(panel.nodes[0].querySelector(".merge-queue")).toBe(table);
  });
});

describe("a panel's opening shows its newest output", () => {
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** Mount NODES in a sub-feed bubble's scroll box, as the merge body does. */
  function inBubble(nodes: readonly HTMLElement[]): { subfeed: HTMLElement } {
    const bubble = document.createElement("div");
    bubble.className = "tool-card bubble-fold";
    const subfeed = document.createElement("div");
    subfeed.className = "agent-panel bubble-subfeed";
    subfeed.append(...nodes);
    bubble.append(subfeed);
    document.body.append(bubble);
    return { subfeed };
  }

  /** Make EL a laid-out box, scrolling 1000px within 300 when SCROLLS. */
  function layOut(el: HTMLElement, rect: { top: number; height: number }, scrolls: boolean): void {
    el.getClientRects = () => [rect] as unknown as DOMRectList;
    el.getBoundingClientRect = () => ({ ...rect, bottom: rect.top + rect.height }) as DOMRect;
    if (!scrolls) return;
    el.style.overflowY = "auto";
    Object.defineProperty(el, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(el, "clientHeight", { value: 300, configurable: true });
  }

  /** The drawn run panel's scroll box. */
  const runBox = (): HTMLElement => {
    const el = document.querySelector<HTMLElement>(".merge-run");
    if (el === null) throw new Error("no run");
    return el;
  };

  it("opens a run at the end of its tests", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(1) } } });
    const panel = mergeTabPanel(tabOf(run), new FakeSubfeed([run]), rc());
    inBubble(panel.nodes);
    layOut(runBox(), { top: 0, height: 300 }, true);
    panel.opened?.();
    expect(runBox().scrollTop).toBe(700);
  });

  it("leaves a run where the reader scrolled it across a push", () => {
    const run = tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(1) } } });
    const panel = mergeTabPanel(tabOf(run), new FakeSubfeed([run]), rc());
    inBubble(panel.nodes);
    layOut(runBox(), { top: 0, height: 300 }, true);
    panel.opened?.();
    runBox().scrollTop = 100;
    panel.update(tabOf(tabRow("t-run", { kind: "testsRun", state: "live", parent: "t-tests", payload: { output: { suites: RUN_SUITES(2) } } })));
    expect(runBox().scrollTop).toBe(100);
  });

  it("opens an agent stream with its last row at the bottom of the sub-feed", () => {
    const tab = tabRow("t1", { kind: "hookPrompt", state: "settled", outcome: "succeeded" });
    const panel = mergeTabPanel(tabOf(tab), new FakeSubfeed([tab, childRow("r1", "t1")]), rc());
    const { subfeed } = inBubble(panel.nodes);
    layOut(subfeed, { top: 0, height: 300 }, true);
    const rows = document.querySelector<HTMLElement>(".merge-tab-rows");
    if (rows === null) throw new Error("no stream");
    layOut(rows, { top: 100, height: 800 }, false);
    panel.opened?.();
    expect(subfeed.scrollTop).toBe(600);
  });

  it("opens a conflict resolution with its last row at the bottom of the sub-feed", () => {
    const resolution = tabRow("cr1", { kind: "conflictResolution", state: "live", payload: { files: [{ path: "a.go" }] } });
    const panel = mergeTabPanel(tabOf(resolution), new FakeSubfeed([resolution, childRow("r1", "cr1")]), rc());
    const { subfeed } = inBubble(panel.nodes);
    layOut(subfeed, { top: 0, height: 300 }, true);
    const rows = document.querySelector<HTMLElement>(".merge-tab-rows");
    if (rows === null) throw new Error("no resolution stream");
    layOut(rows, { top: 50, height: 450 }, false);
    panel.opened?.();
    expect(subfeed.scrollTop).toBe(200);
  });

  it("opens a commit without moving anything", () => {
    const commit = tabRow("c1", { kind: "rebaseCommit", state: "live" });
    const panel = mergeTabPanel(tabOf(commit), new FakeSubfeed([commit]), rc());
    const { subfeed } = inBubble(panel.nodes);
    layOut(subfeed, { top: 0, height: 300 }, true);
    panel.opened?.();
    expect(subfeed.scrollTop).toBe(0);
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
    const head = document.createElement("div");
    head.append(drawTabDuration(tabOf(tabRow("t-fix", { kind: "testsRunFixing", state: "live" })), ticker));
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
