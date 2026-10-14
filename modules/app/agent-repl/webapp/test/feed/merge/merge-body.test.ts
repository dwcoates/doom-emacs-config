// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedBreadcrumbSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { mergeBubbleBody } from "../../../src/feed/merge/merge-body.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { createTicker } from "../../../src/clock.js";
import { tick } from "../../../src/feed/ticking.js";
import { FakeSubfeed, childRow, id, tabRow } from "./fixtures.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { InterruptResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** Mount the body over VIEW. */
function mount(view: FakeSubfeed): { host: HTMLElement; dispose: () => void } {
  const h = harness();
  const host = document.createElement("div");
  const handle = mergeBubbleBody(host, view, rowContext(h.ctx, mergeRow("m1")));
  return { host, dispose: () => handle.dispose() };
}

/** The tabs the strip drew, by kind, in order. */
function stripKinds(host: HTMLElement): string[] {
  return [...host.querySelectorAll(".merge-tab")].map(
    (e) => e.getAttribute("data-merge-tab") ?? "",
  );
}

/** The tabs the bubble's OWN strip drew, by kind, in order. */
function topKinds(host: HTMLElement): string[] {
  return [...host.querySelectorAll(".merge-strip:not(.is-nested) > .merge-tab")].map(
    (e) => e.getAttribute("data-merge-tab") ?? "",
  );
}

/** The tabs every nested strip drew, by kind, in document order. */
function nestedKinds(host: HTMLElement): string[] {
  return [...host.querySelectorAll(".merge-strip.is-nested > .merge-tab")].map(
    (e) => e.getAttribute("data-merge-tab") ?? "",
  );
}

/** The kinds every strip marks active, outermost first. */
function activeKinds(host: HTMLElement): string[] {
  return [...host.querySelectorAll(".merge-tab.is-active")].map((e) => e.getAttribute("data-merge-tab") ?? "");
}

/** A tests tab holding one run whose output carries PAYLOAD. */
function testsWithRun(
  output: Record<string, unknown>,
  run: { state: "live" | "settled"; outcome?: "succeeded" | "failed" } = { state: "live" },
) {
  return [
    tabRow("t-tests", { kind: "tests", state: "live" }),
    tabRow("t-run", { kind: "testsRun", ...run, parent: "t-tests", payload: { output } }),
  ];
}

/** The view a run's panel shows. */
function runViewOf(host: HTMLElement): string | null {
  return host.querySelector(".merge-run")?.getAttribute("data-run-view") ?? null;
}

/** The run's badge in the tests tab's strip. */
function runBadge(host: HTMLElement): HTMLElement | null {
  return host.querySelector<HTMLElement>('.merge-strip.is-nested > [data-merge-tab="testsRun"]');
}

/** The kind of the tab the strip marks active. */
function activeKind(host: HTMLElement): string | null {
  return host.querySelector(".merge-tab.is-active")?.getAttribute("data-merge-tab") ?? null;
}

describe("the tab strip is the sub-feed's tab rows", () => {
  it("draws one tab per served tab row, in order, rebase first", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "rebasing", state: "settled", outcome: "succeeded" }),
      tabRow("t3", { kind: "tests", state: "settled", outcome: "succeeded" }),
      tabRow("t4", { kind: "committing", state: "live" }),
    ]);
    const { host } = mount(view);
    expect(stripKinds(host)).toEqual(["queue", "rebasing", "tests", "committing"]);
  });

  it("draws no tab for an ordinary row of the sub-feed", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "prePrompt", state: "live" }),
      childRow("r1", "t1"),
    ]);
    const { host } = mount(view);
    expect(stripKinds(host)).toEqual(["prePrompt"]);
  });

  it("decorates a second round on the strip", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "committing", state: "live", label: "committing", round: 2 }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-tab-label")?.textContent).toBe("committing (2)");
  });

  it("draws a subtab in its parent tab's strip, never in the bubble's own", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
    ]);
    const { host } = mount(view);
    expect([topKinds(host), nestedKinds(host)]).toEqual([["rebasing"], ["rebaseCommit"]]);
  });
});

describe("which tab is active", () => {
  it("selects the last live tab when the reader has chosen nothing", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ]);
    const { host } = mount(view);
    expect(activeKind(host)).toBe("tests");
  });

  it("selects the last tab when the whole run has settled", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "committing", state: "settled", outcome: "succeeded" }),
    ]);
    const { host } = mount(view);
    expect(activeKind(host)).toBe("committing");
  });

  it("gives the reader's click the tab they clicked", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    expect(activeKind(host)).toBe("queue");
  });

  it("KEEPS the reader's tab across a push that changed no tab's state", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "rebasing", state: "live", payload: { progress: { replayed: 0, total: 2 } } }),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    view.push([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "rebasing", state: "live", payload: { progress: { replayed: 1, total: 2 } } }),
    ]);
    expect(activeKind(host)).toBe("queue");
  });

  it("RELEASES the reader's tab when a tab changes state — the run moved on", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "rebasing", state: "live", startedAtMs: 2_000n }),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="rebasing"]')?.click();
    view.push([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "rebasing", state: "settled", outcome: "succeeded", startedAtMs: 2_000n }),
    ]);
    expect(activeKind(host)).toBe("queue");
  });

  it("RELEASES the reader's tab when a NEW tab arrives — the run moved on", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    view.push([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "tests", state: "settled", outcome: "succeeded" }),
      tabRow("t3", { kind: "committing", state: "live" }),
    ]);
    expect(activeKind(host)).toBe("committing");
  });

  it("follows the run down every strip to its live tab", () => {
    const view = new FakeSubfeed([
      ...testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" }),
      tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" }),
    ]);
    const { host } = mount(view);
    expect([activeKinds(host), runViewOf(host)]).toEqual([["tests", "testsRun"], "remediation"]);
  });

  it("keeps each strip's own pick: a commit picked in the rebasing tab survives the bubble's strip moving", () => {
    const rows = (queue: "live" | "settled") => [
      tabRow("t-queue", { kind: "queue", state: queue, outcome: "succeeded" }),
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "settled", outcome: "succeeded", parent: "t-rebase" }),
      tabRow("c2", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
    ];
    const view = new FakeSubfeed(rows("live"));
    const { host } = mount(view);
    host.querySelectorAll<HTMLElement>('.merge-strip.is-nested [data-merge-tab="rebaseCommit"]')[0]?.click();
    view.push(rows("settled"));
    const active = host.querySelector('.merge-strip.is-nested .merge-tab.is-active');
    expect(active).toBe(host.querySelectorAll('.merge-strip.is-nested [data-merge-tab="rebaseCommit"]')[0]);
  });
});

describe("a rebasing tab is a strip of its commits", () => {
  it("draws its own progress above the commits' strip", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live", payload: { progress: { replayed: 1, total: 2 } } }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
    ]);
    const { host } = mount(view);
    const progress = host.querySelector("[data-merge-progress]");
    const nested = host.querySelector(".merge-strip.is-nested");
    expect([progress?.textContent, progress !== null && nested !== null && Boolean(progress.compareDocumentPosition(nested) & Node.DOCUMENT_POSITION_FOLLOWING)]).toEqual(["1/2", true]);
  });

  it("selects the commit the replay is on, ahead of the pending ones", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase", label: "c1" }),
      tabRow("c2", { kind: "rebaseCommit", state: "pending", parent: "t-rebase", label: "c2" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-strip.is-nested .merge-tab.is-active .merge-tab-label")?.textContent).toBe("c1");
  });

  it("draws a pending commit with no glyph and no clock", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "pending", parent: "t-rebase" }),
    ]);
    const { host } = mount(view);
    const pending = host.querySelector('[data-merge-tab="rebaseCommit"]');
    expect([pending?.querySelector(".merge-tab-glyph")?.textContent, pending?.querySelector(".merge-tab-duration")?.textContent]).toEqual(["", ""]);
  });

  it("draws a commit's narration lines", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase", payload: { lines: [{ text: "replayed 1/2" }] } }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-rebase-commit .merge-line")?.textContent).toBe("replayed 1/2");
  });

  it("draws a conflicted commit's files over its resolution's rows", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", {
        kind: "rebaseCommit",
        state: "live",
        parent: "t-rebase",
        payload: { conflict: { state: { case: "live", value: { startedAtMs: 1n } }, files: [{ path: "a.go" }] } },
      }),
      childRow("r1", "c1"),
    ]);
    const { host } = mount(view);
    const section = host.querySelector(".merge-conflict");
    expect([section?.querySelector(".merge-conflict-file")?.textContent, section?.querySelector("[data-nest] [data-feed-row]")?.getAttribute("data-feed-row")]).toEqual(["a.go", "r1"]);
  });

  it("draws no conflict section on a commit that replayed cleanly", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-conflict")).toBeNull();
  });

  it("draws the conflict section once a push hands the commit to the session", () => {
    const commit = (conflict: boolean) =>
      tabRow("c1", {
        kind: "rebaseCommit",
        state: "live",
        parent: "t-rebase",
        payload: conflict ? { conflict: { state: { case: "live", value: { startedAtMs: 1n } }, files: [{ path: "a.go" }] } } : {},
      });
    const view = new FakeSubfeed([tabRow("t-rebase", { kind: "rebasing", state: "live" }), commit(false)]);
    const { host } = mount(view);
    view.push([tabRow("t-rebase", { kind: "rebasing", state: "live" }), commit(true), childRow("r1", "c1")]);
    expect(host.querySelector(".merge-conflict [data-feed-row]")?.getAttribute("data-feed-row")).toBe("r1");
  });
});

describe("a tests tab is a strip of its runs", () => {
  it("draws each run as a subtab", () => {
    const view = new FakeSubfeed([
      tabRow("t-tests", { kind: "tests", state: "live" }),
      tabRow("t-run1", { kind: "testsRun", state: "settled", outcome: "failed", parent: "t-tests", label: "run 1" }),
      tabRow("t-run2", { kind: "testsRun", state: "live", parent: "t-tests", label: "run 2" }),
    ]);
    const { host } = mount(view);
    const runs = [...host.querySelectorAll('.merge-strip.is-nested > [data-merge-tab="testsRun"] .merge-tab-label')];
    expect(runs.map((e) => e.textContent)).toEqual(["run 1", "run 2"]);
  });

  it("draws a run as one badge, never a strip of its own", () => {
    const view = new FakeSubfeed([
      ...testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" }),
      tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" }),
    ]);
    const { host } = mount(view);
    expect([stripKinds(host), host.querySelector('[data-merge-tab-body="testsRun"] .merge-strip')]).toEqual([["tests", "testsRun"], null]);
  });

  it("shows a run's tests while no fixing attempt follows it", () => {
    const view = new FakeSubfeed(testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" }));
    const { host } = mount(view);
    expect([runViewOf(host), host.querySelector<HTMLElement>(".merge-run-output")?.hidden]).toEqual(["tests", false]);
  });

  it("shows a run's remediation once a fixing attempt follows it", () => {
    const run = testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" });
    const view = new FakeSubfeed(run);
    const { host } = mount(view);
    view.push([...run, tabRow("t-fix", { kind: "testsRunFixing", state: "live", parent: "t-run" }), childRow("r1", "t-fix")]);
    expect([runViewOf(host), host.querySelector(".merge-run-remediation:not([hidden]) [data-feed-row]")?.getAttribute("data-feed-row")]).toEqual([
      "remediation",
      "r1",
    ]);
  });

  it("opens the view dropdown when the selected run's badge is clicked again", () => {
    const view = new FakeSubfeed(fixingRun({ kind: "testsRunFixing", state: "live" }));
    const { host } = mount(view);
    runBadge(host)?.click();
    const items = [...host.querySelectorAll<HTMLElement>(".merge-run-views:not([hidden]) .merge-run-view-item")];
    expect(items.map((e) => [e.textContent, e.getAttribute("aria-checked")])).toEqual([
      ["tests", "false"],
      ["remediation", "true"],
    ]);
  });

  it("switches a run to the view picked in its dropdown, and closes the dropdown", () => {
    const view = new FakeSubfeed(fixingRun({ kind: "testsRunFixing", state: "live" }));
    const { host } = mount(view);
    runBadge(host)?.click();
    host.querySelector<HTMLElement>('.merge-run-view-item[data-run-view="tests"]')?.click();
    expect([runViewOf(host), host.querySelector<HTMLElement>(".merge-run-views")?.hidden]).toEqual(["tests", true]);
  });

  it("keeps the reader's picked view across a push", () => {
    const rows = fixingRun({ kind: "testsRunFixing", state: "live" });
    const view = new FakeSubfeed(rows);
    const { host } = mount(view);
    runBadge(host)?.click();
    host.querySelector<HTMLElement>('.merge-run-view-item[data-run-view="tests"]')?.click();
    view.push([...rows, childRow("r1", "t-fix")]);
    expect(runViewOf(host)).toBe("tests");
  });

  it("offers no dropdown on a run with no remediation", () => {
    const view = new FakeSubfeed(testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" }));
    const { host } = mount(view);
    runBadge(host)?.click();
    expect(host.querySelector<HTMLElement>(".merge-run-views")?.hidden).toBe(true);
  });

  it("closes an open dropdown when the run's badge is clicked once more", () => {
    const view = new FakeSubfeed(fixingRun({ kind: "testsRunFixing", state: "live" }));
    const { host } = mount(view);
    document.body.append(host);
    runBadge(host)?.click();
    runBadge(host)?.click();
    expect(host.querySelector<HTMLElement>(".merge-run-views")?.hidden).toBe(true);
    host.remove();
  });

  it("closes an open dropdown on a click outside it", () => {
    const view = new FakeSubfeed(fixingRun({ kind: "testsRunFixing", state: "live" }));
    const { host } = mount(view);
    document.body.append(host);
    runBadge(host)?.click();
    document.body.click();
    expect(host.querySelector<HTMLElement>(".merge-run-views")?.hidden).toBe(true);
    host.remove();
  });

  it("draws no fixing sub-subtab for a run that passed", () => {
    const view = new FakeSubfeed(testsWithRun({ suites: [] }, { state: "settled", outcome: "succeeded" }));
    const { host } = mount(view);
    expect(host.querySelector('[data-merge-tab="testsRunFixing"]')).toBeNull();
  });
});

describe("resolved tabs draw the tab row's own content", () => {
  it("draws the queue snapshot on the queue tab", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "queue",
        state: "live",
        payload: {
          queue: {
            ahead: [],
            current: {
              workspace: { ref: { id: "mine", dir: "/w/mine" } },
              label: { text: "mine" },
              status: { case: "waiting", value: {} },
            },
            behind: [],
          },
        },
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-queue-label")?.textContent).toBe("mine");
  });

  it("draws the rebasing tab's progress as '3/7'", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "rebasing", state: "live", payload: { progress: { replayed: 3, total: 7 } } }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector("[data-merge-progress]")?.textContent).toBe("3/7");
  });

  it("draws the rebasing tab's narration lines verbatim, oldest first", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "rebasing",
        state: "live",
        payload: { lines: [{ text: "replaying 1/2 · tidy" }, { text: "replaying 2/2 · fix the loop" }] },
      }),
    ]);
    const { host } = mount(view);
    expect([...host.querySelectorAll(".merge-line")].map((e) => e.textContent)).toEqual([
      "replaying 1/2 · tidy",
      "replaying 2/2 · fix the loop",
    ]);
  });

  it("refuses a rebasing tab with no progress", () => {
    const row = tabRow("t1", { kind: "rebasing", state: "live" });
    (row.row.value as { kind: { value: { progress?: unknown } } }).kind.value.progress = undefined;
    expect(() => mount(new FakeSubfeed([row]))).toThrow(MalformedView);
  });

  it("draws the committing tab's merge commit subject verbatim", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "committing", state: "live", payload: { subject: { text: "Merge branch 'fix'" } } }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-commit-subject")?.textContent).toBe("Merge branch 'fix'");
  });

  it("refuses a committing tab with no subject", () => {
    const row = tabRow("t1", { kind: "committing", state: "live" });
    (row.row.value as { kind: { value: { subject?: unknown } } }).kind.value.subject = undefined;
    expect(() => mount(new FakeSubfeed([row]))).toThrow(MalformedView);
  });

  it("says the updating main tab is fetching", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "updatingMain", state: "live" })]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-updating-main")?.textContent).toBe("fetching");
  });

  it("says which commit the updating main tab is fast-forwarding to", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "updatingMain",
        state: "live",
        payload: { step: { step: { case: "fastForwarding", value: { commit: "4f2a1c" } } } },
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-updating-main")?.textContent).toBe("fast-forwarding to 4f2a1c");
  });

  it("refuses an updating main tab whose step names no arm", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "updatingMain", state: "live", payload: { step: {} } }),
    ]);
    expect(() => mount(view)).toThrow(MalformedView);
  });

  it("draws a run's log link with the daemon's label", () => {
    const view = new FakeSubfeed(
      testsWithRun({ suites: [], log: { token: { value: "tok-1" }, label: { text: "~/logs/ws-tests-1.log" } } }),
    );
    const { host } = mount(view);
    expect(host.querySelector(".merge-test-log [data-merge-test-log]")?.textContent).toBe("~/logs/ws-tests-1.log");
  });

  it("draws no log line on a run whose log is not written yet", () => {
    const view = new FakeSubfeed(testsWithRun({ suites: [] }));
    const { host } = mount(view);
    expect(host.querySelector(".merge-test-log")).toBeNull();
  });

  it("draws the suites on a run's output", () => {
    const view = new FakeSubfeed(
      testsWithRun({
        suites: [{ name: "unit", state: { case: "running", value: { soFar: { case: "unreported", value: {} } } }, output: [] }],
      }),
    );
    const { host } = mount(view);
    expect(host.querySelector(".merge-suite-name")?.textContent).toBe("unit");
  });
});

describe("agentic tabs draw the sub-feed rows parented to them", () => {
  it("draws only the rows naming THIS tab as their container", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "prePrompt", state: "live" }),
      tabRow("t2", { kind: "postPrompt", state: "live" }),
      childRow("r1", "t1"),
      childRow("r2", "t2"),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="prePrompt"]')?.click();
    expect(
      [...host.querySelectorAll("[data-nest] [data-feed-row]")].map((e) =>
        e.getAttribute("data-feed-row"),
      ),
    ).toEqual(["r1"]);
  });

  it("draws them through the ordinary row path, chrome included", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "prePrompt", state: "live" }),
      childRow("r1", "t1"),
    ]);
    mount(view);
    expect(view.drawn).toEqual(["r1"]);
  });

  it("draws them in feed order", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "postPrompt", state: "live" }),
      childRow("r1", "t1"),
      childRow("r2", "t1"),
    ]);
    const { host } = mount(view);
    expect(
      [...host.querySelectorAll("[data-nest] [data-feed-row]")].map((e) =>
        e.getAttribute("data-feed-row"),
      ),
    ).toEqual(["r1", "r2"]);
  });

  it("draws no row of a tab the reader is not on", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "prePrompt", state: "settled", outcome: "succeeded" }),
      childRow("r1", "t1"),
      tabRow("t2", { kind: "postPrompt", state: "live" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelectorAll("[data-nest] [data-feed-row]").length).toBe(0);
  });
});

/** A failed run with a fixing attempt FIXING under it, and the rows given. */
function fixingRun(fixing: Parameters<typeof tabRow>[1], ...rows: ReturnType<typeof childRow>[]) {
  return [
    ...testsWithRun({ suites: [] }, { state: "settled", outcome: "failed" }),
    tabRow("t-fix", { ...fixing, parent: "t-run" }),
    ...rows,
  ];
}

describe("a fixing attempt names its attempt above its rows", () => {
  it("draws the attempt as 'attempt 2/3'", () => {
    const view = new FakeSubfeed(
      fixingRun({ kind: "testsRunFixing", state: "live", payload: { attempt: { attempt: 2, maxAttempts: 3 } } }),
    );
    const { host } = mount(view);
    expect(host.querySelector("[data-merge-attempt]")?.textContent).toBe("attempt 2/3");
  });

  it("still draws the rows parented to it under the attempt", () => {
    const view = new FakeSubfeed(fixingRun({ kind: "testsRunFixing", state: "live" }, childRow("r1", "t-fix")));
    const { host } = mount(view);
    expect(host.querySelector(".merge-remediation-head ~ [data-nest] [data-feed-row]")?.getAttribute("data-feed-row")).toBe(
      "r1",
    );
  });

  it("refuses a fixing attempt with no attempt", () => {
    const rows = fixingRun({ kind: "testsRunFixing", state: "live" });
    const fixing = rows[rows.length - 1];
    if (fixing === undefined) throw new Error("fixture drew no fixing row");
    (fixing.row.value as { kind: { value: { attempt?: unknown } } }).kind.value.attempt = undefined;
    expect(() => mount(new FakeSubfeed(rows))).toThrow(MalformedView);
  });
});

describe("nothing parks, so no tab hosts a composer", () => {
  it("leaves the bubble's composer slot out of the merge body", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "prePrompt", state: "live" })]);
    view.composerSlot = document.createElement("div");
    view.composerSlot.className = "bubble-composer";
    const { host } = mount(view);
    expect(host.querySelector(".bubble-composer")).toBeNull();
  });
});

describe("a settled failure states its summary once, under the strip", () => {
  it("draws the daemon's summary for a settled-failed tab", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "tests",
        state: "settled",
        outcome: "failed",
        summary: "3 suites failed",
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-tab-failed")?.textContent).toBe("3 suites failed");
  });

  it("draws nothing there for a tab that succeeded", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "settled", outcome: "succeeded" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-tab-summary")?.hasAttribute("hidden")).toBe(true);
  });
});

describe("breadcrumbs draw only when non-empty (R6)", () => {
  it("draws no header line at all when the trail is empty", () => {
    const { host } = mount(new FakeSubfeed([tabRow("t1", { kind: "queue", state: "live" })]));
    expect(host.querySelector("[data-breadcrumbs]")).toBeNull();
  });

  it("draws the crumbs when the feed carries them", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "queue", state: "live" })]);
    mount(view);
    view.setBreadcrumbs([create(FeedBreadcrumbSchema, { label: "merge", target: id("m1") })]);
    expect(view.breadcrumbs().length).toBe(1);
  });
});

describe("the body cleans up after itself", () => {
  it("takes its own elements off the mount and stops listening", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "queue", state: "live" })]);
    const { host, dispose } = mount(view);
    dispose();
    expect(host.children.length).toBe(0);
  });
});

describe("drawLooseRows: a row that belongs to no drawn tab", () => {
  it("draws a row parented to an id no tab carries beneath the strip", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "live" }),
      childRow("r1", "gone", "orphan"),
    ]);
    const { host, dispose } = mount(view);

    const loose = host.querySelector(".merge-loose-rows");

    expect([...(loose?.children ?? [])].map((e) => e.getAttribute("data-feed-row"))).toEqual([
      "r1",
    ]);
    dispose();
  });

  it("stops the clocks of a loose row a later push drops", () => {
    // Arrange: a loose row holding a clock.
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "live" }),
      childRow("r1", "gone", "orphan"),
    ]);
    const { host, dispose } = mount(view);
    const loose = host.querySelector('.merge-loose-rows [data-feed-row="r1"]');
    if (loose === null) throw new Error("fixture drew no loose row");
    let ticks = 0;
    tick(loose, createTicker(1000), () => (ticks += 1));

    // Act: the row leaves the feed.
    view.push([tabRow("t1", { kind: "tests", state: "live" })]);
    vi.advanceTimersByTime(5000);

    // Assert: only the first paint ever ran.
    expect(ticks).toBe(1);
    dispose();
  });

  it("stops the clocks its parts hold when the body is disposed", () => {
    // Arrange.
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "live" }),
      childRow("r1", "gone", "orphan"),
    ]);
    const { host, dispose } = mount(view);
    const loose = host.querySelector('.merge-loose-rows [data-feed-row="r1"]');
    if (loose === null) throw new Error("fixture drew no loose row");
    let ticks = 0;
    tick(loose, createTicker(1000), () => (ticks += 1));

    // Act.
    dispose();
    vi.advanceTimersByTime(5000);

    // Assert.
    expect(ticks).toBe(1);
  });

  it("draws a subtab whose parent tab is not in the sub-feed beneath the strip", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "gone" }),
    ]);
    const { host, dispose } = mount(view);

    const loose = host.querySelector(".merge-loose-rows");

    expect([...(loose?.children ?? [])].map((e) => e.getAttribute("data-feed-row"))).toEqual(["c1"]);
    dispose();
  });

  it("leaves a row parented to a nested tab out of the loose host", () => {
    const view = new FakeSubfeed([
      tabRow("t-rebase", { kind: "rebasing", state: "live" }),
      tabRow("c1", { kind: "rebaseCommit", state: "live", parent: "t-rebase" }),
      childRow("r1", "c1", "placed"),
    ]);
    const { host, dispose } = mount(view);

    expect(host.querySelector(".merge-loose-rows")?.children.length).toBe(0);
    dispose();
  });

  it("leaves a row parented to a drawn tab out of the loose host", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "live" }),
      childRow("r1", "t1", "placed"),
    ]);
    const { host, dispose } = mount(view);

    const loose = host.querySelector(".merge-loose-rows");

    expect(loose?.children.length).toBe(0);
    dispose();
  });
});

// A MERGE BUBBLE'S UPDATE NEVER MOVES A SCROLL (owner ruling, 2026-10-08). The
// expanded bubble's sub-feed is a scroll box; a redraw that emptied the tab's
// panel and then read layout while drawing its rows would clamp that box's
// scroll to the emptied height, throwing the reader back up. So a redraw draws
// everything before it takes the previous content down.
describe("a redraw of the tab's content", () => {
  it("draws the rows while the tab's previous content still stands", () => {
    // Arrange
    const seen: number[] = [];
    let host: HTMLElement | null = null;
    class Watching extends FakeSubfeed {
      override drawRow(row: Parameters<FakeSubfeed["drawRow"]>[0]): HTMLElement {
        seen.push(host?.querySelector(".merge-tab-panel")?.childElementCount ?? -1);
        return super.drawRow(row);
      }
    }
    const view = new Watching([tabRow("t1", { kind: "prePrompt", state: "live" }), childRow("r1", "t1")]);
    host = mount(view).host;
    const standing = host.querySelector(".merge-tab-panel")?.childElementCount;
    seen.length = 0;
    // Act
    view.push([tabRow("t1", { kind: "prePrompt", state: "live" }), childRow("r1", "t1"), childRow("r2", "t1")]);
    // Assert
    expect(seen).toEqual([standing, standing]);
  });

});


/** A sub-feed whose ordinary row path reuses each row's element, as the real one does. */
class CachingSubfeed extends FakeSubfeed {
  private readonly elements = new Map<string, HTMLElement>();

  override drawRow(row: Parameters<FakeSubfeed["drawRow"]>[0]): HTMLElement {
    const value = row.id?.value ?? "unset";
    const held = this.elements.get(value);
    if (held !== undefined) return held;
    const el = super.drawRow(row);
    this.elements.set(value, el);
    return el;
  }
}

/** A tests tab whose one run carries one running suite at PASSED of 10. */
const testsTab = (passed: number) =>
  testsWithRun({
    suites: [
      { name: "unit", state: { case: "running", value: { soFar: { case: "passing", value: {} } } }, counts: { passed, failed: 0, total: 10 } },
    ],
  });

/**
 * A MERGE BUBBLE'S BODY IS DRAWN IN PLACE (owner request, 2026-10-10): the
 * daemon re-pushes a running tab on every beat of its work, and a body rebuilt
 * on each push restarted every live dot, which read as the dots blinking.
 */
describe("the body redraws in place", () => {
  it("keeps a tab's element and its live dot across a push that moved only the tab's content", () => {
    // Arrange
    const view = new FakeSubfeed(testsTab(1));
    const { host } = mount(view);
    const tab = host.querySelector(".merge-tab");
    const glyph = host.querySelector(".merge-tab-glyph");
    // Act
    view.push(testsTab(2));
    // Assert
    expect([host.querySelector(".merge-tab"), host.querySelector(".merge-tab-glyph")]).toEqual([tab, glyph]);
  });

  it("keeps the shown suite's element and its live dot across a push of new counts", () => {
    // Arrange
    const view = new FakeSubfeed(testsTab(1));
    const { host } = mount(view);
    const glyph = host.querySelector(".merge-suite-glyph");
    // Act
    view.push(testsTab(2));
    // Assert
    expect([host.querySelector(".merge-suite-glyph"), host.querySelector(".merge-suite-counts")?.textContent]).toEqual([
      glyph,
      "2/10",
    ]);
  });

  it("leaves a resolved tab's content standing when its row did not change", () => {
    // Arrange
    const rebasing = tabRow("t-rebase", { kind: "rebasing", state: "live" });
    const view = new FakeSubfeed([rebasing]);
    const { host } = mount(view);
    const content = host.querySelector(".merge-tab-panel")?.firstElementChild;
    // Act
    view.push([rebasing]);
    // Assert
    expect(host.querySelector(".merge-tab-panel")?.firstElementChild).toBe(content);
  });

  it("redraws a resolved tab's content when its row changed", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t-rebase", { kind: "rebasing", state: "live", payload: { progress: { replayed: 1, total: 7 } } })]);
    const { host } = mount(view);
    // Act
    view.push([tabRow("t-rebase", { kind: "rebasing", state: "live", payload: { progress: { replayed: 3, total: 7 } } })]);
    // Assert
    expect(host.querySelector(".merge-tab-panel")?.textContent).toContain("3/7");
  });

  it("keeps an agentic tab's rows container, and a row that stays is never moved", () => {
    // Arrange
    const view = new CachingSubfeed([tabRow("t1", { kind: "prePrompt", state: "live" }), childRow("r1", "t1")]);
    const { host } = mount(view);
    const container = host.querySelector(".merge-tab-rows");
    const row = host.querySelector('[data-feed-row="r1"]');
    // Act
    view.push([tabRow("t1", { kind: "prePrompt", state: "live" }), childRow("r1", "t1"), childRow("r2", "t1")]);
    // Assert
    expect([host.querySelector(".merge-tab-rows"), host.querySelector('[data-feed-row="r1"]'), host.querySelectorAll(".merge-tab-rows > *").length]).toEqual([
      container,
      row,
      2,
    ]);
  });

  it("draws the newly shown tab's content afresh when the shown tab changes", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t-rebase", { kind: "rebasing", state: "live" })]);
    const { host } = mount(view);
    // Act
    view.push([tabRow("t-rebase", { kind: "rebasing", state: "settled" }), ...testsTab(1)]);
    // Assert
    expect(host.querySelector(".merge-tab-panel")?.getAttribute("data-merge-tab-body")).toBe("tests");
  });

  it("stops the clock of a tab no longer served", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t-rebase", { kind: "rebasing", state: "live" }), ...testsTab(1)]);
    const { host } = mount(view);
    // Act
    view.push(testsTab(1));
    // Assert
    expect(topKinds(host)).toEqual(["tests"]);
  });
});

/** Mount the body over VIEW with harness H, answering both. */
function mountWith(view: FakeSubfeed, h = harness()): { host: HTMLElement; h: ReturnType<typeof harness> } {
  const host = document.createElement("div");
  mergeBubbleBody(host, view, rowContext(h.ctx, mergeRow("m1")));
  return { host, h };
}

/** Let a scripted RPC answer land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("a pick tab", () => {
  it("draws its rows like any agentic tab", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "resolving", state: "live" }), childRow("r1", "t1")]);
    const { host } = mountWith(view);
    expect(host.querySelector(".merge-tab-panel .merge-tab-rows [data-feed-row]")?.getAttribute("data-feed-row")).toBe("r1");
  });
});

/** A rebasing tab whose one commit has its conflict section in STATE. */
function conflictedCommit(state: "live" | "waitingOnUser" | "settled") {
  const conflictState =
    state === "settled"
      ? { case: "settled", value: { startedAtMs: 1n, endedAtMs: 2n, outcome: { case: "succeeded", value: {} } } }
      : { case: state, value: { startedAtMs: 1n } };
  return [
    tabRow("t-rebase", { kind: "rebasing", state: "live" }),
    tabRow("c1", {
      kind: "rebaseCommit",
      state: "live",
      parent: "t-rebase",
      payload: { conflict: { state: conflictState, files: [{ path: "a.go" }] } },
    }),
  ];
}

describe("an agent stream's stop control", () => {
  it.each([
    ["a live conflict section", () => conflictedCommit("live"), true],
    ["a conflict section waiting on the user", () => conflictedCommit("waitingOnUser"), true],
    ["a settled conflict section", () => conflictedCommit("settled"), false],
    ["a live resolving tab", () => [tabRow("t1", { kind: "resolving", state: "live" })], true],
    ["a settled resolving tab", () => [tabRow("t1", { kind: "resolving", state: "settled" })], false],
    ["a live fixing attempt", () => fixingRun({ kind: "testsRunFixing", state: "live" }), true],
    ["a live rebasing tab with no conflict", () => [tabRow("t-rebase", { kind: "rebasing", state: "live" })], false],
  ] as const)("is offered on %s: %s", (_name, rows, offered) => {
    // Arrange
    const view = new FakeSubfeed(rows());
    // Act
    const { host } = mountWith(view);
    // Assert
    expect(host.querySelector(".merge-tab-panel .merge-tab-stop [data-interrupt]") !== null).toBe(offered);
  });

  it("stops a conflict by its commit's row id, sent as a merge tab", async () => {
    // Arrange
    const { host, h } = mountWith(new FakeSubfeed(conflictedCommit("live")));
    // Act
    host.querySelector<HTMLElement>(".merge-conflict .merge-tab-stop [data-interrupt]")?.click();
    await settle();
    // Assert
    expect(h.calls.interrupt[0]?.target).toEqual({
      case: "mergeTab",
      value: { $typeName: "frontend.v1.FeedId", value: "c1" },
    });
  });

  it("stops the tab by its own row id, sent as a merge tab", async () => {
    // Arrange
    const { host, h } = mountWith(new FakeSubfeed([tabRow("t1", { kind: "resolving", state: "live" })]));
    // Act
    host.querySelector<HTMLElement>(".merge-tab-stop [data-interrupt]")?.click();
    await settle();
    // Assert
    expect(h.calls.interrupt[0]?.target).toEqual({
      case: "mergeTab",
      value: { $typeName: "frontend.v1.FeedId", value: "t1" },
    });
  });

  it("draws the refusal at the control", async () => {
    // Arrange
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: { case: "error", value: { kind: { case: "notMergeTab", value: {} } } },
        }),
    });
    const { host } = mountWith(new FakeSubfeed([tabRow("t1", { kind: "resolving", state: "live" })]), h);
    // Act
    host.querySelector<HTMLElement>(".merge-tab-stop [data-interrupt]")?.click();
    await settle();
    // Assert
    expect(host.querySelector(".merge-tab-stop .refusal")?.getAttribute("data-arm")).toBe("notMergeTab");
  });

  it("is taken down when the tab settles", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t1", { kind: "resolving", state: "live" })]);
    const { host } = mountWith(view);
    // Act
    view.push([tabRow("t1", { kind: "resolving", state: "settled" })]);
    // Assert
    expect(host.querySelector(".merge-tab-stop")).toBeNull();
  });
});
