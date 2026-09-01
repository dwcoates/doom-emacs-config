// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedBreadcrumbSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { mergeBubbleBody } from "../../../src/feed/merge/merge-body.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { FakeSubfeed, childRow, id, tabRow } from "./fixtures.js";

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

/** The kind of the tab the strip marks active. */
function activeKind(host: HTMLElement): string | null {
  return host.querySelector(".merge-tab.is-active")?.getAttribute("data-merge-tab") ?? null;
}

describe("the tab strip is the sub-feed's tab rows", () => {
  it("draws one tab per served tab row, in order", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "merge", state: "live" }),
    ]);
    const { host } = mount(view);
    expect(stripKinds(host)).toEqual(["queue", "merge"]);
  });

  it("draws no tab for an ordinary row of the sub-feed", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "conflicts", state: "live" }),
      childRow("r1", "t1"),
    ]);
    const { host } = mount(view);
    expect(stripKinds(host)).toEqual(["conflicts"]);
  });

  it("decorates a second round on the strip", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "tests", state: "live", label: "tests", round: 2 }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-tab-label")?.textContent).toBe("tests (2)");
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
      tabRow("t2", { kind: "merge", state: "settled", outcome: "succeeded" }),
    ]);
    const { host } = mount(view);
    expect(activeKind(host)).toBe("merge");
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

  it("KEEPS the reader's tab across a push that only changed a tab's state", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "queue", state: "live" }),
      tabRow("t2", { kind: "tests", state: "live" }),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="queue"]')?.click();
    view.push([
      tabRow("t1", { kind: "queue", state: "settled", outcome: "succeeded" }),
      tabRow("t2", { kind: "tests", state: "live" }),
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
      tabRow("t2", { kind: "tests", state: "settled", outcome: "failed" }),
      tabRow("t3", { kind: "fixes", state: "live" }),
    ]);
    expect(activeKind(host)).toBe("fixes");
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

  it("draws the merge commit's narration lines verbatim", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "merge",
        state: "settled",
        outcome: "succeeded",
        payload: { lines: [{ text: "merged 4 commits · a1b2c3d" }] },
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-line")?.textContent).toBe("merged 4 commits · a1b2c3d");
  });

  it("draws the suites on the tests tab", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "tests",
        state: "live",
        payload: {
          suites: [{ name: "unit", state: { case: "running", value: {} }, output: [] }],
        },
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-suite-name")?.textContent).toBe("unit");
  });
});

describe("agentic tabs draw the sub-feed rows parented to them", () => {
  it("draws only the rows naming THIS tab as their container", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "conflicts", state: "live" }),
      tabRow("t2", { kind: "fixes", state: "live" }),
      childRow("r1", "t1"),
      childRow("r2", "t2"),
    ]);
    const { host } = mount(view);
    host.querySelector<HTMLElement>('[data-merge-tab="conflicts"]')?.click();
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
      tabRow("t1", { kind: "conflicts", state: "settled", outcome: "succeeded" }),
      childRow("r1", "t1"),
      tabRow("t2", { kind: "fixes", state: "live" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelectorAll("[data-nest] [data-feed-row]").length).toBe(0);
  });
});

describe("a parked tab is where the user types", () => {
  it("draws the daemon's standing line and a paused badge", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "conflicts", state: "parked", line: "2 conflicts remain" }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-parked-line")?.textContent).toBe("2 conflicts remain");
    expect(host.querySelector(".merge-parked-badge")?.textContent).toBe("paused");
  });

  it("places the bubble's composer slot INSIDE the parked tab", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "fixes", state: "parked" })]);
    view.composerSlot = document.createElement("div");
    view.composerSlot.className = "bubble-composer";
    const { host } = mount(view);
    expect(host.querySelector(".merge-parked > .bubble-composer")).not.toBeNull();
  });

  it("draws no parked header on a live tab", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "fixes", state: "live" })]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-parked")).toBeNull();
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
  it("hides the header line when the trail is empty", () => {
    const { host } = mount(new FakeSubfeed([tabRow("t1", { kind: "queue", state: "live" })]));
    expect(host.querySelector("[data-breadcrumbs]")?.hasAttribute("hidden")).toBe(true);
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
