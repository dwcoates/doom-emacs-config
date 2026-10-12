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

  it("draws the tests tab's log link with the daemon's label", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "tests",
        state: "settled",
        outcome: "failed",
        payload: { log: { token: { value: "tok-1" }, label: { text: "~/logs/ws-tests-1.log" } } },
      }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-test-log [data-merge-test-log]")?.textContent).toBe("~/logs/ws-tests-1.log");
  });

  it("draws no log line on a tests tab whose log is not written yet", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "tests", state: "live" })]);
    const { host } = mount(view);
    expect(host.querySelector(".merge-test-log")).toBeNull();
  });

  it("draws the suites on the tests tab", () => {
    const view = new FakeSubfeed([
      tabRow("t1", {
        kind: "tests",
        state: "live",
        payload: {
          suites: [{ name: "unit", state: { case: "running", value: { soFar: { case: "unreported", value: {} } } }, output: [] }],
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

describe("a fixes tab names its attempt above its rows", () => {
  it("draws the attempt as 'attempt 2/3'", () => {
    const view = new FakeSubfeed([
      tabRow("t1", { kind: "fixes", state: "live", payload: { attempt: { attempt: 2, maxAttempts: 3 } } }),
    ]);
    const { host } = mount(view);
    expect(host.querySelector("[data-merge-attempt]")?.textContent).toBe("attempt 2/3");
  });

  it("still draws the rows parented to it under the attempt", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "fixes", state: "live" }), childRow("r1", "t1")]);
    const { host } = mount(view);
    expect(host.querySelector("[data-merge-attempt] ~ [data-nest] [data-feed-row]")?.getAttribute("data-feed-row")).toBe(
      "r1",
    );
  });

  it("refuses a fixes tab with no attempt", () => {
    const row = tabRow("t1", { kind: "fixes", state: "live" });
    (row.row.value as { kind: { value: { attempt?: unknown } } }).kind.value.attempt = undefined;
    expect(() => mount(new FakeSubfeed([row]))).toThrow(MalformedView);
  });
});

describe("nothing parks, so no tab hosts a composer", () => {
  it("leaves the bubble's composer slot out of the merge body", () => {
    const view = new FakeSubfeed([tabRow("t1", { kind: "conflicts", state: "live" })]);
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
    const view = new Watching([tabRow("t1", { kind: "conflicts", state: "live" }), childRow("r1", "t1")]);
    host = mount(view).host;
    const standing = host.querySelector(".merge-tab-panel")?.childElementCount;
    seen.length = 0;
    // Act
    view.push([tabRow("t1", { kind: "conflicts", state: "live" }), childRow("r1", "t1"), childRow("r2", "t1")]);
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

/** A tests tab row carrying one running suite at PASSED of 10. */
const testsTab = (passed: number) =>
  tabRow("t-tests", {
    kind: "tests",
    state: "live",
    payload: {
      suites: [
        { name: "unit", state: { case: "running", value: { soFar: { case: "passing", value: {} } } }, counts: { passed, failed: 0, total: 10 } },
      ],
    },
  });

/**
 * A MERGE BUBBLE'S BODY IS DRAWN IN PLACE (owner request, 2026-10-10): the
 * daemon re-pushes a running tab on every beat of its work, and a body rebuilt
 * on each push restarted every live dot, which read as the dots blinking.
 */
describe("the body redraws in place", () => {
  it("keeps a tab's element and its live dot across a push that moved only the tab's content", () => {
    // Arrange
    const view = new FakeSubfeed([testsTab(1)]);
    const { host } = mount(view);
    const tab = host.querySelector(".merge-tab");
    const glyph = host.querySelector(".merge-tab-glyph");
    // Act
    view.push([testsTab(2)]);
    // Assert
    expect([host.querySelector(".merge-tab"), host.querySelector(".merge-tab-glyph")]).toEqual([tab, glyph]);
  });

  it("keeps the shown suite's element and its live dot across a push of new counts", () => {
    // Arrange
    const view = new FakeSubfeed([testsTab(1)]);
    const { host } = mount(view);
    const glyph = host.querySelector(".merge-suite-glyph");
    // Act
    view.push([testsTab(2)]);
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
    const view = new CachingSubfeed([tabRow("t1", { kind: "conflicts", state: "live" }), childRow("r1", "t1")]);
    const { host } = mount(view);
    const container = host.querySelector(".merge-tab-rows");
    const row = host.querySelector('[data-feed-row="r1"]');
    // Act
    view.push([tabRow("t1", { kind: "conflicts", state: "live" }), childRow("r1", "t1"), childRow("r2", "t1")]);
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
    view.push([tabRow("t-rebase", { kind: "rebasing", state: "settled" }), testsTab(1)]);
    // Assert
    expect(host.querySelector(".merge-tab-panel")?.getAttribute("data-merge-tab-body")).toBe("tests");
  });

  it("stops the clock of a tab no longer served", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t-rebase", { kind: "rebasing", state: "live" }), testsTab(1)]);
    const { host } = mount(view);
    // Act
    view.push([testsTab(1)]);
    // Assert
    expect(host.querySelectorAll(".merge-tab")).toHaveLength(1);
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
    const view = new FakeSubfeed([tabRow("t1", { kind: "pick", state: "live" }), childRow("r1", "t1")]);
    const { host } = mountWith(view);
    expect(host.querySelector(".merge-tab-panel .merge-tab-rows [data-feed-row]")?.getAttribute("data-feed-row")).toBe("r1");
  });
});

describe("an agentic tab's stop control", () => {
  it.each([
    ["a live conflicts tab", "conflicts", "live", true],
    ["a conflicts tab waiting on the user", "conflicts", "waitingOnUser", true],
    ["a live pick tab", "pick", "live", true],
    ["a live fixes tab", "fixes", "live", true],
    ["a settled conflicts tab", "conflicts", "settled", false],
    ["a settled pick tab", "pick", "settled", false],
    ["a live rebasing tab", "rebasing", "live", false],
  ] as const)("is offered on %s: %s", (_name, kind, state, offered) => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t1", { kind, state })]);
    // Act
    const { host } = mountWith(view);
    // Assert
    expect(host.querySelector(".merge-tab-panel .merge-tab-stop [data-interrupt]") !== null).toBe(offered);
  });

  it("stops the tab by its own row id, sent as a merge tab", async () => {
    // Arrange
    const { host, h } = mountWith(new FakeSubfeed([tabRow("t1", { kind: "conflicts", state: "live" })]));
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
    const { host } = mountWith(new FakeSubfeed([tabRow("t1", { kind: "conflicts", state: "live" })]), h);
    // Act
    host.querySelector<HTMLElement>(".merge-tab-stop [data-interrupt]")?.click();
    await settle();
    // Assert
    expect(host.querySelector(".merge-tab-stop .refusal")?.getAttribute("data-arm")).toBe("notMergeTab");
  });

  it("is taken down when the tab settles", () => {
    // Arrange
    const view = new FakeSubfeed([tabRow("t1", { kind: "conflicts", state: "live" })]);
    const { host } = mountWith(view);
    // Act
    view.push([tabRow("t1", { kind: "conflicts", state: "settled" })]);
    // Assert
    expect(host.querySelector(".merge-tab-stop")).toBeNull();
  });
});
