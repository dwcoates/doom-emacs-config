// @vitest-environment jsdom
import { announceItemExpanded } from "../../src/expand.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedBreadcrumbSchema,
  FeedPageSchema,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  LoadFeedThroughResponseSchema,
  type LoadFeedThroughResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_load_feed_through_pb";
import { ConnectError, Code } from "@connectrpc/connect";
import { withOrder } from "../feed-order.js";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import type { WatchFeedResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  clearClientFailures,
  onClientVerdict,
  standingClientFailure,
} from "../../src/rpc/link.js";
import { latestEntry, mountFeed } from "../../src/feed/feed.js";
import {
  SELECTED_ENTRY_CLASS,
  SELECTED_ROW_ATTRIBUTE,
} from "../../src/feed/selected-entry.js";
import {
  Channel,
  feedId,
  harness,
  mergeRow,
  openSuccess,
  page,
  push,
  pushSelection,
  responseRow,
  stubRenderers,
  subagentRow,
  tokenFor,
  toolCallRow,
  userPromptRow,
  type Harness,
} from "./harness.js";
import { fireResize } from "../resize-observer.js";
import {
  fireIntersection,
  intersectionObservers,
} from "../intersection-observer.js";
import { OVERSCAN_CLASS } from "../../src/feed/overscan.js";
import { foldTitle } from "../../src/feed/title-fold.js";
import { HAS_MORE_CLASS } from "../../src/feed/bubble-more.js";
import { bubbleBox } from "../../src/feed/bubble-scroll.js";
import { createBubbleBody } from "../../src/bubble/body.js";
import { resetLoggingForTests } from "../../src/log.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 40; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Mount the feed into a fresh host inside a scroll box. */
function mount(h: Harness = harness()) {
  const scroll = document.createElement("div");
  const host = document.createElement("div");
  scroll.append(host);
  document.body.replaceChildren(scroll);
  const feed = mountFeed(host, h.ctx, {
    renderers: stubRenderers(),
    scrollBox: scroll,
  });
  return { feed, host, h };
}

/** A viewport rect at TOP, HEIGHT tall, since jsdom lays out nothing. */
const domRect = (top: number, height: number) => (): DOMRect => ({
  top,
  height,
  bottom: top + height,
  left: 0,
  right: 0,
  width: 0,
  x: 0,
  y: top,
  toJSON: () => ({}),
});

/**
 * Script the feed's scroll box as a 300px viewport at the page's top over
 * 2000px of content, the reader standing at scrollTop 100.
 */
function scriptFeedBox(scroll: HTMLElement): void {
  let top = 100;
  Object.defineProperties(scroll, {
    scrollHeight: { configurable: true, get: () => 2000 },
    clientHeight: { configurable: true, get: () => 300 },
    scrollTop: {
      configurable: true,
      get: () => top,
      set: (next: number) => {
        top = next;
      },
    },
  });
  scroll.getBoundingClientRect = domRect(0, 300);
}

describe("mountFeed: opening the root feed", () => {
  it("opens the ROOT feed, which is the absence of an address", async () => {
    const { h } = mount();
    await settle();
    expect(h.calls.openFeed[0]?.feed).toBeUndefined();
  });

  it("addresses the open to this page's workspace", async () => {
    const { h } = mount();
    await settle();
    expect(h.calls.openFeed[0]?.workspace?.id).toBe("ws-1");
  });

  it("paints the answered page", async () => {
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    const { host } = mount(h);
    await settle();
    expect(host.querySelector('[data-feed-row="p1"]')).not.toBeNull();
  });

  it("tails the token the open minted", async () => {
    const { h } = mount();
    await settle();
    expect(h.calls.watchFeed[0]?.watch?.value).toBe("tok:root");
  });

  it("upserts what the tail pushes", async () => {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({ channels });
    const { host } = mount(h);
    await settle();
    channel.push(push(responseRow("r1")));
    await settle();
    expect(host.querySelector('[data-feed-row="r1"]')).not.toBeNull();
  });

  it("routes a selection frame to the feed rather than reading it as a row", async () => {
    // Arrange — a live tail carrying a drawn response.
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({ channels });
    const { host } = mount(h);
    await settle();
    channel.push(push(responseRow("r1")));
    await settle();
    // Act — a selection frame naming that row (no row of its own).
    channel.push(pushSelection({ response: "r1" }));
    await settle();
    // Assert — the row wears the selection mark, so the frame reached
    // applySelection rather than being rejected as a malformed row.
    expect(
      host
        .querySelector('[data-feed-row="r1"]')
        ?.getAttribute(SELECTED_ROW_ATTRIBUTE),
    ).toBe("response");
  });

  it("clears the selection mark when a clear frame arrives", async () => {
    // Arrange — a row selected on the live tail.
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({ channels });
    const { host } = mount(h);
    await settle();
    channel.push(push(responseRow("r1")));
    channel.push(pushSelection({ response: "r1" }));
    await settle();
    // Act — the clear frame (double-escape).
    channel.push(pushSelection({ none: "returnToTail" }));
    await settle();
    // Assert
    expect(
      host
        .querySelector('[data-feed-row="r1"]')
        ?.hasAttribute(SELECTED_ROW_ATTRIBUTE),
    ).toBe(false);
  });

  it("draws nothing but stays alive when the daemon refuses the open", async () => {
    const h = harness({
      openFeed: () =>
        create(OpenFeedResponseSchema, {
          result: { case: "error", value: {} },
        }),
    });
    const { host } = mount(h);
    await settle();
    expect(host.querySelectorAll("[data-feed-row]")).toHaveLength(0);
  });

  it("opens no tail when the open was refused", async () => {
    const h = harness({
      openFeed: () =>
        create(OpenFeedResponseSchema, {
          result: { case: "error", value: {} },
        }),
    });
    const { h: used } = mount(h);
    await settle();
    expect(used.calls.watchFeed).toHaveLength(0);
  });
});

describe("mountFeed: re-opening the tail", () => {
  /**
   * A daemon whose root page is whatever `pages` yields next, tailed on a
   * channel the test can close to kill the stream.
   */
  function reopening(pages: FeedRow[][]) {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    let served = 0;
    const h = harness({
      channels,
      openFeed: (req) => {
        const rows = pages[Math.min(served, pages.length - 1)];
        served += 1;
        return openSuccess(page(rows), tokenFor(req));
      },
    });
    return { h, channel };
  }

  it("opens the feed again when the tail ends on its own", async () => {
    // Arrange
    const { h, channel } = reopening([[userPromptRow("p1", "cold")]]);
    mount(h);
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert: the token pins the tail to its page, so a reopen re-opens.
    expect(h.calls.openFeed.length).toBeGreaterThan(1);
  });

  it("paints the fresh page over the rows on a reopen", async () => {
    // Arrange
    const { h, channel } = reopening([
      [userPromptRow("p1", "cold")],
      [userPromptRow("p2", "fresh")],
    ]);
    const { host } = mount(h);
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="p2"]')).not.toBeNull();
  });

  it("drops a row the fresh page omits", async () => {
    // Arrange
    const { h, channel } = reopening([
      [userPromptRow("p1", "cold")],
      [userPromptRow("p2", "fresh")],
    ]);
    const { host } = mount(h);
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert: a page is a whole view, so the reopen replaces rather than adds.
    expect(host.querySelector('[data-feed-row="p1"]')).toBeNull();
  });

  it("tails the token the reopen minted", async () => {
    // Arrange
    const { h, channel } = reopening([[userPromptRow("p1", "cold")]]);
    mount(h);
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert
    expect(h.calls.watchFeed.at(-1)?.watch?.value).toBe("tok:root");
  });

  it("opens the feed once for the first attempt", async () => {
    // Arrange / Act
    const { h } = reopening([[userPromptRow("p1", "cold")]]);
    mount(h);
    await settle();
    // Assert: the open belongs to the stream, but only one attempt has run.
    expect(h.calls.openFeed).toHaveLength(1);
  });
});

describe("mountFeed: the bubble kinds", () => {
  /** A daemon whose ROOT page is ROWS and whose sub-feeds are empty. */
  function rootOnly(rows: FeedRow[]): Harness {
    return harness({
      openFeed: (req) =>
        openSuccess(page(req.feed === undefined ? rows : []), tokenFor(req)),
    });
  }

  it("gives a subagent bubble the subagent head", async () => {
    const h = rootOnly([subagentRow("b1")]);
    const { host } = mount(h);
    await settle();
    expect(host.querySelector(".subagent-head")).not.toBeNull();
  });

  it("gives a detached subagent the SAME head, the wrapper being placement only", async () => {
    const h = rootOnly([subagentRow("b1", { detached: true })]);
    const { host } = mount(h);
    await settle();
    expect(host.querySelector(".subagent-head")).not.toBeNull();
  });

  it("gives a merge bubble the merge head renderer", async () => {
    const h = rootOnly([mergeRow("m1")]);
    const { host } = mount(h);
    await settle();
    expect(host.querySelector(".stub-mergeHead")).not.toBeNull();
  });

  it("gives a merge bubble the merge BODY when it opens", async () => {
    const h = rootOnly([mergeRow("m1", false)]);
    const { host } = mount(h);
    await settle();
    expect(host.querySelector(".stub-mergeBody")).not.toBeNull();
  });

  it("opens an unfolded merge bubble's sub-feed by its own id", async () => {
    const h = rootOnly([mergeRow("m1", false)]);
    mount(h);
    await settle();
    expect(h.calls.openFeed.map((req) => req.feed?.value)).toEqual([
      undefined,
      "m1",
    ]);
  });
});

/**
 * A HELD PROMPT'S FIRST DRAW PARKS THE FEED (owner ruling, 2026-09-23): the
 * tray calls `promptHeld`, and the feed parks at its tail and follows under the
 * `promptHeld` cause, as a sent prompt does.
 */
describe("mountFeed: promptHeld", () => {
  afterEach(() => {
    resetLoggingForTests();
  });

  it("parks the feed at its tail", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    // Act
    feed.promptHeld("t1");
    // Assert
    expect(scroll.scrollTop).toBe(2000);
  });

  it("records the park under the promptHeld cause", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    scriptFeedBox(host.parentElement as HTMLElement);
    const capture = captureLogRecords("debug");
    // Act
    feed.promptHeld("t1");
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect((record.context as Record<string, unknown>).cause).toBe(
      "promptHeld",
    );
  });

  it("logs the held prompt it parked for", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    scriptFeedBox(host.parentElement as HTMLElement);
    const capture = captureLogRecords("debug");
    // Act
    feed.promptHeld("t1");
    // Assert
    const record = await forwardedRecord(capture, "feed.held-prompt-parked");
    expect(record.context).toMatchObject({ turn: "t1" });
  });

  it("logs, and moves nothing, when the feed has no scroll box", async () => {
    // Arrange
    const host = document.createElement("div");
    const feed = mountFeed(host, harness().ctx, { renderers: stubRenderers() });
    await settle();
    const capture = captureLogRecords("debug");
    // Act
    feed.promptHeld("t1");
    // Assert
    const record = await forwardedRecord(capture, "feed.held-prompt-unparked");
    expect(record.context).toMatchObject({ turn: "t1" });
  });
});

/**
 * A SWITCH TO THIS WORKSPACE PARKS THE FEED (owner ruling, 2026-10-02): the
 * rail sees the roster's `current` move to this page's workspace and calls
 * `workspaceSelected`; the feed parks at its tail and follows.
 */
describe("mountFeed: workspaceSelected", () => {
  afterEach(() => {
    resetLoggingForTests();
  });

  it("parks the feed at its tail", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    // Act
    feed.workspaceSelected();
    // Assert
    expect(scroll.scrollTop).toBe(2000);
  });

  it("records the park under the workspaceSelected cause", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    scriptFeedBox(host.parentElement as HTMLElement);
    const capture = captureLogRecords("debug");
    // Act
    feed.workspaceSelected();
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect((record.context as Record<string, unknown>).cause).toBe("workspaceSelected");
  });

  it("logs, and moves nothing, when the feed has no scroll box", async () => {
    // Arrange
    const host = document.createElement("div");
    const feed = mountFeed(host, harness().ctx, { renderers: stubRenderers() });
    await settle();
    const capture = captureLogRecords("debug");
    // Act
    feed.workspaceSelected();
    // Assert
    await expect(forwardedRecord(capture, "feed.workspace-selected-unparked")).resolves.toBeDefined();
  });
});

describe("mountFeed: selectDetachedWork", () => {
  /** A page whose rows are ROWS, for the root feed only. */
  function rootPage(rows: FeedRow[], crumbs: ReturnType<typeof crumb>[] = []) {
    return harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page(rows), tokenFor(req))
          : openSuccess(page([], { crumbs }), tokenFor(req)),
    });
  }

  function crumb(target: string, label: string) {
    return create(FeedBreadcrumbSchema, { target: feedId(target), label });
  }

  it("finds a row already drawn on the root feed", async () => {
    const { feed } = mount(rootPage([responseRow("r1")]));
    await settle();
    expect(await feed.selectDetachedWork(feedId("r1"))).toBe(true);
  });

  /** The card drawn in a row: the row's first element child. */
  function cardIn(
    host: HTMLElement,
    rowId: string,
  ): Element | null | undefined {
    return host.querySelector(`[data-feed-row="${rowId}"]`)?.firstElementChild;
  }

  it("marks the revealed row's card, so the reader's eye lands on it", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.selectDetachedWork(feedId("r1"));
    expect(cardIn(host, "r1")?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(
      true,
    );
  });

  it("leaves the revealed row's full-width wrapper unmarked", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.selectDetachedWork(feedId("r1"));
    expect(
      host
        .querySelector('[data-feed-row="r1"]')
        ?.classList.contains(SELECTED_ENTRY_CLASS),
    ).toBe(false);
  });

  it("clears the mark once the eye has had time to land", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.selectDetachedWork(feedId("r1"));
    await vi.advanceTimersByTimeAsync(3000);
    expect(cardIn(host, "r1")?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(
      false,
    );
  });

  /** The feed's scroll box, with rects scripted since jsdom lays out nothing. */
  function scripted(
    scroll: HTMLElement,
    host: HTMLElement,
    rowId: string,
    rowTop: number,
  ): void {
    scriptFeedBox(scroll);
    const row = host.querySelector<HTMLElement>(`[data-feed-row="${rowId}"]`);
    if (row === null) throw new Error(`row ${rowId} is not drawn`);
    row.getBoundingClientRect = domRect(rowTop, 100);
  }

  it("centers the selected detached-work card in the feed's viewport", async () => {
    // Arrange -- the card hangs 500..600 under a 300px viewport at 100.
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scripted(scroll, host, "r1", 500);
    // Act
    await feed.selectDetachedWork(feedId("r1"));
    // Assert -- its midpoint (550) onto the viewport's (150): 100 + 400.
    expect(scroll.scrollTop).toBe(500);
  });

  it("centers the feed on a breadcrumb's target, as every jump does", async () => {
    // Arrange -- owner request, 2026-10-01: a breadcrumb is a jump, and every
    // jump centers its entry. (It used to mark its target and leave the
    // scroll to the reader.) The root page here carries a crumb naming its
    // own row.
    const h = harness({
      openFeed: (req) =>
        openSuccess(
          page([responseRow("r1")], { crumbs: [crumb("r1", "here")] }),
          tokenFor(req),
        ),
    });
    const { host } = mount(h);
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scripted(scroll, host, "r1", 500);
    // Act
    host.querySelector<HTMLElement>(".feed-breadcrumb")?.click();
    await settle();
    // Assert -- the row is marked, and its midpoint (550) is on the viewport's
    // (150): 100 + 400.
    expect([
      cardIn(host, "r1")?.classList.contains(SELECTED_ENTRY_CLASS),
      scroll.scrollTop,
    ]).toEqual([true, 500]);
  });

  it("asks the daemon where an undrawn row lives", async () => {
    const h = rootPage([]);
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("deep"));
    await settle();
    expect(h.calls.openFeed.map((req) => req.feed?.value)).toContain("deep");
  });

  it("walks the breadcrumbs top-down, expanding each container", async () => {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const h = harness({
      channels,
      openFeed: (req) => {
        if (req.feed === undefined)
          return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1") {
          return openSuccess(page([responseRow("deep")]), tokenFor(req));
        }
        return openSuccess(
          page([], { crumbs: [crumb("b1", "Explore")] }),
          tokenFor(req),
        );
      },
    });
    const { feed } = mount(h);
    await settle();
    const revealed = await feed.selectDetachedWork(feedId("deep"));
    await settle();
    expect(revealed).toBe(true);
  });

  it("opens the containers selecting a nested head requires, then the head itself as the jump's entry", async () => {
    // Arrange: a subagent (inner) spawned by a subagent (b1); the probe of the
    // inner HEAD answers its own feed's crumbs, the head itself last.
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const h = harness({
      channels,
      openFeed: (req) => {
        if (req.feed === undefined)
          return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1")
          return openSuccess(page([subagentRow("inner")]), tokenFor(req));
        return openSuccess(
          page([], { crumbs: [crumb("b1", "lead"), crumb("inner", "worker")] }),
          tokenFor(req),
        );
      },
    });
    const { feed } = mount(h);
    await settle();

    // Act
    const revealed = await feed.selectDetachedWork(feedId("inner"));
    await settle();

    // Assert: the head was probed, b1 was opened to reach it, and the head
    // itself was opened last, by the landing (owner request, 2026-10-01: a
    // jump expands its entry). The walk never opened it.
    expect({
      revealed,
      opened: h.calls.openFeed.map((req) => req.feed?.value),
    }).toEqual({ revealed: true, opened: [undefined, "inner", "b1", "inner"] });
  });

  it("answers false when the target's feed cannot be opened (a shell bubble)", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : create(OpenFeedResponseSchema, {
              result: { case: "error", value: {} },
            }),
    });
    const { feed } = mount(h);
    await settle();
    expect(await feed.selectDetachedWork(feedId("shell"))).toBe(false);
  });

  it("answers false when a breadcrumb names a bubble this feed does not hold", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : openSuccess(
              page([], { crumbs: [crumb("ghost", "gone")] }),
              tokenFor(req),
            ),
    });
    const { feed } = mount(h);
    await settle();
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });

  it("answers false when the walk finished without the row appearing", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : openSuccess(page([]), tokenFor(req)),
    });
    const { feed } = mount(h);
    await settle();
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });
});

describe("mountFeed: selectDetachedWork walks older pages (LoadFeedThrough)", () => {
  const pageFrame = (rows: FeedRow[], hasMore = true): LoadFeedThroughResponse =>
    create(LoadFeedThroughResponseSchema, {
      frame: { case: "page", value: page(rows, { hasMore }) },
    });
  const reachedFrame = (target: string): LoadFeedThroughResponse =>
    create(LoadFeedThroughResponseSchema, {
      frame: { case: "reached", value: { target: feedId(target) } },
    });
  const errorFrame = (
    cause: "notFound" | "historyUnavailable",
  ): LoadFeedThroughResponse =>
    create(LoadFeedThroughResponseSchema, {
      frame: {
        case: "error",
        value: {
          cause:
            cause === "notFound"
              ? { case: "notFound", value: {} }
              : { case: "historyUnavailable", value: { detail: "store down" } },
        },
      },
    });

  /** A root feed holding ROWS with older pages remaining; every sub-feed probe refuses. */
  function olderHarness(
    frames: LoadFeedThroughResponse[],
    rows: FeedRow[] = [responseRow("newest")],
  ) {
    return harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page(rows, { hasMore: true }), tokenFor(req))
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
      loadFeedThrough: async function* () {
        yield* frames;
      },
    });
  }

  const rowOrder = (host: HTMLElement) =>
    [...host.querySelectorAll("[data-feed-row]")].map((el) => el.getAttribute("data-feed-row"));

  it("makes no LoadFeedThrough call for a row already drawn", async () => {
    const h = olderHarness([]);
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("newest"));
    expect(h.calls.loadFeedThrough).toHaveLength(0);
  });

  it("makes no LoadFeedThrough call when the root feed holds no older page", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([responseRow("newest")]), tokenFor(req))
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { feed } = mount(h);
    await settle();
    expect(await feed.selectDetachedWork(feedId("gone"))).toBe(false);
    expect(h.calls.loadFeedThrough).toHaveLength(0);
  });

  it("asks for the target by its own id, scoped to the workspace", async () => {
    const h = olderHarness([pageFrame([withOrder(responseRow("t"), "a")]), reachedFrame("t")]);
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("t"));
    expect(h.calls.loadFeedThrough.map((r) => [r.workspace?.id, r.target?.value])).toEqual([["ws-1", "t"]]);
  });

  it("prepends each streamed page in order, then lands on the target", async () => {
    const h = olderHarness([
      pageFrame([withOrder(responseRow("p1"), "b")]),
      pageFrame([withOrder(responseRow("t"), "a")], false),
      reachedFrame("t"),
    ]);
    const { feed, host } = mount(h);
    await settle();
    const reached = await feed.selectDetachedWork(feedId("t"));
    expect({ reached, order: rowOrder(host) }).toEqual({
      reached: true,
      order: ["t", "p1", "newest"],
    });
  });

  it("marks the landed target's card after the walk", async () => {
    const h = olderHarness([pageFrame([withOrder(responseRow("t"), "a")]), reachedFrame("t")]);
    const { feed, host } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("t"));
    expect(
      host.querySelector('[data-feed-row="t"]')?.firstElementChild?.classList.contains(SELECTED_ENTRY_CLASS),
    ).toBe(true);
  });

  it("keeps the pages and lands nothing when the target is not found", async () => {
    const h = olderHarness([pageFrame([withOrder(responseRow("p1"), "a")]), errorFrame("notFound")]);
    const { feed, host } = mount(h);
    await settle();
    const reached = await feed.selectDetachedWork(feedId("t"));
    expect({ reached, order: rowOrder(host), marked: host.querySelector(`.${SELECTED_ENTRY_CLASS}`) }).toEqual({
      reached: false,
      order: ["p1", "newest"],
      marked: null,
    });
  });

  it("does not invent a footer failure of its own when the target is not found", async () => {
    const h = olderHarness([errorFrame("notFound")]);
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("t"));
    expect(standingClientFailure()).toBeNull();
  });

  it("keeps the pages already streamed when history becomes unavailable mid-walk", async () => {
    const h = olderHarness([
      pageFrame([withOrder(responseRow("p1"), "a")]),
      errorFrame("historyUnavailable"),
    ]);
    const { feed, host } = mount(h);
    await settle();
    const reached = await feed.selectDetachedWork(feedId("t"));
    expect({ reached, order: rowOrder(host) }).toEqual({ reached: false, order: ["p1", "newest"] });
  });

  it("reports a transport failure mid-walk and keeps the applied pages", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([responseRow("newest")], { hasMore: true }), tokenFor(req))
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
      loadFeedThrough: async function* () {
        yield pageFrame([withOrder(responseRow("p1"), "a")]);
        throw new ConnectError("link dropped", Code.Unavailable);
      },
    });
    const { feed, host } = mount(h);
    await settle();
    const reached = await feed.selectDetachedWork(feedId("t"));
    expect({ reached, order: rowOrder(host), failure: standingClientFailure()?.kind }).toEqual({
      reached: false,
      order: ["p1", "newest"],
      failure: "unary_transport",
    });
  });

  it("refuses a stream that ends without a terminal frame", async () => {
    const h = olderHarness([pageFrame([withOrder(responseRow("p1"), "a")])]);
    const { feed } = mount(h);
    await settle();
    await expect(feed.selectDetachedWork(feedId("t"))).rejects.toThrow(/terminal/);
  });

  it("disables the older control for the whole walk and re-enables it after", async () => {
    const gate = new Channel<LoadFeedThroughResponse>();
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([responseRow("newest")], { hasMore: true }), tokenFor(req))
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
      loadFeedThrough: () => gate.iterate(),
    });
    const { feed, host } = mount(h);
    await settle();
    const control = () => host.querySelector<HTMLButtonElement>("[data-load-more]");
    // Act
    const walking = feed.selectDetachedWork(feedId("t"));
    await settle();
    const during = control()?.disabled;
    gate.push(pageFrame([withOrder(responseRow("t"), "a")]));
    await settle();
    const midWalk = control()?.disabled;
    gate.push(reachedFrame("t"));
    await walking;
    // Assert
    expect({ during, midWalk, after: control()?.disabled }).toEqual({
      during: true,
      midWalk: true,
      after: false,
    });
  });

  it("walks to a root container the crumbs name when the container's page is unloaded", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([responseRow("newest")], { hasMore: true }), tokenFor(req))
          : openSuccess(
              page([], { crumbs: [create(FeedBreadcrumbSchema, { target: feedId("sub"), label: "sub" })] }),
              tokenFor(req),
            ),
      loadFeedThrough: async function* () {
        yield reachedFrame("sub");
      },
    });
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("inner"));
    expect(h.calls.loadFeedThrough.map((r) => r.target?.value)).toEqual(["sub"]);
  });

  it("walks at most once per selection", async () => {
    const h = olderHarness([reachedFrame("t")]);
    const { feed } = mount(h);
    await settle();
    expect(await feed.selectDetachedWork(feedId("t"))).toBe(false);
    expect(h.calls.loadFeedThrough).toHaveLength(1);
  });
});

/**
 * EVERY JUMP EXPANDS ITS ENTRY, CENTERS IT, AND CLOSES IT AGAIN ONCE WHOLLY OUT
 * OF VIEW (owner request, 2026-10-01). Only what the jump expanded is closed;
 * the reader's own expansions close when the reader returns to the tail.
 */
describe("mountFeed: a jump expands its entry and closes it once wholly out of view", () => {
  /** A tool card that is its own fold, as the real renderer draws one. */
  const toolCard = (): HTMLElement => {
    const el = document.createElement("div");
    el.className = "tool-card tool-fold";
    el.textContent = "card";
    return el;
  };

  /**
   * A mounted root feed holding two tool cards (t1, t2) apart, a subagent
   * bubble (b1) and a last response (r1), in a 300px viewport over 2000px.
   * BUBBLE answers the bubble's own OpenFeed.
   */
  async function jumping(
    bubble: () => ReturnType<typeof openSuccess> = () =>
      openSuccess(page([]), "tok:b1"),
  ) {
    const rows = [
      toolCallRow("t1", "returned"),
      responseRow("r0"),
      toolCallRow("t2", "returned"),
      subagentRow("b1"),
      responseRow("r1"),
    ];
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page(rows), tokenFor(req))
          : bubble(),
    });
    let revealRow:
      ((id: ReturnType<typeof feedId>) => Promise<boolean>) | undefined;
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    scriptFeedBox(scroll);
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers({
        simpleToolCall: toolCard,
        response: (_unit, rc) => {
          revealRow = rc.revealRow;
          return document.createElement("div");
        },
      }),
      scrollBox: scroll,
    });
    await settle();
    const row = (id: string): HTMLElement => {
      const el = host.querySelector<HTMLElement>(`[data-feed-row="${id}"]`);
      if (el === null) throw new Error(`row ${id} is not drawn`);
      return el;
    };
    const card = (id: string): HTMLElement => {
      const el = row(id).querySelector<HTMLElement>(".tool-fold");
      if (el === null) throw new Error(`row ${id} holds no card`);
      return el;
    };
    const open = (el: HTMLElement): boolean =>
      el.classList.contains("expanded");
    const bubbleOpen = (): boolean =>
      row("b1").getAttribute("data-expanded") === "true";
    /** The rows the reader scrolls the entry ID into view and wholly out again. */
    const seenThenLeft = (id: string): void => {
      fireIntersection(row(id), true);
      fireIntersection(row(id), false);
    };
    const cardJump = (): ((
      id: ReturnType<typeof feedId>,
    ) => Promise<boolean>) => {
      if (revealRow === undefined)
        throw new Error("no card was handed the row context");
      return revealRow;
    };
    return {
      feed,
      host,
      scroll,
      h,
      row,
      card,
      open,
      bubbleOpen,
      seenThenLeft,
      cardJump,
    };
  }

  it("expands the tool card the footer's jump lands on", async () => {
    // Arrange
    const j = await jumping();
    // Act
    await j.feed.selectDetachedWork(feedId("t1"));
    // Assert
    expect(j.open(j.card("t1"))).toBe(true);
    j.feed.dispose();
  });

  it("expands the subagent bubble the footer's jump lands on", async () => {
    // Arrange
    const j = await jumping();
    // Act
    await j.feed.selectDetachedWork(feedId("b1"));
    await settle();
    // Assert
    expect(j.bubbleOpen()).toBe(true);
    j.feed.dispose();
  });

  it("expands the entry a card's jump (a breadcrumb, a hook's gated call) lands on", async () => {
    // Arrange
    const j = await jumping();
    // Act
    await j.cardJump()(feedId("t1"));
    // Assert
    expect(j.open(j.card("t1"))).toBe(true);
    j.feed.dispose();
  });

  it("centers the entry it expanded, reading the expanded layout", async () => {
    // Arrange -- the expanded card hangs 500..600 under a 300px viewport at 100.
    const j = await jumping();
    j.scroll.scrollTop = 100;
    j.row("t1").getBoundingClientRect = domRect(500, 100);
    // Act
    await j.feed.selectDetachedWork(feedId("t1"));
    // Assert -- its midpoint (550) onto the viewport's (150): 100 + 400.
    expect(j.scroll.scrollTop).toBe(500);
    j.feed.dispose();
  });

  it("centers a card's jump under entryJumped", async () => {
    // Arrange
    const j = await jumping();
    j.row("t1").getBoundingClientRect = domRect(500, 100);
    const capture = captureLogRecords("debug");
    // Act
    await j.cardJump()(feedId("t1"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect(record.context?.cause).toBe("entryJumped");
    j.feed.dispose();
  });

  it("keeps the jumped card open while any part of it is in view", async () => {
    // Arrange
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("t1"));
    // Act
    fireIntersection(j.row("t1"), true);
    // Assert
    expect(j.open(j.card("t1"))).toBe(true);
    j.feed.dispose();
  });

  it("closes the jumped card once it was seen and then left the view wholly", async () => {
    // Arrange
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("t1"));
    // Act
    j.seenThenLeft("t1");
    // Assert
    expect(j.open(j.card("t1"))).toBe(false);
    j.feed.dispose();
  });

  it("closes the jumped bubble once it was seen and then left the view wholly", async () => {
    // Arrange
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("b1"));
    await settle();
    // Act
    j.seenThenLeft("b1");
    // Assert
    expect(j.bubbleOpen()).toBe(false);
    j.feed.dispose();
  });

  it("closes the card a card's jump expanded once it left the view wholly", async () => {
    // Arrange
    const j = await jumping();
    await j.cardJump()(feedId("t1"));
    // Act
    j.seenThenLeft("t1");
    // Assert
    expect(j.open(j.card("t1"))).toBe(false);
    j.feed.dispose();
  });

  it("leaves a card the reader opened by hand open when a jump lands on it and it leaves the view", async () => {
    // Arrange
    const j = await jumping();
    j.card("t1").click();
    await j.feed.selectDetachedWork(feedId("t1"));
    // Act -- nothing watches it, so only a fire that can reach it is asserted on.
    const watched = intersectionObservers().some(
      (r) =>
        r.root === j.scroll &&
        r.targets.has(j.row("t1")) &&
        r.rootMargin === "0px 0px 0px 0px",
    );
    // Assert
    expect([j.open(j.card("t1")), watched]).toEqual([true, false]);
    j.feed.dispose();
  });

  it("leaves a jumped card the reader then toggled by hand as they left it", async () => {
    // Arrange -- the jump opens it; the reader closes and reopens it.
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("t1"));
    j.card("t1").click();
    j.card("t1").click();
    // Act
    const watched = intersectionObservers().some(
      (r) =>
        r.root === j.scroll &&
        r.targets.has(j.row("t1")) &&
        r.rootMargin === "0px 0px 0px 0px",
    );
    // Assert
    expect([j.open(j.card("t1")), watched]).toEqual([true, false]);
    j.feed.dispose();
  });

  it("keeps a first jump's watch when a second jump lands on another entry", async () => {
    // Arrange
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("t1"));
    await j.feed.selectDetachedWork(feedId("t2"));
    fireIntersection(j.row("t2"), true);
    // Act
    j.seenThenLeft("t1");
    // Assert
    expect([j.open(j.card("t1")), j.open(j.card("t2"))]).toEqual([false, true]);
    j.feed.dispose();
  });

  it("does not close a jumped card whose row was removed rather than scrolled away", async () => {
    // Arrange
    const j = await jumping();
    await j.feed.selectDetachedWork(feedId("t1"));
    const row = j.row("t1");
    const card = j.card("t1");
    fireIntersection(row, true);
    row.remove();
    // Act
    fireIntersection(row, false);
    // Assert
    expect(j.open(card)).toBe(true);
    j.feed.dispose();
  });

  it("files a bubble the daemon would not open for the jump on the warning chip", async () => {
    // Arrange
    const j = await jumping(() =>
      create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    );
    // Act
    await j.feed.selectDetachedWork(feedId("b1"));
    // Assert
    expect(j.h.sink.reported).toContain("controlPlaneFailed");
    j.feed.dispose();
  });

  it("logs a bubble the daemon would not open for the jump at ERROR", async () => {
    // Arrange
    const j = await jumping(() =>
      create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    );
    const capture = captureLogRecords();
    // Act
    await j.feed.selectDetachedWork(feedId("b1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-expand-failed");
    expect([record.level.case, record.context?.row]).toEqual(["error", "b1"]);
    j.feed.dispose();
  });

  it("still centers and marks the entry whose bubble would not open", async () => {
    // Arrange
    const j = await jumping(() =>
      create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    );
    // Act
    const landed = await j.feed.selectDetachedWork(feedId("b1"));
    // Assert
    expect(landed).toBe(true);
    j.feed.dispose();
  });

  it("closes a container the walk opened once it left the view wholly", async () => {
    // Arrange -- the target sits inside b1's sub-feed; the probe's crumbs name b1.
    const h = harness({
      openFeed: (req) => {
        if (req.feed === undefined)
          return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1")
          return openSuccess(page([responseRow("deep")]), tokenFor(req));
        return openSuccess(
          page([], {
            crumbs: [
              create(FeedBreadcrumbSchema, {
                target: feedId("b1"),
                label: "Explore",
              }),
            ],
          }),
          tokenFor(req),
        );
      },
    });
    const { feed, host } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("deep"));
    await settle();
    const b1 = host.querySelector<HTMLElement>(
      '[data-feed-row="b1"]',
    ) as HTMLElement;
    // Act
    fireIntersection(b1, true);
    fireIntersection(b1, false);
    // Assert
    expect(b1.getAttribute("data-expanded")).toBe("false");
    feed.dispose();
  });
});

/**
 * THE READER RETURNING TO THE TAIL CLOSES EVERY EXPANDED ENTRY (owner ruling,
 * 2026-10-01): the ones the reader opened by hand, which scrolling away from
 * never closes, and any a jump opened that are still open, each once.
 */
describe("mountFeed: returning to the tail closes every expanded entry", () => {
  /**
   * A mounted root feed (a tool card t1, a subagent bubble b1, a last response
   * r1) in a 300px viewport over 2000px, the reader wheeled up to 100 with the
   * last row out of view.
   */
  async function awayFromTail() {
    const rows = [
      toolCallRow("t1", "returned"),
      subagentRow("b1"),
      responseRow("r1"),
    ];
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page(rows), tokenFor(req))
          : openSuccess(page([]), tokenFor(req)),
    });
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    scriptFeedBox(scroll);
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers({
        simpleToolCall: () => {
          const el = document.createElement("div");
          el.className = "tool-card tool-fold";
          return el;
        },
      }),
      scrollBox: scroll,
    });
    await settle();
    scroll.dispatchEvent(new Event("wheel"));
    scroll.scrollTop = 100;
    scroll.dispatchEvent(new Event("scroll"));
    const row = (id: string): HTMLElement =>
      host.querySelector<HTMLElement>(`[data-feed-row="${id}"]`) as HTMLElement;
    const card = (): HTMLElement =>
      row("t1").querySelector<HTMLElement>(".tool-fold") as HTMLElement;
    /** The reader wheels back down until the last row shows. */
    const backToTail = (): void => {
      row("r1").getBoundingClientRect = domRect(250, 50);
      scroll.dispatchEvent(new Event("wheel"));
      scroll.scrollTop = 1700;
      scroll.dispatchEvent(new Event("scroll"));
    };
    return { feed, host, scroll, row, card, backToTail };
  }

  it("keeps a card the reader opened by hand open while they scroll away from it", async () => {
    // Arrange
    const t = await awayFromTail();
    t.card().click();
    // Act
    t.scroll.dispatchEvent(new Event("wheel"));
    t.scroll.scrollTop = 50;
    t.scroll.dispatchEvent(new Event("scroll"));
    // Assert
    expect(t.card().classList.contains("expanded")).toBe(true);
    t.feed.dispose();
  });

  it("closes a card the reader opened by hand once they return to the tail", async () => {
    // Arrange
    const t = await awayFromTail();
    t.card().click();
    // Act
    t.backToTail();
    // Assert
    expect(t.card().classList.contains("expanded")).toBe(false);
    t.feed.dispose();
  });

  it("closes a bubble the reader opened by hand once they return to the tail", async () => {
    // Arrange
    const t = await awayFromTail();
    t.row("b1").querySelector<HTMLElement>(".bubble-head")?.click();
    await settle();
    // Act
    t.backToTail();
    // Assert
    expect(t.row("b1").getAttribute("data-expanded")).toBe("false");
    t.feed.dispose();
  });

  it("closes an entry a jump opened once, dropping its watch, when the reader returns to the tail", async () => {
    // Arrange
    const t = await awayFromTail();
    await t.feed.selectDetachedWork(feedId("b1"));
    await settle();
    const capture = captureLogRecords();
    // Act
    t.backToTail();
    // Assert -- one collapse, and nothing watches the row to close it again.
    capture.logger.flush();
    await Promise.resolve();
    const collapses = capture.sent.filter(
      (r) => r.operation === "feed.bubble-collapse",
    ).length;
    const watched = intersectionObservers().some(
      (r) =>
        r.root === t.scroll &&
        r.targets.has(t.row("b1")) &&
        r.rootMargin === "0px 0px 0px 0px",
    );
    expect([collapses, watched]).toEqual([1, false]);
    t.feed.dispose();
  });

  it("records the return at INFO with what it closed", async () => {
    // Arrange
    const t = await awayFromTail();
    t.card().click();
    const capture = captureLogRecords();
    // Act
    t.backToTail();
    // Assert
    const record = await forwardedRecord(capture, "feed.tail-reached-collapse");
    expect([record.level.case, record.context]).toEqual([
      "info",
      expect.objectContaining({ sections: 1, bubbles: 0 }),
    ]);
    t.feed.dispose();
  });
});

describe("mountFeed: disposal", () => {
  it("empties its host", async () => {
    const { feed, host } = mount();
    await settle();
    feed.dispose();
    expect(host.children).toHaveLength(0);
  });

  it("stops upserting once disposed", async () => {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({ channels });
    const { feed, host } = mount(h);
    await settle();
    feed.dispose();
    channel.push(push(responseRow("late")));
    await settle();
    expect(host.querySelector('[data-feed-row="late"]')).toBeNull();
  });
});

describe("mountFeed: the scroll box", () => {
  it("mounts and paints a feed with no scroll box at all", async () => {
    // Arrange: a host with no parent and no scrollBox named — there is then no
    // tail to follow, and the feed must still draw.
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    const host = document.createElement("div");
    document.body.replaceChildren();
    // Act
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers() });
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="p1"]')).not.toBeNull();
    feed.dispose();
  });

  it("takes the host's own parent as the scroll box when none is named", async () => {
    // Arrange
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    // Act
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers() });
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="p1"]')).not.toBeNull();
    feed.dispose();
  });

  /**
   * Script the scroll box's geometry, which jsdom lays out not at all, and
   * clamp `scrollTop` into range on write the way a browser does.
   */
  function withGeometry(
    box: HTMLElement,
    init: { scrollHeight: number; clientHeight: number; scrollTop: number },
  ) {
    let clientHeight = init.clientHeight;
    let scrollTop = init.scrollTop;
    Object.defineProperties(box, {
      scrollHeight: { get: () => init.scrollHeight },
      clientHeight: { get: () => clientHeight },
      scrollTop: {
        get: () => scrollTop,
        set: (next: number) => {
          scrollTop = Math.max(
            0,
            Math.min(next, init.scrollHeight - clientHeight),
          );
        },
      },
    });
    return {
      loseHeight: (px: number) => {
        clientHeight -= px;
      },
      top: () => scrollTop,
    };
  }

  it("re-lands the tail when the docked footer takes height after the render", async () => {
    // Arrange — THE OCCLUSION. The mount must subscribe the scroll box's own
    // size, or a footer settling after the tail render leaves the last bubble
    // below the fold, clipped by the strip.
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const geometry = withGeometry(scroll, {
      scrollHeight: 1000,
      clientHeight: 300,
      scrollTop: 700,
    });
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    // Act — the footer appears and eats 48px of the scroll box.
    geometry.loseHeight(48);
    fireResize(scroll);
    // Assert
    expect(geometry.top()).toBe(748);
    feed.dispose();
  });

  it("stops observing the scroll box's size when the feed is disposed", async () => {
    // Arrange
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const h = harness();
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    // Act
    feed.dispose();
    // Assert — nothing watches it, so the next workspace mounts onto a clean
    // element rather than one a dead owner still re-parks.
    expect(() => fireResize(scroll)).toThrow(/no ResizeObserver/);
  });
});

describe("mountFeed: a bubble row re-pushed as another kind", () => {
  /** A root feed holding ROW, with a tail the test pushes on. */
  function withTail(row: FeedRow) {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({
      channels,
      openFeed: (req) =>
        openSuccess(page(req.feed === undefined ? [row] : []), tokenFor(req)),
    });
    return { h, channel };
  }

  const KINDS: ReadonlyArray<[string, FeedRow]> = [
    ["a sync subagent", subagentRow("b1")],
    ["a detached subagent", subagentRow("b1", { detached: true })],
    ["a merge", mergeRow("b1")],
  ];

  it.each(KINDS)(
    "files %s that stopped being one as an unreadable frame",
    async (_n, row) => {
      // Arrange
      const { h, channel } = withTail(row);
      mount(h);
      await settle();
      // Act: the same id comes back as an ordinary response.
      channel.push(push(responseRow("b1")));
      await settle();
      // Assert
      expect(h.sink.reported).toEqual(["frameUndecodable"]);
    },
  );

  // A PLACEMENT MOVE IS NOT A KIND CHANGE, and the two must not be confused.
  // The daemon announces a background spawn as a SYNCHRONOUS `subagent` unit
  // and re-pushes the SAME row as `detached_subagent` the moment the vendor
  // answers `async_launched` ("daemon.feed.detached_subagent -- a subagent
  // bubble moved to its detached placement"). The bubble is deliberately kept
  // across that push, so its HEAD has to be chosen from the row in hand rather
  // than from the arm the bubble was first mounted with -- which it was, and
  // every detached spawn was refused with `frameUndecodable` and froze on the
  // state it was announced with. Caught by the G51 playbook.
  it("redraws the head when a spawn moves to its detached placement", async () => {
    // Arrange: the announcement, as a synchronous subagent unit.
    const { h, channel } = withTail(subagentRow("b1"));
    const { host } = mount(h);
    await settle();
    // Act: the SAME row, re-pushed in its detached placement and settled.
    channel.push(
      push(
        subagentRow("b1", {
          detached: true,
          settled: { endedAtMs: 5_000n, outcome: "succeeded" },
        }),
      ),
    );
    await settle();
    // Assert: nothing was refused, and the head moved with the row.
    expect(h.sink.reported).toEqual([]);
    expect(
      host
        .querySelector('[data-feed-row="b1"] .subagent-head')
        ?.getAttribute("data-state"),
    ).toBe("succeeded");
  });

  it("keeps the bubble across a placement move rather than rebuilding it", async () => {
    // Arrange
    const { h, channel } = withTail(subagentRow("b1"));
    const { host } = mount(h);
    await settle();
    const before = host.querySelector('[data-feed-row="b1"] .bubble-fold');
    // Act
    channel.push(push(subagentRow("b1", { detached: true })));
    await settle();
    // Assert: the same element, so an open sub-feed survives the move.
    expect(host.querySelector('[data-feed-row="b1"] .bubble-fold')).toBe(
      before,
    );
  });

  it.each(KINDS)(
    "keeps %s's drawn head rather than tearing the feed down",
    async (_n, row) => {
      // Arrange
      const { h, channel } = withTail(row);
      const { host } = mount(h);
      await settle();
      // Act
      channel.push(push(responseRow("b1")));
      await settle();
      // Assert: the bubble the reader was looking at is still there.
      expect(
        host.querySelector('[data-feed-row="b1"] .bubble-fold'),
      ).not.toBeNull();
    },
  );
});

describe("mountFeed: selectDetachedWork's harder answers", () => {
  it("answers false when the probe never reached the daemon", async () => {
    // Arrange: the root open succeeds, the reveal probe throws.
    const h = harness({
      openFeed: (req) => {
        if (req.feed !== undefined) throw new Error("gone");
        return openSuccess(page([]), tokenFor(req));
      },
    });
    const { feed } = mount(h);
    await settle();
    // Act / Assert
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });

  it("answers false when the probe's own page could not be served", async () => {
    // Arrange: the probe answers an ERROR page, so there are no breadcrumbs.
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : openSuccess(
              create(FeedPageSchema, {
                result: {
                  case: "error",
                  value: {
                    headline: { text: "history has a gap", tone: "red" },
                    kind: {
                      case: "historyReplayTruncated",
                      value: { reason: "store closed" },
                    },
                  },
                },
              }),
              tokenFor(req),
            ),
    });
    const { feed } = mount(h);
    await settle();
    // Act / Assert
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });

  it("answers false when a breadcrumb names a row that is no bubble", async () => {
    // Arrange: the crumb points at an ordinary response row on the root feed.
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([responseRow("plain")]), tokenFor(req))
          : openSuccess(
              page([], {
                crumbs: [
                  create(FeedBreadcrumbSchema, {
                    target: feedId("plain"),
                    label: "p",
                  }),
                ],
              }),
              tokenFor(req),
            ),
    });
    const { feed } = mount(h);
    await settle();
    // Act / Assert
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });

  it("does not search inside a bubble the reader has left closed", async () => {
    // Arrange: a collapsed bubble stands on the root feed; the target is not
    // drawn anywhere, so the probe must be made rather than answered locally.
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([subagentRow("b1")]), tokenFor(req))
          : openSuccess(page([]), tokenFor(req)),
    });
    const { feed, h: used } = mount(h);
    await settle();
    // Act
    await feed.selectDetachedWork(feedId("deep"));
    await settle();
    // Assert
    expect(used.calls.openFeed.map((req) => req.feed?.value)).toEqual([
      undefined,
      "deep",
    ]);
  });
});

describe("mountFeed: the root open's harder answers", () => {
  it("paints nothing from an open the disposal overtook", async () => {
    // Arrange: the OpenFeed answer disposes the feed before it is painted.
    let feed: { dispose(): void } | null = null;
    const h = harness({
      openFeed: (req) => {
        feed?.dispose();
        return openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req));
      },
    });
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    // Act
    feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="p1"]')).toBeNull();
  });

  it("opens no tail when the daemon says it has not adopted the workspace yet", async () => {
    // Arrange: a refusal carrying its typed cause.
    const h = harness({
      openFeed: () =>
        create(OpenFeedResponseSchema, {
          result: {
            case: "error",
            value: { cause: { case: "notYetAdopted", value: {} } },
          },
        }),
    });
    const { h: used } = mount(h);
    await settle();
    // Assert
    expect(used.calls.watchFeed).toHaveLength(0);
  });
});

describe("mountFeed: a walk whose container will not open", () => {
  it("answers false when a breadcrumb's bubble refuses to expand", async () => {
    // Arrange: the crumb names a real bubble whose own sub-feed is refused.
    const h = harness({
      openFeed: (req) => {
        if (req.feed === undefined)
          return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1") {
          return create(OpenFeedResponseSchema, {
            result: { case: "error", value: {} },
          });
        }
        return openSuccess(
          page([], {
            crumbs: [
              create(FeedBreadcrumbSchema, {
                target: feedId("b1"),
                label: "Explore",
              }),
            ],
          }),
          tokenFor(req),
        );
      },
    });
    const { feed } = mount(h);
    await settle();
    // Act / Assert
    expect(await feed.selectDetachedWork(feedId("deep"))).toBe(false);
  });
});

describe("mountFeed: disposal is once", () => {
  it("cancels the root watch exactly once, however often dispose is called", async () => {
    // Arrange
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({ channels });
    const { feed, host } = mount(h);
    await settle();
    // Act
    feed.dispose();
    feed.dispose();
    channel.push(push(responseRow("late")));
    await settle();
    // Assert: the second call is inert and nothing reopened.
    expect([
      host.querySelector('[data-feed-row="late"]'),
      h.calls.watchFeed.length,
    ]).toEqual([null, 1]);
  });
});

describe("mountFeed and the client's link verdict", () => {
  afterEach(() => {
    clearClientFailures();
  });

  it("reports the root feed's OpenFeed being REFUSED as a feed that is not tailing", async () => {
    // Arrange: the daemon answers, and refuses. The link is plainly up, so the
    // verdict must not name an unreachable daemon (the audit's N3 row 14).
    const h = harness({
      openFeed: () =>
        create(OpenFeedResponseSchema, {
          result: {
            case: "error",
            value: { cause: { case: "feedUndecodable", value: {} } },
          },
        }),
    });
    const published: Array<string | null> = [];
    const stop = onClientVerdict((verdict) =>
      published.push(verdict?.activity ?? null),
    );
    // Act
    mount(h);
    await settle();
    stop();
    // Assert: the refusal is reported. The retry loop's own ending verdict
    // follows it in the same tick, because a generator that returned is an
    // ending the loop cannot tell from a dead link -- see the judgement row
    // "the feed's refusal verdict is superseded by the tail's ending".
    expect(published).toContain(
      "the daemon refused to open the workspace's root feed",
    );
  });

  it("reports a reveal probe that never reached the daemon as a transport failure", async () => {
    const h = harness({
      openFeed: (req) => {
        if (req.feed !== undefined) throw new Error("gone");
        return openSuccess(page([]), tokenFor(req));
      },
    });
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("deep"));
    expect(standingClientFailure()?.activity).toBe(
      "OpenFeed (the feed's reveal probe) could not reach the daemon",
    );
  });

  it("reports a reveal probe the daemon REFUSED as a feed that is not tailing", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : create(OpenFeedResponseSchema, {
              result: { case: "error", value: {} },
            }),
    });
    const { feed } = mount(h);
    await settle();
    await feed.selectDetachedWork(feedId("shell"));
    expect(standingClientFailure()).toEqual({
      kind: "feed_not_tailing",
      substatus: "feed not tailing",
      activity: "the daemon refused the feed's reveal probe",
    });
  });
});

// THE OVERSCAN BUFFER is created by the mount, rooted on the page's scroll box,
// and torn down when the feed disposes — the same lifecycle as the tail owner's
// resize subscription beside it.

describe("mountFeed: the overscan buffer", () => {
  /** Mount into a fresh scroll box the test keeps a handle on. */
  function mountWithBox(h: Harness = harness()) {
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    return { feed, host, scroll, h };
  }

  it("roots an observer on the page's scroll box", async () => {
    const { scroll } = mountWithBox();
    await settle();
    expect(intersectionObservers().some((r) => r.root === scroll)).toBe(true);
  });

  it("pre-renders a drawn row when it enters the band", async () => {
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    const { host } = mountWithBox(h);
    await settle();
    const row = host.querySelector<HTMLElement>('[data-feed-row="p1"]');
    fireIntersection(row as HTMLElement, true);
    expect(row?.classList.contains(OVERSCAN_CLASS)).toBe(true);
  });

  it("tears the observer down when the feed disposes", async () => {
    const { feed, scroll } = mountWithBox();
    await settle();
    feed.dispose();
    expect(intersectionObservers().some((r) => r.root === scroll)).toBe(false);
  });
});

describe("mountFeed: a card toggle re-measures the titles it owns", () => {
  /** A `.tool-fold` card in the mounted feed, holding one overflowing title. */
  function cardWithTitle(host: HTMLElement): {
    card: HTMLElement;
    title: HTMLElement;
  } {
    const card = document.createElement("div");
    card.className = "tool-card tool-fold";
    const title = document.createElement("pre");
    title.className = "cmd bash-input";
    card.append(foldTitle(title, "card"));
    const row = document.createElement("article");
    row.setAttribute("data-feed-row", "row-1");
    row.append(card);
    host.append(row);
    Object.defineProperty(title, "clientHeight", {
      configurable: true,
      value: 40,
    });
    Object.defineProperty(title, "scrollHeight", {
      configurable: true,
      value: 120,
    });
    return { card, title };
  }

  it("drops the title's has-more when a click expands its card", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const { card, title } = cardWithTitle(host);
    title.classList.add(HAS_MORE_CLASS);

    // Act
    card.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
    feed.dispose();
  });

  it("restores the title's has-more when a click collapses its card", async () => {
    // Arrange — an expanded card whose title is lifted.
    const { feed, host } = mount();
    await settle();
    const { card, title } = cardWithTitle(host);
    card.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    // Act
    card.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
    feed.dispose();
  });
});

describe("mountFeed: expanding a feed item centers it in the feed", () => {
  /** A feed row hung in HOST, laid out at TOP and 100px tall. */
  function rowAt(host: HTMLElement, top: number, id = "row-1"): HTMLElement {
    const row = document.createElement("article");
    row.setAttribute("data-feed-row", id);
    row.getBoundingClientRect = domRect(top, 100);
    host.append(row);
    return row;
  }

  /** A capped `.bubble > .bubble-scroll` hung in ROW. */
  function bubbleIn(row: HTMLElement): HTMLElement {
    const bubble = document.createElement("div");
    bubble.className = "bubble";
    bubble.dataset.role = "response";
    const box = bubbleBox(createBubbleBody(), true);
    bubble.append(box);
    row.append(bubble);
    return box;
  }

  /** A tool card, its own fold, hung in ROW. */
  function toolCardIn(row: HTMLElement): HTMLElement {
    const card = document.createElement("div");
    card.className = "tool-card tool-fold";
    row.append(card);
    return card;
  }

  it("centers the row of a bubble a click expands", async () => {
    // Arrange -- the row hangs 500..600 under a 300px viewport at 100.
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const box = bubbleIn(rowAt(host, 500));
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert -- its midpoint (550) onto the viewport's (150): 100 + 400.
    expect(scroll.scrollTop).toBe(500);
    feed.dispose();
  });

  it("centers the row of a tool card a click expands", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const card = toolCardIn(rowAt(host, 500));
    // Act
    card.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(scroll.scrollTop).toBe(500);
    feed.dispose();
  });

  it("centers the row an item announces itself expanded in", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const inner = document.createElement("div");
    rowAt(host, 500).append(inner);
    // Act
    announceItemExpanded(inner);
    // Assert
    expect(scroll.scrollTop).toBe(500);
    feed.dispose();
  });

  it("centers the NEAREST row, so a nested item is centered as itself", async () => {
    // Arrange -- an outer row at 0..1000 holding a nested row at 700..800.
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const outer = rowAt(host, 0, "outer");
    outer.getBoundingClientRect = domRect(0, 1000);
    const nested = document.createElement("article");
    nested.setAttribute("data-feed-row", "nested");
    nested.getBoundingClientRect = domRect(700, 100);
    outer.append(nested);
    const inner = document.createElement("div");
    nested.append(inner);
    // Act
    announceItemExpanded(inner);
    // Assert -- the nested midpoint (750) onto 150: 100 + 600.
    expect(scroll.scrollTop).toBe(700);
    feed.dispose();
  });

  it("records the move under the itemExpanded cause", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    scriptFeedBox(host.parentElement as HTMLElement);
    const box = bubbleIn(rowAt(host, 500));
    const capture = captureLogRecords("debug");
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect((record.context as Record<string, unknown>).cause).toBe(
      "itemExpanded",
    );
    feed.dispose();
  });

  it("does not move the feed when a click collapses the item", async () => {
    // Arrange -- an expanded bubble, already centered.
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const box = bubbleIn(rowAt(host, 500));
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    scroll.scrollTop = 250;
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(scroll.scrollTop).toBe(250);
    feed.dispose();
  });

  it("stops listening for announced expansions once disposed", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const inner = document.createElement("div");
    rowAt(host, 500).append(inner);
    feed.dispose();
    // Act
    announceItemExpanded(inner);
    // Assert
    expect(scroll.scrollTop).toBe(100);
  });

  it("reports and throws on an expanded item that hangs in no feed row", async () => {
    // Arrange
    const { feed, host } = mount();
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scriptFeedBox(scroll);
    const card = document.createElement("div");
    card.className = "tool-card tool-fold";
    host.append(card);
    const capture = captureLogRecords();
    const thrown: unknown[] = [];
    const onError = (e: ErrorEvent): void => {
      thrown.push(e.error);
      e.preventDefault();
    };
    window.addEventListener("error", onError);
    // Act
    card.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    window.removeEventListener("error", onError);
    // Assert -- the fault is logged with its context, thrown, and nothing moved.
    const record = await forwardedRecord(capture, "feed.center-expanded-item");
    expect([
      record.context,
      (thrown[0] as Error | undefined)?.message,
      scroll.scrollTop,
    ]).toEqual([
      expect.objectContaining({ element: "tool-card tool-fold expanded" }),
      "feed: an expanded feed item hangs in no feed row",
      100,
    ]);
    feed.dispose();
  });
});

describe("latestEntry", () => {
  /** Build a scroll zone from MARKUP, the way index.html lays out the feed and the tray. */
  function zone(markup: string): HTMLElement {
    const box = document.createElement("div");
    box.innerHTML = markup;
    return box;
  }

  it("answers nothing for a zone with nothing drawn", () => {
    // Arrange
    const box = zone(
      '<main id="feed"></main><section id="hold-tray"></section>',
    );
    // Act + Assert
    expect(latestEntry(box)).toBeNull();
  });

  it("answers the last root row when nothing is held", () => {
    // Arrange
    const box = zone(
      '<main id="feed"><article data-feed-row="r1"></article><article data-feed-row="r2"></article></main>' +
        '<section id="hold-tray"></section>',
    );
    // Act + Assert
    expect(latestEntry(box)?.getAttribute("data-feed-row")).toBe("r2");
  });

  it("answers the bubble row, not the last row of its nested sub-feed", () => {
    // Arrange
    const box = zone(
      '<main id="feed"><article data-feed-row="r1"></article>' +
        '<article data-feed-row="bubble"><div><article data-feed-row="inner"></article></div></article></main>',
    );
    // Act + Assert
    expect(latestEntry(box)?.getAttribute("data-feed-row")).toBe("bubble");
  });

  it("answers a tool group's bubble for a last row drawn as one of its tabs", () => {
    // Arrange
    const box = zone(
      '<main id="feed"><div class="feed-group" id="group">' +
        '<article data-feed-row="t1"></article><article data-feed-row="t2" hidden></article></div></main>',
    );
    // Act + Assert
    expect(latestEntry(box)?.id).toBe("group");
  });

  it("answers the last held entry when the tray holds something", () => {
    // Arrange
    const box = zone(
      '<main id="feed"><article data-feed-row="r1"></article></main>' +
        '<section id="hold-tray"><div class="hold-tray"><div class="hold-tray-items">' +
        '<article data-held-turn="t1"></article><article data-held-turn="t2"></article></div></div></section>',
    );
    // Act + Assert
    expect(latestEntry(box)?.getAttribute("data-held-turn")).toBe("t2");
  });
});

describe("mountFeed: the latest-visible latch", () => {
  /**
   * A mounted root feed in a 300px viewport over 2000px of content, with the
   * hold tray after the feed holding HELD cards (none when 0). The first paint
   * parks at the tail; the reader then wheels up to 100, which ends that
   * follow, with nothing yet laid out where they land.
   */
  async function mounted(held: number) {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({
      channels,
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    const tray = document.createElement("section");
    const cards = Array.from(
      { length: held },
      (_, i) => `<article data-held-turn="t${i}"></article>`,
    );
    tray.innerHTML =
      held === 0
        ? ""
        : `<div class="hold-tray"><div class="hold-tray-items">${cards.join("")}</div></div>`;
    scroll.append(host, tray);
    document.body.replaceChildren(scroll);
    scriptFeedBox(scroll);
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    scroll.dispatchEvent(new Event("wheel"));
    scroll.scrollTop = 100;
    scroll.dispatchEvent(new Event("scroll"));
    const place = (el: Element | null, at: number): void => {
      if (!(el instanceof HTMLElement))
        throw new Error("the entry is not drawn");
      el.getBoundingClientRect = domRect(at, 100);
    };
    return { feed, host, tray, scroll, channel, place };
  }

  it("keeps the tail after a scroll brings the last row into view", async () => {
    // Arrange
    const m = await mounted(0);
    m.place(m.host.querySelector('[data-feed-row="r1"]'), 250);
    m.scroll.dispatchEvent(new Event("scroll"));
    // Act
    m.channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(2000);
    m.feed.dispose();
  });

  it("leaves the reader where they are while the last row is out of view", async () => {
    // Arrange
    const m = await mounted(0);
    m.place(m.host.querySelector('[data-feed-row="r1"]'), 300);
    m.scroll.dispatchEvent(new Event("scroll"));
    // Act
    m.channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(100);
    m.feed.dispose();
  });

  it("latches on a held prompt in view even with the last row scrolled above it", async () => {
    // Arrange
    const m = await mounted(1);
    m.place(m.host.querySelector('[data-feed-row="r1"]'), -200);
    m.place(m.tray.querySelector('[data-held-turn="t0"]'), 150);
    m.scroll.dispatchEvent(new Event("scroll"));
    // Act
    m.channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(2000);
    m.feed.dispose();
  });

  it("does not latch on a visible last row while the last held prompt is below the fold", async () => {
    // Arrange
    const m = await mounted(2);
    m.place(m.host.querySelector('[data-feed-row="r1"]'), 50);
    m.place(m.tray.querySelector('[data-held-turn="t0"]'), 200);
    m.place(m.tray.querySelector('[data-held-turn="t1"]'), 300);
    m.scroll.dispatchEvent(new Event("scroll"));
    // Act
    m.channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(100);
    m.feed.dispose();
  });
});

describe("mountFeed: a click on the feed background ends the selection", () => {
  /**
   * A mounted root feed in a 300px viewport over HEIGHT px of content, with r1
   * drawn and the reader wheeled up to 100, and then r1 selected on the live
   * tail — the reply selection holding the follow off.
   */
  async function selected() {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({
      channels,
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const geometry = { top: 100, height: 2000 };
    Object.defineProperties(scroll, {
      scrollHeight: { get: () => geometry.height },
      clientHeight: { get: () => 300 },
      scrollTop: {
        get: () => geometry.top,
        set: (next: number) => {
          geometry.top = next;
        },
      },
    });
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    scroll.dispatchEvent(new Event("wheel"));
    geometry.top = 100;
    scroll.dispatchEvent(new Event("scroll"));
    channel.push(pushSelection({ response: "r1" }));
    await settle();
    return { feed, h, host, scroll, channel, geometry };
  }

  const clickBackground = (host: HTMLElement): void => {
    host.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  };

  it("sends the daemon the clear", async () => {
    // Arrange
    const m = await selected();
    // Act
    clickBackground(m.host);
    await settle();
    // Assert
    expect(m.h.calls.selectFeedRow.map((r) => r.move.case)).toEqual(["clear"]);
    m.feed.dispose();
  });

  it("clears nothing locally before the daemon's push", async () => {
    // Arrange — selecting r1 centered it; the click must move nothing from there.
    const m = await selected();
    const centered = m.scroll.scrollTop;
    // Act
    clickBackground(m.host);
    await settle();
    // Assert
    expect([
      m.host
        .querySelector('[data-feed-row="r1"]')
        ?.getAttribute(SELECTED_ROW_ATTRIBUTE),
      m.scroll.scrollTop,
    ]).toEqual(["response", centered]);
    m.feed.dispose();
  });

  it("parks at the tail on the daemon's cleared push", async () => {
    // Arrange
    const m = await selected();
    clickBackground(m.host);
    await settle();
    // Act
    m.channel.push(pushSelection({ none: "returnToTail" }));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(2000);
    m.feed.dispose();
  });

  it("follows later content after the daemon's cleared push", async () => {
    // Arrange
    const m = await selected();
    clickBackground(m.host);
    await settle();
    m.channel.push(pushSelection({ none: "returnToTail" }));
    await settle();
    // Act
    m.geometry.height = 2400;
    m.channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect(m.scroll.scrollTop).toBe(2400);
    m.feed.dispose();
  });

  it("sends nothing once the selection has cleared", async () => {
    // Arrange
    const m = await selected();
    m.channel.push(pushSelection({ none: "returnToTail" }));
    await settle();
    // Act
    clickBackground(m.host);
    await settle();
    // Assert
    expect(m.h.calls.selectFeedRow).toEqual([]);
    m.feed.dispose();
  });

  it("sends nothing once the feed is disposed", async () => {
    // Arrange
    const m = await selected();
    m.feed.dispose();
    // Act
    clickBackground(m.host);
    await settle();
    // Assert
    expect(m.h.calls.selectFeedRow).toEqual([]);
  });
});

describe("mountFeed: the selected row leaving the viewport ends the selection", () => {
  /** A mounted root feed with r1 and r2 drawn on a live tail. */
  async function mounted() {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:root", channel);
    const h = harness({
      channels,
      openFeed: (req) =>
        openSuccess(
          page([responseRow("r1"), responseRow("r2")]),
          tokenFor(req),
        ),
    });
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const geometry = { top: 0 };
    Object.defineProperties(scroll, {
      scrollHeight: { get: () => 2000 },
      clientHeight: { get: () => 300 },
      scrollTop: {
        get: () => geometry.top,
        set: (next: number) => {
          geometry.top = next;
        },
      },
    });
    const feed = mountFeed(host, h.ctx, {
      renderers: stubRenderers(),
      scrollBox: scroll,
    });
    await settle();
    const row = (id: string): HTMLElement => {
      const el = host.querySelector<HTMLElement>(`[data-feed-row="${id}"]`);
      if (el === null) throw new Error(`no row ${id}`);
      return el;
    };
    return { feed, h, host, scroll, channel, row };
  }

  it("tells the daemon once the selected row it saw has left the viewport", async () => {
    // Arrange
    const m = await mounted();
    m.channel.push(pushSelection({ response: "r1" }));
    await settle();
    fireIntersection(m.row("r1"), true);
    // Act
    fireIntersection(m.row("r1"), false);
    await settle();
    // Assert
    expect(
      m.h.calls.selectFeedRow.map((r) =>
        r.move.case === "leftView" ? r.move.value.row?.value : r.move.case,
      ),
    ).toEqual(["r1"]);
    m.feed.dispose();
  });

  it("leaves the viewport where it is on the daemon's stay push", async () => {
    // Arrange
    const m = await mounted();
    m.channel.push(pushSelection({ response: "r1" }));
    await settle();
    m.scroll.scrollTop = 37;
    // Act
    m.channel.push(pushSelection({ none: "stay" }));
    await settle();
    // Assert
    expect([
      m.scroll.scrollTop,
      m.row("r1").hasAttribute(SELECTED_ROW_ATTRIBUTE),
    ]).toEqual([37, false]);
    m.feed.dispose();
  });
});
