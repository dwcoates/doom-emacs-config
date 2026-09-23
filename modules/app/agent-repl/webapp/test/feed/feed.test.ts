// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedBreadcrumbSchema,
  FeedPageSchema,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import type { WatchFeedResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  clearClientFailures,
  onClientVerdict,
  standingClientFailure,
} from "../../src/rpc/link.js";
import { REVEAL_CLASS, mountFeed } from "../../src/feed/feed.js";
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
  userPromptRow,
  type Harness,
} from "./harness.js";
import { fireResize } from "../resize-observer.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";
import { OVERSCAN_CLASS } from "../../src/feed/overscan.js";
import { foldTitle } from "../../src/feed/title-fold.js";
import { HAS_MORE_CLASS } from "../../src/feed/bubble-more.js";

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
  const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
  return { feed, host, h };
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
      openFeed: (req) => openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
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
    channel.push(pushSelection({ selected: "r1", active: true, center: "r1" }));
    await settle();
    // Assert — the row wears the selection mark, so the frame reached
    // applySelection rather than being rejected as a malformed row.
    expect(host.querySelector('[data-feed-row="r1"]')?.getAttribute("data-selected-response")).toBe(
      "true",
    );
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
    channel.push(pushSelection({ selected: "r1", active: true, center: "r1" }));
    await settle();
    // Act — the clear frame (double-escape).
    channel.push(pushSelection({ active: false }));
    await settle();
    // Assert
    expect(host.querySelector('[data-feed-row="r1"]')?.hasAttribute("data-selected-response")).toBe(
      false,
    );
  });

  it("draws nothing but stays alive when the daemon refuses the open", async () => {
    const h = harness({
      openFeed: () => create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { host } = mount(h);
    await settle();
    expect(host.querySelectorAll("[data-feed-row]")).toHaveLength(0);
  });

  it("opens no tail when the open was refused", async () => {
    const h = harness({
      openFeed: () => create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
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
    expect(h.calls.openFeed.map((req) => req.feed?.value)).toEqual([undefined, "m1"]);
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

  it("marks the revealed row, so the reader's eye lands on it", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.selectDetachedWork(feedId("r1"));
    expect(host.querySelector('[data-feed-row="r1"]')?.classList.contains(REVEAL_CLASS)).toBe(true);
  });

  it("clears the mark once the eye has had time to land", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.selectDetachedWork(feedId("r1"));
    await vi.advanceTimersByTimeAsync(3000);
    expect(host.querySelector('[data-feed-row="r1"]')?.classList.contains(REVEAL_CLASS)).toBe(
      false,
    );
  });

  /** The feed's scroll box, with rects scripted since jsdom lays out nothing. */
  function scripted(scroll: HTMLElement, host: HTMLElement, rowId: string, rowTop: number): void {
    const rect = (top: number, height: number) => (): DOMRect =>
      ({ top, height, bottom: top + height, left: 0, right: 0, width: 0, x: 0, y: top, toJSON: () => ({}) });
    let top = 100;
    Object.defineProperties(scroll, {
      scrollHeight: { get: () => 2000 },
      clientHeight: { get: () => 300 },
      scrollTop: { get: () => top, set: (next: number) => { top = next; } },
    });
    scroll.getBoundingClientRect = rect(0, 300);
    const row = host.querySelector<HTMLElement>(`[data-feed-row="${rowId}"]`);
    if (row === null) throw new Error(`row ${rowId} is not drawn`);
    row.getBoundingClientRect = rect(rowTop, 100);
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

  it("does not scroll the feed for a breadcrumb's reveal", async () => {
    // Arrange -- REMOVED TRIGGER: a breadcrumb click used to scroll the feed to
    // its target. The root page here carries a crumb naming its own row.
    const h = harness({
      openFeed: (req) =>
        openSuccess(page([responseRow("r1")], { crumbs: [crumb("r1", "here")] }), tokenFor(req)),
    });
    const { host } = mount(h);
    await settle();
    const scroll = host.parentElement as HTMLElement;
    scripted(scroll, host, "r1", 500);
    // Act
    host.querySelector<HTMLElement>(".feed-breadcrumb")?.click();
    await settle();
    // Assert -- the row is marked, and the feed is where the reader left it.
    expect([
      host.querySelector('[data-feed-row="r1"]')?.classList.contains(REVEAL_CLASS),
      scroll.scrollTop,
    ]).toEqual([true, 100]);
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
        if (req.feed === undefined) return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1") {
          return openSuccess(page([responseRow("deep")]), tokenFor(req));
        }
        return openSuccess(page([], { crumbs: [crumb("b1", "Explore")] }), tokenFor(req));
      },
    });
    const { feed } = mount(h);
    await settle();
    const revealed = await feed.selectDetachedWork(feedId("deep"));
    await settle();
    expect(revealed).toBe(true);
  });

  it("answers false when the target's feed cannot be opened (a shell bubble)", async () => {
    const h = harness({
      openFeed: (req) =>
        req.feed === undefined
          ? openSuccess(page([]), tokenFor(req))
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
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
          : openSuccess(page([], { crumbs: [crumb("ghost", "gone")] }), tokenFor(req)),
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
      openFeed: (req) => openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
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
      openFeed: (req) => openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
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
          scrollTop = Math.max(0, Math.min(next, init.scrollHeight - clientHeight));
        },
      },
    });
    return { loseHeight: (px: number) => { clientHeight -= px; }, top: () => scrollTop };
  }

  it("re-lands the tail when the docked footer takes height after the render", async () => {
    // Arrange — THE OCCLUSION. The mount must subscribe the scroll box's own
    // size, or a footer settling after the tail render leaves the last bubble
    // below the fold, clipped by the strip.
    const scroll = document.createElement("div");
    const host = document.createElement("div");
    scroll.append(host);
    document.body.replaceChildren(scroll);
    const geometry = withGeometry(scroll, { scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const h = harness({
      openFeed: (req) => openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
    });
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
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
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
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

  it.each(KINDS)("files %s that stopped being one as an unreadable frame", async (_n, row) => {
    // Arrange
    const { h, channel } = withTail(row);
    mount(h);
    await settle();
    // Act: the same id comes back as an ordinary response.
    channel.push(push(responseRow("b1")));
    await settle();
    // Assert
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

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
      host.querySelector('[data-feed-row="b1"] .subagent-head')?.getAttribute("data-state"),
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
    expect(host.querySelector('[data-feed-row="b1"] .bubble-fold')).toBe(before);
  });

  it.each(KINDS)("keeps %s's drawn head rather than tearing the feed down", async (_n, row) => {
    // Arrange
    const { h, channel } = withTail(row);
    const { host } = mount(h);
    await settle();
    // Act
    channel.push(push(responseRow("b1")));
    await settle();
    // Assert: the bubble the reader was looking at is still there.
    expect(host.querySelector('[data-feed-row="b1"] .bubble-fold')).not.toBeNull();
  });
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
                crumbs: [create(FeedBreadcrumbSchema, { target: feedId("plain"), label: "p" })],
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
    expect(used.calls.openFeed.map((req) => req.feed?.value)).toEqual([undefined, "deep"]);
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
    feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
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
        if (req.feed === undefined) return openSuccess(page([subagentRow("b1")]), tokenFor(req));
        if (req.feed.value === "b1") {
          return create(OpenFeedResponseSchema, { result: { case: "error", value: {} } });
        }
        return openSuccess(
          page([], {
            crumbs: [create(FeedBreadcrumbSchema, { target: feedId("b1"), label: "Explore" })],
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
    expect([host.querySelector('[data-feed-row="late"]'), h.calls.watchFeed.length]).toEqual([
      null,
      1,
    ]);
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
          result: { case: "error", value: { cause: { case: "feedUndecodable", value: {} } } },
        }),
    });
    const published: Array<string | null> = [];
    const stop = onClientVerdict((verdict) => published.push(verdict?.activity ?? null));
    // Act
    mount(h);
    await settle();
    stop();
    // Assert: the refusal is reported. The retry loop's own ending verdict
    // follows it in the same tick, because a generator that returned is an
    // ending the loop cannot tell from a dead link -- see the judgement row
    // "the feed's refusal verdict is superseded by the tail's ending".
    expect(published).toContain("the daemon refused to open the workspace's root feed");
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
          : create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
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
    const feed = mountFeed(host, h.ctx, { renderers: stubRenderers(), scrollBox: scroll });
    return { feed, host, scroll, h };
  }

  it("roots an observer on the page's scroll box", async () => {
    const { scroll } = mountWithBox();
    await settle();
    expect(intersectionObservers().some((r) => r.root === scroll)).toBe(true);
  });

  it("pre-renders a drawn row when it enters the band", async () => {
    const h = harness({
      openFeed: (req) => openSuccess(page([userPromptRow("p1", "hello")]), tokenFor(req)),
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
  function cardWithTitle(host: HTMLElement): { card: HTMLElement; title: HTMLElement } {
    const card = document.createElement("div");
    card.className = "tool-card tool-fold";
    const title = document.createElement("pre");
    title.className = "cmd bash-input";
    card.append(foldTitle(title, "card"));
    host.append(card);
    Object.defineProperty(title, "clientHeight", { configurable: true, value: 40 });
    Object.defineProperty(title, "scrollHeight", { configurable: true, value: 120 });
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
