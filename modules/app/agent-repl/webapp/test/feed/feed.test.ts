// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedBreadcrumbSchema,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import type { WatchFeedResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import { REVEAL_CLASS, mountFeed } from "../../src/feed/feed.js";
import {
  Channel,
  feedId,
  harness,
  mergeRow,
  openSuccess,
  page,
  push,
  responseRow,
  stubRenderers,
  subagentRow,
  tokenFor,
  userPromptRow,
  type Harness,
} from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
  // jsdom implements no scrolling at all, and `revealNode` is the browser's
  // own affordance rather than anything this module computes.
  Element.prototype.scrollIntoView = function scrollIntoView(): void {};
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

describe("mountFeed: revealRow", () => {
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
    expect(await feed.revealRow(feedId("r1"))).toBe(true);
  });

  it("marks the revealed row, so the reader's eye lands on it", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.revealRow(feedId("r1"));
    expect(host.querySelector('[data-feed-row="r1"]')?.classList.contains(REVEAL_CLASS)).toBe(true);
  });

  it("clears the mark once the eye has had time to land", async () => {
    const { feed, host } = mount(rootPage([responseRow("r1")]));
    await settle();
    await feed.revealRow(feedId("r1"));
    await vi.advanceTimersByTimeAsync(3000);
    expect(host.querySelector('[data-feed-row="r1"]')?.classList.contains(REVEAL_CLASS)).toBe(
      false,
    );
  });

  it("asks the daemon where an undrawn row lives", async () => {
    const h = rootPage([]);
    const { feed } = mount(h);
    await settle();
    await feed.revealRow(feedId("deep"));
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
    const revealed = await feed.revealRow(feedId("deep"));
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
    expect(await feed.revealRow(feedId("shell"))).toBe(false);
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
    expect(await feed.revealRow(feedId("deep"))).toBe(false);
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
    expect(await feed.revealRow(feedId("deep"))).toBe(false);
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
