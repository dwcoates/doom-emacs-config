// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { mountBubble } from "../../src/feed/bubble.js";
import { defaultBubbleBody, type Handle } from "../../src/feed/renderers.js";
import {
  Channel,
  harness,
  mergeRow,
  openSuccess,
  page,
  push,
  responseRow,
  rowContext,
  stubRenderers,
  subagentRow,
  tokenFor,
  type Harness,
} from "./harness.js";
import type { WatchFeedResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Mount a bubble for ROW with a marked head and the default body. */
function mount(
  row: FeedRow,
  h: Harness = harness(),
  opts: { folded?: boolean; composerFactory?: (host: HTMLElement) => Handle } = {},
) {
  const heads: number[] = [];
  const bubble = mountBubble({
    ctx: h.ctx,
    row,
    rc: rowContext(h.ctx, row),
    head: () => {
      heads.push(1);
      const el = document.createElement("span");
      el.className = "stub-head";
      return el;
    },
    body: defaultBubbleBody,
    renderers: stubRenderers(),
    revealRow: async () => false,
    bubble: () => {
      throw new Error("no nested bubble in this fixture");
    },
    composerFactory:
      opts.composerFactory === undefined
        ? undefined
        : (host) => opts.composerFactory!(host),
    initialFolded: opts.folded ?? true,
  });
  document.body.replaceChildren(bubble.element);
  return { bubble, h, heads };
}

describe("mountBubble: the collapsed head", () => {
  it("starts collapsed for a subagent row, which ships no fold", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.element.getAttribute("data-expanded")).toBe("false");
  });

  it("transfers nothing but the head until the reader asks", () => {
    const { h } = mount(subagentRow("b1"));
    expect(h.calls.openFeed).toHaveLength(0);
  });

  it("draws the head through the renderer it was given", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.element.querySelector(".stub-head")).not.toBeNull();
  });

  it("offers the fold toggle as the expansion control", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.element.querySelector("[data-expand]")?.getAttribute("data-expand")).toBe("b1");
  });

  it("hosts its sub-feed in the marked panel", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.element.querySelector("[data-subfeed]")).not.toBeNull();
  });

  it("redraws the head on a re-push", () => {
    const { bubble, heads } = mount(subagentRow("b1"));
    bubble.update(subagentRow("b1", { tokens: "9k" }));
    expect(heads).toHaveLength(2);
  });
});

describe("mountBubble: the initial fold (R2)", () => {
  it("opens a merge bubble the daemon shipped unfolded", async () => {
    const { bubble } = mount(mergeRow("m1", false), harness(), { folded: false });
    await settle();
    expect(bubble.isExpanded()).toBe(true);
  });

  it("leaves a merge bubble the daemon shipped folded closed", async () => {
    const { bubble } = mount(mergeRow("m1", true), harness(), { folded: true });
    await settle();
    expect(bubble.isExpanded()).toBe(false);
  });

  it("never re-applies the wire's fold on a re-push, the reader's toggle winning", async () => {
    const { bubble } = mount(mergeRow("m1", true), harness(), { folded: true });
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
    bubble.update(mergeRow("m1", true));
    expect(bubble.isExpanded()).toBe(true);
  });
});

describe("mountBubble: expansion", () => {
  it("opens the sub-feed at the bubble row's OWN id", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(h.calls.openFeed[0]?.feed?.value).toBe("b1");
  });

  it("tails the token the open minted, never a token of its own making", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(h.calls.watchFeed[0]?.watch?.value).toBe("tok:b1");
  });

  it("paints the page BEFORE the tail, so the seam cannot gap", async () => {
    const h = harness({
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    expect(bubble.element.querySelector('[data-feed-row="r1"]')).not.toBeNull();
  });

  it("upserts what the tail pushes into the sub-feed", async () => {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    const h = harness({ channels });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    channel.push(push(responseRow("live")));
    await settle();
    expect(bubble.element.querySelector('[data-feed-row="live"]')).not.toBeNull();
  });

  it("says it is expanded, for the integration suite", async () => {
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(bubble.element.getAttribute("data-expanded")).toBe("true");
  });

  it("joins an open already in flight rather than minting a second token", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    const first = bubble.expand();
    const second = bubble.expand();
    await Promise.all([first, second]);
    await settle();
    expect(h.calls.openFeed).toHaveLength(1);
  });

  it("draws the daemon's refusal at the control that made the call", async () => {
    const h = harness({
      openFeed: () => create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    expect(bubble.element.querySelector(".refusal")?.getAttribute("data-arm")).toBe("error");
  });

  it("stays collapsed when the open was refused", async () => {
    const h = harness({
      openFeed: () => create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    expect(bubble.isExpanded()).toBe(false);
  });

  it("says so when the open never reached the daemon", async () => {
    const h = harness({
      openFeed: () => {
        throw new Error("gone");
      },
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    expect(bubble.element.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("mountBubble: collapse", () => {
  it("abandons the token, and the sub-feed goes quiet", async () => {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    const h = harness({ channels });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    channel.push(push(responseRow("after")));
    await settle();
    expect(bubble.element.querySelector('[data-feed-row="after"]')).toBeNull();
  });

  it("keeps the last DOM, so a re-expand is cheap to look at", async () => {
    const h = harness({
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    expect(bubble.element.querySelector('[data-feed-row="r1"]')).not.toBeNull();
  });

  it("re-opens on the next expansion, state being 'now'", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await bubble.expand();
    await settle();
    expect(h.calls.openFeed).toHaveLength(2);
  });

  it("opens a second tail on the re-opened token", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await bubble.expand();
    await settle();
    expect(h.calls.watchFeed).toHaveLength(2);
  });
});

describe("mountBubble: the parity invariant", () => {
  it("issues the IDENTICAL rpc sequence for a subagent and a merge bubble", async () => {
    const subagent = mount(subagentRow("b1"));
    await subagent.bubble.expand();
    await settle();
    const merge = mount(mergeRow("b1"));
    await merge.bubble.expand();
    await settle();
    expect([
      subagent.h.calls.openFeed.map((req) => req.feed?.value),
      subagent.h.calls.watchFeed.map((req) => req.watch?.value),
    ]).toEqual([
      merge.h.calls.openFeed.map((req) => req.feed?.value),
      merge.h.calls.watchFeed.map((req) => req.watch?.value),
    ]);
  });
});

describe("mountBubble: the per-bubble composer (R7)", () => {
  it("mounts the composer into the sub-feed when this build has a factory", async () => {
    const mounted: HTMLElement[] = [];
    const { bubble } = mount(subagentRow("b1"), harness(), {
      composerFactory: (host) => {
        mounted.push(host);
        host.className = "stub-composer";
        return { dispose: () => {} };
      },
    });
    await bubble.expand();
    await settle();
    expect(mounted).toHaveLength(1);
  });

  it("mounts none in production, which runs composer-less", async () => {
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(bubble.element.querySelector(".bubble-composer")).toBeNull();
  });
});

describe("mountBubble: disposal", () => {
  it("takes its element off screen", async () => {
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    bubble.dispose();
    expect(document.body.contains(bubble.element)).toBe(false);
  });

  it("refuses to expand after disposal", async () => {
    const { bubble } = mount(subagentRow("b1"));
    bubble.dispose();
    expect(await bubble.expand()).toBe(false);
  });
});
