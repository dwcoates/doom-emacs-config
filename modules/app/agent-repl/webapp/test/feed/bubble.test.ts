// @vitest-environment jsdom
import { ITEM_EXPANDED_EVENT } from "../../src/expand.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import { FeedIdSchema, FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { clearClientFailures, onClientVerdict } from "../../src/rpc/link.js";
import { mountBubble } from "../../src/feed/bubble.js";
import { foldTitle } from "../../src/feed/title-fold.js";
import { HAS_MORE_CLASS } from "../../src/feed/bubble-more.js";
import { drawFeedSubagent } from "../../src/feed/rows/subagent.js";
import STYLESHEET from "../../src/styles.css?raw";
import { installStylesheet, rulesOf } from "../stylesheet.js";
import { defaultBubbleBody, type Handle } from "../../src/feed/renderers.js";
import {
  Channel,
  harness,
  mergeRow,
  openSuccess,
  page,
  push,
  pushScale,
  pushSelection,
  responseRow,
  rowContext,
  stubRenderers,
  subagentRow,
  tokenFor,
  type Harness,
} from "./harness.js";
import type { WatchFeedResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import { TailFollow } from "../../src/scroll.js";
import { captureLogRecords } from "../log-capture.js";
import { orderFor } from "../feed-order.js";

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
  opts: {
    folded?: boolean;
    composerFactory?: (host: HTMLElement) => Handle;
    /** What the head states about itself, per draw. */
    states?: (string | null)[];
    /** A head of the suite's own, in place of the marked stub. */
    head?: () => HTMLElement;
    /** The page's scroll box and tail owner. */
    scroll?: { box: Element; tail: TailFollow };
  } = {},
) {
  const heads: number[] = [];
  const bubble = mountBubble({
    ctx: h.ctx,
    row,
    rc: rowContext(h.ctx, row),
    head: () => {
      if (opts.head !== undefined) return opts.head();
      const el = document.createElement("span");
      el.className = "stub-head";
      const state = opts.states?.[heads.length];
      if (state !== undefined && state !== null) el.setAttribute("data-state", state);
      heads.push(1);
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
    scroll: opts.scroll,
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

  it("offers the head row itself as the expansion control", () => {
    const { bubble } = mount(subagentRow("b1"));
    const control = bubble.element.querySelector("[data-expand]");
    expect(control?.getAttribute("data-expand")).toBe("b1");
    expect(control?.classList.contains("bubble-head")).toBe(true);
  });

  it("renders no chevron toggle button, the head being the toggle now", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.element.querySelector(".bubble-toggle")).toBeNull();
    expect(bubble.element.querySelector(".agent-caret")).toBeNull();
  });

  it("makes the head an accessible button, focusable and collapsed", () => {
    const { bubble } = mount(subagentRow("b1"));
    const head = bubble.element.querySelector<HTMLElement>(".bubble-head");
    expect(head?.getAttribute("role")).toBe("button");
    expect(head?.tabIndex).toBe(0);
    expect(head?.getAttribute("aria-expanded")).toBe("false");
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

describe("mountBubble: collapse, the jump's and the tail's handle on the fold", () => {
  it("closes an open bubble", async () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    // Act
    bubble.collapse();
    // Assert
    expect(bubble.isExpanded()).toBe(false);
  });

  it("leaves a closed bubble as it is, collapsing nothing", async () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    const capture = captureLogRecords();
    // Act
    bubble.collapse();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "feed.bubble-collapse")).toEqual([]);
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

  // OWNER RULING 2026-09-23 replaces "keeps the last DOM": a collapse WIPES the
  // sub-feed, and the next expansion starts from a fresh OpenFeed page.
  it("disposes the child and leaves no sub-feed row in the DOM", async () => {
    // Arrange
    const h = harness({
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Assert
    expect({
      rows: bubble.element.querySelectorAll("[data-feed-row]").length,
      child: bubble.child(),
    }).toEqual({ rows: 0, child: null });
  });

  it("disposes the bubble's composer with the sub-feed", async () => {
    // Arrange
    const disposed: string[] = [];
    const { bubble } = mount(subagentRow("b1"), harness(), {
      composerFactory: () => ({ dispose: () => disposed.push("composer") }),
    });
    await bubble.expand();
    await settle();
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Assert
    expect(disposed).toEqual(["composer"]);
  });

  it("re-expands from a fresh OpenFeed page alone, never the accumulated history", async () => {
    // Arrange: the first expansion's page and a streamed row, then a collapse.
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    let opens = 0;
    const h = harness({
      channels,
      openFeed: (req) => {
        opens += 1;
        return openSuccess(page(opens === 1 ? [responseRow("old")] : [responseRow("newest")]), tokenFor(req));
      },
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    channel.push(push(responseRow("streamed")));
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Act
    await bubble.expand();
    await settle();
    // Assert: only the second OpenFeed's page is drawn.
    expect([...bubble.element.querySelectorAll("[data-feed-row]")].map((el) => el.getAttribute("data-feed-row"))).toEqual([
      "newest",
    ]);
  });

  it("streams new rows in while the bubble stays open", async () => {
    // Arrange
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    const h = harness({
      channels,
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.push(push(responseRow("r2")));
    await settle();
    // Assert
    expect([...bubble.element.querySelectorAll("[data-feed-row]")].map((el) => el.getAttribute("data-feed-row"))).toEqual([
      "r1",
      "r2",
    ]);
  });

  it("leaves no watch drawing after the collapse", async () => {
    // Arrange
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    const { bubble } = mount(subagentRow("b1"), harness({ channels }));
    await bubble.expand();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Act: the daemon keeps publishing.
    channel.push(push(responseRow("late")));
    await settle();
    // Assert: nothing drew it, and no controller exists to draw it.
    expect({
      rows: bubble.element.querySelectorAll("[data-feed-row]").length,
      child: bubble.child(),
    }).toEqual({ rows: 0, child: null });
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

describe("mountBubble: clicking the head is the toggle", () => {
  /** The head row of a mounted bubble. */
  function headOf(bubble: { element: HTMLElement }): HTMLElement {
    const head = bubble.element.querySelector<HTMLElement>(".bubble-head");
    if (head === null) throw new Error("the bubble mounted no head");
    return head;
  }

  it("expands the bubble on a click anywhere on the head", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).click();
    await settle();
    expect(bubble.isExpanded()).toBe(true);
  });

  it("opens the sub-feed on the first head click", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    headOf(bubble).click();
    await settle();
    expect(h.calls.openFeed[0]?.feed?.value).toBe("b1");
  });

  /** Count the expansions ELEMENT announces. */
  function countAnnouncements(element: HTMLElement): { count: number } {
    const seen = { count: 0 };
    element.addEventListener(ITEM_EXPANDED_EVENT, () => {
      seen.count++;
    });
    return seen;
  }

  it("announces itself expanded once the reader's head click opened it", async () => {
    const { bubble } = mount(subagentRow("b1"));
    const seen = countAnnouncements(bubble.element);
    headOf(bubble).click();
    await settle();
    expect(seen.count).toBe(1);
  });

  it("announces nothing when a reveal opens it, which never scrolls", async () => {
    const { bubble } = mount(subagentRow("b1"));
    const seen = countAnnouncements(bubble.element);
    await bubble.expand();
    await settle();
    expect(seen.count).toBe(0);
  });

  it("announces nothing when the head click collapses it", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).click();
    await settle();
    const seen = countAnnouncements(bubble.element);
    headOf(bubble).click();
    await settle();
    expect(seen.count).toBe(0);
  });

  it("collapses again on a second head click", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).click();
    await settle();
    headOf(bubble).click();
    await settle();
    expect(bubble.isExpanded()).toBe(false);
  });

  it("updates aria-expanded on the head when the fold toggles", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).click();
    await settle();
    expect(headOf(bubble).getAttribute("aria-expanded")).toBe("true");
  });

  it("does NOT toggle when the click lands on a control inside the head", async () => {
    // Arrange: a stop-style control the head carries, as the subagent head's
    // `data-interrupt` button is.
    const { bubble } = mount(subagentRow("b1"));
    const head = headOf(bubble);
    const stop = document.createElement("button");
    stop.setAttribute("data-interrupt", "b1");
    head.append(stop);
    // Act: the control's own click, which bubbles up to the head listener.
    stop.click();
    await settle();
    // Assert: the fold stayed shut; the control did its own thing.
    expect(bubble.isExpanded()).toBe(false);
  });

  it("toggles on Enter while the head is focused", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).dispatchEvent(
      new KeyboardEvent("keydown", { key: "Enter", bubbles: true }),
    );
    await settle();
    expect(bubble.isExpanded()).toBe(true);
  });

  it("toggles on Space while the head is focused", async () => {
    const { bubble } = mount(subagentRow("b1"));
    headOf(bubble).dispatchEvent(
      new KeyboardEvent("keydown", { key: " ", bubbles: true }),
    );
    await settle();
    expect(bubble.isExpanded()).toBe(true);
  });

  it("ignores a key press that arose from a control inside the head", async () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    const head = headOf(bubble);
    const stop = document.createElement("button");
    stop.setAttribute("data-interrupt", "b1");
    head.append(stop);
    // Act: Enter pressed with the inner control as the event's target.
    stop.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    await settle();
    // Assert
    expect(bubble.isExpanded()).toBe(false);
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

describe("mountBubble: the head's state is the bubble's", () => {
  it("repeats what the head stated about itself", () => {
    const { bubble } = mount(subagentRow("b1"), harness(), { states: ["live"] });
    expect(bubble.element.getAttribute("data-state")).toBe("live");
  });

  it("stops stating it once a later head states nothing", () => {
    // Arrange: the first head says "live", the redrawn one says nothing.
    const { bubble } = mount(subagentRow("b1"), harness(), { states: ["live", null] });
    // Act
    bubble.update(subagentRow("b1", { tokens: "9k" }));
    // Assert: the bubble decides nothing of its own.
    expect(bubble.element.hasAttribute("data-state")).toBe(false);
  });
});

describe("mountBubble: the fold is a fact about the ROW", () => {
  it("says the fold on the row chrome the bubble sits in", async () => {
    // Arrange: the bubble mounted inside a real row element.
    const h = harness();
    const row = document.createElement("article");
    row.setAttribute("data-feed-row", "b1");
    const { bubble } = mount(subagentRow("b1"), h);
    row.append(bubble.element);
    document.body.replaceChildren(row);
    // Act
    await bubble.expand();
    await settle();
    // Assert
    expect(row.getAttribute("data-expanded")).toBe("true");
  });
});

describe("mountBubble: expanding what is already open", () => {
  it("issues no second OpenFeed for a bubble that is already expanded", async () => {
    const { bubble, h } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    await bubble.expand();
    await settle();
    expect(h.calls.openFeed).toHaveLength(1);
  });

  it("answers true straight away for a bubble that is already expanded", async () => {
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(await bubble.expand()).toBe(true);
  });
});

describe("mountBubble: a row with no arm at all", () => {
  it("still opens its sub-feed by the row's own id", async () => {
    // Arrange: the id is all a bubble needs; the arm is the head's business.
    const h = harness();
    const bare = create(FeedRowSchema, { id: create(FeedIdSchema, { value: "b1" }), order: orderFor("b1") });
    const { bubble } = mount(bare, h);
    // Act
    await bubble.expand();
    await settle();
    // Assert
    expect(h.calls.openFeed[0]?.feed?.value).toBe("b1");
  });
});

describe("mountBubble: re-opening the tail", () => {
  /** A bubble whose sub-feed page is whatever `pages` yields next. */
  function reopening(pages: FeedRow[][], refuseAfter = Number.POSITIVE_INFINITY) {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    let served = 0;
    const h = harness({
      channels,
      openFeed: (req) => {
        const at = served;
        served += 1;
        if (at >= refuseAfter) {
          return create(OpenFeedResponseSchema, { result: { case: "error", value: {} } });
        }
        return openSuccess(page(pages[Math.min(at, pages.length - 1)]), tokenFor(req));
      },
    });
    return { h, channel };
  }

  it("opens the feed again when the sub-feed's tail ends on its own", async () => {
    // Arrange
    const { h, channel } = reopening([[responseRow("r1")]]);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert
    expect(h.calls.openFeed.length).toBeGreaterThan(1);
  });

  it("paints the fresh page over the sub-feed's rows on a reopen", async () => {
    // Arrange
    const { h, channel } = reopening([[responseRow("r1")], [responseRow("r2")]]);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert
    expect(bubble.element.querySelector('[data-feed-row="r2"]')).not.toBeNull();
  });

  it("drops a row the fresh page omits, a page being a whole view", async () => {
    const { h, channel } = reopening([[responseRow("r1")], [responseRow("r2")]]);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    expect(bubble.element.querySelector('[data-feed-row="r1"]')).toBeNull();
  });

  it("tails the token the reopen minted, never the dead one", async () => {
    const { h, channel } = reopening([[responseRow("r1")]]);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    expect(h.calls.watchFeed.at(-1)?.watch?.value).toBe("tok:b1");
  });

  it("draws no refusal when the REOPEN is refused, there being no click to mark", async () => {
    // Arrange: the expanding open succeeds, every reopen after it is refused.
    const { h, channel } = reopening([[responseRow("r1")]], 1);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert
    expect(bubble.element.querySelector(".refusal")).toBeNull();
  });

  it("reports a refused reopen, the sub-feed having silently stopped tailing", async () => {
    // Arrange: the expanding open succeeds, every reopen after it is refused.
    const { h, channel } = reopening([[responseRow("r1")]], 1);
    const published: Array<string | null> = [];
    const stop = onClientVerdict((verdict) => published.push(verdict?.activity ?? null));
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    stop();
    clearClientFailures();
    // Assert: reported. The tail's own ending verdict follows it in the same
    // tick -- see the judgement row on the superseding ending.
    expect(published).toContain("the daemon refused to re-open a sub-feed after its tail died");
  });

  it("keeps the rows the last good page painted when a reopen is refused", async () => {
    const { h, channel } = reopening([[responseRow("r1")]], 1);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    expect(bubble.element.querySelector('[data-feed-row="r1"]')).not.toBeNull();
  });

  it("opens nothing more once the bubble is disposed mid-reopen", async () => {
    // Arrange
    const { h, channel } = reopening([[responseRow("r1")]]);
    const { bubble } = mount(subagentRow("b1"), h);
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    bubble.dispose();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert: a cancelled watch reopens nothing.
    expect(h.calls.watchFeed).toHaveLength(1);
  });
});

describe("mountBubble: the refusal does not accumulate", () => {
  it("drops the previous refusal when the toggle is used again", async () => {
    // Arrange: a daemon that refuses every open.
    const h = harness({
      openFeed: () => create(OpenFeedResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
    // Assert
    expect(bubble.element.querySelectorAll(".refusal")).toHaveLength(1);
  });
});

describe("mountBubble: disposal is once", () => {
  it("disposes the sub-feed's composer exactly once", async () => {
    // Arrange
    let disposals = 0;
    const { bubble } = mount(subagentRow("b1"), harness(), {
      composerFactory: () => ({
        dispose: () => {
          disposals += 1;
        },
      }),
    });
    await bubble.expand();
    await settle();
    // Act
    bubble.dispose();
    bubble.dispose();
    // Assert
    expect(disposals).toBe(1);
  });
});

describe("mountBubble: the sub-feed it hands the reveal walk", () => {
  it("has no sub-feed before the first expansion", () => {
    const { bubble } = mount(subagentRow("b1"));
    expect(bubble.child()).toBeNull();
  });

  it("hands over the controller holding the sub-feed's rows once open", async () => {
    // Arrange
    const h = harness({
      openFeed: (req) => openSuccess(page([responseRow("r1")]), tokenFor(req)),
    });
    const { bubble } = mount(subagentRow("b1"), h);
    // Act
    await bubble.expand();
    await settle();
    // Assert
    expect(bubble.child()?.rows().map((row) => row.id?.value)).toEqual(["r1"]);
  });
});

describe("mountBubble: the row says the fold on the way back too", () => {
  it("says the collapse on the row chrome the bubble sits in", async () => {
    // Arrange
    const row = document.createElement("article");
    row.setAttribute("data-feed-row", "b1");
    const { bubble } = mount(subagentRow("b1"));
    row.append(bubble.element);
    document.body.replaceChildren(row);
    await bubble.expand();
    await settle();
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
    // Assert
    expect(row.getAttribute("data-expanded")).toBe("false");
  });
});

describe("mountBubble: disposed while a reopen is in flight", () => {
  it("paints no page from a reopen the disposal overtook", async () => {
    // Arrange: the reopen's own OpenFeed disposes the bubble before answering.
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    let served = 0;
    let live: { dispose(): void } | null = null;
    const h = harness({
      channels,
      openFeed: (req) => {
        served += 1;
        if (served === 2) live?.dispose();
        return openSuccess(page([responseRow(served === 1 ? "r1" : "r2")]), tokenFor(req));
      },
    });
    const { bubble } = mount(subagentRow("b1"), h);
    live = bubble;
    await bubble.expand();
    await settle();
    // Act
    channel.close();
    await vi.advanceTimersByTimeAsync(1_000);
    await settle();
    // Assert: the fresh page is dropped rather than painted into a dead bubble.
    expect(bubble.element.querySelector('[data-feed-row="r2"]')).toBeNull();
  });
});

describe("mountBubble: the fold actually hides the sub-feed", () => {
  // THE CASCADE IS THE CLAIM, and it cannot be asked of jsdom. `applyExpanded`
  // folds a bubble by setting `panel.hidden`, and `[hidden]` is a USER-AGENT
  // rule that an author rule setting `display` on the same element outranks by
  // source order -- so `.agent-panel { display: flex }` left a collapsed
  // sub-feed FULLY DRAWN in WebKit while the caret said it was shut, and every
  // other assertion in this file passed in that state. jsdom cannot reproduce
  // it: its `getComputedStyle` answers `none` for a `hidden` element whatever
  // the author sheet says (measured -- an author `.agent-panel { display: flex
  // }` over a hidden element still computes `none` there), so the browser's
  // answer is asserted where the browser is, in a real webview,
  // and what is asserted HERE is the sheet the browser will read.
  //
  // THE SELECTOR COMES FROM THE PRODUCTION ELEMENT, never restated: the class
  // is read off a mounted bubble's own panel, so renaming it in `bubble.ts`
  // moves this assertion with it rather than leaving it pinning a dead name.

  /** The sub-feed panel of a mounted bubble. */
  function panelOf(bubble: { element: HTMLElement }): HTMLElement {
    const panel = bubble.element.querySelector<HTMLElement>("[data-subfeed]");
    if (panel === null) throw new Error("the bubble mounted no sub-feed panel");
    return panel;
  }

  /** Every rule block in the sheet, in source order, as selector + body. */
  function rules(): { selector: string; body: string }[] {
    return rulesOf(STYLESHEET).map((rule) => ({ selector: rule.selectors.join(", "), body: rule.declarations }));
  }

  it("guards every display the sheet sets on the panel with a [hidden] rule", () => {
    // Arrange: the class production actually puts on the sub-feed panel.
    const { bubble } = mount(subagentRow("b1"));
    const classes = [...panelOf(bubble).classList];
    expect(classes.length).toBeGreaterThan(0);
    // Act: the sheet's rules that set `display` on any of those classes, and
    // the ones that set it on the same class WHEN HIDDEN.
    const sets = rules().filter(
      (rule) =>
        classes.some((cls) => rule.selector.includes(`.${cls}`)) &&
        /(^|[;\s])display\s*:/.test(rule.body),
    );
    const guards = sets.filter(
      (rule) => rule.selector.includes("[hidden]") && /display\s*:\s*none/.test(rule.body),
    );
    // Assert: the LAST word on display for a hidden panel is `none`.
    expect(sets.length).toBeGreaterThan(0);
    expect(guards.length).toBeGreaterThan(0);
    expect(sets.indexOf(guards[guards.length - 1])).toBe(sets.length - 1);
  });

  // AND IT STACKS. The bubble is now a `.tool-card` (owner ruling,
  // 2026-09-14), and `.tool-card` sets no `display`, so `.tool-card.bubble-fold`
  // states `display: flex; flex-direction: column` explicitly -- without it the
  // head line and the whole sub-feed would fall back to default block flow
  // rather than the pinned flex-column siblings the fold and reveal walk both
  // rely on. The old `.bubble` flex-ROW failure this replaces (head and
  // sub-feed side by side) was photographed by the G49 playbook. jsdom resolves
  // the cascade for this one, so it is asked here.
  it("lays the sub-feed BENEATH the head rather than beside it", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      document.body.replaceChildren(bubble.element);
      // Act / Assert
      expect(window.getComputedStyle(bubble.element).flexDirection).toBe("column");
    } finally {
      remove();
    }
  });

  it("lets the sub-feed take the bubble's whole width", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      document.body.replaceChildren(bubble.element);
      // Act / Assert: stretched, so a column layout does not shrink-wrap it.
      expect(window.getComputedStyle(bubble.element).alignItems).toBe("stretch");
    } finally {
      remove();
    }
  });

  it("hides the panel on the caret's collapse", async () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    await bubble.expand();
    await settle();
    expect(panelOf(bubble).hidden).toBe(false);
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
    // Assert
    expect(panelOf(bubble).hidden).toBe(true);
  });

  // FLUSH INTERIOR (owner ruling, 2026-09-14): the recursive sub-feed sits
  // DIRECTLY in the tool card, not inside a second bordered `.agent-panel` box.
  // The fill and the corner radius are read from jsdom's resolved cascade (a
  // `background` shorthand and `border-radius` it parses). The BORDER is not
  // measurable here -- the base `.agent-panel { border: 1px solid var(--…) }`
  // shorthand is one jsdom cannot parse into longhands (see the outer-card note
  // above), so its computed border-style stays `none` whether or not the flush
  // override lands -- so the border removal is asserted from the sheet's own
  // source order, scoped to the rules that ACTUALLY match the panel element
  // (`.matches`, so `.fold-fixed > .agent-panel` and the scrollbar
  // pseudo-element rules that merely mention `.agent-panel` are excluded).

  /** The panel-matching rules that set PROP, in the sheet's source order. */
  function panelSetters(bubble: { element: HTMLElement }, prop: string): { selector: string; body: string }[] {
    const panel = panelOf(bubble);
    return rules().filter(
      (rule) =>
        new RegExp(`(^|[;\\s])${prop}\\s*:`).test(rule.body) &&
        rule.selector
          .split(",")
          .some((group) => {
            const bare = group.trim().replace(/::[a-z-]+$/i, "");
            if (bare === "") return false;
            try {
              return panel.matches(bare);
            } catch {
              return false;
            }
          }),
    );
  }

  it("drops the inner box border so the sub-feed is not a card-within-a-card", () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    // Act: the last word the cascade gives the panel's border.
    const setters = panelSetters(bubble, "border");
    const last = setters[setters.length - 1];
    // Assert: the flush `.bubble-subfeed` rule wins, removing the border.
    expect(setters.length).toBeGreaterThan(0);
    expect(last.selector).toContain(".bubble-subfeed");
    expect(last.body).toMatch(/(^|[;\s])border\s*:\s*none/);
  });

  it("drops the inner box fill so the interior reads on the card grey", () => {
    // Arrange: the base panel fills with `var(--bg)`; the flush override clears
    // it, which jsdom resolves to a transparent computed background.
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      document.body.replaceChildren(bubble.element);
      // Act / Assert: no fill of its own, so the card grey shows through.
      expect(window.getComputedStyle(panelOf(bubble)).background).toBe("rgba(0, 0, 0, 0)");
    } finally {
      remove();
    }
  });

  it("squares the inner box corners so no nested panel radius shows", () => {
    // Arrange: jsdom parses `border-radius` reliably, so this one is measured.
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      document.body.replaceChildren(bubble.element);
      // Act / Assert: the flush override squares the corners.
      expect(window.getComputedStyle(panelOf(bubble)).borderRadius).toBe("0px");
    } finally {
      remove();
    }
  });

  // THE EXPANDED CAP (owner ruling, 2026-09-23): an expanded bubble's sub-feed
  // stops at three quarters of the feed's visible height (`75cqh` against the
  // `#feed-scroll` size container) and scrolls inside past that.
  it("caps an expanded sub-feed bubble at the expanded-item ceiling", async () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      // Act
      await bubble.expand();
      await settle();
      // Assert
      expect(window.getComputedStyle(bubble.element).maxHeight).toBe("var(--feed-item-max-h)");
    } finally {
      remove();
    }
  });

  it("leaves a collapsed sub-feed uncapped", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      // Act
      const { bubble } = mount(subagentRow("b1"));
      // Assert
      expect(window.getComputedStyle(panelOf(bubble)).maxHeight).not.toBe("75cqh");
    } finally {
      remove();
    }
  });

  it("scrolls an expanded sub-feed's overflow inside it rather than growing the feed", async () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      // Act
      await bubble.expand();
      await settle();
      // Assert
      expect(window.getComputedStyle(panelOf(bubble)).overflowY).toBe("auto");
    } finally {
      remove();
    }
  });

  it("caps an expanded merge bubble at the same ceiling", async () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(mergeRow("m1"));
      // Act
      await bubble.expand();
      await settle();
      // Assert
      expect(window.getComputedStyle(bubble.element).maxHeight).toBe("var(--feed-item-max-h)");
    } finally {
      remove();
    }
  });
});

// THE DETACHED/SUBAGENT BUBBLE IS FRAMED AS A NORMAL TOOL-CALL CARD (owner
// ruling, 2026-09-14): the outer wears `.tool-card` so it takes the ordinary
// grey card chrome and the shared track cap, NOT the old full-width async
// spread, teal wash, or dashed fold separator. These replace the assertions
// that used to pin the `.async-fold`/`.bubble` async treatment.
describe("mountBubble: the tool-card framing", () => {
  it("frames the outer as a tool card, not the old async fold", () => {
    // Arrange / Act
    const { bubble } = mount(subagentRow("b1"));
    // Assert
    expect(bubble.element.classList.contains("tool-card")).toBe(true);
    expect(bubble.element.classList.contains("bubble-fold")).toBe(true);
  });

  it("drops the async-fold class the old spread was keyed on", () => {
    // Arrange / Act
    const { bubble } = mount(subagentRow("b1"));
    // Assert
    expect(bubble.element.classList.contains("async-fold")).toBe(false);
  });

  it("drops the prompt-bubble class so it takes no `.bubble` lift", () => {
    // Arrange / Act
    const { bubble } = mount(subagentRow("b1"));
    // Assert
    expect(bubble.element.classList.contains("bubble")).toBe(false);
  });

  it("draws the collapsed head as a tool-call head row, not an async pill", () => {
    // Arrange / Act
    const { bubble } = mount(subagentRow("b1"));
    const head = bubble.element.querySelector(".bubble-head");
    // Assert
    expect(head?.classList.contains("tool-head")).toBe(true);
    expect(head?.classList.contains("async-ticker")).toBe(false);
  });

  it("takes the shared track cap rather than the full feed-area width", () => {
    // Arrange: the cap rule is `.feed-item > .tool-card`, so the outer must sit
    // as a direct child of a feed item exactly as the feed mounts it.
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      const item = document.createElement("article");
      item.className = "feed-item";
      item.append(bubble.element);
      document.body.replaceChildren(item);
      // Act / Assert: the shared column cap, never 100%.
      expect(window.getComputedStyle(bubble.element).maxWidth).toBe(
        "var(--agent-bubble-cap)",
      );
    } finally {
      remove();
    }
  });

  it("drops the dashed fold separator the async fold drew above it", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble } = mount(subagentRow("b1"));
      document.body.replaceChildren(bubble.element);
      // Act / Assert: the `.async-fold` dashed top rule no longer reaches this
      // element -- it is a tool card now. (jsdom cannot parse the `.tool-card`
      // `border: 1px solid var(--…)` shorthand into its longhands, so the
      // positive `solid` is asserted in the real webview; what is checkable
      // here is that the dashed separator is gone.)
      expect(window.getComputedStyle(bubble.element).borderTopStyle).not.toBe("dashed");
    } finally {
      remove();
    }
  });
});

/**
 * WHERE THE CARET LEAVES THE READER.
 *
 * The defect, measured at a click: `below=208 scrollTop=40`. Expanding a fold
 * grows the feed BELOW the fold, growth moves nothing on its own, and the
 * sub-feed the reader just asked for unrolled entirely off the bottom of the
 * screen. `TailFollow.release` had been written for this and nothing in
 * production called it.
 *
 * JSDOM LAYS NOTHING OUT, so the two boxes the reveal compares are scripted
 * here exactly as `test/integration/feed-tail.integration.test.ts` scripts the
 * feed's own -- a `scrollTop` that clamps into range on write, a
 * `scrollHeight` that GROWS by the panel's height when the panel is shown, and
 * a panel whose rect is the empty box a hidden element really has. That is an
 * environment substitution, not a seam: every number below is one a browser
 * would have supplied.
 */
describe("mountBubble: the caret's own view rule", () => {
  /** The scroll box's visible height, and the panel's, in one coordinate system. */
  const BOX_HEIGHT = 300;
  const PANEL_HEIGHT = 200;
  /** The feed's height with the bubble SHUT: 1000 - 300 leaves a tail at 700. */
  const CONTENT_HEIGHT = 1000;

  /** A scripted scroll box the bubble hangs in. */
  function scrolling(opts: { scrollTop: number; panelTop: number }) {
    const box = document.createElement("div");
    let panel: HTMLElement | null = null;
    let scrollTop = opts.scrollTop;
    const shown = (): boolean => panel !== null && !panel.hidden;
    Object.defineProperties(box, {
      scrollHeight: { get: () => CONTENT_HEIGHT + (shown() ? PANEL_HEIGHT : 0) },
      clientHeight: { get: () => BOX_HEIGHT },
      scrollTop: {
        get: () => scrollTop,
        set: (next: number) => {
          scrollTop = Math.max(0, Math.min(next, box.scrollHeight - BOX_HEIGHT));
        },
      },
    });
    box.getBoundingClientRect = () => rect(0, BOX_HEIGHT);
    document.body.replaceChildren(box);
    return {
      box,
      top: () => scrollTop,
      /** Adopt a mounted bubble into the box, and script its panel's box. */
      adopt(element: HTMLElement): void {
        document.body.replaceChildren(box);
        box.replaceChildren(element);
        panel = element.querySelector<HTMLElement>("[data-subfeed]");
        if (panel === null) throw new Error("the bubble mounted no sub-feed panel");
        const p = panel;
        p.getBoundingClientRect = () => (p.hidden ? rect(opts.panelTop, 0) : rect(opts.panelTop, PANEL_HEIGHT));
      },
    };
  }

  /** A DOMRect, as far as a reveal reads one. */
  function rect(top: number, height: number): DOMRect {
    return { top, height, bottom: top + height, left: 0, right: 0, width: 0, x: 0, y: top,
      toJSON: () => ({}) };
  }

  /** Click the caret and let the open settle. */
  async function click(bubble: { element: HTMLElement }): Promise<void> {
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    await settle();
  }

  it("does not scroll the opened sub-feed into view (removed trigger)", async () => {
    // Arrange -- the reader holds a place 40px down; the panel will hang from
    // 250, so 150 of its 200px falls below the box's 300px fold. The caret used
    // to scroll the feed by that overhang; the user owns the scroll.
    const view = scrolling({ scrollTop: 40, panelTop: 250 });
    const { bubble } = mount(subagentRow("b1"));
    view.adopt(bubble.element);
    // Act
    await click(bubble);
    // Assert
    expect(view.top()).toBe(40);
  });

  it("does not park the feed at its tail on an expand (removed trigger)", async () => {
    // Arrange -- parked at the tail: 1000 - 300 = 700.
    const view = scrolling({ scrollTop: 700, panelTop: 250 });
    const { bubble } = mount(subagentRow("b1"));
    view.adopt(bubble.element);
    // Act -- the expansion grows the feed by the panel's 200px.
    await click(bubble);
    // Assert -- the caret itself writes nothing; a standing follow, if any, is
    // TailFollow's to keep (scroll.ts).
    expect(view.top()).toBe(700);
  });

  it("does not move the view when the caret COLLAPSES a bubble", async () => {
    // Arrange
    const view = scrolling({ scrollTop: 40, panelTop: 250 });
    const { bubble } = mount(subagentRow("b1"));
    view.adopt(bubble.element);
    await click(bubble);
    // Act -- the second click, which shuts it.
    await click(bubble);
    // Assert
    expect(view.top()).toBe(40);
  });
});

// THE TOKEN COUNT RIDES THE HEAD (owner ruling, 2026-09-14). The subagent head
// carries the daemon's running/settled token sum, and the head is what the
// bubble draws ABOVE its sub-feed in EVERY fold state -- collapse hides the
// panel, never the head -- so the figure stands whether the bubble is collapsed
// or expanded, and a re-push redraws the head with the grown total. These mount
// the REAL subagent head (not the stub the other suites use) to hold that.
describe("mountBubble: the head carries the subagent's token count", () => {
  /** The FeedSubagent inside a fixture subagent row. */
  function subagentOf(row: FeedRow) {
    if (row.row.case === "activity" && row.row.value.unit.case === "subagent") {
      return row.row.value.unit.value;
    }
    throw new Error("fixture is not a synchronous subagent row");
  }

  /** Mount a bubble whose head is the real subagent head. */
  function mountReal(row: FeedRow, h: Harness = harness()) {
    const bubble = mountBubble({
      ctx: h.ctx,
      row,
      rc: rowContext(h.ctx, row),
      head: (r, rc) => drawFeedSubagent(subagentOf(r), rc),
      body: defaultBubbleBody,
      renderers: stubRenderers(),
      revealRow: async () => false,
      bubble: () => {
        throw new Error("no nested bubble in this fixture");
      },
      initialFolded: true,
    });
    document.body.replaceChildren(bubble.element);
    return { bubble, h };
  }

  /** The panel the sub-feed opens into. */
  function panelOf(bubble: { element: HTMLElement }): HTMLElement {
    const panel = bubble.element.querySelector<HTMLElement>("[data-subfeed]");
    if (panel === null) throw new Error("the bubble mounted no sub-feed panel");
    return panel;
  }

  it("shows the token count while the bubble is collapsed", () => {
    // Arrange / Act: a fresh bubble starts collapsed.
    const { bubble } = mountReal(subagentRow("b1", { tokens: "12.4k tok" }));
    // Assert: the head carries the figure with the panel still shut.
    expect(bubble.isExpanded()).toBe(false);
    expect(bubble.element.querySelector(".subagent-tokens")?.textContent).toBe("12.4k tok");
  });

  it("keeps the token count on the head once the bubble is expanded", async () => {
    // Arrange
    const { bubble } = mountReal(subagentRow("b1", { tokens: "12.4k tok" }));
    // Act: open the sub-feed.
    await bubble.expand();
    await settle();
    // Assert: the head -- above the now-shown panel -- still carries the figure.
    expect(panelOf(bubble).hidden).toBe(false);
    expect(bubble.element.querySelector(".subagent-tokens")?.textContent).toBe("12.4k tok");
  });

  it("redraws the head with the grown total on a re-push", () => {
    // Arrange: a live head at one total.
    const { bubble } = mountReal(subagentRow("b1", { tokens: "1.0k tok" }));
    expect(bubble.element.querySelector(".subagent-tokens")?.textContent).toBe("1.0k tok");
    // Act: the same subagent re-pushed with a larger total.
    bubble.update(subagentRow("b1", { tokens: "9.9k tok" }));
    // Assert: the head shows the new figure, not the stale one.
    expect(bubble.element.querySelector(".subagent-tokens")?.textContent).toBe("9.9k tok");
  });
});

describe("mountBubble: the head's title fold follows the bubble's fold", () => {
  /** A head holding one overflowing, card-owned title fold. */
  function titledHead(): { head: () => HTMLElement; title: HTMLElement } {
    const title = document.createElement("span");
    title.className = "shell-command";
    Object.defineProperty(title, "clientHeight", { configurable: true, value: 40 });
    Object.defineProperty(title, "scrollHeight", { configurable: true, value: 120 });
    return {
      title,
      head: () => {
        const el = document.createElement("div");
        el.append(foldTitle(title, "card"));
        return el;
      },
    };
  }

  it("drops the title's has-more when the head click expands the bubble", async () => {
    // Arrange
    const { head, title } = titledHead();
    const { bubble } = mount(subagentRow("b1"), harness(), { head });
    title.classList.add(HAS_MORE_CLASS);

    // Act
    bubble.element.querySelector<HTMLElement>(".bubble-head")?.click();
    await settle();

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("restores the title's has-more when the head click collapses the bubble", async () => {
    // Arrange — an expanded bubble whose title is lifted.
    const { head, title } = titledHead();
    const { bubble } = mount(subagentRow("b1"), harness(), { head });
    const headLine = bubble.element.querySelector<HTMLElement>(".bubble-head");
    headLine?.click();
    await settle();

    // Act
    headLine?.click();
    await settle();

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });
});

describe("mountBubble: a collapse moves nothing the reader is looking at", () => {
  /** A rect, scripted since jsdom lays nothing out. */
  const rect = (top: number, height: number) => (): DOMRect =>
    ({ top, height, bottom: top + height, left: 0, right: 0, width: 0, x: 0, y: top, toJSON: () => ({}) });

  /** Expand a bubble on a scroll box scrolled to 1000, its sub-feed at SUBTOP. */
  async function expandedAt(subTop: number, subHeight: number) {
    const box = document.createElement("div");
    const metrics = { scrollTop: 1000, scrollHeight: 4000, clientHeight: 300 };
    Object.defineProperties(box, {
      scrollTop: { get: () => metrics.scrollTop, set: (next: number) => { metrics.scrollTop = next; } },
      scrollHeight: { get: () => metrics.scrollHeight },
      clientHeight: { get: () => metrics.clientHeight },
    });
    box.getBoundingClientRect = rect(0, 300);
    const tail = new TailFollow(box);
    const { bubble } = mount(subagentRow("b1"), harness(), { scroll: { box, tail } });
    await bubble.expand();
    await settle();
    const sub = bubble.element.querySelector<HTMLElement>(".bubble-subfeed");
    if (sub === null) throw new Error("no sub-feed panel");
    sub.getBoundingClientRect = rect(subTop, subHeight);
    return { bubble, metrics };
  }

  it("compensates a sub-feed wholly above the viewport through the content-preserving cause", async () => {
    // Arrange: a 400px sub-feed ending 10px above the viewport's top.
    const capture = captureLogRecords("debug");
    const { bubble, metrics } = await expandedAt(-410, 400);
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Assert: the view moved back by exactly the 400px that left above it.
    capture.logger.flush();
    await Promise.resolve();
    const moved = capture.sent.filter((record) => record.operation === "scroll.feed-moved");
    expect({ scrollTop: metrics.scrollTop, cause: moved.map((record) => record.context?.cause) }).toEqual({
      scrollTop: 600,
      cause: ["prependCompensation"],
    });
  });

  it("writes no scroll for a sub-feed the reader can see", async () => {
    // Arrange: the sub-feed runs from 50px into the viewport.
    const { bubble, metrics } = await expandedAt(50, 400);
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Assert
    expect(metrics.scrollTop).toBe(1000);
  });

  it("writes no scroll for a sub-feed straddling the viewport's top", async () => {
    // Arrange: the reader is looking into the sub-feed's lower part.
    const { bubble, metrics } = await expandedAt(-200, 400);
    // Act
    bubble.element.querySelector<HTMLElement>("[data-expand]")?.click();
    // Assert
    expect(metrics.scrollTop).toBe(1000);
  });
});

// Regression, 2026-09-28: a merge bubble's tail filed `frameUndecodable` on
// `WatchFeedResponse.row` for the scale push every sub-feed tail receives first.
describe("mountBubble: the non-row frames a sub-feed tail carries", () => {
  async function expandedTail(row: FeedRow) {
    const channels = new Map<string, Channel<WatchFeedResponse>>();
    const channel = new Channel<WatchFeedResponse>();
    channels.set("tok:b1", channel);
    const h = harness({ channels });
    const { bubble } = mount(row, h);
    await bubble.expand();
    await settle();
    return { h, channel, bubble };
  }

  it("reads a merge bubble's feed-text-scale frame as a scale, not an unreadable row", async () => {
    // Arrange
    const { h, channel } = await expandedTail(mergeRow("b1"));
    // Act
    channel.push(pushScale(1.25));
    await settle();
    // Assert
    expect(h.sink.reported).toEqual([]);
  });

  it("applies the feed-text-scale frame a sub-feed tail carries to the document", async () => {
    // Arrange
    document.documentElement.style.removeProperty("--feed-text-scale");
    const { channel } = await expandedTail(subagentRow("b1"));
    // Act
    channel.push(pushScale(1.5));
    await settle();
    // Assert
    expect(document.documentElement.style.getPropertyValue("--feed-text-scale")).toBe("1.5");
  });

  it("still files a selection frame on a sub-feed as unreadable, the selection being root-only", async () => {
    // Arrange
    const { h, channel } = await expandedTail(subagentRow("b1"));
    // Act
    channel.push(pushSelection({ none: "returnToTail" }));
    await settle();
    // Assert
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });
});
