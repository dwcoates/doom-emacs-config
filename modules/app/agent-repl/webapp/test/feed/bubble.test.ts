// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import { FeedIdSchema, FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { mountBubble } from "../../src/feed/bubble.js";
import STYLESHEET from "../../src/styles.css?raw";
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
  opts: {
    folded?: boolean;
    composerFactory?: (host: HTMLElement) => Handle;
    /** What the head states about itself, per draw. */
    states?: (string | null)[];
  } = {},
) {
  const heads: number[] = [];
  const bubble = mountBubble({
    ctx: h.ctx,
    row,
    rc: rowContext(h.ctx, row),
    head: () => {
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
    const bare = create(FeedRowSchema, { id: create(FeedIdSchema, { value: "b1" }) });
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
  // answer is asserted where the browser is, in the playtest's real webview,
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
    const found: { selector: string; body: string }[] = [];
    const pattern = /([^{}]+)\{([^{}]*)\}/g;
    let match: RegExpExecArray | null = pattern.exec(STYLESHEET);
    while (match !== null) {
      found.push({ selector: match[1].trim(), body: match[2] });
      match = pattern.exec(STYLESHEET);
    }
    return found;
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
});
