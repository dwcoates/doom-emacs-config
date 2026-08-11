import { describe, expect, it } from "vitest";
import {
  EDGE_PX,
  PIN_PX,
  SECTION_CLASSES,
  captureFeedAnchor,
  restoreFeedAnchor,
  type AnchorBox,
  inEdgeZone,
  innerScrollerAt,
  TailFollow,
  isPinnedToBottom,
  isScrollBox,
  parkAtTail,
  type ReanchorBox,
  redirectsToFeed,
  sectionFor,
  wheelAction,
  wheelDeltaPx,
  type RevealBlock,
  type RevealTarget,
  revealNode,
} from "../src/scroll.js";
import { renderItem } from "../src/render.js";
import { PermissionItem, TextItem, ToolItem, UserTurnItem } from "../src/store.js";

/** Fake ancestor-chain node: the shape innerScrollerAt walks. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  scrollHeight: number;
  clientHeight: number;
  overflowY: string;
  section: boolean;
}

function node(name: string, over: Partial<FakeNode> = {}): FakeNode {
  return {
    name,
    parentElement: null,
    scrollHeight: 100,
    clientHeight: 100,
    overflowY: "visible",
    section: false,
    ...over,
  };
}

const metrics = (n: FakeNode) => n;
const isSection = (n: FakeNode) => n.section;

describe("isScrollBox", () => {
  it("accepts an overflowing box with overflow-y auto", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "auto" })).toBe(true);
  });

  it("accepts an overflowing box with overflow-y scroll", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "scroll" })).toBe(true);
  });

  it("rejects an overflowing box that does not clip (overflow-y visible)", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "visible" })).toBe(false);
  });

  it("rejects a clipping box whose content fits", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 160, clientHeight: 160, overflowY: "auto" })).toBe(false);
  });

  it("rejects a sub-pixel overflow as rounding noise", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 160.5, clientHeight: 160, overflowY: "auto" })).toBe(false);
  });
});

describe("inEdgeZone", () => {
  const box = { left: 100, right: 500 };

  it("hits inside the left gutter", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, 110, 32)).toBe(true);
  });

  it("hits inside the right gutter", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, 490, 32)).toBe(true);
  });

  it("hits exactly on the gutter boundary", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, 132, 32)).toBe(true);
  });

  it("misses one pixel past the gutter boundary", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, 133, 32)).toBe(false);
  });

  it("misses in the middle of the box", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, 300, 32)).toBe(false);
  });

  it("defaults the gutter width to EDGE_PX", () => {
    // Arrange + Act + Assert
    expect(inEdgeZone(box, box.left + EDGE_PX)).toBe(true);
  });
});

describe("redirectsToFeed", () => {
  const scroller = { left: 100, right: 500 };

  it("redirects a wheel over a section's middle", () => {
    // Arrange + Act + Assert
    expect(redirectsToFeed({ scroller, clientX: 300, feedScrollable: true, edgePx: 32 })).toBe(true);
  });

  it("leaves a wheel over a section's gutter to the section", () => {
    // Arrange + Act + Assert
    expect(redirectsToFeed({ scroller, clientX: 110, feedScrollable: true, edgePx: 32 })).toBe(
      false,
    );
  });

  it("leaves a wheel over no section alone", () => {
    // Arrange + Act + Assert
    expect(
      redirectsToFeed({ scroller: null, clientX: 300, feedScrollable: true, edgePx: 32 }),
    ).toBe(false);
  });

  it("leaves the section scrollable when the feed itself cannot scroll", () => {
    // Arrange + Act + Assert
    expect(redirectsToFeed({ scroller, clientX: 300, feedScrollable: false, edgePx: 32 })).toBe(
      false,
    );
  });
});

describe("wheelDeltaPx", () => {
  it("passes a pixel-mode delta through unchanged", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: 53, deltaMode: 0 }, 600)).toBe(53);
  });

  it("scales a line-mode delta to pixels", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: 3, deltaMode: 1 }, 600)).toBe(48);
  });

  it("scales a page-mode delta by the viewport height", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: -1, deltaMode: 2 }, 600)).toBe(-600);
  });
});

describe("wheelAction", () => {
  const base = {
    scroller: { left: 100, right: 500 },
    clientX: 300,
    deltaY: 40,
    deltaMode: 0,
    feedScrollable: true,
    feedHeight: 600,
    edgePx: 32,
  };

  it("returns the feed delta for a wheel over a section's middle", () => {
    // Arrange + Act + Assert
    expect(wheelAction(base)).toBe(40);
  });

  it("returns null for a wheel over a section's gutter", () => {
    // Arrange + Act + Assert
    expect(wheelAction({ ...base, clientX: 110 })).toBeNull();
  });

  it("returns null for a purely horizontal wheel", () => {
    // Arrange + Act + Assert
    expect(wheelAction({ ...base, deltaY: 0 })).toBeNull();
  });

  it("converts the delta to pixels before handing it to the feed", () => {
    // Arrange + Act + Assert
    expect(wheelAction({ ...base, deltaY: 2, deltaMode: 1 })).toBe(32);
  });
});

describe("innerScrollerAt", () => {
  it("returns null when no ancestor below the feed scrolls", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const card = node("card", { parentElement: feed });
    const text = node("text", { parentElement: card });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)).toBeNull();
  });

  it("finds the scrolling section above the wheel target", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const section = node("section", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "auto",
    });
    const text = node("text", { parentElement: section });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)?.name).toBe("section");
  });

  it("returns the innermost of nested scrolling sections", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const outer = node("outer", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "auto",
    });
    const inner = node("inner", {
      parentElement: outer,
      scrollHeight: 300,
      clientHeight: 80,
      overflowY: "auto",
    });
    // Act + Assert
    expect(innerScrollerAt(inner, feed, metrics)?.name).toBe("inner");
  });

  it("never returns the feed itself", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    // Act + Assert
    expect(innerScrollerAt(feed, feed, metrics)).toBeNull();
  });

  it("returns null for a wheel with no target element", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    // Act + Assert
    expect(innerScrollerAt(null, feed, metrics)).toBeNull();
  });
});

describe("sectionFor", () => {
  it("returns the card enclosing the scroll box, so the bars span the whole section", () => {
    // Arrange — a Bash card whose output box is one of several sub-boxes.
    const feed = node("feed");
    const card = node("card", { parentElement: feed, section: true });
    const output = node("bash-output", { parentElement: card });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("card");
  });

  it("returns the innermost card when cards nest", () => {
    // Arrange
    const feed = node("feed");
    const outer = node("outer-card", { parentElement: feed, section: true });
    const inner = node("inner-card", { parentElement: outer, section: true });
    const output = node("bash-output", { parentElement: inner });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("inner-card");
  });

  it("returns the scroll box itself when it is the card", () => {
    // Arrange
    const feed = node("feed");
    const card = node("card", { parentElement: feed, section: true });
    // Act + Assert
    expect(sectionFor(card, feed, isSection).name).toBe("card");
  });

  it("falls back to the scroll box when no card encloses it", () => {
    // Arrange
    const feed = node("feed");
    const output = node("bare-output", { parentElement: feed });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });

  it("never returns the feed, even when the feed matches", () => {
    // Arrange — the feed is off-limits: lighting it would frame the viewport.
    const feed = node("feed", { section: true });
    const output = node("bare-output", { parentElement: feed });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });
});

describe("SECTION_CLASSES", () => {
  it("names the class the renderer puts on a tool card, which holds the tool scroll boxes", () => {
    // Arrange
    const item: ToolItem = {
      kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      inputJson: "",
      input: { command: "ls" },
      inputDone: true,
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="${SECTION_CLASSES[0]} `);
  });

  it("names the class the renderer puts on a permission card, which holds a preview scroll box", () => {
    // Arrange
    const item: PermissionItem = {
      kind: "permission",
      requestId: "p1",
      toolUseId: "t1",
      toolName: "Write",
      input: {},
      preview: { kind: "generic", summary: "a long preview" },
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="${SECTION_CLASSES[1]} `);
  });

  it("names the class the renderer puts on a response bubble, which holds its capped body", () => {
    // Arrange — the response body caps at 25 lines and scrolls past that, so
    // the bubble is a section: the lit gutters must ride the bubble's own
    // edges rather than sit inset at its body's.
    const item: TextItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "an answer",
      done: true,
      ts: "2026-05-24T10:00:00.000Z",
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="${SECTION_CLASSES[2]} `);
  });

  it("names the class the renderer puts on a prompt bubble, which holds its capped body", () => {
    // Arrange — the prompt body caps at the same 25 lines and scrolls past
    // that, so the prompt bubble is a section on the same terms the response
    // bubble is.
    const item: UserTurnItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "a prompt" }],
      ts: "2026-05-24T10:00:00.000Z",
    };
    // Act + Assert
    expect(renderItem(item)).toContain(`class="${SECTION_CLASSES[2]} `);
  });
});

describe("isPinnedToBottom", () => {
  it("pins a feed sitting exactly at its bottom", () => {
    // Arrange + Act + Assert
    expect(isPinnedToBottom({ scrollHeight: 900, scrollTop: 600, clientHeight: 300 })).toBe(true);
  });

  it("pins a feed within the slack of its bottom", () => {
    // Arrange — 20px of unread tail, inside the 40px slack.
    expect(isPinnedToBottom({ scrollHeight: 900, scrollTop: 580, clientHeight: 300 })).toBe(true);
  });

  it("unpins a feed the user scrolled up past the slack", () => {
    // Arrange — 300px of unread tail, well beyond the slack.
    expect(isPinnedToBottom({ scrollHeight: 900, scrollTop: 300, clientHeight: 300 })).toBe(false);
  });

  it("pins a feed too short to scroll at all", () => {
    // Arrange + Act + Assert
    expect(isPinnedToBottom({ scrollHeight: 300, scrollTop: 0, clientHeight: 300 })).toBe(true);
  });

  it("honors a caller-supplied slack over PIN_PX", () => {
    // Arrange — 20px of tail: pinned at the default slack, not at 10px.
    expect(isPinnedToBottom({ scrollHeight: 900, scrollTop: 580, clientHeight: 300 }, 10)).toBe(
      false,
    );
  });

  it("defaults its slack to PIN_PX", () => {
    // Arrange — one pixel short of PIN_PX of unread tail.
    const pos = { scrollHeight: 900, scrollTop: 600 - (PIN_PX - 1), clientHeight: 300 };
    // Act + Assert
    expect(isPinnedToBottom(pos)).toBe(isPinnedToBottom(pos, PIN_PX));
  });
});

describe("parkAtTail", () => {
  it("jumps a scrolled-up box straight to its tail", () => {
    // Arrange
    const box = { scrollTop: 120, scrollHeight: 900 };
    // Act
    parkAtTail(box);
    // Assert — one assignment, so the tail is there on the next frame.
    expect(box.scrollTop).toBe(900);
  });

  it("leaves a box already at its tail untouched", () => {
    // Arrange
    const box = { scrollTop: 900, scrollHeight: 900 };
    // Act
    parkAtTail(box);
    // Assert
    expect(box.scrollTop).toBe(900);
  });

  it("parks a box too short to scroll at zero", () => {
    // Arrange — an empty feed: scrollHeight is the viewport, scrollTop stays 0.
    const box = { scrollTop: 0, scrollHeight: 0 };
    // Act
    parkAtTail(box);
    // Assert
    expect(box.scrollTop).toBe(0);
  });
});


/**
 * THE SINGLE OWNER of the feed's tail-follow decision. Every case drives the
 * events by hand, so the ordering the browser would decide is the thing each
 * test states rather than something the test hopes for.
 */
describe("TailFollow", () => {
  /** A follow owner plus the two triggers it subscribed to. */
  const armed = (
    box: ReanchorBox,
    pinPx?: number,
  ): { tail: TailFollow; scroll: () => void; resize: () => void } => {
    let onScroll = (): void => {};
    let onResize = (): void => {};
    const tail = new TailFollow(box, pinPx);
    tail.observe(
      (cb) => {
        onScroll = cb;
      },
      (cb) => {
        onResize = cb;
      },
    );
    return { tail, scroll: () => onScroll(), resize: () => onResize() };
  };

  /** A box the reader is following the tail of: 700 + 300 viewport = 1000. */
  const atTail = (): ReanchorBox => ({ scrollTop: 700, scrollHeight: 1000, clientHeight: 300 });

  it("follows the tail when built on a box parked at its bottom", () => {
    // Arrange + Act + Assert
    expect(new TailFollow(atTail()).isFollowing()).toBe(true);
  });

  it("does not follow when built on a box the reader left scrolled up", () => {
    // Arrange + Act + Assert
    expect(
      new TailFollow({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 }).isFollowing(),
    ).toBe(false);
  });

  it("stops following on a scroll UP that stays inside the pin band", () => {
    // Arrange — THE REPORTED BUG. A trackpad flick upward begins with a few
    // px, well inside PIN_PX, and a geometry sample called that "still pinned"
    // and parked the feed back down under the gesture.
    const box = atTail();
    const a = armed(box);
    // Act — 10px up, far short of the 40px slack band.
    box.scrollTop = 690;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("reports the reader's live position even before their scroll event lands", () => {
    // Arrange — the browser dispatches scroll asynchronously, so a render can
    // run between the gesture and its event. Reading the last-seen event there
    // would answer about a position the reader has already left.
    const box = atTail();
    const a = armed(box);
    // Act — the gesture happened; `a.scroll()` is deliberately NOT fired.
    box.scrollTop = 400;
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("keeps a scrolled-away reader unfollowed across a burst of re-renders", () => {
    // Arrange — the reader scrolls up once, then the feed streams on.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 400;
    a.scroll();
    // Act — every render asks the owner before it would park.
    const answers: boolean[] = [];
    for (let i = 0; i < 20; i++) {
      box.scrollHeight += 120;
      answers.push(a.tail.isFollowing());
    }
    // Assert — not once re-enabled, across the whole burst.
    expect(answers.some(Boolean)).toBe(false);
  });

  it("resumes following when the reader scrolls back down to the tail", () => {
    // Arrange — away from the tail, then returning to it.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 200;
    a.scroll();
    // Act
    box.scrollTop = 700;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("does not resume following on a downward scroll short of the tail", () => {
    // Arrange — the reader is paging down through history, not chasing it.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 100;
    a.scroll();
    // Act
    box.scrollTop = 400;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("ignores the scroll event its own park emits", () => {
    // Arrange — the browser dispatches scroll asynchronously after a write.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.park();
    a.scroll();
    // Assert — the park's own event must not be read as the reader moving.
    expect([a.tail.isFollowing(), box.scrollTop]).toEqual([true, 1000]);
  });

  it("does not end a follow when a restore places the box", () => {
    // Arrange — a rebuild restoring the reader's place writes scrollTop; the
    // owner must not read its own write as the reader changing their mind.
    const box = atTail();
    const a = armed(box);
    // Act
    a.tail.place(400);
    a.scroll();
    // Assert
    expect([a.tail.isFollowing(), box.scrollTop]).toEqual([true, 400]);
  });

  it("does not begin a follow when a place lands the box on the tail", () => {
    // Arrange — a backfill shifting the view down by the height it grew above
    // the viewport can land exactly at the bottom. That is arithmetic, not the
    // reader asking to follow again.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.place(700);
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("does not begin a follow when a shift lands the box on the tail", () => {
    // Arrange — a backfill's growth compensation can add exactly enough to
    // reach the bottom. That is arithmetic about content, not the reader
    // asking to follow again.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.shift(600);
    a.scroll();
    // Assert
    expect([a.tail.isFollowing(), box.scrollTop]).toEqual([false, 700]);
  });

  it("shifts from where the reader now is, not from where it last wrote", () => {
    // Arrange — the reader scrolled during the render whose growth this
    // compensates for, and their gesture's event has not been dispatched yet.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 500;
    // Act
    a.tail.shift(40);
    // Assert
    expect(box.scrollTop).toBe(540);
  });

  it("stops following when the reader opens a nested view to read it", () => {
    // Arrange + Act
    const a = armed(atTail());
    a.tail.release();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("re-parks a following box when the resize lands after the snap", () => {
    // Arrange — the switch snap landed first, then the webview was resized.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    a.tail.park();
    // Act — the relayout grows the scrollable height under a fixed scrollTop.
    box.clientHeight = 200;
    box.scrollHeight = 2600;
    a.resize();
    // Assert — RELIABLY at the bottom, not merely near where the snap left it.
    expect(box.scrollTop).toBe(2600);
  });

  it("parks on the settled layout when the snap lands after the resize", () => {
    // Arrange — the other order the switch can produce, which no side controls.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    box.clientHeight = 200;
    box.scrollHeight = 2600;
    a.resize();
    a.tail.park();
    // Assert
    expect(box.scrollTop).toBe(2600);
  });

  it("leaves a scrolled-away box where it is when a resize arrives", () => {
    // Arrange
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 200;
    a.scroll();
    // Act
    box.scrollHeight = 1400;
    a.resize();
    // Assert
    expect(box.scrollTop).toBe(200);
  });

  it("does not read a resize's own downward clamp as the reader scrolling up", () => {
    // Arrange — a shrinking viewport clamps scrollTop by itself. Reconciling
    // that would drop the follow the workspace switch just asked for.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    a.tail.park();
    // Act — the relayout shrank the feed and the browser clamped scrollTop.
    box.scrollHeight = 600;
    box.clientHeight = 200;
    box.scrollTop = 400;
    a.resize();
    // Assert
    expect([a.tail.isFollowing(), box.scrollTop]).toEqual([true, 600]);
  });

  it("honors a custom pin window when deciding a scroll reached the tail", () => {
    // Arrange — a 10px window: 685 is 15px short of the 700 bottom.
    const box = atTail();
    const a = armed(box, 10);
    box.scrollTop = 200;
    a.scroll();
    // Act
    box.scrollTop = 685;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("baselines on the box's live position before any event arrives", () => {
    // Arrange — a box already parked deep in a long feed, as the boot render
    // leaves it. A baseline of 0 here would make the very first reconcile
    // compute a huge upward delta out of nothing.
    const box = { scrollTop: 9700, scrollHeight: 10000, clientHeight: 300 };
    const a = armed(box);
    // Act — the first read, with nothing having moved.
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("keeps a first upward gesture when a resize lands before its scroll event", () => {
    // Arrange — THE RESIDUAL. The browser dispatches scroll asynchronously, so
    // a relayout still settling after a load can reach the owner before the
    // gesture's own event does. It used to park on that stale decision, which
    // is why the FIRST upward scroll after a load was yanked and no later one.
    const box = atTail();
    const a = armed(box);
    // Act — the reader moves up; the resize, not the scroll event, arrives.
    box.scrollTop = 660;
    a.resize();
    // Assert
    expect([a.tail.isFollowing(), box.scrollTop]).toEqual([false, 660]);
  });

  it("does not read a hydration shrink's clamp as the reader scrolling up", () => {
    // Arrange — deferred items settling to their real heights shortens the
    // feed under a parked box, and the browser clamps scrollTop down with it.
    const box = atTail();
    const a = armed(box);
    // Act — the feed shrank by 300; the box rode the clamp down, untouched.
    box.scrollHeight = 700;
    box.scrollTop = 400;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("leaves a reader who scrolled up unfollowed when hydration then grows", () => {
    // Arrange — the reader left the tail during the load.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 200;
    a.scroll();
    // Act — deferred content lands and grows the feed beneath them.
    box.scrollHeight = 4000;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("does not resume following when the feed ends exactly where the reader sits", () => {
    // Arrange — the reader is scrolled up, 300 short of the bottom.
    const box = { scrollTop: 400, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act — the feed SHRINKS to end exactly where the box already sits, so the
    // box is at the tail without the reader having gone there.
    box.scrollHeight = 700;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });
});

/**
 * The shared "bring this into view" primitive. The roster's agent reveal,
 * the keyboard cycle, and any later match-stepping all move the feed
 * through here, so that they cannot drift into moving it differently.
 */
describe("revealNode", () => {
  /** A node recording how it was asked to scroll itself into view. */
  const spy = (): { calls: RevealBlock[] } & RevealTarget => {
    const calls: RevealBlock[] = [];
    return { calls, scrollIntoView: (arg) => calls.push(arg.block) };
  };

  it("scrolls as little as it must by default, leaving a visible target where it is", () => {
    const node = spy();
    revealNode(node);
    expect(node.calls).toEqual(["nearest"]);
  });

  it("puts a jumped-to node flush with the top when the caller asks for it", () => {
    const node = spy();
    revealNode(node, "start");
    expect(node.calls).toEqual(["start"]);
  });
});

/**
 * THE READER'S PLACE ACROSS A REBUILD.
 *
 * A resync re-delivers the same history and the feed rebuilds from nothing.
 * Nothing about the data changed, so nothing about what the reader is looking
 * at may change either — the "jerk and reset" the user reported is a rebuild
 * that dropped them wherever the new layout landed.
 */
describe("feed anchoring across a rebuild", () => {
  /** A scroll box whose items are at fixed offsets, mounted by key. */
  const box = (over: Partial<AnchorBox> & { offsets?: Record<string, number> } = {}): AnchorBox => {
    const offsets = over.offsets ?? {};
    return {
      scrollTop: over.scrollTop ?? 0,
      scrollHeight: over.scrollHeight ?? 1000,
      clientHeight: over.clientHeight ?? 200,
      querySelector: (selector: string) => {
        const key = /\[data-key="(.*)"\]/.exec(selector)?.[1] ?? "";
        const offsetTop = offsets[key];
        return offsetTop === undefined ? null : { offsetTop };
      },
    };
  };

  it("anchors on the topmost item still on screen", () => {
    // Arrange — the reader is 300px down; a, at 100, has scrolled away.
    const b = box({ scrollTop: 300 });
    // Act
    const anchor = captureFeedAnchor(
      b,
      [
        { key: "a", offsetTop: 100 },
        { key: "b", offsetTop: 320 },
        { key: "c", offsetTop: 600 },
      ],
      false,
    );
    // Assert
    expect(anchor).toEqual({ key: "b", offsetPx: 20, pinned: false });
  });

  it("restores the anchor item to the same offset from the viewport top", () => {
    // Arrange — the rebuild moved b from 320 to 480: every height above it
    // changed, and the reader must not notice.
    const b = box({ scrollTop: 0, offsets: { b: 480 } });
    // Act
    restoreFeedAnchor(b, { key: "b", offsetPx: 20, pinned: false }, new TailFollow(b));
    // Assert
    expect(b.scrollTop).toBe(460);
  });

  it("puts a reader who was following the tail back at the tail", () => {
    // Arrange — pinned readers want the newest content, not a fixed pixel.
    const b = box({ scrollTop: 0, scrollHeight: 2000 });
    // Act
    restoreFeedAnchor(b, { key: "", offsetPx: 0, pinned: true }, new TailFollow(b));
    // Assert
    expect(b.scrollTop).toBe(2000);
  });

  it("anchors on the tail when the owner says the reader is following it", () => {
    // Arrange — the owner's latched answer, not this function's own reading.
    const b = box({ scrollTop: 800 - PIN_PX + 1, scrollHeight: 1000, clientHeight: 200 });
    // Act
    const anchor = captureFeedAnchor(b, [{ key: "a", offsetTop: 900 }], true);
    // Assert
    expect(anchor?.pinned).toBe(true);
  });

  it("anchors on an item for a reader inside the pin band who stopped following", () => {
    // Arrange — THE ANCHOR'S HALF OF THE BUG. Geometry called a reader who had
    // nudged a few px off the bottom "pinned", and the restore then parked them
    // at the tail — the same yank, on a rebuild instead of a render. The owner
    // says otherwise, and the owner is the only one asked.
    const b = box({ scrollTop: 800 - PIN_PX + 1, scrollHeight: 1000, clientHeight: 200 });
    // Act
    const anchor = captureFeedAnchor(b, [{ key: "a", offsetTop: 900 }], false);
    // Assert
    expect(anchor).toEqual({ key: "a", offsetPx: 900 - b.scrollTop, pinned: false });
  });

  it("leaves the box alone when the anchor item did not survive the rebuild", () => {
    // Arrange — a clear discarded the item the reader was on; inventing a
    // position for it would move them somewhere they never were.
    const b = box({ scrollTop: 77, offsets: {} });
    // Act
    const restored = restoreFeedAnchor(
      b,
      { key: "gone", offsetPx: 20, pinned: false },
      new TailFollow(b),
    );
    // Assert
    expect([restored, b.scrollTop]).toEqual([false, 77]);
  });

  it("captures nothing for an empty feed, so a rebuild has nothing to restore", () => {
    // Arrange — scrolled up in a box with no items at all.
    const b = box({ scrollTop: 10, scrollHeight: 1000, clientHeight: 200 });
    // Act
    const anchor = captureFeedAnchor(b, [], false);
    // Assert
    expect(anchor).toBeNull();
  });
});

/**
 * THE DRIFT GUARD ON THE OWNER.
 *
 * The defect `TailFollow` exists to end was not a wrong formula —
 * `isPinnedToBottom` was right about the pixels every time it was asked. It was
 * that FOUR mechanisms asked it independently and acted on their own answers:
 * the render's per-render sample, a separate nested-view freeze flag, the
 * relayout re-anchor's latch, and the rebuild anchor's own pin test. Three of
 * them wrote scrollTop. A user scrolling up was arguing with all of them.
 *
 * So the primitives are the owner's alone. Any module that reaches past
 * `TailFollow` for the raw pin test or the raw park is re-opening the question,
 * and that is what this catches — at the import, before it can be believed.
 */
describe("the tail-follow decision has exactly one owner", () => {
  const sources = import.meta.glob("../src/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  /** Every src module's text except scroll.ts, which IS the owner. */
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));

  it("scans a real set of sibling modules, so an empty glob cannot pass it", () => {
    // Arrange + Act + Assert — the guard is worthless if it inspects nothing.
    expect(others.length).toBeGreaterThan(50);
  });

  it("lets no other module reach for the raw pin test", () => {
    // Act — a module deriving "am I at the bottom" for itself is a second owner.
    const offenders = others.filter(([, src]) => /\bisPinnedToBottom\b/.test(src));
    // Assert
    expect(offenders.map(([p]) => p)).toEqual([]);
  });

  it("lets no other module reach for the raw tail park", () => {
    // Act — every park must go through the owner, which latches the intent the
    // park expresses; a bare one moves pixels and leaves the decision stale.
    const offenders = others.filter(([, src]) => /\bparkAtTail\b/.test(src));
    // Assert
    expect(offenders.map(([p]) => p)).toEqual([]);
  });
});
