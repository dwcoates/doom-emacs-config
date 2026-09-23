// @vitest-environment jsdom
//
// The pure decisions in this module need no dom, but `observeScrollBox` is the
// one DOM-facing thing in it: it subscribes a real element's scroll events and
// a real `ResizeObserver` to the tail owner, and the subscription being wired
// to THAT element is half of what the footer-occlusion cases assert.
import { afterEach, describe, expect, it } from "vitest";
import {
  PIN_PX,
  armedWheelAction,
  feedTopChanged,
  installIntentScroll,
  innerScrollerAt,
  TailFollow,
  isPinnedToBottom,
  isScrollBox,
  parkAtTail,
  type ReanchorBox,
  sectionFor,
  sectionTakesWheel,
  wheelDeltaPx,
  type RevealBlock,
  type RevealTarget,
  revealNode,
  revealDelta,
  centerDelta,
  revealInBox,
  observeScrollBox,
} from "../src/scroll.js";
import { fireResize } from "./resize-observer.js";
import { installClickExpand } from "../src/expand.js";

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

  it("rejects a collapsed box that clips without scrolling (overflow-y hidden)", () => {
    // Owner ruling, 2026-09-15: a collapsed bubble/section clips rather than
    // scrolls, so the intent-arm gate (installIntentScroll) must never arm it —
    // its wheel always redirects to the feed.
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "hidden" })).toBe(false);
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

describe("sectionTakesWheel", () => {
  it("keeps the wheel when the armed box is the one under it", () => {
    // Arrange — the reader deliberately entered this box, so it owns the wheel.
    const box = { name: "armed" };
    // Act + Assert
    expect(sectionTakesWheel(box, box)).toBe(true);
  });

  it("hands the wheel off when a DIFFERENT box is armed", () => {
    // Arrange — the wheel landed over a box the reader never entered.
    const armed = { name: "armed" };
    const under = { name: "other" };
    // Act + Assert
    expect(sectionTakesWheel(armed, under)).toBe(false);
  });

  it("hands the wheel off when nothing is armed", () => {
    // Arrange — a box the FEED scrolled under a still cursor is never armed.
    const under = { name: "scrolled-into" };
    // Act + Assert
    expect(sectionTakesWheel(null, under)).toBe(false);
  });

  it("keeps nothing when the wheel is over no box at all", () => {
    // Arrange — over bare feed there is no section to keep the wheel.
    const armed = { name: "armed" };
    // Act + Assert
    expect(sectionTakesWheel(armed, null)).toBe(false);
  });
});

describe("armedWheelAction", () => {
  const base = {
    armed: { name: "armed" },
    wheelScroller: { name: "other" } as { name: string } | null,
    feedScrollable: true,
    deltaY: 40,
    deltaMode: 0,
    feedHeight: 600,
  };

  it("redirects a wheel over a non-armed box to the feed", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction(base)).toBe(40);
  });

  it("leaves a wheel over the armed box to the browser", () => {
    // Arrange — the armed box keeps its own wheel.
    const armed = { name: "armed" };
    // Act + Assert
    expect(armedWheelAction({ ...base, armed, wheelScroller: armed })).toBeNull();
  });

  it("leaves a wheel over no box to the browser", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, wheelScroller: null })).toBeNull();
  });

  it("leaves a purely horizontal wheel to the browser", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, deltaY: 0 })).toBeNull();
  });

  it("leaves the box alone when the feed itself cannot scroll", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, feedScrollable: false })).toBeNull();
  });

  it("converts the delta to pixels before handing it to the feed", () => {
    // Arrange + Act + Assert — line-mode delta 2 at LINE_PX 16.
    expect(armedWheelAction({ ...base, deltaY: 2, deltaMode: 1 })).toBe(32);
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

  it("skips a collapsed (clipping) bubble, so its wheel redirects to the feed", () => {
    // Arrange — a collapsed bubble scroll box (overflow-y hidden) whose content
    // exceeds it; the intent-arm gate must not treat it as a scroller (owner
    // ruling, 2026-09-15: collapsed boxes never scroll).
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const collapsed = node("collapsed", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "hidden",
    });
    const text = node("text", { parentElement: collapsed });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)).toBeNull();
  });

  it("arms an EXPANDED bubble that still overflows its 50vh cap", () => {
    // Arrange — an expanded bubble scroll box: overflow-y auto and content past
    // the 50vh cap, the only shape the intent-arm scroll applies to.
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const expanded = node("expanded", {
      parentElement: feed,
      scrollHeight: 900,
      clientHeight: 400,
      overflowY: "auto",
    });
    const text = node("text", { parentElement: expanded });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)?.name).toBe("expanded");
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
  /**
   * A follow owner plus the three triggers it subscribed to.
   *
   * `gesture` is the READER moving the box: their input reaches it first and
   * the scroll event follows, which is the order the browser delivers them in
   * and the order the owner's attribution rule depends on. `scroll` alone is
   * therefore a movement with no reader behind it -- the box's own clamp.
   */
  const armed = (
    box: ReanchorBox,
    pinPx?: number,
  ): {
    tail: TailFollow;
    scroll: () => void;
    resize: () => void;
    input: () => void;
    gesture: () => void;
  } => {
    let onScroll = (): void => {};
    let onResize = (): void => {};
    let onInput = (): void => {};
    const tail = new TailFollow(box, pinPx);
    tail.observe(
      (cb) => {
        onScroll = cb;
      },
      (cb) => {
        onResize = cb;
      },
      (cb) => {
        onInput = cb;
      },
    );
    return {
      tail,
      scroll: () => onScroll(),
      resize: () => onResize(),
      input: () => onInput(),
      gesture: () => {
        onInput();
        onScroll();
      },
    };
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
    a.gesture();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("reports the reader's live position even before their scroll event lands", () => {
    // Arrange — the browser dispatches scroll asynchronously, so a render can
    // run between the gesture and its event. Reading the last-seen event there
    // would answer about a position the reader has already left.
    const box = atTail();
    const a = armed(box);
    // Act — the gesture happened; `a.scroll()` is deliberately NOT fired. The
    // reader's INPUT has landed, because a wheel or a key precedes the movement
    // it causes; only the browser's scroll event is still outstanding.
    a.input();
    box.scrollTop = 400;
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("keeps a scrolled-away reader unfollowed across a burst of re-renders", () => {
    // Arrange — the reader scrolls up once, then the feed streams on.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 400;
    a.gesture();
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
    a.gesture();
    // Act
    box.scrollTop = 700;
    a.gesture();
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("does not resume following on a downward scroll short of the tail", () => {
    // Arrange — the reader is paging down through history, not chasing it.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 100;
    a.gesture();
    // Act
    box.scrollTop = 400;
    a.gesture();
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
    a.gesture();
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
    a.gesture();
    // Act
    box.scrollTop = 685;
    a.gesture();
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
    // Their input reached the box first, which is what makes the movement
    // theirs rather than the box's own clamp.
    a.input();
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
    a.gesture();
    // Act — deferred content lands and grows the feed beneath them.
    box.scrollHeight = 4000;
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("keeps the follow when a clamp is only seen after the content regrew", () => {
    // Arrange — THE MEASURED DEFECT (the hibernated tab's playbook under load,
    // `scrollTop=52 scrollHeight=853 clientHeight=637`). 52 was the reachable
    // extent one turn earlier: the feed shrank, the box rode its clamp down to
    // 52, and by the time anything reconciled, the extent had already regrown.
    // A baseline still standing at the old tail then read a gesture nobody
    // made, and only arriving back at the tail resumes a follow -- so the tail
    // stayed 400px below the fold while the rows kept coming.
    const box = { scrollTop: 689, scrollHeight: 1326, clientHeight: 637 };
    const a = armed(box);
    a.tail.park();
    // Act — shrink, clamp, and regrowth, with no reconcile in between.
    box.scrollHeight = 853;
    box.scrollTop = 52;
    a.scroll();
    // Assert — no input reached the box, so nothing there was the reader.
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("re-lands the tail on the next size change after such a clamp", () => {
    // Arrange — the same clamp, then the footer or a row changes size.
    const box = { scrollTop: 689, scrollHeight: 1326, clientHeight: 637 };
    const a = armed(box);
    a.tail.park();
    box.scrollHeight = 853;
    box.scrollTop = 52;
    a.scroll();
    // Act
    box.scrollHeight = 1099;
    a.resize();
    // Assert — parked, rather than left 410px below the fold. The write is
    // `scrollHeight` because this fake does not clamp, as a browser's box
    // would; what is under test is that the re-park happened at all.
    expect(box.scrollTop).toBe(1099);
  });

  it("decides nothing on an input that moved the box nowhere", () => {
    // Arrange — a wheel the box had no room to answer, a key that typed into a
    // composer inside it. The input arms the attribution; it is not itself one.
    const box = atTail();
    const a = armed(box);
    // Act
    a.input();
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("stops reading the reader's earlier input once a park has re-landed", () => {
    // Arrange — the reader scrolled up, then a "show me the newest" act parked
    // the tail: their gesture spoke about a position that no longer exists.
    const box = atTail();
    const a = armed(box);
    box.scrollTop = 200;
    a.gesture();
    a.tail.park();
    // Act — the box's own clamp, after the park.
    box.scrollHeight = 700;
    box.scrollTop = 400;
    a.scroll();
    // Assert — the clamp is not the stale gesture's doing.
    expect(a.tail.isFollowing()).toBe(true);
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
  // `**` rather than `*`: the rebuilt webapp puts each component in its own
  // `src/<component>/` directory, and a flat glob would stop scanning exactly
  // the modules most likely to re-open the question.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  /** Every src module's text except scroll.ts, which IS the owner. */
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));

  it("scans a real set of sibling modules, so an empty glob cannot pass it", () => {
    // Arrange + Act + Assert — the guard is worthless if it inspects nothing.
    // The floor is well under the module count on purpose: it exists to catch a
    // glob that resolved to nothing, and the strip to the chassis legitimately
    // took src from ninety-odd modules to a handful, so a floor tracking the
    // real count would have to be retuned by every agent that adds a component.
    expect(others.length).toBeGreaterThan(5);
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

describe("a load-more prepend does not jump the viewport", () => {
  it("a NEW item at the feed's top is what says content was inserted above", () => {
    // Arrange / Act / Assert
    expect(feedTopChanged("older-1", "b-tail")).toBe(true);
  });

  it("an unchanged top item is NOT a prepend, whatever the feed's height did", () => {
    // Arrange — a card expanding or a deferred item settling grows the feed
    // without inserting anything above the reader; compensating those would
    // move the reader instead.
    // Act / Assert
    expect(feedTopChanged("b-tail", "b-tail")).toBe(false);
  });

  it("an empty feed BEFORE the render has no reading position to preserve", () => {
    // Arrange / Act / Assert
    expect(feedTopChanged(null, "b-tail")).toBe(false);
  });

  it("an empty feed AFTER the render has no anchor item to restore", () => {
    // Arrange / Act / Assert
    expect(feedTopChanged("b-tail", null)).toBe(false);
  });
});

describe("sectionFor detached box", () => {
  it("falls back to the box when the chain runs out before reaching the feed", () => {
    // Arrange — a box whose ancestors end at a root that is NOT the feed, the
    // shape a section removed from the document leaves mid-render.
    const feed = node("feed");
    const orphanRoot = node("orphan-root");
    const output = node("bare-output", { parentElement: orphanRoot });
    // Act + Assert — no card was found and the walk still terminated.
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });
});

describe("TailFollow on a box shorter than its viewport", () => {
  it("clamps the reconcile baseline at zero rather than a negative reach", () => {
    // Arrange — a feed with less content than viewport: scrollHeight minus
    // clientHeight is NEGATIVE, so only the Math.max floor keeps the baseline
    // inside the box's real range.
    const box: ReanchorBox = { scrollTop: 0, scrollHeight: 120, clientHeight: 300 };
    const tail = new TailFollow(box);
    // Act — content arrives, still short of the viewport; nothing moved.
    box.scrollHeight = 200;
    // Assert — the follow survives, unclamped arithmetic would have ended it.
    expect(tail.isFollowing()).toBe(true);
  });
});

/**
 * `observeScrollBox` — THE FOOTER OCCLUSION, at its source.
 *
 * The docked progress footer is laid out BELOW the scroll box, so the box's
 * height is already the window's minus the footer's: the space is reserved by
 * the layout and never needed reserving again. What went stale was the
 * POSITION — a footer that appeared or grew AFTER the render that parked the
 * tail shrank the box under a `scrollTop` nobody moved, leaving the last bubble
 * that many pixels below the fold and clipped by the footer's top edge.
 *
 * These drive a real element with scripted geometry, because the subscription
 * being wired to THAT element is half of what is under test.
 */
describe("observeScrollBox", () => {
  /**
   * Let the MutationObserver keeping the watched-child set in step with the
   * DOM deliver. Its callback is a microtask checkpoint rather than a
   * synchronous call, so a child appended a line earlier is not yet watched.
   */
  function flushMutations(): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, 0));
  }

  /**
   * A real element that answers scroll geometry, since jsdom lays nothing out.
   * `scrollTop` clamps into the scrollable range on write, as a browser's does
   * — which is what turns `parkAtTail`'s "assign scrollHeight" into the bottom.
   */
  function scrollBox(init: { scrollHeight: number; clientHeight: number; scrollTop: number }) {
    const element = document.createElement("div");
    // The box's CONTENT root, as the real markup has one (`#feed` inside
    // `#feed-scroll`): the box's scrollHeight is what this child measures, so
    // growth is reported against the child and never against the box.
    const content = document.createElement("main");
    element.append(content);
    let scrollHeight = init.scrollHeight;
    let clientHeight = init.clientHeight;
    let scrollTop = init.scrollTop;
    Object.defineProperties(element, {
      scrollHeight: { get: () => scrollHeight },
      clientHeight: { get: () => clientHeight },
      scrollTop: {
        get: () => scrollTop,
        set: (next: number) => {
          scrollTop = Math.max(0, Math.min(next, scrollHeight - clientHeight));
        },
      },
    });
    return {
      element,
      content,
      /** The footer appearing or growing by PX: the box loses that height. */
      loseHeight: (px: number) => {
        clientHeight -= px;
      },
      /** A deferred expansion settling: the content gains PX, the box none. */
      contentGrows: (px: number) => {
        scrollHeight += px;
        scrollTop = Math.max(0, Math.min(scrollTop, scrollHeight - clientHeight));
      },
      top: () => scrollTop,
    };
  }

  it("re-lands the tail on the shrunken viewport when the footer takes height", () => {
    // Arrange — a reader parked at the tail: 700 + 300 viewport = 1000.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act — the footer settles after the render and eats 48px of the box.
    box.loseHeight(48);
    fireResize(box.element);
    // Assert — the tail is the new bottom, so the last bubble clears the strip.
    expect(box.top()).toBe(748);
  });

  it("re-lands by exactly the height the footer took", () => {
    // Arrange — the same box, so the delta is the only thing being read.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const before = box.top();
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act
    box.loseHeight(48);
    fireResize(box.element);
    // Assert — the reservation IS the footer's height, not an approximation.
    expect(box.top() - before).toBe(48);
  });

  it("leaves a reader who scrolled away exactly where they are", () => {
    // Arrange — the reader left the tail, so nothing may pull them back.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 200;
    box.element.dispatchEvent(new Event("scroll"));
    // Act
    box.loseHeight(48);
    fireResize(box.element);
    // Assert
    expect(box.top()).toBe(200);
  });

  it("hears the box's own scroll events, so a gesture ends the follow", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act — the reader wheels up and the browser dispatches the scroll event.
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("re-lands the tail when the content grows with no render behind it", () => {
    // Arrange — parked at the tail, and nothing else will touch this feed.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act — a bubble the wire pushed unfolded finishes fetching its page and
    // paints 120px of rows into its panel: growth no render performed.
    box.contentGrows(120);
    fireResize(box.content);
    // Assert — the tail is the new bottom, not 120px below the fold.
    expect(box.top()).toBe(820);
  });

  it("leaves a reader who scrolled away where they are when the content grows", () => {
    // Arrange — the reader left the tail, so growth below them is not theirs.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 200;
    box.element.dispatchEvent(new Event("scroll"));
    // Act
    box.contentGrows(120);
    fireResize(box.content);
    // Assert
    expect(box.top()).toBe(200);
  });

  it("watches a content root mounted after the subscription", async () => {
    // Arrange — the box is wired before its content exists, as a mount that
    // creates the scroll zone first and fills it after would leave it.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    const late = document.createElement("section");
    // Act
    box.element.append(late);
    await flushMutations();
    // Assert — the child set follows the DOM, so the late root is watched.
    expect(() => fireResize(late)).not.toThrow();
  });

  it("stops watching a content root removed from the box", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act
    box.content.remove();
    await flushMutations();
    // Assert — an element the box no longer holds is nothing to re-park for.
    expect(() => fireResize(box.content)).toThrow(/no ResizeObserver/);
  });

  it("hears the reader's wheel, so their gesture is attributable to them", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a pointer on its scrollbar", () => {
    // Arrange — a scrollbar drag issues no wheel and no key.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("pointerdown"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a key pressed inside it", () => {
    // Arrange — PageUp and the arrows scroll the box that holds the focus.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act — dispatched on a descendant, so the bubbling is under test too.
    box.content.dispatchEvent(new Event("keydown", { bubbles: true }));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a touch drag", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("touchstart"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("stops hearing the reader's input when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    // Assert — a released mount leaves no listener on the next one's element.
    expect(tail.isFollowing()).toBe(true);
  });

  it("stops observing the content when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    // Assert — the content half is released with the box half, not left on.
    expect(() => fireResize(box.content)).toThrow(/no ResizeObserver/);
  });

  it("stops observing the box when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    // Assert — nothing watches it any more, so a fire finds no observer.
    expect(() => fireResize(box.element)).toThrow(/no ResizeObserver/);
  });
});

/**
 * THE EXPANSION'S OWN REVEAL.
 *
 * A caret opens a sub-feed BELOW the fold and growth moves nothing on its own,
 * so the reader was left looking at the head of something they could not see
 * (measured at a click: `below=208 scrollTop=40`). The arithmetic below is the
 * whole of the answer; the caret's two cases (pinned, not pinned) live in
 * test/feed/bubble.test.ts, where the caret is.
 */
describe("revealDelta", () => {
  /** A 300px viewport starting at the top of the screen. */
  const box = { boxTop: 0, boxHeight: 300 };

  it("moves nothing for a panel already wholly on screen", () => {
    expect(revealDelta({ ...box, nodeTop: 100, nodeHeight: 100 })).toBe(0);
  });

  it("moves by exactly the overhang for a panel running past the fold", () => {
    expect(revealDelta({ ...box, nodeTop: 250, nodeHeight: 200 })).toBe(150);
  });

  it("stops at the panel's own top for a panel taller than the viewport", () => {
    // Capped: the head above it stays on screen rather than being pushed off
    // to chase a bottom edge that cannot fit anyway.
    expect(revealDelta({ ...box, nodeTop: 80, nodeHeight: 900 })).toBe(80);
  });

  it("brings a panel above the viewport back down to its top", () => {
    expect(revealDelta({ ...box, nodeTop: -50, nodeHeight: 100 })).toBe(-50);
  });

  it("counts a panel ending exactly at the fold as visible", () => {
    expect(revealDelta({ ...box, nodeTop: 100, nodeHeight: 200 })).toBe(0);
  });
});

describe("centerDelta", () => {
  /** A 300px viewport over a 1000px feed, currently scrolled to the top. */
  const box = { clientHeight: 300, scrollHeight: 1000, scrollTop: 0 };

  it("centers a row with room on both sides", () => {
    // Arrange — a 100px row whose top sits at 500 in the feed.
    // Act / Assert — its center (550) lands on the viewport center (150 from
    // the box top), so the target scrollTop is 500 - (300 - 100)/2 = 400.
    expect(centerDelta({ ...box, nodeOffsetTop: 500, nodeHeight: 100 })).toBe(400);
  });

  it("clamps at the feed start when centering would scroll above the top", () => {
    // Arrange — a row so near the start there is not room above to center it.
    // Act / Assert — the ideal top is negative, so it is clamped to scrollTop
    // 0 and the delta is 0 (already at the top).
    expect(centerDelta({ ...box, nodeOffsetTop: 20, nodeHeight: 100 })).toBe(0);
  });

  it("clamps at the feed end when centering would scroll past the bottom", () => {
    // Arrange — a row at the very end of a feed scrolled to its top; the last
    // reachable position is scrollHeight - clientHeight = 700.
    // Act / Assert — centering would ask for 900 - 100 = 800, past 700, so it
    // is clamped and the delta is 700.
    expect(centerDelta({ ...box, nodeOffsetTop: 900, nodeHeight: 100 })).toBe(700);
  });

  it("moves nothing when the row is already centered", () => {
    // Arrange — the box is already scrolled so the row's center is on the
    // viewport center (scrollTop 400 for a row-top of 500).
    // Act / Assert — the delta is zero.
    expect(
      centerDelta({ clientHeight: 300, scrollHeight: 1000, scrollTop: 400, nodeOffsetTop: 500, nodeHeight: 100 }),
    ).toBe(0);
  });
});

describe("revealInBox", () => {
  /** An element answering a scripted rect, jsdom laying nothing out. */
  const at = (top: number, height: number): HTMLElement => {
    const el = document.createElement("div");
    el.getBoundingClientRect = () => ({
      top, height, bottom: top + height, left: 0, right: 0, width: 0, x: 0, y: top,
      toJSON: () => ({}),
    });
    return el;
  };

  /** The two writes a reveal is allowed to make, recorded. */
  const writer = () => {
    const shifts: number[] = [];
    let released = 0;
    return { shifts, released: () => released, shift: (d: number) => shifts.push(d),
      release: () => { released += 1; } };
  };

  it("shifts the box by the delta the geometry asks for", () => {
    const w = writer();
    revealInBox(at(0, 300), at(250, 200), w);
    expect(w.shifts).toEqual([150]);
  });

  it("writes nothing when the node is already on screen", () => {
    const w = writer();
    revealInBox(at(0, 300), at(100, 100), w);
    expect(w.shifts).toEqual([]);
  });

  it("ends the follow even when it moves nothing, the reader having opened content to read", () => {
    const w = writer();
    revealInBox(at(0, 300), at(100, 100), w);
    expect(w.released()).toBe(1);
  });
});

describe("installIntentScroll", () => {
  // The intent-arm gate on a real feed: only the section the reader
  // deliberately entered keeps the wheel; every other one — including a
  // section the feed scrolled under a still cursor — redirects to the feed.
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A feed region that reports itself scrollable and stores its scrollTop. */
  function makeFeed(): HTMLElement {
    const feed = document.createElement("div");
    Object.defineProperty(feed, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(feed, "clientHeight", { value: 300, configurable: true });
    let top = 0;
    Object.defineProperty(feed, "scrollTop", {
      get: () => top,
      set: (v: number) => {
        top = v;
      },
      configurable: true,
    });
    document.body.append(feed);
    return feed;
  }

  /** An inner scroll box `innerScrollerAt` will recognize as a section. */
  function makeSection(feed: HTMLElement): HTMLElement {
    const box = document.createElement("div");
    box.style.overflowY = "auto";
    Object.defineProperty(box, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(box, "clientHeight", { value: 100, configurable: true });
    feed.append(box);
    return box;
  }

  /** Dispatch a vertical wheel at `target`; return it so its default can be read. */
  function wheelAt(target: HTMLElement, deltaY = 40): WheelEvent {
    const e = new Event("wheel", { bubbles: true, cancelable: true }) as WheelEvent;
    Object.defineProperty(e, "deltaY", { value: deltaY });
    Object.defineProperty(e, "deltaMode", { value: 0 });
    target.dispatchEvent(e);
    return e;
  }

  /** Dispatch a bubbling pointer/mouse event at `target`. */
  function pointerAt(type: string, target: HTMLElement): void {
    target.dispatchEvent(new Event(type, { bubbles: true }));
  }

  it("redirects a wheel over a non-armed section to the feed", () => {
    // Arrange — a section nobody entered.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    // Act
    wheelAt(box, 40);
    // Assert — the feed took the delta instead of the box.
    expect(feed.scrollTop).toBe(40);
  });

  it("prevents the browser default when it redirects", () => {
    // Arrange — the redirect must stop the browser scrolling the section too.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    // Act
    const e = wheelAt(box, 40);
    // Assert
    expect(e.defaultPrevented).toBe(true);
  });

  it("lets the armed section keep its own wheel", () => {
    // Arrange — the reader moved the pointer INTO the box, arming it.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("pointermove", box);
    // Act
    wheelAt(box, 40);
    // Assert — the feed did not move; the box keeps the wheel.
    expect(feed.scrollTop).toBe(0);
  });

  it("arms a section on a pointerdown inside it", () => {
    // Arrange — a click inside a box is a deliberate entry.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("pointerdown", box);
    // Act
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });

  it("does NOT arm a section the feed scrolled under a still cursor", () => {
    // Arrange — the browser fires mouseenter/mouseover but no pointermove when
    // the feed slides a box under a stationary pointer, so the box stays unarmed.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("mouseenter", box);
    pointerAt("mouseover", box);
    // Act
    wheelAt(box, 40);
    // Assert — the wheel redirects; scrolling is not stuck in the bubble.
    expect(feed.scrollTop).toBe(40);
  });

  it("re-arms to a different section on a pointermove into it", () => {
    // Arrange — the reader armed one box, then moved the pointer into another.
    const feed = makeFeed();
    const first = makeSection(feed);
    const second = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("pointermove", first);
    pointerAt("pointermove", second);
    // Act
    wheelAt(second, 40);
    // Assert — the newly entered box keeps the wheel.
    expect(feed.scrollTop).toBe(0);
  });

  it("disarms the previous section when the pointer moves to another", () => {
    // Arrange — same re-arm, seen from the box the pointer LEFT.
    const feed = makeFeed();
    const first = makeSection(feed);
    const second = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("pointermove", first);
    pointerAt("pointermove", second);
    // Act — a wheel back over the box the reader left.
    wheelAt(first, 40);
    // Assert — it no longer keeps the wheel; the feed does.
    expect(feed.scrollTop).toBe(40);
  });

  it("arms nothing over bare feed, so a wheel over a section is the feed's", () => {
    // Arrange — the pointer sits over the feed itself, not any section.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    pointerAt("pointermove", feed);
    // Act
    wheelAt(box, 40);
    // Assert — nothing armed, so the section hands the wheel to the feed.
    expect(feed.scrollTop).toBe(40);
  });

  it("leaves a purely horizontal wheel to the browser", () => {
    // Arrange — a wide code block inside a section must still pan.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed);
    // Act — a wheel with no vertical component.
    wheelAt(box, 0);
    // Assert — the feed is untouched; the browser owns the horizontal pan.
    expect(feed.scrollTop).toBe(0);
  });

  it("stops redirecting once its unsubscriber is called", () => {
    // Arrange
    const feed = makeFeed();
    const box = makeSection(feed);
    const { uninstall } = installIntentScroll(feed);
    // Act
    uninstall();
    wheelAt(box, 40);
    // Assert — a released mount leaves no wheel listener on the element.
    expect(feed.scrollTop).toBe(0);
  });

  it("arms a section programmatically, without any prior pointermove", () => {
    // Arrange — the owner bug: a click that expands a bubble fires no
    // pointermove, so the box it just revealed must still be armable.
    const feed = makeFeed();
    const box = makeSection(feed);
    const { arm } = installIntentScroll(feed);
    // Act — arm the box the same way a pointermove into it would, but without
    // dispatching any pointer event at all.
    arm(box);
    wheelAt(box, 40);
    // Assert — the box keeps its own wheel; the feed did not move.
    expect(feed.scrollTop).toBe(0);
  });

  it("arm() resolves the innermost scroll box enclosing the given element", () => {
    // Arrange — arm is called with a descendant of the section (as the click
    // handler calls it with the section itself, but the lookup must still
    // walk up from whatever element it is handed).
    const feed = makeFeed();
    const box = makeSection(feed);
    const inner = document.createElement("span");
    box.append(inner);
    const { arm } = installIntentScroll(feed);
    // Act
    arm(inner);
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });

  it("programmatic arm() does not arm a different, unarmed section", () => {
    // Arrange — arming one box must not blanket-arm every section.
    const feed = makeFeed();
    const armedBox = makeSection(feed);
    const other = makeSection(feed);
    const { arm } = installIntentScroll(feed);
    // Act
    arm(armedBox);
    wheelAt(other, 40);
    // Assert — the other section still redirects to the feed.
    expect(feed.scrollTop).toBe(40);
  });
});

describe("click-to-expand arms the just-expanded box (the feed.ts wiring)", () => {
  // OWNER BUG: a click that expands a bubble fires no pointermove, so the
  // just-expanded box stayed unarmed until the cursor moved. feed.ts fixes
  // this by calling installIntentScroll's `arm` from installClickExpand's
  // `afterToggle`, on EXPAND only. These tests wire the two real modules
  // together exactly as feed.ts does and drive real click/wheel events.
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A feed region that reports itself scrollable and stores its scrollTop. */
  function makeFeed(): HTMLElement {
    const feed = document.createElement("div");
    Object.defineProperty(feed, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(feed, "clientHeight", { value: 300, configurable: true });
    let top = 0;
    Object.defineProperty(feed, "scrollTop", {
      get: () => top,
      set: (v: number) => {
        top = v;
      },
      configurable: true,
    });
    document.body.append(feed);
    return feed;
  }

  /** A `.tool-fold` capped section that also overflows once expanded. */
  function makeCappedSection(feed: HTMLElement): HTMLElement {
    const box = document.createElement("div");
    box.classList.add("tool-fold");
    box.style.overflowY = "auto";
    Object.defineProperty(box, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(box, "clientHeight", { value: 100, configurable: true });
    feed.append(box);
    return box;
  }

  /** Dispatch a vertical wheel at `target`. */
  function wheelAt(target: HTMLElement, deltaY = 40): void {
    const e = new Event("wheel", { bubbles: true, cancelable: true }) as WheelEvent;
    Object.defineProperty(e, "deltaY", { value: deltaY });
    Object.defineProperty(e, "deltaMode", { value: 0 });
    target.dispatchEvent(e);
  }

  /** Wire the two modules exactly as feed.ts does, recording every `arm` call. */
  function wire(feed: HTMLElement) {
    const { arm } = installIntentScroll(feed);
    const armCalls: HTMLElement[] = [];
    installClickExpand(feed, () => "", (section, expanded) => {
      if (expanded) {
        armCalls.push(section);
        arm(section);
      }
    });
    return { armCalls };
  }

  it("arms the expanded section's own box on EXPAND, with no pointermove at all", () => {
    // Arrange
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const { armCalls } = wire(feed);
    // Act — click expands the box; no pointer event of any kind precedes it.
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    wheelAt(box, 40);
    // Assert — the box kept its own wheel immediately, and `arm` ran once.
    expect(feed.scrollTop).toBe(0);
    expect(armCalls).toEqual([box]);
  });

  it("does NOT arm on COLLAPSE", () => {
    // Arrange — an already-expanded (and thus armed) box.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const { armCalls } = wire(feed);
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand -> arms
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // collapse
    // Assert — only the expand called `arm`, never the collapse.
    expect(armCalls).toEqual([box]);
  });

  it("a wheel over a DIFFERENT, unarmed section still redirects to the feed", () => {
    // Arrange — expanding one bubble must not blanket-arm every section.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const other = makeCappedSection(feed);
    wire(feed);
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand+arm `box`
    // Act
    wheelAt(other, 40);
    // Assert
    expect(feed.scrollTop).toBe(40);
  });

  it("pointermove arming still works for a box no click has touched", () => {
    // Arrange — regression: the original arming path is untouched.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    wire(feed);
    // Act
    box.dispatchEvent(new Event("pointermove", { bubbles: true }));
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });
});
