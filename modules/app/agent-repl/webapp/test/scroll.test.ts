import { describe, expect, it } from "vitest";
import {
  EDGE_PX,
  PIN_PX,
  SECTION_CLASSES,
  freezeOnScroll,
  freezeOnToggle,
  inEdgeZone,
  innerScrollerAt,
  isPinnedToBottom,
  isScrollBox,
  parkAtTail,
  redirectsToFeed,
  sectionFor,
  wheelAction,
  wheelDeltaPx,
  type RevealBlock,
  type RevealTarget,
  revealNode,
} from "../src/scroll.js";
import { renderItem } from "../src/render.js";
import { PermissionItem, ToolItem } from "../src/store.js";

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

describe("freezeOnToggle", () => {
  it("freezes tail-following when the user opens a nested view", () => {
    // Arrange + Act + Assert
    expect(freezeOnToggle(false, true)).toBe(true);
  });

  it("keeps an existing freeze when the user closes a nested view", () => {
    // Arrange — already frozen, now closing a fold: only a scroll-to-tail lifts it.
    expect(freezeOnToggle(true, false)).toBe(true);
  });

  it("does not freeze on a close from an unfrozen feed", () => {
    // Arrange + Act + Assert
    expect(freezeOnToggle(false, false)).toBe(false);
  });

  it("stays frozen when the user opens a second nested view", () => {
    // Arrange + Act + Assert
    expect(freezeOnToggle(true, true)).toBe(true);
  });
});

describe("freezeOnScroll", () => {
  it("lifts the freeze when the user scrolls back to the tail", () => {
    // Arrange — frozen, now pinned to the bottom again.
    expect(freezeOnScroll(true, true)).toBe(false);
  });

  it("keeps the freeze while the user scrolls short of the tail", () => {
    // Arrange — frozen, still scrolled up above the bottom.
    expect(freezeOnScroll(true, false)).toBe(true);
  });

  it("stays unfrozen when an unfrozen feed reaches its tail", () => {
    // Arrange + Act + Assert
    expect(freezeOnScroll(false, true)).toBe(false);
  });

  it("stays unfrozen when an unfrozen feed is scrolled up", () => {
    // Arrange + Act + Assert
    expect(freezeOnScroll(false, false)).toBe(false);
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
