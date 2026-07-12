import { describe, expect, it } from "vitest";
import {
  EDGE_PX,
  inEdgeZone,
  innerScrollerAt,
  isScrollBox,
  redirectsToFeed,
  wheelAction,
  wheelDeltaPx,
} from "../src/scroll.js";

/** Fake ancestor-chain node: the shape innerScrollerAt walks. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  scrollHeight: number;
  clientHeight: number;
  overflowY: string;
}

function node(name: string, over: Partial<FakeNode> = {}): FakeNode {
  return {
    name,
    parentElement: null,
    scrollHeight: 100,
    clientHeight: 100,
    overflowY: "visible",
    ...over,
  };
}

const metrics = (n: FakeNode) => n;

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
