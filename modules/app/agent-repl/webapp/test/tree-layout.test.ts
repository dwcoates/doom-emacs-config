// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { installTreeLayout, stagedCols, useTreeLayout } from "./tree-layout.js";

/** A column holding a bubble holding a body and a tree probe. */
function stage(attached: boolean): { column: HTMLElement; bubble: HTMLElement; body: HTMLElement; probe: HTMLElement } {
  const column = document.createElement("div");
  const bubble = document.createElement("div");
  bubble.className = "bubble";
  const body = document.createElement("div");
  body.className = "bubble-body";
  const probe = document.createElement("div");
  probe.className = "mp-tree";
  probe.textContent = "0".repeat(10);
  body.append(probe);
  bubble.append(body);
  column.append(bubble);
  if (attached) document.body.append(column);
  return { column, bubble, body, probe };
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("installTreeLayout", () => {
  it("answers an attached layout's reads from the staged geometry", () => {
    // Arrange
    const { uninstall } = installTreeLayout({ charPx: 7, containingPx: 900, maxWidth: "50%", bodyPaddingPx: 4 });
    const { column, bubble, body, probe } = stage(true);
    try {
      // Act
      const reads = [
        probe.getBoundingClientRect().width,
        column.clientWidth,
        getComputedStyle(bubble).maxWidth,
        getComputedStyle(body).paddingLeft,
        getComputedStyle(bubble).borderLeftWidth,
      ];
      // Assert
      expect(reads).toEqual([70, 900, "50%", "4px", "0px"]);
    } finally {
      uninstall();
    }
  });

  it("answers a detached layout's reads as a real engine does: no box, no style", () => {
    // Arrange
    const { uninstall } = installTreeLayout();
    const { column, bubble, body, probe } = stage(false);
    try {
      // Act
      const reads = [
        probe.getBoundingClientRect().width,
        column.clientWidth,
        getComputedStyle(bubble).maxWidth,
        getComputedStyle(body).paddingLeft,
      ];
      // Assert
      expect(reads).toEqual([0, 0, "", ""]);
    } finally {
      uninstall();
    }
  });

  it.each([
    ["an attached", true, [700, 694]],
    ["a detached", false, [0, 0]],
  ] as const)("answers %s scroll box's widths as a real engine does", (_label, attached, want) => {
    // Arrange
    const { uninstall } = installTreeLayout({ scrollbarPx: 6 });
    const { bubble, body } = stage(attached);
    const scroll = document.createElement("div");
    scroll.className = "bubble-scroll bubble-box";
    bubble.append(scroll);
    scroll.append(body);
    try {
      // Act
      const read = [scroll.offsetWidth, scroll.clientWidth];
      // Assert
      expect(read).toEqual(want);
    } finally {
      uninstall();
    }
  });

  it("puts jsdom back when uninstalled", () => {
    // Arrange
    const { uninstall } = installTreeLayout();
    const { column } = stage(true);
    // Act
    uninstall();
    // Assert — jsdom's own zero, and no own override left behind.
    expect([column.clientWidth, Object.hasOwn(HTMLElement.prototype, "clientWidth")]).toEqual([0, false]);
  });
});

describe("installTreeLayout's zoom", () => {
  const staged = useTreeLayout();

  afterEach(() => {
    document.documentElement.style.removeProperty("--feed-text-scale");
  });

  it.each([
    ["no zoom set anywhere", null, null, 80, 400],
    ["the document's zoom", "1.5", null, 120, 600],
    ["a bubble's own zoom over the document's", "1.5", "1", 80, 400],
  ] as const)("draws a tree probe and a bubble at %s", (_label, root, own, probePx, bubblePx) => {
    // Arrange
    const { bubble, probe } = stage(true);
    if (root !== null) document.documentElement.style.setProperty("--feed-text-scale", root);
    if (own !== null) bubble.style.setProperty("--feed-text-scale", own);
    // Act
    const reads = [probe.getBoundingClientRect().width, bubble.getBoundingClientRect().width];
    // Assert
    expect(reads).toEqual([probePx, bubblePx]);
  });

  it("draws a held bubble at its held breadth, whatever the zoom", () => {
    // Arrange
    const { bubble } = stage(true);
    document.documentElement.style.setProperty("--feed-text-scale", "1.5");
    bubble.style.setProperty("--bubble-held-breadth", "321px");
    // Act / Assert
    expect(bubble.getBoundingClientRect().width).toBe(321);
  });

  it.each([
    ["an unheld bubble", null],
    ["a held bubble", "900px"],
  ] as const)("caps %s at its max-width", (_label, held) => {
    // Arrange — 77% of the 1000px column.
    staged.layout.naturalPx = 2000;
    const { bubble } = stage(true);
    if (held !== null) bubble.style.setProperty("--bubble-held-breadth", held);
    // Act / Assert
    expect(bubble.getBoundingClientRect().width).toBe(770);
  });
});

describe("stagedCols", () => {
  it.each([
    { name: "a percentage cap", maxWidth: "77%", scrollbarPx: 8, want: 92 },
    { name: "a px cap", maxWidth: "560px", scrollbarPx: 8, want: 66 },
    { name: "a cap with no gutter", maxWidth: "77%", scrollbarPx: 0, want: 93 },
  ])("computes the budget of $name", ({ maxWidth, scrollbarPx, want }) => {
    // Act + Assert
    expect(stagedCols({ charPx: 8, containingPx: 1000, maxWidth, bodyPaddingPx: 10, scrollbarPx, scrollBoxPx: 700, naturalPx: 400 })).toBe(want);
  });
});

describe("useTreeLayout", () => {
  const staged = useTreeLayout();

  it("installs a live layout for each test", () => {
    // Arrange
    const { column } = stage(true);
    // Act
    staged.layout.containingPx = 640;
    // Assert
    expect(column.clientWidth).toBe(640);
  });

  it("starts each test from the default layout", () => {
    // Act + Assert
    expect(staged.layout.containingPx).toBe(1000);
  });
});

describe("useTreeLayout outside a test", () => {
  const staged = useTreeLayout();
  // Read at collection time, before any beforeEach has run.
  let collected: unknown;
  try {
    collected = staged.layout;
  } catch (err) {
    collected = err;
  }

  it("refuses to hand out a layout that is not installed", () => {
    // Assert
    expect(collected).toBeInstanceOf(Error);
  });
});
