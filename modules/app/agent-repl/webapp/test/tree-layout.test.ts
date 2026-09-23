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

describe("stagedCols", () => {
  it.each([
    { name: "a percentage cap", maxWidth: "77%", want: 93 },
    { name: "a px cap", maxWidth: "560px", want: 67 },
  ])("computes the budget of $name", ({ maxWidth, want }) => {
    // Act + Assert
    expect(stagedCols({ charPx: 8, containingPx: 1000, maxWidth, bodyPaddingPx: 10 })).toBe(want);
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
