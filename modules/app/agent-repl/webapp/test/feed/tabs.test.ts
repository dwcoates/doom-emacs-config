// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import stylesheet from "../../src/styles.css?raw";
import {
  FEED_TAB_CLASS,
  FEED_TAB_STRIP_CLASS,
  drawFeedTab,
  drawFeedTabStrip,
} from "../../src/feed/tabs.js";
import { createToolGroupStore, groupKey } from "../../src/feed/tool-group.js";
import { isControl } from "../../src/control.js";

describe("drawFeedTabStrip", () => {
  it("draws a tablist wearing the shared class beside the extra ones", () => {
    // Act
    const strip = drawFeedTabStrip("feed-group-strip");
    // Assert
    expect(strip.getAttribute("role")).toBe("tablist");
    expect([...strip.classList]).toEqual([FEED_TAB_STRIP_CLASS, "feed-group-strip"]);
  });

  it("states the strip's size once, and no position", () => {
    // Arrange
    const match = /\.feed-tab-strip\s*\{([^}]*)\}/.exec(stylesheet);
    // Assert
    expect(match).not.toBeNull();
    expect(match?.[1]).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(match?.[1]).not.toMatch(/position\s*:/);
  });
});

describe("drawFeedTab", () => {
  it("draws a control with the icon and the 1-based index", () => {
    // Act
    const tab = drawFeedTab({ icon: "$", index: 2, active: false, onPick: () => {} });
    // Assert
    expect(isControl(tab)).toBe(true);
    expect(tab.className).toBe(FEED_TAB_CLASS);
    expect(tab.getAttribute("role")).toBe("tab");
    expect(tab.querySelector(".feed-group-tab-icon")?.textContent).toBe("$");
    expect(tab.querySelector(".feed-group-tab-icon")?.getAttribute("aria-hidden")).toBe("true");
    expect(tab.querySelector(".feed-group-tab-index")?.textContent).toBe("2");
  });

  it("draws no label element when none is given", () => {
    // Act
    const tab = drawFeedTab({ icon: "$", index: 1, active: false, onPick: () => {} });
    // Assert
    expect(tab.querySelector(".feed-group-tab-label")).toBeNull();
  });

  it("draws the label after the index when one is given", () => {
    // Act
    const tab = drawFeedTab({ icon: "?", index: 1, label: "Auth method", active: false, onPick: () => {} });
    // Assert
    expect(tab.lastElementChild?.className).toBe("feed-group-tab-label");
    expect(tab.lastElementChild?.textContent).toBe("Auth method");
  });

  it.each([
    [true, "true"],
    [false, "false"],
  ])("marks active=%s as aria-selected=%s and is-active alike", (active, selected) => {
    // Act
    const tab = drawFeedTab({ icon: "$", index: 1, active, onPick: () => {} });
    // Assert
    expect(tab.getAttribute("aria-selected")).toBe(selected);
    expect(tab.classList.contains("is-active")).toBe(active);
  });

  it("runs onPick on a click", () => {
    // Arrange
    const onPick = vi.fn();
    const tab = drawFeedTab({ icon: "$", index: 1, active: false, onPick });
    // Act
    tab.click();
    // Assert
    expect(onPick).toHaveBeenCalledOnce();
  });
});

describe("the tool group draws the shared tab", () => {
  it("draws its strip and every tab exactly as the shared builders do", () => {
    // Arrange -- a hand-rolled tab that drifted from the shared one fails here.
    const members = ["a", "b"].map((id) => ({ id, element: document.createElement("article") }));
    const expectedTab = drawFeedTab({ icon: "$", index: 1, active: false, onPick: () => {} });
    // Act
    const wrapper = createToolGroupStore().arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    // Assert
    const strip = wrapper.querySelector(".feed-group-strip");
    expect(strip?.classList.contains(FEED_TAB_STRIP_CLASS)).toBe(true);
    const first = wrapper.querySelector(`.${FEED_TAB_CLASS}`);
    expect(first?.getAttribute("role")).toBe(expectedTab.getAttribute("role"));
    expect([...(first?.children ?? [])].map((c) => c.className)).toEqual(
      [...expectedTab.children].map((c) => c.className),
    );
  });
});
