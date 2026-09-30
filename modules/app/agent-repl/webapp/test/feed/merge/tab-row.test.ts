// @vitest-environment jsdom
/**
 * tab-row — a merge tab drawn as a row of its own.
 *
 * Inside a merge bubble the strip consumes every tab and this module is never
 * reached. These cases are the OTHER feed: a tab that arrived somewhere with no
 * strip to hold it still draws, through the strip's own badge and body.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { drawFeedMergeTabRow } from "../../../src/feed/merge/tab-row.js";
import { harness, rowContext } from "../harness.js";
import { FakeSubfeed, tabRow, type TabSpec } from "./fixtures.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** Draw one tab row on its own, the way a non-merge feed would. */
function draw(spec: TabSpec): HTMLElement {
  const h = harness();
  const row = tabRow("t1", spec);
  const view = new FakeSubfeed([row]);
  if (row.row.case !== "mergeTab") throw new Error("the fixture is not a merge tab");
  return drawFeedMergeTabRow(row, row.row.value, view, rowContext(h.ctx, row));
}

describe("a merge tab drawn as its own row", () => {
  it("draws the tab's badge with its kind", () => {
    // Arrange / Act
    const el = draw({ kind: "queue", state: "live" });
    // Assert
    expect(el.querySelector("[data-merge-tab]")?.getAttribute("data-merge-tab")).toBe("queue");
  });

  it("draws the badge with its state", () => {
    // Arrange / Act
    const el = draw({ kind: "queue", state: "live" });
    // Assert
    expect(el.querySelector("[data-merge-tab]")?.getAttribute("data-tab-state")).toBe("live");
  });

  it("carries the tab's state as the card's own state", () => {
    // Arrange / Act
    const el = draw({ kind: "committing", state: "settled", outcome: "succeeded" });
    // Assert
    expect(el.getAttribute("data-state")).toBe("settled");
  });

  it("marks the lone badge active, there being nothing to select between", () => {
    // Arrange / Act
    const el = draw({ kind: "rebasing", state: "live" });
    // Assert
    expect(el.querySelector(".merge-tab")?.getAttribute("aria-selected")).toBe("true");
  });

  it("draws the tab's own body", () => {
    // Arrange / Act
    const el = draw({ kind: "rebasing", state: "live", payload: { lines: [{ text: "replaying 1/1" }] } });
    // Assert
    expect(el.textContent).toContain("replaying 1/1");
  });

  it("draws a settled failure's summary line", () => {
    // Arrange / Act
    const el = draw({
      kind: "tests",
      state: "settled",
      outcome: "failed",
      summary: "two suites are red",
    });
    // Assert
    expect(el.textContent).toContain("two suites are red");
  });
});
