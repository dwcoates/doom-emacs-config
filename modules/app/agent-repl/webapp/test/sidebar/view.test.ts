// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  createSidebarView,
  foldViewKey,
  GROUPING_VIEW_KEY,
  paintSectionFold,
  paintTriangle,
} from "../../src/sidebar/view.js";

/** A paint that records every value it was handed. */
function recorder<T>(): { paint: (value: T) => void; painted: T[] } {
  const painted: T[] = [];
  return { paint: (value) => painted.push(value), painted };
}

describe("the view keys", () => {
  it.each([
    [foldViewKey("task:t1"), "fold:task:t1"],
    [GROUPING_VIEW_KEY, "grouping"],
  ])("spells %s as %s", (key, want) => {
    expect(key).toBe(want);
  });
});

describe("track", () => {
  it("answers the wire when nothing is asked", () => {
    expect(createSidebarView().track("k", true, () => undefined)).toBe(true);
  });

  it("answers the held ask over a wire that still disagrees", () => {
    const view = createSidebarView();
    view.track("k", false, () => undefined);
    view.ask("k", true);
    expect(view.track("k", false, () => undefined)).toBe(true);
  });

  it("releases the ask once the wire agrees", () => {
    const view = createSidebarView();
    view.track("k", false, () => undefined);
    view.ask("k", true);
    view.track("k", true, () => undefined);
    expect(view.track("k", false, () => undefined)).toBe(false);
  });

  it("holds a string value as it holds a boolean", () => {
    const view = createSidebarView();
    view.track(GROUPING_VIEW_KEY, "repository", () => undefined);
    view.ask(GROUPING_VIEW_KEY, "task");
    expect(view.track(GROUPING_VIEW_KEY, "repository", () => undefined)).toBe("task");
  });
});

describe("ask", () => {
  it("paints every copy at once", () => {
    const view = createSidebarView();
    const one = recorder<boolean>();
    const two = recorder<boolean>();
    view.track("k", false, one.paint);
    view.track("k", false, two.paint);
    view.ask("k", true);
    expect([one.painted, two.painted]).toEqual([[true], [true]]);
  });

  it("paints no copy from a roster already thrown away", () => {
    const view = createSidebarView();
    const old = recorder<boolean>();
    view.track("k", false, old.paint);
    view.beginDraw();
    view.ask("k", true);
    expect(old.painted).toEqual([]);
  });

  it("answers a fresh token for every ask", () => {
    const view = createSidebarView();
    expect(view.ask("k", true)).not.toBe(view.ask("k", false));
  });
});

describe("abandon", () => {
  it("repaints every copy from the wire", () => {
    const view = createSidebarView();
    const copy = recorder<boolean>();
    view.track("k", false, copy.paint);
    const token = view.ask("k", true);
    view.abandon("k", token);
    expect(copy.painted).toEqual([true, false]);
  });

  it("releases the hold, so the next push draws the wire", () => {
    const view = createSidebarView();
    view.track("k", false, () => undefined);
    view.abandon("k", view.ask("k", true));
    expect(view.track("k", false, () => undefined)).toBe(false);
  });

  it("leaves a later ask's hold alone", () => {
    const view = createSidebarView();
    view.track("k", false, () => undefined);
    const first = view.ask("k", true);
    view.ask("k", false);
    view.ask("k", true);
    view.abandon("k", first);
    expect(view.track("k", false, () => undefined)).toBe(true);
  });

  it("fails hard on a key no push ever drew", () => {
    const view = createSidebarView();
    const token = view.ask("never-drawn", true);
    expect(() => view.abandon("never-drawn", token)).toThrow(/no wire value/);
  });
});

describe("paintSectionFold", () => {
  it.each([true, false])("states folded=%s on the section and every triangle in it", (folded) => {
    const section = document.createElement("div");
    section.innerHTML = "<span data-section-fold></span><span data-section-fold></span>";
    paintSectionFold(section, folded);
    expect([
      section.classList.contains("folded"),
      ...[...section.querySelectorAll("[data-section-fold]")].map((t) => t.getAttribute("data-folded")),
    ]).toEqual([folded, String(folded), String(folded)]);
  });
});

describe("paintTriangle", () => {
  it.each([
    [true, "▸"],
    [false, "▾"],
  ])("draws folded=%s as %s", (folded, glyph) => {
    const triangle = document.createElement("span");
    paintTriangle(triangle, folded);
    expect(triangle.textContent).toBe(glyph);
  });
});
