// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  createSidebarView,
  foldViewKey,
  paintSectionFold,
  paintFoldControl,
} from "../../src/sidebar/view.js";

/** A fresh view, and one scope of it to track copies in. */
function scopedView() {
  const view = createSidebarView();
  return { view, scope: view.scope() };
}

/** A paint that records every value it was handed. */
function recorder<T>(): { paint: (value: T) => void; painted: T[] } {
  const painted: T[] = [];
  return { paint: (value) => painted.push(value), painted };
}

describe("the view keys", () => {
  it.each([[foldViewKey("repo:r1"), "fold:repo:r1"]])("spells %s as %s", (key, want) => {
    expect(key).toBe(want);
  });
});

describe("track", () => {
  it("answers the wire when nothing is asked", () => {
    expect(scopedView().scope.track("k", true, () => undefined)).toBe(true);
  });

  it("answers the held ask over a wire that still disagrees", () => {
    const { view, scope } = scopedView();
    scope.track("k", false, () => undefined);
    view.ask("k", true);
    expect(scope.track("k", false, () => undefined)).toBe(true);
  });

  it("releases the ask once the wire agrees", () => {
    const { view, scope } = scopedView();
    scope.track("k", false, () => undefined);
    view.ask("k", true);
    scope.track("k", true, () => undefined);
    expect(scope.track("k", false, () => undefined)).toBe(false);
  });

});

describe("ask", () => {
  it("paints every copy at once", () => {
    const { view, scope } = scopedView();
    const one = recorder<boolean>();
    const two = recorder<boolean>();
    scope.track("k", false, one.paint);
    scope.track("k", false, two.paint);
    view.ask("k", true);
    expect([one.painted, two.painted]).toEqual([[true], [true]]);
  });

  it("paints no copy of a disposed scope", () => {
    const { view, scope } = scopedView();
    const old = recorder<boolean>();
    scope.track("k", false, old.paint);
    scope.dispose();
    view.ask("k", true);
    expect(old.painted).toEqual([]);
  });

  it("paints another scope's copies after one scope is disposed", () => {
    const { view, scope } = scopedView();
    const kept = view.scope();
    const copy = recorder<boolean>();
    scope.track("k", false, () => undefined);
    kept.track("k", false, copy.paint);
    scope.dispose();
    view.ask("k", true);
    expect(copy.painted).toEqual([true]);
  });

  it("answers a fresh token for every ask", () => {
    const view = createSidebarView();
    expect(view.ask("k", true)).not.toBe(view.ask("k", false));
  });
});

describe("abandon", () => {
  it("repaints every copy from the wire", () => {
    const { view, scope } = scopedView();
    const copy = recorder<boolean>();
    scope.track("k", false, copy.paint);
    const token = view.ask("k", true);
    view.abandon("k", token);
    expect(copy.painted).toEqual([true, false]);
  });

  it("releases the hold, so the next push draws the wire", () => {
    const { view, scope } = scopedView();
    scope.track("k", false, () => undefined);
    view.abandon("k", view.ask("k", true));
    expect(scope.track("k", false, () => undefined)).toBe(false);
  });

  it("leaves a later ask's hold alone", () => {
    const { view, scope } = scopedView();
    scope.track("k", false, () => undefined);
    const first = view.ask("k", true);
    view.ask("k", false);
    view.ask("k", true);
    view.abandon("k", first);
    expect(scope.track("k", false, () => undefined)).toBe(true);
  });

  it("fails hard on a key no push ever drew", () => {
    const view = createSidebarView();
    const token = view.ask("never-drawn", true);
    expect(() => view.abandon("never-drawn", token)).toThrow(/no wire value/);
  });
});

describe("paintSectionFold", () => {
  it.each([true, false])("states folded=%s on the section and its own header", (folded) => {
    // Arrange
    const section = document.createElement("div");
    section.innerHTML = "<div class=\"repo-head\" data-section-fold></div><div class=\"rows\"></div>";
    // Act
    paintSectionFold(section, folded);
    // Assert
    expect([section.classList.contains("folded"), section.querySelector("[data-section-fold]")?.getAttribute("data-folded")]).toEqual([
      folded,
      String(folded),
    ]);
  });

  it("leaves a nested section's header to that section's own fold", () => {
    // Arrange: the open-workspaces section holds whole sections.
    const outer = document.createElement("div");
    outer.innerHTML =
      "<div id=\"own\" class=\"repo-head\" data-section-fold></div>" +
      "<div class=\"sb-sections\"><div class=\"repo\"><div id=\"nested\" class=\"repo-head\" data-section-fold data-folded=\"false\"></div></div></div>";
    // Act
    paintSectionFold(outer, true);
    // Assert
    expect([outer.querySelector("#own")?.getAttribute("data-folded"), outer.querySelector("#nested")?.getAttribute("data-folded")]).toEqual([
      "true",
      "false",
    ]);
  });
});

describe("paintFoldControl", () => {
  it.each([true, false])("states folded=%s on the control and draws it no glyph", (folded) => {
    const control = document.createElement("div");
    paintFoldControl(control, folded);
    expect([control.getAttribute("data-folded"), control.textContent]).toEqual([String(folded), ""]);
  });
});
