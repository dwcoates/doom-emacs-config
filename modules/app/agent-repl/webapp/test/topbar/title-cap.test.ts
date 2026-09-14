// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { TITLE_CAP_FLOOR_PX, capTopbarTitle, type TitleMetrics } from "../../src/topbar/title-cap.js";

/** A drawn row, in the three classes the cap reads. */
function row(): HTMLElement {
  const el = document.createElement("div");
  el.className = "topbar-row";
  for (const cls of ["topbar-left", "topbar-title", "topbar-right"]) {
    const cell = document.createElement("div");
    cell.className = cls;
    el.append(cell);
  }
  document.body.replaceChildren(el);
  return el;
}

/**
 * The boxes jsdom cannot report. Widths are keyed by class, so a test states
 * the layout it is about and nothing else.
 */
function metrics(widths: Record<string, number>, gap: number): TitleMetrics {
  return {
    width: (el) => widths[el.className] ?? 0,
    gap: () => gap,
  };
}

function titleMaxWidth(el: HTMLElement): string {
  const title = el.querySelector<HTMLElement>(".topbar-title");
  if (title === null) throw new Error("no title in the row");
  return title.style.maxWidth;
}

describe("capTopbarTitle", () => {
  it("caps the title from the wider flank, on the strip measured live", () => {
    // ARRANGE: the owner's strip — row 1090, left group 160, right group 292.
    const el = row();
    const m = metrics({ "topbar-row": 1090, "topbar-left": 160, "topbar-right": 292 }, 8);

    // ACT.
    capTopbarTitle(el, m);

    // ASSERT: 1090 − 2 · 292 − 2 · 8.
    expect(titleMaxWidth(el)).toBe("490px");
  });

  it("caps to the same width when the LEFT group is the wider one", () => {
    // ARRANGE: the same strip mirrored, which must center the title identically.
    const el = row();
    const m = metrics({ "topbar-row": 1090, "topbar-left": 292, "topbar-right": 160 }, 8);

    // ACT.
    capTopbarTitle(el, m);

    // ASSERT.
    expect(titleMaxWidth(el)).toBe("490px");
  });

  it("takes the floor when the flanks leave less than it", () => {
    // ARRANGE: two groups that between them claim nearly the whole row.
    const el = row();
    const m = metrics({ "topbar-row": 600, "topbar-left": 120, "topbar-right": 295 }, 8);

    // ACT.
    capTopbarTitle(el, m);

    // ASSERT: 600 − 590 − 16 is negative, so the floor stands.
    expect(titleMaxWidth(el)).toBe(`${TITLE_CAP_FLOOR_PX}px`);
  });

  it("caps nothing when the row has no width", () => {
    // ARRANGE: a workspace whose panel is not shown reports every box as zero.
    const el = row();
    const m = metrics({}, 8);

    // ACT.
    capTopbarTitle(el, m);

    // ASSERT.
    expect(titleMaxWidth(el)).toBe("");
  });
});
