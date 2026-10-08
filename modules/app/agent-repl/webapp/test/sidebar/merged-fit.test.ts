// @vitest-environment jsdom
/**
 * THE RECENTLY MERGED FIT (owner request, 2026-10-08): the band shows as many
 * merges as fit in the rail without scrolling, and its folded count is that
 * very number. jsdom lays nothing out, so `laidOut` states the geometry the
 * engine would give: a rail SCROLLER px tall, a pane whose content other than
 * the band's rows is OTHERS px, and merged rows ROW px each (the probe's
 * height), the rows region holding the shown rows unless the band is folded.
 */
import { describe, expect, it } from "vitest";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  MERGED_SHOWN_ATTRIBUTE,
  fitMergedSection,
  installMergedFit,
  mergedRowsThatFit,
} from "../../src/sidebar/merged-fit.js";
import { fireResize } from "../resize-observer.js";
import { mergedSection, roster, row, sidebarContext } from "./harness.js";

const ROW = 20;

/** FIVE settled merges' band, folded or not, drawn into a scroller as the rail draws it. */
function drawn(folded: boolean, total = 5): { scroller: HTMLElement; pane: HTMLElement } {
  const rows = Array.from({ length: total }, (_, i) =>
    row({ id: `ws-${i}`, closed: true, status: { case: "merged", value: {} } }),
  );
  const scroller = document.createElement("div");
  scroller.className = "sb-scroll";
  scroller.appendChild(drawWorkspaceRoster(roster({ merged: mergedSection(rows, total, folded) }), sidebarContext()));
  document.body.replaceChildren(scroller);
  const pane = scroller.querySelector('.sb-pane[data-grouping="repository"]') as HTMLElement;
  return { scroller, pane };
}

/** State the layout: the rail's height, the pane's other content, and the row unit. */
function laidOut(
  { scroller, pane }: { scroller: HTMLElement; pane: HTMLElement },
  rail: number,
  others: number,
  rowHeight = ROW,
): void {
  const section = pane.querySelector(".merged-section") as HTMLElement;
  const region = section.querySelector(":scope > .rows") as HTMLElement;
  const probe = section.querySelector(":scope > .merged-row-probe") as HTMLElement;
  Object.defineProperty(scroller, "clientHeight", { configurable: true, value: rail });
  Object.defineProperty(probe, "offsetHeight", { configurable: true, value: rowHeight });
  Object.defineProperty(region, "offsetHeight", {
    configurable: true,
    get: () =>
      section.classList.contains("folded") ? 0 : [...region.children].filter((el) => !(el as HTMLElement).hidden).length * rowHeight,
  });
  Object.defineProperty(pane, "offsetHeight", { configurable: true, get: () => others + region.offsetHeight });
}

/** What the band shows: its visible rows, and the number its count says. */
function shown(pane: HTMLElement): [number, string | null | undefined] {
  const section = pane.querySelector(".merged-section") as HTMLElement;
  const visible = [...(section.querySelector(":scope > .rows") as HTMLElement).children].filter(
    (el) => !(el as HTMLElement).hidden,
  ).length;
  return [visible, section.querySelector("[data-section-count]")?.textContent];
}

describe("mergedRowsThatFit", () => {
  it.each([
    { name: "a room of exactly three rows", available: 60, total: 5, want: 3 },
    { name: "a room just short of three rows", available: 59, total: 5, want: 2 },
    { name: "more room than merges", available: 500, total: 5, want: 5 },
    { name: "no room", available: 0, total: 5, want: 0 },
    { name: "less than no room", available: -40, total: 5, want: 0 },
  ])("fits $want rows in $name", ({ available, total, want }) => {
    // Arrange / Act
    const got = mergedRowsThatFit(available, ROW, total);
    // Assert
    expect(got).toBe(want);
  });

  it("refuses a row unit that measures nothing", () => {
    // Arrange / Act / Assert
    expect(() => mergedRowsThatFit(60, 0, 5)).toThrow(RangeError);
  });
});

describe("fitMergedSection", () => {
  it("shows the merges that fit and counts exactly those", () => {
    // Arrange — room for three rows below 100px of other content.
    const band = drawn(false);
    laidOut(band, 160, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(shown(band.pane)).toEqual([3, "(3)"]);
  });

  it("counts the same number while the band is folded", () => {
    // Arrange
    const band = drawn(true);
    laidOut(band, 160, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(band.pane.querySelector(".merged-section")?.querySelector("[data-section-count]")?.textContent).toBe("(3)");
  });

  it("shows the most recent merges, from the top", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 160, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    const rows = [...(band.pane.querySelector(".merged-section > .rows") as HTMLElement).children] as HTMLElement[];
    expect(rows.map((el) => el.hidden)).toEqual([false, false, false, true, true]);
  });

  it("shows every merge, with no fixed cap, when the rail holds them all", () => {
    // Arrange — fifteen merges, more than the old ten-row cap.
    const band = drawn(false, 15);
    laidOut(band, 1000, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(shown(band.pane)).toEqual([15, "(15)"]);
  });

  it("states the number shown on the band", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 160, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(band.pane.querySelector(".merged-section")?.getAttribute(MERGED_SHOWN_ATTRIBUTE)).toBe("3");
  });

  it("leaves a band that is not laid out as drawn, every row under the daemon's count", () => {
    // Arrange — the other grouping's pane: nothing measures.
    const band = drawn(false);
    laidOut(band, 160, 0, 0);
    // Act
    const got = fitMergedSection(band.scroller, band.pane);
    // Assert
    expect([got, ...shown(band.pane)]).toEqual([null, 5, "(5)"]);
  });

  it("refuses a band drawn without its row probe", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 160, 100);
    band.pane.querySelector(".merged-row-probe")?.remove();
    // Act / Assert
    expect(() => fitMergedSection(band.scroller, band.pane)).toThrow(/row probe/);
  });
});

describe("installMergedFit", () => {
  it("re-fits when the rail is resized", () => {
    // Arrange — fitted at three rows.
    const band = drawn(false);
    laidOut(band, 160, 100);
    const fit = installMergedFit(band.scroller);
    fit.refit();
    // Act — the rail grows by two rows.
    Object.defineProperty(band.scroller, "clientHeight", { configurable: true, value: 200 });
    fireResize(band.scroller);
    // Assert
    expect(shown(band.pane)).toEqual([5, "(5)"]);
    fit.dispose();
  });

  it("re-fits when a pane changes size", () => {
    // Arrange — fitted at three rows.
    const band = drawn(false);
    laidOut(band, 160, 100);
    const fit = installMergedFit(band.scroller);
    fit.refit();
    // Act — a live section above folds, freeing two rows.
    Object.defineProperty(band.pane, "offsetHeight", {
      configurable: true,
      get: () => 60 + (band.pane.querySelector(".merged-section > .rows") as HTMLElement).offsetHeight,
    });
    fireResize(band.pane);
    // Assert
    expect(shown(band.pane)).toEqual([5, "(5)"]);
    fit.dispose();
  });
});
