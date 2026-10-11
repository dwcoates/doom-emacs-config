// @vitest-environment jsdom
/**
 * THE RECENTLY MERGED FIT (owner request, 2026-10-08): the band shows as many
 * merges as fit in the rail without scrolling, and its folded count is that
 * very number. jsdom lays nothing out, so `laidOut` states the geometry the
 * engine would give: a rail SCROLLER px tall, a pane whose content other than
 * the band's rows is OTHERS px, merged rows ROW px each and group headers
 * HEAD px each (the probes' heights), the rows region holding the shown groups'
 * headers and rows unless the band is folded.
 */
import { describe, expect, it } from "vitest";
import type { RosterMergedRepoGroup } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  MERGED_SHOWN_ATTRIBUTE,
  fitMergedSection,
  installMergedFit,
  mergedRowsThatFit,
} from "../../src/sidebar/merged-fit.js";
import { fireResize } from "../resize-observer.js";
import { mergedBand, mergedGroup, mergedSection, roster, row, sidebarContext } from "./harness.js";

const ROW = 20;
const HEAD = 10;

/** TOTAL settled merges of one repository. */
function merges(total: number, prefix = "ws"): ReturnType<typeof row>[] {
  return Array.from({ length: total }, (_, i) =>
    row({ id: `${prefix}-${i}`, closed: true, status: { case: "merged", value: {} } }),
  );
}

/** A band, folded or not, drawn into a scroller as the rail draws it. */
function drawnBand(folded: boolean, groups: RosterMergedRepoGroup[]): { scroller: HTMLElement; pane: HTMLElement } {
  const scroller = document.createElement("div");
  scroller.className = "sb-scroll";
  scroller.appendChild(drawWorkspaceRoster(roster({ merged: mergedBand(groups, undefined, folded) }), sidebarContext()));
  document.body.replaceChildren(scroller);
  const pane = scroller.querySelector('.sb-pane[data-grouping="repository"]') as HTMLElement;
  return { scroller, pane };
}

/** FIVE settled merges' band of one repository, folded or not. */
function drawn(folded: boolean, total = 5): { scroller: HTMLElement; pane: HTMLElement } {
  const scroller = document.createElement("div");
  scroller.className = "sb-scroll";
  scroller.appendChild(drawWorkspaceRoster(roster({ merged: mergedSection(merges(total), total, folded) }), sidebarContext()));
  document.body.replaceChildren(scroller);
  const pane = scroller.querySelector('.sb-pane[data-grouping="repository"]') as HTMLElement;
  return { scroller, pane };
}

/** The band's groups, in order. */
function groupsOf(pane: HTMLElement): HTMLElement[] {
  return [...pane.querySelectorAll<HTMLElement>(".merged-section > .rows > .merged-group")];
}

/** A group's rows. */
function rowsOf(group: HTMLElement): HTMLElement[] {
  return [...(group.querySelector(":scope > .rows") as HTMLElement).children] as HTMLElement[];
}

/** State the layout: the rail's height, the pane's other content, and the units. */
function laidOut(
  { scroller, pane }: { scroller: HTMLElement; pane: HTMLElement },
  rail: number,
  others: number,
  rowHeight = ROW,
  headHeight = HEAD,
): void {
  const section = pane.querySelector(".merged-section") as HTMLElement;
  const region = section.querySelector(":scope > .rows") as HTMLElement;
  const rowProbe = section.querySelector(":scope > .merged-row-probe") as HTMLElement;
  const headProbe = section.querySelector(":scope > .merged-group-head-probe") as HTMLElement;
  Object.defineProperty(scroller, "clientHeight", { configurable: true, value: rail });
  Object.defineProperty(rowProbe, "offsetHeight", { configurable: true, value: rowHeight });
  Object.defineProperty(headProbe, "offsetHeight", { configurable: true, value: headHeight });
  Object.defineProperty(region, "offsetHeight", {
    configurable: true,
    get: () =>
      section.classList.contains("folded")
        ? 0
        : groupsOf(pane)
            .filter((g) => !g.hidden)
            .reduce((sum, g) => sum + headHeight + rowsOf(g).filter((r) => !r.hidden).length * rowHeight, 0),
  });
  Object.defineProperty(pane, "offsetHeight", { configurable: true, get: () => others + region.offsetHeight });
}

/** What the band shows: its visible rows, and the number its count says. */
function shown(pane: HTMLElement): [number, string | null | undefined] {
  const section = pane.querySelector(".merged-section") as HTMLElement;
  const visible = groupsOf(pane)
    .filter((g) => !g.hidden)
    .reduce((sum, g) => sum + rowsOf(g).filter((r) => !r.hidden).length, 0);
  return [visible, section.querySelector("[data-section-count]")?.textContent];
}

describe("mergedRowsThatFit", () => {
  it.each([
    { name: "a room of exactly three rows and a header", available: 70, groups: [[5, 4, 3, 2, 1]], want: [3] },
    { name: "a room just short of three rows and a header", available: 69, groups: [[5, 4, 3, 2, 1]], want: [2] },
    { name: "more room than merges", available: 500, groups: [[5, 4, 3, 2, 1]], want: [5] },
    { name: "no room", available: 0, groups: [[5, 4, 3, 2, 1]], want: [0] },
    { name: "less than no room", available: -40, groups: [[5, 4, 3, 2, 1]], want: [0] },
    { name: "no groups", available: 500, groups: [], want: [] },
    { name: "two groups, the most recent first across both", available: 80, groups: [[9, 2], [8, 7]], want: [1, 2] },
    { name: "two groups, each paying its header", available: 60, groups: [[9, 2], [8, 7]], want: [1, 1] },
    { name: "a tie, which goes to the earlier group", available: 30, groups: [[5], [5]], want: [1, 0] },
    { name: "a newer row that would open a group past the room", available: 50, groups: [[9, 1], [8]], want: [1, 0] },
  ])("fits $want in $name", ({ available, groups, want }) => {
    // Arrange / Act
    const got = mergedRowsThatFit(available, ROW, HEAD, groups);
    // Assert
    expect(got).toEqual(want);
  });

  it("refuses a row unit that measures nothing", () => {
    // Arrange / Act / Assert
    expect(() => mergedRowsThatFit(60, 0, HEAD, [[1]])).toThrow(RangeError);
  });

  it("refuses a header unit below nothing", () => {
    // Arrange / Act / Assert
    expect(() => mergedRowsThatFit(60, ROW, -1, [[1]])).toThrow(RangeError);
  });
});

describe("fitMergedSection", () => {
  it("shows the merges that fit and counts exactly those", () => {
    // Arrange — room for a header and three rows below 100px of other content.
    const band = drawn(false);
    laidOut(band, 170, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(shown(band.pane)).toEqual([3, "(3)"]);
  });

  it("counts the same number while the band is folded", () => {
    // Arrange
    const band = drawn(true);
    laidOut(band, 170, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(band.pane.querySelector(".merged-section")?.querySelector("[data-section-count]")?.textContent).toBe("(3)");
  });

  it("shows the most recent merges, from the top", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 170, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(rowsOf(groupsOf(band.pane)[0]).map((el) => el.hidden)).toEqual([false, false, false, true, true]);
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
    laidOut(band, 170, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect(band.pane.querySelector(".merged-section")?.getAttribute(MERGED_SHOWN_ATTRIBUTE)).toBe("3");
  });

  it("picks the most recent merges across groups", () => {
    // Arrange — alpha's merges are older than beta's newest two.
    const alpha = mergedGroup({ id: "alpha", rows: merges(2, "a") });
    const beta = mergedGroup({ id: "beta", rows: merges(2, "b") });
    beta.rows!.rows.forEach((r, i) => {
      if (r.when?.shown.case === "merged") r.when.shown.value.atMs = BigInt(2_000_000_000 - i);
    });
    const band = drawnBand(false, [alpha, beta]);
    laidOut(band, 150, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert — room for one header and two rows: beta's two.
    expect(groupsOf(band.pane).map((g) => [g.hidden, rowsOf(g).filter((r) => !r.hidden).length])).toEqual([
      [true, 0],
      [false, 2],
    ]);
  });

  it("does not fit a collapsed repository's group", () => {
    // Arrange — beta is collapsed; alpha alone takes the room.
    const band = drawnBand(false, [
      mergedGroup({ id: "alpha", rows: merges(2, "a") }),
      mergedGroup({ id: "beta", rows: merges(2, "b"), repositoryCollapsed: true }),
    ]);
    laidOut(band, 1000, 100);
    // Act
    fitMergedSection(band.scroller, band.pane);
    // Assert
    expect([groupsOf(band.pane)[1].hidden, ...shown(band.pane)]).toEqual([true, 2, "(2)"]);
  });

  it("leaves a band that is not laid out as drawn, every row under the daemon's count", () => {
    // Arrange — the other grouping's pane: nothing measures.
    const band = drawn(false);
    laidOut(band, 170, 0, 0);
    // Act
    const got = fitMergedSection(band.scroller, band.pane);
    // Assert
    expect([got, ...shown(band.pane)]).toEqual([null, 5, "(5)"]);
  });

  it("refuses a band drawn without its row probe", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 170, 100);
    band.pane.querySelector(".merged-row-probe")?.remove();
    // Act / Assert
    expect(() => fitMergedSection(band.scroller, band.pane)).toThrow(/row probe/);
  });

  it("refuses a band drawn without its group header probe", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 170, 100);
    band.pane.querySelector(".merged-group-head-probe")?.remove();
    // Act / Assert
    expect(() => fitMergedSection(band.scroller, band.pane)).toThrow(/group header probe/);
  });

  it("refuses a merged row that states no merged instant", () => {
    // Arrange
    const band = drawn(false);
    laidOut(band, 170, 100);
    rowsOf(groupsOf(band.pane)[0])[0].removeAttribute("data-merged-at");
    // Act / Assert
    expect(() => fitMergedSection(band.scroller, band.pane)).toThrow(/no merged instant/);
  });
});

describe("installMergedFit", () => {
  it("re-fits when the rail is resized", () => {
    // Arrange — fitted at three rows.
    const band = drawn(false);
    laidOut(band, 170, 100);
    const fit = installMergedFit(band.scroller);
    fit.refit();
    // Act — the rail grows by two rows.
    Object.defineProperty(band.scroller, "clientHeight", { configurable: true, value: 210 });
    fireResize(band.scroller);
    // Assert
    expect(shown(band.pane)).toEqual([5, "(5)"]);
    fit.dispose();
  });

  it("re-fits when a pane changes size", () => {
    // Arrange — fitted at three rows.
    const band = drawn(false);
    laidOut(band, 170, 100);
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
