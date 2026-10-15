// @vitest-environment jsdom
/**
 * THE BAND FIT (owner request, 2026-10-11): once the rail is out of room, the
 * Hidden Workspaces and Recently Merged bands are scrolling viewports of at
 * most six lines each, the one line short of both going to Hidden Workspaces
 * so long as Recently Merged keeps five. jsdom lays nothing out, so `laidOut`
 * states the geometry the engine would give: a rail SCROLLER px tall, a pane
 * whose content other than the bands' rows regions is OTHERS px, and lines of
 * ROW px each.
 */
import { describe, expect, it } from "vitest";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  BAND_LINES_ATTRIBUTE,
  allocateBandLines,
  fitBands,
  installBandFit,
  regionStops,
} from "../../src/sidebar/band-fit.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fireResize } from "../resize-observer.js";
import { hiddenSection, mergedSection, roster, row, sidebarContext } from "./harness.js";

const ROW = 20;

/** The stops of a band of N lines, ROW px each. */
function stops(n: number): number[] {
  return Array.from({ length: n + 1 }, (_, k) => k * ROW);
}

/** A rail with HIDDEN hidden and MERGED merged rows, each band folded or not. */
function drawn(
  hidden: number,
  merged: number,
  folded: { hidden?: boolean; merged?: boolean } = {},
): { scroller: HTMLElement; pane: HTMLElement } {
  const hiddenRows = Array.from({ length: hidden }, (_, i) => row({ id: `h-${i}` }));
  const mergedRows = Array.from({ length: merged }, (_, i) =>
    row({ id: `m-${i}`, closed: true, status: { case: "merged", value: {} } }),
  );
  const scroller = document.createElement("div");
  scroller.className = "sb-scroll";
  scroller.appendChild(
    drawWorkspaceRoster(
      roster({
        hidden: hiddenSection(hiddenRows, hidden, folded.hidden ?? false),
        merged: mergedSection(mergedRows, merged, folded.merged ?? false),
      }),
      sidebarContext(),
    ),
  );
  document.body.replaceChildren(scroller);
  const pane = scroller.querySelector('.sb-pane.sb-pane') as HTMLElement;
  return { scroller, pane };
}

/** A DOMRect at TOP, HEIGHT tall. */
function rect(top: number, height: number): DOMRect {
  return { top, height, bottom: top + height, left: 0, right: 0, width: 0, x: 0, y: top, toJSON: () => ({}) };
}

/** Lay one band's rows region out: its lines ROW px each, capped by its max-height. */
function layRegion(section: HTMLElement | null): void {
  const region = section?.querySelector<HTMLElement>(":scope > .rows");
  if (section === null || section === undefined || region === null || region === undefined) return;
  const lines = [...region.querySelectorAll<HTMLElement>(".row")];
  const full = lines.length * ROW;
  const height = (): number => {
    if (section.classList.contains("folded")) return 0;
    return region.style.maxHeight === "" ? full : Number.parseFloat(region.style.maxHeight);
  };
  region.getBoundingClientRect = () => rect(0, height());
  lines.forEach((line, i) => {
    line.getBoundingClientRect = () => rect(i * ROW - region.scrollTop, ROW);
  });
  Object.defineProperty(region, "scrollHeight", { configurable: true, value: full });
}

/** The drawn height of PANE's band of CLS's rows region. */
function regionHeight(pane: HTMLElement, cls: string): number {
  return pane.querySelector<HTMLElement>(`:scope > ${cls} > .rows`)?.getBoundingClientRect().height ?? 0;
}

/**
 * State the rail scroller's drawn height, framed by no border or padding:
 * declared in px, as an engine computes every length (jsdom computes an
 * undeclared one as a keyword).
 */
function railHeight(scroller: HTMLElement, rail: number): void {
  scroller.style.border = "0px none";
  scroller.style.padding = "0px";
  scroller.getBoundingClientRect = () => rect(0, rail);
}

/**
 * State the layout: the rail's height and the pane's other content. OTHERS
 * may read the bands' drawn heights, for a layout whose other content moves
 * with the fit.
 */
function laidOut(
  { scroller, pane }: { scroller: HTMLElement; pane: HTMLElement },
  rail: number,
  others: number | (() => number),
): void {
  for (const cls of [".hidden-section", ".merged-section"]) layRegion(pane.querySelector<HTMLElement>(`:scope > ${cls}`));
  railHeight(scroller, rail);
  const other = typeof others === "number" ? () => others : others;
  pane.getBoundingClientRect = () =>
    rect(0, other() + regionHeight(pane, ".hidden-section") + regionHeight(pane, ".merged-section"));
}

/** A band's section in PANE. */
function band(pane: HTMLElement, cls: "hidden-section" | "merged-section"): HTMLElement {
  return pane.querySelector(`:scope > .${cls}`) as HTMLElement;
}

/** A band's region max-height, as the fit wrote it. */
function maxHeight(pane: HTMLElement, cls: "hidden-section" | "merged-section"): string {
  return (band(pane, cls).querySelector(":scope > .rows") as HTMLElement).style.maxHeight;
}

describe("allocateBandLines", () => {
  it.each([
    { name: "both bands whole when they fit", room: 10 * ROW, h: 4, m: 6, want: { hidden: 4, merged: 6 } },
    { name: "six lines each in room for exactly twelve", room: 12 * ROW, h: 40, m: 40, want: { hidden: 6, merged: 6 } },
    { name: "the eleventh line to Hidden Workspaces", room: 11 * ROW, h: 40, m: 40, want: { hidden: 6, merged: 5 } },
    { name: "six lines each in room for ten, the rail scrolling", room: 10 * ROW, h: 40, m: 40, want: { hidden: 6, merged: 6 } },
    { name: "six lines each in no room at all", room: -100, h: 40, m: 40, want: { hidden: 6, merged: 6 } },
    { name: "the thirteenth line to Hidden Workspaces first", room: 13 * ROW, h: 40, m: 40, want: { hidden: 7, merged: 6 } },
    { name: "spare room grown one line each in turn", room: 14 * ROW, h: 40, m: 40, want: { hidden: 7, merged: 7 } },
    { name: "a short Hidden Workspaces' spare room to Recently Merged", room: 20 * ROW, h: 2, m: 40, want: { hidden: 2, merged: 18 } },
    { name: "Recently Merged down to five when that fits", room: 8 * ROW, h: 3, m: 40, want: { hidden: 3, merged: 5 } },
    { name: "a Recently Merged of five kept whole", room: 9 * ROW, h: 40, m: 5, want: { hidden: 6, merged: 5 } },
    { name: "a folded Hidden Workspaces' room to Recently Merged", room: 3 * ROW, h: 0, m: 40, want: { hidden: 0, merged: 6 } },
  ])("allocates $name", ({ room, h, m, want }) => {
    // Arrange / Act
    const got = allocateBandLines(room, stops(h), stops(m));
    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "no stops", hidden: [] },
    { name: "stops not starting at zero", hidden: [5, 20] },
    { name: "shrinking stops", hidden: [0, 20, 10] },
  ])("refuses $name", ({ hidden }) => {
    // Arrange / Act / Assert
    expect(() => allocateBandLines(100, hidden, stops(3))).toThrow(RangeError);
  });
});

describe("regionStops", () => {
  it("measures one stop per line and the whole region last", () => {
    // Arrange
    const { pane } = drawn(3, 0);
    layRegion(band(pane, "hidden-section"));
    // Act
    const got = regionStops(band(pane, "hidden-section").querySelector(":scope > .rows") as HTMLElement);
    // Assert
    expect(got).toEqual([0, 20, 40, 60]);
  });

  it("measures a scrolled region as an unscrolled one", () => {
    // Arrange
    const { pane } = drawn(3, 0);
    const region = band(pane, "hidden-section").querySelector(":scope > .rows") as HTMLElement;
    layRegion(band(pane, "hidden-section"));
    region.scrollTop = 25;
    // Act
    const got = regionStops(region);
    // Assert
    expect(got).toEqual([0, 20, 40, 60]);
  });

  it("ends each stop on its line's bottom, a header above a line riding with it", () => {
    // Arrange — a 10px group header above the second line.
    const region = document.createElement("div");
    const tops = [0, 30];
    for (const top of tops) {
      const line = document.createElement("div");
      line.className = "row";
      line.getBoundingClientRect = () => rect(top, ROW);
      region.appendChild(line);
    }
    region.getBoundingClientRect = () => rect(0, 50);
    Object.defineProperty(region, "scrollHeight", { configurable: true, value: 50 });
    // Act / Assert
    expect(regionStops(region)).toEqual([0, 20, 50]);
  });

  it("counts a line that draws no box as no line", () => {
    // Arrange — the middle line sits in a collapsed repository's group.
    const region = document.createElement("div");
    for (const [top, height] of [[0, ROW], [0, 0], [20, ROW]]) {
      const line = document.createElement("div");
      line.className = "row";
      line.getBoundingClientRect = () => rect(top, height);
      region.appendChild(line);
    }
    region.getBoundingClientRect = () => rect(0, 40);
    Object.defineProperty(region, "scrollHeight", { configurable: true, value: 40 });
    // Act / Assert
    expect(regionStops(region)).toEqual([0, 20, 40]);
  });

  it("measures a region with no drawn line as no lines", () => {
    // Arrange
    const region = document.createElement("div");
    // Act / Assert
    expect(regionStops(region)).toEqual([0]);
  });
});

describe("fitBands", () => {
  it("caps both bands at six lines once the rail is out of room", () => {
    // Arrange — room for four lines below 100px of other content.
    const rail = drawn(10, 10);
    laidOut(rail, 180, 100);
    // Act
    fitBands(rail.scroller, rail.pane);
    // Assert
    expect([maxHeight(rail.pane, "hidden-section"), maxHeight(rail.pane, "merged-section")]).toEqual(["120px", "120px"]);
  });

  it("states the lines each band shows", () => {
    // Arrange — room for eleven lines.
    const rail = drawn(10, 10);
    laidOut(rail, 320, 100);
    // Act
    fitBands(rail.scroller, rail.pane);
    // Assert
    expect([
      band(rail.pane, "hidden-section").getAttribute(BAND_LINES_ATTRIBUTE),
      band(rail.pane, "merged-section").getAttribute(BAND_LINES_ATTRIBUTE),
    ]).toEqual(["6", "5"]);
  });

  it("leaves both bands uncapped when every line fits", () => {
    // Arrange
    const rail = drawn(3, 4);
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane);
    // Assert
    expect([maxHeight(rail.pane, "hidden-section"), maxHeight(rail.pane, "merged-section")]).toEqual(["", ""]);
  });

  it("gives a folded band's room to the other", () => {
    // Arrange — room for ten lines, Hidden Workspaces folded.
    const rail = drawn(10, 20, { hidden: true });
    laidOut(rail, 300, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane);
    // Assert
    expect(got).toEqual({ hidden: 0, merged: 10 });
  });

  it("re-fits a capped band from its whole content", () => {
    // Arrange — fitted short, then the rail grows to hold everything.
    const rail = drawn(10, 10);
    laidOut(rail, 180, 100);
    fitBands(rail.scroller, rail.pane);
    railHeight(rail.scroller, 1000);
    // Act
    const got = fitBands(rail.scroller, rail.pane);
    // Assert
    expect(got).toEqual({ hidden: 10, merged: 10 });
  });

  it("keeps the daemon's count on a scrolling band", () => {
    // Arrange
    const rail = drawn(10, 10);
    laidOut(rail, 180, 100);
    // Act
    fitBands(rail.scroller, rail.pane);
    // Assert
    expect(band(rail.pane, "merged-section").querySelector("[data-section-count]")?.textContent).toBe("(10)");
  });

  it("leaves a pane that is not laid out as drawn", () => {
    // Arrange — the other grouping's pane: nothing measures.
    const rail = drawn(10, 10);
    laidOut(rail, 0, 0);
    // Act
    const got = fitBands(rail.scroller, rail.pane);
    // Assert
    expect([got, maxHeight(rail.pane, "hidden-section"), maxHeight(rail.pane, "merged-section")]).toEqual([null, "", ""]);
  });

  it("fits nothing when neither band is open", () => {
    // Arrange
    const rail = drawn(3, 3, { hidden: true, merged: true });
    laidOut(rail, 300, 100);
    // Act / Assert
    expect(fitBands(rail.scroller, rail.pane)).toBeNull();
  });

  it("refuses a band drawn without its rows region", () => {
    // Arrange
    const rail = drawn(3, 3);
    laidOut(rail, 300, 100);
    band(rail.pane, "merged-section").querySelector(":scope > .rows")?.remove();
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow(/rows region/);
  });

  it("measures fractional room, unrounded", () => {
    // Arrange — 119.5px of room is short of six lines of 20px; rounded up it would hold them.
    const rail = drawn(3, 6, { hidden: true });
    laidOut(rail, 219.5, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane);
    // Assert
    expect(got).toEqual({ hidden: 0, merged: 5 });
  });

  it("takes the rail's drawn borders and padding out of the room", () => {
    // Arrange — a 240px rail framed by 10px of border and 30px of padding leaves 100px past 100px of other content.
    const rail = drawn(3, 6, { hidden: true });
    laidOut(rail, 240, 100);
    rail.scroller.style.border = "5px solid";
    rail.scroller.style.padding = "10px 0 20px";
    // Act
    const got = fitBands(rail.scroller, rail.pane);
    // Assert
    expect(got).toEqual({ hidden: 0, merged: 5 });
  });

  it("refuses a rail whose border is not in px", () => {
    const rail = drawn(3, 6, { hidden: true });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow(/is not in px/);
  });

  it("logs a rail whose border is not in px", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn(3, 6, { hidden: true });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.unmeasurable");
    expect([record.level.case, record.context]).toMatchObject(["error", { property: "borderTopWidth", value: "medium" }]);
  });

  it("leaves the bands unsized when the rail's border is not in px", () => {
    // Arrange
    const rail = drawn(3, 6, { hidden: true });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow();
    // Assert
    expect([maxHeight(rail.pane, "merged-section"), band(rail.pane, "merged-section").hasAttribute(BAND_LINES_ATTRIBUTE)]).toEqual(["", false]);
  });

  it("refuses a fit that moves its own room", () => {
    // Arrange — the pane's other content grows a pixel whenever Recently Merged is capped.
    const rail = drawn(3, 10, { hidden: true });
    laidOut(rail, 200, () => (maxHeight(rail.pane, "merged-section") === "" ? 100 : 101));
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow(/moved the room/);
  });

  it("logs a fit that moves its own room", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn(3, 10, { hidden: true });
    laidOut(rail, 200, () => (maxHeight(rail.pane, "merged-section") === "" ? 100 : 101));
    // Act
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.unsettled");
    expect([record.level.case, record.context]).toMatchObject([
      "error",
      { room: 100, fitted_room: 99, merged_total: 10, merged_shown: 5 },
    ]);
  });

  it("logs a band drawn without its rows region", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn(3, 3);
    laidOut(rail, 300, 100);
    band(rail.pane, "merged-section").querySelector(":scope > .rows")?.remove();
    // Act
    expect(() => fitBands(rail.scroller, rail.pane)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.malformed");
    expect(record.level.case).toBe("error");
  });
});

describe("installBandFit", () => {
  it("re-fits when the rail is resized", () => {
    // Arrange — fitted out of room.
    const rail = drawn(10, 10);
    laidOut(rail, 180, 100);
    const fit = installBandFit(rail.scroller);
    fit.refit();
    // Act — the rail grows to hold everything.
    railHeight(rail.scroller, 1000);
    fireResize(rail.scroller);
    // Assert
    expect([maxHeight(rail.pane, "hidden-section"), maxHeight(rail.pane, "merged-section")]).toEqual(["", ""]);
    fit.dispose();
  });

  it("re-fits when a pane changes size", () => {
    // Arrange — fitted out of room.
    const rail = drawn(10, 10);
    laidOut(rail, 500, 400);
    const fit = installBandFit(rail.scroller);
    fit.refit();
    // Act — a live section above folds, freeing room for everything.
    laidOut(rail, 500, 50);
    fireResize(rail.pane);
    // Assert
    expect([maxHeight(rail.pane, "hidden-section"), maxHeight(rail.pane, "merged-section")]).toEqual(["", ""]);
    fit.dispose();
  });
});
