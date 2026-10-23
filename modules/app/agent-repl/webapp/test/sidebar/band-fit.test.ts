// @vitest-environment jsdom
/**
 * THE SECTION FIT (owner requests, 2026-10-11 and 2026-10-18): the rail
 * never scrolls. Once it is out of room, the Open section lengthens until
 * Hidden Workspaces and Recently Merged are left three lines each, the two
 * sharing those six lines, and each section scrolls the rest. Recently
 * Merged claims room only for today's merges. The lowest open section
 * reaches the rail's foot, its cut line fading out. jsdom lays nothing out, so
 * `laidOut` states the geometry the engine would give: a rail SCROLLER px
 * tall, a pane whose content other than the sections' rows regions is
 * OTHERS px, and lines of ROW px each.
 */
import { describe, expect, it } from "vitest";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  BAND_LINES_ATTRIBUTE,
  BAND_MORE_ATTRIBUTE,
  BAND_STRETCHED_ATTRIBUTE,
  allocateSectionLines,
  bandShare,
  fitBands,
  installBandFit,
  mergedTodayLines,
  paintBandMore,
  regionLines,
  regionStops,
} from "../../src/sidebar/band-fit.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fireResize } from "../resize-observer.js";
import {
  NOW,
  fakeTicker,
  hiddenSection,
  mergedSection,
  openSection,
  repoSection,
  roster,
  row,
  sidebarContext,
  startOfToday,
  startOfTomorrow,
} from "./harness.js";
import { boxRect } from "../dom-rect.js";

const ROW = 20;

/** The stops of a section of N lines, ROW px each. */
function stops(n: number): number[] {
  return Array.from({ length: n + 1 }, (_, k) => k * ROW);
}

/** What a rail is drawn with: its rows per section, how many merges are today's, and the folds. */
interface Drawing {
  open?: number;
  hidden?: number;
  merged?: number;
  /** How many of the merged rows, the first ones, landed today. */
  today?: number;
  folded?: { open?: boolean; hidden?: boolean; merged?: boolean };
}

/** A rail drawn as DRAWING says. */
function drawn({ open = 0, hidden = 0, merged = 0, today = 0, folded = {} }: Drawing): { scroller: HTMLElement; pane: HTMLElement } {
  const openRows = Array.from({ length: open }, (_, i) => row({ id: `o-${i}` }));
  const hiddenRows = Array.from({ length: hidden }, (_, i) => row({ id: `h-${i}` }));
  const mergedRows = Array.from({ length: merged }, (_, i) =>
    row({
      id: `m-${i}`,
      closed: true,
      status: { case: "merged", value: {} },
      when: { case: "merged", value: { atMs: BigInt(i < today ? startOfToday() + 60_000 : startOfToday() - (i + 1) * 60_000) } },
    }),
  );
  const scroller = document.createElement("div");
  scroller.className = "sb-scroll";
  scroller.appendChild(
    drawWorkspaceRoster(
      roster({
        repos: open === 0 ? [] : [repoSection({ id: "alpha", rows: openRows })],
        open: openSection(folded.open ?? false),
        hidden: hiddenSection(hiddenRows, hidden, folded.hidden ?? false),
        merged: mergedSection(mergedRows, merged, folded.merged ?? false),
      }),
      sidebarContext(),
    ),
  );
  document.body.replaceChildren(scroller);
  const pane = scroller.querySelector(".sb-pane") as HTMLElement;
  return { scroller, pane };
}

/** A DOMRect at TOP, HEIGHT tall. */
function rect(top: number, height: number): DOMRect {
  return boxRect({ top, height });
}

/** Each section's class and its rows region's selector. */
const REGIONS = {
  "open-section": ":scope > .sb-sections",
  "hidden-section": ":scope > .rows",
  "merged-section": ":scope > .rows",
} as const;

type SectionClass = keyof typeof REGIONS;

/** PANE's section of CLS's rows region. */
function region(pane: HTMLElement, cls: SectionClass): HTMLElement | null {
  return pane.querySelector(`:scope > .${cls}`)?.querySelector<HTMLElement>(REGIONS[cls]) ?? null;
}

/** Lay one section's rows region out: its lines ROW px each, as tall as its height says, else capped by its max-height. */
function layRegion(pane: HTMLElement, cls: SectionClass): void {
  const section = pane.querySelector<HTMLElement>(`:scope > .${cls}`);
  const rows = region(pane, cls);
  if (section === null || rows === null) return;
  const lines = [...rows.querySelectorAll<HTMLElement>(".row")];
  const full = lines.length * ROW;
  const height = (): number => {
    if (section.classList.contains("folded")) return 0;
    if (rows.style.height !== "") return Number.parseFloat(rows.style.height);
    return rows.style.maxHeight === "" ? full : Number.parseFloat(rows.style.maxHeight);
  };
  rows.getBoundingClientRect = () => rect(0, height());
  lines.forEach((line, i) => {
    line.getBoundingClientRect = () => rect(i * ROW - rows.scrollTop, ROW);
  });
  Object.defineProperty(rows, "scrollHeight", { configurable: true, value: full });
  Object.defineProperty(rows, "clientHeight", { configurable: true, get: height });
}

/** The drawn height of PANE's section of CLS's rows region. */
function regionHeight(pane: HTMLElement, cls: SectionClass): number {
  return region(pane, cls)?.getBoundingClientRect().height ?? 0;
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

const SECTIONS: SectionClass[] = ["open-section", "hidden-section", "merged-section"];

/**
 * State the layout: the rail's height and the pane's other content. OTHERS
 * may read the sections' drawn heights, for a layout whose other content
 * moves with the fit.
 */
function laidOut(
  { scroller, pane }: { scroller: HTMLElement; pane: HTMLElement },
  rail: number,
  others: number | (() => number),
): void {
  for (const cls of SECTIONS) layRegion(pane, cls);
  railHeight(scroller, rail);
  const other = typeof others === "number" ? () => others : others;
  pane.getBoundingClientRect = () => rect(0, other() + SECTIONS.reduce((sum, cls) => sum + regionHeight(pane, cls), 0));
}

/** A section's box in PANE. */
function band(pane: HTMLElement, cls: SectionClass): HTMLElement {
  return pane.querySelector(`:scope > .${cls}`) as HTMLElement;
}

/** A section's region max-height, as the fit wrote it. */
function maxHeight(pane: HTMLElement, cls: SectionClass): string {
  return (region(pane, cls) as HTMLElement).style.maxHeight;
}

/** Every section's region max-height, top to bottom. */
function maxHeights(pane: HTMLElement): string[] {
  return SECTIONS.map((cls) => maxHeight(pane, cls));
}

/** Every section's region height, as the fit wrote it, top to bottom. */
function heights(pane: HTMLElement): string[] {
  return SECTIONS.map((cls) => (region(pane, cls) as HTMLElement).style.height);
}

/** What `allocateSectionLines` is handed: sections of O, H and M lines, TODAY of M today's. */
function sections(o: number, h: number, m: number, today = 0) {
  return { open: stops(o), hidden: stops(h), merged: stops(m), mergedToday: today };
}

describe("bandShare", () => {
  it.each([
    { name: "three lines each to two long bands", hidden: 10, today: 10, want: { hidden: 3, merged: 3 } },
    { name: "five to Hidden Workspaces beside one merge today", hidden: 10, today: 1, want: { hidden: 5, merged: 1 } },
    { name: "four to Hidden Workspaces beside two merges today", hidden: 10, today: 2, want: { hidden: 4, merged: 2 } },
    { name: "a short Hidden Workspaces' lines to today's merges", hidden: 2, today: 10, want: { hidden: 2, merged: 4 } },
    { name: "only the lines two short bands need", hidden: 3, today: 2, want: { hidden: 3, merged: 2 } },
    { name: "nothing to two empty bands", hidden: 0, today: 0, want: { hidden: 0, merged: 0 } },
  ])("shares $name", ({ hidden, today, want }) => {
    // Arrange / Act
    const got = bandShare(hidden, today);
    // Assert
    expect(got).toEqual(want);
  });
});

describe("allocateSectionLines", () => {
  it.each([
    { name: "every section whole when all fit", room: 12 * ROW, s: sections(2, 4, 6), want: { open: 2, hidden: 4, merged: 6 } },
    { name: "the Open section all but three lines of each band", room: 20 * ROW, s: sections(40, 40, 40, 40), want: { open: 14, hidden: 3, merged: 3 } },
    { name: "five lines to Hidden Workspaces beside one merge today", room: 20 * ROW, s: sections(40, 40, 1, 1), want: { open: 14, hidden: 5, merged: 1 } },
    { name: "four lines to Hidden Workspaces beside two merges today", room: 20 * ROW, s: sections(40, 40, 2, 2), want: { open: 14, hidden: 4, merged: 2 } },
    { name: "no line to older merges while the Open section scrolls", room: 20 * ROW, s: sections(40, 2, 10, 0), want: { open: 18, hidden: 2, merged: 0 } },
    { name: "the lines reaching today's merges, older ones above them", room: 20 * ROW, s: sections(40, 40, 10, 2), want: { open: 14, hidden: 4, merged: 2 } },
    { name: "older merges the room a whole Open and Hidden leave", room: 10 * ROW, s: sections(3, 2, 30, 0), want: { open: 3, hidden: 2, merged: 5 } },
    { name: "Hidden Workspaces grown before older merges", room: 10 * ROW, s: sections(2, 10, 10, 0), want: { open: 2, hidden: 8, merged: 0 } },
    { name: "Hidden Workspaces grown before today's merges", room: 13 * ROW, s: sections(2, 20, 20, 20), want: { open: 2, hidden: 8, merged: 3 } },
    { name: "every merge the room a whole Hidden Workspaces leaves", room: 12 * ROW, s: sections(0, 1, 20, 9), want: { open: 0, hidden: 1, merged: 11 } },
    { name: "a short rail's bands shrunk, the longer first", room: 5 * ROW, s: sections(10, 10, 10, 10), want: { open: 0, hidden: 3, merged: 2 } },
    { name: "nothing in no room at all", room: -100, s: sections(10, 10, 10, 10), want: { open: 0, hidden: 0, merged: 0 } },
    { name: "a folded Open section's room to the bands", room: 9 * ROW, s: sections(0, 5, 40, 0), want: { open: 0, hidden: 5, merged: 4 } },
    {
      name: "the room a tall Open line cannot use to Hidden Workspaces",
      room: 10 * ROW,
      s: { open: [0, ROW, 2 * ROW, 5 * ROW], hidden: stops(10), merged: stops(10), mergedToday: 0 },
      want: { open: 2, hidden: 8, merged: 0 },
    },
    {
      name: "the room a tall Open line and a whole Hidden Workspaces leave to Recently Merged",
      room: 8 * ROW,
      s: { open: [0, ROW, 2 * ROW, 5 * ROW], hidden: stops(4), merged: stops(10), mergedToday: 0 },
      want: { open: 2, hidden: 4, merged: 2 },
    },
    {
      name: "the room a tall Hidden line cannot use to Recently Merged",
      room: 9 * ROW,
      s: { open: stops(1), hidden: [0, ROW, 2 * ROW, 3 * ROW, 4 * ROW, 8 * ROW], merged: stops(10), mergedToday: 3 },
      want: { open: 1, hidden: 4, merged: 4 },
    },
  ])("allocates $name", ({ room, s, want }) => {
    // Arrange / Act
    const got = allocateSectionLines(room, s);
    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "no stops", s: { ...sections(3, 3, 3), hidden: [] } },
    { name: "stops not starting at zero", s: { ...sections(3, 3, 3), open: [5, 20] } },
    { name: "shrinking stops", s: { ...sections(3, 3, 3), merged: [0, 20, 10] } },
    { name: "more lines of today's merges than the band has", s: sections(3, 3, 3, 4) },
    { name: "a negative count of today's merges", s: sections(3, 3, 3, -1) },
    { name: "a fractional count of today's merges", s: sections(3, 3, 3, 1.5) },
  ])("refuses $name", ({ s }) => {
    // Arrange / Act / Assert
    expect(() => allocateSectionLines(100, s)).toThrow(RangeError);
  });
});

describe("regionLines", () => {
  it("counts a folded repository's header as a line", () => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = '<div class="repo folded"><div class="repo-head"></div><div class="rows"><div class="row"></div></div></div><div class="row"></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".repo-head, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    (box.querySelector(".repo .row") as HTMLElement).getBoundingClientRect = () => rect(0, 0);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["repo-head", "row"]);
  });

  it("counts an empty subsection's header as a line", () => {
    // Arrange: an empty subsection draws its header alone.
    const box = document.createElement("div");
    box.innerHTML = '<div class="sb-subsection" data-empty=""><div class="sb-subsection-head"></div><div class="rows"></div></div><div class="row"></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".sb-subsection-head, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["sb-subsection-head", "row"]);
  });

  it("counts a folded subsection's header as a line, and its hidden rows as none", () => {
    // Arrange: the stylesheet hides a collapsed subsection's rows and slot.
    const box = document.createElement("div");
    box.innerHTML = '<div class="sb-subsection folded"><div class="sb-subsection-head"></div><div class="rows"><div class="row"></div></div></div><div class="row"></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".sb-subsection-head, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    for (const el of box.querySelectorAll<HTMLElement>(".sb-subsection .row")) el.getBoundingClientRect = () => rect(0, 0);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["sb-subsection-head", "row"]);
  });

  it("counts an expanded subsection's header as no line", () => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = '<div class="sb-subsection"><div class="sb-subsection-head"></div><div class="rows"><div class="row"></div></div></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".sb-subsection-head, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["row"]);
  });

  it("counts an expanded repository's header as no line", () => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = '<div class="repo"><div class="repo-head"></div><div class="rows"><div class="row"></div></div></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".repo-head, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["row"]);
  });
});

describe("mergedTodayLines", () => {
  /** A region of merged lines landed at each instant of AT. */
  function mergedLines(at: Array<number | null>): HTMLElement[] {
    return at.map((atMs) => {
      const ws = document.createElement("div");
      if (atMs !== null) ws.setAttribute("data-merged-at", String(atMs));
      const line = document.createElement("div");
      line.className = "row";
      ws.appendChild(line);
      return line;
    });
  }

  it.each([
    { name: "every line through the last of today's", at: [startOfToday() + 5, startOfToday() - 5, startOfToday() + 1, startOfToday() - 9], want: 3 },
    { name: "no line when none landed today", at: [startOfToday() - 5, startOfToday() - 9], want: 0 },
    { name: "no line for a merge of tomorrow's date", at: [startOfTomorrow() + 5], want: 0 },
    { name: "every line when all landed today", at: [startOfToday() + 5, startOfToday() + 1], want: 2 },
  ])("reaches $name", ({ at, want }) => {
    // Arrange / Act
    const got = mergedTodayLines(mergedLines(at), NOW);
    // Assert
    expect(got).toBe(want);
  });

  it("reads a nested family row's instant off the row it hangs under", () => {
    // Arrange
    const [line] = mergedLines([startOfToday() + 5]);
    const kid = document.createElement("div");
    kid.className = "row";
    line.parentElement?.appendChild(kid);
    // Act / Assert
    expect(mergedTodayLines([line, kid], NOW)).toBe(2);
  });

  it("refuses a line outside any stamped row", () => {
    // Arrange / Act / Assert
    expect(() => mergedTodayLines(mergedLines([null]), NOW)).toThrow(/no merged instant/);
  });

  it("logs a line outside any stamped row", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => mergedTodayLines(mergedLines([startOfToday(), null]), NOW)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.unstamped");
    expect([record.level.case, record.context]).toMatchObject(["error", { line: 1, merged_at: null }]);
  });
});

describe("regionStops", () => {
  it("measures one stop per line and the whole region last", () => {
    // Arrange
    const { pane } = drawn({ hidden: 3 });
    layRegion(pane, "hidden-section");
    // Act
    const got = regionStops(region(pane, "hidden-section") as HTMLElement);
    // Assert
    expect(got).toEqual([0, 20, 40, 60]);
  });

  it("measures a scrolled region as an unscrolled one", () => {
    // Arrange
    const { pane } = drawn({ hidden: 3 });
    const rows = region(pane, "hidden-section") as HTMLElement;
    layRegion(pane, "hidden-section");
    rows.scrollTop = 25;
    // Act
    const got = regionStops(rows);
    // Assert
    expect(got).toEqual([0, 20, 40, 60]);
  });

  it("ends each stop on its line's bottom, a header above a line riding with it", () => {
    // Arrange — a 10px group header above the second line.
    const rows = document.createElement("div");
    for (const top of [0, 30]) {
      const line = document.createElement("div");
      line.className = "row";
      line.getBoundingClientRect = () => rect(top, ROW);
      rows.appendChild(line);
    }
    rows.getBoundingClientRect = () => rect(0, 50);
    Object.defineProperty(rows, "scrollHeight", { configurable: true, value: 50 });
    // Act / Assert
    expect(regionStops(rows)).toEqual([0, 20, 50]);
  });

  it("counts a line that draws no box as no line", () => {
    // Arrange — the middle line sits in a collapsed repository's group.
    const rows = document.createElement("div");
    for (const [top, height] of [[0, ROW], [0, 0], [20, ROW]]) {
      const line = document.createElement("div");
      line.className = "row";
      line.getBoundingClientRect = () => rect(top, height);
      rows.appendChild(line);
    }
    rows.getBoundingClientRect = () => rect(0, 40);
    Object.defineProperty(rows, "scrollHeight", { configurable: true, value: 40 });
    // Act / Assert
    expect(regionStops(rows)).toEqual([0, 20, 40]);
  });

  it("measures a region with no drawn line as no lines", () => {
    // Arrange
    const rows = document.createElement("div");
    // Act / Assert
    expect(regionStops(rows)).toEqual([0]);
  });
});

describe("paintBandMore", () => {
  /** A section around a rows region scrolled to TOP, CLIENT px tall over SCROLL px of rows. */
  function scrolled(top: number, client: number, scroll: number): HTMLElement {
    const section = document.createElement("div");
    const rows = document.createElement("div");
    section.appendChild(rows);
    rows.scrollTop = top;
    Object.defineProperty(rows, "clientHeight", { configurable: true, value: client });
    Object.defineProperty(rows, "scrollHeight", { configurable: true, value: scroll });
    return rows;
  }

  it.each([
    { name: "marks a viewport with rows below its edge", top: 0, client: 70, scroll: 200, want: true },
    { name: "clears a viewport scrolled to its end", top: 130, client: 70, scroll: 200, want: false },
    { name: "clears a viewport its rows do not fill", top: 0, client: 200, scroll: 200, want: false },
    { name: "clears a viewport half a pixel short of its end", top: 129.6, client: 70, scroll: 200, want: false },
  ])("$name", ({ top, client, scroll, want }) => {
    // Arrange
    const rows = scrolled(top, client, scroll);
    // Act
    paintBandMore(rows);
    // Assert
    expect(rows.parentElement?.hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(want);
  });

  it("refuses a rows region that hangs in no section", () => {
    // Arrange / Act / Assert
    expect(() => paintBandMore(document.createElement("div"))).toThrow(/no section/);
  });

  it("logs a rows region that hangs in no section", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => paintBandMore(document.createElement("div"))).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.orphan-region");
    expect(record.level.case).toBe("error");
  });
});

describe("fitBands", () => {
  it("lengthens the Open section until each band keeps three lines", () => {
    // Arrange — room for twenty lines below 100px of other content.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 500, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([...maxHeights(rail.pane), heights(rail.pane)[2]]).toEqual(["280px", "60px", "", "60px"]);
  });

  it("stretches Recently Merged to the rail's foot through a line it cuts", () => {
    // Arrange — 410px of room: twenty whole lines and half of one more.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "", "70px"]);
  });

  it("states the whole lines a stretched band shows", () => {
    // Arrange — half a line of stretch.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "merged-section").getAttribute(BAND_LINES_ATTRIBUTE)).toBe("3");
  });

  it("stretches Recently Merged to the rail's foot when every line fits", () => {
    // Arrange — 900px of room, 200px of lines.
    const rail = drawn({ open: 3, hidden: 3, merged: 4 });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([...maxHeights(rail.pane), ...heights(rail.pane)]).toEqual(["", "", "", "", "", "780px"]);
  });

  it("stretches Hidden Workspaces when Recently Merged is folded", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { merged: true } });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "840px", ""]);
  });

  it("stretches the Open section when both bands are folded", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { hidden: true, merged: true } });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["900px", "", ""]);
  });

  it("lets go of a stretch once a band below unfolds", () => {
    // Arrange — fitted with Recently Merged folded, Hidden Workspaces stretched.
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { merged: true } });
    laidOut(rail, 1000, 100);
    fitBands(rail.scroller, rail.pane, NOW);
    band(rail.pane, "merged-section").classList.remove("folded");
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "", "780px"]);
  });

  it("measures a stretched band from its rows, not its stretch", () => {
    // Arrange — fitted once, stretched by half a line.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    const first = fitBands(rail.scroller, rail.pane, NOW);
    // Act
    const second = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([second, heights(rail.pane)[2]]).toEqual([first, "70px"]);
  });

  it("marks only the lowest open section as reaching the rail's foot", () => {
    // Arrange
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(SECTIONS.map((cls) => band(rail.pane, cls).hasAttribute(BAND_STRETCHED_ATTRIBUTE))).toEqual([false, false, true]);
  });

  it("marks a stretched band cutting a line with rows below its edge", () => {
    // Arrange — half a line of stretch.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "merged-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(true);
  });

  it("marks a capped Hidden Workspaces band with rows below its edge", () => {
    // Arrange — Hidden Workspaces capped at its share, Recently Merged lowest.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    const hidden = band(rail.pane, "hidden-section");
    expect([hidden.hasAttribute(BAND_STRETCHED_ATTRIBUTE), hidden.hasAttribute(BAND_MORE_ATTRIBUTE)]).toEqual([false, true]);
  });

  it("marks a capped Open band with rows below its edge", () => {
    // Arrange — the Open section capped above the bands, Recently Merged lowest.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    const open = band(rail.pane, "open-section");
    expect([open.hasAttribute(BAND_STRETCHED_ATTRIBUTE), open.hasAttribute(BAND_MORE_ATTRIBUTE)]).toEqual([false, true]);
  });

  it("leaves a stretched band whose rows all show unmarked", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 4 });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "merged-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(false);
  });

  it("clears the foot mark from a band that is no longer lowest", () => {
    // Arrange — fitted with Recently Merged folded, Hidden Workspaces lowest.
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { merged: true } });
    laidOut(rail, 1000, 100);
    fitBands(rail.scroller, rail.pane, NOW);
    band(rail.pane, "merged-section").classList.remove("folded");
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "hidden-section").hasAttribute(BAND_STRETCHED_ATTRIBUTE)).toBe(false);
  });

  it("stretches nothing in a rail shorter than the headers", () => {
    // Arrange — the other content alone overflows the rail.
    const rail = drawn({ open: 3, hidden: 3, merged: 4 });
    laidOut(rail, 50, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "", "0px"]);
  });

  it("states the lines each section shows", () => {
    // Arrange — room for twenty lines, one merge today.
    const rail = drawn({ open: 30, hidden: 10, merged: 5, today: 1 });
    laidOut(rail, 500, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(SECTIONS.map((cls) => band(rail.pane, cls).getAttribute(BAND_LINES_ATTRIBUTE))).toEqual(["14", "5", "1"]);
  });

  it("gives older merges no room while the Open section scrolls", () => {
    // Arrange — room for twenty lines, no merge today.
    const rail = drawn({ open: 30, hidden: 2, merged: 10 });
    laidOut(rail, 500, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 18, hidden: 2, merged: 0 });
  });

  it("leaves every section uncapped when every line fits", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 4 });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(maxHeights(rail.pane)).toEqual(["", "", ""]);
  });

  it("gives a folded band's share to the other band", () => {
    // Arrange — room for ten lines, Hidden Workspaces folded.
    const rail = drawn({ open: 5, hidden: 10, merged: 20, today: 20, folded: { hidden: true } });
    laidOut(rail, 300, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 4, hidden: 0, merged: 6 });
  });

  it("re-fits a capped section from its whole content", () => {
    // Arrange — fitted short, then the rail grows to hold everything.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 180, 100);
    fitBands(rail.scroller, rail.pane, NOW);
    railHeight(rail.scroller, 1000);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 10, hidden: 10, merged: 10 });
  });

  it("keeps the daemon's count on a scrolling band", () => {
    // Arrange
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 180, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "merged-section").querySelector("[data-section-count]")?.textContent).toBe("(10)");
  });

  it("leaves a pane that is not laid out as drawn", () => {
    // Arrange — the other grouping's pane: nothing measures.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 0, 0);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([got, ...maxHeights(rail.pane)]).toEqual([null, "", "", ""]);
  });

  it("fits nothing when no section is open", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 3, folded: { open: true, hidden: true, merged: true } });
    laidOut(rail, 300, 100);
    // Act / Assert
    expect(fitBands(rail.scroller, rail.pane, NOW)).toBeNull();
  });

  it.each([
    { name: "band", cls: "merged-section" as const },
    { name: "Open section", cls: "open-section" as const },
  ])("refuses a $name drawn without its rows region", ({ cls }) => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 3 });
    laidOut(rail, 300, 100);
    region(rail.pane, cls)?.remove();
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow(/rows region/);
  });

  it("measures fractional room, unrounded", () => {
    // Arrange — 119.5px of room is short of six lines of 20px; rounded up it would hold them.
    const rail = drawn({ merged: 6, folded: { hidden: true } });
    laidOut(rail, 219.5, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 0, hidden: 0, merged: 5 });
  });

  it("takes the rail's drawn borders and padding out of the room", () => {
    // Arrange — a 240px rail framed by 10px of border and 30px of padding leaves 100px past 100px of other content.
    const rail = drawn({ merged: 6, folded: { hidden: true } });
    laidOut(rail, 240, 100);
    rail.scroller.style.border = "5px solid";
    rail.scroller.style.padding = "10px 0 20px";
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 0, hidden: 0, merged: 5 });
  });

  it("refuses a rail whose border is not in px", () => {
    // Arrange
    const rail = drawn({ merged: 6, folded: { hidden: true } });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow(/is not in px/);
  });

  it("logs a rail whose border is not in px", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn({ merged: 6, folded: { hidden: true } });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.unmeasurable");
    expect([record.level.case, record.context]).toMatchObject(["error", { property: "borderTopWidth", value: "medium" }]);
  });

  it("leaves the sections unsized when the rail's border is not in px", () => {
    // Arrange
    const rail = drawn({ merged: 6, folded: { hidden: true } });
    laidOut(rail, 220, 100);
    rail.scroller.style.borderTopWidth = "medium";
    // Act
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow();
    // Assert
    expect([maxHeight(rail.pane, "merged-section"), band(rail.pane, "merged-section").hasAttribute(BAND_LINES_ATTRIBUTE)]).toEqual(["", false]);
  });

  it("refuses a fit that moves its own room", () => {
    // Arrange — the pane's other content grows a pixel whenever Recently Merged is capped.
    const rail = drawn({ merged: 10, folded: { hidden: true } });
    laidOut(rail, 200, () => ((region(rail.pane, "merged-section") as HTMLElement).style.height === "" ? 100 : 101));
    // Act / Assert
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow(/moved the room/);
  });

  it("logs a fit that moves its own room", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn({ merged: 10, folded: { hidden: true } });
    laidOut(rail, 200, () => ((region(rail.pane, "merged-section") as HTMLElement).style.height === "" ? 100 : 101));
    // Act
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.unsettled");
    expect([record.level.case, record.context]).toMatchObject([
      "error",
      { room: 100, fitted_room: 99, merged_total: 10, merged_today: 0, merged_shown: 5 },
    ]);
  });

  it("logs a band drawn without its rows region", async () => {
    // Arrange
    const capture = captureLogRecords();
    const rail = drawn({ hidden: 3, merged: 3 });
    laidOut(rail, 300, 100);
    region(rail.pane, "merged-section")?.remove();
    // Act
    expect(() => fitBands(rail.scroller, rail.pane, NOW)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.band-fit.malformed");
    expect([record.level.case, record.context]).toMatchObject(["error", { band: "merged-section" }]);
  });
});

describe("installBandFit", () => {
  it("re-fits when the rail is resized", () => {
    // Arrange — fitted out of room.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 180, 100);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    // Act — the rail grows to hold everything.
    railHeight(rail.scroller, 1000);
    fireResize(rail.scroller);
    // Assert
    expect(maxHeights(rail.pane)).toEqual(["", "", ""]);
    fit.dispose();
  });

  it("re-fits when a pane changes size", () => {
    // Arrange — fitted out of room.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 500, 400);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    // Act — the pane's other content shrinks, freeing room for everything.
    laidOut(rail, 700, 50);
    fireResize(rail.pane);
    // Assert
    expect(maxHeights(rail.pane)).toEqual(["", "", ""]);
    fit.dispose();
  });

  it("re-fits when a box inside a capped section changes size", () => {
    // Arrange — fitted out of room.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 500, 400);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    const repo = region(rail.pane, "open-section")?.firstElementChild as HTMLElement;
    // Act — the room grows while the Open section's repository resizes inside its capped viewport.
    laidOut(rail, 700, 50);
    fireResize(repo);
    // Assert
    expect(maxHeights(rail.pane)).toEqual(["", "", ""]);
    fit.dispose();
  });

  it("re-fits when the local date turns over", () => {
    // Arrange — one merge today holds a line of the share; tomorrow it holds none.
    const ticker = fakeTicker();
    const rail = drawn({ open: 30, hidden: 10, merged: 1, today: 1 });
    laidOut(rail, 500, 100);
    const fit = installBandFit(rail.scroller, ticker);
    fit.refit();
    // Act
    ticker.tick(startOfTomorrow() + 1);
    // Assert
    expect(band(rail.pane, "merged-section").getAttribute(BAND_LINES_ATTRIBUTE)).toBe("0");
    fit.dispose();
  });

  it("leaves the fit alone on a tick of the same date", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = drawn({ open: 30, hidden: 10, merged: 1, today: 1 });
    laidOut(rail, 500, 100);
    const fit = installBandFit(rail.scroller, ticker);
    fit.refit();
    railHeight(rail.scroller, 1000);
    // Act
    ticker.tick(NOW + 1);
    // Assert
    expect(maxHeight(rail.pane, "open-section")).toBe("280px");
    fit.dispose();
  });

  it("repaints a section's foot mark when its rows scroll to their end", () => {
    // Arrange — fitted with a line cut at the foot.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    const rows = region(rail.pane, "merged-section") as HTMLElement;
    // Act
    rows.scrollTop = 200;
    rows.dispatchEvent(new Event("scroll"));
    // Assert
    expect(band(rail.pane, "merged-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(false);
    fit.dispose();
  });

  it("stops repainting the foot mark once disposed", () => {
    // Arrange
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    fit.dispose();
    const rows = region(rail.pane, "merged-section") as HTMLElement;
    // Act
    rows.scrollTop = 200;
    rows.dispatchEvent(new Event("scroll"));
    // Assert
    expect(band(rail.pane, "merged-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(true);
  });

  it("stops listening to the ticker once disposed", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = drawn({ open: 3 });
    const fit = installBandFit(rail.scroller, ticker);
    // Act
    fit.dispose();
    // Assert
    expect(ticker.subscribers()).toBe(0);
  });
});
