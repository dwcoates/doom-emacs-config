// @vitest-environment jsdom
/**
 * THE SECTION FIT (owner requests, 2026-10-11, 2026-10-18 and 2026-10-27):
 * the rail never scrolls. Once it is out of room, the Open section lengthens
 * until Deploys and Hidden Workspaces below it are left three lines each, the
 * two sharing those six lines, and each section scrolls the rest. Deploys
 * claims room only for its queues and today's merges, and a short rail
 * shrinks Hidden Workspaces before it. The lowest open section
 * reaches the rail's foot, its cut line fading out. jsdom lays nothing out, so
 * `laidOut` states the geometry the engine would give: a rail SCROLLER px
 * tall, a pane whose content other than the sections' rows regions is
 * OTHERS px, and lines of ROW px each.
 */
import { describe, expect, it } from "vitest";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { createRecentlyDeployed } from "../../src/sidebar/recently-deployed.js";
import { MERGE_QUEUES_CLASS } from "../../src/sidebar/merge-queue-view.js";
import {
  BAND_LINES_ATTRIBUTE,
  BAND_MORE_ATTRIBUTE,
  BAND_STRETCHED_ATTRIBUTE,
  allocateSectionLines,
  bandShare,
  fitBands,
  installBandFit,
  mergedTodayLines,
  recentlyDeployedFloorLines,
  paintBandMore,
  regionLines,
  regionStops,
  REGION_SELECTOR,
  sectionRegions,
} from "../../src/sidebar/band-fit.js";
import { captureLogRecords, forwardedRecord, forwardedRecords } from "../log-capture.js";
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
  /** Whether "Recently Deployed" shows its rows (recently-deployed.ts); shown unless said. */
  expanded?: boolean;
}

/** A rail drawn as DRAWING says. */
function drawn({ open = 0, hidden = 0, merged = 0, today = 0, folded = {}, expanded = true }: Drawing): { scroller: HTMLElement; pane: HTMLElement } {
  const recently = createRecentlyDeployed(document, () => undefined);
  if (expanded) recently.toggle();
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
        merged: mergedSection(mergedRows, folded.merged ?? false),
      }),
      sidebarContext(undefined, undefined, undefined, undefined, undefined, recently),
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

/**
 * Lay one section's rows region out: its lines ROW px each, as tall as its
 * height says, else capped by its max-height. A collapsed "Recently Deployed"
 * heading is a line, and the merged rows under it are drawn as none, as the
 * stylesheet draws them.
 */
function layRegion(pane: HTMLElement, cls: SectionClass): void {
  const section = pane.querySelector<HTMLElement>(`:scope > .${cls}`);
  const rows = region(pane, cls);
  if (section === null || rows === null) return;
  const collapsed = rows.querySelector(".sb-merged-label[data-collapsed]") !== null;
  const lines = [...rows.querySelectorAll<HTMLElement>(".row, .sb-merged-label[data-collapsed]")].filter(
    (line) => !(collapsed && line.closest(".merged-group") !== null),
  );
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

const SECTIONS: SectionClass[] = ["open-section", "merged-section", "hidden-section"];

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
function sections(o: number, h: number, m: number, today = 0, floor = 0) {
  return { open: stops(o), hidden: stops(h), merged: stops(m), mergedToday: today, mergedFloor: floor };
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
    const got = bandShare(today, hidden);
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
    { name: "today's merges grown before Hidden Workspaces", room: 13 * ROW, s: sections(2, 20, 20, 20), want: { open: 2, hidden: 3, merged: 8 } },
    { name: "every merge the room a whole Hidden Workspaces leaves", room: 12 * ROW, s: sections(0, 1, 20, 9), want: { open: 0, hidden: 1, merged: 11 } },
    { name: "a short rail's Hidden Workspaces shrunk first", room: 5 * ROW, s: sections(10, 10, 10, 10), want: { open: 0, hidden: 2, merged: 3 } },
    { name: "a short rail's Hidden Workspaces shrunk away before Deploys gives a line", room: 2 * ROW, s: sections(10, 10, 10, 10), want: { open: 0, hidden: 0, merged: 2 } },
    { name: "an expanded Recently Deployed's floor taken from Hidden Workspaces' share", room: 20 * ROW, s: sections(40, 40, 40, 0, 4), want: { open: 14, hidden: 2, merged: 4 } },
    { name: "a floor of the whole share taken from Hidden Workspaces entirely", room: 20 * ROW, s: sections(40, 40, 40, 0, 6), want: { open: 14, hidden: 0, merged: 6 } },
    { name: "a floor beyond the share taken from the Open section", room: 20 * ROW, s: sections(40, 40, 40, 0, 8), want: { open: 12, hidden: 0, merged: 8 } },
    { name: "a floor shrunk only in a rail too short for it", room: 5 * ROW, s: sections(10, 10, 10, 0, 6), want: { open: 0, hidden: 0, merged: 5 } },
    { name: "nothing in no room at all", room: -100, s: sections(10, 10, 10, 10), want: { open: 0, hidden: 0, merged: 0 } },
    { name: "a folded Open section's room to the bands", room: 9 * ROW, s: sections(0, 5, 40, 0), want: { open: 0, hidden: 5, merged: 4 } },
    {
      name: "the room a tall Open line cannot use to Hidden Workspaces",
      room: 10 * ROW,
      s: { open: [0, ROW, 2 * ROW, 5 * ROW], hidden: stops(10), merged: stops(10), mergedToday: 0, mergedFloor: 0 },
      want: { open: 2, hidden: 8, merged: 0 },
    },
    {
      name: "the room a tall Open line and a whole Hidden Workspaces leave to older merges",
      room: 8 * ROW,
      s: { open: [0, ROW, 2 * ROW, 5 * ROW], hidden: stops(4), merged: stops(10), mergedToday: 0, mergedFloor: 0 },
      want: { open: 2, hidden: 4, merged: 2 },
    },
    {
      name: "the room a tall Hidden line cannot use to older merges",
      room: 9 * ROW,
      s: { open: stops(1), hidden: [0, ROW, 2 * ROW, 3 * ROW, 4 * ROW, 8 * ROW], merged: stops(10), mergedToday: 3, mergedFloor: 0 },
      want: { open: 1, hidden: 4, merged: 4 },
    },
  ])("allocates $name", ({ room, s, want }) => {
    // Arrange / Act
    const got = allocateSectionLines(room, s);
    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "Open its ungated lines, the bands none, when the gate takes their share", room: 20 * ROW, gate: 6 * ROW, s: sections(40, 40, 40, 40), want: { open: 20, hidden: 0, merged: 0 } },
    { name: "the gate's height out of Hidden Workspaces before today's merges", room: 20 * ROW, gate: 3 * ROW, s: sections(40, 40, 40, 40), want: { open: 17, hidden: 0, merged: 3 } },
    { name: "the bands' remaining room to today's merges, then Hidden Workspaces", room: 20 * ROW, gate: 2 * ROW, s: sections(40, 40, 10, 2), want: { open: 16, hidden: 2, merged: 2 } },
    { name: "Open lines only where Open alone overruns the gated rail", room: 10 * ROW, gate: 5 * ROW, s: sections(40, 0, 0, 0), want: { open: 10, hidden: 0, merged: 0 } },
    { name: "every section whole when all fit beside the gate", room: 12 * ROW, gate: 6 * ROW, s: sections(2, 4, 6), want: { open: 2, hidden: 4, merged: 6 } },
    { name: "older merges the room a whole Open leaves beside the gate", room: 10 * ROW, gate: 4 * ROW, s: sections(3, 2, 30, 0), want: { open: 3, hidden: 2, merged: 5 } },
  ])("beside a standing gate, allocates $name", ({ room, gate, s, want }) => {
    // Arrange / Act
    const got = allocateSectionLines(room, s, gate);
    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "a negative gate band", gate: -1 },
    { name: "a gate band that is not a number", gate: Number.NaN },
  ])("refuses $name", ({ gate }) => {
    // Arrange / Act / Assert
    expect(() => allocateSectionLines(5 * ROW, sections(40, 40, 40, 40), gate)).toThrow(/band fit: a gate band of/);
  });

  it("fills Deploys through an expanded floor first in the room a gate band leaves the bands", () => {
    // Arrange — the Open section keeps the 7 lines it has in the room plus the gate.
    const s = sections(10, 10, 10, 0, 5);
    // Act
    const got = allocateSectionLines(10 * ROW, s, 3 * ROW);
    // Assert
    expect(got).toEqual({ open: 7, merged: 3, hidden: 0 });
  });

  it.each([
    { name: "no stops", s: { ...sections(3, 3, 3), hidden: [] } },
    { name: "stops not starting at zero", s: { ...sections(3, 3, 3), open: [5, 20] } },
    { name: "shrinking stops", s: { ...sections(3, 3, 3), merged: [0, 20, 10] } },
    { name: "more lines of today's merges than the band has", s: sections(3, 3, 3, 4) },
    { name: "a negative count of today's merges", s: sections(3, 3, 3, -1) },
    { name: "a fractional count of today's merges", s: sections(3, 3, 3, 1.5) },
    { name: "a floor longer than the band", s: sections(3, 3, 3, 0, 4) },
    { name: "a negative floor", s: sections(3, 3, 3, 0, -1) },
    { name: "a fractional floor", s: sections(3, 3, 3, 0, 1.5) },
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

  it("counts a merge queue's head card and each waiting merge as a line", () => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = '<div class="repo"><div class="repo-head"></div><div class="rows sb-merge-queue"><div class="sb-mq-head"></div><div class="sb-mq-waiting"></div></div></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".repo-head, .sb-mq-head, .sb-mq-waiting")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["sb-mq-head", "sb-mq-waiting"]);
  });

  it.each([
    { name: "counts the Deploy Queue heading over no queue as a line", empty: true, want: ["sb-deploy-queue-label", "row"] },
    { name: "counts the Deploy Queue heading over a queue as no line", empty: false, want: ["row"] },
  ])("$name", ({ empty, want }) => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = `<div class="sb-merge-queues"><div class="sb-deploy-queue-label"${empty ? ' data-empty=""' : ""}></div></div><div class="row"></div>`;
    for (const el of box.querySelectorAll<HTMLElement>(".sb-deploy-queue-label, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(want);
  });

  it.each([
    { name: "counts the collapsed Recently Deployed heading as a line", collapsed: true, want: ["sb-merged-label", "row"] },
    { name: "counts the expanded Recently Deployed heading as no line", collapsed: false, want: ["row"] },
  ])("$name", ({ collapsed, want }) => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = `<div class="sb-merged-label"${collapsed ? ' data-collapsed=""' : ""}></div><div class="row"></div>`;
    for (const el of box.querySelectorAll<HTMLElement>(".sb-merged-label, .row")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(want);
  });

  it("counts a queue group's header as no line", () => {
    // Arrange
    const box = document.createElement("div");
    box.innerHTML = '<div class="sb-merge-queues"><div class="mq-group"><div class="merged-group-head mq-group-head"></div><div class="rows sb-merge-queue"><div class="sb-mq-head"></div></div></div></div>';
    for (const el of box.querySelectorAll<HTMLElement>(".mq-group-head, .sb-mq-head")) el.getBoundingClientRect = () => rect(0, ROW);
    // Act / Assert
    expect(regionLines(box).map((el) => el.className)).toEqual(["sb-mq-head"]);
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

describe("recentlyDeployedFloorLines", () => {
  /** A queue line, then a merged line per merged row, as a Deploys region draws them. */
  function deploysLines(merged: number): HTMLElement[] {
    const queues = document.createElement("div");
    queues.className = MERGE_QUEUES_CLASS;
    const head = document.createElement("div");
    head.className = "sb-mq-head";
    queues.appendChild(head);
    const rows = Array.from({ length: merged }, () => {
      const ws = document.createElement("div");
      ws.setAttribute("data-merged-at", String(startOfToday() - 5));
      const line = document.createElement("div");
      line.className = "row";
      ws.appendChild(line);
      return line;
    });
    return [head, ...rows];
  }

  it.each([
    { name: "every line through the fifth merged row", merged: 8, want: 6 },
    { name: "every line through the last of fewer merged rows", merged: 3, want: 4 },
    { name: "no line while no merged row is drawn", merged: 0, want: 0 },
  ])("keeps $name", ({ merged, want }) => {
    // Arrange / Act
    const got = recentlyDeployedFloorLines(deploysLines(merged));
    // Assert
    expect(got).toBe(want);
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

  it.each([
    { name: "every queue line ahead of older merges", queued: 2, at: [startOfToday() - 5], want: 2 },
    { name: "every queue line and today's merges after them", queued: 1, at: [startOfToday() + 5, startOfToday() - 5], want: 2 },
  ])("reaches $name", ({ queued, at, want }) => {
    // Arrange: the queues' lines stand above the merged rows.
    const queues = document.createElement("div");
    queues.className = MERGE_QUEUES_CLASS;
    const queueLines = Array.from({ length: queued }, () => {
      const line = document.createElement("div");
      line.className = "sb-mq-waiting";
      queues.appendChild(line);
      return line;
    });
    // Act
    const got = mergedTodayLines([...queueLines, ...mergedLines(at)], NOW);
    // Assert
    expect(got).toBe(want);
  });

  it("reaches the Deploy Queue heading standing over no queue", () => {
    // Arrange: an empty queue box draws its heading alone, above older merges.
    const queues = document.createElement("div");
    queues.className = MERGE_QUEUES_CLASS;
    const heading = document.createElement("div");
    heading.className = "sb-deploy-queue-label";
    queues.appendChild(heading);
    // Act
    const got = mergedTodayLines([heading, ...mergedLines([startOfToday() - 5])], NOW);
    // Assert
    expect(got).toBe(1);
  });

  it("reaches the collapsed Recently Deployed heading below the queues", () => {
    // Arrange: a collapsed heading draws no merged row, and is a line of its own.
    const heading = document.createElement("div");
    heading.className = "sb-merged-label";
    heading.setAttribute("data-collapsed", "");
    // Act / Assert
    expect(mergedTodayLines([heading], NOW)).toBe(1);
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
  it("lengthens the Open section until the bands keep their six-line share", () => {
    // Arrange — room for twenty lines below 100px of other content; a
    // collapsed Recently Deployed is its heading's one line.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10, expanded: false });
    laidOut(rail, 500, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([...maxHeights(rail.pane), heights(rail.pane)[2]]).toEqual(["280px", "", "", "100px"]);
  });

  it("keeps an expanded Recently Deployed's five rows, Hidden Workspaces the share's last line", () => {
    // Arrange — room for twenty lines below 100px of other content.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 500, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([...maxHeights(rail.pane), heights(rail.pane)[2]]).toEqual(["280px", "100px", "", "20px"]);
  });

  it("takes an expanded Recently Deployed's five rows from the Open section once Hidden Workspaces has none", () => {
    // Arrange — room for twenty lines, nothing hidden.
    const rail = drawn({ open: 30, merged: 10 });
    laidOut(rail, 500, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 15, merged: 5, hidden: 0 });
  });

  it("stretches Hidden Workspaces to the rail's foot through a line it cuts", () => {
    // Arrange — 410px of room: twenty whole lines and half of one more.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "", "30px"]);
  });

  it("states the whole lines a stretched band shows", () => {
    // Arrange — half a line of stretch.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "hidden-section").getAttribute(BAND_LINES_ATTRIBUTE)).toBe("1");
  });

  it("stretches Hidden Workspaces to the rail's foot when every line fits", () => {
    // Arrange — 900px of room, 200px of lines.
    const rail = drawn({ open: 3, hidden: 3, merged: 4 });
    laidOut(rail, 1000, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([...maxHeights(rail.pane), ...heights(rail.pane)]).toEqual(["", "", "", "", "", "760px"]);
  });

  it("stretches Deploys when Hidden Workspaces is folded", () => {
    // Arrange
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { hidden: true } });
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
    // Arrange — fitted with Hidden Workspaces folded, Deploys stretched.
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { hidden: true } });
    laidOut(rail, 1000, 100);
    fitBands(rail.scroller, rail.pane, NOW);
    band(rail.pane, "hidden-section").classList.remove("folded");
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(heights(rail.pane)).toEqual(["", "", "760px"]);
  });

  it("measures a stretched band from its rows, not its stretch", () => {
    // Arrange — fitted once, stretched by half a line.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    const first = fitBands(rail.scroller, rail.pane, NOW);
    // Act
    const second = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect([second, heights(rail.pane)[2]]).toEqual([first, "30px"]);
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
    expect(band(rail.pane, "hidden-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(true);
  });

  it("marks a capped Deploys band with rows below its edge", () => {
    // Arrange — Deploys capped at its share, Hidden Workspaces lowest.
    const rail = drawn({ open: 30, hidden: 10, merged: 10, today: 10 });
    laidOut(rail, 510, 100);
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    const merged = band(rail.pane, "merged-section");
    expect([merged.hasAttribute(BAND_STRETCHED_ATTRIBUTE), merged.hasAttribute(BAND_MORE_ATTRIBUTE)]).toEqual([false, true]);
  });

  it("marks a capped Open band with rows below its edge", () => {
    // Arrange — the Open section capped above the bands, Hidden Workspaces lowest.
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
    expect(band(rail.pane, "hidden-section").hasAttribute(BAND_MORE_ATTRIBUTE)).toBe(false);
  });

  it("clears the foot mark from a band that is no longer lowest", () => {
    // Arrange — fitted with Hidden Workspaces folded, Deploys lowest.
    const rail = drawn({ open: 3, hidden: 3, merged: 4, folded: { hidden: true } });
    laidOut(rail, 1000, 100);
    fitBands(rail.scroller, rail.pane, NOW);
    band(rail.pane, "hidden-section").classList.remove("folded");
    // Act
    fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(band(rail.pane, "merged-section").hasAttribute(BAND_STRETCHED_ATTRIBUTE)).toBe(false);
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

  it("gives a collapsed Recently Deployed only its heading's line while the Open section scrolls", () => {
    // Arrange — room for twenty lines, no merge today.
    const rail = drawn({ open: 30, hidden: 2, merged: 10, expanded: false });
    laidOut(rail, 500, 100);
    // Act
    const got = fitBands(rail.scroller, rail.pane, NOW);
    // Assert
    expect(got).toEqual({ open: 17, hidden: 2, merged: 1 });
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
    expect(band(rail.pane, "hidden-section").querySelector("[data-section-count]")?.textContent).toBe("(10)");
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

  it("keeps the Open section's height when a docking gate band shrinks the rail, taking it from the bands", () => {
    // Arrange — fitted out of room: Open 4 lines, Deploys its collapsed
    // Recently Deployed heading's 1, Hidden Workspaces 5.
    const rail = drawn({ open: 10, hidden: 10, merged: 10, expanded: false });
    laidOut(rail, 300, 100);
    let gate = 0;
    const fit = installBandFit(rail.scroller, fakeTicker(), { gateBand: () => gate });
    fit.refit();
    const before = { open: maxHeights(rail.pane)[0], hidden: heights(rail.pane)[2] };
    // Act — a gate band three lines tall docks under the rail.
    gate = 3 * ROW;
    railHeight(rail.scroller, 300 - gate);
    fireResize(rail.scroller);
    // Assert — Hidden Workspaces, the lowest, stands as tall as its lines.
    expect([before, { open: maxHeights(rail.pane)[0], hidden: heights(rail.pane)[2] }]).toEqual([
      { open: `${4 * ROW}px`, hidden: `${5 * ROW}px` },
      { open: `${4 * ROW}px`, hidden: `${2 * ROW}px` },
    ]);
    fit.dispose();
  });

  it("keeps a stretched section's scroll across a re-fit that lets it shrink to its rows", async () => {
    // Arrange — Hidden Workspaces, the lowest, stretched to six of its ten
    // lines, scrolled three lines down. Its scroll clamps to whatever height it
    // lays out at, and keeps the clamp, as the engine does.
    const capture = captureLogRecords("debug");
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 300, 100);
    const fit = installBandFit(rail.scroller, fakeTicker());
    fit.refit();
    const rows = region(rail.pane, "hidden-section") as HTMLElement;
    let top = 0;
    const max = (): number => Math.max(0, rows.scrollHeight - rows.clientHeight);
    Object.defineProperty(rows, "scrollTop", {
      configurable: true,
      get: () => (top = Math.min(top, max())),
      set: (v: number) => {
        top = Math.max(0, Math.min(v, max()));
      },
    });
    rows.scrollTop = 3 * ROW;
    // Act
    fireResize(rail.scroller);
    // Assert
    expect(rows.scrollTop).toBe(3 * ROW);
    const record = await forwardedRecord(capture, "sidebar.band-fit.scroll-kept");
    expect(record.context).toMatchObject({ fitted_at: 0, kept: 3 * ROW, to: 3 * ROW });
    fit.dispose();
  });

  it("leaves a section's scroll alone when the fit did not move it", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 300, 100);
    const fit = installBandFit(rail.scroller, fakeTicker());
    // Act
    fit.refit();
    // Assert
    const records = await forwardedRecords(capture);
    expect(records.filter((r) => r.operation === "sidebar.band-fit.scroll-kept")).toEqual([]);
    fit.dispose();
  });

  it("tells ON_RESIZED after the fit a resize provoked, with the sections already fitted", () => {
    // Arrange — fitted out of room.
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 180, 100);
    const seen: string[][] = [];
    const fit = installBandFit(rail.scroller, fakeTicker(), { onResized: () => seen.push(maxHeights(rail.pane)) });
    fit.refit();
    // Act — the rail grows to hold everything.
    railHeight(rail.scroller, 1000);
    fireResize(rail.scroller);
    // Assert
    expect(seen).toEqual([["", "", ""]]);
    fit.dispose();
  });

  it("does not tell ON_RESIZED for a draw's own refit", () => {
    // Arrange
    const rail = drawn({ open: 10, hidden: 10, merged: 10 });
    laidOut(rail, 180, 100);
    let told = 0;
    const fit = installBandFit(rail.scroller, fakeTicker(), {
      onResized: () => {
        told += 1;
      },
    });
    // Act
    fit.refit();
    // Assert
    expect(told).toBe(0);
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
    // Arrange — six merges today hold the whole share past the five-row
    // floor; tomorrow only the floor holds.
    const ticker = fakeTicker();
    const rail = drawn({ open: 30, merged: 8, today: 6 });
    laidOut(rail, 500, 100);
    const fit = installBandFit(rail.scroller, ticker);
    fit.refit();
    // Act
    ticker.tick(startOfTomorrow() + 1);
    // Assert
    expect(band(rail.pane, "merged-section").getAttribute(BAND_LINES_ATTRIBUTE)).toBe("5");
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

describe("sectionRegions", () => {
  /** A section of class CLS holding a rows region of class REGION. */
  const section = (cls: string, region: string): HTMLElement => {
    const el = document.createElement("div");
    el.className = cls;
    const rows = document.createElement("div");
    rows.className = region;
    el.appendChild(rows);
    return el;
  };

  it("names each section's rows region by its section's class and its place among them", () => {
    // Arrange
    const root = document.createElement("div");
    root.append(section("open-section", "sb-sections"), section("merged-section", "rows"), section("hidden-section", "rows"), section("hidden-section", "rows"));
    // Act
    const names = sectionRegions(root).map((r) => r.name);
    // Assert
    expect(names).toEqual(["open-section#0", "merged-section#0", "hidden-section#0", "hidden-section#1"]);
  });

  it("finds exactly the regions REGION_SELECTOR matches, so the fit and the keeper see the same viewports", () => {
    // Arrange
    const root = document.createElement("div");
    root.append(section("open-section", "sb-sections"), section("merged-section", "rows"), section("open-section", "rows"));
    // Act
    const found = sectionRegions(root).map((r) => r.region);
    // Assert
    expect(found).toEqual([...root.querySelectorAll(REGION_SELECTOR)]);
  });
});
