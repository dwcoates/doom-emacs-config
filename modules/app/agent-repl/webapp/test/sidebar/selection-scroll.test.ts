// @vitest-environment jsdom
/**
 * THE SELECTED ROW IS SCROLLED INTO ITS SECTION'S VIEW (owner request,
 * 2026-10-20), once per selection, and only as far as the row needs (owner
 * request, 2026-10-26). jsdom lays nothing out, so `band` states the geometry
 * the engine would give: a rows region VIEW lines tall at the page's top,
 * scrolling lines of ROW px each, its scroll padding as the stylesheet
 * computes it.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { resetLoggingForTests } from "../../src/log.js";
import {
  contextSpan,
  createSelectionScroll,
  RAIL_BODY_CLASS,
  scrollLineIntoBand,
  SELECTION_CONTEXT_ROWS,
} from "../../src/sidebar/selection-scroll.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { boxRect } from "../dom-rect.js";

const ROW = 20;

function rect(top: number, height: number): DOMRect {
  return boxRect({ top, height });
}

/** A band of LINES rows (ids `ws-0`..), VIEW lines tall, CLS its section, CURRENT the highlighted ids. */
interface BandSpec {
  lines: number;
  view: number;
  current?: string[];
  cls?: string;
  regionClass?: string;
  /** The rows are chat threads: their list sits in the rail's body, the scroller. */
  thread?: boolean;
  /** The region's computed scroll padding, `auto` (none) unless stated. */
  padTop?: string;
  padBottom?: string;
}

/** A laid-out band as SPEC says: its root, its rows region, and each row's line. */
function band({
  lines,
  view,
  current = [],
  cls = "open-section",
  regionClass = "sb-sections",
  thread = false,
  padTop = "auto",
  padBottom = "auto",
}: BandSpec): {
  root: HTMLElement;
  region: HTMLElement;
  line: (id: string) => HTMLElement;
  /** Lay the viewport out VIEW lines tall, as a resize does. */
  resize: (view: number) => void;
} {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.className = `repo ${cls}`;
  const region = document.createElement("div");
  region.className = thread ? RAIL_BODY_CLASS : regionClass;
  section.appendChild(region);
  root.appendChild(section);
  const list = thread ? document.createElement("div") : region;
  if (thread) {
    list.setAttribute("data-thread-section", "");
    region.appendChild(list);
  }
  let top = 0;
  let shown = view;
  Object.defineProperty(region, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (v: number) => {
      top = Math.max(0, Math.min(v, lines * ROW - shown * ROW));
    },
  });
  Object.defineProperty(region, "clientHeight", { configurable: true, get: () => shown * ROW });
  Object.defineProperty(region, "clientTop", { configurable: true, value: 0 });
  region.getBoundingClientRect = () => rect(0, shown * ROW);
  const computed = window.getComputedStyle.bind(window);
  vi.spyOn(window, "getComputedStyle").mockImplementation((el, pseudo) =>
    el === region ? ({ scrollPaddingTop: padTop, scrollPaddingBottom: padBottom } as CSSStyleDeclaration) : computed(el, pseudo),
  );
  for (let i = 0; i < lines; i += 1) {
    const ws = document.createElement("div");
    ws.className = "ws";
    ws.setAttribute("data-roster-row", `ws-${i}`);
    if (current.includes(`ws-${i}`)) ws.setAttribute("data-current", "true");
    const line = document.createElement("div");
    line.className = "row";
    line.getBoundingClientRect = () => rect(i * ROW - region.scrollTop, ROW);
    ws.appendChild(line);
    list.appendChild(ws);
  }
  const line = (id: string): HTMLElement => {
    const found = root.querySelector<HTMLElement>(`[data-roster-row='${id}'] > .row`);
    if (found === null) throw new Error(`no line ${id}`);
    return found;
  };
  const resize = (next: number): void => {
    shown = next;
  };
  return { root, region, line, resize };
}

/** Move ROOT's highlight to ID, as a draw or `markShown` does. */
function highlight(root: HTMLElement, id: string): void {
  for (const ws of root.querySelectorAll("[data-roster-row]")) {
    ws.toggleAttribute("data-current", ws.getAttribute("data-roster-row") === id);
  }
}

afterEach(() => {
  vi.restoreAllMocks();
  resetLoggingForTests();
});

describe("scrollLineIntoBand", () => {
  it("brings a chat thread's line below the view to its bottom by scrolling the rail's body, as no band holds it", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, thread: true });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([{ from: 0, to: 4 * ROW }, 4 * ROW]);
  });

  it("leaves a chat thread's line that already shows whole where it is", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, thread: true });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-1"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([null, 0]);
  });

  it("leaves a line that already shows whole where it is", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-2"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([null, 0]);
  });

  it("brings a line below the viewport only to its bottom edge", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([{ from: 0, to: 4 * ROW }, 4 * ROW]);
  });

  it("brings a line above the viewport, with the rows above it that fit, only to its top edge", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    b.region.scrollTop = 7 * ROW;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: 7 * ROW, to: ROW });
  });

  it("scrolls a line the viewport's foot cuts through only until its bottom shows", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    b.region.scrollTop = ROW / 2;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: ROW / 2, to: ROW });
  });

  it("scrolls a line the viewport's top cuts through until the rows above it that fit show", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    b.region.scrollTop = 3 * ROW + ROW / 2;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: 3 * ROW + ROW / 2, to: ROW });
  });

  it("scrolls a line under the faded foot's scroll padding until it clears it", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, padBottom: "10px" });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-2"));
    // Assert
    expect(moved).toEqual({ from: 0, to: 10 });
  });

  it("brings a line below the viewport to the edge of its bottom scroll padding", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, padBottom: "10px" });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect(moved).toEqual({ from: 0, to: 4 * ROW + 10 });
  });

  it("brings a line above the viewport to the edge of its top scroll padding", () => {
    // Arrange: 50px of view hold the line and one row above it.
    const b = band({ lines: 10, view: 3, padTop: "10px" });
    b.region.scrollTop = 7 * ROW;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: 7 * ROW, to: 2 * ROW - 10 });
  });

  it("refuses a viewport whose scroll padding is not in px, logging it at ERROR and moving nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const b = band({ lines: 10, view: 3, padBottom: "1.4rem" });
    // Act
    const act = (): unknown => scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect(act).toThrow(/computed scrollPaddingBottom '1.4rem' is not in px/);
    expect(b.region.scrollTop).toBe(0);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.unmeasurable");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ edge: "scrollPaddingBottom", value: "1.4rem", scroller: "sb-sections" });
  });

  it("scrolls the Hidden Workspaces band's rows region too", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, cls: "hidden-section", regionClass: "rows" });
    // Act
    scrollLineIntoBand(b.line("ws-9"));
    // Assert
    expect(b.region.scrollTop).toBe(7 * ROW);
  });

  it("refuses a row in no section's rows region, logging it at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const ws = document.createElement("div");
    ws.setAttribute("data-roster-row", "stray");
    const line = document.createElement("div");
    line.className = "row";
    ws.appendChild(line);
    // Act
    const act = (): unknown => scrollLineIntoBand(line);
    // Assert
    expect(act).toThrow(/hangs in no section's rows region/);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.orphan-row");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspace: "stray" });
  });

  it("refuses a chat thread's row outside the rail's body, logging it at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const list = document.createElement("div");
    list.setAttribute("data-thread-section", "");
    const ws = document.createElement("div");
    ws.setAttribute("data-roster-row", "stray-thread");
    const line = document.createElement("div");
    line.className = "row";
    ws.appendChild(line);
    list.appendChild(ws);
    // Act
    const act = (): unknown => scrollLineIntoBand(line);
    // Assert
    expect(act).toThrow(/hangs in no rail body/);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.orphan-row");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspace: "stray-thread", thread: true });
  });
});

describe("scrollLineIntoBand keeps the selected row's neighbors in view", () => {
  it("brings a line below the viewport up until the five rows below it show", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-20"));
    // Assert: ws-25's bottom sits on the view's bottom.
    expect(moved).toEqual({ from: 0, to: 11 * ROW });
  });

  it("brings a line above the viewport down until the five rows above it show", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    b.region.scrollTop = 15 * ROW;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-10"));
    // Assert: ws-5's top sits on the view's top.
    expect(moved).toEqual({ from: 15 * ROW, to: 5 * ROW });
  });

  it("scrolls a line already in view that sits within five rows of the view's foot", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-12"));
    // Assert
    expect(moved).toEqual({ from: 0, to: 3 * ROW });
  });

  it("leaves a line whose five rows each side already show where it is", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-7"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([null, 0]);
  });

  it("shows the rows a line near the section's top has above it, however few", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    b.region.scrollTop = 10 * ROW;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-2"));
    // Assert
    expect(moved).toEqual({ from: 10 * ROW, to: 0 });
  });

  it("shows the rows a line near the section's foot has below it, however few", () => {
    // Arrange
    const b = band({ lines: 30, view: 15 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-28"));
    // Assert
    expect(moved).toEqual({ from: 0, to: 15 * ROW });
  });

  it("shows the five rows above first when the viewport cannot hold five each side", () => {
    // Arrange
    const b = band({ lines: 30, view: 8 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-20"));
    // Assert: ws-15 at the top, ws-22 at the foot.
    expect(moved).toEqual({ from: 0, to: 15 * ROW });
  });

  it("counts only laid-out rows as neighbors, so a folded row is skipped", () => {
    // Arrange: ws-15..ws-19 draw no box, as a folded subsection's rows do.
    const b = band({ lines: 30, view: 15 });
    for (let i = 15; i < 20; i += 1) b.line(`ws-${i}`).getBoundingClientRect = () => rect(0, 0);
    // Act
    const moved = scrollLineIntoBand(b.line("ws-20"));
    // Assert: ws-10 is the fifth laid-out row above, ws-24 the farthest below that still fits.
    expect(moved).toEqual({ from: 0, to: 10 * ROW });
  });

  it("refuses a line that is not among its viewport's laid-out lines, logging it at ERROR and moving nothing", async () => {
    // Arrange
    const capture = captureLogRecords();
    const b = band({ lines: 10, view: 3 });
    b.line("ws-6").getBoundingClientRect = () => rect(0, 0);
    // Act
    const act = (): unknown => scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect(act).toThrow(/not among its viewport's laid-out lines/);
    expect(b.region.scrollTop).toBe(0);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.unlisted");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspace: "ws-6", lines: 9 });
  });
});

describe("contextSpan", () => {
  /** N lines of ROW px from 0. */
  const lines = (n: number): Array<{ top: number; bottom: number }> =>
    Array.from({ length: n }, (_, i) => ({ top: i * ROW, bottom: (i + 1) * ROW }));

  it("keeps five rows each side of the selected row", () => {
    // Arrange + Act + Assert
    expect([SELECTION_CONTEXT_ROWS, contextSpan(lines(30), 15, 11 * ROW)]).toEqual([5, { top: 10 * ROW, bottom: 21 * ROW }]);
  });

  it("gives up rows below before any row above", () => {
    // Arrange + Act + Assert
    expect(contextSpan(lines(30), 15, 7 * ROW)).toEqual({ top: 10 * ROW, bottom: 17 * ROW });
  });

  it("gives up the farthest rows above once no row below fits", () => {
    // Arrange + Act + Assert
    expect(contextSpan(lines(30), 15, 3 * ROW)).toEqual({ top: 13 * ROW, bottom: 16 * ROW });
  });

  it("answers the line alone when it is taller than the room", () => {
    // Arrange + Act + Assert
    expect(contextSpan(lines(30), 15, ROW / 2)).toEqual({ top: 15 * ROW, bottom: 16 * ROW });
  });

  it("stops at the first and last lines there are", () => {
    // Arrange + Act + Assert
    expect(contextSpan(lines(4), 1, 30 * ROW)).toEqual({ top: 0, bottom: 4 * ROW });
  });

  it("refuses an index naming no line", () => {
    // Arrange + Act
    const act = (): unknown => contextSpan(lines(3), 3, 10 * ROW);
    // Assert
    expect(act).toThrow(/line 3 of 3 does not exist/);
  });
});

describe("createSelectionScroll", () => {
  it("scrolls a newly highlighted row into view", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    // Act
    const outcome = createSelectionScroll().follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 6 * ROW]);
  });

  it("answers visible for a newly highlighted row already in view", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-1"] });
    // Act
    const outcome = createSelectionScroll().follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["visible", 0]);
  });

  it("does not scroll back to a row it already followed once the reader scrolled away", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.region.scrollTop = 0;
    // Act
    const outcome = follower.follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["followed", 0]);
  });

  it("follows the next selection", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    highlight(b.root, "ws-3");
    // Act
    const outcome = follower.follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", ROW]);
  });

  it("follows a switch back to a row it followed before", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    highlight(b.root, "ws-1");
    follower.follow(b.root);
    highlight(b.root, "ws-8");
    // Act
    const outcome = follower.follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 6 * ROW]);
  });

  it("answers none when no row is highlighted", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    // Act
    const outcome = createSelectionScroll().follow(b.root);
    // Assert
    expect(outcome).toBe("none");
  });

  it("leaves a highlighted row that draws no box for the draw that lays it out", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    let laidOut = false;
    b.line("ws-8").getBoundingClientRect = () => (laidOut ? rect(8 * ROW - b.region.scrollTop, ROW) : rect(0, 0));
    const follower = createSelectionScroll();
    const first = follower.follow(b.root);
    laidOut = true;
    // Act
    const second = follower.follow(b.root);
    // Assert
    expect([first, second, b.region.scrollTop]).toEqual(["unlaid", "scrolled", 6 * ROW]);
  });

  it("states the scroll at INFO with the workspace and where the band moved", async () => {
    // Arrange
    const capture = captureLogRecords();
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    // Act
    createSelectionScroll().follow(b.root);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-scroll");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ workspace: "ws-8", from: 0, to: 6 * ROW });
  });

  it("states a row already showing with its neighbors at DEBUG, as nothing moved", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const b = band({ lines: 10, view: 3, current: ["ws-1"] });
    // Act
    createSelectionScroll().follow(b.root);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-scroll");
    expect(record.level.case).toBe("debug");
    expect([record.context?.workspace, record.context?.from]).toEqual(["ws-1", undefined]);
  });

  it("refuses a rail with two highlighted rows, logging it at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const b = band({ lines: 10, view: 3, current: ["ws-1", "ws-2"] });
    // Act
    const act = (): unknown => createSelectionScroll().follow(b.root);
    // Assert
    expect(act).toThrow(/2 rows are highlighted/);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.ambiguous");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspaces: ["ws-1", "ws-2"] });
  });

  it("refuses a highlighted row drawn without its line, logging it at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const b = band({ lines: 3, view: 3, current: ["ws-1"] });
    b.line("ws-1").remove();
    // Act
    const act = (): unknown => createSelectionScroll().follow(b.root);
    // Assert
    expect(act).toThrow(/row ws-1 has no line/);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll.malformed");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspace: "ws-1" });
  });
});

describe("createSelectionScroll keeps the followed row in view across a resize", () => {
  it("brings a row that showed back with its neighbors when its section shrinks under it", async () => {
    // Arrange: ws-12 shows with five rows each side in a 20-line view.
    const capture = captureLogRecords();
    const b = band({ lines: 30, view: 20, current: ["ws-12"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.resize(10);
    // Act
    const outcome = follower.resized(b.root);
    // Assert: ws-7 at the top, ws-16 at the foot.
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 7 * ROW]);
    const record = await forwardedRecord(capture, "sidebar.selection-scroll");
    expect(record.context).toMatchObject({ workspace: "ws-12", trigger: "resize", from: 0, to: 7 * ROW });
  });

  it("answers visible for a row whose neighbors still show after the resize", () => {
    // Arrange
    const b = band({ lines: 30, view: 15, current: ["ws-6"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.resize(20);
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["visible", 0]);
  });

  it("moves nothing when the section kept its height", () => {
    // Arrange
    const b = band({ lines: 30, view: 15, current: ["ws-6"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.region.scrollTop = 4 * ROW;
    follower.scrolled(b.root);
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["unchanged", 4 * ROW]);
  });

  it("leaves a row the reader scrolled out of view where it is", () => {
    // Arrange
    const b = band({ lines: 30, view: 10, current: ["ws-2"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.region.scrollTop = 15 * ROW;
    follower.scrolled(b.root);
    b.resize(8);
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["away", 15 * ROW]);
  });

  it("brings back a row the reader scrolled into view again before the resize", () => {
    // Arrange
    const b = band({ lines: 30, view: 10, current: ["ws-2"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.region.scrollTop = 15 * ROW;
    follower.scrolled(b.root);
    b.region.scrollTop = 0;
    follower.scrolled(b.root);
    b.resize(3);
    // Act
    const outcome = follower.resized(b.root);
    // Assert: a 3-line view holds ws-2 with the rows above that fit.
    expect([outcome, b.region.scrollTop]).toEqual(["visible", 0]);
  });

  it("answers none before any row was followed", () => {
    // Arrange
    const b = band({ lines: 30, view: 10, current: ["ws-2"] });
    // Act
    const outcome = createSelectionScroll().resized(b.root);
    // Assert
    expect(outcome).toBe("none");
  });

  it("answers none once the highlight moved to a row not yet followed", () => {
    // Arrange
    const b = band({ lines: 30, view: 10, current: ["ws-2"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    highlight(b.root, "ws-20");
    b.resize(5);
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["none", 0]);
  });

  it("answers unlaid for a followed row that draws no box after the resize", () => {
    // Arrange
    const b = band({ lines: 30, view: 10, current: ["ws-2"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.line("ws-2").getBoundingClientRect = () => rect(0, 0);
    b.resize(0);
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect(outcome).toBe("unlaid");
  });

  it("acts once per change in height, so a second report of the same height moves nothing", () => {
    // Arrange
    const b = band({ lines: 30, view: 20, current: ["ws-12"] });
    const follower = createSelectionScroll();
    follower.follow(b.root);
    b.resize(10);
    follower.resized(b.root);
    b.region.scrollTop = 0;
    // Act
    const outcome = follower.resized(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["unchanged", 0]);
  });
});
