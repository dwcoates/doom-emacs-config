// @vitest-environment jsdom
/**
 * THE SELECTED ROW IS SCROLLED INTO ITS SECTION'S VIEW (owner request,
 * 2026-10-20), once per selection. jsdom lays nothing out, so `band` states
 * the geometry the engine would give: a rows region VIEW lines tall at the
 * page's top, scrolling lines of ROW px each.
 */
import { afterEach, describe, expect, it } from "vitest";
import { resetLoggingForTests } from "../../src/log.js";
import { createSelectionScroll, scrollLineIntoBand } from "../../src/sidebar/selection-scroll.js";
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
}

/** A laid-out band as SPEC says: its root, its rows region, and each row's line. */
function band({ lines, view, current = [], cls = "open-section", regionClass = "sb-sections" }: BandSpec): {
  root: HTMLElement;
  region: HTMLElement;
  line: (id: string) => HTMLElement;
} {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.className = `repo ${cls}`;
  const region = document.createElement("div");
  region.className = regionClass;
  section.appendChild(region);
  root.appendChild(section);
  let top = 0;
  Object.defineProperty(region, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (v: number) => {
      top = Math.max(0, Math.min(v, lines * ROW - view * ROW));
    },
  });
  Object.defineProperty(region, "clientHeight", { configurable: true, value: view * ROW });
  Object.defineProperty(region, "clientTop", { configurable: true, value: 0 });
  region.getBoundingClientRect = () => rect(0, view * ROW);
  for (let i = 0; i < lines; i += 1) {
    const ws = document.createElement("div");
    ws.className = "ws";
    ws.setAttribute("data-roster-row", `ws-${i}`);
    if (current.includes(`ws-${i}`)) ws.setAttribute("data-current", "true");
    const line = document.createElement("div");
    line.className = "row";
    line.getBoundingClientRect = () => rect(i * ROW - region.scrollTop, ROW);
    ws.appendChild(line);
    region.appendChild(ws);
  }
  const line = (id: string): HTMLElement => {
    const found = root.querySelector<HTMLElement>(`[data-roster-row='${id}'] > .row`);
    if (found === null) throw new Error(`no line ${id}`);
    return found;
  };
  return { root, region, line };
}

/** Move ROOT's highlight to ID, as a draw or `markShown` does. */
function highlight(root: HTMLElement, id: string): void {
  for (const ws of root.querySelectorAll("[data-roster-row]")) {
    ws.toggleAttribute("data-current", ws.getAttribute("data-roster-row") === id);
  }
}

afterEach(() => {
  resetLoggingForTests();
});

describe("scrollLineIntoBand", () => {
  it("leaves a line that already shows whole where it is", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-2"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([null, 0]);
  });

  it("centers a line below the viewport", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    // Act
    const moved = scrollLineIntoBand(b.line("ws-6"));
    // Assert
    expect([moved, b.region.scrollTop]).toEqual([{ from: 0, to: 5 * ROW }, 5 * ROW]);
  });

  it("centers a line above the viewport", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    b.region.scrollTop = 7 * ROW;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: 7 * ROW, to: 2 * ROW });
  });

  it("scrolls a line the viewport's foot cuts through", () => {
    // Arrange
    const b = band({ lines: 10, view: 3 });
    b.region.scrollTop = ROW / 2;
    // Act
    const moved = scrollLineIntoBand(b.line("ws-3"));
    // Assert
    expect(moved).toEqual({ from: ROW / 2, to: 2 * ROW });
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
});

describe("createSelectionScroll", () => {
  it("scrolls a newly highlighted row into view", () => {
    // Arrange
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    // Act
    const outcome = createSelectionScroll().follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 7 * ROW]);
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
    highlight(b.root, "ws-1");
    // Act
    const outcome = follower.follow(b.root);
    // Assert
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 0]);
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
    expect([outcome, b.region.scrollTop]).toEqual(["scrolled", 7 * ROW]);
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
    expect([first, second, b.region.scrollTop]).toEqual(["unlaid", "scrolled", 7 * ROW]);
  });

  it("states the scroll at DEBUG with the workspace and where the band moved", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const b = band({ lines: 10, view: 3, current: ["ws-8"] });
    // Act
    createSelectionScroll().follow(b.root);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-scroll");
    expect(record.level.case).toBe("debug");
    expect(record.context).toMatchObject({ workspace: "ws-8", from: 0, to: 7 * ROW });
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
