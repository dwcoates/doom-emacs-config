// @vitest-environment jsdom
// A row's "⋯" verb menu, opened beside its row as its hover panel is. See
// src/sidebar/row-menu.ts.
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import { createDropdowns, type Dropdowns } from "../../src/sidebar/dropdowns.js";
import { createRowMenus, rowMenuKey, type RowMenus } from "../../src/sidebar/row-menu.js";
import type { PopupMirror } from "../../src/host/sidebar-popups.js";
import { captureLogRecords } from "../log-capture.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src");

/** A drawn row: its line at TOP with the "⋯" control in it, and its shut menu. */
function drawnRow(top = 40) {
  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", "ws-1");
  const line = document.createElement("div");
  line.className = "row";
  line.getBoundingClientRect = () => DOMRect.fromRect({ x: 0, y: top, width: 220, height: 22 });
  const more = document.createElement("button");
  more.className = "sb-more";
  line.append(more);
  const menu = document.createElement("div");
  menu.className = "sb-menu";
  menu.hidden = true;
  menu.getBoundingClientRect = () => DOMRect.fromRect({ x: 0, y: 0, width: 160, height: 120 });
  ws.append(line, menu);
  document.body.append(ws);
  return { ws, line, more, menu };
}

/** A mirror that records what it was told, as `kind:key`. */
function recordingMirror() {
  const told: string[] = [];
  const shown: Array<[HTMLElement, HTMLElement]> = [];
  const mirror: PopupMirror = {
    show: (key, detail, line) => {
      told.push(`show:${key}`);
      shown.push([detail, line]);
    },
    place: (key) => void told.push(`place:${key}`),
    hide: (key) => void told.push(`hide:${key}`),
    onDismissed: () => () => undefined,
    pointerOnCopy: () => false,
    dispose: () => undefined,
  };
  return { mirror, told, shown };
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("rowMenuKey", () => {
  it("keys a row's menu apart from its panel, which the workspace's id keys", () => {
    expect(rowMenuKey("ws-1")).toBe("row-menu:ws-1");
  });
});

describe("a row's verb menu, in a page drawing its own sidebar", () => {
  let dropdowns: Dropdowns;
  let menus: RowMenus;

  function setUp() {
    dropdowns = createDropdowns();
    menus = createRowMenus(dropdowns);
    return drawnRow();
  }

  it("opens on its toggle", () => {
    const { ws, menu } = setUp();
    menus.toggle("ws-1", ws);
    expect(menu.hidden).toBe(false);
  });

  it("stands where the row's panel stands: on the line's right edge, its top on the line's", () => {
    const { ws, menu } = setUp();
    menus.toggle("ws-1", ws);
    expect([menu.style.left, menu.style.top]).toEqual(["220px", "40px"]);
  });

  it("shuts on a second toggle rather than stacking a second menu", () => {
    const { ws, menu } = setUp();
    menus.toggle("ws-1", ws);
    menus.toggle("ws-1", ws);
    expect(menu.hidden).toBe(true);
  });

  it("is one of the rail's dropdowns: another one opening shuts it", () => {
    // ARRANGE
    const { ws, menu } = setUp();
    menus.toggle("ws-1", ws);
    // ACT
    dropdowns.opened({ kind: "other", element: document.createElement("div"), openers: [], close: () => undefined });
    // ASSERT
    expect(menu.hidden).toBe(true);
  });

  it("is shut by a click outside it, and kept by a click on its own control", () => {
    // ARRANGE
    const { ws, menu, more } = setUp();
    menus.toggle("ws-1", ws);
    // ACT
    dropdowns.dismissOutside(more);
    const keptByControl = !menu.hidden;
    dropdowns.dismissOutside(document.body);
    // ASSERT
    expect([keptByControl, menu.hidden]).toEqual([true, true]);
  });

  it("shuts the menu open before when another row's opens", () => {
    // ARRANGE
    const first = setUp();
    const second = drawnRow(80);
    menus.toggle("ws-1", first.ws);
    // ACT
    menus.toggle("ws-2", second.ws);
    // ASSERT
    expect([first.menu.hidden, second.menu.hidden]).toEqual([true, false]);
  });

  it("stands beside its line again when the line moved", () => {
    // ARRANGE
    const { ws, line, menu } = setUp();
    menus.toggle("ws-1", ws);
    line.getBoundingClientRect = () => DOMRect.fromRect({ x: 0, y: 90, width: 220, height: 22 });
    // ACT
    menus.placeOpen();
    // ASSERT
    expect(menu.style.top).toBe("90px");
  });

  it("opens nothing for a row drawing no menu of its own", () => {
    // ARRANGE
    setUp();
    const nested = document.createElement("div");
    const child = drawnRow();
    nested.append(child.ws);
    // ACT
    menus.toggle("ws-1", nested);
    // ASSERT
    expect(child.menu.hidden).toBe(true);
  });

  it("refuses a row whose menu has no line to stand beside, and records it at error", () => {
    // ARRANGE
    setUp();
    const capture = captureLogRecords();
    const ws = document.createElement("div");
    const menu = document.createElement("div");
    menu.className = "sb-menu";
    ws.append(menu);
    // ACT
    const act = (): void => menus.toggle("ws-1", ws);
    // ASSERT
    expect(act).toThrow(/no row line/);
    capture.logger.flush();
    const record = capture.sent.find((r) => r.operation === "sidebar.row-menu.no-line");
    expect(record?.level.case).toBe("error");
  });
});

describe("a row's verb menu, in the sidebar page", () => {
  function setUp() {
    const dropdowns = createDropdowns();
    const recorded = recordingMirror();
    const menus = createRowMenus(dropdowns, recorded.mirror);
    return { dropdowns, menus, ...recorded, ...drawnRow() };
  }

  it("is handed to the host to draw over the feed, beside its row's line", () => {
    const { menus, ws, menu, line, told, shown } = setUp();
    menus.toggle("ws-1", ws);
    expect([told, shown]).toEqual([["show:row-menu:ws-1"], [[menu, line]]]);
  });

  it("leaves its own placement to the host", () => {
    const { menus, ws, menu } = setUp();
    menus.toggle("ws-1", ws);
    expect(menu.style.left).toBe("");
  });

  it("is taken off the feed when its toggle shuts it", () => {
    const { menus, ws, told } = setUp();
    menus.toggle("ws-1", ws);
    menus.toggle("ws-1", ws);
    expect(told.at(-1)).toBe("hide:row-menu:ws-1");
  });

  it("is taken off the feed when the rail's dropdowns shut it", () => {
    const { menus, dropdowns, ws, told } = setUp();
    menus.toggle("ws-1", ws);
    dropdowns.dismissAll("the page lost focus");
    expect(told.at(-1)).toBe("hide:row-menu:ws-1");
  });

  it("is handed over again when its line moved", () => {
    const { menus, ws, told } = setUp();
    menus.toggle("ws-1", ws);
    menus.placeOpen();
    expect(told).toEqual(["show:row-menu:ws-1", "show:row-menu:ws-1"]);
  });

  it("is taken off the feed when a draw threw its row away", () => {
    // ARRANGE
    const { menus, dropdowns, ws, told } = setUp();
    menus.toggle("ws-1", ws);
    dropdowns.forgetWithin(ws);
    ws.remove();
    // ACT
    menus.placeOpen();
    // ASSERT
    expect(told.at(-1)).toBe("hide:row-menu:ws-1");
  });

  it("hands nothing over with no menu open", () => {
    const { menus, told } = setUp();
    menus.placeOpen();
    expect(told).toEqual([]);
  });

  it("records its opening, naming the row and whether the host draws it", () => {
    // ARRANGE
    const capture = captureLogRecords("debug");
    const { menus, ws } = setUp();
    // ACT
    menus.toggle("ws-1", ws);
    capture.logger.flush();
    // ASSERT
    const record = capture.sent.find((r) => r.operation === "sidebar.row-menu.open");
    expect(record?.level.case).toBe("debug");
  });
});

describe("the menu's place is the row's panel's", () => {
  it("is placed through the one placeBeside the panel is placed through", () => {
    const source = readFileSync(path.join(SRC, "sidebar", "row-menu.ts"), "utf8");
    expect(/placeBeside\(/.test(source)).toBe(true);
  });

  it("wears the panel's card: the stylesheet draws both from one rule", () => {
    const css = readFileSync(path.join(SRC, "styles.css"), "utf8");
    const card = /#ws-sidebar \.ws\.open > \.detail,\s*#ws-sidebar \.detail\.sb-memory\.open,\s*#ws-sidebar \.ws > \.sb-menu \{[^}]*position: fixed;[^}]*background: var\(--card\);/;
    expect(card.test(css)).toBe(true);
  });
});
