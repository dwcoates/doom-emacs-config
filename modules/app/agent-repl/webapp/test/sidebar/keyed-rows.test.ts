// @vitest-environment jsdom
// A repository section's rows drawn each under its own scope. See src/sidebar/keyed-rows.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterOpenRowsSchema, RosterOpenRowTabSchema, type RosterRow } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  disposeKeyedRows,
  drawKeyedRows,
  patchKeyedRegions,
  patchKeyedRows,
  type KeyedRows,
} from "../../src/sidebar/keyed-rows.js";
import { appContext, fakeTicker, NOW, openRow, row, sidebarContext } from "./harness.js";

const rows = (...r: RosterRow[]) => create(RosterOpenRowsSchema, { rows: r.map(openRow) });
const ticking = (id: string, current = false): RosterRow =>
  row({ id, current, when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } });
const elementOf = (box: HTMLElement, id: string): Element | null => box.querySelector(`[data-roster-row='${id}']`);

describe("keyed rows", () => {
  it("redraws only the rows whose wire changed, where they stand", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a", current: true }), row({ id: "b" }), row({ id: "c" })), rail, "r", "grouping", kept);
    const [a, b, c] = ["a", "b", "c"].map((id) => elementOf(box, id));

    // Act
    const patched = patchKeyedRows(kept, rows(row({ id: "a" }), row({ id: "b", current: true }), row({ id: "c" })), rail, "r", "grouping");

    // Assert
    expect(patched).toBe(true);
    expect([elementOf(box, "a") === a, elementOf(box, "b") === b, elementOf(box, "c") === c]).toEqual([false, false, true]);
    expect([...box.querySelectorAll("[data-current]")].map((el) => el.getAttribute("data-roster-row"))).toEqual(["b"]);
    expect([...box.children].map((el) => el.getAttribute("data-roster-row"))).toEqual(["a", "b", "c"]);
  });

  it("redraws no row when only the tabs were renumbered, a child's included", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const numbered = (n: number) => ({ tab: { case: "numbered" as const, value: { number: n } } });
    const first = rows(row({ id: "a", children: [row({ id: "a1" })] }), row({ id: "b" }));
    first.rows[0].tab = create(RosterOpenRowTabSchema, numbered(1));
    first.rows[0].children[0].tab = create(RosterOpenRowTabSchema, numbered(2));
    first.rows[1].tab = create(RosterOpenRowTabSchema, numbered(3));
    const box = drawKeyedRows(first, rail, "r", "grouping", kept);
    const before = ["a", "a1", "b"].map((id) => elementOf(box, id));
    const next = rows(row({ id: "a", children: [row({ id: "a1" })] }), row({ id: "b" }));
    next.rows[0].tab = create(RosterOpenRowTabSchema, numbered(4));
    next.rows[0].children[0].tab = create(RosterOpenRowTabSchema, numbered(5));

    // Act
    const patched = patchKeyedRows(kept, next, rail, "r", "grouping");

    // Assert
    expect([patched, ["a", "a1", "b"].map((id) => elementOf(box, id))]).toEqual([true, before]);
  });

  it.each([
    ["a row more", [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })]],
    ["the rows reordered", [row({ id: "b" }), row({ id: "a" })]],
    ["another workspace in a row's place", [row({ id: "a" }), row({ id: "z" })]],
  ])("refuses %s, changing nothing", (_name, next) => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping", kept);
    const before = [...box.children];

    // Act
    const patched = patchKeyedRows(kept, rows(...next), rail, "r", "grouping");

    // Assert
    expect([patched, [...box.children]]).toEqual([false, before]);
  });

  it("takes down a redrawn row's clocks with its old drawing", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const kept: KeyedRows = { entries: [] };
    drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", "grouping", kept);
    const drawnOnce = ticker.subscribers();

    // Act
    patchKeyedRows(kept, rows(ticking("a", true), ticking("b")), rail, "r", "grouping");

    // Assert
    expect(ticker.subscribers()).toBe(drawnOnce);
  });

  it("takes down every row's clocks when the rows are disposed", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const before = ticker.subscribers();
    const kept: KeyedRows = { entries: [] };
    drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", "grouping", kept);

    // Act
    disposeKeyedRows(kept);

    // Assert
    expect([ticker.subscribers(), kept.entries.length]).toEqual([before, 0]);
  });
});

describe("keyed regions", () => {
  it("patches every region of a section when each lists the same workspaces", () => {
    // Arrange: the section's own rows, and one subsection's.
    const rail = sidebarContext();
    const own: KeyedRows = { entries: [] };
    const sub: KeyedRows = { entries: [] };
    const ownBox = drawKeyedRows(rows(row({ id: "a", current: true })), rail, "own", "grouping", own);
    const subBox = drawKeyedRows(rows(row({ id: "b" })), rail, "sub", "grouping", sub);

    // Act
    const patched = patchKeyedRegions(
      [
        { kept: own, rows: rows(row({ id: "a" })), path: "own" },
        { kept: sub, rows: rows(row({ id: "b", current: true })), path: "sub" },
      ],
      rail,
      "grouping",
    );

    // Assert
    expect(patched).toBe(true);
    expect([ownBox.querySelector("[data-current]"), subBox.querySelector("[data-current]")?.getAttribute("data-roster-row")]).toEqual([
      null,
      "b",
    ]);
  });

  it("changes no region when one region's workspaces moved", () => {
    // Arrange: the row in the section's own rows changes, and a workspace
    // moved out of the subsection.
    const rail = sidebarContext();
    const own: KeyedRows = { entries: [] };
    const sub: KeyedRows = { entries: [] };
    const ownBox = drawKeyedRows(rows(row({ id: "a", current: true })), rail, "own", "grouping", own);
    drawKeyedRows(rows(row({ id: "b" })), rail, "sub", "grouping", sub);
    const before = [...ownBox.children];

    // Act
    const patched = patchKeyedRegions(
      [
        { kept: own, rows: rows(row({ id: "a" })), path: "own" },
        { kept: sub, rows: rows(), path: "sub" },
      ],
      rail,
      "grouping",
    );

    // Assert
    expect([patched, [...ownBox.children], ownBox.querySelector("[data-current]") !== null]).toEqual([false, before, true]);
  });
});
