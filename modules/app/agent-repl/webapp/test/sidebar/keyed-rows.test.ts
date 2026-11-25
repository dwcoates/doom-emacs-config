// @vitest-environment jsdom
// A repository section's rows drawn each under its own scope. See src/sidebar/keyed-rows.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  RosterBacklogRowsSchema,
  RosterOpenRowsSchema,
  RosterOpenRowTabSchema,
  type RosterBacklogRow,
  type RosterRow,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  disposeKeyedRows,
  drawKeyedBacklogRows,
  drawKeyedRows,
  BACKLOG_ROW,
  OPEN_ROW,
  pathedBacklogRows,
  patchKeyedRows,
  placeKeyedRows,
  reconcileKeyedRegions,
  reconciledAnything,
  type KeyedRows,
} from "../../src/sidebar/keyed-rows.js";
import { appContext, fakeTicker, NOW, openRow, row, sidebarContext } from "./harness.js";
import { pathedOpenRows } from "../../src/sidebar/entries.js";

const rows = (...r: RosterRow[]) => create(RosterOpenRowsSchema, { rows: r.map(openRow) });
const ticking = (id: string, current = false): RosterRow =>
  row({ id, current, when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } });
const elementOf = (box: HTMLElement, id: string): Element | null => box.querySelector(`[data-roster-row='${id}']`);
const idsOf = (box: HTMLElement): Array<string | null> => [...box.children].map((el) => el.getAttribute("data-roster-row"));
const backlogRows = (...r: RosterRow[]) => create(RosterBacklogRowsSchema, { rows: r.map((one) => ({ row: one })) });

describe("keyed rows", () => {
  it("redraws only the rows whose wire changed, where they stand", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a", current: true }), row({ id: "b" }), row({ id: "c" })), rail, "r", "grouping", kept);
    const [a, b, c] = ["a", "b", "c"].map((id) => elementOf(box, id));

    // Act
    const patched = patchKeyedRows(box, kept, rows(row({ id: "a" }), row({ id: "b", current: true }), row({ id: "c" })), rail, "r", "grouping");

    // Assert
    expect(patched).toEqual({ drawn: 2, removed: 0, moved: 0 });
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
    const patched = patchKeyedRows(box, kept, next, rail, "r", "grouping");

    // Assert
    expect([reconciledAnything(patched), ["a", "a1", "b"].map((id) => elementOf(box, id))]).toEqual([false, before]);
  });

  it("moves the rows reordered, drawing none", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" }), row({ id: "c" })), rail, "r", "grouping", kept);
    const [a, b, c] = ["a", "b", "c"].map((id) => elementOf(box, id));

    // Act
    const patched = patchKeyedRows(box, kept, rows(row({ id: "c" }), row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping");

    // Assert
    expect([patched, idsOf(box), [elementOf(box, "a") === a, elementOf(box, "b") === b, elementOf(box, "c") === c]]).toEqual([
      { drawn: 0, removed: 0, moved: 3 },
      ["c", "a", "b"],
      [true, true, true],
    ]);
  });

  it("draws a row that arrived alone, in its place", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping", kept);
    const [a, b] = ["a", "b"].map((id) => elementOf(box, id));

    // Act
    const patched = patchKeyedRows(box, kept, rows(row({ id: "a" }), row({ id: "z" }), row({ id: "b" })), rail, "r", "grouping");

    // Assert
    expect([patched, idsOf(box), elementOf(box, "a") === a, elementOf(box, "b") === b]).toEqual([
      { drawn: 1, removed: 0, moved: 1 },
      ["a", "z", "b"],
      true,
      true,
    ]);
  });

  it("takes down a row that left, and its clocks, keeping the rest", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", "grouping", kept);
    const a = elementOf(box, "a");
    const both = ticker.subscribers();

    // Act
    const patched = patchKeyedRows(box, kept, rows(ticking("a")), rail, "r", "grouping");

    // Assert
    expect([patched, idsOf(box), elementOf(box, "a") === a, ticker.subscribers() < both]).toEqual([
      { drawn: 0, removed: 1, moved: 0 },
      ["a"],
      true,
      true,
    ]);
  });

  it("changes nothing when nothing changed", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping", kept);
    const before = [...box.children];

    // Act
    const patched = patchKeyedRows(box, kept, rows(row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping");

    // Assert
    expect([patched, [...box.children]]).toEqual([{ drawn: 0, removed: 0, moved: 0 }, before]);
  });

  it("refuses a workspace drawn twice in one section, changing nothing", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "r", "grouping", kept);
    const before = [...box.children];

    // Act
    const patch = (): unknown => patchKeyedRows(box, kept, rows(row({ id: "a" }), row({ id: "a" })), rail, "r", "grouping");

    // Assert
    expect(patch).toThrow(/drawn twice in one section/);
    expect([[...box.children], kept.entries.map((entry) => entry.id)]).toEqual([before, ["a", "b"]]);
  });

  it("takes down a redrawn row's clocks with its old drawing", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const kept: KeyedRows = { entries: [] };
    drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", "grouping", kept);
    const drawnOnce = ticker.subscribers();

    // Act
    const box = document.createElement("div");
    placeKeyedRows(box, kept);
    patchKeyedRows(box, kept, rows(ticking("a", true), ticking("b")), rail, "r", "grouping");

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
  it("patches every region of a section at once", () => {
    // Arrange: the section's own rows, and one subsection's.
    const rail = sidebarContext();
    const own: KeyedRows = { entries: [] };
    const sub: KeyedRows = { entries: [] };
    const ownBox = drawKeyedRows(rows(row({ id: "a", current: true })), rail, "own", "grouping", own);
    const subBox = drawKeyedRows(rows(row({ id: "b" })), rail, "sub", "grouping", sub);

    // Act
    const patched = reconcileKeyedRegions(
      [
        { kept: own, rows: pathedOpenRows(rows(row({ id: "a" })), "own") },
        { kept: sub, rows: pathedOpenRows(rows(row({ id: "b", current: true })), "sub") },
      ],
      rail,
      "grouping",
      OPEN_ROW,
    );

    // Assert
    expect(patched).toEqual({ drawn: 2, removed: 0, moved: 0 });
    expect([ownBox.querySelector("[data-current]"), subBox.querySelector("[data-current]")?.getAttribute("data-roster-row")]).toEqual([
      null,
      "b",
    ]);
  });

  it("keeps a row that moved from one region to another, drawing none", () => {
    // Arrange: b leaves the section's own rows for the subsection.
    const rail = sidebarContext();
    const own: KeyedRows = { entries: [] };
    const sub: KeyedRows = { entries: [] };
    const ownBox = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "own", "grouping", own);
    const subBox = drawKeyedRows(rows(), rail, "sub", "grouping", sub);
    const b = elementOf(ownBox, "b");

    // Act
    const patched = reconcileKeyedRegions(
      [
        { kept: own, rows: pathedOpenRows(rows(row({ id: "a" })), "own") },
        { kept: sub, rows: pathedOpenRows(rows(row({ id: "b" })), "sub") },
      ],
      rail,
      "grouping",
      OPEN_ROW,
    );
    placeKeyedRows(ownBox, own);
    placeKeyedRows(subBox, sub);

    // Assert
    expect([patched, idsOf(ownBox), idsOf(subBox), elementOf(subBox, "b") === b]).toEqual([
      { drawn: 0, removed: 0, moved: 1 },
      ["a"],
      ["b"],
      true,
    ]);
  });
});

describe("keyed backlogged rows", () => {
  const hiddenOf = (box: HTMLElement, id: string): Element | null => elementOf(box, id);

  it.each([
    {
      name: "redraws only the hidden row whose wire changed",
      next: [row({ id: "a" }), row({ id: "b", current: true })],
      want: { drawn: 1, removed: 0, moved: 0 },
      keptA: true,
    },
    { name: "moves the backlogged rows reordered, drawing none", next: [row({ id: "b" }), row({ id: "a" })], want: { drawn: 0, removed: 0, moved: 2 }, keptA: true },
  ])("$name", ({ next, want, keptA }) => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows<RosterBacklogRow> = { entries: [] };
    const box = drawKeyedBacklogRows(backlogRows(row({ id: "a" }), row({ id: "b" })), rail, "h", "backlog", kept);
    const a = hiddenOf(box, "a");

    // Act
    const patched = reconcileKeyedRegions([{ kept, rows: pathedBacklogRows(backlogRows(...next), "h") }], rail, "backlog", BACKLOG_ROW);
    placeKeyedRows(box, kept);

    // Assert
    expect([patched, idsOf(box), hiddenOf(box, "a") === a]).toEqual([want, next.map((r) => r.workspace?.workspace?.id ?? null), keptA]);
  });
});
