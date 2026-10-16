// @vitest-environment jsdom
// A repository section's rows drawn each under its own scope. See src/sidebar/keyed-rows.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterRowsSchema, type RosterRow } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  disposeKeyedRows,
  drawKeyedRows,
  patchKeyedRows,
  type KeyedRows,
} from "../../src/sidebar/keyed-rows.js";
import { appContext, fakeTicker, NOW, row, sidebarContext } from "./harness.js";

const rows = (...r: RosterRow[]) => create(RosterRowsSchema, { rows: r });
const ticking = (id: string, current = false): RosterRow =>
  row({ id, current, when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } });
const elementOf = (box: HTMLElement, id: string): Element | null => box.querySelector(`[data-roster-row='${id}']`);

describe("keyed rows", () => {
  it("redraws only the rows whose wire changed, where they stand", () => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a", current: true }), row({ id: "b" }), row({ id: "c" })), rail, "r", true, "grouping", kept);
    const [a, b, c] = ["a", "b", "c"].map((id) => elementOf(box, id));

    // Act
    const patched = patchKeyedRows(kept, rows(row({ id: "a" }), row({ id: "b", current: true }), row({ id: "c" })), rail, "r", true, "grouping");

    // Assert
    expect(patched).toBe(true);
    expect([elementOf(box, "a") === a, elementOf(box, "b") === b, elementOf(box, "c") === c]).toEqual([false, false, true]);
    expect([...box.querySelectorAll("[data-current]")].map((el) => el.getAttribute("data-roster-row"))).toEqual(["b"]);
    expect([...box.children].map((el) => el.getAttribute("data-roster-row"))).toEqual(["a", "b", "c"]);
  });

  it.each([
    ["a row more", [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })]],
    ["the rows reordered", [row({ id: "b" }), row({ id: "a" })]],
    ["another workspace in a row's place", [row({ id: "a" }), row({ id: "z" })]],
  ])("refuses %s, changing nothing", (_name, next) => {
    // Arrange
    const rail = sidebarContext();
    const kept: KeyedRows = { entries: [] };
    const box = drawKeyedRows(rows(row({ id: "a" }), row({ id: "b" })), rail, "r", true, "grouping", kept);
    const before = [...box.children];

    // Act
    const patched = patchKeyedRows(kept, rows(...next), rail, "r", true, "grouping");

    // Assert
    expect([patched, [...box.children]]).toEqual([false, before]);
  });

  it("takes down a redrawn row's clocks with its old drawing", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const kept: KeyedRows = { entries: [] };
    drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", true, "grouping", kept);
    const drawnOnce = ticker.subscribers();

    // Act
    patchKeyedRows(kept, rows(ticking("a", true), ticking("b")), rail, "r", true, "grouping");

    // Assert
    expect(ticker.subscribers()).toBe(drawnOnce);
  });

  it("takes down every row's clocks when the rows are disposed", () => {
    // Arrange
    const ticker = fakeTicker();
    const rail = sidebarContext(appContext(undefined, ticker));
    const before = ticker.subscribers();
    const kept: KeyedRows = { entries: [] };
    drawKeyedRows(rows(ticking("a"), ticking("b")), rail, "r", true, "grouping", kept);

    // Act
    disposeKeyedRows(kept);

    // Assert
    expect([ticker.subscribers(), kept.entries.length]).toEqual([before, 0]);
  });
});
