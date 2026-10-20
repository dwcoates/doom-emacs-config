/**
 * keyed-rows — a repository section's rows, each drawn under a scope of its
 * own, so a push that changed one row redraws that row alone.
 *
 * WHY (2026-10-15). A workspace switch moves the highlight from one row to
 * another, and stamps the newly selected row's last-selected instant: two
 * rows' wires change, and that used to redraw both rows' whole sections. A
 * section whose rows are the same workspaces in the same order now keeps
 * every row whose wire is unchanged, DOM and registrations alike, and draws
 * only the rows that changed in their places.
 */
import { clone, equals } from "@bufbuild/protobuf";
import {
  RosterOpenRowSchema,
  type RosterOpenRow,
  type RosterOpenRows,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { AttentionMarks, RailContext } from "./context.js";
import { openScope, type DrawScope } from "./scope.js";
import { drawRosterFamily, drawRosterRowWorkspace, type RowPlace } from "./row.js";
import { requireMessage } from "../rpc/strict.js";
import { pathedOpenRows, type PathedOpenRow } from "./entries.js";

/** One drawn row: the wire it was drawn from, its element, and its scope. */
interface KeyedRow {
  id: string;
  row: RosterOpenRow;
  element: HTMLElement;
  scope: DrawScope;
}

/**
 * ROW with every tab of its family cleared: what a row DRAWS. A tab's number
 * is painted after every draw (`tab-numbers.ts`), so a push that only
 * renumbers tabs redraws no row.
 */
function withoutTabs(row: RosterOpenRow): RosterOpenRow {
  const out = clone(RosterOpenRowSchema, row);
  const strip = (r: RosterOpenRow): void => {
    r.tab = undefined;
    r.children.forEach(strip);
  };
  strip(out);
  return out;
}

/** Whether two open rows draw the same, their tabs aside. */
function sameDrawing(a: RosterOpenRow, b: RosterOpenRow): boolean {
  return equals(RosterOpenRowSchema, withoutTabs(a), withoutTabs(b));
}

/** A section's drawn rows, in the order they are drawn. */
export interface KeyedRows {
  entries: KeyedRow[];
}

/** The top-level open rows the region draws, each with its path and workspace id. */
function visible(rows: readonly PathedOpenRow[]): Array<{ row: RosterOpenRow; path: string; id: string }> {
  return rows.map((entry) => {
    const row = requireMessage(entry.row.row, `${entry.path}.row`);
    return {
      row: entry.row,
      path: entry.path,
      id: drawRosterRowWorkspace(requireMessage(row.workspace, `${entry.path}.row.workspace`), `${entry.path}.row.workspace`).id,
    };
  });
}

/** Draw one row under a scope of its own. */
function drawKeyed(entry: { row: RosterOpenRow; path: string; id: string }, rail: RailContext, place: RowPlace): KeyedRow {
  const scope = openScope(rail);
  return { id: entry.id, row: entry.row, element: drawRosterFamily(entry.row, scope.context, entry.path, place), scope };
}

/**
 * Draw ROWS, each under a scope of its own, recording each into INTO, and
 * answer their elements in order for the caller to place: a section's top
 * list stands its subsections among them.
 */
export function drawKeyedEntries(rows: readonly PathedOpenRow[], rail: RailContext, place: RowPlace, into: KeyedRows): HTMLElement[] {
  into.entries = visible(rows).map((entry) => drawKeyed(entry, rail, place));
  return into.entries.map((entry) => entry.element);
}

/** Draw U's rows into a fresh box, recording each into INTO. */
export function drawKeyedRows(
  u: RosterOpenRows,
  rail: RailContext,
  path: string,
  place: RowPlace,
  into: KeyedRows,
): HTMLElement {
  const box = document.createElement("div");
  box.className = "rows";
  box.append(...drawKeyedEntries(pathedOpenRows(u, path), rail, place, into));
  return box;
}

/** One region of a section to bring in place: the rows drawn, and the wire's rows to bring them to. */
export interface KeyedRegion {
  kept: KeyedRows;
  rows: readonly PathedOpenRow[];
}

/**
 * Bring KEPT to U in place: answers false, changing nothing, when U's drawn
 * rows are not the same workspaces in the same order; otherwise redraws each
 * row whose wire changed where it stands and answers true.
 */
export function patchKeyedRows(
  kept: KeyedRows,
  u: RosterOpenRows,
  rail: RailContext,
  path: string,
  place: RowPlace,
): boolean {
  return patchKeyedRegions([{ kept, rows: pathedOpenRows(u, path) }], rail, place);
}

/**
 * Bring every one of a section's REGIONS in place at once: its own rows and
 * each subsection's. Answers false, changing NOTHING in any region, when any
 * region's drawn rows are not the same workspaces in the same order, so a
 * section is either patched whole or left for a full redraw.
 */
export function patchKeyedRegions(
  regions: readonly KeyedRegion[],
  rail: RailContext,
  place: RowPlace,
): boolean {
  const next = regions.map((region) => visible(region.rows));
  const matches = regions.every(
    (region, r) =>
      next[r].length === region.kept.entries.length && next[r].every((entry, i) => entry.id === region.kept.entries[i].id),
  );
  if (!matches) return false;
  regions.forEach((region, r) => {
    const kept = region.kept;
    next[r].forEach((entry, i) => {
      const old = kept.entries[i];
      if (sameDrawing(old.row, entry.row)) {
        old.row = entry.row;
        return;
      }
      const drawn = drawKeyed(entry, rail, place);
      old.element.replaceWith(drawn.element);
      old.scope.dispose(old.element);
      kept.entries[i] = drawn;
    });
  });
  return true;
}

/** A drawing pass keeps ROWS: mark their attention markers again on BASE. */
export function remarkKeyedRows(rows: KeyedRows, base: AttentionMarks): void {
  for (const entry of rows.entries) entry.scope.remark(base);
}

/** ROWS' section is gone: tear down every row's registrations. */
export function disposeKeyedRows(rows: KeyedRows): void {
  for (const entry of rows.entries.splice(0)) entry.scope.dispose(entry.element);
}
