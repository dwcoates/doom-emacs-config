/**
 * keyed-rows — a section's rows, each drawn under a scope of its own and kept
 * by its workspace, so a push that changed one row redraws that row alone.
 *
 * WHY (2026-10-15). A workspace switch moves the highlight from one row to
 * another, and stamps the newly selected row's last-selected instant: two
 * rows' wires change, and that used to redraw both rows' whole sections. A
 * section now keeps every row whose wire is unchanged, DOM and registrations
 * alike, and draws only the rows that changed.
 *
 * A REORDER DRAWS NO ROW (2026-11-23). A handover moved rows between status
 * groups and in and out of sections, and every such push drew the whole
 * section anew, every one of its rows (92 draws of all 95 rows in three
 * minutes). Rows are now matched by their workspace across the whole
 * section, wherever they stand: a row that moved is moved, a row that
 * arrived is drawn, a row that left is taken down, and a row whose drawing
 * is unchanged is never touched.
 */
import { clone, equals } from "@bufbuild/protobuf";
import {
  RosterBacklogRowSchema,
  RosterOpenRowSchema,
  type RosterBacklogRow,
  type RosterBacklogRows,
  type RosterOpenRow,
  type RosterOpenRows,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { AttentionMarks, RailContext } from "./context.js";
import { openScope, type DrawScope } from "./scope.js";
import { drawRosterFamily, drawRosterRowWorkspace, type RowPlace } from "./row.js";
import { requireMessage } from "../rpc/strict.js";
import { familyRows, pathedOpenRows, type PathedOpenRow } from "./entries.js";
import { pathedFamilies, type RowFamily } from "./family.js";
import { placeChildren } from "../dom.js";

/** One drawn row: the wire it was drawn from, its element, and its scope. */
interface KeyedRow<R extends RowFamily> {
  id: string;
  row: R;
  element: HTMLElement;
  scope: DrawScope;
}

/** A section's drawn rows of one region, in the order they are drawn. */
export interface KeyedRows<R extends RowFamily = RosterOpenRow> {
  entries: Array<KeyedRow<R>>;
}

/** One row of a region as the wire states it, with its path. */
export interface PathedRow<R extends RowFamily> {
  row: R;
  path: string;
}

/** What tells two drawings of one kind of row apart: equal answers true when they draw the same. */
export interface RowKind<R extends RowFamily> {
  same(a: R, b: R): boolean;
}

/**
 * ROW with every tab of its family cleared: what an open row DRAWS. A tab's
 * number is painted after every draw (`tab-numbers.ts`), so a push that only
 * renumbers tabs redraws no row.
 */
function withoutTabs(row: RosterOpenRow): RosterOpenRow {
  const out = clone(RosterOpenRowSchema, row);
  for (const { row: r } of familyRows(out, "")) r.tab = undefined;
  return out;
}

/** An open row (frontend.v1.RosterOpenRow): two draw the same when they are equal but for their tabs. */
export const OPEN_ROW: RowKind<RosterOpenRow> = {
  same: (a, b) => equals(RosterOpenRowSchema, withoutTabs(a), withoutTabs(b)),
};

/** A hidden row (frontend.v1.RosterBacklogRow): two draw the same when they are equal. */
export const HIDDEN_ROW: RowKind<RosterBacklogRow> = {
  same: (a, b) => equals(RosterBacklogRowSchema, a, b),
};

/** The top-level rows a region draws, each with its path and workspace id. */
function visible<R extends RowFamily>(rows: ReadonlyArray<PathedRow<R>>): Array<PathedRow<R> & { id: string }> {
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
function drawKeyed<R extends RowFamily>(entry: PathedRow<R> & { id: string }, rail: RailContext, place: RowPlace): KeyedRow<R> {
  const scope = openScope(rail);
  return { id: entry.id, row: entry.row, element: drawRosterFamily(entry.row, scope.context, entry.path, place), scope };
}

/**
 * Draw ROWS, each under a scope of its own, recording each into INTO, and
 * answer their elements in order for the caller to place: a section's top
 * list stands its subsections among them.
 */
export function drawKeyedEntries<R extends RowFamily>(
  rows: ReadonlyArray<PathedRow<R>>,
  rail: RailContext,
  place: RowPlace,
  into: KeyedRows<R>,
): HTMLElement[] {
  into.entries = visible(rows).map((entry) => drawKeyed(entry, rail, place));
  return into.entries.map((entry) => entry.element);
}

/** Draw U's open rows into a fresh box, recording each into INTO. */
export function drawKeyedRows(
  u: RosterOpenRows,
  rail: RailContext,
  path: string,
  place: RowPlace,
  into: KeyedRows,
): HTMLElement {
  return drawKeyedBox(pathedOpenRows(u, path), rail, place, into);
}

/** Draw U's hidden rows into a fresh box, recording each into INTO. */
export function drawKeyedHiddenRows(
  u: RosterBacklogRows,
  rail: RailContext,
  path: string,
  place: RowPlace,
  into: KeyedRows<RosterBacklogRow>,
): HTMLElement {
  return drawKeyedBox(pathedHiddenRows(u, path), rail, place, into);
}

/** ROWS' top-level hidden rows, each with its path under PATH. */
export function pathedHiddenRows(rows: RosterBacklogRows, path: string): Array<PathedRow<RosterBacklogRow>> {
  return pathedFamilies(rows.rows, `${path}.rows`).map((entry, index) => ({ row: rows.rows[index], path: entry.path }));
}

/** Draw ROWS into a fresh rows box, recording each into INTO. */
function drawKeyedBox<R extends RowFamily>(
  rows: ReadonlyArray<PathedRow<R>>,
  rail: RailContext,
  place: RowPlace,
  into: KeyedRows<R>,
): HTMLElement {
  const box = document.createElement("div");
  box.className = "rows";
  box.append(...drawKeyedEntries(rows, rail, place, into));
  return box;
}

/** One region of a section to bring in place: the rows drawn, and the wire's rows to bring them to. */
export interface KeyedRegion<R extends RowFamily = RosterOpenRow> {
  kept: KeyedRows<R>;
  rows: ReadonlyArray<PathedRow<R>>;
}

/** What bringing a section's rows in place did. */
export interface Reconciled {
  /** Rows drawn: each that arrived, and each whose drawing changed. */
  drawn: number;
  /** Rows taken down because their workspace left the section. */
  removed: number;
  /** Rows kept as drawn but moved to another place. */
  moved: number;
}

/** Whether reconciling changed anything a reader sees. */
export function reconciledAnything(r: Reconciled): boolean {
  return r.drawn + r.removed + r.moved > 0;
}

/**
 * Bring every one of a section's REGIONS to its wire's rows, matching rows by
 * their workspace across the whole section: a row kept anywhere in the
 * section is kept wherever it now stands, redrawn only when its drawing
 * changed. Each region's entries end in the wire's order; the CALLER places
 * the elements (`placeKeyedRows`, or the section's own interleaving).
 *
 * A WORKSPACE TWICE in one section's wire is a broken invariant of the
 * daemon's roster (each workspace is one row of the rail), thrown before
 * anything is changed.
 */
export function reconcileKeyedRegions<R extends RowFamily>(
  regions: ReadonlyArray<KeyedRegion<R>>,
  rail: RailContext,
  place: RowPlace,
  kind: RowKind<R>,
): Reconciled {
  const next = regions.map((region) => visible(region.rows));
  const seen = new Set<string>();
  for (const entry of next.flat()) {
    if (seen.has(entry.id)) throw new Error(`invariant: workspace ${entry.id} is drawn twice in one section (${entry.path})`);
    seen.add(entry.id);
  }
  const pool = new Map<string, { entry: KeyedRow<R>; region: number; index: number }>();
  regions.forEach((region, r) => region.kept.entries.forEach((entry, index) => pool.set(entry.id, { entry, region: r, index })));
  const result: Reconciled = { drawn: 0, removed: 0, moved: 0 };
  const retired: Array<KeyedRow<R>> = [];
  const entries = next.map((rows, r) =>
    rows.map((entry, index) => {
      const old = pool.get(entry.id);
      if (old === undefined) {
        result.drawn += 1;
        return drawKeyed(entry, rail, place);
      }
      pool.delete(entry.id);
      if (old.region !== r || old.index !== index) result.moved += 1;
      if (kind.same(old.entry.row, entry.row)) {
        old.entry.row = entry.row;
        return old.entry;
      }
      result.drawn += 1;
      const drawn = drawKeyed(entry, rail, place);
      // The redrawn row takes the old one's place now, so a caller that
      // places nothing (a region whose order held) still shows it.
      old.entry.element.replaceWith(drawn.element);
      retired.push(old.entry);
      return drawn;
    }),
  );
  for (const { entry } of pool.values()) {
    result.removed += 1;
    entry.element.remove();
    retired.push(entry);
  }
  regions.forEach((region, r) => {
    region.kept.entries = entries[r];
  });
  // Taken down only once every kept row is recorded, so a retired row's
  // teardown never reaches a row still drawn.
  for (const entry of retired) entry.scope.dispose(entry.element);
  return result;
}

/** Place ROWS' elements as BOX's children, in order, moving only what is out of place. */
export function placeKeyedRows<R extends RowFamily>(box: HTMLElement, rows: KeyedRows<R>): void {
  placeChildren(
    box,
    rows.entries.map((entry) => entry.element),
  );
}

/**
 * Bring KEPT, drawn into BOX, to U's open rows in place: the one-region form
 * of `reconcileKeyedRegions`.
 */
export function patchKeyedRows(
  box: HTMLElement,
  kept: KeyedRows,
  u: RosterOpenRows,
  rail: RailContext,
  path: string,
  place: RowPlace,
): Reconciled {
  const result = reconcileKeyedRegions([{ kept, rows: pathedOpenRows(u, path) }], rail, place, OPEN_ROW);
  placeKeyedRows(box, kept);
  return result;
}

/** A drawing pass keeps ROWS: mark their attention markers again on BASE. */
export function remarkKeyedRows<R extends RowFamily>(rows: KeyedRows<R>, base: AttentionMarks): void {
  for (const entry of rows.entries) entry.scope.remark(base);
}

/** ROWS' section is gone: tear down every row's registrations. */
export function disposeKeyedRows<R extends RowFamily>(rows: KeyedRows<R>): void {
  for (const entry of rows.entries.splice(0)) entry.scope.dispose(entry.element);
}

export type { PathedOpenRow };
