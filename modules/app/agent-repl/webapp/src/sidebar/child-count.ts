/**
 * child-count — the "(working/merging/total)" a workspace row wears after its
 * name when it has more than `CHILD_COUNT_THRESHOLD` children (owner request,
 * 2026-11-15; spelled as every Open-section count is, `open-count.ts`, owner
 * request 2026-11-19).
 *
 * It is the parenthetical a subsection header wears
 * (`showWorkspaceCountParenthetical`, owner request 2026-11-20), so a row with
 * many children says how many of them work, how many merge synchronously and
 * how many it has. The children counted are the row's direct ones: its own
 * repository's and every repository block's rows (`RowKin`).
 */
import type { RowFamily, RowKin } from "./family.js";
import { openKin } from "./entries.js";
import { type StatusCount, showWorkspaceCountParenthetical, statusCountOf } from "./open-count.js";

/** A row shows its child count only when it has MORE children than this. */
export const CHILD_COUNT_THRESHOLD = 5;

/** The class of a row's child count. */
export const CHILD_COUNT_CLASS = "sb-child-count";

/** A row's child count: how many direct children it holds, and how many of them work or merge. */
export interface ChildCount extends StatusCount {
  workspaces: number;
}

/**
 * How many direct children KIN, the kin of the open row at PATH, holds (its
 * own repository's plus every block's rows), and how many of them work or
 * merge synchronously. A child with no status is malformed.
 */
export function childCountOf(kin: RowKin, path: string): ChildCount {
  const children = openKin<RowFamily>(kin, path);
  return {
    workspaces: children.length,
    ...statusCountOf(children.map((child) => ({ row: child.row.row, path: `${child.path}.row` }))),
  };
}

/** The "(working/merging/total)" element for KIN at PATH, or undefined when it has no more than the threshold's children. */
export function drawChildCount(kin: RowKin, path: string): HTMLElement | undefined {
  const count = childCountOf(kin, path);
  if (count.workspaces <= CHILD_COUNT_THRESHOLD) return undefined;
  const element = showWorkspaceCountParenthetical(count.workspaces, count);
  element.classList.add(CHILD_COUNT_CLASS);
  return element;
}
