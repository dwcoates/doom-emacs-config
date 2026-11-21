/**
 * child-count — the "(merging/total)" a workspace row wears after its
 * name when it has any children (owner request,
 * 2026-11-15; spelled as every Open-section count is, `open-count.ts`, owner
 * request 2026-11-19).
 *
 * It is the parenthetical a subsection header wears
 * (`showWorkspaceCountParenthetical`, owner request 2026-11-20), so a row with
 * many children says how many of them merge synchronously and how many it has. The children counted are the row's direct ones: its own
 * repository's and every repository block's rows (`RowKin`).
 */
import type { RowFamily, RowKin } from "./family.js";
import { openKin } from "./entries.js";
import { type StatusCount, showWorkspaceCountParenthetical, statusCountOf } from "./open-count.js";

/** The class of a row's child count. */
export const CHILD_COUNT_CLASS = "sb-child-count";

/** A row's child count: how many direct children it holds, and how many of them merge synchronously. */
export interface ChildCount extends StatusCount {
  workspaces: number;
}

/**
 * How many direct children KIN, the kin of the open row at PATH, holds (its
 * own repository's plus every block's rows), and how many of them merge
 * synchronously. A child with no status is malformed.
 */
export function childCountOf(kin: RowKin, path: string): ChildCount {
  const children = openKin<RowFamily>(kin, path);
  return {
    workspaces: children.length,
    ...statusCountOf(children.map((child) => ({ row: child.row.row, path: `${child.path}.row` }))),
  };
}

/** The "(merging/total)" count element for KIN at PATH, or undefined when it has no children. */
export function drawChildCount(kin: RowKin, path: string): HTMLElement | undefined {
  const count = childCountOf(kin, path);
  if (count.workspaces === 0) return undefined;
  const element = showWorkspaceCountParenthetical(count.workspaces, count);
  element.classList.add(CHILD_COUNT_CLASS);
  return element;
}
