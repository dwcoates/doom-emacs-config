/**
 * child-count — the grey "(unfolded/total)" a workspace row wears after its
 * name when it has more than `CHILD_COUNT_THRESHOLD` children (owner request,
 * 2026-11-15; spelled as every Open-section count is, `open-count.ts`, owner
 * request 2026-11-19).
 *
 * It is drawn as a subsection header's count is (`sb-count`, slightly smaller
 * and grey), so a row with many children says how many are drawn beneath it
 * of how many it has. The children counted are the row's direct ones: its own
 * repository's and every repository block's rows (`RowKin`). A child is
 * unfolded when its tab is numbered (`tabUnfolded`): its parent's children are
 * expanded and no other fold hides it, or it is surfaced.
 */
import type { RowKin } from "./row.js";
import { type OpenCount, openCountText, tabUnfolded } from "./open-count.js";

/** A row shows its child count only when it has MORE children than this. */
export const CHILD_COUNT_THRESHOLD = 5;

/** The class of a row's child count. */
export const CHILD_COUNT_CLASS = "sb-child-count";

/**
 * How many direct children KIN, the kin of the open row at PATH, holds (its
 * own repository's plus every block's rows), and how many of them are
 * unfolded. A child with no tab is malformed.
 */
export function childCountOf(kin: RowKin, path: string): OpenCount {
  const children = [
    ...(kin.children ?? []).map((child, i) => ({ child, path: `${path}.children[${i}]` })),
    ...(kin.repositoryBlocks ?? []).flatMap((block, b) =>
      block.rows.map((child, i) => ({ child, path: `${path}.repositoryBlocks[${b}].rows[${i}]` })),
    ),
  ];
  return {
    workspaces: children.length,
    unfolded: children.filter(({ child, path: childPath }) => tabUnfolded(child.tab, `${childPath}.tab`)).length,
  };
}

/** The "(unfolded/total)" element for KIN at PATH, or undefined when it has no more than the threshold's children. */
export function drawChildCount(kin: RowKin, path: string): HTMLElement | undefined {
  const count = childCountOf(kin, path);
  if (count.workspaces <= CHILD_COUNT_THRESHOLD) return undefined;
  const element = document.createElement("span");
  element.className = `sb-count ${CHILD_COUNT_CLASS}`;
  element.textContent = openCountText(count);
  return element;
}
