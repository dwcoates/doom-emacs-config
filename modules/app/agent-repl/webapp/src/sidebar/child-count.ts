/**
 * child-count — the grey "(N)" a workspace row wears after its name when it has
 * more than `CHILD_COUNT_THRESHOLD` children (owner request, 2026-11-15).
 *
 * It is drawn as a subsection header's count is (`sb-count`, slightly smaller
 * and grey), so a row with many children says how many are folded or drawn
 * beneath it. The children counted are the row's direct ones: its own
 * repository's and every repository block's rows (`RowKin`).
 */
import type { RowKin } from "./row.js";

/** A row shows its child count only when it has MORE children than this. */
export const CHILD_COUNT_THRESHOLD = 5;

/** The class of a row's child count. */
export const CHILD_COUNT_CLASS = "sb-child-count";

/** How many direct children KIN holds: its own repository's plus every block's rows. */
export function childCountOf(kin: RowKin): number {
  const own = kin.children?.length ?? 0;
  return (kin.repositoryBlocks ?? []).reduce((total, block) => total + block.rows.length, own);
}

/** The "(N)" element for KIN, or undefined when it has no more than the threshold's children. */
export function drawChildCount(kin: RowKin): HTMLElement | undefined {
  const count = childCountOf(kin);
  if (count <= CHILD_COUNT_THRESHOLD) return undefined;
  const element = document.createElement("span");
  element.className = `sb-count ${CHILD_COUNT_CLASS}`;
  element.textContent = `(${count})`;
  return element;
}
