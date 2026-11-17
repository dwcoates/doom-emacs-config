/**
 * drawn-rows — every drawn copy of one workspace's row.
 *
 * A workspace's row can be drawn more than once under the rail (a band and its
 * section), and a fade that outlives a redraw paints every copy. This is the
 * one lookup each fade paints through.
 */
import { queryAll } from "../dom.js";

/** Every row drawn under ROOT for workspace ID, in document order. */
export function drawnRowsOf(root: ParentNode, id: string): HTMLElement[] {
  return queryAll<HTMLElement>(root, "[data-roster-row]").filter((row) => row.getAttribute("data-roster-row") === id);
}
