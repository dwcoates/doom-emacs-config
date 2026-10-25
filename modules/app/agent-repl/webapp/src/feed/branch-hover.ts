/**
 * branch-hover — A RESPONSE'S BRANCH IS BOXED WHILE THE POINTER IS ON IT, as
 * the composer's is while the person types its entry (owner ruling,
 * 2026-10-24): the box over the hovered node's subtree (subtree-box.ts) says
 * which node a click answers. The click itself is bubble-selection's
 * (`branchAt`): it asks Emacs to open a blank entry answering the node in the
 * composer, and never selects the bubble.
 *
 * The box has its own class and edges, so it never moves the composer's box
 * over the same lines, and the stylesheet draws it over the composer's while
 * the pointer is there.
 */
import { log } from "../log.js";
import { branchAt, governedRowAt } from "./bubble-selection.js";
import { feedRowId } from "./row-id.js";
import { boxSubtree, unboxAll, type SubtreeBox } from "./subtree-box.js";

/** The class every line of the hovered branch's box wears. */
export const BRANCH_HOVER_CLASS = "mp-line-hovered";

/** The property naming the hover box's left edge, in `ch`, on each line inside it. */
export const HOVER_LEFT_PROPERTY = "--hover-left";

/** The property naming the hover box's right edge, in `ch`, on each line inside it. */
export const HOVER_RIGHT_PROPERTY = "--hover-right";

/** The hover's box (subtree-box.ts). */
const HOVER_BOX: SubtreeBox = {
  className: BRANCH_HOVER_CLASS,
  leftProperty: HOVER_LEFT_PROPERTY,
  rightProperty: HOVER_RIGHT_PROPERTY,
};

/** Arm the hover box on the root feed HOST. Answers the uninstall. */
export function installBranchHover(host: HTMLElement): () => void {
  let hovered: { row: HTMLElement; label: string } | undefined;
  const clear = (): void => {
    if (hovered === undefined) return;
    unboxAll(hovered.row, HOVER_BOX);
    hovered = undefined;
  };
  const onOver = (event: MouseEvent): void => {
    const row = governedRowAt(event.target, host);
    const branch = row !== null && event.target instanceof Element ? branchAt(row, event.target) : null;
    if (row === null || branch === null) {
      clear();
      return;
    }
    // A repaint of the row's prose drops the box, so the same node is boxed
    // again while its line no longer wears it.
    if (hovered?.row === row && hovered.label === branch.label && branch.line.classList.contains(BRANCH_HOVER_CLASS)) return;
    clear();
    boxSubtree(row, branch.label, HOVER_BOX);
    hovered = { row, label: branch.label };
    log.debug("boxed the response branch under the pointer", {
      operation: "feed.branch-hover",
      context: { row: feedRowId(row) ?? "unset", label: branch.label },
    });
  };
  host.addEventListener("mouseover", onOver);
  host.addEventListener("mouseleave", clear);
  return () => {
    host.removeEventListener("mouseover", onOver);
    host.removeEventListener("mouseleave", clear);
    clear();
  };
}
