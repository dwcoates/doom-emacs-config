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
 *
 * A CITATION IN A RESPONSE'S TREE BOXES THE NODE IT CITES while the pointer is
 * on it (owner request, 2026-10-26): the citation's label (`(1.3.1)`, marked
 * by metaprompt-tree.ts's `markReferences`) boxes the cited node's subtree in
 * a box of its own color, and the branch holding the citation is not boxed
 * meanwhile.
 */
import { log } from "../log.js";
import { branchAt, governedRowAt } from "./bubble-selection.js";
import { TREE_CLASS, TREE_REFERENCE_ATTRIBUTE, TREE_REFERENCE_CLASS } from "../metaprompt-tree.js";
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

/** The class every line of the subtree a hovered citation cites wears. */
export const REFERENCE_HOVER_CLASS = "mp-line-referenced";

/** The property naming the cited box's left edge, in `ch`, on each line inside it. */
export const REFERENCE_LEFT_PROPERTY = "--reference-left";

/** The property naming the cited box's right edge, in `ch`, on each line inside it. */
export const REFERENCE_RIGHT_PROPERTY = "--reference-right";

/** The box over the node a hovered citation cites (subtree-box.ts). */
const REFERENCE_BOX: SubtreeBox = {
  className: REFERENCE_HOVER_CLASS,
  leftProperty: REFERENCE_LEFT_PROPERTY,
  rightProperty: REFERENCE_RIGHT_PROPERTY,
};

/**
 * The label TARGET cites, inside a drawn tree of the response ROW, or null:
 * a citation is a response's own cross-reference, so only a response's count.
 */
export function citationAt(row: HTMLElement, target: Element): string | null {
  if (row.getAttribute("data-unit") !== "response") return null;
  const citation = target.closest<HTMLElement>(`.${TREE_CLASS} .${TREE_REFERENCE_CLASS}`);
  if (citation === null || !row.contains(citation)) return null;
  const label = citation.getAttribute(TREE_REFERENCE_ATTRIBUTE);
  if (label === null) throw new Error("a citation the reference selector matched names no label");
  return label;
}

/** What the pointer boxes: a branch it is on, or the node a citation it is on cites. */
function boxUnder(row: HTMLElement, target: Element): { label: string; box: SubtreeBox; operation: string } | null {
  const cited = citationAt(row, target);
  if (cited !== null) return { label: cited, box: REFERENCE_BOX, operation: "feed.branch-hover.reference" };
  const branch = branchAt(row, target);
  return branch === null ? null : { label: branch.label, box: HOVER_BOX, operation: "feed.branch-hover" };
}

/** Arm the hover box on the root feed HOST. Answers the uninstall. */
export function installBranchHover(host: HTMLElement): () => void {
  let hovered: { row: HTMLElement; label: string; box: SubtreeBox } | undefined;
  const clear = (): void => {
    if (hovered === undefined) return;
    unboxAll(hovered.row, hovered.box);
    hovered = undefined;
  };
  const onOver = (event: MouseEvent): void => {
    const row = governedRowAt(event.target, host);
    const under = row !== null && event.target instanceof Element ? boxUnder(row, event.target) : null;
    if (row === null || under === null) {
      clear();
      return;
    }
    // A repaint of the row's prose drops the box, so the same node is boxed
    // again while its lines no longer wear it.
    if (
      hovered?.row === row &&
      hovered.label === under.label &&
      hovered.box === under.box &&
      row.querySelector(`.${under.box.className}`) !== null
    )
      return;
    clear();
    boxSubtree(row, under.label, under.box);
    hovered = { row, label: under.label, box: under.box };
    log.debug(
      under.box === REFERENCE_BOX
        ? "boxed the response branch the citation under the pointer cites"
        : "boxed the response branch under the pointer",
      { operation: under.operation, context: { row: feedRowId(row) ?? "unset", label: under.label } },
    );
  };
  host.addEventListener("mouseover", onOver);
  host.addEventListener("mouseleave", clear);
  return () => {
    host.removeEventListener("mouseover", onOver);
    host.removeEventListener("mouseleave", clear);
    clear();
  };
}
