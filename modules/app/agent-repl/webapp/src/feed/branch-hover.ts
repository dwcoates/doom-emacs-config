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
 *
 * A PROMPT'S ENTRY LABEL BOXES THE RESPONSE NODE IT ANSWERS while the pointer
 * is on it (owner request, 2026-10-27), in the box a response's own branch
 * takes, drawn as the composer's is while the person types that entry: the
 * label (`entries.ts`'s `answeredNodeAt`) names the response row and node.
 *
 * THE AREA THAT BOXES A BRANCH IS THE BOX IT DRAWS (owner request,
 * 2026-10-28): on a tree line the pointer boxes the deepest node whose box
 * covers it (`branchAt`'s rule, the one a click answers by), so it is
 * followed across a line as well as onto one. A line's boxes are measured
 * once while the pointer stays on it.
 */
import { log } from "../log.js";
import { selectionTouches } from "../selection.js";
import { branchLineAt, governedRowAt, isResponseRow } from "./bubble-selection.js";
import { TREE_CLASS, TREE_REFERENCE_ATTRIBUTE, TREE_REFERENCE_CLASS } from "../metaprompt-tree.js";
import { answeredNodeAt } from "./entries.js";
import { feedRowId } from "./row-id.js";
import {
  boxSubtree,
  boxesOverLine,
  lengthInLine,
  nodeBoxedAt,
  unboxAll,
  xInLine,
  type LengthResolver,
  type LineBox,
  type SubtreeBox,
} from "./subtree-box.js";

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
  if (!isResponseRow(row)) return null;
  const citation = target.closest<HTMLElement>(`.${TREE_CLASS} .${TREE_REFERENCE_CLASS}`);
  if (citation === null || !row.contains(citation)) return null;
  const label = citation.getAttribute(TREE_REFERENCE_ATTRIBUTE);
  if (label === null) throw new Error("a citation the reference selector matched names no label");
  return label;
}

/** One box the pointer stands: the row it is drawn in, the node it boxes, and how. */
interface Hovered {
  readonly row: HTMLElement;
  readonly label: string;
  readonly box: SubtreeBox;
}

/** The boxes over a tree line, measured once per line the pointer stands on. */
type LineBoxes = (line: HTMLElement) => readonly LineBox[];

/**
 * What the pointer at CLIENT_X on TARGET, in the governed ROW of the feed
 * HOST, boxes: a branch whose box it is in, the node a citation it is on
 * cites, or the response node a prompt's entry label it is on answers.
 */
function boxUnder(
  host: HTMLElement,
  row: HTMLElement,
  target: Element,
  clientX: number,
  boxesOf: LineBoxes,
): (Hovered & { operation: string; record: string }) | null {
  const cited = citationAt(row, target);
  if (cited !== null) {
    return {
      row,
      label: cited,
      box: REFERENCE_BOX,
      operation: "feed.branch-hover.reference",
      record: "boxed the response branch the citation under the pointer cites",
    };
  }
  const answered = answeredNodeAt(host, row, target);
  if (answered !== null) {
    return {
      ...answered,
      box: HOVER_BOX,
      operation: "feed.branch-hover.entry",
      record: "boxed the response branch the prompt entry under the pointer answers",
    };
  }
  const line = branchLineAt(row, target);
  if (line === null) return null;
  const label = nodeBoxedAt(boxesOf(line), xInLine(line, clientX));
  if (label === null) return null;
  return { row, label, box: HOVER_BOX, operation: "feed.branch-hover", record: "boxed the response branch under the pointer" };
}

/** Arm the hover box on the root feed HOST, RESOLVE measuring box edges. Answers the uninstall. */
export function installBranchHover(host: HTMLElement, resolve: LengthResolver = lengthInLine): () => void {
  let hovered: Hovered | undefined;
  let measured: { line: HTMLElement; boxes: readonly LineBox[] } | undefined;
  const boxesOf: LineBoxes = (line) => {
    if (measured?.line !== line) measured = { line, boxes: boxesOverLine(line, resolve) };
    return measured.boxes;
  };
  const clear = (): void => {
    if (hovered === undefined) return;
    unboxAll(hovered.row, hovered.box);
    hovered = undefined;
  };
  const onOver = (event: MouseEvent): void => {
    const row = governedRowAt(event.target, host);
    // A BUBBLE THE READER IS HIGHLIGHTING IN IS NOT BOXED (owner request,
    // 2026-10-28): the highlight is what a prompt sent now acts on, and a box
    // over the branch beneath it would say otherwise.
    if (row !== null && selectionTouches(row, host.ownerDocument)) {
      clear();
      return;
    }
    const under = row !== null && event.target instanceof Element ? boxUnder(host, row, event.target, event.clientX, boxesOf) : null;
    if (under === null) {
      clear();
      return;
    }
    // A repaint of the row's prose drops the box, so the same node is boxed
    // again while its lines no longer wear it.
    if (
      hovered?.row === under.row &&
      hovered.label === under.label &&
      hovered.box === under.box &&
      under.row.querySelector(`.${under.box.className}`) !== null
    )
      return;
    clear();
    boxSubtree(under.row, under.label, under.box);
    hovered = { row: under.row, label: under.label, box: under.box };
    log.debug(under.record, {
      operation: under.operation,
      context: { row: feedRowId(under.row) ?? "unset", label: under.label },
    });
  };
  // A SELECTION MADE UNDER A STANDING BOX takes the box down at once, before
  // the pointer moves again.
  const onSelectionChange = (): void => {
    if (hovered !== undefined && selectionTouches(hovered.row, host.ownerDocument)) clear();
  };
  host.addEventListener("mouseover", onOver);
  host.addEventListener("mousemove", onOver);
  host.addEventListener("mouseleave", clear);
  host.ownerDocument.addEventListener("selectionchange", onSelectionChange);
  return () => {
    host.removeEventListener("mouseover", onOver);
    host.removeEventListener("mousemove", onOver);
    host.removeEventListener("mouseleave", clear);
    host.ownerDocument.removeEventListener("selectionchange", onSelectionChange);
    clear();
  };
}
