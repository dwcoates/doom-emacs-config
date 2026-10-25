/**
 * subtree-box — ONE BOX OVER A NODE'S SUBTREE in a drawn response tree, the
 * shape every highlight of a response's node takes (owner ruling,
 * 2026-10-24):
 *   - its rows are the node's lines and every line of each node beneath it
 *     (a response tree's labels are its nodes' places, as the daemon repairs
 *     them, so the subtree of `3.1` is `3.1` and every label opening on
 *     `3.1.`);
 *   - its left edge is the column the node's label starts at, so the
 *     connector leading to the node is outside it and its descendants' rails
 *     are inside;
 *   - its right edge is the nearest column covering every boxed line's text.
 * A tree is drawn in a monospace face with every wide character boxed to two
 * columns (metaprompt-tree.ts), so a column is one `ch`.
 *
 * Each highlight is a SubtreeBox of its own (the composer's, a hover's, a
 * revealed node's), with its own class and edge properties, so two boxes over
 * the same lines never overwrite each other's edges.
 *
 * A box that STANDS over a node until it is moved (`standingBox`: the
 * composer's, a revealed node's) is restated on every paint: a response's
 * prose is repainted whenever it re-wraps or re-arrives, so the response's
 * prose slot marks it each time it paints, and moving the box re-marks
 * whatever is drawn now.
 */
import { queryAll } from "../dom.js";
import { FEED_ROW_SELECTOR, feedRowId } from "./row-id.js";
import { TREE_CLASS, TREE_LABEL_CLASS, TREE_LINE_CLASS, TREE_NODE_ATTRIBUTE, textWidth } from "../metaprompt-tree.js";

/** One kind of box: the class its lines wear and the properties naming its edges, in `ch`. */
export interface SubtreeBox {
  readonly className: string;
  readonly leftProperty: string;
  readonly rightProperty: string;
}

/** Whether NODE (a line's node label) is LABEL or beneath it. */
export function inSubtree(node: string | null, label: string): boolean {
  return node !== null && (node === label || node.startsWith(`${label}.`));
}

/** The column LINE's own label starts at: the width of its prefix before the label. */
export function labelColumn(line: HTMLElement): number {
  let text = "";
  for (const child of line.querySelector(".mp-prefix")?.childNodes ?? []) {
    if (child instanceof HTMLElement && child.classList.contains(TREE_LABEL_CLASS)) break;
    text += child.textContent ?? "";
  }
  return textWidth(text);
}

/** Take BOX off every line in SCOPE. */
export function unboxAll(scope: ParentNode, box: SubtreeBox): void {
  for (const line of queryAll<HTMLElement>(scope, `.${box.className}`)) {
    line.classList.remove(box.className);
    line.style.removeProperty(box.leftProperty);
    line.style.removeProperty(box.rightProperty);
  }
}

/**
 * Draw BOX over the subtree of LABEL in each tree in SCOPE that holds it, each
 * tree on its own. Lines already wearing BOX elsewhere keep it: the caller
 * takes the old box off first (`unboxAll`) when the box moves.
 */
export function boxSubtree(scope: ParentNode, label: string, box: SubtreeBox): void {
  for (const tree of queryAll<HTMLElement>(scope, `.${TREE_CLASS}`)) {
    const lines = queryAll<HTMLElement>(tree, `.${TREE_LINE_CLASS}`).filter((l) =>
      inSubtree(l.getAttribute(TREE_NODE_ATTRIBUTE), label),
    );
    const head = lines.find((l) => l.getAttribute(TREE_NODE_ATTRIBUTE) === label);
    if (head === undefined) continue;
    const left = labelColumn(head);
    const right = Math.max(...lines.map((l) => textWidth((l.textContent ?? "").trimEnd())));
    for (const line of lines) {
      line.classList.add(box.className);
      line.style.setProperty(box.leftProperty, `${left}ch`);
      line.style.setProperty(box.rightProperty, `${right}ch`);
    }
  }
}

/** A node of a drawn response's tree: the response's row and the node's label. */
export interface BoxedNode {
  readonly row: string;
  readonly label: string;
}

/** A box standing over one node, or none, until it is moved. */
export interface StandingBox {
  /** Stand the box over NODE (none takes it down), redrawn over every row drawn in DOC. */
  stand(doc: Document, node: BoxedNode | undefined): void;
  /** Redraw the box in RENDERED, the drawn prose of the row ROW: run on every paint. */
  mark(rendered: HTMLElement, row: string): void;
}

/** A StandingBox drawn as BOX, standing over no node to begin with. */
export function standingBox(box: SubtreeBox): StandingBox {
  let standing: BoxedNode | undefined;
  const mark = (rendered: HTMLElement, row: string): void => {
    unboxAll(rendered, box);
    if (standing === undefined || standing.row !== row) return;
    boxSubtree(rendered, standing.label, box);
  };
  return {
    stand(doc: Document, node: BoxedNode | undefined): void {
      standing = node;
      unboxAll(doc.body, box);
      if (node === undefined) return;
      for (const row of queryAll<HTMLElement>(doc.body, FEED_ROW_SELECTOR)) {
        if (feedRowId(row) === node.row) mark(row, node.row);
      }
    },
    mark,
  };
}
