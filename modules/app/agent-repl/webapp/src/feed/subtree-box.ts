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
 * The edges are the ones the tree was wrapped by: every drawn branch line
 * carries where its text ends, and a head line where its label starts, in the
 * unit its measure measured in (metaprompt-tree.ts), so a box covers exactly
 * the text as it draws, in any face.
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
import { log } from "../log.js";
import {
  TREE_CLASS,
  TREE_LABEL_AT_ATTRIBUTE,
  TREE_LINE_CLASS,
  TREE_NODE_ATTRIBUTE,
  TREE_TEXT_END_ATTRIBUTE,
  cssLength,
  type TreeMeasure,
} from "../metaprompt-tree.js";

/** One kind of box: the class its lines wear and the properties naming its edges, as CSS lengths. */
export interface SubtreeBox {
  readonly className: string;
  readonly leftProperty: string;
  readonly rightProperty: string;
}

/** Whether NODE (a line's node label) is LABEL or beneath it. */
export function inSubtree(node: string | null, label: string): boolean {
  return node !== null && (node === label || node.startsWith(`${label}.`));
}

/** A drawn line's edge, as a CSS length, split into its number and its unit. */
interface Edge {
  readonly n: number;
  readonly unit: TreeMeasure["unit"];
}

/** The edge LINE carries in ATTRIBUTE: every line the renderer drew carries it. */
function edgeOf(line: HTMLElement, attribute: string): Edge {
  const value = line.getAttribute(attribute) ?? "";
  const m = /^(-?\d+(?:\.\d+)?(?:e-?\d+)?)(ch|px)$/.exec(value);
  if (m === null) {
    log.error("a drawn tree line carries no edge to box it by", {
      operation: "feed.subtree-box.edge",
      context: { attribute, value, node: line.getAttribute(TREE_NODE_ATTRIBUTE) },
    });
    throw new Error(`subtree box: a tree line's ${attribute} is ${JSON.stringify(value)}`);
  }
  return { n: Number(m[1]), unit: m[2] as TreeMeasure["unit"] };
}

/** Where LINE's own label starts (a head line's), as a CSS length. */
export function labelColumn(line: HTMLElement): string {
  const edge = edgeOf(line, TREE_LABEL_AT_ATTRIBUTE);
  return cssLength(edge.n, edge.unit);
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
    const span = subtreeSpan(tree, label);
    if (span === null) continue;
    for (const line of span.lines) {
      line.classList.add(box.className);
      line.style.setProperty(box.leftProperty, span.left);
      line.style.setProperty(box.rightProperty, span.right);
    }
  }
}

/** The lines a node's box covers in one tree, and its edges as CSS lengths. */
export interface SubtreeSpan {
  readonly lines: readonly HTMLElement[];
  readonly left: string;
  readonly right: string;
}

/**
 * Where the box over LABEL's subtree stands in TREE (one drawn tree), as
 * the module note describes it, or null when TREE draws no line of LABEL.
 * Every highlight draws its box from this, and the pointer's hit test
 * (`nodeBoxedAt`) reads the very same edges, so the area that boxes a node is
 * the box it draws.
 */
export function subtreeSpan(tree: HTMLElement, label: string): SubtreeSpan | null {
  const lines = queryAll<HTMLElement>(tree, `.${TREE_LINE_CLASS}`).filter((l) =>
    inSubtree(l.getAttribute(TREE_NODE_ATTRIBUTE), label),
  );
  const head = lines.find((l) => l.getAttribute(TREE_NODE_ATTRIBUTE) === label);
  if (head === undefined) return null;
  const ends = lines.map((l) => edgeOf(l, TREE_TEXT_END_ATTRIBUTE));
  return { lines, left: labelColumn(head), right: cssLength(Math.max(...ends.map((e) => e.n)), ends[0].unit) };
}

/** Resolves LENGTH (a box edge) to px from LINE's own left edge, in LINE's own face. */
export type LengthResolver = (line: HTMLElement, length: string) => number;

/**
 * The px LENGTH draws in LINE: a probe as wide as LENGTH, laid out in LINE so
 * a `ch` is LINE's own column, measured and taken out again.
 */
export function lengthInLine(line: HTMLElement, length: string): number {
  const probe = line.ownerDocument.createElement("span");
  probe.style.cssText = `position:absolute;visibility:hidden;height:0;padding:0;border:0;width:${length}`;
  line.append(probe);
  const px = probe.getBoundingClientRect().width;
  probe.remove();
  if (!Number.isFinite(px)) {
    log.error("a tree box's edge cannot be measured in px", {
      operation: "feed.subtree-box.unmeasurable",
      context: { length, node: line.getAttribute(TREE_NODE_ATTRIBUTE) },
    });
    throw new RangeError(`subtree box: the edge ${length} measured ${String(px)}px`);
  }
  return px;
}

/** LABEL, then each node it sits beneath, nearest first: `1.2.3`, `1.2`, `1`. */
export function selfAndAncestors(label: string): string[] {
  const parts = label.split(".");
  return parts.map((_, i) => parts.slice(0, parts.length - i).join("."));
}

/** One node whose box covers a line, and that box's px span across it. */
export interface LineBox {
  readonly label: string;
  readonly left: number;
  readonly right: number;
}

/**
 * Every box that covers LINE (a drawn tree line naming its node), deepest
 * first: the box of LINE's own node and of each node above it, each in px
 * from LINE's left edge as RESOLVE measures it.
 */
export function boxesOverLine(line: HTMLElement, resolve: LengthResolver = lengthInLine): LineBox[] {
  const node = line.getAttribute(TREE_NODE_ATTRIBUTE);
  const tree = line.closest<HTMLElement>(`.${TREE_CLASS}`);
  if (node === null || tree === null) {
    log.error("a tree line cannot be hit-tested: it names no node or hangs in no tree", {
      operation: "feed.subtree-box.unplaced-line",
      context: { node, in_tree: tree !== null },
    });
    throw new Error("subtree box: a hit-tested line names no node or hangs in no tree");
  }
  return selfAndAncestors(node).flatMap((label) => {
    const span = subtreeSpan(tree, label);
    return span === null ? [] : [{ label, left: resolve(line, span.left), right: resolve(line, span.right) }];
  });
}

/** The deepest of BOXES (`boxesOverLine`) covering X px from its line's left edge, or null. */
export function nodeBoxedAt(boxes: readonly LineBox[], x: number): string | null {
  return boxes.find((box) => box.left <= x && x < box.right)?.label ?? null;
}

/** X, a pointer's viewport x, as px from LINE's left edge (the box's own origin). */
export function xInLine(line: HTMLElement, clientX: number): number {
  return clientX - (line.getBoundingClientRect().left + line.clientLeft);
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
