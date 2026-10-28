/**
 * tree-fixture — drawn response trees for the tests of every highlight of a
 * response's node (subtree-box, composer-entry, branch-hover).
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { ProseTreeNodeSchema, ProseTreeSchema, type ProseTree } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import type { LengthResolver, SubtreeBox } from "../../src/feed/subtree-box.js";
import { TREE_CLASS, renderTreeHtml, columnMeasure } from "../../src/metaprompt-tree.js";

/** A node's place: a root with no PARENT, else a branch under it. */
function placeOf(parent: number | undefined): MessageInitShape<typeof ProseTreeNodeSchema>["place"] {
  return parent === undefined ? { case: "root", value: {} } : { case: "branch", value: { parent: { index: parent } } };
}

/** A tree of NODES, each (label, text, parent index or none). */
export function labelledTree(nodes: [string, string, number?][]): ProseTree {
  return create(ProseTreeSchema, {
    nodes: nodes.map(([label, text, parent], index) => ({
      id: { index },
      place: placeOf(parent),
      label: { text: label },
      text: { markdown: text },
    })),
  });
}

/**
 * The owner's example: 1, its child 1.1 and grandchild 1.1.1, then 1.2 and
 * 1.10, whose label merely opens on 1.1's characters.
 */
export const exampleTree = labelledTree([
  ["1", "The subtree highlight landed"],
  ["1.1", "It hasn't reached the running build yet", 0],
  ["1.1.1", "It goes live with the next rollout", 1],
  ["1.2", "A sibling", 0],
  ["1.10", "A lookalike", 0],
]);

/** The width of the widest line of exampleTree's 1.1 subtree: its grandchild's. */
export const exampleSubtreeWidth = `    └── 1.1.1 It goes live with the next rollout`.length;

/** The px of one drawn column under `TEN_PX_COLUMNS`. */
export const COLUMN_PX = 10;

/**
 * A box edge resolved as a 10px-column page lays it out: jsdom lays nothing
 * out, so a hit test is handed this in place of the real probe.
 */
export const TEN_PX_COLUMNS: LengthResolver = (_line, length) => {
  const m = /^(-?\d+(?:\.\d+)?)(ch|px)$/.exec(length);
  if (m === null) throw new Error(`no length in ${length}`);
  return m[2] === "ch" ? Number(m[1]) * COLUMN_PX : Number(m[1]);
};

/** A row ROW drawing TREES, each in its own tree container, as the document's whole body. */
export function drawnRow(row: string, ...trees: ProseTree[]): HTMLElement {
  const el = document.createElement("article");
  el.setAttribute("data-feed-row", row);
  el.innerHTML = trees.map((t) => `<div class="${TREE_CLASS}">${renderTreeHtml(t, (s) => s, columnMeasure(120))}</div>`).join("");
  document.body.replaceChildren(el);
  return el;
}

/** The line of EL drawing NODE. */
export function lineOf(el: HTMLElement, node: string): HTMLElement {
  const line = el.querySelector<HTMLElement>(`[data-node="${node}"]`);
  if (line === null) throw new Error(`no line draws ${node}`);
  return line;
}

/** The label drawn on the line of EL drawing NODE. */
export function labelOf(el: HTMLElement, node: string): Element {
  const label = lineOf(el, node).querySelector(".mp-label");
  if (label === null) throw new Error(`${node}'s line draws no label`);
  return label;
}

/** Each line of EL wearing BOX, as (its node, its left edge, its right edge). */
export const boxedBy = (el: HTMLElement, box: SubtreeBox): [string | null, string, string][] =>
  [...el.querySelectorAll<HTMLElement>(`.${box.className}`)].map((l) => [
    l.getAttribute("data-node"),
    l.style.getPropertyValue(box.leftProperty),
    l.style.getPropertyValue(box.rightProperty),
  ]);
