/**
 * composer-entry — THE SUBTREE THE COMPOSER IS ANSWERING, boxed while the
 * person types its entry in Emacs.
 *
 * The daemon pushes, on the root feed's watch, the node of a response's tree
 * that the composer's entry at point answers (frontend.v1.FeedComposerEntry),
 * and says so again whenever it moves. The highlight is ONE BOX over that
 * node's subtree (owner ruling, 2026-10-24):
 *   - its rows are the node's lines and every line of each node beneath it
 *     (a response tree's labels are its nodes' places, as the daemon repairs
 *     them, so the subtree of `3.1` is `3.1` and every label opening on
 *     `3.1.`);
 *   - its left edge is the column the node's label starts at, so the
 *     connector leading to the node is outside it and its descendants' rails
 *     are inside;
 *   - its right edge is the nearest column covering every highlighted line's
 *     text.
 * A tree is drawn in a monospace face with every wide character boxed to two
 * columns (metaprompt-tree.ts), so a column is one `ch`.
 *
 * THE BOX IS RESTATED ON EVERY PAINT: a response's prose is repainted whenever
 * it re-wraps or re-arrives, so the response's prose slot marks it each time
 * it paints (`markComposerEntry`), from the node this module holds standing,
 * and a push re-marks whatever is drawn now.
 */
import type { FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { TREE_LABEL_CLASS, TREE_LINE_CLASS, TREE_NODE_ATTRIBUTE, textWidth } from "../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { FEED_ROW_SELECTOR, feedRowId } from "./row-id.js";

/** The class every line inside the box wears. */
export const COMPOSER_ENTRY_CLASS = "mp-line-composing";

/** The property naming the box's left edge, in `ch`, on each line inside it. */
export const COMPOSER_LEFT_PROPERTY = "--composer-left";

/** The property naming the box's right edge, in `ch`, on each line inside it. */
export const COMPOSER_RIGHT_PROPERTY = "--composer-right";

/** The node the composer answers, as the last push named it: its row and label. */
interface StandingNode {
  readonly row: string;
  readonly label: string;
}

/** The page's standing node, or undefined while the composer answers none. */
let standing: StandingNode | undefined;

/** The node ENTRY names, or undefined for none. A push with no state, or a node naming no row, is refused. */
function nodeOf(entry: FeedComposerEntry): StandingNode | undefined {
  const at = requireCase(entry.at, "FeedComposerEntry.at");
  switch (at.case) {
    case "none":
      return undefined;
    case "node":
      return { row: requireMessage(at.value.row, "FeedComposerEntryNode.row").value, label: at.value.label };
    default: {
      const other: { case: string } = at;
      return unreachableArm("FeedComposerEntry.at", other.case);
    }
  }
}

/** Whether NODE (a line's node label) is LABEL or beneath it. */
function inSubtree(node: string | null, label: string): boolean {
  return node !== null && (node === label || node.startsWith(`${label}.`));
}

/** The column LINE's own label starts at: the width of its prefix before the label. */
function labelColumn(line: HTMLElement): number {
  let text = "";
  for (const child of line.querySelector(".mp-prefix")?.childNodes ?? []) {
    if (child instanceof HTMLElement && child.classList.contains(TREE_LABEL_CLASS)) break;
    text += child.textContent ?? "";
  }
  return textWidth(text);
}

/** Take the box off LINE. */
function unmark(line: HTMLElement): void {
  line.classList.remove(COMPOSER_ENTRY_CLASS);
  line.style.removeProperty(COMPOSER_LEFT_PROPERTY);
  line.style.removeProperty(COMPOSER_RIGHT_PROPERTY);
}

/**
 * Box, in RENDERED (the drawn prose of the response row ROW), the subtree of
 * the node the composer answers, and take any box off every other line. Each
 * tree in RENDERED holding the node is boxed on its own. Run on every paint of
 * a response's prose, and over every drawn row on a push.
 */
export function markComposerEntry(rendered: HTMLElement, row: string): void {
  for (const line of queryAll<HTMLElement>(rendered, `.${COMPOSER_ENTRY_CLASS}`)) unmark(line);
  const node = standing;
  if (node === undefined || node.row !== row) return;
  for (const tree of queryAll<HTMLElement>(rendered, ".mp-tree")) {
    const lines = queryAll<HTMLElement>(tree, `.${TREE_LINE_CLASS}`).filter((l) =>
      inSubtree(l.getAttribute(TREE_NODE_ATTRIBUTE), node.label),
    );
    const head = lines.find((l) => l.getAttribute(TREE_NODE_ATTRIBUTE) === node.label);
    if (head === undefined) continue;
    const left = labelColumn(head);
    const right = Math.max(...lines.map((l) => textWidth((l.textContent ?? "").trimEnd())));
    for (const line of lines) {
      line.classList.add(COMPOSER_ENTRY_CLASS);
      line.style.setProperty(COMPOSER_LEFT_PROPERTY, `${left}ch`);
      line.style.setProperty(COMPOSER_RIGHT_PROPERTY, `${right}ch`);
    }
  }
}

/** The highlight of one page: `apply` draws each pushed state. */
export interface ComposerEntryHighlight {
  apply(entry: FeedComposerEntry): void;
}

/** The highlight drawn in DOC: each push stands its node and re-marks every drawn row. */
export function createComposerEntryHighlight(doc: Document): ComposerEntryHighlight {
  return {
    apply(entry: FeedComposerEntry): void {
      const node = nodeOf(entry);
      standing = node;
      for (const line of queryAll<HTMLElement>(doc.body, `.${COMPOSER_ENTRY_CLASS}`)) unmark(line);
      if (node !== undefined) {
        for (const row of queryAll<HTMLElement>(doc.body, FEED_ROW_SELECTOR)) {
          if (feedRowId(row) === node.row) markComposerEntry(row, node.row);
        }
      }
      log.info(node === undefined ? "the composer answers no node; nothing is highlighted" : "highlighted the subtree the composer answers", {
        operation: "feed.composer-entry.applied",
        context: { row: node?.row ?? "none", label: node?.label ?? "none" },
      });
    },
  };
}
