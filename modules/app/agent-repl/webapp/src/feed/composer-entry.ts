/**
 * composer-entry — THE SUBTREE THE COMPOSER IS ANSWERING, boxed while the
 * person types its entry in Emacs.
 *
 * The daemon pushes, on the root feed's watch, the node of a response's tree
 * that the composer's entry at point answers (frontend.v1.FeedComposerEntry),
 * and says so again whenever it moves. The highlight is ONE BOX over that
 * node's subtree, drawn as every highlight of a response's node is
 * (subtree-box.ts).
 *
 * THE BOX IS RESTATED ON EVERY PAINT: a response's prose is repainted whenever
 * it re-wraps or re-arrives, so the response's prose slot marks it each time
 * it paints (`markComposerEntry`), from the node this module holds standing,
 * and a push re-marks whatever is drawn now.
 */
import type { FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { FEED_ROW_SELECTOR, feedRowId } from "./row-id.js";
import { boxSubtree, unboxAll, type SubtreeBox } from "./subtree-box.js";

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

/** The composer's box (subtree-box.ts). */
const COMPOSER_BOX: SubtreeBox = {
  className: COMPOSER_ENTRY_CLASS,
  leftProperty: COMPOSER_LEFT_PROPERTY,
  rightProperty: COMPOSER_RIGHT_PROPERTY,
};

/**
 * Box, in RENDERED (the drawn prose of the response row ROW), the subtree of
 * the node the composer answers, and take any box off every other line. Each
 * tree in RENDERED holding the node is boxed on its own. Run on every paint of
 * a response's prose, and over every drawn row on a push.
 */
export function markComposerEntry(rendered: HTMLElement, row: string): void {
  unboxAll(rendered, COMPOSER_BOX);
  const node = standing;
  if (node === undefined || node.row !== row) return;
  boxSubtree(rendered, node.label, COMPOSER_BOX);
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
      unboxAll(doc.body, COMPOSER_BOX);
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
