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
 * The box stands over the node until the next push moves it, restated on
 * every paint of the response's prose (`markComposerEntry`, subtree-box.ts).
 */
import type { FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { standingBox, type BoxedNode } from "./subtree-box.js";

/** The class every line inside the box wears. */
export const COMPOSER_ENTRY_CLASS = "mp-line-composing";

/** The property naming the box's left edge, in `ch`, on each line inside it. */
export const COMPOSER_LEFT_PROPERTY = "--composer-left";

/** The property naming the box's right edge, in `ch`, on each line inside it. */
export const COMPOSER_RIGHT_PROPERTY = "--composer-right";

/** The node ENTRY names, or undefined for none. A push with no state, or a node naming no row, is refused. */
function nodeOf(entry: FeedComposerEntry): BoxedNode | undefined {
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

/** The composer's box (subtree-box.ts), standing over the node the composer answers. */
const composerBox = standingBox({
  className: COMPOSER_ENTRY_CLASS,
  leftProperty: COMPOSER_LEFT_PROPERTY,
  rightProperty: COMPOSER_RIGHT_PROPERTY,
});

/**
 * Box, in RENDERED (the drawn prose of the response row ROW), the subtree of
 * the node the composer answers, and take any box off every other line. Run
 * on every paint of a response's prose.
 */
export function markComposerEntry(rendered: HTMLElement, row: string): void {
  composerBox.mark(rendered, row);
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
      composerBox.stand(doc, node);
      log.info(node === undefined ? "the composer answers no node; nothing is highlighted" : "highlighted the subtree the composer answers", {
        operation: "feed.composer-entry.applied",
        context: { row: node?.row ?? "none", label: node?.label ?? "none" },
      });
    },
  };
}
