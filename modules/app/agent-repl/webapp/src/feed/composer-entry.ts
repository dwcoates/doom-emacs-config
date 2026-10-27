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
 *
 * THE FEED BRINGS THE BOX INTO VIEW when a push moves it to another node and
 * the box is not wholly in view (owner request, 2026-10-26): the feed moves
 * only as far as the box needs (`TailFollow.composerEntryRevealed`), and a box
 * already in view moves nothing. A push restating the node already boxed moves
 * nothing either: only a change moves the feed.
 */
import type { FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import type { RevealGeometry, TailFollow } from "../scroll.js";
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

/** The feed the box is brought into view in: its scroll box and the box's tail owner. */
export interface ComposerEntryScroll {
  readonly box: HTMLElement;
  readonly tail: TailFollow;
}

/** Whether A and B name the same node (none naming none). */
function sameNode(a: BoxedNode | undefined, b: BoxedNode | undefined): boolean {
  return a?.row === b?.row && a?.label === b?.label;
}

/**
 * The reveal geometry of the composer's box drawn in DOC against BOX: the
 * span from its first laid-out line's top to its last's bottom, or null when
 * no line of it is laid out (its row not drawn, or drawn folded away).
 */
export function composerBoxGeometry(doc: Document, box: HTMLElement): RevealGeometry | null {
  const lines = queryAll<HTMLElement>(doc.body, `.${COMPOSER_ENTRY_CLASS}`)
    .map((line) => line.getBoundingClientRect())
    .filter((rect) => rect.height > 0);
  if (lines.length === 0) return null;
  const top = Math.min(...lines.map((rect) => rect.top));
  const bottom = Math.max(...lines.map((rect) => rect.bottom));
  const view = box.getBoundingClientRect();
  return { boxTop: view.top, boxHeight: view.height, nodeTop: top, nodeHeight: bottom - top };
}

/**
 * The highlight drawn in DOC: each push stands its node and re-marks every
 * drawn row, and a push moving the box to another node brings it into view in
 * SCROLL's feed (none in a fixture with no scroll box).
 */
export function createComposerEntryHighlight(doc: Document, scroll?: ComposerEntryScroll): ComposerEntryHighlight {
  let standing: BoxedNode | undefined;
  /** Bring the box over NODE into view in SCROLL's feed, as far as it needs. */
  const reveal = (node: BoxedNode, on: ComposerEntryScroll): void => {
    const geometry = composerBoxGeometry(doc, on.box);
    if (geometry === null) {
      log.debug("the node the composer answers is not laid out in this feed; the feed stays", {
        operation: "feed.composer-entry.reveal-absent",
        context: { row: node.row, label: node.label },
      });
      return;
    }
    const moved = on.tail.composerEntryRevealed(geometry);
    log.debug(moved ? "brought the subtree the composer answers into view" : "the subtree the composer answers is in view; the feed stays", {
      operation: "feed.composer-entry.revealed",
      context: { row: node.row, label: node.label, moved },
    });
  };
  return {
    apply(entry: FeedComposerEntry): void {
      const node = nodeOf(entry);
      const changed = !sameNode(standing, node);
      standing = node;
      composerBox.stand(doc, node);
      log.info(node === undefined ? "the composer answers no node; nothing is highlighted" : "highlighted the subtree the composer answers", {
        operation: "feed.composer-entry.applied",
        context: { row: node?.row ?? "none", label: node?.label ?? "none" },
      });
      if (changed && node !== undefined && scroll !== undefined) reveal(node, scroll);
    },
  };
}
