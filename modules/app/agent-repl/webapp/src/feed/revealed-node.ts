/**
 * revealed-node — THE RESPONSE NODE A PROMPT'S ENTRY ANSWERS, boxed while the
 * selection the entry's click made stands (owner ruling, 2026-10-24).
 *
 * A click on a prompt's entry asks the daemon to select the response the
 * entry answers (agentrepl.v1.SelectFeedRowEntry), which pushes that
 * selection with frontend.v1.FeedSelectionRowViewportRevealNode naming the
 * node. The box stands over the node, with its subtree, for as long as that
 * selection does, drawn as every highlight of a response's node is
 * (subtree-box.ts), and goes with the next selection push that names no such
 * node.
 */
import type { FeedSelection } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { standingBox, type BoxedNode } from "./subtree-box.js";

/** The class every line of the revealed node's box wears. */
export const REVEALED_NODE_CLASS = "mp-line-revealed";

/** The property naming the revealed box's left edge, in `ch`, on each line inside it. */
export const REVEAL_LEFT_PROPERTY = "--reveal-left";

/** The property naming the revealed box's right edge, in `ch`, on each line inside it. */
export const REVEAL_RIGHT_PROPERTY = "--reveal-right";

/** The revealed node's box (subtree-box.ts). */
const revealedBox = standingBox({
  className: REVEALED_NODE_CLASS,
  leftProperty: REVEAL_LEFT_PROPERTY,
  rightProperty: REVEAL_RIGHT_PROPERTY,
});

/** The node SELECTION reveals, or undefined when it reveals none. A row arm with no row or viewport is refused. */
export function revealedNodeOf(selection: FeedSelection): BoxedNode | undefined {
  const arm = requireCase(selection.selection, "FeedSelection.selection");
  if (arm.case === "none") return undefined;
  const viewport = requireCase(
    requireMessage(arm.value.viewport, "FeedSelectionRowViewport").viewport,
    "FeedSelectionRowViewport.viewport",
  );
  if (viewport.case !== "revealNode") return undefined;
  return { row: requireMessage(arm.value.row, "FeedSelection.row").value, label: viewport.value.label };
}

/** Stand the box over the node SELECTION reveals in DOC, or take it down when it reveals none. */
export function applyRevealedNode(doc: Document, selection: FeedSelection): void {
  const node = revealedNodeOf(selection);
  revealedBox.stand(doc, node);
  log.debug(node === undefined ? "the selection reveals no node" : "boxed the node the selection reveals", {
    operation: "feed.revealed-node.applied",
    context: { row: node?.row ?? "none", label: node?.label ?? "none" },
  });
}

/** Redraw the revealed node's box in RENDERED, the drawn prose of the row ROW: run on every paint. */
export function markRevealedNode(rendered: HTMLElement, row: string): void {
  revealedBox.mark(rendered, row);
}
