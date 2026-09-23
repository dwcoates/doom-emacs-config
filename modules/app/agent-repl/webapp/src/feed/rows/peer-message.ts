/**
 * peer-message — A MESSAGE FROM ANOTHER CLAUDE: an inter-session peer message
 * or a subagent hand-back.
 *
 * ITS SPEC, AND NOTHING ELSE (owner rulings, 2026-09-23). It is a prompt-role
 * bubble — the right rail and the prompt fill, like every prompt kind — whose
 * header strip is the sender's label and whose collapsed line limit is ZERO:
 * collapsed, it shows its label and nothing of its body, which is the
 * abbreviated aside it has always been. It opens and closes through the one
 * toggle every bubble has (expand.ts: a click on the strip or the body toggles
 * the scroll box), wears the one has-more fade, and its body is a markdown
 * slot the one body pipeline paints, so a tree in it wraps at the bubble's cap.
 */
import { log } from "../../log.js";
import { markdownSlot } from "../../bubble/body.js";
import { drawBubble } from "../../bubble/draw.js";
import type { FeedPeerMessage } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** The class the peer bubble's hooks know it by. */
export const PEER_BUBBLE_CLASS = "peer";

/** The header strip's one element: the sender label. */
export const PEER_LABEL_CLASS = "peer-label";

/** The class of the markdown slot the peer's body is painted into. */
export const PEER_BODY_CLASS = "peer-body";

/** The peer bubble: its spec, drawn (in place over PREVIOUS) by the one bubble. */
export function drawFeedPeerMessage(msg: FeedPeerMessage, previous?: HTMLElement): HTMLElement {
  log.info("drawing a peer-message row", {
    operation: "feed.draw-peer-message",
    context: { sender: msg.sender },
  });
  const label = document.createElement("div");
  label.className = PEER_LABEL_CLASS;
  // The daemon composes the label ("agent <sender>"); drawn verbatim.
  label.textContent = msg.sender;
  return drawBubble(
    {
      role: "prompt",
      variant: "peer",
      hooks: [PEER_BUBBLE_CLASS],
      working: false,
      strip: [label],
      content: [markdownSlot(PEER_BODY_CLASS, msg.body)],
      capLines: 0,
    },
    previous,
  ).bubble;
}
